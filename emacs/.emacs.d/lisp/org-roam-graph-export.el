;;; org-roam-graph-export.el --- Graphing API -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/org-roam/org-roam
;; Keywords: org-mode, roam, convenience
;; Version: 1.2.3
;; Package-Requires: ((emacs "26.1") (dash "2.13") (f "0.17.2") (s "1.12.0") (org "9.3") (emacsql "3.0.0") (emacsql-sqlite3 "1.0.2"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library provides graphing functionality for org-roam.
;;
;;; Code:
(require 'xml) ;xml-escape-string
(require 's)   ;s-truncate, s-replace
(require 'org-roam-graph)
(eval-and-compile
  (require 'org-roam-macs))
(require 'org-roam-db)

(declare-function org-roam--org-roam-file-p  "org-roam")
(declare-function org-roam--path-to-slug     "org-roam")
(declare-function org-roam-mode              "org-roam")

;;;; Functions
(defun org-roam-graph-export--dot (node-query format)
  "Build the graphviz dot string for NODE-QUERY.
The Org-roam database titles table is read, to obtain the list of titles.
The links table is then read to obtain all directed links, and formatted
into a digraph."
  (org-roam-db--ensure-built)
  (org-roam--with-temp-buffer nil
    (let* ((nodes (org-roam-db-query node-query))
           (edges-query
            `[:with selected :as [:select [file] :from ,node-query]
              :select :distinct [dest source] :from links
              :where (and (in dest selected) (in source selected))])
           (edges-cites-query
            `[:with selected :as [:select [file] :from ,node-query]
              :select :distinct [file source]
              :from links :inner :join refs :on (and (= links:dest refs:ref)
                                                     (= links:type "cite")
                                                     (= refs:type "cite"))
              :where (and (in file selected) (in source selected))])
           (edges       (org-roam-db-query edges-query))
           (edges-cites (org-roam-db-query edges-cites-query)))
      (insert "digraph \"org-roam\" {\n")
      (dolist (option org-roam-graph-extra-config)
        (insert (org-roam-graph--dot-option option) ";\n"))
      (dolist (attribute '("node" "edge"))
        (insert (format " %s [%s];\n" attribute
                        (mapconcat (lambda (var)
                                     (org-roam-graph--dot-option var nil "\""))
                                   (symbol-value
                                    (intern (concat "org-roam-graph-" attribute "-extra-config")))
                                   ","))))
      (dolist (node nodes)
        (let* ((file (xml-escape-string (car node)))
               (title (or (cadr node)
                          (org-roam--path-to-slug file)))
               (shortened-title (pcase org-roam-graph-shorten-titles
                                  (`truncate (s-truncate org-roam-graph-max-title-length title))
                                  (`wrap (s-word-wrap org-roam-graph-max-title-length title))
                                  (_ title)))
               (shortened-title (org-roam-string-quote shortened-title))
               (title (org-roam-string-quote title))
	       (fnse (file-name-sans-extension (file-name-base file)))
	       (url (pcase format
		      ('html (concat fnse ".html"))
		      (_ (concat "org-protocol://roam-file?file=" (url-hexify-string file)))))
               (node-properties
                `(("label"   . ,shortened-title)
                  ("URL"     . ,url)
                  ("tooltip" . ,(xml-escape-string title)))))
          (insert
           (format "  \"%s\" [%s];\n" file
                   (mapconcat (lambda (n)
                                (org-roam-graph--dot-option n nil "\""))
                              node-properties ",")))))
      (dolist (edge edges)
        (insert (apply #'format `("  \"%s\" -> \"%s\";\n"
                                  ,@(mapcar #'xml-escape-string edge)))))
      (insert (format "  edge [%s];\n"
                      (mapconcat #'org-roam-graph--dot-option
                                 org-roam-graph-edge-cites-extra-config ",")))
      (dolist (edge edges-cites)
        (insert (apply #'format `("  \"%s\" -> \"%s\";\n"
                                  ,@(mapcar #'xml-escape-string edge)))))
      (insert "}")
      (buffer-string))))

(defun org-roam-graph-export--build (&optional node-query format callback)
  "Generate a graph showing the relations between nodes in NODE-QUERY.
Execute CALLBACK when process exits successfully.
CALLBACK is passed the graph file as its sole argument."
  (unless (stringp org-roam-graph-executable)
    (user-error "`org-roam-graph-executable' is not a string"))
  (unless (executable-find org-roam-graph-executable)
    (user-error (concat "Cannot find executable \"%s\" to generate the graph.  "
                        "Please adjust `org-roam-graph-executable'")
                org-roam-graph-executable))
  (let* ((node-query (or node-query
                         `[:select [file title] :from titles
                           ,@(org-roam-graph--expand-matcher 'file t)
                           :group :by file]))
         (graph      (org-roam-graph-export--dot node-query format))
         (temp-dot   (make-temp-file "graph." nil ".dot" graph))
         (temp-graph (make-temp-file "graph." nil ".svg")))
    (org-roam-message "building graph")
    (make-process
     :name "*org-roam-graph--build-process*"
     :buffer "*org-roam-graph--build-process*"
     :command `(,org-roam-graph-executable ,temp-dot "-Tsvg" "-o" ,temp-graph)
     :sentinel (when callback
                 (lambda (process _event)
                   (when (= 0 (process-exit-status process))
                     (funcall callback temp-graph)))))))

;;;; Commands
;;;###autoload
(defun org-roam-graph-export (&optional arg file node-query format output)
  "Build and possibly display a graph for FILE from NODE-QUERY.
If FILE is nil, default to current buffer's file name.
ARG may be any of the following values:
  - nil       show the graph.
  - `\\[universal-argument]'     show the graph for FILE.
  - `\\[universal-argument]' N   show the graph for FILE limiting nodes to N steps.
  - `\\[universal-argument] \\[universal-argument]' build the graph.
  - `\\[universal-argument]' -   build the graph for FILE.
  - `\\[universal-argument]' -N  build the graph for FILE limiting nodes to N steps."
  (interactive "P")
  (unless org-roam-mode (org-roam-mode))
  (org-roam-message "starting graph build & export...")
  (org-roam-message (concat "org-roam-graph-executable is "
			    org-roam-graph-executable))
  (org-roam-message (concat "temporary-file-directory is "
			    temporary-file-directory))
  (let ((file (or file (buffer-file-name (buffer-base-buffer)))))
    (unless (or (not arg) (equal arg '(16)))
      (unless file
        (user-error "Cannot build graph for nil file. Is current buffer visiting a file?"))
      (unless (org-roam--org-roam-file-p file)
        (user-error "\"%s\" is not an org-roam file" file)))
    (org-roam-graph-export--build node-query format
				  (lambda (file)
				    (and (org-roam-message
					  (concat "Attempting to copy " file " to " output))
					 (rename-file file output t))))))

(provide 'org-roam-graph-export)

;;; org-roam-graph.el ends here

