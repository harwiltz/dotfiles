(defcustom hatch-work-time 49
  "Length of work period in minutes"
  :type 'integer)

(defcustom hatch-short-break-time 10
  "Length of short breaks between work intervals, in minutes"
  :type 'integer)

(defcustom hatch-long-break-time 60
  "Length of long breaks between sets of work intervals, in minutes"
  :type 'integer)

(defcustom hatch-work-set-reps 2
  "Number of work intervals for each set separated by a long break"
  :type 'integer)

(defcustom hatch-dir "~/.emacs.d/lisp/hatch-button"
  "Directory where hatch-button is installed"
  :type 'string)

(defvar hatch-sounds-dir (concat hatch-dir "/sounds")
  "Directory where sound files are stored")

(defcustom hatch-warning-sound "censor-beep"
  "Sound to play when timer is about to go off"
  :type 'string)

(defcustom hatch-alarm-sound "censor-beep"
  "Sound to play when timer is about to go off"
  :type 'string)

(defcustom hatch-reset-sound "smoothtone"
  "Sound to play when timer is reset"
  :type 'string)

(defcustom hatch-start-sound "electronic-chime"
  "Sound to play when work interval starts"
  :type 'string)

(defcustom hatch-volume 0.85
  "Volume for sound effects"
  :type 'float)

(defun hatch-sound-file (sound)
  "Resolve file path of sound from its name"
  (concat hatch-sounds-dir "/" sound ".wav"))

(defun hatch-sound-play (sound &optional volume)
  "Play the requested sound"
  (play-sound-file (hatch-sound-file sound) (or volume 1.0))
  (let ((visual-bell t)) (ding)))

(defun hatch-start-work (&optional reps-remaining)
  "Start work interval"
  (interactive)
  (hatch-sound-play hatch-start-sound)
  (setq hatch/reps-remaining (or reps-remaining hatch-work-set-reps))
  (setq hatch/warning-timer
	(run-at-time (concat (number-to-string (- hatch-work-time 1)) " min")
	nil
	'hatch-sound-play
	hatch-warning-sound
	hatch-volume))
  (setq hatch/cur-timer
	(run-at-time (concat (number-to-string hatch-work-time) " min")
		     3 ;; repeat alarm every 3 seconds
		     'hatch-sound-play
		     hatch-alarm-sound
		     hatch-volume)))

(defun hatch-reset (password)
  "Reset the timer"
  (interactive "s> ")
  (when (string= password "4 8 15 16 23 42")
    (hatch-sound-play hatch-reset-sound hatch-volume)
    (cancel-timer hatch/cur-timer)
    (let* ((short-break-p (> hatch/reps-remaining 0))
	   (break-length (if short-break-p hatch-short-break-time hatch-long-break-time))
	   (break-end-time (concat (number-to-string break-length) " min"))
	   (reps-remaining (if short-break-p (- hatch/reps-remaining 1) hatch-work-set-reps)))
      (setq hatch/cur-timer (run-at-time break-end-time
					 nil
					 'hatch-start-work
					 reps-remaining)))))

(defun hatch-monitor ()
  "Check remaining time and current state"
  (interactive)
  (when (boundp 'hatch/cur-timer)
    (if hatch/cur-timer
	(let* ((timer hatch/cur-timer)
	       (time (list (aref timer 1) (aref timer 2) (aref timer 3)))
	       (end-fn (symbol-name (aref timer 5)))
	       (time-remaining (truncate (hatch-seconds-remaining)))
	       (hours (/ time-remaining 3600))
	       (minutes (truncate (/ (% time-remaining 3600) 60)))
	       (seconds (% time-remaining 60))
	       (h-unit (if (= hours 1) "hour" "hours"))
	       (m-unit (if (= minutes 1) "minute" "minutes"))
	       (s-unit (if (= seconds 1) "second" "seconds"))
	       (h (if (> hours 0) (concat (number-to-string hours) " hours, ") ""))
	       (m (if (or (> hours 0) (> minutes 0)) (concat (number-to-string minutes) " minutes, ") ""))
	       (s (concat (number-to-string seconds) " seconds."))
	       (state-str (if (hatch-work-p) "WORK" "BREAK")))
	  (message (concat "[" state-str "] Time remaining: " h m s)))
      (message (concat "No timer scheduled. I guess the hatch imploded.")))))

(defun hatch-work-p ()
  (when (and (boundp 'hatch/cur-timer) hatch/cur-timer)
    (let* ((timer hatch/cur-timer)
	   (end-fn (symbol-name (aref timer 5))))
      (string= end-fn "hatch-sound-play"))))

(defun hatch-break-p ()
  (when (and (boundp 'hatch/cur-timer) hatch/cur-timer)
    (let* ((timer hatch/cur-timer)
	   (end-fn (symbol-name (aref timer 5))))
      (string= end-fn "hatch-start-work"))))

(defun hatch-seconds-remaining ()
  (when (and (boundp 'hatch/cur-timer) hatch/cur-timer)
    (let* ((timer hatch/cur-timer)
	   (time (list (aref timer 1) (aref timer 2) (aref timer 3))))
      (float-time (time-subtract time nil)))))

(defun hatch-kill ()
  (cancel-timer hatch/cur-timer)
  (cancel-timer hatch/warning-timer)
  (setq hatch/cur-timer nil)
  (setq hatch/warning-timer nil)
  (setq hatch/reps-remaining hatch-work-set-reps))

(provide 'hatch-button)
