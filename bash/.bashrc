# /etc/skel/.bashrc
#
# This file is sourced by all *interactive* bash shells on startup,
# including some apparently interactive shells such as scp and rcp
# that can't tolerate any output.  So make sure this doesn't display
# anything or bad things will happen !


# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
if [[ $- != *i* ]] ; then
	# Shell is non-interactive.  Be done now!
	return
fi


# Put your fun stuff here.
function git_unstaged() {
    if git status 1> /dev/null 2>&1
    then
        if git status | grep -oP "Untracked files|Changes not staged" 1> /dev/null 2>&1
        then
            echo "+"
        fi
    fi
}

function git_staged() {
    if git status 1> /dev/null 2>&1
    then
        if git status | grep -oP "Changes to be committed" 1> /dev/null 2>&1
        then
            echo "+"
        fi
    fi
}

function git_status_info() {
    gitstatus="\[\033[01;30m\]no git\[\033[00m\]"
    if git rev-parse --git-dir > /dev/null 2>&1; then
        branch=$(git branch | grep -oP "\* .+" | sed "s/\* //g")
        staged=$(git_staged)
        unstaged=$(git_unstaged)
        gitstatus="\[\033[00;34m\]$branch\[\033[00;31m\]$unstaged\[\033[00;33m\]$staged\[\033[00m\]"
    fi
    echo "\[\033[01;39m\](\[\033[00m\]$gitstatus\[\033[01;39m\])\[\033[00m\]"
}

export -f git_status_info

recalc_prompt() {
    PS1="\[\033[01;33m\]\u@\[\033[01;39m\]\h\[\033[01;34m\] \w\[\033[00m\] $(git_status_info)\[\033[01;36m\] |Ψ 〉 \[\033[00m\]"
}

PROMPT_COMMAND=recalc_prompt
