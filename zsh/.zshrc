# The following lines were added by compinstall

zstyle ':completion:*' completer _complete _ignored _correct
zstyle ':completion:*' file-sort name
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' menu select
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle :compinstall filename '/home/harwiltz/.zshrc'


export VISUAL="/usr/bin/vim"
export EDITOR="$VISUAL"

export ANDROID_SDK_ROOT=/home/harwiltz/Android/Sdk

autoload -Uz compinit
compinit
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
bindkey -v
# End of lines configured by zsh-newuser-install
# Created by harwiltz

autoload -U colors && colors

alias ls="ls --color=auto"
alias grep='grep --color=auto'
alias l='asciiquarium'

alsi -a arch
#echo ""

# Attempt to fix PS1 in emacs shell
if [[ $TERM = dumb || $TERM = eterm-color ]]; then
    #echo "Unsetting zle_bracketed_paste"
    unset zle_bracketed_paste
    #echo "Unsetting zle_line_init"
    unset zle_line_init
else
    setopt PROMPT_SUBST
fi

bindkey '^?' backward-delete-char

success() {
    RESULT=\$?
    if [ $RESULT == 0 ]; then
        echo "✓"
    else
        echo "✘"
    fi
}
#DIRAC NOTATION WITH GIT INTEGRATION
#PROMPT='%{$fg_bold[yellow]%}%n@%F{103}%M%F{240}:%{$reset_color%}%{$fg_bold[blue]%}$(python /home/harwiltz/scripts/trunccwd.py) %{$fg_bold[magenta]%}($(git_branch)%{$reset_color%}%{$fg[red]%}$(git_unstaged)%{$fg[green]%}$(git_staged)%{$fg_bold[magenta]%})%{$reset_color%}%(?.%{$fg_bold[green]%}│Ψ > .%{$fg_bold[red]%}♯ %? )%{$reset_color%}%{$reset_color%}'

alias batterystatus='acpi'
alias showtime='tty-clock -s -c -b'
alias listen='mp3blaster'
alias sdmf='xinit bspwm'
alias hotkeys='vim ~/.config/sxhkd/sxhkdrc'
alias blackhawkdown='shutdown -h now'
alias updatefonts='fc-cache -vf'
alias nettest='ping -c 3 www.google.com'
alias interface='wlp3s0'
alias editvimcolors='sudo vim /usr/share/vim/vim74/colors'
alias pokerstars='C:\\Program\ Files\ (x86)\\PokerStars\\PokerStars.exe'
#alias tig2d='wine ~/win32/drive_c/"Program Files"/"Tigarmageddon2D"/"Tigarmageddon 2D.exe"'
alias please='sudo $(fc -lnr -1)'
alias gtfo='xrdb -load .Xdefaults && xrdb -merge .Xdefaults && exit'
alias rstream='~/CPP/rstream/rstream'
alias copy="nohup termite -d $PWD &"

alias gl="git log --pretty=tformat:'%C(226)%h %C(60 bold)%d%Creset %s %C(97)by %an, %cr%Creset' --graph"

export LESS='-R'

#eval $(dircolors ~/.dircolors)

#setopt AUTO_CD
#setopt CORRECT

command_not_found_handler() {
    ~/CPP/Typo/a.out
    exit 127
}

function git_branch() {
    if git status 1> /dev/null 2>&1
    then
        echo $(git status | head -n 1 | grep -oP "(?<=branch) \S+")
    else
        echo "no git"
    fi
}

function stopwatch() {
    start=`date +%s`
    cur=0
    while true; do
        cur="$(date -u --date @$((`date +%s` - $start)) +%H:%M:%S)"
        echo -ne "$cur\r"
    done
    echo "\n"
    echo "Timed: $cur"
}


################
# PROMPT STUFF #
################
CWD_THRESH=30

PS1_USERHOST="%B%F{yellow}%n@%F{magenta}%M%f%b"
PS1_CWD_LONG="%B%F{blue}%~%f%b"
PS1_CWD_SHORT="%B%F{cyan}_/%F{blue}%1~%f%b"
#PS1_CWD_TRUNC="%B%F{blue}%3~%f%b"
PS1_CWD_TRUNC="%B%F{blue}%20<..<%~%<<"
PS1_PROMPT="%(?.%B%F{green}│Ψ > .%B%F{red}♯ %? )%f%b"
#export PROMPT='$(get_ps1)'


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
    gitstatus="%B%F{237}no git%b%f"
    if git rev-parse --git-dir > /dev/null 2>&1; then
        branch=$(git branch | grep -oP "\* .+" | sed "s/\* //g")
        staged=$(git_staged)
        unstaged=$(git_unstaged)
        gitstatus="%B%F{27}$branch%b%F{red}$unstaged%F{green}$staged%f"
    fi
    echo "%B%F{cyan}(%b%f$gitstatus%B%F{cyan})%b%f"
}

function get_ps1() {
    cwd=$(pwd)
    if [[ ${#cwd} -gt CWD_THRESH ]]; then
        echo "$PS1_USERHOST %{$(git_status_info)%}\n$PS1_CWD_SHORT $PS1_PROMPT"
    else
        echo "$PS1_USERHOST $PS1_CWD_LONG %{$(git_status_info)%} $PS1_PROMPT"
    fi
}

function get_ps1_trunc() {
    echo "$PS1_USERHOST $PS1_CWD_TRUNC %{$(git_status_info)%} $PS1_PROMPT"
}

export PROMPT='$(get_ps1_trunc)'

export PATH="/home/harwiltz/.local/bin:$PATH"
export PATH="/home/harwiltz/android-studio/android-studio/bin:$PATH"
#source /home/harwiltz/api_tokens

#source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/home/harwiltz/nomadic-packages/google-cloud-sdk/path.zsh.inc' ]; then . '/home/harwiltz/nomadic-packages/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/home/harwiltz/nomadic-packages/google-cloud-sdk/completion.zsh.inc' ]; then . '/home/harwiltz/nomadic-packages/google-cloud-sdk/completion.zsh.inc'; fi
