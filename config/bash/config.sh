git_branch() { # get current git branch
  temp=$(git branch 2>/dev/null | sed 's/^\* //')
  if [ $(echo $temp | wc -l) -eq 1 ]; then
    echo $temp
  fi
}

make_prompt() {
  local red="\[$(tput setaf 9)\]"
  local olive="\[$(tput setaf 10)\]"
  local yellow="\[$(tput setaf 11)\]"
  local orange="\[$(tput setaf 12)\]"
  local purple="\[$(tput setaf 13)\]"
  local green="\[$(tput setaf 14)\]"
  local bold="\[$(tput bold)\]"
  local reset="\[$(tput sgr0)\]"

  local user="$orange\u$reset"
  local host="$red$bold\h$reset"
  local repo="$green\$(git_branch)$reset"
  local dir="$olive\w$reset"
  local symbol="$yellow|>$reset"

  echo "$user@$host($repo) $dir\n$symbol "
}

PS1=$(make_prompt)

shopt -s checkwinsize # refresh when window size change
shopt -s histappend # only append history

alias cd='cd -P' # follow symlinks
alias gpg=gpg2
alias ls='ls -F --color=auto' # colors and short descriptors (e.g. */?)

alias   e="emacsclient --tty --socket-name=emacsd"
alias edit="emacsclient -nc --socket-name=emacsd"

alias surch="xbps-query -R -s"
alias wget='wget --hsts-file="$XDG_CACHE_HOME/wget-hsts"'
alias anime="ani-cli -q 720"
alias manga="manga-cli"
