# This is the gentoo default
#export PS1='\[\033[01;32m\]\u@\h\[\033[01;34m\] \w \$\[\033[00m\] '

RESET='[00m'
RED='[01;31m'
GREEN='[01;32m'
YELLOW='[01;33m'
BLUE='[01;34m'
MAGENTA='[01;35m'
CYAN='[01;36m'
WHITE='[01;37m'
UNDERLINE='[04m'

shopt -s checkwinsize
stty -ixon -ixoff

export HISTIGNORE='git*--amend*:ls:cd:git*-m*:git*-am*:git*-f*:rm -rf*'
export HISTCONTROL=ignoredups:ignorespace

function header()
{
  begin_str=">>>"
  end_str=">>>"
  echo -en "\e${RED}${begin_str}\e${WHITE}${line// /-}[\e${MAGENTA}${banner}\e${WHITE}]${line// /-}\e${RED}${end_str}\e${RESET}"
}

function get_hg_repos_id()
{
  hg id -bint 2> /dev/null
}

function get_git_repos_branch()
{
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/\1/"
}

function repos_info()
{
  HG_REPOS_ID="$(get_hg_repos_id)"
  if [ "${HG_REPOS_ID}" ]
  then
    echo
    echo -en "\e${WHITE}[\e${CYAN}Mercurial\e${WHITE}] Revision ID \e${YELLOW}${HG_REPOS_ID}"
  else
    GIT_REPOS_BRANCH="$(get_git_repos_branch)"
    if [ "${GIT_REPOS_BRANCH}" ]
    then
      echo
      echo -en "\e${WHITE}[\e${CYAN}Git\e${WHITE}] Current Branch \e${YELLOW}${GIT_REPOS_BRANCH}"
    fi
  fi
}

export PS1='$(repos_info)\n\
\e${WHITE}[\e${CYAN}Login\e${WHITE}] \e${GREEN}\u \e${RESET}at \e${WHITE}\h \e${RESET}in \e${BLUE}\w\n\
\[\e${RED}\]\$\[\e${RESET}\] '

alias rm='rm -i'
alias mv='mv -i'
alias ll='ls -l'

[[ -f /etc/profile.d/bash-completion.sh ]] && source /etc/profile.d/bash-completion.sh

