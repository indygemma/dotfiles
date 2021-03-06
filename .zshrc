# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
export ZSH_THEME="random"
# themes I like:
#
#   nanotech
#   jonathan
#   muse
#   darkblood
#

# Set to this to use case-sensitive completion
# export CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# export DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# export DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# export DISABLE_AUTO_TITLE="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git pip git-flow)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
export PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:~/.cabal/bin:/var/lib/gems/1.8/bin

# add some CD aliases
ng=~/projects/python/newsgrape-ng
blog=~/projects/websites/indygemma.com

# bind pushd to "page up"
function directory_up {
  pushd -1
  zle reset-prompt
}
zle -N directory-up directory_up
bindkey "\e[5~" directory-up
bindkey ^W forward-word
bindkey ^B backward-word
bindkey ^D delete-word
alias v="vim --remote-tab-silent"

# project shortcuts
alias project_ng="cd ~/projects/python/newsgrape-ng && . ve/bin/activate"
alias project_ngcode="project_ng && cd src/newsgrape"
alias project_medusa="cd ~/projects/python/medusa && . ve/bin/activate"

export PATH=~/Library/Haskell/bin:$PATH
export PATH=/usr/texbin:$PATH

alias tmux="TERM=screen-256color-bce tmux"
alias mosh="LC_CTYPE="en_US.UTF-8" mosh"

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"
