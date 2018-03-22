alias pp='emacsclient -e "(bongo-pause/resume)"'
alias pl='emacsclient -e "(select-window (new-frame))(switch-to-buffer \"*Bongo Playlist*\")"'
alias vim='emacsclient -nw'
alias vi='/usr/bin/vim'
alias nvi='/usr/bin/vim'
# make emacs use useful colors...
export TERM=xterm-256color

function! find-file()
{
  emacsclient -e "(find-file \"$1\")";
}

function! eman()
{
  emacsclient -e "(man \"$1\")";
}

if [ -n "$INSIDE_EMACS" ]; then
  chpwd() { print -P "\033AnSiTc %d" }
  print -P "\033AnSiTu %n"
  print -P "\033AnSiTc %d"
  export TERM=eterm-color
fi
