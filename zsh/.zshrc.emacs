#alias pp='emacsclient -e "(bongo-pause/resume)"'
#alias pl='emacsclient -e "(select-window (new-frame))(switch-to-buffer \"*Bongo Playlist*\")"'
alias evim='emacsclient'
alias vi='/usr/bin/vim'
alias nvi='/usr/bin/vim'
# make emacs use useful colors...
export TERM=xterm-256color

# if we have $HOME/opt/bin, use it in path
if [ -d "$HOME/opt/bin" ];
then
    export PATH="$HOME/opt/bin:$PATH"
fi

function! find-file()
{
  emacsclient -u -e "(find-file \"$1\")";
}

function! eman()
{
  emacsclient -u -e "(man \"$1\")";
}

function! cvim() {
  file="$(echo "$1" | cut -d ':' -f 1)"
  line="$(echo "$1" | cut -d ':' -f 2)"
  emacsclient -u "+$line" "$file"
}

if [ -n "$INSIDE_EMACS" ]; then
  chpwd() { print -P "\033AnSiTc %d" }
  print -P "\033AnSiTu %n"
  print -P "\033AnSiTc %d"
  export TERM=eterm-color
fi

autoload -U add-zsh-hook
add-zsh-hook chpwd emacs_chdir

function emacs_chdir() {
  case $PWD in
      /home/steffen/kk/git/build/*)
          echo "In builddir, not telling emacs." ;;
      *)
          emacsclient -q -n -e "(setq my-custom-directory \"$PWD/\")" 2>&1 > /dev/null;;
  esac
}
