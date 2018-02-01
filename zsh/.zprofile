[ -z "$DISPLAY" ] && [ "$(tty)" = '/dev/tty1' ] && exec xinit -- vt01
