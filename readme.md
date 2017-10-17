My configuration files
======================

Without those a fresh unix system does not help.

- zsh
- emacs

To start emacs as a service, you need to instruct systemd to run it for you: 
$ systemctl --user enable|start|status|stop emacs

This will ensure an emacs server is started when I first login. The emacs will
survive until the machine is rebooted.
