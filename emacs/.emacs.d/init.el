;; keep this config clean
(setq custom-file "~/.emacs.d/custom.el")
(if (file-readable-p custom-file)
    (load custom-file))

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(defun is-slow-system ()
  "Return t if running on a slow system, aka my phone."
  (string= system-name "localhost"))

;;; Store additional config in a 'lisp' subfolder and add it to the load path so
;;; that `require' can find the files.
;; borrowed from https://github.com/Ambrevar/dotfiles
(add-to-list 'load-path (expand-file-name "elisp/" user-emacs-directory))

;; ensure repo cache is up to date (don't to that on slow systems...)
;(unless (is-slow-system)
;    (if (file-exists-p package-user-dir)
;        (package-refresh-contents)))

;; Install use-package
(defun ensure-package-installed (package)
  "Ensure packages are installed"
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))

(ensure-package-installed 'use-package)
(setq use-package-always-ensure t)

(ignore-errors
  (require 'async-bytecomp)
  (async-bytecomp-package-mode 1))

;(add-to-list 'load-path (concat user-emacs-directory "config"))
;; (require 'mu4e-conf)

;; make C-t be C-x
;;(keyboard-translate ?\C-t ?\C-x)
;;(global-set-key (kbd "C-t") ctl-x-map)

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

;; hide startup message
(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t)

(unless (is-slow-system) ;window-system)
  ;; hide toolbar
  (tool-bar-mode -1)
  ;; hide scrollbars
  (scroll-bar-mode -1)
  ;; Cosmetics
  ;;(use-package 'color-theme-modern)
  ;;(load-theme 'blue-mood) ; cobalt
  ;;(load-theme 'blue-sea)
  ;(use-package suscolors-theme)
  ;(load-theme 'suscolors t) ;; 'inkpot is also a great choice
  (use-package color-theme-modern
    :disabled
    :config
    (load-theme 'cobalt t))
  (use-package plan9-theme
    :config
    (load-theme 'plan9))

  ;; powerline
  (use-package powerline
    :demand t
    :disabled)

  (use-package powerline-evil
    :disabled
    :config
    (powerline-evil-vim-color-theme)))

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(setq make-backup-files t
      backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.saves"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t       ; use versioned backups
      ; scroll like vim
      scroll-step 1
      scroll-margin 1
      scroll-conservatively 9999
      dired-listing-switches "-alh" ; use human readable file sizes in dired
      scroll-conservatively 9999
      x-select-enable-clipboard t) ; Copy to clipboard as well

;; tramp backup path (if not set, save in local backup directory)
(setq tramp-backup-directory-alist nil
      tramp-auto-save-directory "~/.tramp-saves")

;(setq starttls-use-gnutls t
;      starttls-gnutls-program "gnutls-cli"
;      starttls-extra-arguments '("--starttls" "--insecure"))

(setq-default standard-indent 2
              indent-tabs-mode nil
              fill-column 78
              c-basic-offset 2
              lua-indent-level 2)

;; use cperl instead of perl-mode
(fset 'perl-mode 'cperl-mode)
(setq cperl-indent-level 2
      cperl-close-paren-offset -2
      cperl-electric-parens nil
      cperl-electric-keywords nil)
(add-hook 'cperl-mode-hook
          (lambda () (setq-local local-abbrev-table nil)))

(show-paren-mode)   ; match parentesis
(global-hl-line-mode) ; highlight line
(line-number-mode)
(column-number-mode)
(display-time-mode)

;; set c++-mode for files without extension
(setq major-mode 'c++-mode)

;; no not delete trailing newlines
(setq-default delete-trailing-newlines nil)
;; delete trailing whitespace automatically on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(remove-hook 'before-save-hook 'delete-trailing-whitespace)

(defun my-other-window ()
  "Ensure evil-mode is in normal state when switching windows"
  (interactive)
  (evil-normal-state)
  (other-window 1))
(global-set-key (kbd "C-x o") 'my-other-window)

(use-package saveplace
  :demand t
  :config
  (progn
    (setq save-place-file "~/.emacs.d/saveplace") ; remember cursor positions of open files
    (setq-default save-place t)
    ))

;;; esc quits, used for evil-mode
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*")
      (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(use-package evil
  :demand t
  :config
  (progn
    ;; enable evil-mode everywhere
    (evil-mode t)
    (setq evil-emacs-state-cursor '("red" box)
          evil-normal-state-cursor '("green" box)
          evil-visual-state-cursor '("orange" box)
          evil-insert-state-cursor '("red" bar)
          evil-replace-state-cursor '("red" bar)
          evil-operator-state-cursor '("red" hollow)
          ;; make '*' and '#' search for the whole symbol and not only the
          ;; word under the cursor, just like in vim
          evil-symbol-word-search t
          evil-shift-width 2
          ;; toggle evil-mode with C-z
          evil-toggle-key "C-z"
          evil-jumper-auto-center t
          evil-jumper-file  "~/.emacs.d/cache/evil-jumps"
          evil-jumper-auto-save-interval 3600
          ;; fix tab in tmux
          evil-want-C-i-jump nil)
    ;; dont cry when I do :W instead of :w
    (evil-ex-define-cmd "W" 'save-buffer)
    ;; dont quit emacs when I :q
    ;(evil-ex-define-cmd "q" 'kill-buffer)
    ;; use swiper for searching
    (define-key evil-normal-state-map "/" 'swiper)
    (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
    (define-key evil-normal-state-map (kbd "*")
      (lambda () (interactive) (swiper (format "\\<%s\\>" (thing-at-point 'symbol)))))
    (define-key evil-normal-state-map (kbd "#")
      (lambda () (interactive) (swiper (format "\\<%s\\>" (thing-at-point 'word)))))
    (define-key evil-normal-state-map [escape] 'keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)
    (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-completion-map [escape]
      'minibuffer-keyboard-quit)
    (define-key minibuffer-local-must-match-map [escape]
      'minibuffer-keyboard-quit)
    (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
    (define-key evil-normal-state-map (kbd "M-.") 'find-tag)
    ))

(use-package evil-leader
  :demand t
  :disabled
  :config
  (progn
    (global-evil-leader-mode)
    (setq evil-leader/in-all-states t) ; leader
    (evil-leader/set-leader ",")
    (evil-leader/set-key
      "e" 'find-file
      "b" 'switch-to-buffer
      "k" 'kill-buffer)))

(use-package magit
  :config
  (setq git-commit-fill-column 72))

(use-package evil-magit
  :config
  (setq evil-magit-state 'motion))

(use-package let-alist)
(use-package evil-org)

(use-package evil-escape
  :if (not (is-slow-system))
  :config
  (progn
    (setq-default evil-escape-key-sequence "kj")
    (setq-default evil-escape-delay 0.2)
    (evil-escape-mode)))

(use-package evil-visualstar)

(use-package evil-indent-textobject)

(use-package evil-surround
  :config
  (global-evil-surround-mode t))

(use-package evil-matchit
  :config
  (global-evil-matchit-mode t))

(use-package evil-search-highlight-persist
  :config
  (global-evil-search-highlight-persist t))

;; disable use evil-mode in ansi-term
;(eval-after-load 'evil-vars
;  '(evil-set-initial-state 'term-mode 'emacs))

;; disable evil-mode in bongo
(eval-after-load 'evil-vars
  '(evil-set-initial-state 'bongo-playlist-mode 'emacs))

(global-set-key (kbd "<f2>") 'compile)
(global-set-key (kbd "<f3>") 'magit-status)
;;(global-set-key (kbd "<f4>") 'org-capture)
;;(global-set-key (kbd "<f2>") 'bongo-pause/resume)
(global-set-key (kbd "C-x C-b") 'buffer-menu)
;; this disables the strange habit of trying to put emacs in background on C-z
(global-unset-key (kbd "C-z"))

(add-hook 'calendar-load-hook
          (lambda ()
            (calendar-set-date-style 'european)))
(setq calendar-week-start-day 1
      calendar-day-name-array ["Sonntag" "Montag" "Dienstag" "Mittwoch"
                               "Donnerstag" "Freitag" "Samstag"]
      calendar-month-name-array ["Januar" "Februar" "März" "April" "Mai"
                                 "Juni" "Juli" "August" "September"
                                 "Oktober" "November" "Dezember"])

(defun my-org-journal-add-entry ()
  "Append to current dairy entry.
If there is no entry for today, a new one will be added"
  (interactive)
  (let* ((datetree-date (or (org-entry-get nil "TIMESTAMP" t)
                            (org-read-date t nil "now")))
         (date (org-date-to-gregorian datetree-date)))
    (unless (file-readable-p (car org-agenda-files))
      (error "file org-agenda-file not set, cannot append journal"))
    (switch-to-buffer (find-file-noselect (car org-agenda-files)))
    (org-datetree-find-date-create date)
    (org-end-of-subtree)
    (when (boundp 'evil-mode)
      (evil-append-line 1))
    ))

(global-set-key (kbd "C-c s") 'my-org-journal-add-entry)

(setq my-org-journal-review-current-date nil)
(defun my-org-journal-review-month ()
  "Go through all journal entries of the current month. If called repeatedly,
  goes through all entries, day by day (if available), and wraps over to the
  first entry"
  (interactive)
  (let* ((datetree-date (org-read-date nil nil "1"))
         (date (org-date-to-gregorian datetree-date))
         (journal (car org-agenda-files)))
    (message "First day of month is: %s" date)
    (unless (file-readable-p journal)
      (error "Journal file not found"))
  ))

;; org-mode - this is why I am here and not in vim
(use-package org
  :config
  (progn
    (cond
     ((or (string= system-name "charon")
          (string= system-name "pluto"))
      (setq org-agenda-files (list "~/org/journal.org")
            org-directory "~/org"))
     ((string= system-name "dione")
      (setq org-agenda-files (list "~/kk/org/journal.org")
            org-directory "~/kk/org/"))
     (t
      (setq org-directory "~/org")
      (message "Warning: Cannot properly setup org as this is an unknown host")))

    (define-key global-map "\C-cl" 'org-store-link)
    (define-key global-map "\C-ca" 'org-agenda)
    ;;(define-key global-map "\C-cc" 'org-capture)
    (setq org-log-done t
          org-log-repeat 'time
          org-default-notes-file (concat org-directory "/notes.org")
          org-capture-templates
          '(("t" "Todo" entry (file+headline
                               (concat org-directory "gtd.org") "Tasks")
             "* TODO %? %^G\nEntered on %U\n")
            ("j" "Journal" entry (file+datetree
                                  (concat org-directory "journal.org"))
             "* %? %^G\nEntered on %U")
            ("n" "Note" entry (file+headline
                               (concat org-directory "gtd.org") "Notes")
             "* %?\nEntered on %U\n")
            ("c" "Add to currently clocked item" item (clock)
             "* %?\n")
            ))
    ))

; do not load this on my phone
(use-package projectile
  :if (not (is-slow-system))
  :config
  (progn
    (projectile-mode t)
    (setq projectile-completion-system 'ivy)))

(use-package terminal-here
  :config
  (progn
    (if (file-exists-p "/usr/bin/konsole")
        (setq terminal-here-terminal-command (list "/usr/bin/konsole")))
    (global-set-key (kbd "C-x t") #'terminal-here-launch)))

;; XXX: candidate for elimination because ansi-term seems to work great
(use-package multi-term
  :config
  ;; allow to send an escape-code in ansi-term
  (add-to-list 'term-bind-key-alist '("C-c C-e" . term-send-escape)))

;; show fill-column in prog-modes and org-mode
(use-package fill-column-indicator
  :if (version<= "25" emacs-version))

(add-hook 'prog-mode-hook (lambda ()
                           (turn-on-auto-fill)
                           (if (boundp 'fci-mode) (fci-mode))
                           (set-fill-column 78)))

;; have variables color coded
(use-package color-identifiers-mode
  :demand t
  :config
  (global-color-identifiers-mode))

;; have delimiters color coded
(use-package rainbow-delimiters
  :demand t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(defun my-save-imenu-jump (item)
  "WIP: Tells evil-jump to save position before jumping via imenu (or counsel-imenu)"
  (evil--jumps-push))

(use-package ivy
  :demand t
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t
          ivy-count-format "(%d/%d) ")
    (advice-add 'imenu :before 'my-save-imenu-jump)
    (define-key ivy-minibuffer-map
      (kbd "<C-return>") 'ivy-immediate-done)))

(setq my-custom-directory "~/")
(defun my-counsel-find-file (&optional initial-input)
  (interactive)
  (progn
    (when (eq major-mode 'exwm-mode)
      (setq default-directory my-custom-directory))
    (counsel-find-file initial-input)))

(use-package counsel
  :demand t
  :config
  (progn
    (global-set-key (kbd "C-c l") 'counsel-locate)
    (global-set-key (kbd "C-c m") 'counsel-imenu)
    (global-set-key (kbd "C-c _") 'counsel-git-grep) ;; Quickly open external
    (global-set-key (kbd "M-x")   'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'my-counsel-find-file)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    ;(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))

(use-package swiper
  :demand t
  :bind ("C-s" . swiper)
  :config
  (progn
    (setq enable-recursive-minibuffers t)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    ;(global-set-key (kbd "<f6>") 'ivy-resume)
    ))

;; lua-mode required for anything LUA
(use-package lua-mode)

(use-package easy-hugo
  :config
  (progn
    (setq easy-hugo-basedir "/home/steffen/web/stepardo.de/"
          easy-hugo-postdir "content/blog"
          easy-hugo-url "https://stepardo.de"
          easy-hugo-sshdomain "rpi"
          easy-hugo-root "/var/stepardo.de"
          easy-hugo-previewtime "300"
          easy-hugo-default-ext ".org")
    ;; (define-key global-map (kbd "C-c C-e") 'easy-hugo)
    ))

(use-package emms
  :if (not (string= system-name "localhost"))
  :disabled
  :config
  (progn
    (require 'emms-setup)
    (emms-standard)
    (emms-default-players)
    (require 'init-evil-emms)))

;; .cfg files are mostly LUA for me
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cfg\\'" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.vbus\\'" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.devs\\'" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.scenario\\'", yaml-mode))

;; save undo tree along with file
(eval-after-load 'undo-tree-mode
  '(setq undo-tree-save-history t))

;; clang format -> load support if clang-format.el is installed
;; (on most systems it is not installed...)
(let ((clang-format-file
      "/usr/share/emacs/site-lisp/clang-format-3.8/clang-format.el"))
  (if (file-readable-p clang-format-file)
      (progn
        (load clang-format-file)
        (global-set-key [C-M-y] 'clang-format-region)
        (message "clang format supported"))
    (progn
      (message "clang format not installed -> not loaded")
      (message "apt-get install -y clang-format-3.8 if you want"))))

(setq-default grep-command (mapconcat 'identity
                                      '("grep"
                                        "-rnH"
                                        "--binary-files=without-match"
                                        "--exclude-dir=html"
                                        "--exclude-dir=doc"
                                        "--exclude-dir=.git"
                                        "--exclude-dir=.svn ")
                                      " "))

;; directory-local stuff
(dir-locals-set-class-variables 'l4re-dir
				'((nil . ((compile-command  . "make -C . O=/home/steffen/kk/git/build/64")
					  (indent-tabs-mode . nil)
					  (tab-width        . 2)
					  (c-default-style  . "gnu")))))

(dir-locals-set-class-variables 'linux-dir
				'((nil . ((indent-tabs-mode . t)
					  (tab-width        . 8)
					  (c-default-style  . "linux")))))

(dir-locals-set-directory-class "/home/steffen/kk/git/repo" 'l4re-dir)
(dir-locals-set-directory-class "/home/steffen/kk/git/repo/l4linux" 'linux-dir)

;; this runs my perl tools on 'compile' in scenario files
(add-hook 'yaml-mode-hook
	  '(lambda ()
	     (if buffer-file-name
		 (set (make-local-variable 'compile-command)
		      (format
                       "perl generate_scenario.pl %s && perl run_scenario.pl %s"
                       (shell-quote-argument buffer-file-name)
                       (shell-quote-argument buffer-file-name))))))

;; enable spell checking for comments

(add-hook 'c-mode-common-hook 'flyspell-prog-mode)

;;(defun steffen-insert-new-journal-entry ()
;;  "Inserts '*** YYYY-MM-DD Tuesday' at beginning of line and a newline"
;;  (interactive)
;;  (beginning-of-line)
;;  (insert (format "*** %s\n" (format-time-string "%Y-%m-%d %A"))))
;;
;;(global-set-key (kbd "<f5>") 'steffen-insert-new-journal-entry)

;; info mode paths
;(add-to-list Info-default-directory-list "/usr/share/info/emacs-24/") ; dpkg -L emacs24-common-non-dfsg

;; use emacs as window manager as well
(use-package exwm
  :if (not (is-slow-system))
  :config
  (progn
    (require 'exwm-systemtray)
    (exwm-systemtray-enable)
    (display-time-mode 1)
    (display-battery-mode 1)
    (setq display-time-string-forms '((format-time-string "%H:%M " now)))
    (require 'exwm-my-config)
    (exwm-my-config)
    (when (boundp 'window-divider-mode)
      (setq window-divider-default-right-width 1)
      (window-divider-mode))))

(use-package eshell
  :config
  (progn
    (require 'em-smart)
    (require 'em-rebind)
    (setq eshell-where-to-jump 'begin
          eshell-review-quick-commands nil
          eshell-smart-space-goes-to-end t)))

;; cool pdf support
(use-package pdf-tools
  :if (not (is-slow-system))
  ;:pin manual ; update manually
  :config
  (pdf-tools-install)
  (require 'init-evil-pdf)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))
                                        ; doesn't work with swiper

;; have a browser
(use-package eww
  :config
  (progn
    (use-package eww-lnum)
    (evil-define-key 'normal eww-mode-map
      "&" 'eww-browse-with-external-browser ;; default in eww-mode
      "q" 'eww-quit ;; different in vimperator (run macro)
      "a" 'eww-add-bookmark
      "yy" 'eww-copy-page-url
      "f" 'eww-lnum-follow
      "F" 'eww-lnum-universal ;; in vimperator open new tab
      "gu" 'eww-up-url
      "gt" 'eww-top-url
      "f" 'eww-lnum-follow
      "F" 'eww-lnum-universal
      "H" 'eww-back-url ;; H in vimperator, because h is :help, but I think lowercase is better for us
      "L" 'eww-forward-url ;; in vimperator, L is used for consistency, but again I think lower case is nicer for us
      "r" 'eww-reload
      )))

(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings "
  (let ((case-fold-search nil))
    (if (and
         (string-match "compilation" (buffer-name buffer))
         (string-match "finished" string)
         (not
          (with-current-buffer buffer
            (goto-char 1)
            (search-forward "warning" nil t))))
        (run-with-timer 1 nil
                        (lambda (buf)
                          (bury-buffer buf)
                          (switch-to-prev-buffer (get-buffer-window buf) 'kill))
                        buffer))))
(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

(setq tags-table-list
      '("~/kk/git/repo/pkg" "~/kk/git/repo/fiasco"))

(defmacro music_cmd (command)
  "Run music command"
  `(let ((spotify "/home/steffen/bin/spotifycmd "))
    (start-process-shell-command "spotify" nil (concat spotify ,command))))

(defun play_pause ()
  "Play/pause spotify"
  (interactive)
  (music_cmd "playpause"))

(defun play_next ()
  "Play next track"
  (interactive)
  (music_cmd "next"))

(global-set-key (kbd "\C-c y") 'play_pause)
(global-set-key (kbd "\C-c u") 'play_next)

;; save sessions
(desktop-save-mode 1)
