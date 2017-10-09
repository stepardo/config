(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; ensure repo cache is up1date
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; Install use-package
(defun ensure-package-installed (package)
  "Ensure packages are installed"
  (if (package-installed-p package)
      nil
    (progn
      (package-refresh-contents)
      (package-install package))))

(ensure-package-installed 'use-package)

(require 'async-bytecomp)

;; hide startup message
(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t)

;; hide toolbar
(tool-bar-mode -1)
;; hide scrollbars
(scroll-bar-mode -1)

;; match parentesis
(show-paren-mode t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(setq standard-indent 2)
(setq-default indent-tabs-mode nil)

(setq make-backup-files t)
(setq version-control t)
(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups

(line-number-mode 1)
(column-number-mode 1)

;; scroll like vim
(setq scroll-step 1)
(setq scroll-margin 1)
(setq scroll-conservatively 9999)

(setq-default fill-column 78)
(setq-default c-basic-offset 2)
(setq-default lua-indent-level 2)

(use-package saveplace
  :demand t
  :ensure t
  :config
  (progn
    (setq save-place-file "~/.emacs.d/saveplace") ;; remember cursor positions of open files
    (setq-default save-place t)
    ))

;; delete trailing whitespace automatically on save
;(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; set c++-mode for files without extension
(setq default-major-mode 'c++-mode)

;;; esc quits, used for evil-mode
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on
                                        "*Completions*"))
    (abort-recursive-edit)))

(use-package evil
  :ensure t
  :demand t
  :config
  (progn
    ;; make '*' and '#' search for the whole
    ;; symbol and not only the word under the cursor, just like in vim
    (setq evil-symbol-word-search t)
    (setq evil-shift-width 2)
    ;; enable evil-mode everywhere
    (evil-mode t)
    (setq evil-emacs-state-cursor '("red" box))
    (setq evil-normal-state-cursor '("green" box))
    (setq evil-visual-state-cursor '("orange" box))
    (setq evil-insert-state-cursor '("red" bar))
    (setq evil-replace-state-cursor '("red" bar))
    (setq evil-operator-state-cursor '("red" hollow))
    ;; toggle evil-mode with C-z
    (setq evil-toggle-key "C-z")
    ;; dont cry when I do :W instead of :w
    (evil-ex-define-cmd "W" 'save-buffer)
    ;; dont quit emacs when I :q
    ;(evil-ex-define-cmd "q" 'kill-buffer)
    (define-key evil-normal-state-map "/" 'swiper)
    (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)
    (setq  evil-jumper-auto-center t
           evil-jumper-file  "~/.emacs.d/cache/evil-jumps"
           evil-jumper-auto-save-interval 3600)
    (setq evil-want-C-i-jump nil) ;; this fixes TAB in tmux
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
    ))

(use-package evil-leader
  :ensure t
  :demand t
  :config
  (progn
    (global-evil-leader-mode)
    (setq evil-leader/in-all-states t) ; leader
    (evil-leader/set-leader ",")
    (evil-leader/set-key
      "e" 'find-file
      "b" 'switch-to-buffer
      "k" 'kill-buffer)))

(use-package evil-magit
  :ensure t
  :demand t
  :config
  (setq evil-magit-state 'motion))

(use-package evil-org
  :ensure t
  :demand t)

(use-package evil-escape-mode
  :ensure t
  :demand t
  :config
  (progn
    (setq-default evil-escape-key-sequence "kj")
    (setq-default evil-escape-delay 0.2)
    (evil-escape-mode)))

;;(modify-syntax-entry ?_ "w")

(use-package evil-visualstar
  :demand t
  :ensure t)

(use-package evil-indent-textobject
  :demand t
  :ensure t)

(use-package evil-surround
  :demand t
  :ensure t
  :config
  (global-evil-surround-mode t))

(use-package evil-matchit
  :demand t
  :ensure t
  :config
  (global-evil-matchit-mode t))

(use-package evil-search-highlight-persist
  :demand t
  :ensure t
  :config
  (global-evil-search-highlight-persist t))

;; dont use evil in ansi-term
;; disabled- can temporarily disable it with C-z
;(eval-after-load 'evil-vars
;  '(evil-set-initial-state 'term-mode 'emacs))

(eval-after-load 'evil-vars
  '(evil-set-initial-state 'bongo-playlist-mode 'emacs))

(global-set-key (kbd "<f2>") 'compile)
(global-set-key (kbd "<f3>") 'magit-status)
(global-set-key (kbd "<f4>") 'org-capture)
(global-set-key (kbd "C-x C-b") 'buffer-menu)
;; this disables the strange habit of trying to put emacs in background on C-z
(global-unset-key (kbd "C-z"))

;;(global-set-key (kbd "<f2>") 'bongo-pause/resume)

;; org-mode - this is why I am here and not in vim
(use-package org
  :demand t
  :ensure t
  :config
  (progn
    (cond
     ((string= system-name "charon")
      (progn
        (setq org-agenda-files (list "~/org/journal.org"))
        (setq org-directory "~/org")))
     ((string= system-name "dione")
      (progn
        (setq org-agenda-files (list "~/kk/org/worklog.org"
                                     "~/kk/org/journal.org"
                                     "~/kk/org/gtd.org"))

        (setq org-directory "~/kk/org/")))
     ((t)
      (message
       "Warning: Cannot properly setup org as this is an unknown host")))

    (define-key global-map "\C-cl" 'org-store-link)
    (define-key global-map "\C-ca" 'org-agenda)
    (setq org-log-done t)
    (setq org-log-repeat 'time)

    (setq org-default-notes-file (concat org-directory "/notes.org"))
    ;;(define-key global-map "\C-cc" 'org-capture)

    (setq org-capture-templates
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

;; Cosmetics
;;(use-package 'color-theme-modern)
;(load-theme 'blue-mood) ; cobalt
;;(load-theme 'blue-sea)
(load-theme 'suscolors t) ;; 'inkpot is also a great choice
(display-time-mode t)

;; powerline
(use-package powerline
  :ensure t
  :demand t
  :disabled)

(use-package powerline-evil
  :ensure t
  :demand t
  :disabled
  :config
  (powerline-evil-vim-color-theme))

(use-package projectile
  :ensure t
  :demand t
  :config
  (progn
    (projectile-global-mode t)
    (setq projectile-completion-system 'ivy)))

(use-package terminal-here
  :ensure t
  :demand t
  :config
  (progn
    (setq terminal-here-terminal-command (list "/usr/bin/konsole"))
    (global-set-key (kbd "C-x t") #'terminal-here-launch)))

;; XXX: candidate for elimination because ansi-term seems to work great
(use-package multi-term
  :ensure t
  :demand t
  :config
  (progn
    ;; allow to send an escape-code in ansi-term
    (add-to-list 'term-bind-key-alist '("C-c C-e" . term-send-escape))
    ))

;; show fill-column in prog-modes and org-mode
(use-package fill-column-indicator
  :demand t)

(add-hook 'prog-mode-hook (lambda ()
                           (turn-on-auto-fill)
                           (fci-mode)
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

(use-package ivy
  :ensure t
  :demand t
  :config (progn
            (ivy-mode 1)
            (setq ivy-use-virtual-buffers t)
            (setq ivy-count-format "(%d/%d) ")
            (define-key ivy-minibuffer-map
              (kbd "<C-return>") 'ivy-immediate-done)))

(use-package counsel
  :demand t
  :ensure t
  :config
  (progn
    (global-set-key (kbd "C-c l") 'counsel-locate)
    (global-set-key (kbd "C-c m") 'counsel-imenu)
    (global-set-key (kbd "C-c _") 'counsel-git-grep) ;; Quickly open external
    (global-set-key (kbd "M-x")   'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))

(use-package swiper
  :demand t
  :ensure t
  :bind ("C-s" . swiper)
  :config
  (progn
    (setq enable-recursive-minibuffers t)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    ))

;; lua-mode required for anything LUA
(use-package lua-mode
  :demand t)

(use-package easy-hugo
  :demand t
  :config
  (progn
    (setq easy-hugo-basedir "/home/steffen/web/stepardo.de/")
    (setq easy-hugo-postdir "content/blog")
    (setq easy-hugo-url "https://stepardo.de")
    (setq easy-hugo-sshdomain "rpi")
    (setq easy-hugo-root "/var/stepardo.de")
    (setq easy-hugo-previewtime "300")
    (define-key global-map (kbd "C-c C-e") 'easy-hugo)
    (setq easy-hugo-default-ext ".org")
    ))

;; .cfg files are mostly LUA for me
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cfg\\'" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.vbus\\'" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.devs\\'" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.scenario\\'", yaml-mode))

;; save undo tree along with file
(eval-after-load 'undo-tree-mode
  '(setq undo-tree-save-history t))

;; make C-t be C-x
;;(keyboard-translate ?\C-t ?\C-x)
;;(global-set-key (kbd "C-t") ctl-x-map)

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

(load "/usr/share/emacs/site-lisp/clang-format-3.8/clang-format.el")
(global-set-key [C-M-y] 'clang-format-region)

(add-to-list 'load-path (concat user-emacs-directory "config"))

;; (require 'mu4e-conf)

(defvar grep-opts "-rnH --binary-files=without-match --exclude-dir=html --exclude-dir=doc --exclude-dir=.git --exclude-dir=.svn")
(setq-default grep-command (concat "grep " grep-opts))

;; directory-local stuff
(dir-locals-set-class-variables 'l4re-dir
				'((nil . ((compile-command  . "make -C . O=/home/steffen/kk/git/build/64")
					  (indent-tabs-mode . nil)
					  (tab-width        . 2)
					  (c-default-style  . "gnu")))))

(dir-locals-set-class-variables 'linux-dir
				'((nil . ((indent-tabs-mode . t)
					  (tab-width        . 4)
					  (c-default-style  . "linux")))))

(dir-locals-set-directory-class "/home/steffen/kk/git/repo" 'l4re-dir)
(dir-locals-set-directory-class "/home/steffen/kk/git/repo/l4linux" 'linux-dir)

;; this runs my perl tools on 'compile' in scenario files
(add-hook 'yaml-mode-hook
	  '(lambda ()
	     (if buffer-file-name
		 (set (make-local-variable 'compile-command)
		      (format "perl generate_scenario.pl %s && perl run_scenario.pl %s" (shell-quote-argument buffer-file-name) (shell-quote-argument buffer-file-name))))))

;; enable spell checking for comments

(add-hook 'c-mode-common-hook 'flyspell-prog-mode)

(defun steffen-insert-new-journal-entry ()
  "Inserts '*** YYYY-MM-DD Tuesday' at beginning of line and a newline"
  (interactive)
  (beginning-of-line)
  (insert (format "*** %s\n" (format-time-string "%Y-%m-%d %A"))))

(global-set-key (kbd "<f5>") 'steffen-insert-new-journal-entry)

;; info mode paths
;(add-to-list Info-default-directory-list "/usr/share/info/emacs-24/") ; dpkg -L emacs24-common-non-dfsg

;; save sessions
(desktop-save-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups/"))))
 '(custom-safe-themes
   (quote
    ("72c530c9c8f3561b5ab3bf5cda948cd917de23f48d9825b7a781fe1c0d737f2f" "77515a438dc348e9d32310c070bfdddc5605efc83671a159b223e89044e4c4f1" "392f19e7788de27faf128a6f56325123c47205f477da227baf6a6a918f73b5dc" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" "d9e811d5a12dec79289c5bacaecd8ae393d168e9a92a659542c2a9bab6102041" "7bd626fcc9fbfb44186cf3f08b8055d5a15e748d5338e47f9391d459586e20db" "a455366c5cdacebd8adaa99d50e37430b0170326e7640a688e9d9ad406e2edfd" "a11043406c7c4233bfd66498e83600f4109c83420714a2bd0cd131f81cbbacea" "fb09acc5f09e521581487697c75b71414830b1b0a2405c16a9ece41b2ae64222" "1a2cde373eff9ffd5679957c7ecfc6249d353e1ee446d104459e73e924fe0d8a" "5c83b15581cb7274085ba9e486933062652091b389f4080e94e4e9661eaab1aa" "db510eb70cf96e3dbd48f5d24de12b03db30674ea0853f06074d4ccf7403d7d3" "ed92c27d2d086496b232617213a4e4a28110bdc0730a9457edf74f81b782c5cf" "5eb4b22e97ddb2db9ecce7d983fa45eb8367447f151c7e1b033af27820f43760" "05d009b7979e3887c917ef6796978d1c3bbe617e6aa791db38f05be713da0ba0" default)))
 '(evil-toggle-key "C-z")
 '(safe-local-variable-values
   (quote
    ((c-default-style . "linux")
     (c-default-style . "gnu")
     (compile-command . "make -C . O=/home/steffen/kk/git/build/64")
     (grep-command . "grep -rnH --binary-files=without-match --exclude-dir=html --exclude-dir=doc --exclude-dir=.git --exclude-dir=.svn")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
