(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; after macro
(if (fboundp 'with-eval-after-load)
    (defmacro after (feature &rest body)
      "After FEATURE is loaded, evaluate BODY."
      (declare (indent defun))
      `(with-eval-after-load ,feature ,@body))
  (defmacro after (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

(defun require-package (package)
  (setq-default highlight-tabs t)
  "Install given PACKAGE."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

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

;; remember cursor positions of open files
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require-package 'saveplace)

;; delete trailing whitespace automatically on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require-package 'evil-leader)
(require-package 'evil-org)
(require-package 'evil)
(global-evil-leader-mode)
(evil-leader/set-leader ",") (evil-leader/set-key
  "e" 'find-file
  "b" 'switch-to-buffer
  "k" 'kill-buffer)
(evil-mode t)
;; make '*' and '#' search for the whole
;; symbol and not only the word under the cursor, just like in vim
(after 'evil
  (setq evil-symbol-word-search t))

(require-package 'evil-visualstar)
(require-package 'evil-indent-textobject)
(require-package 'evil-surround)
(require-package 'evil-matchit)
(require-package 'evil-search-highlight-persist)

(global-evil-matchit-mode t)
(global-evil-surround-mode t)
(global-evil-search-highlight-persist t)
(global-evil-leader-mode t)
(after 'evil
  (setq evil-leader/in-all-states t)) ; leader

(require-package 'swiper)

(define-key evil-normal-state-map (kbd "*")
  (lambda () (interactive) (swiper (format "\\<%s\\>" (thing-at-point 'symbol)))))
(define-key evil-normal-state-map (kbd "#")
  (lambda () (interactive) (swiper (format "\\<%s\\>" (thing-at-point 'word)))))

(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

;; dont use evil in ansi-term
(eval-after-load 'evil-vars
  '(evil-set-initial-state 'term-mode 'emacs))

(eval-after-load 'evil-vars
  '(evil-set-initial-state 'bongo-playlist-mode 'emacs))

(setq  evil-jumper-auto-center t
       evil-jumper-file  "~/.emacs.d/cache/evil-jumps"
       evil-jumper-auto-save-interval 3600)

;; dont cry when I do :W instead of :w
(evil-ex-define-cmd "W" 'save-buffer)
;; dont quit emacs when I :q
(evil-ex-define-cmd "q" 'kill-buffer)

;; optional: this is the evil state that evil-magit will use
;; (setq evil-magit-state 'normal)
;; optional: disable additional bindings for yanking text
;; (setq evil-magit-use-y-for-yank nil)
(require-package 'evil-magit)
(global-set-key (kbd "<f2>") 'compile)
(global-set-key (kbd "<f3>") 'magit-status)
(global-set-key (kbd "<f4>") 'org-capture)
;;(global-set-key (kbd "<f2>") 'bongo-pause/resume)

;; org-mode - this is why I am here and not in vim
(require-package 'org)
(setq org-agenda-files (list "~/kk/org/worklog.org"
			     "~/kk/org/genua.org"
                             "~/kk/org/journal.org"
                             "~/kk/org/offsite_2016.org"
                             "~/kk/org/gtd.org"))

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-log-repeat 'time)

(setq org-directory "~/kk/org/")
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

;; Cosmetics
(require-package 'color-theme-modern)
(load-theme 'blue-sea)
;;(load-theme 'suscolors t) ;; 'inkpot is also a great choice
(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))

(display-time-mode t)

;; Clean powerline
(require-package 'diminish)
(diminish 'visual-line-mode)
(after 'autopair (diminish 'autopair-mode))
(after 'undo-tree (diminish 'undo-tree-mode))
(after 'auto-complete (diminish 'auto-complete-mode))
(after 'projectile (diminish 'projectile-mode))
(after 'yasnippet (diminish 'yas-minor-mode))
(after 'guide-key (diminish 'guide-key-mode))
(after 'eldoc (diminish 'eldoc-mode))
(after 'smartparens (diminish 'smartparens-mode))
(after 'company (diminish 'company-mode))
(after 'elisp-slime-nav (diminish 'elisp-slime-nav-mode))
(after 'git-gutter+ (diminish 'git-gutter+-mode))
(after 'magit (diminish 'magit-auto-revert-mode))
(after 'hs-minor-mode (diminish 'hs-minor-mode))
(after 'color-identifiers-mode (diminish 'color-identifiers-mode))

;; show fill-column in prog-modes and org-mode
(require-package 'fill-column-indicator)

(add-hook 'prog-mode-hook (lambda ()
                           (turn-on-auto-fill)
                           (fci-mode)
                           (set-fill-column 78)))

(require-package 'ag)

;; have variables color coded
(require-package 'color-identifiers-mode)
(global-color-identifiers-mode)

;; have delimiters color coded
(require-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; completion
(require-package 'ido)
(ido-mode 1)
(ido-everywhere 1)
(require-package 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; lua-mode required for anything LUA
(require-package 'lua-mode)
;; .cfg files are mostly LUA for me
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cfg\\'" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.vbus\\'" . lua-mode))
(add-to-list 'auto-mode-alist '("\\.devs\\'" . lua-mode))

;; save undo tree along with file
(eval-after-load 'undo-tree-mode
  '(setq undo-tree-save-history t))

;; make C-t be C-x
;;(keyboard-translate ?\C-t ?\C-x)
;;(global-set-key (kbd "C-t") ctl-x-map)

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
  '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
  '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

; prepare for emacsclient
(server-start)

;; end of file
