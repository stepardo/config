;;; -*- mode: emacs-lisp; -*-

;; this is nil if exwm is loaded by emacs -l

(defun exwm-my-config ()
  ;(when (featurep 'exwm)

    (require 'exwm-randr)
    (require 'exwm-systemtray)
    (require 's)

    (setq exwm-workspace-number 2
	  exwm-workspace-show-all-buffers t
	  exwm-layout-show-all-buffers t
	  exwm-randr-workspace-output-plist '(0 "eDP-1" 1 "DP-2-1" ))

    (defun exwm-update-buffer-name ()
      (exwm-workspace-rename-buffer (format "%% %.20s — %.20s" exwm-title exwm-class-name)))
    (add-hook 'exwm-update-class-hook #'exwm-update-buffer-name)
    (add-hook 'exwm-update-title-hook #'exwm-update-buffer-name)

    (macrolet ((launch-program (command)
			       `(lambda ()
				  (interactive)
				  (start-process-shell-command ,command nil ,command))))

      (exwm-input-set-key (kbd "s-.") #'exwm-reset)
      (exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch)

      (dotimes (i 10)
	(exwm-input-set-key (kbd (format "s-%d" i))
			    `(lambda ()
			       (interactive)
			       (exwm-workspace-switch ,i))))

      (exwm-input-set-key (kbd "s-j")
			  (lambda ()
			    (interactive)
			    (let ((window (next-window (selected-window) nil nil)))
			      (if window (select-window window)))))
      (exwm-input-set-key (kbd "s-k")
			  (lambda ()
			    (interactive)
			    (let ((window (previous-window (selected-window) nil nil)))
			      (if window (select-window window)))))
      (exwm-input-set-key (kbd "s-h") #'previous-multiframe-window)
      (exwm-input-set-key (kbd "s-l") #'next-multiframe-window)

      (exwm-input-set-key (kbd "s-o")
			  (lambda (command)
			    (interactive (list (read-shell-command "Run: ")))
			    (start-process-shell-command command nil command)))
      (exwm-input-set-key (kbd "s-<return>")
                          (launch-program "konsole"))
      ;(exwm-input-set-key (kbd "s-p")
      ;                    (lambda ()
      ;                      (interactive)
      ;                      (let ((command "bash /home/steffen/bin/dmenu_run"))
      ;                        (start-process-shell-command command nil command)
      ;                        (message (concat "tried to launch " command)))))

      (exwm-input-set-key [XF86AudioRaiseVolume]
			  (launch-program (s-join " "
						  '("pactl -- set-sink-volume"
						    "alsa_output.pci-0000_00_1f.3.analog-stereo +4%"))))
      (exwm-input-set-key [XF86AudioLowerVolume]
			  (launch-program (s-join " "
						  '("pactl -- set-sink-volume"
						    "alsa_output.pci-0000_00_1f.3.analog-stereo -4%"))))
      (exwm-input-set-key [XF86AudioMute]
			  (launch-program (s-join " "
						  '("pactl -- set-sink-mute"
						    "alsa_output.pci-0000_00_1f.3.analog-stereo toggle"))))
      (exwm-input-set-key [XF86AudioMicMute]
			  (launch-program (s-join " "
						  '("pactl -- set-source-mute"
						    "alsa_input.pci-0000_00_1f.3.analog-stereo toggle"))))

      (dolist (key (list (kbd "C-M-l") [XF86ScreenSaver] [XF86Sleep]) nil)
	(exwm-input-set-key key (launch-program "systemctl start physlock.service"))))


    (exwm-enable)
    (exwm-randr-enable)
    (exwm-systemtray-enable)

    (tool-bar-mode -1)
    (menu-bar-mode -1)
    (scroll-bar-mode -1)
    (fringe-mode 1)

    (if (fboundp #'ido-mode) (ido-mode -1)))

(provide 'exwm-my-config)
;;(add-hook 'emacs-startup-hook #'exwm-my-config)
