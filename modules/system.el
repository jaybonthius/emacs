;;; system.el --- System integration and platform-specific settings -*- lexical-binding: t; -*-
;;; Commentary:
;; System-level configurations including PATH, macOS settings, and platform-specific tweaks
;;; Code:

;; Make Emacs inherit PATH from shell on macOS
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(add-to-list 'exec-path "/Applications/Racket v8.17/bin")
(add-to-list 'exec-path "/opt/homebrew/bin/")

(when (eq system-type 'darwin)
  (setq mac-option-key-is-meta nil)
  (setq mac-command-key-is-meta t)
  (setq mac-command-modifier 'control)
  (setq mac-option-modifier nil))

(when (eq system-type 'darwin)
  (setq
   ns-command-modifier 'control
   ns-option-modifier 'meta
   ns-control-modifier 'super
   ns-function-modifier 'hyper))

(unless (display-graphic-p)
  (xterm-mouse-mode 1))

;; Configure Emacs to ask for confirmation before exiting
(setq confirm-kill-emacs 'y-or-n-p)

;; Configure the built-in Emacs server to start after initialization,
;; allowing the use of the emacsclient command to open files in the
;; current session.
(use-package server
  :ensure nil
  :commands server-start
  :hook
  (after-init . server-start))

(provide 'system)
;;; system.el ends here
