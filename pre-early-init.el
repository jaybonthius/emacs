;;; pre-early-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

(setq debug-on-error t)

(setq minimal-emacs-ui-features '(context-menu tool-bar menu-bar dialogs tooltips))

;;; Reducing clutter in ~/.emacs.d by redirecting files to ~/.emacs.d/var/
;; NOTE: This must be placed in 'pre-early-init.el'
(setq user-emacs-directory (expand-file-name "var/" minimal-emacs-user-directory))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

;; By default, minimal-emacs-package-initialize-and-refresh is set to t, which
;; makes minimal-emacs.d call the built-in package manager. Since Elpaca will
;; replace the package manager, there is no need to call it.
(setq minimal-emacs-package-initialize-and-refresh nil)

(defun display-startup-time ()
  "Display the startup time and number of garbage collections."
  (message "Emacs init loaded in %.2f seconds (Full emacs-startup: %.2fs) with %d garbage collections."
           (float-time (time-subtract after-init-time before-init-time))
           (time-to-seconds (time-since before-init-time))
           gcs-done))

(add-hook 'emacs-startup-hook #'display-startup-time 100)