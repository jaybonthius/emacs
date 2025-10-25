;;; git.el --- Git integration and version control -*- lexical-binding: t; -*-
;;; Commentary:
;; Git and version control configurations including Magit and diff highlighting
;;; Code:

;; Ensure transient is upgraded before magit loads
(use-package transient
  :ensure t)

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode))

(use-package fullframe
  :commands (fullframe))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  (fullframe magit-status magit-mode-quit-window))

(use-package difftastic
  :ensure t)

(provide 'git)
;;; git.el ends here
