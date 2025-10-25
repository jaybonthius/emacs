;;; lsp.el --- Language Server Protocol configuration -*- lexical-binding: t; -*-
;;; Commentary:
;; LSP configuration using Eglot
;;; Code:

;; Set up the Language Server Protocol (LSP) servers using Eglot.
(use-package eglot
  :ensure nil
  :commands (eglot-ensure
             eglot-rename
             eglot-format-buffer))

;; Python LSP
;; Configure Eglot to enable or disable certain options for the pylsp server
;; in Python development. (Note that a third-party tool,
;; https://github.com/python-lsp/python-lsp-server, must be installed),
(add-hook 'python-mode-hook #'eglot-ensure)
(add-hook 'python-ts-mode-hook #'eglot-ensure)
(setq-default eglot-workspace-configuration
              `(:pylsp (:plugins
                        (;; Fix imports and syntax using `eglot-format-buffer`
                         :isort (:enabled t)
                         :autopep8 (:enabled t)

                         ;; Syntax checkers (works with Flymake)
                         :pylint (:enabled t)
                         :pycodestyle (:enabled t)
                         :flake8 (:enabled t)
                         :pyflakes (:enabled t)
                         :pydocstyle (:enabled t)
                         :mccabe (:enabled t)

                         :yapf (:enabled :json-false)
                         :rope_autoimport (:enabled :json-false)))))

;; Linting
(use-package flycheck
  :ensure t
  :commands flycheck-mode)

(provide 'lsp)
;;; lsp.el ends here
