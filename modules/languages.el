;;; languages.el --- Language-specific configurations -*- lexical-binding: t; -*-
;;; Commentary:
;; Language-specific modes and configurations including Racket, Markdown, Org, etc.
;;; Code:

;; RACKET ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package racket-mode
  :ensure t
  :mode (("\\.rkt\\'" . racket-mode))
  :after flycheck
  :bind (:map racket-mode-map
              ("C-c C-d" . racket-xp-describe)
              ("C-c C-r" . racket-xp-rename)
              ("C-c r t" . racket-tidy-requires)
              ("C-c r i" . racket-add-require-for-identifier)
              ("C-c ."   . xref-find-definitions)
              ("C-c ,"   . xref-go-back))
  :config
  (with-eval-after-load 'racket-hash-lang
    (define-key racket-hash-lang-mode-map (kbd "C-c C-d") 'racket-xp-describe)
    (define-key racket-hash-lang-mode-map (kbd "C-c C-r") 'racket-xp-rename)
    (define-key racket-hash-lang-mode-map (kbd "C-c .") 'xref-find-definitions)
    (define-key racket-hash-lang-mode-map (kbd "C-c ,") 'xref-go-back))

  (flycheck-define-checker racket-review
    "check racket source code using racket-review"
    :command ("raco" "review" source)
    :error-patterns
    ((error line-start (file-name) ":" line ":" column ":error:" (message) line-end)
     (warning line-start (file-name) ":" line ":" column ":warning:" (message) line-end))
    :modes racket-mode)

  (add-to-list 'flycheck-checkers 'racket-review)

  :hook ((racket-mode . racket-xp-mode)
         (racket-hash-lang-mode . racket-xp-mode)))

(use-package scribble-mode
  :mode "\\.scrbl\\'")

(use-package pollen-mode
  :mode "\\.p[mp]?\\'")

;; ORG MODE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode is a major mode designed for organizing notes, planning, task
;; management, and authoring documents using plain text with a simple and
;; expressive markup syntax.
(use-package org
  :ensure t
  :commands (org-mode org-version)
  :mode
  ("\\.org\\'" . org-mode)
  :custom
  (org-hide-leading-stars t)
  (org-startup-indented t)
  (org-adapt-indentation nil)
  (org-edit-src-content-indentation 0)
  (org-startup-truncated t))

;; MARKDOWN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The markdown-mode package provides a major mode for Emacs for syntax
;; highlighting, editing commands, and preview support for Markdown documents.
;; It supports core Markdown syntax as well as extensions like GitHub Flavored
;; Markdown (GFM).
(use-package markdown-mode
  :commands (gfm-mode
             gfm-view-mode
             markdown-mode
             markdown-view-mode)
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :bind
  (:map markdown-mode-map
        ("C-c C-e" . markdown-do)))

;; Automatically generate a table of contents when editing Markdown files
(use-package markdown-toc
  :ensure t
  :commands (markdown-toc-generate-toc
             markdown-toc-generate-or-refresh-toc
             markdown-toc-delete-toc
             markdown-toc--toc-already-present-p)
  :custom
  (markdown-toc-header-toc-title "**Table of Contents**"))

;; TREE-SITTER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree-sitter in Emacs is an incremental parsing system introduced in Emacs 29
;; that provides precise, high-performance syntax highlighting.
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; SPELL CHECKING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The flyspell package is a built-in Emacs minor mode that provides
;; on-the-fly spell checking.
(use-package ispell
  :ensure nil
  :commands (ispell-word ispell-region ispell-buffer)
  :custom
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--camel-case")))

(use-package flyspell
  :ensure nil
  :commands (flyspell-mode flyspell-prog-mode)
  :hook
  ((text-mode . flyspell-mode)
   (prog-mode . flyspell-prog-mode)))

(provide 'languages)
;;; languages.el ends here
