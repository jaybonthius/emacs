;;; modal-editing.el --- Modal editing with Meow and Symex -*- lexical-binding: t; -*-
;;; Commentary:
;; Configuration for Meow modal editing and Symex structural editing
;;; Code:

;; SYMEX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package symex-core
  :ensure (:host github
                 :repo "drym-org/symex.el"
                 :files ("symex-core/symex*.el")))

;; There's not that many commands to learn:
;; ijkl directional
;; C-ik (up down) for branch navigation
;; M-ik (up down) for goto highest / lowest
;; H / A for inserting before / after
;; h / a for inserting at beginning / end
;; u / U for undo / undo-redo (TODO: move to meow fallback?)
;; TODO: does emacs have modal transparency?

(use-package symex
  :after symex-core
  :ensure (:host github
                 :repo "drym-org/symex.el"
                 :files ("symex/symex*.el"
                         "symex/doc/*.texi"
                         "symex/doc/figures"))
  :config
  (symex-mode 1)
  (setq symex-orientation 'inverted)
  (global-set-key (kbd "s-;") #'symex-mode-interface)
  (lithium-define-keys symex-editing-mode
    (("h" symex-insert-at-beginning :exit)
     ("i" symex-go-down)
     ("j" symex-go-backward)
     ("k" symex-go-up)
     ("l" symex-go-forward)
     ("C-i" symex-descend-branch)
     ("C-k" symex-climb-branch)
     ("M-i" symex-goto-lowest)
     ("M-k" symex-goto-highest)
     ;; I prefer arrow keys tbh
     ("gk" symex-next-visual-line)
     ("gj" backward-char)
     ("gi" symex-previous-visual-line)
     ("gl" forward-char)
     ("H" symex-insert-before :exit)
     ("A" symex-append-after :exit)
     ("a" symex-append-at-end :exit)
     ;; ("u" undo)
     ;; ("U" undo-redo)
     )))

(use-package symex-ide
  :after symex
  :ensure (:host github
                 :repo "drym-org/symex.el"
                 :files ("symex-ide/symex*.el"))
  :config
  (symex-ide-mode 1))

;; MEOW ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package meow
  :ensure (meow :host github :repo "meow-edit/meow")
  :config
  ;; Define the QWERTY keybinding setup
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

    ;; Motion state keys (for special modes like dired)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))

    ;; Leader key bindings (accessed via SPC)
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state
     ;; TODO: fix these
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))

    ;; Normal state keys (main modal editing keys)
    (meow-normal-define-key
     ;; Expansion
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     ;; Movement and editing
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-insert)
     '("H" . meow-open-above)
     '("j" . meow-left)
     '("J" . meow-left-expand)
     '("k" . meow-next)
     '("K" . meow-next-expand)
     '("i" . meow-prev)
     '("I" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)))

  ;; Run the setup function
  (meow-setup)

  ;; Enable Meow globally
  (meow-global-mode 1))

(global-eldoc-mode -1)

(provide 'modal-editing)
;;; modal-editing.el ends here
