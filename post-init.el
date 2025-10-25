;;; post-init.el --- Main configuration entry point -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;; This file loads all configuration modules from the modules/ directory.
;; Each module focuses on a specific aspect of the Emacs configuration.
;;; Code:

;; Add modules directory to load path
(add-to-list 'load-path (expand-file-name "modules" (file-name-directory (or load-file-name buffer-file-name))))

;; Load all modules
(require 'system)         ; System integration and platform-specific settings
(require 'ui)             ; UI, themes, and appearance
(require 'git)            ; Git and version control
(require 'modal-editing)  ; Meow and Symex modal editing
(require 'completion)     ; Completion frameworks (Corfu, Vertico, Consult, etc.)
(require 'editor)         ; Editor enhancements (undo, formatting, etc.)
(require 'lsp)            ; Language Server Protocol configuration
(require 'languages)      ; Language-specific modes and configurations
(require 'navigation)     ; File navigation, sessions, and buffer management

(provide 'post-init)
;;; post-init.el ends here
