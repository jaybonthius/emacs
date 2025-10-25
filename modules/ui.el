;;; ui.el --- UI, appearance and theme settings -*- lexical-binding: t; -*-
;;; Commentary:
;; Visual appearance, themes, fonts, window decorations, and UI enhancements
;;; Code:

(setq-default cursor-type 'bar)

;; Remove title bar and window decorations (including stoplight buttons)
(add-to-list 'default-frame-alist '(undecorated . t))

;; Disable all active themes
(mapc #'disable-theme custom-enabled-themes)

(set-face-attribute 'default nil
                    :height 130
                    :weight 'regular
                    :family "Fira Code")

;; This assumes you've installed the package via MELPA.
(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;; Themes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar jb-current-theme-mode nil
  "Current theme mode: 'light, 'dark, or nil for system detection.")

(defun jb-remove-themes ()
  "Remove all of the themes that are currently enabled."
  (interactive)
  (mapcar #'disable-theme custom-enabled-themes))

(defun jb-load-theme ()
  "Load a theme interactively, removing all other themes first."
  (interactive)
  (jb-remove-themes)
  (call-interactively #'load-theme))

(defun jb-system-dark-mode-p ()
  "Check if macOS is in dark mode."
  (when (eq system-type 'darwin)
    (not (null (string-match-p "Dark"
                               (shell-command-to-string
                                "defaults read -g AppleInterfaceStyle 2>/dev/null || echo Light"))))))

(defun jb-load-light-theme ()
  "Load the light theme."
  (interactive)
  (jb-remove-themes)
  (load-theme 'twilight-bright t)
  (setq jb-current-theme-mode 'light)
  (message "Switched to light theme"))

(defun jb-load-dark-theme ()
  "Load the dark theme."
  (interactive)
  (jb-remove-themes)
  (load-theme 'twilight-anti-bright t)
  (setq jb-current-theme-mode 'dark)
  (message "Switched to dark theme"))

(defun jb-toggle-theme ()
  "Toggle between light and dark themes."
  (interactive)
  (cond
   ((eq jb-current-theme-mode 'light) (jb-load-dark-theme))
   ((eq jb-current-theme-mode 'dark) (jb-load-light-theme))
   (t (if (jb-system-dark-mode-p)
          (jb-load-light-theme)
        (jb-load-dark-theme)))))

(defun jb-detect-and-apply-system-theme ()
  "Detect system theme and apply appropriate Emacs theme."
  (interactive)
  (if (jb-system-dark-mode-p)
      (progn
        (jb-remove-themes)
        (load-theme 'twilight-anti-bright t)
        (setq jb-current-theme-mode 'dark))
    (progn
      (jb-remove-themes)
      (load-theme 'twilight-bright t)
      (setq jb-current-theme-mode 'light))))

;; Add local theme directories to custom-theme-load-path
(let ((config-dir (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name))))))
  (add-to-list 'custom-theme-load-path (expand-file-name "themes/twilight-bright-theme" config-dir))
  (add-to-list 'custom-theme-load-path (expand-file-name "themes/twilight-anti-bright-theme" config-dir)))

(jb-detect-and-apply-system-theme)

(use-package perfect-margin
  :custom
  (perfect-margin-visible-width 128)
  :config
  (perfect-margin-mode t)
  ;; auto-center minibuffer windows
  (setq perfect-margin-ignore-filters nil)
  ;; auto-center special windows
  (setq perfect-margin-ignore-regexps nil)
  ;; only set left margin
  (setq perfect-margin-only-set-left-margin t)
  ;; add additinal bding on margin area
  (dolist (margin '("<left-margin> " "<right-margin> "))
    (global-set-key (kbd (concat margin "<mouse-1>")) 'ignore)
    (global-set-key (kbd (concat margin "<mouse-3>")) 'ignore)
    (dolist (multiple '("" "double-" "triple-"))
      (global-set-key (kbd (concat margin "<" multiple "wheel-up>")) 'mwheel-scroll)
      (global-set-key (kbd (concat margin "<" multiple "wheel-down>")) 'mwheel-scroll))))

;; keep me from accidentally zooming after using scroll wheel
(global-unset-key (kbd "C-<wheel-up>"))
(global-unset-key (kbd "C-<wheel-down>"))

;; for terminal emacs
(define-key key-translation-map (kbd "<f12>") 'event-apply-control-modifier)

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode 1)
  (setq hl-todo-keyword-faces
        '(("TODO"   . (:inherit error :weight bold))
          ("FIXME"  . (:inherit error :weight bold))
          ("DEBUG"  . (:inherit warning :weight bold))
          ("GOTCHA" . (:inherit warning :weight bold))
          ("STUB"   . (:inherit font-lock-keyword-face :weight bold))
          ("HACK"   . (:inherit warning :weight bold))
          ("NOTE"   . (:inherit success :weight bold))))
  (keymap-set hl-todo-mode-map "C-c p" #'hl-todo-previous)
  (keymap-set hl-todo-mode-map "C-c n" #'hl-todo-next)
  (keymap-set hl-todo-mode-map "C-c o" #'hl-todo-occur))

(global-eldoc-mode -1)

;; Displays visible indicators for page breaks
(use-package page-break-lines
  :ensure t
  :commands (page-break-lines-mode
             global-page-break-lines-mode)
  :hook
  (emacs-lisp-mode . page-break-lines-mode))

;; Configure the `tab-bar-show` variable to 1 to display the tab bar exclusively
;; when multiple tabs are open:
(setopt tab-bar-show 1)

(use-package persist-text-scale
  :commands (persist-text-scale-mode
             persist-text-scale-restore)
  :hook (after-init . persist-text-scale-mode))

;; When Delete Selection mode is enabled, typed text replaces the selection
;; if the selection is active.
(delete-selection-mode 1)

;; Display the current line and column numbers in the mode line
(setq line-number-mode t)
(setq column-number-mode t)
(setq mode-line-position-column-line-format '("%l:%C"))

; disable line numbers in the buffer
(setq display-line-numbers-type nil)

;; Set the maximum level of syntax highlighting for Tree-sitter modes
(setq treesit-font-lock-level 4)

(use-package which-key
  :ensure nil ; builtin
  :commands which-key-mode
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 1.5)
  (which-key-idle-secondary-delay 0.25)
  (which-key-add-column-padding 1)
  (which-key-max-description-length 40))

(unless (and (eq window-system 'mac)
             (bound-and-true-p mac-carbon-version-string))
  ;; Enables `pixel-scroll-precision-mode' on all operating systems and Emacs
  ;; versions, except for emacs-mac.
  ;;
  ;; Enabling `pixel-scroll-precision-mode' is unnecessary with emacs-mac, as
  ;; this version of Emacs natively supports smooth scrolling.
  ;; https://bitbucket.org/mituharu/emacs-mac/commits/65c6c96f27afa446df6f9d8eff63f9cc012cc738
  (setq pixel-scroll-precision-use-momentum nil) ; Precise/smoother scrolling
  (pixel-scroll-precision-mode 1))

;; Display the time in the modeline
(add-hook 'after-init-hook #'display-time-mode)

;; Paren match highlighting
(add-hook 'after-init-hook #'show-paren-mode)

;; Track changes in the window configuration, allowing undoing actions such as
;; closing windows.
(add-hook 'after-init-hook #'winner-mode)

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator "•")
  (uniquify-after-kill-buffer-p t))

;; Window dividers separate windows visually. Window dividers are bars that can
;; be dragged with the mouse, thus allowing you to easily resize adjacent
;; windows.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Window-Dividers.html
(add-hook 'after-init-hook #'window-divider-mode)

;; Enables visual indication of minibuffer recursion depth after initialization.
(add-hook 'after-init-hook #'minibuffer-depth-indicate-mode)

;; When tooltip-mode is enabled, certain UI elements (e.g., help text,
;; mouse-hover hints) will appear as native system tooltips (pop-up windows),
;; rather than as echo area messages. This is useful in graphical Emacs sessions
;; where tooltips can appear near the cursor.
(setq tooltip-hide-delay 20)    ; Time in seconds before a tooltip disappears (default: 10)
(setq tooltip-delay 0.4)        ; Delay before showing a tooltip after mouse hover (default: 0.7)
(setq tooltip-short-delay 0.08) ; Delay before showing a short tooltip (Default: 0.1)
(tooltip-mode 1)

(provide 'ui)
;;; ui.el ends here
