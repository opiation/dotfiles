;;; opiation --- Summary -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; This is my personal config worked over with some effort.  The configuration
;;; is expected to work both with Emacs opened as a windowed application or
;;; opened in a terminal `--tty'.

;;; Code:



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management with `straight.el'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.
;; See `package-archive-priorities` and `package-pinned-packages`.
;; Most users will not need or want to do this.
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize t)
(require 'use-package-ensure)

(use-package use-package-ensure-system-package
  :ensure t)

;(defvar bootstrap-version)

;; Fetch `straight.el's bootstrap script
;(let ((bootstrap-file
;       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;      (bootstrap-version 5))
;  (unless (file-exists-p bootstrap-file)
;    (with-current-buffer
;        (url-retrieve-synchronously
;         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;         'silent 'inhibit-cookies)
;      (goto-char (point-max))
;      (eval-print-last-sexp)))
;  (load bootstrap-file nil 'nomessage))
;
;(straight-use-package 'use-package)

;(require 'straight)
;(setq straight-use-package-by-default t)

;;;;;;;;;;;;;;;;;;;;;;
;; Styling and display
;;;;;;;;;;;;;;;;;;;;;;

(defvar opiation/default-font-height
  (cond
    ((equal (system-name) "betsy") 120)
    (t 140))
  "The default font height based on the system.")



;; By defualt, Emacs looks quite dated. Colors, fonts and themes are all quite
;; basic, alongside with button iconography in the GUI menu. To give it a chance
;; to shine in a world of HTML-rendered editors, we need some saner defaults.
(defun my/set-sane-defaults ()
  "Setup sane defaults for a /modern/-looking Emacs."
  (interactive)

  ;; Remove the default Emacs splash screen on startup.
  (setq inhibit-startup-message t)

  ;; Increase the default buffer padding a little.
  (set-fringe-mode 10)

  ;; Remove the menu bar (e.g.: `File', `Edit', etc.) as we have key bindings
  ;; for everything we need.
  (menu-bar-mode -1)

  ;; Disable the computer beep from Emacs in favor of a visual flash instead.
  (setq visible-bell t)

  ;; Use a vertical bar `|' as a cursor.
  (setq-default cursor-type 'bar)

  (column-number-mode)

  ;; Display line numbers in the lefthand-side gutter in programming modes
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)

  ;; Use Fira Code Retina as a default font
  (set-face-attribute 'default nil
                      :font "Iosevka Nerd Font Mono"
		      ;:font "Ubuntu Mono"
                      :height opiation/default-font-height)

  ;; Use Fira Code Retina as a fixed-width font
  ;; This is useful in cases where mode override the default but rely on
  ;; fixed-width and variable-width settings
  (set-face-attribute 'fixed-pitch nil
                      :family "Iosevka Nerd Font Mono"
		      ;:family "Fira Code"
		      ;:family "Ubunto Mono"
                      :height 1.0)

  ;; Use Ubuntu as a variable-width font
  (set-face-attribute 'variable-pitch nil
		      :family "Iosevka Nerd Font"
                      ;:family "Ubuntu"
                      :height 1.1
                      :weight 'regular))
(my/set-sane-defaults)

(defun find-user-init-file ()
  "Open your current Emacs configuration file."
  (interactive)
  (find-file user-init-file))

(use-package emacs
  :bind (("C-," . find-user-init-file)))

(use-package emacs
  ;; Add a little breathing room between lines for most full-height fonts.
  :custom (line-spacing 0.1)
  :if (display-graphic-p))

;; Consider only applying line spacing to programming modes as it may make
;; shells and other buffers look a little strange. Fancy `zsh' prompts for
;; instance don't look quite as nice when affected by this setting.
(defun my/configure-graphic-display ()
  "Configure Emacs to run with graphic settings."
  (interactive)

  ;; Set the selected frame to fullscreen.
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  ;; Set frames to be fullscreen by default.
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; Disable various GUI features we don't need since we're relying on bindings.
  (scroll-bar-mode -1)
  (tool-bar-mode -1)

  (message "Configured UI options for the GUI interface"))



;; Sometimes, we may need to use emacs in a terminal, such as remotely accessing
;; our machine through SSH or accessing a machine through SSH and using Emacs
;; there. For such cases, it's useful to set up Emacs specifically for such an
;; interface, making it more terminal-friendly and avoiding any configurations
;; that are unnecessary in such an environment.
(defun my/configure-tty-display ()
  "Configure Emacs to run with TTY display settings."
  (interactive)

  ;; If the frame is viewed in a terminal, enable XTerm mouse capabilities such
  ;; as scrolling, clicking, etc. It's note strictly necessary but is very
  ;; convenient when still unfamiliar with many key bindings.
  (xterm-mouse-mode 1)
  (message "Configured UI options for the TTY interface"))

(if (display-graphic-p)
    (my/configure-graphic-display)
  (my/configure-tty-display))

(use-package xclip
  :config (xclip-mode 1)
  :ensure t
  :if (not (display-graphic-p)))

(defun my/configure-file-management ()
  "Configure file management preference in Emacs."
  (interactive)

  ;; Use `~/.emacs_saves' for save backups
  (setq backup-directory-alist '(("." . "~/.emacs_saves"))))
(my/configure-file-management)



(defun my/increase-garbage-collection-threshold ()
  "Increase garbage collection thresholds."
  (interactive)
  
  ;; Increase garbage collection threshold as some packages generate a lot
  ;; memory garbage like `lsp-mode'.
  (setq gc-cons-threshold (* 100 1000 1000))

  ;; Increase bandwidth between processes
  (setq read-process-output-max (* 1024 1024)))
(my/increase-garbage-collection-threshold)



(use-package async
  :ensure t)

(use-package dash
  :ensure t)

(use-package diminish :demand
  :ensure t)

;; In order to acquire and install these fonts on your local system, you must
;; run the following:
;;
;; M-x nerd-icons-install-fonts
(use-package nerd-icons
  :ensure t)

(use-package page-break-lines
  :ensure t)

(use-package doom-modeline
  :after (dash nerd-icons)

  :custom
  (doom-modeline-height 40)
  (doom-modeline-buffer-encoding nil)

  :ensure t
  :functions doom-modeline-mode

  :init
  ;; TODO: Set this variable to `t' in the `:custom' section.
  (doom-modeline-mode 1))

(use-package doom-themes
  :after (doom-modeline)

  :config
  (load-theme 'doom-gruvbox t)
  ;;(load-theme 'doom-ayu-dark t) ; Dark theme
  ;;(load-theme 'doom-one-light t) ; Light theme
  (doom-themes-visual-bell-config)

  ;; (doom-themes-org-config))
  :custom
  (doom-themes-enable-bold t "Allow doom themes to use bold font variants.")
  (doom-themes-enable-italic t "Allow doom themes to use italic font variants.")

  :defines doom-themes-enable-bold doom-themes-enable-italic
  :ensure t
  :functions doom-themes-visual-bell-config)

(use-package spacious-padding
  :config (spacious-padding-mode 1)
  :demand
  :ensure t)



;;;;;;;;;;;;;;;;;;;;;
;; Project management
;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings and related dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Allows using ESC to quickly quit any menu, minibuffer, etc.
(global-set-key (kbd "<escape>") #'keyboard-escape-quit)

;; Removed any global binding to C-SPC
(global-unset-key (kbd "C-SPC"))

(use-package drag-stuff
  :config
  (drag-stuff-define-keys)
  :ensure t
  :functions drag-stuff-define-keys

  :hook
  (prog-mode . drag-stuff-mode))

(use-package which-key
  :config
  (which-key-mode 1)

  :custom
  (which-key-idle-delay 0.3 "Add a small before showing key binding options.")
  (which-key-side-window-max-height 0.6)

  :diminish which-key-mode
  :ensure t
  :functions which-key-mode)

(use-package vc-hooks
  :custom
  (vc-follow-symlinks t))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search and autocompletion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package consult
  :after (xref)
  :bind (
         ("C-x b" . consult-buffer)
         ("C-p" . consult-project-buffer)
         ("C-f" . consult-line)
         ("C-S-f" . consult-ripgrep))
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :ensure t)

(use-package corfu
  :bind (
         ("C-SPC" . completion-at-point))
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-popupinfo-delay '(1.0 . 0.6))
  :ensure t
  :init (global-corfu-mode))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  :ensure t
  :init
  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package orderless
  :custom (completion-styles '(orderless basic))
  :ensure t)

(use-package vertico
  :ensure t
  :init (vertico-mode)
  :vc (:lisp-dir "extensions/"
       :rev :newest
       :url "https://github.com/minad/vertico"))



;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode configuration
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/org-font-setup ()
  "Configure `org-mode' fonts."
  (interactive)
  
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '(
                  (org-document-title . 2.0)
                  (org-level-1 . 1.4)
                  (org-level-2 . 1.2)
                  (org-level-3 . 1.1)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil
      :font "Ubuntu"
      :height (cdr face)
      :weight 'regular))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil
    :foreground nil
    :height 0.9
    :inherit 'fixed-pitch)
  (set-face-attribute 'org-checkbox nil
    :height 0.9
    :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil
    :height 0.9
    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-formula nil
    :height 0.9
    :inherit 'fixed-pitch)
  (set-face-attribute 'org-meta-line nil
    :height 0.9
    :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-special-keyword nil
    :height 0.9
    :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-table nil
    :height 0.9
    :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil
    :height 0.9
    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil
    :height 0.9
    :inherit '(shadow fixed-pitch)))

(use-package visual-fill-column
  :defines visual-fill-column-center-text visual-fill-column-width
  :ensure t
  :functions visual-fill-column-mode)

(defun my/org-mode-setup ()
  "Configure `org-mode' buffer on open."
  (interactive)

  (require 'visual-fill-column)
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1)
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :after (visual-fill-column)
  :config
  (my/org-font-setup)

  :custom
  (org-agenda-files '("~/Projects/notes/") "Specify location of agenda files.")
  (org-ellipsis " ▾" "Provide custom ellipsis for when headers are collapsed.")
  (org-hide-emphasis-markers t "Do not show markup characters in Org files.")
  ;; This requires a working installation of some LaTeX drawing engine (usually
  ;; external to Emacs) with which to draw LaTeX source inline or in exports.
  (org-preview-latex-default-process 'dvisvgm "Specify LaTeX drawing engine.")
  (org-support-shift-select t "Allow using SHIFT to select text in `org-mode'.")

  :ensure t
  :functions org-indent-mode
  :hook
  (org-mode . my/org-mode-setup)

  :vc (:rev :newest
       :url "https://code.orgmode.org/bzg/org-mode"))

(use-package ob-deno
  :after (org ob-typescript)
  :config
  (add-to-list 'org-babel-load-languages '(deno . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-src-lang-modes '("deno" . typescript))
  :ensure t)

(use-package ob-typescript
  :after (org)
  :config
  (add-to-list 'org-babel-load-languages '(typescript . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  :ensure t

  :init
  (require 'ob-js)
  (add-to-list 'org-babel-load-languages '(js . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

(use-package org-bullets
  :after (org)

  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))
  :ensure t

  :hook
  (org-mode . org-bullets-mode))

(use-package org-tree-slide
  :custom
  (org-image-actual-width nil)
  :ensure t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming language support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dash
  :ensure t)

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines)
  :ensure t)

;; Install rainbow delimiters to make lisp a little more bearable
(use-package rainbow-delimiters
  :ensure t
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

(use-package flycheck
  :config (global-flycheck-mode)
  :ensure t
  :functions global-flycheck-mode)

(defun opiation/lsp-mode-setup ()
  "Setup `lsp-mode' when visiting and LSP-supported file."
  
  (lsp-enable-which-key-integration)
  (lsp-headerline-breadcrumb-mode))

(use-package eglot)

(use-package eldoc
  :custom
  (eldoc-idle-delay 0.05))

(use-package eldoc-box
  :ensure t
  :hook (eglot-managed-mode . eldoc-box-hover-mode))

;;
;; You may need to install requiring language servers for this to work
;; M-x lsp-install-server ts-ls
;;
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-keymap-prefix "C-c l" "Specify the key binding for `lsp-mode' key map.")
  
  :defines lsp-headerline-breadcrumb-segments lsp-keymap-prefix lsp-mode-map
  :ensure t
  :hook
  (lsp-mode . opiation/lsp-mode-setup)

  :functions lsp-enable-which-key-integration lsp-headerline-breadcrumb-mode)

(use-package lsp-ui
  :after (lsp)
  :commands lsp-ui-mode
  :config
  (lsp-ui-sideline-mode -1)

  :ensure t
  :functions lsp-ui-sideline-mode)

(defun reduce-font-height (frame)
  "Reduce the default font height of the given FRAME."
  (set-face-attribute 'default frame :height 0.6))

(use-package lsp-ui-doc
  :after (lsp-ui)
  :custom
  (lsp-ui-doc-delay 0.1 "Just a bit snappier")
  (lsp-ui-doc-header t "Show a header above the symbol description")
  (lsp-ui-doc-max-height 20 "Increase the max height of the doc UI.")
  (lsp-ui-doc-position 'top "Show the hover over doc UI in the top right.")
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-text-scale-level -2)

  :init
  ;(progn
  ;  (add-hook 'lsp-ui-doc-frame-hook #'reduce-font-height))
  )

(use-package simple
  :custom
  (indent-tabs-mode nil))

(use-package js
  :after (treesit)
  :config
  (unless (treesit-language-available-p 'javascript)
    (treesit-install-language-grammar 'javascript))
  :custom
  (js-indent-level 2)
  :hook (js-ts-mode . eglot-ensure)
  :mode (("\\.js$" . js-ts-mode)))

(use-package typescript-ts-mode
  :after (treesit)
  :config
  (progn
    (unless (treesit-language-available-p 'typescript)
      (treesit-install-language-grammar 'typescript))
    (unless (treesit-language-available-p 'tsx)
      (treesit-install-language-grammar 'tsx)))

  :hook
  ((typescript-ts-mode tsx-to-mode) . eglot-ensure)

  :mode (("\\.ts$" . typescript-ts-mode)
         ("\\.tsx$" . tsx-ts-mode)))

(use-package rust-mode
  :ensure t
  :hook (rust-mode . eglot-ensure))

(use-package treesit
  :custom
  (treesit-language-source-alist '((bash "https://github.com/tree-sitter/tree-sitter-bash")
                                   (c "https://github.com/tree-sitter/tree-sitter-c")
                                   (cmake "https://github.com/uyha/tree-sitter-cmake")
                                   (common-lisp "https://github.com/theHamsta/tree-sitter-commonlisp")
                                   (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
                                   (csharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
                                   (css "https://github.com/tree-sitter/tree-sitter-css")
                                   (elisp "https://github.com/Wilfred/tree-sitter-elisp")
                                   (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
                                   (go "https://github.com/tree-sitter/tree-sitter-go")
                                   (heex "https://github.com/phoenixframework/tree-sitter-heex")
                                   (html "https://github.com/tree-sitter/tree-sitter-html")
                                   (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
                                   (js . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
                                   (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc")
                                   (json "https://github.com/tree-sitter/tree-sitter-json")
                                   (make "https://github.com/alemuller/tree-sitter-make")
                                   (markdown "https://github.com/ikatyang/tree-sitter-markdown")
                                   (python "https://github.com/tree-sitter/tree-sitter-python")
                                   (rust "https://github.com/tree-sitter/tree-sitter-rust")
                                   (toml "https://github.com/tree-sitter/tree-sitter-toml")
                                   (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
                                   (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
                                   (yaml "https://github.com/ikatyang/tree-sitter-yaml"))))

(use-package elixir-ts-mode
  :config
  (progn
    (unless (treesit-language-available-p 'elixir)
      (treesit-install-language-grammar 'elixir))
    (unless (treesit-language-available-p 'heex)
      (treesit-install-language-grammar 'heex)))
  
  :hook ((elixir-mode . elixir-ts-mode)
         (elixir-ts-mode . eglot-ensure))
  :init
  (add-to-list 'eglot-server-programs '(elixir-ts-mode "~/.emacs.d/elixir-ls/release/language_server.sh"))
  :mode ("\\.exs?$"))

(use-package yaml-ts-mode
  :mode ("\\.ya?ml$"))

(defun my/center-like-article ()
  "Configure buffer like an 80-character centered article."
  
  (require 'visual-fill-column)
  (setq visual-fill-column-center-text t
        visual-fill-column-width 80)
  (visual-fill-column-mode 1))

(use-package markdown-mode
  :after (visual-fill-column)
  :custom
  (markdown-fontify-code-blocks-natively t)
  :ensure t
  :hook
  (markdown-mode . my/center-like-article))

(use-package lsp-java
  :ensure t
  :hook
  (java-mode . eglot-ensure))

(use-package editorconfig
  :config (editorconfig-mode 1)
  :ensure t
  :functions editorconfig-mode)
  


;;;;;;;;;;;;;;;;;;
;; Version control
;;;;;;;;;;;;;;;;;;

(use-package magit
  :after (dash rainbow-delimiters)
  :custom
  (magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1)
  :ensure t)

;; For Github integration which requires setting up a GitHub token before use
;; (use-package forge)



;;;;;;;;;;;;;;;;;;;
;; Terminal support
;;;;;;;;;;;;;;;;;;;

(defun my/remove-fringes ()
  "Remove left and right fringes in the current buffer."

  (setq left-fringe-width 0
	line-spacing 0
        right-fringe-width 0))

(use-package eshell
  :commands eshell
  :custom
  (eshell-buffer-maximum-lines 1000 "Increase EShell buffer length to 10,000.")
  (eshell-destroy-buffer-when-process-dies t)
  (eshell-hist-ignoredups t)
  (eshell-history-size 10000 "Increase EShell history to 10,000.")
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-visual-commands '("htop" "less" "lynx" "more" "tmux" "top" "zsh"))

  :hook
  (eshell-mode . my/remove-fringes))

(use-package eshell-git-prompt
  :after (eshell)
  :config
  (eshell-git-prompt-use-theme 'powerline)
  :ensure t
  :functions eshell-git-prompt-use-theme)

(use-package term
  :commands term
  :config
  ;;(setq explicit-zsh-args '())        ;; For shell-specific args
  
  :custom
  (explicit-shell-file-name "zsh")
  
  ;; Match the default shell prompt.  Update this if you have a custom prompt
  ;; (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))
  :hook (term-mode . my/remove-fringes))

(use-package eterm-256color
  :after (eshell term)
  :ensure t
  :hook
  (eshell-mode . eterm-256color-mode)
  (term-mode . eterm-256color-mode))

(use-package tramp
  :custom
  ;; This will set the `TERM' environment variable to "tramp" when accessing a
  ;; remote machine.
  ;; It is useful in particular to so that tramp doesn't have to parse
  ;; elaborate shell prompts to understand when it is in a shell.  This
  ;; expects shell configs on the remote target (`~/.bashrc', `~/.zshrc') to
  ;; return early if detecting this env variable.
  ((tramp-terminal-type "tramp" "Specify a known terminal type for SSH."))
  :ensure t)

(use-package vterm
  :commands vterm
  :ensure t
  :ensure-system-package libtool-bin
  :hook
  (vterm-mode . my/remove-fringes))



;;;;;;;;;;;;;;;;;;;;
;; Window management
;;;;;;;;;;;;;;;;;;;;

(defun my/popper/fit-window-to-buffer (win)
  "Resize window WIN to within one to two thirds the frame height."
  
  (let ((one-third-frame-height (floor (frame-height) 40))
        (two-thirds-frame-height (floor (frame-height) 40)))
    (fit-window-to-buffer win
                          two-thirds-frame-height
                          one-third-frame-height)))

(defun my/popper/select-terminal-popup (buffer &optional _alist)
  "Display and switch to the terminal popup BUFFER at the bottom of the screen."
  
  (let ((window (display-buffer-in-side-window
                 buffer
                 '((window-height . #'my/popper/fit-window-to-buffer)
                   (side . bottom)
                   (slot . 1)))))
    (select-window window)))

(use-package popper
  :bind (("C-`" . popper-toggle))
  :commands (ansi-term eshell term vterm)
  :custom
  (popper-display-function #'my/popper/select-terminal-popup)
  (popper-mode-line nil "Disable the modeline for popup windows.")
  (popper-reference-buffers '("\\*ansi-term\\*"
			      "\\*eshell\\*"
			      "\\*term*\\*"
			      "\\*vterm\\*") "Consider exclusively terminal
buffers as popups.")

  :defines popper-reference-buffers
  :ensure t
  :functions popper-mode

  :init
  (popper-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; File system navigation
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/dired/hide-permission-details ()
  "Hide permission and ownership details in Dired buffer."
  (interactive)
  
  (dired-hide-details-mode 1))

;; Configure the built-in `dired' package
(use-package dired
  :commands (dired dired-jump)

  :bind
  (("C-x C-j" . dired-jump))

  :functions dired-hide-details-mode

  :hook
  (dired-mode . my/dired/hide-permission-details))

(use-package dired-single
  :after (dired)
  :ensure t)

(use-package dired-subtree
  :after (dash dired)
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle)))

(use-package nerd-icons-dired
  :after (nerd-icons dired)
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(corfu-popupinfo-delay (1.0 . 0.6) nil nil "Customized with use-package corfu")
 '(custom-safe-themes
   '("88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e"
     "9f297216c88ca3f47e5f10f8bd884ab24ac5bc9d884f0f23589b0a46a608fe14"
     default))
 '(package-selected-packages
   '(async consult corfu diminish dired-single doom-modeline doom-themes
           drag-stuff editorconfig eldoc-box eshell-git-prompt
           eterm-256color evil-nerd-commenter flycheck lsp-java lsp-ui
           lsp-ui-doc marginalia nerd-icons-dired ob-deno
           ob-typescript orderless org-bullets org-tree-slide
           page-break-lines popper rainbow-delimiters rust-mode
           spacious-padding use-package-ensure-system-package vertico
           visual-fill-column vterm which-key xclip)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t :background "#282828")))
 '(header-line ((t :box (:line-width 4 :color "#37302f" :style nil))))
 '(header-line-highlight ((t :box (:color "#ebdbb2"))))
 '(keycast-key ((t)))
 '(line-number ((t :background "#282828")))
 '(mode-line ((t :box (:line-width 6 :color "#37302f" :style nil))))
 '(mode-line-active ((t :box (:line-width 6 :color "#37302f" :style nil))))
 '(mode-line-highlight ((t :box (:color "#ebdbb2"))))
 '(mode-line-inactive ((t :box (:line-width 6 :color "#282828" :style nil))))
 '(tab-bar-tab ((t :box (:line-width 4 :color "#282828" :style nil))))
 '(tab-bar-tab-inactive ((t :box (:line-width 4 :color "#1d2021" :style nil))))
 '(tab-line-tab ((t)))
 '(tab-line-tab-active ((t)))
 '(tab-line-tab-inactive ((t)))
 '(vertical-border ((t :background "#282828" :foreground "#282828")))
 '(window-divider ((t (:background "#282828" :foreground "#282828"))))
 '(window-divider-first-pixel ((t (:background "#282828" :foreground "#282828"))))
 '(window-divider-last-pixel ((t (:background "#282828" :foreground "#282828")))))
