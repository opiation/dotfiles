;;; opiation --- Summary
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

(defvar bootstrap-version)

;; Fetch `straight.el's bootstrap script
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(require 'straight)
(setq straight-use-package-by-default t)



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
                      :font "Fira Code"
                      :height opiation/default-font-height)

  ;; Use Fira Code Retina as a fixed-width font
  ;; This is useful in cases where mode override the default but rely on
  ;; fixed-width and variable-width settings
  (set-face-attribute 'fixed-pitch nil
                      :family "Fira Code"
                      :height 1.0)

  ;; Use Ubuntu as a variable-width font
  (set-face-attribute 'variable-pitch nil
                      :family "Ubuntu"
                      :height 1.1
                      :weight 'regular))
(my/set-sane-defaults)



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

  ;; Add a little breathing room between lines for most full-height fonts.
  (setq-default line-spacing 0.2)
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



(use-package async)

(use-package diminish :demand)

;; In order to acquire and install these fonts on your local system, you must
;; run the following:
;;
;; M-x all-the-icons-install-fonts
(use-package all-the-icons
  :init
  ;; Acquire and install fonts from `all-the-icons' onto your local system if
  ;; they have not already been installed, and only while in GUI mode.
  (when (and (display-graphic-p)
	     (not (member "all-the-icons" (font-family-list))))
    (all-the-icons-install-fonts t)))

(use-package page-break-lines)

(use-package doom-modeline
  :after (all-the-icons)

  :custom
  (doom-modeline-height 30)
  (doom-modeline-buffer-encoding nil)

  :functions doom-modeline-mode

  :init
  ;; TODO: Set this variable to `t' in the `:custom' section.
  (doom-modeline-mode 1))

(use-package doom-themes
  :after (doom-modeline)

  :config
  (load-theme 'doom-gruvbox t) ; Dark theme
  ;;(load-theme 'doom-one-light t) ; Light theme
  (doom-themes-visual-bell-config)

  ;; (doom-themes-org-config))
  :custom
  (doom-themes-enable-bold t "Allow doom themes to use bold font variants.")
  (doom-themes-enable-italic t "Allow doom themes to use italic font variants.")

  :defines doom-themes-enable-bold doom-themes-enable-italic
  :functions doom-themes-visual-bell-config
  )



;;;;;;;;;;;;;;;;;;;;;
;; Project management
;;;;;;;;;;;;;;;;;;;;;

(use-package projectile
  :bind
  ("C-S-f" . projectile-ripgrep)

  :bind-keymap
  ("C-c p" . projectile-command-map)

  :config
  (projectile-mode)

  :custom
  (projectile-completion-system 'ivy)

  :diminish projectile-mode

  :init
  ;; NOTE: Set this to the directory where you keep your Git repos.
  (when (file-directory-p "~/Projects")
    (setq projectile-project-search-path '("~/Projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after (counsel projectile)
  :config (counsel-projectile-mode)
  :functions counsel-projectile-mode)

(use-package treemacs
  :after (doom-themes)
  :bind (:map global-map
	      ("C-b" . treemacs)
	      ("<f8>" . treemacs))
  :config
  (doom-themes-treemacs-config)

  :functions doom-themes-treemacs-config)

(use-package treemacs-all-the-icons
  :after (all-the-icons treemacs))

(use-package treemacs-magit
  :after (magit treemacs))

(use-package treemacs-projectile
  :after (projectile treemacs))



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
  :functions which-key-mode)

(use-package counsel
  :bind
  (("C-M-j" . #'counsel-switch-buffer)
   :map minibuffer-local-map
   ("C-r" . #'counsel-minibuffer-history))

  :config
  (counsel-mode 1)
  
  ;; :custom
  ;; (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only))

  :functions counsel-minibuffer-history counsel-mode counsel-switch-buffer)

(use-package general
  :after (counsel which-key)

  :config
  (general-create-definer my/leader-keys
    ;; :keymaps '(normal insert visual emacs) ; used for evil-mode
    :prefix "C-SPC"
    ;; :global-prefix "C-SPC")
    )

  (my/leader-keys
    "t" '(counsel-load-theme :which-key "Choose theme"))

  :functions general-create-definer)

(use-package hydra
  :after (counsel general which-key)
  
  :config
  (defhydra hydra-text-scale ()
    "Scale text"
    ("<up>" text-scale-increase "Up")
    ("<down>" text-scale-decrease "Down")
    ("x" nil "Exit" :exit t))
  
  (my/leader-keys
    "s" '(hydra-text-scale/body :which-key "Scale text"))

  (defhydra hydra-help (global-map "s-/")
    "

Help ?

The purpose of this help screen is to provide a number of convenient keyboard
shortcuts to help you!

Use _<esc>_ or _s-/_ anytime to exit this menu.

_b_ : Switch buffer

"
    ("<esc>" nil nil :exit t)
    ("s-/" nil nil :exit t)
    ("b" counsel-switch-buffer nil :exit t))

  (global-set-key (kbd "s-/") #'hydra-help/body)
  (my/leader-keys
    "?" '(hydra-help/body :which-key "Help ?"))

  :functions hydra-show-hint)

(use-package hydra-posframe
  :after (hydra)

  :config (hydra-posframe-mode 1)
  
  :custom
  (hydra-posframe-parameters '((alpha . 90)
			       (left-fringe . 24)
			       (right-fringe . 24))
			     "Adjust look and feel of `hydra' popups.")
  
  :straight (:host github
              :repo "Ladicle/hydra-posframe"
              :type git))

(use-package command-log-mode
  :commands command-log-mode)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search and autocompletion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package ivy
  :bind
  (("C-f" . swiper)
   :map ivy-minibuffer-map
   ("TAB" . ivy-alt-done)
   ("C-l" . ivy-alt-done)
   ("C-j" . ivy-next-line)
   ("C-k" . ivy-previous-line)
   :map ivy-switch-buffer-map
   ("C-k" . ivy-previous-line)
   ("C-l" . ivy-done)
   ("C-d" . ivy-switch-buffer-kill)
   :map ivy-reverse-i-search-map
   ("C-k" . ivy-previous-line)
   ("C-d" . ivy-reverse-i-search-kill))

  :config
  (ivy-mode 1)

  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-height 40 "Count of suggestions in the Ivy minibuffer.")
  (ivy-use-virtual-buffers t)

  :defines ivy-minibuffer-map ivy-reverse-i-search-map ivy-switch-buffer-map
  :diminish
  :functions ivy-mode)

(use-package ivy-rich
  :after (ivy)

  :config
  (ivy-rich-mode 1)
  
  :functions ivy-rich-mode)

(use-package ivy-prescient
  :after (counsel ivy)

  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  ;(prescient-persist-mode 1)
  (ivy-prescient-mode 1)

  :custom
  (ivy-prescient-enable-filtering nil)

  :functions ivy-prescient-mode)

(use-package helpful
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key)

  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable))



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

  :functions org-indent-mode
  :hook
  (org-mode . my/org-mode-setup)
  
  :straight
  (:type git :repo "https://code.orgmode.org/bzg/org-mode.git"))

(use-package ob-deno
  :after (org ob-typescript)
  :config
  (add-to-list 'org-babel-load-languages '(deno . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-to-list 'org-src-lang-modes '("deno" . typescript)))

(use-package ob-typescript
  :after (org)
  :config
  (add-to-list 'org-babel-load-languages '(typescript . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)

  :init
  (require 'ob-js)
  (add-to-list 'org-babel-load-languages '(js . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

(use-package org-bullets
  :after (org)

  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))

  :hook
  (org-mode . org-bullets-mode))

(use-package org-tree-slide
  :custom
  (org-image-actual-width nil))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming language support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;; Install rainbow delimiters to make lisp a little more bearable
(use-package rainbow-delimiters
  :hook
  (emacs-lisp-mode . rainbow-delimiters-mode))

(use-package flycheck
  :config
  (global-flycheck-mode)

  :functions global-flycheck-mode)

(defun opiation/lsp-mode-setup ()
  "Setup `lsp-mode' when visiting and LSP-supported file."
  
  (lsp-enable-which-key-integration)
  (lsp-headerline-breadcrumb-mode))

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
  :hook
  (
   ((elixir-mode javascript-mode js-mode typescript-mode) . lsp-deferred)
   (lsp-mode . opiation/lsp-mode-setup))

  :functions lsp-enable-which-key-integration lsp-headerline-breadcrumb-mode)

(use-package lsp-ui
  :after (lsp)
  :commands lsp-ui-mode
  :config
  (lsp-ui-sideline-mode -1)
  
  :custom
  (lsp-ui-doc-position 'top "Show the hover over doc UI in the top right.")
  (lsp-ui-doc-max-height 20 "Increase the max height of the doc UI.")

  :functions lsp-ui-sideline-mode
  
  ;; :hook
  ;;(lsp-mode . lsp-ui-mode))
  )

(use-package lsp-treemacs
  :after (lsp treemacs)
  :commands lsp-treemacs-errors-list)

(use-package lsp-ivy
  :after (ivy lsp)
  :commands lsp-ivy-workspace-symbol)

(use-package company
  :after (lsp)

  :bind
  (:map company-active-map
	("<tab>" . company-complete-selection))
  (:map lsp-mode-map
	("<tab>" . company-indent-or-complete-common))

  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)

  :defines company-active-map

  :hook
  (lsp-mode . company-mode))

;; `company-box' PR to address `dash-functional' deprecation
;; https://github.com/sebastiencs/company-box/pull/152
(use-package company-box
  :after (company)

  :hook
  (company-mode . company-box-mode))

;; TODO: Automatically start mode using `:mode' parameter.
(use-package js2-mode
  :config
  (setq-default indent-tabs-mode nil
		js-indent-level 2)

  :hook (js2-mode . lsp-deferred)
  
  :mode "\\.jsx?$")

(use-package typescript-mode
  :config
  (setq-default indent-tabs-mode nil
		typescript-indent-level 2)

  :hook
  (typescript-mode . lsp-deferred)

  :mode "\\.tsx?$")

(use-package rust-mode
  :hook
  (rust-mode . lsp-deferred))

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

  :hook
  (markdown-mode . my/center-like-article))

(use-package lsp-java
  :hook
  (java-mode . lsp-deferred))

(use-package editorconfig
  :config
  (editorconfig-mode 1)

  :functions editorconfig-mode)



;;;;;;;;;;;;;;;;;;
;; Version control
;;;;;;;;;;;;;;;;;;

(use-package magit
  :custom
  (magit-display-buffer-function
    #'magit-display-buffer-same-window-except-diff-v1))

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
  (eshell-mode . my/remove-fringes)
  
  :straight (:type built-in))

(use-package eshell-git-prompt
  :after (eshell)
  :config
  (eshell-git-prompt-use-theme 'powerline)
  
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
  
  :straight (:type built-in))

(use-package vterm
  :commands vterm
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
  :bind (("C-`" . popper-toggle-latest))
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
  :functions popper-mode

  :init
  (popper-mode 1))

(defun my/mini-frame-parameters ()
  "Return `mini-frame' parameters."
  '((alpha . 90)
    (height . 1)
    (left . 0.5)
    (left-fringe . 8)
    (right-fringe . 8)
    (top . 0.2)
    (width . 0.8)))

(use-package mini-frame
  :custom
  (mini-frame-color-shift-step 16 "Distinguish the frame slightly.")
  (mini-frame-show-parameters #'my/mini-frame-parameters)
  
  :hook (after-init . mini-frame-mode))

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
  (dired-mode . my/dired/hide-permission-details)

  :straight (:type built-in))

(use-package dired-single
  :after (dired))

(use-package dired-subtree
  :after (dired)
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle)))

(use-package all-the-icons-dired
  :after (all-the-icons dired)

  :hook
  (dired-mode . all-the-icons-dired-mode))

(provide 'init)
;;; init.el ends here
