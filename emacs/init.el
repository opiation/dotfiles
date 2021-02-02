;; Add `melpa` to the list of available emacs package repositories
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; This will set the TERM environment variable to "tramp" when accessing a
;; remote machine
;; It is useful in particular to so that tramp doesn't have to parse
;; elaborate shell prompts to understand when it is in a shell.  This
;; expects shell configs on the remote target (`~/.bashrc`, `~/.zshrc`) to
;; return early if detecting this env variable.
(setq tramp-terminal-type "tramp")

;; Be sure to install use-package with:
;; package-install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Run this once to install required fonts
;; all-the-icons-install-fonts
(use-package all-the-icons
  ;:config
  ;(all-the-icons-install-fonts t)
  :ensure t)

;; Show line numbers in lefthandside gutter
(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode)
  :ensure t)

(use-package markdown-mode
  :config (setq-default markdown-fontify-code-blocks-natively 1)
  :ensure t)

(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  :ensure t)

(use-package treemacs
  :bind (:map global-map
	      ("<f8>" . treemacs))
  :defer t
  :ensure t)

(use-package treemacs-projectile
  :after projectile treemacs 
  :ensure t)

(use-package treemacs-magit
  :after magit treemacs
  :ensure t)

;; Magit Git plugin
(use-package magit
  :bind (:map global-map
	      ("C-x g" . magit-status))
  :ensure t)

(use-package drag-stuff
  :config (drag-stuff-define-keys)
  :ensure t
  :hook (prog-mode . drag-stuff-mode))

;;
;; Themes!!
;;
(use-package atom-one-dark-theme
  :config (load-theme 'atom-one-dark t)
  :ensure t)
(use-package solarized-theme
  :ensure t)
(use-package zenburn-theme
  :ensure t)

(use-package centaur-tabs
  :after all-the-icons
  :bind
  (:map global-map
	("C-<prior>" . centaur-tabs-backward)
	("C-<next>" . centaur-tabs-forward))
  :demand
  :config
  (setq centaur-tabs-height 44
;	centaur-tabs-modified-marker "*"
	centaur-tabs-set-icons t
	centaur-tabs-set-modified-marker t
	centaur-tabs-show-navigation-buttons t
	centaur-tabs-style "bar")
  (centaur-tabs-mode 1)
  :ensure t)

(use-package powerline
  :config (powerline-center-theme)
  :ensure t)

(use-package lsp-mode
  :config
  ;; (lsp-install-server ts-ls)
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (javascript-mode . lsp))
         ;; if you want which-key integration
         ;;(lsp-mode . lsp-enable-wqhich-key-integration))
  :commands lsp)

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :ensure t)

;; if you are helm user
;;(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
;;(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :after lsp-mode treemacs
  :commands lsp-treemacs-errors-list
  :ensure t)

(use-package editorconfig
  :config (editorconfig-mode 1)
  :ensure t)

(use-package visual-fill-column
  :config (setq-default fill-column 80)
  :ensure t
  :hook
  (prog-mode . display-fill-column-indicator-mode))

;;
;; Using emacs in windowed mode
;;
;; Kill splash screen at startup
(setq inhibit-startup-screen t)

;;
;; Remove menu and tool bars from the window
(menu-bar-mode 0)
(tool-bar-mode 0)

;;
;; Set an appropriate font and size depending on the machine
;;
(cond
 ((equal (system-name) "betsy")
  (set-face-attribute 'default nil :font "Fira Code-12"))
 ((equal (system-name) "dellbot")
  (set-face-attribute 'default nil :font "Fira Code-14")))

;; Use `smex` package for auto-completion in Emacs commands
(use-package smex
  :bind (
	 ("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)
	 ("C-c C-c M-x" . execute-extended-command))
  :ensure t)

;; Auto-complete in some commands
(ido-mode 1)



;; Use `~/.emacs_saves` for save backups
(setq backup-directory-alist '(("." . "~/.emacs_saves")))





;; Generated configuration
;;(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;; '(default ((t (:family "Ubuntu Mono" :foundry "DAMA" :slant normal :weight normal :height 143 :width normal)))))

;; Word wrap
;;
;; This ensure lines are wrapped at word boundaries instead of inbetween words
;; at exactly the edge
;;
(global-visual-line-mode 1)


;;
;; Session tabs
;;
;; Frame-based tab bar and related shortcuts
;; Use [SUPER] and left/right arrows to navigate between tabs
;;
;;(tab-bar-mode 1)
;;(global-set-key [M-left] 'tab-bar-switch-to-prev-tab)
;;(global-set-key [M-right] 'tab-bar-switch-to-next-tab)

;; Typescript support
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))
(setq company-tooltip-align-annotations t)

(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

(setq tide-format-options '(:indentSize 2
			    :tabSize 2))

;; Language settings
(setq js-indent-level 2
      typescript-indent-level 2)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(message "Hey, there!  We're done loading!")
