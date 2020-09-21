;; Add `melpa` to the list of available emacs package repositories
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)



;;
;; Using emacs in windowed mode
;;
;; Kill splash screen at startup
(setq inhibit-startup-screen t)
;;
;; Remove menu and tool bars from the window
(menu-bar-mode 0)
(tool-bar-mode 0)



;; `smex` configuration
;;
;; Install using the following:
;;
;; package-install [RET] smex
;;
;; Use `smex` package for auto-completion in Emacs commands
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)



;; Auto-complete in some commands
(ido-mode 1)



;; Use `~/.emacs_saves` for save backups
(setq backup-directory-alist '(("." . "~/.emacs_saves")))



;; Generated configuration
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "13a8eaddb003fd0d561096e11e1a91b029d3c9d64554f8e897b2513dbf14b277" "0fffa9669425ff140ff2ae8568c7719705ef33b7a927a0ba7c5e2ffcfac09b75" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "5f824cddac6d892099a91c3f612fcf1b09bb6c322923d779216ab2094375c5ee" default))
 '(frame-brackground-mode 'dark)
 '(package-selected-packages
   '(projectile elixir-mode magit solarized-theme js2-mode editorconfig company tide neotree markdown-preview-mode visual-fill-column markdown-mode gruber-darker-theme smex typescript-mode)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



;;
;; Theme and display configuration
;;
;; Load default theme
(load-theme 'solarized-gruvbox-dark)

;; Show line numbers in the left-hand side gutter
(require 'display-line-numbers)
(global-display-line-numbers-mode)

;; Word wrap
;;
;; This ensure lines are wrapped at word boundaries instead of inbetween words
;; at exactly the edge
;;
;; (require 'visual-line-mode)
(global-visual-line-mode 1)

;; (require 'visual-fill-column)
(global-visual-fill-column-mode 1)
(setq-default fill-column 80)
(global-display-fill-column-indicator-mode 1)

(require 'markdown-mode)
(setq-default markdown-fontify-code-blocks-natively 1)

;;
;; Session tabs
;;
;; Frame-based tab bar and related shortcuts
;; Use [SUPER] and left/right arrows to navigate between tabs
;;
(tab-bar-mode 1)
(global-set-key [M-left] 'tab-bar-switch-to-prev-tab)
(global-set-key [M-right] 'tab-bar-switch-to-next-tab)

;; Directory tree viewing with neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

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


;; Editor config
(editorconfig-mode 1)



;; Projectile
(unless (package-installed-p 'projectile)
  (package-install 'projectile))
(require 'projectile)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)



;; Magit setup
(unless (package-installed-p 'magit)
  (package-install 'magit))
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
  


;; Language settings
(setq js-indent-level 2
      typescript-indent-level 2)
