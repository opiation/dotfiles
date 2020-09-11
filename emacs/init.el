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
   '("5f824cddac6d892099a91c3f612fcf1b09bb6c322923d779216ab2094375c5ee" default))
 '(package-selected-packages
   '(js2-mode editorconfig company tide neotree markdown-preview-mode visual-fill-column markdown-mode gruber-darker-theme smex typescript-mode)))

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
(load-theme 'gruber-darker)

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



;; Language settings
(setq js-indent-level 2
      typescript-indent-level 2)
