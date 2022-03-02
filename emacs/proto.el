;;; opiation --- My personal Emacs configuration -*- lexical-binding: t; -*-
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
(require 'straight)

(straight-use-package 'all-the-icons)
(with-eval-after-load 'all-the-icons
  (when (and (display-graphic-p)
	     (not (member "all-the-icons" (font-family-list))))
    (when (fboundp 'all-the-icons-install-fonts)
      (all-the-icons-install-fonts t))
    (message "%s" "Installed all icons into fonts."))
  (message "%s" "Loaded `all-the-icons'"))

(straight-use-package 'doom-modeline)
(with-eval-after-load 'doom-modeline
  (require 'all-the-icons)

  (customize-set-variable 'doom-modeline-buffer-encoding nil)
  (customize-set-variable 'doom-modeline-height 30)
  (customize-set-variable 'doom-modeline-percent-position nil)

  (when (fboundp 'doom-modeline-mode)
    (doom-modeline-mode 1))
  (message "%s" "Loaded `doom-modeline'"))

(straight-use-package 'doom-themes)
(with-eval-after-load 'doom-themes
  (require 'doom-modeline)
  
  (customize-set-variable 'doom-themes-enable-bold t
			  "Allow doom themes to use bold font variants.")
  (customize-set-variable 'doom-themes-enable-italic t
			  "Allow doom themes to use italic font variants.")
  
  (when (fboundp 'load-theme)
    (load-theme 'doom-one t))
  (when (fboundp 'doom-themes-visual-bell-config)
    (doom-themes-visual-bell-config))
  (message "%s" "Loaded `doom-themes'"))

(straight-use-package '(modus-themes :host gitlab
				     :repo "protesilaos/modus-themes"
				     :type git))


(defun ng/setup-ui ()
  "Setup the Emacs UI to be a little more minimal by default."
  (setq cursor-type 'bar
	visible-bell nil)
  
  (when (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
  
  (when (fboundp 'set-fringe-mode)
    (set-fringe-mode 16))
  
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

  (set-face-attribute 'default nil
		      :family "Roboto Mono"
		      :height 140)

  (set-face-attribute 'variable-pitch nil
  		      :family "Roboto")
  
  (if (display-graphic-p)
      (progn
	(when (fboundp 'scroll-bar-mode)
	  (scroll-bar-mode -1))
	(when (fboundp 'tool-bar-mode)
	  (tool-bar-mode -1))
	(setq-default line-spacing 0.2)
	(when (fboundp 'tab-bar-mode)
	  (tab-bar-mode 1)))
    (progn
      (when (fboundp 'xterm-mouse-mode)
	(xterm-mouse-mode 1))))

  ;; Resize to full screen.
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  ;; Set frames to be fullscreen by default.
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  (require 'doom-themes))
(add-hook 'emacs-startup-hook #'ng/setup-ui)

(defun ng/debug-startup ()
  "Enable some debugging options."
  (split-window-right)
  (display-buffer "*Messages*")
  (message "%s" "Debugging startup"))
;; (add-hook 'emacs-startup-hook #'ng/debug-startup)

(straight-use-package 'rust-mode)
(with-eval-after-load 'rust-mode
  (message "%s" "Loaded `rust-mode'"))

(straight-use-package 'typescript-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
(customize-set-variable 'typescript-indent-level 2)
(with-eval-after-load 'typescript-mode
  (message "%s" "Loaded `typescript-mode'"))

(straight-use-package 'tree-sitter)
(with-eval-after-load 'tree-sitter
  (message "%s" "Loaded `tree-sitter'"))
(add-hook 'emacs-startup-hook #'global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(straight-use-package 'tree-sitter-langs)


(straight-use-package 'eglot)
(with-eval-after-load 'eglot
  (message "%s" "Loaded `eglot'"))

(require 'eglot)
(add-hook 'rust-mode-hook #'eglot-ensure)
(add-hook 'typescript-mode-hook #'eglot-ensure)

(with-eval-after-load 'simple
  (when (fboundp 'column-number-mode)
    (add-hook 'prog-mode-hook #'column-number-mode))
  (when (fboundp 'line-number-mode)
    (add-hook 'prog-mode-hook #'line-number-mode))
  (message "%s" "Loaded `simple'"))

(with-eval-after-load 'linum
  (when (fboundp 'linum-mode)
    (add-hook 'prog-mode-hook #'linum-mode))
  (message "%s" "Loaded `linum'"))
(require 'linum)

(add-hook 'dired-mode-hook #'auto-revert-mode)

(straight-use-package 'vertico)
(with-eval-after-load 'vertico
  (message "%s" "Loaded `vertico'"))
(add-hook 'emacs-startup-hook #'vertico-mode)

(straight-use-package 'vertico-posframe)
(with-eval-after-load 'vertico-posframe
  ;; TODO: Configure frame with additional padding and some transparemcy.
  (setq vertico-posframe-parameters
	'((alpha . 80)
	  (left-fringe . 8)
          (right-fringe . 8)))
  (message "%s" "Loaded `vertico-posframe'"))
(add-hook 'emacs-startup-hook #'vertico-posframe-mode)

(straight-use-package 'marginalia)
(add-hook 'emacs-startup-hook #'marginalia-mode)

(straight-use-package 'corfu)
(defun ng/init-corfu ()
  "Initialize the Corfu package."
  (require 'corfu)
  (corfu-global-mode))
(add-hook 'emacs-startup-hook #'ng/init-corfu)

(straight-use-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(straight-use-package 'consult)
(with-eval-after-load 'consult
  (message "%s" "Loaded `consult'"))
(global-set-key (kbd "C-x b") #'consult-buffer)
;; TODO: Use CTRL-f on linux systems
(global-set-key (kbd "s-f") #'consult-line)
;; TODO: Use CTRL-SHIFT-f on linux systems
(global-set-key (kbd "s-F") #'consult-line-multi)
;; Uses SUPER-p to search through project files like VS Code
(global-set-key (kbd "s-p") #'consult-find)

(straight-use-package 'orderless)
(with-eval-after-load 'orderless
  ;;(message "%s" "Loaded `orderless'")
  )
(customize-set-variable 'completion-styles '(orderless))

;; Allows using ESC to quickly quit any menu, minibuffer, etc.
(global-set-key (kbd "<escape>") #'keyboard-escape-quit)

(straight-use-package 'org)
(with-eval-after-load 'org
  ;; Set faces for heading levels
  (dolist (face '(
                  (org-document-title . 2.0)
                  (org-level-1 . 1.5)
                  (org-level-2 . 1.3)
                  (org-level-3 . 1.2)
                  (org-level-4 . 1.15)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.05)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil
			:height (cdr face)))
  (message "%s" "Loaded `org'"))

(straight-use-package '(org-appear :host github
				   :repo "awth13/org-appear"
				   :type git))
(add-hook 'org-mode-hook #'org-appear-mode)
(with-eval-after-load 'org-appear-mode
  (message "%s" "Loaded `org-appear'"))

(straight-use-package '(org-modern :host github
				   :repo "minad/org-modern"
				   :type git))
(add-hook 'org-mode-hook #'org-modern-mode)
(with-eval-after-load 'org-modern
  (message "%s" "Loaded `org-modern'"))

(progn
  ;; Agenda-related
  (customize-set-variable 'org-agenda-files '("~/Projects/notes/")
			  "Specify the location of agenda files.")
  (customize-set-variable 'org-log-into-drawer t
			  "Use a default `LOGBOOK' drawer for notes.")
  (customize-set-variable 'org-todo-keywords '((sequence "TODO" "NEXT" "|" "DONE(@)"))
			  "Add NEXT to `org-mode' task keywords.")

  ;; General behavior-related
  (customize-set-variable 'org-support-shift-select t
			  "Allow using SHIFT to select text in `org-mode'.")

  ;; Style-related
  (customize-set-variable 'org-ellipsis " ▾"
			  "Provide custom ellipsis for when headers are collapsed.")
  (customize-set-variable 'org-hide-emphasis-markers t
			  "Do not show markup characters in Org files.")
  ;; This requires a working installation of some LaTeX drawing engine (usually
  ;; external to Emacs) with which to draw LaTeX source inline or in exports.
  (customize-set-variable 'org-preview-latex-default-process 'dvisvgm
			  "Specify LaTeX drawing engine."))

(add-hook 'org-mode-hook #'visual-line-mode)
(straight-use-package 'visual-fill-column)
(with-eval-after-load 'visual-fill-column
  (message "%s" "Loaded `visual-fill-column'"))
(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
(customize-set-variable 'visual-fill-column-center-text t
			"Center contents in the horizontal middle of the buffer.")
(customize-set-variable 'visual-fill-column-width 100
			"Limit width of line wrapping to 100 characters.")
(customize-set-variable 'visual-fill-column-fringes-outside-margins nil)

(straight-use-package 'vterm)

;; Inhibit various Emacs startup features
;; Note, these should be synchronous an execute before the end of this script
;; because Emacs startup features are loaded immediately after initialization.
(setq inhibit-startup-echo-area-message t
      inhibit-startup-screen t)

(message "%s" (emacs-init-time))
(provide 'proto)
;;; proto.el ends here
