;;; early-init --- Early initialization for Emacs before package loading

;;; Commentary:

;;; Code:

;; Disable `package.el' at start up as we intend to use `straight.el' instead.
(setq package-enable-at-startup nil)

;; Increase the garbage collection threshold before initialization so that emacs
;; does not slow down repeatedly on startup to garbage collect.
;; The default is 800 kilobytes. The value is in bytes.
(setq gc-cons-threshold (* 50 1024 1024))

(when (featurep 'native-compile)
  (defvar native-comp-async-report-warnings-errors)
  (setq native-comp-async-report-warnings-errors 'silent))

;; Remove the tool bar before first render.
(push '(tool-bar-lines . 0) default-frame-alist)

;; Remove unneeded vertical scroll bars before first render.
(push '(vertical-scroll-bars) default-frame-alist)

;; Set the background color to a darker face before first render to avoid the
;; blinding white screen that first appears.
;; To make theme loading feel a little more seemless, use the "Background" color
;; from your preferred theme by using `describe-face' after your theme loads.
(push '(background-color . "#282c34") default-frame-alist)

;; Like the background color, use the foreground color of the theme you intend
;; to use so the initial load feels seemless.
(push '(foreground-color . "#ECEFF4") default-frame-alist)

(provide 'early-init)
;;; early-init.el ends here
