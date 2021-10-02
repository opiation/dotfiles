;;; early-init --- Early initialization for Emacs before package loading

;;; Commentary:

;;; Code:

;; Disable `package.el' at start up as we intend to use `straight.el' instead.
(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
