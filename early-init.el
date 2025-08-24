;;; early-init.el --- Emacs 27+ early initialization file

(unless (>= emacs-major-version 29)
  (error "ONLY EMACS v29+ IS SUPPORTED!"))

;; Speed up startup by increasing GC threshold
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 8 1024 1024)  ; 8MB
                  gc-cons-percentage 0.1)))

;; Uncomment to prevent unwanted native compilation (Emacs 28+)
;; (setq inhibit-automatic-native-compilation t)

;; Package initialize occurs automatically, set to nil if using external manager
;; (setq package-enable-at-startup nil)

;; Clean GUI
(dolist (mode '(tool-bar-mode scroll-bar-mode menu-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(provide 'early-init)

;;; early-init.el ends here
;;; Local Variables:
;;; byte-compile-warnings: (not free-vars)

