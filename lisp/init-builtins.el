;;; init-builtins --- settings for builtins
;;; Commentary:
;;; Code:

;; os & coding settings
(when (and (boundp 'cabins-os-win) cabins-os-win
           (boundp 'w32-get-true-file-attributes))
  (setq w32-get-true-file-attributes nil
        w32-pipe-read-delay 0
        w32-pipe-buffer-size (* 64 1024)))

(when (and (boundp 'cabins-os-mac) cabins-os-mac)
  (when (boundp 'mac-command-modifier)
    (setq mac-command-modifier 'meta))
  (when (boundp 'mac-option-modifier)
    (setq mac-option-modifier 'super))
  (when (boundp 'ns-use-native-fullscreen)
    (setq ns-use-native-fullscreen t)))

;; solve the Chinese paste issue / selection coding
(prefer-coding-system 'utf-8)
(unless (and (boundp 'cabins-os-win) cabins-os-win)
  (when (fboundp 'set-selection-coding-system)
    (set-selection-coding-system 'utf-8)))

;; NOTE: package-archives removed per request

;; make use-package default behavior better (if use-package is present)
(when (featurep 'use-package)
  (setq use-package-enable-imenu-support t
        use-package-expand-minimally t))

;; Emacs builtin packages / defaults
(setq-default auto-window-vscroll nil
              default-directory "~"
              default-text-properties '(line-spacing 0.2 line-height 1.2) ; default line height
              frame-title-format "%b"
              help-window-select t
              initial-major-mode 'fundamental-mode
              inhibit-startup-screen t ; disable the startup screen splash
              kill-whole-line t
              make-backup-files nil	; disable backup file
              read-process-output-max (* 4 1024 1024)
              require-final-newline t
              scroll-conservatively 1000
              show-trailing-whitespace t
              system-time-locale "C"
              use-short-answers t)

;; isearch
(when (featurep 'isearch)
  (setq-default isearch-allow-motion t
                isearch-lazy-count t))

;; auto revert
(when (featurep 'autorevert)
  (add-hook 'after-init-hook #'global-auto-revert-mode))

;; auto save to the visited file (provided by `files.el')
(when (featurep 'files)
  (add-hook 'after-init-hook #'auto-save-visited-mode))

;; Delete Behavior
(when (featurep 'delsel)
  (add-hook 'after-init-hook #'delete-selection-mode))

;; visual-line-mode for programming buffers
(when (featurep 'simple)
  (add-hook 'prog-mode-hook #'visual-line-mode))

;; pixel-scroll-precision-mode (if available)
(when (fboundp 'pixel-scroll-precision-mode)
  (add-hook 'after-init-hook #'pixel-scroll-precision-mode))

;; fido-mode (icomplete) if available
(when (featurep 'icomplete)
  (when (fboundp 'fido-mode)
    (add-hook 'after-init-hook #'fido-mode))
  (setq completions-detailed t))

;; Highlight Current Line in GUI frames
(when (and (display-graphic-p) (featurep 'hl-line))
  (add-hook 'prog-mode-hook #'hl-line-mode))

;; ibuffer alias
(when (featurep 'ibuffer)
  (defalias 'list-buffers 'ibuffer))

;; Org Mode: enable org-num-mode only if present
(when (featurep 'org)
  (add-hook 'org-mode-hook (lambda ()
                             (when (fboundp 'org-num-mode)
                               (org-num-mode))))
  (setq org-hide-leading-stars t
        org-hide-emphasis-markers t
        org-startup-indented t))

;; Pulse the cursor line if available
(when (fboundp 'pulse-momentary-highlight-one-line)
  (dolist (cmd '(recenter-top-bottom other-window))
    (advice-add cmd :after (lambda (&rest _) (pulse-momentary-highlight-one-line)))))

;; Show Paren Mode
(when (featurep 'paren)
  (setq show-paren-when-point-in-periphery t
        show-paren-when-point-inside-paren t
        show-paren-style 'mixed))

;; Recentf
(when (featurep 'recentf)
  (add-hook 'after-init-hook #'recentf-mode)
  (global-set-key
   (kbd "C-c r")
   (lambda ()
     (interactive)
     (cond
      ((fboundp 'recentf-open)
       (call-interactively 'recentf-open))
      ((fboundp 'recentf-open-files)
       (call-interactively 'recentf-open-files))
      (t
       (user-error "recentf-open / recentf-open-files not available"))))))

;; windmove
(when (featurep 'windmove)
  (windmove-default-keybindings))

;; which-key: only enable if available (not assuming installed)
(when (require 'which-key nil 'noerror)
  (add-hook 'after-init-hook #'which-key-mode))

;; Consolidated prog-mode hook to enable common programming helpers
(add-hook 'prog-mode-hook
          (lambda ()
            ;; show column number in mode line for this buffer
            (when (boundp 'column-number-mode)
              (setq-local column-number-mode t))
            ;; modes that may or may not be present in the build
            (when (fboundp 'display-line-numbers-mode)
              (display-line-numbers-mode))
            (when (fboundp 'electric-pair-mode)
              (electric-pair-mode))
            (when (fboundp 'hs-minor-mode)
              (hs-minor-mode))
            (when (fboundp 'prettify-symbols-mode)
              (prettify-symbols-mode))
            (when (fboundp 'which-function-mode)
              (which-function-mode))))

;; Flymake
(when (featurep 'flymake)
  (add-hook 'prog-mode-hook #'flymake-mode)
  (global-set-key (kbd "M-n") #'flymake-goto-next-error)
  (global-set-key (kbd "M-p") #'flymake-goto-prev-error))

;; Eglot (language server client)
(when (featurep 'eglot)
  (global-set-key (kbd "C-c e f") #'eglot-format)
  (when (fboundp 'eglot-code-action-organize-imports)
    (advice-add 'eglot-code-action-organize-imports :before #'eglot-format-buffer))
  (add-hook 'eglot-managed-mode-hook
            (lambda () (when (fboundp 'eglot-format-buffer)
                         (add-hook 'before-save-hook #'eglot-format-buffer nil t))))
  (add-hook 'prog-mode-hook
            (lambda ()
              (unless (eq major-mode 'emacs-lisp-mode)
                (when (fboundp 'eglot-ensure)
                  (eglot-ensure))))))

;; treesit (if available)
(when (and (fboundp 'treesit-available-p) (treesit-available-p))
  (when (require 'treesit nil 'noerror)
    (setq treesit-font-lock-level 4)
    (setq major-mode-remap-alist
          '((sh-mode         . bash-ts-mode)
            (c-mode          . c-ts-mode)
            (c++-mode        . c++-ts-mode)
            (c-or-c++-mode   . c-or-c++-ts-mode)
            (css-mode        . css-ts-mode)
            (js-mode         . js-ts-mode)
            (java-mode       . java-ts-mode)
            (js-json-mode    . json-ts-mode)
            (makefile-mode   . cmake-ts-mode)
            (python-mode     . python-ts-mode)
            (ruby-mode       . ruby-ts-mode)
            (conf-toml-mode  . toml-ts-mode)))
    (setq treesit-language-source-alist
          '((bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
            (c          . ("https://github.com/tree-sitter/tree-sitter-c"))
            (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp"))
            (css        . ("https://github.com/tree-sitter/tree-sitter-css"))
            (cmake      . ("https://github.com/uyha/tree-sitter-cmake"))
            (csharp     . ("https://github.com/tree-sitter/tree-sitter-c-sharp.git"))
            (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
            (elisp      . ("https://github.com/Wilfred/tree-sitter-elisp"))
            (go         . ("https://github.com/tree-sitter/tree-sitter-go"))
            (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod.git"))
            (html       . ("https://github.com/tree-sitter/tree-sitter-html"))
            (java       . ("https://github.com/tree-sitter/tree-sitter-java.git"))
            (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
            (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
            (lua        . ("https://github.com/Azganoth/tree-sitter-lua"))
            (make       . ("https://github.com/alemuller/tree-sitter-make"))
            (markdown   . ("https://github.com/MDeiml/tree-sitter-markdown" nil "tree-sitter-markdown/src"))
            (ocaml      . ("https://github.com/tree-sitter/tree-sitter-ocaml" nil "ocaml/src"))
            (org        . ("https://github.com/milisims/tree-sitter-org"))
            (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
            (php        . ("https://github.com/tree-sitter/tree-sitter-php"))
            (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
            (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
            (ruby       . ("https://github.com/tree-sitter/tree-sitter-ruby"))
            (rust       . ("https://github.com/tree-sitter/tree-sitter-rust"))
            (sql        . ("https://github.com/m-novikov/tree-sitter-sql"))
            (vue        . ("https://github.com/merico-dev/tree-sitter-vue"))
            (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml"))
            (toml       . ("https://github.com/tree-sitter/tree-sitter-toml"))
            (zig        . ("https://github.com/GrayJack/tree-sitter-zig"))))))

(provide 'init-builtins)

;;; init-builtins.el ends here
