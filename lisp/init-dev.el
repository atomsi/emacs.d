;;; init-lsp.el --- -*- lexical-binding: t; -*-
;; 通用 LSP
(use-package lsp-mode
  :ensure t
  :defer t
  :hook ((python-mode python-ts-mode rust-mode rust-ts-mode) . lsp-deferred)
  :custom
  (lsp-idle-delay 0.2)
  (lsp-auto-guess-root t)
  (lsp-keep-workspace-alive nil))

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode))

(use-package consult-lsp
  :ensure t
  :after consult)

;; Python
(use-package lsp-pyright
  :ensure t
  :after lsp-mode)
(use-package blacken
  :ensure t
  :hook ((python-mode python-ts-mode) . blacken-mode))

(use-package py-isort
  :ensure t
  :hook ((python-mode python-ts-mode) . py-isort-mode))

;; 虚拟环境
(setenv "WORKON_HOME" "/home/tom/miniconda3/envs")
(setq lsp-pyright-python-executable-cmd
      (lambda () (or (executable-find "python")
                (executable-find "python3")
                "python3")))
;; Rust
(use-package rust-mode
  :ensure t
  :mode ("\\.rs\\'" . rust-mode))

(use-package rustic
  :ensure t
  :after lsp-mode
  :hook (rust-mode . rustic-mode)
  :bind (:map rustic-mode-map
              ("C-c C-c" . rustic-cargo-run)))

(provide 'init-dev)
;;; init-dev.el ends here
