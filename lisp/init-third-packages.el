;;; init-third-packages --- settings for third-party packages (sorted by package names)
;;; Commentary:
;;; Code:

;; benchmark-init - 性能分析，仅调试时建议启用
(use-package benchmark-init
  :ensure t
  :config
  (add-hook 'after-init-hook #'benchmark-init/deactivate))

;; company - 自动补全
(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config
  (setq company-show-quick-access 'left
        company-minimum-prefix-length 1
        company-idle-delay 0.2
        company-format-margin-function nil))

;; exec-path-from-shell - 修正环境变量，仅 macOS/Linux GUI 下启用
(use-package exec-path-from-shell
  :ensure t
  :when (memq window-system '(mac ns x))
  :init
  (exec-path-from-shell-initialize))

;; format-all - 自动格式化
(use-package format-all
  :ensure t
  :hook (prog-mode . format-all-mode)
  :config
  (add-hook 'format-all-mode-hook #'format-all-ensure-formatter)
  :bind ("C-c f" . format-all-region-or-buffer))

;; iedit - 批量编辑
(use-package iedit
  :ensure t
  :bind ("C-;" . iedit-mode))

;; move-dup - 行/区域移动复制，建议直接全局开启
(use-package move-dup
  :ensure t
  :init
  (global-move-dup-mode 1))

;; markdown-mode
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)))

;; protobuf-mode
(use-package protobuf-mode
  :ensure t
  :mode ("\\.proto\\'" . protobuf-mode))

;; quickrun
(use-package quickrun
  :ensure t
  :commands (quickrun quickrun-region))

;; restclient
(use-package restclient
  :ensure t
  :mode ("\\.http\\'" . restclient-mode))

(provide 'init-third-packages)
;;; init-third-packages.el ends here
