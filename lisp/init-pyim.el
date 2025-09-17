;;; init-pyim.el --- -*- lexical-binding: t; -*-
(use-package pyim
  :ensure t
  :demand t
  :custom
  (default-input-method "pyim")
  (pyim-page-length 5)
  (pyim-dicts
   '((:name "pyim-tsinghua"
            :file "~/.emacs.d/eim/pyim-tsinghua-dict.pyim")))
  (pyim-cloudim 'baidu)
  (pyim-default-scheme 'quanpin)
  :config
  ;; 定义切换中英文标点的函数（放在 :config 内，确保 pyim 已加载）
  (defun atom/pyim-toggle-punctuation ()
    "切换 pyim 中英文标点翻译模式"
    (interactive)
    (setq pyim-punctuation-translate-p (not pyim-punctuation-translate-p))
    (message "pyim 标点模式：%s"
             (if pyim-punctuation-translate-p "中文标点" "英文标点")))

  ;; 1. 绑定到输入法全局映射（推荐，任何输入法激活时生效）
  ;;(define-key input-method-map (kbd "C-.") #'atom/pyim-toggle-punctuation)

  ;; 2. 额外绑定全局快捷键（备选，即使输入法未激活也能触发）
  (global-set-key (kbd "C-.") #'atom/pyim-toggle-punctuation)

  ;; 切换输入法的快捷键（保持不变）
  (global-set-key (kbd "C-\\") #'toggle-input-method))

(provide 'init-pyim)
;;; init-pyim.el ends here
