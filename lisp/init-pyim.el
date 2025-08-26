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
  (global-set-key (kbd "C-\\") #'toggle-input-method))
(provide 'init-pyim)
;;; init-pyim.el ends here
