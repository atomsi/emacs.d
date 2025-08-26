;;; init.el --- the entry of emacs config -*- lexical-binding: t -*-
;;; Author: Tom
;;; Commentary: (c) Tom Si 2025
;;; Code:

;; 系统类型变量
(defconst cabins-os-win (memq system-type '(ms-dos windows-nt cygwin)))
(defconst cabins-os-mac (eq system-type 'darwin))

;; 字体设置
;; UTF-8
(set-language-environment "UTF-8")
(setopt locale-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

(defconst font-size 18) ;; 可按需调整

;; lisp 目录加入 load-path（需目录存在）
(let ((lisp-dir (concat user-emacs-directory "lisp")))
  (when (file-directory-p lisp-dir)
    (add-to-list 'load-path lisp-dir)))

;; 包管理与 use-package 初始化
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("nongnu". "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
        ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t) ;; 自动安装未安装的包

;; 加载自定义配置模块，模块不存在时报错提示
(dolist (file '(init-functions init-builtins init-third-packages))
  (condition-case err
      (require file)
    (error (message "Could not load %s: %s" file err))))

;; 功能
(require 'init-pyim)
(require 'init-ellama)
(require 'init-org)
(require 'init-dev)

;; custom.el 的设置与加载
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)

;;; init.el ends here
;;; Local Variables:
;;; coding: utf-8
;;; byte-compile-warnings: (not unresolved obsolete)
;;; End:
