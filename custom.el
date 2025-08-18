;;; package --- Summary:
;;; this is personal emacs configure file, it includes
;;; 1. normal settings
;;; 2. org mode
;;; 3. python mode
;;; 4. rust mode

;;; Commentary:

;;; Code:


(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

(use-package ellama
  :init
  (setopt ellama-language "Chinese")
  (require 'llm-ollama)
  (setopt ellama-provider
	  (make-llm-ollama
	   :chat-model "qwen2.5-coder:32b-instruct-q8_0"
	   :embedding-model "qwen2.5-coder:32b-instruct-q8_0"
	   )))

;;;;;;;;;;
;;PYIM
;;;;;;;;;

(require 'pyim)
;; ;; 将 Emacs 默认输入法设置为 pyim.
(setq default-input-method "pyim")
(setq pyim-page-length 5)

(global-set-key (kbd "C-\\") 'toggle-input-method)
(setq pyim-dicts
      '((:name "pyim-tsinghua" :file "~/.emacs.d/eim/pyim-tsinghua-dict.pyim")))
(pyim-default-scheme 'quanpin)

;; ;; 设置 pyim 是否使用云拼音
(setq pyim-cloudim 'baidu)



;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; org-mode        ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org-download
  :ensure t
  ;;将截屏功能绑定到快捷键：Ctrl + Shift + Y
  :bind ("C-S-y" . org-download-screenshot)
  :config
  (require 'org-download)
  ;; Drag and drop to Dired
  (add-hook 'dired-mode-hook 'org-download-enable)
  )

(auto-image-file-mode t)
(setq org-image-actual-width nil)

(setq org-startup-indented t)
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (ditaa . t)
   (plantuml . t)
   (dot . t)
   ))
(setq org-plantuml-jar-path (expand-file-name "/usr/share/plantuml/plantuml.jar"))
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))

;;状态变为done的时候自动加上时间和提示输入说明
(setq org-log-done 'note)
;;adding special markers ‘!’ (for a timestamp) or ‘@’ (for a note with timestamp) in parentheses after each keyword.
(setq org-todo-keywords
      '((sequence "TODO(t!)" "WAIT(w)" "|" "DONE(d!)" "CANCELED(c@/!)")))
;; to set the display size for jpg & png
(setq org-image-actual-width nil);; 绑定键位
(define-key global-map "\C-cc" 'org-capture)

(eval-after-load 'ol
  '(dolist (scheme '("http" "https"))
     (org-link-set-parameters
      scheme :follow (lambda (url arg) (eww (concat scheme ":" url) arg)))))

(setq org-capture-templates
      '(("i" "Idea" entry (file+headline "~/Sync/orgmod/idea.org" "Idea")
	 "* %?\n  %i\n  %a")
	("d" "Diary" entry (file+datetree "~/Sync/orgmod/diary.org.gpg" "Diary")
	 "* %?\nEntered on %U\n %i\n %a")
	("r" "Reading" entry (file+headline "~/Sync/orgmod/reading.org")
	 "* %?\n  %i\n  %a")
	("t" "Todo" entry (file+headline "~/Sync/orgmod/gtd.org" "Tasks")
	 "* TODO %?\n  %i\n  %a")))
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

(eval-after-load "eww"
  '(progn (define-key eww-mode-map "f" 'eww-lnum-follow)
          (define-key eww-mode-map "F" 'eww-lnum-universal)))
(autoload 'notmuch "notmuch" "notmuch mail" t)

(require 'org-download)
;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)

(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))


;; EasyPG: it is enabled by init scripts
(require 'epa-file)
(epa-file-enable)

(use-package org-roam
  :ensure t ;; 自动安装
  :custom
  (org-roam-directory "~/Sync/orgmod/roam") ;; 默认笔记目录, 提前手动创建好
  (org-roam-dailies-directory "daily/") ;; 默认日记目录, 上一目录的相对路径
  (org-roam-db-gc-threshold most-positive-fixnum) ;; 提高性能
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n l" . org-roam-node-link)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n b" . org-roam-buffer-toggle) ;; 显示后链窗口
         ("C-c n u" . org-roam-ui-mode))      ;; 浏览器中可视化
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map) ;; 日记菜单
  :config
  (require 'org-roam-dailies)  ;; 启用日记功能
  (org-roam-db-autosync-mode)) ;; 启动时自动同步数据库

(require 'org-tempo);; enable the structure templates by <key->

(use-package org-roam-ui
  :ensure t ;; 自动安装
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t) ;; 同步 Emacs 主题
  (org-roam-ui-follow t) ;; 笔记节点跟随
  (org-roam-ui-update-on-save t))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(lsp-python-ms anaconda-mode py-isort blacken pyenv-mode flycheck elpy pyim which-key restclient quickrun protobuf-mode move-dup markdown-mode iedit format-all  ellama company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;
;;Python Mode
;;;;;;;;;;;;;;;;;;;;;;;;

;; 安装并配置 elpy
(use-package elpy
  :ensure t
  :pin melpa
  :config
  ;; 为所有 Python 文件启用 elpy 模式
  (add-hook 'python-mode-hook (lambda () (elpy-mode)))
  ;; 使用 flycheck 作为 elpy 的语法检查后端
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; 安装并配置 company
(use-package company
  :ensure t
  :config
  ;; 全局启用 company 模式
  (global-company-mode)
  ;; 减少 company 模式的延迟
  (setq company-idle-delay 0)
  ;; 在候选列表中显示数字
  (setq company-show-numbers t))

;; 安装并配置 flycheck
(use-package flycheck
  :ensure t
  :config
  ;; 全局启用 flycheck 模式
  (global-flycheck-mode))

;; 安装并配置 pyenv
(setenv "WORKON_HOME" "/home/tom/miniconda3/envs/")
;; (use-package pyenv-mode
;;   :ensure t
;;   :config
;;   ;; 启用 pyenv 模式
;;   (pyenv-mode)
;;   )


;; 安装并配置 blacken
(use-package blacken
  :ensure t
  :config
  ;; 在保存 Python 文件之前自动格式化
  (add-hook 'before-save-hook #'blacken-buffer))

;; 安装并配置 py-isort
(use-package py-isort
  :ensure t
  :config
  ;; 在保存 Python 文件之前自动排序导入
  (add-hook 'before-save-hook #'py-isort-before-save))

;; 安装并配置 anaconda-mode
(use-package anaconda-mode
  :ensure t
  :config
  ;; 启用 anaconda 模式
  (anaconda-mode)
  ;; 启用 anaconda 的 eldoc 模式
  (anaconda-eldoc-mode))

;; ;; 如果你想使用语言服务器协议 (LSP)，可以安装并配置 lsp-python-ms
;; (use-package lsp-python-ms
;;   :ensure t
;;   :config
;;   ;; 在 Python 模式下启用 LSP
;;   (add-hook 'python-mode-hook #'lsp))

(add-hook 'elpy-mode-hook 'py-autopep8-mode)

(provide 'custom)
;;;
