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
;; 将 Emacs 默认输入法设置为 pyim.
(setq default-input-method "pyim")
(setq pyim-page-length 5)

(global-set-key (kbd "C-\\") 'toggle-input-method)
(setq pyim-dicts
      '((:name "pyim-tsinghua" :file "~/.emacs.d/eim/pyim-tsinghua-dict.pyim")))
(pyim-default-scheme 'quanpin)

;; 设置 pyim 是否使用云拼音
(setq pyim-cloudim 'baidu)

;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode
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
(setq org-image-actual-width nil)
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
(add-hook 'dired-mode-hook 'org-download-enable)

(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))

;; EasyPG: it is enabled by init scripts
(require 'epa-file)
(epa-file-enable)

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/Sync/orgmod/roam")
  (org-roam-dailies-directory "daily/")
  (org-roam-db-gc-threshold most-positive-fixnum)
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n l" . org-roam-node-link)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n b" . org-roam-buffer-toggle)
         ("C-c n u" . org-roam-ui-mode))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode))

(require 'org-tempo)

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t))

(custom-set-variables
 '(package-selected-packages
   '(lsp-python-ms anaconda-mode py-isort blacken pyenv-mode flycheck elpy pyim which-key restclient quickrun protobuf-mode move-dup markdown-mode iedit format-all ellama company)))

(custom-set-faces
 )

;;;;;;;;;;;;;;;;;;;;;;;;
;; Python Mode
;;;;;;;;;;;;;;;;;;;;;;;;

;; 安装并配置 elpy
(use-package elpy
  :ensure t
  :pin melpa
  :config
  ;; 兼容 python-mode 和 python-ts-mode
  (add-hook 'python-mode-hook (lambda () (elpy-mode)))
  (add-hook 'python-ts-mode-hook (lambda () (elpy-mode))))

;; 使用 flycheck 作为 elpy 的语法检查后端
(add-hook 'elpy-mode-hook 'flycheck-mode)
(add-hook 'python-ts-mode-hook 'flycheck-mode)

;; 自动格式化
(use-package blacken
  :ensure t
  :config
  (add-hook 'before-save-hook #'blacken-buffer))

;; 自动排序导入
(use-package py-isort
  :ensure t
  :config
  (add-hook 'before-save-hook #'py-isort-before-save))

;; anaconda-mode
(use-package anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-ts-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  (add-hook 'python-ts-mode-hook 'anaconda-eldoc-mode))

;; 自动格式化 pep8
(add-hook 'elpy-mode-hook 'py-autopep8-mode)
(add-hook 'python-ts-mode-hook 'py-autopep8-mode)

;; (use-package lsp-python-ms
;;   :ensure t
;;   :config
;;   (add-hook 'python-mode-hook #'lsp)
;;   (add-hook 'python-ts-mode-hook #'lsp))

;; pyenv 配置建议放到 lisp/init-third-packages.el
(setenv "WORKON_HOME" "/home/tom/miniconda3/envs/")

(provide 'custom)
;;;
