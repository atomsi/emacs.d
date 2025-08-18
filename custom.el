;;; package --- Summary:
;;; this is personal emacs configure file, it includes
;;; 1. normal settings
;;; 2. org mode
;;; 3. python mode
;;; 4. rust mode

;;; Commentary:

;;; Code:

;; 推荐 setopt，但 setq 兼容性更好
(setopt browse-url-browser-function 'browse-url-generic)
(setopt browse-url-generic-program "google-chrome")

(use-package ellama
  :init
  (setopt ellama-language "Chinese")
  :config
  (require 'llm-ollama)
  (setopt ellama-provider
	  (make-llm-ollama
	   :chat-model "qwen2.5-coder:32b-instruct-q8_0"
	   :embedding-model "qwen2.5-coder:32b-instruct-q8_0")))

;;;;; PYIM 输入法配置
(use-package pyim
  :ensure t
  :init
  (setopt default-input-method "pyim")
  (setopt pyim-page-length 5)
  (setopt pyim-dicts
          '((:name "pyim-tsinghua"
                   :file "~/.emacs.d/eim/pyim-tsinghua-dict.pyim")))
  (setopt pyim-cloudim 'baidu)
  (pyim-default-scheme 'quanpin)
  :config
  (global-set-key (kbd "C-\\") 'toggle-input-method))

;;;;; org-mode 配置
(use-package org
  :ensure t
  :init
  (setopt org-image-actual-width nil
          org-startup-indented t
          org-log-done 'note)
  (setopt org-todo-keywords
          '((sequence "TODO(t!)" "WAIT(w)" "|" "DONE(d!)" "CANCELED(c@/!)")))
  :config
  (with-eval-after-load 'org
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((ditaa . t)
       (plantuml . t)
       (dot . t)))
    (setopt org-plantuml-jar-path (expand-file-name "/usr/share/plantuml/plantuml.jar"))
    (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
    (define-key global-map "\C-cc" 'org-capture)
    (add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))
    ;; 修正 org-capture-templates
    (setq org-capture-templates
	  '(("i" "Idea" entry (file+headline "~/Sync/orgmod/idea.org" "Idea")
             "* %?\n  %i\n  %a")
            ("d" "Diary" entry (file+olp+datetree "~/Sync/orgmod/diary.org.gpg")
             "* %?\nEntered on %U\n %i\n %a")
            ("r" "Reading" entry (file+headline "~/Sync/orgmod/reading.org" "Reading")
             "* %?\n  %i\n  %a")
            ("t" "Todo" entry (file+headline "~/Sync/orgmod/gtd.org" "Tasks")
             "* TODO %?\n  %i\n  %a")))
    (require 'org-tempo)))

(use-package org-download
  :ensure t
  :bind ("C-S-y" . org-download-screenshot)
  :config
  (add-hook 'dired-mode-hook 'org-download-enable))

(auto-image-file-mode 1)

(with-eval-after-load "ol"
  (dolist (scheme '("http" "https"))
    (org-link-set-parameters
     scheme :follow (lambda (url arg) (eww (concat scheme ":" url) arg)))))

(with-eval-after-load "eww"
  (define-key eww-mode-map "f" 'eww-lnum-follow)
  (define-key eww-mode-map "F" 'eww-lnum-universal))

(autoload 'notmuch "notmuch" "notmuch mail" t)

(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))

;; EasyPG
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

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t))

(custom-set-variables
 '(package-selected-packages
   '(lsp-pyright anaconda-mode py-isort blacken pyenv-mode flycheck elpy pyim which-key restclient quickrun protobuf-mode move-dup markdown-mode iedit format-all ellama company)))

(custom-set-faces
 )

;;;;; Python Mode

(use-package elpy
  :ensure t
  :pin melpa
  :init
  (add-hook 'python-mode-hook #'elpy-mode)
  (add-hook 'python-ts-mode-hook #'elpy-mode))

;; flycheck 语法检查
(add-hook 'elpy-mode-hook #'flycheck-mode)
(add-hook 'python-ts-mode-hook #'flycheck-mode)

;; blacken 自动格式化
(use-package blacken
  :ensure t
  :hook (before-save . blacken-buffer))

;; py-isort 自动排序导入
(use-package py-isort
  :ensure t
  :hook (before-save . py-isort-before-save))

(use-package anaconda-mode
  :ensure t
  :hook ((python-mode python-ts-mode) . anaconda-mode)
  :config
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  (add-hook 'python-ts-mode-hook 'anaconda-eldoc-mode))

;; 推荐使用 lsp-pyright 替代 lsp-python-ms
(use-package lsp-pyright
  :ensure t
  :hook ((python-mode python-ts-mode) . lsp-deferred))

;; pyenv
(setenv "WORKON_HOME" "/home/tom/miniconda3/envs/")

(provide 'custom)

;;; custom.el ends here
