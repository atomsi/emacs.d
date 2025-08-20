;;; package --- Summary:
;;; this is personal emacs configure file, it includes
;;; 1. normal settings
;;; 2. org mode
;;; 3. python mode
;;; 4. rust mode

;;; Commentary:

;;; Code:

;; 推荐 setopt（Emacs 30+）
(setopt browse-url-browser-function 'browse-url-generic)
(setopt browse-url-generic-program "google-chrome")


(use-package ellama
  :ensure t
  :bind ("C-c e" . ellama)
  ;; send last message in chat buffer with C-c C-c
  :hook (org-ctrl-c-ctrl-c-final . ellama-chat-send-last-message)
  :init
  ;; setup key bindings
  ;; (setopt ellama-keymap-prefix "C-c e")
  ;; language you want ellama to translate to
  (setopt ellama-language "Chinese")
  ;; could be llm-openai for example
  (require 'llm-ollama)
  (setopt ellama-provider
          (make-llm-ollama
           :chat-model "qwen3:14b"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("num_ctx" . 40000))))
  (setopt ellama-summarization-provider
          (make-llm-ollama
           :chat-model "qwen3:14b"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("num_ctx" . 40000))))
  (setopt ellama-coding-provider
          (make-llm-ollama
           :chat-model "qwen3-coder:30b-a3b-q8_0"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("num_ctx" . 256000))))
  ;; Naming new sessions with llm
  (setopt ellama-naming-provider
          (make-llm-ollama
           :chat-model "qwen3:14b"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params '(("stop" . ("\n")))))
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
  ;; Translation llm provider
  (setopt ellama-translation-provider
          (make-llm-ollama
           :chat-model "qwen3:14b"
           :embedding-model "nomic-embed-text"
           :default-chat-non-standard-params
           '(("num_ctx" . 40000))))
  (setopt ellama-extraction-provider (make-llm-ollama
                                      :chat-model "qwen3-coder:30b-a3b-q8_0"
                                      :embedding-model "dengcao/Qwen3-Embedding-4B:Q8_0"
                                      :default-chat-non-standard-params
                                      '(("num_ctx" . 256000))))
  ;; customize display buffer behaviour
  ;; see ~(info "(elisp) Buffer Display Action Functions")~
  (setopt ellama-chat-display-action-function #'display-buffer-full-frame)
  (setopt ellama-instant-display-action-function #'display-buffer-at-bottom)
  :config
  ;; show ellama context in header line in all buffers
  (ellama-context-header-line-global-mode +1)
  ;; show ellama session id in header line in all buffers
  (ellama-session-header-line-global-mode +1)
  ;; handle scrolling events
  (advice-add 'pixel-scroll-precision :before #'ellama-disable-scroll)
  (advice-add 'end-of-buffer :after #'ellama-enable-scroll))

			 
  ;; Naming new sessions with llm
  (setopt ellama-naming-provider
  	  (make-llm-ollama
  	   :chat-model "qwen3:14b"
  	   :embedding-model "nomic-embed-text"
  	   :default-chat-non-standard-params '(("stop" . ("\n")))))
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
  ;; Translation llm provider
  (setopt ellama-translation-provider
  	  (make-llm-ollama
  	   :chat-model "qwen3:14b"
  	   :embedding-model "nomic-embed-text"
  	   :default-chat-non-standard-params
  	   '(("num_ctx" . 32768))))
  (setopt ellama-extraction-provider (make-llm-ollama
  				      :chat-model "qwen3-coder:30b-a3b-q8_0"
  				      :embedding-model "nomic-embed-text"
  				      :default-chat-non-standard-params
  				      '(("num_ctx" . 32768))))
  ;; customize display buffer behaviour
  ;; see ~(info "(elisp) Buffer Display Action Functions")~
  (setopt ellama-chat-display-action-function #'display-buffer-full-frame)
  (setopt ellama-instant-display-action-function #'display-buffer-at-bottom)
  :config
  ;; show ellama context in header line in all buffers
  (ellama-context-header-line-global-mode +1)
  ;; show ellama session id in header line in all buffers
  (ellama-session-header-line-global-mode +1)
  ;; handle scrolling events
  (advice-add 'pixel-scroll-precision :before #'ellama-disable-scroll)
  (advice-add 'end-of-buffer :after #'ellama-enable-scroll))


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
  (setopt pyim-default-scheme 'quanpin)
  :config
  (global-set-key (kbd "C-\\") 'toggle-input-method))

;;;;; org-mode 配置
(use-package org
  :ensure t
  :init
  (setopt org-image-actual-width nil
          org-startup-indented t
          org-log-done 'note
          org-todo-keywords
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
    (add-hook 'org-mode-hook (lambda () (setopt truncate-lines nil)))
    ;; 修正 org-capture-templates
    (setopt org-capture-templates
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
