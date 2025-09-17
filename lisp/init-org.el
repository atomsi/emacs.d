;;; init-org.el  --- Linux + Emacs 30 专用，无兼容、无自动建目录 -*- lexical-binding: t; -*-

(require 'url-handlers)                 ; 抑制 org-download 编译警告

(use-package org
  :ensure t
  :defer t
  :hook (org-mode . (lambda ()
                      (setq truncate-lines nil)
                      (abbrev-mode 1)))
  :custom
  (org-startup-indented t)
  (org-log-done 'note)
  (org-todo-keywords '((sequence "TODO(t!)" "WAIT(w)" "|" "DONE(d!)" "CANCELED(c@/!)")))
  (org-plantuml-jar-path
   (or (let* ((exe (executable-find "plantuml"))
              (jar (and exe (expand-file-name "plantuml.jar"
                                              (file-name-directory exe)))))
         (and (file-exists-p jar) jar))
       "~/.emacs.d/plugin/plantuml.jar"))
  (org-babel-results-keyword "results")

  :config
  ;; 添加 keybinding 到 config 部分
  (define-key org-mode-map (kbd "×") (kbd "*"))
  (define-key org-mode-map (kbd "－") (kbd "-"))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (ditaa . t)
     (python . t)
     (shell . t)
     (latex . t)
     (plantuml . t)
     (dot . t)
     (lisp . t)
     (org . t)
     (java . t)))

  (require 'org-tempo)

  (global-set-key (kbd "C-c c") #'org-capture)
  (setq org-capture-templates
        '(("i" "Idea"  entry (file+headline "~/Sync/orgmod/idea.org" "Idea")
           "* %?\n  %i\n  %a")
          ("d" "Diary" entry (file+olp+datetree "~/Sync/orgmod/diary.org.gpg")
           "* %?\nEntered on %U\n %i\n %a")
          ("r" "Reading" entry (file+headline "~/Sync/orgmod/reading.org" "Reading")
           "* %?\n  %i\n  %a")
          ("t" "Todo" entry (file+headline "~/Sync/orgmod/gtd.org" "Tasks")
           "* TODO %?\n  %i\n  %a")))

  (defun atom/display-inline-images ()
    (condition-case nil
        (org-display-inline-images)
      (error nil)))
  (add-hook 'org-babel-after-execute-hook #'atom/display-inline-images))

;; ------------------------------------------------------------------
;; org-download
;; ------------------------------------------------------------------
(use-package org-download
  :ensure t
  :bind (("C-S-y" . org-download-screenshot))
  :hook (dired-mode . org-download-enable))

(defun my-org-setup-inline-images ()
  (org-display-inline-images))
(add-hook 'org-mode-hook #'my-org-setup-inline-images)
(advice-add 'org-download-image :after #'org-display-inline-images)

(setq org-confirm-babel-evaluate nil)

;; ------------------------------------------------------------------
;; PlantUML 骨架缩写
;; ------------------------------------------------------------------
(define-skeleton skel-org-block-plantuml
  "Insert a org plantuml block, querying for filename."
  "File (no extension): "
  "#+begin_src plantuml :file " str ".png :cache yes :cmdline -charset UTF-8\n"
  "@startuml\n"
  "title **" str "**\n"
  "autonumber\n"
  "@enduml\n"
  "#+end_src\n")
(with-eval-after-load 'org
  (define-abbrev org-mode-abbrev-table "spuml" "" 'skel-org-block-plantuml))

;; ------------------------------------------------------------------
;; org-roam + org-roam-ui
;; ------------------------------------------------------------------
(use-package org-roam
  :ensure t
  :defer t
  :bind (("C-c r f" . org-roam-node-find)
         ("C-c r i" . org-roam-node-insert)
         ("C-c r c" . org-roam-capture)
         ("C-c r b" . org-roam-buffer-toggle)
         ("C-c r d" . org-roam-dailies-map))
  :custom
  (org-roam-directory "~/Sync/orgmod/roam")
  (org-roam-dailies-directory "daily/")
  (org-roam-db-gc-threshold (* 128 1024 1024))
  :config
  (require 'org-roam-dailies)
  (setq org-roam-database-connector 'sqlite-builtin)
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t))

;; ------------------------------------------------------------------
;; EasyPG
;; ------------------------------------------------------------------
(require 'epa-file)
(setq epa-file-name-regexp "\\.org\\.gpg\\'")
(epa-file-enable)

;; ------------------------------------------------------------------
;; pdf-tools
;; ------------------------------------------------------------------
(with-eval-after-load 'pdf-tools
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode)))

(provide 'init-org)
;;; init-org.el ends here
