;;; init-org.el --- -*- lexical-binding: t; -*-
(use-package org
  :ensure t
  :defer t
  :hook (org-mode . (lambda () (setq truncate-lines nil)))
  :custom
  (org-image-actual-width nil)
  (org-startup-indented t)
  (org-log-done 'note)
  (org-todo-keywords '((sequence "TODO(t!)" "WAIT(w)" "|" "DONE(d!)" "CANCELED(c@/!)")))
  (org-plantuml-jar-path (or (executable-find "plantuml")
                             "/usr/share/plantuml/plantuml.jar"))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t)
     (plantuml . t)
     (dot . t)))

  (require 'org-tempo)

  (setopt org-capture-templates
          '(("i" "Idea"  entry (file+headline "~/Sync/orgmod/idea.org" "Idea")
             "* %?\n  %i\n  %a")
            ("d" "Diary" entry (file+olp+datetree "~/Sync/orgmod/diary.org.gpg")
             "* %?\nEntered on %U\n %i\n %a")
            ("r" "Reading" entry (file+headline "~/Sync/orgmod/reading.org" "Reading")
             "* %?\n  %i\n  %a")
            ("t" "Todo" entry (file+headline "~/Sync/orgmod/gtd.org" "Tasks")
             "* TODO %?\n  %i\n  %a"))))

(use-package org-download
  :ensure t
  :bind (("C-S-y" . org-download-screenshot))
  :hook (dired-mode . org-download-enable))

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

;; EasyPG
(require 'epa-file)
(setq epa-file-name-regexp  "\\.org\\.gpg\\'")
(epa-file-enable)

;; 打开 pdf 用 pdf-view-mode
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))

(provide 'init-org)
;;; init-org.el ends here
