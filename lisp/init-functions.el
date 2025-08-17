;;; init-functions.el --- provides some useful functions. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'subr-x)

;;;###autoload
(defun efs/display-startup-time ()
  "Show statistics for the startup time.
If `before-init-time' or `after-init-time' aren't available, fall back
to `emacs-init-time'."
  (interactive)
  (if (and (boundp 'before-init-time) (boundp 'after-init-time))
      (message "Emacs loaded in %s with %d garbage collections."
               (format "%.2f seconds"
                       (float-time (time-subtract after-init-time before-init-time)))
               gcs-done)
    ;; fallback if variables are not available for some reason
    (message "Emacs initialized: %s" (emacs-init-time))))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;;; Theme helpers --------------------------------------------------------------

(defun efs--disable-all-themes ()
  "Disable all currently enabled themes."
  (mapc #'disable-theme custom-enabled-themes))

;;;###autoload
(defun dark-theme ()
  "Activate a dark theme (Modus Vivendi) after disabling other themes."
  (interactive)
  (efs--disable-all-themes)
  (when (not (functionp #'load-theme))
    (user-error "load-theme is not available"))
  (load-theme 'modus-vivendi t)
  (message "Activated dark theme: modus-vivendi"))

;;;###autoload
(defun light-theme ()
  "Activate a light theme (Modus Operandi) after disabling other themes."
  (interactive)
  (efs--disable-all-themes)
  (when (not (functionp #'load-theme))
    (user-error "load-theme is not available"))
  (load-theme 'modus-operandi t)
  (message "Activated light theme: modus-operandi"))

;;; Preferences / quick open ---------------------------------------------------

;;;###autoload
(defun preferences ()
  "Open the user's init file (`user-init-file`)."
  (interactive)
  (let ((file (or user-init-file (locate-user-emacs-file "init.el"))))
    (if file
        (find-file file)
      (user-error "No user init file found"))))

;;;###autoload
(defun preference-custom ()
  "Open the user's custom file (`custom-file`). If `custom-file' is nil,
falls back to `user-init-file'."
  (interactive)
  (let ((file (or custom-file user-init-file (locate-user-emacs-file "init.el"))))
    (if file
        (find-file file)
      (user-error "No custom/user init file found"))))

;;; Input methods --------------------------------------------------------------

;;;###autoload
(defun input-chinese-methods ()
  "Load and toggle the Chinese input method helpers from init-input-methods.
This will `require' the `init-input-methods' feature (if present) then
call `toggle-input-method'. Gives a friendly message if the helper file
is not available."
  (interactive)
  (condition-case err
      (progn
        (require 'init-input-methods)
        (if (fboundp 'toggle-input-method)
            (toggle-input-method)
          (user-error "`toggle-input-method' is not available")))
    (error
     (message "Could not enable Chinese input methods: %s" (error-message-string err)))))

;;; Tree-sitter helpers -------------------------------------------------------

(defcustom efs/treesit-default-languages
  '(bash c go gomod html java javascript json markdown python rust typescript yaml)
  "Default list of languages to install grammars for when using
`treesit-install-language-grammar-all'."
  :type '(repeat symbol)
  :group 'convenience)

;;;###autoload
(defun treesit-install-language-grammar-all (&optional langs)
  "Install tree-sitter grammars for LANGS (a list of symbols).
If called interactively prompt for languages (comma separated).  If LANGS
is nil use `efs/treesit-default-languages'.  If `treesit-install-language-grammar'
is not available, inform the user."
  (interactive
   (list
    (let* ((input (read-string (format "Languages (comma separated) [%s]: "
                                       (string-join (mapcar #'symbol-name efs/treesit-default-languages) ", "))))
           (trimmed (string-trim input)))
      (if (string-empty-p trimmed)
          nil
        (mapcar #'intern (mapcar #'string-trim (split-string trimmed ",")))))))
  (if (not (fboundp #'treesit-install-language-grammar))
      (user-error "Tree-sitter grammar installer (`treesit-install-language-grammar') is not available in this Emacs")
    (let ((to-install (or langs efs/treesit-default-languages)))
      (dolist (lang to-install)
        (condition-case err
            (progn
              (message "Installing treesit grammar for %s..." lang)
              (treesit-install-language-grammar lang)
              (message "Installed treesit grammar for %s" lang))
          (error
           (message "Failed to install %s: %s" lang (error-message-string err))))))))

(provide 'init-functions)

;;; init-functions.el ends here
