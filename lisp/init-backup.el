(defvar backup-directory (concat user-emacs-directory "backups/"))

(if (not (file-exists-p backup-directory))
    (make-directory backup-directory t))

;; Set the backup and autosave function to use the backup directory
(setq backup-directory-alist `((".*" . ,backup-directory)))

;; (defun my-backup-file-name (fpath)
;;   "Return a new file path of a given file FPATH.
;; If the new path's directories does not exist, create them."
;;   (let* (
;;          (backupRootDir backup-directory)
;;          (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path, for example, “C:”
;;          (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") ))
;;          )
;;     (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
;;     backupFilePath
;;     )
;;   )
;; (setq make-backup-file-name-function 'my-backup-file-name)

(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth (file-attributes file))))
                  week))
      (message "%s" file)
      (delete-file file))))

(setq auto-save-file-name-transforms `((".*" ,backup-directory t)))
(setq create-lockfiles nil)

;; Define the other options
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )

(provide 'init-backup)
