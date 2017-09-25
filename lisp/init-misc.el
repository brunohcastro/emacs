;;----------------------------------------------------------------------------
;; Misc config - yet to be placed in separate files
;;----------------------------------------------------------------------------

(add-auto-mode 'tcl-mode "Portfile\\'")
(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'prog-mode-hook 'goto-address-prog-mode)
(setq goto-address-mail-face 'link)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(add-hook 'after-save-hook 'sanityinc/set-mode-for-new-scripts)

(defun sanityinc/set-mode-for-new-scripts ()
  "Invoke `normal-mode' if this file is a script and in `fundamental-mode'."
  (and
   (eq major-mode 'fundamental-mode)
   (>= (buffer-size) 2)
   (save-restriction
     (widen)
     (string= "#!" (buffer-substring (point-min) (+ 2 (point-min)))))
   (normal-mode)))


(setq-default regex-tool-backend 'perl)
(after-load 're-builder
  ;; Support a slightly more idiomatic quit binding in re-builder
  (define-key reb-mode-map (kbd "C-c C-k") 'reb-quit))

(add-auto-mode 'conf-mode "Procfile")

(maybe-require-package 'editorconfig)
(maybe-require-package 'adaptive-wrap)
(editorconfig-mode 1)

(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(require 'doc-view)

(add-hook 'doc-view-mode-hook
          (lambda ()
            (editorconfig-mode -1)
            (whitespace-mode -1)
            (linum-mode -1)
            (projectile-mode -1)
            (company-mode -1)
            (whole-line-or-region-local-mode -1)
            (whitespace-cleanup-mode -1)
            (fci-mode -1)))

(defun comment-or-uncomment-current-line-or-region ()
  "Comments or uncomments current current line or whole lines in region."
  (interactive)
  (save-excursion
    (let (min max)
      (if (region-active-p)
          (setq min (region-beginning) max (region-end))
        (setq min (point) max (point)))
      (comment-or-uncomment-region
       (progn (goto-char min) (line-beginning-position))
       (progn (goto-char max) (line-end-position))))))

(global-set-key (kbd "C-c u") 'comment-or-uncomment-current-line-or-region)

(maybe-require-package 'lorem-ipsum)

(provide 'init-misc)
