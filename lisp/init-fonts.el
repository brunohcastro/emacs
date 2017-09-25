;;; Changing font sizes and faces

(require-package 'default-text-scale)
(add-hook 'after-init-hook 'default-text-scale-mode)

(add-to-list 'default-frame-alist '(font . "FantasqueSansMono Nerd Font-11"))
(set-face-attribute 'default t :font "FantasqueSansMono Nerd Font-11")

(defun sanityinc/maybe-adjust-visual-fill-column ()
  "Readjust visual fill column when the global font size is modified.
This is helpful for writeroom-mode, in particular."
  ;; TODO: submit as patch
  (if visual-fill-column-mode
      (add-hook 'after-setting-font-hook 'visual-fill-column--adjust-window nil t)
    (remove-hook 'after-setting-font-hook 'visual-fill-column--adjust-window t)))

(add-hook 'visual-fill-column-mode-hook
          'sanityinc/maybe-adjust-visual-fill-column)

(provide 'init-fonts)
