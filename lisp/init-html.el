(require-package 'tidy)
(add-hook 'html-mode-hook (lambda () (tidy-build-menu html-mode-map)))

(require-package 'tagedit)
(after-load 'tagedit-mode
  (add-hook 'tagedit-mode-hook
            (lambda ()
              (local-set-key (kbd "M-<right>") 'tagedit-forward-slurp-tag)
              (local-set-key (kbd "C-)") 'tagedit-forward-slurp-tag)
              (local-set-key (kbd "M-<left>") 'tagedit-forward-barf-tag)
              (local-set-key (kbd "C-}") 'tagedit-forward-barf-tag)
              (local-set-key (kbd "M-r") 'tagedit-raise-tag)
              (local-set-key (kbd "M-s") 'tagedit-splice-tag)
              (local-set-key (kbd "M-S") 'tagedit-split-tag)
              (local-set-key (kbd "M-J") 'tagedit-join-tags)
              (local-set-key (kbd "M-?") 'tagedit-convolute-tags)
              (local-set-key (kbd "M-'") 'tagedit-goto-tag-content)
              (local-set-key (kbd "C-c C-<backspace>") 'te/kill-current-tag)
              (local-set-key (kbd "C-%") 'te/goto-tag-match)
              (local-set-key (kbd "C-^") 'te/goto-tag-begging)
              (local-set-key (kbd "C-$") 'te/goto-tag-end)
              (local-set-key (kbd "s-k") 'tagedit-kill-attribute)
              (local-set-key (kbd "s-<return>") 'tagedit-toggle-multiline-tag)
              (after-load 'sgml-mode
                (add-hook 'sgml-mode-hook
                          (lambda () (tagedit-mode 1)))))))

(add-auto-mode 'html-mode "\\.\\(jsp\\|tmpl\\)\\'")

;; Note: ERB is configured in init-ruby-mode

(provide 'init-html)
