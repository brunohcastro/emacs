(maybe-require-package 'helm)
(maybe-require-package 'helm-gitlab)
(maybe-require-package 'helm-ag)
(maybe-require-package 'helm-swoop)
(maybe-require-package 'helm-projectile)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-'") 'helm-resume)
(global-set-key (kbd "C-c s") 'helm-swoop)
(global-set-key (kbd "C-c f") 'helm-recentf)

(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)
(define-key isearch-mode-map (kbd "C-o") 'helm-occur-from-isearch)

(helm-mode 1)
(helm-projectile-on)

;; Define default window size
(setq helm-display-buffer-default-height nil)

(provide 'init-helm)
