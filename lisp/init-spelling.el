(require 'ispell)

(when (executable-find ispell-program-name)
  (require 'init-flyspell))

(setq ispell-dictionary "brasileiro")

(provide 'init-spelling)
