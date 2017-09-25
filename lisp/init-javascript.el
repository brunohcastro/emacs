;;; init-javascript.el --- Javascript and tools configuration

;;; Commentary:

;;; Code:


;; Flycheck config for JS

(require 'flycheck)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-mode 'javascript-eslint 'rjsx-mode)

;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  "Use project eslint instead of global."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)


;; Tern plugin

(maybe-require-package 'tern)
(maybe-require-package 'company-tern)
(require 'tern)

(add-hook 'js2-mode-hook (lambda ()
                           (company-mode 1)
                           (tern-mode 1)))

(eval-after-load 'company '(add-to-list 'company-backends 'company-tern))

;; Leave the jumping around to xref

(define-key tern-mode-keymap (kbd "M-.") nil)
(define-key tern-mode-keymap (kbd "M-,") nil)
(define-key tern-mode-keymap (kbd "C-c C-.") 'tern-find-definition)
(define-key tern-mode-keymap (kbd "C-c C-,") 'tern-pop-find-definition)


;; General JS Config

(maybe-require-package 'prettier-js)
(maybe-require-package 'json-mode)

(defcustom preferred-javascript-mode
  (first (remove-if-not #'fboundp '(js2-mode js-mode)))
  "Javascript mode to use for .js files."
  :type 'symbol
  :group 'programming
  :options '(js2-mode js-mode))

(defconst preferred-javascript-indent-level 2)
(setq js-switch-indent-offset 2)

;; Need to first remove from list if present, since elpa adds entries too, which
;; may be in an arbitrary order
(eval-when-compile (require 'cl))
(setq auto-mode-alist (cons `("\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'" . ,preferred-javascript-mode)
                            (loop for entry in auto-mode-alist
                                  unless (eq preferred-javascript-mode (cdr entry))
                                  collect entry)))


;; JS2-mode

(maybe-require-package 'js2-mode)

;; Change some defaults: customize them to override
(setq-default js2-basic-offset 2
              js2-bounce-indent-p nil)
(after-load 'js2-mode
  ;; Disable js2 mode's syntax error highlighting by default...
  (setq-default js2-mode-show-parse-errors nil
                js2-mode-show-strict-warnings nil)
  ;; ... but enable it if flycheck can't handle javascript
  (autoload 'flycheck-get-checker-for-buffer "flycheck")
  (defun sanityinc/disable-js2-checks-if-flycheck-active ()
    (unless (flycheck-get-checker-for-buffer)
      (set (make-local-variable 'js2-mode-show-parse-errors) t)
      (set (make-local-variable 'js2-mode-show-strict-warnings) t)))
  (add-hook 'js2-mode-hook 'sanityinc/disable-js2-checks-if-flycheck-active)

  (add-hook 'js2-mode-hook (lambda () (setq mode-name "JS2")))

  (after-load 'js2-mode
    (js2-imenu-extras-setup)))

;; js-mode
(setq-default js-indent-level preferred-javascript-indent-level)

(add-to-list 'interpreter-mode-alist (cons "node" preferred-javascript-mode))


;; The Silver Searcher

(when (and (executable-find "ag")
           (maybe-require-package 'xref-js2))
  (after-load 'js2-mode
    (define-key js2-mode-map (kbd "M-.") nil)
    (add-hook 'js2-mode-hook
              (lambda () (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))))


;;; Coffeescript

(maybe-require-package 'coffee-mode)

(after-load 'coffee-mode
  (setq coffee-js-mode preferred-javascript-mode
        coffee-tab-width preferred-javascript-indent-level))

(when (fboundp 'coffee-mode)
  (add-to-list 'auto-mode-alist '("\\.coffee\\.erb\\'" . coffee-mode)))

;; ---------------------------------------------------------------------------
;; Run and interact with an inferior JS via js-comint.el
;; ---------------------------------------------------------------------------

(when (maybe-require-package 'js-comint)
  (setq inferior-js-program-command "js")

  (defvar inferior-js-minor-mode-map (make-sparse-keymap))
  (define-key inferior-js-minor-mode-map "\C-x\C-e" 'js-send-last-sexp)
  (define-key inferior-js-minor-mode-map "\C-\M-x" 'js-send-last-sexp-and-go)
  (define-key inferior-js-minor-mode-map "\C-cb" 'js-send-buffer)
  (define-key inferior-js-minor-mode-map "\C-c\C-b" 'js-send-buffer-and-go)
  (define-key inferior-js-minor-mode-map "\C-cl" 'js-load-file-and-go)

  (define-minor-mode inferior-js-keys-mode
    "Bindings for communicating with an inferior js interpreter."
    nil " InfJS" inferior-js-minor-mode-map)

  (dolist (hook '(js2-mode-hook js-mode-hook))
    (add-hook hook 'inferior-js-keys-mode)))

;; ---------------------------------------------------------------------------
;; Alternatively, use skewer-mode
;; ---------------------------------------------------------------------------

(when (maybe-require-package 'skewer-mode)
  (after-load 'skewer-mode
    (add-hook 'skewer-mode-hook
              (lambda () (inferior-js-keys-mode -1)))))


;; Typescript

(maybe-require-package 'typescript-mode)
(maybe-require-package 'tide)

(defun setup-tide-mode ()
  "Setup tide mode variables."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

;;(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

(when (maybe-require-package 'add-node-modules-path)
  (after-load 'typescript-mode
    (add-hook 'typescript-mode-hook 'add-node-modules-path))
  (after-load 'js2-mode
    (add-hook 'js2-mode-hook 'add-node-modules-path)))

(setq typescript-indent-level 2)
(setq typescript-expr-indent-offset 0)

(setq company-tooltip-align-annotations t)

;; this overrides the default typescript-mode indentations and uses the one
;; from the js-mode package, which seems to be more accurate.
(require 'js)
(advice-add 'typescript--proper-indentation :override 'js--proper-indentation)


;; Angular.io config

(maybe-require-package 'ng2-mode)

(defvar ng2-html-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c ,") 'ng2-html-goto-binding)
    (define-key map (kbd "C-c .") 'ng2-open-counterpart)
    map)
  "Keymap for ng2-html-mode.")

(defvar ng2-ts-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c .") 'ng2-open-counterpart)
    map)
  "Keymap for ng2-ts-mode.")


;; React config

(maybe-require-package 'rjsx-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))

(defadvice js-jsx-indent-line (after js-jsx-indent-line-after-hack activate)
  "Workaround `sgml-mode' and follow airbnb component style."
  (save-excursion
    (let* ((cur-line (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position))))
      (goto-char (line-beginning-position))
      (if (string-match "^\\( +\\)\/?> *$" cur-line)
          (let* ((empty-spaces (match-string 1 cur-line)))
            (when
                (re-search-forward empty-spaces
                                   (line-end-position))
              (replace-match (make-string (- (length empty-spaces) sgml-basic-offset) 32)))
            )))))

(provide 'init-javascript)

;;; init-javascript.el ends here
