(require 'mu4e)
(require 'smtpmail)
(require 'org-mu4e)
(maybe-require-package 'mu4e-maildirs-extension)

(global-set-key (kbd "C-x C-d") 'mu4e)

(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(setq smtpmail-debug-info t)
(setq smtpmail-smtp-service 587)
(setq smtpmail-stream-type 'starttls)

(setq mu4e-maildir "~/Mail")
(setq mu4e-view-show-images t)
(setq mu4e-view-prefer-html t)
(setq mu4e-image-max-width 800)
(setq mu4e-headers-results-limit 100)
(setq mu4e-headers-include-related t)
(setq mu4e-headers-skip-duplicates t)
(setq mu4e-html2text-command "w3m -dump -cols 80 -T text/html")
(setq mu4e-get-mail-command "offlineimap -q")
(setq httextml-mu4e-convert-to-html t)
(setq message-send-mail-function 'smtpmail-send-it
      send-mail-function 'smtpmail-send-it)

(setq mu4e-user-mail-address-list '("brunohcastro@gmail.com"
                                    "brunohcastro@hotmail.com"
                                    "brunohcastro@icloud.com"
                                    "bruno@dastro.com.br"))

(add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)
(add-to-list 'mu4e-view-actions '("gopen in gmail" . djr/mu4e-open-message-in-google) t)

(add-hook 'mu4e-compose-mode-hook (lambda ()
                                    (visual-line-mode -1)
                                    (turn-on-auto-fill)))
(setq mu4e-contexts
      `(
        ,(make-mu4e-context
          :name "Gmail"
          :enter-func (lambda () (mu4e-message "Switch to the Gmail context"))
          :match-func (lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches msg :to "brunohcastro@gmail.com")))
          :vars '((user-mail-address              . "brunohcastro@gmail.com")
                  (user-full-name                 . "Bruno Castro")
                  (mu4e-compose-signature         . (concat "Bruno H. de Castro\n"
                                                            "Desenvolvedor | CSL Soluções em Informática\n"
                                                            "brunohcastro@gmail.com\n"
                                                            "+55 (84) 99927-4646"))
                  (mu4e-sent-folder               . "/brunohcastro.gmail.com/[Gmail].Sent Mail")
                  (mu4e-drafts-folder             . "/brunohcastro.gmail.com/[Gmail].Drafts")
                  (mu4e-trash-folder              . "/brunohcastro.gmail.com/[Gmail].Trash")
                  (smtpmail-smtp-server           . "smtp.gmail.com")
                  ))
        ,(make-mu4e-context
          :name "Dastro"
          :enter-func (lambda () (mu4e-message "Switch to the Dastro context"))
          :match-func (lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches msg :to "bruno@dastro.com.br")))
          :vars '((user-mail-address              . "bruno@dastro.com.br" )
                  (user-full-name                 . "Bruno H. de Castro" )
                  (mu4e-compose-signature         . (concat "Bruno H. de Castro\n"
                                                            "Diretor | Dastro Tecnologia\n"
                                                            "bruno@dastro.com.br\n"
                                                            "+55 (84) 99927-4646"))
                  (mu4e-sent-folder               . "/bruno.dastro.com.br/[Gmail].Enviados")
                  (mu4e-drafts-folder             . "/bruno.dastro.com.br/[Gmail].Rascunhos")
                  (mu4e-trash-folder              . "/bruno.dastro.com.br/[Gmail].Lixeira")
                  (smtpmail-smtp-server           . "smtp.gmail.com")
                  ))
        ,(make-mu4e-context
          :name "Hotmail"
          :enter-func (lambda () (mu4e-message "Switch to the Hotmail context"))
          ;; leave-fun not defined
          :match-func (lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches msg :to "brunohcastro@hotmail.com")))
          :vars '((user-mail-address              . "brunohcastro@hotmail.com" )
                  (user-full-name                 . "Bruno H. de Castro" )
                  (mu4e-compose-signature         . (concat "Bruno H. de Castro\n"
                                                            "Desenvolvedor | CSL Soluções em Informática\n"
                                                            "Estudante Engenharia Elétrica | UFRN\n"
                                                            "brunohcastro@hotmail.com\n"
                                                            "+55 (84) 99927-4646"))
                  (mu4e-sent-folder               . "/brunohcastro.hotmail.com/Sent")
                  (mu4e-drafts-folder             . "/brunohcastro.hotmail.com/Drafts")
                  (mu4e-trash-folder              . "/brunohcastro.hotmail.com/Deleted")
                  (smtpmail-smtp-server           . "smtp-mail.outlook.com")
                  ))
        ,(make-mu4e-context
          :name "iCloud"
          :enter-func (lambda () (mu4e-message "Switch to the iCloud context"))
          ;; leave-fun not defined
          :match-func (lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches msg :to "brunohcastro@iCloud.com")))
          :vars '((user-mail-address        . "brunohcastro@iCloud.com" )
                  (user-full-name           . "Bruno Castro" )
                  (mu4e-compose-signature   . "Bruno Castro" )
                  (mu4e-sent-folder         . "/brunohcastro.icloud.com/Sent")
                  (mu4e-drafts-folder       . "/brunohcastro.icloud.com/Drafts")
                  (mu4e-trash-folder        . "/brunohcastro.icloud.com/Deleted Messages")
                  (smtpmail-smtp-server . "smtp.mail.me.com")
                  ))))

(defun mu4e-update-inbox (run-in-background)
  "Get a new mail by running `mu4e-get-mail-command'.
If RUN-IN-BACKGROUND is non-nil (or called with prefix-argument), run
in the background; otherwise, pop up a window."
  (interactive "P")
  (when (and (buffer-live-p mu4e~update-buffer)
             (process-live-p (get-buffer-process mu4e~update-buffer)))
    (mu4e-error "Update process is already running"))
  (run-hooks 'mu4e-update-pre-hook)
  (unless mu4e-get-mail-command
    (mu4e-error "`mu4e-get-mail-command' is not defined"))

  (let* ((process-connection-type t)
         (proc (start-process-shell-command
                "mu4e-update" " *mu4e-update*"
                "offlineimap -f INBOX"))
         (buf (process-buffer proc))
         (win (or run-in-background
                  (mu4e~temp-window buf mu4e~update-buffer-height))))
    (setq mu4e~update-buffer buf)
    (when (window-live-p win)
      (with-selected-window win
        ;; ;;(switch-to-buffer buf)
        ;; (set-window-dedicated-p win t)
        (erase-buffer)
        (insert "\n") ;; FIXME -- needed so output start
        (mu4e~update-mail-mode)))
    (setq mu4e~progress-reporter
          (unless mu4e-hide-index-messages
            (make-progress-reporter
             (mu4e-format "Retrieving mail..."))))
    (set-process-sentinel proc 'mu4e~update-sentinel-func)
    ;; if we're running in the foreground, handle password requests
    (unless run-in-background
      (process-put proc 'x-interactive (not run-in-background))
      (set-process-filter proc 'mu4e~get-mail-process-filter))))


(defun djr/mu4e-open-message-in-google (msg)
  (let* ((msgid (mu4e-message-field msg :message-id))
         (url (concat "https://mail.google.com/mail/u/0/?shva=1#search/rfc822msgid%3A"
                      (url-encode-url msgid))))
    (start-process "" nil "open" url)))

(provide 'init-mail)
