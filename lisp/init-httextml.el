(require 'mu4e)

(defun httextml-mu4e-mime-multipart (plain html &optional images)
  "Create a multipart/alternative with text/plain and text/html alternatives.
If the html portion of the message includes images, wrap the html
and images in a multipart/related part."
  (concat "<#multipart type=alternative><#part type=text/plain>"
          plain
          (when images "<#multipart type=related>")
          "<#part type=text/html>"
          html
          images
          (when images "<#/multipart>\n")
          "<#/multipart>\n"))

(defun htmling (begin end)
  (save-excursion
    (save-restriction
      (goto-char begin)
      (narrow-to-region begin end)
      (goto-char (point-min))
      (replace-string "&" "&amp;")
      (goto-char (point-min))
      (replace-string "<" "&lt;")
      (goto-char (point-min))
      (replace-string ">" "&gt;")
      (goto-char (point-min))
      (forward-paragraph -1)
      (delete-blank-lines)
      (while (and (not (eobp)) (not (looking-at "\\s *$")))
        (unless (looking-at "^\>+")
          (insert "<p>")
          (forward-paragraph 1)
          (insert "</p>")
          (unless (eobp)
            (delete-blank-lines)
            (delete-char 1))
          )
        )
      ))
  (buffer-substring begin (point-max))
  )

(defun markdown-to-html (filename body)
  (write-region body nil filename)
  (shell-command-to-string (concat "/usr/bin/markdown " filename))
  )

(defun httextml-mu4e-mime-convert-to-html ()
  "Convert the current body to html."
  (let* ((begin
          (save-excursion
            (goto-char (point-min))
            (search-forward mail-header-separator)))
         (end (point-max))
         (raw-body (buffer-substring begin end))
         (tmp-file (make-temp-name (expand-file-name "mail"
                                                     temporary-file-directory)))
         (mail-body (markdown-to-html tmp-file raw-body)))
    (delete-region begin (point-max))
    (delete-file tmp-file nil)
    (save-excursion
      (goto-char begin)
      (newline)
      (insert (httextml-mu4e-mime-multipart
               raw-body mail-body)))))


(defvar httextml-mu4e-convert-to-html nil
  "Whether to do automatic org-mode => html conversion when sending messages.")

(defun httextml-mu4e-mime-convert-to-html-maybe ()
  "Convert to html if `httextml-mu4e-convert-to-html' is non-nil.
This function is called when sending a message (from
`message-send-hook') and, if non-nil, will send the message as
the rich-text version of the what is assumed to be an org-mode
body."
  (when httextml-mu4e-convert-to-html
    (mu4e-message "Converting to html")
    (httextml-mu4e-mime-convert-to-html)))

(add-hook 'mu4e-compose-mode-hook (lambda ()
                                    (add-hook 'message-send-hook 'httextml-mu4e-mime-convert-to-html-maybe nil t)))


(provide 'init-httextml)
