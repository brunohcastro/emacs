(maybe-require-package 'auctex)
(maybe-require-package 'latex-extra)
(maybe-require-package 'latex-math-preview)
(maybe-require-package 'latex-preview-pane)
(maybe-require-package 'latex-pretty-symbols)

(setq-default TeX-master nil)

(setq LaTeX-item-indent 0
      LaTeX-eqnarray-label "eq"
      LaTeX-equation-label "eq"
      LaTeX-figure-label "fig"
      LaTeX-table-label "tab"
      LaTeX-myChapter-label "chap"
      TeX-PDF-mode t
      TeX-auto-save t
      TeX-parse-self t
      TeX-save-query nil
      TeX-source-correlate-mode t
      TeX-source-correlate-start-server t
      TeX-electric-sub-and-superscript nil
      TeX-newline-function 'reindent-then-newline-and-indent
      reftex-plug-into-AUCTeX t)

(setq LaTeX-section-hook
      '(LaTeX-section-heading
        LaTeX-section-title
        LaTeX-section-toc
        LaTeX-section-section
        LaTeX-section-label)
      TeX-view-program-selection
      '((output-pdf "Zathura")
        (mode-io-correlate "Zathura")))

(setq LaTeX-math-abbrev-prefix "'"
      LaTeX-math-list '(("v l"  "leftrightarrow" "Arrows" 8596)
                        ("v d"  "vDash" "Relational" 8872)
                        ("v n"  "varnothing" ("AMS" "Misc") 8709)
                        ("n"    "neg" "Misc Symbol" 172)
                        ("m"    "models" "Relational" 8871)
                        ("v v"  "vee" "Binary Op" 8744)         ;; #X2228
                        ("v s"  "simeq" "Relational" 8771)      ;; #X2228
                        ("w"    "wedge" "Binary Op" 8743)))

(defvar LaTeX-no-autofill-environments
  '("equation"
    "equation*"
    "includegraphics")
  "A list of LaTeX environment names in which `auto-fill-mode' should be inhibited.")

(defun run-latexmk ()
  "Run external latexmk tool."
  (interactive)
  (let ((TeX-save-query nil)
        (master-file (TeX-master-file)))
    (TeX-save-document "")
    (TeX-run-TeX "latexmk"
                 (TeX-command-expand
                  "latexmk -pdflatex='pdflatex -file-line-error -synctex=1' -pdf %s -halt-on-error" 'TeX-master-file)
                 master-file)))

(defun master-run-latexmk ()
  "Only run-latexmk if buffer has a master."
  (interactive)
  (if (not (eq latexmk-on-save nil))
      (run-latexmk)))

(defun insert-region (name start end)
  "Insert NAME markup region from START to END."
  (save-excursion
    (goto-char end) (insert "}")
    (goto-char start) (insert (concat "\\" name "{"))
    )
  )

(defun red-region (start end)
  "Insert red region from START to END."
  (interactive "r")
  (insert-region "red" start end)  )

(defun blue-region (start end)
  "Insert blue region from START to END."
  (interactive "r")
  (insert-region "blue" start end))

(defun text-region (start end)
  "Insert text region from START to END."
  (interactive "r")
  (insert-region "text" start end))

(defun textbf-region (start end)
  "Insert bold region from START to END."
  (interactive "r")
  (insert-region "textbf" start end))

(defun ul-region (start end)
  "Insert underline region from START to END."
  (interactive "r")
  (insert-region "underline" start end))

(defun LaTeX-fill-and-space ()
  "Run LaTeX-fill-paragraph and restore a space for the next line."
  (just-one-space)
  (delete-char -1)
  (LaTeX-fill-paragraph)
  (insert " "))

(defun LaTeX-auto-fill-by-sentences ()
  "Fill and add space if prepending to the end of a sentence."
  (if (looking-back (concat sentence-end-base " +\\(.*\\)?") (point-at-bol))
      (LaTeX-fill-and-space)
    (do-auto-fill)))

(defun LaTeX-auto-fill-function ()
  "This function check whether point is currently inside one of the LaTeX environments listed in `LaTeX-no-autofill-environments'. If so, it inhibits automatic filling of the current paragraph."
  (let ((do-auto-fill t)
        (current-environment "")
        (level 0))
    (while (and do-auto-fill (not (string= current-environment "document")))
      (setq level (1+ level)
            current-environment (LaTeX-current-environment level)
            do-auto-fill (not (member current-environment LaTeX-no-autofill-environments))))
    (when do-auto-fill
      (LaTeX-auto-fill-by-sentences))))

(defun LaTeX-setup-auto-fill ()
  "This function turn on `auto-fill-mode' and set the function used to fill a paragraph to `LaTeX-auto-fill-function'."
  (auto-fill-mode 1)
  (setq normal-auto-fill-function 'LaTeX-auto-fill-function))

(defadvice LaTeX-fill-region-as-paragraph (around LaTeX-sentence-filling)
  "Start each sentence on a new line."
  (let ((from (ad-get-arg 0))
        (to-marker (set-marker (make-marker) (ad-get-arg 1)))
        tmp-end)
    (while (< from (marker-position to-marker))
      (forward-sentence)
      ;; might have gone beyond to-marker --- use whichever is smaller:
      (ad-set-arg 1 (setq tmp-end (min (point) (marker-position to-marker))))
      ad-do-it
      (ad-set-arg 0 (setq from (point)))
      (unless (or
               (bolp)
               (looking-at "\\s *$"))
        (just-one-space)
        (delete-char -1)
        (LaTeX-newline)))
    (set-marker to-marker nil)))

(ad-activate 'LaTeX-fill-region-as-paragraph)

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (require 'reftex)
            (local-set-key (kbd "C-c f") 'find-file-at-point)
            (local-set-key (kbd "C-c e") 'TeX-next-error)
            (local-set-key (kbd "C-c E") 'TeX-previous-error)
            (local-set-key (kbd "C-c C-i") 'LaTeX-indent-line)
            (local-set-key (kbd "C-c C-a C-r") 'red-region)
            (local-set-key (kbd "C-c C-a C-g") 'blue-region)
            (local-set-key (kbd "C-c C-a C-t") 'text-region)
            (local-set-key (kbd "C-c C-a C-b") 'textbf-region)
            (local-set-key (kbd "C-c C-a C-u") 'ul-region)
            (setq fill-column 80)
            (setq latexmk-on-save t)
            (setq truncate-lines t)
            (add-hook 'after-save-hook #'master-run-latexmk nil 'make-it-local)
            (LaTeX-setup-auto-fill)
            (LaTeX-math-mode 1)
            (TeX-fold-mode 1)
            (TeX-source-correlate-mode 1)
            (hs-minor-mode 1)
            (flyspell-mode 1)
            (flyspell-buffer)
            (reftex-mode 1)))

(add-hook 'latex-mode-hook 'turn-on-reftex)

(provide 'init-latex)
