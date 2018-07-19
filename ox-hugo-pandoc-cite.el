;;; ox-hugo-pandoc-cite.el --- Pandoc Citations support for ox-hugo -*- lexical-binding: t -*-

;; Authors: Kaushal Modi <kaushal.mod@gmail.com>
;; URL: https://ox-hugo.scripter.co

;;; Commentary:

;; *This is NOT a stand-alone package.*
;;
;; It is used by ox-hugo to add support for parsing Pandoc Citations.

;;; Code:

;; TODO: Change the defconst to defvar
(defvar org-hugo-pandoc-cite-pandoc-args-list
  '("-f" "markdown"
    "-t" "markdown-citations-simple_tables+pipe_tables"
    "--atx-headers")
  "Pandoc arguments used in `org-hugo-pandoc-cite--run-pandoc'.

-f markdown: Convert *from* Markdown

-t markdown: Convert *to* Markdown
  -citations: Remove the \"citations\" extension.  This will cause
              citations to be expanded instead of being included as
              markdown citations.

  -simple_tables: Remove the \"simple_tables\" style.

  +pipe_tables: Add the \"pipe_tables\" style insted that Blackfriday
                understands.

--atx-headers: Use \"# foo\" style heading for output markdown.

These arguments are added to the `pandoc' call in addition to the
\"--bibliography\", output file (\"-o\") and input file
arguments.")

(defvar org-hugo-pandoc-cite-pandoc-meta-data
  '("nocite" "csl")
  "List of meta-data fields specific to Pandoc.")

(defvar org-hugo-pandoc-cite--run-pandoc-buffer "*Pandoc Citations*"
  "Buffer to contain the `pandoc' run output and errors.")

(defun org-hugo-pandoc-cite--run-pandoc (outfile bib-list)
  "Run the `pandoc' process and return the exit code.

OUTFILE is the Org exported file name.

BIB-LIST is a list of one or more bibliography files."
  (let* ((bib-args (mapcar (lambda (bib-file)
                             (concat "--bibliography="
                                     bib-file))
                           bib-list))
         (pandoc-arg-list (append
                           org-hugo-pandoc-cite-pandoc-args-list
                           bib-args
                           `("-o" ,outfile ,outfile))) ;-o <OUTPUT FILE> <INPUT FILE>
         (pandoc-arg-list-str (mapconcat #'identity pandoc-arg-list " ")))
    (message (concat "[ox-hugo] Post-processing citations using Pandoc command:\n"
                     "  pandoc " pandoc-arg-list-str))
    (apply 'call-process
           (append
            `("pandoc" nil ,org-hugo-pandoc-cite--run-pandoc-buffer :display)
            pandoc-arg-list))))         ;Return the exit code

(defun org-hugo-pandoc-cite--remove-pandoc-meta-data (fm)
  "Remove Pandoc meta-data from front-matter string FM and return it.

The list of Pandoc specific meta-data is defined in
`org-hugo-pandoc-cite-pandoc-meta-data'."
  (with-temp-buffer
    (insert fm)
    (goto-char (point-min))
    (dolist (field org-hugo-pandoc-cite-pandoc-meta-data)
      (let ((regexp (format "^%s\\(:\\| =\\) " (regexp-quote field))))
        (delete-matching-lines regexp)))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun org-hugo-pandoc-cite--fix-pandoc-output (content loffset)
  "Fix the Pandoc output CONTENT and return it.

Required fixes:

- Unescape the Hugo shortcodes: \"{{\\\\=< shortcode \\\\=>}}\" ->
  \"{{< shortcode >}}\".

- Replace \"::: {#refs .references}\" with \"## References\"
  where the number of hashes depends on HUGO_LEVEL_OFFSET,
  followed by an opening HTML div tag.

- Replace \"::: {#ref-someref}\" with \"<div
  id=\"ref-someref\">\".

- Replace \"^:::$\" with closing HTML div tags.

LOFFSET is the offset added to the base level of 1 for headings."
  (with-temp-buffer
    (insert content)
    (let ((case-fold-search nil)
          (level-mark (make-string (+ loffset 1) ?#)))
      (goto-char (point-min))
      ;; Fix Hugo shortcodes.
      (save-excursion
        (let ((regexp (concat "{{\\\\<"
                              "\\(\\s-\\|\n\\)+"
                              "\\(?1:.+?\\)"
                              "\\(\\s-\\|\n\\)+"
                              "\\\\>}}")))
          (while (re-search-forward regexp nil :noerror)
            (replace-match "{{< \\1 >}}" :fixedcase))))
      ;; Convert Pandoc ref ID style to HTML div's.
      (save-excursion
        (let ((regexp "^::: {#ref-\\(.+?\\)}$"))
          (while (re-search-forward regexp nil :noerror)
            (replace-match (concat "<div id=\"ref-\\1\">"
                                   "\n  <div></div>\n") ;See footnote 1
                           :fixedcase)
            (re-search-forward "^:::$")
            (replace-match "\n</div>"))))
      ;; Replace "::: {#refs .references}" with a base-level
      ;; "References" heading in Markdown, followed by an opening HTML
      ;; div tag.
      (save-excursion
        (let ((regexp "^::: {#refs \\.references}$"))
          ;; There should be one-and-only-one replacement needed for
          ;; this.
          (re-search-forward regexp nil :noerror)
          (replace-match (concat level-mark
                                 " References {#references}\n\n"
                                 "<div id=\"refs .references\">"
                                 "\n  <div></div>\n\n")) ;See footnote 1
          (re-search-forward "^:::$")
          (replace-match "\n\n</div> <!-- ending references -->")))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun org-hugo-pandoc-cite--parse-citations-maybe (info)
  "Check if Pandoc needs to be run to parse citations.

INFO is a plist used as a communication channel."
  ;; (message "pandoc citations keyword: %S"
  ;;          (org-hugo--plist-get-true-p info :hugo-pandoc-citations))
  ;; (message "pandoc citations prop: %S"
  ;;          (org-entry-get nil "EXPORT_HUGO_PANDOC_CITATIONS" :inherit))
  (let ((outfile (plist-get info :outfile)))
    (when (and outfile
               (or (org-entry-get nil "EXPORT_HUGO_PANDOC_CITATIONS" :inherit)
                   (org-hugo--plist-get-true-p info :hugo-pandoc-citations)))
      (unless (executable-find "pandoc")
        (user-error "[ox-hugo] pandoc executable not found in PATH"))
      (org-hugo-pandoc-cite--parse-citations info outfile))))

(defun org-hugo-pandoc-cite--parse-citations (info outfile)
  "Parse Pandoc Citations in OUTFILE and update that file.

INFO is a plist used as a communication channel.

OUTFILE is the Org exported file name."
  (let ((bib-list (let ((bib-raw
                         (org-string-nw-p
                          (or (org-entry-get nil "EXPORT_BIBLIOGRAPHY" :inherit)
                              (org-export-data (plist-get info :bibliography) info))))) ;`org-export-data' required
                    (when bib-raw
                      ;; Multiple bibliographies can be comma or
                      ;; newline separated. The newline separated
                      ;; bibliographies work only for the
                      ;; #+bibliography keyword; example:
                      ;;
                      ;;   #+bibliography: bibliographies-1.bib
                      ;;   #+bibliography: bibliographies-2.bib
                      ;;
                      ;; If using the subtree properties they need to
                      ;; be comma-separated (now don't use commas in
                      ;; those file names, you will suffer):
                      ;;
                      ;;   :EXPORT_BIBLIOGRAPHY: bibliographies-1.bib, bibliographies-2.bib
                      (let ((bib-list-1 (org-split-string bib-raw "[,\n]")))
                        ;; - Don't allow spaces around bib names.
                        ;; - Convert file names to absolute paths.
                        ;; - Remove duplicate bibliographies.
                        (delete-dups
                         (mapcar (lambda (bib-file)
                                   (let ((fname (file-truename
                                                 (org-trim
                                                  bib-file))))
                                     (unless (file-exists-p fname)
                                       (user-error "[ox-hugo-pandoc-cite] Bibliography file %S does not exist"
                                                   fname))
                                     fname))
                                 bib-list-1)))))))
    (if bib-list
        (let ((fm (plist-get info :front-matter))
              (loffset (string-to-number
                        (or (org-entry-get nil "EXPORT_HUGO_LEVEL_OFFSET" :inherit)
                            (plist-get info :hugo-level-offset))))
              (exit-code (org-hugo-pandoc-cite--run-pandoc outfile bib-list)))
          ;; (message "[ox-hugo parse citations] fm :: %S" fm)
          ;; (message "[ox-hugo parse citations] loffset :: %S" loffset)
          ;; (message "[ox-hugo parse citations] exit-code :: %S" exit-code)

          (unless (= 0 exit-code)
            (user-error (format "[ox-hugo] Pandoc execution failed. See the %S buffer"
                                org-hugo-pandoc-cite--run-pandoc-buffer)))

          ;; If no error has happened, we don't need the Pandoc run
          ;; buffer; kill it.
          (kill-buffer org-hugo-pandoc-cite--run-pandoc-buffer)

          ;; Prepend the original ox-hugo generated front-matter to
          ;; Pandoc output.
          (let* ((fm (org-hugo-pandoc-cite--remove-pandoc-meta-data fm))
                 (post-pandoc-contents (with-temp-buffer
                                         (insert-file-contents outfile)
                                         (buffer-substring-no-properties
                                          (point-min) (point-max))))
                 (contents-fixed (org-hugo-pandoc-cite--fix-pandoc-output post-pandoc-contents loffset))
                 (fm-plus-content (concat fm "\n" contents-fixed)))
            (write-region fm-plus-content nil outfile)))
      (message "[ox-hugo-pandoc-cite] No bibliography file was specified"))))


(provide 'ox-hugo-pandoc-cite)



;;; Footnotes

;;;; Footnote 1
;; The empty HTML element tags like "<div></div>" is a hack to get
;; around a Blackfriday limitation.  Details:
;; https://github.com/kaushalmodi/ox-hugo/issues/93.


;;; ox-hugo-pandoc-cite.el ends here
