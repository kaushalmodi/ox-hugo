;;; ox-hugo-pandoc-cite.el --- Pandoc Citations support for ox-hugo -*- lexical-binding: t -*-

;; Authors: Kaushal Modi <kaushal.mod@gmail.com>
;; URL: https://ox-hugo.scripter.co

;;; Commentary:

;; *This is NOT a stand-alone package.*
;;
;; It is used by ox-hugo to add support for parsing Pandoc Citations.

;;; Code:

(require 'org)

(declare-function org-hugo--plist-get-true-p "ox-hugo")
(declare-function org-hugo--front-matter-value-booleanize "ox-hugo")

(defcustom org-hugo-pandoc-cite-references-heading "References {#references}"
  "Markdown title for Pandoc inserted references section."
  :group 'org-export-hugo
  :type 'string)

(defvar org-hugo--fm-yaml)              ;Silence byte-compiler

(defvar org-hugo-pandoc-cite-pandoc-args-list
  `("-f" "markdown"
    "-t" ,(concat "markdown-citations"
                  "-simple_tables"
                  "+pipe_tables"
                  "-raw_attribute"
                  "-fenced_divs"
                  "-fenced_code_attributes"
                  "-bracketed_spans")
    "--markdown-headings=atx"
    "--id-prefix=fn:"
    "--citeproc")
  "Pandoc arguments used in `org-hugo-pandoc-cite--run-pandoc'.

-f markdown : Convert *from* Markdown

-t markdown : Convert *to* Markdown
  -citations : Remove the \"citations\" extension.  This will cause
               citations to be expanded instead of being included as
               markdown citations.

  -simple_tables : Remove the \"simple_tables\" style.

  +pipe_tables : Add the \"pipe_tables\" style insted that Blackfriday
                 understands.

  -fenced_divs : Do not replace HTML <div> tags with Pandoc fenced
                 divs \":::\".

  -fenced_code_attributes : Create fenced code blocks like
                 \"``` lang .. ```\" instead of \"``` {.lang} .. ```\".

  -bracketed_spans : Do not replace HTML <span> tags with Pandoc
                 bracketed class notation \"{.some-class}\".

--atx-headers : Use \"# foo\" style heading for output markdown.

--id-prefix=fn: : Create footnote ID's like \"[^fn:1]\" instead of
                  \"[^1]\" to be consistent with default ox-hugo
                  exported Markdown footnote style.

These arguments are added to the `pandoc' call in addition to the
\"--bibliography\", output file (\"-o\") and input file
arguments.")

(defvar org-hugo-pandoc-cite-pandoc-meta-data
  '("nocite" "csl" "link-citations")
  "List of meta-data fields specific to Pandoc.")

(defvar org-hugo-pandoc-cite--run-pandoc-buffer "*Pandoc Citations*"
  "Buffer to contain the `pandoc' run output and errors.")

(defvar org-hugo-pandoc-cite--references-header-regexp
  "^<div id=\"refs\" class=\"references[^>]+>"
  "Regexp to match the Pandoc-inserted references header string.

This string is present only if Pandoc has resolved one or more
references.

Pandoc 2.11.4.")

(defvar org-hugo-pandoc-cite--reference-entry-regexp
  "^<div id=\"ref-[^\"]+\" .*csl-entry[^>]+>"
  "Regexp to match the Pandoc-inserted reference entry strings.

Pandoc 2.11.4.")

(defun org-hugo-pandoc-cite--restore-fm-in-orig-outfile (orig-outfile fm &optional orig-full-contents)
  "Restore the intended front-matter format in ORIG-OUTFILE.

ORIG-OUTFILE is the Org exported file name.

FM is the intended front-matter format.

ORIG-FULL-CONTENTS is a string of ORIG-OUTFILE contents.  If this
is nil it is created in this function.

If FM is already in YAML format, this function doesn't do
anything.  Otherwise, the YAML format front-matter in
ORIG-OUTFILE is replaced with TOML format."
  (unless (string= fm org-hugo--fm-yaml)
    (unless orig-full-contents
      (setq orig-full-contents (with-temp-buffer
                                 (insert-file-contents orig-outfile)
                                 (buffer-substring-no-properties
                                  (point-min) (point-max)))))
    (setq fm (org-hugo-pandoc-cite--remove-pandoc-meta-data fm))
    (let* ((orig-contents-only
            (replace-regexp-in-string
             ;; The `orig-contents-only' will always be in YAML.
             ;; Delete that first.
             "\\`---\n\\(.\\|\n\\)+\n---\n" "" orig-full-contents))
           (toml-fm-plus-orig-contents (concat fm orig-contents-only)))
      ;; (message "[ox-hugo-pandoc-cite] orig-contents-only: %S" orig-contents-only)
      (write-region toml-fm-plus-orig-contents nil orig-outfile))))

(defun org-hugo-pandoc-cite--run-pandoc (orig-outfile bib-list)
  "Run the `pandoc' process and return the generated file name.

ORIG-OUTFILE is the Org exported file name.

BIB-LIST is a list of one or more bibliography files."
  ;; First kill the Pandoc run buffer if already exists (from a
  ;; previous run).
  (when (get-buffer org-hugo-pandoc-cite--run-pandoc-buffer)
    (kill-buffer org-hugo-pandoc-cite--run-pandoc-buffer))
  (let* ((pandoc-outfile (make-temp-file ;ORIG_FILE_BASENAME.RANDOM.md
                          (concat (file-name-base orig-outfile) ".")
                          nil ".md"))
         (bib-args (mapcar (lambda (bib-file)
                             (concat "--bibliography="
                                     bib-file))
                           bib-list))
         (pandoc-arg-list (append
                           org-hugo-pandoc-cite-pandoc-args-list
                           bib-args
                           `("-o" ,pandoc-outfile ,orig-outfile))) ;-o <OUTPUT FILE> <INPUT FILE>
         (pandoc-arg-list-str (mapconcat #'identity pandoc-arg-list " "))
         exit-code)
    (message (concat "[ox-hugo] Post-processing citations using Pandoc command:\n"
                     "  pandoc " pandoc-arg-list-str))

    (setq exit-code (apply 'call-process
                           (append
                            `("pandoc" nil
                              ,org-hugo-pandoc-cite--run-pandoc-buffer :display)
                            pandoc-arg-list)))

    (unless (= 0 exit-code)
      (user-error (format "[ox-hugo] Pandoc execution failed. See the %S buffer"
                          org-hugo-pandoc-cite--run-pandoc-buffer)))
    pandoc-outfile))

(defun org-hugo-pandoc-cite--remove-pandoc-meta-data (fm)
  "Remove Pandoc meta-data from front-matter string FM and return it.

The list of Pandoc specific meta-data is defined in
`org-hugo-pandoc-cite-pandoc-meta-data'."
  (with-temp-buffer
    (insert fm)
    (goto-char (point-min))
    (let ((regexp (format "^%s\\(:\\| =\\) "
                          (regexp-opt org-hugo-pandoc-cite-pandoc-meta-data 'words))))
      (delete-matching-lines regexp))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun org-hugo-pandoc-cite--fix-pandoc-output (content loffset info)
  "Fix the Pandoc output CONTENT and return it.

LOFFSET is the heading level offset.

Required fixes:

- Prepend Pandoc inserted \"references\" class div with
  `org-hugo-pandoc-cite-references-heading'.

- When not using Goldmark (Hugo v0.60.0+), add the Blackfriday
  required \"<div></div>\" hack to Pandoc divs with \"ref\" id's.

- Unescape the Hugo shortcodes: \"{{\\\\=< shortcode \\\\=>}}\" ->
  \"{{< shortcode >}}\"

INFO is a plist used as a communication channel."
  (with-temp-buffer
    (insert content)
    (let ((case-fold-search nil))
      (goto-char (point-min))

      ;; Prepend the Pandoc inserted "references" class div with
      ;; `org-hugo-pandoc-cite-references-heading' heading in Markdown.
      (save-excursion
        ;; There should be at max only one replacement needed for
        ;; this.
        (when (re-search-forward org-hugo-pandoc-cite--references-header-regexp nil :noerror)
          (let ((references-heading ""))
            (when (org-string-nw-p org-hugo-pandoc-cite-references-heading)
              (let ((level-mark (make-string (+ loffset 1) ?#)))
                (setq references-heading (concat level-mark " " org-hugo-pandoc-cite-references-heading))))
            (replace-match (concat references-heading "\n\n\\&"
                                   (unless (org-hugo--plist-get-true-p info :hugo-goldmark)
                                     "\n  <div></div>\n")))))) ;See footnote 1

      ;; Add the Blackfriday required hack to Pandoc ref divs.
      (unless (org-hugo--plist-get-true-p info :hugo-goldmark)
        (save-excursion
          (while (re-search-forward org-hugo-pandoc-cite--reference-entry-regexp nil :noerror)
            (replace-match "\\&\n  <div></div>")))) ;See footnote 1

      ;; Fix Hugo shortcodes.
      (save-excursion
        (let ((regexp (concat "{{\\\\<"
                              "\\(\\s-\\|\n\\)+"
                              "\\(?1:[[:ascii:][:nonascii:]]+?\\)"
                              "\\(\\s-\\|\n\\)+"
                              "\\\\>}}")))
          (while (re-search-forward regexp nil :noerror)
            (let* ((sc-body (match-string-no-properties 1))
                   (sc-body-no-newlines (replace-regexp-in-string "\n" " " sc-body))
                   ;; Remove all backslashes except for the one
                   ;; preceding double-quotes, like in:
                   ;;   {{< figure src="nested-boxes.svg" caption="<span class=\"figure-number\">Figure 1: </span>
                   ;;   PlantUML generated figure showing nested boxes" >}}
                   (sc-body-no-backlash (replace-regexp-in-string
                                         "\"\"" "\\\\\\\\\""
                                         (replace-regexp-in-string
                                          (rx "\\" (group anything)) "\\1" sc-body-no-newlines))))
              (replace-match (format "{{< %s >}}" sc-body-no-backlash) :fixedcase)))))

      ;; Fix square bracket. \[ abc \] -> [ abc ]
      (save-excursion
        (let ((regexp (concat
                       "\\\\\\["
                       "\\(.+\\)"
                       "\\\\\\]")))
          (while (re-search-forward regexp nil :noerror)
            (let* ((sc-body (match-string-no-properties 1)))
              ;; (message "square bracket [%s]" sc-body)
              (replace-match (format "[%s]" sc-body) :fixedcase)))))

      (buffer-substring-no-properties (point-min) (point-max)))))

(defun org-hugo-pandoc-cite--parse-citations (info orig-outfile)
  "Parse Pandoc Citations in ORIG-OUTFILE and update that file.

INFO is a plist used as a communication channel.

ORIG-OUTFILE is the Org exported file name."
  (let ((bib-list (let ((bib-raw
                         (org-string-nw-p
                          (or (org-entry-get nil "EXPORT_BIBLIOGRAPHY" :inherit)
                              (format "%s" (plist-get info :bibliography))))))
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
                        ;; - Remove duplicate bibliographies.
                        (delete-dups
                         (mapcar (lambda (bib-file)
                                   (let ((fname (org-trim bib-file)))
                                     (unless (file-exists-p fname)
                                       (user-error "[ox-hugo] Bibliography file %S does not exist"
                                                   fname))
                                     fname))
                                 bib-list-1)))))))
    (if bib-list
        (let ((fm (plist-get info :front-matter))
              (loffset (string-to-number
                        (or (org-entry-get nil "EXPORT_HUGO_LEVEL_OFFSET" :inherit)
                            (plist-get info :hugo-level-offset))))
              (pandoc-outfile (org-hugo-pandoc-cite--run-pandoc orig-outfile bib-list)))
          ;; (message "[ox-hugo parse citations] fm :: %S" fm)
          ;; (message "[ox-hugo parse citations] loffset :: %S" loffset)
          ;; (message "[ox-hugo parse citations] pandoc-outfile :: %S" pandoc-outfile)

          (let* ((pandoc-outfile-contents (with-temp-buffer
                                            (insert-file-contents pandoc-outfile)
                                            (buffer-substring-no-properties
                                             (point-min) (point-max))))
                 (content-has-references (string-match-p
                                          org-hugo-pandoc-cite--references-header-regexp
                                          pandoc-outfile-contents)))
            ;; Prepend the original ox-hugo generated front-matter to
            ;; Pandoc output, only if the Pandoc output contains
            ;; references.
            (if content-has-references
                (let* ((contents-fixed (org-hugo-pandoc-cite--fix-pandoc-output
                                        pandoc-outfile-contents loffset info))
                       (fm (org-hugo-pandoc-cite--remove-pandoc-meta-data fm))
                       (fm-plus-content (concat fm "\n" contents-fixed)))
                  (write-region fm-plus-content nil orig-outfile))
              (org-hugo-pandoc-cite--restore-fm-in-orig-outfile orig-outfile fm)
              (message (concat "[ox-hugo] Using the original Ox-hugo output instead "
                               "of Pandoc output as it contained no References"))))
          (delete-file pandoc-outfile)

          (with-current-buffer org-hugo-pandoc-cite--run-pandoc-buffer
            (if (> (point-max) 1)             ;buffer is not empty
                (message
                 (format
                  (concat "[ox-hugo] See the %S buffer for possible Pandoc warnings.\n"
                          "          Review the exported Markdown file for possible missing citations.")
                  org-hugo-pandoc-cite--run-pandoc-buffer))
              ;; Kill the Pandoc run buffer if it is empty.
              (kill-buffer org-hugo-pandoc-cite--run-pandoc-buffer))))
      (message "[ox-hugo] No bibliography file was specified"))))

(defun org-hugo-pandoc-cite--parse-citations-maybe (info)
  "Check if Pandoc needs to be run to parse citations; and run it.

INFO is a plist used as a communication channel."
  ;; (message "pandoc citations keyword: %S"
  ;;          (org-hugo--plist-get-true-p info :hugo-pandoc-citations))
  ;; (message "pandoc citations prop: %S"
  ;;          (org-entry-get nil "EXPORT_HUGO_PANDOC_CITATIONS" :inherit))
  (let* ((orig-outfile (plist-get info :outfile))
         (fm (plist-get info :front-matter))
         (has-nocite (string-match-p "^nocite\\(:\\| =\\) " fm))
         (orig-outfile-contents (with-temp-buffer
                                  (insert-file-contents orig-outfile)
                                  (buffer-substring-no-properties
                                   (point-min) (point-max))))
         ;; http://pandoc.org/MANUAL.html#citations
         ;; Each citation must have a key, composed of `@' + the
         ;; citation identifier from the database, and may optionally
         ;; have a prefix, a locator, and a suffix. The citation key
         ;; must begin with a letter, digit, or _, and may contain
         ;; alphanumerics, _, and internal punctuation characters
         ;; (:.#$%&-+?<>~/).
         ;; A minus sign (-) before the @ will suppress mention of the
         ;; author in the citation.
         (valid-citation-key-char-regexp "a-zA-Z0-9_:.#$%&+?<>~/-")
         (citation-key-regexp (concat "[^" valid-citation-key-char-regexp "]"
                                      "\\(-?@[a-zA-Z0-9_]"
                                      "[" valid-citation-key-char-regexp "]+\\)"))
         (has-@ (string-match-p citation-key-regexp orig-outfile-contents)))
    ;; Either the nocite front-matter should be there, or the
    ;; citation keys should be present in the `orig-outfile'.
    (if (or has-nocite has-@)
        (progn
          (unless (executable-find "pandoc")
            (user-error "[ox-hugo] pandoc executable not found in PATH"))
          (org-hugo-pandoc-cite--parse-citations info orig-outfile))
      (org-hugo-pandoc-cite--restore-fm-in-orig-outfile
       orig-outfile fm orig-outfile-contents))))

(defun org-hugo-pandoc-cite--meta-data-generator (data)
  "Return YAML front-matter to pass citation meta-data to Pandoc.

DATA is the alist containing all the post meta-data for
front-matter generation.

Pandoc accepts `csl', `nocite' and `link-citations' variables via
a YAML front-matter.

References:
- https://pandoc.org/MANUAL.html#citation-rendering
- https://pandoc.org/MANUAL.html#including-uncited-items-in-the-bibliography
- https://pandoc.org/MANUAL.html#other-relevant-metadata-fields"
  (let* ((yaml ())
         (link-citations (cdr (assoc 'link-citations data)))
         (link-citations (if (symbolp link-citations)
                             (symbol-name link-citations)
                           link-citations))
         (csl (cdr (assoc 'csl data)))
         (nocite (cdr (assoc 'nocite data))))
    (push "---" yaml)
    (when link-citations
      (push (format "link-citations: %s"
                    (org-hugo--front-matter-value-booleanize link-citations))
            yaml))
    (when csl
      (push (format "csl: %S" csl) yaml))
    (when nocite
      (push (format "nocite: [%s]"
                    (string-join
                     (mapcar (lambda (elem)
                               (format "%S" (symbol-name elem)))
                             nocite)
                     ", "))
            yaml))
    (push "---\n" yaml)
    ;; (message "[org-hugo-pandoc-cite--meta-data-generator DBG] yaml: %S" yaml)
    (string-join (nreverse yaml) "\n")))


(provide 'ox-hugo-pandoc-cite)



;;; Footnotes

;;;; Footnote 1
;; The empty HTML element tags like "<div></div>" is a hack to get
;; around a Blackfriday limitation.  Details:
;; https://github.com/kaushalmodi/ox-hugo/issues/93.


;;; ox-hugo-pandoc-cite.el ends here
