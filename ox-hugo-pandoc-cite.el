;;; ox-hugo-pandoc-cite.el --- Pandoc Citations support for ox-hugo -*- lexical-binding: t -*-

;; Authors: Kaushal Modi <kaushal.mod@gmail.com>
;; URL: https://ox-hugo.scripter.co

;;; Commentary:

;; *This is NOT a stand-alone package.*
;;
;; It is used by ox-hugo to add support for parsing Pandoc Citations.

;;; Code:

;; TODO: Change the defconst to defvar
(defconst ox-hugo-pandoc-cite-pandoc-args-list
  '("--filter" "pandoc-citeproc"
    "-f" "markdown"
    "-t" "markdown-citations"
    "--atx-headers"     ;Use "# foo" style heading for output markdown
    "--standalone")     ;Include meta-data at the top
  "Pandoc arguments used in `ox-hugo-pandoc-cite-run-pandoc'.

These arguments are added to the `pandoc' call in addition to the
\"--bibliography\", output file and input file arguments in that
function.")

(defun ox-hugo-pandoc-cite-run-pandoc (outfile bib-list)
  "Run the `pandoc' process.

OUTFILE is the Org exported file name.

BIB-LIST is a list of one or more bibliography files."
  (let ((bib-args (mapcar (lambda (bib-file)
                            (concat "--bibliography="
                                    bib-file))
                          bib-list)))
    ;; TODO: Figure out how to transfer the error in the below
    ;; `call-process' to the user.
    (apply 'call-process
           (append
            '("pandoc" nil " *Pandoc Parse Citations*" :display)
            ox-hugo-pandoc-cite-pandoc-args-list
            bib-args
            `("-o" ,outfile ,outfile)))   ;-o <OUTPUT FILE> <INPUT FILE>
    ;; TODO: Figure out how to transfer the error in the below
    ;; `start-process' to the user.
    ;; (start-process "pandoc-parse-citations" " *Pandoc Parse Citations*"
    ;;                "pandoc"
    ;;                "--filter" "pandoc-citeproc"
    ;;                "--from=markdown"
    ;;                "--to=markdown-citations"
    ;;                "--atx-headers" ;Use "# foo" style heading for output markdown
    ;;                "--standalone"  ;Include meta-data at the top
    ;;                (concat "--output=" outfile) ;Output file
    ;;                outfile)                     ;Input file
    ))

(defun ox-hugo-pandoc-cite--parse-citations-maybe (info outfile)
  "Check if Pandoc needs to be run to parse citations.

INFO is a plist used as a communication channel.

OUTFILE is the Org exported file name."
  ;; (message "pandoc citations keyword: %S"
  ;;          (org-hugo--plist-get-true-p info :hugo-pandoc-citations))
  ;; (message "pandoc citations prop: %S"
  ;;          (org-entry-get nil "EXPORT_HUGO_PANDOC_CITATIONS" :inherit))
  (when (and outfile
             (or (org-entry-get nil "EXPORT_HUGO_PANDOC_CITATIONS" :inherit)
                 (org-hugo--plist-get-true-p info :hugo-pandoc-citations)))
    (unless (executable-find "pandoc")
      (user-error "[ox-hugo] pandoc executable not found in PATH"))
    (ox-hugo-pandoc-cite--parse-citations info outfile)))

(defun ox-hugo-pandoc-cite--parse-citations (info outfile)
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
        (ox-hugo-pandoc-cite-run-pandoc outfile bib-list)
      (message "[ox-hugo-pandoc-cite] No bibliography file was specified"))))


(provide 'ox-hugo-pandoc-cite)

;;; ox-hugo-pandoc-cite.el ends here
