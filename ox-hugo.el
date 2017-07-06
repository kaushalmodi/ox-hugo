;;; ox-hugo.el --- Hugo Markdown Back-End for Org Export Engine  -*- lexical-binding: t -*-

;; Copyright (C) 2016 Helloyi He

;; Author: Helloyi He
;; Keywords: org, hugo, markdown

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements a Markdown back-end compatible with the
;; Hugo static site generator (https://gohugo.io/).

;;; Code:

(require 'ox-blackfriday)


;;; User-Configurable Variables

(defgroup org-export-hugo nil
  "Options for exporting Org mode files to Hugo-compatible Markdown."
  :tag "Org Export Hugo"
  :group 'org-export
  :version "25.2")

(defcustom org-hugo-front-matter-format "toml"
  "Format used to front matter.
This variable can be set to either \"toml\" or \"yaml\"."
  :group 'org-export-hugo
  :type '(choice
          (const :tag "TOML" "toml")
          (const :tag "YAML" "yaml")))

(defcustom org-hugo-default-section-directory "posts"
  "Default section for Hugo posts.

This variable is the name of the directory under the \"content/\"
directory where all Hugo posts should go by default."
  :group 'org-export-hugo
  :type 'directory
  :safe 'stringp)


;;; Define Back-End

(org-export-define-derived-backend 'hugo 'blackfriday
  :menu-entry
  '(?H "Export to Hugo-compatible Markdown"
       ((?H "Subtree to file             " (lambda (a _s v b) (org-hugo-export-to-md a :subtreep v)))
        (?h "To file" (lambda (a s v b) (org-hugo-export-to-md a s v)))
        (?T "Subtree to temporary buffer "
            (lambda (a _s v b) (org-hugo-export-as-md a :subtreep v)))
        (?t "To temporary buffer"
            (lambda (a s v b) (org-hugo-export-as-md a s v)))
        (?O "Subtree to file and open    "
            (lambda (a _s v b)
              (if a (org-hugo-export-to-md t :subtreep v)
                (org-open-file (org-hugo-export-to-md nil :subtreep v)))))
        (?o "To file and open"
            (lambda (a s v b)
              (if a (org-hugo-export-to-md t s v)
                (org-open-file (org-hugo-export-to-md nil s v)))))))
  :translate-alist '((src-block . org-hugo-src-block)
                     (link . org-hugo-link)
                     ;;(inner-template . org-hugo-inner-template)
                     (table . org-hugo-table))
  :filters-alist '((:filter-body . org-hugo-body-filter)
                   ;; (:filter-link . org-hugo-image-filter)
                   )

  ;;                KEY                       KEYWORD                    OPTION  DEFAULT                     BEHAVIOR
  :options-alist '((:hugo-front-matter-format "HUGO_FRONT_MATTER_FORMAT" nil     org-hugo-front-matter-format)
                   ;; Front matter variables - https://gohugo.io/content/front-matter/
                   ;; Required front matter variables
                   ;; "title" is parsed from the Org #+TITLE or the subtree heading.
                   ;; "date" is parsed from the Org #+DATE or subtree property EXPORT_HUGO_DATE
                   (:hugo-description "HUGO_DESCRIPTION" nil nil)
                   (:hugo-date "HUGO_DATE" nil nil)
                   (:hugo-categories "HUGO_CATEGORIES" nil nil)
                   (:hugo-tags "HUGO_TAGS" nil nil) ;TODO: Also parse the Org tags as post tags
                   (:date "DATE" nil nil)
                   (:keywords "KEYWORDS" nil nil 'space)
                   (:hugo-tags "HUGO_TAGS" nil nil 'space) ;TODO: Also parse the Org tags as post tags
                   ;; Optional front matter variables
                   (:hugo-aliases "HUGO_ALIASES" nil nil)
                   (:hugo-draft "HUGO_DRAFT" nil nil)
                   (:hugo-publishdate "HUGO_PUBLISHDATE" nil nil)
                   (:hugo-expirydate "HUGO_EXPIRYDATE" nil nil)
                   (:hugo-type "HUGO_TYPE" nil nil)
                   (:hugo-iscjklanguage "HUGO_ISCJKLANGUAGE" nil nil)
                   (:hugo-weight "HUGO_WEIGHT" nil nil)
                   (:hugo-markup "HUGO_MARKUP" nil nil)
                   (:hugo-slug "HUGO_SLUG" nil nil)
                   (:hugo-url "HUGO_URL" nil nil)
                   ;; Non-front-matter options
                   (:hugo-section "HUGO_SECTION" nil org-hugo-default-section-directory)
                   (:hugo-export-dir "HUGO_EXPORT_DIR" nil nil)
                   (:hugo-static-images "HUGO_STATIC_IMAGES" nil "images")))


;;; Transcode Functions

;;;; Source Blocks

(defun org-hugo-src-block (src-block _contents _info)
  "Convert SRC-BLOCK element to the Hugo `highlight' shortcode."
  (let* ((lang (org-element-property :language src-block))
         (code (org-element-property :value src-block))
         (shortcode (concat "{{< highlight " lang " >}}\n"))
         (close-shortcode "{{< /highlight >}}\n"))
    (concat shortcode code close-shortcode)))

;;;; Links

(defun org-hugo-link (link contents info)
  "Convert LINK to Markdown format.

CONTENTS is the link's description.
INFO is a plist used as a communication channel.

Unlike `org-md-link', this function will copy local images and
rewrite link paths to make blogging more seamless."
  (let ((link-org-files-as-md
	 (lambda (raw-path)
           ;; Treat links to `file.org' as links to `file.md'.
           (if (string= ".org" (downcase (file-name-extension raw-path ".")))
               (concat (file-name-sans-extension raw-path) ".md")
             raw-path)))
	(type (org-element-property :type link)))
    (message "[ox-hugo DBG] link filename is : %s" (expand-file-name (plist-get (car (cdr link)) :path)))
    (message "[ox-hugo DBG] link type is %s" type)
    (message "[ox-hugo DBG] link ")
    (cond
     ;; Link type is handled by a special function.
     ((org-export-custom-protocol-maybe link contents 'md))
     ((member type '("custom-id" "id" "fuzzy"))
      (let ((destination (if (string= type "fuzzy")
                             (org-export-resolve-fuzzy-link link info)
                           (org-export-resolve-id-link link info))))
	(pcase (org-element-type destination)
          (`plain-text			; External file.
           (let ((path (funcall link-org-files-as-md destination)))
             (if (not contents) (format "<%s>" path)
               (format "[%s](%s)" contents path))))
          (`headline
           (format
            "[%s](#%s)"
            ;; Description.
            (cond ((org-string-nw-p contents))
                  ((org-export-numbered-headline-p destination info)
                   (mapconcat #'number-to-string
                              (org-export-get-headline-number destination info)
                              "."))
                  (t (org-export-data (org-element-property :title destination)
                                      info)))
            ;; Reference.
            (or (org-element-property :CUSTOM_ID destination)
		(org-export-get-reference destination info))))
          (_
           (let ((description
                  (or (org-string-nw-p contents)
                      (let ((number (org-export-get-ordinal destination info)))
			(cond
			 ((not number) nil)
			 ((atom number) (number-to-string number))
			 (t (mapconcat #'number-to-string number ".")))))))
             (when description
               (format "[%s](#%s)"
                       description
                       (org-export-get-reference destination info))))))))
     ((org-export-inline-image-p link org-html-inline-image-rules)
      (message "org-hugo-link proccessing an image %s" contents)

      (let ((path (org-hugo--attachment-rewrite
                   (let ((raw-path (org-element-property :path link)))
                     (if (not (file-name-absolute-p raw-path)) raw-path
                       (expand-file-name raw-path))) info))
            (caption (org-export-data
                      (org-export-get-caption
                       (org-export-get-parent-element link)) info)))
	(format "![img](%s)"
		(if (not (org-string-nw-p caption)) path
                  (format "%s \"%s\"" path caption)))))
     ((string= type "coderef")
      (let ((ref (org-element-property :path link)))
	(format (org-export-get-coderef-format ref contents)
		(org-export-resolve-coderef ref info))))
     ((equal type "radio") contents)
     (t (let* ((raw-path (org-element-property :path link))
               (path
		(cond
		 ((member type '("http" "https" "ftp"))
                  (concat type ":" raw-path))
		 ((string= type "file")
                  (org-hugo--attachment-rewrite
                   (org-export-file-uri
                    (funcall link-org-files-as-md raw-path))info))
		 (t raw-path))))
          (if (not contents) (format "<%s>" path)
            (format "[%s](%s)" contents path)))))))

;;;;; Helpers

(defun org-hugo--attachment-rewrite (path info)
  "Copy local images and pdfs to the \"static/\" directory.
Also rewrite image links.

PATH is the path to the image or pdf attachment.
INFO is a plist used as a communication channel."
  (message "[ox-hugo DBG] The Hugo sectioimage dir is: %s" (plist-get info :hugo-static-images) )
  (message "[ox-hugo DBG] The Hugo section is: %s" (plist-get info :hugo-section) )
  (message "[ox-hugo DBG] The Hugo export dir is: %s" (plist-get info :hugo-export-dir) )

  (let* ((full-path (file-truename path))
         (exportables '("jpg" "jpeg" "tiff" "png" "pdf" "odt" ))
         (file-name (file-name-nondirectory path))
         (image-export-dir (concat
                            (file-name-as-directory (plist-get info :hugo-export-dir))
                            "static/"
                            (file-name-as-directory (plist-get info :hugo-static-images))
                            ))
         (exported-image (concat image-export-dir file-name)))
    (message "[ox-hugo DBG] Image export dir is: %s" image-export-dir)
    (if (and (file-exists-p full-path)
             (member (file-name-extension path) exportables)
             (file-directory-p image-export-dir))
        (progn
          (unless (file-exists-p exported-image)
            (copy-file full-path exported-image))
          (concat "/static/" (file-name-as-directory (plist-get info :hugo-static-images)) file-name))
      path
      )))


;;;; Hugo Front Matter

(defun org-hugo--wrap-string-in-quotes (str)
  "Wrap STR with double quotes and return the string."
  (cond ((string-empty-p str)
         "")
        ((and (string= (substring str 0 1) "\"")
              (string= (substring str -1) "\""))
         str)
        (t
         (concat "\"" str "\""))))

(defun org-hugo--get-front-matter (info)
  "Return the Hugo front matter string.

INFO is a plist used as a communication channel."
  (let* ((fm-format (org-export-data (plist-get info :hugo-front-matter-format) info))
         (hugo-date-fmt "%Y-%m-%dT%T%z")
         (date (or ;; Get the date from the subtree property `EXPORT_DATE' if available
                (org-string-nw-p (org-export-data (plist-get info :date) info))
                ;; Else try to get it from the #+DATE keyword in the Org file
                (org-string-nw-p (org-export-get-date info hugo-date-fmt))
                ;; Else finally set the date to the current date
                (format-time-string hugo-date-fmt (current-time))))
         (tags (concat
                (org-export-data (plist-get info :hugo-tags) info) " "
                (org-export-data (plist-get info :keywords) info)))
         (data `((title . ,(org-export-data (plist-get info :title) info))
                 (date . ,date)
                 (description . ,(org-export-data (plist-get info :hugo-description) info))
                 (tags . ,tags)
                 (categories . ,(org-export-data (plist-get info :hugo-categories) info))
                 (aliases . ,(org-export-data (plist-get info :hugo-aliases) info))
                 (draft . ,(org-export-data (plist-get info :hugo-draft) info))
                 (publishdate . ,(org-export-data (plist-get info :hugo-publishdate) info))
                 (expirydate . ,(org-export-data (plist-get info :hugo-expirydate) info))
                 (type . ,(org-export-data (plist-get info :hugo-type) info))
                 (isCJKLanguage . ,(org-export-data (plist-get info :hugo-iscjklanguage) info))
                 (weight . ,(org-export-data (plist-get info :hugo-weight) info))
                 (markup . ,(org-export-data (plist-get info :hugo-markup) info))
                 (slug . ,(org-export-data (plist-get info :hugo-slug) info))
                 (url . ,(org-export-data (plist-get info :hugo-url) info)))))
    (org-hugo--gen-front-matter data fm-format)))

(defun org-hugo--gen-front-matter (data format)
  "Generate the Hugo post front matter, and return that string.

DATA is an alist of the form \((KEY1 . VAL1) (KEY2 . VAL2) .. \),
where KEY is a symbol and VAL is a string.

Generate the front matter in the specified FORMAT.  Valid values
are \"toml\" and \"yaml\"."
  (let ((sep (cond ((string= format "toml") "+++\n")
                   ((string= format "yaml") "---\n")
                   (t "")))
        (sign (cond ((string= format "toml") "=")
                    ((string= format "yaml") ":")
                    (t "")))
        (front-matter ""))
    ;; (message "hugo fm format: %s" format)
    (dolist (pair data)
      (let ((key (symbol-name (car pair)))
            (value (cdr pair)))
        (unless (or (null value)
                    (string= "" value))
          ;; In TOML/YAML, the value portion needs to be wrapped in double quotes
          ;; TOML example:
          ;;     title = "My Post"
          ;; YAML example:
          ;;     title : "My Post"
          (setq front-matter
                (concat front-matter
                        (format "%s %s %s\n"
                                key
                                sign
                                (cond ((or (string= key "tags")
                                           (string= key "categories"))
                                       ;; "abc def" -> "[\"abc\", \"def\"]"
                                       (concat "["
                                               (mapconcat #'identity
                                                          (mapcar #'org-hugo--wrap-string-in-quotes
                                                                  (split-string value)) ", ")
                                               "]"))
                                      (t
                                       (org-hugo--wrap-string-in-quotes value)))))))))
    (concat sep front-matter sep)))

;;;; Template

(defun org-hugo-body-filter (body _backend info)
  "Add front matter to the BODY of the document.

BODY is the result of the export.
INFO is a plist holding export options."
  (format "%s\n%s" (org-hugo--get-front-matter info) body))


;;; Interactive functions

;;;###autoload
(defun org-hugo-export-as-md (&optional async subtreep visible-only)
  "Export current buffer to a Hugo-compatible Markdown buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org Hugo Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (org-export-to-buffer 'hugo "*Org Hugo Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))

;;;###autoload
(defun org-hugo-convert-region-to-md ()
  "Convert text in the current region to Hugo-compatible Markdown.
The text is assumed to be in Org mode format.

This can be used in any buffer.  For example, you can write an
itemized list in Org mode syntax in a Markdown buffer and use
this command to convert it."
  (interactive)
  (org-export-replace-region-by 'hugo))

;;;###autoload
(defun org-hugo-export-to-md (&optional async subtreep visible-only)
  "Export current buffer to a Hugo-compatible Markdown file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  ;; steals some plist reading code
  ;; from =org-export-as=
  ;; allows us to extract destination file info from
  ;; export-options-alist
  (let* ((info (org-combine-plists
		(org-export--get-export-attributes
		 'hugo subtreep visible-only)
		(org-export--get-buffer-attributes)
                (org-export-get-environment 'hugo subtreep)))
         (export-dir-path (if (null (plist-get info :hugo-export-dir))
                              (user-error "It is mandatory to set the HUGO_EXPORT_DIR property")
                            (file-name-as-directory (plist-get info :hugo-export-dir))))
         (content-dir "content/")
         (section-dir (if (null (plist-get info :hugo-section))
                          (user-error "It is mandatory to set the HUGO_SECTION property")
                        (file-name-as-directory (plist-get info :hugo-section))))
         (pub-dir (let ((dir (concat export-dir-path content-dir section-dir)))
                    (make-directory dir :parents) ;Create the directory if it does not exist
                    dir))
         (outfile (org-export-output-file-name ".md" subtreep pub-dir)))
    (org-export-to-file 'hugo outfile async subtreep visible-only)))

;;;###autoload
(defun org-hugo-walk-headlines ()
  "Publish each 1st level Org headline to a Hugo post."
  (interactive)
  (org-map-entries
   '(lambda ()
      (let* ((entry (org-element-at-point))
             (level (org-element-property :level entry))
             (commentedp (org-element-property :commentedp entry))
             (tags (org-element-property :tags entry)))
        (message "[ox-hugo DBG] On headline %s\n level is %s \n tags are %s \n commentedp is %s\n test value is %s"
                 (org-element-property :raw-value entry)
                 level tags commentedp
                 (and (eq 1 level)
                      (not (member "noexport" tags))
                      (not commentedp))
                 )
        (if (and (eq 1 level)
                 (not (member "noexport" tags))
                 (not commentedp))
            (org-hugo-export-to-md nil t)))))
  ;; (org-publish-subtrees-to
  ;; (quote hugo) (buffer-file-name)
  ;; md nil
  ;; (concat (file-name-as-directory hugo-content-dir) hugo-section))
  )

;;;###autoload
(defun org-hugo-publish-to-md (plist filename pub-dir)
  "Publish an Org file to Hugo-compatible Markdown file.

PLIST is the property list for the given project.  FILENAME is
the filename of the Org file to be published.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'hugo filename ".md" plist pub-dir))


(provide 'ox-hugo)

;;; ox-hugo.el ends here
