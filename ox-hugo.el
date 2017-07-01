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
This variable can be set to either `toml' or `yaml'."
  :group 'org-export-hugo
  :type 'string)


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

  :options-alist '((:hugo-front-matter-format "HUGO_FRONT_MATTER_FORMAT" nil org-hugo-front-matter-format)
                   (:hugo-tags "HUGO_TAGS" nil nil)
                   (:hugo-categories "HUGO_CATEGORIES" nil nil)
                   (:hugo-description "HUGO_DESCRIPTION" nil nil)
                   (:hugo-slug "HUGO_SLUG" nil nil)
                   (:hugo-url "HUGO_URL" nil nil)
                   (:hugo-export-dir "HUGO_EXPORT_DIR" nil nil)
                   (:hugo-section "HUGO_SECTION" "posts" nil)
                   (:hugo-static-images "HUGO_STATIC_IMAGES" "images" nil)))


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

(defun org-hugo--get-front-matter (info)
  "Return the Hugo front matter string.

INFO is a plist used as a communication channel."
  (let* ((fm-format (org-export-data (plist-get info :hugo-front-matter-format) info))
         (title (org-hugo--get-front-matter-title info))
         (date (org-hugo--get-front-matter-date info))
         (description (org-hugo--get-string-front-matter info :hugo-description))
         (tags (org-hugo--get-list-front-matter info :hugo-tags))
         (categories (org-hugo--get-list-front-matter info :hugo-categories))
         (slug (org-hugo--get-string-front-matter info :hugo-slug))
         (url (org-hugo--get-string-front-matter info :hugo-url))

         (data (list "title" title "date" date))
         (data (if description (plist-put data "description" description) data))
         (data (if tags (plist-put data "tags" tags) data))
         (data (if categories (plist-put data "categories" categories) data))
         (data (if slug (plist-put data "slug" slug) data))
         (data (if url (plist-put data "url" url) data)))

    ;; (message "%s" data)
    (cond ((string= fm-format "toml")
           (org-hugo--encode-front-matter-to-toml data))
          ((string= fm-format "yaml")
           (org-hugo--encode-front-matter-to-yaml data))
          (t
           ""))))

(defun org-hugo--get-front-matter-title (info)
  "Get the Title from the front matter.
If it is nil, set it with current buffer file name.

INFO is a plist used as a communication channel."
  (let ((title (org-hugo--get-string-front-matter info :title)))
    (if title title
      (org-hugo--wrap-string-in-quotes
       (file-name-sans-extension
        (file-name-nondirectory (buffer-file-name)))))))

(defun org-hugo--get-front-matter-date (info)
  "Get the Date from the front matter.
If it is nil, set it to the current time.

INFO is a plist used as a communication channel."
  (let ((date (org-export-get-date info "%Y-%m-%d %T %z")))
    (if date (org-hugo--wrap-string-in-quotes date)
      (org-hugo--wrap-string-in-quotes (format-time-string "%Y-%m-%d %T %z" (current-time))))))

(defun org-hugo--get-list-front-matter (info key)
  "Get Hugo front matter of list type.
INFO is a plist holding export options.
KEY is a key of Hugo front matter."
  (let ((value (org-export-data (plist-get info key) info))
        ;; (key (substring (symbol-name key) 1))
        )
    (cond ((string-empty-p value) nil)
          (t (mapcar 'org-hugo--wrap-string-in-quotes (split-string value))))))

(defun org-hugo--get-string-front-matter (info key)
  "Get Hugo front matter of string type.
INFO is a plist holding export options.
KEY is a key of Hugo front matter."
  (let ((value (org-export-data (plist-get info key) info))
        ;; (key (substring (symbol-name key) 1))
        )
    (cond ((string-empty-p value) nil)
          (t (org-hugo--wrap-string-in-quotes value)))))

(defun org-hugo--wrap-string-in-quotes (str)
  "Wrap STR with double quotes and return the string."
  (cond ((string-empty-p str)
         "")
        ((and (string= (substring str 0 1) "\"")
              (string= (substring str -1) "\""))
         str)
        (t
         (concat "\"" str "\""))))

(defun org-hugo--encode-front-matter-to-toml (data)
  "Encode Hugo front matter to a string in TOML format.
Return that string.

DATA is a property list, which is a list of the form \(PROP1
VALUE1 PROP2 VALUE2 ...\).  PROP is a string and VAL is any
object."
  (let ((front-matter "+++\n"))
    (cl-loop for (key value) on data by 'cddr do
             (setq front-matter
                   (concat front-matter
                           key " = "
                           (cond ((or (string= key "tags") (string= key "categories"))
                                  (concat "[" (mapconcat 'identity value ", ") "]"))
                                 (value))
                           "\n")))
    (setq front-matter (concat front-matter "+++\n"))
    front-matter))

(defun org-hugo--encode-front-matter-to-yaml (data)
  "Encode Hugo front matter to a string in YAML format.
Return that string.

DATA is a property list, which is a list of the form \(PROP1
VALUE1 PROP2 VALUE2 ...\).  PROP is a string and VAL is any
object."
  (let ((front-matter "---\n"))
    (cl-loop for (key value) on data by 'cddr do
             (setq front-matter
                   (concat front-matter key ": "
                           (cond ((string= key "tags")
                                  (concat "[" (mapconcat 'identity value ", ") "]"))
                                 ((string= key "categories")
                                  (concat "\n - " (mapconcat 'identity value "\n - ")))
                                 (value))
                           "\n")))
    (setq front-matter (concat front-matter "---\n"))
    front-matter))

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
