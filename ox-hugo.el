;;; ox-hugo.el --- Hugo Markdown Back-End for Org Export Engine

;; Copyright (C) 2016 Helloyi He

;; Author: Helloyi He
;; Keywords: org, hugo, markdown, gitpage

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements a Markdown back-end (hugo flavor) for Org
;; exporter, based on the `md' back-end.

;;; Code:

(require 'ox-blackfriday)

;;; User-Configurable Variables

(defgroup org-export-hugo nil
  "Options specific to Markdown export back-end."
  :tag "Org Hugo Flavored Markdown"
  :group 'org-export
  :version "24.4"
  :package-version '(Org . "8.0"))

(defcustom org-hugo-metadata-format "toml"
  "Format used to metadata.
This variable can be set to either `toml' or `yaml'."
  :group 'org-export-hugo
  :type 'string)

;;; Define Back-End

(org-export-define-derived-backend 'hugo 'blackfriday
  ;;:export-block '("HMD" "HUGO FLAVORED MARKDOWN")
  :menu-entry
  '(?H "Export to Hugo Flavored Markdown"
       ((?M "To temporary buffer"
            (lambda (a s v b) (org-hugo-export-as-md a s v)))
        (?m "To file" (lambda (a s v b) (org-hugo-export-to-md a s v)))
        (?o "To file and open"
            (lambda (a s v b)
              (if a (org-hugo-export-to-md t s v)
                (org-open-file (org-hugo-export-to-md nil s v)))))))
  :translate-alist '((src-block . org-hugo-src-block)
                     ;;(inner-template . org-hugo-inner-template)
                     (table . org-hugo-table))
  :filters-alist '((:filter-body . org-hugo-body-filter))

  :options-alist '((:hugo-metadata-format "HUGO_METADATA_FORMAT" nil org-hugo-metadata-format)
                   (:hugo-tags        "HUGO_TAGS" nil nil)
                   (:hugo-categories  "HUGO_CATEGORIES" nil nil)
                   (:hugo-description "HUGO_DESCRIPTION" nil nil)
                   (:hugo-slug        "HUGO_SLUG" nil nil)
                   (:hugo-url         "HUGO_URL" nil nil)
                   (:hugo-export-dir  "HUGO_EXPORT_DIR" nil nil)
                   (:hugo-section     "HUGO_SECTION" "posts" nil)
                   (:hugo-static-images "HUGO_STATIC_IMAGES" "image" nil)))


;;; Transcode Functions

;;;; Src Block

(defun org-hugo-src-block (src-block contents info)
  "Transcode SRC-BLOCK element into Hugo Flavored Markdown
format. CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let* ((lang (org-element-property :language src-block))
         (code (org-element-property :value src-block))
         (shortcode (concat "{{< highlight " lang " >}}\n"))
         (close-shortcode "{{< /highlight >}}\n"))
    (concat shortcode code close-shortcode)))




;;;; Hugo metadata

(defun org-hugo-metadata (info)
  "..."
  (let* ((mt-format (org-export-data (plist-get info :hugo-metadata-format) info))
         (title       (org-hugo--get-metadata-title info))
         (date        (org-hugo--get-metadata-date  info))

         (description (org-hugo--get-string-metadata info :hugo-description))
         (tags        (org-hugo--get-list-metadata   info :hugo-tags))
         (categories  (org-hugo--get-list-metadata   info :hugo-categories))
         (slug        (org-hugo--get-string-metadata info :hugo-slug))
         (url         (org-hugo--get-string-metadata info :hugo-url))

         (data (list "title" title "date" date))
         (data (if description (plist-put data "description" description) data))
         (data (if tags        (plist-put data "tags" tags) data))
         (data (if categories  (plist-put data "categories" categories) data))
         (data (if slug        (plist-put data "slug" slug) data))
         (data (if url         (plist-put data "url" url) data)))

    (message "%s" data)
    (cond ((string= mt-format "toml") (org-hugo--encode-metadata-to-toml data))
          ((string= mt-format "yaml") (org-hugo--encode-metadata-to-yaml data))
          "")))

(defun org-hugo--get-metadata-title (info)
  "Get title of hugo.
If title is nil, set it with current buffer name"
  (let ((title (org-hugo--get-string-metadata info :title)))
    (if title title
      (org-hugo-string--wrap-quotes
       (file-name-sans-extension
        (file-name-nondirectory (buffer-file-name)))))))

(defun org-hugo--get-metadata-date (info)
  "Get date of hugo.
If date is nil, set it with current time"
  (let ((date (org-export-get-date info "%Y-%m-%d %T %z")))
    (if date (org-hugo-string--wrap-quotes date)
      (org-hugo-string--wrap-quotes (format-time-string "%Y-%m-%d %T %z" (current-time))))))

(defun org-hugo--get-list-metadata (info key)
  "Get hugo metadata of list type.
INFO is a plist holding export options.
KEY is a key of hugo metadata."
  (let ((value (org-export-data (plist-get info key) info))
        (key (substring (symbol-name key) 1)))
    (cond ((string-empty-p value) nil)
          (t (mapcar 'org-hugo-string--wrap-quotes (split-string value))))))

(defun org-hugo--get-string-metadata (info key)
  "Get hugo metadata of string type.
INFO is a plist holding export options.
KEY is a key of hugo metadata."
  (let ((value (org-export-data (plist-get info key) info))
        (key (substring (symbol-name key) 1)))
    (cond ((string-empty-p value) nil)
          (t (org-hugo-string--wrap-quotes value)))))

(defun org-hugo-string--wrap-quotes (str)
  "Wrap double quotes to string."
  (cond ((string-empty-p str) "")
        ((and (string= (substring str 0 1) "\"")
              (string= (substring str -1) "\"")) str)
        (t (concat "\"" str "\""))))

(defun org-hugo--encode-metadata-to-toml (data)
  "Encode hugo metadata to toml format."
  (setq metadata "+++\n")
  (cl-loop for (key value) on data by 'cddr do
           (setq metadata
                 (concat metadata
                         key " = "
                         (cond ((or (string= key "tags") (string= key "categories"))
                                (concat "[" (mapconcat 'identity value ", ") "]"))
                               (value))
                         "\n")))
  (concat metadata "+++\n"))

(defun org-hugo--encode-metadata-to-yaml (data)
  "Encode hugo metadata to yaml format."
  (setq metadata "---\n")
  (cl-loop for (key value) on data by 'cddr do
           (setq metadata
                 (concat metadata key ": "
                         (cond ((string= key "tags")
                                (concat "[" (mapconcat 'identity value ", ") "]"))
                               ((string= key "categories")
                                (concat "\n  - " (mapconcat 'identity value "\n  - ")))
                               (value))
                         "\n")))
  (concat metadata "---\n"))

;;;; Template

(defun org-hugo-body-filter (body backend info)
  "Add frontmatter to  body of document. 
BODY is the result of the export. BACKEND is always going to be hugo.  INFO is a plist
holding export options."
  (format "%s\n%s" (org-hugo-metadata info) body))


;;; Interactive function

;;;###autoload
(defun org-hugo-export-as-md (&optional async subtreep visible-only)
  "Export current buffer to a Hugo Flavored Markdown buffer.

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
  "Assume the current region has org-mode syntax, and convert it
to Hugo Flavored Markdown.  This can be used in any buffer.
For example, you can write an itemized list in org-mode syntax in
a Markdown buffer and use this command to convert it."
  (interactive)
  (org-export-replace-region-by 'hugo))


;;;###autoload
(defun org-hugo-export-to-md (&optional async subtreep visible-only)
  "Export current buffer to a Hugo Flavored Markdown file.

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
         (pub-dir (concat (file-name-as-directory (plist-get info :hugo-export-dir) )
                          (file-name-as-directory "content")
                          (file-name-as-directory (plist-get info :hugo-section))))
         (outfile (org-export-output-file-name ".md" subtreep pub-dir)))
    (org-export-to-file 'hugo outfile async subtreep visible-only)))

;;;###autoload
(defun org-hugo-walk-headlines ()
  "Publish each 1st-level headline to hugo-mode"
  (interactive)
  (org-map-entries
   '(lambda ()
      (let* ((entry (org-element-at-point))
             (level (org-element-property :level entry))
             (commentedp (org-element-property :commentedp entry))
             (tags (org-element-property :tags entry)))
        (message "on headline %s\n level is %s \n tags are %s \n commentedp is %s\n test value is %s" 
                 (org-element-property :raw-value entry)
                 level tags commentedp
                 (and  (eq 1 level)
                       (not (member "noexport" tags))
                       (not commentedp))
                 )
        (if (and  (eq 1 level)
                  (not (member "noexport" tags))
                  (not commentedp))
            (org-hugo-export-to-md nil t)))))
  ;; (org-publish-subtrees-to
  ;;  (quote hugo) (buffer-file-name)
  ;;  md nil
  ;;  (concat (file-name-as-directory hugo-content-dir) hugo-section))
  )

;;;###autoload
(defun org-hugo-publish-to-md (plist filename pub-dir)
  "Publish an org file to Markdown.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'hugo filename ".md" plist pub-dir))

(provide 'ox-hugo)

;;; ox-hugo.el ends here
