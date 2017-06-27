;;; ox-hugo-helper.el --- Helper functions to write hugo blog posts in org-mode -*- lexical-binding: t -*-

;;; Commentary:
;; This file is a colletion of legacy files pilfered from the web.
;; preserved here as resources for a more systematic approach.


;; http://www.holgerschurig.de/en/emacs-blog-from-org-to-hugo/
(defvar hugo-content-dir (concat (getenv "HOME") "/sandbox/org/ox-hugo/content/")
  "Path to Hugo's content directory")

(defun hugo-ensure-property (property)
  "Make sure that a property exists. If not, it will be created.

Returns the property name if the property has been created, otherwise nil."
  (unless (org-entry-get nil property)
    (org-entry-put nil property "")
    property))

(defun hugo-ensure-properties ()
  "This ensures that several properties exists.

If not, these properties will be created in an empty form. In this case, the
drawer will also be opened and the cursor will be positioned at the first
element that needs to be filled.

Returns list of properties that still must be filled in"
  (require 'dash)
  (let ((current-time (format-time-string
                       (org-time-stamp-format :long :inactive)
                       (org-current-time)))
        first)
    (save-excursion
      (unless (org-entry-get nil "TITLE")
        (org-entry-put nil "TITLE" (nth 4 (org-heading-components))))
      (setq first (--first it (mapcar #'hugo-ensure-property
                                      '("HUGO_TAGS" "HUGO_TOPICS" "HUGO_FILE"))))
      (unless (org-entry-get nil "HUGO_DATE")
        (org-entry-put nil "HUGO_DATE" current-time)))
    (when first
      (goto-char (org-entry-beginning-position))
      ;; The following opens the drawer
      (forward-line 1)
      (beginning-of-line 1)
      (when (looking-at org-drawer-regexp)
        (org-flag-drawer nil))
      ;; And now move to the drawer property
      (search-forward (concat ":" first ":"))
      (end-of-line))
    first))

(defun hugo ()
  (interactive)
  (unless (hugo-ensure-properties)
    (let* ((title (concat "title = \""
                          (org-entry-get nil "TITLE")
                          "\"\n"))
           (date (concat "date = \""
                         (format-time-string
                          "%Y-%m-%d"
                          (apply #'encode-time
                                 (org-parse-time-string
                                  (org-entry-get nil "HUGO_DATE"))))
                         "\"\n"))
           (topics (concat "topics = [ \""
                           (mapconcat #'identity
                                      (split-string (org-entry-get nil "HUGO_TOPICS")
                                                    "\\( *, *\\)" :omit-nulls)
                                      "\", \"")
                           "\" ]\n"))
           (tags (concat "tags = [ \""
                         (mapconcat #'identity
                                    (split-string (org-entry-get nil "HUGO_TAGS")
                                                  "\\( *, *\\)" :omit-nulls)
                                    "\", \"")
                         "\" ]\n"))
           (fm (concat "+++\n"
                       title
                       date
                       tags
                       topics
                       "+++\n\n"))
           (file (org-entry-get nil "HUGO_FILE"))
           (coding-system-for-write buffer-file-coding-system)
           backend
           blog)
      ;; Load ox-gfm.el if available and use it as backend
      (if (require 'ox-gfm nil :noerror)
          (setq backend 'gfm)
        (progn
          (require 'ox-md)
          (setq backend 'md)))
      (setq blog (org-export-as backend :subtreep))
      ;; Normalize save file path
      (unless (string-match "^[/~]" file)
        (setq file (concat hugo-content-dir file))
        (unless (string-match "\\.md$" file)
          (setq file (concat file ".md")))
        ;; Save markdown
        (with-temp-buffer
          (insert fm)
          (insert blog)
          (write-file file)
          (message "Exported to %s" file))))))

;; http://whyarethingsthewaytheyare.com/setting-up-the-blog/
(defun diego/org-hugo-export ()
  "Export current subheading to markdown using pandoc."
  (interactive)
  (save-excursion
    (unless (eq (org-current-level) 1)
      (outline-up-heading 10))
    (let* ((org-pandoc-format 'markdown)
           (org-pandoc-options-for-markdown '((standalone . t)
                                              (atx-headers . t)
                                              (columns . 79)))
           (properties (org-entry-properties))
           (filename (cdr (assoc "EXPORT_FILE_NAME" properties)))
           (title (concat "\"" (cdr (assoc "ITEM" properties)) "\""))
           (slug (concat "\"" (cdr (assoc "SLUG" properties)) "\""))
           (date (concat "\"" (cdr (assoc "DATE" properties)) "\""))
           (categories
            (concat "[\"" (mapconcat
                           'identity
                           (remove ""
                                   (split-string
                                    (cdr (assoc "TAGS" properties)) ":"))
                           "\",\"") "\"]")))
      (org-export-to-file
          'pandoc
          (org-export-output-file-name
           (concat (make-temp-name ".tmp") ".org") t)
        nil t nil nil nil
        (lambda (f)
          (org-pandoc-run-to-buffer-or-file f 'markdown t nil)))
      (sleep-for 0.5)
      (with-temp-file filename
        (insert-file-contents filename)
        (goto-char (point-min))
        (re-search-forward "---\\(.\\|\n\\)+?---\n\n")
        (replace-match "")
        (goto-char (point-min))
        (insert
         (format
          "---\ntitle: %s\nslug: %s\ndate: %s\ncategories: %s\n---\n\n"
          title slug date categories))
        (dolist (reps '(("^#" . "##")
                        ("\n``` {\\.\\(.+?\\)}" . "```\\1")))
          (goto-char (point-min))
          (while (re-search-forward (car reps) nil t)
            (replace-match (cdr reps))))))))

(provide 'ox-hugo-helper)

;;; ox-hugo.el ends here
