;; Fixes for Org mode that haven't yet been released to GNU Elpa

;; https://lists.gnu.org/r/emacs-orgmode/2023-01/msg00287.html
;; https://git.savannah.gnu.org/cgit/emacs/org-mode.git/commit/?id=a52d0f091
(defun fixed/org-export-as
    (backend &optional subtreep visible-only body-only ext-plist)
  "Transcode current Org buffer into BACKEND code.

BACKEND is either an export back-end, as returned by, e.g.,
`org-export-create-backend', or a symbol referring to
a registered back-end.

If narrowing is active in the current buffer, only transcode its
narrowed part.

If a region is active, transcode that region.

When optional argument SUBTREEP is non-nil, transcode the
sub-tree at point, extracting information from the headline
properties first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only return body
code, without surrounding template.

Optional argument EXT-PLIST, when provided, is a property list
with external parameters overriding Org default settings, but
still inferior to file-local settings.

Return code as a string."
  (when (symbolp backend) (setq backend (org-export-get-backend backend)))
  (org-export-barf-if-invalid-backend backend)
  (org-fold-core-ignore-modifications
    (save-excursion
      (save-restriction
        ;; Narrow buffer to an appropriate region or subtree for
        ;; parsing.  If parsing subtree, be sure to remove main
        ;; headline, planning data and property drawer.
        (cond ((org-region-active-p)
               (narrow-to-region (region-beginning) (region-end)))
              (subtreep
               (org-narrow-to-subtree)
               (goto-char (point-min))
               (org-end-of-meta-data)
               ;; Make the region include top heading in the subtree.
               ;; This way, we will be able to retrieve its export
               ;; options when calling
               ;; `org-export--get-subtree-options'.
               (when (bolp) (backward-char))
               (narrow-to-region (point) (point-max))))
        ;; Initialize communication channel with original buffer
        ;; attributes, unavailable in its copy.
        (let* ((org-export-current-backend (org-export-backend-name backend))
               (info (org-combine-plists
                      (org-export--get-export-attributes
                       backend subtreep visible-only body-only)
                      (org-export--get-buffer-attributes)))
               (parsed-keywords
                (delq nil
                      (mapcar (lambda (o) (and (eq (nth 4 o) 'parse) (nth 1 o)))
                              (append (org-export-get-all-options backend)
                                      org-export-options-alist))))
               tree modified-tick)
          ;; Update communication channel and get parse tree.  Buffer
          ;; isn't parsed directly.  Instead, all buffer modifications
          ;; and consequent parsing are undertaken in a temporary copy.
          (org-export-with-buffer-copy
           (font-lock-mode -1)
           ;; Run first hook with current back-end's name as argument.
           (run-hook-with-args 'org-export-before-processing-hook
                               (org-export-backend-name backend))
           (org-export-expand-include-keyword)
           (org-export--delete-comment-trees)
           (org-macro-initialize-templates org-export-global-macros)
           (org-macro-replace-all org-macro-templates parsed-keywords)
           ;; Refresh buffer properties and radio targets after previous
           ;; potentially invasive changes.
           (org-set-regexps-and-options)
           (org-update-radio-target-regexp)
           (setq modified-tick (buffer-chars-modified-tick))
           ;;  Possibly execute Babel code.  Re-run a macro expansion
           ;;  specifically for {{{results}}} since inline source blocks
           ;;  may have generated some more.  Refresh buffer properties
           ;;  and radio targets another time.
           (when org-export-use-babel
             (org-babel-exp-process-buffer)
             (org-macro-replace-all '(("results" . "$1")) parsed-keywords)
             (unless (eq modified-tick (buffer-chars-modified-tick))
               (org-set-regexps-and-options)
               (org-update-radio-target-regexp))
             (setq modified-tick (buffer-chars-modified-tick)))
           ;; Run last hook with current back-end's name as argument.
           ;; Update buffer properties and radio targets one last time
           ;; before parsing.
           (goto-char (point-min))
           (save-excursion
             (run-hook-with-args 'org-export-before-parsing-hook
                                 (org-export-backend-name backend)))
           (unless (eq modified-tick (buffer-chars-modified-tick))
             (org-set-regexps-and-options)
             (org-update-radio-target-regexp))
           (setq modified-tick (buffer-chars-modified-tick))
           ;; Update communication channel with environment.
           (setq info
                 (org-combine-plists
                  info (org-export-get-environment backend subtreep ext-plist)))
           ;; Pre-process citations environment, i.e. install
           ;; bibliography list, and citation processor in INFO.
           (org-cite-store-bibliography info)
           (org-cite-store-export-processor info)
           ;; De-activate uninterpreted data from parsed keywords.
           (dolist (entry (append (org-export-get-all-options backend)
                                  org-export-options-alist))
             (pcase entry
               (`(,p ,_ ,_ ,_ parse)
                (let ((value (plist-get info p)))
                  (plist-put info
                             p
                             (org-export--remove-uninterpreted-data value info))))
               (_ nil)))
           ;; Install user's and developer's filters.
           (setq info (org-export-install-filters info))
           ;; Call options filters and update export options.  We do not
           ;; use `org-export-filter-apply-functions' here since the
           ;; arity of such filters is different.
           (let ((backend-name (org-export-backend-name backend)))
             (dolist (filter (plist-get info :filter-options))
               (let ((result (funcall filter info backend-name)))
                 (when result (setq info result)))))
           ;; Parse buffer.
           (setq tree (org-element-parse-buffer nil visible-only))
           ;; Prune tree from non-exported elements and transform
           ;; uninterpreted elements or objects in both parse tree and
           ;; communication channel.
           (org-export--prune-tree tree info)
           (org-export--remove-uninterpreted-data tree info)
           ;; Call parse tree filters.
           (setq tree
                 (org-export-filter-apply-functions
                  (plist-get info :filter-parse-tree) tree info))
           ;; Now tree is complete, compute its properties and add them
           ;; to communication channel.
           (setq info (org-export--collect-tree-properties tree info))
           ;; Process citations and bibliography.  Replace each citation
           ;; and "print_bibliography" keyword in the parse tree with
           ;; the output of the selected citation export processor.
           (org-cite-process-citations info)
           (org-cite-process-bibliography info)
           ;; Eventually transcode TREE.  Wrap the resulting string into
           ;; a template.
           (let* ((body (org-element-normalize-string
                         (or (org-export-data tree info) "")))
                  (inner-template (cdr (assq 'inner-template
                                             (plist-get info :translate-alist))))
                  (full-body (org-export-filter-apply-functions
                              (plist-get info :filter-body)
                              (if (not (functionp inner-template)) body
                                (funcall inner-template body info))
                              info))
                  (template (cdr (assq 'template
                                       (plist-get info :translate-alist))))
                  (output
                   (if (or (not (functionp template)) body-only) full-body
                     (funcall template full-body info))))
             ;; Call citation export finalizer.
             (setq output (org-cite-finalize-export output info))
             ;; Remove all text properties since they cannot be
             ;; retrieved from an external process.  Finally call
             ;; final-output filter and return result.
             (org-no-properties
              (org-export-filter-apply-functions
               (plist-get info :filter-final-output)
               output info)))))))))
(advice-add 'org-export-as :override #'fixed/org-export-as)
;; (advice-remove 'org-export-as #'fixed/org-export-as)


(provide 'upstream-org-fixes)
