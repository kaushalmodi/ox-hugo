(defun xeijin/conv-chkbox-items-to-front-matter (hl)
  "Find the heading exactly matching HL.

Then find all plain list items under HL and return as a
list \\='((checked . (VALa VALb ..)) (not-checked . (VALx VALy
..))).

- The values in \"checked\" cons are the Org list items with
  checkbox in \"on\" state.

- The value in \"not-checked\" cons are the Org list items with
  any other checkbox state, or no checkbox."
  ;; (message "dbg x: pt: %d" (point))
  (let (hl-as-element
        checked not-checked
        ret)
    (save-restriction
      (ignore-errors
        (org-narrow-to-subtree)) ;This will give error when there's no
                                        ;heading above the point, which will
                                        ;be the case for per-file post flow.
      (save-excursion
        (goto-char (point-min))
        ;; (message "dbg y: pt: %d" (point))
        (let (case-fold-search) ;Extracted from `org-find-exact-headline-in-buffer'
          (re-search-forward
	   (format org-complex-heading-regexp-format (regexp-quote hl)) nil :noerror))
        ;; (message "dbg z: pt: %d" (point))
        (save-restriction
          (org-narrow-to-subtree) ;Narrow to the `hl' heading
	  (setq hl-as-element (org-element-parse-buffer)))
        ;; (message "dbg: %S" hl-as-element)
        (org-element-map hl-as-element 'item ;Map over heading's items
	  (lambda (item)
	    (let* ((checkbox-state (org-element-property :checkbox item)) ;Get checkbox value of item
		   (item-text (org-trim (substring-no-properties
                                         (org-element-interpret-data
                                          (org-element-contents item))))))
              (cond
               ((eq checkbox-state 'on)
                (push item-text checked))
               (t ;checkbox state in `off' or `trans' state, or if no checkbox present
                (push item-text not-checked))))))
        (setq ret `((checked . ,(nreverse checked))
                    (not-checked . ,(nreverse not-checked))))))
    ;; (message "dbg: ret: %S" ret)
    ret))
