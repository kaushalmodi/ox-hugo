;; Time-stamp: <2017-09-20 13:42:45 kmodi>

;; Install ox-hugo if needed
(defvar ox-hugo-elpa (let ((dir (getenv "OX_HUGO_ELPA")))
                       (when dir
                         (make-directory (file-name-as-directory dir) :parents))
                       dir))

(let* ((bin-dir (when (and invocation-directory
                           (file-exists-p invocation-directory))
                  (file-truename invocation-directory)))
       (prefix-dir (when bin-dir
                     (replace-regexp-in-string "bin/\\'" "" bin-dir)))
       (share-dir (when prefix-dir
                    (concat prefix-dir "share/")))
       (lisp-dir-1 (when share-dir ;Possibility where the lisp dir is something like ../emacs/26.0.50/lisp/
                     (concat share-dir "emacs/"
                             ;; If `emacs-version' is x.y.z.w, remove the ".w" portion
                             ;; Though, this is not needed and also will do nothing in emacs 26+
                             ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=22b2207471807bda86534b4faf1a29b3a6447536
                             (replace-regexp-in-string "\\([0-9]+\\.[0-9]+\\.[0-9]+\\).*" "\\1" emacs-version)
                             "/lisp/")))
       (lisp-dir-2 (when share-dir ;Possibility where the lisp dir is something like ../emacs/25.2/lisp/
                     (concat share-dir "emacs/"
                             (replace-regexp-in-string "\\([0-9]+\\.[0-9]+\\).*" "\\1" emacs-version)
                             "/lisp/"))))
  ;; (message "setup-packages:: bin-dir: %s" bin-dir)
  ;; (message "setup-packages:: prefix-dir: %s" prefix-dir)
  ;; (message "setup-packages:: share-dir: %s" share-dir)
  ;; (message "setup-packages:: lisp-dir-1: %s" lisp-dir-1)
  ;; (message "setup-packages:: lisp-dir-2: %s" lisp-dir-2)
  (defvar my/default-share-directory (when (file-exists-p share-dir)
                                       share-dir)
    "Share directory for this Emacs installation.")
  (defvar my/default-lisp-directory (cond
                                     ((file-exists-p lisp-dir-1)
                                      lisp-dir-1)
                                     ((file-exists-p lisp-dir-2)
                                      lisp-dir-2)
                                     (t
                                      nil))
    "Directory containing lisp files for the Emacs installation.

This value must match the path to the lisp/ directory of the
Emacs installation.  If Emacs is installed using
--prefix=\"${PREFIX_DIR}\" this value would typically be
\"${PREFIX_DIR}/share/emacs/<VERSION>/lisp/\"."))

(defvar my/packages '(ox-hugo))

(if (and (stringp ox-hugo-elpa)
         (file-exists-p ox-hugo-elpa))
    (progn
      (setq package-user-dir (format "%selpa_%s/" ox-hugo-elpa emacs-major-version))

      ;; Below require will auto-create `package-user-dir' it doesn't exist.
      (require 'package)

      (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                          (not (gnutls-available-p))))
             (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
        (add-to-list 'package-archives (cons "melpa" url) :append)) ;For `ox-hugo'
      (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") :append) ;For latest `org'

      ;; Load emacs packages and activate them
      ;; This must come before configurations of installed packages.
      ;; Don't delete this line.
      (package-initialize)
      ;; `package-initialize' call is required before any of the below
      ;; can happen.

      (defvar my/missing-packages '()
        "List populated at each startup that contains the list of packages that need
to be installed.")

      (dolist (p my/packages)
        (unless (package-installed-p p)
          (add-to-list 'my/missing-packages p)))

      (when my/missing-packages
        (message "Emacs is now refreshing its package database...")
        (package-refresh-contents)
        ;; Install the missing packages
        (dolist (p my/missing-packages)
          (message "Installing `%s' .." p)
          (package-install p))
        (setq my/missing-packages '())))
  (error "The environment variable OX_HUGO_ELPA needs to be set"))

;; Remove Org that ships with Emacs from the `load-path'.
(with-eval-after-load 'package
  (dolist (path load-path)
    (when (string-match-p (expand-file-name "org" my/default-lisp-directory) path)
      (setq load-path (delete path load-path)))))

;; (message "`load-path': %S" load-path)
;; (message "`load-path' Shadows:")
;; (message (list-load-path-shadows :stringp))

(require 'ox-hugo)
(defun org-hugo-export-all-subtrees-to-md ()
  (org-hugo-export-subtree-to-md :all-subtrees))

(setq-default require-final-newline t)
(setq-default indent-tabs-mode nil)

(with-eval-after-load 'org
  ;; Allow multiple line Org emphasis markup
  ;; http://emacs.stackexchange.com/a/13828/115
  (setcar (nthcdr 4 org-emphasis-regexp-components) 20) ;Up to 20 lines, default is just 1
  ;; Below is needed to apply the modified `org-emphasis-regexp-components'
  ;; settings from above.
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

  ;; Prevent prompts like:
  ;;   Non-existent agenda file
  (defun org-check-agenda-file (file)))

(with-eval-after-load 'ox
  (setq org-export-with-sub-superscripts '{}))

;; Wed Sep 20 13:37:06 EDT 2017 - kmodi
;; Below does not get applies when running emacs --batch.. need to
;; figure out a solution.
(custom-set-variables
 '(safe-local-variable-values
   (quote
    ((org-hugo-footer . "

[//]: # \"Exported with love from a post written in Org mode\"
[//]: # \"- https://github.com/kaushalmodi/ox-hugo\"")))))
