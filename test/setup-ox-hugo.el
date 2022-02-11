;; Setup to export Org files to Hugo-compatible Markdown using
;; `ox-hugo' in an "emacs -Q" environment.

;; Some sane settings
(setq-default require-final-newline t)
(setq-default indent-tabs-mode nil)
(setq-default make-backup-files nil)

;; Toggle debug on error, including `user-error'.
(setq debug-ignored-errors (remq 'user-error debug-ignored-errors))
(toggle-debug-on-error)

(defvar ox-hugo-test-setup-verbose nil
  "When non-nil, enable printing more messages from setup-ox-hugo.el.")

(defvar ox-hugo-install-org-from-elpa (or (null (getenv "OX_HUGO_DEFAULT_ORG"))
                                          (version< emacs-version "26.0"))
  "When non-nil, install Org from Org Elpa.

The default value of this variable is non-nil if emacs version is
older than 26 AND the environment variable OX_HUGO_DEFAULT_ORG is
unset, else it is nil.

Emacs 26 onwards comes with at least Org 9.1.4.  So there is no
need to install Org from Elpa as that Org version meets the
minimum requirement for `ox-hugo'.  So set the environment
variable OX_HUGO_DEFAULT_ORG to a value like 1 if using emacs 26
or newer.")

(defvar ox-hugo-default-share-directory nil
  "Share directory for this Emacs installation.")

(defvar ox-hugo-default-lisp-directory nil
  "Directory containing lisp files for the Emacs installation.

This value must match the path to the lisp/ directory of the
Emacs installation.  If Emacs is installed using
--prefix=\"${PREFIX_DIR}\" this value would typically be
\"${PREFIX_DIR}/share/emacs/<VERSION>/lisp/\".")

;; Remove Org that ships with Emacs from the `load-path' if installing
;; it from Elpa.
(when ox-hugo-install-org-from-elpa
  (let* ((bin-dir (when (and invocation-directory
                             (file-exists-p invocation-directory))
                    (file-truename invocation-directory)))
         (prefix-dir (when bin-dir        ;Because bin-dir = prefix-dir + "bin/"
                       (file-name-directory (directory-file-name bin-dir))))
         (share-dir (when prefix-dir
                      (let ((share-dir-1 (file-name-as-directory (expand-file-name "share" prefix-dir))))
                        (when (file-exists-p share-dir-1)
                          (setq ox-hugo-default-share-directory share-dir-1))
                        share-dir-1)))
         (version-dir (when share-dir
                        (let* ((emacs-dir (file-name-as-directory (expand-file-name "emacs" share-dir)))
                               ;; Possibility where the lisp dir is something like
                               ;; ../emacs/26.0.50/lisp/.  If `emacs-version' is
                               ;; x.y.z.w, remove the ".w" portion.
                               ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=22b2207471807bda86534b4faf1a29b3a6447536
                               (version (replace-regexp-in-string "\\([0-9]+\\.[0-9]+\\.[0-9]+\\).*" "\\1" emacs-version))
                               (version-dir-1 (file-name-as-directory (expand-file-name version emacs-dir))))
                          (if (file-exists-p version-dir-1)
                              version-dir-1
                            ;; Possibility where the lisp dir is something like
                            ;; ../emacs/25.2/lisp/.  If `emacs-version' is x.y.z,
                            ;; remove the ".z" portion.
                            (setq version (replace-regexp-in-string "\\([0-9]+\\.[0-9]+\\).*" "\\1" emacs-version))
                            (setq version-dir-1 (file-name-as-directory (expand-file-name version emacs-dir)))
                            (when (file-exists-p version-dir-1)
                              version-dir-1))))))
    (when version-dir
      (let ((lisp-dir-1 (file-name-as-directory (expand-file-name "lisp" version-dir))))
        (when (file-exists-p lisp-dir-1)
          (setq ox-hugo-default-lisp-directory lisp-dir-1)))))
  (when ox-hugo-test-setup-verbose
    (message "ox-hugo-default-share-directory: %s" ox-hugo-default-share-directory)
    (message "ox-hugo-default-lisp-directory: %s" ox-hugo-default-lisp-directory))

  (with-eval-after-load 'package
    ;; Remove Org that ships with Emacs from the `load-path'.
    (let ((default-org-path (expand-file-name "org" ox-hugo-default-lisp-directory)))
      (setq load-path (delete default-org-path load-path)))

    ;; Remove Org installed in `site-lisp' directory from the `load-path'.
    (let ((site-lisp-org-path (expand-file-name "emacs/site-lisp/org" ox-hugo-default-share-directory)))
      (setq load-path (delete site-lisp-org-path load-path)))

    ;; Remove Org from `package--builtin-versions'.
    (setq package--builtin-versions (delete (assoc 'org package--builtin-versions) package--builtin-versions))
    ;; Remove Org from `package--builtins'.
    (require 'finder-inf nil t)         ;Populate `package--builtins'
    (setq package--builtins (delete (assoc 'org package--builtins) package--builtins))

    (when ox-hugo-test-setup-verbose
      (message "org detected as installed initially? %S" (package-installed-p 'org))
      (message "load-path: %s" load-path)
      ;; (message "`load-path' Shadows:")
      ;; (message (list-load-path-shadows :stringp))
      )))

(defvar ox-hugo-tmp-dir (let ((dir (file-name-as-directory (getenv "OX_HUGO_TMP_DIR"))))
                          (unless dir
                            (setq dir
                                  (let* ((dir-1 (file-name-as-directory (expand-file-name user-login-name temporary-file-directory)))
                                         (dir-2 (file-name-as-directory (expand-file-name "ox-hugo-dev" dir-1))))
                                    dir-2)))
                          (setq dir (file-name-as-directory dir))
                          (make-directory dir :parents)
                          dir))
(when ox-hugo-test-setup-verbose
  (message "ox-hugo-tmp-dir: %s" ox-hugo-tmp-dir))

(defvar ox-hugo-packages '(toc-org citeproc))
(when ox-hugo-install-org-from-elpa
  ;; Fri Sep 22 18:24:19 EDT 2017 - kmodi
  ;; Install the packages in the specified order. We do not want
  ;; `toc-org' to be installed first. If that happens, `org' will be
  ;; required before the newer version of Org gets installed and we
  ;; will end up with mixed Org version.  So put `org' at the
  ;; beginning of `ox-hugo-packages'.
  (add-to-list 'ox-hugo-packages 'org))

(defvar ox-hugo-site-git-root (progn
                                (require 'vc-git)
                                (file-truename (vc-git-root default-directory)))
  "Absolute path of the git root of the current project.")
(when ox-hugo-test-setup-verbose
  (message "ox-hugo-site-git-root: %S" ox-hugo-site-git-root))

(defvar ox-hugo-autoloads-file (expand-file-name "ox-hugo-autoloads.el" ox-hugo-site-git-root)
  "Path to ox-hugo package's generated autoloads file.")

(if (and (stringp ox-hugo-tmp-dir)
         (file-exists-p ox-hugo-tmp-dir))
    (progn
      ;; Load newer version of .el and .elc if both are available
      (setq load-prefer-newer t)

      (setq package-user-dir (format "%selpa_%s/" ox-hugo-tmp-dir emacs-major-version))

      ;; Below require will auto-create `package-user-dir' it doesn't exist.
      (require 'package)

      ;; Only for 25.x versions
      ;; Workaround for these failures on Travis CI when installing Org
      ;;    Contacting host: elpa.gnu.org:80
      ;;    Debugger entered--Lisp error: (bad-signature "archive-contents.sig")
      ;;      signal(bad-signature ("archive-contents.sig"))
      (when (and (version< emacs-version "26.0")
                 (version<= "25.0" emacs-version))
        (setq package-check-signature nil))

      ;; Note: Org stable is now fetched from the GNU Elpa.
      (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                          (not (gnutls-available-p))))
             (protocol (if no-ssl
                           "http"
                         "https"))
             (melpa-url (concat protocol "://melpa.org/packages/")))
        (add-to-list 'package-archives (cons "melpa" melpa-url) :append) ;For `toc-org', `citeproc'
        )

      ;; Delete element with "nongnu" car from `package-archives'.
      (setq package-archives (delq (assoc "nongnu" package-archives) package-archives))

      ;; Generate/update and load the autoloads for ox-hugo.el and co.
      (let ((generated-autoload-file ox-hugo-autoloads-file))
        (update-directory-autoloads ox-hugo-site-git-root)
        (load-file ox-hugo-autoloads-file))

      ;; Load emacs packages and activate them.
      ;; Don't delete this line.
      (package-initialize)
      ;; `package-initialize' call is required before any of the below
      ;; can happen.

      (add-to-list 'load-path (concat ox-hugo-site-git-root "doc/")) ;For ox-hugo-export-gh-doc.el
      (add-to-list 'load-path ox-hugo-site-git-root) ;For ox-hugo.el, ox-blackfriday.el, etc.

      (defvar ox-hugo-missing-packages '()
        "List populated at each startup that contains the list of packages that need
to be installed.")

      (dolist (p ox-hugo-packages)
        ;; (message "Is %S installed? %s" p (package-installed-p p))
        (unless (package-installed-p p)
          (add-to-list 'ox-hugo-missing-packages p :append)))

      (when ox-hugo-missing-packages
        (message "Emacs is now refreshing its package database...")
        (package-refresh-contents)
        ;; Install the missing packages
        (dolist (p ox-hugo-missing-packages)
          (message "Installing `%s' .." p)
          (package-install p))
        (setq ox-hugo-missing-packages '())))
  (error "The environment variable OX_HUGO_TMP_DIR needs to be set"))

(require 'oc-csl nil :noerror)          ;Auto-register csl processor
(require 'ox-hugo)
(defun org-hugo-export-all-wim-to-md ()
  (org-hugo-export-wim-to-md :all-subtrees nil nil :noerror))

(require 'ox-hugo-export-gh-doc)        ;For `ox-hugo-export-gh-doc'

;; Allow setting few vars in Local Variables in the test files.
(put 'org-hugo-auto-set-lastmod 'safe-local-variable 'booleanp)
(put 'org-hugo-suppress-lastmod-period 'safe-local-variable 'floatp)

(with-eval-after-load 'org
  ;; Allow multiple line Org emphasis markup
  ;; http://emacs.stackexchange.com/a/13828/115
  (setcar (nthcdr 4 org-emphasis-regexp-components) 20) ;Up to 20 lines, default is just 1
  ;; Below is needed to apply the modified `org-emphasis-regexp-components'
  ;; settings from above.
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

  ;; Prevent prompts like:
  ;;   Non-existent agenda file
  (defun org-check-agenda-file (file))

  (let (ob-lang-alist)
    (add-to-list 'ob-lang-alist '(emacs-lisp . t))
    (add-to-list 'ob-lang-alist '(org . t))
    (org-babel-do-load-languages 'org-babel-load-languages ob-lang-alist))

  (with-eval-after-load 'ob-core
    (defun ox-hugo-org-confirm-babel-evaluate-fn (lang body)
      "Mark `org' as a safe language for ox-hugo tests and docs."
      (let* ((ob-enabled-langs '("org"))
             (ob-enabled-langs-re (regexp-opt ob-enabled-langs 'words))
             (unsafe t)) ;Set the return value `unsafe' to t by default
        (when (string-match-p ob-enabled-langs-re lang)
          (setq unsafe nil))
        unsafe))
    (setq org-confirm-babel-evaluate #'ox-hugo-org-confirm-babel-evaluate-fn))

  (with-eval-after-load 'ox
    (setq org-export-headline-levels 4))) ;default is 3


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Settings *only* for tests (applied during "make test")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (string= "1" (getenv "TEST_ENABLED"))
  ;; Set the time-zone to UTC.
  ;; If TZ is unset, Emacs uses system wall clock time, which is a
  ;; platform-dependent default time zone --
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Time-Zone-Rules.html
  (setenv "TZ" "UTC")

  ;; Force the locate to en_US for the tests.
  (set-locale-environment "en_US.UTF-8")
  (setenv "LANGUAGE" "en_US.UTF-8")

  ;; Set all local variables from .dir-locals.el, etc.
  (setq enable-local-variables :all)

  ;; Override the default `org-hugo-export-creator-string' so that this
  ;; string is consistent in all ox-hugo tests.
  (setq org-hugo-export-creator-string "Emacs + Org mode + ox-hugo")

  ;; Override the inbuilt `current-time' function so that the "lastmod"
  ;; tests work.
  (defun ox-hugo-test/current-time-override (&rest args)
    "Hard-code the 'current time' so that the lastmod tests are reproducible.
Fake current time: 2100/12/21 00:00:00 (arbitrary)."
    (encode-time 0 0 0 21 12 2100))
  (advice-add 'current-time :override #'ox-hugo-test/current-time-override)
  ;; (advice-remove 'current-time #'ox-hugo-test/current-time-override)

  ;; issue # 272
  (load (expand-file-name "test/issue-272-chkbox-items-to-front-matter.el"
                          ox-hugo-site-git-root)
        nil :nomessage :nosuffix)

  (with-eval-after-load 'ox
    (add-to-list 'org-export-exclude-tags "dont_export_during_make_test"))

  ;; Wed Sep 04 22:23:03 EDT 2019 - kmodi
  ;; The ox-hugo tests were failing on Travis only on Emacs 24.4 and
  ;; 24.5 because the "/" got auto-appended to links without them:
  ;; https://travis-ci.org/kaushalmodi/ox-hugo/jobs/580990010#L3740
  ;; So when the https://ox-hugo.scripter.co link got exported via
  ;; `org-html-link' internally, it got converted to
  ;; https://ox-hugo.scripter.co/! As it turns out, this behavior got
  ;; fixed in Emacs 25+ in:
  ;; https://git.savannah.gnu.org/cgit/emacs.git/commit/?id=b792ecea1715e080ad8e232d3d154b8a25d2edfb
  (unless (version<= "25.0" emacs-version)
    (with-eval-after-load 'url-parse
      (defun url-path-and-query (urlobj)
        "Return the path and query components of URLOBJ.
These two components are stored together in the FILENAME slot of
the object.  The return value of this function is (PATH . QUERY),
where each of PATH and QUERY are strings or nil."
        (let ((name (url-filename urlobj))
	          path query)
          (when name
            (if (string-match "\\?" name)
	            (setq path  (substring name 0 (match-beginning 0))
		              query (substring name (match-end 0)))
	          (setq path name)))
          (cons path query))))))
