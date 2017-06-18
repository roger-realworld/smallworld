;; List of subdirectories to add to `load-path'.
;; Also subdirs.el in each added subdirectory is also loaded.
;; Set EMACS_VERBOSE_SUBDIRS environment variable to see each subdirs.el load.

;; Include Emacs version specific directory for loading byte code.
(let* ((subdir-list (list ;placed on load-path in reverse order.
		     "deprecated"
		     "smallworld"
		     "features"
		     (concat (file-name-as-directory "features") (number-to-string emacs-major-version))
		     "local" ;ensure 'local' directory comes first.
		     ))
       subdir
       (dir (file-name-directory load-file-name))
       (default-directory (directory-file-name dir))
       (quiet (null (getenv "EMACS_VERBOSE_SUBDIRS"))))
  (while subdir-list
    (setq subdir (car subdir-list)
	  subdir-list (cdr subdir-list))
    (normal-top-level-add-to-load-path (list subdir))
    (let ((default-directory (concat dir subdir)))
      (if (file-exists-p default-directory)
	  (load (concat (file-name-as-directory default-directory) "subdirs.el") nil quiet)
	(or quiet (message "Skipping non-existent directory: %s" default-directory))))))

;;; MAINTAINER/CUSTOMISATION NOTE

;; Explicit subdirs.el file are used instead of using
;; normal-top-level-add-subdirs-to-load-path .nosearch The reason for
;; this is to avoid placing .nosearch files in the 3rd party provided
;; directories under "features".
;;
;; The "local" directory contains a subdirs.el that does call
;; normal-top-level-add-subdirs-to-load-path. This "local" directory
;; structure allows you to place your own set of downloaded Emacs
;; features in a structured place within the Smallworld Emacs
;; site-lisp directory structure.
