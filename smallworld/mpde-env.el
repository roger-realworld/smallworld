;;; MPDE Main Patch variables
;;
(require 'utils-sw)
(require 'mpde-boot)

(defconst mpde-env-version "$Revision: 1.1 $")

(defun concat-u-colon (str)
  "Return U: + the str if on NT else just the str."
  (if (running-under-nt)
      (concat "u:" str)
    str))

(defun concat-/ (dir-name file-name)
  "Returns the concatenation of DIR-NAME and FILE-NAME with a slash in between if there isn't one already."
  (concat (file-name-as-directory dir-name) file-name))

(defvar mpde-env-unsubmitted-patches-dir (concat-u-colon "/swdev/release/unsubmitted_patches")
  "*Directory containing patches that have not been unsubmitted yet.
This directory is actually only used by MPDE when the mater configuration
is unavailable.")
(mpde-register-variable 'mpde-env-unsubmitted-patches-dir
			(concat-u-colon "/swdev/release/unsubmitted_patches")
			'master
			'mpde-refresh-action-make-directory
			'directory)
(defvar mpde-boot-master-site-lisp (if (eq system-type 'windows-nt)
				       "u:/usr/local/share/emacs/site-lisp"
				     "/usr/local/share/emacs/site-lisp")
  "Directory containing the Master site-lisp code.")

(defvar mpde-env-proposed-patches-dir (concat-u-colon "/swdev/release/proposed_patches")
  "*Directory containing patches for all the products for review, etc.")
(mpde-register-variable 'mpde-env-proposed-patches-dir
			(concat-u-colon "/swdev/release/proposed_patches")
			'master
			'(mpde-refresh-action-copy-files "^P.*\\.magik$")
			'directory)

(defvar mpde-env-rejected-patches-dir (concat-/ mpde-env-proposed-patches-dir
						"rejected_patches")
  "*Directory into which rejected patches are placed")
(mpde-register-variable 'mpde-env-rejected-patches-dir
			(concat-u-colon "/swdev/release/proposed_patches/rejected_patches")
			'master
			'mpde-refresh-action-make-directory
			'directory)

(defvar mpde-env-bug-report-dir (concat-u-colon "/support/logging/bug_reports")
  "*Directory location for text file copies of bug reports.
Note that this directory is not reproduced when a new configuration is created
due to its size.")
(mpde-register-variable 'mpde-env-bug-report-dir
			(concat-u-colon "/support/logging/bug_reports")
			'master
			'mpde-refresh-action-make-directory ;We do not want/need to copy it.
			'directory)

(defvar mpde-env-magik-patch-submissions-file (concat-/ mpde-env-proposed-patches-dir
							"magik_patch_submissions")
  "*The control file containing data about the patches that have been submitted for review")
(mpde-register-variable 'mpde-env-magik-patch-submissions-file
			(concat-u-colon "/swdev/release/proposed_patches/magik_patch_submissions")
			'master
			'mpde-refresh-action-create-file ;We do not want/need to copy it.
			'file)

(defvar mpde-env-magik-patch-review-log-file (concat-/ mpde-env-proposed-patches-dir
						       "magik_patch_review_log")
  "*The log file that monitors that activity of review and approval of patches")
(mpde-register-variable 'mpde-env-magik-patch-review-log-file
			(concat-u-colon "/swdev/release/proposed_patches/magik_patch_review_log")
			'master
			'mpde-refresh-action-create-file ;We do not want/need to copy it.
			'file)

(defvar mpde-env-mpde-help-file (concat-/ mpde-env-proposed-patches-dir
					      "MPDE.txt")
  "*The help file detailing how to use this package effectively.")
(mpde-register-variable 'mpde-env-mpde-help-file
			(concat-u-colon "/swdev/release/proposed_patches/MPDE.txt")
			'master
			'mpde-refresh-action-copy-file
			'file)

(defvar mpde-env-patch-options-file (concat-/ mpde-env-proposed-patches-dir
					      "patch_options.txt")
  "*A data file containg the data about available products and names of reviewers for patch topics")
(mpde-register-variable 'mpde-env-patch-options-file
			(concat-u-colon "/swdev/release/proposed_patches/patch_options.txt")
			'master
			'mpde-refresh-action-copy-file
			'file)

(defvar mpde-env-patch-help-file (concat-/ mpde-env-proposed-patches-dir
					   "help.txt")
  "*A help file detailing how to create patches")
(mpde-register-variable 'mpde-env-patch-help-file
			(concat-u-colon "/swdev/release/proposed_patches/help.txt")
			'master
			'mpde-refresh-action-copy-file
			'file)

(defvar mpde-env-bug-report-dir-cache-file (concat-/ mpde-env-proposed-patches-dir
						     "bug_reports_cache.el")
"*Cache of filenames from mpde-env-bug-report-dir due to its size.")
(mpde-register-variable 'mpde-env-bug-report-dir-cache-file
			(concat-u-colon "/swdev/release/proposed_patches/bug_reports_cache.el")
			'master
			'mpde-refresh-action-copy-file
			'file)


(provide 'mpde-env)
