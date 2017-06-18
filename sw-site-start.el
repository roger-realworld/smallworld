;;; site-start.el - Emacs startup file.

;;; This file, if it exists in the emacs load path is loaded before
;;; the users' .emacs file.

;;; We use it to point emacs at its info, etc and executable files
;;; except if a centralised development environment is detected.

(defvar Info-default-directory-list)

(cond ((eq system-type 'windows-nt)
       ;; Emacs on Windows requires no extra environment setup here.
       nil)
      ((locate-file "sitename" exec-path '("" ".bat" ".cmd"))
	;; Found a development site, assume a centrally managed Emacs environment
       nil)
      (t
       ;; Found a local Emacs installation.
       ;; Fix up various variables to account for the local bin and etc structure.
       ;;  bin - has a HOST_OS.HOST_ARCH subdirectory structure storing all Unix binaries
       ;;        Emacs "libexec" executables are also stored in the appropriate
       ;;        bin directory too.
       ;;  etc - This is the Emacs data and doc directory.
       ;;        It is not the "libexec" directory.
       (let* ((emacs-exe     (locate-file (car command-line-args) exec-path))
	      (emacs-exe-dir (file-name-directory emacs-exe))
	      (emacs-bin     (file-name-directory (directory-file-name emacs-exe-dir)))
	      (emacs-root    (file-name-directory (directory-file-name emacs-bin)))
	      (emacs-etc     (file-name-as-directory (concat emacs-root "etc")))
	      (emacs-info    (file-name-as-directory (concat emacs-root "info"))))
	 (setq exec-directory emacs-exe-dir
	       exec-path      (reverse (cons exec-directory (reverse exec-path))))
	 (if (file-exists-p (concat emacs-info "dir"))
	     ;; Found Info directory containing the "dir" file.
	     (setq Info-default-directory-list (cons emacs-info Info-default-directory-list)))
	 (if (file-exists-p (concat emacs-etc "NEWS"))
	     ;; Found the Emacs data directory containing the Emacs NEWS file.
	     (setq data-directory emacs-etc
		   doc-directory  emacs-etc
		   tutorial-directory (file-name-as-directory (concat emacs-etc "tutorials")))))))

(provide 'site-start)

;;; site-start.el ends here
