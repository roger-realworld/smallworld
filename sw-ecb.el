(defvar sw-ecb-loaded nil
  "See `sw-ecb'.")

(defun sw-ecb ()
  "Ensure ecb code loaded on demand.
Done to delay loading of CEDET at start up becasue it takes a few seconds as well as ECB."

  (if sw-ecb-loaded
      nil
    (message "Loading ECB and pre-requisite features, please wait...")
    ;; Setup CEDET
    (load "common/cedet" t)
    (require 'ecb-autoloads))
  (setq sw-ecb-loaded t))

(defun sw-ecb-minor-mode (&optional arg)
  "Toggle ECB minor mode, calls `ecb-minor-mode'.
Also ensures ECB required features are loaded, see `sw-ecb-loaded'."
  (interactive "P")
  (sw-ecb)
  (call-interactively 'ecb-minor-mode))

;; Attempt to pre-configure certain parts of ECB functionality, in order to aid performance. 
(setq ecb-prescan-directories-for-emptyness nil)

(eval-after-load 'ecb
  '(progn
     (setq ecb-ecb-dir
	   (file-name-as-directory (expand-file-name "ecb-2.40" site-lisp-features-dir)))
     ;;(add-to-list 'ecb-cache-directory-contents-not '("^/net"))
     ;;(add-to-list 'ecb-cache-directory-contents-not '("^/view"))
     ;;(add-to-list 'ecb-cache-directory-contents-not '("^[mM]:"))
     ;;(add-to-list 'ecb-cache-directory-contents-not '("^[vV]:"))
     (add-to-list 'ecb-cache-directory-contents-not "^/net")
     (add-to-list 'ecb-cache-directory-contents-not "^/view")
     (add-to-list 'ecb-cache-directory-contents-not "^[mM]:")
     (add-to-list 'ecb-cache-directory-contents-not "^[vV]:")
     ;; Remove default behaviour to looks for TAGS files for symbol definitions
     ;; TODO add c-mode, c++-mode to use it if required later.
     ;;      do not know whether tags supports Java files at present.
     (setcdr (assoc 'default ecb-symboldef-find-functions) 'ecb-symboldef-find-null)
     (custom-set-variables (list 'ecb-symboldef-find-functions ecb-symboldef-find-functions t))
))

;;Fix bug in processing of ecb-cache-directory-contents-not.
(eval-after-load 'ecb-file-browser
  '(defun ecb-check-directory-for-caching (dir number-of-contents)
       "Return not nil if DIR matches not any regexp of the option
`ecb-cache-directory-contents-not' but matches at least one regexp in
`ecb-cache-directory-contents' and NUMBER-OF-CONTENTS is greater then the
related threshold."
       (and (not (catch 'exit
		   (dolist (elem ecb-cache-directory-contents-not)
		     (let ((case-fold-search t))
		       (save-match-data
			 (if (string-match elem dir) ;;Fix list of regexps for ecb-cache-directory-contents-not
			     (throw 'exit elem)))    ;; 
		       nil))))
	    (catch 'exit
	      (dolist (elem ecb-cache-directory-contents)
		(let ((case-fold-search t))
		  (save-match-data
		    (if (and (string-match (car elem) dir)
			     (> number-of-contents (cdr elem)))
			(throw 'exit (car elem))))
		  nil))))))

(tool-bar-add-item "ecb" 'sw-ecb-minor-mode 'sw-ecb-minor-mode ; If ECB not loaded, autoload will load the package.
		   :help "Toggle Emacs Code Browser mode")
(tool-bar-add-item "ecb_compile_window" 'ecb-toggle-compile-window 'ecb-toggle-compile-window
		   :help "Show/Hide Compilation Window"
		   :visible  '(and (featurep 'ecb) (eq (selected-frame) ecb-frame) (eq ecb-minor-mode t) (not ecb-windows-hidden))
		   :enable t)
(tool-bar-add-item "ecb_toggle_windows" 'ecb-toggle-ecb-windows 'ecb-toggle-ecb-windows
		   :help "Show/Hide ECB Windows"
		   :visible  '(and (featurep 'ecb) (eq (selected-frame) ecb-frame) (eq ecb-minor-mode t))
		   :enable t)
(tool-bar-add-item "ecb_layouts" ' ecb-toggle-layout 'ecb-toggle-layout
		   :help "Toggle ECB Layouts"
		   :visible  '(and (featurep 'ecb) (eq (selected-frame) ecb-frame) (eq ecb-minor-mode t) (not ecb-windows-hidden)  (> (length ecb-toggle-layout-sequence) 1))
		   :enable t)


(provide 'sw-ecb)
