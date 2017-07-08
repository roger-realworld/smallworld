;;; sw-help.el - Info and HTML Help interface

(eval-when-compile
  (require 'cl)
  (require 'info)
  (require 'macros-sw)
  (require 'resources)
  (require 'utils-sw))

;;We use Info-goto-node so that we can direct the use to a specific place in a .info file
;;However,it is not an official info API but there is no official info API for that, only
;;to the top of the file... Require the file so that function is accessible in later Emacsen
(require 'info)

(defconst sw-help-version "$Revision: 1.6 $")

(defvar sw-help-id (list (cons "dir" nil)
			 '("swEmacs.chm" . nil))
  "Help ID data for Main Smallworld Emacs on-line help.")

(defvar sw-help-keys-id '(("EmacsForSWProducts" . "Keys for Smallworld Emacs commands"))
  "Help ID data for Smallworld Emacs key index section of the on-line help.")

(defvar sw-help-sw-menu-id '(("EmacsForSWProducts" . "The SW menu"))
  "Help ID data for Smallworld Emacs SW Menu index section of the on-line help.")

;;;Mode specific help ids
(defvar sw-help-aliases-id '(("EmacsForSWProducts" . "Aliases mode"))
  "Help ID data for Aliases mode on-line help.")

(defvar sw-help-cb-id '(("EmacsForSWProducts" . "Cb mode and using the Class Browser"))
  "Help ID data for Class Browser on-line help.")

(defvar sw-help-product-id '(("EmacsForSWProducts" . "Product mode"))
  "Help ID data for Product Mode on-line help.")

(defvar sw-help-module-id '(("EmacsForSWProducts" . "Module mode"))
  "Help ID data for Module Mode on-line help.")

(defvar sw-help-gis-version-id '(("EmacsForSWProducts" . "Gis-version mode"))
  "Help ID data for Gis-Version Mode on-line help.")

(defvar sw-help-gis-id '(("EmacsForSWProducts" . "Gis mode and running a Smallworld process"))
  "Help ID data for Gis Mode on-line help.")

(defvar sw-help-loadlist-id '(("EmacsForSWProducts" . "Loadlist mode"))
  "Help ID data for Loadlist Mode on-line help.")

(defvar sw-help-magik-id '(("EmacsForSWProducts" . "Magik mode and editing Magik source code"))
  "Help ID data for Magik Mode on-line help.")

(defvar sw-help-magik-language-id '(("MagikLangRef" .  nil))
  "Help ID data for Magik Language Reference Library on-line help.")

(defvar sw-help-msg-id '(("EmacsForSWProducts" . "Msg mode"))
  "Help ID data for Msg Mode on-line help.")

;; Submenu creation
(defvar sw-help-submenu nil
  "The SW->Help Menu entries.")

(defun sw-help-update-submenu ()
  "Update Help submenu in SW menu bar."
  (if sw-help-submenu
      nil
    (condition-case err
	(progn
	  (setq sw-help-submenu
		`(
		  [,resources-menu-sw-help-info     sw-help-info        :active t :keys "f1"]
		  [,resources-menu-sw-help-htmlhelp sw-help-htmlhelp    :active t] ;; :key-sequence nil
		  [,resources-menu-sw-help-sw-menu  sw-help-sw-menu     :active t] ;; :key-sequence nil
		  [,resources-menu-sw-help-keys     sw-help-keys        :active t] ;; :key-sequence nil
		  )) ;; :key-sequence nil
	  (easy-menu-change (list resources-menu-sw)
			    resources-menu-sw-help
			    sw-help-submenu))
    (error nil))))

(defun sw-help-htmlhelp-path (filename)
  "Return the full path to the given HTML Help file.
FILENAME may be a fully qualified path with embedded Environment variables.
This function will attempt to resolve them including SW_LANGUAGE.
if not already set SW_LANGUAGE will be set to values in `resources-languages'.
Otherwise FILENAME will be searched for under the following help directories:
   `mpde-boot-site-lisp-dir'/smallworld/resources/$SW_LANGUAGE/help
   `site-lisp-dir'/smallworld/resources/$SW_LANGUAGE/help

If no file can be found a message is displayed.
"
  (let (languages directories file)
    (cond ((and (not (getenv "SW_LANGUAGE"))
		(save-match-data (string-match "SW_LANGUAGE" filename)))
	   ;;See if given filename is fully qualified except for embedded SW_LANGUAGE
	   ;;environment variable that is not defined. - Use resources-languages to resolve
	   (let ((process-environment (copy-list process-environment))
		 (languages (copy-list resources-languages)))
	     (while languages
	       (setenv "SW_LANGUAGE" (car languages))
	       (condition-case err
		   (setq file (expand-file-name (substitute-in-file-name filename))
			 languages (cdr languages))
		 (error nil))
	       (if (file-exists-p file)
		   (setq languages nil)
		 (setq file nil)))
	     file))
	  ((let ((f (condition-case err
			(expand-file-name (substitute-in-file-name filename))
		      (error nil))))
	     (and f (file-exists-p f) (setq file f)))
	   ;;See if given filename is fully qualified
	   file)
	  (t
	   (setq directories (list mpde-boot-site-lisp-dir site-lisp-dir))
	   (while (and (not file) directories)
	     ;;Look for file in .../smallworld/resources/$SW_LANGUAGE/help
	     (setq languages (copy-list resources-languages))
	     (while (and (not file) languages)
	       (setq file
		     (concat (file-name-as-directory (car directories))
			     "smallworld/resources/"
			     (car languages)
			     "/help/"
			     filename)
		     languages   (cdr languages)
		     directories (cdr directories))
	       (if (file-exists-p file) nil (setq file nil))))
	   (or file
	       (message resources-sw-help-no-htmlhelp-file-error filename))
	   file))))

(defun sw-help-open (help-id &optional sw-help-uses-htmlhelp)
  "Open the HELP-ID reference using either Info or HTMLHelp.
HELP-ID is reference data used to define the location of the help
in Info or HTMLHelp. It is a list (INFO HTML-HELP).
With INFO being data to open Info in Emacs and HTML-HELP
is data used to open a HTMLHelp window using 'hh.exe' on Windows.

HTMLHelp is used if `sw-help-uses-htmlhelp' is t.

If the HTMLHelp file cannot be found then Info will be used instead.
"
  (let ((htmlhelp-file (and sw-help-uses-htmlhelp
			    (running-under-nt)
			    (cdr help-id)
			    (sw-help-htmlhelp-path (car (elt help-id 1))))))
    (cond (htmlhelp-file
	   ;; Open HTML Help
	   (let ((id   (cdr (elt help-id 1)))
		 args)
	     (subst-char-in-string ?/ ?\\ htmlhelp-file t)
	     (cond ((integerp id)
		    (setq args (list "-mapid" (number-to-string id) htmlhelp-file)))
		   ((stringp  id)
		    (setq args (list (concat htmlhelp-file "::" id))))
		   (t
		    (setq args (list htmlhelp-file))))
	     (apply 'start-process "htmlhelp" nil "hh.exe" args)))
	  ((cdr (elt help-id 0))
	   ;; Open Info on a topic.
	   (let ((id (elt help-id 0)))
	     (Info-goto-node (concat "(" (car id) ")" (cdr id)))))
	  (t
	   ;; Open Info on a file
	   (info (car (elt help-id 0)))))))

(defun sw-help-info ()
  "Top-level entry to the Smallworld Help Info pages."
  (interactive)
  (sw-help-open sw-help-id)
  ;; Go to Smallworld section
  (save-match-data
    (goto-char (point-min))
    (and (search-forward "Smallworld" nil t)
	 (forward-line 1))))

(defun sw-help-htmlhelp ()
  "Top-level entry to the Smallworld Help HTMLHelp pages."
  (interactive)
  (sw-help-open sw-help-id t))

(defun sw-help-keys ()
  "Go to the 'Key Index' section of the Smallworld Info system."
  (interactive)
  (sw-help-open sw-help-keys-id))

(defun sw-help-sw-menu ()
  "Go to the 'SW Menu' section of the Smallworld Info system."
  (interactive)
  (sw-help-open sw-help-sw-menu-id))

(defun sw-help-view-swdefaults ()
  "Visit the first \"sw_defaults.el\" file on the load-path."
  (interactive)
  (find-file
   (which-file "sw_defaults.el" resources-sw-view-load-path-file-error))
  (view-mode))

(defun sw-help-view-occasional ()
  "Visit the \"occasional.emacs\" example configuration file on the load-path."
  (interactive)
  (find-file
   (which-file "occasional.emacs" resources-sw-view-load-path-file-error))
  (view-mode))

(defun sw-help-view-developer ()
  "Visit the \"developer.emacs\" example configuration file on the load-path."
  (interactive)
  (find-file
   (which-file "developer.emacs" resources-sw-view-load-path-file-error))
  (view-mode))

(provide 'sw-help)
