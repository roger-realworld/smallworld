;;;Magik Patch development environment
;;
;; This package provides the development environment for the Magik Patch
;; submission/review and approval process.
;; It has been written to better support offline/remote working.
;;
;; For documentation please see the following files:
;;
;;   /swdev/release/proposed_patches/MPDE.txt
;;   /swdev/release/proposed_patches/README.txt
;;   /swdev/release/proposed_patches/help.txt
;;   /swdev/release/proposed_patches/patch_options.txt
;;
;;   The magik-template files are also copied but see magik-template.el
;;   /swdev/release/proposed_patches/*_template_[n].magik
;;
;; When MPDE is installed, mpde-boot.el is copied to ~/ and loaded by ~/.mpde
;; if MPDE is not installed, we still need the code so we require it here but it is
;; loaded from the site-lisp directory.

(eval-when-compile
  (require 'dired)
  (or (fboundp 'dired-create-files) (require 'dired-aux))
  (require 'macros-sw)
  (require 'resources)
  (require 'misc-sw)
  (require 'utils-sw)
  (require 'mpde-boot)
  (require 'mpde-env))

(require 'dired)
(or (fboundp 'dired-create-files) (require 'dired-aux))

(defgroup mpde nil
  "Provides up-to-date Magik Patch Development Environment for Smallworld Emacs."
  :tag "Magik Patch Development Environment"
  :group 'smallworld)

(defconst mpde-version "$Revision: 1.6 $")

(defcustom mpde-display-startup nil
  "*If t, then display list of available configurations at startup.
If a symbol or list which matches any of the currently defined configurations
then the display is presented.
So it could be set to 'master to indicate to the user that the master configuration
is available."
  :group 'mpde
  :type 'sexp)

(defcustom mpde-refresh-from-master t
  "*Controls whether the local configuration data is refreshed from the master.
If t,   then when the master configuration is available then a refresh will take place,
   nil  then no refresh occurs when \\[mpde-set-configuration] is used,
        you can still refresh manually by using \\[mpde-refresh-configuration]
        assuming the master configuration is available,
   'ask then the user is prompted for whether they wish to perform the refresh.

This is useful to set to nil or 'ask if master is available but only via a slow network link."
  :group 'mpde
  :type '(choice (const :tag "Yes" t)
		 (const :tag "No" nil)
		 (const :tag "Ask" ask)))

(defcustom mpde-refresh-emacs 'ask
  "*Controls whether the current Emacs session is updated after a refresh.
If t,   then the Emacs session will always be updated with the latest code automatically,
   nil  then no update of the code occurs. The refreshed code will be available when
        Emacs is started. You can still refresh manually by using \\[mpde-refresh-emacs]
   'ask then the user is prompted for whether they wish their Emacs session to be updated.

There is a chance that the new code requires changes in emacs data. So if the data in the
current session is not converted in some wayt after updating with the latest lisp code
there is a chance that Emacs stability could be affected. However, restarting Emacs
should be sufficient."
  :group 'mpde
  :type '(choice (const :tag "Yes" t)
		 (const :tag "No" nil)
		 (const :tag "Ask" ask)))

(defcustom mpde-configuration-root-dir (if (eq system-type 'windows-nt)
					   "c:/smallworld/mpde"
					 "~/mpde")
  "*The default location used to store the configuration data files."
  :group 'mpde
  :type  'directory)

(defcustom mpde-set-configuration-hook nil
  "*Hook that is run by \\[mpde-set-configuration].
Functions in this hook will be executed at initial start up and
whenever a new configuration is selected."
  :group 'mpde
  :type 'hook)

(defcustom mpde-refresh-configuration-hook nil
  "*Hook that is run by \\[mpde-refresh-configuration]."
  :group 'mpde
  :type 'hook)

(defcustom mpde-store-configuration-file-hook nil
  "*Hook that is run by \\[mpde-store-configuration-file] at refresh time."
  :group 'mpde
  :type 'hook)

(defcustom mpde-after-load-hook nil
  "*Hook run after configuration file is loaded."
  :group 'mpde
  :type 'hook)

(defcustom mpde-debug nil
  "*Turn on debugging of MPDE.
Currently, this simply reports the creation and copying of the files for different
configurations but does not actually perform any file changes."
  :group 'mpde
  :type 'boolean)

;; mpde-menu-display's default depends upon whether MPDE has been installed or not.
(defvar mpde-menu-display (stringp mpde-boot-installed)
  "*Controls whether the MPDE menu is displayed.
If nil, then the menu is not displayed,
     t, then the menu is displayed,
   integer, then the menu is displayed if the number of available
configurations equals or exceeds that integer.
So a value of 2 would mean that the menu is displayed only if 2 or more
configurations have been defined.")

(defconst mpde-menu
  `(,resources-mpde-menu
    [,resources-mpde-menu-set                      mpde-set-configuration                t]
    [,resources-mpde-menu-refresh                  mpde-refresh-configuration            (and (mpde-master-available-p)
									   (not (eq mpde-configuration-current 'master)))]
    [,resources-mpde-menu-upload-submissions       mpde-upload-submissions               (mpde-upload-pending-p)]
    "---"                        
    [,resources-mpde-menu-available                mpde-display-available-configurations t]
    [,resources-mpde-menu-display-configuration    mpde-display-configuration            t]
    [,resources-mpde-menu-display-resource         mpde-display-resource                 t]
    "---"                        
    [,resources-mpde-menu-make                     mpde-make-configuration               (eq mpde-configuration-current 'master)]
    [,resources-mpde-menu-delete                   mpde-delete-configuration             (> (length mpde-configuration-path-alist) 1)]
    [,resources-mpde-menu-install                  mpde-install                          (not mpde-boot-installed)]
    "---"
    [,resources-menu-sw-customize                  mpde-customize                        t]
    [,resources-menu-sw-help                       mpde-help            t]))

(defvar mpde-copy-directory-program (if (eq system-type 'windows-nt)
					"xcopy" "cp")
  "System Command used for copying.")

(defvar mpde-copy-directory-args (if (eq system-type 'windows-nt)
					'("/y" "/i" "/e") '("-r"))
  "Arg list for `mpde-copy-directory-program' to copy a directory recursively.")

(defvar mpde-delete-directory-program (if (eq system-type 'windows-nt)
					"rmdir" "rm")
  "System Command used for deleting directories and their contents.")

(defvar mpde-delete-directory-args (if (eq system-type 'windows-nt)
					'("/s" "/q") '("-r" "-f"))
  "Arg list for `mpde-delete-directory-program' to delete a directory including its contentsy.")

;;; Utility functions
;;Copied from custom-file definition from Customization buffer functionality in cus-edit.el
(defun mpde-dotemacs-file ()
  "Return the users .emacs file name."
  (let ((user-init-file user-init-file)
	(default-init-file
	  (if (eq system-type 'ms-dos) "~/_emacs" "~/.emacs")))
    (if (null user-init-file)
	(progn
	  (if (or (file-exists-p default-init-file)
		  (and (running-under-nt)
		       (file-exists-p "~/_emacs")))
	      ;; Started with -q, i.e. the file containing
	      ;; Custom settings hasn't been read.  Saving
	      ;; settings there would overwrite other settings.
	      (error resources-mpde-save-overwrite-error))
	  (setq user-init-file default-init-file)))
    user-init-file))

(defun mpde-copy-directory (source destination &optional buffer)
  "Helper function to copy contents of one directory to another.
All subdirectories are copied too.
This is implemented via an OS system call since Elisp does not have a directory copy command."
  (if buffer
      (save-excursion
	(set-buffer (get-buffer-create buffer))
	(erase-buffer)
	(insert (format (concat resources-mpde-source-dir        " %s\n") source))
	(insert (format (concat resources-mpde-destination-dir " %s\n\n") destination))))
  (if (running-under-nt)
      (progn
	(setq source (subst-char-in-string ?/ ?\\ source))
	(setq destination (subst-char-in-string ?/ ?\\ destination))))
	
  (let* ((args (append mpde-copy-directory-args (list source destination) nil))
	 (code (apply 'call-process
		      mpde-copy-directory-program
		      nil
		      buffer
		      nil
		      args)))
    (if (eq code 0)
	nil
      (and buffer (display-buffer buffer t))
      (error resources-mpde-copy-dir-error (number-to-string code)))))

(defun mpde-delete-directory (dir &optional buffer)
  "Helper function to delete entire contents of a directory.
This is implemented via an OS system call since Elisp does not have a
directory delete command that deletes contents as well."

;; The Windows shell command rd (aka rmdir) is NOT available as an executable
;; i.e. it is a shell builtin. So we have to go through some hoops to call it.
;; shell-command could be used but we would also have to handle any args with spaces in.
  (let* ((w32-p (running-under-nt))
	 (dir  (if w32-p (subst-char-in-string ?/ ?\\ dir) dir))
	 (args (if w32-p
		   (append (list "/c" mpde-delete-directory-program)
			   mpde-delete-directory-args (list dir) nil)
	       (append mpde-delete-directory-args (list dir) nil)))
	 (directory-program (if w32-p
				(getenv "COMSPEC")
			      mpde-delete-directory-program))
	 (code (apply 'call-process directory-program nil buffer nil args)))

    (if (eq code 0)
	nil
      (and buffer (display-buffer buffer t))
      (error resources-mpde-delete-dir-error dir (number-to-string code)))))

(defun mpde-copy-files (source destination &optional match buffer)
  "Helper function to copy a set of files from SOURCE directory.
DESTINATION is the directory to copy files to.
MATCH will restrict the copy to those filenames matching MATCH.
BUFFER send output from copying process."
  (save-excursion
    (if buffer
	(progn
	  (set-buffer (get-buffer-create buffer))
	  (erase-buffer)
	  (insert (format (concat resources-mpde-source-dir      " %s\n") source))
	  (insert (format (concat resources-mpde-destination-dir " %s\n\n") destination))))
    (condition-case err
	(mapcar (function
		 (lambda (file)
		   (let ((source-file (concat-/ source file))
			 (destination-file (concat-/ destination file)))
		     (and (file-exists-p destination-file)
			  (delete-file destination-file))
		     (copy-file source-file destination-file t)
		     (insert (format resources-mpde-copied-file-to
				     source-file destination-file) "\n"))))
		(directory-files source nil match))
      (error
       (and buffer (display-buffer buffer))
       (error (error-message-string err))))))

(defun mpde-load-configuration-files ()
  "Load all the configuration mpde-config.el files.
This is done after the mpde library has been loaded.
Consequently, the mpde library MUST be the last Smallworld Library loaded,
to ensure that all libraries who have registered variables with MPDE are defined."
  (let ((alist mpde-configuration-path-alist)
	config
	name
	path)
    (while alist
      (setq config (car alist)
	    name (car config)
	    path (file-name-as-directory (cdr config)))
      (if (equal name "master")
	  nil ;ignore 'master configuration
	(message resources-mpde-loading-configuration name)
	(load (concat path "mpde-config") t))
      (setq alist (cdr alist)))))

(defun mpde-refresh-emacs ()
  "Reload the load_sw_defs library to update the Emacs session with the latest code.
This could cause instability in the Emacs session if code changes and data changes have
occured. Loading the libary code will not necessarily change the data structures
unless they can be set to nil and then regenerated."
  (interactive)
  (if (or (eq mpde-refresh-emacs t)
	  (and (eq mpde-refresh-emacs 'ask)
	       (yes-or-no-p (concat resources-mpde-yn-reload-elisp " "))))
      (let ((mpde-refresh-from-master nil)
	    (mpde-refresh-emacs nil))
	(load (concat-/ (get 'mpde-boot-site-lisp-dir mpde-configuration-current)
			"load_sw_defs")))))

(defun mpde-read-configuration (master-p &optional no-default)
  "Function to perform completing-read for configurations.
MASTER-P if t then the master configuration is an available option.
NO-DEFAULT if t no default is filled in. If a default is required
and the master configuration is current but MASTER-P is nil then one of
the other configurations is set as the default."
  (let ((default (cond (no-default nil)
		       (master-p (symbol-name mpde-configuration-current))
		       (t (car (car mpde-configuration-alist)))))
	(alist (if master-p
		   mpde-configuration-alist
		 (delete (assoc 'master mpde-configuration-alist)
			 (copy-alist mpde-configuration-alist)))))

    (intern
     (completing-read (concat resources-mpde-configuration-prompt " ")
		      (mapcar (lambda (x)
				(cons (symbol-name (car x)) (car x)))
			      alist)
		      nil t default nil default))))

(defun mpde-customize ()
  "Open Customization buffer for MPDE."
  (interactive)
  (customize-group 'mpde))

;;; Main functions
(defun mpde-set-configuration (&optional name toggle-refresh)
  "Setup the development environment configuration called NAME.
The configuration will be refreshed from the master if is available
and depending on the value of `mpde-refresh-from-master' and
TOGGLE-REFRESH which can also be set via use of a prefix argument.

So assuming the master is available, then the following table describes what happens

                             mpde-refresh-from-master
                  ----------------+--------------+---------------
                      |     t     |     'ask     |    nil
                  ----+-----------+--------------+---------------
                  nil | Refreshed | User prompt  |    Not
                      |           |              | Refreshed
Prefix Argument/  ----+-----------+--------------+---------------
toggle-refresh     t  |    Not    | User prompt  | Refreshed
                      | Refeshed  |              |

"
  (interactive (list (mpde-read-configuration (mpde-master-available-p) t)))
  (setq name (or name mpde-configuration-current))
  (or (assoc name mpde-configuration-alist)
      (error resources-mpde-configuration-unknown-error name))

  (setq mpde-configuration-current name)
  (mpde-refresh-resources nil
			  (function
			   (lambda (v sv)
			     (if (plist-member (symbol-plist v) mpde-configuration-current)
				 (set v (get v mpde-configuration-current))))))
  (if (or (eq mpde-configuration-current 'master)
	  (not (mpde-master-available-p)))
      nil
    (cond ((and (eq mpde-refresh-from-master 'ask)
		(y-or-n-p (format (concat resources-mpde-yn-refresh " ") name)))
	   (mpde-refresh-configuration))
	  ((and (eq mpde-refresh-from-master t)
		(not current-prefix-arg))
	   (mpde-refresh-configuration))
	  ((and (eq mpde-refresh-from-master nil)
		current-prefix-arg)
	   (mpde-refresh-configuration))
	  (t
	   nil)))
  (run-hooks 'mpde-set-configuration-hook)

  (if (get-buffer " *mpde display*")
      (mpde-display-available-configurations)))

(defun mpde-store-configuration (name)
  "Store the given configuration to the initialisation file, ~/.mpde and its mpde-config.el file.

Calling this also sets up the variables in your Emacs session."

  ;; TODO Need to store an mpde-config.el file and append to ~/.mpde.
  ;;      Should also do an Install unless already installed...

  (or mpde-boot-installed (call-interactively 'mpde-install))
  (mpde-set-configuration name)
  (mpde-store-configuration-file name)
  (mpde-store-configuration-definition name))

(defun mpde-store-configuration-file (name)
  "Create NAME configuration's mpde-config.el file."
  
  (let* ((path (cdr (assoc (symbol-name name) mpde-configuration-path-alist)))
	 (file (concat-/ path "mpde-config.el"))
	 (format-name (format "(quote %S)" name)) ;allows whitespace in name
	 buffer)
    (save-excursion
      (setq buffer (find-file file))
      (set-buffer buffer)
      (erase-buffer)

      (insert ";;; MPDE property settings\n")
      (mpde-refresh-resources nil
			      (function
			       (lambda (v sv)
				 (if (boundp v) ;protect against old registered variables
				     (insert
				      (format
				       "(mpde-register-variable '%s %S %s)\n"
				       (symbol-name v)
				       (get v name)
				       format-name))))))

      (insert "\n;;; Miscellaneous settings\n")
      (if mpde-store-configuration-file-hook
	  (progn
	    (insert "(if (eq mpde-configuration-current " format-name ")\n")
	    (insert "    (progn\n"
		    "       ;; additional output from mpde-store-configuration-file-hook:\n"
		    (format "      ;;%s\n" mpde-store-configuration-file-hook))

	    (run-hook-with-args 'mpde-store-configuration-file-hook name)

	    (insert "      ))\n\n")))

      (write-file file)
      (kill-buffer buffer))))

(defun mpde-store-configuration-definition (name)
  "Append configuration definition to ~/.mpde file."
  
  (let* ((file "~/.mpde")
	 (description (cdr (assoc name mpde-configuration-alist)))
	 (path        (cdr (assoc (symbol-name name) mpde-configuration-path-alist)))
	 buffer)
    (save-excursion
      (setq buffer (find-file file))
      (set-buffer buffer)
      (goto-char (point-max))
      (insert (format "(mpde-boot-configuration (quote %S) %S %S)\n"
		      name path description))
      (write-file file)
      (kill-buffer buffer))))

;;; Display functions
(defun mpde-display-internal (title insert-fn &optional more)
  "General Display buffer for configuration data.
TITLE is the title placed at the start of the buffer.
insert-fn is a function that takes arg CONFIGURATION.
FOOTER is optional text displayed at the end."
  (let ((available (reverse (mapcar 'car mpde-configuration-alist)))
	(current-buffer (current-buffer)) ; avoid save-excursion so we can set point
	(buffer    (get-buffer-create " *mpde display*")))
    (set-buffer buffer)
    (erase-buffer)
    (insert title
	    "\n"
	    (make-string (length title) ?=)
	    "\n\n")
    (while available
      (funcall insert-fn (car available))
      (setq available (cdr available)))
    (and more (insert "---\n" more "\n"))
    (goto-char (point-min))
    (set-buffer current-buffer)
    (display-buffer buffer)))

(defun mpde-display-resource (resource)
  "Display a variable's settings under available configurations.

* marks currently selected configuration
x indicates configuration not available or misconfigured"
  (interactive (list (intern (completing-read (concat resources-mpde-resource-variable ": ")
					      (mapcar (lambda (x)
							(cons (symbol-name x) x))
						      mpde-registered-variables)
					      nil t))))
  (mpde-display-internal (format (concat "%s" resources-mpde-resource-variable " %s")
				 (if (boundp resource) ""
				   (concat resources-mpde-resource-deprecated " "))
				 resource)
			 (function  ;;resource is a local variable to this defun.
			  (lambda (config) 
			    (let ((status ""))
			     (if (eq config mpde-configuration-current)
				 (setq status "*"))
			     (if (mpde-configuration-available-p config)
				 nil
			       (setq status (concat status "x")))
			     (insert (format "%-3s%-20s %S\n"
					     status
					     config
					     (if (plist-member (symbol-plist resource) config)
						 (get resource config)
					       'VOID))))))
			 (format (concat "   %-20s %S\n\n---\n   %-20s %S\n\n=====\n"
					 resources-mpde-resource-documentation "\n%s\n")
				 resources-mpde-resource-action
				 (get resource 'mpde-action)
				 resources-mpde-resource-current-value
				 (if (boundp resource) (symbol-value resource) 'VOID)
				 (or (documentation-property resource 'variable-documentation)
				     resources-mpde-resource-no-docstring))))

(defun mpde-display-available-configurations ()
  "Display list of available configurations.

* marks currently selected configuration
x indicates configuration not available or misconfigured"
  (interactive)
  (mpde-display-internal resources-mpde-available-configurations
			 (lambda (config)
			   (let ((status ""))
			     (if (eq config mpde-configuration-current)
				 (setq status "*"))
			     (if (mpde-configuration-available-p config)
				 nil
			       (setq status (concat status "x")))
			     (insert (format "%-3s%-20s \"%s\"\n"
					     status
					     config
					     (cdr (assoc config mpde-configuration-alist))))))
			 resources-mpde-available-configurations-help))

(defun mpde-display-configuration (&optional name)
  "Display configuration settings.

With a prefix arg, aask user for configuration to display."
  (interactive "P")
  (setq name (if current-prefix-arg
		 (mpde-read-configuration t t)
	       (or name mpde-configuration-current)))
  (let ((current-buffer (current-buffer)) ; avoid save-excursion so we can set point
	(buffer    (get-buffer-create " *mpde display*"))
	(title (format (concat "%s : " resources-mpde-resource-variables) name)))
    (set-buffer buffer)
    (erase-buffer)
    (insert title
	    "\n"
	    (make-string (length title) ?=)
	    "\n\n")
    (mpde-refresh-resources nil
			    (function
			     (lambda (v sv)
			       (let ((mpde-plist-p (plist-member (symbol-plist v) 'master)))
				 (insert (format "%-40s%s%S\n"
						 (symbol-name v)
						 (if mpde-plist-p " = " " : ")
						 (if (boundp v)
						     (if mpde-plist-p
							 (get v name)
						       (symbol-value v))
						   'VOID)))))))
    (goto-char (point-min))
    (set-buffer current-buffer)
    (display-buffer buffer)))

(defun mpde-display-startup ()
  "Display available configurations according to `mpde-display-startup'."
  (cond ((not mpde-display-startup)
	 nil)
	((or (eq mpde-display-startup t)
	     (and (symbolp mpde-display-startup)
		  (mpde-configuration-available-p mpde-display-startup))
	     (and (listp mpde-display-startup)
		  (let ((startup mpde-display-startup)
			display)
		    (while (and (not display) startup)
		      (setq display (assoc (car startup) mpde-configuration-alist))
		      (setq startup (cdr startup)))
		    display)))
	 (delete-other-windows)
	 (split-window-vertically)
	 (mpde-display-available-configurations))
	(t nil)))

(defun mpde-help ()
  "Display the documentation file, MPDE.txt, from the site-lisp directory."
  (interactive)
  (let ((help (get-buffer-create " *mpde help*"))
	(file (concat-/ mpde-boot-site-lisp-dir "MPDE.txt")))
    (set-buffer help)
    (erase-buffer)
    (insert-file-contents file)
    (goto-char (point-min))
    (display-buffer help t)))

(defun mpde-refresh-action-register-variable (var &optional new-value)
  "When refreshing VAR but is nil, registers VAR and sets it to VALUE for current configuration.
This is so that when an new variable is registered with MPDE
the current configuration's mpde-config.el is updated accordingly."

  (let* ((root-dir (cdr (assoc (symbol-name mpde-configuration-current) mpde-configuration-path-alist)))
	 (path (concat-/ root-dir (file-name-nondirectory (get var 'master))))
	 (mpde-type (get var 'mpde-type)))
    (cond ((or (eq mpde-type 'directory) (eq mpde-type 'file))
	   (mpde-register-variable var
				   (or new-value path)
				   mpde-configuration-current))
	  (t
	   (mpde-register-variable var
				   (or new-value (get var 'master))
				   mpde-configuration-current)))))

(defun mpde-refresh-resources (&optional type function message)
  "Loop over `mpde-registered-variables' and perform the mpde-action property function on each variable.
If optional TYPE is not nil, then it only performs the action if the mpde-type property matches TYPE.
If optional FUNCTION is given then that is performed instead of the registered mpde-action property function.
If optional MESSAGE is given then a message is displayed after each resource is refreshed."

  ;;This loop is written so that if a registered variable does not actually exist
  ;; then the loop continues to work - i.e. no error is returned if a variable
  ;; is not defined.
  ;; There are two reasons why this situation could occur.
  ;;  1. the registered variable is in a package not yet loaded
  ;;  2. The MPDE code has been updated and a user configuration's
  ;;     mpde-config.el is refering to an out of date variable.
  (let ((variables mpde-registered-variables)
	(fn-no-action (function
		       (lambda (var val)
			 (message
			  resources-mpde-action-no-action var))))
	(fn-void-var  (function
		       (lambda (var val)
			 (message resources-mpde-action-void-variable var))))
	v value fn mpde-type mpde-action)
    (while variables
      (setq v           (car variables)
	    value       (if (boundp v)
			    (or (get v mpde-configuration-current) (symbol-value v)))
	    mpde-type   (get v 'mpde-type)
	    mpde-action (if (boundp v)
			    (or function (get v 'mpde-action) fn-no-action)
			  fn-void-var))
      (if message
	  (message (concat message " [%s]") v)) 
      (if (or (eq type nil) (eq type mpde-type))
	  (progn
	    (if (functionp mpde-action)
		(funcall mpde-action v value)
	      ;; mpde-action is a list where car is the function and
	      ;;                         and cdr are the arguments    
	      (apply 'funcall (car mpde-action) v value (cdr mpde-action)))))
      (setq variables (cdr variables))
      (sit-for 0.001 1))))

(defun mpde-refresh-configuration (&optional name)
  "Makes sure current configuration is up-to-date with respect to the master configuration.
This only does something if the master configuration is available"
  (interactive)
  (if name (setq mpde-configuration-current name))
  (cond ((eq mpde-configuration-current 'master)
	 (error resources-mpde-refresh-master-error))
	((not (mpde-master-available-p))
	 (error resources-mpde-no-master-error))
	(t
	 (let* ((buffer (get-buffer-create " *mpde refresh*"))
		(template-boot (concat-/ (get 'mpde-boot-site-lisp-dir 'master) "smallworld/swlisp/mpde-boot.el")))
	   (message resources-mpde-refreshing-configuration mpde-configuration-current)
	   (switch-to-buffer-other-window buffer)
	   (sit-for 0.001 1)
	   (condition-case err
	       (progn
		 (goto-char (point-max))
		 (insert (format "\tConfiguration: %s\n" mpde-configuration-current))
		 ;;Refresh directories first - in case files are stored in them.
		 (display-buffer buffer)
		 (mpde-refresh-resources 'directory nil
					 (format (concat resources-mpde-refreshing-configuration
							 " "
							 resources-mpde-refreshing-directories)
						 mpde-configuration-current))
		 (display-buffer buffer)
		 (mpde-refresh-resources 'file      nil
					 (format (concat resources-mpde-refreshing-configuration
							 " "
							 resources-mpde-refreshing-files)
						 mpde-configuration-current))
		 (mpde-refresh-resources 'data      nil
					 (format (concat resources-mpde-refreshing-configuration
							 " "
							 resources-mpde-refreshing-data)
						 mpde-configuration-current))
		 (display-buffer buffer)

		 ;; Update ~/mpde-boot.el
		 (message (concat resources-mpde-refreshing-configuration
				  " "
				  resources-mpde-refreshing-boot)
			  mpde-configuration-current)
		 (insert (format (concat resources-mpde-copied-file-to)
				 template-boot "~/mpde-boot.el") "\n")
		 (copy-file template-boot "~/mpde-boot.el" t)
		 (display-buffer buffer)

		 (message (concat resources-mpde-refreshing-configuration " mpde-config")
			  mpde-configuration-current)
		 (insert (format resources-mpde-storing-file "mpde-config.el\n"))
		 (mpde-store-configuration-file mpde-configuration-current)

		 (run-hooks 'mpde-refresh-configuration-hook)
		 (bury-buffer buffer)
		 (message (concat resources-mpde-refreshing-configuration
				  " "
				  resources-mpde-refreshing-done)
			  mpde-configuration-current))
	     (error
	      (save-excursion
		(set-buffer buffer)
		(insert (error-message-string err) "\n")
		(message (concat resources-mpde-refreshing-configuration
				  " "
				  resources-mpde-refreshing-failed)
			 mpde-configuration-current))))))))

;; Refresh action functions - action used is obtained from mpde-action property.
(defun mpde-refresh-action-make-directory (var dir &rest args)
  "Refresh Action Function to make directory."

  ;;If dir is nil then var is newly registered with MPDE
  (setq dir (or dir (put var
			 mpde-configuration-current
			 (concat-/ (cdr (assoc (symbol-name mpde-configuration-current) mpde-configuration-path-alist))
				   (file-name-nondirectory (get var 'master))))))
  (if (file-directory-p dir)
      nil ;; directory already exists
    (if mpde-debug
	(insert resources-mpde-debug " ")
      (make-directory dir t))
    (insert (format resources-mpde-created-directory dir) "\n")))

(defun mpde-refresh-action-create-file (var file &rest args)
  "Refresh Action Function to create an empty file."

  ;;If file is nil then var is newly registered with MPDE
  (setq file (or file (put var
			   mpde-configuration-current
			   (concat-/ (cdr (assoc (symbol-name mpde-configuration-current) mpde-configuration-path-alist))
				     (file-name-nondirectory (get var 'master))))))

  (if (file-exists-p file)
      nil ;; File already exists
    (if mpde-debug
	(insert resources-mpde-debug " ")
      (save-excursion
	(let ((b (create-file-buffer file)))
	  (set-buffer b)
	  ;;(erase-buffer) ;;TOO dangerous!!!
	  (setq default-directory (file-name-directory file))
	  (write-file file)
	  (kill-buffer b))))
    (insert (format resources-mpde-creating-file file) "\n")))

(defun mpde-refresh-action-copy-file (var file &rest args)
  "Refresh Action Function to copy file from the master configuration to the current configuration."
  ;;If file is nil then var is newly registered with MPDE
  (setq file (or file (put var
			   mpde-configuration-current
			   (concat-/ (cdr (assoc (symbol-name mpde-configuration-current) mpde-configuration-path-alist))
				     (file-name-nondirectory (get var 'master))))))

  (let ((master (get var 'master)))
    (insert (format resources-mpde-copied-file-to master file) "\n")
    (if mpde-debug
	(insert resources-mpde-debug " ")
      (if (equal (file-truename file) (file-truename master))
	  t
	(and (file-exists-p file) (delete-file file))
	(apply 'copy-file master file args)))
  
    (insert (format resources-mpde-copied-file-to master file) "\n")))

(defun mpde-refresh-action-copy-directory (var dir &rest args)
  "Refresh Action Function to copy contents of one directory to another."

  ;;If dir is nil then var is newly registered with MPDE
  (if dir
      nil
    (setq dir (or dir (put var
			   mpde-configuration-current
			   (concat-/ (cdr (assoc (symbol-name mpde-configuration-current) mpde-configuration-path-alist))
				     (file-name-nondirectory (get var 'master))))))
    (mpde-register-variable var dir mpde-configuration-current))
  
  (let ((master (get var 'master)))
    (insert (format resources-mpde-copying-directory-to master dir) "\n")
    (if mpde-debug
	(insert resources-mpde-debug " ")
      (if (equal (file-truename dir) (file-truename master))
	  t
	(mpde-copy-directory master dir (format " *mpde: copy directory %s*" var))))
    (insert (format resources-mpde-copying-directory-to master dir) "\n")))

(defun mpde-refresh-action-copy-files (var dir &optional match)
  "Refresh Action Function to copy files from one directory to another.
Optional argument MATCH restricts copying to files that match MATCH."

  (mpde-refresh-action-make-directory var dir)

  (let ((master (get var 'master)))
    (insert (if match
		(format resources-mpde-copying-files-matching master match dir)
	      (format resources-mpde-copying-files master dir))
	    "\n")
    (if mpde-debug
	(insert "DEBUG: ")
      (if (equal (file-truename dir) (file-truename master))
	  t
	(mpde-copy-files master dir match (format " *mpde: copy files %s*" var))))
    (insert (if match
		(format resources-mpde-copied-files-matching master match dir)
	      (format resources-mpde-copied-files master dir))
	    "\n")))

;; Upload functionality
(defun mpde-upload-pending-p ()
  "Return t if there were patches submitted whilst offline, nil otherwise."
  (interactive)
  ;;TODO for performance only operate if not in master configuration
  (and (not (eq mpde-configuration-current 'master))
       (directory-files mpde-env-unsubmitted-patches-dir nil "\\.magik$" t)))

(defun mpde-upload-submissions ()
  "Using Dired mode select patches that have been submitted whilst offline."
  (interactive)
  ;;TODO: In future see if the Web based upload form can be used.
  (dired mpde-env-unsubmitted-patches-dir)
  (message resources-mpde-upload-submissions))

(defun mpde-dired-do-upload (&optional arg)
  "Perform the Upload of the selected files."
  (interactive "P")
  (let ((target (get 'mpde-env-proposed-patches-dir 'master)))
    (or (file-directory-p target)
	(error resources-sw-no-directory-error target))
    (dired-create-files
     (function dired-copy-file) "Upload" (dired-get-marked-files nil arg)
     (function
      (lambda (from)
	(expand-file-name (file-name-nondirectory from) target))))))

(define-key dired-mode-map "U" 'mpde-dired-do-upload)

;;	(define-key dired-mode-map [menu-bar operate sw-upload]
;;	  (list 'menu-item resources-mpde-menu-dired-upload 'mpde-dired-do-upload
;;		':active nil
;;		':help resources-mpde-menu-dired-upload-help)
;;	(menu-item "SW Upload..." mpde-dired-do-upload :active nil :help "SW Upload current file or all marked files"))
;;	(easy-menu-add '("Operate")
;;		       (list 'menu-item resources-mpde-menu-dired-upload 'mpde-dired-do-upload
;;			     ':active nil
;;			     ':help resources-mpde-menu-dired-upload-help))

(defun mpde-install ()
  "Install the MPDE files, ~/.mpde & ~/mpde-boot.el locally and update .emacs to load ~/.mpde."
  (interactive)
  (let* ((site-lisp     (get 'mpde-boot-site-lisp-dir 'master))
	 (template-dir  (file-name-as-directory (get 'mpde-boot-site-lisp-dir 'master)))
	 (template-file (if template-dir (concat template-dir "smallworld/swlisp/.mpde")))
	 (template-boot (if template-dir (concat template-dir "smallworld/swlisp/mpde-boot.el")))

	 (dotemacs-header ";;MPDE configuration")
	 (dotemacs-loader (concat "(load \"~/.mpde\")\n"))
	 (dotemacs-buffer (save-excursion (find-file (mpde-dotemacs-file)) (current-buffer))))
    (cond ((not (mpde-master-available-p))
	   (error resources-mpde-no-master-error))
	  ((not (file-exists-p template-file))
	   (error resources-mpde-no-master-template-error template-file))
	  ((not (file-exists-p template-boot))
	   (error resources-mpde-no-boot-library-error template-boot))
	  ((and (file-exists-p "~/.mpde")
		(not (yes-or-no-p (concat resources-mpde-yn-overwrite-dotmpde " "))))
	   (error resources-mpde-file-exists-error "~/.mpde"))
	  ((save-excursion
	     (set-buffer dotemacs-buffer)
	     (goto-char (point-min))
	     (if (save-match-data
		   (and (search-forward dotemacs-header nil t)
			(search-forward dotemacs-loader nil t)))
		 (if (not (yes-or-no-p (concat resources-mpde-yn-install-mpde " ")))
		     t ;; goto error below
		   (setq dotemacs-header nil   ;;Already loaded so do not bother
			 dotemacs-loader nil)
		   nil))) ;goto next cond 
	   (error resources-mpde-already-installed-error))
	  (t
	   (copy-file template-file "~/.mpde" t)
	   (copy-file template-boot "~/mpde-boot.el" t)
	   (set-buffer dotemacs-buffer)
	   (goto-char (point-min))
	   (if dotemacs-header
	       (progn
		 (insert dotemacs-header "\n"
			 dotemacs-loader "\n")
		 (goto-char (point-min))
		 (save-buffer)))
	   (display-buffer dotemacs-buffer)))))

(defun mpde-make-configuration (name root-dir description)
  "A simple interface to create a development configuration for offline working.
Often offline working is done on a laptop computer.

This command generally needs to be run just once to set up the configuration.
However, this command modifies your .emacs file so you may wish to modify your .emacs
file afterwards if you have a more complex setup."

  (interactive
   (cond ((not (eq mpde-configuration-current 'master))
	  (error resources-mpde-not-master-error))
	 ((not (mpde-master-available-p))
	  (error resources-mpde-no-master-error))
	 (t
	  (let* ((name (read-from-minibuffer (concat resources-mpde-configuration-prompt " ")
					     resources-mpde-configuration-default
					     nil nil nil
					     resources-mpde-configuration-default))
		 (upcase-name (upcase-initials name))
		 (description (format resources-mpde-configuration-description-default
				      upcase-name)))
	    (list (intern name)
		  (directory-file-name
		   (expand-file-name
		    (read-file-name (concat (format resources-mpde-configuration-root-dir upcase-name) " ")
				    mpde-configuration-root-dir
				    mpde-configuration-root-dir
				    nil)))
		  (read-from-minibuffer (concat resources-mpde-configuration-description-prompt " ")
					description nil nil nil description))))))

  (setq mpde-configuration-root-dir (file-truename (file-name-directory root-dir)))
  (if (file-directory-p mpde-configuration-root-dir)
	(make-directory root-dir)
      (or (y-or-n-p
	   (format resources-mpde-yn-create-root-directory
		   mpde-configuration-root-dir))
	  (error resources-mpde-no-parent-directory-error
		 mpde-configuration-root-dir))
      (make-directory root-dir t))
      
  (or mpde-boot-installed (call-interactively 'mpde-install))
  (mpde-boot-configuration name root-dir description)
  (mpde-refresh-resources 'directory
			  (function
			   (lambda (v sv)
			     (mpde-register-variable
			      v
			      (concat-/ root-dir (file-name-nondirectory sv))
			      name))))

  (mpde-refresh-resources 'file
			  (function
			   (lambda (v sv)
			     (mpde-register-variable
			      v
			      (concat-/ root-dir (file-name-nondirectory sv))
			      name))))

  (mpde-refresh-resources 'data
			  (function
			   (lambda (v sv)
			     (if (plist-member (symbol-plist v) 'master)
				 (mpde-register-variable v sv name)))))
  
  ;; TODO Handle mpde-boot-site-lisp-dir separately???
  (mpde-refresh-configuration name)
  (mpde-store-configuration name)
  (mpde-display-available-configurations))

(defun mpde-delete-configuration (name)
  "Remove a configuration.

Removes the root directory that contains configuratino data
and the configuration's entry in the ~/.mpde file.

For obvious reasons you cannot delete the master configuration!"
  (interactive (list (mpde-read-configuration nil t)))

  (if (eq name 'master)
      (error resources-mpde-delete-master-error))
  (if (eq name mpde-configuration-current)
      (error resources-mpde-delete-current-error mpde-configuration-current))

  (let ((root-dir (cdr (assoc (symbol-name name) mpde-configuration-path-alist))))
    (or (yes-or-no-p (format (concat resources-mpde-yn-delete-configuration " ") root-dir))
	(error resources-mpde-delete-abort-error name))
    
    ;;First remove entry from ~/.mpde
    (save-excursion
      (set-buffer (find-file "~/.mpde"))
      (if (re-search-forward (format "^(mpde-boot-configuration.*%S" name) nil t)
	  (progn
	    (beginning-of-line)
	    (kill-line 1)
	    (save-buffer)
	    (message resources-mpde-removed-entry name))))

    ;;Now delete directory
    (mpde-delete-directory root-dir)

    (setq mpde-configuration-path-alist
	  (delq (assoc (symbol-name name) mpde-configuration-path-alist)
		mpde-configuration-path-alist)
	  mpde-configuration-alist
	  (delq (assoc name mpde-configuration-alist)
		mpde-configuration-alist))
    
    (mpde-display-available-configurations)))

(defun mpde-menu-display ()
  "Return t if the setting of `mpde-menu-display' determines that the MPDE Menu should be displayed."
  (or (eq mpde-menu-display t)
      (and (integerp mpde-menu-display)
	   (>= (length (mpde-configurations-available-p)) mpde-menu-display))))

(if (mpde-menu-display)
    (easy-menu-change nil resources-mpde-menu (cdr mpde-menu)))

(add-hook 'mpde-refresh-configuration-hook 'mpde-refresh-emacs)

(add-hook 'mpde-after-load-hook 'mpde-set-configuration)
(add-hook 'mpde-after-load-hook 'mpde-load-configuration-files)
(add-hook 'mpde-after-load-hook 'mpde-display-startup)
(add-to-list 'after-load-alist '("mpde" (run-hooks 'mpde-after-load-hook)))

(provide 'mpde)

