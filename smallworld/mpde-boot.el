;; When MPDE is installed, this file is copied to the user's HOME
;; directory, ~/mpde-boot.el.
;;
;; When MPDE is installed, this code is loaded by the ~/.mpde file.
;; If MPDE is not installed, it is loaded by the mpde.el library.
;;
;; The code here defines the minimal set of functionality required to start MPDE.
;;
;; By keeping this code in a separate file and having ~/.mpde call it
;; this means that ~/.mpde contains minimal code so that the MPDE
;; refresh process is able to even update this file without needing to
;; touch the User's configuration file ~/.mpde.  Also by keeping this
;; code separate and having it installed in the HOME directory means
;; that MPDE can be used on local Smallworld installations, yet still
;; be using the latest Emacs code.

;; Customization
;; -------------
;; The User variables defined here are NOT suitable for control by the
;; Emacs Customization features.  The reason is that this code must be
;; loaded first in your .emacs but customization code is always added
;; at the end of the .emacs file If you do want to set any of the user
;; variables then you must set them before the loading of ~/.mpde for
;; them to take effect.

(defconst mpde-boot-version "$Revision: 1.2 $")

;; User variables
(defvar mpde-boot-configuration nil
  "*Default configuration to use at Emacs boot up.
If nil, then boot will offer choice of available configurations.")

(defvar mpde-boot-from-master t
  "*Controls whether the local configuration data is refreshed from the master.
If t,   then when the master configuration is available then a refresh will take place,
   nil  then no refresh occurs when \\[mpde-set-configuration] is used,
        you can still refresh manually by using \\[mpde-refresh-configuration]
        assuming the master configuration is available,
   'ask then the user is prompted for whether they wish to perform the refresh.

This is useful to set to nil or 'ask if master is available but only via a slow network link.")

(defvar mpde-boot-master-site-lisp (if (eq system-type 'windows-nt)
				       "u:/tools/general/emacs/site-lisp"
				     "/tools/general/emacs/site-lisp")
  "Directory containing the Master site-lisp code.")

(defvar mpde-boot-site-lisp-dir mpde-boot-master-site-lisp
  "*Directory containing the Main Elisp files.")

;; Other Variables. 
(defvar mpde-configuration-current 'master
  "The current configuration selected by the user.")

(defvar mpde-configuration-alist
  '((master . "The Master development environment configuration."))
  "Alist of environments that the user can change to.
Each car is a symbol and the cdr is a description string.
Normally this is simply the name given to the configuration where the development
environment is being run from the network.

\\[mpde-make-configuration] is the main user level interface for setting up
new development environment configurations.
This function modifies the various mpde-* variables and then calls
\\[mpde-store-configuration] for you to store your new development environment
configuration.

If you wish to create a more esoteric development environment then modify the
appropriate variables mpde-* yourself and then call \\[mpde-store-configuration]
directly.

All configurations are syncronised with the master configuration.")

(defvar mpde-configuration-path-alist
  (list (cons "master" (file-name-directory mpde-boot-master-site-lisp)))
  "Alist that stores the directory associated with each configuration.
Each car is a string and the cdr is a directory path.
The use of a string is so that it can be used with \\[completing-read] more easily.")

(defvar mpde-registered-variables nil
  "List of variables registered with MPDE.")

;;This is defvar'ed here so that it can be referenced in magik-patch.el
;;However, in mpde.el it is defcustom'ed so that the documentation string is set and
;;allows users to modify it - but only if they know what they are doing!
(defvar mpde-store-configuration-file-hook nil)

(defvar mpde-boot-installed nil
  "This records whether MPDE has been installed in your .emacs.")

(defvar mpde-boot-set-load-path nil
  "Ensure \\[mpde-boot-set-load-path] is only executed once, at boot time.")

(defvar mpde-master-available-p 'site-lisp
  "Variable controlling how `mpde-master-available-p' function behaves.
Users should use `mpde-refresh-from-master' variable to control
how MPDE updates from the master site.

This variable is only used to debug MPDE.

If 'site-lisp then master availability depends upon existence
of `mpde-boot-master-site-lisp' directory, otherwise returns value of
this variable, ie. nil or t.")

;; MPDE Functions
(defun mpde-master-available-p ()
  "Checks to see if the master site-lisp directory exists."
  (if (eq mpde-master-available-p 'site-lisp)
      (file-exists-p mpde-boot-master-site-lisp)
    mpde-master-available-p))

(defun mpde-configuration-available-p (name)
  "Checks to see if configuration NAME files are available.
Currently checks to see if NAME's site-lisp-dir exists.
If configuration is available then its site-lisp directory is returned,
otherwise nil."
  (let* ((configuration-dir       (cdr (assoc (symbol-name name)
					      mpde-configuration-path-alist)))
	 (configuration-site-lisp (concat (file-name-as-directory configuration-dir)
					  "site-lisp")))
    (and (file-exists-p configuration-site-lisp)
	 configuration-site-lisp)))

(defun mpde-configurations-available-p (&optional not-master)
  "Checks to see if any MPDE configurations (optionally excluding the master) are actually available.
Returns nil if no configurations are available or a list of those that are."

  (let ((alist mpde-configuration-alist)
	name
	available)
    (while alist
      (setq name (car (car alist)))
      
      (and (not (and not-master (eq name 'master))) ; exclude master check
	   (mpde-configuration-available-p name)
	   (add-to-list 'available name))

      (setq alist (cdr alist)))
    available))

(defun mpde-register-variable (var value configuration &optional action type)
  "Register VAR with MPDE.
Sets VAR to VALUE and also sets VAR's configuration setting to VALUE.
Optionally also sets up an ACTION type used when refreshing the configuration.

TYPE is set to either 'directory, 'file or 'data.
At refresh directory types are processed before file types and data types are
processed last.

When first registering a variable, if CONFIGURATION is set to nil,
then VAR will be registered with MPDE but will not be given configuration specific
values. This is useful when you simply need to set an ACTION on a variable
to provide MPDE specific features."

  (cond ((eq configuration 'master)
	 ;;Only set the current value of the variable when
	 ;; the master config is registered and current config is master.
	 ;; This allows reloading of library files when in a local config
	 ;; without overwriting the value.
	 (if (eq mpde-configuration-current 'master)
	     (set var value))
	 (put var 'master value)
	 (put var 'mpde-action action)
	 (put var 'mpde-type type))
	((eq configuration nil)
	 ;; variable is to be registered but not with different values for each configuration
	 (put var 'mpde-action action)
	 (put var 'mpde-type type))
	((get var 'master)
	 ;; Only set separate values for each configuration if configuration is given
	 ;; AND the master property has been set.
	 (put var configuration value))
	(t
	 ;; configuration is not master and master property has not been set.
	 ;; therefore we must be reading a configuration file.
	 nil))

  (add-to-list 'mpde-registered-variables var))

;;; Boot code
(defun mpde-boot-after-load ()
  "Code run after loading ~/.mpde Loader file."
  (if mpde-boot-set-load-path
      nil ;;Already booted!
    (let ((configuration (if (symbolp mpde-boot-configuration)
			     mpde-boot-configuration
			   (intern mpde-boot-configuration))))
      (if (and configuration (assq configuration mpde-configuration-alist))
	  (mpde-boot-set-load-path configuration)
	(call-interactively 'mpde-boot-set-load-path)))))

(defun mpde-boot-configuration (name path description)
  "Bootstrap the development environment configuration called NAME."
  (cond ((not (symbolp name))
	 (error "You must define configuration names as a symbol not a string"))
	((eq name 'master)
	 (error "Master Configuration cannot be redefined"))
	((eq name 'mpde-action)
	 (error "'%s' is a reserved name" name))
	(t
	 (add-to-list 'mpde-configuration-alist
		      (cons name description))
	 (add-to-list 'mpde-configuration-path-alist
		      (cons (symbol-name name) path)))))

(defun mpde-boot-set-load-path (configuration)
  "Function to set `load-path' to master or current configuration.
The load-path is only modified once. I.e. it is NOT intended to modify
load-path and hence reload the Elisp libraries depending on the current configuration.
Then it reloads mpde and the dependent libraries as given by
`mpde-required-libraries'.

The policy is that if you use MPDE then you should be using the latest possible
site-lisp code which means either that from the master configuration
or if that is not accessible then you use the current configuration.

To use an installed Emacs' site-lisp directory code simply comment out
the (load \"<mpde-configuration-file>\") command from your .emacs file.

This function is added to the `mpde-after-load-hook' which is explicitly run by your
.emacs file."
  (interactive
   (let* ((alist (if (mpde-master-available-p)
		   mpde-configuration-path-alist
		 (delete (assoc "master" mpde-configuration-path-alist)
			 (copy-alist mpde-configuration-path-alist))))
	  (len (length alist)))
     (list
      (intern
       (if (eq len 1)
	   ;; only one configuration found either master
	   ;; or one configuration and master not available
	   (car (car alist))
	 (let ((c "")
	       (prompt "Select Configuration: "))
	   (while (equal "" c)
	     (setq c (completing-read prompt alist nil t)
		   prompt "Select Configuration (press TAB to list and select): "))
	   c))))))

  (setq mpde-boot-set-load-path t)
  (let* ((master-available-p      (mpde-master-available-p))
	 (configuration-dir       (cdr (assoc (symbol-name configuration)
					      mpde-configuration-path-alist)))
	 (configuration-site-lisp (concat (file-name-as-directory configuration-dir)
					  "site-lisp")))
    (or master-available-p
	(message "MPDE: master configuration site-lisp is not available: %s" mpde-boot-master-site-lisp))
    (cond
     ((and configuration-dir
	   (file-exists-p configuration-site-lisp))
      (message "MPDE: Configuration %s. Loading: %s"
	       configuration configuration-site-lisp)
      (setq mpde-configuration-current configuration
	    load-path (cons configuration-site-lisp load-path)))
     ((and (eq mpde-boot-from-master 'ask)
	   master-available-p
	   (y-or-n-p "Load Emacs code from master? "))
      (setq load-path (cons mpde-boot-master-site-lisp load-path)))
     ((and (eq mpde-boot-from-master t)
	   master-available-p)
      (setq load-path (cons mpde-boot-master-site-lisp load-path)))
     (t
      (message "MPDE: load-path not updated. Using unmodified emacs environment..."))))

  ;;Ensure subdirs.el and site-start.el file loaded.
  (let ((default-directory (car load-path)))
    (load (expand-file-name "subdirs.el") t)
  ;  (load (expand-file-name "site-start.el") t)
  ))

(mpde-register-variable 'mpde-boot-site-lisp-dir
			mpde-boot-master-site-lisp
			'master
			'mpde-refresh-action-copy-directory
			'directory)

(provide 'mpde-boot)
