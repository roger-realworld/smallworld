;; package --- Interface to the gis_version environment switching tool.
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'sw-aliases-mode)

(defvar sw-gis-version-position nil)
(defvar sw-gis-version nil)
(defvar aliases-process-environment nil)
(defvar aliases-exec-path nil)
(defvar aliases-program-args nil)
(defvar sw-original-process-environment nil)
(defvar sw-original-exec-path nil)

(defgroup sw-gis-version nil
  "Multiple Smallworld Environments."
  :tag "Smallworld Environment"
  :group 'smallworld)

(defcustom sw-gis-version-frame-title-format '(multiple-frames "%b"
					    ("" invocation-name "@" system-name " " sw-gis-version-current))
  "*The frame title string to use when a version has been selected."
  :group 'sw-gis-version
  :type  'sexp)

(defcustom sw-gis-version-icon-title-format sw-gis-version-frame-title-format
  "*The icon title string to use when a version has been selected."
  :group 'sw-gis-version
  :type  'sexp)

(defcustom sw-gis-version-program "gis_version"
  "*The program name for the gis_version environment program."
  :group 'sw-gis-version
  :type  'file)

(defcustom sw-gis-version-file (let ((file (concat (file-name-as-directory (getenv "HOME"))
						"gis_version.txt")))
			      (if (file-exists-p file)
				  file))
  "*A file containing locations of GIS versions.
This provides an alternative interface to a gis_version program."
  :group 'sw-gis-version
  :type  '(choice (file)
		  (const nil)))

(defcustom sw-gis-version-match "^[* ] \\(\\S-+\\)\\s-*\\(\\S-+\\)\\s-*\\(.*\\)"
  "*Regexp matching valid versions listed by `sw-gis-version-program' or `sw-gis-version-file'."
  :group 'sw-gis-version
  :type  'regexp)

(defcustom sw-gis-version-program-error "gis_version: "
  "*The error string that the gis version program returns when stream is invalid."
  :group 'sw-gis-version
  :type  'string)

(defcustom sw-gis-version-invalid-string "(invalid)"
  "*The string marking an invalid gis version entry."
  :group 'sw-gis-version
  :type  'string)

(defcustom sw-gis-version-select-hook nil
  "*Hook to run after a selection has been made."
  :group 'sw-gis-version
  :type  'hook)
(add-hook 'sw-gis-version-select-hook  'aliases-update-sw-menu)

(defcustom sw-gis-version-help resources-sw-gis-version-help
  "Help text displayed at top of gis_version buffer."
  :group 'sw-gis-version
  :type  'string)

(defcustom sw-gis-version-help-file-add resources-sw-gis-version-help-file-add
  "Help text for Adding to `sw-gis-version-file' displayed after `sw-gis-version-help'."
  :group 'sw-gis-version
  :type  'string)

(defcustom sw-gis-version-mode-font-lock-defaults
  (list
   '("^.*(invalid).*$" . font-lock-warning-face)
   '("^\\([*]\\s-+\\S-+\\)\\s-+\\(\\S-+\\)"
     (1 font-lock-constant-face)
     (2 font-lock-variable-name-face))
   '("^ \\s-+\\(\\S-+\\)\\s-+\\(\\S-+\\)"
     (1 font-lock-function-name-face)
     (2 font-lock-variable-name-face))
   '("^\\S-.*" . font-lock-doc-face)
   )
  "Default fontification of gis_version."
  :group 'sw-gis-version
  :type 'sexp)

(defvar sw-gis-version-file-format "  %-17s   %-23s   %s\n"
  "*Header format to use for a newly created gis version file.")

(defvar sw-gis-version-file-header
  (concat (format sw-gis-version-file-format "name" "version" "directory")
	  (format sw-gis-version-file-format
		  "-----------------"
		  "------------------------"
		  "--------------------------------"))
  "*Header string to use for a newly created gis version file.")

(defvar sw-gis-version-sw-path-list nil
  "Stores list of Smallworld directories added to PATH.")

(defvar sw-gis-version-mode-map (make-sparse-keymap)
  "Keymap for selection of alternative GIS environments.")

(defvar sw-gis-version-menu nil
  "Keymap for the gis_version buffer menu bar.")



(define-key sw-gis-version-mode-map [f1]   'sw-gis-version-help)
(define-key sw-gis-version-mode-map " "    'sw-gis-version-next)
(define-key sw-gis-version-mode-map "a"    'sw-gis-version-gis-aliases)
(define-key sw-gis-version-mode-map "+"    'sw-gis-version-file-add)
(define-key sw-gis-version-mode-map "q"    'sw-gis-version-quit)
(define-key sw-gis-version-mode-map "r"    'sw-gis-version-run)
(define-key sw-gis-version-mode-map "\r"   'sw-gis-version-select)
(define-key sw-gis-version-mode-map [mouse-2] 'sw-gis-version-mouse-select)

(defvar sw-gis-version-mode-syntax-table nil
  "Syntax table in use in DEF-mode buffers.")

(defvar sw-gis-version-current nil
  "Current gis_version stream.")
(make-variable-buffer-local 'sw-gis-version-current)

(defvar sw-gis-version-position nil
  "A position in the gis version buffer above which the user shouldn't click.")

(defun sw-gis-version-customize ()
  "Open Customization buffer for Sw-Gis-Version Mode."
  (interactive)
  (customize-group 'sw-gis-version))

(defun sw-gis-version (sw-gis-version)
  "Return the output from running the gis version script SW-GIS-VERSION.
As opposed to running the alias.
If it fails, return the symbol, 'failed."
  (with-temp-buffer
    (if sw-gis-version-file
	(insert-file-contents sw-gis-version-file)
      (call-process sw-gis-version-program nil t nil sw-gis-version))
    (goto-char (point-min))
    (prog1
        (if (search-forward sw-gis-version-program-error nil t)
            'failed
          (buffer-string))
      (kill-buffer (current-buffer)))))

(defun sw-gis-version-program-current ()
  "Run `sw-gis-version-program' and determine the current version.
Return the current gis version that this Emacs inherited from its environment,
or nil if there isn't one, or 'no-sw-gis-version-script if there is no gis version command."
  (with-temp-buffer
    (condition-case nil
        (progn
          (call-process sw-gis-version-program nil t nil)
          (goto-char (point-min))
          (prog1
              (if (search-forward "\n* " nil t)
                  (buffer-substring
		   (point)
		   (progn (search-forward " ") (1- (point))))
                nil)
            (kill-buffer (current-buffer))))
      (error
       (kill-buffer (current-buffer))
       'no-sw-gis-version-script))))

(defun sw-gis-version-run ()
  "Run GIS command in selected version."
  (interactive)
  (beginning-of-line)
  (let ((process-environment (cl-copy-list process-environment))
	(exec-path (cl-copy-list exec-path))
	stream version buffer)
    (setq stream (car (sw-gis-version-select-internal))
	  buffer  (concat "*gis " stream "*"))
    (gis buffer)
    (setq sw-gis-version-current stream)))

(defun sw-gis-version-gis-aliases ()
  "Open gis_aliases file of selected version.
Will prompt for layered product to use if selected version
has more than one aliases file available."
  ;;Does not cope with 'partial stream versions' where the directory components list 2 (or more directories)
  (interactive)
  (beginning-of-line)
  (let* ((process-environment (cl-copy-list process-environment))
	 (exec-path (cl-copy-list exec-path))
	 (version-list (sw-gis-version-select-internal))
	 lp-alist
	 alias-file)
    (if (null (car version-list))
	(error "Invalid selection")
      (setq lp-alist (aliases-layered-products-file
			   (aliases-expand-file "$SMALLWORLD_GIS/config/LAYERED_PRODUCTS")))
      (cond ((null lp-alist) nil)
	    ((eq (length lp-alist) 1)
	     (setq alias-file (concat (cdar lp-alist) "/config/gis_aliases")))
	    (t
	     (let* ((lp   (completing-read (concat "Select a Layered Product with gis_aliases file:" " ") lp-alist nil t))
		    (path (cdr (assoc lp lp-alist))))
	       (if path
		   (setq alias-file (concat path "/config/gis_aliases"))))))
      (if alias-file
	  (progn
	    (find-file alias-file)
	    (setq aliases-process-environment (cl-copy-list process-environment)
		  aliases-exec-path (cl-copy-list exec-path)
		  aliases-program-args (list "-p" (caddr version-list))
		  sw-gis-version-current (car version-list))
	    ;;(set 'aliases-sw-gis-version-current current-version)
	    )))))

(defun sw-gis-version-next ()
  "Move point to next valid version listed."
  (interactive)
  (forward-line 1)
  (save-match-data
    (while (and (not (eobp))
		(or (looking-at (concat "^.*" sw-gis-version-invalid-string))
		    (not (looking-at sw-gis-version-match))))
      (forward-line 1))
    (if (not (eobp))
	(beginning-of-line)
      (goto-char sw-gis-version-position)
      (re-search-forward sw-gis-version-match nil t)
      (beginning-of-line))))
  
(defun sw-gis-version-current ()
  "Return the current gis_version."
  (interactive)
  (if sw-gis-version-file
      sw-gis-version-current
    (sw-gis-version-program-current)))

(define-derived-mode sw-gis-version-mode prog-mode "sw-gis-version"
  "Major Mode for gis_version."
  :group 'smallworld
  (setq font-lock-defaults sw-gis-version-mode-font-lock-defaults))
  

(defun sw-gis-version-smallworld-gis-p (path)
  "Return t if PATH points to a Smallworld installation."
  (file-directory-p (concat (file-name-directory path) "config")))

(defun sw-gis-version-read-smallworld-gis-completion (string predicate flag)
  "Provide directory completion for finding Smallworld installations.
Starts with STRING PREDICATE and FLAG.
Repeated TAB and \\[minibuffer-completion-help] still provide
directory listing so users can navigate a directory structure looking
for a Smallworld installation.  Only when
`sw-gis-version-smallworld-gis-p' returns t for a given path will the
path be considered to be a real Smallworld installation directory
suitable for selection."
  (if (sw-gis-version-smallworld-gis-p string)
      (cond ((eq flag 'lambda) t)
	    ((null flag)       t)
	    (t            string))
    (cl-flet ((read-smallworld-gis-predicate (d) (equal d (file-name-directory d))))
      (let ((root (file-name-directory string))
	    (completions (all-completions string
					  'read-file-name-internal
					  'read-smallworld-gis-predicate)))
	(cond ((or (eq this-command 'minibuffer-completion-help)
		   (and flag (eq this-command 'minibuffer-complete)))
	       ;;Provide directory completions for user feedback ONLY
	       (mapcar (function (lambda (d) (concat root d))) completions))
	      (flag
	       ;; all-completions. Do not want to return anything here
	       ;; otherwise any directory is accepted after a Return
	       nil)
	      (t
	       ;;try-completion
	       (setq completions
		     (try-completion (file-name-nondirectory string)
				     (mapcar 'list completions)))
	       (if (eq completions t)
		   string
		 (concat (or root "") completions))))))))
  
(defun sw-gis-version-read-smallworld-gis ()
  "Prompt for a valid value for SMALLWORLD_GIS."
  (let ((path
	 (completing-read (concat "Enter product directory for Core installation:" " ")
			  'sw-gis-version-read-smallworld-gis-completion
			  nil t nil nil (getenv "SMALLWORLD_GIS"))))
    (setq path (directory-file-name (file-name-directory path)))
    (if (running-under-nt)
	(subst-char-in-string ?/ ?\\ path t))
    path))

(defun sw-gis-version-file-add (root name version)
  "Add a new entry to the file given by `sw-gis-version-file'.
The new entry contains ROOT, NAME and VERSION."
  (interactive
   (let* ((ok (or sw-gis-version-file
		  (error "File interface is not being used")))
	  (root (sw-gis-version-read-smallworld-gis))
	  (product-version-file (concat (file-name-as-directory root)
					"config/PRODUCT_VERSION"))
	  name version)
     (if (file-exists-p product-version-file)
	 (with-current-buffer (get-buffer-create " *product_version*")
	   (erase-buffer)
	   (insert-file-contents product-version-file)
	   (goto-char (point-min))
	   (setq version (current-word)
		 name    version)))
     (list root
	   (read-string "Enter name for this installation:" name)
	   (read-string "Enter version number of this installation: " version))))
  
  (or sw-gis-version-file
      (error "File interface is not being used"))
  (with-current-buffer (find-file-noselect sw-gis-version-file)
    (goto-char (point-max))
    (let ((inhibit-read-only t))
      (insert (format sw-gis-version-file-format name version root))
      (save-buffer)))
  (if (eq major-mode 'sw-gis-version-mode)
      (sw-gis-version-selection)))

(defun sw-gis-version-file-create (file root)
  "Create a gis version format file based upon the current environment.
Called if no sw-gis-version program exists or `sw-gis-version-file' is nil.
Will set `sw-gis-version-file' to FILE and ROOT."
  (interactive
   (if (y-or-n-p (concat "Create File Interface for SW Environments?" " "))
       (let ((smallworld-gis (getenv "SMALLWORLD_GIS"))
	     (dir (file-name-as-directory (getenv "HOME")))
	     (file "gis_version.txt"))
	 (if smallworld-gis
	     nil
	   (setq smallworld-gis (sw-gis-version-read-smallworld-gis)))
	 (list
	  (read-file-name "Enter New Environment List File: " dir file nil file)
	  smallworld-gis))
     (error "")))
  (setq sw-gis-version-file file)
  
  (if (file-exists-p file)
      (find-file file)
    (find-file file)
    (insert sw-gis-version-file-header)
    (let ((process-environment (cl-copy-list process-environment)))
      (setenv "SMALLWORLD_GIS" root)
       (call-interactively 'sw-gis-version-file-add))
    (save-buffer))
  nil)

(defun sw-gis-version-selection ()
  "Display a list of possible gis products for the user to choose between."
  (interactive
   (cond (sw-gis-version-file
	  ;; already supplied a file
	  nil)
	 ((eq sw-gis-version-current 'no-sw-gis-version-script)
	  (call-interactively 'sw-gis-version-file-create))
	 (t nil)))

  ;(if (eq sw-gis-version-current 'no-sw-gis-version-script)
  ;    (error resources-g-v-no-command-error sw-gis-version-program))
  (set-buffer (get-buffer-create "*gis version selection*"))
  (sw-gis-version-mode)

  (message "Starting %s selection..." sw-gis-version-program)

  (setq buffer-read-only nil)
  (erase-buffer)
  (insert sw-gis-version-help)
  (if (and sw-gis-version-file sw-gis-version-help-file-add)
      (insert "\n" sw-gis-version-help-file-add "\n"))

  (save-excursion
    (save-match-data
      (if (not sw-gis-version-file)
	  (call-process sw-gis-version-program nil t nil)
	(insert-file-contents sw-gis-version-file)
	(goto-char (point-min))
	
	(if (search-forward "-------" nil t) (forward-line 1)) ;skip a header
	(while (re-search-forward sw-gis-version-match nil t)
	  (beginning-of-line)
	  (forward-char 1)
	  (backward-delete-char 1)
	  (insert " ")
	  (cond ((string-match sw-gis-version-invalid-string (match-string-no-properties 3))
		 nil)
		((file-exists-p (match-string-no-properties 3))
		 nil)
		(t
		 (goto-char (match-beginning 3))
		 (insert sw-gis-version-invalid-string " ")))))))

  (if (stringp sw-gis-version-current)
      (save-excursion
	(save-match-data
	  (if (re-search-forward (concat "^. " sw-gis-version-current " ") nil t)
	      (progn
		(beginning-of-line)
		(delete-char 1)
		(insert "*"))))))
  
  (setq sw-gis-version-position (point))
  (save-match-data
    (if (search-forward "-------" nil t) (setq sw-gis-version-position (point)))) ;skip a header

  (message "Starting %s selection... done" sw-gis-version-program)
  (setq buffer-read-only t)
  (set-buffer-modified-p nil)
  (switch-to-buffer (current-buffer)))

(defun sw-gis-version-quit ()
  "Quit, without selecting anything, gis version selection mode."
  (interactive)
  (kill-buffer (current-buffer)))

(defun sw-gis-version-display-title ()
  "Modify the Frame and Icon titles according to the Environment."
  (interactive)
  (if sw-gis-version-frame-title-format
      (setq frame-title-format sw-gis-version-frame-title-format))
  (if sw-gis-version-icon-title-format
      (setq icon-title-format  sw-gis-version-icon-title-format)))

(defun sw-gis-version-mouse-select (click)
  "Choose product using mouse event CLICK."
  (interactive "e")
  (mouse-set-point click)
  (beginning-of-line)
  (sw-gis-version-select))

(defun sw-gis-version-select ()
  "Store the gis product name.
Store it in the global variable, `sw-gis-version-current'.
So that `F2 z' will set the correct product's environment
before starting the gis.
Also now update the Emacs `process-environment' correctly.
The frame and icon title strings will be modified according to
`sw-gis-version-frame-title-format' and `sw-gis-version-icon-title-format'."
  (interactive)
  (let ((stream (car (sw-gis-version-select-internal))))
    (setq-default sw-gis-version-current stream)
    (kill-buffer (current-buffer))
    (sw-gis-version-display-title)
    (run-hooks 'sw-gis-version-select-hook)
    (message "The current installation for this Emacs is now %s." stream)))

(defun sw-gis-version-select-internal ()
  "Modify `process-environment' and `exec-path' for current version.
Return (STREAM VERSION SMALLWORLD_GIS)."
  (if (< (point) sw-gis-version-position)
      (error "No Environment at this point"))
  (if (save-excursion
        (beginning-of-line)
        (search-forward sw-gis-version-invalid-string (point-eol) t))
      (error "You have selected an (invalid) Environment"))
  (let (stream
	version
	smallworld-gis)
    (beginning-of-line)
    (if (looking-at sw-gis-version-match)
	(setq stream (match-string-no-properties 1)
	      version (match-string-no-properties 2)
	      smallworld-gis  (match-string-no-properties 3))
      (error "No Environment on this line"))
    (if (not (and sw-gis-version-current
		  (string-equal stream sw-gis-version-current)))
	(sw-gis-version-set-environment smallworld-gis
				     stream
				     version
				     (if (and sw-gis-version-program
					      (not sw-gis-version-file))
					 sw-gis-version-program)))
    (list stream version smallworld-gis)))

(defun sw-gis-version-set-environment (smallworld-gis stream version program)
  "Modify the process and 'exec-path' environment.
Modify it given SMALLWORLD-GIS, STREAM, VERSION and PROGRAM."
  (setenv "SMALLWORLD_GIS" smallworld-gis)
  (setenv "SW_STREAM" stream)
  (setenv "SW_VERSION" version)

  (with-temp-buffer
    (erase-buffer)
    (cond ((and (running-under-nt) program)
	   (sw-gis-version-call-process-windows program nil t nil stream "&&" "set"))
	  ((running-under-nt)
	   ;; Use cmdproxy explicitly and not shell-file-name since Cygwin
	   ;; may be being used and in this case 'set' sometimes reports shell values
	   ;; to be VAR='c:/path/to/file' i.e it inserts ''s.
	   (sw-gis-version-call-process-windows "cmdproxy" nil t nil
			"/c"
			(concat "echo off && "
				(expand-file-name "config/environment.bat"  smallworld-gis)
				"&& set")))
	  ;; UNIX
	  (program
	   (sw-gis-version-call-process-unix (concat "eval `" program " " stream "`")))
	  (t
	   (sw-gis-version-call-process-unix
	    (concat ". " (expand-file-name "data/setup/SHARED_profile" smallworld-gis)))))
    (goto-char (point-min))
    (sw-gis-version-set-emacs-environment)))

(defun sw-gis-version-call-process-windows (&rest args)
  "Run Windows command with ARGS and return its environment variables."
  (let ((default-directory temporary-file-directory) ;avoid command shell complaining about cwd being a UNC path
	(w32-quote-process-args t)   ;ensure quoting of arguments
	(win32-quote-process-args t));
    (apply 'call-process args)))

(defun sw-gis-version-call-process-unix (command)
  "Run unix COMMAND and return its environment variables."
  (call-process "/bin/sh" nil t nil "-c"
		(concat "SHELL=/bin/sh ; export SHELL ; " command " ; env")))

(defun sw-gis-version-prepend-sw-paths (orig new)
  "Ensure Smallworld directories are prepended to PATH variable.
The original paths are in ORIG.
The new paths are in NEW.
For sw-gis-version code to work the SMALLWORLD paths
 need to be prepended to PATH.
On UNIX PATH is modified to have SMALLWORLD appended
\(on Windows it is prepended).

Also sets `sw-gis-version-sw-path-list' to be the
list of directories added to PATH
by the current Smallworld version."
  (save-match-data
    (let ((new-list  (delq nil (parse-colon-path new)))
	  (orig-list (delq nil (parse-colon-path orig)))
	  sw-list
	  cnt)
      ;; Collect new paths added in sw-list
      (dolist (p new-list)
	(cond ((not (member p orig-list))
	       (setq sw-list (append sw-list (list p))))
	      ((and (> (setq cnt (count p new-list :test 'equal)) 1)
		    (not (eq cnt (count p orig-list :test 'equal))))
	       ;;Found mismatching numbers of multiple entries
	       ;;selected version has added more
	       (or (member p sw-list)
		   (setq sw-list (append sw-list (list p)))))
	      (t
	       nil)))
      ;;remove previous SW paths using sw-gis-version-sw-list
      ;;remove new SW paths first then prepend them.
      (dolist (p (append sw-list sw-gis-version-sw-path-list))
	(if (member p new-list)
	    (setq new-list (delete p new-list))))
      (setq sw-gis-version-sw-path-list (cl-copy-list sw-list)
	    new (mapconcat 'directory-file-name
			   (append sw-list new-list)
			   path-separator))
      (if (running-under-nt)
	  (subst-char-in-string ?/ ?\\ new)
	new))))

(defun sw-gis-version-set-emacs-environment ()
  "Update `process-environment' and `exec-path' variables."
  (let* ((orig-shell (getenv "SHELL"))
	 (orig-path  (getenv "PATH")))
    (setq process-environment nil)
    (while (not (eobp))
      (if (looking-at ".*=")
	  (push (buffer-substring (point-bol) (point-eol)) process-environment))
      (forward-line 1))
    (setenv "SHELL" orig-shell)
    (setenv "PATH" (sw-gis-version-prepend-sw-paths orig-path (getenv "PATH")))
    (setq exec-path (append (parse-colon-path (getenv "PATH"))
			    (parse-colon-path (getenv "EMACSPATH"))))))

(defun sw-gis-version-reset-emacs-environment ()
  "Reset the Emacs environment back to how it was when Emacs started.
This affects all Environment variables `process-environment'
and the execution path `exec-path'."
  (interactive)
  (if (yes-or-no-p "Are you sure you want to reset the process environment?")
	(setq process-environment (cl-copy-list sw-original-process-environment)
	      exec-path           (cl-copy-list sw-original-exec-path))
	(setq-default sw-gis-version-current nil)
	(sw-gis-version-selection)))

(defun sw-gis-version-header-string ()
  "Insert a string describing the gis_version status.
Used before running a GIS process."

  (let (sw-gis-version-script)
    (if (stringp sw-gis-version-current)
	(setq sw-gis-version-script (sw-gis-version sw-gis-version-current)))
    (cond ((eq sw-gis-version-current 'no-sw-gis-version-script)
	   nil)
	  ((null sw-gis-version-current)
	   (insert "\n" "None selected" "\n"))
	  ((eq sw-gis-version-script 'failed)
	   (insert "\n"
		   (format "** Can't find the currently selected product, %s." sw-gis-version-current)
		   "\n"
		   "** (Attempting to run anyway)."
		   "\n"))
	  (t
	   (insert (format "Gis Environment: %s" sw-gis-version-current) "\n")))))

;;; Package initialisation
(if sw-gis-version-mode-syntax-table
    nil
  (setq sw-gis-version-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?_  "w"  sw-gis-version-mode-syntax-table))

(setq-default sw-gis-version-current (sw-gis-version-current))

(add-hook 'gis-start-process-pre-hook 'sw-gis-version-header-string)

(provide 'sw-gis-version)

;;; sw-gis-version-mode.el ends here
