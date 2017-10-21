;;; sw-aliases.el -- mode for editing GIS sw-aliases files.
;;; Commentary:
;;; Code:

(require 'cl-lib)
(autoload 'gis-mode "gis")
(autoload 'gis-start-process "gis")
(autoload 'sw-buffer-mode-list "utils-sw")
(autoload 'gis-version-current "gis-version")
 
(defvar gis-exec-path)
(defvar gis-process-environment)
(defvar gis-current-command)
(defvar gis-version-current)

(defcustom sw-aliases-user-file-list '("$HOME/gis_aliases")
  "A list of a User's personal gis_aliases files."
  :group 'aliases
  :type  'file)

(defcustom sw-aliases-common-file-list nil
  "*List of common gis_aliases files.
This list is expected to be setup by the Emacs maintainer,
a user can setup their personal gis_aliases file list using
`aliases-user-file-list'.  Both these lists are concatenated to
form the top section of the SW->Alias Files submenu."
  :group 'aliases
  :type  '(repeat file))

(defcustom sw-aliases-mode-hook nil
  "*Hook to run after SW-ALIASES mode is set."
  :group 'aliases
  :type  'hook)

(defcustom sw-aliases-program (if (eq system-type 'windows-nt)
			       "runalias.exe"
			     "gis")
  "*Program to process an alias file."
  :group 'aliases
  :type  'string)

(defcustom sw-aliases-program-path (if (eq system-type 'windows-nt)
				    '("../bin/x86" "../../product/bin/x86")
				  '("../bin/share" "../../product/bin/share"))
  "*Path to `aliases-program'.
Setting this sets the default value.  When opening a gis_aliases file,
the buffer local value of this variable will be set to the directory
containing the `aliases-program' if it is in a relative path to the file."
  :group 'aliases
  :type  '(repeat directory))

(defcustom sw-aliases-program-args nil
  "*Arguments to pass to `aliases-program'."
  :group 'aliases
  :type  '(repeat string))

(defcustom sw-aliases-switch-to-buffer t
  "*User control for switching to the process buffer of a selected alias.
If this is t then the buffer is displayed."
  :group 'aliases
  :type  'boolean)

(defcustom sw-aliases-switch-to-buffer-regexp nil
  "*User control for switching to the process buffer of a selected alias.
If the alias name matches the given regular expression the buffer
is displayed."
  :group 'aliases
  :type  '(choice regexp (const nil)))

(defcustom sw-aliases-switch-to-buffer-hooks nil
  "*User control for switching to the process buffer of a selected alias.
Each function in the hook is passed the name of the alias.
If any function returns t, then the buffer is displayed."
  :group 'aliases
  :type  'hook)

(defvar sw-aliases-mode-map (make-sparse-keymap)
  "Keymap for GIS sw-aliases files.")

(define-key sw-aliases-mode-map [f1]   'aliases-help)
(define-key sw-aliases-mode-map "\C-c" 'aliases-run-program)

(defvar sw-aliases-definition-regexp "^\\([^#]\\S-+\\):\\s-*$"
  "Regexp matching an alias definition.")

;; Font-lock configuration
(defvar sw-aliases-mode-font-lock-defaults
  '((
     (cons sw-sw-aliases-definition-regexp 'font-lock-function-name-face)
     ("^\\s-+\\([A-Z_]+\\)\\s-*:=" 1 font-lock-type-face)
     ("^\\(setq )-+\\([A-Z_]+\\)\\s-*=" 1 font-lock-variable-name-face)
     ("^\\s-+\\(\\sw+\\)\\s-*=" 1 font-lock-builtin-face)
     ("\\s$\\sw+\\s$" . font-lock-constant-face)
     )))
   
(defvar sw-aliases-exec-path nil
  "Stored `exec-path' for executing GIS command.")

(defvar sw-aliases-process-environment nil
  "Stored `process-environment' for executing GIS command.")

(defun sw-aliases-customize ()
  "Open Customization buffer for sw-aliases Mode."
  (interactive)
  (customize-group 'aliases))

;;;autoload
(define-derived-mode sw-aliases-mode prog-mode "sw-aliases"
  "Major mode for editing Magik sw-aliases files."
  :group 'smallworld
  (setq font-lock-defaults sw-aliases-mode-font-lock-defaults))

(defun sw-aliases-kill-buffer ()
  "Function to run when an sw-aliases mode buffer is run."
  (if (eq major-mode 'aliases-mode)
      (progn
	(setq major-mode 'fundamental-mode) ; prevent current buffer being listed.
	(sw-aliases-update-sw-menu))))
  
(defun sw-aliases-list ()
  "Return list of alias definitions."
  (let (list)
    (save-excursion
      (save-match-data
	(goto-char (point-max))
	(while (re-search-backward sw-aliases-definition-regexp nil t)
	  (push (match-string-no-properties 1) list))))
    list))

(defun sw-aliases-switch-to-buffer (alias)
  "Return true to switch to the GIS buffer.
If true switches to the buffer that ALIAS is running in.
Since some entries in the sw-aliases file do not start a Smallworld Magik GIS
process we do not necessarily want to switch to the buffer running the
process all the time.  These are the following methods by which we control
when the buffer is displayed:

Hook: `aliases-switch-to-buffer-hooks'
Each function in the hook is passed the name of the alias.

If any function returns t, then the buffer is displayed.
Regexp: `aliases-switch-to-buffer-regexp'
If the alias name matches the given regular expression the buffer is displayed.
Variable: `aliases-switch-to-buffer'
If this is t then the buffer is displayed."
  (cond ((run-hook-with-args-until-success 'aliases-switch-to-buffer-hooks alias)
	 t)
	((stringp sw-aliases-switch-to-buffer-regexp)
	 (save-match-data
	   (match-string sw-aliases-switch-to-buffer-regexp alias)))
	(t
	 sw-aliases-switch-to-buffer)))

(defun sw-aliases-program-set (&optional default)

  "Return the program (DEFAULT) to use to operate on a gis_aliases file."

  (let ((path sw-aliases-program-path)
	finished program)
    (while path
      (setq program (expand-file-name
		     (concat (file-name-as-directory (car path)) default))
	    path (cdr path))
      (if (file-executable-p program)
	  (setq path nil)
	(setq program nil)))
    (or program default)))

(defun sw-aliases-run-program (&optional alias file dir)
  "Run ALIAS using gis.exe on the sw-aliases file FILE in DIR.

With a prefix arg, ask user for current directory to use."
  (interactive (if (not (sw-aliases-at-alias-definition))
		   (list
		    (completing-read (concat "Definition:" " ")
				     (mapcar (function
					      (lambda (d) (cons d d)))
					     (sw-aliases-list))
				     nil t)
		    nil nil)))
  (cond (current-prefix-arg
	 (setq dir (file-name-as-directory
		    (expand-file-name
		     (read-file-name (concat "Set current working directory" " "))))))
	((null dir)
	 (setq dir default-directory)))

  (let ((program sw-aliases-program)
	(args    sw-aliases-program-args)
	(file    (or file (buffer-file-name)))
	(buf     "gis")
	(version (if (boundp 'gis-version-current)
		     (symbol-value 'gis-version-current)))
	(process-environment-aliases sw-aliases-process-environment)
	(exec-path-aliases sw-aliases-exec-path))
    (save-excursion
      (cond (alias nil)
	    ((re-search-backward sw-aliases-definition-regexp nil t)
	     (setq alias (match-string-no-properties 1)))
	    (t
	     (error "Cannot find any alias definitions")))
      (if (eq system-type 'windows-nt)
	  (if (file-exists-p (concat (file-name-directory file) "environment.bat"))
	      (setq args (append args (list "-e" (concat (file-name-directory file) "environment.bat")) nil))))
      (setq args (append args (list "-a" file alias) nil)) ;; alias name MUST be last

      (if (stringp version)
	  (setq buf (concat buf " " version)))
      (if alias
	  (setq buf (concat buf " " alias)))
      (setq buf (generate-new-buffer (concat "*" buf "*")))
      (set-buffer buf)
      (gis-mode)
      
      (insert "Command" ": " program " ")
      (mapc (function (lambda (s) (insert s " "))) args)
      (setq default-directory dir
	    args (append (list program) args)
	    gis-exec-path (cl-copy-list (or exec-path-aliases exec-path))
	    gis-process-environment (cl-copy-list (or process-environment-aliases process-environment))
	    gis-current-command (mapconcat 'identity args " "))
      (if (stringp version)
	  (set 'gis-version-current version))
	    
      (insert "\n" (format "Cwd is: %s" default-directory) "\n\n")
      (gis-start-process args))
    (if (sw-aliases-switch-to-buffer alias)
	(switch-to-buffer buf))))

(defun sw-aliases-at-alias-definition ()
  "Return definition, if point is in an alias definition."
  (save-excursion
    (beginning-of-line)
    (save-match-data
      (cond ((looking-at sw-aliases-definition-regexp)
	     (match-string-no-properties 1))
	    ((looking-at "\\(\\s-\\|\n\\)*\n\\sw+:\\s-*$")
	     ;;At point in between definitions
	     nil)
	     ((re-search-backward sw-aliases-definition-regexp nil t)
	      (match-string-no-properties 1))
	     (t nil)))))

(defun sw-aliases-expand-file (file)
  "Expand FILE path including environment variables.
Returns nil if FILE cannot be expanded."
  (condition-case nil
      (expand-file-name (substitute-in-file-name file))
    (error nil)))

(defun sw-aliases-layered-products-file (file)
  "Read contents of FILE with the format of LAYERED_PRODUCTS configuration file."
  (if (file-exists-p file)
      (with-current-buffer " *aliases LAYERED_PRODUCTS*"
	(insert-file-contents file nil nil nil 'replace)
	
	;; Always ensure that a default sw_core: set to SMALLWORLD_GIS is present
	;; in case the value has been manually modified but we still wish to locate
	;; a gis_aliases file next to the LAYERED_PRODUCTS file.
	(goto-char (point-min))
	(insert "sw_core:\n	path		= %SMALLWORLD_GIS%\n")
	(sw-aliases-layered-products-alist))))

(defun sw-aliases-layered-products-alist ()
  "Return alist of contents for LAYERED_PRODUCTS file."
  (save-excursion
    (save-match-data
      (let (alist pt lp dir)
	(goto-char (point-min))
	(while (re-search-forward "^\\([^\r\n:]+\\):" nil t)
	  (setq lp (match-string-no-properties 1))
	  (if (re-search-forward "^\\s-*path\\s-*=\\s-*" nil t)
	      (progn
		(setq pt (point))
		(end-of-line)
		(skip-syntax-backward "-")
		(skip-chars-backward "/\\") ;avoid trailing directory character.
		(setq dir
		      (sw-aliases-expand-file
		       (buffer-substring-no-properties pt (point))))
		(if (file-exists-p (concat dir "/config/gis_aliases"))
		    (let ((lp-dir (cons lp dir)))
		      (or (member lp-dir alist) (push lp-dir alist))) ))))
	alist))))

(defun sw-aliases-update-menu ()
  "Update the dynamic sw-aliases submenu."
  (interactive)
  (if (eq major-mode 'aliases-mode)
      (let ((sw-aliases (sw-aliases-list))
	    entries def)
	(while sw-aliases
	  (setq def (car sw-aliases)
		sw-aliases (cdr sw-aliases))
	  (push (vector def (list 'aliases-run-program def) t) entries)) ;; :key-sequence nil
	(easy-menu-change (list "Aliases")
			  "Definitions"
			  (or entries (list "No Aliases found"))))))

(defun sw-aliases-update-sw-menu ()
  "Update 'resources-menu-sw-alias-files' submenu in SW menu bar."
  (interactive)
  (if (and (boundp 'sw-set-keys) (symbol-value 'sw-set-keys))
      (let (default-files
	    lp-files
	    buffers
	    (rescan (list "---" (vector "*Rescan*" 'aliases-update-sw-menu t))))
	(dolist (f (append sw-aliases-user-file-list sw-aliases-common-file-list ))
	  (push `[,f
		  (progn
		    (find-file (sw-aliases-expand-file ,f))
		    (sw-aliases-mode))
		  (and ,f (sw-aliases-expand-file ,f))
		  ]
		default-files))
	
	(when (getenv "SMALLWORLD_GIS")
	  (dolist (lp (sw-aliases-layered-products-file
		       (sw-aliases-expand-file "$SMALLWORLD_GIS/../smallworld_registry/LAYERED_PRODUCTS")))
	    (push `[,(format "%s: %s" (car lp) (cdr lp))
		    (progn
		      (find-file ,(concat (cdr lp) "/config/gis_aliases"))
		      (sw-aliases-mode))
		    ,(cdr lp)
		    ]
		  lp-files))
	  (push "---" lp-files))

	(cl-loop for buf in (sw-buffer-mode-list 'aliases-mode)
	      do (push (vector (buffer-file-name (get-buffer buf))
			       (list 'switch-to-buffer buf)
			       t) buffers))
	(or (eq (length buffers) 0) (push "---" buffers)))))
  
(defvar sw-aliases-mode-syntax-table nil "Syntax for SW sw-aliases mode.")

(setq sw-aliases-mode-syntax-table
      (let ((st (make-syntax-table)))
        (modify-syntax-entry ?_ "w" st)
        (modify-syntax-entry ?: "w" st)
        (modify-syntax-entry ?% "$" st); Windows Environment vars
        (modify-syntax-entry ?# "<" st)
        (modify-syntax-entry ?\n ">" st)
        st))

;;; Package registration
(or (assoc "aliases$" auto-mode-alist)
    (push '("aliases$" . sw-aliases-mode) auto-mode-alist))
(or (assoc "aliases.txt$" auto-mode-alist)
    (push '("aliases.txt$" . sw-aliases-mode) auto-mode-alist))

(provide 'sw-aliases-mode)

;;; sw-aliases-mode.el ends here
