;;; aliases.el -- mode for editing GIS aliases files.

(eval-when-compile
  (require 'cl)
  (defvar msb-menu-cond)

  (require 'macros-sw)
  (require 'resources)
  (require 'misc-sw)
  (require 'utils-sw)
  (require 'sw-help)
  (defvar gis-exec-path)
  (defvar gis-process-environment)
  (defvar gis-current-command)
  ;;Cannot require 'gis because of circular dependency
)

(defgroup aliases nil
  "Customise Magik aliases files group."
  :group 'smallworld
  :group 'tools)

(defconst aliases-version "$Revision: 1.15 $")

(defcustom aliases-user-file-list '("$HOME/gis_aliases")
  "A list of a User's personal gis_aliases files."
  :group 'aliases
  :type  'file)

(defcustom aliases-common-file-list nil
  "*List of common gis_aliases files.
This list is expected to be setup by the Emacs maintainer,
a user can setup their personal gis_aliases file list using
`aliases-user-file-list'. Both these lists are concatenated to
form the top section of the SW->Alias Files submenu."
  :group 'aliases
  :type  '(repeat file))

(defcustom aliases-mode-hook nil
  "*Hook to run after ALIASES mode is set."
  :group 'aliases
  :type  'hook)

(defcustom aliases-program (if (eq system-type 'windows-nt)
			       "gis.exe"
			     "gis")
  "*Program to process an alias file."
  :group 'aliases
  :type  'string)

(defcustom aliases-program-path (if (eq system-type 'windows-nt)
				    '("../bin/x86" "../../product/bin/x86")
				  '("../bin/share" "../../product/bin/share"))
  "*Path to `aliases-program'.
Setting this sets the default value. When opening a gis_aliases file,
the buffer local value of this variable will be set to the directory
containing the `aliases-program' if it is in a relative path to the file."
  :group 'aliases
  :type  '(repeat directory))

(defcustom aliases-program-args nil
  "*Arguments to pass to `aliases-program'."
  :group 'aliases
  :type  '(repeat string))

(defcustom aliases-switch-to-buffer t
  "*User control for switching to the process buffer of a selected alias.
If this is t then the buffer is displayed."
  :group 'aliases
  :type  'boolean)

(defcustom aliases-switch-to-buffer-regexp nil
  "*User control for switching to the process buffer of a selected alias.
If the alias name matches the given regular expression the buffer
is displayed."
  :group 'aliases
  :type  '(choice regexp (const nil)))

(defcustom aliases-switch-to-buffer-hooks nil
  "*User control for switching to the process buffer of a selected alias.
Each function in the hook is passed the name of the alias.
If any function returns t, then the buffer is displayed."
  :group 'aliases
  :type  'hook)

(defvar aliases-mode-map (make-sparse-keymap)
  "Keymap for GIS aliases files")

(define-key aliases-mode-map [f1]   'aliases-help)
(define-key aliases-mode-map "\C-c" 'aliases-run-program)

(defvar aliases-menu nil
  "Menu for Aliases mode.")

(easy-menu-define aliases-menu aliases-mode-map
  "Menu for aliases mode."
  `(,resources-aliases-menu
    [,resources-aliases-menu-run-gis  aliases-run-program t]
    "----"
    (,resources-aliases-menu-definitions)
    "---"
    [,resources-menu-sw-customize     aliases-customize   t]
    [,resources-menu-sw-help          aliases-help        t]))

(defvar aliases-mode-syntax-table nil
  "Syntax table in use in Aliases-mode buffers.")

(defvar aliases-definition-regexp "^\\([^#]\\S-+\\):\\s-*$"
  "Regexp matching an alias definition")

;; Imenu configuration
(defvar aliases-imenu-generic-expression
  (list
   (list nil aliases-definition-regexp 1)
    )
  "Imenu generic expression for Aliases mode.  See `imenu-generic-expression'.")

;; Font-lock configuration
(defcustom aliases-font-lock-keywords
  (list 
   (cons aliases-definition-regexp 'font-lock-function-name-face)
   '("^\\s-+\\([A-Z_]+\\)\\s-*:=" 1 font-lock-type-face)
   '("^\\s-+\\([A-Z_]+\\)\\s-*=" 1 font-lock-variable-name-face)
   '("^\\s-+\\(\\sw+\\)\\s-*=" 1 font-lock-builtin-face)
   '("\\s$\\sw+\\s$" . font-lock-constant-face)
   )
  "Default fontification of Aliases buffers."
  :group 'aliases
  :type 'sexp)

(defvar aliases-exec-path nil
  "Stored `exec-path' for executing GIS command.")

(defvar aliases-process-environment nil
  "Stored `process-environment' for executing GIS command.")

(defun aliases-help ()
  "Display help on how to use the Aliases Mode interface."
  (interactive)
  (sw-help-open sw-help-aliases-id))

(defun aliases-customize ()
  "Open Customization buffer for Aliases Mode."
  (interactive)
  (customize-group 'aliases))

;;;autoload
(defun aliases-mode ()
  "Major mode for editing Magik aliases files.

You can customise aliases-mode with the aliases-mode-hook."

  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'require-final-newline)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'aliases-program)
  (make-local-variable 'aliases-exec-path)
  (make-local-variable 'aliases-process-environment)

  (use-local-map aliases-mode-map)
  (easy-menu-add aliases-menu)
  (set-syntax-table aliases-mode-syntax-table)

  (setq major-mode 'aliases-mode
	mode-name resources-aliases-menu
	aliases-program (aliases-program-set aliases-program)
	require-final-newline t
	comment-start "#" 
	comment-end   ""
	imenu-generic-expression aliases-imenu-generic-expression
	font-lock-defaults
	 '(aliases-font-lock-keywords
	   nil nil))

  (add-hook menu-bar-update-hook-sym 'aliases-update-menu)
  (add-hook 'kill-buffer-hook 'aliases-kill-buffer nil t)
  ;;Avoid menu-bar-update-hook, since this is executed
  ;;many times and the aliases-update-sw-menu function does
  ;;perform file existence checks. So by using a local kill-buffer-hook
  ;;it should cut down on the number of times this function is executed
  ;;whilst still retaining the accuracy of the SW->Alias Files submenu.
  ;;(add-hook menu-bar-update-hook-sym 'aliases-update-sw-menu)

  (run-hooks 'aliases-mode-hook))

(defun aliases-kill-buffer ()
  "Function to run when an Aliases mode buffer is run."
  (if (eq major-mode 'aliases-mode)
      (progn
	(setq major-mode 'fundamental-mode) ; prevent current buffer being listed.
	(aliases-update-sw-menu))))
  
(defun aliases-list ()
  "Return list of alias definitions."
  (let (list)
    (save-excursion
      (save-match-data
	(goto-char (point-max))
	(while (re-search-backward aliases-definition-regexp nil t)
	  (push (match-string-no-properties 1) list))))
    list))

(defun aliases-switch-to-buffer (alias)
  "Return t, to switch to the buffer that the GIS.exe process is running in.
Since some entries in the aliases file do not start a Smallworld Magik GIS
process we do not necessarily want to switch to the buffer running the
process all the time. These are the following methods by which we control
when the buffer is displayed:
  Hook: `aliases-switch-to-buffer-hooks'
       Each function in the hook is passed the name of the alias.
       If any function returns t, then the buffer is displayed.
  Regexp: `aliases-switch-to-buffer-regexp'
       If the alias name matches the given regular expression the buffer
       is displayed.
  Variable: `aliases-switch-to-buffer'
       If this is t then the buffer is displayed.
"
  (cond ((run-hook-with-args-until-success 'aliases-switch-to-buffer-hooks alias)
	 t)
	((stringp aliases-switch-to-buffer-regexp)
	 (save-match-data
	   (match-string aliases-switch-to-buffer-regexp alias)))
	(t
	 aliases-switch-to-buffer)))

(defun aliases-program-set (&optional default)
  "Return the program to use to operate on a gis_aliases file."
  (let ((path aliases-program-path)
	finished program)
    (while path
      (setq program (expand-file-name
		     (concat (file-name-as-directory (car path)) default))
	    path (cdr path))
      (if (file-executable-p program)
	  (setq path nil)
	(setq program nil)))
    (or program default)))

(defun aliases-run-program (&optional alias file dir)
  "Run gis.exe on the aliases file.

With a prefix arg, ask user for current directory to use."
  (interactive (if (not (aliases-at-alias-definition))
		   (list
		    (completing-read (concat resources-aliases-definition-prompt " ")
				     (mapcar (function
					      (lambda (d) (cons d d)))
					     (aliases-list))
				     nil t)
		    nil nil)))
  (cond (current-prefix-arg
	 (setq dir (file-name-as-directory
		    (expand-file-name
		     (read-file-name (concat resources-aliases-cwd-prompt " "))))))
	((null dir)
	 (setq dir default-directory)))

  (let ((program aliases-program)
	(args    aliases-program-args)
	(file    (or file (buffer-file-name)))
	(buf     "gis")
	(version (if (boundp 'gis-version-current)
		     (symbol-value 'gis-version-current)))
	(process-environment-aliases aliases-process-environment)
	(exec-path-aliases aliases-exec-path))
    (save-excursion
      (cond (alias nil)
	    ((re-search-backward aliases-definition-regexp nil t)
	     (setq alias (match-string-no-properties 1)))
	    (t
	     (error resources-aliases-no-definition)))
      (if (running-under-nt)
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
      
      (insert resources-aliases-command ": " program " ")
      (mapc (function (lambda (s) (insert s " "))) args)
      (setq default-directory dir
	    args (append (list program) args)
	    gis-exec-path (copy-list (or exec-path-aliases exec-path))
	    gis-process-environment (copy-list (or process-environment-aliases process-environment))
	    gis-current-command (mapconcat 'identity args " "))
      (if (stringp version)
	  (set 'gis-version-current version))
	    
      (insert "\n" (format resources-aliases-cwd default-directory) "\n\n")
      (gis-start-process args))
    (if (aliases-switch-to-buffer alias)
	(switch-to-buffer buf))))

(defun aliases-at-alias-definition ()
  "Return definition, if point is in an alias definition."
  (save-excursion
    (beginning-of-line)
    (save-match-data
      (cond ((looking-at aliases-definition-regexp)
	     (match-string-no-properties 1))
	    ((looking-at "\\(\\s-\\|\n\\)*\n\\sw+:\\s-*$")
	     ;;At point in between definitions
	     nil)
	     ((re-search-backward aliases-definition-regexp nil t)
	      (match-string-no-properties 1))
	     (t nil)))))

(defun aliases-expand-file (file)
  "Expand FILE path including environment variables.
Returns nil if FILE cannot be expanded."
  (condition-case nil
      (expand-file-name (substitute-in-file-name file))
    (error nil)))

(defun aliases-layered-products-file (file)
  "Read contents of FILE with the format of LAYERED_PRODUCTS configuration file."
  (if (file-exists-p file)
      (save-excursion
	(set-buffer (get-buffer-create " *aliases LAYERED_PRODUCTS*"))
	(insert-file-contents file nil nil nil 'replace)

	;; Always ensure that a default sw_core: set to SMALLWORLD_GIS is present
	;; in case the value has been manually modified but we still wish to locate
	;; a gis_aliases file next to the LAYERED_PRODUCTS file.
	(goto-char (point-min))
	(insert "sw_core:\n	path		= %SMALLWORLD_GIS%\n")
	(aliases-layered-products-alist))))

(defun aliases-layered-products-alist ()
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
		      (aliases-expand-file
		       (buffer-substring-no-properties pt (point))))
		(if (file-exists-p (concat dir "/config/gis_aliases"))
		    (let ((lp-dir (cons lp dir)))
		      (or (member lp-dir alist) (push lp-dir alist))) ))))
	alist))))

(defun aliases-update-menu ()
  "Update the dynamic Aliases submenu."
  (interactive)
  (if (eq major-mode 'aliases-mode)
      (let ((aliases (aliases-list))
	    entries def)
	(while aliases
	  (setq def (car aliases)
		aliases (cdr aliases))
	  (push (vector def (list 'aliases-run-program def) t) entries)) ;; :key-sequence nil
	(easy-menu-change (list resources-aliases-menu)
			  resources-aliases-menu-definitions
			  (or entries (list resources-aliases-menu-none-found))))))

(defun aliases-update-sw-menu ()
  "Update 'resources-menu-sw-alias-files' submenu in SW menu bar."
  (interactive)
  (if (and (boundp 'sw-set-keys) (symbol-value 'sw-set-keys))
      (let (default-files
	    lp-files
	    buffers
	    (rescan (list "---" (vector resources-aliases-sw-menu-rescan 'aliases-update-sw-menu t))))
	(dolist (f (append aliases-user-file-list aliases-common-file-list ))
	  (push `[,f
		  (progn
		    (find-file (aliases-expand-file ,f))
		    (aliases-mode))
		  (and ,f (aliases-expand-file ,f))
		  ]
		default-files))
	
	(when (getenv "SMALLWORLD_GIS")
	  (dolist (lp (aliases-layered-products-file
		       (aliases-expand-file "$SMALLWORLD_GIS/config/LAYERED_PRODUCTS")))
	    (push `[,(format "%s: %s" (car lp) (cdr lp)) 
		    (progn
		      (find-file ,(concat (cdr lp) "/config/gis_aliases"))
		      (aliases-mode))
		    ,(cdr lp)
		    ]
		  lp-files))
	  (push "---" lp-files))

	(loop for buf in (sw-buffer-mode-list 'aliases-mode)
	      do (push (vector (buffer-file-name (get-buffer buf))
			       (list 'switch-to-buffer buf)
			       t) buffers))
	(or (eq (length buffers) 0) (push "---" buffers))

	(easy-menu-change (list resources-menu-sw)
			  resources-menu-sw-alias-files
			  (append default-files lp-files buffers rescan)))))
  
;;; Package initialisation
(aliases-update-sw-menu)
(add-hook 'aliases-mode-hook 'aliases-update-sw-menu)

(if aliases-mode-syntax-table
    ()
  (setq aliases-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?_ "w" aliases-mode-syntax-table)
  (modify-syntax-entry ?: "w" aliases-mode-syntax-table)
  (modify-syntax-entry ?% "$" aliases-mode-syntax-table); Windows Environment vars
  (modify-syntax-entry ?# "<" aliases-mode-syntax-table)
  (modify-syntax-entry ?\n ">" aliases-mode-syntax-table))

;;; Package registration
(or (assoc "aliases$" auto-mode-alist)
    (push '("aliases$" . aliases-mode) auto-mode-alist))
(or (assoc "aliases.txt$" auto-mode-alist)
    (push '("aliases.txt$" . aliases-mode) auto-mode-alist))

;;; speedbar configuration
(eval-after-load 'speedbar
  '(speedbar-add-supported-extension "aliases$"))

;;MSB configuration
(defun aliases-msb-configuration ()
  "Adds Aliases files to msb menu, supposes that msb is already loaded."
  (let* ((l (length msb-menu-cond))
	 (last (nth (1- l) msb-menu-cond))
	 (precdr (nthcdr (- l 2) msb-menu-cond)) ; cdr of this is last
	 (handle (1- (nth 1 last))))
    (setcdr precdr (list
		    (list
		     '(eq major-mode 'aliases-mode)
		     handle
		     "Aliases Files (%d)")
		    last))))

(eval-after-load 'msb
  '(aliases-msb-configuration))

(provide 'aliases)


;;; aliases.el ends here
