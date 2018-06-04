;; Interface to the gis_version environment switching tool.

;;; Commentary:

;;; Code:

(require 'resources)

(defgroup gis-version nil
  "Multiple Smallworld Environments."
  :tag "Smallworld Environment"
  :group 'smallworld)

(defconst gis-version-version "$Revision: 1.7 $")

(defcustom gis-version-frame-title-format '(multiple-frames "%b"
					    ("" invocation-name "@" system-name " " gis-version-current))
  "*The frame title string to use when a version has been selected."
  :group 'gis-version
  :type  'sexp)

(defcustom gis-version-icon-title-format gis-version-frame-title-format
  "*The icon title string to use when a version has been selected."
  :group 'gis-version
  :type  'sexp)

(defcustom gis-version-program "gis_version"
  "*The program name for the gis_version environment program."
  :group 'gis-version
  :type  'file)

(defcustom gis-version-file (let ((file (concat (file-name-as-directory (getenv "HOME"))
						"gis_version.txt")))
			      (if (file-exists-p file)
				  file))
  "*A file containing locations of GIS versions.
This provides an alternative interface to a gis_version program.
"
  :group 'gis-version
  :type  '(choice (file)
		  (const nil)))

(defcustom gis-version-match "^[* ] \\(\\S-+\\)\\s-*\\(\\S-+\\)\\s-*\\(.*\\)"
  "*Regexp matching valid versions listed by `gis-version-program' or `gis-version-file'."
  :group 'gis-version
  :type  'regexp)

(defcustom gis-version-program-error "gis_version: "
  "*The error string that the gis version program returns when stream is invalid."
  :group 'gis-version
  :type  'string)

(defcustom gis-version-invalid-string "(invalid)"
  "*The string marking an invalid gis version entry."
  :group 'gis-version
  :type  'string)

(defcustom gis-version-select-hook nil
  "*Hook to run after a selection has been made."
  :group 'gis-version
  :type  'hook)
(add-hook 'gis-version-select-hook  'aliases-update-sw-menu)

(defcustom gis-version-help resources-gis-version-help
  "Help text displayed at top of gis_version buffer."
  :group 'gis-version
  :type  'string)

(defcustom gis-version-help-file-add resources-gis-version-help-file-add
  "Help text for Adding to `gis-version-file' displayed after `gis-version-help'."
  :group 'gis-version
  :type  'string)

(defcustom gis-version-font-lock-keywords
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
  :group 'gis-version
  :type 'sexp)

(defvar gis-version-file-format "  %-17s   %-23s   %s\n"
  "*Header format to use for a newly created gis version file.")

(defvar gis-version-file-header
  (concat (format gis-version-file-format "name" "version" "directory")
	  (format gis-version-file-format
		  "-----------------"
		  "------------------------"
		  "--------------------------------"))
  "*Header string to use for a newly created gis version file.")

(defvar gis-version-sw-path-list nil
  "Stores list of Smallworld directories added to PATH.")

(defvar gis-version-mode-map (make-sparse-keymap)
  "Keymap for selection of alternative GIS environments")

(defvar gis-version-menu nil
  "Keymap for the gis_version buffer menu bar")

(easy-menu-define gis-version-menu gis-version-mode-map
  "Menu for gis_version mode."
  `(,resources-g-v-menu
    [,resources-g-v-menu-select   gis-version-select      t]
    [,resources-g-v-menu-run      gis-version-run         t]
    [,resources-g-v-menu-next     gis-version-next        t]
    [,resources-g-v-menu-aliases  gis-version-gis-aliases t]
    [,resources-g-v-menu-quit     gis-version-quit        t]
    "---"
    [,resources-g-v-menu-file-add gis-version-file-add    gis-version-file]
    [,resources-g-v-menu-reset    gis-version-reset-emacs-environment t]
    "---"
    [,resources-menu-sw-customize gis-version-customize   t]
    [,resources-menu-sw-help      gis-version-help        t]))

(define-key gis-version-mode-map [f1]   'gis-version-help)
(define-key gis-version-mode-map " "    'gis-version-next)
(define-key gis-version-mode-map "a"    'gis-version-gis-aliases)
(define-key gis-version-mode-map "+"    'gis-version-file-add)
(define-key gis-version-mode-map "q"    'gis-version-quit)
(define-key gis-version-mode-map "r"    'gis-version-run)
(define-key gis-version-mode-map "\r"   'gis-version-select)
(define-key gis-version-mode-map [mouse-2] 'gis-version-mouse-select)

(defvar gis-version-mode-syntax-table nil
  "Syntax table in use in DEF-mode buffers.")

(defvar gis-version-current nil
  "Current gis_version stream.")
(make-variable-buffer-local 'gis-version-current)

(defvar gis-version-position nil
  "A position in the gis version buffer above which the user shouldn't click.")

(defun gis-version-help ()
  "Display help on how to use the Gis-Version Mode interface."
  (interactive)
  (sw-help-open sw-help-gis-version-id))

(defun gis-version-customize ()
  "Open Customization buffer for Gis-Version Mode."
  (interactive)
  (customize-group 'gis-version))

(defun gis-version (gis-version)
  "Return the output from running the gis version script (as opposed to the alias).
If it fails, return the symbol, 'failed."
  (save-excursion
    (set-buffer (generate-new-buffer "*gis-version temp buffer*"))
    (if gis-version-file
	(insert-file-contents gis-version-file)
      (call-process gis-version-program nil t nil gis-version))
    (goto-char (point-min))
    (prog1
        (if (search-forward gis-version-program-error nil t)
            'failed
          (buffer-string))
      (kill-buffer (current-buffer)))))

(defun gis-version-program-current ()
  "Run `gis-version-program' and determine the current version.
Return the current gis version that this Emacs inherited from its environment,
or nil if there isn't one, or 'no-gis-version-script if there is no gis version command."
  (save-excursion
    (set-buffer (generate-new-buffer "*gis-version temp buffer*"))
    (condition-case nil
        (progn
          (call-process gis-version-program nil t nil)
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
       'no-gis-version-script))))

(defun gis-version-run ()
  "Run GIS command in selected version."
  (interactive)
  (beginning-of-line)
  (let ((process-environment (copy-list process-environment))
	(exec-path (copy-list exec-path))
	stream version buffer)
    (setq stream (car (gis-version-select-internal))
	  buffer  (concat "*gis " stream "*"))
    (gis buffer)
    (setq gis-version-current stream)))

(defun gis-version-gis-aliases ()
  "Open gis_aliases file of selected version.
Will prompt for layered product to use if selected version
has more than one aliases file available."
  ;;Does not cope with 'partial stream versions' where the directory components list 2 (or more directories)
  (interactive)
  (beginning-of-line)
  (let* ((process-environment (copy-list process-environment))
	 (exec-path (copy-list exec-path))
	 (version-list (gis-version-select-internal))
	 lp-alist
	 alias-file)
    (if (null (car version-list))
	(error resources-g-v-invalid-selection-error)
      (setq lp-alist (aliases-layered-products-file
			   (aliases-expand-file "$SMALLWORLD_GIS/config/LAYERED_PRODUCTS")))
      (cond ((null lp-alist) nil)
	    ((eq (length lp-alist) 1)
	     (setq alias-file (concat (cdar lp-alist) "/config/gis_aliases")))
	    (t
	     (let* ((lp   (completing-read (concat resources-g-v-lp-alias-prompt " ") lp-alist nil t))
		    (path (cdr (assoc lp lp-alist))))
	       (if path
		   (setq alias-file (concat path "/config/gis_aliases"))))))
      (if alias-file
	  (progn
	    (find-file alias-file)
	    (setq aliases-process-environment (copy-list process-environment)
		  aliases-exec-path (copy-list exec-path)
		  aliases-program-args (list "-p" (caddr version-list))
		  gis-version-current (car version-list))
	    ;;(set 'aliases-gis-version-current current-version)
	    )))))

(defun gis-version-next ()
  "Move point to next valid version listed."
  (interactive)
  (forward-line 1)
  (save-match-data
    (while (and (not (eobp))
		(or (looking-at (concat "^.*" gis-version-invalid-string))
		    (not (looking-at gis-version-match))))
      (forward-line 1))
    (if (not (eobp))
	(beginning-of-line)
      (goto-char gis-version-position)
      (re-search-forward gis-version-match nil t)
      (beginning-of-line))))
  
(defun gis-version-current ()
  "Return the current gis_version."
  (interactive)
  (if gis-version-file
      gis-version-current
    (gis-version-program-current)))

(defun gis-version-mode ()
  "Major Mode for gis_version."
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'gis-version-position)

  (use-local-map gis-version-mode-map)
  (easy-menu-add gis-version-menu)
  (set-syntax-table gis-version-mode-syntax-table)

  (setq major-mode 'gis-version-mode
	mode-name resources-g-v-menu
	buffer-undo-list t
	font-lock-defaults
	'(gis-version-font-lock-keywords
	  nil t
	  (("-" . "w"))))

  (run-hooks 'gis-version-hook))

(defun gis-version-smallworld-gis-p (path)
  "Return t if path points to a Smallworld installation."
  (file-directory-p (concat (file-name-directory path) "config")))

(defun gis-version-read-smallworld-gis-completion (string predicate flag)
  "Provide directory completion for finding Smallworld installations.
Repeated TAB and \\[minibuffer-completion-help] still provide
directory listing so users can navigate a directory structure looking
for a Smallworld installation. Only when
`gis-version-smallworld-gis-p' returns t for a given path will the
path be considered to be a real Smallworld installation directory
suitable for selection."
  (if (gis-version-smallworld-gis-p string)
      (cond ((eq flag 'lambda) t)
	    ((null flag)       t)
	    (t            string))
    (flet ((read-smallworld-gis-predicate (d) (equal d (file-name-directory d))))
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
  
(defun gis-version-read-smallworld-gis ()
  "Prompt for a valid value for SMALLWORLD_GIS."
  (let ((path
	 (completing-read (concat resources-g-v-smallworld-gis-prompt " ")
			  'gis-version-read-smallworld-gis-completion
			  nil t nil nil (getenv "SMALLWORLD_GIS"))))
    (setq path (directory-file-name (file-name-directory path)))
    (if (running-under-nt)
	(subst-char-in-string ?/ ?\\ path t))
    path))

(defun gis-version-file-add (root name version)
  "Add a new entry to the file given by `gis-version-file'."
  (interactive
   (let* ((ok (or gis-version-file
		  (error resources-g-v-file-not-used-error)))
	  (root (gis-version-read-smallworld-gis))
	  (product-version-file (concat (file-name-as-directory root)
					"config/PRODUCT_VERSION"))
	  name version)
     (if (file-exists-p product-version-file)
	 (save-excursion
	   (set-buffer (get-buffer-create " *product_version*"))
	   (erase-buffer)
	   (insert-file-contents product-version-file)
	   (goto-char (point-min))
	   (setq version (current-word)
		 name    version)))
     (list root
	   (read-string (concat resources-g-v-name-prompt    " ") name)
	   (read-string (concat resources-g-v-version-prompt " ") version))))
  
  (or gis-version-file
      (error resources-g-v-file-not-used-error))
  (save-excursion
      (set-buffer (find-file-noselect gis-version-file))
      (goto-char (point-max))
      (let ((inhibit-read-only t))
	(insert (format gis-version-file-format name version root))
	(save-buffer)))
  (if (eq major-mode 'gis-version-mode)
      (gis-version-selection)))

(defun gis-version-file-create (file root)
  "Create a gis version format file based upon the current environment.
Called if no gis-version program exists or `gis-version-file' is nil.
Will set `gis-version-file' to FILE."
  (interactive
   (if (y-or-n-p (concat resources-g-v-yn-file-create " "))
       (let ((smallworld-gis (getenv "SMALLWORLD_GIS"))
	     (dir (file-name-as-directory (getenv "HOME")))
	     (file "gis_version.txt"))
	 (if smallworld-gis
	     nil
	   (setq smallworld-gis (gis-version-read-smallworld-gis)))
	 (list
	  (read-file-name (concat resources-g-v-file-prompt " ") dir file nil file)
	  smallworld-gis))
     (error "")))
  (setq gis-version-file file)
  
  (if (file-exists-p file)
      (find-file file)
    (find-file file)
    (insert gis-version-file-header)
    (let ((process-environment (copy-list process-environment)))
      (setenv "SMALLWORLD_GIS" root)
       (call-interactively 'gis-version-file-add))
    (save-buffer))
  nil)

(defun gis-version-selection ()
  "Display a list of possible gis products for the user to choose between."
  (interactive
   (cond (gis-version-file
	  ;; already supplied a file
	  nil)
	 ((eq gis-version-current 'no-gis-version-script)
	  (call-interactively 'gis-version-file-create))
	 (t nil)))

  ;(if (eq gis-version-current 'no-gis-version-script)
  ;    (error resources-g-v-no-command-error gis-version-program))
  (set-buffer (get-buffer-create "*gis version selection*"))
  (gis-version-mode)

  (message resources-g-v-starting-command gis-version-program)

  (setq buffer-read-only nil)
  (erase-buffer)
  (insert gis-version-help)
  (if (and gis-version-file gis-version-help-file-add)
      (insert "\n" gis-version-help-file-add "\n"))

  (save-excursion
    (save-match-data
      (if (not gis-version-file)
	  (call-process gis-version-program nil t nil)
	(insert-file-contents gis-version-file)
	(goto-char (point-min))
	
	(if (search-forward "-------" nil t) (forward-line 1)) ;skip a header
	(while (re-search-forward gis-version-match nil t)
	  (beginning-of-line)
	  (forward-char 1)
	  (backward-delete-char 1)
	  (insert " ")
	  (cond ((string-match gis-version-invalid-string (match-string-no-properties 3))
		 nil)
		((file-exists-p (match-string-no-properties 3))
		 nil)
		(t
		 (goto-char (match-beginning 3))
		 (insert gis-version-invalid-string " ")))))))

  (if (stringp gis-version-current)
      (save-excursion
	(save-match-data
	  (if (re-search-forward (concat "^. " gis-version-current " ") nil t)
	      (progn
		(beginning-of-line)
		(delete-char 1)
		(insert "*"))))))
  
  (setq gis-version-position (point))
  (save-match-data
    (if (search-forward "-------" nil t) (setq gis-version-position (point)))) ;skip a header

  (message resources-g-v-starting-command-done gis-version-program)
  (setq buffer-read-only t)
  (set-buffer-modified-p nil)
  (switch-to-buffer (current-buffer)))

(defun gis-version-quit ()
  "Quit, without selecting anything, gis version selection mode."
  (interactive)
  (kill-buffer (current-buffer)))

(defun gis-version-display-title ()
  "Modify the Frame and Icon titles according to the Environment."
  (interactive)
  (if gis-version-frame-title-format
      (setq frame-title-format gis-version-frame-title-format))
  (if gis-version-icon-title-format
      (setq icon-title-format  gis-version-icon-title-format)))

(defun gis-version-mouse-select (click)
  "Choose product using mouse event."
  (interactive "e")
  (mouse-set-point click)
  (beginning-of-line)
  (gis-version-select))

(defun gis-version-select ()
  "Store the gis product name in the global variable, `gis-version-current', so that
`F2 z' will set the correct product's environment before starting the gis.
Also now update the emacs `process-environment' correctly.
The frame and icon title strings will be modified according to
`gis-version-frame-title-format' and `gis-version-icon-title-format'."
  (interactive)
  (let ((stream (car (gis-version-select-internal))))
    (setq-default gis-version-current stream)
    (kill-buffer (current-buffer))
    (gis-version-display-title)
    (run-hooks 'gis-version-select-hook)
    (message resources-g-v-current stream)))

(defun gis-version-select-internal ()
  "Modify `process-environment' and `exec-path' for current version.
Return (STREAM VERSION SMALLWORLD_GIS)."
  (if (< (point) gis-version-position)
      (error resources-g-v-position-error))
  (if (save-excursion
        (beginning-of-line)
        (search-forward gis-version-invalid-string (point-eol) t))
      (error resources-g-v-invalid-error))
  (let (stream
	version
	smallworld-gis)
    (beginning-of-line)
    (if (looking-at gis-version-match)
	(setq stream (match-string-no-properties 1)
	      version (match-string-no-properties 2)
	      smallworld-gis  (match-string-no-properties 3))
      (error resources-g-v-match-error))
    (if (not (and gis-version-current
		  (string-equal stream gis-version-current)))
	(gis-version-set-environment smallworld-gis
				     stream
				     version
				     (if (and gis-version-program
					      (not gis-version-file))
					 gis-version-program)))
    (list stream version smallworld-gis)))

(defun gis-version-set-environment (smallworld-gis stream version program)
  "Modify the process and exec-path environment given stream and smallworld-gis path."
  (setenv "SMALLWORLD_GIS" smallworld-gis)
  (setenv "SW_STREAM" stream)
  (setenv "SW_VERSION" version)

  (save-excursion
    (set-buffer (get-buffer-create " *temp gis stream select*"))
    (erase-buffer)
    (cond ((and (running-under-nt) program)
	   (gis-version-call-process-windows program nil t nil stream "&&" "set"))
	  ((running-under-nt)
	   ;; Use cmdproxy explicitly and not shell-file-name since Cygwin
	   ;; may be being used and in this case 'set' sometimes reports shell values
	   ;; to be VAR='c:/path/to/file' i.e it inserts ''s.
	   (gis-version-call-process-windows "cmdproxy" nil t nil
			"/c"
			(concat "echo off && "
				(expand-file-name "config/environment.bat"  smallworld-gis)
				"&& set")))
	  ;; UNIX
	  (program
	   (gis-version-call-process-unix (concat "eval `" program " " stream "`")))
	  (t
	   (gis-version-call-process-unix
	    (concat ". " (expand-file-name "data/setup/SHARED_profile" smallworld-gis)))))
    (goto-char (point-min))
    (gis-version-set-emacs-environment)))

(defun gis-version-call-process-windows (&rest args)
  "Run Windows command and return the environment variables it sets up."
  (let ((default-directory temporary-file-directory) ;avoid command shell complaining about cwd being a UNC path
	(w32-quote-process-args t)   ;ensure quoting of arguments
	(win32-quote-process-args t));
    (apply 'call-process args)))

(defun gis-version-call-process-unix (command)
  "Run unix COMMAND and return the environment variables it sets up."
  (call-process "/bin/sh" nil t nil "-c"
		(concat "SHELL=/bin/sh ; export SHELL ; " command " ; env")))

(defun gis-version-prepend-sw-paths (orig new)
  "Ensure Smallworld directories are prepended to PATH variable.
For gis-version code to work the SMALLWORLD paths need to be prepended to PATH.
On UNIX PATH is modified to have SMALLWORLD appended (on Windows it is prepended).

Also sets `gis-version-sw-path-list' to be the list of directories added to PATH
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
      ;;remove previous SW paths using gis-version-sw-list
      ;;remove new SW paths first then prepend them.
      (dolist (p (append sw-list gis-version-sw-path-list))
	(if (member p new-list)
	    (setq new-list (delete p new-list))))
      (setq gis-version-sw-path-list (copy-list sw-list)
	    new (mapconcat 'directory-file-name
			   (append sw-list new-list)
			   path-separator))
      (if (running-under-nt)
	  (subst-char-in-string ?/ ?\\ new)
	new))))

(defun gis-version-set-emacs-environment ()
  "Update `process-environment' and `exec-path' variables."
  (let* ((orig-shell (getenv "SHELL"))
	 (orig-path  (getenv "PATH")))    
    (setq process-environment nil)
    (while (not (eobp))
      (if (looking-at ".*=")
	  (push (buffer-substring (point-bol) (point-eol)) process-environment))
      (forward-line 1))
    (setenv "SHELL" orig-shell)
    (setenv "PATH" (gis-version-prepend-sw-paths orig-path (getenv "PATH")))
    (setq exec-path (append (parse-colon-path (getenv "PATH"))
			    (parse-colon-path (getenv "EMACSPATH"))))))

(defun gis-version-reset-emacs-environment ()
  "Reset the Emacs environment back to how it was when Emacs started.
This affects all Environment variables `process-environment'
and the execution path `exec-path'."
  (interactive)
  (if (yes-or-no-p (concat resources-g-v-yn-reset " "))
      (progn
	(setq process-environment (copy-list sw-original-process-environment)
	      exec-path           (copy-list sw-original-exec-path))
	(setq-default gis-version-current nil)
	(gis-version-selection))))

(defun gis-version-header-string ()
  "Insert a string describing the gis_version status.
Used before running a GIS process."

  (let (gis-version-script)
    (if (stringp gis-version-current)
	(setq gis-version-script (gis-version gis-version-current)))
    (cond ((eq gis-version-current 'no-gis-version-script)
	   nil)
	  ((null gis-version-current)
	   (insert "\n" resources-gis-version-none-selected "\n"))
	  ((eq gis-version-script 'failed)
	   (insert "\n"
		   (format resources-g-v-invalid-warning-1 gis-version-current)
		   "\n"
		   resources-g-v-invalid-warning-2
		   "\n"))
	  (t
	   (insert (format resources-g-v-current-version gis-version-current) "\n")))))

;;; Package initialisation
(if gis-version-mode-syntax-table
    nil
  (setq gis-version-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?_  "w"  gis-version-mode-syntax-table))

(setq-default gis-version-current (gis-version-current))

(add-hook 'gis-start-process-pre-hook 'gis-version-header-string)

(provide 'gis-version)


