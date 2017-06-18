;;; loadlist.el -- mode for editing Magik load_list.txt files.

(eval-when-compile
  (require 'cl)
  (require 'misc-sw)
  (require 'sw-help)
  (require 'utils-sw)
  (require 'gis)
  (require 'magik))

(require 'font-lock)
(require 'magik)

(defgroup loadlist nil
  "Customise Magik load_list.txt files group."
  :group 'smallworld)

(defconst loadlist-version "$Revision: 1.4 $")

(defcustom loadlist-mode-hook nil
  "*Hook to run after loadlist mode is set."
  :group 'loadlist
  :type  'hook)

(defcustom loadlist-compile-files t
  "*If t, Compile the files that are specified."
  :group 'loadlist
  :type  'boolean)

(defcustom loadlist-ignore-regexp-list '("RCS")
  "List of Regexps used to miss certain files from load_list.txt files.
Intial ^ and final $ is automatically added in `loadlist-ignore'."
  :group 'loadlist
  :type  '(repeat regexp))

(defvar loadlist-mode-map (make-sparse-keymap)
  "Keymap for Magik load_list.txt files")

(defvar loadlist-f2-map (make-sparse-keymap)
  "F2 Keymap for Magik load_list.txt files")

(fset 'loadlist-f2-map loadlist-f2-map)

(define-key loadlist-mode-map [f2]    'loadlist-f2-map)

(define-key loadlist-f2-map   "b"     'loadlist-transmit)
(define-key loadlist-f2-map   "c"     'loadlist-toggle-compile)
(define-key loadlist-mode-map "\C-cr" 'loadlist-refresh-contents)

(defvar loadlist-menu nil
  "Keymap for the Magik loadlist buffer menu bar")

(easy-menu-define loadlist-menu loadlist-mode-map
  "Menu for loadlist mode."
  `(,resources-loadlist-menu
    [,resources-loadlist-menu-refresh    loadlist-refresh-contents :active t]
    "---"
    [,resources-loadlist-menu-transmit   loadlist-transmit         :active t :keys "f2 b"]
    [,resources-loadlist-menu-toggle-compile  loadlist-toggle-compile
	 :active t
	 :style toggle
	 :selected loadlist-compile-files
	 :keys "f2 c"]
    "---"
    [,resources-menu-sw-customize        loadlist-customize        t]
    [,resources-menu-sw-help             loadlist-help             t]))

(define-key loadlist-mode-map [f1] 'loadlist-help)

(defvar loadlist-mode-syntax-table nil
  "Syntax table in use in loadlist mode buffers.")

;; Font-lock configuration
(defcustom loadlist-font-lock-keywords
  (list 
   '("^.+\\([\\/]\\)" 0 font-lock-keyword-face)
   '("^.+"            0 font-lock-variable-name-face)
   )
  "Default fontification of load_list.txt files."
  :group 'loadlist
  :type 'sexp)

(defun loadlist-help ()
  "Display help on how to use the Loadlist Mode interface."
  (interactive)
  (sw-help-open sw-help-loadlist-id))

(defun loadlist-customize ()
  "Open Customization buffer for Loadlist Mode."
  (interactive)
  (customize-group 'loadlist))

(defun loadlist-mode ()
  "Major mode for editing Magik load_list.txt files.

You can customise loadlist-mode with the loadlist-mode-hook."

  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'require-final-newline)
  (make-local-variable 'font-lock-defaults)

  (use-local-map loadlist-mode-map)
  (easy-menu-add loadlist-menu)
  (set-syntax-table loadlist-mode-syntax-table)

  (setq major-mode 'loadlist-mode
	mode-name resources-loadlist-menu
	require-final-newline t
	font-lock-defaults
	'(loadlist-font-lock-keywords
	  nil t))

  (run-hooks 'loadlist-mode-hook))

(defun loadlist-buffer-list ()
  "Return contents of loadlist buffer."
  (goto-char (point-min))
  (let (start
	contents file)
    (while (not (eobp))
      (skip-syntax-forward "-")
      (if (eq (following-char) ?#)
	  (forward-line)
	(setq start (point))
	(if (search-forward "#" (point-eol) t)
	    (backward-char)
	  (end-of-line))
	(skip-syntax-backward "-")
	(setq file (buffer-substring-no-properties start (point)))
	(cond ((equal file "")
	       (setq file nil))
	      ((eq (substring file -1) ?\\)
	       (aset file (1- (length file)) ?/))
	      ((and (> (length file) 5)
		    (equal (substring file -6) ".magik"))
	       (setq file (substring file 0 (- (length file) 6))))
	      (t nil))
	(if file (push (loadlist-file-data file (point-bol)) contents))
	(forward-line)))
    contents))

(defun loadlist-file-data (file &optional data)
  "Return list of data describing FILE."
  (let ((lc (downcase file)))
    (list lc
	  (if (equal file lc) nil file) ;used to check for case differences for UNIX
	  data)))

(defun loadlist-ignore (file)
  "Return t if FILE matches any regexps from loadlist-ignore-regexp-list.
Regexp does not need to include ^ or $."
  (loop for r in loadlist-ignore-regexp-list
	for match = (string-match (concat "^" r "$") file)
	if match return t))

(defun loadlist-directory-list (&optional dir)
  "Return contents of directory."
  (let ((contents (directory-files-and-attributes (or dir default-directory)))
	files)
    (setq contents (delq (assoc ".." contents) contents)) ; Remove .. directory from list
    (setq contents (delq (assoc "."  contents) contents)) ; Remove .  directory from list
    (save-match-data
      (loop for a in contents
	    for f = (car a)
	    if (loadlist-ignore f)
	      do (progn
		   (princ (format resources-loadlist-output-ignored f))
		   (princ "\n"))
	    else if (string-match "\\.magik$" f) ; a .magik file
	      do (push (loadlist-file-data
			(substring f 0 (- (length f) 6)))
		       files)
	    else if (cadr a) ; a subdirectory
	      do (push (loadlist-file-data (concat f "/")) files)
	    end)
      files)))

(defun loadlist-refresh-contents (arg &optional dir)
  "Replace contents of loadlist buffer with contents of its directory.
With a prefix arg accept all changes without prompting."
  (interactive "*P")
  (unless dir
    (setq dir (file-name-directory (buffer-file-name))))
  ;;Do not bother prompting if buffer is empty.
  (if (zerop (buffer-size)) (setq arg t))

  (let ((buflist (loadlist-buffer-list))
	updated
	buf-i
	newlist)
    (with-output-to-temp-buffer "*loadlist changes*"
      (dolist (i (loadlist-directory-list dir))
	(cond ((setq buf-i (assoc (elt i 0) buflist))
	       (let ((real-file    (elt i 1))
		     (replace-file (or (elt i 1) (elt i 0)))
		     (real-str     (elt buf-i 1))
		     (len  (length (elt buf-i 0)))
		     (pt   (elt buf-i 2)))
		 (if (and (or real-file real-str)
			  (not (equal real-file real-str)))
		     (progn
		       (goto-char pt)
		       (delete-char len)
		       (insert replace-file)
		       (princ (format resources-loadlist-output-updated
				      (or real-str (elt buf-i 0))
				      replace-file))
		       (princ "\n")
		       (setq updated t)))
		 (setcdr buf-i nil)))
	      (t
	       (push (or (elt i 1) (elt i 0)) newlist))))
      (dolist (d (reverse buflist))
	(when (elt d 2)
	  (goto-char (elt d 2))
	  (beginning-of-line)
	  (let ((prompt (concat (format resources-loadlist-output-remove
					(or (elt d 1) (elt d 0))) " ")))
	    (when (or arg (y-or-n-p prompt))
	      (kill-line 1)
	      (princ prompt)
	      (princ "\n")
	      (setq updated t)))))
      (goto-char (point-max))
      (or (looking-at "$") (insert "\n"))
      (dolist (n newlist)
	(let ((prompt (concat (format resources-loadlist-output-append
				      n) " ")))
	  (when (or arg (y-or-n-p prompt))
	    (insert n "\n")
	    (princ prompt)
	    (princ "\n")
	    (setq updated t))))
      (unless updated
	(princ resources-loadlist-output-no-changes)
	(princ "\n")))))

(defun loadlist-compile-files-magik-proc ()
  "Return the Magik procedure to use.
If `loadlist-compile-files' is not nil, use \"compile_file_list\"
otherwise use \"load_file_list\"."
  (if loadlist-compile-files "compile_file_list" "load_file_list"))

(defun loadlist-transmit (&optional gis)
  "Load the loadlist.txt into the GIS process."
  (interactive)
  (let* ((gis (sw-get-buffer-mode gis
				  'gis-mode
				  resources-gis-enter-buffer
				  gis-buffer
				  'gis-buffer-alist-prefix-function))
	 (process (barf-if-no-gis gis))
	 (dir  (file-name-directory buffer-file-name))
	 (file (file-name-nondirectory buffer-file-name)))
    (message resources-loadlist-loaded-in-buffer file gis)
    (process-send-string
     process
     (concat 
      (magik-function (loadlist-compile-files-magik-proc) dir 'unset file)
      "$\n"))))    

(defun loadlist-toggle-compile (arg)
  "Toggle loading of .magikc files in operations from Loadlist and Module major modes."
  (interactive "P")
  (setq loadlist-compile-files
	(if (null arg)
	    (not loadlist-compile-files)
	  (> (prefix-numeric-value arg) 0)))
  (if loadlist-compile-files
      resources-loadlist-compile-files-on
    resources-loadlist-compile-files-off))

(defun loadlist-gis-drag-n-drop-load (gis filename)
  "Interface to Drag 'n' Drop GIS mode.
Called by `gis-drag-n-drop-load' when a load_list.txt file is dropped."
  (let ((process (barf-if-no-gis gis))
	(dir  (file-name-directory filename))
	(file (file-name-nondirectory filename)))
    (message resources-loadlist-loaded-in-buffer filename gis)
    (process-send-string
     process
     (concat
      (magik-function (loadlist-compile-files-magik-proc) dir 'unset file)
      "$\n"))))

;;; Package initialisation
(if loadlist-mode-syntax-table
    nil
  (setq loadlist-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?_ "w" loadlist-mode-syntax-table)
  (modify-syntax-entry ?# "<" loadlist-mode-syntax-table)
  (modify-syntax-entry ?\n ">" loadlist-mode-syntax-table))

;;; Package registration

(or (assoc "load_list\\.txt$" auto-mode-alist)
    (push '("load_list\\.txt$" . loadlist-mode) auto-mode-alist))

(provide 'loadlist)


;;; loadlist.el ends here
