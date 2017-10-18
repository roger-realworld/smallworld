;;; sw-loadlis-mode.el -- mode for editing Magik load_list.txt files.

(defgroup sw-loadlist nil
  "Customise Magik load_list.txt files group."
  :group 'smallworld
  :version "$Revision: 2.0 $")

(defcustom sw-loadlist-ignore-regexp-list '("RCS")
  "List of Regexps used to miss certain files from load_list.txt files.
Intial ^ and final $ is automatically added in `loadlist-ignore'."
  :group 'loadlist
  :type  '(repeat regexp))


(defvar sw-loadlist-mode-map
  (let ((my-map (make-sparse-keymap)))
    (define-key my-map (kbd "C-c r")    'sw-loadlist-refresh-contents)
    (define-key my-map (kbd "<f2> b")   'sw-loadlist-transmit-buffer)
    my-map)
  "Keymap for Magik Message files.")

(defvar sw-loadlist-mode-font-lock-defaults
  '((
     ("^.+\\([\\/]\\)" 0 font-lock-keyword-face)
     ("^.+"            0 font-lock-variable-name-face)
     )))


(define-derived-mode sw-loadlist-mode prog-mode "sw-loadlist"
    "Major mode for editing Magik load_list.txt files."

  :group 'loadlist
  (setq font-lock-defaults sw-loadlist-mode-font-lock-defaults))


(defun sw-loadlist-ignore (file)
  "Return t if FILE match sw-loadlist-ignore-regexp-list.
Regexp does not need to include ^ or $."
  (loop for r in sw-loadlist-ignore-regexp-list
	for match = (string-match (concat "^" r "$") file)
	if match return t))


(defun sw-loadlist-directory-list (&optional dir)
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


(defun sw-loadlist-refresh-contents (arg &optional dir)
  "Replace contents of sw-loadlist buffer with contents of its directory.
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

(defun sw-loadlist-transmit (&optional gis)
  "Load the sw-loadlist.txt into the GIS process."
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
      (magik-function "load_file_list" dir 'unset file)
      "$\n"))))    


  
(provide 'sw-loadlist-mode)


;;; sw-loadlist-mode.el ends here
