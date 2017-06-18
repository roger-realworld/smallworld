;;; auto-gis.el; Running Gis Processes direct from the Emacs command line.

;; From an idea of Andrew Clarkson.

(require 'cl)
(require 'gis)

(defconst auto-gis-version "$Revision: 1.2 $")

(defvar auto-gis-post-hook nil
  "*Hook run after \\[auto-gis] command.")

(defvar auto-gis-command-list nil)
(defvar auto-gis-buffer-alist nil)
(defvar auto-gis-magik-file-alist nil)

(defvar auto-gis-load-file nil)
(make-variable-buffer-local 'auto-gis-load-file)
(put 'auto-gis-load-file 'permanent-local t)

(defun auto-gis-load-file ()
  ;;Wait for first Magik Prompt. gis-prompt set by gis-prompt-get
  (if auto-gis-load-file
      (progn
	(while (not gis-prompt)
	  (sit-for 1))
	(goto-char (point-max))
	;;Use insert so that the command appears in the buffer and
	;;therefore in the .gis_output log file
	(insert (concat "load_file(\"" auto-gis-load-file "\")"))
	(goto-char (point-max))
	(gis-newline nil)
	(message resources-auto-gis-load-file (current-buffer) auto-gis-load-file))))

(defun auto-gis-command-line-parser ()
  "Process the extra command line options added to Emacs to support `auto-gis'.
See `auto-gis' for more details."
  (interactive)
  (cond ((equal argi "-gis-command")
	 (setq gis-command (car command-line-args-left))
	 (subst-char-in-string ?| ?/ gis-command t)
	 (subst-char-in-string ?# ?  gis-command t)
	 (push gis-command auto-gis-command-list)
	 (message "gis-command='%s'" gis-command)
	 (setq command-line-args-left (cdr command-line-args-left))
	 t)
	((equal argi "-gis-buffer")
	 (or gis-command (error resources-auto-gis-no-command-error))
	 (push (cons gis-command (car command-line-args-left))
	       auto-gis-buffer-alist)
	 (setq command-line-args-left (cdr command-line-args-left))
	 t)
	((equal argi "-magik-file")
	 (or gis-command (error resources-auto-gis-no-command-error))
	 (push (cons gis-command (car command-line-args-left))
	       auto-gis-magik-file-alist)
	 (setq command-line-args-left (cdr command-line-args-left))
	 t)
	(t nil)))

(defun auto-gis-process-environment ()
  "Read Environment variables for `auto-gis'."
  (loop for e in process-environment
      if (string-match "^GIS_COMMAND\\([^=]*\\)=\\(.*\\)" e)
      do
      (let* ((postfix (match-string 1 e))
	     (command (match-string 2 e))
	     (buf     (getenv (concat "GIS_BUFFER" postfix)))
	     (file    (getenv (concat "MAGIK_FILE" postfix))))
	(or (member command auto-gis-command-list)
	    (push   command auto-gis-command-list))
	(or (assoc  buf     auto-gis-buffer-alist)
	    (push (cons command buf) auto-gis-buffer-alist))
	(or (assoc  file auto-gis-magik-file-alist)
	    (push (cons command file) auto-gis-magik-file-alist)))))
	       

(defun auto-gis (&optional buffer)
  "Run a Gis process direct from the command line of Emacs.
Adds the following command line arguments:

  -gis-command    command string to use to start Gis process

  -gis-buffer     buffer name to use for Gis process

  -magik-file     magik file to be loaded using load_file

Instead of -gis-command set the Environment Variable GIS_COMMAND.
Instead of -gis-buffer  set the Environment Variable GIS_BUFFER.
Instead of -magik-file  set the Environment Variable MAGIK_FILE.

If both the Environment Variable and the corresponding command line
argument are given then the command line takes precedent.

Note that it is imperative that the Emacs command line argument
\"-f auto-gis\" appears last.

Note that to overcome any limitations in your shell you may wish to
use a hash to represent a space and a | to represent a / in the
-gis-command argument.

Note also that the if the initial directory is omitted then the
current directory is added automatically.

Examples
--------
Commands are given using Windows command shell.

Using Environment Variables
  set GIS_COMMAND=sw_magik_win32 -Mextdir c:/temp -image d:/smallworld/product/images/swaf.msf
  set MAGIK_FILE=c:/Magik_startup.magik
  emacs -f auto-gis

Using command line options
  emacs -gis-command \"sw_magik_win32 -Mextdir c:/temp -image d:/smallworld/product/images/swaf.msf\" -magik-file \"c:/Magik_startup.magik\" -f auto-gis

Multiple Gis Processes
----------------------
You can start multiple Gis processes by giving multiple -gis-command arguments.
The -gis-buffer and -magik-file arguments can also be repeated.
The arguments are grouped thus:
  emacs -gis-command CMD1 -gis-buffer BUF1 -magik-file FILE1
        -gis-command CMD2 -gis-buffer BUF2 -magik-file FILE2
so that CMD1 is run in buffer BUF1 loading FILE1 at startup and
CMD2 is run in buffer BUF2 loading FILE2 at its startup.

Alternatively, the Environment variables can be used to specify
multiple sessions by adding any postfix to each Environment variable.
For example, the following three environment variables will act in
concert:
  GIS_COMMAND_SWAF
  GIS_BUFFER_SWAF
  MAGIK_FILE_SWAF
The postfix, _SWAF, is not used other than to group related variables."

  (interactive)
  (message "PATH=%s" (getenv "PATH"))
  (message "exec-path=%s" exec-path)
  (auto-gis-process-environment)

  ;; Exit Emacs unless a Gis command has been found.
  (if (zerop (length auto-gis-command-list))
      (kill-emacs 1))

  (dolist (command auto-gis-command-list)
    (let ((buf  (or buffer
		    (cdr (assoc command auto-gis-buffer-alist))
		    gis-buffer-default-name))
	  (file (cdr (assoc command auto-gis-magik-file-alist))))
      (set-buffer (get-buffer-create (generate-new-buffer-name buf)))
      (setq auto-gis-load-file file)
      (message "%s: %s. " (current-buffer) command)
      (gis (current-buffer) command)
      (run-hooks 'auto-gis-post-hook))))

;;; Package registration
(if (boundp 'command-line-functions)
    (add-to-list 'command-line-functions 'auto-gis-command-line-parser))
(add-hook 'gis-start-process-post-hook 'auto-gis-load-file t)

(provide 'auto-gis)
