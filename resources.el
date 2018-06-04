;;; Smallworld internationalisation code of Emacs.

;;; Commentary:

;;; Code:
(require 'macros-sw)

(defgroup resources nil
  "Group for Smallworld Emacs Message internationalisation."
  :group 'smallworld)

(defconst resources-version "$Revision: 1.4 $")

(defcustom resources-languages '("en_gb")
  "*Language(s) for Smallworld Emacs messages.
This is a list of languages to load for Smallworld Emacs messages.
The list has the primary language listed first."
  :group 'resources
  :type  '(repeat (choice (const :tag "English in United Kingdom" "en_gb")
			  (const :tag "English in United States" "en_us")
			  (const :tag "Francais en France" "fr_fr")
			  (const :tag "Francais au Canada" "fr_ca")
			  (const :tag "Francais en Belgique" "fr_be")
			  (const :tag "Francais en Suisse" "fr_ch")
			  (const :tag "Deutsch in Deutschland" "de_de")
			  (const :tag "Espanol de Espana" "es_es")
			  (const :tag "Dansk i Danmark" "da_dk")
			  (const :tag "Vlaams in Belgie" "nl_be")
			  (const :tag "Hollands in Nederland" "nl_nl")
			  (const :tag "Finnish in Finland" "fi_fi")
			  (const :tag "Deutsch in der Schweiz" "de_ch")
			  (const :tag "Italiano in Italia" "it_it")
			  (const :tag "Norsk i Norge" "no_no")
			  (const :tag "Portugues de Portugal" "pt_pt")
			  (const :tag "Svenska i Sverige" "sv_se"))))

(defcustom resources-additional-directory-list nil
  "*Additional list of directories contain Smallworld messages to load."
  :group 'resources
  :type  '(repeat directory))

;;; default resources definitions.
(defvar resources-load-resource-dir     "Loading resources %s.")
(defvar resources-entry-compile-dir     "Resources dir:")
(defvar resources-compile-resource-dir  "Compiling Resource %s...")
(defvar resources-compile-resource-done "Compiling Resources ... done")
(defvar resources-compile-buffer-error  "This is not an Emacs msg file")

;;; Interfaces for loading Emacs resource files.

(defun resources-load-directory (dir)
  "Load all the compiled resource files from DIR."
  (let ((msgc (concat (file-name-as-directory dir) "resources.msgc.el")))
    (message resources-load-resource-dir dir)
    (if (file-exists-p msgc)
	(load msgc t t t))))

(defun resources-load (&optional resources-dir additional)
  "Load resources from RESOURCES-DIR and ADDITIONAL if specified."
  
"ptional DIR specifies parent directory containing the 'resources' directory structure.
Optional ADDITIONAL list are directories containing additional messages to load.
Messages loaded via ADDITIONAL will override any previous definitions."
  (let (dir dirs)
    ;;First get list of directories according to specified language(s)
    (setq dirs (mapcar (function
			(lambda (lang)
			  (expand-file-name
			   (concat "resources/" lang "/messages")
			   resources-dir)))
		       (reverse resources-languages)))
    ;;Add any additional directories specified
    (setq dirs (append dirs (reverse additional)))
    (while dirs
      (setq dir (car dirs)
	    dirs (cdr dirs))
      (if (file-directory-p dir)
	  (resources-load-directory dir)))))

;;Note we even load resources that configure the messages for the
;;rest of this package!
(resources-load (file-name-directory load-file-name)
		resources-additional-directory-list)

;;; Interfaces for compiling Emacs resource files.

(defvar resources-syntax-table (make-syntax-table)
  "Syntax table for parsing message files for conversion to Elisp.
Based upon the syntax used in `msg-mode'.")

(modify-syntax-entry ?: "w" resources-syntax-table)
(modify-syntax-entry ?- "w" resources-syntax-table)
(modify-syntax-entry ?_ "w" resources-syntax-table)
(modify-syntax-entry ?? "w" resources-syntax-table)
(modify-syntax-entry ?! "w" resources-syntax-table)
(modify-syntax-entry ?| "$" resources-syntax-table)
(modify-syntax-entry ?# "/" resources-syntax-table)
(modify-syntax-entry ?\n "." resources-syntax-table)

(defun resources-compile-file (file)
  "Convert a Smallworld Emacs Message file FILE to Elisp."
  (save-excursion
    (let* ((visiting-buf (find-buffer-visiting file))
	   (file-buf (or visiting-buf (find-file file))))
      (resources-compile-buffer file-buf)
      (or visiting-buf (kill-buffer file-buf)))))

(defun resources-compile-buffer (&optional buffer)
  "Convert a Smallworld Emacs Message BUFFER to Elisp."
  (save-excursion
    (set-buffer (or buffer (current-buffer)))
    (let ((resources (get-buffer-create "*resources*"))
	  (file (buffer-file-name))
	  (syntax-table-orig (syntax-table))
	  (elisp (concat (file-name-directory (buffer-file-name)) "resources.msgc.el"))   ;;(concat (buffer-file-name) "c.el")
	  var-val
	  hmsg-p)
      (or (string-match "msg$" file)
	  (error resources-compile-buffer-error))
      (save-match-data
	(set-buffer resources)
	(erase-buffer)
	(set-buffer buffer)
	(and (buffer-modified-p) (save-buffer))
	(set-syntax-table resources-syntax-table)
	(goto-char (point-min))
	(setq hmsg-p (string-match "\\.hmsg$" file))
	(while (re-search-forward "^\\(:\\s$\\S$*\\s$\\)\\s-+\\(:\\sw+\\)?\\s-*\\(.*\\)"
				  nil t)
	  (setq var-val
		(if hmsg-p
		    (resources-compile-hmsg-file resources)
		  (resources-compile-msg-file resources)))
	  (save-excursion
	    (set-buffer resources)
	    (goto-char (point-max))
	    (insert (format "(setq resources-%s\n      %S)\n"
			    (car var-val) (cdr var-val)))))
	(set-syntax-table syntax-table-orig)
	(set-buffer resources)
	(write-region (point-min) (point-max) elisp t)))))

(defun resources-compile-msg-file (buffer)
  "Convert a Smallworld Emacs .msg BUFFER to Elisp, extension .msgc.el or .hmsgc.el."
  (let* ((str (match-string-no-properties 1))
	 (var (substring str 2 (- (length str) 1)))
	 (val (replace-regexp-in-string
	       "#[0-9]+" "%s" (match-string-no-properties 3))))
    (cons var val)))
    
(defun resources-compile-hmsg-file (buffer)
  "Convert a Smallworld Emacs .hmsg BUFFER to Elisp."
  (let* ((str (match-string-no-properties 1))
	 (var (substring str 2 (- (length str) 1)))
	 (val (match-string-no-properties 3))
	 pt)
    (if (> (length val) 0)
	nil
      (forward-line 1)
      (setq pt (point))
      (if (re-search-forward "^\\(:\\s$\\S$*\\s$\\)" nil t)
	  (beginning-of-line)
	(goto-char (point-max)))
      (skip-chars-backward " \t\n")
      (setq val (concat (buffer-substring-no-properties pt (point)) "\n")))
    (cons var val)))

(defun resources-compile-directory (&optional dir)
  "Compile resource files in DIR.
DIR will use the current directory by default.
Use a prefix key to select a specific directory."
  (interactive (let* ((d (file-name-directory (or buffer-file-name default-directory)))
		      (elisp (concat d "resources.msgc.el")))
		 (list
		  (if (or current-prefix-arg
			  (not (file-exists-p elisp)))
		      (read-file-name (concat resources-entry-compile-dir " ") d nil t)
		    d))))
  (let ((files (directory-files dir t "msg$"))
	(msgc (concat (file-name-as-directory dir) "resources.msgc.el"))
	file)
    (if (file-exists-p msgc) (delete-file msgc))
    (while files
      (setq file (car files))
      (message resources-compile-resource-dir file)
      (resources-compile-file file)
      (setq files (cdr files)))
    (message resources-compile-resource-done)))

(defun resources-add-to-Info-dir-list (resources-dir)
  "Add .info files to Info search path with stem RESOURCES-DIR.
Only the first directory that is found containing a 'dir' file
is added."
  ;;First get list of directories according to specified language(s)
  (let ((dirs (mapcar (function
		       (lambda (lang)
			 (expand-file-name
			  (concat "resources/" lang "/help/")
			  resources-dir)))
		      (reverse resources-languages)))
	(Info-var 'Info-additional-directory-list)
	dir)
    
    (while dirs
      (setq dir (car dirs)
	    dirs (cdr dirs))
      (if (file-exists-p (concat dir "dir"))
	  (progn
	    (add-to-list Info-var dir)
	    (setq dirs nil))))))

;;Add the Info directories ./resources/LANG/help relative to this source file
(resources-add-to-Info-dir-list (file-name-directory load-file-name))

(eval '(defvar Info-additional-directory-list nil))


(provide 'resources)
;;; resources.el ends here
