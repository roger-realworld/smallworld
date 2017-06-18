;;; magik-patch.el - support for the creation, submission and approval of Magik patches.
;;
;; You can test this file by running the `eval-buffer' command.
;; i.e. it will load over the top of existing magik-patches.el.
;;
;; The debug section below can be uncommented so that the testing
;; will happen here in a private version of proposed_patches.
;;
;; This file is divided into 7 sections.  Each section has its functions
;; arranged in top-down order.  i.e. the top-level interactive functions
;; first and the functions they call next.
;;
;; 1. Admin.
;;
;; 2. Toggles.  The '/' and '\' keys.
;;
;; 3. Bugs.  A short section for jumping to a bug.
;;
;; 4. Patch creation. 
;;    a. Set up of Template code for Patches
;;
;;    b. Rest of patch user interface for creating/jumping to existing patches
;;    and change notes.  Includes patch-number completion and setting up of
;;    defaults.
;;
;; 5. Patch Process
;;
;;    A section containing the `magik-patch-submit' function and all the 
;;    complex code for approval, rejection and deferring of patches.
;;    Includes moving parts of the patch around and
;;    updating the `mpde-env-magik-patch-review-log-file', the `mpde-env-magik-patch-submissions-file'
;;    and the appropriate patch load list.
;;    Modified to call an external command to actually perform the real work
;;    but the code that remains is there to check the files validity as far as possible
;;    before running the external command.
;;
;; 6. Parsing and cache maintenance.  Reading the mpde-env-patch-options-file
;;    and magik-template files and updating and accessing the
;;    directory-file caches.
;;
;; 7. Utils.
;;
;; 8. MPDE Integration
;;
;;    Configuration code for the Magik Patch Development Environment.
;;    MPDE is designed to support Offline/Home working in a consistent way.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;      1.  A D M I N
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)
(require 'advice)
(require 'comint)

(require 'resources)
(require 'misc-sw)
(require 'utils-sw)
(require 'pragma)
(require 'magik-template)
(require 'magik)
(require 'sw-electric)
(require 'mpde-boot)
(require 'mpde-env)

(or (fboundp 'clearcase-path-convert)
    (defalias 'clearcase-path-convert 'identity))

;;; User variables
(defgroup magik-patch nil
  "Magik Patch minor mode, change management for Magik."
  :group 'magik)

(defconst magik-patch-version "$Revision: 1.59 $")

(defcustom magik-patch-who (capitalize (user-login-name))
  "*The name to use in the Who field when creating new magik patches.
Also used if you perform any other Magik patch processes."
  :group 'magik-patch
  :type 'string)

(defcustom magik-patch-interested-streams t
  "*This can be set to a list of those streams that you want patch caching to occur on.
If t   then all directories found in patch_options.txt with patchdir keyword are cached,
   nil then no caching is performed.

Normally set to a list of streams the developer is working on."
  :group 'magik-patch
  :type  'sexp)

(defcustom magik-patch-read-only-fields t
  "*Stores whether the read-only property is applied or not.
You must use \\[magik-patch-toggle-read-only-fields] to toggle this value."
  :group 'magik-patch
  :type 'boolean)
(make-variable-buffer-local 'magik-patch-read-only-fields)

(defconst magik-patch-date-format "%e %b %Y"
  "Returns current date in form 'dd mmm yyyy'")

(defface magik-patch-header-face
  '((((type tty) (class color)) ())
    (((class grayscale) (background light)) ())
    (((class grayscale) (background dark)) ())
    (((class color) (background light)) ())
    (((class color) (background dark)) ())
    (t ()))
  "Font-lock Face to use when displaying Magik patch headers."
  :group 'magik-faces)

(defface magik-patch-field-face
  '((((type tty) (class color)) ())
    (((class grayscale) (background light)) (:bold t :inherit magik-patch-header-face))
    (((class grayscale) (background dark)) (:bold t :inherit magik-patch-header-face))
    (((class color) (background light)) (:bold t :inherit magik-patch-header-face))
    (((class color) (background dark)) (:bold t :inherit magik-patch-header-face))
    (t (:bold t :inherit magik-patch-header-face)))
  "Font-lock Face to use when displaying field names in Magik patch headers."
  :group 'magik-faces)

(defvar magik-bug-history nil
  "History value for storing user entered bug and patch numbers")

;;Force the patch_options to be reparsed whenever this Lisp is re-loaded.
(if (buffer-live-p " *cache: patch_options.txt*")
    (kill-buffer " *cache: patch_options.txt*"))

;; These globals are read from the `magik-template' and `mpde-env-patch-options-file'
;; files when the user presses uses the `F2 o' or `F2 d' commands.  They are not
;; read every time the user presses a '/' or '\' key.
;;
(defvar magik-patch-patchdirs nil
  "A name-directory association list.
E.g. it could be
 ((\"leo\" . \"/bla/bla/bla\") (\"foo\" . \"/some/other/bla\"))")

(defvar magik-patch-toggles nil
  "A fieldname-optionlist association list.
E.g. it could be
 ((\"Risk\" . (\"Medium\" \"High\" \"Low\")) (\"Veto-Automerge\" . (\"No\" \"Yes\"))
  (\"Patch-Intended-For-Release\" . (\"leo\" \"leo SC\" \"mira\")))"
  )

(defvar magik-patch-cache-patchdirs nil
  "This global is for caching the results of calls to the `directory-files'.
It is a pathname-details association list, where the details include
a date-stamp, a list suitable for completion functions like
completing-read and all-completions.

E.g. it could be
 ((\"/bla/bla\" . ((13785 43739) ((\"1234_1\") (\"2222_2\"))))
  (\"/foo/bar\" . ((13785 12345) ((\"3333_3\")) \"3333_3\")))")

(defvar magik-patch-cache-allpatches nil
  "This cache contains all the valid patch numbers as a list of strings.
It is made by collecting together all the patch numbers from each patch directory
contained in the data of each `magik-patch-cache-patchdirs' cache.
e.g. 
  ((\"1234_1\") (\"2222_2\") (\"3333_3\"))

It is given directly to completing-read during the `F2 o' and `F2 d' commands.
It is re-calculated whenever any of the elements of magik-patch-cache-patchdirs
change.")

(defvar magik-patch-cache-bug-dir nil
  "The bug cache.  A global in the same format as one of the `magik-patch-cache-patchdirs'.
i.e. dirname, time and data. However, the actual list of bug numbers is
now stored in `magik-patch-cache-bug-list' which is set and read in from a separate file
for efficiency due to the extreme size of the directory.
  (\"/support/work/bug_reports\" (13785 43739) nil)")

(defvar magik-patch-cache-bug-list nil
  "Is the list of currently valid  bug numbers.
For efficiency, this is set and read in from a separate file due to the
extreme size of the bug directory.")

(defvar magik-patch-cache-topic-data nil
  "Cache to store time-stamp of `magik-patch-topic-data-file'.")

(defvar magik-patch-bug-number-regexp "[0-9][0-9][0-9][0-9][0-9]?[0-9]?[0-9]?[0-9]?"
  "Regexp that matches Bug numbers.

This is used by the variables magik-patch-number-regexp and magik-patch-file-regexp")

(defvar magik-patch-bug-filename-format "CBG%08d.txt"
  "Default format for converting a number into a bug filename.")

(defvar magik-patch-bug-file-regexp (concat "CBG0*\\(" magik-patch-bug-number-regexp "\\)\\.txt$")
  "Regexp that matches Magik patch file names")

(defvar magik-patch-number-regexp (concat "\\(" magik-patch-bug-number-regexp "\\)_\\([0-9]+[a-z]?\\)")
  "Regexp that matches Magik patch numbers.")

(defvar magik-patch-file-regexp (concat "P\\(" magik-patch-number-regexp "\\)\\.magik$")
  "Regexp that matches Magik patch file names")

(defvar magik-patch-mode nil
  "Minor mode variable for indicating whether Patch minor mode is enabled/disabled")
(make-variable-buffer-local 'magik-patch-mode)

(defvar magik-patch-goto-code-regexp "^#-------------------------------[-]*\\s-*\\(\n#\\s-*\\)?$"
  "Regexp used to match the line immediately preceeding Magik code.")

(defun magik-patch-goto-code ()
  "Goto start of code in Change file."
  (save-match-data
    (goto-char (point-max))
    (if (and magik-patch-goto-code-regexp
	     magik-patch-mode
	     (re-search-backward magik-patch-goto-code-regexp nil t))
	(forward-line 1)
      (goto-char (point-min))))
  (point))

(defun magik-patch-help ()
  "Display the documentation on writing Magik patches."
  (interactive)
  (set-buffer (get-buffer-create "*magik patch help*"))
  (erase-buffer)
  (magik-patch-mode-compile-help)
  (insert (documentation 'magik-patch-mode))
  (goto-char (point-min))
  (switch-to-buffer (current-buffer))
  (set-buffer-modified-p nil))

(defun magik-patch-mode-internal (&optional arg)
  "Performs the actual work magik-patch-mode does.
Since magik-patch-mode needs to be compiled on the fly to get its documentation
string set correctly the compiled function calls this internal function so that we only need
a single copy of the code."
  (or (eq major-mode 'magik-mode)
      (error resources-patch-invalid-major-mode-error major-mode))
  (make-local-variable 'magik-goto-code-function)

  (setq magik-patch-mode
	(if (if (null arg) (not magik-patch-mode)
	      (> (prefix-numeric-value arg) 0))
	    t))

  ;; setup magik-goto-code-function. For now have it always set once magik-patch-mode has
  ;; been enabled. I could reset it to the default value if magik-patch-mode is nil
  ;; but I will leave it for now. 
  (setq magik-goto-code-function 'magik-patch-goto-code)

  (force-mode-line-update)

  (magik-template-file-type)

  ;;Process the form adding properties and do rudimentary checking of form layout
  (save-excursion
    (goto-char (point-min))
    (let ((new-style (re-search-forward "^#Type: .*" nil t))
	  (header-end (re-search-forward "^$" nil t))
	  (inhibit-read-only t)
	  (buffer-undo-list t) ;Avoid adding these changes to undo list
	  (buffer-modified-p (buffer-modified-p)))
      (goto-char (point-min))
      (and new-style (magik-patch-field-setup header-end (not magik-patch-mode)))

      (or buffer-modified-p (set-buffer-modified-p nil))

      ;;Perform generic validation of loaded patch
      (or (= (buffer-size) 0) ;Cannot validate an empty file! But we do not warn since file may have just been created
	  (magik-patch-generic-validation "load" t))))
  
  ;;Allow users to turn off read-only attributes (some people don't like them).
  (or magik-patch-read-only-fields
      (let ((inhibit-read-only t))
	(remove-text-properties (point-min) (point-max) '(read-only nil))))
  magik-patch-mode)

;;TODO replace with info file.
(defun magik-patch-mode-compile-help ()
  "Creates the `magik-patch-mode' function taking the documentation string from an external file.
The external file MUST start and end with double quotes and also escape any double quotes in the
text."
  (save-excursion
    (set-buffer (get-buffer-create " *temp patch help*"))
    (erase-buffer)

    (if (file-exists-p mpde-env-patch-help-file)
	;;we use 'replace option for efficiency. See insert-file-contents for why.
	(insert-file-contents mpde-env-patch-help-file nil nil nil 'replace)
      (insert "\"No Patch help available.\nHelp file not found:"
	      mpde-env-patch-help-file "\n\"\n"))
    (goto-char (point-max))
    
    (insert "  (interactive \"P\")\n")
    (insert "  (magik-patch-mode-internal arg))\n")

    (goto-char (point-min))
    (insert "(defun magik-patch-mode (&optional arg)\n")
    (eval-buffer)))

(eval-when-compile (defun magik-patch-mode (&optional arg) (interactive "P"))) ; Make compiler shut up
(magik-patch-mode-compile-help) ; To define magik-patch-mode function and fill in its documentation

(defun magik-patch-toggle-read-only-fields (&optional arg)
  "Set/Unset the read-only text attribute on the fields in Magik Patch mode."
  (interactive "P")
  (or magik-patch-mode
      (error resources-patch-not-in-minor-mode-error))

  (setq magik-patch-read-only-fields
	(if (if (null arg) (not magik-patch-read-only-fields)
	      (> (prefix-numeric-value arg) 0))
	    t))
  (if magik-patch-read-only-fields
      (progn
	(magik-patch-mode -1) ;;force back on by turning Patch minor mode off and on
	(magik-patch-mode 1)
	(message resources-patch-fields-readonly))
    (let ((inhibit-read-only t))
      (remove-text-properties (point-min) (point-max) '(read-only nil))
      (message resources-patch-fields-not-readonly)))
  magik-patch-read-only-fields)

(or (assoc 'magik-patch-mode minor-mode-alist)
    (push (list 'magik-patch-mode (concat " " resources-patch-menu)) minor-mode-alist))

(defun magik-patch-maybe-turn-on-patch-mode (&rest ignore)
  "Turns on magik-patch-mode minor mode if the file is called a P*****_*.magik"
  (if buffer-file-name
      (save-match-data
	(if (string-match magik-patch-file-regexp
			  (file-name-nondirectory buffer-file-name))
	    (progn
	      (magik-patch-mode t)
	      (add-hook 'local-write-file-hooks 'magik-patch-mpde-save-buffer))))))

(add-hook 'magik-mode-hook           'magik-patch-maybe-turn-on-patch-mode)

(remove-hook 'find-file-not-found-hooks 'maybe-insert-patch-template) ;another old hook
(remove-hook 'find-file-not-found-hooks 'magik-patch-file-check) ;the old hook.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;      2.  T O G G L E S
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun magik-patch-electric-/ (arg)
  "Toggles the current patch-field setting or pragma setting, or else inserts a slash."
  (interactive "*p")
  (magik-patch-electric-toggle-option arg 'forward))

(defun magik-patch-electric-back-/ (arg)
  "Toggles the current patch-field setting or pragma setting, or else inserts a backslash."
  (interactive "*p")
  (magik-patch-electric-toggle-option arg 'backward))

(defun magik-patch-electric-toggle-option (arg direction)
  (if (member last-command '(magik-patch-electric-/
			     magik-patch-electric-back-/))
      nil
    (magik-patch-cache-refresh-patch-options)
    (magik-patch-cache-refresh-templates)
    (magik-patch-cache-refresh-topic-data))
  (or (magik-patch-toggle-option direction)
      (pragma-electric-toggle-option arg direction)))

(defun magik-patch-electric-space (arg &optional doit)
  "expand magik keywords into programming templates in a Magik Patch file."
  (interactive "*p")
  (cond
   ((save-excursion
      (back-to-indentation)
      (looking-at "##?"))
    (let*
	((match-str (match-string-no-properties 0)) ;we do not want to copy any text properties.
	 (auto-fill-function 'do-auto-fill)
	 (fill-prefix (concat
		       (save-excursion
			 (back-to-indentation)
			 (buffer-substring (point-bol) (point)))
		       match-str
		       " ")) ;we want to make the next line indented with a space
	 (fill-column (save-excursion
			(back-to-indentation)
			(+ (current-column) 63))))
      (self-insert-command arg)))
   (t
    (electric-magik-space arg doit))))

(defvar magik-patch-toggle-hook nil
  "List of functions to run after setting a toggled field.
Functions take two arguments, the name of the field and the value.")

(defun magik-patch-field-remove-help-text (current start end)
  "Removes a help text prompter (<leave-blank> etc.) from the current field."
  (let (help-start help-end)
    (save-match-data
      (if (string-match "<\\(leave-blank\\|mandatory\\)[^>\n]*>" current)
	  (progn
	    (setq help-start (match-beginning 0)
		  help-end (match-end 0))
	    (delete-region (+ start help-start) (+ start help-end))
	    (- end (- help-end help-start)))))))

(defun magik-patch-toggle-option (direction &optional field current choices start end)
  "Toggles the current patch field according to the magik-patch-toggles global.
magik-patch-toggles either provides a list of valid values or a symbol to a lisp function
to call to get the list of possible values.
If a function is called then magik-patch-toggle-option is then called again with the
optional arguments set.

Returns t if it actually did a toggle."
  ;;Get field, current value and possible choices if not already given
  ;;ensure point is at start of were current value is.
  (if (not (and field current choices start end))
      (save-match-data
	(if (save-excursion
	      (beginning-of-line)
	      (looking-at "^#\\([A-Z][A-Za-z_-]+\\):[ \t]*\\(.*\\)"))
	    (let (new-end)
	      (setq field (match-string-no-properties 1)
		    current (match-string-no-properties 2)
		    start   (match-beginning 2)
		    end     (match-end 2)
		    choices (cdr (assoc field magik-patch-toggles))
		    new-end (magik-patch-field-remove-help-text current start end))
	      (if new-end
		  (setq end new-end
			current (buffer-substring-no-properties start new-end)))
	      (goto-char start)))))

  (if (and field current choices start end)
      (let ((inhibit-read-only t))
	;;The return value from cond is impmortant!
	(cond ((functionp choices)
	       ;;Call function to find choices then call magik-patch-toggle-option recursively (once!)
	       ;;returns the value of the recursive call
	       (magik-patch-toggle-option direction field current (funcall choices start end) start end))
	      ((equal current "")
	       ;;No value currently set, use first of available choices
	       (let ((new (car choices)))
		 (insert new)
		 (run-hook-with-args 'magik-patch-toggle-hook (intern field) new))
	       t ;;success
	       )
	      (t
	       ;;We have a list of real choices - not a function call.
	       (let* ((direction (if (eq direction 'forward) 1 -1))
		      (position (position current choices :test 'equal))
		      (start (point))
		      (end (+ start (length current)))
		      new)
		 (if position
		     (progn		     
		       (delete-region start end)
		       (setq new (nth (mod (+ position direction) (length choices)) choices))
		       (insert new)
		       (run-hook-with-args 'magik-patch-toggle-hook (intern field) new)
		       t ;;success
		       )
		   (error resources-patch-field-value-invalid field current))
		 ))))))

(defun magik-patch-data-alist (names alist &rest keys)
  "Return key in multi-level alist"
  (if (car keys)
      (let* ((name (pop names))
	     (key (pop keys))
	     (value-alist (assoc key alist)))
	(cond ((not value-alist)
	       (error resources-patch-type-key-unknown name key))
	      ((eq (length keys) 0)
	       (cdr value-alist))
	      (t
	       ;;RECURSIVE
	       (apply 'magik-patch-data-alist names (cdr value-alist) keys))))
    ;;keys has been passed nil which I want to ignore
    ))

(defun magik-patch-field-get-single (field &optional noerror optional-space)
  "Get the value of a FIELD single field.
If NOERROR is t then if field cannot be found then function
returns nil instead of rasing an error.
If OPTIONAL-SPACE is t then do not force the requirement of a space after the :
but insert one if not there and save the buffer.
"
  (save-excursion
    (goto-char (point-min))
    (save-match-data 
      (if (re-search-forward
	   (concat "^#" field ":[ \t]" (if optional-space "*" "+") "\\(.*\\)")
	   nil t)
	  (let ((value (match-string-no-properties 1))
		(inhibit-read-only optional-space)
		(inhibit-point-motion-hooks optional-space))

	    ;;Ensure there is a space between : and value if optional-space option given
	    (if (and optional-space (eq (preceding-char) ?  ))
		(progn
		  (goto-char (match-beginning 1))
		  (insert " ")
		  (save-buffer)))
	  
	    ;;strip off any whitespace
	    (if (string-match "\\s-+$" value)
		(setq value (substring value 0 (match-beginning 0))))
	    value)
	(if noerror
	    nil	;;return nil
	  (error resources-patch-no-field-error field))))))

(defun magik-patch-field-get-multi (field &optional noerror)
  "Get the value of a FIELD multi-line field."
  (save-excursion
    (goto-char (point-min))
    (save-match-data 
      (if (re-search-forward (concat "^#" field ":[ \t]*\\(.*\n\\(# .*\n\\)+\\)") nil t)
	  (let ((value (match-string-no-properties 1)))
	    (while (string-match "# " value)
	      (setq value (concat (substring value 0 (match-beginning 0))
				  (substring value (match-end 0)))))
	    (if (string-match "^\\s-*\n" value)
		(setq value (substring value (match-end 0))))
	    (if (string-match "\n$" value)
		(setq value (substring value 0 (match-beginning 0)))))
	(if noerror
	    nil	;;return nil
	  (error resources-patch-no-field-error field))))))

(defun magik-patch-summary-line ()
  "Return the #Summary field line.

The Summary field is treated specially because although it should
be a single line field, sometimes the word-wrapping splits the text
over multiple lines."
  (let ((summary (or (magik-patch-field-get-multi "Summary" t)
		     (magik-patch-field-get-single "Summary"))))
    (if (not summary)
	(error "No #Summary: line")
      (setq summary (magik-patch-make-summary-line summary))
      (if (equal summary "")
	  (error "#Summary: line is empty")))
    summary))

(defvar magik-patch-field-completion-hook nil
  "List of functions to run after setting field hook via completion (i.e. TAB).
Functions take two arguments, the name of the field and the value.")


(defun magik-patch-field-completion (field field-alist start end)
  "Does field completion based upon the magik-patch-toggles global.
"
  (interactive "*")
  (let* ((inhibit-point-motion-hooks t)
	 (completion-ignore-case t)
	 (field-string (buffer-substring start end))
	 (empty-string-valid-p (and (equal field-string "")
				    (assoc field-string field-alist)))
	 (field-try-completion (try-completion field-string field-alist)))
    (cond
     ((null field-try-completion)
      (error resources-patch-field-no-completions-error field field-string))
     ((eq field-try-completion t)
      (message resources-patch-field-already-complete field-string field))
     ((> (length field-try-completion) (length field-string))
      (goto-char start)
      (delete-region start end)
      (insert field-try-completion)
      (setq field-string field-try-completion))
     (t
      (let ((field-completions (all-completions field-string field-alist)))
	(cond
	 ((eq (length field-completions) 1)
	  (goto-char start)
	  (delete-region start end)
	  (insert field-try-completion)
	  (setq field-string field-try-completion)
	  (message resources-patch-field-already-complete field-string field))
	 (t
	  (goto-char end)
	  (and empty-string-valid-p
	       (message resources-patch-empty-string-is-valid field))
	  (comint-dynamic-list-completions field-completions)
	  )))))
    (run-hook-with-args 'magik-patch-field-completion-hook (intern field) field-string (mapcar 'car field-alist))))

(defun magik-patch-indent-command ()
  "Does topic completion or else magik indent."
  (interactive "*")
  (let (used-completion-p)
    (save-match-data
      (if (save-excursion
	    (beginning-of-line)
	    (looking-at "^#\\([A-Z][A-Za-z_-]+\\):[ \t]*\\(.*\\)"))
	  (let* ((field (match-string-no-properties 1))
		 (current (match-string-no-properties 2))
		 (start (match-beginning 2))
		 (end   (match-end 2))
		 alist
		 choices
		 new-end)
	    (magik-patch-cache-refresh-patch-options)
	    (magik-patch-cache-refresh-templates)
	    (magik-patch-cache-refresh-topic-data)
	    (setq alist (assoc field magik-patch-toggles)
		  choices (cdr alist))
	    (if alist
		(progn
		  (setq used-completion-p t
			new-end (magik-patch-field-remove-help-text current start end))
		  (if new-end
		      (setq end new-end
			    current (buffer-substring-no-properties start new-end)))
			
		  (if (functionp choices)
		      ;;Must call a function to retrieve list of choices then make into an alist
		      (setq choices (mapcar '(lambda (c) (cons c c)) (funcall choices start end)))
		    ;;Make list of choices into an alist
		    (setq choices (mapcar '(lambda (c) (cons c c)) choices)))
		  ;;Now do the completion
		  (magik-patch-field-completion field choices start end)))))
      ;;if completion functionality was not used just indent.
      (or used-completion-p
	  (magik-indent-command)))))

(defvar magik-patch-topic-data nil
  "The data structure for stream, topic, subtopic, owner and reviewer data")

(defvar magik-patch-topic-data-file
  (concat-u-colon "/swdev/data/CQ/Emacs/topic_data.el")
  "The data file containing the stream, topic, subtopic, owner and reviewer data.
This file is generated from the master data held within ClearQuest")

;;(defun magik-patch-toggle-streams (&optional start end)
;;  "Return list of posible Topics for this patch."
;;  (let* ((key       "Patch-Intended-For-Release")
;;	 (choices   (magik-patch-data-alist (list key)
;;					    magik-patch-topic-data)))
;;    (if choices
;;	(setq choices (mapcar 'car choices))
;;      (error "No %s found" key ))
;;    choices))

(defun magik-patch-toggle-topics (&optional start end)
  "Return list of posible Topics for this patch."
  (let* ((stream    (car (nth 1 (magik-patch-target-alist))))
	 (key       "Topic")
	 (choices   (magik-patch-data-alist (list "Stream" key)
					    magik-patch-topic-data stream)))
    (if choices
	(setq choices (mapcar 'car choices))
      (error resources-patch-no-topic-for-stream-error key stream))
    choices))

(defun magik-patch-toggle-subtopics (&optional start end)
  "Return list of posible Sub-Topics for this patch."
  (let* ((stream    (car (nth 1 (magik-patch-target-alist))))
	 (topic     (magik-patch-field-get-single "Topic"))
	 (key       "Sub-Topic")
	 (choices   (magik-patch-data-alist (list "Stream" "Topic" key)
					    magik-patch-topic-data stream topic)))
    (if choices
	(setq choices (mapcar 'car choices))
      (error resources-patch-no-subtopic-for-topic-stream-error key stream topic))
    choices))

(defun magik-patch-toggle-reviewers (&optional start end)
  "Return list of registered reviewers for this patch.
The current user name is also added to the list if not already there."
  (let* ((username  (car (magik-patch-toggle-current-user)))
	 (stream    (car (nth 1 (magik-patch-target-alist))))
	 (topic     (magik-patch-field-get-single "Topic"))
	 (sub-topic (magik-patch-field-get-single "Sub-Topic"))
	 (key       "Reviewers")
	 (choices   (mapcar 'capitalize (car (magik-patch-data-alist
					      (list "Stream" "Topic" "Sub-Topic" key)
					      magik-patch-topic-data stream topic sub-topic key)))))
    (or (position username choices :test 'equal)
	(position (downcase username) choices :test 'equal)
	(push username choices))
    (or choices
	(error resources-patch-no-developer-error key stream topic sub-topic))

    (if start
	(save-excursion
	  ;;To provide backward compatibility for old style patch template
	  ;;must check that the character before START is a space.
	  (goto-char start)
	  (or (eq (preceding-char) ?  )
	      (setq choices (mapcar '(lambda (n) (concat " " n)) choices)))))
    choices))

(defun magik-patch-toggle-owners (&optional start end)
  "Return list of registered reviewers for this patch.
The current user name is also added to the list if not already there."
  (let* ((username  (car (magik-patch-toggle-current-user)))
	 (stream    (car (nth 1 (magik-patch-target-alist))))
	 (topic     (magik-patch-field-get-single "Topic"))
	 (sub-topic (magik-patch-field-get-single "Sub-Topic"))
	 (key       "Owners")
	 (choices   (mapcar 'capitalize (car (magik-patch-data-alist
					      (list "Stream" "Topic" "Sub-Topic" key)
					      magik-patch-topic-data stream topic sub-topic key)))))
    (or (position username choices :test 'equal)
	(position (downcase username) choices :test 'equal)
	(push username choices))
    (or choices
	(error resources-patch-no-developer-error key stream topic sub-topic))

    (if start
	(save-excursion
	  ;;To provide backward compatibility for old style patch template
	  ;;must check that the character before START is a space.
	  (goto-char start)
	  (or (eq (preceding-char) ?  )
	      (setq choices (mapcar '(lambda (n) (concat " " n)) choices)))))
    choices))

(defun magik-patch-toggle-current-user (&optional start end)
  "Returns current user name as a list for use in toggle and TAB completion functionality."
  (let ((username magik-patch-who))
    (if start
	(save-excursion
	  ;;To provide backward compatibility for old style patch template
	  ;;must check that the character before START is a space.
	  (goto-char start)
	  (or (eq (preceding-char) ?  )
	      (setq username (concat " " username)))))
    (list username)))

;;(or (assoc "Patch-Intended-For-Release" magik-patch-toggles)
;;    (push (cons "Patch-Intended-For-Release" 'magik-patch-toggle-streams) magik-patch-toggles))
(or (assoc "Topic" magik-patch-toggles)
    (push (cons "Topic" 'magik-patch-toggle-topics) magik-patch-toggles))
(or (assoc "Sub-Topic" magik-patch-toggles)
    (push (cons "Sub-Topic" 'magik-patch-toggle-subtopics) magik-patch-toggles))
(or (assoc "Tested-By" magik-patch-toggles)
    (push (cons "Tested-By" 'magik-patch-toggle-owners) magik-patch-toggles))
(or (assoc "Checked-By" magik-patch-toggles)
    (push (cons "Checked-By" 'magik-patch-toggle-reviewers) magik-patch-toggles))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;      3.  B U G S
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun magik-patch-bug-filename (number &optional format)
  "Return the filename for a bug report given a number.
Automatically converts a number supplied as a string.
If the string equivalent is not valid number then uses a format of %s.
ie.  12345          -> CBG00012345.txt
    \"12345\"         -> CBG00012345.txt
    CBG00012345.txt -> CBG00012345.txt"
  (if (stringp number)
      ;; Been given a string...
      (if (> (string-to-number number) 0) ;;successful conversion string was simply stringified number
	  (setq number (string-to-number number))
	;;string was not a pure number use simple format unless one has been supplied.
	(or format (setq format "%s"))))
      
  (concat-/ mpde-env-bug-report-dir
	    (format (or format magik-patch-bug-filename-format) number)))

(defun magik-patch-bug-get (number)
  "Jumps to a bug report.
If the current buffer is a patch with a matching bug number,
that bug number is the default.

Returns the buffer if the file containing the bug is found or nil."
  (interactive (magik-patch-bug-get-interactive-info))
  (let* ((filename (magik-patch-bug-filename number))
	 buffer)
    (if (file-exists-p filename)
	(prog1				;return buffer
	    (setq buffer (find-file-noselect filename))
	  (switch-to-buffer-other-window buffer)
	  (setq buffer-read-only t)))))
(defalias 'get-magik-bug 'magik-patch-bug-get)

(defun magik-patch-bug-get-interactive-info ()
  "Reads the bug number from the user.
Implements bug number completion based on the entire contents of the bug directory.
The user gets offered a default bug number if the current file is a patch file."
  (magik-patch-cache-refresh-bugdata)
  (let* ((pot-number (magik-patch-bug-number-potential))
	 (bug-file (and pot-number (magik-patch-bug-filename pot-number))))
    (list
     (completing-read (concat resources-patch-bug-number-prompt " ")
		      magik-patch-cache-bug-list
		      nil t
		      (if (and bug-file
			       (file-exists-p bug-file))
			  (file-name-nondirectory bug-file)
			;;Perform a complete on the empty string to pre fill the selection
			(try-completion "" magik-patch-cache-bug-list))
		      'magik-bug-history))))

(defun magik-patch-bug-number-potential ()
  "If the current file is a patch file, returns the associated bug number.  Otherwise returns nil."
   (if buffer-file-name
      (let ((leafname (file-name-nondirectory buffer-file-name)))
	(save-match-data
	  (if (string-match magik-patch-file-regexp leafname)
	      (match-string 2 leafname))))))

(defun magik-patch-bug-is-defect-p (bug-number)
  "Return t if the BUG-NUMBER supplied is of Type: Bug.
Return nil otherwise, e.g. BUG-NUMBER is a suggestion of enhancement request."
  (let* ((bug-filename (magik-patch-bug-filename bug-number))
	 (buffer-visiting-bug (find-buffer-visiting bug-filename))
	(bug-is-defect-p nil))
    (save-window-excursion
      (save-excursion
	(if (magik-patch-bug-get bug-number)
	    (save-excursion
	      (goto-char (point-min))
	      (if (search-forward "Type: " nil t)
		  (setq bug-is-defect-p
			(equal (buffer-substring-no-properties (point) (+ (point) 3)) "Bug")))
	      (or buffer-visiting-bug (kill-buffer (current-buffer))))
	  (error resources-patch-no-file-error bug-filename))))
    bug-is-defect-p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;      4.  P A T C H   C R E A T I O N
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;; Patch Template code
;;;;;;;;;;;;;;;;;;

;;
;; These two variables have to be dynamics so that they get picked up by an argument-less
;; hook function, `magik-patch-maybe-turn-on-patch-mode'.
;;
;; They are set in magik-patch-make and magik-patch-make-change-note and magik-patch-make-internal
;; which calls find-file which may trigger the not-found hook called
;; magik-patch-maybe-turn-on-patch-mode.
;;
(defvar magik-patch-topic nil)

(defvar magik-patch-file-type-templates-default '((patch  "Patch"  "template_magik_patch.magik")
						  (change "Change" "template_change_note.magik"))
  "Default list for Patch templates.
The order is important since the first in the list is the default template used when
the user presses [return] when asked to select a template.
If you want to change the order and hence the default used.

For example, to make the last in the list the default use:
(magik-template-file-type-alist-add \"Patch\"
			   'magik-patch-file-number
			   (reverse magik-patch-file-type-templates-default)
			   'replace)")

(defvar magik-patch-file-type-test-template
  '((test "Test" "template_unit_test.magik"))
"Default list for Test templates.")

(defvar magik-patch-field-singleline-properties
  '((initial-newline     . (read-only t rear-nonsticky t front-sticky nil))
    (field-text          . (read-only t intangible t rear-nonsticky nil front-sticky t fontified t face magik-patch-field-face multi-line nil))
    (trailing-whitespace . (read-only t intangible nil rear-nonsticky t front-sticky nil fontified t face magik-patch-field-face multi-line nil)))
  "Default properties for the multiline text field.
This is an alist that defines the properties to use for each part of the line.")

(defvar magik-patch-field-multiline-properties
  '((initial-newline . (read-only t rear-nonsticky t front-sticky nil))
    (field-text       . (read-only t intangible t rear-nonsticky nil front-sticky t fontified t face magik-patch-field-face multi-line t)))
  "Default properties for the multiline text field.
This is an alist that defines the properties to use for each part of the line.")

(defvar magik-patch-field-multiline-text-properties
  '(read-only t intangible t rear-nonsticky t front-sticky t fontified t face magik-patch-field-face multi-line nil)
  "Default properties for the line immediately following a multiline text field")

(defvar magik-patch-field-type-map nil
  "Local map for the #Type: field.")

(defvar magik-patch-field-changenumber-map nil
  "Local map for the #Change-Number: field")

(if magik-patch-field-type-map
    nil
  (setq magik-patch-field-type-map (make-sparse-keymap))
  (define-key magik-patch-field-type-map [mouse-2] 'magik-patch-convert))

(if magik-patch-field-changenumber-map
    nil
  (setq magik-patch-field-changenumber-map (make-sparse-keymap))
  (define-key magik-patch-field-changenumber-map [mouse-2] 'magik-patch-rename))

(defvar magik-patch-field-special-properties
  (let ((type-props          (list 'mouse-face 'highlight
				   'face 'magik-patch-field-face
				   'rear-nonsticky nil
				   'read-only t 'intangible nil
				   'local-map magik-patch-field-type-map
				   'help-echo (format resources-patch-help-convert "mouse-2")))
	(change-number-props (list 'mouse-face 'highlight
				   'face 'magik-patch-field-face
				   'rear-nonsticky nil
				   'read-only t 'intangible nil
				   'local-map magik-patch-field-changenumber-map
				   'help-echo (format resources-patch-help-rename "mouse-2"))))
    (list (cons "Type:"
		(list (cons 'field-text          type-props)
		      (cons 'trailing-whitespace type-props)
		      (cons 'default-value       type-props)))
	  (cons "Change-Number:"
		(list (cons 'field-text          change-number-props)
		      (cons 'trailing-whitespace change-number-props)
		      (cons 'default-value       change-number-props)))))
  "Special properties alist keyed first on field name then by line component.")

;;Overriding default setting of magik-template-file-type-p
(defun magik-template-file-type-p (buffer-name)
  "Hook function that identifies 'default' Magik files.

Modify this function to return a suitable match for the various templates
you have.

This hook should come last."
  (if (re-search-forward "^_package sw" nil t)
      'default))

(defun magik-template-file-type-change-note-p (buffer-name)
  "Hook function that identifies Change Note files."
  (if (and (string-match (concat "^P" magik-patch-number-regexp "\\.magik$") buffer-name)
	   (or (re-search-forward "^#Type: Change" nil t) (re-search-forward "^#Module:" nil t)))
      'change))
(add-hook 'magik-template-file-type-hook 'magik-template-file-type-change-note-p)

(defun magik-template-file-type-patch-p (buffer-name)
  "Hook function that identifies Magik Patch patch files."
  (if (and (string-match magik-patch-file-regexp buffer-name)
	   (or (re-search-forward "^#Type: Patch" nil t) (re-search-forward "^#Patch-Number:" nil t)))
      'patch))
(add-hook 'magik-template-file-type-hook 'magik-template-file-type-patch-p)

(defun magik-template-file-type-unit-test-p (buffer-name)
  "Hook function that identifies Unit Test files."
  (if (and (string-match (concat "^P" magik-patch-number-regexp "\\.magik$") buffer-name)
	   (re-search-forward "^#Type: Test" nil t))
      'test))
(add-hook 'magik-template-file-type-hook 'magik-template-file-type-unit-test-p)

(defun magik-patch-field-setup (end-header &optional remove)
  "Sets up/removes the form control for the fields in the Patch form.

END-HEADER is the point upto which the field processing is performed.

If REMOVE is t then ALL the properties in the buffer are removed.

SPECIAL-FIELDS is an alist of Fields that can be made to have different properties thatn the default.
The cdr is list of extra properties to set for that field."
  (let (initial-newline-s
	initial-newline-e
	field-text-s
	field-text-e
	trailing-whitespace-s
	trailing-whitespace-e
	default-value-s
	default-value-e
	field-name
	special-props
	(inhibit-read-only t))
    (if remove
	(set-text-properties (point-min) (point-max) nil)
      (add-text-properties (point) end-header '(face magik-patch-header-face))
      (while (re-search-forward "\\(\n\\)\\(#\\([A-Z]?[-A-Za-z]*:\\)\\)\\( ?\\)\\s-*\\([^\n]*\\)" end-header t)
	;;set up the text properties
	(setq initial-newline-s     (match-beginning 1)
	      initial-newline-e     (match-end 1)
	      field-text-s          (match-beginning 2)
	      field-text-e          (match-end 2)
	      field-name            (buffer-substring (match-beginning 3) (match-end 3))
	      trailing-whitespace-s (match-beginning 4)
	      trailing-whitespace-e (match-end 4)
	      default-value-s       (match-beginning 5)
	      default-value-e       (match-end 5)
	      special-props         (cdr (assoc field-name magik-patch-field-special-properties)))
	
	(cond ((equal field-name "")
	       ;;possible start of text for multi-line text field
	       (if (and (get-text-property (save-excursion (forward-line -1) (point)) 'multi-line)
			(> trailing-whitespace-e trailing-whitespace-s))
		   (magik-patch-field-multiline-text-properties field-text-s trailing-whitespace-e)))
	      ((eq trailing-whitespace-s trailing-whitespace-e)
	       ;;Multi-line text field
	       (magik-patch-field-multiline-properties initial-newline-s initial-newline-e
						       field-text-s      field-text-e))
	      (t
	       ;;Single line field
	       (magik-patch-field-singleline-properties initial-newline-s     initial-newline-e
							field-text-s          field-text-e
							trailing-whitespace-s trailing-whitespace-e)))
	
	(if (not (eq default-value-s default-value-e))
	    ;;We have a default value
	    ;;Make all <> text intangible
	    (save-excursion
	      (goto-char default-value-s)
	      (while (re-search-forward "<[^>]+>" default-value-e t)
		(add-text-properties (match-beginning 0) (match-end 0) '(intangible t rear-nonsticky t front-sticky nil)))))
	
	(if special-props
	    (magik-patch-field-special-properties special-props
						  initial-newline-s     initial-newline-e
						  field-text-s          field-text-e
						  trailing-whitespace-s trailing-whitespace-e
						  default-value-s       default-value-e))))))

(defun magik-patch-field-singleline-properties (initial-newline-start initial-newline-end
								      field-text-start field-text-end
								      trailing-whitespace-start trailing-whitespace-end)
  "Set the properties for the singleline text field.

INITIAL-NEWLINE-START      is the point preceding the initial newline.
INITIAL-NEWLINE-END        is the point following the initial newline.
FIELD-TEXT-START           is the point preceding the field text string.
FIELD-TEXT-END             is the point following the field text string.
TRAILING-WHITESPACE-START  is the point preceding the single whitespace marker.
TRAILING-WHITESPACE-END    is the point following the single whitespace marker."

  (let ((initial-newline-props     (cdr (assq 'initial-newline     magik-patch-field-singleline-properties)))
	(field-text-props          (cdr (assq 'field-text          magik-patch-field-singleline-properties)))
	(trailing-whitespace-props (cdr (assq 'trailing-whitespace magik-patch-field-singleline-properties))))

    (add-text-properties initial-newline-start     initial-newline-end     initial-newline-props)
    (add-text-properties field-text-start          field-text-end          field-text-props)
    (add-text-properties trailing-whitespace-start trailing-whitespace-end trailing-whitespace-props)))

(defun magik-patch-field-multiline-properties (initial-newline-start initial-newline-end field-text-start field-text-end)
  "Set the properties for the multiline text field.

INITIAL-NEWLINE-START is the point preceding the initial newline.
INITIAL-NEWLINE-END   is the point following the initial newline.
FIELD-TEXT-START      is the point preceding the field text string.
FIELD-TEXT-END        is the point following the field text string."

  (let ((initial-newline-props (cdr (assq 'initial-newline magik-patch-field-multiline-properties)))
	(field-text-props      (cdr (assq 'field-text      magik-patch-field-multiline-properties))))

    (add-text-properties initial-newline-start initial-newline-end initial-newline-props)
    (add-text-properties field-text-start      field-text-end      field-text-props)))


(defun magik-patch-field-multiline-text-properties (start end)
  "Set the properties for the line immediately following a multiline text field.

START is the start of the line.
END is the point following initial string, usually \"# \"."
  (add-text-properties start end magik-patch-field-multiline-text-properties))

(defun magik-patch-field-special-properties (property-alist
					     initial-newline-start initial-newline-end
					     field-text-start field-text-end
					     trailing-whitespace-start trailing-whitespace-end
					     default-value-start default-value-end)
  "Set the properties for the singleline text field.

PROPERTY-ALIST             is the alist containing the special properties 
INITIAL-NEWLINE-START      is the point preceding the initial newline.
INITIAL-NEWLINE-END        is the point following the initial newline.
FIELD-TEXT-START           is the point preceding the field text string.
FIELD-TEXT-END             is the point following the field text string.
TRAILING-WHITESPACE-START  is the point preceding the single whitespace marker.
TRAILING-WHITESPACE-END    is the point following the single whitespace marker.
DEFAULT-VALUE-START        is the point preceding the default text.
DEFAULT-VALUE-END          is the point following the default text."

  (let ((initial-newline-props     (cdr (assq 'initial-newline     property-alist)))
	(field-text-props          (cdr (assq 'field-text          property-alist)))
	(trailing-whitespace-props (cdr (assq 'trailing-whitespace property-alist)))
	(default-value-props       (cdr (assq 'default-value       property-alist))))

    (add-text-properties initial-newline-start     initial-newline-end     initial-newline-props)
    (add-text-properties field-text-start          field-text-end          field-text-props)
    (add-text-properties trailing-whitespace-start trailing-whitespace-end trailing-whitespace-props)
    (add-text-properties default-value-start       default-value-end       default-value-props)))

;;Build Patch templates.
(magik-template-file-type-alist-add "Patch"
				    'magik-patch-file-number
				    magik-patch-file-type-templates-default)
(magik-template-file-type-alist-add "Test"
				    'magik-patch-file-number
				    magik-patch-file-type-test-template)

;;;;;;;;;;;;;;;;;;
;;Magik Patch interface
;;;;;;;;;;;;;;;;;;

(defun magik-patch-make (number)
  "Creates/fetches a Magik patch file.
Creates a new Magik patch file for patch NUMBER (a string, eg 1234_1) or jumps to the patch if it already exists.
If run interactively from within a bug-report, defaults to the highest version extant patch
with that number.  If we are in a bug-report, the TOPIC is deduced.

Unless a prefix arg is given, any patch residing under
`mpde-env-rejected-patches-dir' will not be displayed.
"
  (interactive (list (magik-patch-make-interactive-info 'patch)))
  (setq this-command 'magik-patch-make)
  (magik-patch-make-internal number 'patch))
(defalias 'make-magik-patch 'magik-patch-make)

(defun magik-patch-make-change-note (number)
  "Creates/fetches a Change Note file.
Creates a change note for change note NUMBER (a string, eg 1234_1) or jumps to the patch if it already exists.
If run interactively from within a bug-report, defaults to the highest version extant patch
with that number.  If we are in a bug-report, the TOPIC is deduced.

Unless a prefix arg is given, any patch residing under
`mpde-env-rejected-patches-dir' will not be displayed.
"
  (interactive (list (magik-patch-make-interactive-info 'change)))
  (setq this-command 'magik-patch-make-change-note)
  (magik-patch-make-internal number 'change))
(defalias 'make-change-note 'magik-patch-make-change-note)

(defun magik-patch-make-unit-test (number)
  "Creates/fetches a Unit Test file.
Creates a Magik Unit Test for test NUMBER (a string, eg 1234_1) or jumps to the patch if it already exists.
If run interactively from within a bug-report, defaults to the highest version extant patch
with that number.  If we are in a bug-report, the TOPIC is deduced.

Unless a prefix arg is given, any patch residing under
`mpde-env-rejected-patches-dir' will not be displayed.
"
  (interactive (list (magik-patch-make-interactive-info 'test)))
  (setq this-command 'magik-patch-make-unit-test)
  (magik-patch-make-internal number 'test))
(defalias 'make-unit-test 'magik-patch-make-unit-test)

(defun magik-patch-view (number)
  "View the given Magik Patch or Change Note.
Designed to be used from the Emacs command line:
   emacs -execute '(magik-patch-view \"999999_9\")'
or
   gnudoit '(magik-patch-view \"999999_9\")'

The patch is displayed using `view-mode'.

If the supplied patch number does not exist then an error is thrown.

Side-effect: sets `inhibit-startup-message' to t, since this function's
main use is being called as an argument to emacs or gnudoit.

Unless a prefix arg is given, any patch residing under
`mpde-env-rejected-patches-dir' will not be displayed.
"
  (interactive (list (magik-patch-make-interactive-info 'patch)))

  ;; Command-line use is not called interactively so we explicitly refresh the required caches.
  (setq inhibit-startup-message t
	this-command 'magik-patch-view)
  (magik-patch-cache-refresh-patch-options)
  (magik-patch-cache-refresh-patchdirs)
  (if (magik-patch-make-internal number 'patch t)
      (view-mode-enter)
    (error resources-patch-not-a-patch-file-error)))

(defun magik-patch-rename (new-name)
  "Renames the patch file in the current buffer to NEW-NAME.
All patch number references in the buffer are changed to reflect the
new new-name, NEW-NAME. If the current buffer is visiting a file then the user is
offered the opportunity to delete the old patch.

The NEW-NAME is checked to see if it is the right format to match the variable,
magik-patch-number-regexp..

NEW-NAME is also compared with the list of patches known to Emacs.
If NEW-NAME causes a partial or complete match to this list then the function
aborts. As a guide, if you can press TAB after your NEW-NAME and completion
reports [No match] then you will be able to successfully rename the patch.

If you really must, you may avoid this list checking by using a prefix argument i.e
    C-u M-x magik-patch-rename
but note that this check is intended to avoid the creation of multiple patch files with
the same name."
  (interactive (let ((current-patch (or (and magik-template-file-type (magik-patch-file-number))
					(error resources-patch-not-a-patch-file-error))))
		 (list (magik-patch-make-interactive-info
			nil
			(concat (format resources-patch-rename-prompt current-patch) " ")
			current-patch))))
  
  (if (not (string-match magik-patch-number-regexp new-name))
      (error resources-patch-number-error new-name))
  ;;prevent rename to a patch that exists.
  ;;This also prevents you creating a patch _1 when a _1a already exists which is probably a good thing
  (or current-prefix-arg
      (not (all-completions new-name magik-patch-cache-allpatches nil))
      (error resources-patch-already-exists-error new-name))
  (save-excursion
    (save-match-data
      (let ((current-name (magik-patch-file-number))
	    current-patch-number
	    current-part-number
	    new-patch-number
	    new-part-number
	    (inhibit-read-only t)
	    (inhibit-point-motion-hooks t))
	(string-match magik-patch-number-regexp current-name)
	(setq current-patch-number (match-string 1 current-name))
	(setq current-part-number  (match-string 2 current-name))
	(string-match magik-patch-number-regexp new-name)
	(setq new-patch-number (match-string 1 new-name))
	(setq new-part-number  (match-string 2 new-name))
	
	;;First replace occurences of full patch name
	(global-replace-regexp current-name new-name)
	
	;;Replace lines that have patch number and patch name near each other
	;;e.g declare_patch lines
	(global-replace-regexp (concat current-patch-number ", +\"" current-part-number)
			       (concat new-patch-number     ", \"" new-part-number))
	
	;;Support for old style patches, Change-Number now includes entire patch name.
	(if (save-excursion
	      (goto-char (point-min))
	      (re-search-forward "^#Type:" nil t))
	    nil
	  (global-replace-regexp "#Change-Number:.*" (concat "#Change-Number: " new-name)))
	
	(let* ((old-file (buffer-file-name))
	       (new-file (progn (string-match current-name old-file) (replace-match new-name nil t old-file))))
	  (and old-file
	       (file-exists-p old-file)
	       (yes-or-no-p (concat (format resources-patch-yn-delete-original old-file) " "))
	       (delete-file old-file))
	  (rename-buffer (concat "P" new-name ".magik") t)
	  (setq buffer-file-name new-file)
	  (setq buffer-file-truename (file-truename new-file)))))))
(defalias 'rename-magik-patch 'magik-patch-rename)

(defun magik-patch-convert ()
  "Converts a magik patch file to a change note and vice-versa."
  (interactive)
  (setq this-command 'magik-patch-convert)
  (or magik-template-file-type
      (error resources-patch-not-a-patch-file-error))
  (save-excursion
    (save-match-data
      (let*
	  ((number (magik-patch-file-number))
	   (patch-number (progn (string-match magik-patch-number-regexp number) (match-string 1 number)))
	   (part-number (match-string 2 number))
	   (keyword-regexp "^\\(#[a-zA-Z-]+:\\).*")
	   (start-keyword "^#Fixes-Bug-Numbers:")
	   patch-fields-alist)

	(re-search-forward start-keyword nil t)
	(goto-char (match-beginning 0))
	(while (not (looking-at "^\\s-*$"))
	  (if (looking-at keyword-regexp)
	      (let ((field-name (buffer-substring (match-beginning 1) (match-end 1)))
		    (contents   (buffer-substring (match-end 1) (match-end 0)))
		    (multi-line-field-p (eq (match-end 1) (match-end 0)))
		    (keyword-point (point)))
		(forward-line)
		(if multi-line-field-p
		    ;;Gather up the contents of the multiline field
		    (while (and (not (looking-at "^\\s-*$"))
				(not (looking-at "^#-------[-]*\\(\n#\\s-*\\)?$"))
				(not (looking-at keyword-regexp)))
		      (setq contents (concat contents (buffer-substring (point-bol) (point-eol)) "\n"))
		      (forward-line)))
		(set 'patch-fields-alist (cons (cons field-name contents) patch-fields-alist)))
	    (forward-line)))
      
	(setq patch-fields-alist (reverse patch-fields-alist))

	(set-buffer (magik-patch-make-internal
		     (concat patch-number "_99z")
		     (if (eq magik-template-file-type 'patch)
			 'change
		       'patch)))
	(save-excursion
	  (goto-char (point-min))
	  (re-search-forward start-keyword nil t)
	  (goto-char (match-beginning 0))
	  (let ((inhibit-read-only t)
		(inhibit-point-motion-hooks t))
	    (set-text-properties (point-min) (point-max) nil)
	    (loop for field in patch-fields-alist
		  for field-name = (car field)
		  for field-contents = (cdr field)
		  if (search-forward field-name nil t)
		  do (let ((start (match-end 0))
			   (end   (point-eol)))
		       (goto-char start)
		       (if (eq start end)
			   (progn
			     ;;multi-line field
			     (forward-line)
			     (delete-region (point-bol) (progn (forward-line) (point-bol))) ; delete the ^# \n line
			     (insert field-contents))
			 ;;single line field
			 (delete-region start end)
			 (insert field-contents)))))
	  (magik-patch-mode t)
	  (let ((current-prefix-arg t))
	    (magik-patch-rename number)))))))
(defalias 'convert-magik-patch 'magik-patch-convert)

(defun magik-patch-make-interactive-info (template &optional prompt default)
  "Reads the patch number from the user.  Returns the patch-number as a string, e.g. `1234_1a'.
The TEMPLATE is used to determine the type of template file to use and is part of the prompt.
Implements patch-number completion based on the contents of all the patch directories.
Offers a default based on the current bug number (if in a bug-report) or
the current patch number (if in the magik_patch_submissions file). Note that in the
case of duplicate patches in different directories, a prompter is brought up for
the directory"
  (magik-patch-cache-refresh-patch-options)
  (magik-patch-cache-refresh-templates)
  (magik-patch-cache-refresh-patchdirs)
  ;;(magik-patch-cache-refresh-topic-data) ;;Not required at this stage
  (let ((name (completing-read (or prompt (concat (capitalize (symbol-name template)) " number: "))
			       'magik-patch-filter-completion
			       nil
			       nil
			       (or default (magik-patch-default-number))
			       'magik-bug-history))
	dirs)
    
    (if (<= (count (list name) magik-patch-cache-allpatches :test 'equal) 1)
	name
      (setq dirs 
	    (loop for (dir timestamp numbers) in magik-patch-cache-patchdirs
		  if (assoc name numbers) 
		  collect (list dir) into dirs
		  finally return (delete-duplicates dirs :test 'equal)))
      (list name
	    (completing-read (concat resources-patch-select-directory-prompt " ")
			     dirs
			     nil
			     t
			     (try-completion "" dirs))))))

(defun magik-patch-filter-completion (string pred flag)
  "Used to minimise duplicates when prompting for patches - we can't
   ensure magik-patch-cache-allpatches is duplicate-free, as it's too big to clean,
   but if we're only prompting with a few strings we can remove duplicates
   gumbily"
  (let ((completions (all-completions string magik-patch-cache-allpatches pred)))
    (if (<= (length completions) 64)
	(progn
	  (setq completions (remove-duplicates completions :test 'equal))
	  (if (not flag)
	      (setq completions (mapcar 'list completions))))
	  
      (if (not flag)	
	  (setq completions magik-patch-cache-allpatches)))
    
    (if flag	   ; nil means try-completion, t means all-completions
	completions
      (try-completion string completions pred))))

(defun magik-patch-default-number ()
  "Returns a suitable value for the default patch number.
This is based upon either the current word that point is on
or the current word at the beginning of the current line
or the buffer name of a Bug Report."
  (save-match-data
    (let ((word (current-word))
	  bug
	  patches)
      ;; reduce word to patch number
      (cond ((null word)
	     ;no word nearby must set to string for later use.
	     (setq word ""))
	    ((string-match magik-patch-number-regexp word)
	     (setq word (match-string 0 word)))
	    ((save-excursion
	       (beginning-of-line)
	       (if (and (setq word (current-word))
			(string-match magik-patch-number-regexp word))
		   (setq word (match-string 0 word)))))
	    ((string-match magik-patch-bug-file-regexp (buffer-name))
	     (setq word (match-string 1 (buffer-name))))
	    (t nil))

      ;; now extract bug number from patch number in word (if any).
      (if (and word (string-match magik-patch-bug-number-regexp word))
	  (setq bug (match-string 0 word)))

      (if bug
	  (progn
	    (setq bug (number-to-string (string-to-number bug))	;remove initial zeros
		  patches (or (assoc word magik-patch-cache-allpatches) ;found exact match
			      (all-completions (concat bug "_") ;_ ensures match of full bug number 
					    magik-patch-cache-allpatches)))))

      (cond ((eq (length patches) 1) ; exact match
	     (car patches))
	    (patches                 ; multiple matches return number that user was on
	     word)
	    (bug                     ; bug exists but no patches found default to _1.
	     (concat bug "_1"))
	    (t nil)))))

(defun magik-patch-find-file (dir file)
  "Open a patch file. Open readonly if path is not proposed or rejected.

If file-not-found error then the `file-not-found-hook' is run to
create a new file."
  (let ((path (concat-/ dir file)))
    (cond ((equal dir mpde-env-unsubmitted-patches-dir)
	   (find-file path))
	  ((equal dir mpde-env-proposed-patches-dir)
	   (find-file path))
	  ((equal dir mpde-env-rejected-patches-dir)
	   (if (file-exists-p path)
	       (progn
		 (find-file path)
		 (view-mode-enter)
		 (message resources-patch-view-rejected-patch (buffer-name)))
	     (error resources-patch-no-reject-error path)))
	  ((file-exists-p path)
	   (find-file-read-only path))
	  (t
	   (error resources-patch-no-file-error path)))))

(defun magik-patch-make-internal (number type &optional no-create)
  "Finds an existing patch (or change note) or makes a new patch (or change note) with number, NUMBER.
The number should be a string of the form '12345_1' or '12345_1a'.
Assumes the `magik-patch-cache-patchdirs' are up-to-date.
This should be ok because this function gets called after
magik-patch-make-interactive-info.

Unless a prefix arg is given, any patch residing under
`mpde-env-rejected-patches-dir' will not be displayed.

Function also returns the buffer for the patch."
  (setq-default magik-template-file-type type)
  (let (dir leafname)
    (if (listp number)
	(setq dir (elt number 1)
	      number (elt number 0)))

    (or (string-match (concat "^" magik-patch-number-regexp "$") number)
	(error resources-patch-number-error number))

    (setq leafname (concat "P" number ".magik"))
    (if dir 
	(magik-patch-find-file dir leafname)
      (loop for (dir timestamp numbers) in magik-patch-cache-patchdirs
	    do
	    (if (assoc number numbers)
		(progn
		  (if (and (not current-prefix-arg)
		     (equal dir mpde-env-rejected-patches-dir))
		      ;; Omit rejected-patches unless prefix arg given.
		      (progn
			(message resources-patch-avoid-rejected-patch leafname this-command)
			(return)))
		  (magik-patch-find-file dir leafname)
		  (return)))
	    finally
	    (cond (no-create
		   (setq no-create 'no-patch-exists))
		  ((or (eq mpde-configuration-current 'master)
		       (mpde-master-available-p))
		   (find-file
		    (concat-/ (get 'mpde-env-proposed-patches-dir 'master)
			      leafname)))
		  (t
		   (find-file (concat-/ mpde-env-unsubmitted-patches-dir leafname)))))))
  (setq-default magik-template-file-type nil) ; reset value

  ;; Return patch buffer unless no-create option failed to find one and none was created.
  (if (eq no-create 'no-patch-exists)
      nil
    (current-buffer)))

(defun magik-patch-get-topic ()
  "Returns the topic value in the current buffer, or nil if there isn't a topic."
  (save-excursion
    (goto-char (point-min))
    (save-match-data 
      (cond ((re-search-forward " +Topic.*:[ \t]*\\(.*\\)" nil t)
	     (match-string 1))
	    ((re-search-forward " +Functional Area:[ \t]*\\(.*\\)" nil t)
	     (let ((topic (match-string 1))
		   (subtopic ""))
	       (if (re-search-forward " +Sub-Functional Area:[ \t]*\\(.*\\)" nil t)
		   (setq subtopic (match-string 1)))
	       (cons topic subtopic)))
	    (t
	     nil)))))

(defun magik-patch-initialise-template ()
  "Strips out the preamble, fills in the `who' fields etc. and changes Yes/No etc. to Yes.
Also loads the bug report if it can find one and uses that to prefill fields too."

  (let* ((leafname (if buffer-file-name
		       (file-name-nondirectory buffer-file-name)
		     ""))
	 (patch-number (progn
			 (string-match magik-patch-file-regexp leafname)
			 (match-string 1 leafname)))
	 (bug-number  (match-string 2 leafname))
	 (bug-filename (magik-patch-bug-filename bug-number))
	 (bug-version (concat "\"" (match-string 3 leafname) "\""))
	 (buffer-visiting-bug (find-buffer-visiting bug-filename))
	 magik-patch-topic)

    (save-excursion
      (if (magik-patch-bug-get bug-number)
	  (progn
	    (setq magik-patch-topic (magik-patch-get-topic))
	    (or buffer-visiting-bug (kill-buffer (current-buffer))))))

    (cond ((and patch-number (not magik-patch-mode))
	   (error resources-patch-invalid-template-type-error magik-template-file-type))
	  (magik-patch-mode
	   (global-replace-regexp "/[^'].*" "")
	   
	   (if (or (assoc (file-name-nondirectory bug-filename) magik-patch-cache-bug-list)
		   (file-exists-p bug-filename))
	       ;; Found valid bug number from cache or file.
	       (global-replace-regexp "#Fixes-Bug-Numbers:.*" (concat "#Fixes-Bug-Numbers: " bug-number)))
	   (global-replace-regexp "#Patch-Number:.*"  (concat "#Patch-Number: " patch-number))
	   (global-replace-regexp "#Change-Number:.*" (concat "#Change-Number: " patch-number))
	   (global-replace-regexp "<patch-number>" bug-number)
	   (global-replace-regexp "<patch-version>" bug-version)
	   (global-replace-regexp "#Date:.*" (concat "#Date: " (format-time-string magik-patch-date-format)))
	   (cond ((stringp magik-patch-topic)
		  (global-replace-regexp "#Topic:.*" (concat "#Topic: " magik-patch-topic)))
		 ((consp magik-patch-topic)
		  (global-replace-regexp "#Topic:.*" (concat "#Topic: " (car magik-patch-topic)))
		  (global-replace-regexp "#Sub-Topic:.*" (concat "#Sub-Topic: " (cdr magik-patch-topic))))
		 (t nil))
	   (global-replace-regexp "#Who: " (concat "#Who: " magik-patch-who))
	   
	   (goto-char (point-min))
	   (re-search-forward "#[A-Z][a-zA-Z-]+:\\s-+$" nil t))
	  (t
	   nil))
    (set-buffer-modified-p nil)))

(add-hook 'magik-template-initialise-hook 'magik-patch-initialise-template)

(defun magik-patch-resurrect ()
  "Operating on an existing, approved patch in the current directory, resurrect it 
in its original form in the submissions directory, ready for resubmission to a different stream"
  (interactive)
  (let* ((number (magik-patch-file-number))
	 (new-patch-name (concat "P" number ".magik"))
	 (new-file-name (concat-/ mpde-env-proposed-patches-dir new-patch-name)))
    (if (not number)
	(error resources-patch-not-a-patch-file-error))
    (if (file-exists-p new-file-name)
	(error resources-patch-proposed-already-error))
    
    (magik-patch-cache-refresh-patch-options)
    (magik-patch-cache-refresh-templates)
    (goto-char (point-min)) 
    (cond ((save-excursion
	     (re-search-forward (concat "^#?Change-Number:[ \t]*" number "[ \t]*$") nil t))
	   (magik-patch-resurrect-internal-full number))
	  ((save-excursion
	     (re-search-forward (concat "^#?Patch-Number:[ \t]*" number "[ \t]*$") nil t))
	   (magik-patch-resurrect-internal-partial number))
	  (t
	   (error resources-patch-no-change-number-field-error number)))
    (message resources-patch-help-resurrected)))
(defalias 'resurrect-magik-patch 'magik-patch-resurrect)

(defun magik-patch-resurrect-internal-full (number)
  "Resurrect a patch where all of its fields have been retained by the approval process.
The old approval process stripped off the second section of fields from the patch file,
placing them in the CHANGES file." 
  (interactive)
  (let* ((new-patch-name (concat "P" number ".magik"))
	 (current-buff (current-buffer))
	 (current-buffer-name (buffer-name current-buff))
	 (inhibit-read-only t)
	 (inhibit-point-motion-hooks t)
	 (new-file-name (concat-/ mpde-env-proposed-patches-dir new-patch-name))
	 (new-patch-buf (generate-new-buffer new-patch-name)))

    (set-buffer new-patch-buf)
    (insert-buffer-substring current-buff)
    (magik-patch-resurrect-internal-reset-fields)
    (write-file new-file-name)
    (kill-buffer current-buff)
    (switch-to-buffer new-patch-buf)
    (rename-buffer current-buffer-name)
    (magik-mode)
    (magik-patch-mode t)))

(defun magik-patch-resurrect-internal-partial (number)
  "Resurrect a patch where some of its fields are in the patch and the rest are in CHANGES file.
The old approval process stripped off the second section of fields from the patch file,
placing them in the CHANGES file."
  (interactive)
  (let* ((new-patch-name (concat "P" number ".magik"))
	 (current-buff (current-buffer))
	 (current-buffer-name (buffer-name current-buff))
	 (working-buf (get-buffer-create " *temp changes buf*"))
	 (inhibit-read-only t)
	 (inhibit-point-motion-hooks t)
	 (new-file-name (concat-/ mpde-env-proposed-patches-dir new-patch-name))
	 new-patch-buf changes-buf first-pos second-pos)

    (setq changes-buf (find-file-noselect "CHANGES"))
    (setq new-patch-buf (generate-new-buffer new-patch-name))
    (set-buffer changes-buf)

    (goto-char (point-min)) 
    (re-search-forward (concat "^Patch-Number:[ \t]*" number "[ \t]*$"))
    (re-search-forward "^Risk:[ \t]*\\w+[ \t]*$")
    (beginning-of-line)
    (setq first-pos (point))
    (setq second-pos (re-search-forward "^-+$"))

    (set-buffer working-buf)
    (erase-buffer)
    (insert-buffer-substring changes-buf first-pos second-pos)
    (goto-char (point-min))
    (replace-regexp "^\\(.*\\)$" "#\\1") 
    (newline)

    (set-buffer new-patch-buf)
    (insert-buffer-substring current-buff)
    (goto-char (point-min))
    (re-search-forward "^#-+\n")
    (insert-buffer-substring working-buf)
    (magik-patch-resurrect-internal-reset-fields)
    (write-file new-file-name)
    (kill-buffer current-buff)
    (switch-to-buffer new-patch-buf)
    (rename-buffer current-buffer-name)
    (magik-mode)
    (magik-patch-mode t)))

(defun magik-patch-resurrect-internal-reset-fields ()
  "Reset fields of an approved patch back to their default values ready for resubmission."
  (goto-char (point-min))
  (replace-regexp "^#Who:.*$" (concat "#Who: " magik-patch-who))
  (goto-char (point-min))
  (replace-regexp "^#Patch-Intended-For-Release:.*$" "#Patch-Intended-For-Release: ")
  (goto-char (point-min))
  (replace-regexp "^#Approved-By:.*$" "#Approved-By: <leave-blank>")
  (goto-char (point-min))
  (replace-regexp "^#Approved-Date:.*$" "#Approved-Date: <leave-blank>")
  (goto-char (point-min))
  (replace-regexp "^#TT-Checked-By:.*$" "#TT-Checked-By: <leave-blank-for-TT>")
  (goto-char (point-min))
  (replace-regexp "^#Checked-By:.*$" "#Checked-By: <leave-blank>")
  (goto-char (point-min))
  (replace-regexp "^#Tested-By:.*$" "#Tested-By: <leave-blank>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;      5.  P A T C H   P R O C E S S
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; See actual configuration of this function and data in site-start.el
(defvar magik-patch-process-parser-function 'ignore
  "Function to parse the output from a command given by `magik-patch-process-command-alist`")

(defvar magik-patch-process-comment-function 'magik-patch-process-comment
  "Function to process a comment string`")

(defvar magik-patch-process-command-alist nil
  "Association list of command arguments for running the magik-patch processes.")

(defun magik-patch-process-comment (comment)
  "Parse COMMENT, modifying it if necessary to escape any special characters.
Called via `magik-patch-process-comment-function'."
  ;; By default, return empty string if no comment string given.
  (or comment ""))

(defun magik-patch-process-command (action args)
  "Return string ACTION with ARGS as ':property value' arguments."
  (let ((command (cdr (assoc action magik-patch-process-command-alist)))
	arg key value)
    (if (null command)
	(error resources-patch-action-no-command-error action))
    (while args
      (setq key (upcase (substring (symbol-name (car args)) 1)) ; strip of leading : in symbol name
	    value (cadr args)
	    args (cddr args))
      (if (null value)
	  (error resources-patch-action-no-value-error action key))
      (while (string-match (concat "{" key  "}") command)
	  (setq command (replace-match value t t command))))
    command))

(defun magik-patch-process (action &rest args)
  "Start a magik patch process to run the ACTION with :property style arguments ARGS."
  (let* ((buffer-name (concat "*patch-process-" (symbol-name action) "*"))
	 (buffer  (get-buffer-create buffer-name))
	 (command (magik-patch-process-command action args))
	 (revert-without-query '(".*"))
	 exitcode)
    (save-excursion
      (set-buffer buffer)
      (erase-buffer)
      (message resources-patch-action-start action buffer-name))
    (setq exitcode (call-process shell-file-name nil buffer t
				 shell-command-switch command))
    (revert-buffer)
    (if (and (funcall magik-patch-process-parser-function buffer)
	     (zerop exitcode))
	(message resources-patch-action-complete action buffer-name)
      (save-excursion
	(set-buffer buffer)
	(goto-char (point-max))
	(insert "\n" command))
      (message resources-patch-action-failed action buffer-name exitcode)
      (display-buffer buffer-name)
      nil)))

(defun magik-patch-generic-validation (action ignore-mpde-check)
  "Perform a simple validation check on the buffer to ensure consistency.

This validation is a generic validation procedure, specific checks
are done when a certain action is performed, review, approve etc.

If ignore-mpde-check is t then the development environment is not checked."

  ;;Check number of separation lines
  (let ((nseparators 0))
    (goto-char (point-min))
    (while (re-search-forward "^#-------[-]*$" nil t)
      (setq nseparators (1+ nseparators)))
    
    (or (eq nseparators 2)
	(eq nseparators 1)
	(message resources-patch-warn-separator-lines (number-to-string nseparators))))
  (or ignore-mpde-check
      (mpde-master-available-p)
      (error resources-mpde-failed-validation-error
	     action (symbol-name mpde-configuration-current))))

(defun magik-patch-check-in-submissions-file (regexp)
  "Search for regexp in file specified in `mpde-env-magik-patch-submissions-file'"
  (save-excursion
    (save-match-data
      (let ((buffer (get-buffer-create " *magik_patch_submissions*")))
	(set-buffer buffer)
	(insert-file-contents mpde-env-magik-patch-submissions-file nil nil nil 'replace)
	(goto-char (point-max))
	(re-search-backward regexp nil t)))))

(defun magik-patch-make-summary-line (summary)
  "Convert given multi-line summary string with # prefixes to a single line."

  ;;First join multiple lines together
  (while (string-match "\n" summary)
    (setq summary (replace-match " " nil t summary 0)))

  ;;remove # and whitespace replacing with single space.
  (while (string-match "#\\s-+" summary)
    (setq summary (replace-match " " nil t summary 0)))

  ;;replace " with ' because the resulting string will be delimited with "".
  (while (string-match "\"" summary)
    (setq summary (replace-match "'" nil t summary 0)))

  ;;remove initial whitespace
  (string-match "^\\s-*" summary)
  (setq summary (replace-match "" nil t summary 0))

  ;;remove trailing whitespace
  (string-match "\\s-*$" summary)
  (setq summary (replace-match "" nil t summary 0))

  ;;return the resulting string.
  summary)

(defun magik-patch-append-to-file (start end filename)
  "Append the region between START and END (or just the string START)
 to filename, clearing the file-modified time if the file is in a buffer
to prevent witterage."
  (let ((buffer (get-file-buffer filename))
        (string (if (stringp start)
                    start
                  (buffer-substring start end))))
    (if buffer
        (save-excursion
          (set-buffer buffer)
          (if (buffer-modified-p (current-buffer))
              (if (y-or-n-p (format (concat resources-sw-yn-buffer-modified " ") filename))
                  (save-buffer)
                (error resources-sw-buffer-modified-error)))
          (clear-visited-file-modtime)
          (append-to-file string nil filename)
          (revert-buffer t t))
      (append-to-file string nil filename))))

(defun magik-patch-update-magik-patch-submissions (patch intended-version-str who reviewer tt)
  "Add a line to the mpde-env-magik-patch-submissions-file."

  (magik-patch-append-to-file
   (format "%-15s %-16s %-8s %s  %s%s\n"
	   patch
	   intended-version-str
	   who
	   (format-time-string magik-patch-date-format)
	   (if reviewer (concat "." reviewer "*") ".unknown*")
	   (if tt " TT*" ""))
   nil mpde-env-magik-patch-submissions-file))

;;;TODO: Currently hacked magik-patch-submit to accomodate Type Test - Will need complete rewrite...
(defun magik-patch-submit ()
  "Record the current patch as formally submitted.
The patch contents are checked before a submission is performed.
If a patch has already been submitted, then it is not submitted again.

The patch contents check code is performed before checking whether the
patch has been submitted so you can use \\[submit-magik-patch] to
check the validity of an already submitted patch if you wish.

If an error is emitted indicating the Patch is invalid but you need
override this protection and submit anyway, you may use a prefix
argument, C-u \\[magik-patch-submit]. You will then be asked to
continue after each error found."
  (interactive)
  ;;TODO MPDE allow submission whilst offline so that at next refresh patch submissions are uploaded. 

  (or magik-patch-mode
      (and (eq major-mode 'magik-mode)
	   (magik-patch-file-number)
	   (y-or-n-p (concat resources-patch-yn-enable-minor-mode ". "
			     resources-sw-yn-continue " "))
	   (magik-patch-mode 1))
      (error resources-patch-not-a-patch-file-error))

  (magik-patch-cache-refresh-patch-options)
  (magik-patch-cache-refresh-templates)
  (magik-patch-cache-refresh-topic-data)

  ;;Ensure buffer is saved first. This is so that any subsequent buffer changes by the
  ;;submission process can be done silently.
  (if (buffer-modified-p (current-buffer))
      (if (y-or-n-p (concat resources-patch-yn-save-changes " "))
	  (save-buffer)
	(error resources-patch-not-submitted-error (current-buffer))))

  (let*
      ((number (magik-patch-file-number)) ;patch number according to file name
       (change-note-p (eq magik-template-file-type 'change))
       ;;patch number according to #Change-Number string in file (or #Patch-Number: for compatibility
       (part-number (progn (string-match "_\\([0-9]+[a-z]?\\)" number) (match-string 1 number)))
       (bug-number  (progn (string-match "\\([0-9]+\\)_" number) (match-string 1 number)))
       (type-name (or (magik-patch-field-get-single "Type" t)
		      (if change-note-p "Change Note" "Patch")))
       (patch-proc-re (concat bug-number "\\s-*,\\s-*\"" part-number "\""))
       (patch-number  (or (magik-patch-field-get-single "Patch-Number" t)
		          (magik-patch-field-get-single "Change-Number")))
       (who (magik-patch-field-get-single "Who"))
       (summary-line (if (magik-patch-field-get-single "Summary" t) (magik-patch-summary-line)))
       (defect-origin (magik-patch-field-get-single "Defect-Origin" (eq magik-template-file-type 'test)))
       (documentation-impact (magik-patch-field-get-single "Documentation-Impact" (eq magik-template-file-type 'test)))
       (ux-impact (magik-patch-field-get-single "UX-Impact" t))
       (checked-by (magik-patch-field-get-single "Checked-By"))
       (tested-by  (magik-patch-field-get-single "Tested-By" (eq magik-template-file-type 'test) t)) ;;OLD format had no trailing space.
       (testing-comments (magik-patch-field-get-multi "Testing-Comments"))
       (stream-alist (magik-patch-target-alist)) ; ensure Patch-Intended-For-Release is valid.
       (major-stream-data (nth 1 stream-alist))
       topic
       subtopic
       reviewer)

    (or number
        (error resources-patch-not-a-patch-file-error))
    (or (equal number patch-number)
	(error resources-patch-name-file-mismatch-error type-name patch-number number))

    (and (equal who "")
	 (error resources-patch-no-author))

    ;;Check Topic and Sub-Topic values are valid for the 'major' stream providing User
    ;;given us a valid 'major' stream, ie. major-stream-data is not nil.
    ;; If major-stream-data is nil then we proceed without error since User has asked us to continue already.
    (if (and (not (eq magik-template-file-type 'test)) major-stream-data)
	(condition-case err
	    (progn
	      (setq reviewer (car (magik-patch-data-alist (list "Stream" "Topic" "Sub-Topic" "Reviewers")
							  magik-patch-topic-data
							  (car major-stream-data) ;;stream name
							  (magik-patch-field-get-single "Topic")
							  (magik-patch-field-get-single "Sub-Topic")
							  "Reviewers")))
	      (setq reviewer
		    (if (and reviewer
			     (equal (upcase (car reviewer)) (upcase who)))
			(cadr reviewer)	;;Choose alternative reviewer if default same as author
		      (car reviewer))))
	  (error
	   (or (and current-prefix-arg
		    (y-or-n-p (concat (error-message-string err) ". "
				      resources-sw-yn-continue " ")))
	       (error (error-message-string err))))))

    ;;#Defect-Origin field is mandatory ONLY if #Fixes-Bug-Numbers field is set to
    ;;a bug that is of Type: Bug. 'Bugs' that are Suggestions (enhancements) are not required to
    ;;have the #Defect-Origin field set.
    (if defect-origin
	(let ((mandatory-p (save-match-data (string-match "<mandatory[^>\n]*>" defect-origin)))
	      (defect-list (split-string (magik-patch-field-get-single "Fixes-Bug-Numbers")))
	      (options (cdr (assoc "Defect-Origin" magik-patch-toggles))))
	  (while defect-list
	    (and (magik-patch-bug-is-defect-p (car defect-list)) ; side-effect checks bug exists
		 mandatory-p
		 (error (concat resources-patch-fill-in-field-error " " resources-patch-not-submitted-error) "Defect-Origin" type-name))
	    (setq defect-list (cdr defect-list)))
	  (or mandatory-p
	      (member defect-origin options)
	      (error (concat resources-patch-fill-in-field-error " " "\"%s\" != %S") "Defect-Origin" defect-origin options))))

    (if documentation-impact
	(or (string-match "Yes\\|Maybe\\|No" documentation-impact)
	    (error (concat resources-patch-fill-in-field-error " " resources-patch-not-submitted-error) "Documentation-Impact" type-name)))
    
    (if ux-impact
	(or (string-match "Yes\\|No" ux-impact)
	    (error (concat resources-patch-fill-in-field-error " " resources-patch-not-submitted-error) "UX-Impact" type-name)))

    (cond ((equal checked-by "<leave-blank>")
	   t)
	  ((equal checked-by "")
	   (let ((inhibit-read-only t)
		 (inhibit-point-motion-hooks t))
	     (save-excursion
	       (goto-char (point-min))
	       (search-forward "#Checked-By: ")
	       (insert "<leave-blank>")	;;Ensure <leave-blank> is inserted.
	       (save-buffer))))
	  (t
	   (error (concat resources-patch-not-fill-in-field-error " " resources-patch-not-submitted-error) "Checked-By" type-name)))

    (if (and tested-by (equal tested-by ""))
	(error (concat resources-patch-fill-in-field-error " " resources-patch-not-submitted-error) "Tested-By" type-name))

    (if testing-comments
	(let ((string testing-comments))
	  ;;Currently testing-comments only includes text up to the first empty line.
	  ;;this should be sufficient for this test
	  (while (string-match "\n\\|\\s-+" string)
	    (setq string (concat (substring string 0 (match-beginning 0)) (substring string (match-end 0)))))
	  (if (or (equal testing-comments "")
		  (string-match "<mandatory>" testing-comments))
	      (error (concat resources-patch-fill-in-mandatory-field-error " " resources-patch-not-submitted-error) "Testing-Comments" type-name))))
    
    (goto-char (point-min))
    (if (re-search-forward "^\\s-*sw!patch_software.*[<>]" nil t)
	(error resources-patch-software-line-invalid-error))

    (goto-char (point-min))
    (if (re-search-forward "^\\s-*sw!patch_requires\\s-*(\\(.*\\)" nil t)
	(let ((proc-line (match-string 1)))
	  (if (not (string-match patch-proc-re proc-line))
	      (error resources-patch-requires-mismatch-error number))))

    (goto-char (point-min))
    (if (re-search-forward "^\\s-*sw!patch_requires.*[<>]" nil t)
	(error resources-patch-requires-line-invalid-error))

    (goto-char (point-min))
    (if (re-search-forward "^\\s-*sw!declare_patch\\s-*(\\(.*\\)\\()\\s-*\n#?\\s-*\\$\\)" nil t)
	(let ((proc-line (match-string 1)))
	  (if (save-match-data (not (string-match patch-proc-re proc-line)))
	      (error resources-patch-declare-mismatch-error number))
	  (and change-note-p ;; ensure declare_patch has a final $
	       (save-match-data (not (string-match ":change" proc-line)))
	       (replace-match ", :change_note)\n$\n" nil t nil 2))))
    

    ;;Ensure sw!declare_patch line has an uncommented $ on the next line.
    (goto-char (point-min))
    (if (re-search-forward "^\\s-*sw!declare_patch\\s-*(.*)\\s-*\n\\(#\\s-*\\)\\$" nil t)
	(replace-match "" nil t nil 1))

    (goto-char (point-min))
    (if (search-forward "<\"One line description\">" nil t)
	(if summary-line
	    (replace-match (concat "\"" summary-line "\"") nil t nil 0)
	  (error resources-patch-declare-invalid-error "sw!declare_patch" type-name)))

    (magik-patch-generic-validation 'submit nil)
    (save-buffer 0)

    ;;validations for submission
    (if buffer-file-name
	(or (file-exists-p buffer-file-name)
	    (error resources-patch-not-edited-error number)))
    
    ;;Put check for submitted patch last so that you can use submit-magik-patch to check the
    ;;validity of the file contents on a submitted patch
    (if (magik-patch-check-in-submissions-file (concat "^P" number "\\.magik"))
	(error resources-patch-submitted-already-error number))
    
    (magik-patch-process 'submit
			 :author (capitalize who)
			 :number number
			 :actiontype "perform")))
(defalias 'submit-magik-patch 'magik-patch-submit)

(defun magik-patch-unsubmit ()
  "Remove a patch from the patch process by unsubmitting it from the record."
  (interactive)
  (let*
      ((number (magik-patch-file-number)) ;patch number according to file name
       (who (magik-patch-field-get-single "Who")))
    (or magik-patch-mode
	(and (eq major-mode 'magik-mode)
	     number)
	(error resources-patch-not-a-patch-file-error))

    (magik-patch-process 'unsubmit
			 :author (capitalize who)
			 :number number
			 :actiontype "perform")))

(defun magik-patch-approve ()
  "Move the current patch into the approved patch directory"
  (interactive)
  (if (not (magik-patch-file-number))
      (error resources-patch-not-a-patch-file-error))

  (let*
      ((number (magik-patch-file-number))
       (author (capitalize (magik-patch-field-get-single "Who")))
       (file-buffer (current-buffer)))

    (if (magik-patch-process 'approve
			     :author author
			     :approver magik-patch-who
			     :number number
			     :actiontype "perform")
	(kill-buffer file-buffer))))
(defalias 'approve-magik-patch 'magik-patch-approve)

(defun magik-patch-reject (comment)
  "Formally reject a patch permanently, requesting a brief message to say why"
  (interactive "sReason/comment: ")
  (if (not (magik-patch-file-number))
      (error resources-patch-not-a-patch-file-error))

  (let*
      ((number (magik-patch-file-number))
       (comment (funcall magik-patch-process-comment-function comment))
       (file-buffer (current-buffer)))

    (if (magik-patch-process 'reject
			     :rejector magik-patch-who
			     :number number
			     :comment comment
			     :actiontype "perform")
	(kill-buffer file-buffer))))
(defalias 'reject-magik-patch 'magik-patch-reject)

(defun magik-patch-review ()
  "Marks the specified patch as Reviewed."
  (interactive)
  (let
      ((number (magik-patch-file-number)))
    (if (not number)
	(error resources-patch-not-a-patch-file-error))

    (magik-patch-generic-validation 'review nil)

    (magik-patch-process 'review
			 :number number
			 :reviewer magik-patch-who
			 :actiontype "perform")))
(defalias 'mark-as-reviewed 'magik-patch-review)

(defun magik-patch-query (comment)
  "Marks the current Patch with a query status and COMMENT."
  (interactive "sReason/comment: ")
  (let
      ((number (magik-patch-file-number)))
    (if (not number)
	(error resources-patch-not-a-patch-file-error))
    (setq comment (funcall magik-patch-process-comment-function comment))

    (magik-patch-generic-validation 'query nil)
    
    (magik-patch-process 'query
			 :number number
			 :reviewer magik-patch-who
			 :comment comment
			 :actiontype "perform")))
(defalias 'query-patch       'magik-patch-query)
(defalias 'magik-patch-defer 'magik-patch-query)
(defalias 'defer-magik-patch 'magik-patch-query)

(defun magik-patch-unreview ()
  "Remove a Review or Query status from the current Patch."
  (interactive)
  (if (not (magik-patch-file-number))
      (error resources-patch-not-a-patch-file-error))

  (let*
      ((number (magik-patch-file-number))
       (author (capitalize (magik-patch-field-get-single "Who"))))

    (magik-patch-process 'unreview
			 :author author
			 :reviewer magik-patch-who
			 :number number
			 :actiontype "perform")))

(defun magik-patch-tt-query (comment)
  "Marks the current Patch with a TTQuery status and COMMENT."
  (interactive "sReason/comment: ")
  (let
      ((number (magik-patch-file-number)))
    (if (not number)
	(error resources-patch-not-a-patch-file-error))
    (setq comment (funcall magik-patch-process-comment-function comment))

    (magik-patch-generic-validation 'tt-query nil)
    
    (magik-patch-process 'tt-query
			 :number number
			 :ttreviewer magik-patch-who
			 :comment comment
			 :actiontype "perform")))

(defun magik-patch-tt-review ()
  "Marks the current Patch with a TTReview status."
  (interactive)
  (let
      ((number (magik-patch-file-number)))
    (if (not number)
	(error resources-patch-not-a-patch-file-error))

    (magik-patch-generic-validation 'tt-review nil)
    
    (magik-patch-process 'tt-review
			 :number number
			 :ttreviewer magik-patch-who
			 :actiontype "perform")))

(defun magik-patch-tt-unreview ()
  "Removes the TTQuery or TTReview status of the current Patch."
  (interactive)
  (let
      ((number (magik-patch-file-number)))
    (if (not number)
	(error resources-patch-not-a-patch-file-error))

    (magik-patch-generic-validation 'tt-unreview nil)
    
    (magik-patch-process 'tt-unreview
			 :number number
			 :ttreviewer magik-patch-who
			 :actiontype "perform")))

(defun magik-patch-make-public ()
  "Remove the internal patch comment fields from a patch for release to customers"
  (interactive)
  (if (not (magik-patch-file-number))
      (error resources-patch-not-a-patch-file-error))

  (magik-patch-cache-refresh-patch-options)
  (magik-patch-cache-refresh-templates)
  (save-excursion
    (let*
	((inhibit-read-only t)
	 (orig-buf (current-buffer))
	 (filename (or buffer-file-name ""))
	 (orig-file (concat filename ".~original~"))
	 (leafname (file-name-nondirectory filename))
	 ;; Comment out checks for valid streams so that we can operate on "rebadged" streams
	 ;; (intended-version-str  (car (magik-patch-intended-version-str-and-list)))
	 ;; (intended-version-list (cdr (magik-patch-intended-version-str-and-list)))
	 (approved-by-p (save-excursion (goto-char (point-min)) (re-search-forward "^#Approved-By:" nil t)))
	 (first-pos (progn (goto-char (point-min)) (re-search-forward "^#------+#?\n" nil t)
			   (match-end 0))) ;;So that we keep a separator line
	 (second-pos (progn (goto-char first-pos) (forward-line 1)
			    (and (re-search-forward "^#------+#?\n" nil t)
				 (match-end 0))))
	 (buffer-read-only nil))
      (if (buffer-modified-p (current-buffer))
	  (if (y-or-n-p (concat resources-patch-yn-save-changes " "))
	      (save-buffer)
	    (error resources-patch-not-approved-error (current-buffer))))

      (if (string-match (get 'mpde-env-proposed-patches-dir 'master) buffer-file-name)
	  (error resources-patch-proposed-already-error))
      (if approved-by-p
	  (if (progn
		(goto-char (point-min))
		(not (re-search-forward "^#Approved-By: \\w+\n" nil t)))
	      (error resources-patch-fill-in-field-approve-error "Approved-By" leafname)))

      (if (and first-pos second-pos)
	  (progn
	    (if (file-exists-p orig-file)
		(if (y-or-n-p (concat (format resources-patch-yn-overwrite orig-file) " "))
		    (progn ;;Need to delete it since file permissions ay prevent overwrite
		      (delete-file orig-file)
		      (rename-file filename orig-file t)))
	      (rename-file filename orig-file t))
	    (kill-region first-pos second-pos)))
      (if (buffer-modified-p) (save-buffer)))))

(defun magik-patch-make-public-directory (dir)
  "Make all patches in a given directory public by removing internal fields.
If a buffer is already open on one of the files in the directory then that buffer
will be modified but will not be closed."
  (interactive "DDirectory: ")
  (let ((patch-list (directory-files dir t magik-patch-file-regexp t)))
    (while patch-list
      (let* ((file (car patch-list))
	     (buffer (find-buffer-visiting file))
	     (buffer-exists-p buffer))
	(setq patch-list (cdr patch-list)
	      buffer (or buffer (find-file-noselect file t)))
	(save-excursion
	  (set-buffer buffer)
	  (condition-case err
	      (progn
		(message resources-patch-processing (buffer-name))
		(magik-patch-make-public))
	    (error
	     (or (y-or-n-p (concat (file-name-nondirectory file) ": "
				   (error-message-string err) ". "
				   resources-sw-yn-continue " "))
		 (setq patch-list nil)) ;"n" terminates the loop
	     ))
	  (or buffer-exists-p (kill-buffer buffer)))))
    (message resources-patch-processing-done)))


(defun magik-patch-target-alist (&optional noerror)
  "Returns an alist for the target entry.
The first item has a key 'Patch-Intended-For-Release' as its value is the string value for the field.
The remaining entrys are (STREAM . PATCHDIR) in the order they appear in the string value.
i.e. the second entry is always the 'major' stream.

If NOERROR is t then no error checking is performed.
If NOERROR is 'prompt then user is asked to proceed even though errors exist."

  (let* ((field "Patch-Intended-For-Release")
	 (value (magik-patch-field-get-single field))
	 (value-list (split-string value " +"))
	 (nstreams (length value-list))
	 (alist (list (cons field value)))
	 err)
    (while value-list
      (push (assoc (car value-list) magik-patch-patchdirs) alist)
      (setq value-list (cdr value-list)))
    (setq alist (reverse alist))
    
    (if (eq noerror t)
	alist				;just return the alist
      (setq err
	    (cond ((equal value "")
		   (format resources-patch-fill-in-field-error field))
		  ((null (nth 1 alist))
		   (format resources-patch-invalid-primary-stream-error field value))
		  ((position nil alist)
		   (format resources-patch-invalid-secondary-stream-error field value))
		  (t
		   nil)))
      (and err
	   (eq noerror 'prompt)
	   (y-or-n-p (concat err " " resources-sw-yn-continue " "))
	   (setq err nil))

      (if err
	  (error err)
	alist))))

(defun magik-patch-intended-version-str-and-list ()
  "Returns the intended versions as a string and as a list (using a cons).
Also raises an error if any of the intended versions aren't in the magik-patch-patchdirs."
  (save-excursion
    (goto-char (point-min))
    (if (not (re-search-forward "Patch-Intended-For-Release:[ \t]*\\([^ \t\n].*\\)" nil t))
        (error resources-patch-fill-in-field-error "Patch-Intended-For-Release"))
    (let*
        ((intended-version-str (match-string 1))
         (intended-version-list (split-string intended-version-str " +")))
      (loop for ver in intended-version-list
            do
            (or (assoc ver magik-patch-patchdirs)
                (error resources-patch-invalid-release-version-error ver)))
      (cons intended-version-str intended-version-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;      6 .  P A R S I N G   A N D   C A C H E   M A I N T E N A N C E
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun magik-patch-cache-update (cache &optional update &rest args)
  "Checks whether CACHE is up-to-date.
Returns nil is CACHE is up-to-date, t if the cache was changed.
If the CACHE is not up-to-date and UPDATE is not nil, then
if UPDATE is a function, then the UPDATE function is called with arguments CACHE and ARGS.
otherwise the cache is set to UPDATE.

A CACHE has the structure: (PATH (TIME) DATA)."
  (let* ((path       (first cache))
	 (cache-time (second cache))
	 (path-time  (sixth (file-attributes path)))) ;last mod time
    (cond ((and (eq (first  cache-time) (first  path-time))
		(eq (second cache-time) (second path-time)))
	   nil)
	  ((functionp update)
	   (setf (second cache) path-time)
	   (apply update cache args)
	   t)	       ; return t to indicate cache was updated.
	  (update
	   (setf (second cache) path-time
		 (third cache) update)
	   t)	       ; return t to indicate cache was updated.
	  (t nil))))

(defun magik-patch-cache-update-patch-dirs (cache)
  "Updates the patch directory CACHE using a call to directory-files."
  (let ((path (first cache))
	(regexp (concat "^[Pp]" magik-patch-number-regexp "\\.magik$")))
    (if (file-exists-p path)
	(progn
	  (message resources-patch-creating-caches path)
	  (setf (third cache)
		(loop for name in (directory-files path nil regexp)
		      collect (list (substring name 1 -6))))))))

(defun magik-patch-cache-compare-buffers (cache-buffer file fn &optional case-fold-search)
  "Compares a 'cache file' buffer with the files current contents.

Compares the buffer CACHE-BUFFER with the contents of file FILE.
If CACHE-BUFFER does not exist it is created with the contents of the file.
If different (or if CACHE-BUFFER does not exist) FN is called.

CASE-FOLD-SEARCH can be set to t to ignore case, by default case checking in on."

  (let ((compare-buffer (get-buffer-create " *cache: compare*"))
	(case-fold-search case-fold-search))
    (save-excursion
      (or (buffer-live-p cache-buffer) (get-buffer-create cache-buffer))
      (set-buffer (get-buffer-create compare-buffer))
      (insert-file-contents file nil nil nil 'replace)

      ;;If first time through then compare-buffer will be empty thuse following test will be false.
      (if (eq (compare-buffer-substrings compare-buffer nil nil cache-buffer nil nil) 0)
	  nil
	;;update cache-buffer with compare-buffer contents.
	;;ensuring function runs in correct buffer.
	(kill-buffer cache-buffer)
	(set-buffer compare-buffer)
	(rename-buffer cache-buffer)
	(funcall fn)))))

(defun magik-patch-cache-refresh-patch-options ()
  "Re-parses the patch_options.txt file if changed from last read.

If there is a parse error, the session will not be updated until the
file has been changed again."
  (magik-patch-cache-compare-buffers " *cache: patch_options.txt*"
				     mpde-env-patch-options-file
				     'magik-patch-cache-parse-patch-options))

(defun magik-patch-cache-refresh-templates ()
  "Re-parses the template files if any of the files have changed since last time.
If there is a parse error, the session will not be updated until
one of the files is changed again."
  (if (mpde-master-available-p)
      (mapcar 'magik-patch-cache-refresh-template magik-template-alist)))

(defun magik-patch-cache-refresh-template (template-data)
  "Re-parses a template file as defined by TEMPLATE-DATA."

  (let ((magik-template-file-type (car template-data)))
    (magik-patch-cache-compare-buffers (format " *cache: template %s*" magik-template-file-type)
				       (magik-template-file magik-template-file-type)
				       'magik-patch-cache-parse-template)))

(defun magik-patch-cache-parse-patch-options ()
  "Parses the patch_options.txt file in the current buffer."
  (save-match-data
    (goto-char (point-min))
    (let (new-patchdirs new-caches new-toggles new-submit)
      (while (re-search-forward "^[ \t]*[^# \t\n]" nil t)
	(backward-char)
	(let ((line-str (buffer-substring (point-bol) (point-eol))))
	  (cond
	   ((looking-at "patchdir")
	    (if (looking-at "patchdir[ \t]+\\(-\\|[-_0-9a-zA-Z]+\\)[ \t]+\\(/[-_0-9a-zA-Z/.]+\\)\\([# \t]\\|$\\)")
		(let* ((name (match-string 1))
		       (path (match-string 2))
		       (dir  (if (and (equal (substring path 0 5) "/view") (boundp 'clearcase-viewroot-drive))
				 (let ((p (subst-char-in-string ?\\ ?/ (clearcase-path-convert path)))
				       (d (symbol-value 'clearcase-viewroot-drive)))
				   (file-truename p) ;;TODO Forces Clearcase to start view (but not mount the vob though)
				   (if (equal (substring p 0 6) "//view") ;convert to drive letter for performance
				       (setq p (concat d (substring p 6))))
				   p)
			       (concat-u-colon path)))
		       (interested-p (or (eq magik-patch-interested-streams t)
					 (member name magik-patch-interested-streams))))
		  (if interested-p
		      (progn
			(or (assoc dir new-caches)
			    (push (list dir '(0 0) nil) new-caches))
			(push (cons name dir) new-patchdirs))))
	      (error "Unexpected patchdir format: %s in file %s" line-str mpde-env-patch-options-file)))
	   ((looking-at "toggle")
	    (if (looking-at "toggle[ \t]+\\([-_0-9a-zA-Z]+\\)[ \t]+\\([-_0-9a-zA-Z/]+\\([ \t]+[-_0-9a-zA-Z/]+\\)+\\)")
		(push (cons (match-string 1) (split-string (match-string 2) " +")) new-toggles)
	      (error "Unexpected toggle format: %s in file %s" line-str mpde-env-patch-options-file)))
	   ((looking-at "submit")
	    (if (looking-at "submit[ \t]+\\([-_0-9a-zA-Z]+\\([ \t]+[-_0-9a-zA-Z]+\\)*\\)")
		(let* ((name (match-string 1))
		       (interested-p (or (eq magik-patch-interested-streams t)
					 (member name magik-patch-interested-streams))))
		  (if interested-p
		      (push (match-string 1) new-submit)))
	      (error "Unexpected submit format: %s in file %s" line-str mpde-env-patch-options-file)))
	   ((looking-at "reviewer")
	    ;;deprecated. This data is now sourced from CQ
	    ;;TODO reviewer data is actually the stream default reviewer list, reinstate...
	    )
	   (t
	    (error "Unexpected line: %s in file %s" line-str mpde-env-patch-options-file))))
	(forward-char))
      (setq magik-patch-cache-patchdirs
	    (cons (list mpde-env-unsubmitted-patches-dir '(0 0) nil)
		  (cons (list mpde-env-proposed-patches-dir '(0 0) nil)
			(cons (list mpde-env-rejected-patches-dir '(0 0) nil)
			      new-caches))))
      (push (cons "Patch-Intended-For-Release" (reverse new-submit)) magik-patch-toggles)
      (mapcar '(lambda (toggle) (push toggle magik-patch-toggles)) new-toggles)
      (setq magik-patch-patchdirs (reverse new-patchdirs)))))

(defun magik-patch-cache-parse-template ()
  "Read the magik patch template_TYPE.magik file in the current buffer.
TYPE is given by the variable `magik-template-file-type'."
  (save-match-data
    (goto-char (point-min))
    (or (re-search-forward "^#" nil t)
	(error resources-template-no-header-error
	       (magik-template-file magik-template-file-type)))
    (while
	(re-search-forward "^#\\([-_0-9a-zA-Z]+\\):[ \t]+\\([-_0-9a-zA-Z]+\\(/[^/\n]+\\)+\\)" nil t)
      (push (cons (match-string 1) (split-string (match-string 2) "/+")) magik-patch-toggles))))

(defun magik-patch-cache-refresh-patchdirs ()
  "Refresh `magik-patch-cache-patchdirs' and if any
of them were out-of-date it also recomputes the `magik-patch-cache-allpatches'.

MPDE: Only performed if master configuration is available."
  (let (fix-the-total-cache)
    (loop for cache in magik-patch-cache-patchdirs
	  do
	  (and (magik-patch-cache-update cache 'magik-patch-cache-update-patch-dirs)
	       (setq fix-the-total-cache t)))
    (if fix-the-total-cache
	(setq magik-patch-cache-allpatches
	      (loop for (path time data) in magik-patch-cache-patchdirs
		    append data)))))

(defun magik-patch-cache-refresh-bugdata ()
  "Refresh `magik-patch-cache-bug-dir' and `magik-patch-cache-bug-list' if out-of-date.

Actually checks whether `mpde-env-bug-report-dir-cache-file' has changed
and not `mpde-env-bug-report-dir' since it is the cache file that is loaded,
for performance reasons."
  
  ;;To force a refresh of cache you only need to set it to nil now.
  (if (null magik-patch-cache-bug-dir)
      (setq magik-patch-cache-bug-dir
	    (list mpde-env-bug-report-dir-cache-file '(0 0) nil)))

  (if (equal (first magik-patch-cache-bug-dir) mpde-env-bug-report-dir-cache-file)
      nil
    ;;MPDE has switched the value
    (setq magik-patch-cache-bug-dir
	  (list mpde-env-bug-report-dir-cache-file '(0 0) nil)))

  (magik-patch-cache-update
   magik-patch-cache-bug-dir
   (function
    (lambda (cache)
      (let (bug-cache-list)
	(message resources-patch-creating-bug-cache)
	(load mpde-env-bug-report-dir-cache-file t nil)
	(setq magik-patch-cache-bug-list bug-cache-list))))))

(defun magik-patch-cache-refresh-topic-data ()
  "Refresh `magik-patch-cache-topic-data' and `magik-patch-topic-data' if out-of-date."
  
  ;;To force a refresh of cache you only need to set it to nil now.
  (if (null magik-patch-cache-topic-data)
      (setq magik-patch-cache-topic-data
	    (list magik-patch-topic-data-file '(0 0) nil)))

  (if (equal (first magik-patch-cache-topic-data) magik-patch-topic-data-file)
      nil
    ;;MPDE has switched the value
    (setq magik-patch-cache-topic-data
	  (list magik-patch-topic-data-file '(0 0) nil)))

  (magik-patch-cache-update
   magik-patch-cache-topic-data
   (function
    (lambda (cache)
      (message resources-patch-load-topic-data)
      (load magik-patch-topic-data-file t nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;      7.  U T I L S and A D V I C E
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun magik-patch-file-number ()
  "Returns the patch number as a string (e.g. `1234_1a') of the current file, or nil if the current file isn't a patch."
  (if buffer-file-name
      (let ((leafname (file-name-nondirectory buffer-file-name)))
	(save-match-data
	  (if (string-match magik-patch-file-regexp leafname)
	      (match-string 1 leafname))))))

(defadvice choose-completion-delete-max-match (around inhibit-point-motion-for-magik-patch activate compile)
  "Override Magik Patch mode use of point motion hooks so that TAB completion works"
  (let ((inhibit-point-motion-hooks (and (boundp 'inhibit-point-motion-hooks)
					 (or (symbol-value 'inhibit-point-motion-hooks)
					     magik-patch-mode))))
    ad-do-it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;      8.  MPDE Functionality & Registration
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun magik-patch-mpde-save-buffer ()
  "Run when saving a Change file.
It is run via the `local-write-file-hooks' hook.
If the MPDE configuration is not 'master then we save the buffer
to `mpde-env-unsubmitted-patches-dir' directory."
  (cond ((eq mpde-configuration-current 'master)
	 nil)
	((equal (directory-file-name (file-truename default-directory))
		(file-truename mpde-env-unsubmitted-patches-dir))
	 nil)
	((file-exists-p (concat (file-name-as-directory mpde-env-unsubmitted-patches-dir)
				(file-name-nondirectory buffer-file-name)))
	 (error resources-patch-already-in-unsubmitted-error))
	((y-or-n-p (concat resources-patch-yn-save-unsubmitted " "))
	 (setq default-directory (file-name-as-directory mpde-env-unsubmitted-patches-dir)
	       buffer-file-name (concat default-directory
					(file-name-nondirectory buffer-file-name)))
	 nil ;return nil to indicate buffer can now be saved.
	 )
	(t
	 (error resources-mpde-loosing-changes-error))))

(defun magik-patch-mpde-store-caches (configuration)
  "Stores the various caches out into the MPDE configuration file.
This function is run from the hook `mpde-store-configuration-file-hook'."
  (magik-patch-cache-refresh-patch-options)
  (magik-patch-cache-refresh-templates)
  (magik-patch-cache-refresh-bugdata)
  (magik-patch-cache-refresh-patchdirs)
  (let ((vars '(magik-patch-cache-patchdirs  magik-patch-patchdirs
		magik-patch-cache-bug-list   magik-patch-cache-bug-dir
		magik-patch-cache-allpatches))
	v)
    (while vars
      (setq v (car vars))
      (insert (format "      (setq %s (quote %S))\n" (symbol-name v) (symbol-value v)))
      (setq vars (cdr vars)))))

(if (featurep 'mpde-boot)
    (progn
      (mpde-register-variable 'magik-patch-topic-data-file
			      (concat-u-colon "/swdev/data/CQ/Emacs/topic_data.el")
			      'master
			      'mpde-refresh-action-copy-file
			      'file)
      (add-hook 'mpde-set-configuration-hook 'magik-patch-mode-compile-help)
      (add-hook 'mpde-store-configuration-file-hook 'magik-patch-mpde-store-caches)))

(provide 'magik-patch)

;;; magik-patch.el ends here
