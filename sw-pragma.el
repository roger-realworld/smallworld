;;; pragma.el -- tool for filling in Magik pragma statements.
;;; Commentary:
;;; Code:

(defgroup sw-pragma nil
  "Magik Sw-Pragma features."
  :group 'magik)

(defvar topic-select-mode-map
  (let ((map (make-keymap)))
    (define-key map "y"  'topic-select-mark)
    (define-key map "n"  'topic-select-unmark)
    (define-key map "m"  'topic-select-mark)
    (define-key map "u"  'topic-select-unmark)
    (define-key map " "  'forward-line)
    (define-key map "q"  'topic-select-quit)
    (define-key map "\r" 'topic-select-select)
    (define-key map "e"  'topic-edit))
  "Keymap for Topic selection in _sw-pragma lines.")

;;;;;;;;;;;;;;;;;;;; User interface ;;;;;;;;;;;;;;;;;;;

(defun electric-sw-pragma-tab (sw-pragma-brackets)
  "Hop from one sw-pragma field to the next.
This command handles multiline _sw-pragma statements.
Uses SW-PRAGMA-BRACKETS."
       ;; try to work out which field we're in.
  (let ((start-bracket (car sw-pragma-brackets))
	(end-bracket (cdr sw-pragma-brackets)))
    (save-match-data
      (if (re-search-forward "\\s-*\\sw+\\s-*=" end-bracket t) ;find the start of the next option
	  (goto-char (match-end 0))
	(goto-char start-bracket)
	(search-forward "=" end-bracket t)))))

(defun electric-sw-pragma-/ (arg)
  "Insert the forward-slash.
Unless the current line starts with `_sw-pragma', in
which case we toggle back through the various sw-pragma options.
Uses ARG."
  (interactive "*p")
  (sw-pragma-electric-toggle-option arg 'forward))

(defun electric-sw-pragma-back-/ (arg)
  "Insert a backslash char.
Unless the current line starts with `_sw-pragma', in
which case we toggle back through the various sw-pragma options.
Uses ARG."
  (interactive "*p")
  (sw-pragma-electric-toggle-option arg 'backward))

;;;;;;;;;;;;;;;;;;;; generic functions ;;;;;;;;;;;;;;;;;;;

(defun sw-pragma-line-p ()
  "Determine if point is on a _sw-pragma line.
Returns a cons cell with locations of the start and end
brackets of the _sw-pragma statement if point
is somewhere in a sw-pragma statement
or nil otherwise.
Note that this command does handle a multiline _sw-pragma statement."
  (save-excursion
    (save-match-data
      (let* ((pt (point))
	     (end-bracket (search-forward ")" (save-excursion (forward-line 3) (point)) t))
	     (start-bracket (and end-bracket (condition-case err
						 (progn
						   (backward-sexp)
						   (point))
					       (error nil)))))
	(and start-bracket
	     end-bracket
	     (goto-char start-bracket)
	     (forward-line 0)
	     (looking-at "_sw-pragma(")
	     (>= pt (point))      ;;ensure original point location is after start of _sw-pragma
	     (<= pt end-bracket)  ;;and before the final bracket.
	     (cons start-bracket end-bracket))))))

(defun sw-pragma-do-if-match (list &optional default-elem reverse)
"Given an list of elems (NAME MATCH FUNCTION [OTHER...])
execute each match until it returns t.
If MATCH returns t eval the corresponding FUNCTION with the first arg being the
elem of the matching element and the second arg being the next elem in the list.
The optional arguments OTHER may be used by FUNCTION to modify its behaviour.
E.g. sw-pragma-if-match-replace-with-next uses the 4th arg to specify the subexpression to replace.

Optional arg DEFAULT-ELEM (DEFAULT MATCH FUNCTION [OTHER...]) is used if no matches are obtained from the list.
If this matches then the  default's function is called with next elem set to the first elem of the list.
  (Or last if REVERSE is t).
Optional arg REVERSE reverses the given list.

This can be thought to be equivalent to creating a cond construct using the MATCH in the list
as the tests and the FUNCTION as the form to be evualated when MATCH is true.
The extra bit this provides is that the called function knows what the next elem would be.
Also being able to make up a data structure means that it is easy to add new things to test for.

Returns nil if no change or the list (CURRENT-ELEM NEXT-ELEM) elements."
(if reverse
    (setq list (reverse list)))
(let* ((len          (1- (length list)))
       (first-elem   (elt list 0))
       (current-elem nil)
       (next-elem    first-elem)
       (n -1)
       (fn nil))
  (while (and (<= n len)
	      (progn
		(setq n (1+ n)
		      current-elem (elt list n)
		      next-elem (if (eq n len) first-elem (elt list (1+ n)))
		      fn (caddr current-elem))
		(not (eval (cadr current-elem))))))
  (cond ((and (symbolp fn) (fboundp fn))
	 (funcall fn current-elem next-elem reverse)
	 (list current-elem next-elem))
	(fn
	 ;fn is not a function so we evaluate it. The form can
	 ;can access current-elem, next-elem and reverse since we are still inside the let. I think...
	   (eval fn)
	   (list current-elem next-elem))
	((and default-elem
	      (eval (cadr default-elem)))
	  (funcall (caddr default-elem) default-elem first-elem reverse)
	  (list default-elem first-elem))
	(t nil))))

(defun sw-pragma-if-match-replace-with-next (current next reverse)
"Remove the current match region and replace with next.
The optional fourth item of CURRENT specifies a subexpression of the match.
Next is the car of the NEXT element.
Uses REVERSE.

It says to replace just that subexpression instead of the whole match.
The element follows that described in sw-pragma-do-if-match."
(save-excursion
  (let ((match-num (or (elt current 3) 0))
	(key (car next)))
    (delete-region (match-beginning match-num) (match-end match-num))
    (insert (if (symbolp key)
		(symbol-name key)
	      key)))))


;;;;;;;;;;;;;;;;;;;; Sw-Pragma deprecated magik code ;;;;;;;;;;;;;;;;;;;
;;When user de/selects a classify_level of deprecated then in addition
;;remove/insert a comment template.
 
(defvar sw-pragma-magik-deprecated-template-start
"## -------Deprecated------
"
"Start of deprecated method templates.
This is used for searching for the start of a template.")

(defvar sw-pragma-magik-deprecated-template-end
"## -----------------------
"
"End of deprecated method templates.
This is used for searching for the end of a template.")

(defvar sw-pragma-magik-deprecated-template
  (concat sw-pragma-magik-deprecated-template-start
"## Reason     : <why>
## Use instead: <other method>
## Action     : <action to be taken - use / for options>
## Deprecated : <timestamp>
"
sw-pragma-magik-deprecated-template-end)
"Template for inserting into comment header for deprecated methods.")

(defvar sw-pragma-magik-deprecated-template-re nil
  "Regexp that matches any indented Template for deprecated methods.")

;;Make a regexp to match template when indented. ie. insert \s-* at front of every none empty line.
(if (sw-pragma-magik-deprecated-template-re)  ;;Already defined so do nothing.
    nil
  (setq sw-pragma-magik-deprecated-template-re sw-pragma-magik-deprecated-template)
  (let (start)
    (while (string-match "^\\(\\s-*\\)\\S-+" sw-pragma-magik-deprecated-template-re start)
      (setq sw-pragma-magik-deprecated-template-re (replace-match "\\s-*" nil t sw-pragma-magik-deprecated-template-re 1))
      (setq start (1+ (match-end 0))))))

(defvar sw-pragma-magik-deprecated-action-list
  '(("Remove at next release."
     (looking-at " *Remove at next release. *")       sw-pragma-if-match-replace-with-next)
    ("Restricted at next release."
     (looking-at " *Restricted at next release. *")   sw-pragma-if-match-replace-with-next))
"The list used to control the Action field in the Magik deprecated template.
The format follows that described in sw-pragma-do-if-match.")

(defun sw-pragma-magik-deprecated-action-toggle (direction)
  "Toggle the current deprecated action option.
Uses DIRECTION."
   (goto-char (match-end 0))
   (sw-pragma-do-if-match sw-pragma-magik-deprecated-action-list
		       '(default  (looking-at "<.*>") sw-pragma-if-match-replace-with-next)
		       (eq direction 'backward)))

(defun sw-pragma-goto-magik-deprecated-template ()
"Goto the point at which the template should be placed."
;;limit is set so that the searching only looks at the next non-blank line.
(let ((limit (save-excursion (skip-chars-forward " \n") (end-of-line) (point))))
  (cond ((looking-at "\\s-*##")
	 (forward-line 0)
	 t)
	((re-search-forward "_method" limit t)
	 (forward-line 1)
	 t)
	((re-search-forward "(" limit t)
	 ;;This is intended to catch lines with define_shared_constant, define_shared_variable,
	 ;; define_slot_access, def_mixin, def_indexed_mixin, new_slotted_exemplar, new_indexed_exemplar
	 ;; i.e. anything which has a ( following it and which could include a ## comment
	 ;; between the ( and the matching ).
	 (backward-char) ; to place point in front of '(' ready for forward-sexp call
	 ;;if following search fails then the default is to insert immediately before this line
	 (re-search-forward "\\s-*##" (save-excursion (forward-sexp) (point)) t)
	 (forward-line 0)
	 t)
	(t
	 ;;By default insert immediately after the sw-pragma line
	 (forward-line 0)
	 t))))

(defun sw-pragma-insert-magik-deprecated-template ()
"Insert the template for deprecated methods."
(save-excursion
  (save-match-data
    (search-forward ")") ; find end of _sw-pragma statement
    (delete-horizontal-space)
    (if (eq (point) (point-max)) ;protect against being at the end of the buffer
	(insert "\n")
      (forward-line 1))
    ;;Now search for a suitable place to insert the template
    (and (sw-pragma-goto-magik-deprecated-template)
	 (not (looking-at (concat "\\s-*" sw-pragma-magik-deprecated-template-start)))
	 (let ((start (point))
	       (column (current-indentation))
	       (template (copy-sequence sw-pragma-magik-deprecated-template)))
	   (string-match "<timestamp>" template)
	   (setq template (replace-match (format-time-string "%d %B %Y") t t template))
	   ;;Insert the template setting read-only property on the start and end text fields
	   (insert template)
	   (indent-region start (point) column)
	   (message resources-sw-pragma-deprecated-template))))))

(defun sw-pragma-remove-magik-deprecated-template ()
"Remove the template for deprecated methods.
If the text to be removed has been modifed then the user is asked whether they
wish to remove it otherwise the template is removed silently."
(save-excursion
  (search-forward ")") ; find end of _sw-pragma statement
  (forward-line 1)
  (if (sw-pragma-goto-magik-deprecated-template)
      (let ((start nil)
	    (end nil))
	(if (looking-at sw-pragma-magik-deprecated-template-re)
	    ;;No changes made just remove whole template
	    (delete-region (match-beginning 0) (match-end 0))
	  (and (looking-at (concat "\\s-*" sw-pragma-magik-deprecated-template-start))
	       (setq start (match-beginning 0)))
	  (setq end (re-search-forward (concat "\\s-*" sw-pragma-magik-deprecated-template-end) nil t))
	  (and start
	       end
	       (y-or-n-p (concat resources-sw-pragma-yn-deprecated-modify-comments " "))
	       (delete-region start end)))))))

;;;;;;;;;;;;;;;;;;;; Sw-Pragma toggle options ;;;;;;;;;;;;;;;;;;;

(defvar sw-pragma-electric-toggle-list
      '(
	(classify-level (looking-at "c?lassify_level") sw-pragma-if-match-insert-classify_level)
	(usage          (looking-at "u?sage")          sw-pragma-if-match-insert-usage)
	(topic          (looking-at "t?opic")          sw-pragma-if-match-do-the-electric-sw-pragma-topics))
"The list used to control behaviour for the various fields in _sw-pragma.
The format follows that described in sw-pragma-do-if-match.")

(defvar sw-pragma-classify_level-list
  '((basic      (looking-at " *basic *")      sw-pragma-if-match-replace-with-next)
    (advanced   (looking-at " *advanced *")   sw-pragma-if-match-replace-with-next)
    (restricted (looking-at " *restricted *") sw-pragma-if-match-replace-with-next)
    (deprecated (looking-at " *deprecated *") sw-pragma-if-match-replace-with-next)
    (debug      (looking-at " *debug *")      sw-pragma-if-match-replace-with-next))
"The list used to control behaviour for the classify_level field in _sw-pragma.
The format follows that described in sw-pragma-do-if-match.")

(defvar sw-pragma-usage-list
  '(("{subclassable}"
     (looking-at" *{ *subclassable *} *")                          sw-pragma-if-match-replace-with-next)
    ("{redefinable}"
     (looking-at " *{ *redefinable *} *")                          sw-pragma-if-match-replace-with-next)
    ("{redefinable, subclassable}"
     (looking-at " *{ *redefinable *, *subclassable *} *")         sw-pragma-if-match-replace-with-next)
    ("{external}"
     (looking-at " *{ *external *} *")                             sw-pragma-if-match-replace-with-next)
    ("{not_international}"
     (looking-at " *{ *not_international *} *")                    sw-pragma-if-match-replace-with-next)
    ("{not_international, subclassable}"
     (looking-at " *{ *not_international *, *subclassable *} *")   sw-pragma-if-match-replace-with-next)
    ("{not_international, redefinable}"
     (looking-at " *{ *not_international *, *redefinable *} *")    sw-pragma-if-match-replace-with-next)
    ("{not_international, redefinable, subclassable}"
     (looking-at "{not_international, redefinable, subclassable}") sw-pragma-if-match-replace-with-next))
"The list used to control behaviour for the usage field in _sw-pragma.
The format follows that described in sw-pragma-do-if-match.")

(defun sw-pragma-electric-toggle-option (arg direction)
  "Insert the forward-slash.
Unless the current line starts with `_sw-pragma', in
which case we toggle through the various sw-pragma options.
Uses ARG and DIRECTION."
  (save-match-data
    (let ((sw-pragma-brackets (sw-pragma-line-p)))
      (cond ((consp sw-pragma-brackets)
	     (let ((curr-pos (point))
		   (start-bracket (car sw-pragma-brackets))
		   (end-bracket (cdr sw-pragma-brackets))
		   option-pos)
	       (goto-char (1+ start-bracket))
	       (re-search-forward "\\s-*" nil t)
	       (setq option-pos (point))

	       ;;loop over the positions where each option starts and check to see which option
	       ;;point is currently located in. This loop copes with multiline sw-pragmas and sensibly
	       ;;handles the cases when point is in a whitespace section between , and the start of the next option
	       (search-forward "=" end-bracket t) ; skip over current option
	       (while (and (re-search-forward "\\s-*\\(\\sw+\\)\\s-*=" end-bracket t)
			   (goto-char (match-beginning 0))        ;found start of next option including preceeding space
			   (<= (point) curr-pos)                  ;test if point is in this option or a later one.
			   (setq option-pos (match-beginning 1))) ;store true start of option
		 (search-forward "=" end-bracket t))              ; LOOP: skip passed current option

	       (goto-char option-pos)
	       (sw-pragma-electric-toggle direction)))
	    ((save-excursion
	       (beginning-of-line)
	       (looking-at "\\s-*## Action\\s-+: "))
	     (sw-pragma-magik-deprecated-action-toggle direction))
	    (t
	     (self-insert-command arg))))))

(defun sw-pragma-electric-toggle (direction)
"Toggle the values for the different fields used in the sw-pragma line.

DIRECTION indicates whether the values should change 'forward or 'backward
relative the current setting and available values."
  ;;Handle the case where the sw-pragma line is completely empty separately.
  (if (save-excursion (beginning-of-line) (looking-at "_sw-pragma()"))
      (progn
	;;Insert classify_level and place point between ( and c.
	(delete-region (match-beginning 0) (match-end 0))
	(insert "_sw-pragma(classify_level=)")
	(backward-char 16)))

  (sw-pragma-do-if-match sw-pragma-electric-toggle-list nil (eq direction 'backward)))

(defun sw-pragma-if-match-insert-classify_level (current next reverse)
"Insert the classify_level according to the current setting.
Also adds a template in the comment section when the classify_level is set
to deprecated.
When the classify_level is changed from deprecrated then the template is removed.
However, if data has been changed in the fields of the template then the user
is asked if they wish to remove the contents of the depreacted template.
Uses CURRENT, NEXT and REVERSE."
;;Ensure point stays immediately after = by searching for = and doing the replace
;;inside save-excursion
(search-forward "=")
(save-excursion
  (let ((res (sw-pragma-do-if-match sw-pragma-classify_level-list
				    '(default (looking-at "\\([^,]*\\),")
				       sw-pragma-if-match-replace-with-next 1)
				 reverse)))
    (cond ((eq (caadr res) 'deprecated)
	   ;;next element is deprecated i.e. user has just selected deprecated
	   (sw-pragma-insert-magik-deprecated-template))
	  ((eq (caar res) 'deprecated)
	   ;;current element is deprecated i.e. user has just deselected deprecated
	   (sw-pragma-remove-magik-deprecated-template))
	  (t nil)))))

(defun sw-pragma-if-match-insert-usage (current next reverse)
"Insert the usage according to the current setting.
Uses CURRENT, NEXT and REVERSE."
;;Ensure point stays immediately after = by searching for = and doing the replace inside save-excursion
(search-forward "=")
(save-excursion
  (sw-pragma-do-if-match sw-pragma-usage-list
		      '(default (looking-at "{.*}") sw-pragma-if-match-replace-with-next)
		      reverse)))

;;;;;;;;;;;;;;;;;;;; Topic Select Mode ;;;;;;;;;;;;;;;;;;;

(defvar topic-select-mode-syntax-table nil
  "Syntax table used while in topic-select mode.")
(if topic-select-mode-syntax-table
    ()
  (setq topic-select-mode-syntax-table (make-syntax-table)))

(defvar topic-select-mode-abbrev-table nil
  "Abbrev table used while in topic-select mode.")
(define-abbrev-table 'topic-select-mode-abbrev-table ())

(defvar sw-pragma-sw-window-configuration nil
"Window configuration to return to after topic selection mode.")

(defun sw-pragma-if-match-do-the-electric-sw-pragma-topics (current next reverse)
  "Select sw-pragma topics from a menu.
Uses CURRENT, NEXT and REVERSE."
  (let* ((buffer-dir (if buffer-file-name (file-name-directory buffer-file-name) default-directory))
	 (sw-pragma-files (if buffer-dir (sw-find-files-up buffer-dir "data/doc/sw-pragma_topics")))
	 (product-sw-pragma-file (if (getenv "SMALLWORLD_GIS") (expand-file-name (concat (getenv "SMALLWORLD_GIS") "/data/doc/sw-pragma_topics"))))
	 topics pos)
    (re-search-forward "= *")
    (setq pos (point))
    (if (not (eq (following-char) ?{ ))
        (progn (insert "{") (search-forward ",") (backward-char) (insert "}")))
    (goto-char pos)
    (forward-char)
    (while
        (looking-at " *\\(\\w+\\)")
      (push (cons (match-string 1) t) topics)
      (goto-char (match-end 0))
      (looking-at " *,")
      (goto-char (match-end 0)))
    (goto-char pos)
    (setq sw-pragma-sw-window-configuration
          (current-window-configuration))
    (pop-to-buffer "*topic-selection*")
    (erase-buffer)
    (topic-select-mode)
    (insert resources-sw-pragma-topic-selection)
    (mapc 'insert-file-contents sw-pragma-files)
    (and product-sw-pragma-file
	 (not (member product-sw-pragma-file sw-pragma-files))
	 (file-exists-p product-sw-pragma-file)
	 (insert-file-contents product-sw-pragma-file))
    (or sw-pragma-files
	product-sw-pragma-file
	(error resources-sw-pragma-no-smallworld-gis-env))
    (goto-char
     (prog1
         (point)
       (while (not (eq (point) (point-max)))
         (let
             ((topic
               (and
		(looking-at "\\s-*\\S-+\\s-+\\(\\S-+\\)")
                (match-string 1))))
           (beginning-of-line)
           (insert
            (if (assoc topic topics) "> " "  "))
           (forward-line)))))))

(defun topic-select-mode ()
  "Major mode for selecting topics in sw-pragmas."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'topic-select-mode)
  (setq mode-name resources-sw-pragma-topic-select)
  (use-local-map topic-select-mode-map)
  (set-syntax-table topic-select-mode-syntax-table)
  (setq local-abbrev-table topic-select-mode-abbrev-table)
  (run-hooks 'topic-select-mode-hook))

(defun topic-select-mark ()
  "Mark a line to indicate that the process should be run."
  (interactive "*")
  (topic-replace-char ">"))

(defun topic-replace-char (ch)
  "Add CH to the beginning of the current line and move down a line.
Beep if not looking at \"[ >] (\""
  (if
      (not (looking-at "[ >] "))
      (beep)
    (beginning-of-line)
    (insert ch)
    (delete-char 1)
    (forward-line)))

(defun topic-select-unmark ()
  "Remove the mark from the current line."
  (interactive "*")
  (topic-replace-char " "))

(defun topic-select-quit ()
  "Quit from topic selection by restoring the window configuration."
  (interactive)
  (kill-buffer (current-buffer))
  (set-window-configuration sw-pragma-sw-window-configuration))

(defun topic-select-select ()
  "Put the selected topics back into the sw-pragma."
  (interactive)
  (goto-char (point-min))
  (let
      ((str ""))
    (while
        (not (eq (point) (point-max)))
      (if (looking-at ">\\s-*\\S-+\\s-+\\(\\S-+\\)")
          (setq str (concat str (match-string 1) ", ")))
      (forward-line))
    (if (not (equal str ""))
        (setq str (substring str 0 (- (length str) 2))))
    (kill-buffer (current-buffer))
    (set-window-configuration sw-pragma-sw-window-configuration)
    (forward-char)
    (delete-region
     (point)
     (progn (search-forward "}") (backward-char) (point)))
    (insert str)))

(defun topic-edit ()
  "Edit the topics file for the Smallworld Product \".../data/doc/sw-pragma_topics\"."
  (interactive)
  (let* ((buffer-dir (if buffer-file-name (file-name-directory buffer-file-name) default-directory))
	 (sw-pragma-file (if buffer-dir (sw-find-files-up buffer-dir "data/doc/sw-pragma_topics" t))))
    (cond (sw-pragma-file
	   (find-file (car sw-pragma-file)))
	  ((getenv "SMALLWORLD_GIS")
	   (find-file
	    (concat (file-name-as-directory (getenv "SMALLWORLD_GIS"))
		    "data/doc/sw-pragma_topics")))
	  (t
	   (error resources-sw-pragma-no-smallworld-gis-env)))))

(provide 'sw-pragma)

;;; sw-pragma.el ends here
