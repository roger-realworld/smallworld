;;; sw-electric.el -- inserting magik templates etc.

(eval-when-compile
  (require 'cl)
  (require 'resources)
  (require 'indent-magik)
  (require 'magik))

(require 'pragma)

(defgroup sw-electric nil
  "Customise Electric Magik group."
  :tag   "Electric Magik"
  :group 'magik)

(defconst sw-electric-version "$Revision: 1.9 $")

(defcustom electric-magik-mode nil
  "*Says whether the electric mode is on or off."
  :group 'sw-electric
  :type  'boolean)

(defcustom electric-insert-template-line-pre-hook nil
  "*Hook run before `electric-insert-template-line'."
  :group 'sw-electric
  :type  'hook)

(defcustom electric-insert-template-line-post-hook nil
  "*Hook run after `electric-insert-template-line'."
  :group 'sw-electric
  :type  'hook)

(defvar magik-electric-templates-methods
  '(("define_shared_constant" -1 1 (prev_pragma "_pragma(classify_level=, topic={}, usage={})")
     (prev_class_name "define_shared_constant(:" ",\n\t## \n\t## \n\t## \n\t)\n" dollar))
    ("define_shared_variable" -1 1 (prev_pragma "_pragma(classify_level=, topic={}, usage={})")
     (prev_class_name "define_shared_variable(:" ",\n\t## \n\t## \n\t## \n\t)\n" dollar))
    ("define_slot_access" -1 1 (prev_pragma "_pragma(classify_level=, topic={}, usage={})")
     (prev_class_name "define_slot_access(:" ",\n\t## \n\t## \n\t## \n\t)\n" dollar))
    ("define_pseudo_slot" -1 1 (prev_pragma "_pragma(classify_level=, topic={}, usage={})")
     (prev_class_name "define_pseudo_slot(:" ",\n\t## \n\t## \n\t## \n\t)\n" dollar))
    ("def_mixin" -1 1 (prev_pragma "_pragma(classify_level=, topic={}, usage={})")
     (prev_class_name "def_mixin(:" ",\n\t## \n\t## \n\t## \n\t)\n" dollar))
    ("def_property" -1 1 (prev_pragma "_pragma(classify_level=, topic={}, usage={})")
     (prev_class_name "def_property(:" ",\n\t## \n\t## \n\t## \n\t)\n" dollar))
    ("define_property" -1 1 (prev_pragma "_pragma(classify_level=, topic={}, usage={})")
     (prev_class_name "define_property(:" ",\n\t## \n\t## \n\t## \n\t)\n" dollar))
    ("define_condition" -1 0
     ("condition.define_condition(:" ",\n\t:,\n\t{})\n" dollar))
    ("define_binary_operator_case" -1 1 (prev_pragma "_pragma(classify_level=, topic={}, usage={})")
     (prev_class_name "define_binary_operator_case(:" ",\n\t## \n\t## \n\t## \n\t)\n" dollar)))
  " These 'method' templates automatically insert the class name at the front.")
(defvar magik-electric-templates
  (append
   '(("iter" e 1 (prev_pragma "_pragma(classify_level=, topic={},
usage={})") ("_iter _method " prev_class_name) "\t## " "\t## " -
"_endmethod" dollar)
     ("private" e 1 (prev_pragma "_pragma(classify_level=restricted, topic={}, usage={})") ("_private _method " prev_class_name) "\t## " "\t## "
      - "_endmethod" dollar)
     ("abstract" e 1 (prev_pragma "_pragma(classify_level=, topic={}, usage={})") ("_abstract _method " prev_class_name) "\t## " "\t## "
      - "_endmethod" dollar)
     ("method" e 1 (prev_pragma "_pragma(classify_level=, topic={}, usage={})") ("_method " prev_class_name) "\t## " "\t## " - "_endmethod"
      dollar)
     ("pragma" 17 0 (prev_pragma "_pragma(classify_level=, topic={}, usage={})"))
     ("def_slotted_exemplar" e 2 (prev_pragma "_pragma(classify_level=, topic={}, usage={})")
      ("def_slotted_exemplar(" filename_as_symbol ",\n\t## \n\t## \n\t## \n\t{\n\t},\n\t{})\n" dollar))
     
     ("if" e 0 "_if " "_then" - "_endif")
     ("over" e 0 "_over " "_loop" - "_endloop")
     ("catch" e 1 "_catch" - "_endcatch")
     ("block" e 1 "_block" - "_endblock")
     ("protect" e 1 "_protect" - "_protection" - "_endprotect")
     ("lock" e 0 "_lock " - "_endlock")
     ("try" e 1 "_try" - "_when" - "_endtry")
     ("proc" e 0 "_proc " - "_endproc")
     ("loop" e 1 "_loop" - "_endloop")
     ("for" 2 0 "_for  _over " "_loop" - "_endloop"))
   magik-electric-templates-methods)
  "*An association list of magik templates.")

(defun electric-magik-mode (&optional arg)
  "Toggle the electric switch."
  (interactive)
  (setq electric-magik-mode
	(if (null arg)
	    (not electric-magik-mode)
	  (> (prefix-numeric-value arg) 0)))
  (message (if electric-magik-mode
               resources-magik-electric-on
             resources-magik-electric-off)))
(defalias 'electric-magik-toggle 'electric-magik-mode) ;compatibility

(defun electric-magik-hash (arg)
  "insert the char, `#', and if this is the first `#' on the line and
the previous line starts with a `#' align with that."
  (interactive "*p")
  (self-insert-command arg)
  (if (save-excursion
        (and (progn (back-to-indentation) (eq (following-char) ?#))
             (eq (forward-line -1) 0)
             (progn (back-to-indentation) (eq (following-char) ?#))
             (current-column)))
      (magik-indent-command)))
        
(defun insert-magik-pragma ()
  "Insert a magik pragma statement on the current line."
  (interactive "*")
  ;;first prepare the line we are inserting on
  (beginning-of-line)
  (let ((blank-linep (looking-at "^\\s-*$"))
	(end-of-bufferp (save-excursion (end-of-line) (eq (point) (point-max)))))
    (and blank-linep (delete-horizontal-space))
    (if (or (not blank-linep) end-of-bufferp)
	(progn
	  (insert "\n")
	  (forward-line -1))))
  ;;Insert the default pragma statement
  (insert "prag")
  (explicit-electric-magik-space))

(defun explicit-electric-magik-space ()
  "insert magik programming templates"
  (interactive "*")
  (let*
      ((p (point))
       (str (save-excursion
              (skip-chars-backward "a-zA-Z_")
              (buffer-substring p (point))))
       (len (length str))
       keyword)

    ;;Since we now allow for _ in templates for def_slotted_exemplar etc.
    ;;we have to remove any initial _ for normal magik keywords
    (if (and (> (length str) 0)
	     (equal (substring str 0 1) "_"))
	(setq str (substring str 1)
	      len (length str)))
    (setq keyword (all-completions str magik-electric-templates))
    (cond ((null keyword)
	   nil)
	  ((eq (length keyword) 1)
	   (setq keyword (car keyword)))
	  (t
	   (setq keyword (completing-read
			  (concat resources-magik-electric-template-prompt " ")
			  (mapcar (function (lambda (k) (cons k k))) keyword)
			  nil t))))
    (if keyword
        (progn
          (save-excursion
            (skip-chars-backward "a-zA-Z_")
            (if (eq (following-char) ?_)
                (delete-char 1)))
          (insert (substring keyword len))
          (electric-magik-space 1 t))
      (error resources-magik-electric-no-template-error str))))

(defun electric-magik-space (arg &optional doit)
  "expand magik keywords into programming templates"
  (interactive "*p")
    (cond
     ((save-excursion
        (back-to-indentation)
        (looking-at "##?"))
      (let*
          ((match-str (match-string 0))
	   (auto-fill-function 'do-auto-fill)
           (fill-prefix (concat
                         (save-excursion
                           (back-to-indentation)
			   (buffer-substring (point-bol) (point)))
                         match-str
			 " "));we want to make the next line indented with a space
           (fill-column (save-excursion
                          (back-to-indentation)
                          (+ (current-column) 63))))
        (self-insert-command arg)))
     ((and (or doit
               (and electric-magik-mode
                    (or (eq (point) (point-max))
                        (looking-at "[ \t]*$"))
                    (save-excursion
                      (not (re-search-backward "[#\"]" (point-bol) t)))))
           (save-excursion
             (skip-chars-backward "a-zA-Z_")
             (and (or (eq (point) (point-min))
                      (save-excursion
                        (backward-char)
                        (looking-at "[^a-zA-Z0-9_!?|\\.]")))
                  (or (looking-at "\\([a-zA-Z_]+\\)[ \t]*$")
                      (and (eq this-command 'explicit-electric-magik-space)
                           (looking-at "\\([a-zA-Z_]+\\)")))))
           (assoc (match-string 1) magik-electric-templates))
      (electric-magik-insert-template (match-string 1)))
     (t
      (self-insert-command arg))))

(defun electric-magik-insert-template (name)
  "Insert template NAME."
  (let*
      ((template    (cdr (assoc name magik-electric-templates)))
       (len (length (car (assoc name magik-electric-templates))))
       (x (car template))
       (y (cadr template))
       (col (- (current-column) len))
       (p (point)))
    (backward-delete-char len)
    (if (save-excursion (re-search-forward "[^ \t]" (point-eol) t))
	(progn
	  (insert ?\n)
	  (backward-char))
      (delete-region (point) (point-eol)))
    (setq template (cddr template))
    (while template
      (electric-insert-template-line name (car template) col)
      (insert "\n")
      (pop template))
    (backward-delete-char 1)
    (goto-char p)
    (forward-line y)
    (cond ((not (numberp x))
	   (end-of-line))
	  ((>= x 0)
	   (move-to-column (+ col len x)))
	  (t
	   (end-of-line)
	   (forward-char x)))))

(defun electric-insert-template-line (name line col)
  "Interpret one line of the electric template."
  (if (and (listp line)
	   (not (eq (car line) 'prev_pragma)))
      ;;RECURSIVE
      (dolist (x line)
        (electric-insert-template-line name x col))
    (run-hook-with-args 'electric-insert-template-line-pre-hook name line col)
    (cond
     ((and (listp line) 
	   (eq (car line) 'prev_pragma))
      (if (save-excursion
	    (re-search-backward "^_pragma([^)]*)" nil t))
	  (let ((str (match-string 0))
		(pt (point)))
	    (insert str)
	    (save-excursion
	      (and (string-match "classify_level=deprecated" str) ;Was it deprecated?
		   (goto-char pt)                                 ;place point ready to insert deprecated template
		   (pragma-insert-magik-deprecated-template)      ;because this fn assumes that it is on the pragma line
		   )))
	(insert (cadr line))))
      ((eq line '-)
       (magik-indent-command))
      ((eq line 'dollar)
       (delete-region (point-bol) (point))
       (insert "$\n"))
      ((eq line 'prev_class_name)
       (let (class)
	 (save-excursion
	   (cond ((re-search-backward "_method[ \t]+\\(\\(\\sw\\|_\\)+\\)\\." nil t)
		  (setq class (match-string 1)))
		 ((re-search-backward "def_slotted_exemplar\\s-*(\\(\\s-\\|\n\\)*:\\(\\(\\sw\\|_\\)+\\)" nil t)
		  (setq class (match-string 2)))
		 (t nil)))
	 (if class (insert class "."))))
      ((eq line 'filename_as_symbol)
       (let ((name (buffer-name)))
	 (if (string-match "\\.magik$" name)
	     (setq name (substring name 0 (- (length name) 6))))
	 (insert ":" name)))
      ((eq (string-to-char line) ?\t)
       (indent-to (+ col magik-indent-level))
       (insert (substring line 1)))
      (t
       (indent-to col)
       (insert line)))
    (run-hook-with-args 'electric-insert-template-line-post-hook name line col)))

;;; setup of minor modes via setting of variable before load.
(and electric-magik-mode (electric-magik-mode 1))

(provide 'sw-electric)

;;; sw-electric.el ends here
