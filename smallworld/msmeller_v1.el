;; msmeller.el -- Code Smeller for Magik code

;;                    The Magik Smeller
;;                    =================
;;
;; VERSION 1.00
;;
;; Split the code logic from the code smell data.
;;
;;
;; VERSION 0.05
;;
;; WHAT'S NEW IN THIS RELEASE?
;; - check for .value and suggest the use of .value_in()
;; - 
;;
;; VERSION 0.04
;;
;; WHAT'S NEW IN THIS RELEASE?
;; - trapped for the recursive condition of smelling the *Magik Smeller* buffer.
;; - fixed a false positive on 'BAD >> USAGE'.  Now includes a check for _return.
;; - check for use of .get_global().  This is bad practice and should be taken out of the code.
;; - check for substitute_character().  This does not make a copy of the receiver so you need 
;;   to make sure that you are not unitentionally changing the receiver object.
;; - make the "goto" key binding something safer than C-c C-c
;;    This one done. I.Lapitski, Nov 2005.  It now uses F3-j
;;
;; DESCRIPTION
;; ===========
;; A tool to interactively report actual or possible code syntax problems that 
;; are acceptable to the Magik compiler but do not meet development standards
;; or may be considered inefficient.
;;
;; Why "Smeller"?  I was inspired by a discussion on: http://c2.com/cgi/wiki?CodeSmell
;; There, a CodeSmell is defined as "a hint that something has gone wrong somewhere in 
;;  your code. Use the smell to track down the problem".  
;; Thus, the MSmeller is not intended to fix your problems, but it is helpful for code 
;; reviewers to quickly find common coding "flags" that are a problem themselves or 
;; provide a "smell" of a larger problem.  This module is called *M*smeller because it
;; is customised for Magik code, but it could just as easily be written for other 
;; languages (jsmeller, csmeller, etc).
;;
;; To use, compile this file.  Then, when you are in a buffer that contains Magik code,
;; execute "M-x msmeller".
;;
;; A new buffer called *Magik Smeller* will appear showing a list of all possible code
;; "smells", their line number in the Magik file and a short description.  To jump back
;; to a line of code that has been reported as a smell, place your cursor on that line
;; in the *Magik Smeller* and press "F3-j".
;;
;; I have included a starter set of code smells in 'msmeller-rule-list and intend to expand
;; this list as I become aware of them.  Many of the current smells are determined
;; via regexp searches.  In the near future I would also like to implement smell tests
;; that take into account context within a method.  Specifically, I want to be able to test
;; that an expression is not being repeatedly evaluated within a loop when it would 
;; suffice to evaluate it once outside the loop.
;;
;; If you find some smells missing or too annoying, please let me know.  If you develop
;; new smell functions, please send them to me so I can incorporate them into a master
;; list. Please forward any comments or improvements to me at: alfred.sawatzky@ge.com
;;
;; Thanks to: Igor Lapitski, Roger Nyberg, Glenn Nicholls, Mike Buller, Ed Charnock
;;
;;
;; Alfred Sawatzky
;; January 2004

;; TO DO
;; - font locking on priority level

;; - search for 30.48 / 304.8 / 3.048 and suggest to use units/coordinate_systems
;; - search for 3.14 and suggest to use float.pi
;; - ensure that no default slot values are included in def_slotted_exemplar

;; - Generalise to work on any file type
;; - Generalise to invoke external program, e.g. lint, perl -cw, gcc -Wall
;;    a program which massages lint output for proper parsing in an emacs
;;    M-X compile command.
;;   
;;    an input line of the form
;;                  fname(lnum): error
;;    is massaged to:
;;                  "fnam", line lnum: error
;;   
;;   lint $* | sed -e 's/^/"/' -e 's/(/", line /' -e 's/)//' 

;;AJM

;; Instead of levels, use named types. Then can enable types of smells.

;; Need to code a macro like
(defmacro defsmell (name level doc &rest args)  ;;c.f. defface
  "Declare a Code smell. "
  (list 'let (list 'fn)
    (list 'setq 'fn (list 'defun name doc args))
    (list 'put 'fn (list 'quote 'smell-level) level)
    'fn)
  ;;(nconc (list 'put) (list (list 'defun name doc args)) (list (list 'quote 'smell-level)) (list level))
  )
(package 'smell)

;;Need to create a separate file containing a language file's smells.
;;e.g.
(defsmell magik-condition-or-show_alert 5
  "Raise a condition instead of explicitly showing an alert and then _returning"
  (identity "show_alert\\(.*\n\\|.*\n.*\n\\|.*\n.*\n.*\n\\).*_return"))

(defsmell magik-patch-check 5
  "Raise a condition instead of explicitly showing an alert and then _returning"
  (identity "check"))
(package 'smell-magik)

;;
;;(get 'magik-condition-or-show_alert 'smell-level)
;;
;;AJM Need smell-describe which calls describe function but than amends *Help*
;;    with Smell level etc.

;;AJM Should both minor mode and major mode smells be tested. Yes but perhaps ask user?
;; AJM when checking a buffer attempt to require smell-[major-mode] and smell-[minor-modes]
;;     only do it if require succeeds.
;;     Needs (require FEATURE &optional FILENAME NOERROR)? Earlier Emacsen?

(defvar msmeller-hash (make-hash-table)
  "A hash table keyed on integers (smell number) whose values are a list of three elements: (smell-priority,short-description,long-description) ")


;; USER-CONFIGURABLE VALUES
;;
(defconst msmeller-show-description-p t
  "Setting this value to t will cause the detailed description to show under each rule violation. Setting it to nil will prevent the detailed description from being reported")

(defconst msmeller-method-too-long-size 40
  "for the msmeller-rule-method-too-long fuction, sets the threshold of the number of lines to consider 'too long' for a method.")

(defconst msmeller-min-priority-level 0
  "the minimum smell-priority level to consider when scanning for smells.")

(defconst msmeller-max-priority-level 9
  "the maximum smell-priority level to consider when scanning for smells.")

;; do not change the hash keys (ie, smell-number).  They correspond to actual smelling code further down.
;; You are, however, very welcome to change the smell-priority and short/long descriptions to suit
;; your taste and/or language.
(puthash   1 (list 5 "Condition vs. Show_alert" 
		   "Raise a condition instead of explicitly showing an alert and then _returning") msmeller-hash)

(puthash   2 (list 5 "Checkpoint created" 
		   "Examine the code to make sure that automatically-created checkpoints are also removed automatically later.  Having unexpected checkpoints in the database can cause unnecessary database growth and poor database performance over time.") msmeller-hash)

(puthash   3 (list 5 "Use enabled_state_aspect instead"
		   "Do not set the .visibility attribute directly.  Use the :enabled_state_aspect property when you define widgets whose visibility you want to control.") msmeller-hash)

(puthash   4 (list 1 "date tags"
		   "Look for dates within the code - these usually indicate change labels that are better off handled by the source control system.") msmeller-hash)

(puthash   5 (list 5 "record_transaction vs lwt"
		   "Starting at CST 4, it is more robust programming style to use record_transaction API instead of start/end_lwt()") msmeller-hash)

(puthash   6 (list 5 "Debug traceback"
		   "Remove any calls to !traceback!() that were used only for debugging.") msmeller-hash)

(puthash   7 (list 5 "code review"
		   "If the code comment states that it needs code review, then review the code and remove the comment.") msmeller-hash)

(puthash   8 (list 5 "FIXME"
		   "There should not be any references to FIXME in production code.") msmeller-hash)

(puthash   9 (list 5 "Check try/endtry"
		   "Not a problem by itself.  Review that we are not handling high-level conditions without some meaningful error reporting.  Having a when block that does nothing makes debugging VERY difficult.") msmeller-hash)

(puthash  10 (list 5 "Dead Code"
		   "Do not leave commented-out code in production.  Use version management software to recall old code, if necessary.") msmeller-hash)

(puthash  11 (list 5 "too many concatenated calls"
		   "Using too many concatenated method calls causes the code to be unreadable.  Split the code into shorter statements instead.") msmeller-hash)

(puthash  12 (list 5 "new_safe() vs new()"
		   "Use button_item.new_safe() instead of :new() to allow more user-friendly error reporting.") msmeller-hash)

(puthash  13 (list 5 "WORLD.GEOMETRIES() GEOM_SPEC"
		   "In many cases, asking a world for its :geometries() is done to get a certain type of geometry.  It is faster to simply pass a geom_spec than to individually ask each geometry what its :geom_type is.") msmeller-hash)

(puthash  14 (list 5 ".RWO.RWO_TYPE"
		   "A geometry knows its :rwo_type, so there is no need to first ask it for its rwo.") msmeller-hash)
    
(puthash  15 (list 5 ">> _TRUE / >> _FALSE"
		   "Often a >>_true/>>_false is an indication that the if/then block could be replaced with an expression that simply evaluates to the boolean.") msmeller-hash)

(puthash  16 (list 5 "RAISING GENERIC ERRORS"
		   "Do not raise generic errors.  Create a specific condition so that handling code can easily discern what kind of error was really encountered.") msmeller-hash)

(puthash  17 (list 9 "Review input/output streams"
		   "Not a problem by iteslf.  Make sure that input/output streams are always :close() within a protect block.") msmeller-hash)

(puthash  18 (list 9 "slow iterators"
		   "use .fast_elements()/.fast_keys_and_elements()/.fast_keys() as long as the object being looped over is not changing within the loop.   Note a single caveat to the above: it is always legitimate to remove the current record (ie the one just yielded).") msmeller-hash)

(puthash  19 (list 5 "remex"
		   "Should be none of this in the code.") msmeller-hash)
   
(puthash  20 (list 9 "sys! methods"
		   "These methods are to be used for DEBUGGING only") msmeller-hash)
   
(puthash  21 (list 9 ".source_file"
		   "never use .source_file call in code") msmeller-hash)

(puthash  22 (list 5 "default evaluations"
		   "The argument to the :default() method is always evaluated.  If it is an expensive evaluation, consider using a if/then block.") msmeller-hash)

(puthash  23 (list 5 "use .empty?"
		   "use _is *.empty? or _not *.empty? instead of size comparisons.") msmeller-hash)

(puthash  24 (list 5 "review tracing arguments" 
		   "Not a problem as such.  Review the arguments to trace_out()/shortest_path() to ensure that all expensive and repetitive argument evaluations are done only once.  Use caching constructs if possible.") msmeller-hash)

(puthash  25 (list 5 "Review network_follower instantiation" 
		   "Not a problem as such.  Review the network_follower creation calls to ensure that it is cached after the first instantiation.") msmeller-hash)

(puthash  26 (list 5 "remove .write_string"
		   "It is faster to simply call :index_of_seq() directly on the symbol.") msmeller-hash)

(puthash  27 (list 5 "use index_of() instead"
		   "If index_of_seq() takes a single-character string as an argument, use :index_of() instead.") msmeller-hash)

(puthash  28 (list 1 "possible wrong collection type"
		   "quite often this is an indication that you should have used equality_* collections") msmeller-hash)

(puthash  29 (list 1 "remove debug write statements"
		   "Remove debug output statements from code.  Raise conditions, if necessary.") msmeller-hash)

(puthash  30 (list 9 "Responds To"
		   "May be an indicator of poor OO design.  As a last resort, use has_field?() or :method_table[]") msmeller-hash)

(puthash  31 (list 9 "Is Kind Of"
		   "May be an indicator of poor 00 design.  Should not be used unless there is no other solution.  Look for alternative solutions first.") msmeller-hash)

(puthash  32 (list 9 ".class_name"
		   "Use .is_class_of?() instead") msmeller-hash)
   
(puthash  33 (list 5 "Use Integer operators"
		   "If the code was meant to divide integers, then use _div and _mod.  If one of the operands is a float, then force the integer to be a float by writing it with a .0") msmeller-hash)

(puthash  34 (list 5 "Use Float operators"
		   "If the code was meant to divide floats, then use /.  If one of the operands is a float, then force the integer to be a float by writing it with a .0") msmeller-hash)

(puthash  35 (list 5 "Enumerated class identity instead of equality"
		   "use _is or _isnt to compare enumerated classes instead of = or <>.  Identity should be used on enumerated classes (ie., boolean,symbol,character,integer).  It is difficult to trap for float values so some results may be False Positive.") msmeller-hash)

(puthash  36 (list 5 "equality instead of identity"
		   "use = or <> instead of _is or _isnt") msmeller-hash)

(puthash  37 (list 5 "canonical instead of lowercase"
		   "call :canonical instead of :lowercase on strings.  This is better internationalization") msmeller-hash)

(puthash  38 (list 5 "hardcoded string"
		   "use the message system. Do not hardcode strings.") msmeller-hash)
   
(puthash  39 (list 9 "concatenating strings"
		   "never concatenate strings using the + operator or write_string() procedure.  Use char16_vector.concatenation() instead.") msmeller-hash)

(puthash  40 (list 9 "_andif/_orif vs _and/_or"
		   "always use _andif/_orif instead of _and/_or unless you have a good reason for doing otherwise") msmeller-hash)

(puthash  41 (list 5 "text encoding"
		   "a Magik file MUST have a text_encoding declaration as its first line.  For example:\n#% text_encoding = iso8859_1") msmeller-hash)

(puthash  42 (list 5 "package declaration"
		   "a Magik file MUST have a _package declaration at the top (usually the second line).  For example:\n_package sw") msmeller-hash)
   
(puthash  43 (list 5 "Copyright statement"
		   "Magik code should have a copyright statement at the top.  For example:\n# Copyright (C) GE Network Reliability Services 2005, all rights reserved.") msmeller-hash)

(puthash  44 (list 5 "Missing Pragma Statement"
		   "Define a _pragma statement for this block of code.") msmeller-hash)

(puthash  45 (list 5 "Method too long"
		   (format "Long methods make code more difficult to maintain. The threshold for determining 'too long' is %d statement lines." msmeller-method-too-long-size)) msmeller-hash)

(puthash  46 (list 5 "Bad >> Usage"
		   ">> was probably intended to return from a method but really it only returns from a block") msmeller-hash)

(puthash  47 (list 5 "file name mismatch"
		   "The file name and the class name do not match.  They should.") msmeller-hash)

(puthash  48 (list 1 "get_global()"
		   "there is no need to use the method .get_global().  Just reference the global directly.") msmeller-hash)

(puthash  49 (list 1 "substitute_character()"
		   "substitute_character() does not make a copy of the receiver.  Double-check the code to make sure you really intend to change the receiver and not a copy of it.") msmeller-hash)

(puthash  50 (list 1 "ambiguous unit value"
		   "simply calling .value on a unit_value class may not produce the results you are expecting.  It is better to use .value_in() to return the value in the explicit unit that you are interested in.") msmeller-hash)

;;(puthash <smell-number> (list <smell-priority> "short description"
;;		   "long description") msmeller-hash)
;;


;; ============ END USER-CONFIGURABLE VALUES






;; FUNCTIONALITY

(defvar msmeller-mode-map ())

(if msmeller-mode-map
    ()
  (setq msmeller-mode-map (make-sparse-keymap))
  (define-key msmeller-mode-map [f3 ?j] 'msmeller-goto-line)
  )

(defvar msmeller-buffer nil)

(defcustom msmeller-mode-hook '()
  "*Hook for customising Msmeller mode."
  :type 'hook
  :group 'msmeller)

(defun msmeller-outline-regexp-hook()
;;  (setq outline-regexp "([0-9]+)"))
  (setq outline-regexp "^.+\n=+"))

(add-hook 'msmeller-mode-hook 'msmeller-outline-regexp-hook)

(defun msmeller-mode ()
  "Major mode for output from \\[msmeller].

All changes made in this buffer will be propagated to the buffer where
you ran \\[msmeller].

Press \\[msmeller-mode-find] to return to the original buffer."
  (kill-all-local-variables)
  (use-local-map msmeller-mode-map)
  (setq major-mode 'msmeller-mode)
  (setq mode-name "Magik Smeller")
  (make-local-variable 'msmeller-buffer)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(magik-font-lock-keywords nil t))
  (run-hooks 'msmeller-mode-hook))

(defvar msmeller-processed-smells nil
  "Used by msmeller-record-smell to keep track of whether or not a smell as been recorded in this run.")

(defvar msmeller-score 0
  "Used by msmeller to keep track of the total smell score for a given file")

(defconst msmeller-typo-regexps
  (list 
   ;; TO DO LIST
   ;;  -  Look for classes with no class comment at the top.
   ;;  -  the number of statement lines per _self call is an indication of whether the current method was written on the correct class.  If there are very few calls to _self, then maybe the method should have been written on a different class.
   ;;  -  Look for # comments within the method - if you need to comment the code then the code should be made better.
   ;;  -  move as much evaluation out of a loop as possible
   ;;  -  all variables should have a _local/_global, etc. declaration.
   ;;  -  Be able to run on a single method - maybe useful.
   ;;  -  Look for methods with no ## comment at the header - all methods should have a ## comment at the top explaining what they do.
   ;;  -  Explanation behind the reasons why the item was picked up - for example, why it is not a good idea to concatenate strings with a '+'
   ;;  -  maybe the check for hard-coded strings should only be done within methods.  It seems that one could legitimately put hard-coded strings in shared_constants.

;;    (list ""
;; 	 ""
;; 	 "")
;;
   
;;    (list "[ 	({,][a-Z!\\?][a-Z0-9!\\?_]*[ 	)},.]"
   ;; 	 "This is the regexp for variables in a method."
;; 	 "")
;;
   
;; (list smell-number regexp)
   (list 1 "show_alert\\(.*\n\\|.*\n.*\n\\|.*\n.*\n.*\n\\).*_return")
   (list 2 "\\.checkpoint(")
   (list 3 "\\.visibility *<<")
   (list 4 (concat "#.*"
		   "[0-9]+"
		   "\\(\\\\\\|/\\|-\\|\\.\\)"
		   "[0-9]+"
		   "\\(\\\\\\|/\\|-\\|\\.\\)"
		   "[0-9]+"))
   (list 5 "start_lwt(")
   (list 6 "!traceback!")
   (list 7 "code review")
   (list 8 "FIXME")
   (list 9 "_when *\\([, ]*error[^a-Z]*\\|[, ]*user_error[^a-Z]*\\)\\(_when\\|_endtry\\)")
   (list 10 "#.*\\(_method\\|_loop\\|_endloop\\|_if\\|_then\\|_proc\\|_try\\|_when\\|_handling\\|<<\\|>>\\|[a-z]+\\.[a-Z]+\\)")
   (list 11 "^[ \t]*[^#\n]*\\(\\. *$\\|\\. *#.*\\|[^ \t\n]+\\.[^ ,\n]+\\.[^ ,\n]+\\.[^ ,\n]+\\.[^ ,\n]+\\)")
   (list 12 "[^_]button_item\\.new(")
   (list 13 "\\.geometries( *)")
   (list 14 "\\.rwo\\.rwo_type")
   (list 15 "\\(_then\\|_else\\).*\n.*\\(>> *_true\\|>> *_false\\)")
   (list 16 "condition\\.raise( *\\(:error\\|:user_error\\)")
   (list 17 "\\(external_text_output_stream\\.new\\|external_text_input_stream\\.new\\)")
   (list 18 "\\(\\.elements()\\|\\.keys_and_elements()\\|\\.keys()\\)")
   (list 19 "remex(")
   (list 20 "\\.sys!")
   (list 21 "\\.source_file")
   (list 22 "\\.default(.*\\..*)")
   (list 23 "\\.size *[=<>~_is]\\{1,5\\} *0")
   (list 24 "\\(\.trace_out(\\|\.shortest_path(\\)")
   (list 25 "network_follower\\.new( *[^)]")
   (list 26 "\\.write_string\\.index_of_seq")
   (list 27 "\\.index_of_seq( *\".?\" *)")
   (list 28 "\\.as_symbol()")
   (list 29 "\\(^[ \t]*print(\\|^[ \t]*show(\\|^[ \t]*write(\\)")
   (list 30 "responds_to")
   (list 31 "\\.is_kind_of\\?(")
   (list 32 "\\.class_name *\\(_is\\|_isnt\\|[=<>~]\\{1,2\\}\\)")
   (list 33 "\\([^\\.\n][0-9][0-9]*[ \t]*/\\|/[ \t]*[0-9][^\\.]\\)")
   (list 34 "\\(\\(_div\\|_mod\\)[ \t]*[0-9]+\\.[0-9]+\\|[0-9]+\\.[0-9]+[ \t]\\(_div\\|_mod\\)\\)")
   (list 35 (concat "\\("
		    ;; regexp for boolean
		    "\\(\\(=\\|<>\\)[ \t]*_[mMtTfF]+[a-Z]\\{3,5\\}\\|[ \t]+_[mMtTfF]+[a-Z]\\{3,5\\}[ \t]*\\(=\\|<>\\)\\)"
		    "\\|"
		    ;; regexp for symbol
		    "\\(\\(=\\|<>\\)[ \t]*:[a-Z|]+\\|[ \t]*:[a-Z|]+[ \t]*\\(=\\|<>\\)\\)"
		    "\\|"
		    ;; regexp for character
		    "\\(\\(=\\|<>\\)[ \t]*%[^ \t\n]+\\|[ \t]*%[^ \t\n]+[ \t]*\\(=\\|<>\\)\\)"
		    "\\|"
		    ;; regexp for integer
		    "\\([^\\.\n][0-9][0-9]*[ \t]*\\(=\\|<>\\)\\|\\(=\\|<>\\)[ \t]*[0-9][^\\.]\\)"
		    "\\)"))
   (list 36 (concat "\\("
		    ;; regexp for floats
		    "\\(_is[nt]*[ \t]*[0-9]+\\.[0-9]+\\|[0-9]+\\.[0-9]+[ \t]*_is\\)"
		    "\\|"
		    ;; regexp for strings
		    "\\(_is[nt]*[ \t]*\"+\\|\"+[ \t]*_is\\)"
		    "\\)"))
   (list 37 "lowercase")
   (list 38 "^[ \t]*[^#\n]*\".+\"")
   (list 39 "\\(\".+\\+\\|\\+.+\"\\|write_string(\\)")
   (list 40 "\\(_or[ \t\n]+\\|_and[ \t\n]+\\)")
   (list 48 "\\.get_global[^_]+")
   (list 49 "\\.substitute_character([^_]+")
   (list 50 "\\.value[^_]+")
   )
  )

(defun msmeller-run-smell-check-p (smell-number)
  "returns 't' if smell-number is a runnable smell.  Otherwise returns 'nil'.  To be runnable, smell-number needs to correspond to a smell-priority that is within the values specified in msmeller-min-priority-level and msmeller-max-priority-level"
  (let ((smell-priority nil))
    (setq smell-priority (nth 0 (gethash smell-number msmeller-hash)))
    (and (>= smell-priority msmeller-min-priority-level) 
	 (<= smell-priority msmeller-max-priority-level))))

(defun msmeller-record-smell (smell-number &optional line-number)
  "Given smell-number, records the appropriate error message at the current Magik Smeller buffer location.  Also increments the smell counter"
  (let ((smell-info nil)
	(smell-priority nil)
	(short-title nil)
	(long-description nil)
	(heading nil)
	; if smell-number is not in the list of msmeller-processed-smells, that means we
        ; have encountered this smell for the first time and should therefore write out
					; some heading information
	(first-time (not (memq smell-number msmeller-processed-smells))))

    (setq smell-info (gethash smell-number msmeller-hash))
    (setq smell-priority (nth 0 smell-info))
    (setq short-title (nth 1 smell-info))
    (setq long-description (nth 2 smell-info))
    
    (if first-time
	(progn
	  (princ "\n")
	  (princ (setq heading (format "(%d) %s" smell-priority (upcase short-title))))
	  (princ "\n")
	  (princ (make-string (string-width heading) ?=))
	  (princ "\n")
	  
	  (if msmeller-show-description-p
	      (progn
		(princ long-description)
		(princ "\n")))
	  
	  (push smell-number msmeller-processed-smells)))
    
    (if (not line-number)
	(progn
	  (goto-char (match-beginning 0))
	  (beginning-of-line)
	  (setq prevpos (point))
	  (princ (what-line))
	  (princ ": ")
	  (princ (buffer-substring (point-bol) (point-eol)))
	  (goto-char (match-end 0))
	  )
      (if (stringp line-number)
	  (princ line-number)
	(princ (format "Line %d:" line-number))))
    
    (setq msmeller-score (+ msmeller-score smell-priority))
    (princ "\n"))
  )


(defun msmeller-rule-typos (buffer)
  (set-buffer buffer)
  (goto-char (point-min))
  
  (let ((prevpos (point-min))
	(regexp nil)
	(smell-number nil))
    (dolist (typo-info msmeller-typo-regexps)
      (setq smell-number (nth 0 typo-info))
      (setq regexp (nth 1 typo-info))
      (if (msmeller-run-smell-check-p smell-number)
	  (save-excursion
	    ;; Find next match, but give up if prev match was at end of buffer.
	    (while (and (not (= prevpos (point-max)))
			(re-search-forward regexp nil t))
	      
	      (msmeller-record-smell smell-number)
	      )
	    )
	)
      )
    )
  )
  
(defun msmeller-rule-loops (buffer)
  )

(defconst msmeller-file-regexps
  (list 
   (list 41 "^#% text_encoding *= *[a-Z]+")
   (list 42 "^_package *[a-Z]+")
   (list 43 "\\(^#.*copyright\\|^#.*(c)\\)")
   )
  )

(defun msmeller-rule-file (buffer)
  (set-buffer buffer)
  (goto-char (point-min))
  
  (let ((regexp nil)
	(smell-number nil))
    (dolist (file-info msmeller-file-regexps)
      (setq smell-number (nth 0 file-info))
      (setq regexp (nth 1 file-info))
      (if (msmeller-run-smell-check-p smell-number)
	  (save-excursion
	    ;; see if a match exists.
	    (if (re-search-forward regexp nil t) ()
	      (msmeller-record-smell smell-number 1)
	      )
	    )
	)
      )
    )
  )

(defun msmeller-rule-pragma (buffer)
  (set-buffer buffer)
  (goto-char (point-min))
  
  (let ((smell-number 44)
	(prevpos (point-min))
	(start-regexp "\\(\\(^\\| \\)_method\\|define_shared\\|define_slot_access\\|^def_slotted_exemplar\\|^_proc\\)")
	(stop-regexp "\\(^_pragma\\|_endmethod\\|^\\$\\|^_endproc\\)")
	(start-line nil)
	(stop-line nil)
	(line-number nil))
    (if (msmeller-run-smell-check-p smell-number)
	;; Find next match, but give up if prev match was at end of buffer.
	(while (and (not (= prevpos (point-max)))
		    (re-search-forward start-regexp nil t))
	  (setq start-line nil)
	  (setq line-number (what-line))
	  (setq start-line (buffer-substring (point-bol) (point-eol)))
	  (setq stop-line nil)
	  (save-excursion
	    (if (and (re-search-backward stop-regexp nil t)
		     (looking-at "^_pragma"))
		(setq stop-line (buffer-substring (point-bol) (point-eol)))))
	  
	  ;; if the current regexp 'hit' is after a #, then ignore this result.
	  (if (and (not (string-match "#" start-line))
		   (not stop-line))
	      (msmeller-record-smell smell-number (concat line-number ": " start-line))
	    )
	  )
      )
    )
  )

(defun msmeller-rule-method-too-long (buffer)
  "count the number of non-commented non-empty lines in the current method.  If it is greater than 'msmeller-method-too-long-size', it is considered unmaintainable."
  (set-buffer buffer)
  (goto-char (point-min))
  
  (let ((smell-number 45)
	(prevpos (point-min))
	(start-regexp "\\(^\\| \\)_method")
	(stop-regexp "_endmethod")
	(start-line nil)
	(stop-line nil)
	(line-number nil)
	(statement-line-count nil)
	(max-lines msmeller-method-too-long-size))
    (if (msmeller-run-smell-check-p smell-number)
	;; Find next match, but give up if prev match was at end of buffer.
	(while (and (not (= prevpos (point-max)))
		    (re-search-forward start-regexp nil t))
	  (setq line-number (what-line))
	  (setq start-line (buffer-substring (point-bol) (point-eol)))
	  (setq statement-line-count 0)
	  (save-excursion
	    (forward-line)
	    (while (not (looking-at stop-regexp))
	      
	      (if (not (looking-at "\\([ \t]*#\\|[ \t]*\n\\)")) 
		  (setq statement-line-count (1+ statement-line-count)))
	      (forward-line)
	      )
	    )
	  
	  (if (> statement-line-count max-lines)
	      (msmeller-record-smell smell-number (format "%s: %s  (# of lines=%d)" line-number start-line statement-line-count)))
	  )
      )
    )
  )
  

(defun msmeller-rule-bad-return (buffer)
  "Scans for the instance where >> was probably used to return from a method but really it only returns from a block."
  (set-buffer buffer)
  (goto-char (point-min))
  
  (let ((smell-number 46)
	(prevpos (point-min))
	(start-regexp ">>")
	(assign-regexp "\\(_proc\\|_if\\|_block\\)")
	(stop-regexp "\\(_end\\|_proc\\|_if\\|_block\\|_method\\)")
	(start-line nil)
	(stop-line nil)
	(line-number nil))

    (if (msmeller-run-smell-check-p smell-number)
	;; Find next match, but give up if prev match was at end of buffer.
	(while (and (not (= prevpos (point-max)))
		    (re-search-forward start-regexp nil t))
	  (setq start-line nil)
	  (setq line-number (what-line))
	  (setq start-line (buffer-substring (point-bol) (point-eol)))
	  (setq stop-line nil)
	  (save-excursion
	    (if (and (re-search-backward stop-regexp nil t)
		     (not (string-match "\\(<<\\|>>\\|_return\\)" (buffer-substring (point-bol) (point))))
		     (string-match assign-regexp (buffer-substring (point-bol) (point-eol))))
		(setq stop-line (buffer-substring (point-bol) (point-eol)))))
	  
	  ;; if the current regexp 'hit' is after a #, then ignore this result.
	  (if (and (not (string-match "#" start-line))
		   stop-line)
	      (msmeller-record-smell smell-number (concat line-number ": " start-line)))
	  )
      )
    )
  )

(defun msmeller-rule-file-name-mismatch (buffer)
  (set-buffer buffer)
  (goto-char (point-min))
  
  (let ((smell-number 47)
	(prevpos (point-min))
	(class-name (nth 0 (split-string (nth 0 (reverse (split-string (buffer-name) "/"))) "\\.")))
	(start-regexp "\\(\\(^\\| \\)_method\\|\\.define_shared\\|def_slotted_exemplar\\)")
	(stop-regexp nil)
	(start-line nil)
	(stop-line nil)
	(line-number nil)
	)
    (setq stop-regexp (concat "\\(" class-name "\\.\\|:" class-name "\\)"))
    
    (if (msmeller-run-smell-check-p smell-number)
	
	;; Find next match, but give up if prev match was at end of buffer.
	(while (and (not (= prevpos (point-max)))
		    (re-search-forward start-regexp nil t))
	  (setq start-line nil)
	  (setq line-number (what-line))
	  (setq start-line (buffer-substring (point-bol) (point-eol)))
	  (setq stop-line nil)
	  (if (string-match stop-regexp start-line)
	      (setq stop-line (buffer-substring (point-bol) (point-eol))))
	  
	  ;; if the current regexp 'hit' is after a #, then ignore this result.
	  (if (and (not (string-match "#" start-line))
		   (not stop-line))
	      (msmeller-record-smell smell-number (concat line-number ": " start-line)))
	  )
      )
    )
  )


(defconst msmeller-rule-list 
  (list 'msmeller-rule-file
	'msmeller-rule-typos
	'msmeller-rule-loops
	'msmeller-rule-pragma
	'msmeller-rule-method-too-long
	'msmeller-rule-bad-return
	'msmeller-rule-file-name-mismatch)
  "A list of symbols that represent msmeller rules that are called by the Magik Smeller.")

(defun msmeller ()
  "Does a smell of the Magik code in the current buffer"
  (interactive)
  
  (setq msmeller-buffer (current-buffer))
  (setq msmeller-processed-smells (list))
  (setq msmeller-score 0)

  (if (not (string-equal (buffer-name (current-buffer)) "*Magik Smeller*"))
      (let ((buffer (current-buffer))
	    (regexp)
	    (statement-lines 0)
	    (score-string))
	(with-output-to-temp-buffer "*Magik Smeller*"
	  (save-excursion
	    (set-buffer standard-output)
	    (msmeller-mode)
	    
	    (princ "A Magik Smell is a piece of code that points to a possible coding problem.  \nTo jump to the code in the original file that a line number is referring to, place your cursor anywhere on the 'Line *:' line and press F3-j.\n\n")
	    
	    
	    (dolist (msmeller-rule msmeller-rule-list)
	      (funcall msmeller-rule buffer))
	    
	    (setq score-string (format "Final Smell Score: %d\n   Smell per line: %f\n\n" msmeller-score (/ (float msmeller-score) (count-lines (point-min) (point-max)))))
	    
	    )
	  )
	
	(set-buffer "*Magik Smeller*")
	(goto-char (point-min))
	(insert score-string)
	)
    )
  )



(defun msmeller-goto-line ()
  (interactive)
  (let ((line-number nil))
    
    (beginning-of-line)
     (re-search-forward "^Line [0-9]*:" nil t)

     (setq line-number (string-to-number (nth 1 (split-string (match-string 0) "[ :]+"))))
     (princ line-number)
     (pop-to-buffer msmeller-buffer)
     (goto-line line-number)
    )
  )


;; FONT-LOCK definitions

(defvar msmeller-font-lock-dynamic-face 'msmeller-font-lock-dynamic-face)
(make-face 'msmeller-font-lock-dynamic-face)
(copy-face 'font-lock-keyword-face 'msmeller-font-lock-dynamic-face)

(defcustom msmeller-font-lock-keywords
  '(
    ("^(0)" . msmeller-font-lock-high-priority)
    )
  "Expressions to highlight in msmeller mode.")


 (custom-set-faces
  '(msmeller-font-lock-high-priority ((t (:foreground "red"))))
  '(msmeller-font-lock-medium-priority ((t (:foreground "yellow"))))
  '(msmeller-font-lock-low-priority ((t (:foreground "green"))))
 )

(provide 'msmeller)

;;; msmeller.el ends here

(eval-after-load 'magik
  '(tool-bar-local-item "msmeller" 'msmeller 'msmeller magik-mode-map
			:help "Magik Smeller"))

