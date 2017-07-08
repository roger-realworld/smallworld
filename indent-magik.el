;;; indent-magik.el -- calc. the indentation for a line of Magik code.

;;; The main entry point is `calc-magik-indent()'.
;; 
;;; There are also entry separate entry points for tokenising
;; bits of magik.
;;    tokenise-magik-region-no-eol-nor-point-min
;;    tokenise-magik-region-no-eol
;;    tokenise-magik-line
;; 

(eval-when-compile (require 'cl))
(require 'resources)
(require 'utils-sw)

(defconst indent-magik-version "$Revision: 1.10 $")

;;also defined in sw-electric.el
(defcustom magik-indent-level 8
  "*How much to indent each nested level"
  :group 'magik
  :type  'integer)

(defvar magik-electric-templates-methods) ;keep compiler happy
(defvar magik-stack)        ; a dynamic var.

(defvar magik-state-table nil
  "An alist of symbol and 256-vectors describing the lexical transitions when parsing Magik.

This is compiled by `init-magik-state-table()' from the
global constant, `magik-transitions'.")

(defconst magik-operator-precedences
  '(("highest-op" . 99)
    (">>" . 50) ("_return" . 50) ("_not" . 50) ("~" . 50) ("_private" . 50) ("_iter" . 50)
    ("." . 11)
    ("**" . 10)
    ("*" . 9) ("/" . 9) ("_div" . 9) ("_mod" . 9)
    ("+" . 8) ("-" . 8)
    ("<" . 7) ("<=" . 7) (">" . 7) (">=" . 7) ("_cf" . 7)
    ("=" . 6) ("~=" . 6) ("_is" . 6) ("_isnt" . 6)
    ("_and" . 5) ("_andif" . 5)
    ("_xor" . 4)
    ("_or" . 3) ("_orif" . 3)
    ("<<" . 2) ("^" . 2)
    ("," . 1)
    ("lowest-op" . 0)
    ("\n" . -1)
    ("$" . -2)
    ("point-min" . -3))
  "Association list of operators (as strings) with their precedences.")

(defconst magik-begins-and-ends
  '(("_if" . "_endif")
    ("_loop" . "_endloop")
    ("_lock" . "_endlock")
    ("_block" . "_endblock")
    ("_catch" . "_endcatch")
    ("_try" . "_endtry")
    ("_protect" . "_endprotect")
    ("_method" . "_endmethod")
    ("_proc" . "_endproc")
    ("{" . "}")
    ("[" . "]")
    ("(" . ")"))
  "An association list of Magik begin keywords (like \"_if\" and \"{\") and their partners.")

(defconst magik-ends-and-begins
  '(("_endif" . "_if")
    ("_endloop" . "_loop")
    ("_endlock" . "_lock")
    ("_endblock" . "_block")
    ("_endcatch" . "_catch")
    ("_endtry" . "_try")
    ("_endprotect" . "_protect")
    ("_endmethod" . "_method")
    ("_endproc" . "_proc")
    ("}" . "{")
    ("]" . "[")
    (")" . "("))
  "An association list of Magik end keywords (like \"_endif\" and \"}\" and their partners.")
    
(defconst magik-others-and-ends
  '(("_else" . "_endif")
    ("_elif" . "_endif")
    ("_then" . "_endif")
    ("_finally" . "_endloop")
    ("_protection" . "_endprotect")
    ("_when" . "_endtry")
    ;;("_with" . "_endtry")             ; we have to leave this out for now because
                                        ; it seems to confuse the leave with construct!
    ("_using" . "_endtry"))
  "An association list of Magik keywords like \"_else\" and their corresponding endings (like \"_if\").")

(defun skip-blank-lines-backward ()
  "Move back a line skipping white-space lines (lines with only spaces
tabs and comments).  Return t if we succeed."
  (if (eq (forward-line -1) -1)
      nil
    (while
        (and (looking-at "[ \t]*[#\n]")
             (zerop (forward-line -1))))
    (not (looking-at "[ \t]*[#\n]"))))

(defun calc-magik-indent ()
  "Return the appropriate indentation for the current line."
  (save-excursion
    (let*
        ((toks (tokenise-magik-line))
         (tok (car toks))
         (str (car tok))
         (pos (cdr tok))
         (len (length str)))

      (cond

       ((save-excursion
          (and (progn (back-to-indentation) (eq (following-char) ?#))
               (eq (forward-line -1) 0)
               (progn (back-to-indentation) (eq (following-char) ?#))
               (current-column))))

       ((or (not (save-excursion (skip-blank-lines-backward)))
            (equal str "$")
            (equal str "_endmethod")
            (assoc "_method" toks)
            (assoc "_private" toks))
        0)

       ((or (assoc str magik-ends-and-begins)
            (assoc str magik-others-and-ends))

        ;; We must find the `begin' token corresponding to this `end'
        ;; token.  We do this by searching back for any operator lower
        ;; than "highest-op" i.e. any operator at all.  Because
        ;; operators inside brackets or begin-end pairs don't count,
        ;; we will land on the operator just in front of the correct
        ;; begin token.

        (goto-char (+ pos len))
        (find-magik-operator-backwards "highest-op")
        (forward-magik-token)
        (current-column))

       (t
        (calc-magik-indent-normal))))))

(defun calc-magik-indent-normal ()
  "Return the appropriate indentation for the current line, assuming it is reasonably normal.
i.e. there is some previous non-blank line and this line doesn't start with something
like \"_endif\" or \"}\" or \"_then\"."
  (skip-blank-lines-backward)
  (let*
      ((toks (tokenise-magik-line))
       (last-tok (car (last toks)))
       (last-str (car last-tok))
       (last-pos (cdr last-tok))
       (penul-tok (nth (- (length toks) 2) toks))
       (penul-str (car penul-tok))
       (penul-pos (cdr penul-tok)))

    (cond
     ((or (equal (car (car toks)) "$")
          (equal (car (car toks)) "_endmethod"))
      0)

     ((equal (car (first  toks)) "def_slotted_exemplar")
;      (goto-char (cdr (third toks))) ;third is first REAL token of def_slotted_exemplar command
;      (current-column)
      magik-indent-level)

     ((assoc (car (third toks)) magik-electric-templates-methods)
      magik-indent-level)
     ((and (equal last-str "\n")
           (equal penul-str "("))
      (goto-char penul-pos)
      (skip-chars-backward "a-z0-9A-Z._!?")
      (+ (current-column) magik-indent-level))

     ((and (equal last-str "\n")
           (equal penul-str "_then")
           (or (equal (caar toks) "_if") (equal (caar toks) "_elif")))
      (goto-char (cdr (first toks)))
      (+ (current-column) magik-indent-level))

     ;; Indent " _try _with ..."
					;This does not work if _try is first token in a file
					;because toks variable then includes the point-min token.
					;However, since this is rare, I have not attempted to
					;resolve it.
     ((and (equal last-str "\n")
           (equal (caadr toks) "_with")
           (equal (caar toks) "_try"))
      (goto-char (cdr (first toks)))
      (+ (current-column) magik-indent-level))

     ;; Indent " _when ..."
     ((and (equal last-str "\n")
           (equal (caar toks) "_when"))
      (goto-char (cdr (first toks)))
      (+ (current-column) magik-indent-level))


     ((and (equal last-str "\n")
           (or (assoc penul-str magik-begins-and-ends)
               (assoc penul-str magik-others-and-ends)))

      ;; just indent.
      (goto-char penul-pos)
      (+ (current-column) magik-indent-level))

     (t
      ;; Else, there are two cases: either this line (the one before
      ;; the one we're trying to indent) ends in an operator or it
      ;; doesn't.  If it does, then we must line up with a previous
      ;; lower-precedence operator.  If it doesn't, then we line up
      ;; with the start of this (possibly multi-line) statement.  In
      ;; either case, we are searching back for an operator 'cos a
      ;; statement-separating newline counts as an operator.  The
      ;; "lowest_op" operator is an imaginary operator just above the
      ;; statement-separator operator.

      (goto-char last-pos)
      (let
          ((found-tok (find-magik-operator-backwards (if (equal last-str "\n")
                                                         "lowest-op"
                                                       last-str))))
        (if (and (member (car found-tok) '("_proc" "_method" "_iter" "_private" "_abstract"))
                 (equal last-str "\n"))
            (+ (current-column) magik-indent-level)
          (if (or (not (member (car found-tok) '("_if" "_elif")))
                  (not (equal last-str "\n")))
              (forward-magik-token))
          (if (and (equal (car found-tok) "\n")
                   (not (equal last-str "\n")))
              (+ (current-column) magik-indent-level)
            (current-column))))))))

(defun find-magik-operator-backwards (operator-arg)
  "Search backwards from point for an operator with a lower precedence
than OPERATOR-ARG.  Return the token.  Take into account brackets (and
foo/endfoo pairs) so that things inside brackets are hidden from the
search.  An unmatched open-bracket (or begin-type keyword)
automatically qualifies.  Statement separation and beginning-of-buffer
are treated as the lowest level-operators."
  (let
      ((toks (reverse (tokenise-magik-region (point-bol) (point))))
       (magik-stack nil))  ; stack of ends looking for begins.
    (if (equal (car (car toks)) "\n")
        (pop toks))
    (while
        (and (progn
               (while
                   (and toks
                        (not (found-the-magik-operator (car toks) operator-arg)))
                 (pop toks))
               (null toks))             ;exit if there are tokens left.  i.e. if we
                                        ;exited the inner loop 'cos we found what we
                                        ;were looking for rather than because we ran
                                        ;out of tokens.
             (skip-blank-lines-backward))
      (setq toks (reverse (tokenise-magik-line))))

    ;; special fix for abstract, private, iterator methods etc.
    (if (member (caar toks) '("_method" "_procs"))
        (let
            ((t2 (car (second toks)))
             (t3 (car (third toks)))
             (t4 (car (fourth toks))))
          (cond
           ((and (equal t4 "_abstract") (equal t3 "_private") (equal t2 "_iter"))
            (goto-char (cdr (fourth toks)))
            (fourth toks))
           ((and (equal t3 "_private") (equal t2 "_iter"))
            (goto-char (cdr (third toks)))
            (third toks))
           ((and (equal t3 "_abstract") (equal t2 "_iter"))
            (goto-char (cdr (third toks)))
            (third toks))
           ((and (equal t3 "_abstract") (equal t2 "_private"))
            (goto-char (cdr (third toks)))
            (third toks))
           ((member t2 '("_abstract" "_private" "_iter"))
            (goto-char (cdr (second toks)))
            (second toks))
           (t
            (car toks))))
      (car toks))))

(defun found-the-magik-operator (tok operator-arg)
  "See if the token, TOK, is an `exposed' operator with lower precedence than the
operator, OPERATOR-ARG or an extra begin keyword or left bracket.  Exposed means not
inside brackets or a foo/endfoo pair.  This function operates on the dynamically
imported list, `magik-stack': endfoo keywords and right brackets are pushed onto the stack
and corresponding begin keywords and left brackets are popped off the stack."
  (let*
      ((str (car tok))
       (pos (cdr tok)))
    (cond
     ((assoc str magik-ends-and-begins)  ; e.g. "_endif"
      (push str magik-stack)
      nil)
     ((assoc str magik-begins-and-ends)   ; e.g. "_if"
      (cond
       ((null magik-stack)
        (goto-char pos)
        t)
       ((equal (cdr (assoc str magik-begins-and-ends)) (car magik-stack))
        (pop magik-stack)
        nil)
       (t
        (error resources-magik-expecting-token-error
               (car magik-stack)
               (cdr (assoc str magik-begins-and-ends))))))

     ((assoc str magik-others-and-ends)  ; e.g. "_else" or "_then"
      (cond
       ((and (null magik-stack)
             (equal operator-arg "highest-op"))
        (push (cdr (assoc str magik-others-and-ends)) magik-stack)
        nil)
       ((null magik-stack)
        (goto-char pos)
        t)
       ((equal (cdr (assoc str magik-others-and-ends)) (car magik-stack))
        nil)
       (t
        (error resources-magik-expecting-token-error
               (car magik-stack)
               (cdr (assoc str magik-others-and-ends))))))
     ((and (assoc str magik-operator-precedences)
           (null magik-stack)
           (< (cdr (assoc str          magik-operator-precedences))
              (cdr (assoc operator-arg magik-operator-precedences))))
      (goto-char pos)
      t)
     (t
      nil))))

(defun forward-magik-token ()
  "Move point forward to the beginning of the next Magik token.  End of line tokens don't count."
  (let
      ((toks (tokenise-magik-region-no-eol (point) (point-eol))))

    (if (> (length toks) 1)

        ;; there is another token on this line, so goto there.
        (goto-char (cdr (second toks)))
      (while
          (and
            (eq (forward-line) 0)
            (null (setq toks (tokenise-magik-line-no-eol-nor-point-min)))))
      (if toks
          (goto-char (cdr (car toks)))))))

;; T O K E N I S A T I O N
;; _______________________
;; 
(defun tokenise-magik-line ()
  "Return a list of the token-string and token-position pairs in the current line.
Don't include comments and trailing @foo tokens.  Add a newline token unless the last token
is an operator."
  (tokenise-magik-region (point-bol) (point-eol)))

(defun tokenise-magik-region (start end)
  "Return a list of the token-string and token-position pairs between START and END.
Don't include comments and trailing @foo tokens.  Add a newline token unless the last token
is an operator."
  ;;; We do this using a state machine parsing technique.
  ;;; We are also dropping the stack.  This is made possible by
  ;;; splitting the bar and slash states in to the var-bar, sym-bar,
  ;;; at-bar, var-slash, sym-slash and at-slash states.
  ;;; We, therefore, don't need an 'up' transition any more.
  (save-excursion
    (goto-char start)
    (let
        ((ans nil)
         (arr (cdr (assq 'neutral magik-state-table)))
         token-start
         (state 'neutral)
         new-state
         (reached-the-end nil))
      (while
          (and (<= (point) end)
               (not reached-the-end))
	;;In multibyte mode, following-char will return a value > 256.
	;;In this case we normally set the character's state to be whatever the
	;;current state is unless the state is neutral in which case
	;;the character is probably unquoted so we raise an error.
	;;Otherwise the lisp goes into an infinite loop.
	;;I have tried setting up magik-state-table
	;; to have vectors of length 65536 but the memory usage is quite high.
	(cond ((<= (following-char) 256)
	       (setq new-state (aref arr (following-char))))
	      ((eq state 'neutral)
	       (error resources-magik-unquoted-multibyte-error
		      (number-to-string (point))))
	      (t
	       (setq new-state state)))
        (if
            (eq new-state 'stay)
            ()
          (cond
           ((eq new-state 'neutral)
            (push (cons (buffer-substring-no-properties token-start (point)) token-start)
                  ans))
           ((eq state 'neutral)
            (setq token-start (point))))
          (setq state new-state)
          (setq arr (cdr (assq state magik-state-table)))
          (if (eq state 'neutral)
              (backward-char)))
        (if (eq (point) (point-max))
            (setq reached-the-end t)
          (forward-char)))
      (backward-char)  ; 'cos we over-stepped in order to the last token.
      (if (and ans
               (eq (aref (car (car ans)) 0) ?@))
          (pop ans))
      (or (assoc (car (car ans)) magik-operator-precedences)
          (push (cons "\n" (point-eol)) ans))
      (setq ans (reverse ans))
      (if (not (skip-blank-lines-backward))
          (push (cons "point-min" (point-min)) ans))
      ans)))

(defun tokenise-magik-region-no-eol (start end)
  "Like `tokenise-magik-region()' but with end-of-line tokens chopped off."
  (let
      ((reverse_ans (reverse (tokenise-magik-region start end))))
    
    (if (equal (car (car reverse_ans)) "\n")
        (pop reverse_ans))
    (reverse reverse_ans)))

(defun tokenise-magik-region-no-eol-nor-point-min (start end)
  "Like `tokenise-magik-region()' but with end-of-line and beginning-of-buffer tokens taken out."
  (let
      ((ans (tokenise-magik-region-no-eol start end)))
    (if (equal (car (car ans)) "point-min")
        (pop ans))
    ans))

(defun tokenise-magik-line-no-eol-nor-point-min ()
  "Like `tokenise-magik-line()' but with end-of-line and beginning-of-buffer tokens taken out."
  (tokenise-magik-region-no-eol-nor-point-min (point-bol) (point-eol)))

(defconst magik-transitions
  '((neutral "#" comment
             "%" char
             "@" at
             "\"" string
             "_" keyword
             ":" sym
             "|" var-bar
             "a-zA-Z!?" var
             "0-9" num
             "->+*/;^=~" operator
             "<" chevron
             "()[]{}" bracket
             "^" boot
             "$" dollar
             "." dot
             t stay)
    (operator "->+*/;^=~" stay
              t neutral)
    (chevron "<" becomes-finish
             "=" le-finish
             t neutral)
    (becomes-finish t neutral)
    (le-finish t neutral)
    (bracket t neutral)
    (boot t neutral)
    (dot t neutral)
    (dollar t neutral)
    (num "0-9" stay   ; shouldn't matter about floats.
         t neutral)
    (at "|" at-bar
        "\\" at-backslash
        "a-zA-Z0-9_!?" stay
        t neutral)
    (var "|" var-bar
         "\\" var-backslash
         "a-zA-Z0-9_!?" stay
         t neutral)
    (sym "|" sym-bar
         "\\" sym-backslash
         "a-zA-Z0-9_!?" stay
         t neutral)
    (var-bar "|" var t stay)
    (at-bar  "|" at  t stay)
    (sym-bar "|" sym t stay)
    (var-backslash t var)
    (at-backslash  t at )
    (sym-backslash t sym)
    (keyword "a-zA-Z0-9_!?" stay
             t neutral)
    (char t char-finish)
    (char-finish t neutral)
    (string "\"" string-finish
            t stay)
    (string-finish t neutral)
    (comment t stay))
  "A description of the lexical state transitions in Magik.  This gets
compiled into a more efficient form by `init-magik-state-table()'.")

(defun init-magik-state-table ()
  "Initialise the global variable `magik-state-table' unless it has already
been initialised from the global variable, `magik-transitions'."
  (if (null magik-state-table)
      (progn
        (let
            ((defs magik-transitions))
          (dolist (d defs)
            (let*
                ((lis (reverse (cdr d)))
                 vec)
              (while lis
                (let
                    ((str (cadr lis))
                     (state (car lis)))
                  (if (eq str t)
                      (setq vec (make-vector 256 state))
                    (while
                        (not (equal str ""))
                      (if
                          (or (< (length str) 3)
                              (not (eq (string-to-char (substring str 1)) ?-)))
                          (progn
                            (aset vec (string-to-char str) state)
                            (setq str (substring str 1)))
                        (let
                            ((i   (string-to-char str))
                             (end (aref str 2)))
                          (while
                              (<= i end)
                            (aset vec i state)
                            (incf i)))
                        (setq str (substring str 3))))))
                (setq lis (cddr lis)))
              (push (cons (car d) vec) magik-state-table)))))))

(init-magik-state-table)

(provide 'indent-magik)

;;; indent-magik.el ends here
