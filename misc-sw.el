;;; misc-sw.el -- non-gis-specific utils.
;;
;; This file is used to normalise features found in different versions of FSF Emacs.
;;

;; DO NOT byte-compile this file!

(require 'cl)
(require 'macros-sw)
(require 'advice)

(defconst misc-sw-version "$Revision: 1.29 $")

;;XEmacs variable compatibility
(defvar mode-line-format-sym 'mode-line-format
  "Compatibility variable for FSF Emacs and XEmacs.")

(defvar mode-line-process-sym 'mode-line-process
  "Compatibility variable for FSF Emacs and XEmacs.")

(defvar mode-line-buffer-identification-sym 'mode-line-buffer-identification
  "Compatibility variable for FSF Emacs and XEmacs.")

(defvar menu-bar-update-hook-sym 'menu-bar-update-hook
  "Compatibility variable for FSF Emacs and XEmacs.")

;;Early Emacsen do not define some useful faces
(eval-after-load 'font-lock
  '(progn
     (defvar font-lock-doc-face       'font-lock-comment-face)
     (defvar font-lock-reference-face 'font-lock-keyword-face)))

;; imenu-progress-message macro before Emacs 22 referred to imenu-scanning-message variable
(eval-after-load 'imenu
  '(or (boundp 'imenu-scanning-message)
     (defvar imenu-scanning-message nil)))

;; define mode-line buffer keymap variable for early Emacsen
(or (boundp 'mode-line-buffer-identification-keymap)
    (set 'mode-line-buffer-identification-keymap nil))

;; Setup auto-coding-regexp-alist. Done here since it is a central setting.
(or (boundp 'auto-coding-regexp-alist)
    (defvar auto-coding-regexp-alist nil))
(or (assoc "text_encoding[ 	]*=[ 	]*utf8" auto-coding-regexp-alist)
    (push '("text_encoding[ 	]*=[ 	]*utf8" . utf-8) auto-coding-regexp-alist))
(or (assoc "text_encoding[ 	]*=[ 	]*iso8859_15" auto-coding-regexp-alist)
    (push '("text_encoding[ 	]*=[ 	]*iso8859_15" . iso-8859-15) auto-coding-regexp-alist))
(or (assoc "text_encoding[ 	]*=[ 	]*iso8859_1" auto-coding-regexp-alist)
    (push '("text_encoding[ 	]*=[ 	]*iso8859_1" . iso-8859-1) auto-coding-regexp-alist))

;; Add all Windows 'cp\d+' codepages
(let ((codepages
       (delq nil
      (mapcar '(lambda (cp)
		 (save-match-data
		   (let ((s (symbol-name cp)))
		     (if (string-match "^cp[0-9]+$" s)
			  s))))
	      coding-system-list)))
      cpage text_encoding)
  (while codepages
    (setq cpage (car codepages)
	  codepages (cdr codepages)
	  text_encoding (concat "text_encoding[ 	]*=[ 	]*" cpage))
    (or (assoc text_encoding auto-coding-regexp-alist)
	(push (cons text_encoding (intern cpage)) auto-coding-regexp-alist))))


(defadvice file-truename (after convert-to-backslash)
  "Replace / with \\ to make a consistent filename on Windows.

This advice deliberately does not change case since sometimes it will
have the correct case and I wanted to ensure that this was retained.

It does not recognise similiar files accessed via a drive letter
and a UNC path that is mapped to that drive letter.

This advice only applies to Windows clients and is also only enabled if
either find-file-existing-other-name or find-file-visit-truename are t."
  (if (and (eq system-type 'windows-nt)
	   (or find-file-existing-other-name find-file-visit-truename))
      (setq ad-return-value (subst-char-in-string ?/ ?\\ ad-return-value))
    ad-return-value))

(defadvice substitute-in-file-name (before handle-%-variables activate compile)
  "Handle %VARIABLE% Environment variables."
  (let ((file (ad-get-arg 0)))
    (while (string-match "%\\([^%]+\\)%" file)
      (setq file
	    (replace-match (concat "${" (match-string-no-properties 1 file) "}") t t file)))
    (ad-set-arg 0 file)))

(fset 'delete-backward-char-untabify  ;get round typo in the original
      'backward-delete-char-untabify) ;template .emacs.

(defun-if-gnu-emacs mouse-scroll-screen (event)
  "Scroll a window by dragging the mouse."
  (interactive "e")
  (let*
      ((fn 'posn-col-row)               ; An indirection to stop the
                                        ; byte-compiler inlining.
                                        ; We have to do this because
                                        ; the function is different
                                        ; in 19.22 and 19.23.
       (pos1 (event-start event))
       (pos2 (event-end   event))
       (win1 (posn-window pos1))
       (row1 (+ (cdr (funcall fn pos1))
                (second (window-edges (posn-window pos1)))))
       (row2 (+ (cdr (funcall fn pos2))
                (second (window-edges (posn-window pos2)))))
       (orig-win (selected-window)))
    (select-window win1)
    (scroll-down (- row2 row1))
    (if (window-live-p orig-win)
        (select-window orig-win))))

(defun-if-gnu-emacs mode-line-resize (event)
  "Drag a mode line up and down, to resize the windows."
  (interactive "e")
  (let*
      ((fn 'posn-col-row)           ; An indirection (see comment above).
       (pos1 (event-start event))
       (pos2 (event-end   event))
       (win1 (posn-window pos1))
       (row1 (+ (cdr (funcall fn pos1))
                (second (window-edges (posn-window pos1)))))
       (row2 (+ (cdr (funcall fn pos2))
                (second (window-edges (posn-window pos2)))))
       (orig-win (selected-window)))
    (select-window win1)
    (if (eq row1 (- (frame-height) 2))
        (split-window-vertically (1+ (- row2 (second (window-edges)))))
      (enlarge-window (- row2 row1)))
    (if (window-live-p orig-win)
        (select-window orig-win))))

;; copied from the 19.31 subr.el because Emacs 19.25 doesn't have this function.
(defun-if (not (fboundp 'match-string))
  match-string (num &optional string)
  "Return string of text matched by last search.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING."
  (if (match-beginning num)
      (if string
	  (substring string (match-beginning num) (match-end num))
	(buffer-substring (match-beginning num) (match-end num)))))

(defun-if (not (fboundp 'match-string-no-properties))
  match-string-no-properties (num &optional string)
  "Return string of text matched by last search, without text properties.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING."
  (if (match-beginning num)
      (if string
	  (let ((result
		 (substring string (match-beginning num) (match-end num))))
	    (set-text-properties 0 (length result) nil result)
	    result)
	(buffer-substring-no-properties (match-beginning num)
					(match-end num)))))
    
(defun-if (not (fboundp 'buffer-substring-no-properties))
  buffer-substring-no-properties (start end)
  "Return the characters of part of the buffer, without the text properties.
The two arguments START and END are character positions;
they can be in either order."
  (save-excursion
    (let ((string (buffer-substring start end))
	  (temp-buffer (set-buffer (get-buffer-create " *buffer-substring-no-properties*"))))
      (setq inhibit-read-only t)
      (erase-buffer)
      (insert string)
      (set-text-properties (point-min) (point-max) nil)
      (setq string (buffer-substring (point-min) (point-max)))
					;(kill-buffer temp-buffer)
      string)))

;; Emacs 21 provides an optional argument to add-to-list
(defun-if (or emacs19 emacs20)
  add-to-list (list-var element &optional append)
  "Add to the value of LIST-VAR the element ELEMENT if it isn't there yet.
The test for presence of ELEMENT is done with `equal'.
If ELEMENT is added, it is added at the beginning of the list,
unless the optional argument APPEND is non-nil, in which case
ELEMENT is added at the end.

If you want to use `add-to-list' on a variable that is not defined
until a certain package is loaded, you should put the call to `add-to-list'
into a hook function that will be run only after loading the package.
`eval-after-load' provides one way to do this.  In some cases
other hooks, such as major mode hooks, can do the job."
  (if (member element (symbol-value list-var))
      (symbol-value list-var)
    (set list-var
	 (if append
	     (append (symbol-value list-var) (list element))
	   (cons element (symbol-value list-var))))))

(defmacro-if (not (fboundp 'error-message-string))
  error-message-string (err)
  "Convert an error value (ERROR-SYMBOL . DATA) to an error message."
  (list 'car (list 'cdr err)))

(defun-if (not (fboundp 'functionp))
  functionp (object)
  "Non-nil if OBJECT is a type of object that can be called as a function."
  (or (subrp object) (byte-code-function-p object)
      (eq (car-safe object) 'lambda)
      (and (symbolp object) (fboundp object))))

(defun-if (not (fboundp 'subst-char-in-string))
  subst-char-in-string (fromchar tochar string &optional inplace)
  "Replace FROMCHAR with TOCHAR in STRING each time it occurs.
Unless optional argument INPLACE is non-nil, return a new string."
  (let ((i (length string))
	(newstr (if inplace string (copy-sequence string))))
    (while (> i 0)
      (setq i (1- i))
      (if (eq (aref newstr i) fromchar)
	  (aset newstr i tochar)))
    newstr))

(defun-if (not (fboundp 'rassoc))
  rassoc (elt lis)
  "[Slowly!] Returns non-nil if ELT is the cdr of an element of LIST.  Comparison done with equal.
The value is actually the element of LIST whose cdr is ELT."
  (while
      (and lis
	   (not (equal (cdr (car lis)) elt)))
    (pop lis))
  (car lis))

(defun-if (not (fboundp 'split-string))
  split-string (string &optional separators)
  "Splits STRING into substrings where there are matches for SEPARATORS.
Each match for SEPARATORS is a splitting point.
The substrings between the splitting points are made into a list
which is returned.
If SEPARATORS is absent, it defaults to \"[ \\f\\t\\n\\r\\v]+\".

If there is match for SEPARATORS at the beginning of STRING, we do not
include a null substring for that.  Likewise, if there is a match
at the end of STRING, we don't include a null substring for that."
  (let ((rexp (or separators "[ \f\t\n\r\v]+"))
	(start 0)
	notfirst
	(list nil))
    (while (and (string-match rexp string
			      (if (and notfirst
				       (= start (match-beginning 0))
				       (< start (length string)))
				  (1+ start) start))
		(< (match-beginning 0) (length string)))
      (setq notfirst t)
      (or (eq (match-beginning 0) 0)
	  (and (eq (match-beginning 0) (match-end 0))
	       (eq (match-beginning 0) start))
	  (setq list
		(cons (substring string start (match-beginning 0))
		      list)))
      (setq start (match-end 0)))
    (or (eq start (length string))
	(setq list
	      (cons (substring string start)
		    list)))
    (nreverse list)))

(defun-if (not (fboundp 'buffer-live-p))
  buffer-live-p (object)
  "Emulation of buffer-live-p, calls bufferp."
  (bufferp object))

(defun-if (not (fboundp 'plist-member))
  plist-member (plist prop)
  "Emulation of plist-member found in later Emacsen.
It actually only calls plist-get and so cannot distinguish between
properties that are defined but nil and properties that do not exist."
  (plist-get plist prop))

(defun-if (not (fboundp 'regexp-opt))
  regexp-opt (strings &optional paren)
  "Simple replacement for regexp-opt functionality found in newer Emacsen.
simply concatenates STRINGS list with \"\\\\|\""

  (let ((open-paren (if paren "\\\\(" ""))
	(close-paren (if paren "\\\\)" "")))
    (concat open-paren (mapconcat 'regexp-quote strings "\\\\|") close-paren)))

(or (fboundp 'x-focus-frame)
    (defalias 'x-focus-frame 'focus-frame))

(defun-if (not (fboundp 'select-frame-set-input-focus))
  select-frame-set-input-focus (frame)
  "Select FRAME, raise it, and set input focus, if possible."
    (select-frame frame)
    (raise-frame frame)
    ;; Ensure, if possible, that frame gets input focus.
    (when (eq window-system 'w32)
      (w32-focus-frame frame))
    (cond (focus-follows-mouse
	   (unless (eq window-system 'w32)
	     (set-mouse-position (selected-frame) (1- (frame-width)) 0)))
	  (t
	   (when (eq window-system 'x)
	     (x-focus-frame frame)))))

(defun-if (not (fboundp 'make-comint-in-buffer))
  make-comint-in-buffer (name buffer program &optional startfile &rest switches)
  "Make a comint process NAME in BUFFER, running PROGRAM.
If BUFFER is nil, it defaults to NAME surrounded by `*'s.
PROGRAM should be either a string denoting an executable program to create
via `start-process', or a cons pair of the form (HOST . SERVICE) denoting a TCP
connection to be opened via `open-network-stream'.  If there is already a
running process in that buffer, it is not restarted.  Optional third arg
STARTFILE is the name of a file to send the contents of to the process.

If PROGRAM is a string, any more args are arguments to PROGRAM."
  (or (fboundp 'start-process)
      (error "Multi-processing is not supported for this system"))
  (setq buffer (get-buffer-create (or buffer (concat "*" name "*"))))
  ;; If no process, or nuked process, crank up a new one and put buffer in
  ;; comint mode.  Otherwise, leave buffer and existing process alone.
  (unless (comint-check-proc buffer)
    (with-current-buffer buffer
      (comint-mode)) ; Install local vars, mode, keymap, ...
    (comint-exec buffer name program startfile switches))
  buffer)

(defun-if (not (fboundp 'directory-files-and-attributes))
  directory-files-and-attributes (directory &optional full match nosort)
  "Return a list of names of files and their attributes in DIRECTORY.
There are three optional arguments:
If FULL is non-nil, return absolute file names.  Otherwise return names
 that are relative to the specified directory.
If MATCH is non-nil, mention only file names that match the regexp MATCH.
If NOSORT is non-nil, the list is not sorted--its order is unpredictable.
 NOSORT is useful if you plan to sort the result yourself."
  (let ((files (directory-files directory full match nosort))
	f att)
    (while files
      (setq f (car files)
	    att (append att
			(list (cons f
				    (file-attributes
				     (if full
					 f
				       (concat (file-name-as-directory "/tools/general/emacs") f))))))
	    files (cdr files)))
    att))

(or (fboundp 'defvaralias)
    (defalias 'defvaralias 'ignore))

(defun-if (not (fboundp 'event-buffer))
  event-buffer (event)
  "Return the buffer of the window over which mouse event EVENT occurred.

Mirrored XEmacs Event function in FSF Emacs."
  (window-buffer (posn-window (event-start event))))

(defun-if (not (fboundp 'event-x))
  event-x (event)
  "Return the X position of the mouse event EVENT in characters.
This is relative to the window the event occurred over.

Mirrored XEmacs Event function in FSF Emacs."
  (car (posn-col-row (event-start event))))

(or (fboundp 'add-untranslated-filesystem)
    (defalias 'add-untranslated-filesystem 'ignore))

(or (fboundp 'custom-set-variables)
    (defalias 'custom-set-variables 'ignore))

(or (fboundp 'custom-initialize-value)
    (defalias 'custom-initialize-value 'setq-default))

(or (fboundp 'tool-bar-add-item)
    (defalias 'tool-bar-add-item 'ignore))

(or (fboundp 'tool-bar-local-item)
    (defalias 'tool-bar-local-item 'ignore))

;;; replace-match
;; Note replace match is a 'built-in' i.e. its source is in C code.
;;
;; Make earlier emacs versions accept 5 argument replace-match to allow
;; Smallworld code to load and be byte-compiled on earlier versions of Emacs.
;; This does mean that the places where the 5 argument version is used will not work on Emacs 19.
(if (and (or emacs19)
	 (not (fboundp 'replace-match-emacs19)))
    (let ((doc-string (documentation 'replace-match)))
      (fset 'replace-match-emacs19 (symbol-function 'replace-match))
      (fset 'replace-match
	    (list 'lambda '(NEWTEXT &optional FIXEDCASE LITERAL STRING SUBEXP)
		  doc-string
		  '(replace-match-emacs19 NEWTEXT FIXEDCASE LITERAL)))))

;; Some useful Elisp interfaces to Window functions
(defun-if (running-under-nt)
  w32-frame-restore (&optional frame)
  "Restore a frame."
  (interactive)
  (w32-send-sys-command ?\xf120 frame))

(defun-if (running-under-nt)
  w32-frame-move (&optional frame)
  "Move a frame."
  (interactive)
  (w32-send-sys-command ?\xf010 frame))

(defun-if (running-under-nt)
  w32-frame-size (&optional frame)
  "Change size of a frame. You probably want to use `set-frame-size' or `set-frame-width"
  (interactive)
  (w32-send-sys-command ?\xf000 frame))

(defun-if (running-under-nt)
  w32-frame-minimize (&optional frame)
  "Minimise a frame."
  (interactive)
  (w32-send-sys-command ?\xf020 frame))

(defun-if (running-under-nt)
  w32-frame-maximize (&optional frame)
  "Maximise a frame."
  (interactive)
  (w32-send-sys-command ?\xf030 frame))

(provide 'misc-sw)

;;; misc-sw.el ends here
