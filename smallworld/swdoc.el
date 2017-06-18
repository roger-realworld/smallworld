;;; swdoc.el -- Noddy Document Formatter

;;; Some very crude documentation facilities, such as pagination etc

(eval-and-compile (require 'cl))
(require 'utils-sw)

(defconst swdoc-version "$Revision: 1.15 $")

(defvar sw:page-length 75)     ;; length of body text (not headers etc)
                               ;; big font
(defvar sw:page-width 72)      ;; width of normal body text
(defvar sw:left-margin 13)      ;; width to globally offset text
(defvar sw:current-doc-title "")  ;; should be over-ridden in doc
(defvar sw:current-doc-date "")   ;;   ditto
(defvar sw:current-doc-id "")     ;;   ditto
(defvar sw:current-doc-author "") ;;   ditto

(defvar sw:list-mode nil)
(defvar sw:if-mode nil)
(defvar sw:bold-list nil)
(defvar sw:underline-list nil)
(defvar sw:doc-outline-regexp "\\(^[ \t]*[0-9]\\(.[0-9]\\)*.*\n[ \t]*-+\\)\\|\\(^[ \t]*[0-9]+\n[ \t]*\\)")

(defvar sw:auto-unformat-out-file t)

(or (assoc "\\.idoc$" auto-mode-alist)
    (push '("\\.idoc$" . sw:doc-mode) auto-mode-alist))

(or (assoc 'sw:list-mode minor-mode-alist)
    (push '(sw:list-mode " List") minor-mode-alist))   ;; mode for lists

(or (assoc 'sw:if-mode minor-mode-alist)
    (push '(sw:if-mode " Interface") minor-mode-alist))   ;; mode for interfaces

;;;###autoload
(defun sw:doc-mode ()
  "Mode for simple document formatting.

If you create your document as .idoc, you will get a standard header
block. Fill it in (fixing the date, which gets set wrong
sometimes). .idoc also turns on doc-mode in the buffer, to give
WYSIWYG type functionality.

Text typed into the buffer is now formatted to ~70 character column
width by default. This is \"normal\" format.

There are four possible formats. To select the one you want, precede
the text with the mode required (starting in column 1, on a line by
itself) exactly as follows:

.(sw:doc-normal)  - \"normal\" mode - what you are in now. Text will be realigned
		    to the ~70 character width when formatted (so if you add
		    something into the middle of a line, it will get properly
		    formatted later). 

.(sw:doc-list)  -   used for lists, expects four character indentation, so use 
		    <space><space>o<space> to bullet lists. If you also do 
                    M-x sw:list-mode at the same time, your typing
		    will follow the same pattern. M-x sw:list-mode is a toggle,
		    so just do it again to disable it.

.(sw:doc-literal) - disables normal formatting, but retains pagination and
		    page numbering. Essential for pictures, clever layouts
		    etc.

.(sw:doc-if)      - used for formatting C interfaces in a standard way, 
                    with a 7 character indentation. If you also do 
                    M-x sw:if-mode at the same time, your typing
		    will follow the same pattern. M-x sw:if-mode is a toggle,
		    so just do it again to disable it. Standard line
		    prefixes for if mode are Descr: (to describe the
		    interface), Notes: (describing other information),
		    and Excp: (describing exceptions).

Whatever format you select stays in force until you set another
one. If a paragraph gets unformatted as you modify it (eg because you
are adding text in the middle of lines), you can use M-Q to reformat
it so you can see now how it will look in the output. To reformat a region, 
use M-g.

If you put one blank line between paragraphs, and two between
sections, the paginator does a very good job separating things at
sensible places. However, if you want to force a page throw, just put
in C-L (ie throw page). [To insert C-L, you have to type C-Q
C-L]. Note that you also get improved layout if you put two spaces
after a full stop, rather than just one.

When you've typed in your document, you simply do M-x
sw:cformat-document to paginate it etc. It will overwrite the file
<docname>.out each time, but this shouldn't generally matter because
that's the output you want. The .out file will automatically turn up
in the \"other\" window. You can then check this for sensible pagination 
etc, fix the idoc if necessary and repeat till the output looks good. Two 
iterations, even on a large document, are usually sufficient.

To print the document, use print -q, which disables the standard header.

If you subsequently return to a .out file in Emacs, it will transform it 
in the buffer so that the indentation, pagination and underlining are
removed, making more screen readable. The result of this operation is
saved (if permitted) in a file with a .tout suffix, partly to save time 
(if there's an up to date .tout next time, this gets used without formality) 
and partly so it can be sent in Mail etc. This entire feature can be disabled
(or subsequently re-enabled) using M-x sw:unformat-out-file-mode, which is 
a toggle.


The idea is that the whole thing is pretty WYSIWYG, so you get a good
idea of what the document will look like, but it tidies and paginates
so the resulting document is neat and printable.

Underlining is supported in a printer-independent manner, by
surrounding the text to be underlined with {_ and _}. However, many
people prefer using a second row of --- instead (especially for
headings). If you want to do this, you should indent both the heading
and the underline by a single space, otherwise the formatter will glue
them together.

There is currently no support for automatic contents listing. However,
if you set the variable outline-regexp to the value
sw:doc-outline-regexp, outline-mode hide-body will pick out both
underlined headings (of the form 1, 1.1, 1.1.1 etc, underlined with
minus signs) and page-break page numbers, and the resulting text can
be manually massaged very simply to make a contents listing.

For people with poor eyesight, M-x sw:bformat-document (also, for historical
reasons known as sw:format-document) paginates suitably for printing using
print -bq (ie Big Letters). M-x sw:dformat-document historically formatted
with very small pages, for landscape printing with -dq. This does not seem
to work with the current printer."

  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'sw:doc-mode)
  (setq mode-name "Documentation")
  (make-local-variable 'paragraph-start)
  (setq paragraph-start "^[ \t\n\f]")
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate "^[ \t\f]*$")
  (make-local-variable 'default-fill-column)
  (make-local-variable 'fill-column)
  (setq default-fill-column sw:page-width)
  (setq fill-column sw:page-width)
  (make-local-variable 'sw:list-mode)
  (make-local-variable 'sw:if-mode)
  (setq sw:list-mode nil)
  (auto-fill-mode 1)
  (run-hooks 'sw:doc-mode-hook))

(defun sw:id (author date id title)
  (setq sw:current-doc-author author)
  (setq sw:current-doc-date date)
  (setq sw:current-doc-id id)
  (setq sw:current-doc-title title)
  (insert ".(sw:doc-normal)")		;sneakily insert first formatting command
  (newline 1)
  (previous-line 1))

(defun sw:copy-buffer ()
  "Copy current buffer verbatim to a new buffer (with \".out\" appended
   to it). Makes the new buffer current and returns old buffer"
  (let ((old-buffer (current-buffer))
        (bufnam (buffer-name (current-buffer))))
    (set-buffer 
     (get-buffer-create
      (concat;; add .doc as suffix
       (substring bufnam
                  0
                  (if (string-match "\\.idoc$" bufnam)
                      (- (length bufnam) 5) ;; if ends in .doc, strip
		    (length bufnam)         ;; else simply append
		    ))
       ".out")))

    (toggle-read-only 0)
    (erase-buffer);; in case the "new" buffer 
    ;; is left over from last time
    (insert-buffer-substring old-buffer)
    ;; return the old buffer
    old-buffer))

(defun sw:tidy-up (flag)  ;; flag says whether to display
  "Finish processing - make new buffer visible and write to disk"
  (if flag 
      (switch-to-buffer-other-window (current-buffer)))

  (goto-char (point-min))
  (write-file (buffer-name (current-buffer)))
  (sw:sort-out-tout-file))

(defun sw:sort-out-tout-file ()
  "Remove existing .tout file if present"
  (condition-case nil
      (let ((temp-file-name (concat (substring (buffer-file-name) 0 -3) "tout")))
	(if (sw:file-is-tout-file-p temp-file-name)
	    (delete-file temp-file-name)))
    (error nil)))

(defun sw:file-is-tout-file-p (filename)
  "Returns TRUE if file is a genuing .tout file"
  (if (file-readable-p filename)
      (save-excursion
	(set-buffer (find-file-noselect filename))
	(goto-char (point-min))
	(let ((result (re-search-forward "Author:.*\n\nDate:.*\n\n\\(Status:.*\n\n\\)?\\(\\(Topic\\)\\|\\(Id\\)\\):.*\n\nTitle:.*\n" 2000 t)))
	  (kill-buffer (current-buffer))
	  result))
    nil))
	    

(defun sw:repaginate ()
  "Repaginate the current buffer, taking account of user supplied forced
  page breaks"
  ;; First, muck about putting a page break at the beginning 
  ;; of the document, otherwise we get curious effects
  (mark-whole-buffer)
  (untabify (point) (mark))
  (goto-char (point-max))
  (if (string-equal (buffer-substring (1- (point)) (point)) "")
      nil
    (insert ""))

  (goto-char (point-min))
  (if (string-equal (buffer-substring (point) (1+ (point))) "")
      (forward-char)
    (insert "")
    (newline))

  ;; now loop through between forced page breaks, breaking each chunk
  ;; into pages page-length long

  (while (not (eq (point) (point-max)))
    (mark-page)
    ;;   (dotimes (i (/ (- (count-lines (point) (mark)) 3) sw:page-length))
    (let ((end (mark)))
      (forward-line (1+ sw:page-length))
      (let ((curpos (point));; remember current position
	    (pos1 (progn (forward-line -5) (point))) ; pos 5 lines back
	    (pos2 (progn (forward-line -5) (point))) ; pos 10 lines back
	    )
	(goto-char curpos)
	(if (< curpos end)
	    (progn 
	      (if (re-search-backward "^[ \t]*\n[ \t]*$" pos2 t) ; look for 2 blank lines
		  nil			                 ; stay there
		(re-search-backward "^[ \t]*$" pos1 t) ; try for one blank line
		)
	      (insert "")
	      (newline)
	      (backward-char))
	  (goto-char end)))))

  ;; we are within a page... fill the end of it with newlines as necc
  ;; so footer will be in the right place
  (goto-char (point-min))
  (forward-line 1)
  (while (not (eq (point) (point-max)))
    (mark-page)
    (let 
	((left-to-be-filled 
	  (+ (- sw:page-length (count-lines (point) (mark))) 2)))
      (search-forward "\C-l" nil 1) 
      (beginning-of-line)
      (newline left-to-be-filled)
      (search-forward "\C-l" nil 1)))

  ;; now remove any pages which are wholly blank, then remove leading
  ;; and trailing throws
  (goto-char (point-min))
  (while (re-search-forward "[ \t\n]*" nil t)
    (replace-match "" t t))

  (goto-char (point-max))
  (backward-char 1) (beginning-of-line) 
  (kill-line);; remove trailing page throw
  (goto-char (point-min))
  ;; remove leading page throw
  (delete-char 2))

(defun sw:header (page-no)
  "Function which defines the header. Format and content are entirely
   unprogrammable - ie they are cast in this piece of code"
  (newline)
  (insert 
   ;; simply construct a left/empty-middle/right layout
   (concat sw:current-doc-title
	   (make-string (- sw:page-width
			   (+ (length sw:current-doc-title)
			      (length sw:current-doc-date))
			   ) 32)   ;; 32 = ASCII space!!!
	   sw:current-doc-date))
  (newline 2))

(defun sw:footer (page-no)
  "Function which defines the footer. Format and content are entirely
   unprogrammable - ie they are cast in this piece of code"
  (newline 2)
  (insert  "Smallworld Confidential")
  (center-line)
  (newline)
  (insert (number-to-string page-no))
  (center-line))
  
(defun sw:do-headers-and-footers ()
  "Run through a paginated document, putting headers and footers in
   the right places"
  (goto-char (point-min))
  (let ((page-no 1))
    (while (not (eq (point) (point-max)))
      (sw:header page-no) (newline)
      (search-forward "\C-l" nil 1) (beginning-of-line)
      (sw:footer page-no) (newline) (next-line 1)
      (incf page-no 1))))

(defun sw:document-id ()
  "Function to put a standard identification block at the start of
   the document"
  (goto-char (point-min))
  (newline 3)
  (insert (concat "Author:       " sw:current-doc-author))
  (newline 2)
  (insert (concat "Date:         " sw:current-doc-date))
  (newline 2)
  (insert (concat "Topic:        " sw:current-doc-id))
  (newline 2)
  (insert "Title:        ") 
  ;;   (set-mark-command (point)) 
  (insert sw:current-doc-title)
  ;;   (underline-region (mark) (point))
  (newline 3))

(defun sw:eval-sexps ()
  "A function which scans a buffer looking for lines beginning \".(\"
   and executes them as lisp expressions. The value of point when the
   expression is executed is the first character of the line 
   immediately following the end of the expression"
  (goto-char (point-min))
  (while (re-search-forward "^\\.(" (point-max) t)
    (backward-char)
    (let ((current-point (point)))
      (forward-sexp)
      (let;; when found, suck out string, delete, then do
	  (( sexp (read-from-string 
		   (buffer-substring current-point (point)))))
	(backward-kill-sexp 1)
	(backward-delete-char 1)
	(kill-line);; so we don't get grotty blank lines
	;; GO FOR IT
	(eval (car sexp))))))

(defun sw:add-left-margin ()
  "Globally indent formatted document to give left margin for holes"
  (indent-rigidly (point-min) (point-max) sw:left-margin))

(defun sw:internal-format-document ()	; helper to do the work
  (sw:copy-buffer)
  (set-fill-column sw:page-width)
  (sw:bolden-part-1)
  (sw:underline-part-1)
  (sw:eval-sexps)
  (sw:document-id)
  (sw:repaginate)
  (sw:underline-part-2)
  (sw:bolden-part-2)
  (sw:do-headers-and-footers)
  (sw:add-left-margin)
  (sw:tidy-up t))


;;;###autoload
(defun sw:bformat-document ();; for BIG CHARACTER listings (the old default)
  "Format document for BIG characters, printed with -bq option"
  (interactive)
  (let ((sw:page-length 56)  ;; found by trial and error.
	(sw:page-width 60)
	(sw:left-margin 8))
    (sw:internal-format-document)))

;;;###autoload
(defun sw:cformat-document ()  ;; for compact listings
  "Format document for small character (compact) listings, printed with -q option"
  (interactive) 
  (let ((sw:page-length 75)   ;; found by trial and error.
	(sw:page-width 72)
	(sw:left-margin 13))
    (sw:internal-format-document)))
      

;;;###autoload
(defun sw:dformat-document ()  ;; for double listings
  "Format document for double listings"
  (interactive) 
  (let ((sw:page-length 44)    ;; found by trial and error.
	(sw:page-width 60)
	(sw:left-margin 8))
    (sw:internal-format-document)))


;;;###autoload
(defun sw:format-document () 
  "Format document for BIG characters, printed with -bq option"
  (interactive) 
  (sw:bformat-document))


;;;###autoload
(defun sw:list-mode ()
  "Toggle list mode on and off. In list mode, the paragraph
   edge is indented by 4, and your leading blob should be
   indented by 2"
  (interactive)
  (setq sw:list-mode (not sw:list-mode))
  (if sw:list-mode
      (setq fill-prefix "    "
	    fill-column (- sw:page-width 2))
    (setq fill-prefix nil
	  fill-column sw:page-width))

  ;; update mode line
  (set-buffer-modified-p (buffer-modified-p)))


;;;###autoload
(defun sw:if-mode ()
  "Toggle Interface mode on and off. In Interface mode, the paragraph
   edge is indented by 7"
  (interactive)
  (setq sw:if-mode (not sw:if-mode))
  (if sw:if-mode
      (setq fill-prefix "       ") 
    (setq fill-prefix nil))
  ;; update mode line
  (set-buffer-modified-p (buffer-modified-p)))


;;;###autoload
(defun sw:bolden-part-1 ()
  "remember where the emboldening goes as a list of markers"
  (interactive)
  (goto-char (point-min))
  (setq sw:bold-list ())
  (while (re-search-forward "{{\\([^}]*\\)}}" nil t) ; get inner matches
    (replace-match "\\1" t nil)
    (push (cons (set-marker (make-marker) 
			    (match-beginning 0) 
			    (current-buffer)) 
		(set-marker (make-marker) 
			    (- (match-end 0) 4)
			    (current-buffer))) 
	  sw:bold-list)))


;;;###autoload
(defun sw:bolden-part-2 ()
  "go back to the list of markers and do whatever needs doing"
  (interactive)
  (let ((boldp sw:bold-list))
    (while boldp
      (goto-char (marker-position (car (car boldp))))
      (insert "(s3B")
      (goto-char (marker-position (cdr (car boldp))))
      (insert "(s0B")
      (setq boldp (cdr boldp)))))


;;;###autoload
(defun sw:underline-part-1 ()
  "remember where the underlining goes as a list of markers"
  (interactive)
  (goto-char (point-min))
  (setq sw:underline-list ())
  (while (re-search-forward "{_\\([^}]*\\)_}" nil t) ; get inner matches
    (replace-match "\\1" t nil)
    (push (cons (set-marker (make-marker) 
			    (match-beginning 0) 
			    (current-buffer)) 
		(set-marker (make-marker) 
			    (- (match-end 0) 4)
			    (current-buffer))) 
	  sw:underline-list)))


;;;###autoload
(defun sw:underline-part-2 ()
  "go back to the list of markers and do whatever needs doing"
  (interactive)
  (let ((underlp sw:underline-list))
    (while underlp
      (underline-region (marker-position (car (car underlp)))
			(marker-position (cdr (car underlp))))
      (setq underlp (cdr underlp)))))

(defun sw:doc-normal ()
  (let ((curpos (point)))		; remember where we are
    (re-search-forward "^\.(" nil 1)	; look for next sexp
    (forward-line -1)			; go back one
    (setq fill-prefix nil)		; unset
    (setq fill-column sw:page-width)
    (fill-region (point) curpos)))

(defun sw:doc-list ()
  (let ((curpos (point)))
    (re-search-forward "^\.(" nil 1)
    (forward-line -1)
    (setq fill-prefix "    ") 
    (setq fill-column (- sw:page-width 2))
    (fill-region (point) curpos) ))

(defun sw:doc-literal ();; don't reformat
  (re-search-forward "^\.(" nil 1)  
  (forward-line -1))


(defun sw:doc-if ()
  (let ((curpos (point)))
    (re-search-forward "^\.(" nil 1)
    (forward-line -1)
    (setq fill-prefix "       ") 
    (setq fill-column sw:page-width)
    (fill-region (point) curpos)))

(defun sw:doc-mif ()
  (let ((curpos (point)))
    (re-search-forward "^\.(" nil 1)
    (forward-line -1)
    (setq fill-prefix "        ") 
    (setq fill-column sw:page-width)
    (fill-region (point) curpos)))

;;
;; Code for massaging a .out file so that it is more readable in Emacs. The
;; page-throws and headers and footers are removed, and the right indentation
;; is stripped. The result is stored in a file called <foo>.tout, which is 
;; automatically regenerated if it is out of date with respect to the .out 
;; file. The reason for retaining the munged form is (a) that the munging
;; takes some time to do, and (b) it could then be mailed or whatever elsewhere.
;;
;; All the automatic munging can be disabled by toggling using 
;;  M-x sw:unformat-out-file-mode
;;

(defun sw:unformat-out-file-mode ()
  "Toggle whether automatic unformatting of idoc .out files is enabled"
  (interactive)
  (if (setq sw:auto-unformat-out-file (not sw:auto-unformat-out-file))
      (message "Auto-unformatting of .out files now enabled")
    (message "Auto-unformatting of .out files disabled")))


(defun sw:unformat-document ()
  "Operates on the current buffer, which should formerly have been an sw;doc .out
file, removing indentation and pagination. The buffer is unhooked from the file
so the changes do not accidentally get written back"
  (interactive)
  (message "Formatting screen-readable form of .out file... ")
  (set-visited-file-name nil)
  (toggle-read-only 0)
  (goto-char (point-min))
  (while
      (re-search-forward "\\([^ \t]+[.?!:][ \t]*\n\\)?\\(\n*[ \t]*Smallworld Confidential\n[ \t]*[0-9]*\n[ \t]*\n\n.*\n*\\)\\([ \t]*[0-9.]*[ \t]*[A-Z][^.\n]+\n\\)?" nil t)
    (replace-match 
     (concat 
      (if (match-beginning 1)
	  (buffer-substring (match-beginning 1) (match-end 1)))
      "\n"
      (if (match-beginning 3)
	  (concat "\n" (buffer-substring (match-beginning 3) (match-end 3)))))))
  (indent-rigidly (point-min) (point-max) (sw:doc-deduce-indentation))
  (global-replace-regexp "_" "")
  (goto-char (point-min))
  (kill-line 7)
  (goto-char (point-max))
  (kill-line -3)
  (goto-char (point-min))
  (set-buffer-modified-p nil))

(defun sw:doc-deduce-indentation ()
  "Deduce the indentation in a .out file. This is assumed to be the first actual indentaion, and is returned as a negative value"
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^\\([ \t]+\\)\\([^ \t\n]+\\)" nil t)
	(- (progn (goto-char (match-beginning 1)) (current-column))
	   (progn (goto-char (match-end 1)) (current-column)))
      0)))


(defun sw:is-idoc-outfile-p ()
  "Returns TRUE if the current buffer looks like an idoc .out file"
  (and 
   sw:auto-unformat-out-file
   (buffer-file-name)
   (string-match "^.*\.out$" (buffer-file-name))
   (save-excursion
     (goto-char (point-min))
     (re-search-forward "\n.+\n*[ \t]*Author:.*\n\n[ \t]*Date:.*\n\n\\([ \t]*Status:.*\n\n\\)?[ \t]*\\(\\(Topic\\)\\|\\(Id\\)\\):.*\n\n[ \t]*Title:.*\n" 2000 t))))
  

(defun sw:doc-out-file-check ()
  "find-file hook which tests if the file being opened is an idoc .out file, and
if so reads the equivalent .tout file, if up to date, or makes a new one"
  (condition-case nil
      (if (sw:is-idoc-outfile-p)
	  (let* ((temp-file-name (concat (substring (buffer-file-name) 0 -3) "tout"))
		 (buffer-name (buffer-name))
		 (readable-p (file-readable-p temp-file-name)))
	    
	    (if (and readable-p
		     (file-newer-than-file-p temp-file-name (buffer-file-name)))
		(progn 
		  (toggle-read-only 0)
		  (set-visited-file-name nil)
		  (delete-region (point-min) (point-max))
		  (insert-file-contents temp-file-name t)
		  (goto-char (point-min)))
	      
	      (progn
		(sw:unformat-document)
		(if (and (or (not readable-p)
			     (sw:file-is-tout-file-p temp-file-name))
			 (file-writable-p temp-file-name))
		    (write-file temp-file-name)
		  (set-visited-file-name temp-file-name))
		(rename-buffer buffer-name)
		(set-buffer-modified-p nil)))
	    (toggle-read-only 1)))
    (error nil)))


(add-hook 'find-file-hooks 'sw:doc-out-file-check)

(provide 'swdoc)
