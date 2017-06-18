;;; gis-filter.el -- deal with the output from the gis process.

;; The filter receives all the text coming back from magik.  Under
;; normal conditions, it just puts the text in the buffer.  However,
;; all text between a C-a character (ascii 1) and a C-e character are
;; intercepted and displayed in special ways according to the
;; character immediately following the C-a character.
;; 
;; The filter rotates between 3 states, which we indicate by the
;; string it is searching for in order to come out of that state.
;; 
;; It stays in `normal' state until it sees a C-a character.  Then it
;; sits in `special' state until it sees a C-e character.  Then it is
;; just waiting (in `prompt' state) for the space character, before it
;; goes back into `normal' state again.
;; 
;; Excess characters after the particular character it is looking for
;; are dealt with by calling ourselves recursively with the remainder
;; of the string.

(eval-when-compile (require 'cl)
		   (defvar comint-last-input-end) ;;avoid compiler warning
		   (require 'comint))

(require 'resources)
(require 'utils-sw)
(require 'cb)
(require 'magik)
(require 'gis)

(defconst gis-filter-version "$Revision: 1.11 $")

(defvar gis-filter-state "\C-a"
  "Either \"\\C-a\", \"\\C-e\, \"\\C-f\" or \" \".")

(defcustom gis-filter-action-alist nil
  "An alist that matches the different filter actions with the action character.
The alist is a cons cell of the form (CHARACTER . FUNCTION).
FUNCTION takes one argument, the string after the action character."
  :group 'gis
  :type '(repeat (cons (character :tag "Filter character")
		       (function :tag "Action function"))))

(defun gis-filter-get-state (buf)
  "Displays the state of the GIS filter."
  (save-excursion
    (set-buffer buf)
    gis-filter-state))

(defun gis-filter (proc str)
  "Deal with strings coming back from Magik."
  (save-match-data
    (let*
        ((buf (process-buffer proc))
         (n (string-match (gis-filter-get-state buf) str))
         (filter-buf (concat " *filter*" (buffer-name buf))))
      (cond ((equal (gis-filter-get-state buf) "\C-e")
	     (save-excursion
	       (set-buffer (get-buffer-create filter-buf))
	       (insert (if n (substring str 0 n) str))
	       (message resources-gis-filtering-magik (number-to-string (point-max))))
	     (if n
		 (condition-case err
		     (gis-filter-action
		      proc
		      (save-excursion
			(set-buffer (get-buffer-create filter-buf))
			(prog1
			    (buffer-string)
			  (erase-buffer))))
		   (error
		    (save-excursion
		      (set-buffer buf)
		      (setq n nil
			    str ""
			    gis-filter-state "\C-a")
		      (message "Error: %s" (error-message-string err)))))))
	    ((equal (gis-filter-get-state buf) "\C-a")
	     (gis-filter-insert buf proc n str))
	    (t
	     nil))
      ;; else if in " " or "\C-f" state then do nothing.
      (if n
          (progn
            (save-excursion
              (set-buffer buf)
              (setq gis-filter-state
                    (cdr (assoc gis-filter-state
                                '(("\C-a" . "\C-e")
                                  ("\C-e" . " ")
                                  ("\C-f" . "\C-a")
                                  (" " . "\C-a"))))))
            (gis-filter proc (substring str (1+ n))))))))

(defun gis-filter-insert (buf proc n str)
  "insert into BUF at the process-mark of PROC, N chars from STR.
If N is nil insert the whole of STR.
We insert before all markers except the comint-last-input-end and the last command
from prev-gis-cmds."
  (save-excursion
    (set-buffer buf)
    (goto-char (process-mark proc))
    ;; we make sure that the end-marker for the last command typed by the user
    ;; (if there is one, else just the null initial command)
    ;; is not moved along by the `insert-before-markers' function.
    ;; Also the marker `comint-last-input-end' mustn't shift.
    ;;
    ;; BAD!!!  Evil bug: we mustn't assume that `buf' has a window!
    ;; BAD!!!            This was only put in so that the initial gis
    ;; BAD!!!            startup messages would show.  This is now fixed
    ;; BAD!!!            by simply inserting a welcome string in the buffer.
    ;; BAD!!! Also, we must make sure that any window start markers remain pinned to the
    ;; BAD!!! start of the line!
    (let*
        ((b (car (aref prev-gis-cmds (max 0 (- no-of-gis-cmds 2)))))
         (e (cdr (aref prev-gis-cmds (max 0 (- no-of-gis-cmds 2)))))
         (b-pos (marker-position b))
         (e-pos (marker-position e))
         (comint-last-input-end-pos (marker-position comint-last-input-end))
	 (pt (point))
         ;; BAD!!! (w (get-buffer-window buf))
         )
      (insert-before-markers (if n (substring str 0 n) str))
      (set-marker b b-pos)  ;shouldn't really be necessary
      (set-marker e e-pos)
      (set-marker comint-last-input-end comint-last-input-end-pos)
      ;; BAD!!! (goto-char (window-start w))
      ;; BAD!!! (beginning-of-line)
      ;; BAD!!! (set-window-start w (point) t)
      (save-restriction
	(save-match-data
	  (narrow-to-region pt (point))
	  (goto-char (point-min))
	  (while (re-search-forward (concat "^\\*\\*\\*\\*.*" "on line" " \\([0-9]+\\)") nil t)
	    (add-text-properties (match-beginning 0) (match-end 0)
				 (list 'mouse-face 'highlight
				       'help-echo "mouse-2: Goto error in file"
				       'local-map gis-mode-error-map)))))
	   
    )))

(defun toggle-gis-filter (&optional buffer)
  "Toggle the filter on the GIS buffer.

With a prefix arg, ask user for GIS buffer to use."
  (interactive)
  (setq buffer (sw-get-buffer-mode buffer
				   'gis-mode
				   resources-gis-enter-buffer
				   gis-buffer
				   'gis-buffer-alist-prefix-function))
  (barf-if-no-gis buffer)

  (let ((process (get-buffer-process buffer)))
    (if (process-filter process)
	(progn
	  (set-process-filter process nil)
	  (message resources-gis-filter-cancelled buffer))
      (save-excursion
	(set-buffer (get-buffer-create (concat " *filter*" buffer)))
	(erase-buffer)
	(set-buffer buffer)
	(setq gis-filter-state "\C-a")
	(set-process-filter process 'gis-filter))
      (message resources-gis-filter-set buffer))))

(defun gis-filter-action (proc str)
  "Deal with STR back from Magik according to `gis-filter-action-alist'."
  (let* ((s (substring str 0 1))
	 (minibuffer-window (minibuffer-window))
	 (minibuffer-active-p (and minibuffer-window
				   (minibuffer-window-active-p minibuffer-window)))
	 fn)

    ;;Protect against filter running with minibuffer window active.
    (if minibuffer-active-p
	(set-buffer (process-buffer proc)))

    (setq str (substring str 1)
	  fn (cdr (assoc s gis-filter-action-alist)))
    (if fn
	(progn
	  (funcall fn proc str)
	  (and minibuffer-active-p (select-window minibuffer-window)))
      (and minibuffer-active-p (select-window minibuffer-window))
      (error resources-gis-filter-char-error s))))

(defun gis-filter-register-action (string function &optional replace)
  "Registers the GIS filter STRING with execution of FUNCTION.
STRING is normally a single character.
If STRING has already been registered then an error is raised unless
REPLACE is t. If REPLACE is t then the FUNCTION will replace the old
action's function setting."
  (let ((action (assoc string gis-filter-action-alist)))
    (cond ((not action)
	   (add-to-list 'gis-filter-action-alist (cons string function)))
	  (replace
	   (setcdr action function))
	  (load-file-name ;avoid message when reloading library
	   nil)
	  (t
	   (message resources-gis-filter-char-set string))))) 

;;; generic gis-filter code ends here.

;;; Set up filter action functions for the gis process
(defun gis-filter-action-completion (proc str)
  "Gis Filter Action interface for a magik symbol completion according to STR returned from Magik."
  (let ((ans (read str))
	(curr-word-len (length (sw-curr-word))))
    (cond
     ((eq (length ans) 0)
      (message resources-gis-filter-complete-no-match (sw-curr-word)))
     ((eq (length ans) 1)
      (if (eq (length (car ans)) curr-word-len)
          (message resources-gis-filter-complete-sole)
        (insert (substring (car ans) curr-word-len))
        (if (<= curr-word-len 2)
            (message resources-gis-filter-complete-done))))
     (t
      (let*
          ((longest-common-prefix (car ans))
           (len (length longest-common-prefix))
           (strings (cdr ans))
           i)
        (while
            strings
          (setq i 0)
          (while
              (and (< i len)
                   (< i (length (car strings)))
                   (eq (aref (car strings) i) (aref longest-common-prefix i)))
            (incf i))
          (setq len i)
          (pop strings))
        (if (> len curr-word-len)
            (insert (substring longest-common-prefix curr-word-len len))
          (with-output-to-temp-buffer "*magik completions*"
            (display-completion-list ans))))
      (if (<= curr-word-len 2)
          (message resources-gis-filter-complete-done))))))


(defun gis-filter-action-deep-print (proc str)
  "Gis Filter Action interface for a deep print action according to the STR back from magik."
  (let ((buffer (concat "*deep print*" (buffer-name (process-buffer proc)))))
    (pop-to-buffer buffer) ;;Buffer should always exist since the only USer interface is via deep-print.
    (if (eq (string-to-char str) ?\n)     ; then this is a sub-structure
	(progn
	  (forward-line)
	  (setq str (substring str 1)))
      ;; else this is an initial structure.
      (erase-buffer))
    (insert str)))

(defun gis-filter-action-find-file (proc str)
  "(Deprecated) Gis Filter Action interface for find-file.
Find a file and goto a particular line number
STR is of the form 42:/bla/bla/foo.magik or
:/bla/bla/foo.magik which means don't jump to any
particular line number."
  (save-match-data
    (string-match ":" str)
    (let
	((beg (match-beginning 0)))
      (find-file-other-window (substring str (match-end 0)))
      (or (zerop beg)
	  (goto-line (string-to-number str))))))

(defun gis-filter-action-file-open (proc str)
  "Gis Filter Action interface for opening files in Emacs.
STR consists of newline separated KEY=VALUE pairs.
Recognised KEYs are:
  file        Name of file to open
  function    Elisp function to open file with (default \\[find-file])
  line        Line to jump to.
  column      Column to place the cursor on.
Search based keys:
  method      Method to search for in file.
  class       Class  to search for in file.
  search      String to search for in file.

function must return the buffer of the file.

method and class searching combine to a specific search
search is a generic search string. If given with method and class
the string will be searched for after the method and class search has completed.

line and column can be used together.
The behaviour is undefined if any search key and line or column are used."
  (save-match-data
    (let ((alist '((function . "find-file")))
	  val start-pt)
      ;;process KEY=VALUE pairs
      (dolist (i (split-string str "\n+"))
	(when (string-match "^\\([^=]+\\)=\\(.*\\)" i)
	  (setq alist (cons (cons (intern (match-string 1 i)) (match-string 2 i))
			    alist))))

      ;;Open file
      (set-buffer (funcall (intern (cdr (assq 'function alist))) (cdr (assq 'file alist))))
      
      ;;act on keys and values.
      (if (setq val (assq 'method alist))
	  (progn
	    (widen)
	    (goto-char (point-min))
	    (magik-goto-class-method (cdr val) (cdr (assq 'class alist)))
	    (setq start-pt (point))))
      (if (setq val (assq 'search alist))
	  (progn
	    (widen)
	    (goto-char (or start-pt (point-min))) ;;continue search from class.method?
	    (if (search-forward (cdr val) nil t)
	      (goto-char (match-beginning 0)))))
      (if (setq val (assq 'line alist))   (goto-line (string-to-number (cdr val))))
      (if (setq val (assq 'column alist)) (move-to-column (string-to-number (cdr val)))))))

(defun gis-filter-action-cb-mf (proc socketname)
  "Magik has started a method_finder and tells Emacs what the socketname is."
  (setq cb--mf-socket-synchronised socketname))

(defun gis-filter-action-cb-goto-method (proc str)
  "GIS Filter Action interface for cb-goto-method." 
  (cb-goto-method str nil))

(defun gis-filter-action-gis-prompt-set (proc str)
  "Gis Filter Action for setting `gis-prompt' variable."
  (save-excursion
    (set-buffer (buffer-name (process-buffer proc)))
    (setq gis-prompt str)
    (gis-prompt-update-font-lock)))

(defun gis-filter-action-eval-elisp (proc str)
  "Gis Filter Action that enables Magik to send Elisp code for Emacs to evaluate."
  (save-excursion
    (set-buffer (get-buffer-create
		 (generate-new-buffer-name ; Be ultra careful to avoid multiple sessions eval'ing Elisp code!!!
		  (concat "*Magik Elisp eval*" (buffer-name (process-buffer proc))))))
    (erase-buffer)
    (insert str)
    (condition-case err
	(eval-region (point-min) (point-max))
      (error ;convert errors to messages
       (message (error-message-string err))))
    (kill-buffer (current-buffer))))

(gis-filter-register-action "\n" 'gis-filter-action-completion)
(gis-filter-register-action "p"  'gis-filter-action-deep-print)
(gis-filter-register-action "f"  'gis-filter-action-find-file)

(gis-filter-register-action "s"  'gis-filter-action-cb-mf)
(gis-filter-register-action "j"  'gis-filter-action-cb-goto-method)

(gis-filter-register-action "P"  'gis-filter-action-gis-prompt-set)
(gis-filter-register-action "E"  'gis-filter-action-eval-elisp)
(gis-filter-register-action "F"  'gis-filter-action-file-open)

;;Note Magik Studio had reserved the e and v characters.

(provide 'gis-filter)
