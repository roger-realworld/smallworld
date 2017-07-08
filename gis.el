;;; gis.el -- mode for running a Smallworld GIS.
;;
;;; The filter for the gis process is in filter.el
;;
;; This is a new version of the gis-mode that uses a vector of marker
;; pairs rather than a list.  This allows us to move up and down the
;; array efficiently and also use things like binary search.
;;
;; Every hundred commands, a new bigger vector is created and the
;; invalid or degenerate previous commands are cleaned out - previous
;; commands are counted as degenerate if they point at a non-existent
;; buffer (cos the user killed the buffer and created a new one) or
;; have length zero (cos the user deleted the text the the markers
;; bounded).
;;
;; Unlike previous versions, the markers will mark the whole of the
;; text sent to the gis, including the dollar and the return.
;; 
;; Note that all previous commands need to be kept, not just the last
;; 20 or so because the markers are the only way we can know what the
;; commands were - looking for prompts and dollars is too unreliable.
;; 
;; Unlike shell-mode, we don't keep comint-last-input-start and
;; comint-last-input-end.  (??? I've changed last-input-* to
;; comint-last-input-* everywhere.  I hope this still works!)
;;
;; The previous commands are kept in a buffer local variable called
;; gis-prev-cmds.
;; 
;; Where possible, we try to allow more than one gis to be running.
;; This gets a bit tricky for things like transmit-method-to-magik
;; because they have to know where to send the magik to.  In order to
;; simplify this, we are getting rid of the variable,
;; magik-process-name, because it is a duplicate of gis-buffer.  We
;; also don't ever refer to the process by its name but always by its
;; buffer - this should save any confusion with gis process naming.
;;
;; We don't rely on the form of the prompt any more.  We just rely on
;; it ending in a space.  The only place where we need to be sure is in
;; the filter.
;; 
;; In this version of gis-mode, we don't have any automatic
;; indentation of magik-code.  The tab is just for inserting tabs and
;; nothing else.
;; 
;; During a sequence of M-p and M-n commands, the actual command
;; we're looking at is recorded in the buffer-local variable,
;; gis-cmd-num.
;; 
;; Unlike direct-gis-mode.el we keep the oldest command at the front.
;; This is fine because we can get to the end of a vector quickly.
;; We record how many commands are in our vector in the buffer-local
;; variable, no-of-gis-cmds.  To get rid of annoying edge
;; effects in going up and down the vector, we keep a pair of markers
;; that bound an empty bit of text at the end of the vector.
;;
;; Arbitrary decision: if a command is recalled by grabbing it with
;; the RET key, the gis-cmd-num is set to 0 (as if it had been
;; typed by hand) rather than the number of the command that was
;; recalled.
; shut compiler up
(eval-when-compile
  (require 'cl)
  (require 'comint)
  (defvar comint-last-input-start)
  (defvar comint-last-input-end)
  (defvar msb-menu-cond)
  ;;(defvar ac-sources)
  
  (require 'macros-sw)
  (require 'utils-sw)
  (require 'sw-help)
  (require 'indent-magik)
  (require 'magik)
  (require 'sw-electric)
  (require 'cb))

(require 'cl)
(require 'magik)
(or (boundp 'ac-sources) (setq ac-sources nil))

(defgroup gis nil
  "Running Smallworld GIS."
  :group 'smallworld
  :group 'processes)

(defconst gis-version "$Revision: 1.51 $")

(defcustom gis-buffer nil
  "*The default Smallworld session.
Used for switching to the first Smallworld session."
  :group 'gis
  :type '(choice string (const nil)))

(defcustom gis-buffer-default-name "*gis*"
  "*The default name of a Gis process buffer when creating new Smallworld sessions."
  :group 'gis
  :type 'string)

(defcustom gis-prompt nil
  "String or Regular expression identifing the default Magik Prompt.
If global value is nil, a GIS session will attempt to discover the current
setting of the Magik Prompt by calling `gis-prompt-get'."
  :group 'gis
  :type '(choice regexp (const nil)))

(defcustom gis-command-history-max-length 90
  "*The maximum length of the displayed `gis-command' in the SW -> GIS Command History submenu.
`gis-command' is a string of the form \"[DIRECTORY] COMMAND ARGS\"."
  :group 'magik
  :type  'integer)

(defcustom gis-command-history-max-length-dir (floor (/ gis-command-history-max-length 2))
  "*The maximum length of the displayed directory path in the SW -> GIS Command History submenu."
  :group 'magik
  :type  'integer)

(defcustom gis-recall-cmd-move-to-end nil
  "*If t, move the cursor point to the end of the recalled command.
This behaviour is available for \\[recall-prev-gis-cmd] and \\[recall-next-gis-cmd] only.
The default is nil, which preserves the original behaviour to leave
the cursor point in the same position.

The similar commands, \\[recall-prev-matching-gis-cmd] and \\[recall-next-matching-gis-cmd]
that use command string matching are not affected by this setting."
  :group 'gis
  :type 'boolean)

(defcustom gis-font-lock-prompt-face 'font-lock-type-face
  "*Font-lock Face to use when displaying the Magik Prompt."
  :group 'gis
  :type 'face)

(defcustom gis-font-lock-error-face 'font-lock-warning-face
  "*Font-lock Face to use when displaying Error lines."
  :group 'gis
  :type 'face)

(defcustom gis-font-lock-traceback-face 'font-lock-warning-face
  "*Font-lock Face to use when displaying Traceback lines."
  :group 'gis
  :type 'face)

(defcustom gis-font-lock-keywords
  (append
   magik-font-lock-keywords-1
   magik-font-lock-keywords-2
   (list
     '("^\\*\\*\\*\\* Error:.*$"      0 gis-font-lock-error-face t)
     '("^\\*\\*\\*\\* Warning:.*$"    0 font-lock-warning-face t)
     '("^---- traceback.* ----" . gis-font-lock-traceback-face)
     '("^@.*$"                . font-lock-reference-face)
     ;;gis-prompt entries are handled by gis-filter-set-gis-prompt-action
     ))
  "Additional expressions to highlight in GIS mode."
  :type 'sexp
  :group 'gis)

(defcustom gis-mode-hook nil
  "*Hook for customising GIS mode."
  :type 'hook
  :group 'gis)

(defcustom gis-start-process-pre-hook nil
  "*Hook run before starting the process."
  :type 'hook
  :group 'gis)

(defcustom gis-start-process-post-hook nil
  "*Hook run after starting the process."
  :type 'hook
  :group 'gis)

(defcustom gis-sentinel-hooks nil
"*Hooks to run after the gis process has finished.
Each hook is passed the exit status of the gis process."
  :type 'hook
  :group 'gis)

(defcustom gis-drag-n-drop-mode nil
  "Variable storing setting of \\[gis-drag-n-drop-mode].

To make this mode operate on a per-buffer basis, simply make
this variable buffer-local by putting the following in your .emacs

  (defvar gis-mode-hook nil)
  (defun gis-drag-n-drop-mode-per-buffer ()
    (set (make-local-variable 'gis-drag-n-drop-mode) gis-drag-n-drop-mode))
  (add-hook 'gis-mode-hook 'gis-drag-n-drop-mode-per-buffer)
"

;;Use of integers is a standard way of forcing minor modes on and off.
  :type '(choice (const :tag "On" 1)
		 (const :tag "Off" -1))
  :group 'gis)

(defcustom auto-gis-post-hook nil
  "*Hook run after \\[auto-gis] command."
  :type 'hook
  :group 'gis)

(defvar gis-buffer-alist nil
  "Alist storing GIS buffer name and number used for prefix key switching.")

(defvar gis-drag-n-drop-mode-line-string nil
  "Mode-line string to use for Drag 'n' Drop mode.")

(defvar gis-filter-state nil
  "State variable for the filter function.")

(defvar gis-mode-map (make-keymap)
  "Keymap for GIS command buffers")

(defvar gis-menu nil
  "Keymap for the GIS buffer menu bar")

(easy-menu-define gis-menu gis-mode-map
  "Menu for GIS mode."
  `(,resources-gis-menu
    [,resources-gis-menu-previous        recall-prev-gis-cmd           t]
    [,resources-gis-menu-next            recall-next-gis-cmd           t]
    [,resources-gis-menu-previous-match  recall-prev-matching-gis-cmd  :active t :keys "f2 p"]
    [,resources-gis-menu-next-match      recall-next-matching-gis-cmd  :active t :keys "f2 n"]
    "----"                 
    [,resources-gis-menu-fold            display-gis-history           :active t :keys "f2 up"]
    [,resources-gis-menu-unfold          undisplay-gis-history         :active t :keys "f2 down"]
    "----"
    [,resources-gis-menu-magik-template  explicit-electric-magik-space :active t :keys "f2 SPC"]
    [,resources-gis-menu-complete        magik-symbol-complete         :active t :keys "f4 f4"]
    [,resources-deep-print-menu-start    deep-print                    :active t :keys "f2 x"]
    "----"
    [,resources-gis-menu-tb-previous     gis-traceback-up              :active t :keys "f4 up"]
    [,resources-gis-menu-tb-next         gis-traceback-down            :active t :keys "f4 down"]
    [,resources-gis-menu-tb-print        gis-traceback-print           :active t :keys "f4 P, f2 ="]
    [,resources-gis-menu-tb-save         gis-traceback-save            :active t :keys "f4 S"]
    "----"
    [,resources-gis-menu-shell           gis-shell                     :active t :keys "f4 $"]
    [,resources-gis-menu-kill            gis-kill-process              :active (and gis-process
										    (eq (process-status gis-process) 'run))]
    (,resources-gis-menu-history)
    "---"
    (,resources-gis-menu-toggle
     [,resources-gis-menu-toggle-filter   toggle-gis-filter             :active t :keys "f2 f"
					  :style toggle :selected (let ((b (get-buffer-process
									    (current-buffer))))
								    (and b (process-filter b)))]
     [,resources-gis-menu-toggle-drag-n-drop gis-drag-n-drop-mode       :active t
					     :style toggle :selected gis-drag-n-drop-mode])
    [,resources-menu-sw-customize        gis-customize               t]
    [,resources-menu-sw-help             gis-help                    t]))

(defvar gis-mode-error-map (make-sparse-keymap)
  "Keymap for Jumping to error messages.")

(define-key gis-mode-error-map [mouse-2]  'gis-error-goto-mouse)
(sw-define-key gis-mode-error-map [C-return] 'gis-error-goto)

(defvar gis-process nil
  "The process object of the command running in the buffer.")

(defvar gis-current-command nil
  "The current `gis-command' in the current buffer.")

(defvar gis-exec-path nil
  "Stored value of `exec-path' when the GIS process was started.")
(make-variable-buffer-local 'gis-exec-path)

(defvar gis-process-environment nil
  "Stored value of `process-environment' when the GIS process was started.")
(make-variable-buffer-local 'gis-process-environment)

(defvar gis-cb-buffer nil
  "The Class browser buffer associated with the GIS process.")

(defvar no-of-gis-cmds nil
  "No. of commands we have sent to this buffer's gis including the
null one at the end, but excluding commands that have been spotted as
being degenerate.")

(defvar gis-cmd-num nil
  "A number telling us what command is being recalled.  Important for
M-p and M-n commands.  The first command typed is number 0.  The
current command being typed is number (1- no-of-gis-cmds).")

(defvar prev-gis-cmds nil
  "A vector of pairs of markers, oldest commands first.  Every time
the vector fills up, we copy to a new vector and clean out naff
markers.")

(defvar gis-history-length 20
  "The default number of commands to fold.")

(defvar gis-command-syntax-table nil
  "Syntax table in use for parsing quotes in gis-command.")

;; Create the syntax table
(if gis-command-syntax-table
    ()
  (setq gis-command-syntax-table (make-syntax-table))
  ;; Allow embedded environment variables in Windows %% and Unix $ or ${} formats
  (modify-syntax-entry ?$  "w"  gis-command-syntax-table)
  (modify-syntax-entry ?\{ "w"  gis-command-syntax-table)
  (modify-syntax-entry ?\} "w"  gis-command-syntax-table)
  (modify-syntax-entry ?%  "w"  gis-command-syntax-table)

  (modify-syntax-entry ?_  "w"  gis-command-syntax-table) ;make _ a word character for environment variable sustitution

  (modify-syntax-entry ?\' "\"" gis-command-syntax-table) ;count single quotes as a true quote
  (modify-syntax-entry ?\" "\"" gis-command-syntax-table) ;count double quotes as a true quote
  (modify-syntax-entry ?\\ "\\" gis-command-syntax-table) ;allow a \ as an escape character
  (modify-syntax-entry ?.  "w"  gis-command-syntax-table) ;(for filenames)

  ;; Special characters for Windows filenames
  (modify-syntax-entry ?:  "w"  gis-command-syntax-table)
  (modify-syntax-entry ?~  "w"  gis-command-syntax-table) ;(mainly for NT 8.3 filenames)
)

(defconst gis-command-default "[%HOME%] %SMALLWORLD_GIS%/bin/x86/runalias.exe swaf_mega"
  "The default value for gis-command.
It illustrates how Environment variables can be embedded in the command.
Also it neatly shows the three ways of referencing Environment variables,
via the Windows and Unix forms: %%, $ and ${}. All of which are
expanded irrespective of the current Operating System.")

;;Although still settable by the user via M-x set-variable,
;;it is preferred that gis-comand-history be used instead.
(defvar gis-command gis-command-default
  "*The command used to invoke the gis.  It is offered as the default
string for next time.")

(defcustom gis-command-history nil
  "*List of commands run by a GIS buffer."
  :group 'gis
  :type  '(choice (const nil)
		  (repeat string)))
(put 'gis-command-history 'permanent-local t)

(defcustom gis-kill-process-function 'sw-delete-process-safely
  "*The function used to terminate a Magik PROCESS in the GIS buffer.

`kill-process'   terminates the process but the process may tidy itself up
                 before exiting and so Emacs will not display the terminated
                 process message in the buffer until that is complete.

`delete-process' terminates the process and Emacs immediately displays the
                 process terminated message.

`quit-process'   Sends SIGQUIT signal if the OS implements it.
                 Not implemented on Windows OSes."
  :group 'gis
  :type  'function)

(defun gis-help ()
  "Display help on how to use the Gis Mode interface."
  (interactive)
  (sw-help-open sw-help-gis-id))

(defun gis-customize ()
  "Open Customization buffer for Gis Mode."
  (interactive)
  (customize-group 'gis))


(defun gis-prompt-update-font-lock ()
  "Update the Font-lock variable `gis-font-lock-keywords' with current `gis-prompt' setting."
  (let ((entry (list (concat "^" gis-prompt) 0 gis-font-lock-prompt-face t)))
    (if (member entry gis-font-lock-keywords)
	nil ;; Already entered
      (setq gis-font-lock-keywords (append gis-font-lock-keywords (list entry)))
      (if (fboundp 'font-lock-set-defaults)
	  (progn  ;; Emacs 20 and later font-lock mode post-process its variables 
	    (set 'font-lock-set-defaults nil)
	    (funcall 'font-lock-set-defaults))))))

(defun gis-prompt-get (&optional force-query-p)
  "If `gis-prompt' is nil, get the GIS session's command line prompt.
If interactive and a prefix arg is used then GIS session will be
queried irrespective of default value of `gis-prompt'"
  (interactive "P")
  (if (and (null force-query-p)
	   (stringp (default-value 'gis-prompt))) ;user has overridden setting
      (progn
	(setq gis-prompt (or gis-prompt ;user may have set a local value for it
			     (default-value 'gis-prompt)))
	(gis-prompt-update-font-lock))
    (process-send-string
     gis-process
     "_block 
        !terminal!.put(%x.from_value(1))
        !terminal!.put(%P)
        _if magik_rep.responds_to?(:prompt_generator)
        _then !terminal!.write(magik_rep.prompt_generator.invoke(\"Magik> \"))
        _else !terminal!.write(\"Magik> \")
        _endif
        !terminal!.put(%x.from_value(5))
        !terminal!.put(%space)
    _endblock\n$\n")))
(add-hook 'gis-start-process-post-hook 'gis-prompt-get)

(defun gis-shell ()
  "Start a command shell with the same environment as the current GIS process."
  (interactive)
  (require 'shell)
  (let ((process-environment (copy-list gis-process-environment))
	(exec-path (copy-list gis-exec-path))
	(buffer (concat "*shell*" (buffer-name)))
	(version (and (boundp 'gis-version-current) (symbol-value 'gis-version-current))))
    (make-comint-in-buffer "gis-shell"
			   buffer
			   "cmd" nil "/k"
			   (concat (getenv "SMALLWORLD_GIS") "\\config\\environment.bat"))
    (save-excursion
      (set-buffer buffer)
      (if (stringp version) (set 'gis-version-current version)))
    (switch-to-buffer-other-window buffer)))

(defun gis-parse-gis-command (command)
  "Parse the gis-command string taking care of any quoting
and return a list of all the components of the command."

  ;;Copy the string into a temp buffer.
  ;;Use the Emacs sexp code and an appropriate syntax-table 'gis-command-syntax-table'
  ;;to cope with quotes and possible escaped quotes.
  ;;forward-sexp therefore guarantees preservation of white within quoted regions.
  ;;However, I do some extra work to try and remove the surrounding quotes from the returned result
  ;;unless I am running on NT Emacs19, because the sub-process functionality is less sophisticated.

  (let ((temp-buf (get-buffer-create " *gis-command parser*"))
	(command-list)
	(leave-quotes (and (running-under-nt) emacs19))) ;leave surrounding quotes on NT Emacs19 
    (save-excursion
      (save-match-data
	(set-buffer temp-buf)
	(erase-buffer)
	(set-syntax-table gis-command-syntax-table)
	(insert command)
    
					;Remove excess trailing whitespace to avoid spurious extra empty arguments being passed
	(goto-char (point-max))
	(delete-horizontal-space)

	(goto-char (point-min))
	(condition-case var
	    (setq command-list
		  (loop
		   with start-char ;point containing valid word character - not whitespace or a quote 
		   with substr ;substring containing command-line argument
		   do (progn
			(setq start-char
			      (save-excursion
				(skip-chars-forward " \t") ;skip intervening white space
				(if leave-quotes
				    (point)
				  (and (looking-at "[\"\']") (forward-char 1)) ;strip begin-quote
				  (point))))
			(forward-sexp)
			(setq substr (buffer-substring start-char (point)))
			(and (not leave-quotes)
			     (string-match "[\"\']$" substr) ;strip end-quote if any
			     (setq substr (substring substr 0 (match-beginning 0))))
					;Now look for embeded environment variables
			(setq substr (substitute-in-file-name substr)))
		   collect substr
		   until (eobp)))
	  (scan-error
	   (error resources-gis-scan-error (cadr var)))))
      (kill-buffer temp-buf)
      command-list)))

(defun gis-buffer-alist-remove ()
  "Remove current buffer from `gis-buffer-alist'."
  (let ((c (rassoc (buffer-name) gis-buffer-alist)))
    (if c
	(progn
	  (setcdr c nil)
	  (car c)))))

(defun gis-buffer-alist-prefix-function (arg mode predicate)
  "Function to process prefix keys when used with \\[gis]."
  (let ((buf (cdr (assq arg gis-buffer-alist))))
    (if (and buf
	     (save-excursion
	       (set-buffer buf)
	       (sw-buffer-mode-list-predicate-p predicate)))
	t
      (error "No GIS buffer"))
    buf))

(defun gis-command-display (command)
  "Return shortened Gis command suitable for display."
  (if (stringp command) ; defensive programming. Should be a string but need to avoid errors
      (let              ; because this function is called in a menu-update-hook
	  ((command-len (- (min (length command) gis-command-history-max-length)))
	   (label ""))
	(save-match-data
	  (if (string-match "^\\[[^\]]*\\]" command)
	      (setq label
		    (concat (sw-file-name-display (match-string 0 command)
						  gis-command-history-max-length-dir)
			    "..."))))
	(concat label (substring command (+ command-len (length label)))))))

(defun gis-update-sw-menu ()
  "Update GIS process submenu in SW menu bar."
  (let* ((gis-alist (sort (copy-alist gis-buffer-alist)
			  '(lambda (a b) (< (car a) (car b)))))
	 gis-list)
    (dolist (c gis-alist)
      (let ((i   (car c))
	    (buf (cdr c)))
      (if buf
	  (setq gis-list
		(append gis-list
			(list (vector buf
				      (list 'switch-to-buffer buf)
				      ':active t
				      ':keys (format "M-%d f2 z" i))))))))
    ;;GIS buffers ordered according to when they were started.
    ;;killed session numbers are reused.
    (easy-menu-change (list resources-menu-sw)
		      resources-menu-sw-gis-procs
		      (or gis-list (list resources-menu-sw-no-procs)))))

(defun gis-update-gis-menu ()
  "Update the GIS menu bar."
  (if (eq major-mode 'gis-mode)
      (let (command-list)
	(save-match-data
	  ;;Delete duplicates from gis-command-history local and global values
	  ;;Note: delete-duplicates does not appear to work on localised variables.
	  (setq gis-command-history
		(remove-duplicates gis-command-history :test 'equal))
	  (setq-default gis-command-history
			(remove-duplicates (default-value 'gis-command-history)
					   :test 'equal))
	  
	  (dolist (command gis-command-history)
	    (push (apply
		   'vector
		   (gis-command-display command)
		   (list 'gis (buffer-name) (purecopy command))
		   ':active
		   '(not (get-buffer-process (buffer-name)))
		   ;; ':key-sequence nil
		   (if xemacs-p nil (list ':help (purecopy command)))) ;XEmacs does not handle :help
		  command-list)))

	(if (get-buffer-process (buffer-name))
	    (setq command-list
		  (append command-list
			  (list "---"
				(apply 'vector (gis-command-display gis-current-command)
				       'ignore ':active nil
				       (if xemacs-p nil (list ':key-sequence nil
							      ':help (purecopy gis-current-command))))
				(apply 'vector resources-menu-sw-run-new-gis 'gis-new-buffer
				       ':active t
				       ':keys '("C-u f2 z"))))))

	(easy-menu-change (list resources-gis-menu)
			  resources-gis-menu-history
			  (or command-list (list resources-gis-menu-no-history))))))

(defun gis-update-sw-shell-menu ()
  "Update GIS shell submenu in SW menu bar."
  (let ((shell-bufs (sw-buffer-mode-list 'shell-mode
					 (function (lambda () (getenv "SMALLWORLD_GIS")))))
	shell-list)
    (loop for buf in shell-bufs
	  do (push (vector buf (list 'switch-to-buffer buf) t) shell-list))
    (easy-menu-change (list resources-menu-sw)
		      resources-menu-sw-shell-procs
		      (or shell-list (list resources-menu-sw-no-procs)))))


(defun gis-mode ()
  "Major mode for run a gis as a direct sub-process.

The default name for a buffer running a gis is \"*gis*\".  The name of
the current gis buffer is kept in the user-option, `gis-buffer'.

There are many ways of recalling previous commands (see the on-line
help on F1).

Commands are sent to the gis with the F8 key or the return key.

Entry to this mode calls the value of gis-mode-hook."

  (interactive)
  (let
      ((tmp-no-of-gis-cmds            no-of-gis-cmds)
       (tmp-gis-cmd-num               gis-cmd-num)
       (tmp-prev-gis-cmds             prev-gis-cmds))
    (kill-all-local-variables)
    (make-local-variable 'selective-display)
    (make-local-variable 'comint-last-input-start)
    (make-local-variable 'comint-last-input-end)
    (make-local-variable 'font-lock-defaults)

    (make-local-variable 'gis-command-history)
    (make-local-variable 'gis-current-command)
    (make-local-variable 'no-of-gis-cmds)
    (make-local-variable 'gis-cmd-num)
    (make-local-variable 'prev-gis-cmds)
    (make-local-variable 'gis-filter-state)
    (make-local-variable 'gis-process)
    (make-local-variable 'gis-prompt)
    (make-local-variable 'gis-exec-path)
    (make-local-variable 'gis-process-environment)
    (make-local-variable 'gis-cb-buffer)
    (make-local-variable 'gis-drag-n-drop-mode-line-string)
    (make-local-variable 'magik-transmit-debug-mode-line-string)
    (make-local-variable 'ac-sources)

    ;(make-local-hook 'kill-buffer-hook) ;add-hook uses local option

    (use-local-map gis-mode-map)
    (easy-menu-add gis-menu)
    (set-syntax-table magik-mode-syntax-table)

    (if (null tmp-no-of-gis-cmds)
        (progn
          (setq no-of-gis-cmds 1)
          (setq gis-cmd-num 0)
          (setq prev-gis-cmds (make-vector 100 nil))
          ;; the null marker-pair is a pair of references to the same marker.
          ;; This is so that they will always move together and therefore be null.
          (aset prev-gis-cmds 0 (let ((m (point-min-marker))) (cons m m))))
      (setq no-of-gis-cmds            tmp-no-of-gis-cmds)
      (setq gis-cmd-num               tmp-gis-cmd-num)
      (setq prev-gis-cmds             tmp-prev-gis-cmds))

    (setq major-mode 'gis-mode
	  mode-name resources-gis-menu
	  selective-display t
	  local-abbrev-table magik-mode-abbrev-table
	  comint-last-input-start (make-marker)
	  comint-last-input-end   (make-marker)
	  gis-command-history (or gis-command-history (default-value 'gis-command-history))
	  gis-filter-state "\C-a"
	  gis-cb-buffer    (concat "*cb*" (buffer-name))
	  gis-drag-n-drop-mode-line-string " DnD"
	  magik-transmit-debug-mode-line-string " #DEBUG"
	  font-lock-defaults '(gis-font-lock-keywords
			       nil t
			       ((?_ . "w")))
	  ac-sources (append '(
			     magik-ac-class-method-source
			     magik-ac-dynamic-source
			     magik-ac-global-source
			     magik-ac-object-source
			     magik-ac-raise-condition-source
			     )
			   ac-sources))

    ;; Update gis-buffer to current buffer name if gis-buffer's buffer
    ;; does not exist.  this effectively stores the first most likely
    ;; default value in gis-buffer even if an aliases file GIS session
    ;; was started first.
    (if (and gis-buffer (get-buffer gis-buffer))
	nil
      (setq-default gis-buffer (buffer-name)))

    (if (rassoc (buffer-name) gis-buffer-alist)
	nil
      ;; Update gis-buffer-alist
      (let ((n 1))
	(while (cdr (assq n gis-buffer-alist))
	  (setq n (1+ n)))
	(if (assq n gis-buffer-alist)
	    (setcdr (assq n gis-buffer-alist) (buffer-name))
	  (add-to-list 'gis-buffer-alist (cons n (buffer-name))))))

    ;; *gis* buffer always inherits the current global environment
    (if (equal (buffer-name) "*gis*")
	(setq gis-exec-path (copy-list exec-path)
	      gis-process-environment (copy-list process-environment))
      (setq gis-exec-path (copy-list (or gis-exec-path exec-path))
	    gis-process-environment (copy-list (or gis-process-environment
						   process-environment))))

    (set mode-line-process-sym '(": %s"))

    (abbrev-mode 1)
    (save-excursion
      (set-buffer (get-buffer-create (concat " *filter*" (buffer-name))))
      (erase-buffer))

    (add-hook menu-bar-update-hook-sym 'gis-update-gis-menu)
    (add-hook menu-bar-update-hook-sym 'gis-update-sw-menu)
    (add-hook menu-bar-update-hook-sym 'gis-update-sw-shell-menu)
    (add-hook 'kill-buffer-hook 'gis-buffer-alist-remove nil t) ;local hook
    (run-hooks 'gis-mode-hook)))

(defun gis-sentinel (proc msg)
"Sentinel function, runs when the gis process exits."
(let ((gis-exit-status (process-exit-status proc))
      (buf (process-buffer proc)))
  (save-excursion
    (set-buffer buf)
    ;; ensure process end message is at end of buffer.
    (goto-char (point-max))
    (cond ((eq (process-status proc) 'exit)
	   (insert "\n\n" (format resources-gis-sentinel-process-text
				  (process-name proc)
				  msg)
		   "\n")
	   (message resources-gis-sentinel-process-exited buf msg))
	  ((eq (process-status proc) 'signal)
	   (insert "\n\n" (format resources-gis-sentinel-process-text
				  (process-name proc)
				  msg)
		   "\n")
	   (message resources-gis-sentinel-process-signalled buf msg)))

    (message resources-gis-sentinel-process-terminated
	     buf (process-name proc) (number-to-string gis-exit-status))

    ;;Allow messages to appear in *Messages* buffer
    (sit-for 0.01)
    (run-hook-with-args 'gis-sentinel-hooks gis-exit-status))))

(defun gis-start-process (args)
  "Run a Gis process in the current buffer.
Adds `gis-current-command' to `gis-command-history' if not already there."
  (let ((exec-path (copy-list gis-exec-path))
	(process-environment (copy-list gis-process-environment)))
    (run-hooks 'gis-start-process-pre-hook)
    (or (member gis-current-command gis-command-history)
	(add-to-list 'gis-command-history gis-current-command))
    (setq gis-process
	  (apply 'start-process "gis-process" (current-buffer) (car args) (cdr args)))
    (set-process-sentinel gis-process 'gis-sentinel)
    (set-marker (process-mark gis-process) (point-max))
    (set-process-filter gis-process 'gis-filter)

    ;;MF New bit for connecting to the method finder:
    ;;MF We nuke the current cb first and reconnect later.
    (when (and cb-dynamic (get-buffer gis-cb-buffer))
      (let ((cb-process (get-buffer-process gis-cb-buffer)))
	(if cb-process (delete-process cb-process)))
      (process-send-string gis-process "_if method_finder _isnt _unset\n_then\n  method_finder.lazy_start?\n  method_finder.send_socket_to_emacs()\n_endif\n$\n"))
    (sit-for 0.01)
    (run-hooks 'gis-start-process-post-hook)))

;; Put up here coz of load order problems.
;; The logic of the `F2 s' is still not quite right anyway.

;;;autoload
(defun gis (&optional buffer command)
  "Run a Gis process in a buffer in `gis-mode'.

The command is typically \"sw_magik_win32\" or \"sw_magik_motif\", but
can be any interactive program such as \"csh\".

The program that is offered as a default is stored in the variable,
`gis-command', which you can customise.  e.g.

\(setq gis-command
  \"[$HOME] sw_magik_win32 -Mextdir %TEMP% -image $SMALLWORLD_GIS/images/gis.msf\"
\)
The command automatically expands environment variables using
Windows %% and Unix $ and ${} nomenclature.

You can setup a list of standard commands by setting the
default value of `gis-command-history'.

Prefix argument controls:
With a numeric prefix arg, switch to the Gis process of that number
where the number indicates the order it was started. The
SW->Gis Processes indicates which numbers are in use. If a Gis process
buffer is killed, its number is reused when a new Gis process is started.

With a non-numeric prefix arg, ask user for buffer name to use for
GIS.  This will default to a unique currently unused name based upon
the current value of `gis-buffer-default-name'.

If there is already a Gis process running in a visible window or
frame, just switch to that buffer, or prompt if more than one.  If
there is not, prompt for a command to run, and then run it."

  (interactive)
  (if command (setq gis-command command))
  (let (dir
        cmd
        args
	;;read-string's history arg does not work with buffer-local variables
	;;history also always has something see Package Registration at end
        (command-history gis-command-history)
        alias-beg
        alias-expansion
	(alias-buffer "*temp gis alias buffer*")
        (keepgoing t)
	(gis-start-process-pre-hook gis-start-process-pre-hook)
	(buffer (sw-get-buffer-mode (cond (buffer buffer)
					  ((eq major-mode 'gis-mode) (buffer-name))
					  (t nil))
				    'gis-mode
				    resources-gis-enter-buffer
				    (or gis-buffer gis-buffer-default-name)
				    'gis-buffer-alist-prefix-function
				    (generate-new-buffer-name gis-buffer-default-name)))
        (rev-1920-regexp " +\\[rev\\(19\\|20\\)\\] +")
        (alias-subst-regexp "\\\\!\\(\\\\\\)?\\*"))
    (if (and (get-buffer-process buffer)
             (eq (process-status (get-buffer-process buffer)) 'run))
        (progn
	  (pop-to-buffer buffer)
	  (goto-char (point-max)))

      ;; Else start a fresh gis:
      ;; We keep going round expanding aliases until there is no alias expansion.
      ;; Each time round the user can edit the expanded alias.
      ;; We also silently remove any strings of the form [rev20] or [rev19].

      (save-excursion
        (set-buffer (get-buffer-create alias-buffer))
        (erase-buffer)
        (if (and (equal (getenv "SHELL") "/bin/csh")
                 (file-readable-p "~/.alias"))
            (insert-file-contents "~/.alias"))

        (while keepgoing
	  (setq keepgoing nil)
	  (setq gis-command (sub gis-command rev-1920-regexp " "))
	  (or (eq (string-match "\\[" gis-command) 0)
	      (setq gis-command (concat "[" default-directory "] " gis-command)))
	  (or command
	      (setq gis-command
		    (read-string (concat resources-gis-command-prompt " ")
				 (car command-history)
				 'command-history)))
	  (if (string-match rev-1920-regexp gis-command)
	      (progn
		(setq keepgoing t)
		(setq gis-command (sub gis-command rev-1920-regexp " "))))
	  (or (eq (string-match "\\[" gis-command) 0)
	      (setq gis-command (concat "[" default-directory "] " gis-command)))
	  (string-match "\\[\\([^\]]*\\)\\] *\\([^ ]*\\) *\\(.*\\)" gis-command)
	  (setq dir  (substring gis-command (match-beginning 1) (match-end 1)))
	  (setq cmd  (substring gis-command (match-beginning 2) (match-end 2)))
	  (setq args (substring gis-command (match-beginning 3) (match-end 3)))

	  (goto-char (point-min))
	  (if (re-search-forward (concat "^alias[ \t]+" (regexp-quote cmd) "[ \t]+") nil t)
	      (progn
		(setq keepgoing t)
		(setq alias-beg (match-end 0))
		(goto-char alias-beg)
		(if (looking-at "['\"]")
		    (progn
		      (incf alias-beg)
		      (end-of-line)
		      (re-search-backward "['\"]"))
		  (end-of-line))
		(setq alias-expansion (buffer-substring alias-beg (point)))
		(or (string-match alias-subst-regexp alias-expansion)
		    (setq alias-expansion (concat alias-expansion " \\!*")))
		(setq alias-expansion (sub alias-expansion alias-subst-regexp args))
		(setq gis-command (concat "[" dir "] " alias-expansion)))))

        (kill-buffer alias-buffer))

      (pop-to-buffer (get-buffer-create buffer))
      (gis-mode)
      (goto-char (point-max))
      (insert "\n" (current-time-string) "\n")
      (setq default-directory (expand-file-name
			       (file-name-as-directory
				(substitute-in-file-name dir)))
	    gis-current-command (copy-sequence gis-command)
	    gis-command-history (cons gis-current-command
				      (delete gis-current-command gis-command-history)))
      (setq-default gis-command-history (cons gis-current-command
				      (delete gis-current-command gis-command-history)))
      (or (file-directory-p default-directory)
	  (error resources-sw-no-directory-error default-directory))
      (add-hook 'gis-start-process-pre-hook
		(function (lambda () (insert gis-command ?\n ?\n)))
		t)
      (gis-start-process (gis-parse-gis-command (concat cmd " " args))))))

(defun gis-new-buffer ()
  "Start a new GIS session."
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively 'gis)))

(defun gis-kill-process ()
  "Kill the current gis process.
Uses `gis-kill-process-function' function to kill the process given in `gis-process'."
  (interactive)
  (if (and gis-process
	   (eq (process-status gis-process) 'run)
	   (y-or-n-p (concat resources-gis-yn-kill-gis " ")))
      (let ((status (process-status gis-process)))
	(funcall gis-kill-process-function gis-process)
	(sit-for 0.1)
	(if (eq status (process-status gis-process))
	    (insert (concat "\n" resources-gis-kill-process-pending "\n"))))))

(defun query-interrupt-shell-subjob ()
  "Ask and then comint-interrupt-subjob."
  (interactive)
  (if (y-or-n-p (concat resources-gis-yn-kill-gis " "))
      (if (running-under-nt)
          (comint-kill-subjob)
        (comint-interrupt-subjob))))

(defun query-quit-shell-subjob ()
  "Ask and then comint-quit-subjob."
  (interactive)
  (if (y-or-n-p (concat resources-gis-yn-kill-gis " "))
      (comint-quit-subjob)))

(defun query-stop-shell-subjob ()
  "Ask and then comint-stop-subjob."
  (interactive)
  (if (y-or-n-p (concat resources-gis-yn-suspend-gis " "))
      (comint-stop-subjob)))

(defun query-shell-send-eof ()
  "Ask and then comint-send-eof."
  (interactive)
  (if (y-or-n-p (concat resources-gis-yn-eof-gis " "))
      (comint-send-eof)))

;; R E C A L L I N G   C O M M A N D S
;; ___________________________________
;;
;;; Each gis command is recorded by vec-gis-mode.el so that the
;;; the user can recall and edit previous commands.  This file
;;; also adds dollars and implements the history-folding feature.

(defun copy-gis-cmd (n offset)
  "Copy command number N to the bottom of the buffer (replacing
any current command) and locate the cursor to an offset OFFSET."
  (delete-region (process-mark (get-buffer-process (current-buffer))) (point-max))
  (goto-char (point-max))
  (let*
      ((pair (aref prev-gis-cmds n))
       (str (subst-char-in-string  ?\r ?\n (buffer-substring (car pair) (cdr pair))))
       (len (length str)))
    (insert str)
    (forward-char (- (max 0 (min len offset)) len))
    (if (pos-visible-in-window-p)
        (while
            (not (pos-visible-in-window-p (point-max)))
          (scroll-up 1)))))

(defun send-gis-region (beg end)
  "Record in `prev-gis-cmds' the region BEG to END and send to the gis.
Also update `gis-cmd-num'.  Also append the string to \" *history**gis*\"."
  (save-excursion
    (let ((str (buffer-substring beg end)))
      (set-buffer (get-buffer-create (concat " *history*" (buffer-name))))
      (magik-mode)
      (let ((orig-point (point)))
        (goto-char (point-max))
        (insert str "\n")
        (goto-char orig-point))))
  (let ((n no-of-gis-cmds))
    (if (= n (length prev-gis-cmds))
        (make-new-gis-cmds-vec))
    (setq n no-of-gis-cmds)   ;; aaargh! I had forgotten this line and had a horrible intermittent bug.
    ;; NB: we are keeping a null marker at the end and this must be moved along.
    (aset prev-gis-cmds n (aref prev-gis-cmds (1- n)))
    (aset prev-gis-cmds (1- n) (cons (copy-marker beg) (copy-marker end)))
    (setq gis-cmd-num no-of-gis-cmds)
    (incf no-of-gis-cmds)

    (set-marker comint-last-input-start beg)
    (set-marker comint-last-input-end   end)
    (set-marker (process-mark (get-buffer-process (current-buffer))) end)
    (goto-char (point-max))
    (save-excursion
      (while
          (> (- end beg) 240)
        (goto-char (+ beg 240))
        (if (search-backward "\n" beg t)
            (process-send-region (get-buffer-process (current-buffer)) beg (1+ (point)))
          (error resources-gis-long-line-error))
        (setq beg (1+ (point)))))
    (process-send-region (get-buffer-process (current-buffer)) beg end)))

(defun make-new-gis-cmds-vec ()
  "Create a new bigger vector for `prev-gis-cmds' and copy the
non-degenerate commands into it."
  (message resources-gis-resizing-vector)
  (let*
      ((len (length prev-gis-cmds))
       (v (make-vector (+ len 100) nil))
       (i 0)
       (v_i 0))
    (while
        (< i len)
      (let
          ((x (aref prev-gis-cmds i)))
        (if (and (marker-buffer (car x))
                 (marker-buffer (cdr x))
                 (> (cdr x) (car x)))
            (progn
              (aset v v_i x)
              (incf v_i))))
      (incf i))
    (let
	((m (copy-marker (point-min))))
      (aset v v_i (cons m m)))
    (setq no-of-gis-cmds (1+ v_i))
    (setq prev-gis-cmds v)
    (message resources-gis-resizing-vector-done (number-to-string v_i))))

(defun gis-beginning-of-line (&optional n)
  "Move point to beginning of Nth line or just after prompt.
If command is repeated then place point at beginning of prompt."
  (interactive "p")
  (beginning-of-line n)
  ;;Only move to end of prompt if last-command was this one
  ;;AND a prefix key has not be used (n=1).
  (and (not (and (eq last-command 'gis-beginning-of-line) (eq n 1)))
       (looking-at gis-prompt)
       (goto-char (match-end 0))))

(defun gis-newline (arg)
  "If in a prev. cmd., recall.
If within curr. cmd., insert a newline.
If at end of curr. cmd. and cmd. is complete, send to gis.
If at end of curr. cmd. and cmd. is not complete, insert a newline.
Else (not in any cmd.) recall line."
  (interactive "*P")
  (let
      ((n (get-curr-gis-cmd-num))
       (p (process-mark (get-buffer-process (current-buffer)))))
    (cond
     (n  ; in a prev. cmd.
      (copy-gis-cmd n
                    (- (point)
                       (car (aref prev-gis-cmds n)))))

     ((>= (point) p)
      (if abbrev-mode (save-excursion (expand-abbrev)))
      (cond
       ((looking-at "[ \t\n]*\\'")  ; at end of curr. cmd.
        (newline arg)
        (cond
         ((save-excursion
            (and (progn
                   (skip-chars-backward " \t\n")
                   (eq (preceding-char) ?$))
                 (> (point) p)))
          (skip-chars-backward " \t\n")
          (forward-char)
          (delete-region (point) (point-max))
          (send-gis-region (marker-position p) (point)))
         ((complete-magik-p p (point))
          (delete-region (point) (point-max))
          (send-gis-region (marker-position p) (point)))))
       ((looking-at "[ \t\n]*\\$[ \t\n]*\\'")
        (if (complete-magik-p p (point))
            (progn
              (search-forward "$")
              (delete-region (point) (point-max))
              (insert "\n")
              (send-gis-region (marker-position p) (point)))
          (newline arg)))
       (t
        (newline arg))))
         
     (t  ; not in any cmd.
      (delete-region (process-mark (get-buffer-process (current-buffer))) (point-max))
      (let
          ((str (buffer-substring (point-bol) (point-eol)))
           (n (- (point-eol) (point))))
        (goto-char (point-max))
        (insert str)
        (backward-char n))))))

(defun complete-magik-p (beg end)
  "Return t if the region from BEG to END is a syntactically complete piece of
Magik.  Also write a message saying why the magik is not complete."
  (save-excursion
    (goto-char beg)
    (let
        (stack  ; ...of pending brackets and keywords (strings).
         last-tok)
      (while
          (progn
            (let
                ((toks (tokenise-magik-region-no-eol-nor-point-min (point) (min (point-eol) end))))
              (if toks (setq last-tok (car (last toks))))
              (dolist (tok toks)
                (cond
                 ((or (and (equal (car stack) "_for")    (equal (car tok) "_over"))
                      (and (equal (car stack) "_over")   (equal (car tok) "_loop"))
                      (and (equal (car stack) "_pragma") (equal (car tok) "_method"))
                      (and (equal (car stack) "_pragma") (equal (car tok) "_proc")))
                  (pop stack)
                  (push (car tok) stack))
                 ((member (car tok) '("_for" "_over" "_pragma"))
                  (push (car tok) stack))
                 ((assoc (car tok) magik-begins-and-ends)
                  (push (car tok) stack))
                 ((assoc (car tok) magik-ends-and-begins)
                  (cond
                   ((null stack)
                    (error resources-gis-no-end-token-error
                           (car tok)
                           (cdr (assoc (car tok) magik-ends-and-begins))))
                   ((equal (cdr (assoc (car tok) magik-ends-and-begins)) (car stack))
                    (pop stack))
                   (t
                    (error resources-gis-expecting-token-error
                           (car tok)
                           (cdr (assoc (car stack) (append magik-begins-and-ends
                                                           '(("_for" . "_over")
                                                             ("_over" . "_loop")
                                                             ("_pragma" . "_proc or _method"))))))))))))
            (/= (point-eol) (point-max)))
        (forward-line))
      (cond
       (stack
        (message resources-gis-not-sent-waiting-for
                 (cdr (assoc (car stack) (append magik-begins-and-ends
                                                 '(("_for" . "_over")
                                                   ("_over" . "_loop")
                                                   ("_pragma" . "_proc or _method"))))))
        nil)
       ((assoc (car last-tok) magik-operator-precedences)
        (message resources-gis-not-send-pending-op (car last-tok))
        nil)
       (t
        t)))))

(defun get-curr-gis-cmd-num ()
  "Return the num of the command that point is in, or nil if not in a command.
Being in the prompt before the command counts too.  We do this by binary search."
  (get-curr-gis-cmd-num-2 0 (1- no-of-gis-cmds)))

(defun get-curr-gis-cmd-num-2 (min max)
  "Return the num of the command that point is in, or nil if it isn't in the half-open
range [MIN, MAX)."
  (if (> max min)
      (let*
          ((mid (/ (+ min max) 2))
           (pair (aref prev-gis-cmds mid))
           (p (point)))
        (cond
         ((or (null (marker-buffer (car pair))))
              ;(= (car pair) (cdr pair)))
          (get-curr-gis-cmd-num-2 (1+ mid) max))
         ((save-excursion
            (goto-char (car pair))
            (beginning-of-line)
            (and (>= p (point))
                 (< p (cdr pair))))
          mid)
         ((>= p (cdr pair))
          (get-curr-gis-cmd-num-2 (1+ mid) max))
         ((< p (car pair))
          (get-curr-gis-cmd-num-2 min mid))
         (t
          (error resources-gis-command-recall-error))))))

(defun gis-electric-magik-space (arg)
  "copy blocks to the bottom of the gis buffer first and then do an electric space."
  (interactive "*p")
  (prepare-for-gis-edit-cmd)
  (electric-magik-space arg))

(defun gis-insert-char (arg)
  "Take a copy of a command before inserting the char."
  (interactive "*p")
  (prepare-for-gis-edit-cmd)
  (self-insert-command arg))

(defun gis-delete-char (arg)
  "Take a copy of a command before deleting the char."
  (interactive "*p")
  (prepare-for-gis-edit-cmd)
  (delete-char arg))

(defun gis-kill-word (arg)
  "Take a copy of a command before killing the word."
  (interactive "*p")
  (prepare-for-gis-edit-cmd)
  (kill-word arg))

(defun gis-backward-kill-word (arg)
  "Take a copy of a command before killing the word."
  (interactive "*p")
  (prepare-for-gis-edit-cmd)
  (backward-kill-word arg))

(defun gis-backward-delete-char (arg)
  "Take a copy of a command before deleting the char."
  (interactive "*p")
  (prepare-for-gis-edit-cmd)
  (delete-backward-char arg))

(defun gis-kill-line (arg)
  "Take a copy of a command before killing the line."
  (interactive "*P")
  (prepare-for-gis-edit-cmd)
  (kill-line))

(defun gis-kill-region (beg end)
  "Ask if they really want to kill the region, before killing it."
  (interactive "*r")
  (if (y-or-n-p (concat resources-gis-yn-kill-region " "))
      (kill-region beg end)))

(defun prepare-for-gis-edit-cmd ()
  "If we're in a previous command, replace any current command with
this one."
  (let
      ((n (get-curr-gis-cmd-num)))
    (if n
        (copy-gis-cmd n
                      (- (point)
                         (car (aref prev-gis-cmds n)))))))

(defun gis-send-command-at-point ()
  "Send the command at point, copying to the end of the buffer if necessary and
don't add extra dollars."
  (interactive "*")
  (or (get-buffer-process (current-buffer))
      (error resources-gis-no-process-error))
  (let
      ((n (get-curr-gis-cmd-num))
       (p (process-mark (get-buffer-process (current-buffer)))))
    (cond
     (n
      (copy-gis-cmd n 0)
      (send-gis-region (marker-position p) (point-max)))
     ((>= (point)
          (save-excursion
            (goto-char p)
            (beginning-of-line)
            (point)))
      (goto-char (point-max))
      (or (eq (preceding-char) ?\n)
          (insert ?\n))
      (send-gis-region (marker-position p) (point-max)))
     (t
      (error resources-gis-not-command-error)))))

(defun matching-gis-cmd-p (n str)
  "Return t if prev-gis-cmds[N] is a non-degenerate command matching STR or off the end (i.e.
n<0 or n>=no-of-gis-cmds)."
  (or (< n 0)
      (>= n no-of-gis-cmds)
      (let
          ((pair (aref prev-gis-cmds n))
           (len (length str)))
        (and (marker-buffer (car pair))
             (> (- (cdr pair) (car pair)) len)
             (equal (buffer-substring (car pair) (+ (car pair) len)) str)))))

(defun gis-recall (str step end-of-command-p)
  "Recall a command starting with STR in the direction STEP.
If END-OF-COMMAND-P is t then cursor is placed at and of the recalled command.
An internal function that deals with 4 cases."
  (let ((n gis-cmd-num)
	mark )
    (while
        (progn
          (incf n step)
          (not (matching-gis-cmd-p n str))))
    (if (= n -1)
        (if (equal str "")
            (error resources-gis-no-previous-command-error)
          (error resources-gis-no-previous-matching-command-error str)))
    (setq mark (process-mark (get-buffer-process (current-buffer))))
    (if (= n no-of-gis-cmds)
        (decf n))
    (copy-gis-cmd n
                  (if (equal str "")
                      (- (point) mark)
                    (length str)))
    (setq gis-cmd-num n)
    (if end-of-command-p
	(progn
	  (goto-char (point-max))
	  ;; skip back past \n$\n and whitespace
	  (skip-chars-backward " \t\n$" mark)
	  ))))

(defun recall-prev-gis-cmd ()
  "Recall the earlier gis commands
Cursor point is placed at end of command.
Compare with \\[recall-prev-matching-gis-cmd] placing cursor
immediately at the start of a command"
  (interactive "*")
  (gis-recall "" -1 gis-recall-cmd-move-to-end))

(defun recall-next-gis-cmd ()
  "Recall the later gis commands
Cursor point is placed at end of command.
Compare with \\[recall-next-matching-gis-cmd] placing cursor
immediately at the start of a command"
  (interactive "*")
  (gis-recall "" 1 gis-recall-cmd-move-to-end))

(defun recall-prev-matching-gis-cmd ()
  "Recall the earlier and earlier gis commands that match the part of
the command before the cursor."
  (interactive "*")
  (gis-recall (buffer-substring
               (process-mark (get-buffer-process (current-buffer)))
               (point))
              -1
	      nil))

(defun recall-next-matching-gis-cmd ()
  "Recall the earlier and earlier gis commands that match the part of
the command before the cursor."
  (interactive "*")
  (gis-recall (buffer-substring
               (process-mark (get-buffer-process (current-buffer)))
               (point))
              1
	      nil))

(defun display-gis-history (arg)
  "Fold (hide) away the parts of the gis buffer in between the last ARG commands.
If ARG is null, use a default of `gis-history-length'."
  (interactive "*P")
  (setq arg (if (null arg) gis-history-length (prefix-numeric-value arg)))
  (let
      ((b (current-buffer)))
    (or (eq major-mode 'gis-mode)
	(set-buffer gis-buffer))
    (setq selective-display t)
    (let
        ((p (point))
         (i (max 0 (min (- no-of-gis-cmds arg 1) (1- no-of-gis-cmds))))
         j)
      ;; look for the start point of the folding; this may involve skipping
      ;; over bad markers.
      (while
          (and (< i no-of-gis-cmds)
               (not (marker-buffer (car (aref prev-gis-cmds i)))))
        (incf i))
      (if (= i (1- no-of-gis-cmds))
          (error resources-gis-no-commands-to-fold))
      ;; we now have the index of the first command to fold.
      (message resources-gis-folding-commands (number-to-string arg))
      (goto-char (car (aref prev-gis-cmds i)))
      (while (search-forward "\n" nil t)
	(replace-match "\r"))
      (setq j i)
      (while 
          (< j (1- no-of-gis-cmds))
        (goto-char (car (aref prev-gis-cmds j)))
        (if (re-search-backward "[\r\n]" nil t)
            (progn
              (insert ?\n)
              (delete-char 1)))
        (incf j))
      (goto-char (point-max))
      (if (search-backward "\r" nil t)
          (progn
            (insert ?\n)
            (delete-char 1)))
      (message resources-gis-folding-commands-done (number-to-string arg))
      (goto-char p)
      (set-buffer b))))

(defun undisplay-gis-history (arg)
  "Unfold the last ARG commands.  If ARG is null, use a default of `gis-history-length'."
  (interactive "*P")
  (setq arg (if (null arg) gis-history-length (prefix-numeric-value arg)))
  (let
      ((b (current-buffer)))
    (or (eq major-mode 'gis-mode)
	(set-buffer gis-buffer))
    (setq selective-display t)
    (let
        ((p (point))
         (i (max 0 (min (- no-of-gis-cmds arg 1) (1- no-of-gis-cmds)))))
      ;; look for the start point of the folding; this may involve skipping
      ;; over bad markers.
      (while
          (and (< i no-of-gis-cmds)
               (not (marker-buffer (car (aref prev-gis-cmds i)))))
        (incf i))
      (if (= i (1- no-of-gis-cmds))
          (error resources-gis-no-commands-to-unfold))
      ;; we now have the index of the first command to unfold.
      (message resources-gis-unfolding-commands (number-to-string arg))
      (goto-char (car (aref prev-gis-cmds i)))
      (while (search-forward "\r" nil t)
	(replace-match "\n"))
      (message resources-gis-unfolding-commands-done (number-to-string arg))
      (goto-char p)
      (set-buffer b))))

(defun goto-process-mark ()
  "(goto-char (process-mark (get-buffer-process (current-buffer))))"
  (interactive)
  (goto-char (process-mark (get-buffer-process (current-buffer)))))

(defun set-process-mark-to-eob ()
  "(set-marker (process-mark (get-buffer-process (current-buffer))) (point-max))"
  (interactive)
  (set-marker (process-mark (get-buffer-process (current-buffer))) (point-max)))
;;;
;;;  T R A C E B A C K
;;;

;; support for `gis-traceback-print()'
(defun gis-print-region-and-fold (start end switches)
  "like `print-region-1()' but with long lines folded first."
  (let ((name (concat (buffer-name) " Emacs buffer"))
	(width tab-width))
    (save-excursion
      (message resources-gis-printing)
      (let ((oldbuf (current-buffer)))
        (set-buffer (get-buffer-create " *spool temp*"))
        (widen) (erase-buffer)
        (insert-buffer-substring oldbuf start end)
        (setq tab-width width)
        (untabify (point-min) (point-max))
        (goto-char (point-min))
        (while
            (not (eobp))
          (if (> (- (point-eol) (point)) 72)
              (progn
                (forward-char 72)
                (insert ?\n))
            (forward-line)))
        (setq start (point-min) end (point-max)))
      (apply 'call-process-region
             (nconc (list start end lpr-command
                          nil nil nil)
                    (nconc (and (eq system-type 'berkeley-unix)
                                (list "-J" name "-T" name))
                           switches)))
      (message resources-gis-printing-done))))

(defun gis-error-narrow-region ()
  "Narrow region between the current Magik prompts."
  (narrow-to-region (save-excursion (re-search-backward gis-prompt))
		    (save-excursion
		      (or (re-search-forward gis-prompt nil t) (goto-char (point-max)))
		      (beginning-of-line)
		      (point))))

(defun gis-error-line-col (line)
  "Return (LINE . COLUMN) cons for location of error."
  (let ((col 0))
    (save-excursion
      (save-match-data
	(search-backward "--- line")
	(end-of-line)
	(forward-word -1)
	(setq line (+ line (string-to-number (current-word))))
	(if (re-search-forward "^\\s-*\\^" nil t)
	    (setq col (1- (length (match-string 0)))))))
    (cons line col)))

(defun gis-error-goto ()
  "Goto file that contains the Magik error."
  (interactive)
  (let ((case-fold-search nil) ;case-sensitive searching required for "Loading"
	(line-adjust 0)
	(pos 0)
	file line-col buf)
    (save-match-data
      (save-restriction
	(gis-error-narrow-region)
	(save-excursion
	  (beginning-of-line)
	  (if (looking-at (concat "^\\*\\*\\*\\*.*" "on line" " \\([0-9]+\\)$"))
	      (progn
		(setq line-col (gis-error-line-col (string-to-number (match-string-no-properties 1)))
		      file (and (save-excursion (re-search-backward "Loading \\(.*\\)" nil t))
				(match-string-no-properties 1)))
		(if (file-exists-p file)
		    (setq buf (or (find-buffer-visiting file) (find-file-noselect file)))
		  (if (re-search-backward "^\\*\\*\\*\\* Emacs: buffer=\\(.*\\) file=\\(.*\\) position=\\([0-9]+\\)" nil t)
		      (setq buf  (match-string-no-properties 1)
			    file (match-string-no-properties 2)
			    pos  (string-to-number (match-string-no-properties 3))
			    line-adjust -4))))))))
    (or file
	(error resources-gis-goto-error-invalid-line))
    (pop-to-buffer buf)
    (goto-char pos)

    ;;Subtract line-adjust lines because we add lines
    ;;to the transmitted buffer and Magik counts lines from 0.
    (forward-line (+ (car line-col) line-adjust))

    (move-to-column (cdr line-col))))

(defun gis-error-goto-mouse (click)
  "Goto error at mouse point."
  (interactive "e")
  (mouse-set-point click)
  (gis-error-goto))

(defun gis-traceback-print ()
  "Send the text from the most recent \"**** Error\" to the end of
the buffer to the printer.  Query first."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (if (search-backward "\n**** Error" nil t)
        (if (y-or-n-p (concat (format resources-gis-yn-print-last-traceback
				      (number-to-string (count-lines (point) (point-max)))) " "))
            (gis-print-region-and-fold (point) (point-max) nil))
      (error resources-gis-no-error-line-print-error "**** Error"))))

(defun gis-traceback-save ()
  "Save in \"~/traceback.txt\" all the text onwards from the most recent \"**** Error\"."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (if (search-backward "\n**** Error" nil t)
        (progn
          (write-region (point) (point-max) "~/traceback.txt")
          (message resources-gis-save-traceback "~/traceback.txt"))
      (error resources-gis-no-error-line-save-error "**** Error"))))

(defun gis-traceback-up ()
  "Move up buffer to next traceback."
  (interactive)
  (save-match-data
    (re-search-backward "---- traceback: "))
  (forward-line -1))

(defun gis-traceback-down ()
  "Move down buffer to next traceback."
  (interactive)
  (forward-line 1)
  (forward-line 1)
  (save-match-data
    (re-search-forward "---- traceback: "))
  (forward-line -1))

;;; Drag 'n' Drop
;;
;; When a file is dragged and dropped and the current buffer is
;; as GIS mode buffer, the file is loaded into the GIS session.

(defun gis-drag-n-drop-mode (&optional arg)
  "Toggle Drag 'n' drop GIS loading functionality."
  (interactive "P")
  (setq gis-drag-n-drop-mode
	(if (null arg)
	    (not gis-drag-n-drop-mode)
	  (> (prefix-numeric-value arg) 0)))
  (add-hook 'find-file-hooks 'gis-drag-n-drop-load)
  (if gis-drag-n-drop-mode
      (message resources-gis-drag-n-drop-on)
    (message resources-gis-drag-n-drop-off))
  (force-mode-line-update))

(defun gis-drag-n-drop-load ()
  "Load a drag and dropped file into the Gis session.
If the previous buffer was a GIS session buffer and the previous event was
a drag & drop event then we load the dropped file into the GIS session.

The file must be in a Major mode that defines the function:
  MODE-gis-drag-n-drop-load
where MODE is the name of the major mode with the '-mode' postfix."
  (let (fn gis)
    ;;hopefully the tests are done in the cheapest, most efficient order
    ;;but gis-drag-n-drop-mode is checked last in case user has set
    ;;up a per-buffer Drag 'n' drop mode
    (if (and (listp last-input-event)
	     (eq (car last-input-event) 'drag-n-drop)
	     (setq fn (intern (concat (substring (symbol-name major-mode) 0 -5)
				      "-gis-drag-n-drop-load")))
	     (fboundp fn)
	     (windowp (caadr last-input-event))
	     (setq gis (window-buffer (caadr last-input-event)))
	     (save-excursion
	       (set-buffer gis)
	       (and gis-drag-n-drop-mode
		    (eq major-mode 'gis-mode))))
	(funcall fn gis (buffer-file-name)))))

;;;Package registration

;;Ensure Default gis-command are placed at head of gis-command-history
(mapcar (function
	 (lambda (c)
	   (and c
		(not (member c gis-command-history))
		(push c gis-command-history))))
	(list gis-command-default gis-command))
		

;;; package setup via setting of variable before load.
(and gis-drag-n-drop-mode (gis-drag-n-drop-mode gis-drag-n-drop-mode))

(or (assq 'gis-drag-n-drop-mode minor-mode-alist)
    (push '(gis-drag-n-drop-mode gis-drag-n-drop-mode-line-string) minor-mode-alist))
;;(or (assoc 'gis-drag-n-drop-mode minor-mode-alist)
;;    (push (list 'gis-drag-n-drop-mode " DnD") minor-mode-alist))

;;MSB configuration
(defun gis-msb-configuration ()
  "Adds GIS buffers to MSB menu, supposes that MSB is already loaded."
  (let* ((l (length msb-menu-cond))
	 (last (nth (1- l) msb-menu-cond))
	 (precdr (nthcdr (- l 2) msb-menu-cond)) ; cdr of this is last
	 (handle (1- (nth 1 last))))
    (setcdr precdr (list
		    (list
		     '(eq major-mode 'gis-mode)
		     handle
		     "GIS (%d)")
		    last))))

(eval-after-load 'msb
  '(gis-msb-configuration))

(eval-after-load 'auto-complete
  '(add-to-list 'ac-modes 'gis-mode))

(eval-after-load 'ecb
  '(add-to-list 'ecb-compilation-major-modes 'gis-mode))

(provide 'gis)
;;; gis.el ends here
