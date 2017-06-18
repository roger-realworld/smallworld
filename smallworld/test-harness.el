;;; test-harness.el: Emacs Test Environment

(defconst test-harness-version "$Revision: 1.4 $")

(require 'advice)

;; Avoid use of gis-version functionality, MOTH is defined to use existing environment
(set 'gis-version-program nil)
(set 'gis-version-file nil)
(require 'gis-version)

(require 'gis)
(require 'gis-filter)

;;; User customisation

(defvar test-harness-msgbox nil
  "Simple program to display a message box with a message and window title.
It displays a message box in the same way that the net send command would do:

   net send MACHINE MESSAGE

for backwards compatibility.

If nil, then 'net send' is used.")

(defvar gis-newline-wait-function 'sleep-for
  "*Function to call to wait for Magik command.
Called with one argument N which is the iteration number encountered.")

(defvar gis-newline-waits 10
  "*Number of times to check for command to complete.")

(defvar test-harness-timeout-time 0
  "*Time in seconds for timeout. If 0 then no timeout is set")

(defvar test-harness-timeout-hook nil
  "*Hook to run when Emacs times out the GIS process.")

;;;Variables
(defvar test-harness nil
  "When test harness enabled this variable contains the old value of frame-title-format.

This is also used within the run-tests function created by MOTH to
determine whether the test harness has been enabled. If it has then it
will send the terminating command like \"quit(0)\" to the process being
tested.
However, the run-tests function is also written so that it may be
loaded and executed by users without the need to load this harness
code.
")

(defvar test-harness-timer-object nil
  "When test harness enabled this variable contains the timer object that implements the time out functionality")

(defvar test-harness-special-buffers-log-path nil
  "Directory in which to save certain buffers.
See `test-harness-save-special-buffers' for more details.")

(defvar test-harness-special-buffers-to-files-alist '(("*Messages*" ".emacs_messages")
						      ('gis-buffer ".emacs_gis_buffer"))
  "Alist indicating which file each special buffer is saved to.
The path used is given by `test-harness-special-buffers-log-path'. This path contains an initial file
name root e.g. '/log_root/log_file.log' so the files saved will be like
'/log_root/log_file.log.emacs_messages'")


(defvar test-harness-timed-out nil
  "Set to t, if test-harness timer times out the gis process.")

(defvar test-harness-timeout-mode 'none
  "The timeout mode to use:
  'none   - no timeout mode
  'gis    - timeout operates at gis program level
  'emacs  - timeout operates at emacs level
The different modes determine the behaviour after a
timeout occurs.
 If you want to allow a gis session to
start and stop several times then the timeout must
operate on the Emacs application level
i.e. on the kill-emacs-hook
 If you just want to start a single gis session
and kill the gis session when the timeout is reached
then the timeout is operating at the gis application level
i.e. gis-sentinel-hooks. The Emacs application is
just a facilitator in this case.")

(defvar test-harness-emacs-exit nil
  "Make Emacs quit when either GIS process dies or a timeout occurs")

(defvar test-harness-timeout-popup nil
  "Variable to determine whether a popup window is required to inform an external process
that a timeout event has occured, e.g. used by WinRunner.
See `test-harness-timeout-popup-function' for more details.")

;;;Advice
(defadvice gis-newline (around gis-synchronised-process)
  "Will make Emacs wait for Magik command to complete.
Will actually wait the sum of `gis-newline-waits' seconds for command."
  (let ((gis-prompt-wait t))
    ad-do-it
    (process-send-string
     gis-process
     "_block 
        !terminal!.put(%x.from_value(1))
        !terminal!.put(%W)
        !terminal!.put(%x.from_value(5))
        !terminal!.put(%space)
    _endblock\n$\n")
    (sit-for 0.1)
    (let ((n 0))
      (while (and gis-prompt-wait (<= n gis-newline-waits))
	(setq n (1+ n))
	(sit-for 0.1)
	(message "Waiting for command (%d)..." n)
	(funcall 'gis-newline-wait-function n))
      (message "%sWaited %d seconds ...done"
	       (if (>= n gis-newline-waits) "Command unfinished. " "")
	       n))))

;;;Functions
(defun test-harness (&optional switch)
  "Enable/disable/toggle the Emacs test harness for GIS.

With a prefix arg, ask user for Time out Mode to use."
  (interactive "P")
  (if current-prefix-arg
      (let ((mode (completing-read (format "Time out Mode (current: %s): " test-harness-timeout-mode)
				   '(("gis" 1) ("emacs" 2) ("none" 3))
				   nil t)))
	(or (equal mode "")
	    (setq test-harness-timeout-mode (intern mode))))) ;intern - convert to symbol

  (if (eq test-harness-timeout-mode 'none)
      (progn
	(message "Time out mode not set. Use CLI -tmode or a prefix arg on this command")
	(setq switch 'disable)))
  (cond ((or (eq switch 'enable) (and (null switch) (not test-harness)))
	 (setq test-harness (if (and (stringp frame-title-format)
				     (equal frame-title-format "Emacs SmallWorld Test Harness"))
				test-harness ;Do not change if test-harness is forced on.
			      frame-title-format)
	       frame-title-format "Emacs SmallWorld Test Harness"
	       icon-title-format frame-title-format) ;prevent icon changing name - confuses Winrunner

	 (cond ((eq test-harness-timeout-mode 'gis)
		(remove-hook 'kill-emacs-hook 'test-harness-save-special-buffers)
		(add-hook 'gis-sentinel-hooks 'test-harness-save-special-buffers)
		(add-hook 'gis-sentinel-hooks
			  (lambda (exit-code) (if (and (not test-harness-timed-out)
						       test-harness-emacs-exit)
						  (kill-emacs exit-code)))
			  t))
	       ((eq test-harness-timeout-mode 'emacs)
		(remove-hook 'gis-sentinel-hooks 'test-harness-save-special-buffers)
		(add-hook 'kill-emacs-hook 'test-harness-save-special-buffers)))
	 (ad-enable-advice 'gis-newline 'around 'gis-synchronised-process)

	 (message "Test Harness Enabled"))
	((or (eq switch 'disable) (and (eq switch nil) test-harness))
	 (setq frame-title-format (or test-harness frame-title-format)
	       icon-title-format  (or test-harness icon-title-format)
	       test-harness nil)
	 ;;Remove fns from all possible hooks that are used by the timeout mechanism 
	 (remove-hook 'gis-sentinel-hooks 'test-harness-save-special-buffers)
	 (remove-hook 'kill-emacs-hook 'test-harness-save-special-buffers)
	 (ad-disable-advice 'gis-newline 'around 'gis-synchronised-process)

	 (message "Test Harness Disabled")))
  (and (fboundp 'w32-frame-restore) (w32-frame-restore)) ;;On windows ensure frame is restored
  (and test-harness t)			;returns nil or t
  )

(defun test-harness-timer-cancel ()
  (interactive)
  (if test-harness-timer-object
      (cancel-timer test-harness-timer-object)
    (message "No timer set")))

(defun test-harness-timer-set (&optional time)
  (interactive "nTime out: ")
  (or time (setq time test-harness-timeout-time))
  (if (zerop time)
      (message "No timer set")
					;first remove old timer if there is one
    (and test-harness-timer-object (cancel-timer test-harness-timer-object))
					;set the timer
    (setq test-harness-timer-object (run-at-time (concat (number-to-string time) " sec")
						 nil 'test-harness-timeout (concat (number-to-string time) " seconds are up")))
    (message "Timer set for %d seconds from now" time)))

(defun test-harness-save-special-buffers (&optional dummy)
  "This function saves the following buffers to log files in directory given by test-harness-special-buffers-log-path.
*Messages*   => logfile.emacs_messages
'gis-buffer' => logfile.gis_output
This function is used just before Emacs is killed."
  (if test-harness-special-buffers-log-path
      (save-excursion
	(let ((require-final-newline t)	;Ensure file ends with a newline for neatness
	      (select-safe-coding-system-function nil) ;ignore the coding system for the files
	      )
	  (mapc '(lambda (buffer-file)
		   (let* ((buffer (eval (eval (car buffer-file)))) ;strings eval to themselves
			  (file (concat test-harness-special-buffers-log-path (cadr buffer-file)))
			  (bakfile (concat file ".bak")))
		     (if (file-exists-p bakfile)
			 (progn
			   (delete-file bakfile)
			   (rename-file file bakfile t)))		       
		     (and (get-buffer buffer)
			  (set-buffer buffer)
			  (write-file file))))
		test-harness-special-buffers-to-files-alist)))))

(defun test-harness-timeout (msg)
  "This function is called when a time out event has occured"
  (if (and gis-process	       ;AJM: what about class browser process?
	   (eq (process-status gis-process) 'run))
      ;;Ensure that the gis process is stopped
      (progn
	(process-send-string gis-process
			     (concat "!output!.write(\"*** ERROR: TIMEOUT "
				     msg 
				     " ***\")\n$\n"))
	(message "*** ERROR: TIMEOUT :- %s ***" msg)
	;;sit-for is required because there are some things that are done during
	;;Emacs idle time that need to happen. sit-for allows these things to run
	;;before continuing with the rest of the lisp.

	(sit-for 1) ;if omitted the output messages is not written to the *gis* or *Message* buffers

	;;prevent the gis-sentinel process from killing Emacs because we want to kill it later on
	(setq test-harness-timed-out t)

	;;must use kill-process/delete-process since quit-process does not work on Windows
	(delete-process gis-process)

	(sit-for 1))) ;if omited you are still asked "active processes exist quit? y/n"
       
  ;;Some test harnesses, e.g. WinRunner, require a way of informing them that a timeout has occured
  (if test-harness-timeout-popup (test-harness-timeout-popup-function))

  (if (not test-harness-emacs-exit)
      (message "TIMEOUT occured... but not exiting Emacs")
    (message "TIMEOUT occured... Exiting Emacs")
    (sit-for 0.01)	 ;Allow message to appear in *Messages* buffer
    (run-hooks 'test-harness-timeout-hook)
    (kill-emacs 9)))

(defun test-harness-timeout-popup-function ()
  "This function is used to display a popup window when a timeout is reached.
This is required for some test harnesses eg. WinRunner.
If the variable `test-harness-msgbox' is set then it is run with two arguments

   'test-harness-msgbox' MESSAGE WINDOW_NAME

otherwise, when it is nil, the default action to do
 on NT   is 'net send' to display a popup window.
 on UNIX is nothing. We could use the lisp function x-popup-dialog
         but this blocks Emacs from further processing and so is not implemented."
  (interactive)
  (if (running-under-nt)
      (save-excursion
	(set-buffer "*Messages*")

	(if test-harness-msgbox
	    (progn
	      (insert "starting " test-harness-msgbox "\n")
	      (call-process test-harness-msgbox nil 0 nil
			    "Test Harness TIMEOUT" "Messenger Service"))
	  
	  (insert "starting net send:\n")
	  (insert "Exit code: "
		  (number-to-string
		   (call-process "net" nil "*Messages*" nil
				 "send" (getenv "COMPUTERNAME") "Test Harness TIMEOUT"))
		  "\n")))

    ;;(x-popup-dialog t '("Test Harness TIMEOUT" ("Ok" t)))
    ))

(defun test-harness-enable ()
  "Enable test-harness. Used by `auto-gis-post-hook'. See \\[test-harness]."
  (test-harness 'enable))

(defun test-harness-gis-filter-action-gis-prompt-wait (proc str)
  "Gis Filter Action that synchronises the GIS with Emacs."
  (save-excursion
    (set-buffer (buffer-name (process-buffer proc)))
    (set 'gis-prompt-wait nil)))

(defun test-harness-command-line-parser ()
  "Add test harness command line options to Emacs.
These are used in conjunction with those provided by \\[auto-gis-command-line-parser].

Additional command line options are:

-timeout       takes a time out value in seconds. If the time out is
               reached, the gis process is killed.

-notimeout     This will cause Emacs to ignore any timeout set
               i.e. Emacs will not quit.

-timeout-popup  This will cause Emacs to popup a dialog box indiciating
                a timeout event has occured.

-logfile        Emacs will use this filename as a template to save the
                buffers *Message* and *gis* by simply adding a postfix
                to it. eg. for -logfile=/tmp/my_logfiles.log
                Emacs will create additional logfiles:
                /tmp/my_logfiles.log.emacs_gis_buffer and
                /tmp/my_logfiles.log.emacs_messages

-tmode          The timeout mode to use:
                gis    - timeout operates at gis program level
                emacs  - timeout operates at emacs level
                 The different modes determine the behaviour after a
                 timeout occurs.
                  If you want to allow a gis session to
                 start and stop several times then the timeout must
                 operate on the Emacs application level
                 i.e. on the kill-emacs-hook
                  If you just want to start a single gis session
                 and kill the gis session when the timeout is reached
                 then the timeout is operating at the gis application level
                 i.e. gis-sentinel-hooks. (The Emacs application is
                 just a facilitator in this case.)
                 
"

  (interactive)
  ;;argi is dynamically buond in the command-line parsing code of Emacs.
  (cond ((equal argi "-timeout")
	 (setq test-harness-emacs-exit t)
	 (setq test-harness-timeout-time (string-to-number (car command-line-args-left)))
	 (setq command-line-args-left (cdr command-line-args-left))
	 t)
	((equal argi "-notimeout")
	 (message "Emacs will not exit on timeout")
	 (setq test-harness-emacs-exit nil)
	 t)
	((equal argi "-timeout-popup")
	 (message "Emacs will Popup a Dialog box indicating a timeout has occured")
	 (setq test-harness-timeout-popup t)
	 t)
	((equal argi "-tmode")
	 (setq test-harness-timeout-mode (intern (car command-line-args-left)))
	 (message "Time out Mode: %s" test-harness-timeout-mode)
	 (setq command-line-args-left (cdr command-line-args-left))
	 t)
	((equal argi "-logfile")
	 (setq test-harness-special-buffers-log-path (car command-line-args-left))
	 (message "will save *Messages* and *gis* buffers to %s.*" test-harness-special-buffers-log-path)
	 (setq command-line-args-left (cdr command-line-args-left))
	 t)
	(t nil)
	))

;;; Package initialisation
;;Keep all *Message* buffer entries:
(setq message-log-max t)

(add-hook 'auto-gis-post-hook 'test-harness-timer-set)
(add-hook 'auto-gis-post-hook 'test-harness-enable)

(gis-filter-register-action "W" 'test-harness-gis-filter-action-gis-prompt-wait)

;; Command-line parsing will only operate if this file is itself loaded, -l, from the command line 
(add-to-list 'command-line-functions 'test-harness-command-line-parser)

;; f2 is a hot key that is active even when WinRunner is not the active application
;; f2 toglles WinRunner record mode and so it is annoying when it is activated accidently.
(define-key global-map     [(meta f2)] 'sw-f2-map)
(define-key gis-mode-map   [(meta f2)] 'gis-f2-map)
(define-key magik-mode-map [(meta f2)] 'magik-f2-map)

(provide 'test-harness)
