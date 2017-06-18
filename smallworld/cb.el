;;; cb.el -- Class Browser for Magik methods and classes.
;;
;;; Works in conjunction with the method_finder C program.
;;
;;        The Smallworld Class Browser  -  CB
;;        -----------------------------------
;;
;; There is a main cb buffer that has to be called "*cb*" and a collection of
;; global rather than buffer-local variables.  Note that this doesn't
;; completely rule out the use of more than one cb at a time because the main
;; cb buffer could be renamed and its state taken with it in some form.
;; Also cb processes are fairly heavy and if someone really wants another
;; cb they can start another emacs.
;;
;; The subsidiary cb buffer, for want of a better name, is called "*cb2*"
;; and is meant to be temporary.  (All the useful cb state is kept in the
;; cb globals, which makes debugging a lot easier because the globals are
;; readable and settable from the lisp buffers).  At different times the
;; "*cb2*" buffer behaves in quite different ways (topic selection,
;; family-tree display, full method details display), but its mode is
;; always cb-mode (just like the main "*cb*" buffer).
;;
;; Entry to the cb is usually via F3 F3 which basically does "C-x 4 b
;; *cb*".  We also record some details of the window configuration so
;; that exit is smoother.  A typical window layout at Smallworld is a
;; Magik window at the top and the gis at the bottom.  When they first
;; press F3 F3, the cb replaces the gis in the bottom window, and more or
;; less stays there while any temporary subsidiary action takes place in
;; the top window.  This is so that the user can see the documentation in
;; relation to the source code they were pondering at the time.  Exit is
;; via the space key or F3 q and will destroy any subsidiary buffers on
;; the first press and bury the main cb buffer on the second press.  i.e.
;; the user backs out of the cb in the two stages that he came in by.
;;
;; The cb process is called "cb" and the process itself is kept in the
;; local variable, cb-process.
;;
;; The only useful state that is kept in the "*cb*" buffer is the last
;; dollop of output from the C, and the position the user has moved point
;; to (so that the alphabetical position can be preserved).  It does no
;; harm if the contents get lost, because all the state is kept in the cb
;; globals.  However, the buffer has to be kept because the cb process
;; belongs to it.
;;
;; We talk to the cb process with "(cb-send-string str ...)".
;;
;; The distinction between topics and flags is now blurred.  They are
;; both represented in one global association list, cb-topics.
;;
;; 
;; 12/7/94 main changes:
;;
;;   simplified startup because the method finder automatically
;;   does print_curr_methods after loading a file.
;;
;;   much less initialisation parameters being sent because there
;;   is a reset in the method_finder that matches what we want, and
;;   because the method_finder sets the clients queries to how we
;;   want as well.
;;
;;   startup is further simplified by only allowing F3 F3 as the
;;   the startup command.

;;shut compiler up...
(eval-when-compile
  (require 'cl)
  (defvar mode-line-format-sym)
  (defvar msb-menu-cond)
  (defvar ac-triggered)
  (defvar ac-prefix)
  (defvar ac-limit)

  (require 'macros-sw)
  (require 'misc-sw)
  (require 'utils-sw)
  (require 'sw-help)
  (require 'magik)
  (defvar gis-buffer)
  ;;Cannot require 'gis because of circular dependency
  )

(require 'cl)

(require 'macros-sw)
(require 'misc-sw)
(require 'utils-sw)

(defgroup cb nil
  "Running Magik Class Browser."
  :tag "Class Browser"
  :group 'smallworld
  :group 'tools)

;; Could be in oop group but it is empty in Emacs 21.
(defconst cb-version "$Revision: 1.69 $")  ; as opposed to "cb202".

(defconst cb-in-keyword (concat "  " resources-cb-in "  ")
  "The method 'IN' class keyword.")

(defgroup cb-faces nil
  "Fontification colours for Class Browser."
  :group 'cb)

(defface cb-font-lock-optional-face
   '((((type tty) (class color)) (:foreground "yellow" :weight light))
     (((class grayscale) (background light))
      (:foreground "Gray90" :bold t))
     (((class grayscale) (background dark))
      (:foreground "DimGray" :bold t))
     (((class color) (background light)) (:bold t :foreground "DarkGoldenrod"))
     (((class color) (background dark)) (:bold t :foreground "LightGoldenrod"))
     (t (:bold t)))
  "Font-lock Face to use when displaying _optional variables.

Based upon `font-lock-variable-name-face'"
  :group 'cb-faces)

(defface cb-cursor-face
  (if xemacs-p ;XEmacs cannot display background data on its mode-line and does not support inverse video either
      '((t (:foreground "Grey50")))
    '((t (:inverse-video t))))
  "Face to use for the Mode line cursor."
  :group 'cb-faces)

;; Originally just italic, but due to GDI object leak, made bold too - see magik.el for more details.
(defface cb-font-lock-gather-face
  '((((type tty) (class color)) (:foreground "yellow" :weight light))
    (((class grayscale) (background light))
     (:foreground "Gray90" :italic t))
    (((class grayscale) (background dark))
     (:foreground "DimGray" :italic t))
    (((class color) (background light)) (:italic t :foreground "DarkGoldenrod" :bold t))
    (((class color) (background dark)) (:italic t :foreground "LightGoldenrod" :bold t))
    (t (:italic t)))
  "Font-lock Face to use when displaying _gather variables.

Based upon `font-lock-variable-name-face'"
  :group 'cb-faces)

(defcustom cb-mode-hook '()
  "*Hook for customising CB mode."
  :group 'cb
  :type 'hook)

(defcustom cb-font-lock-class-face 'font-lock-type-face
  "*Font-lock Face to use when displaying the class."
  :group 'cb
  :type 'face)

(defcustom cb-font-lock-method-face 'font-lock-function-name-face
  "*Font-lock Face to use when displaying the method name."
  :group 'cb
  :type 'face)

(defcustom cb-font-lock-optional-face 'cb-font-lock-optional-face
  "*Font-lock Face to use when displaying the _optional variables."
  :group 'cb
  :type 'face)

(defcustom cb-font-lock-gather-face 'cb-font-lock-gather-face
  "*Font-lock Face to use when displaying the _gather variable."
  :group 'cb
  :type 'face)

(defcustom cb-cursor-face 'cb-cursor-face
  "*Face to use for the Mode line cursor."
  :group 'cb
  :type 'face)

(defcustom cb-font-lock-keywords
  `(("\\*\\*\\*.*" . font-lock-comment-face)
    ("##.*$" . font-lock-doc-face)
    (,(concat "\\(.*\\)" cb-in-keyword "\\(\\S-+\\)")
     (1 cb-font-lock-method-face)
     (2 cb-font-lock-class-face))
    ("^\\(\\S-+\\)$" . cb-font-lock-method-face)
    ("^\\s-+\\(.*\\)\\(OPT.+\\)\\(GATH.+\\)"
     (1 font-lock-variable-name-face)
     (2 cb-font-lock-optional-face)
     (3 cb-font-lock-gather-face))
    ("^\\s-+\\(.*\\)\\(GATH.+\\)"
     (1 font-lock-variable-name-face)
     (2 cb-font-lock-gather-face))
    ("^\\s-+\\(.*\\)\\(OPT.+\\)"
     (1 font-lock-variable-name-face)
     (2 cb-font-lock-optional-face))
    ("^\\s-+.*$" . font-lock-variable-name-face)
    )
  "*Font lock setting for Class Browser fontification."
  :group 'cb
  :type  'sexp)

;; User configuration options
(defcustom cb-jump-replaces-cb-buffer nil
  "*If t, then when jumping to a source file, via \\[cb-jump-to-source],
the file buffer replaces the *cb* buffer.
If nil, the file is displayed in another window and also keeps the *cb* buffer
visible.

The situation where it is useful to set this to t is as follows:
you have two buffers, one with a magik file, the other with
the class browser. If you jump to a file containing a method,
the file containing the method will replace the window displaying the class
browser. Thus, you now have two windows one displaying your magik file
the other displaying the source file containing the method.

You can now use Ediff to compare the buffers!"
  :group 'cb
  :type  'boolean)

(defcustom cb-generalise-file-name-alist nil
  "*An Alist used to modify paths returned by method finder.
Each element is a cons cell (REGEXP . PATH), where REGEXP is matched against
the file name and PATH is the string that replaces a match of REGEXP."
  :group 'cb
  :type  '(alist :key-type regexp :value-type string))

(defcustom cb-coding-system (if (and (boundp 'coding-system-alist)
				  (assoc "utf-8" coding-system-alist))
			     'utf-8
			   'iso-8859-1)
  "*Coding system to use for reading the temp file output from the CB process."
  :group 'cb
  :type  'coding-system)

(defcustom cb-mode-line-cursor (cond (xemacs-p "|") ;XEmacs cannot display background data on its mode-line
				     ((or emacs19 emacs20) "'") ; Early GNU Emacsen cannot either
				     (t " "))
  "*String to use as the cursor in the mode-line.
`cb-cursor-face' is also used to modify the display of the character
Can be set using \\[cb-set-mode-line-cursor]."
  :group 'cb
  :type  'string)

(defcustom cb-debug nil
  "Set to t to enable debugging output from the C."
  :group 'cb
  :type  'boolean)

;In case used in a version of Emacs prior to 20
(or (fboundp 'set-process-coding-system)
    (defalias 'set-process-coding-system 'ignore)) 

(defvar cb-buffer-alist nil
  "Alist storing CB buffer filename and number used for prefix key switching.")

(defvar cb-process nil
  "method finder process.")
(put 'cb-process 'permanent-local t)

(defvar cb-topics nil
  "Alist of topics and flags.")
(put 'cb-topics 'permanent-local t)

(defvar cb-was-one-window nil
  "t if the cb was started from an unsplit-screen configuration.")

(defvar cb-was-started-from-top-half nil
  "If the screen was split this tells us whether the cb was invoked
from the top-most window or not.")

(defvar cb-quote-file-name nil
  "If t, then method_finder accepts a quoted filename when the file path contains spaces.
Only supported in method_finder version 5.3.0 and above") 
(put 'cb-quote-file-name 'permanent-local t)

(defvar cb-mf-extended-flags nil
  "If t, then method_finder accepts queries with deprecated and restricted flags.
Only support in method_finder version 6.0.0 and above")
(put 'cb-mf-extended-flags 'permanent-local t)

(defvar cb-temp-method-name nil
  "If not nil, name of method used in last pr_source_file command when F3 J is 
done from a Magik buffer.")

(defvar cb-filename nil
  "Name of file used for the standalone CB session.")
(put 'cb-filename 'permanent-local t)

(defvar cb-filter-str nil
  "Contains unprocessed data coming back from the C.
This has to be kept between calls to the filter because the
data can return in different size lumps.")
(put 'cb-filter-str 'permanent-local t)

(defvar cb-n-methods-str nil
  "A string (possibly of the form \">200\") as returned by the C.
This is displayed in the modeline.")
(put 'cb-n-methods-str 'permanent-local t)

(defvar cb-topic-pos nil
  "Where the user's cursor was when they last left the topics selection mode.
We don't rely on the state of the \"*cb2*\" buffer because it is only temporary.")
(put 'cb-topic-pos 'permanent-local t)

(defvar cb-cursor-pos nil
  "Whether the CB modeline cursor is in the method or class part of the modeline.
Takes the values 'method-name and 'class-name.")
(put 'cb-cursor-pos 'permanent-local t)

(defvar cb-pending-message nil
  "Whether we should write an empty message when the method_finder gives us an answer.
This will stop the \"Loading documentation...\" message from hanging around.")
(put 'cb-pending-message 'permanent-local t)

(defvar cb-dynamic t
  "*Non-nil if the cb is connected to a live gis, rather than a static file.")

(defvar cb-mode-map (make-keymap)
  "Keymap for the Class Browser")

(defvar cb-menu nil
  "Keymap for the CB menu bar")

(easy-menu-define cb-menu cb-mode-map
  "Menu for CB mode."
  `(,resources-cb-menu
    [,resources-cb-menu-jump           cb-jump-to-source        :active t :keys "f3 j, mouse-2"]
    [,resources-cb-menu-family-tree    cb-family                :active t :keys "f3 f, mouse-2"]
    [,resources-cb-menu-fold           cb-fold                  :active (or (cb-topic-on-p "show-topics")
									     (cb-topic-on-p "show-comments")
									     (cb-topic-on-p "show-args")
									     (cb-topic-on-p "show-classes"))
				                                :keys "f3 up"]
    [,resources-cb-menu-unfold         cb-unfold                :active (or (not (cb-topic-on-p "show-topics"))
									     (not (cb-topic-on-p "show-comments"))
									     (not (cb-topic-on-p "show-args"))
									     (not (cb-topic-on-p "show-classes")))
				                                :keys "f3 down"]
    "---"                        
    [,resources-cb-menu-options        cb-edit-topics-and-flags :active t :keys "f3 s, ;"]
    [,resources-cb-menu-toggle-topics  cb-toggle-all-topics     :active t :keys "f3 t"]
    [,resources-cb-menu-reset          cb-reset                 :active t :keys "f3 r"]
    [,resources-cb-menu-quit           cb-quit                  :active t :keys "space, f3 h"]
    "---"
    [,resources-cb-menu-toggle-override-flags
     cb-toggle-override-flags
     :active t
     :style toggle
     :selected (cb-topic-on-p "override-flags")
     :keys "f3 F, f3 o"]
    [,resources-cb-menu-toggle-override-topics
     cb-toggle-override-topics
     :active t
     :style toggle
     :selected (cb-topic-on-p "override-topics")
     :keys "f3 T"]
    [,resources-cb-menu-toggle-override-200-limit
     cb-toggle-override-200-limit
     :active t
     :style toggle
     :selected (cb-topic-on-p "override-200-limit")
     :keys "f3 2"]
    "---"
    [,resources-cb-menu-tab            cb-tab                   t]
    [,resources-cb-menu-clear          cb-clear                 t]
    [,resources-cb-menu-clear-m-c      cb-and-clear             :active t :keys "f3 /"]
    "---"
    [,resources-cb-menu-gis            cb-gis                   :active (get-buffer (cb-gis-buffer)) :keys "f3 g"]
    [,resources-cb-menu-gis-shell      cb-gis-shell             :active (get-buffer
									  (concat "*shell*" (cb-gis-buffer)))
				                                :keys "f3 $"]
    "---"
    [,resources-menu-sw-customize      cb-customize             t]
    [,resources-menu-sw-help           cb-help                  t]))

(defvar cb--mf-socket-synchronised nil
  "Internal variable for controlling Class Browser processes started from GIS processes.
Set to the socketname returned by `gis-filter-action-cb-mf' when starting CB from Gis process via \\[cb].")

;; T O P I C   A N D   F L A G   D A T A
;; _____________________________________
;;
(defvar cb-initial-topics
  '(
    ("basic"                     t     "add basic"            "unadd basic")
    ("advanced"                  nil   "add advanced"         "unadd advanced")
    ("subclassable"              nil   "add subclass"         "unadd subclass")
    ("redefinable"               nil   "add redefinable"      "unadd redefinable")
    ("debug"                     t     "add debug"            "unadd debug")

    ("restricted"                nil   "add restricted"       "unadd restricted")
    ("deprecated"                nil   "add deprecated"       "unadd deprecated")
    
    ("local-only"                t     "local_only")
    ("inherit-not-\"object\""  t     "inherit_not_obj")
    ("inherit-from-\"object\"" t     "inherit_all")

    ("show-methods"      t     "show_method_names"    "dont_show_method_names")
    ("show-classes"      t     "show_classes"         "dont_show_classes")
    ("show-args"         nil   "show_args"            "dont_show_args")
    ("show-comments"     nil   "show_comments"        "dont_show_comments")
    ("show-topics"       nil   "show_topics"          "dont_show_topics")

    ("override-flags"       nil    "override_flags"    "dont_override_flags")
    ("override-topics"      nil    "override_topics"   "dont_override_topics")
    ("override-200-limit"   nil   "method_cut_off 1000000"   "method_cut_off 200")
    )
  "An association list of all the topics and flags together with
their on/off status and their corresponding commands for sending
to the C.")

(defvar cb-flag-groups
  '(("basic" "advanced" "subclassable" "redefinable" "debug" "deprecated" "restricted"))
  "List of CB flags.")

(defvar cb-thermometer-group
  '("local-only"  "inherit-not-\"object\""  "inherit-from-\"object\"")
  "A set of 3 flags that should only be on if all the previous ones are on.")

;; C B 2
;; _____

(defvar cb2-mode nil
  "The sort of data the subsidiary cb buffer is displaying.
We use this variable instead of inventing new major modes
because the keymaps in all these modes will be the same anyway.")

(defvar cb2-direct-p nil
  "Whether the user got directly into \"*cb2*\" without going via \"*cb*\".
This affects the way we might want to exit.
Not used yet.")

(defvar cb2-was-one-window nil
  "t if the cb2 was started from an unsplit-screen configuration.")

(defvar cb--ac-candidates nil
  "Internal return value from CB auto-complete process.")

(defvar cb-ac-process nil
  "Class Browser process object to use for auto-complete-mode.")

(defcustom cb2-font-lock-on-face 'font-lock-function-name-face
  "*Font-lock Face to use when displaying the variable."
  :group 'cb
  :type 'face)

(defcustom cb2-font-lock-off-face 'font-lock-variable-name-face
  "*Font-lock Face to use when displaying the variable."
  :group 'cb
  :type 'face)

(defcustom cb2-font-lock-thermometer-on-face 'font-lock-type-face
  "*Font-lock Face to use when displaying a thermometer variable that is on."
  :group 'cb
  :type 'face)

(defcustom cb2-font-lock-thermometer-off-face 'font-lock-constant-face
  "*Font-lock Face to use when displaying a thermometer variable that is off."
  :group 'cb
  :type 'face)

(defcustom cb2-font-lock-keywords
  '(
    ("[+] \\(\\sw+\\)" 1 cb2-font-lock-on-face)
    ("[-] \\(\\sw+\\)" 1 cb2-font-lock-off-face)
    ("[*] \\(\\sw+\\)" 1 cb2-font-lock-thermometer-on-face)
    ("[.] \\(\\sw+\\)" 1 cb2-font-lock-thermometer-off-face)
    ("^    .*$" . font-lock-doc-face)
    )
  "*Font lock setting for Class Browser fontification."
  :group 'cb
  :type  'sexp)

(defcustom cb2-mode-hook '()
  "*Hook for customising CB toggle mode (CB2)."
  :group 'cb
  :type 'hook)

;;; Functions
;;; _________

(defun cb-gis ()
  "Start/goto GIS with the same environment as the current CB process."
  (interactive)
  (let ((buf (cb-gis-buffer)))
    (if (one-window-p t)
	(split-window-vertically)
      (other-window 1))
    (gis buf)))

(defun cb-gis-shell ()
  "Start a command shell with the same environment as the current CB process."
  (interactive)
  (let ((gis (cb-gis-buffer)))
    (save-excursion
      (set-buffer gis)
      (gis-shell))))

(defun cb-customize ()
  "Open Customization buffer for Class Browser Mode."
  (interactive)
  (customize-group 'cb))

;; S T A R T U P
;; _____________
;;;autoload
(defun cb (&optional gis method class)
  "Start or resume a Smallworld Class Browser.

With a prefix arg, ask user for GIS buffer to associate with.

Main top level entry to the cb.

Create the buffer and/or start the process if necessary.
Do a no-op if already in the cb."
  (interactive)
  (let (cb-file running-p buffer gis-proc visible-bufs bufs)
    (cond ((and (integerp current-prefix-arg) (> current-prefix-arg 0))
	   (setq gis (sw-get-buffer-mode gis
					 'gis-mode
					 resources-gis-enter-buffer
					 (cond ((eq major-mode 'cb-mode) (cb-gis-buffer))
					       ((eq major-mode 'gis-mode) (buffer-name))
					       (t gis-buffer))
					 'gis-buffer-alist-prefix-function))
	   (unless (get-buffer-process gis)
	     (pop-to-buffer gis)
	     (error resources-gis-no-process-error))
	   (unless (get-buffer gis)
	     (pop-to-buffer gis)
	     (error resources-cb-none-running-error)))
	  ((and (integerp current-prefix-arg) (< current-prefix-arg 0))
	   (setq buffer (sw-get-buffer-mode nil
					    'cb-mode
					    resources-cb-enter-buffer
					    nil
					    'cb-buffer-alist-prefix-function
					    nil
					    'cb-filename))
	   (unless (get-buffer buffer)
	     (pop-to-buffer buffer)
	     (error resources-cb-none-running-error)))
	  (current-prefix-arg
	   (setq cb-file (cb-set-filename)
		 buffer (generate-new-buffer-name
			 (concat "*cb*" "*" (or buffer (file-name-nondirectory cb-file)) "*"))
		 gis    (cb-gis-buffer buffer)))
	  ((eq major-mode 'cb-mode)
	   (setq gis (cb-gis-buffer)))
	  ((eq major-mode 'gis-mode)
	   (setq gis (buffer-name)))
	  ((and ;List of *visible* cb-mode *and* gis-mode buffers.
	    (setq bufs
		  (delete nil
			  (mapcar (function (lambda (b) (if (cdr b) b)))
				  (setq visible-bufs
					(sw-buffer-visible-list '(cb-mode gis-mode))))))
	    ;;restrict list to those whose cdr is t.
	    (setq buffer
		  (if (= (length bufs) 1)
		      (caar bufs)
		    (completing-read
		     (concat resources-cb-or-gis-enter-buffer " ")
		     visible-bufs 'cdr t)))
	    (not (equal buffer "")))
	   (if (equal (substring buffer 0 4) "*cb*")
	       nil ;;Selected a CB buffer
	     (setq gis buffer
		   buffer (concat "*cb*" buffer))))
	  ((and
	    visible-bufs
	    (setq buffer
		  ;;Find visible CB buffer in other frame, allowing for a visible GIS buffer too.
		  (cond ((= (length visible-bufs) 1)
			  (caar visible-bufs))
			 ((and (= (length visible-bufs) 2)
			       (equal (substring (caar visible-bufs) 0 4) "*cb*"))
			  (caar visible-bufs))
			 ((and (= (length visible-bufs) 2)
			       (equal (substring (caadr visible-bufs) 0 4) "*cb*"))
			  (caadr visible-bufs))
			 (t
			  (completing-read
			   (concat resources-cb-or-gis-enter-buffer " ")
			   visible-bufs nil t))))
	    (not (equal buffer "")))
	   (select-frame-set-input-focus
	    (window-frame (get-buffer-window buffer 'visible)))
	   (if (equal (substring buffer 0 4) "*cb*")
	       nil ;;Selected a CB buffer
	     (setq gis buffer
		   buffer (concat "*cb*"  buffer))))
	  ((setq buffer (sw-get-buffer-mode nil
					    'cb-mode
					    resources-cb-enter-buffer
					    (let ((cb (concat "*cb*" gis-buffer)))
					    (if (get-buffer cb) cb))))
	   t)
	  ((and gis-buffer (get-buffer gis-buffer) (get-buffer-process gis-buffer))
	   (setq gis gis-buffer))
	  (t
	   (setq cb-file (cb-set-filename)
		 buffer (generate-new-buffer-name
			 (concat "*cb*" "*" (file-name-nondirectory cb-file) "*"))
		 gis    (cb-gis-buffer buffer))))

    (setq buffer   (or buffer (concat "*cb*" gis))
	  gis-proc (and gis (get-buffer-process gis)))
    
    (cond ((cb-is-running buffer)
	   (setq running-p t
		 cb-process (get-buffer-process buffer)))
	  ((and cb-dynamic gis-proc)
	   (setq buffer (get-buffer-create buffer)))
	  (t
	   (setq gis-proc nil)))
    (pop-to-buffer buffer)
    (cb-mode)
    
    (if (not running-p)
	(progn
	  (setq cb-process (cb-get-process-create buffer 'cb-filter gis cb-file))
	  (cb-interactive-buffer)
	  (sleep-for 0.1)))

    (if (not cb-process)
	(error resources-cb-not-running-error (current-buffer)))

    (if (cb-set-method-and-class method class)
	(cb-send-modeline-and-pr)
      (cb-redraw-modeline))

    (cb-set-windows)))

(defun cb-new-buffer ()
  "Start a new Class Browser session."
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively 'cb)))

;; This function is not a user-level entry-point.  It is just a place
;; to put the mode help.
(defun cb-mode ()
  "Major mode for running the Smallworld Class Browser.
   Full help is available on the CB pull-down menu or by typing

  M-x cb-help

Useful configuration variables are:

cb-jump-replaces-cb-buffer

To view the help on these variables type C-h v [Return] [variable-name]"
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'cb-process)
  (make-local-variable 'cb-topics)
  (make-local-variable 'cb-quote-file-name)
  (make-local-variable 'cb-mf-extended-flags)
  (make-local-variable 'cb-filename)
  (make-local-variable 'cb-filter-str)
  (make-local-variable 'cb-n-methods-str)
  (make-local-variable 'cb-topic-pos)
  (make-local-variable 'cb-cursor-pos)
  (make-local-variable 'cb-pending-message)

  (make-local-variable 'font-lock-defaults)

  ;(make-local-hook 'kill-buffer-hook) ;add-hook uses local option

  (use-local-map cb-mode-map)
  (easy-menu-add cb-menu)
  (set-syntax-table magik-mode-syntax-table)

  (setq major-mode 'cb-mode
	buffer-read-only t
	buffer-undo-list t
	font-lock-defaults '(cb-font-lock-keywords nil t ((?_ . "w"))))

  (add-hook menu-bar-update-hook-sym 'cb-update-sw-menu)
  (add-hook 'kill-buffer-hook 'cb-buffer-alist-remove nil t) ;local hook
  (run-hooks 'cb-mode-hook))

(defun cb-gis-buffer (&optional buffer)
  "Return the GIS process buffer associated with this Class Browser."
  (setq buffer (if (bufferp buffer) (buffer-name buffer) (or buffer (buffer-name))))
  (let ((cb-bit (substring buffer 0 5)))
    (if (equal cb-bit "*cb2*")
	(substring buffer 5)
      (substring buffer 4))))

(defun cb-buffer (&optional buffer)
  "Name of the CB buffer."
  (get-buffer-create (concat "*cb*" (cb-gis-buffer buffer))))

(defun cb-process (&optional buffer)
  "Process object of the CB buffer.
If `cb-process' is not nil, returns that irrespective of given BUFFER."
  (or cb-process (get-buffer-process (cb-buffer buffer))))

(defun cb-topics (&optional newval buffer)
  "Get/Set `cb-topics' variable from the CB buffer."
  (save-excursion
    (set-buffer (cb-buffer buffer))
    (if newval
	(setq cb-topics newval)
      cb-topics)))

(defun cb-cursor-pos (&optional newval buffer)
  "Get/Set `cb-cursor-pos' variable from the CB buffer."
  (save-excursion
    (set-buffer (cb-buffer buffer))
    (if newval
	(setq cb-cursor-pos newval)
      cb-cursor-pos)))

(defun cb-mf-extended-flags (&optional buffer)
  "Get `cb-mf-extended-flags' variable from the CB buffer."
  (save-excursion
    (set-buffer (cb-buffer buffer))
    cb-mf-extended-flags))

(defun cb-buffer-alist-remove ()
  "Remove current buffer from `cb-buffer-alist'."
  (let ((c (rassoc (buffer-name) cb-buffer-alist)))
    (if c
	(progn
	  (setcdr c nil)
	  (car c)))))

(defun cb-buffer-alist-prefix-function (arg mode predicate)
  "Function to process prefix keys when used with \\[cb]."
  (let (buf)
    (cond ((zerop arg) (set buf nil))
	  ((> arg 0)
	   ;; Look for GIS buffers
	   (setq buf (cdr (assq arg (symbol-value 'gis-buffer-alist))))
	   (unless (and buf
			(save-excursion
			  (set-buffer buf)
			  (sw-buffer-mode-list-predicate-p predicate)))
	     (error resources-gis-no-process-error)))
	((< arg 0)
	 ;; Look for CB buffers
	 (setq buf (cdr (assq arg cb-buffer-alist)))
	 (unless (and buf
		      (save-excursion
			(set-buffer buf)
			(sw-buffer-mode-list-predicate-p predicate)))
	   (error resources-cb-none-running-error))))
    buf))

(defun cb-update-sw-menu ()
  "Update CB submenu in SW menu bar."
  (let ((cb-gis-alist (sort (copy-alist (symbol-value 'gis-buffer-alist))
			    '(lambda (a b) (< (car a) (car b))))); 1, 2 etc.
	(cb-alist     (sort (copy-alist cb-buffer-alist); -1, -2, etc.
			    '(lambda (a b) (> (car a) (car b)))))
	cb-list)
    ;; Order is such that CB of *gis* will be first see gis.el for more details.
    (dolist (c cb-alist)
      (let ((i   (- (car c)))
	    (buf (cdr c)))
      (if buf
	  (setq cb-list
		(append cb-list
			(list (vector buf
				      (list 'switch-to-buffer buf)
				      ':active t
				      ':keys (format "M-- M-%d f3 f3" i))))))))
    (setq cb-list (append cb-list (list "---")))
    (dolist (c cb-gis-alist)
      (let ((i   (car c))
	    (buf (and (cdr c) (concat "*cb*" (cdr c)))))
      (if (and buf (get-buffer buf))
	  (setq cb-list
		(append cb-list
			(list (vector buf
				      (list 'switch-to-buffer buf)
				      ':active t
				      ':keys (format "M-%d f3 f3" i))))))))
    
    (easy-menu-change (list resources-menu-sw)
		      resources-menu-sw-cb-procs
		      (if (eq (length cb-list) 1)
			  (list resources-menu-sw-no-procs)
			cb-list))))

(defun cb-gis-get-mf-socketname (gis-process)
  "Returns from a GIS process its method_finder socketname interface."
  ;; The gis-filter will set cb--mf-socket-synchronised, which we trap here.
  (setq cb--mf-socket-synchronised nil)
  (let ((buffer (buffer-name (process-buffer gis-process)))
	(i 1)
	cb--mf-socket-synchronised)
    (process-send-string gis-process
			 "method_finder.send_socket_to_emacs()\n$\n")
    (while (and (null cb--mf-socket-synchronised) (not (zerop i)))
      (if (= i 100)
	  (progn
	    (message resources-cb-gis-get-mf-busy buffer)
	    (sleep-for 0.01)))
    
      (if (or (not (zerop (% i 1000)))
	      (not (y-or-n-p (format resources-cb-gis-get-mf-abort buffer))))
	  (progn
	    ;; either count i has not reached a multiple of 1000
	    ;;     or conunt i is a multiple of 1000 but user has chosen to continue
	    (sleep-for 0.01)
	    (setq i (1+ i)))
	;; User aborted loop.
	(setq i 0)))
    (if (and (stringp cb--mf-socket-synchronised) (not (equal cb--mf-socket-synchronised "")))
      cb--mf-socket-synchronised)))

(defun cb-start-process (buffer command &rest args)
  "Start a Class Browser process in BUFFER and return process object.
BUFFER may be nil, in which case only the process is started.
Adds SW_ACP_PATH environment variable to PATH."
  (let* ((acp-path (getenv "SW_ACP_PATH"))
	 (exec-path
	  (if acp-path
	      (append (parse-colon-path acp-path) exec-path)
	    exec-path))
	 cb-process)
    (setq cb-process (apply 'start-process "cb" buffer command args))
    (set-process-filter        cb-process 'cb-filter)
    (set-process-sentinel      cb-process 'cb-sentinel)
    (set-process-coding-system cb-process cb-coding-system cb-coding-system)
    (cb-send-tmp-file-name (cb-temp-file-name cb-process))
    cb-process))

(defun cb-get-process-create (buffer filter &optional gis cb-file)
  "Return a method finder process in BUFFER, creating one using GIS buffer or CB_FILE if needed.
Either starts a method_finder process or if a GIS session is running
it starts a mf_connector process to communicate with the method_finder
in the GIS.
If FILTER is given then it is set on the process.
It also detects the method_finder version and configures the following buffer local variables:
  `cb-quote-file-name'
  `cb-mf-extended-flags'
  `cb-filter-str'
  `cb-cursor-pos'
  `cb-n-methods-str'
  `cb-topic-pos'
  `cb-topics'
  `cb-process'
"
  (setq buffer (get-buffer-create buffer)) ; get a real buffer object.
  (if (get-buffer-process buffer)
      (get-buffer-process buffer) ;returns running process
    (let* ((process-environment (copy-list (save-excursion
					     (and gis (get-buffer gis) (set-buffer gis))
					     (or (symbol-value 'gis-process-environment)
						 process-environment))))
	   (exec-path (copy-list (save-excursion
				   (and gis (get-buffer gis) (set-buffer gis))
				   (or (symbol-value 'gis-exec-path) exec-path))))
	   (gis-proc (and gis (get-buffer-process gis)))
	   cb-process)
      
      (cond (gis-proc
	     ;; then ask Magik to start a method_finder.  Magik will
	     ;; tell us if it succeeds in starting a new method_finder.
	     (let ((socketname (cb-gis-get-mf-socketname gis-proc)))
	       (if socketname
		   (setq cb-process (cb-start-process buffer "mf_connector" "-e" socketname))
		 (if buffer
		     (with-current-buffer buffer
		       (let ((buffer-read-only nil))
			 (goto-char (point-max))
			 (insert "\n\n" resources-cb-gis-no-mf-file)
			 (ding) (ding) (ding)
			 (error "cannot start CB using mf_connector")))))))
	    (cb-file
	     ;; otherwise start our own method_finder.
	     (setq cb-process
		   (cb-start-process buffer
				     "method_finder"
				     "-e"
				     ;; we give a socket-name or pipe-name
				     ;; even though no-one is going to connect
				     ;; to the method_finder.  This is because
				     ;; the method_finder no longer has a single-user mode.
				     (if (running-under-nt)
					 (concat "\\\\.\\pipe\\method_finder\\time"
						 (number-to-string (first (current-time)))
						 "."
						 (number-to-string (second (current-time)))
						 "\\pointmax"
						 (number-to-string (point-max)))
				       (concat "/tmp/emacs_mf_sock" (number-to-string (emacs-pid))))))
	     (cb-send-load cb-file))
	    (ac-triggered
	     (message "auto-complete mode not using class browser"))
	    (t
	     (error "cannot start CB")))

      (if cb-process
	  (progn
	    (save-excursion
	      (let ((version (cb-method-finder-version)))
		(set-buffer (get-buffer-create buffer))
		(cb-mode)
		(setq cb-quote-file-name   (string< "5.2.0" version)
		      cb-mf-extended-flags (string< "6.0.0" version)
		      cb-filter-str ""
		      cb-cursor-pos 'method-name
		      cb-n-methods-str "0"
		      cb-topic-pos 1
		      cb-topics (mapcar '(lambda (x) (append x ())) cb-initial-topics)
		      cb-pending-message t
		      cb-filename cb-file
		      cb-process cb-process)))
	    ;; Note that cb-start-process uses cb-filter when the process starts.
	    ;; This is so that it can handle the topic information that the method finder
	    ;; process sends back. At the moment cb-ac-filter (the only other filter in use)
	    ;; does not include that code. A future rework may tidy this up.
	    (if filter
		(set-process-filter cb-process filter))))
      cb-process)))

(defun cb-interactive-buffer ()
  "Initialise an interactive Class Browser in current buffer"
      ;;Ensure interaction buffers are empty
      (cb-set-method-str "")
      (cb-set-class-str "")

      (let ((buffer-read-only nil))
	(erase-buffer)
	(insert resources-cb-header))
      (goto-char (point-min))
      (cb-redraw-modeline)
      (message resources-cb-loading)
      (cb-print-curr-methods)
      (message "")

      ;; Update cb-buffer-alist using negative numbers if loading from a file,
      ;; positive numbers are used by gis-buffer-alist for loading from GIS
      (if (and cb-filename
	       (not (rassoc (buffer-name) cb-buffer-alist)))
	  (let ((n -1))
	    (while (cdr (assq n cb-buffer-alist))
	      (setq n (1- n)))
	    (if (assq n cb-buffer-alist)
		(setcdr (assq n cb-buffer-alist) (buffer-name))
	      (add-to-list 'cb-buffer-alist (cons n (buffer-name))))
	    (assq n cb-buffer-alist))))

(defun cb-set-windows (&optional buffer)
  (setq buffer (or buffer (current-buffer)))
  (if (get-buffer-window buffer)
      (select-window (get-buffer-window buffer))
    (setq cb-was-one-window (one-window-p t)
	  cb-was-started-from-top-half (window-highest-p (selected-window)))
    (switch-to-buffer-other-window buffer)))

(defun-if-gnu-emacs window-highest-p (win)
  (zerop (second (window-edges win))))

(defun cb-set-filename ()
  "Read a filename off the user and return it."
  (let* ((gis (or (getenv "SMALLWORLD_GIS")
		  (error resources-cb-getenv-error "SMALLWORLD_GIS")))
	 (completion-ignored-extensions
		  (cons ".msf" (cons ".mi" completion-ignored-extensions)))
	 (ans
	  (expand-file-name
	   (substitute-in-file-name
	    (read-file-name (concat resources-cb-enter-load-file " ")
			    (concat (file-name-as-directory gis) "images/")
			    nil t)))))
    (if (file-directory-p ans)
        (error resources-cb-file-is-dir-error))
    (save-excursion
      (set-buffer (get-buffer-create " *mf header"))
      (erase-buffer)
      (insert-file-contents ans nil 0 4)
      (if (or (equal (buffer-string) "mfcb")
              (equal (buffer-string) "bcfm")
              (y-or-n-p (concat (format resources-cb-enter-yn-load-file ans) " ")))
          ()
        (error resources-cb-file-not-loaded ans)))
    ans))

;; T H E   C L A S S    B R O W S E R    F I L T E R
;; _________________________________________________

(defun cb-filter (p s)
  "Process data coming back from the C."
  (save-match-data
    (let* ((b (process-buffer p))
	   jump-str)
      (set-buffer b)
      (if cb-pending-message
	  (progn
	    (message "")
	    (setq cb-pending-message nil)))
      
      ;; diagnostic to see if stuff is coming back from the C.
      (if cb-debug
	  (let ((debug-buf (get-buffer-create (concat "*cb debug*" (buffer-name b)))))
	    (save-excursion
	      (set-buffer debug-buf)
	      (insert s)
	      (message "DEBUG output set to buffer %s" (buffer-name)))))

      (setq cb-filter-str (concat cb-filter-str s))

      (if (string-match "\C-e" cb-filter-str) (cb-read-methods p))
      (if (string-match "\C-u" cb-filter-str) (cb-force-query  p))
      (if (string-match "\C-c" cb-filter-str) (cb-read-classes p))

      (save-excursion
	(set-buffer b)
	(while (string-match "[\C-t\C-f].*\n" cb-filter-str)
	  (let ((str (substring cb-filter-str (1+ (match-beginning 0)) (1- (match-end 0)))))
	    (if (eq (aref cb-filter-str (match-beginning 0)) ?\C-t)
		(cb-new-topic str)
	      (setq jump-str str)))
	  (setq cb-filter-str (substring cb-filter-str (match-end 0))))
      
	(setq cb-filter-str
	      (if (string-match "[\C-t\C-f]" cb-filter-str)
		  (substring cb-filter-str (match-beginning 0))
		""))

	(if jump-str
	    (cb-goto-method jump-str (eq major-mode 'cb-mode)))))))

(defun cb-read-methods (p)
  "Deal with a C-e or a C-u char coming back from the C by loading
from \"/tmp\" into the main cb buffer.  Be careful to maintain the
position in the listing.  Also extract the number-of-methods from
the last line of the file, and put it in the global `cb-n-methods-str'.
"
  (let ((buf (process-buffer p))
	(buffer-read-only nil)
	(coding-system-for-read cb-coding-system)
	method-str)
    (or (looking-at "^[^ \n]") (re-search-backward "^[^ \n]" nil 1))

    (setq method-str (buffer-substring (point-bol) (point-eol)))
    (erase-buffer)
    (insert-file-contents (cb-temp-file-name p))
    (goto-char (point-max))
    (forward-line -1)
    (setq cb-n-methods-str (buffer-substring (point) (point-eol)))
    (delete-region (point) (point-max))
    (goto-char (cb-find-latest-<= method-str (point-min) (point-max)))
    (if (get-buffer-window buf)
        (set-window-point (get-buffer-window buf) (point)))
    (cb-redraw-modeline)))  ; for the method count.
 
(defun cb-force-query (p)
  "Override the current modeline. The class name pattern is cleared
and the method name pattern is set to match the method name in
cb-temp-method-name. Then a suitable query is sent to the method
finder process to return the list of methods.
None of the current topics or flags settings are overridden.
"
  (cb-set-class-str "")
  (cb-set-method-str (concat "^" cb-temp-method-name "$") )
  (cb-send-modeline-and-pr)
  (cb-set-windows (process-buffer p)))

(defun cb-read-classes (p)
  "Deal with a C-c character coming back from the C by displaying
the classes in \"*cb2*\".

We assume that whatever lisp requested this info has made sure the
buffer is being displayed in some window.  We just dump the data
in \"*cb2*\" and note that \"*cb2*\" is now in family mode.
"
  (set-buffer (cb2-buffer (process-buffer p)))
  (let ((buffer-read-only nil)
	(coding-system-for-read cb-coding-system))
    ;(erase-buffer)
    (insert-file-contents (cb-temp-file-name p) nil nil nil t)
    (if (search-forward "\C-l" nil t)
	(progn
	  (backward-delete-char 1)
	  (insert "\n\n\n")))
    (goto-char (point-min))
    (if (re-search-forward "^[^ ]" nil t)
	(backward-char))
    (setq cb2-mode 'family)

    (if (get-buffer-window (current-buffer))
	(set-window-point (get-buffer-window (current-buffer)) (point)))))

(defun cb-goto-method (jump-str other-window-p) ;; ??? %env% ??? unix filenames on NT etc.
  "Deal with a C-f character coming back from the C by 'finding' the
method described in the string, JUMP-STR.

JUMP-STR contains the filename, the methodname and the classname
separated by spaces."
  (let ((c (string-to-char jump-str)))
    (or (eq c ?/)
	(eq c ?$)
	(eq c ?\\)
	(eq c ?%)
	(save-match-data (string-match "^[a-zA-Z]:" jump-str))
	(error resources-cb-cannot-jump-error jump-str)))

  ;;Now extract filename class and method from string separated by spaces
  ;;Assuming neither method nor class contains spaces
  ;;analyse the string so that if the filename contains spaces it is retained
  ;;even if there are two concurrent spaces!
  (let* ((lis         (reverse (split-string jump-str " ")))
	 (class-name
	  (save-match-data
	    (let ((class (car lis)))
	      (if (string-match ":" class)
		(substring class (match-end 0))
		class))))
	 (method-name (cadr lis))
	 (filename    (cb-generalise-file-name
		       (mapconcat 'identity (reverse (cddr lis)) " ")))
	 search-str)
    (cond ((file-readable-p filename)
	   t)
	  ((string-match "[/\\]source[/\\]sys_core[/\\]" filename)
	   (error resources-cb-no-code-error class-name method-name))
	  (t
	   (error resources-cb-no-file-error filename)))

    (if (and (not cb-jump-replaces-cb-buffer) other-window-p)
        (find-file-other-window filename)
      (find-file filename))
    (goto-char (point-min))
    (magik-goto-class-method method-name class-name)))

(defun cb-new-topic (str)
  "Add the topic, STR, to cb-topics."
  (if (cb-topic-elt str)
      nil
    (let ((truncated-str str)
	  (cb2 (cb2-buffer))
	  (topics (cb-topics)))
      (if (equal truncated-str "database_collections_and_records")
          (setq truncated-str "db_colls_and_records"))
      (if (equal truncated-str "database_version_management")
          (setq truncated-str "db_version_mngmnt"))
      (if (string-match "^database" truncated-str)
          (setq truncated-str (concat "db" (substring truncated-str (length "database")))))
      (push (list truncated-str
                  t
                  (concat "add topic ^" truncated-str "$")
                  (concat "unadd topic ^" truncated-str "$"))
            topics)
      (cb-topics topics)
      (if (and (get-buffer-window cb2)
               (eq cb2-mode 'topic))
	  (let ((buffer-read-only nil))
            (set-buffer cb2)
            (erase-buffer)
            (cb-insert-topics-and-flags))))))

;;; S E N T I N E L
;; function to run when process changes state.

(defun cb-sentinel (proc msg)
  (let ((status (process-status proc))
	(buf (process-buffer proc)))
    (if (and (or (eq status 'exit) (eq status 'signal))
	     (buffer-live-p buf))
	(save-excursion
	  (set-buffer buf)
	  (setq cb-filename nil)
	  (cb-redraw-modeline)))))

;; E X I T I N G
;; _____________


;; In this new version of the cb, we exit in 2 stages regardless of
;; where the cursor happens to be.  The exit command will usually be
;; bound to SPACE.
;;
;; If "*cb2*" is showing in some window, we can just kill it because it
;; doesn't store any useful state.
;;
;; Otherwise deal with the main cb window.

(defun cb-quit ()
  "Temporarily leave the cb (in stages)."
  (interactive)
  (let ((cb2 (cb2-buffer)))
    (cond ((get-buffer-window cb2)
	   (if (eq cb2-mode 'topic)
	       (save-excursion
		 (set-buffer cb2)
		 (let ((pt (point)))
		   (set-buffer (cb-buffer))
		   (setq cb-topic-pos pt))))
	   (if (and cb2-was-one-window
		    (not (one-window-p t)))
	       (progn
		 (delete-window (get-buffer-window cb2))
		 (kill-buffer cb2))
	     (kill-buffer cb2))
	   (if (get-buffer-window (current-buffer))
	       (select-window (get-buffer-window (current-buffer)))))

	  ((get-buffer-window (current-buffer))
	   (cb-quit-main-buffer)))))

;; If "*cb*" is occupying the whole screen, then shrink back to a half
;; screen.  (The user can then press space again to exit).
;;
;; If "*cb*" is showing in some window, bury it.  If the cb was started
;; from an unsplit-screen configuration then do a "C-x 0" as well as a bury.
;;
;; There are 4 cases to consider, determined by whether there was just one
;; window when the cb was started and whether there is just one window showing
;; now.
;;
;; The aim is to return the user to original number of windows but without
;; hijacking control of emacs with a complete restoration of the window
;; configuration.  We ignore the case where the screen is split into 3 or 4
;; but expect it to work out ok anyway.  We also ignore the issue of the
;; cb buffer being displayed in more than one window.
;;
;; The user can quit the cb from any window (not just the cb window) using
;; the `F3 q' command.
;;
;; We rely on the natural ordering of buffers to restore the original buffers
;; once the cb has been buried.

(defun cb-quit-main-buffer ()
  (cond
   ;; first the 2 shrink back cases.
   ((and (one-window-p t) cb-was-started-from-top-half)
    (split-window-vertically)
    (set-window-buffer (selected-window) (other-buffer))
    (select-window (next-window (selected-window) 1)))
   ((one-window-p t)
    (display-buffer (other-buffer)))
   (cb-was-one-window
    (delete-window (get-buffer-window (current-buffer)))
    (bury-buffer (current-buffer)))
   (t
    ;; is 2 windows and was 2 windows.
    (set-buffer (current-buffer))
    (bury-buffer)
    (select-window (next-window (selected-window) 1)))))

;; T O P I C S   A N D   F L A G S
;; _______________________________
;;
;; Unlike earlier versions of the cb, the topics and flags are
;; now combined.  Also, where internal variables and functions
;; relate to both topics and flags we just use the word, topic,
;; in the name.


;; Put the user into "*cb2*" for toggling of topics and flags.
;;
;; The one slightly complicated case is when the cb isn't split
;; and the cb was started from the top half.  In this case we
;; want "*cb2*" to appear in the top half, so as to leave "*cb*"
;; in the bottom half.
;;
;; Another issue to do with state is that of restoring the user
;; to the cursor position they were in last time.

(defun cb-edit-topics-and-flags ()
  "Alter the current topics and flags by editing a list of them."
  (interactive)
  (let ((cb2 (cb2-buffer))
	(topic-pos (save-excursion (set-buffer (cb-buffer)) cb-topic-pos)))
    (set-buffer (get-buffer-create cb2))
    (cb2-mode)
    (if (cb2-get-window 'topic) ;YUCK relies on buffer not being displayed...
	(let ((buffer-read-only nil))
	  (setq cb2-mode 'topic)
	  (erase-buffer)
	  (cb-insert-topics-and-flags)
	  (goto-char topic-pos)))))

(defun cb-insert-topics-and-flags ()
  "Write the topics and flags and the current + signs into the current buffer."
  (let ((buffer-read-only nil))
    (insert resources-cb-topic-header)
    ;;WARNING:
    ;; each flag/topic must be surrounded by a single space.
    ;; therefore the flag/topics printed here at the end of the lines
    ;; have a white-space character
    (insert "
  + basic             * local-only                     show-methods 
    advanced          * inherit-not-\"object\"           show-classes 
    subclassable      * inherit-from-\"object\"          show-args 
    redefinable                                        show-comments 
  + debug                                              show-topics ")
    (if (cb-mf-extended-flags)
	(insert "
    deprecated 
    restricted 
    "))
  (insert (concat "\n" resources-cb-topic-overrides))
  (insert "
    override-flags          override-topics            override-200-limit \n")
  (insert (concat "\n" resources-cb-topic-toggles "\n\n"))
  (cb-insert-topics)
  (cb-display-all-topics)))

(defun cb-insert-topics ()
  "Format all the topics in columns.  Don't bother with the + signs."
  (let*
      ((max-len 0)
       n-cols
       ans
       (total-width (- (window-width (get-buffer-window (current-buffer))) 1))
       (n-rows 0)
       col-width
       col-length
       (curr-col 0)
       (curr-row 0)
       ;sort the topics alphabetically. sort has side-effects, so the alist, cb-topics, is copied first
       (cb-sorted-topics (sort (copy-alist (cb-topics)) '(lambda (x y) (string< (car x) (car y)))))
       (last-char (string-to-char (caar cb-sorted-topics))))

    ;; first pass for calculating the column widths etc.
    (save-excursion
      (set-buffer (generate-new-buffer "*cbtemp*"))
      (dolist
          (x cb-sorted-topics)
        (let*
            ((topic (car x))
             (this-char (string-to-char topic)))
          (if (cb-is-a-topic topic)
              (progn
                (incf n-rows)
                (or (eq last-char this-char) (incf n-rows))
                (setq last-char this-char)
                (setq max-len (max max-len (length topic)))))))

      (setq col-width (+ max-len 4))
      (setq n-cols (/ total-width col-width))
      (setq col-length (/ n-rows n-cols))

      ;; second pass to put the topic names in the buffer.
      (setq last-char (string-to-char (caar cb-sorted-topics)))
      (dolist
          (x cb-sorted-topics)
        (let*
            ((topic (car x))
             (this-char (string-to-char topic)))
          (if (cb-is-a-topic topic)
	      (let ((buffer-read-only nil))
                (if (not (eq last-char this-char))
                    (progn
                      (incf curr-row)
                      (if (eobp) (insert ?\n) (forward-line))))
                (if (> curr-row col-length)
                    (progn
                      (setq curr-row 0)
                      (goto-char (point-min))
                      (setq curr-col (+ curr-col col-width))))
                (setq last-char this-char)
                (end-of-line)
                (indent-to-column curr-col)
                (insert "  " topic " ")
                (incf curr-row)
                (if (eobp) (insert ?\n) (forward-line))))))
      (setq ans (buffer-string))
      (kill-buffer (current-buffer)))
    (let ((buffer-read-only nil))
      (insert ans))))

(defun cb-toggle-all-topics ()
  "Turn all the topics on or all the topics off."
  (interactive)
  (let
      ((all-on (cb-all-topics-on-p)))
    (cb-set-all-topics (not all-on))
    (cb-display-all-topics)
    (cb-send-string (if all-on "unadd topic\n" "add topic\n"))
    (cb-print-curr-methods)))

(defun cb-toggle-topic-or-flag ()
  "Toggle the topic or flag under the cursor."
  (interactive)
  (let*
      ((str (cb-curr-topic)))
    (if (cb-topic-elt str)
        (cb-toggle str))))

;; T O P I C   A N D   F L A G   U T I L S
;; _______________________________________
;;
;; cb-topic-elt (str)
;;
;; cb-is-a-topic (str)
;; cb-topic-on-p (str)
;; cb-all-topics-on-p ()         DOESN'T APPLY TO FLAGS.
;;
;; cb-set-topic (str new-val)
;; cb-send-topic (str)
;; cb-display-topic (str)     PROVIDED "*cb2*" EXISTS AND IS IN TOPIC MODE.
;;
;; cb-toggle (str)        SET, SEND and DISPLAY.
;;
;; cb-set-all-topics (new-val)   DOESN'T APPLY TO FLAGS.
;; cb-send-all-topics ()
;; cb-display-all-topics ()
;;
;; cb-curr-topic ()

(defun cb-topic-elt (str)
  "Return an element from the topic and flag list."
  (assoc str (cb-topics)))

(defun cb-is-a-topic (str)
  "Return t if str is a topic.  We can tell something is a topic
rather than a flag because it doesn't appear in cb-initial-topics."
  (not (assoc str cb-initial-topics)))

(defun cb-topic-on-p (str)
  "Return t if the topic or flag, STR, is set."
  (second (cb-topic-elt str)))

(defun cb-all-topics-on-p ()
  "Return t if all the topics are on."
  (let ((ans t))
    (dolist (x (cb-topics))
      (let ((str (first x)))
        (if (and (cb-is-a-topic str)
                 (not (cb-topic-on-p str)))
            (setq ans nil))))
    ans))

(defun cb-set-topic (str new-val)
  "Set the topic or flag, STR, to the NEW-VAL."
  (rplaca (cdr (cb-topic-elt str)) new-val))

(defun cb-send-topic (str)
  "Send the status of the topic or flag, STR, to the C.
Don't ask for a response, though."
  (cb-send-string (if (cb-topic-on-p str)
                      (third (cb-topic-elt str))
                    (fourth (cb-topic-elt str)))
                  "\n"))

(defun cb-display-topic (str)
  (let ((cb2 (cb2-buffer))) 
    (if (and (get-buffer cb2) (eq cb2-mode 'topic))
	(let ((on-p (cb-topic-on-p str))
	      (term-p (member str cb-thermometer-group))
	      buffer-read-only
	      case-fold-search)
	  (set-buffer cb2)
	  (goto-char (point-min))
	  (search-forward (concat " " str " "))
	  (backward-char (+ 2 (length str)))
	  (backward-delete-char 1)
	  (insert
	   (cond ((and term-p on-p)       "*")
		 ((and term-p (not on-p)) ".")
		 (on-p                    "+")
		 ((not on-p)              "-")
		 (t ;should never get here
		  "?")))))))
  
(defun cb-toggle (str)
  "Toggle the topic or flag, STR.  Set, send and display it.
Provided the \"*cb2*\" buffer exists and is in topic mode."
  (if (member str cb-thermometer-group)
      (cb-set-thermometer-flags str)
    (cb-set-topic str (not (cb-topic-on-p str)))
    (cb-make-sure-something-is-on str)
    (cb-send-topic str)
    (cb-display-topic str)
    (cb-print-curr-methods)))

(defun cb-set-thermometer-flags (str)
  "Deal with the set of flags that act like a thermometer.
This is a set of flags which are only on if the previous ones are on.

Specifically, make sure all topics in `cb-thermometer-group' up to
and including STR are on and all remaining ones are off.  Also send the
the STR to the method_finder."
  (let ((found-the-topic nil))
    (loop for topic in cb-thermometer-group do
          (if found-the-topic
              (cb-set-topic topic nil)
            (cb-set-topic topic t)
            (if (equal topic str)
                (setq found-the-topic t)))
          (cb-display-topic topic)))
  (cb-send-topic str)
  (cb-print-curr-methods))

(defun cb-next-inheritance-setting ()
  "Toggle the inheritance setting round the next setting.  The settings are:
    local-only       - only display methods that are defined on the current classes.
    inherit-not-\"obj\"   - display inherited methods too but not anything on object.
    inherit-from-\"obj\"  - display methods on object too."
  (interactive)
  (cond 
   ((cb-topic-on-p "inherit-from-\"object\"")
    (cb-set-thermometer-flags "local-only")
    (message resources-cb-inherit-local))
   ((cb-topic-on-p "inherit-not-\"object\"")
    (cb-set-thermometer-flags "inherit-from-\"object\"")
    (message resources-cb-inherit-object))
   (t
    (cb-set-thermometer-flags "inherit-not-\"object\"")
    (message resources-cb-inherit-not-object))))

(defun cb-make-sure-something-is-on (str)
  "Make sure that some flag from the group of flags containing STR is on.
If the one turned off is the first in the group, turn on the 2nd, else the 1st."
  (if (cb-topic-on-p str)
      nil
    (dolist
        (group cb-flag-groups)
      (if (member str group)
          (let
              ((something-is-set-p nil))
            (dolist (f group) (if (cb-topic-on-p f) (setq something-is-set-p t)))
            (if something-is-set-p
                ()
              (cb-toggle (if (equal str (first group)) (second group) (first group)))))))))

(defun cb-set-all-topics (new-val)
  "Set all the topics to NEW-VAL."
  (dolist (x (cb-topics))
    (let ((str (first x)))
      (if (cb-is-a-topic str)
          (cb-set-topic str new-val)))))

(defun cb-send-all-topics ()
  "Send the current values of all the topics and flags to the C."
  (dolist (x (cb-topics))
    (cb-send-topic (first x))))

;; (I don't think save-excursion works because of the cutting and pasting
;; of text).

(defun cb-display-all-topics ()
  "Put pluses or spaces in front of all topics and flags, and
be careful to preserve the position in \"*cb2*\"."
  (let ((orig-buf (current-buffer))
	(cb2 (cb2-buffer))
	orig-point)
    (if (and (get-buffer cb2) (eq cb2-mode 'topic))
	(progn
	  (set-buffer cb2)
	  (setq orig-point (point))
          (dolist (x (cb-topics))
            (cb-display-topic (first x)))
          (goto-char orig-point)
	  (set-buffer orig-buf)))))

(defun cb-curr-topic ()
  "Return the string under the cursor or after point."
  (save-excursion
    (let ((case-fold-search nil))
      (if (looking-at "[-a-zA-Z0-9?#&_\"]")
          (search-backward " " nil t))
      (if (re-search-forward "[-a-zA-Z0-9?#&_\"]+" nil t)
          (match-string 0)
        (re-search-backward "[ \r\n]\\([-a-zA-Z0-9?#&_\"]+\\)" nil t)
        (match-string 1)))))

(defun cb-set-mode-line-cursor (cursor)
  "Set properties on the `cb-mode-line-cursor'."
  (interactive "cCharacter to use for CB cursor: ")
  (setq cb-mode-line-cursor (if (stringp cursor) cursor (char-to-string cursor)))
  (add-text-properties 0
		       (length cb-mode-line-cursor)
		       (list 'face cb-cursor-face
			     'help-echo (purecopy resources-cb-modeline-help-cursor))
		       cb-mode-line-cursor))

;; C B 2
;; _____

(defun cb2-mode ()
  "Make sure \"*cb2*\" exists and is in cb-mode and has the right keymap and modeline."
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'cb2-mode)

  (setq major-mode 'cb2-mode
	buffer-read-only t
	buffer-undo-list t
	font-lock-defaults '(cb2-font-lock-keywords nil t ((?_ . "w"))))

  (use-local-map cb-mode-map)
  (set-syntax-table (copy-syntax-table magik-mode-syntax-table))
  (modify-syntax-entry ?\" "w")
  (modify-syntax-entry ?- "w")

  (cb-redraw-modeline)
  (run-hooks 'cb2-mode-hook))

(defun cb2-buffer (&optional buffer)
  "Name of the CB2 buffer."
  (get-buffer-create (concat "*cb2*" (cb-gis-buffer buffer))))

(defun cb2-get-window (mode)
  "Set up a window for \"*cb2*\" and return nil if \"*cb2*\" already had a
window and was in the right mode.  (If it didn't have a window, the
buffer is re-filled from the cb global variables).  We also save
some state for a clean exit."
  (let* ((cb2 (cb2-buffer)) ;actually always called from *cb2* buffer.
	 (buf (cb-buffer))
	 (win (get-buffer-window cb2)))
;    (if (get-buffer-window cb2)
;	(progn
;	  (select-window (get-buffer-window cb2))
;	  (not (eq cb2-mode mode)))
    ;; else the window doesn't exist.
    (setq cb2-was-one-window (one-window-p t))
    (setq cb2-direct-p (not (get-buffer-window buf)))
    (let ((cb-win (get-buffer-window buf)))
      (cond
       ((and (fboundp 'ecb-toggle-compile-window-height)
	     (boundp 'ecb-minor-mode) (symbol-value 'ecb-minor-mode))
	(sleep-for 0.1)
	(funcall 'ecb-toggle-compile-window-height 1))
       ((not cb-win)
        ;; I'm not sure what to do here!  For now we'll just do the
        ;; same sort of start up as for "*cb*", and just note that the user
        ;; got into "*cb2*" without going via "*cb*".
        (setq cb-was-one-window (one-window-p t))
        ;;;;; doesn't appear to be used ;;;;;
        ;;(setq cb2-was-started-from-top-half
        ;;      (zerop (second (window-edges (selected-window)))))
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (switch-to-buffer-other-window cb2))

       ((not (one-window-p t))
        ;; if there's 3 windows maybe we should try and zap the least wanted
        ;; window.  For now we just zap the next in rotation.
        (let
            ((cb2-win (next-window cb-win 1)))
          (set-window-buffer cb2-win cb2)
          (select-window cb2-win)))

       ;; Now the 2 cases when "*cb*" is occupying the whole screen.
       ;; The aim is to leave "*cb*" in the half of the screen that was
       ;; its original home before the user did a `C-x 1'.

       (cb-was-started-from-top-half
        ;; make "*cb2*" appear in the top half.
        (split-window-vertically)
        (switch-to-buffer cb2))

       (t
        (switch-to-buffer-other-window cb2))))
    t))
;)


;; M O D E L I N E
;; _______________
;;
;; Like in the previous version of the cb, we use invisible permanent
;; shadow buffers, " m*cb*" and " c*cb*" to store the state of the
;; modeline.
(defun cb-set-buffer-m ()
  (set-buffer (get-buffer-create (concat " m" (buffer-name (cb-buffer))))))

(defun cb-set-buffer-c ()
  (set-buffer (get-buffer-create (concat " c" (buffer-name (cb-buffer))))))

(defun cb-beginning-of-line ()
  "Do a beginning-of-line in the mode-line."
  (interactive)
  (save-excursion
    (set-buffer (cb-buffer))
    (if (or (eq cb-cursor-pos 'method-name)
	    (eq last-command 'cb-beginning-of-line))
	(progn
	  (setq cb-cursor-pos 'method-name)
	  (cb-set-buffer-m)
	  (beginning-of-line))
      (cb-set-buffer-c)
      (beginning-of-line))
    (message resources-cb-goto-beginning))
  (cb-redraw-modeline))

(defun cb-end-of-line ()
  "Do an end-of-line in the mode-line."
  (interactive)
  (save-excursion
    (set-buffer (cb-buffer))
    (if (or (eq cb-cursor-pos 'class-name)
	    (eq last-command 'cb-end-of-line))
	(progn
	  (setq cb-cursor-pos 'class-name)
	  (cb-set-buffer-c)
	  (end-of-line))
      (cb-set-buffer-m)
      (end-of-line)))
  (message resources-cb-goto-end)
  (cb-redraw-modeline))

(defun cb-forward-char ()
  "Do a forward-char in the mode-line."
  (interactive)
  (save-excursion
    (set-buffer (cb-buffer))
    (if (eq cb-cursor-pos 'method-name)
	(if (save-excursion (cb-set-buffer-m) (eobp))
	    (progn
	      (setq cb-cursor-pos 'class-name)
	      (cb-set-buffer-c)
	      (goto-char (point-min)))
	  (cb-set-buffer-m)
	  (forward-char))
      (cb-set-buffer-c)
      (forward-char)))
  (cb-redraw-modeline))

(defun cb-backward-char ()
  "Do a backward-char in the mode-line."
  (interactive)
  (save-excursion
    (set-buffer (cb-buffer))
    (if (eq cb-cursor-pos 'class-name)
	(if (save-excursion (cb-set-buffer-c) (bobp))
	    (progn
	      (setq cb-cursor-pos 'method-name)
	      (cb-set-buffer-m)
	      (goto-char (point-max)))
	  (cb-set-buffer-c)
	  (backward-char))
      (cb-set-buffer-m) (backward-char)))
  (cb-redraw-modeline))

(defun cb-insert-command (arg)
  "Do a self-insert-command into the mode-line and refresh the method display."
  (interactive "p")
  (save-excursion
    (set-buffer (cb-buffer))
    (save-excursion
      (if (eq cb-cursor-pos 'method-name)
	  (progn
	    (cb-set-buffer-m)
	    (self-insert-command arg))
	(cb-set-buffer-c)
	(self-insert-command arg)))
    (cb-send-modeline-and-pr)))

(defun cb-backward-delete-char (arg &optional killflag)
  "Do a delete-char in the mode-line and refresh the method display."
  (interactive "p\nP")
  (save-excursion
    (set-buffer (cb-buffer))
    (save-excursion
      (if (eq cb-cursor-pos 'method-name)
	  (progn
	    (cb-set-buffer-m)
	    (backward-delete-char arg killflag))
	(cb-set-buffer-c)
	(if (bobp)
	    (progn (cb-set-buffer-m)
		   (goto-char (point-max))
		   (backward-delete-char arg killflag))
	  (backward-delete-char arg killflag))))
    (cb-send-modeline-and-pr)))

(defun cb-delete-char (arg &optional killflag)
  "Do a delete-char in the mode-line and refresh the method display."
  (interactive "p\nP")
  (save-excursion
    (set-buffer (cb-buffer))
    (save-excursion
      (if (eq cb-cursor-pos 'class-name)
	  (progn
	    (cb-set-buffer-c)
	    (delete-char arg killflag))
	(cb-set-buffer-m)
	(if (eobp)
	    (progn
	      (cb-set-buffer-c)
	      (goto-char (point-min))
	      (delete-char arg killflag))
	  (delete-char arg killflag))))
    (cb-send-modeline-and-pr)))

(defun cb-kill-line (arg)
  "Do a kill-line in the mode-line and refresh the method display."
  (interactive "p")
  (save-excursion
    (set-buffer (cb-buffer))
    (save-excursion
      (if (eq cb-cursor-pos 'class-name) (cb-set-buffer-c) (cb-set-buffer-m))
      (kill-line arg))
    (cb-send-modeline-and-pr)))

(defun cb-yank (&optional arg)
  "Do a yank in the mode-line and refresh the method display."
  (interactive "P")
  (save-excursion
    (set-buffer (cb-buffer))
    (save-excursion
      (if (eq cb-cursor-pos 'class-name) (cb-set-buffer-c) (cb-set-buffer-m))
      (yank arg)
      (cb-delete-lines)
      (set-text-properties (point-min) (point-max) nil) ;remove text properties
      (setq this-command 'yank))
    (cb-send-modeline-and-pr)))

(defun cb-yank-pop (arg)
  "Do a yank in the mode-line and refresh the method display."
  (interactive "p")
  (save-excursion
    (set-buffer (cb-buffer))
    (save-excursion
      (if (eq cb-cursor-pos 'class-name) (cb-set-buffer-c) (cb-set-buffer-m))
      (yank-pop arg)
      (cb-delete-lines)
      (set-text-properties (point-min) (point-max) nil) ;remove text properties
      (setq this-command 'yank))
    (cb-send-modeline-and-pr)))

(defun cb-delete-lines ()
  "Delete all the lines in the current buffer except the first line.
Also delete the end-of-line character."
  (save-excursion
    (goto-char (point-min))
    (if (search-forward "\n" nil t)
        (progn
          (delete-region (1- (point)) (point-max))
          (message resources-cb-yank-one-line)))))

(defun cb-redraw-modeline ()
  "Copy the contents of the invisible \" m*cb*\" and \" c*cb*\" onto the
modelines of \"*cb*\" and \"*cb2*\" and put in a (') character."
  (save-excursion
    (set-buffer (cb-buffer))
    (set mode-line-format-sym
	 (concat
	  (make-string (max 0 (- 5 (length cb-n-methods-str))) ? )
	  cb-n-methods-str  "    "
	  (save-excursion (cb-set-buffer-m) (buffer-substring (point-min) (point)))
	  (if (eq cb-cursor-pos 'method-name) cb-mode-line-cursor "")
	  (save-excursion (cb-set-buffer-m) (buffer-substring (point) (point-max)))
	  cb-in-keyword
	  (save-excursion (cb-set-buffer-c) (buffer-substring (point-min) (point)))
	  (if (eq cb-cursor-pos 'method-name) "" cb-mode-line-cursor)
	  (save-excursion (cb-set-buffer-c) (buffer-substring (point) (point-max)))
	  "          "
	  (cb-modeline-flags)))
    (set-buffer-modified-p (buffer-modified-p))

    ;;update CB2 if buffer exists.
    (let ((cb2 (cb2-buffer))
	  (mode-line (symbol-value mode-line-format-sym)))
      (when (get-buffer cb2)
	(set-buffer cb2)
	(set mode-line-format-sym mode-line)
	(set-buffer-modified-p (buffer-modified-p))))))

(defun cb-modeline-flags ()
  "Return a string that looks something like this:

           *b  a  *s  r  d  <inh>  F  T  2 dp rs   GIS
"
  (let ((ans "")
	s)
    (loop for topic in '("basic" "advanced" "subclassable" "redefinable" "debug")
	  do (progn
	       (setq s (concat
			(if (cb-topic-on-p topic) "*" " ")
			(substring topic 0 1)
			" "))
	       (add-text-properties 0 (length s)
				    (list 'help-echo
					  (format "mouse-1, mouse-2: toggle %s flag" topic))
				    s)
	       (setq ans (concat ans s))))
    (setq s (cond ((cb-topic-on-p "inherit-from-\"object\"") " <inh> ")
		  ((cb-topic-on-p "inherit-not-\"object\"")  " <obj> ")
		  (t                                         " <loc> ")))
    (add-text-properties 0 (length s)
			 (list 'help-echo
			       (format "mouse-1, mouse-2: toggle %s flag" "inherit"))
			 s)
    (setq ans (concat ans s))
    (loop for topic in '("override-flags" "override-topics" "override-200-limit")
	  do (progn
	       (setq s (concat
			(if (cb-topic-on-p topic) "*" " ")
			(upcase (substring topic (length "override-") (1+ (length "override-"))))
			" "))
	       (add-text-properties 0 (length s)
				    (list 'help-echo
					  (format "mouse-1, mouse-2: toggle %s flag" topic))
				    s)
	       (setq ans (concat ans s))))
    (if cb-mf-extended-flags
          (loop for topic in '( "deprecated" "restricted" )
		do (progn
		     (setq s (concat
			      (if (cb-topic-on-p topic) "*" " ")
			      (substring topic 0 1)
			      (substring topic 2 3 )
			      " "))
		     (add-text-properties 0 (length s)
					  (list 'help-echo
						(format resources-cb-modeline-help-flag topic))
					  s)
		     (setq ans (concat ans s)))))

    
    (setq ans (concat ans "  "))
    (if cb-filename
	;;(buffer-name) will be main CB buffer since this is evaluated their
	;; for CB2 mode buffers.
	(setq ans (concat ans (substring (buffer-name) 4)))
      (setq s (cb-gis-buffer))
      (add-text-properties 0 (length s)
			   (list 'help-echo
				 (format resources-cb-modeline-help-buffer s))
			   s)
      (setq ans (concat ans s)))

    ans))

(defun cb-send-modeline-and-pr ()
  "Redraw the modeline, send its contents to the C and request new methods."
  (cb-redraw-modeline)
  (cb-send-string            ;??? is there some duplication of sending stuff???
   (concat "method_name "
           (save-excursion (cb-set-buffer-m) (buffer-string))
           "\nunadd class\nadd class "
           (save-excursion (cb-set-buffer-c) (buffer-string))
           "\n"))
  (cb-print-curr-methods))

;; F A M I L Y
;; ___________

(defun cb-family (class)
  "Draw an ancestry and hierarchy for CLASS."
  (interactive (sw-find-tag-tag "class-name: "))
  (if (and (stringp class) (not (equal class "")))
      (let ((cb2 (get-buffer-create (cb2-buffer))))
	(set-buffer cb2)
        (cb2-mode)
	(display-buffer cb2)
        (cb2-get-window 'family)
        (setq cb2-mode 'family
	      font-lock-defaults nil) ;remove colourisation from family mode.
        (cb-send-string "pr_family " class "\n"))))
;; M O U S E
;; _________

;; Deal with a middle click of the left mouse button in "*cb*" or "*cb2*".
;; By the time this is called, the current buffer and point will be where the user
;; clicked.

(defun cb-mouse (click)
  "Either toggle a class browser flag or show a class hierarchy."
  (interactive "e")
  (mouse-set-point click)
  (cond ((eq major-mode 'cb-mode)
	 (if (save-excursion (search-backward " " (point-bol) t))
	     (cb-family (sw-find-tag-default))
	   (cb-jump-to-source)))
	((eq cb2-mode 'topic)
	 (cb-toggle-topic-or-flag))
	((eq cb2-mode 'family)
	 (cb-family (sw-find-tag-default)))))

(defun cb-mode-line-click (event)
  "Move the cb modeline 'cursor'."
  (interactive "@e")
  (let*
      ((b (event-buffer event))
       (p (get-buffer-process b))
       (x (event-x event))
       (effective-len-cb-n-methods-str 1)
       (cursor-pos (save-excursion (set-buffer b) cb-cursor-pos))
       (offset1 (- x (length "    ") effective-len-cb-n-methods-str (length "    ")))
       (len1 (save-excursion (cb-set-buffer-m) (1- (point-max))))
       (len2 (save-excursion (cb-set-buffer-c) (1- (point-max))))
       (offset2 (- offset1 (+ len1 (length cb-in-keyword)))))

    (cond

     ((and (>= offset1 -1) (<= offset1 (+ 2 len1)))
      (cb-set-buffer-m)
      (if (and (eq cursor-pos 'method-name)
               (<= (point) offset1))
          ;; then goto one more than the offset because emacs counts buffer positions
          ;; from 1 rather than 0 and then take one off because of the (') cursor.
          (goto-char offset1)
        ;; else goto one more than the offset because emacs ...etc.
        (goto-char (1+ offset1)))
      (set-buffer b)
      (setq cb-cursor-pos 'method-name))
     ((and (>= offset2 -1) (<= offset2 (+ 2 len2)))
      (cb-set-buffer-c)
      (if (or (eq cursor-pos 'method-name)
              (<= (point) offset2))
          (goto-char offset2)
        (goto-char (1+ offset2)))
      (set-buffer b)
      (setq cb-cursor-pos 'class-name))
     ((and (>= x (+ 25 len1 len2))
           (<  x (+ 25 15 len1 len2)))
      (let
          ((flag (nth (/ (- x (+ 25 len1 len2)) 3)
                      '("basic" "advanced" "subclassable" "redefinable" "debug"))))
        (cb-toggle flag)
        (message (if (cb-topic-on-p flag) resources-cb-toggle-flag-on resources-cb-toggle-flag-off)
		 flag)))

     ((and (>= x (+ 25 15 len1 len2))
           (<  x (+ 25 22 len1 len2)))
      (cb-next-inheritance-setting))

     ((and (>= x (+ 25 22 len1 len2))
           (<  x (+ 25 22 9 len1 len2)))
      (let
        ((flag (nth (/ (- x (+ 25 22 len1 len2)) 3)
                      '("override-flags" "override-topics" "override-200-limit"))))
        (cb-toggle flag)
        (message (if (cb-topic-on-p flag) resources-cb-toggle-flag-on resources-cb-toggle-flag-off)
		 flag)))

     ((and (>= x (+ 25 22 9 len1 len2))
           (<  x (+ 25 22 9 8 len1 len2)))
        (if (cb-mf-extended-flags)
            (let
              ((flag (nth (/ (- x (+ 25 22 9 len1 len2)) 4)
                          '("deprecated" "restricted"))))
            (cb-toggle flag)
            (message (if (cb-topic-on-p flag) resources-cb-toggle-flag-on resources-cb-toggle-flag-off)
		     flag))))
     ((and (>= x (+ 25 22 9 8 3 len1 len2))
	   (buffer-live-p (get-buffer (cb-gis-buffer)))
	   (get-buffer-process (get-buffer (cb-gis-buffer))))
      (switch-to-buffer-other-window (cb-gis-buffer)))))
  (cb-redraw-modeline))

;; U S E R   I N T E R F A C E
;; ___________________________

(defun cb-and-clear ()
  "Start or resume the CB.  And clear out the method and class strings."
  (interactive)
  (cb nil "" ""))

(defun cb-paste-method ()
  "Set the CB method name to the word under the cursor, and enter the CB."
  (interactive)
  (cb nil (concat "^" (cb-curr-method-name) "$") nil))

(defun cb-paste-class ()
  "Set the CB class name to the word under the cursor, and enter the CB."
  (interactive)
  (let ((class (sw-find-tag-default)))
    (if (null class)
	(error resources-cb-no-current-class-error))

    (save-match-data 
      (if (string-match ":" class)
	  (setq class (replace-match ":^" nil t class))
	(setq class (concat "^" class))))
    (setq class (concat class "$"))

    (cb nil nil class)))

(defun cb-tab ()
  "Move backwards and forwards between the method name and the class name."
  (interactive)
  (save-excursion
    (set-buffer (cb-buffer))
    (setq cb-cursor-pos
	  (if (eq cb-cursor-pos 'method-name) 'class-name 'method-name))
    (cb-redraw-modeline)))

(defun cb-clear ()
  "Clear the cb method-name or class-name."
  (interactive)
  (save-excursion
    (set-buffer (cb-buffer))
    (if (eq cb-cursor-pos 'method-name) (cb-set-buffer-m) (cb-set-buffer-c))
    (let ((buffer-read-only nil))
      (erase-buffer)))
  (cb-send-modeline-and-pr))

(defun cb-unfold ()
  "Add more detail to the listing, by `unfolding' it."
  (interactive)
  (if (not (cb-topic-on-p "show-methods")) (cb-toggle "show-methods"))
  (cond
   ((not (cb-topic-on-p "show-classes"))  (cb-toggle "show-classes"))
   ((not (cb-topic-on-p "show-args"))     (cb-toggle "show-args"))
   ((not (cb-topic-on-p "show-comments")) (cb-toggle "show-comments"))
   ((not (cb-topic-on-p "show-topics"))   (cb-toggle "show-topics")
    (message resources-cb-fully-unfolded))
   (t
    (error resources-cb-already-unfolded-error))))

(defun cb-fold ()
  "Remove detail from the listing."
  (interactive)
  (if (not (cb-topic-on-p "show-methods")) (cb-toggle "show-methods"))
  (cond
   ((cb-topic-on-p "show-topics")   (cb-toggle "show-topics"))
   ((cb-topic-on-p "show-comments") (cb-toggle "show-comments"))
   ((cb-topic-on-p "show-args")     (cb-toggle "show-args"))
   ((cb-topic-on-p "show-classes")  (cb-toggle "show-classes")
    (message resources-cb-fully-folded))
   (t
    (error resources-cb-already-folded-error))))

(defun cb-reset ()
  "Reset the topics, flags and method and class strings."
  (interactive)
  (save-excursion
    (let ((buffer-read-only nil))
      (cb-set-buffer-m)
      (erase-buffer)
      (cb-set-buffer-c)
      (erase-buffer)))
  (dolist (x (cb-topics))
    (let ((str (first x)))
      (rplaca (cdr x)
              (or (cb-is-a-topic str)
                  (second (assoc str cb-initial-topics))))))
  (cb-send-all-topics)
  (cb-display-all-topics)
  (cb-send-modeline-and-pr))

(defun cb-toggle-override-flags ()
  "Toggle the `override-flags' setting."
  (interactive)
  (cb-toggle "override-flags")
  (cb-redraw-modeline))

(defun cb-toggle-override-topics ()
  "Toggle the `override-topics' setting."
  (interactive)
  (cb-toggle "override-topics")
  (cb-redraw-modeline))

(defun cb-toggle-override-200-limit ()
  "Toggle the cut off at 200 methods."
  (interactive)
  (cb-toggle "override-200-limit")
  (cb-redraw-modeline))

;; J U M P   T O   S O U R C E
;; ___________________________

(defun cb-magik-ediff-methods (cb)
  "Find current method in CB session and compare with the version from the CB session."
  (interactive
   (let ((bufs (sw-buffer-mode-list 'cb-mode))
	 buffer)
     (setq buffer
	   (cond ((null bufs)
		  (error resources-cb-none-running-error))
		 ((= (length bufs) 1)
		  (car bufs))
		 (t
		  (completing-read
		   (concat resources-cb-enter-buffer " ")
		   bufs nil t))))
     (if (equal buffer "")
	 nil
       (list buffer))))
  (let* ((method-exemplar-block (magik-current-method-name))
	 (method  (elt method-exemplar-block 0))
	 (class   (elt method-exemplar-block 1))
	 (package (elt method-exemplar-block 2))
	 (cb-jump-replaces-cb-buffer t) ; # Put the source file in the right window.
	 (buf-A (current-buffer))
	 (pt-A (point))
	 (current-wc (current-window-configuration))
	 buf-B pt-B)

    (set-buffer cb)
    (cb-send-string (format "pr_source_file %s %s:%s\n" method package class))
    (sit-for 0.1)

     ;; Hopefully this should be the file from the CB filter
    (setq buf-B (window-buffer)
	  pt-B  (point))

    (if (not (eq buf-A buf-B))
	(magik-ediff-methods buf-A buf-B)

      ;; Otherwise ensure user's buffer isn't shown in two windows
      (set-window-configuration current-wc)
      (error resources-magik-goto-no-m-in-c-error method (concat package ":" class)))))

(defun cb-jump-to-source-from-cb ()
  "Jump to source for the method under the cursor in a CB buffer."
  (let ((regexp (concat "^\\(\\S-+\\)" cb-in-keyword "\\(\\S-+\\)"))
	(buffer (current-buffer)))
    (or (cb-is-running buffer)
	(error resources-cb-not-running-error buffer))
    (save-excursion
      (while (and (progn
		    (beginning-of-line)
		    (not
		     (looking-at regexp)))
		  (zerop (forward-line -1))))
      (if (looking-at regexp)
	  (cb-send-string (concat "pr_source_file " (match-string 1) " " (match-string 2) "\n"))
	(error resources-cb-no-in-line-error)))))

(defun cb-jump-to-source ()
  "Jump to the source for the method under the cursor."
  (interactive)
  (if (eq major-mode 'cb-mode)
      (cb-jump-to-source-from-cb)
    (setq cb-temp-method-name (cb-curr-method-name))
    (cb nil cb-temp-method-name "")))

;; H E L P
;; _______
(defun cb-help ()
  "Display help on how to use the Class Browser interface."
  (interactive)
  (sw-help-open sw-help-cb-id))

;; U T I L S
;; _________

(defun cb-execute-method-finder ()
  "Run method finder executable directly
Primary use is for debugging the method finder.
Do not use unless you understand the method finder direct user interface"
  (interactive)
  (let* ((acp-path (getenv "SW_ACP_PATH"))
	 (exec-path
	  (if acp-path
	      (append (parse-colon-path acp-path) exec-path)
	    exec-path)))
    (comint-run "method_finder")))

(defun cb-is-running (&optional buffer process)
  "Return t is CB process is running."
  (setq buffer  (or buffer (current-buffer))
	process (or process (get-buffer-process buffer)))
  (if process
      (eq (process-status process) 'run)))

(defun cb-set-method-str (str)
  "Set Method string to STR.
If STR is nil, this is a no-op."
  (if str
    (save-excursion
      (cb-set-buffer-m)
      (let ((buffer-read-only nil))
	(erase-buffer)
	(insert str))
      t)))

(defun cb-set-class-str (str)
  "Set Class string to STR.
If STR is nil, this is a no-op."
  (if str
    (save-excursion
      (cb-set-buffer-c)
      (let ((buffer-read-only nil))
	(erase-buffer)
	(insert str))
      t)))

(defun cb-set-method-and-class (method class)
  "Set METHOD and CLASS, return t if either were updated."
  (let (updatep)
    (if (cb-set-method-str method)
	(setq updatep t))
    (if (cb-set-class-str  class)
	(setq updatep t))
    updatep))

(defun cb-print-curr-methods ()
  "This is the ONLY proc that should say \"print_curr_methods\" to the C.
This is separated out in case we want to do some event
compression or lazy re-draw or something."
  (cb-send-string "print_curr_methods\n"))

;; The following commands involve filenames being passed to the C.
;; Method_finder versions >= 5.3.0 can now accept quotes to enable paths that contain spaces
;; to be sent. 

(defun cb-send-tmp-file-name (file)
  "Send tmp_file_name command to the method finder"
  (setq file (if cb-quote-file-name
		 (concat "'" file "'")
	       file))
  (cb-send-string "tmp_file_name " file "\n"))

(defun cb-send-load (file)
  "Send load command to the method finder"
  (setq file (if cb-quote-file-name
		 (concat "'" file "'")
	       file))
  (cb-send-string "load " file "\n"))

;; Send all the STRINGS to the C.  All calls to process-send-string should go
;; through here, so that we can do diagnostics like this:
;;
;; (defun cb-send-string (&rest strings)
;;   (process-send-string cb-process (apply 'concat strings))
;;   (save-excursion
;;     (set-buffer (get-buffer-create "cb_diag"))
;;     (goto-char (point-max))
;;     (apply 'insert strings)))

;; we put a delay in here for hps because they seem to
;; lose data if you send it too fast.  Not any more because
;; the HP problem is fixed at 2.1.
;;    (if (equal (getenv "HOST_OS") "HP-UX")
;;        (sleep-for 0.1))

(defun cb-send-string (&rest strings)
  (process-send-string (cb-process) (apply 'concat strings)))

(defun cb-find-latest-<= (target-str beg end)
  "Return the position of the start of the latest line (that has no indent) in
the range BEG to END (inclusive) that is <= to TARGET-STR."
  (if (= beg end)
      beg
    (let
        ((mid (/ (+ beg end 1) 2)))
      (goto-char mid)
      (if (cb-earlier-p target-str)
          (cb-find-latest-<= target-str beg (1- mid))
        (cb-find-latest-<= target-str mid end)))))

(defun cb-earlier-p (target-str)
  "Return t if the start of TARGET-STR is earlier than the current point."
  (or (not (re-search-forward "^[^ \n]" nil t))
      (cb-method-str< target-str (buffer-substring (point-bol) (point-eol)))))

(defun cb-method-str< (a b)
  "Return t if method A is earlier in the alphabet than method B.  Cut out trailing
comments etc."
  (let ((in-re (concat "\\([^ ]*" cb-in-keyword "[^ ]*\\)")))
    (if (and (string-match cb-in-keyword a)
	     (string-match cb-in-keyword b))
	(progn
	  (string-match in-re a)
	  (setq a (substring a 0 (match-end 1)))
	  (string-match in-re b)
	  (setq b (substring b 0 (match-end 1))))
      (string-match "\\([^ ]*\\)" a)
      (setq a (substring a 0 (match-end 1)))
      (string-match "\\([^ ]*\\)" b)
      (setq b (substring b 0 (match-end 1))))
    (string< a b)))

(defun cb-curr-method-name ()
  "Return the method-name under point including brackets and chevrons."
  (save-excursion
    (while (looking-at "\\sw\\|\\s_")
      (forward-char 1))
    (if (re-search-backward "\\sw\\|\\s_" nil t)
        (let*
            ((end (progn (forward-char 1) (point)))
             (beg (progn (skip-chars-backward "a-zA-Z0-9_!?") (point)))
             (name (buffer-substring-no-properties beg end)))
          (goto-char end)
          (skip-chars-forward " \t")
	  (concat name (magik-method-name-postfix)))
      (error resources-cb-no-current-word-error))))

(defun cb-method-str ()
  (save-excursion (cb-set-buffer-m) (buffer-string)))

(defun cb-class-str ()
  (save-excursion (cb-set-buffer-c) (buffer-string)))

(defun cb-method-finder-version ()
  "Return as a string (e.g. \"2.0.0\") the version of the method_finder.
Assumes method_finder is in SW_ACP_PATH."
  (let* ((acp-path (getenv "SW_ACP_PATH"))
	 (exec-path
	  (if acp-path
	      (append (parse-colon-path acp-path) exec-path)
	    exec-path))
	 cb-process)
    (save-excursion
      (set-buffer (get-buffer-create " *method finder version*"))
      (erase-buffer)
      (call-process "method_finder" nil t nil "-v")
      (goto-char (point-min))
      (prog1
          (if (re-search-forward "[0-9.]+" nil t)
              (buffer-substring (match-beginning 0) (match-end 0))
            resources-cb-mf-version-error)
        (kill-buffer (current-buffer))))))

(defun cb-temp-file-name (p)
  "The file-name of the file that the method_finder uses
for passing data back to the class browser."
  (let ((file (concat "mfm" (number-to-string (process-id p)))))
    (if (running-under-nt)
	(concat (getenv "TEMP") "\\" file)
      (concat "/tmp/" file))))

(defun cb-generalise-file-name (f)
  "Translate F into a filename appropriate for Unix or Windows-NT:
Turn slash characters around.
Expand either $foo or %foo% variables
Introduce or remove drive names.

See the variable `cb-generalise-file-name-alist' to provide more customisation."
  (save-match-data
    (setq f (substitute-in-file-name f))
    (if cb-generalise-file-name-alist
	(progn
	  (subst-char-in-string ?\\ ?/ f t)
	  (loop for i in cb-generalise-file-name-alist
		if (and (string-match (car i) f)
			(setq f (replace-match (cdr i) nil t f)))
		return f)))
    (if (running-under-nt)
	(progn
	  (subst-char-in-string ?/ ?\\ f t)
	  (if (or (string-match "^[a-zA-Z]:" f)
		  (string-match "^\\\\\\\\" f))
	      f
	    (let* ((buffer (cb-gis-buffer))
		   (drive-name (if (get-buffer buffer)
				   (save-excursion
				     (set-buffer buffer)
				     (substring default-directory 0 2))
				 (substring default-directory 0 2))))
	      (concat drive-name f))))
      (if (string-match "^[a-zA-Z]:" f)
	  (setq f (substring f 2)))
      (subst-char-in-string ?\\ ?/ f t))))

;; A U T O - C O M P L E T E
;; _________________________

(defun cb-ac-filter (p s)
  "Process data coming back from the CB auto-complete buffer."
  (with-current-buffer (process-buffer p)
    (unwind-protect
	(let ((buffer (current-buffer))
	      (buffer-read-only nil)
	      (coding-system-for-read cb-coding-system)
	      fn)
	  (setq cb-filter-str (concat cb-filter-str s))
	  (save-match-data
	    (setq fn (cond ((string-match "\C-e" cb-filter-str)
			    'cb-ac-candidate-methods)
			   ((string-match "\C-c" cb-filter-str)
			    'cb-ac-candidate-classes)
			   (t
			    nil))))
	  (setq cb-filter-str ""
		cb--ac-candidates (if fn
				      (progn
					(insert-file-contents (cb-temp-file-name p) nil nil nil t)
					(funcall fn)))))
	(setq cb-filter-str ""
	    cb--ac-candidates (if (eq cb--ac-candidates 'unset) nil cb--ac-candidates)))))

(defun cb-ac-start-process ()
  "Start a Class Browser process for auto-complete-mode.
Stores process object in `cb-ac-process'."

  ; TODO get-gis-buffer
  (setq cb-ac-process (cb-get-process-create "*cb-ac*" 'cb-ac-filter "*gis*" nil)))

(defun cb-ac-candidate-methods ()
  "Return candidate methods matching `ac-prefix' from Method finder output."
;;TODO combine method definition with its signature.
  (let ((method (car ac-prefix))
	(class (cdr ac-prefix))
	(ac-limit ac-limit))
    (setq method
	  (if (zerop (length method))
	      "\\sw"
	    (regexp-quote method)))
      (let ((i 0)
	    (regexp (concat "^\\(" method "\\S-*\\)" cb-in-keyword "\\(\\S-+\\)\\s-+\\(.*\\)\n\\(.*\n\\)\n\\(\\( +##.*\n\\)*\\)")) ; capture item and comments
	    candidate
	    classify
	    args
	    documentation
	    candidates)
	(goto-char (point-min))
	(save-match-data
	  (while (and (or (null ac-limit) (< i ac-limit))
		      (re-search-forward regexp nil t))
	    (setq candidate (match-string-no-properties 1)
		  class     (match-string-no-properties 2)
		  classify  (match-string-no-properties 3)
		  args      (cb-method-args (match-beginning 4))
		  documentation (match-string-no-properties 5))
	    (put-text-property 0 (length candidate)
			       'document
			       (cb-method-docstring class candidate args classify documentation)
			       candidate)
	    (if (member candidate candidates)
		nil ; already present
	      (setq candidates (append (list candidate) candidates)
		    i (1+ i)))))
	(nreverse candidates))))

(defun cb-method-args (pt)
  "Return method arguments from Class Browser at point."
  (save-excursion
    (goto-char pt)
    (save-match-data
      (let ((case-fold-search nil)
	    optional
	    args
	    gather
	    opt
	    name)
	(if (looking-at "$")
	    nil ; No arguments
	  (forward-char 1) ; space
	  (while (not (looking-at "$"))
	    (setq pt (point))
	    (cond ((looking-at "\\(OPT \\)?GATH \\(.*\\)")
		   (setq gather (buffer-substring-no-properties (match-beginning 2) (match-end 2)))
		   (goto-char (match-end 0)))
		  ((looking-at "OPT ")
		   (setq opt t)
		   (goto-char (match-end 0)))
		  ((> (skip-syntax-forward "w_") 0) ; found argument (may contain _ which may be classed as symbols.)
		   (setq name (list (buffer-substring-no-properties pt (point))))
		   (if opt
		       (setq optional (append optional name))
		     (setq args (append args name)))
		   (if (eq (following-char) ? )
		       (forward-char 1)))
		  (t ;catch all error
		   (message "Found unrecognised charcter at %d in %s" (point) (current-buffer))
		   (goto-char (end-of-line))))))
	(list args optional gather)))))

;;TODO extract out Magik method signature from callsification and documentation processing.
(defun cb-method-docstring (class candidate args classify documentation)
  "Return method documentation string."
  (let* ((required (elt args 0))
	 (optional (elt args 1))
	 (gather (elt args 2))
	 (method-signature (magik-method-name-type candidate))
	 (method (car method-signature))
	 (signature (cdr method-signature))
	 (signature-p (> (length signature) 0))
	 (method-p (and signature-p (equal (substring signature 0 1) "(")))
	 assignment
	 string)

    ;;Standardise classification string
    (cond ((zerop (length classify))
	   ;;do nothing
	   nil)
	  ((equal (substring classify 0 1) "A")
	   (setq classify (concat "Advanced" (substring classify 1))))
	  ((equal (substring classify 0 1) "B")
	   (setq classify (concat "Basic" (substring classify 1))))
	  (t
	   ;;do nothing
	   nil))
		
    ;; Handle << assignment like signatures - take first required argument
    (if (and signature-p (equal (substring signature -1) "<"))
	(setq assignment (car required)
	      required (cdr required)))
    (if documentation
	(while (string-match "^ +## " documentation)
	  (setq documentation (replace-match "" nil nil documentation))))
    (if gather
	;; prefix rest args with _gather and convert to a list.
	(setq gather (list (concat "_gather " gather))))
    (if optional
	;; prefix first optional arg with _optional
	(setcar optional (concat "_optional " (car optional))))
  ;; TODO handle arrays [], []<< etc.
    (concat 
     (cond ((equal class "<condition>")
	    (concat "raise(:" method (if required ",\n      ")
		    (mapconcat (lambda (r) (concat ":" r ", <value>")) required ",\n      ")
		    ")\n"
		    "  " classify
		    "\n"))
	   ((equal class "<global>")
	    ;; Globals are either procedures with arguments or dynamics.
	    (let* ((args-string (mapconcat 'identity (append required optional gather) ", "))
		   (argsp (not (equal args-string ""))))
	      (concat method
		      (if argsp "(")
		      args-string
		      (if argsp ")")
		      "\n"
		      "  " classify
		      "\n")))
	   ((not signature-p)
	    (concat method
		    "\n"
		    "  " classify
		    "\n"))
	   ((equal (substring signature 0 1) "(")
	    (let ((args-string (mapconcat 'identity (append required optional gather) ", ")))
	      (concat method "("
		      args-string
		      (substring signature 1) ;; appends rest of signature. ), )<< and )^<<
		      assignment ;; allows for ()<< and ()^<< too.
		      "\n"
		      "  " classify
		      "\n")))
	   (assignment ;; handle << and ^<<
	    (concat method
		    signature
		    assignment
		    "\n"
		    "  " classify
		    "\n"))
	   (t "UNKNOWN??\n\n"))
     documentation)))

(defun cb-ac-candidate-classes ()
  "Return candidate classes from Method finder output."
  ;TODO handle package definitions?
  (let ((i 0)
	(regexp (concat "\\(\\S-+:\\)\\(\\S-+\\)")) ; capture class name and its package
	candidate
	package
	candidates)
    (goto-char (point-min))
    (save-match-data
      (while (re-search-forward regexp nil t)
	(setq candidate (match-string-no-properties 2)
	      package (match-string-no-properties 1))
	(save-match-data
	  (if (member candidate candidates)
	      nil ; already present
	    (setq candidates (append (list candidate) candidates)
		  i (1+ i)))))
      (nreverse candidates))))

(defun cb-ac-method-candidates ()
  "Return list of methods for a class matching AC-PREFIX for auto-complete mode.
AC-PREFIX is of the form \"CLASS\".\"METHOD_NAME_PREFIX\"
"
  (let ((cb--ac-candidates 'unset) ; use 'unset symbol since nil is also a valid return value.
	(ac-prefix ac-prefix)
	(ac-limit (or ac-limit 1000000))
	class method character)
    (save-match-data
      (cond ((null cb-ac-process)
	     (setq cb--ac-candidates nil))
	    ((not (string-match "\\(\\S-+\\)\\.\\(.*\\)" ac-prefix))
	     (setq cb--ac-candidates nil))
	    (t
	     (setq  class (match-string-no-properties 1 ac-prefix)
		    method (match-string-no-properties 2 ac-prefix)
		    character (if (equal method "") method (substring method 0 1))
		    ac-prefix (cons method class))
	     (process-send-string cb-ac-process
				  (concat "method_name ^" character "\n"
					  "unadd class \nadd class " class "\n"
					  "method_cut_off " (number-to-string ac-limit) "\n"
					  "override_flags\nshow_classes\nshow_args\nshow_comments\nprint_curr_methods\n"))
	     (while (and (eq cb--ac-candidates 'unset)
			 (cb-is-running nil cb-ac-process))
	       (sleep-for 0.1))
	     (setq cb--ac-candidates (append (list (concat " " class "." character)) cb--ac-candidates)))))
    cb--ac-candidates))

(defun cb-ac-class-candidates ()
  "Return list of classes matching AC-PREFIX for auto-complete mode."
  (let ((cb--ac-candidates 'unset)) ; use 'unset symbol since nil is also a valid return value.
    (cond ((null cb-ac-process)
	   (setq cb--ac-candidates nil))
	  (t
	   (process-send-string cb-ac-process
				(concat "dont_override_flags\npr_family " ac-prefix "\n"))
	   (while (and (eq cb--ac-candidates 'unset)
		       (cb-is-running nil cb-ac-process))
	     (sleep-for 0.1))))
    cb--ac-candidates))

;;Package configuration
(cb-set-mode-line-cursor cb-mode-line-cursor)

(provide 'cb)

;;MSB configuration
(defun cb-msb-configuration ()
  "Adds CB buffers to msb menu, supposes that msb is already loaded."
  (let* ((l (length msb-menu-cond))
	 (last (nth (1- l) msb-menu-cond))
	 (precdr (nthcdr (- l 2) msb-menu-cond)) ; cdr of this is last
	 (handle (1- (nth 1 last))))
    (setcdr precdr (list
		    (list
		     '(eq major-mode 'cb-mode)
		     handle
		     "CB (%d)")
		    last))))

(eval-after-load 'msb
  '(cb-msb-configuration))

(eval-after-load 'ecb
  '(progn
     (add-to-list 'ecb-compilation-major-modes 'cb-mode)
     (add-to-list 'ecb-compilation-major-modes 'cb2-mode)))

;;; cb.el ends here
