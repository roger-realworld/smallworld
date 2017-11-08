;;; module.el -- mode for editing Magik module.def files.

(eval-when-compile (require 'cl)
		   (require 'easymenu)
		   (require 'font-lock)
		   (require 'misc-sw)
		   (require 'sw-help)
		   (require 'utils-sw)
		   (require 'magik)
		   (require 'gis))

(defgroup sw-module nil
  "Customise Magik sw-module.def files group."
  :group 'smallworld
  :group 'tools)

(defconst sw-module-version "$Revision: 1.4 $")

(defcustom sw-module-mode-hook nil
  "*Hook to run after Sw-Module Mode is set."
  :group 'sw-module
  :type  'hook)

(defcustom sw-module-option-save-magikc t
  "*If t, save .magikc files when loading sw-module."
  :group 'smallworld
  :type  'boolean)

(defcustom sw-module-option-force-reload t
  "*If t, save .magikc files when loading sw-module."
  :group 'smallworld
  :type  'boolean)

(defvar sw-module-mode-map (make-sparse-keymap)
  "Keymap for Magik sw-module.def files")

(defvar sw-module-f2-map (make-sparse-keymap)
  "Keymap for the F2 function key in Magik sw-module.def buffers")

(fset 'sw-module-f2-map   sw-module-f2-map)

(define-key sw-module-mode-map [f2]    'sw-module-f2-map)

(define-key sw-module-f2-map   "b"     'sw-module-transmit-buffer)
(define-key sw-module-f2-map   "c"     'sw-module-compile-messages)
(define-key sw-module-f2-map   "d"     'sw-module-reload-sw-module-definition)
(define-key sw-module-f2-map   "s"     'sw-module-toggle-save-magikc)
(define-key sw-module-f2-map   "r"     'sw-module-toggle-force-reload)
(define-key sw-module-f2-map   "R"     'sw-module-toggle-remove-sw-module)

(defvar sw-module-menu nil
  "Keymap for the Magik sw-module.def buffer menu bar")

(easy-menu-define sw-module-menu sw-module-mode-map
  "Menu for Sw-Module mode."
  `(,resources-sw-module-menu
    [,resources-sw-module-menu-load-sw-module
     sw-module-transmit-buffer
     :active (sw-buffer-mode-list 'gis-mode)
     :keys "f2 b"]
    [,resources-sw-module-menu-reload-definition
     sw-module-reload-sw-module-definition
     :active (sw-buffer-mode-list 'gis-mode)
     :keys "f2 d"]
    [,resources-sw-module-menu-compile-messages
     sw-module-compile-messages
     :active (sw-buffer-mode-list 'gis-mode)
     :keys "f2 c"]
     [,resources-sw-module-menu-remove-sw-module
     sw-module-remove-sw-module
     :active (sw-buffer-mode-list 'gis-mode)
     :keys "f2 R"]
    (,resources-sw-module-menu-set-options
     [,resources-sw-module-menu-option-save-magikc-false
      (sw-module-toggle-save-magikc -1)
      :active (sw-buffer-mode-list 'gis-mode)
      :style radio
      :selected (null sw-module-option-save-magikc)
      :keys "f2 s, M-- M-1 f2 s"]
     [,resources-sw-module-menu-option-save-magikc-true
      (sw-module-toggle-save-magikc 1)
      :active (sw-buffer-mode-list 'gis-mode)
      :style radio
      :selected sw-module-option-save-magikc
      :keys "f2 s, M-1 f2 s"]
     "---"
     [,resources-sw-module-menu-option-force-reload-false
      (sw-module-toggle-force-reload -1)
      :active (sw-buffer-mode-list 'gis-mode)
      :style radio
      :selected (null sw-module-option-force-reload)
      :keys "f2 r, M-- M-1 f2 r"]
     [,resources-sw-module-menu-option-force-reload-prerequisites
      (sw-module-toggle-force-reload 'prerequisites)
      :active (sw-buffer-mode-list 'gis-mode)
      :style radio
      :selected (eq sw-module-option-force-reload 'prerequisites)
      :keys "C-u f2 r"]
     [,resources-sw-module-menu-option-force-reload-true
      (sw-module-toggle-force-reload 1)
      :active (sw-buffer-mode-list 'gis-mode)
      :style radio
      :selected (eq sw-module-option-force-reload t)
      :keys "f2 r, M-1 f2 r"])
    "---"
    [,resources-menu-sw-customize     sw-module-customize   t]
    [,resources-menu-sw-help          sw-module-help        t]))

(define-key sw-module-mode-map [f1] 'sw-module-help)

(defvar sw-module-mode-syntax-table nil
  "Syntax table in use in Sw-Module Mode buffers.")

;; Imenu configuration
(defvar sw-module-imenu-generic-expression
  '(
    (nil "^\\(\\sw+\\)\\s-*\n\\(.\\|\n\\)*\nend\\s-*$" 1)
    )
  "Imenu generic expression for Magik Message mode.  See `imenu-generic-expression'.")

;; Font-lock configuration
(defcustom sw-module-font-lock-keywords
  (list 
   '("^end\\s-*$" . font-lock-keyword-face)
   '("^hidden$" . font-lock-keyword-face)
   '("^\\(language\\)\\s-+\\(\\sw+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-type-face))
   '("^\\(\\sw+\\)\\s-*$" . font-lock-variable-name-face)
   '("^\\(\\sw+\\s-*\\sw*\\)\\s-*\\([0-9]*\\s-*[0-9]*\\)"
     (1 font-lock-function-name-face)
     (2 font-lock-constant-face))
   )
  "Default fontification of sw-module.def files."
  :group 'sw-module
  :type 'sexp)

(defun sw-module-help ()
  "Display help on how to use the Sw-Module Mode interface."
  (interactive)
  (sw-help-open sw-help-sw-module-id))

(defun sw-module-customize ()
  "Open Customization buffer for Sw-Module Mode."
  (interactive)
  (customize-group 'sw-module))

(defun sw-module-mode ()
  "Major mode for editing Magik sw-module.def files.

You can customise Sw-Module Mode with the `sw-module-mode-hook'."

  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'require-final-newline)
  (make-local-variable 'font-lock-defaults)

  (use-local-map sw-module-mode-map)
  (easy-menu-add sw-module-menu)
  (set-syntax-table sw-module-mode-syntax-table)

  (setq major-mode 'sw-module-mode
	mode-name resources-sw-module-menu
	require-final-newline t
	imenu-generic-expression sw-module-imenu-generic-expression
	font-lock-defaults
	'(sw-module-font-lock-keywords
	  nil t))

  (run-hooks 'sw-module-mode-hook))

(defun sw-module-toggle-save-magikc (arg)
  "Toggle saving of .magikc files when loading sw-module."
  (interactive "P")
  (setq sw-module-option-save-magikc
	(if (null arg)
	    (not sw-module-option-save-magikc)
	  (> (prefix-numeric-value arg) 0)))
  (message resources-sw-module-option-save-magikc-set
	   (magik-function-convert sw-module-option-save-magikc)))

(defun sw-module-toggle-force-reload (arg)
  "Toggle force_reload? option when loading sw-module.
If called with a non-integer prefix key then the :prerequisites
option is set."
  (interactive "P")
  (setq sw-module-option-force-reload
	(cond ((null arg)
	       (not sw-module-option-force-reload))
	      ((symbolp arg)
	       arg)
	      ((and current-prefix-arg (not (integerp current-prefix-arg)))
	       'prerequisites)
	      (t
	       (> (prefix-numeric-value arg) 0))))

  (message resources-sw-module-option-force-reload-set
	   (magik-function-convert sw-module-option-force-reload)))

(defun sw-module-name ()
  "Return current Sw-Module's name as a string."
  (save-excursion
    (goto-char (point-min))
    (current-word)))

(defun sw-module-reload-sw-module-definition (&optional gis)
  "Reload the sw-module definition in the GIS process."
  (interactive)
  (let* ((gis (sw-get-buffer-mode gis
				  'gis-mode
				  resources-gis-enter-buffer
				  gis-buffer
				  'gis-buffer-alist-prefix-function))
	 (sw-module (intern (concat "|" (sw-module-name) "|")))
	 (process (barf-if-no-gis gis)))
    (display-buffer gis t)
    (process-send-string
     process
     (concat
      (magik-function "sw_sw-module_manager.reload_sw-module_definition" sw-module) ;include version number?
      "$\n"))
    gis))

(defun sw-module-compile-messages (&optional gis)
  "Compile all the Sw-Module's messages in the GIS process."
  (interactive)
  (let* ((gis (sw-get-buffer-mode gis
				  'gis-mode
				  resources-gis-enter-buffer
				  gis-buffer
				  'gis-buffer-alist-prefix-function))
	 (sw-module (intern (concat "|" (sw-module-name) "|")))
	 (process (barf-if-no-gis gis)))
    (display-buffer gis t)
    (process-send-string
     process
     (concat
      (magik-function
       "_proc(sw-module_name, version)
         _if (a_sw-module << sw_sw-module_manager.sw-module(sw-module_name, version, _true)) _isnt _unset
         _then
           sw_sw-module_manager.compile_messages(a_sw-module)
         _endif
       _endproc"
       sw-module 'unset) ;include version number?
      "\n$\n"))
    gis))

(defun sw-module-remove-sw-module (&optional gis)
  "Remove the sw-module definition from the GIS process.
If sw-module definition is not known to the Magik GIS it is loaded as
a standalone sw-module."
  (interactive)
  (let* ((gis (sw-get-buffer-mode gis
				  'gis-mode
				  resources-gis-enter-buffer
				  gis-buffer
				  'gis-buffer-alist-prefix-function))
	 (sw-module (intern (concat "|" (sw-module-name) "|")))
	 (process (barf-if-no-gis gis)))
    (display-buffer gis t)
    (process-send-string
     process
     (concat
      (magik-function "sw_sw-module_manager.remove_sw-module" sw-module)
      "$\n"))
    gis))

(defun sw-module-transmit-load-sw-module (filename process)
  "Load the sw-module into the GIS process.
If sw-module definition is not known to the Magik GIS it is loaded as
a standalone sw-module."
  (let ((sw-module (intern (concat "|" (sw-module-name) "|"))))
    (process-send-string
     process
     (concat
      "_try\n"
      (magik-function "sw_sw-module_manager.load_sw-module" sw-module 'unset
		      'save_magikc?  sw-module-option-save-magikc
		      'force_reload? sw-module-option-force-reload)
      "_when sw_sw-module_no_such_sw-module\n"
      (magik-function "sw_sw-module_manager.load_standalone_sw-module_definition" filename
		      'save_magikc?  sw-module-option-save-magikc
		      'force_reload? sw-module-option-force-reload)
      "_endtry\n"
      "$\n"))))

(defun sw-module-transmit-buffer (&optional gis)
  "Send current buffer to GIS."
  (interactive)
  (let* ((gis (sw-get-buffer-mode gis
				  'gis-mode
				  resources-gis-enter-buffer
				  gis-buffer
				  'gis-buffer-alist-prefix-function))
	 (process (barf-if-no-gis gis))
	 (filename (buffer-file-name)))
    (display-buffer gis t)
    (sw-module-transmit-load-sw-module filename process)
    gis))

(defun sw-module-gis-drag-n-drop-load (gis filename)
  "Interface to Drag 'n' Drop GIS mode.
Called by `gis-drag-n-drop-load' when a Sw-Module file is dropped."
  (let ((process (barf-if-no-gis gis)))
    (sw-module-transmit-load-sw-module filename process)
    gis))

;;; Package initialisation
(if sw-module-mode-syntax-table
    nil
  (setq sw-module-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?_ "w" sw-module-mode-syntax-table)
  (modify-syntax-entry ?# "<" sw-module-mode-syntax-table)
  (modify-syntax-entry ?\n ">" sw-module-mode-syntax-table))

;;; Package registration

(or (assoc "sw-module\\.def$" auto-mode-alist)
    (push '("sw-module\\.def$" . sw-module-mode) auto-mode-alist))

(provide 'sw-module)


;;; sw-module.el ends here
