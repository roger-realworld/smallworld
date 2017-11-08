;;; module.el -- mode for editing Magik module.def files.

(eval-when-compile (require 'cl)
		   (require 'easymenu)
		   (require 'font-lock)
		   (require 'misc-sw)
		   (require 'sw-help)
		   (require 'utils-sw)
		   (require 'magik)
		   (require 'gis))

(defgroup sw2-module nil
  "Customise Magik sw2-module.def files group."
  :group 'smallworld
  :group 'tools)

(defconst sw2-module-version "$Revision: 1.4 $")

(defcustom sw2-module-mode-hook nil
  "*Hook to run after Module Mode is set."
  :group 'module
  :type  'hook)

(defcustom sw2-module-option-save-magikc t
  "*If t, save .magikc files when loading module."
  :group 'smallworld
  :type  'boolean)

(defcustom sw2-module-option-force-reload t
  "*If t, save .magikc files when loading module."
  :group 'smallworld
  :type  'boolean)

(defvar sw2-module-mode-map (make-sparse-keymap)
  "Keymap for Magik module.def files")

(defvar sw2-module-f2-map (make-sparse-keymap)
  "Keymap for the F2 function key in Magik module.def buffers")

(fset 'module-f2-map   sw2-module-f2-map)

(define-key module-mode-map [f2]    'module-f2-map)

(define-key sw2-module-f2-map   "b"     'module-transmit-buffer)
(define-key sw2-module-f2-map   "c"     'module-compile-messages)
(define-key sw2-module-f2-map   "d"     'module-reload-module-definition)
(define-key sw2-module-f2-map   "s"     'module-toggle-save-magikc)
(define-key sw2-module-f2-map   "r"     'module-toggle-force-reload)
(define-key sw2-module-f2-map   "R"     'module-toggle-remove-module)

(defvar sw2-module-menu nil
  "Keymap for the Magik module.def buffer menu bar")

(easy-menu-define sw2-module-menu sw2-module-mode-map
  "Menu for Module mode."
  `(,resources-module-menu
    [,resources-module-menu-load-module
     sw2-module-transmit-buffer
     :active (sw-buffer-mode-list 'gis-mode)
     :keys "f2 b"]
    [,resources-module-menu-reload-definition
     sw2-module-reload-module-definition
     :active (sw-buffer-mode-list 'gis-mode)
     :keys "f2 d"]
    [,resources-module-menu-compile-messages
     sw2-module-compile-messages
     :active (sw-buffer-mode-list 'gis-mode)
     :keys "f2 c"]
     [,resources-module-menu-remove-module
     sw2-module-remove-module
     :active (sw-buffer-mode-list 'gis-mode)
     :keys "f2 R"]
    (,resources-module-menu-set-options
     [,resources-module-menu-option-save-magikc-false
      (module-toggle-save-magikc -1)
      :active (sw-buffer-mode-list 'gis-mode)
      :style radio
      :selected (null sw2-module-option-save-magikc)
      :keys "f2 s, M-- M-1 f2 s"]
     [,resources-module-menu-option-save-magikc-true
      (module-toggle-save-magikc 1)
      :active (sw-buffer-mode-list 'gis-mode)
      :style radio
      :selected sw2-module-option-save-magikc
      :keys "f2 s, M-1 f2 s"]
     "---"
     [,resources-module-menu-option-force-reload-false
      (module-toggle-force-reload -1)
      :active (sw-buffer-mode-list 'gis-mode)
      :style radio
      :selected (null sw2-module-option-force-reload)
      :keys "f2 r, M-- M-1 f2 r"]
     [,resources-module-menu-option-force-reload-prerequisites
      (module-toggle-force-reload 'prerequisites)
      :active (sw-buffer-mode-list 'gis-mode)
      :style radio
      :selected (eq sw2-module-option-force-reload 'prerequisites)
      :keys "C-u f2 r"]
     [,resources-module-menu-option-force-reload-true
      (module-toggle-force-reload 1)
      :active (sw-buffer-mode-list 'gis-mode)
      :style radio
      :selected (eq sw2-module-option-force-reload t)
      :keys "f2 r, M-1 f2 r"])
    "---"
    [,resources-menu-sw-customize     sw2-module-customize   t]
    [,resources-menu-sw-help          sw2-module-help        t]))

(define-key sw2-module-mode-map [f1] 'module-help)

(defvar sw2-module-mode-syntax-table nil
  "Syntax table in use in Module Mode buffers.")

;; Imenu configuration
(defvar sw2-module-imenu-generic-expression
  '(
    (nil "^\\(\\sw+\\)\\s-*\n\\(.\\|\n\\)*\nend\\s-*$" 1)
    )
  "Imenu generic expression for Magik Message mode.  See `imenu-generic-expression'.")

;; Font-lock configuration
(defcustom sw2-module-font-lock-keywords
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
  "Default fontification of module.def files."
  :group 'module
  :type 'sexp)

(defun sw2-module-help ()
  "Display help on how to use the Module Mode interface."
  (interactive)
  (sw-help-open sw-help-module-id))

(defun sw2-module-customize ()
  "Open Customization buffer for Module Mode."
  (interactive)
  (customize-group 'module))

(define-generic-mode sw2-module-mode
    '(?#)
    nil
   '(("^end\\s-*$" . font-lock-keyword-face)
     ("^hidden$" . font-lock-keyword-face)
     ("^\\(language\\)\\s-+\\(\\sw+\\)"
      (1 font-lock-keyword-face)
      (2 font-lock-type-face))
     ("^\\(\\sw+\\)\\s-*$" . font-lock-variable-name-face)
     ("^\\(\\sw+\\s-*\\sw*\\)\\s-*\\([0-9]*\\s-*[0-9]*\\)"
      (1 font-lock-function-name-face)
      (2 font-lock-constant-face)))
   '("module\\.def$" "module\\.def$")
   '((make-local-variable 'require-final-newline)
     (make-local-variable 'font-lock-defaults)
     (use-local-map sw2-module-mode-map)
     (easy-menu-add sw2-module-menu)
     (set-syntax-table sw2-module-mode-syntax-table)
     
     (setq major-mode 'module-mode
           mode-name resources-module-menu
           require-final-newline t
           imenu-generic-expression sw2-module-imenu-generic-expression
           font-lock-defaults
           '(module-font-lock-keywords
             nil t))

  (run-hooks 'module-mode-hook))

(defun sw2-module-mode ()
  "Major mode for editing Magik module.def files.

You can customise Module Mode with the `module-mode-hook'."

  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'require-final-newline)

  (use-local-map sw2-module-mode-map)
  (easy-menu-add sw2-module-menu)
  (set-syntax-table sw2-module-mode-syntax-table)

  (setq major-mode 'module-mode
	mode-name resources-module-menu
	require-final-newline t
	imenu-generic-expression sw2-module-imenu-generic-expression
	font-lock-defaults
	'(module-font-lock-keywords
	  nil t))

  (run-hooks 'module-mode-hook))

(defun sw2-module-toggle-save-magikc (arg)
  "Toggle saving of .magikc files when loading module."
  (interactive "P")
  (setq sw2-module-option-save-magikc
	(if (null arg)
	    (not sw2-module-option-save-magikc)
	  (> (prefix-numeric-value arg) 0)))
  (message resources-module-option-save-magikc-set
	   (magik-function-convert sw2-module-option-save-magikc)))

(defun sw2-module-toggle-force-reload (arg)
  "Toggle force_reload? option when loading module.
If called with a non-integer prefix key then the :prerequisites
option is set."
  (interactive "P")
  (setq sw2-module-option-force-reload
	(cond ((null arg)
	       (not sw2-module-option-force-reload))
	      ((symbolp arg)
	       arg)
	      ((and current-prefix-arg (not (integerp current-prefix-arg)))
	       'prerequisites)
	      (t
	       (> (prefix-numeric-value arg) 0))))

  (message resources-module-option-force-reload-set
	   (magik-function-convert sw2-module-option-force-reload)))

(defun sw2-module-name ()
  "Return current Module's name as a string."
  (save-excursion
    (goto-char (point-min))
    (current-word)))

(defun sw2-module-reload-module-definition (&optional gis)
  "Reload the module definition in the GIS process."
  (interactive)
  (let* ((gis (sw-get-buffer-mode gis
				  'gis-mode
				  resources-gis-enter-buffer
				  gis-buffer
				  'gis-buffer-alist-prefix-function))
	 (module (intern (concat "|" (module-name) "|")))
	 (process (barf-if-no-gis gis)))
    (display-buffer gis t)
    (process-send-string
     process
     (concat
      (magik-function "sw_module_manager.reload_module_definition" sw2-module) ;include version number?
      "$\n"))
    gis))

(defun sw2-module-compile-messages (&optional gis)
  "Compile all the Module's messages in the GIS process."
  (interactive)
  (let* ((gis (sw-get-buffer-mode gis
				  'gis-mode
				  resources-gis-enter-buffer
				  gis-buffer
				  'gis-buffer-alist-prefix-function))
	 (module (intern (concat "|" (module-name) "|")))
	 (process (barf-if-no-gis gis)))
    (display-buffer gis t)
    (process-send-string
     process
     (concat
      (magik-function
       "_proc(module_name, version)
         _if (a_module << sw_module_manager.module(module_name, version, _true)) _isnt _unset
         _then
           sw_module_manager.compile_messages(a_module)
         _endif
       _endproc"
       sw2-module 'unset) ;include version number?
      "\n$\n"))
    gis))

(defun sw2-module-remove-module (&optional gis)
  "Remove the module definition from the GIS process.
If module definition is not known to the Magik GIS it is loaded as
a standalone module."
  (interactive)
  (let* ((gis (sw-get-buffer-mode gis
				  'gis-mode
				  resources-gis-enter-buffer
				  gis-buffer
				  'gis-buffer-alist-prefix-function))
	 (module (intern (concat "|" (module-name) "|")))
	 (process (barf-if-no-gis gis)))
    (display-buffer gis t)
    (process-send-string
     process
     (concat
      (magik-function "sw_module_manager.remove_module" sw2-module)
      "$\n"))
    gis))

(defun sw2-module-transmit-load-module (filename process)
  "Load the module into the GIS process.
If module definition is not known to the Magik GIS it is loaded as
a standalone module."
  (let ((module (intern (concat "|" (module-name) "|"))))
    (process-send-string
     process
     (concat
      "_try\n"
      (magik-function "sw_module_manager.load_module" sw2-module 'unset
		      'save_magikc?  sw2-module-option-save-magikc
		      'force_reload? sw2-module-option-force-reload)
      "_when sw_module_no_such_module\n"
      (magik-function "sw_module_manager.load_standalone_module_definition" filename
		      'save_magikc?  sw2-module-option-save-magikc
		      'force_reload? sw2-module-option-force-reload)
      "_endtry\n"
      "$\n"))))

(defun sw2-module-transmit-buffer (&optional gis)
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
    (module-transmit-load-module filename process)
    gis))

(defun sw2-module-gis-drag-n-drop-load (gis filename)
  "Interface to Drag 'n' Drop GIS mode.
Called by `gis-drag-n-drop-load' when a Module file is dropped."
  (let ((process (barf-if-no-gis gis)))
    (module-transmit-load-module filename process)
    gis))

;;; Package initialisation
(if sw2-module-mode-syntax-table
    nil
  (setq sw2-module-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?_ "w" sw2-module-mode-syntax-table)
  (modify-syntax-entry ?# "<" sw2-module-mode-syntax-table)
  (modify-syntax-entry ?\n ">" sw2-module-mode-syntax-table))

;;; Package registration



(provide 'module)


;;; sw2-module.el ends here
