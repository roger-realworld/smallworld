;; package --- mode for editing Magik sw-module.def files.
;;; Commentary:
;;; Code:

(defvar gis-buffer nil)

(defcustom sw-module-option-save-magikc t
  "*If t, save .magikc files when loading sw-module."
  :group 'smallworld
  :type  'boolean)

(defcustom sw-module-option-force-reload t
  "*If t, save .magikc files when loading sw-module."
  :group 'smallworld
  :type  'boolean)

(defvar sw-module-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<f2> b")     'sw-module-transmit-buffer)
    (define-key map (kbd "<f2> c")     'sw-module-compile-messages)
    (define-key map (kbd "<f2> d")     'sw-module-reload-sw-module-definition)
    (define-key map (kbd "<f2> s")     'sw-module-toggle-save-magikc)
    (define-key map (kbd "<f2> r")     'sw-module-toggle-force-reload)
    (define-key map (kbd "<f2> R")     'sw-module-toggle-remove-sw-module)
    (define-key map (kbd "<f1>")       'sw-module-help))
  "Keymap for Magik sw-module.def files.")

;;; Package initialisation
(defvar sw-module-mode-syntax-table nil)

(setq sw-module-mode-syntax-table
      (let ((st (make-syntax-table)))
	(modify-syntax-entry ?_ "w" st)
	(modify-syntax-entry ?# "<" st)
	(modify-syntax-entry ?\n ">" st)
	st))

;; Imenu configuration
(defvar sw-module-imenu-generic-expression
  '(
    (nil "^\\(\\sw+\\)\\s-*\n\\(.\\|\n\\)*\nend\\s-*$" 1)
    )
  "Imenu generic expression for Magik Message mode.  See `imenu-generic-expression'.")

;; Font-lock configuration
(defvar sw-module-mode-font-lock-defaults
  '((
     ("^end\\s-*$" . font-lock-keyword-face)
     ("^hidden$" . font-lock-keyword-face)
     ("^\\(language\\)\\s-+\\(\\sw+\\)"
      (1 font-lock-keyword-face)
      (2 font-lock-type-face))
     ("^\\(\\sw+\\)\\s-*$" . font-lock-variable-name-face)
     ("^\\(\\sw+\\s-*\\sw*\\)\\s-*\\([0-9]*\\s-*[0-9]*\\)"
      (1 font-lock-function-name-face)
      (2 font-lock-constant-face))
     )))

(define-derived-mode sw-module-mode prog-mode "SW Module"
  "Major mode for editing Magik sw-module.def files."
  :group 'smallworld
  (setq font-lock-defaults sw-module-mode-font-lock-defaults))

(defun sw-module-toggle-save-magikc (arg)
  "Toggle saving of .magikc files with ARG when loading sw-module."
  (interactive "P")
  (setq sw-module-option-save-magikc
	(if (null arg)
	    (not sw-module-option-save-magikc)
	  (> (prefix-numeric-value arg) 0)))
  (message ":save_magikc? to %s"
	   (magik-function-convert sw-module-option-save-magikc)))

(defun sw-module-toggle-force-reload (arg)
  "Toggle force_reload? option when loading sw-module.
If called with a non-integer prefix key (ARG) then the :prerequisites
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

  (message ":force_reload? option to %s"
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
				  "Enter Gis process buffer:"
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
				  "Enter Gis process buffer:"
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
				  "Enter Gis process buffer:"
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
  "Load the sw-module FILENAME into the GIS process PROCESS.
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
				  "Enter Gis process buffer:"
				  gis-buffer
				  'gis-buffer-alist-prefix-function))
	 (process (barf-if-no-gis gis))
	 (filename (buffer-file-name)))
    (display-buffer gis t)
    (sw-module-transmit-load-sw-module filename process)
    gis))

(defun sw-module-gis-drag-n-drop-load (gis filename)
  "Interface to Drag 'n' Drop into GIS file FILENAME.
Called by `gis-drag-n-drop-load' when a Sw-Module file is dropped."
  (let ((process (barf-if-no-gis gis)))
    (sw-module-transmit-load-sw-module filename process)
    gis))

;;; Package registration

(or (assoc "sw-module\\.def$" auto-mode-alist)
    (push '("sw-module\\.def$" . sw-module-mode) auto-mode-alist))

(provide 'sw-module-mode)


;;; sw-module-mode.el ends here
