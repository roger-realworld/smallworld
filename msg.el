;;; msg.el -- mode for editing Magik msg and hmsg Message files.

(eval-when-compile
  (require 'cl)
  (require 'easymenu)
  (require 'font-lock)
  (defvar msb-menu-cond)

  (require 'misc-sw)
  (require 'sw-help)
  (require 'utils-sw)
  (require 'magik)
  (require 'gis))

(defgroup msg nil
  "Customise Magik Messages group."
  :group 'smallworld
  :group 'tools)

(defconst msg-version "$Revision: 1.3 $")

(defcustom msg-mode-hook nil
  "*Hook to run after MSG mode is set."
  :group 'msg
  :type  'hook)

(defvar msg-mode-map (make-sparse-keymap)
  "Keymap for Magik Message files")

(defvar msg-f2-map (make-sparse-keymap)
  "Keymap for the F2 function key in Magik Message buffers")

(fset 'msg-f2-map   msg-f2-map)

(define-key msg-mode-map [f2]    'msg-f2-map)

(define-key msg-f2-map    [down] 'msg-forward-message)
(define-key msg-f2-map    [up]   'msg-backward-message)
(define-key msg-f2-map    "b"    'msg-transmit-buffer)
(define-key msg-f2-map    "c"    'msg-compile-module-messages)
(define-key msg-f2-map    "m"    'msg-mark-message)

(defvar msg-menu nil
  "Keymap for the Magik Message buffer menu bar")

(easy-menu-define msg-menu msg-mode-map
  "Menu for msg mode."
  `(,resources-msg-menu
    [,resources-msg-menu-transmit-buffer msg-transmit-buffer
					 :active (sw-buffer-mode-list 'gis-mode)
					 :keys "f2 b"]
    [,resources-msg-menu-compile-module  msg-compile-module-messages
					 :active (sw-buffer-mode-list 'gis-mode)
					 :keys "f2 c"]
    [,resources-msg-menu-next            msg-forward-message
					 :active t
					 :keys "f2 down"]
    [,resources-msg-menu-previous        msg-backward-message
					 :active t
					 :keys "f2 up"]
    [,resources-msg-menu-mark-message    msg-mark-message
					 :active t
					 :keys "f2 m"]
    "---"
    [,resources-menu-sw-customize        msg-customize        t]
    [,resources-menu-sw-help             msg-help             t]))

(define-key msg-mode-map [f1] 'msg-help)

(defvar msg-mode-syntax-table nil
  "Syntax table in use in MSG-mode buffers.")

;; Imenu configuration
(defvar msg-imenu-generic-expression
  '(
    (nil "^:\\(\\sw+\\)" 1) ;;Normal messages
    (nil "^:\\s$\\(\\S$+\\)\\s$" 1) ;; | | Quoted messages
    ("Groups" "^+\\s-+\\(\\sw+\\)" 1)
    )
  "Imenu generic expression for Magik Message mode.  See `imenu-generic-expression'.")

;; Font-lock configuration
(defcustom msg-font-lock-keywords
  (list 
   '("^\\(:\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\)\\s-+\\(:\\sw+\\)"
     (1 font-lock-function-name-face)
     (3 font-lock-constant-face))
   '("^\\([+]\\)\\s-+\\(\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\)"
     (1 font-lock-type-face)
     (2 font-lock-keyword-face))
   '("^:\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?" . font-lock-function-name-face)
   '("^#%\\s-*text_encoding.*$" . font-lock-warning-face)
   '("#[0-9]+" . font-lock-variable-name-face)
   '("#.*" . font-lock-comment-face)
   )
  "Default fontification of Magik Messages."
  :group 'msg
  :type 'sexp)

;; Help
(defun msg-help ()
  "Display help on how to use the Msg Mode interface."
  (interactive)
  (sw-help-open sw-help-msg-id))

(defun msg-customize ()
  "Open Customization buffer for Msg Mode."
  (interactive)
  (customize-group 'msg))

(defun msg-forward-message ()
  "Put point at beginning of line of next message."
  (interactive)
  (if (not (looking-at "^:"))
      (re-search-forward "^:" nil t)
    (forward-char 1)
    (or (re-search-forward "^:" nil t)
	(goto-char (point-max))))
  (beginning-of-line))

(defun msg-backward-message ()
  "Put point at beginning of line of previous message."
  (interactive)
  (if (not (looking-at "^:"))
      (re-search-backward "^:" nil t)
    (backward-char 1)
    (or (re-search-backward "^:" nil t)
	(goto-char (point-min))))
  (beginning-of-line))

(defun msg-mark-message ()
  "Mark the current message."
  ;; also returns the value of mark.
  (interactive)
  (push-mark (point))
  (msg-forward-message)
  (if (save-match-data (string-match "hmsg$" (buffer-name)))
      nil
    (re-search-backward "^\n" (- (point) 1) t))
  (push-mark (point) nil t)
  (msg-backward-message))

(defun msg-mode ()
  "Major mode for editing Magik Message files.

You can customise msg-mode with the msg-mode-hook."

  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'require-final-newline)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'outline-regexp)

  (use-local-map msg-mode-map)
  (easy-menu-add msg-menu)
  (set-syntax-table msg-mode-syntax-table)

  (setq major-mode 'msg-mode
	mode-name resources-msg-menu
	require-final-newline t
	imenu-generic-expression msg-imenu-generic-expression
	font-lock-defaults
	'(msg-font-lock-keywords
	  nil t)
	outline-regexp "^:\\(\\sw+\\).*")

  (run-hooks 'msg-mode-hook))

(defun msg-transmit-buffer (&optional gis)
  "Send the buffer to the GIS process.
The GIS process used is either that given by BUF or the variable `gis-buffer'."
  (interactive)
  (let ((gis (sw-get-buffer-mode gis
				 'gis-mode
				 resources-gis-enter-buffer
				 gis-buffer
				 'gis-buffer-alist-prefix-function))
	(process (barf-if-no-gis gis))
	(filename (buffer-file-name)))
    ;; Load messages
    (message resources-msg-loaded-in-buffer filename gis)
    (process-send-string
     process
     (concat
      (magik-function "message_handler.compile_message_file" filename)
      "\n$\n"))
    gis))

(defun msg-compile-module-messages (&optional gis)
  "Compile all messages asociated with the module this buffer is assocaiated with in a GIS process.
The GIS process used is either that given by BUF or the variable `gis-buffer'."
  (interactive)
  (let ((gis (sw-get-buffer-mode gis
				 'gis-mode
				 resources-gis-enter-buffer
				 gis-buffer
				 'gis-buffer-alist-prefix-function))
	(process (barf-if-no-gis gis))
	(directory (file-name-directory (buffer-file-name))))
    ;; Load messages
    (message resources-msg-compile-module-in-buffer gis)
    (process-send-string
     process
     (format
      "_proc(directory)
         module << sw_module_manager.locate_module(directory)
         sw_module_manager.compile_messages(module)
      _endproc(%S)\n$\n"
      directory))
    gis))

(defun msg-gis-drag-n-drop-load (gis filename)
  "Interface to Drag 'n' Drop GIS mode.
Called by `gis-drag-n-drop-load' when a Msg file is dropped."
  (let ((process (barf-if-no-gis gis)))
    (message resources-msg-loaded-in-buffer filename gis)
    (process-send-string
     process
     (concat 
      (magik-function "load_message_file" filename 'image_override)
      "$\n"))))

(defvar msg-multi-gis-processes nil
  "Note whether more than one GIS has been used.")
 
;;; Package initialisation
(if msg-mode-syntax-table
    ()
  (setq msg-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?: "w" msg-mode-syntax-table)
  (modify-syntax-entry ?_ "w" msg-mode-syntax-table)
  (modify-syntax-entry ?? "w" msg-mode-syntax-table)
  (modify-syntax-entry ?! "w" msg-mode-syntax-table)
  ;; multi quote
  (modify-syntax-entry ?| "$" msg-mode-syntax-table)
  ;; variable intro
  (modify-syntax-entry ?# "/" msg-mode-syntax-table))

;;; Package registration

(or (assoc "\\.msg$" auto-mode-alist)
    (push '("\\.msg$" . msg-mode) auto-mode-alist))
(or (assoc "\\.hmsg$" auto-mode-alist)
    (push '("\\.hmsg$" . msg-mode) auto-mode-alist))

;; speedbar configuration
(eval-after-load 'speedbar
  '(progn
     (speedbar-add-supported-extension ".msg")
     (speedbar-add-supported-extension ".hmsg")))

;;MSB configuration
(defun msg-msb-configuration ()
  "Adds Msg files to msb menu, supposes that msb is already loaded."
  (let* ((l (length msb-menu-cond))
	 (last (nth (1- l) msb-menu-cond))
	 (precdr (nthcdr (- l 2) msb-menu-cond)) ; cdr of this is last
	 (handle (1- (nth 1 last))))
    (setcdr precdr (list
		    (list
		     '(eq major-mode 'msg-mode)
		     handle
		     "Msg Files (%d)")
		    last))))

(eval-after-load 'msb
  '(msg-msb-configuration))

(provide 'msg)


;;; msg.el ends here
