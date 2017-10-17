;;; sw-msg.el -- mode for editing Magik msg and hmsg Message files.

(eval-when-compile
  (require 'easymenu))

(require 'gis)
(require 'resources)

(defgroup sw-msg nil
  "Customise Magik Messages group."
  :group 'smallworld
  :group 'tools)

(defconst sw-msg-version "$Revision: 1.3 $")

(defvar sw-msg-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<f1>") 'sw-msg-help)
    (define-key map (kbd "<f2> b")    'sw-msg-transmit-buffer)
    (define-key map (kbd "<f2> c")    'sw-msg-compile-module-messages)
    (define-key map (kbd "<f2> m")    'sw-msg-mark-message))
    "Keymap for Magik Message files")

(defvar sw-msg-menu nil
  "Keymap for the Magik Message buffer menu bar")

(easy-menu-define sw-msg-menu sw-msg-mode-map
  "Menu for msg mode."
  `(,resources-msg-menu
    [,resources-msg-menu-transmit-buffer sw-msg-transmit-buffer
					 :active (sw-buffer-mode-list 'gis-mode)
					 :keys "f2 b"]
    [,resources-msg-menu-compile-module  sw-msg-compile-module-messages
					 :active (sw-buffer-mode-list 'gis-mode)
					 :keys "f2 c"]
    [,resources-msg-menu-next            sw-msg-forward-message
					 :active t
					 :keys "f2 down"]
    [,resources-msg-menu-previous        sw-msg-backward-message
					 :active t
					 :keys "f2 up"]
    [,resources-msg-menu-mark-message    sw-msg-mark-message
					 :active t
					 :keys "f2 m"]
    "---"
    [,resources-menu-sw-customize        sw-msg-customize        t]
    [,resources-menu-sw-help             sw-msg-help             t]))


(defvar sw-msg-mode-syntax-table nil "Syntax table for message mode.")

(setq sw-msg-mode-syntax-table
      (let ((my-syntax-table (make-syntax-table)))
        (modify-syntax-entry ?: "w" my-syntax-table)
        (modify-syntax-entry ?_ "w" my-syntax-table)
        (modify-syntax-entry ?? "w" my-syntax-table)
        (modify-syntax-entry ?! "w" my-syntax-table)
        ;; multi quote
        (modify-syntax-entry ?| "$" my-syntax-table)
        ;; variable intro
        (modify-syntax-entry ?# "/" my-syntax-table)
        my-syntax-table))

;; Imenu configuration
(defvar sw-msg-imenu-generic-expression
  '(
    (nil "^:\\(\\sw+\\)" 1) ;;Normal messages
    (nil "^:\\s$\\(\\S$+\\)\\s$" 1) ;; | | Quoted messages
    ("Groups" "^+\\s-+\\(\\sw+\\)" 1)
    )
  "Imenu generic expression for Magik Message mode.  See `imenu-generic-expression'.")

;; Font-lock configuration
(defvar sw-msg-font-lock-defaults
  '((
     ("^\\(:\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\)\\s-+\\(:\\sw+\\)"
      (1 font-lock-function-name-face)
      (3 font-lock-constant-face))
     ("^\\([+]\\)\\s-+\\(\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\)"
     (1 font-lock-type-face)
     (2 font-lock-keyword-face))
     ("^:\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?" . font-lock-function-name-face)
     ("^#%\\s-*text_encoding.*$" . font-lock-warning-face)
     ("#[0-9]+" . font-lock-variable-name-face)
     ("#.*" . font-lock-comment-face)
     )))

;; Help
(defun sw-msg-help ()
  "Display help on how to use the Msg Mode interface."
  (interactive)
  (sw-help-open sw-help-msg-id))

(defun sw-msg-customize ()
  "Open Customization buffer for Msg Mode."
  (interactive)
  (customize-group 'msg))

(defun sw-msg-forward-message ()
  "Put point at beginning of line of next message."
  (interactive)
  (if (not (looking-at "^:"))
      (re-search-forward "^:" nil t)
    (forward-char 1)
    (or (re-search-forward "^:" nil t)
	(goto-char (point-max))))
  (beginning-of-line))

(defun sw-msg-backward-message ()
  "Put point at beginning of line of previous message."
  (interactive)
  (if (not (looking-at "^:"))
      (re-search-backward "^:" nil t)
    (backward-char 1)
    (or (re-search-backward "^:" nil t)
	(goto-char (point-min))))
  (beginning-of-line))

(defun sw-msg-mark-message ()
  "Mark the current message."
  ;; also returns the value of mark.
  (interactive)
  (push-mark (point))
  (sw-msg-forward-message)
  (if (save-match-data (string-match "hmsg$" (buffer-name)))
      nil
    (re-search-backward "^\n" (- (point) 1) t))
  (push-mark (point) nil t)
  (sw-msg-backward-message))

(define-derived-mode sw-msg-mode text-mode "RfMessage"
  "Major mode for editing Magik Message files."
  (easy-menu-add sw-msg-menu)
  (setq font-lock-defaults sw-msg-font-lock-defaults)
  (setq imenu-generic-expression sw-msg-imenu-generic-expression))

(defun sw-msg-transmit-buffer (&optional gis)
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

(defun sw-msg-compile-module-messages (&optional gis)
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

(defun sw-msg-gis-drag-n-drop-load (gis filename)
  "Interface to Drag 'n' Drop GIS mode.
Called by `gis-drag-n-drop-load' when a Msg file is dropped."
  (let ((process (barf-if-no-gis gis)))
    (message resources-msg-loaded-in-buffer filename gis)
    (process-send-string
     process
     (concat 
      (magik-function "load_message_file" filename 'image_override)
      "$\n"))))

(defvar sw-msg-multi-gis-processes nil
  "Note whether more than one GIS has been used.")
 
;;; Package initialisation

;;; Package registration

(or (assoc "\\.msg$" auto-mode-alist)
    (push '("\\.msg$" . sw-msg-mode) auto-mode-alist))
(or (assoc "\\.hmsg$" auto-mode-alist)
    (push '("\\.hmsg$" . sw-msg-mode) auto-mode-alist))

;; speedbar configuration
(speedbar-add-supported-extension ".msg")
(speedbar-add-supported-extension ".hmsg")

(provide 'sw-msg)


;;; sw-msg.el ends here
