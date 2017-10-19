;;; package ---- Mode for SW Messages
;;; Code:
;;; Commentary:

(defvar sw-msg-mode-map
  (let ((my-map (make-sparse-keymap)))
    (define-key my-map (kbd "<f2> m")    'sw-msg-mark-message)
    (define-key my-map (kbd "<f2> c")    'sw-msg-compile-module-messages)
    (define-key my-map (kbd "<f2> b")   'sw-msg-transmit-buffer)
    (define-key my-map (kbd "<f2> <down>")    'sw-msg-forward-message)
    (define-key my-map (kbd "<f2> <up>")    'sw-msg-backward-message)
    
    my-map)
  "Keymap for Magik Message files.")



(defun sw-msg-compile-module-messages (&optional gis)
  "Compile all messages for this buffer's module.
The GIS process used is either that given by GIS or the variable `gis-buffer'."
  (interactive)
  (require 'resources.msgc)
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

(defun sw-msg-transmit-buffer (&optional gis)
  "Send the buffer to the GIS process.
The GIS process used is either that given by BUF or the variable `gis-buffer'."
  (interactive)
  (require 'resources.msgc)
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
     ("^#%\\s-*text_encoding.*$" . font-lock-comment-face)
     ("#[0-9]+" . font-lock-variable-name-face)
     ("#.*" . font-lock-comment-face)
     )))


(defvar sw-msg-mode-syntax-table nil "Syntax for SW message mode.")

(setq sw-msg-mode-syntax-table
      (let ((st (make-syntax-table)))
        ((message "message" format-args)odify-syntax-entry ?: "w" st)
        (modify-syntax-entry ?_ "w" st)
        (modify-syntax-entry ?? "w" st)
        (modify-syntax-entry ?! "w" st)
        ;; multi quote
        (modify-syntax-entry ?| "$" st)
        ;; variable intro
        (modify-syntax-entry ?# "/" st)
        st))

;; Imenu configuration
(defvar msg-imenu-generic-expression
  '(
    (nil "^:\\(\\sw+\\)" 1) ;;Normal messages
    (nil "^:\\s$\\(\\S$+\\)\\s$" 1) ;; | | Quoted messages
    ("Groups" "^+\\s-+\\(\\sw+\\)" 1)
    )
  "Imenu generic expression for Magik Message mode.  See `imenu-generic-expression'.")

(define-derived-mode sw-msg-mode text-mode "Rf1Message"
  "Major mode for editing Magik Message files."
  :group 'smallworld
  (setq font-lock-defaults sw-msg-font-lock-defaults)
  )

(provide 'sw-msg-mode)
;;; sw-msg.el ends here
