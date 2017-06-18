;;; sw-xemacs.el  -- XEmacs Configuration data

;; This file contains the necessary variable and data overrides and
;; configuration for XEmacs.
;; This must be loaded before any of the SW code is loaded to ensure
;; the overrides take place.

(defconst sw-xemacs-version "$Revision: 1.1 $")

(defvar inhibit-point-motion-hooks) ;;Shut up defadvice compiler on XEmacs

;; Configuration settings
(defvar mode-line-format-sym                'modeline-format)
(defvar mode-line-process-sym               'modeline-process)
(defvar mode-line-buffer-identification-sym 'modeline-buffer-identification)
(defvar menu-bar-update-hook-sym            'activate-menubar-hook)

;; User overrides

;; The following code example is required for XEmacs because the
;; default setting of this variable includes :help properties that are
;; not understood by XEmacs nor does it silently ignore them. since
;; the menu-sw-tools-submenu variable is designed to be configurable
;; per site I have not coded up the default value to cater for XEmacs'
;; restriction.
(defun sw-xemacs-imenu (event)
  "Start Imenu from Menu.
I cannot figure out how to get XEmacs to display a popup menu.
When selected from the Menu bar EVENT is a 'misc-event' which is
not classified as a mouse-event.

Therefore, this function simply defaults to using a
*Completions* buffer instead."
  (interactive "@e")
  (let ((last-command-event nil))
    (call-interactively 'imenu)))

(defvar menu-sw-tools-submenu
  ;;not translating these as they are not Smallworld code
  '(;; "Dynamic TODO mode" is actually written by me but is too
    ;; general to be classified as a Smallworld extension.
    ["Dynamic TODO mode" todo-dynamic-mode
     :active (fboundp 'todo-dynamic-mode)
     :style toggle
     :selected (and (boundp 'todo-dynamic-timer)
		    todo-dynamic-timer)]
    "---"
    ["Global Font Lock" global-font-lock-mode
     :active (fboundp 'global-font-lock-mode)
     :style toggle
     :selected (and (boundp 'global-font-lock-mode)
		    global-font-lock-mode)]
    ["Imenu" sw-xemacs-imenu
     :active (fboundp 'imenu)
     :keys "Shift Mouse-2"]
    ["Occur" occur
     :active (fboundp 'occur)]
    ["MSB" msb-mode
     :active (fboundp 'msb-mode)
     :style toggle
     :selected (and (boundp 'msb-mode)
		    msb-mode)]
    ["Which Function Mode" which-func-mode
     :active (fboundp 'which-func-mode)
     :style toggle
     :selected (and (boundp 'which-func-mode) which-func-mode)]
    ["Show Trailing Whitespace" toggle-show-trailing-whitespace
     :active (boundp 'show-trailing-whitespace)
     :style toggle
     :selected (and (boundp 'show-trailing-whitespace)
		    show-trailing-whitespace)]))

;; XEmacs apparently cannot display background data on its mode-line
;; and does not support inverse video either
(defface cb-cursor-face
  '((t (:foreground "Grey50")))
  "Face to use for the Mode line cursor."
  :group 'cb-faces)

(defvar cb-mode-line-cursor "|")

(defun cb-mode-line-interface ()
  "Enable mode-line mouse clicks in XEmacs."
  (interactive)
  (make-variable-buffer-local 'modeline-map)
  (define-key modeline-map 'button1 'cb-mode-line-click)
  (define-key modeline-map 'button2 'cb-mode-line-click))

(defvar cb-mode-hook nil)
(add-hook 'cb-mode-hook 'cb-mode-line-interface)

(defun magik-method-name-set-text-extent (buf method)
  "Return string combining BUF and METHOD suitable for display in mode-line."
  (list (cons modeline-buffer-id-left-extent "XEmacs%N:")
	(cons modeline-buffer-id-right-extent (format " %-17s[%s]" buf method))))
  
(setq magik-method-name-set-text-function 'magik-method-name-set-text-extent)

;;;Set faces that are "inherited" from font-lock.
;;XEmacs does not use variables to refer to the face to use as FSF Emacs does.
(eval-after-load "font-lock"
'(progn
   (or (find-face 'font-lock-doc-face) ;;XEmacs defines font-lock-doc-string-face
       (copy-face 'font-lock-doc-string-face 'font-lock-doc-face))

   (or (find-face 'cb-font-lock-class-face)
       (copy-face 'font-lock-type-face 'cb-font-lock-class-face))

   (or (find-face 'cb-font-lock-method-face)
       (copy-face 'font-lock-function-name-face 'cb-font-lock-method-face))

   (or (find-face 'cb2-font-lock-on-face)
       (copy-face 'font-lock-function-name-face 'cb2-font-lock-on-face))

   (or (find-face 'cb2-font-lock-off-face)
       (copy-face 'font-lock-variable-name-face 'cb2-font-lock-off-face))

   (or (find-face 'cb2-font-lock-thermometer-on-face)
       (copy-face 'font-lock-type-face 'cb2-font-lock-thermometer-on-face))

   (or (find-face 'cb2-font-lock-thermometer-off-face)
       (copy-face 'font-lock-constant-face 'cb2-font-lock-thermometer-off-face))

   (or (find-face 'gis-font-lock-prompt-face)
       (copy-face 'font-lock-type-face 'gis-font-lock-prompt-face))

   (or (find-face 'gis-font-lock-error-face)
       (copy-face 'font-lock-warning-face 'gis-font-lock-error-face))

   (or (find-face 'gis-font-lock-traceback-face)
       (copy-face 'font-lock-warning-face 'gis-font-lock-traceback-face))

   (or (find-face 'magik-font-lock-class-face)
       (copy-face 'font-lock-type-face 'magik-font-lock-class-face))

   (or (find-face 'magik-font-lock-method-face)
       (copy-face 'font-lock-function-name-face 'magik-font-lock-method-face))

   (or (find-face 'magik-font-lock-dynamic-face)
       (copy-face 'font-lock-variable-name-face 'magik-font-lock-dynamic-face))
   ))

(provide 'sw-xemacs)
