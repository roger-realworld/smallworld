;;; Example .emacs file for Developer/Experienced users

;; File version: $Revision: 1.4 $

;; This file contains various configurable settings a Developer using Emacs
;; may want to set. For clarity, the Emacs Customize interface is not used.

;; This file may be safely copied and loaded without harm.
;; Some settings are given but commented out in order to provide an example of
;; how that type of configuration option is set.

;;;////////////////////////////////////////
;;; Window colours
;;;////////////////////////////////////////
;; For a list of all available colours type: M-x list-colors-display
;;(set-background-color "BACKGROUND_COLOUR")
;;(set-foreground-color "FOREGROUND_COLOUR")
;;(set-cursor-color "CURSOR_COLOUR")
;;(set-mouse-color  "MOUSE_COLOUR")
;; See also initial-frame-alist and default-frame-alist variables to
;; configure the initial and default (i.e. new frames using C-x 5 2) frame parameters
;; (setq default-frame-alist '((vertical-scroll-bars . right) (width . 80)))
;; (setq initial-frame-alist '((vertical-scroll-bars . right) (width . 80)))

;;;////////////////////////////////////////
;;; Faces
;;;////////////////////////////////////////
;; For a list of all available faces type: M-x list-faces-display
;; Faces can be more conveniently set via the Customize interface
;; However, the following examples show how other functions can be used
;; to modify faces. In this case Make hte font-lock Comment face Bold and Gold.
;; (setq font-lock-comment-face (copy-face 'bold 'font-lock-comment-face))
;; (set-face-foreground 'font-lock-comment-face "Gold")

(setq inhibit-startup-message t)        ; prevent display of startup message
;;(setq inhibit-startup-echo-area-message ;prevent startup echo area message
;;      "YOUR-USER-NAME")                 ;

(setq visible-bell t)                   ; flash not beep
(setq transient-mark-mode t)            ; Enable highlighting of active marked region
(setq blink-matching-paren-distance 500000) ;maximum distance to search for paren
(setq line-number-display-limit nil)    ; Show line count for big files
(column-number-mode 1)                  ; Show current column number

(setq fill-column 80)                   ; line wrap column

;;;////////////////////////////////////////
;;; Class Browser 
;;;////////////////////////////////////////
;;(require 'cus-edit) ;for the "custom-button" faces 
;;(setq cb2-font-lock-on-face 'custom-button-pressed-face 
;;      cb2-font-lock-off-face 'custom-button-face)


;;;////////////////////////////////////////
;;; Shell buffers
;;;////////////////////////////////////////
(add-hook 'comint-output-filter-functions    ; In shells, look for password prompts
	  'comint-watch-for-password-prompt) ; to prevent typing in plain text


;;;////////////////////////////////////////
;;; Buffer fontification
;;;////////////////////////////////////////
(global-font-lock-mode t)               ; fontify all buffers
(setq font-lock-maximum-size 5000000)   ; Set 5Mb file size limit for fontification
;;(setq font-lock-maximum-decoration      ; fontification levels
;;      '((magik-mode . 3)                  ; For magik use level 3 instead of maximum.
;;	(t . t)))                         ; For all other modes use maximum level


;;;////////////////////////////////////////
;;; Other Useful packages
;;;////////////////////////////////////////
(require 'uncompress) ;; Auto decompression of .Z and .gz compressed files.

;;;////////////////////////////////////////
;;; Enable various commands that are disabled by default
;;;////////////////////////////////////////
(put 'narrow-to-region 'disabled nil)
(put 'rmail            'disabled nil)
(put 'upcase-region    'disabled nil)
(put 'eval-expression  'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'erase-buffer     'disabled nil)

;;;////////////////////////////////////////
;;; Disable unwanted commands
;;;////////////////////////////////////////
(put 'backward-kill-sentence 'disabled t)

;;;////////////////////////////////////////
;;; Gnuserv
;;;////////////////////////////////////////
(require 'gnuserv)
(setq gnuserv-frame (selected-frame))
(gnuserv-start)

;;;////////////////////////////////////////
;;; Printing
;;;////////////////////////////////////////
;;(if (eq system-type 'windows-nt)
;;    (setq ; Windows configuration
;;     lpr-command ""
;;     lpr-switches nil
;;     printer-name "\\\\COMPUTER\\WINDOWS PRINTER NAME")
;;  (setq ; Unix configuration
;;   lpr-command "lp"
;;   lpr-switches '("-dPRINTER")
;;   printer-name ""))

;;;////////////////////////////////////////
;;; My Abbreviations
;;;////////////////////////////////////////
(setq-default abbrev-mode t)            ; enable global abbreviations

;;(define-abbrev global-abbrev-table "das"      "c:/Documents and Settings/")

;;;////////////////////////////////////////
;;; Source Management configuration
;;;////////////////////////////////////////
(define-key global-map "\C-x\C-q" 'vc-toggle-read-only) ;; Not setup in Emacs 22

;;;////////////////////////////////////////
;;; My Key Bindings
;;;////////////////////////////////////////
(global-set-key [home] 'beginning-of-buffer)      ; top of buffer
(global-set-key [end]  'end-of-buffer)            ; bottom of buffer

(defvar my-f5-map (make-sparse-keymap))
(fset 'my-f5-map  my-f5-map)
(define-key my-f5-map "w" 'compare-windows)
(define-key my-f5-map "b" 'bury-buffer)

;;;////////////////////////////////////////
;;; Make numbered backups
;;;////////////////////////////////////////
;;(setq version-control t
;;      kept-old-versions 0
;;      kept-new-versions 2
;;      delete-old-versions t)

;;;////////////////////////////////////////
;;; Desktop: saves Emacs sessions
;;;////////////////////////////////////////
;; Must be one of the last things in the .emacs file.
(require 'desktop)
(desktop-load-default)
(setq desktop-dirname "~")
(setq desktop-basefilename (concat "emacs.desktop." system-name))
;;(setq desktop-missing-file-warning nil)
(add-to-list 'desktop-globals-to-save 'file-name-history)
(desktop-read)

