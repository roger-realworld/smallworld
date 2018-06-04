;;; sw_defaults.el -- Smallworld customisations.

;;; Commentary:

;; This file gets loaded by the standard Emacs Smallworld `.emacs' file.

;; The Smallworld extensions to Emacs have been designed to complement
;; rather than replace the existing Emacs functionality - most of the
;; standard Emacs features still work in the standard way.  The
;; standard features that have been changed are collected here so that
;; you can see them, and maybe override them in your own `~/.emacs' file.

;; The sure way to find out whether something is standard is to try it
;; in a bare Emacs, which you can start at any time by typing:
;;     `emacs --no-site-file --no-init-file'
;; at a shell prompt.

;; Appended to the bottom of this file are lots of examples of how to
;; change Emacs key-bindings, and also how to add abbrevs.

;;; Code:

(eval-and-compile (require 'cl))
(require 'macros-sw)
(require 'utils-sw)
(require 'eieio-core)
(require 'swkeys)

(defconst sw_defaults-version "$Revision: 1.49 $")

(defvar x-pointer-left-ptr)

;;   C H A N G I N G   K E Y S
;;   -------------------------

;; Make `C-z' in a native GUI emacs run a shell, instead of
;; iconifying a frame.

(if window-system
    (global-set-key "\C-z" 'shell))


;; Make `M-,' find the next tag.

(global-set-key "\M-,"
                '(lambda () (interactive)
                   (switch-to-buffer (find-tag-noselect nil t))))


;; Add an extra binding, control+DEL, for "undo".

(sw-global-set-key [C-delete] 'undo)


;; Make shift+RET work like RET.

(sw-global-set-key [S-return] [return])


;; Make emacs verify if you really want to kill a buffer containing
;; a running process.

(defun check-buffer-has-no-process ()
  "Return non-nil if there is no process in the current buffer.
Or if the user confirms that it is ok to kill the buffer anyway."
  (let* ((proc (get-buffer-process (current-buffer)))
	 (filter (and proc (process-filter proc)))
	 (response t))
    (if proc
	(setq response
	      (yes-or-no-p
	       (format "Buffer %s has subprocess(es) running; kill anyway? "
		       (current-buffer)))))
    (if response
	(progn
	  (and filter (set-process-filter proc nil))
	  (and proc (kill-process proc))))
    response))

(setq kill-buffer-query-functions
      (adjoin 'check-buffer-has-no-process
	      kill-buffer-query-functions))


;;   M O U S E   D R A G G I N G
;;   ---------------------------

;; "Drag" buffers and modelines up and down.  In Emacs-18, we bound
;; this to the left mouse button but in later version of Emacs we have
;; switched to the middle button because the left button is already in
;; use.  In XEmacs we are not implementing this feature.  (The
;; modeline dragging is already implemented in XEmacs and is bound to
;; the left button).

(global-set-key [drag-mouse-2] 'mouse-scroll-screen)
(global-set-key [mode-line drag-mouse-2]  'mode-line-resize)

;; So that people don't inadvertantly try and drag the modeline with
;; the left mouse button, we define an action for drag-mouse-1:

;; Note for Emacs 21, dragging with mouse-1 is already setup and is
;; actually still enabled so for some reason the following code is ignored...
(defun drag-mouse-message ()
  "Drag Mouse message."
  (interactive)
  (message "To drag modelines or buffers up and down, use the middle button."))

(global-set-key [mode-line drag-mouse-1] 'drag-mouse-message))

;; In case the mouse doesn't have a middle button, we make
;; Control-Shift-left-button do whatever the middle button would do.

(sw-global-set-key [C-S-mouse-1] 'c-s-mouse-1)

(defun c-s-mouse-1 (e arg)
  "Do whatever the middle mouse button is meant to do on E with ARG."
  ;; To make this work we are assuming that the global mouse-2 action
  ;; has a prefix arg and the local mouse-2 actions don't.
  (interactive "e\nP")
  (let
      ((f (or (minor-mode-key-binding [mouse-2])
              (local-key-binding [mouse-2]))))
    (if f
        (funcall f e)
      (funcall (global-key-binding [mouse-2]) e arg))))

;; You can get rid of all this with:
;;
;; (global-set-key [drag-mouse-2] nil)
;; (global-set-key [mode-line drag-mouse-2] nil)
;; (global-set-key [mode-line drag-mouse-1] nil)
;; (global-set-key [C-S-mouse-1] nil)
;;
;; If you prefer you can use the left button, and override the normal
;; left button behaviour.
;;
;; (global-set-key [drag-mouse-1] 'mouse-scroll-screen)
;; (global-set-key [mode-line drag-mouse-1] 'mode-line-resize)
;;
;; Or use the shifted left button:
;;
;; (global-set-key [S-drag-mouse-1] 'mouse-scroll-screen)
;; (global-set-key [mode-line S-drag-mouse-1] 'mode-line-resize)




;;;   E N A B L I N G   S T A N D A R D   F E A T U R E S
;;    ---------------------------------------------------
;; Enable certain useful fetures for everyone.
;; Also lists a few useful features that you may find useful but are
;; currently commented out.

;;; paren mode
;; Enable the parenthesis highlighting feature

(require 'paren)
(and (fboundp 'show-paren-mode)
     (show-paren-mode t))
;; Toggle this feature with this command:
;;
;; (show-paren-mode)
;;
;; paren mode only checks parenthesis matching upto a certain character
;; limit. To change that limit use:
;;    (setq blink-matching-paren-distance 500000)

;;; line-number mode
;; Show the current line number in the modeline.
;;   (line-number-mode nil) ; turn off line-number mode.
;; You can change the maximum size of a buffer to perform line counting on,
;; defaults to buffers with upto 1,000,000 characters.
;;   (setq line-number-display-limit 10000000)

(line-number-mode t)

;;; Imenu mode
;; Enable the mode-specific buffer indexes and bind to Shift mouse-2.
;; S-mouse-2 conflict with Windows CUA-mode???

(defvar imenu-sort-function 'imenu--sort-by-name)
(require 'imenu)
(global-set-key [(shift mouse-2)] 'imenu)

;;An alternative interface for Imenu, make an Index Menu item in Magik mode.
;;(add-hook 'magik-mode-hook 'imenu-add-menubar-index)

;;; desktop feature
;; Saves partial status of Emacs when killed.
;; Make emacs start up with the same files loaded into buffers as you left it.
;; To enable it uncomment the require line and inform users how to enable it in
;; their .emacs file as described below.
;;
;; (require 'desktop)
;; To use desktop, users should put these two lines in the bottom of your .emacs
;; file (the later the better):
;;
;;	(desktop-load-default)
;;	(desktop-read)



;;; incremental search options
;; Make incremental search highlight the current match.
;; You can cancel this with:  (setq search-highlight nil)
(setq search-highlight t)

;;   I N T E R N A T I O N A L I S A T I O N
;;   ---------------------------------------


;;;;   M I S C E L L A N E O U S
;;;;   -------------------------

;;; SHELL
;;  -----
;;
;; Set command shell switch for common windows shells
;; such as cmd.exe
;;
;; If cmd.exe must be used then to avoid the warning messages that Emacs
;; will emit add:
;;     (remove-hook 'after-init-hook 'w32-check-shell-configuration)
;; either to your .emacs file or here

(if (string-match "cmd.exe" shell-file-name)
    (progn
	(setq shell-command-switch "/c")
	(and (boundp 'w32-quote-process-args)   (setq w32-quote-process-args nil))
	(and (boundp 'win32-quote-process-args) (setq win32-quote-process-args nil)))
  (setq shell-command-switch "-c"))

;;A useful shell hook is to check for password prompts to prevent typing in plain text
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

;;; C mode
;; At GNU Emacs 20.4 (and probably before) cc-mode became the
;; standard Emacs C mode.  cc-mode has different controls,
;; which we set here.

(eval-after-load 'cc-mode
  '(progn
     (c-initialize-cc-mode)
     (c-set-offset 'substatement 2 nil)
     (c-set-offset 'substatement-open 0 nil)
     (c-set-offset 'topmost-intro-cont 2 nil)
     (setq c-basic-offset 4)
     ;; the default is normally "gnu", but changes are made to the "user" style
     ;; to make personalised changes users should modify this to use an alist
     ;; see documentation on c-default-style for further details.
     (setq c-default-style "user")))


;; Make Emacs scroll in 4-line jumps rather than half-screen jumps.
;; You can get the default behaviour back again with:  (setq scroll-step 0)
(setq scroll-step 4)


;; To get the default behaviour back, restore `etags-file-of-tag'
;; like this:
;; 
;; (fset 'etags-file-of-tag
;;       (symbol-function 'etags-file-of-tag-orig))


;; Warn people in case they accidently get into the emacs rmail.

(put 'rmail 'disabled t)

;; Cancel this with:
;;
;; (put 'rmail 'disabled nil)


;; Make emacs use copying to create backups for files with multiple names.
;; This causes the alternate names to refer to the latest version as edited.

(setq backup-by-copying-when-linked t)

;; You can undo this with:
;;
;; (setq backup-by-copying-when-linked nil)



;;   C O S M E T I C   C H A N G E S
;;   -------------------------------

;; Make the mouse cursor into a blue arrow instead of a black bar,
;; and change the modeline to brown.  (We need to specify the modeline
;; foreground as white, to prevent it defaulting new frames to a black
;; foreground).

(require 'faces)
(if (running-under-x)
    (progn
      (setq x-pointer-shape x-pointer-left-ptr)
      (set-mouse-color "blue")
      (set-face-background 'modeline "brown")
      (set-face-foreground 'modeline "white")))

;; Undo all this with:
;;
;; (setq x-pointer-shape nil)
;; (if (running-under-x) (set-mouse-color "black"))
;; (if (running-under-x) (set-face-background 'modeline "black"))





;;          EXAMPLE KEY-BINDING CUSTOMISATIONS
;;          ==================================

;; You can add any of these lines to your `~/.emacs' file.

;; e.g. to make the delete key into a normal delete command
;;      rather than the default, delete-backward-char-untabify.
;; 
;;  (global-set-key [?\C-?] 'backward-delete-char)
;; 
;; 
;; e.g. to make the function key, F7, execute the macro command:
;; 
;;   (global-set-key [f7] 'call-last-kbd-macro)
;; 
;; 
;; or use shift+F7 instead.
;; 
;;   (global-set-key [S-f7] 'call-last-kbd-macro)
;; 
;; 
;; e.g. to make several commands start with F6.
;; 
;;   (global-set-key [f6] nil)
;;   (global-set-key [f6 ?g] 'goto-line)
;;   (global-set-key [f6 ?w] 'what-line)
;;   (global-set-key [f6 ?b] 'bury-buffer)
;; 
;; 
;; e.g. to make ESC C-b give you a buffer menu:
;; 
;;   (global-set-key [?\e ?\C-b] 'buffer-menu)
;; 
;; 
;; or to define ESC C-b only for magik-mode:
;; 
;;   (define-key magik-mode-map [?\e ?\C-b] 'buffer-menu)





;;       EXAMPLE ABBREVS
;;       ===============

;; To save a lot of typing, you can predefine abbreviations for the
;; long expressions that you commonly type.  For example, if you have
;; to type "scrapbook_collection_stream" a lot, you can add this to
;; your `.emacs' file:
;;
;;   (define-global-abbrev "scs" "scrapbook_collection_stream")
;;
;; to define the abbreviation, and:
;; 
;;   (setq-default abbrev-mode t)
;;
;; to turn abbrev mode on for all your buffers.

;;       OVERRIDING EXTERNAL LIBRARY SETTINGS
;;       ====================================

;; Make use of the eval-after-load construct to configure packages
;; after they are loaded. This removes/minimises any hardcoded changes from these packages
;; that we want to make.
;;
;; For example, in nXML the &nbsp; entity definition is missing, so instead of
;; copying the current setting from the package we just use an eval-after-load form
;; with some code to append the nbsp entity definition. Future versions of nXML may
;; fix this omission and the code below will not break or reduce functionality.

(eval-after-load "nxml-mode"
  '(progn
     (add-to-list 'xmltok-predefined-entity-alist '("nbsp" " " . " ") t)))


(provide 'sw_defaults)

;;; sw_defaults.el ends here
