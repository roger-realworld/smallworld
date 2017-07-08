;;; todo-dynamic-mode.el. 'dynamic' minor mode for listing regexp in file buffers e.g. TODO

;; This is implemented as a specialised interface on Occur mode.

(require 'cl)

(defconst todo-dynamic-version "$Revision: 1.1 $")

(defgroup todo-dynamic nil
  "Dynamic *TODO* temporary buffer display. Displays all matches of `todo-dynamic-regexp'."
  :group 'convenience
  :group 'smallworld)

(defcustom todo-dynamic-regexp "TODO\\|FIXME"
  "Regexp string to match lines to display in the *TODO* buffer."
  :group 'todo-dynamic
  :type  'regexp)

(defcustom todo-dynamic-buffer-function 'todo-dynamic-buffer-default-p
  "Default function to test whether current buffer should display *TODO* buffer."
  :group 'todo-dynamic
  :type  'function)

(defcustom todo-dynamic-buffer-auto-refresh-p t
  "If t will continually ensure *TODO* buffer is refreshed.
If nil, then *TODO* buffer is only refreshed when a new file is opened."
  :group 'todo-dynamic
  :type  'boolean)

(defcustom todo-dynamic-case-fold-search nil
  "If nil, the search using `todo-dynamic-regexp' ignores case."
  :group 'todo-dynamic
  :type  'boolean)

(defcustom todo-dynamic-buffer-max-height nil
  "Maximum height of windows displaying *TODO* buffer.
If nil, defaults to `temp-buffer-max-height'."
  :group 'todo-dynamic
  :type '(choice integer (const nil)))

;; Not under defcustom because no widget to define a frame
(defvar todo-dynamic-frame nil
  "*Name of frame to use for TODO buffer.")

(defcustom todo-dynamic-frame-parameters-default
  (list
   '(name . "TODO")
   ;;'(unsplittable . t)
   '(minibuffer . nil)
   '(user-position . t)	      ; Emacs only
   ;;'(vertical-scroll-bars . nil)  ; Emacs only
   ;;'(scrollbar-width . 0)         ; XEmacs only
   '(menu-bar-lines . 0)          ; Emacs only
   '(tool-bar-lines . 0)          ; Emacs 21+ only
   ;; don't lower nor auto-raise
   '(auto-lower . nil)
   '(auto-raise . nil)
   '(visibility . t)

   ;; this blocks queries from  window manager as to where to put
   ;; the frame. we put the frame outside the display,
   ;; so the initial frame won't jump all over the screen
   '(top . 1) '(left . 1)
   )
  "Frame parameters for displaying TODO frame."
  :group 'todo-dynamic
  :type  '(repeat cons))

(defvar todo-dynamic-mode-line-string nil
  "Mode line entry when mode is in operation.")
(make-variable-buffer-local 'todo-dynamic-mode-line-string)

(defvar todo-dynamic-buffer-mode-line '("     %b  {"
					(:eval
					 (if occur-buffer
					     (buffer-name occur-buffer)
					   ""))
					"}")
  "*Mode line to use for the actual *TODO* buffer.")

(defcustom todo-dynamic-idle-time 2
  "Idle time to wait before the *TODO* may be displayed."
  :group 'todo-dynamic
  :type 'integer)

(defvar todo-dynamic-timer nil
  "Variable storing idle timer object.")

(defvar todo-dynamic-buffer nil
  "Store the buffer the *TODO* buffer last operated on.")

(defun todo-dynamic-buffer-default-p ()
  "Return t if current buffer should display *TODO* buffer.
See `todo-dynamic-buffer-auto-refresh-p' for additional control."
  (if buffer-file-name
      (if (eq todo-dynamic-buffer (current-buffer))
	  todo-dynamic-buffer-auto-refresh-p
	t)))

(defun todo-dynamic-set-frame-size ()
  "Set the dimensions of the TODO Frame."
  (let ((cols (frame-width))
	(rows (cond ((integerp todo-dynamic-buffer-max-height)
		       todo-dynamic-buffer-max-height)
		      ((functionp todo-dynamic-buffer-max-height)
		       (funcall todo-dynamic-buffer-max-height (current-buffer)))
		      ((integerp temp-buffer-max-height)
		       temp-buffer-max-height)
		      ((functionp temp-buffer-max-height)
		       (funcall temp-buffer-max-height (current-buffer)))
		      (t 4))))
    (set-frame-size todo-dynamic-frame cols rows)))

(defun todo-dynamic-get-make-frame ()
  "Create new TODO Frame unless frame already exists."
  (if (frame-live-p todo-dynamic-frame)
      todo-dynamic-frame
    (setq todo-dynamic-frame
	  (make-frame todo-dynamic-frame-parameters-default))
    (todo-dynamic-set-frame-size)))

(defun todo-dynamic-select-frame ()
  "Update the TODO frame.
No frame is raised so that focus is preserved."
  (let ((f (selected-frame))
	(buf (get-buffer "*TODO*")))
    (todo-dynamic-get-make-frame)
    (select-frame todo-dynamic-frame)
    (if (equal (current-buffer) "*TODO*")
	nil
      (set-window-dedicated-p (get-buffer-window buf) nil)
      (switch-to-buffer "*TODO*")
      (set-window-dedicated-p (get-buffer-window buf) t))
    (select-frame f)))

(defun todo-dynamic-buffer-p ()
  "Return t, if buffer is suitable for listing TODO items.
Minibuffer is explicitly ignored for *TODO* buffer,
If the *TODO* buffer is in a visible window then it is refreshed
when Emacs is idle for `todo-dynamic-idle-time' seconds.
Otherwise run the function given by `todo-dynamic-buffer-function'
to determine whether to display the *TODO* buffer."
  (cond ((active-minibuffer-window)     nil)
	((equal (buffer-name (current-buffer)) "*TODO*") nil)
	((eq this-command 'todo-dynamic-buffer)	 t)
	((get-buffer-window "*TODO*") t)
	(t
	 (funcall todo-dynamic-buffer-function))))

(defun todo-dynamic-buffer ()
  "Display TODO lines in current buffer.
When called interactively it will force a refresh."
  (interactive)
  (if (todo-dynamic-buffer-p)
      (unwind-protect
	  (save-window-excursion
	    (save-excursion
	      (setq todo-dynamic-mode-line-string " TODO")
	      (setq todo-dynamic-buffer (current-buffer))
	      ;; protect user's use of existing *Occur* buffer.
	      ;; and reuse any existing *TODO* buffer.
	      (save-excursion
		(when (get-buffer "*Occur*")
		  (set-buffer "*Occur*")
		  (rename-buffer "  *Occur*"))
		(if (get-buffer "*TODO*")
		    (let ((win (get-buffer-window (get-buffer "*TODO*")
						  (and (frame-live-p todo-dynamic-frame) todo-dynamic-frame))))
		      (set-buffer "*TODO*")
		      (and win (set-window-dedicated-p win nil))
		      (rename-buffer "*Occur*"))))
	      (save-restriction
		(widen)
		(goto-char (point-min))
		(save-match-data
		  (let ((case-fold-search todo-dynamic-case-fold-search)
			(temp-buffer-resize-mode t)
			(temp-buffer-show-hook temp-buffer-show-hook)
			(temp-buffer-max-height (or todo-dynamic-buffer-max-height
						    temp-buffer-max-height)))
		    (if (not (re-search-forward todo-dynamic-regexp nil t))
			(and (get-buffer "*Occur*") (kill-buffer "*Occur*"))
		      (save-excursion
			(add-hook 'temp-buffer-show-hook 'resize-temp-buffer-window)
			(occur todo-dynamic-regexp)
			(set-buffer "*Occur*")
			(rename-buffer "*TODO*"))))))
	      (set-buffer (get-buffer-create "*TODO*"))
	      (setq mode-line-format todo-dynamic-buffer-mode-line)
	      (todo-dynamic-select-frame)))
	(if (get-buffer "  *Occur*")
	    (save-excursion ;protect user's use of existing *Occur* buffer.
	      (set-buffer "  *Occur*")
	      (rename-buffer "*Occur*"))))))

(defun todo-dynamic-mode (arg)
  "Toggle TODO mode in file buffers.
Display in a *TODO* temporary buffer lines matching the regexp
in `todo-dynamic-regexp'."
  (interactive "P")
  (let ((mode (if (null arg)
	  (not todo-dynamic-timer)
	(> (prefix-numeric-value arg) 0))))
    (if todo-dynamic-timer
	(progn
	  (cancel-timer todo-dynamic-timer)
	  (setq todo-dynamic-timer  nil
		todo-dynamic-buffer nil)))
    (if mode
	(setq todo-dynamic-timer (run-with-idle-timer
				  todo-dynamic-idle-time t 'todo-dynamic-buffer)))
    (message "Dynamic TODO Mode is %s" (if mode "on" "off")))
  (force-mode-line-update))

(or (assq 'todo-dynamic-timer minor-mode-alist)
    (push '(todo-dynamic-timer todo-dynamic-mode-line-string) minor-mode-alist))

(provide 'todo-dynamic)
