;;deep-print.el -- mode for examining Magik Object structure. 

(require 'resources)
(require 'utils-sw)

(defgroup deep-print nil
  "Customise Magik Object structure group."
  :group 'magik)

(defconst deep-print-version "$Revision: 1.3 $")

(defcustom deep-print-mode-hook nil
  "*Hook to run after deep-print mode is set."
  :group 'deep-print
  :type  'hook)

(defvar deep-print-mode-map (make-sparse-keymap)
  "Keymap for deep-print functionality")

(defvar deep-print-menu nil
  "Keymap for the deep-print buffer menu bar")

(easy-menu-define deep-print-menu deep-print-mode-map
  "Menu for deep-print mode."
  `(,resources-deep-print-menu
    [,resources-deep-print-menu-unfold     deep-print-unfold t]))

(defcustom deep-print-font-lock-keywords
  (list 
   '("^\\s-*\\(([^\)]+)\\)" 1 font-lock-type-face)
   '("^\\s-*\\(\\[:[^\]]+]\\)" 1 magik-font-lock-slot-face)
   '("\\(\\sw+:\\sw+\\)"  1 magik-font-lock-class-face)
   '("\\<:\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\sw*" . magik-font-lock-symbol-face)
   )
  "Font lock setting for deep-print fontification."
  :group 'deep-print
  :type 'sexp)

(defvar deep-print-gis-process nil
  "Gis process used by deep-print buffer.")

(defun deep-print-mode ()
  "Major mode for exploring a data-structure interactively."
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'deep-print-gis-process)

  (use-local-map deep-print-mode-map)
  (easy-menu-add deep-print-menu)

  (setq mode-name resources-deep-print-menu
	major-mode 'deep-print-mode
	font-lock-defaults
	'(deep-print-font-lock-keywords
	  nil t
	  (("_" . "w") ("|" . "$"))))

  (run-hooks 'deep-print-mode-hook))

;;TODO fix menu active logic
(defun deep-print (arg &optional buffer)
  "Interactively explore a data-structure.  Entry point to deep-print-mode.

With a prefix arg, ask user for GIS buffer to use."
  (interactive
   (let ((buffer (sw-get-buffer-mode (and (eq major-mode 'gis-mode) (buffer-name (current-buffer)))
				     'gis-mode
				     resources-gis-enter-buffer
				     gis-buffer
				     'gis-buffer-alist-prefix-function)))
     (barf-if-no-gis buffer)
     (list (sw-find-tag-tag (concat resources-deep-print-prompt ": ")) buffer)))
  (if (and (not (equal arg ""))
           (not (equal arg nil)))
      (let ((p (get-buffer-process buffer)))
        (or (and p (eq (process-status p) 'run))
	    (error resources-sw-no-process-error buffer))
	(pop-to-buffer (get-buffer-create (concat "*deep print*" buffer)))
	(deep-print-mode)
	(setq deep-print-gis-process p)
        (process-send-string
	 p
	 (format "emacs_interactive_deep_print(\n%s,\n\"%s\",\n4)\n$\n" arg arg)))))

(defun deep-print-mouse-unfold (click)
  "Unfold a level of the data-structure."
  (interactive "e")
  (mouse-set-point click)
  (deep-print-unfold))

(defun deep-print-unfold ()
  "Unfold a level of the data-structure."
  (interactive)
  (save-excursion
    (let* ((request-str "")
	   (col (progn (back-to-indentation) (current-column)))
	   (next-col-str (number-to-string (+ col 4))))
      (while
          (progn
            (back-to-indentation)
            (cond
             ((looking-at "\\[:\\([^]]+\\)\\]")
              (setq request-str (concat
                                 "[\n"
                                 "symbol.intern(\"" (match-string 1) "\")"
                                 "]"
                                 request-str)))
             ((looking-at "\\[\\([0-9]+\\)\\]")
              (setq request-str (concat
                                 "[\n"
                                 (match-string 1)
                                 "]"
                                 request-str)))
             ((looking-at "(\\([0-9]+\\))")
              (setq request-str (concat
                                 ".\nsys!at0("
                                 (number-to-string (1- (string-to-number (match-string 1))))
                                 ")"
                                 request-str)))
             ((looking-at "(\\([^)]+\\))")
              (setq request-str (concat
                                 ".\nsys!slot(:"
                                 (match-string 1)
                                 ")"
                                 request-str)))
             ((zerop col)
              (forward-line -1)
              (setq request-str (concat
                                 (buffer-substring (point-bol) (point-eol))
                                 request-str)))
             (t
              (error resources-deep-print-unfold-limit-error)))
            (setq col (- col 4))
            (>= col 0))
        (re-search-backward (concat "^" (make-string col ? ) "[^ ]")))
      (process-send-string
       deep-print-gis-process
       (format "emacs_interactive_deep_print((q << %s),\n \"\", \n%s)\n$\n"
	       request-str next-col-str)))))

(provide 'deep-print)
