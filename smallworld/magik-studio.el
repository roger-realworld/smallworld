;; Emacs interface to Magik Studio

(require 'resources)
(require 'magik)
(require 'gis-filter)
(require 'cb)

(defgroup magik-studio nil
  "Magik Studio Development Environment."
  :group 'smallworld)

(defcustom magik-studio-editor-version 22
  "The current instruction set that Magik Studio is using."
  :group 'magik-studio
  :type 'integer)

(defun magik-studio-gis-filter-go-command (proc str)
  "STR contains instructions separated by char ^C (#3).
The instructions are, in order:
action:       what to, this is either: file, definition, exemplar, global or block
                (note that the latter is not implemented yet)
raise:        Should we bring Emacs to the foreground ('t') or not
off-lines:    Offset in lines after we located to the sought position
off-colls:    Offset in columns after we located to the sought position
filename:     which file we want to open and search in
class-name:   name of the class we want to locate
method-name:  name of the method we want to locate
feedback-str: string we want to be displayed in the minibuffer"
  ;; get the file-name, offset, action, and args
  (save-match-data
    (let* ((lis          (split-string str (concat (char-to-string ?\^C) "+")))
	   (action       (first   lis))
	   (raise        (second  lis))
	   (off-lines    (string-to-number (third lis)))
	   (off-colls    (string-to-number (fourth lis)))
	   (filename     (cb-generalise-file-name (fifth lis)))
	   (class-name   (sixth   lis))
	   (method-name  (seventh lis))
	   (feedback-str (eighth  lis))
	   ;;(enable-recursive-minibuffers t)
	   (search-str))

      ;; set feedback
      (if (null feedback-str)
	  (message "Magik Studio %s: %s.%s in %s" action class-name method-name filename)
	(message feedback-str))

      ;; make sure we are in the right frame, etc.
      ;; do *not* jump into *gis* buffer if more than 2 windows/frames open
      ;; and one is reserved to just *gis*
      (cond ((and (> (length (frame-list)) 1)
		  (eq major-mode 'gis-mode))
	     (select-frame (next-frame)))
	    ((and (> (count-windows) 1)
		  (eq major-mode 'gis-mode))
	     (select-window (next-window)))
	    (t
	     nil))

      ;; jump to the right file
      (cond ((file-readable-p filename)
	     t)
	    ((string-match "[/\\]source[/\\]sys_core[/\\]" filename)
	     (error resources-cb-no-code-error class-name method-name))
	    (t
	     (error resources-cb-no-file-error filename)))
      (find-file filename)
      (goto-char (point-min))

      ;; carry out the action
      ;;INFO magik-goto-class-method now performs most of this functionality
      ;;     however, this function has only minimal changes to make it compatible
      ;;     with the new Emacs code.
      (cond ((equal "exemplar" action)
	     (setq search-str (concat "^def_[_a-z]*exemplar\\([ \t\n]*:" (regexp-quote class-name))))
	    ((equal "file" action)
	     (goto-line off-lines)
	     (move-to-column off-colls))
	    ((equal "global" action)
	     (setq search-str (concat (regexp-quote class-name) "[ \t]*<<")))
	    ((equal "definition" action)
	     (setq search-str (concat "^" (regexp-quote class-name) "\\.define_[_a-z]*\\([ \t\n]*:" (regexp-quote method-name))))
	    (t
	     nil))

      (cond ((equal "method" action)
	     ;;Use new Magik interface for searching for Methods.
	     (magik-goto-class-method method-name class-name))
	    ((equal search-str nil)
	     nil)
	    ((re-search-forward search-str nil t) ;;look for the search construct
	     (magik-goto-class-method-loop search-str action)
	     ;; do the offset in the file, bring Emacs to the front
	     (forward-line off-lines)
	     (move-to-column off-colls))
	    (t
	     nil))

      ;;Raise Emacs?
      (if (equal raise "t")
	  (raise-frame))

      ;; set feedback
      (if (null feedback-str)
	  (message "Magik Studio %s: %s.%s in %s" action class-name method-name filename)
	(error feedback-str))

      ;; somehow, the feedback message disappears, when something is evaluated in emacs and an magik parser error occurs
      ;; (message "parser error: %s" feedback-str))
      )))

(defun magik-studio-gis-filter-editor-version (proc str)
  "Write magik_studio.editor_version << {<version>, <editor_name>} on the gis prompt.
<version>:    the instruction set of magik_studio that is supported
<editor_name>: Either Emacs, Realmacs or Magician.

STR is the desired version and sets `magik-studio-editor-version'."
  
  (let* ((gis (process-buffer proc)))
    (barf-if-no-gis gis)
    ;; the desired version of supported commands
    (setq magik-studio-editor-version (string-to-number str))
    (save-excursion
      (process-send-string proc
			   (concat "magik_studio.set_editor_version(\"Emacs\", "
				   (number-to-string magik-studio-editor-version)
				   ")\n$\n")))))

;; Forcibly add the following actions
;; in case they have been reused despite the warning in gis-filter.el
(gis-filter-register-action "e" 'magik-studio-gis-filter-go-command     t)
(gis-filter-register-action "v" 'magik-studio-gis-filter-editor-version t)

(provide 'magik-studio)
