;; The Smallworld GNU Emacs default configuration file, `default.el'.
;; --------------------------------------------------------------------

;;Skeleton-pair mode
(require 'skeleton)
(setq skeleton-pair-on-word nil)  ;; don't apply skeleton trick in front of a word.
(global-set-key (kbd "(")  'skeleton-pair-insert-maybe)
(global-set-key (kbd "[")  'skeleton-pair-insert-maybe)
(global-set-key (kbd "{")  'skeleton-pair-insert-maybe)
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
(defun skeleton-pair-mode (&optional arg)
  "Toggle value of `skeleton-pair'.
Primarily used for keys that use `skeleton-pair-insert-maybe'
for example: [, (, { and \".
"
  (interactive "P")
  (setq skeleton-pair (not skeleton-pair)))

;; Load and configure Smallworld Emacs functionality
;; including standard sw_defaults.el
(load "require_sw_defs")
(require 'sw_defaults)
;;TODO: to get this to be truly loadable on demand without require_sw_def.el,
;;      we will need to move local mode define-keys from swkeys.el
;;      to their respective elisp file.
;;(require 'sw-autoload)
;;(defvar sw-set-keys nil)
(or sw-set-keys (sw-set-keys))

(autoload 'global-highline-mode "highline" "Highlights current line." t)

;; dedicated window buffer
(require 'dedicated)
(if (boundp 'tool-bar-map)
    (let* ((image-on  (find-image (list '(:type xpm :file "dedicated_on.xpm"))))
	   (image-off (find-image (list '(:type xpm :file "dedicated_off.xpm"))))
	   (image-vec (vector image-on image-off image-on image-off)))
      (define-key-after tool-bar-map [dedicated-mode]
	`(menu-item "dedicated-mode" dedicated-mode
		    :image ,image-vec
		    :selected dedicated-mode
		    :help "Lock/UnLock Current buffer in window"))))
;(tool-bar-add-item nil 'dedicated-mode 'dedicated-mode
;		   :image ["dedicated_on" "dedicated_on" "dedicated_off" "dedicated_off"]
;		   :help "Lock/UnLock Current buffer in window"
;		   :visible t)

(autoload 'msmeller "msmeller" "Code smell functionality for Magik code" t)

(autoload 'cmd-mode "cmd-mode" "Windows Batch file editor for .bat and .cmd files." t)
(setq auto-mode-alist (append '(("\\.\\(cmd\\|bat\\)$" . cmd-mode))
                              auto-mode-alist))
(autoload 'bc-set               "breadcrumb" "Set bookmark in current point."   t)
(autoload 'bc-previous          "breadcrumb" "Go to previous bookmark."         t)
(autoload 'bc-next              "breadcrumb" "Go to next bookmark."             t)
(autoload 'bc-local-previous    "breadcrumb" "Go to prev bookmark."             t)
(autoload 'bc-local-next        "breadcrumb" "Go to next local bookmark."       t)
(autoload 'bc-goto-current      "breadcrumb" "Go to the current bookmark."      t)
(autoload 'bc-list              "breadcrumb" "List all bookmarks in menu mode." t)
(autoload 'bc-clear             "breadcrumb" "Clear all bookmarks."             t)

(tool-bar-add-item "goto_line" 'goto-line 'goto-line
		   :help "Go To Line #")
(tool-bar-add-item "bookmark_set" 'bookmark-set 'bookmark-set
		   :help "Set Bookmark")
(tool-bar-add-item "bookmark_get" 'edit-bookmarks 'edit-bookmarks
		   :help "Edit/Get Bookmark")

(autoload 'setnu-mode "setnu+" "Shows line numbers in a buffer" t) ;Note: setnu+ requires setnu.
(tool-bar-add-item "line_numbers" 'setnu-mode 'setnu-mode
		   :help "Show/Hide Line Numbers in buffer")
(tool-bar-add-item "outline" 'outline-minor-mode 'outline-minor-mode
		   :help "Toggle Outline Minor Mode")

(autoload 'all "all"
  "Just like occur, except that changes in the *All* buffer is
propagated to the original buffer." t)
(tool-bar-add-item "all" 'all 'all
		   :help "Show All matching regexp...")
(if (boundp 'tool-bar-map)
    (let* ((image-on  (find-image (list '(:type xpm :file "hide_show_on.xpm"))))
	   (image-off (find-image (list '(:type xpm :file "hide_show_off.xpm"))))
	   (image-vec (vector image-on image-off image-on image-off)))
      (define-key-after tool-bar-map [hs-mode]
	`(menu-item "hs-mode" hs-org/minor-mode
		    :image ,image-vec
		    :visible (featurep 'hideshow-org)
		    :selected hs-org/minor-mode
		    :help "Toggle Hide/Show in buffer"))))
;(tool-bar-add-item nil 'hs-mode-on 'hs-mode-on
;		   :image ["hide_show_on" "hide_show_on" "hide_show_off" "hide_show_off"]
;		   :visible 'hs-org/minor-mode
;		   :help "Toggle Hide/Show")

(tool-bar-add-item "highlight" 'highlight-changes-mode 'highlight-changes-mode
		   :visible '(fboundp 'highlight-changes-mode)
		   :help "Toggle Highlight Changes")
(tool-bar-add-item "highlight_next" 'highlight-changes-next-change 'highlight-changes-next-change
		   :visible '(and (boundp 'highlight-changes-mode)
				  highlight-changes-mode)
		   :enable '(eq highlight-changes-mode 'active)
		   :help "Highlight Next Change")

(tool-bar-add-item "highlight_prev" 'highlight-changes-previous-change 'highlight-changes-previous-change
		   :visible '(and (boundp 'highlight-changes-mode)
				  highlight-changes-mode)
		   :enable '(eq highlight-changes-mode 'active)
		   :help "Previous Change")

(tool-bar-add-item (if (running-under-nt) "shell_windows" "shell_unix") 'shell 'shell
		   :help "Start a Shell in a buffer")

(defvar site-lisp-dir
  (file-name-directory (locate-library "site-start.el" t)))
(defvar site-lisp-features-dir
  (file-name-as-directory (concat site-lisp-dir "features")))
(defvar site-lisp-data-dir
  (file-name-as-directory (concat site-lisp-dir "data")))
;; Setup YASNIPPET
(let ((yas-load-path (locate-library "yasnippet.elc" t)))
  (if yas-load-path
      (progn
	(custom-initialize-set 'yas/global-mode t)
	(require 'yasnippet)
	(setq yas/use-menu 'abbreviate
	      yas/root-directory
	      (append
	       (list (concat site-lisp-data-dir "snippets"))
	       (if (stringp 'yas/root-directory)
		   (list yas/root-directory)
		 yas/root-directory)))
	(if yas/global-mode (yas/global-mode 1)))))
;;	(yas/reload-all)

;; Tabbar mode
(eval-if (locate-library "tabbar.elc" t)
    (custom-initialize-set 'tabbar-mode t)
    (require 'tabbar)
    (if tabbar-mode (tabbar-mode 1)))

;; setup ecb
(if (locate-library "ecb.elc" t)
    (require 'sw-ecb))

;; setup auto-complete
(let ((ac-load-path (locate-library "auto-complete-config.elc")))
  (if ac-load-path
      (progn
	(require 'auto-complete-config)
	(require 'pos-tip) ; for improved/reliable documentation help popups.
	(add-to-list 'ac-dictionary-directories (concat (file-name-directory ac-load-path) "dict") t)
	(ac-config-default))))

;; Incremental completion for Minibuffer
(custom-initialize-set 'icomplete-mode t)
(require 'icomplete)
(if icomplete-mode (icomplete-mode 1))

;; Load and configure other packages
;; Uncomment following line to have ClearCase package loaded automatically.
;;(require 'sw-clearcase)                   ; Loader for ClearCase package.

;;; Please read CUSTOMISATION_FAQ.txt

(provide 'default)
