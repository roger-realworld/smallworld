;;; menu-sw.el - set the Smallworld menus.

(eval-when-compile
  (require 'resources)
  (require 'aliases)
  (require 'sw-help))

(require 'easymenu)

(defcustom menu-sw-tools-submenu
  ;;not translating these as they are not Smallworld code
  '(
    ["Emacs Code Browser" ecb-minor-mode   (fboundp 'ecb-minor-mode)]
    ["Breadcrumbs list"   bc-list          (fboundp 'bc-list)]
    ["Command Shell"      shell            (fboundp 'shell)]
    ["Imenu"              imenu            (fboundp 'imenu)]
    ["Occur"              occur            (fboundp 'occur)]
    ["All"                all              (fboundp 'all)]
    "---"
    ["Speedbar" speedbar
     :active (fboundp 'speedbar)
     :style toggle
     :selected speedbar-frame
     :help "Toggle display of Speedbar frame"]
    ;; "Dynamic TODO mode" is actually written by Andrew but is too
    ;; general to be classified as a Smallworld extension.
    ["Dynamic TODO Mode" todo-dynamic-mode
     :active (fboundp 'todo-dynamic-mode)
     :style toggle
     :selected (and (boundp 'todo-dynamic-timer)
		    todo-dynamic-timer)
     :help "Toggle display of dynamic *TODO* buffer"]
    ["Global YASnippet mode" yas/global-mode
     :active (fboundp 'yas/global-mode)
     :style toggle
     :selected (and (boundp 'yas/global-mode)
		    yas/global-mode)
     :help "Toggle Font-Lock mode in every buffer"]
    ["Global Font Lock" global-font-lock-mode
     :active (fboundp 'global-font-lock-mode)
     :style toggle
     :selected (and (boundp 'global-font-lock-mode)
		    global-font-lock-mode)
     :help "Toggle Font-Lock mode in every buffer"]
    ["Global Highlight Current Line mode" global-highline-mode
     :active (fboundp 'global-highline-mode)
     :style toggle
     :selected (and (boundp 'global-highline-mode) global-highline-mode)
     :help "Toggle icomplete mode, ."]
    ["Skeleton Pair mode"  skeleton-pair-mode
     :active (fboundp 'skeleton-pair-mode)
     :style toggle
     :selected (and (boundp 'skeleton-pair)
		    (symbol-value 'skeleton-pair))
     :help "Toggle Skeleton Pair mode"]
    ["Auto-Complete Mode" auto-complete-mode
     :active (fboundp 'auto-complete-mode)
     :style toggle
     :selected (and (boundp 'auto-complete-mode)
		    auto-complete-mode)
     :help "Toggle Auto-Complete Mode"]
    ["More Structured Buffers" msb-mode
     :active (fboundp 'msb-mode)
     :style toggle
     :selected (and (boundp 'msb-mode) msb-mode)
     :help "Toggle Msb mode, changing the Buffer menu structure."]
    ["Minibuffer Incremental Complete mode" icomplete-mode
     :active (fboundp 'icomplete-mode)
     :style toggle
     :selected (and (boundp 'icomplete-mode) icomplete-mode)
     :help "Toggle icomplete mode, ."]
    ["Show Trailing Whitespace"  toggle-show-trailing-whitespace
     :active (boundp 'show-trailing-whitespace)
     :style toggle
     :selected (and (boundp 'show-trailing-whitespace)
		    (symbol-value 'show-trailing-whitespace))
     :help "Toggle Show Trailing Whitespace mode"]
    "---"
    ["Highlight Changes Mode: Active" (highlight-changes-mode 1)
     :active (fboundp 'highlight-changes-mode)
     :style radio
     :selected (and (boundp 'highlight-changes-mode)
		    (eq highlight-changes-mode 'active))
     :help "Active Highlight Changes Mode"]
    ["Highlight Changes Mode: Passive" (highlight-changes-mode 0)
     :active (fboundp 'highlight-changes-mode)
     :style radio
     :selected (and (boundp 'highlight-changes-mode)
		    (eq highlight-changes-mode 'passive))
     :help "Passive Highlight Changes Mode"]
    ["Highlight Changes Mode: Off" (highlight-changes-mode -1)
     :active (fboundp 'highlight-changes-mode)
     :style radio
     :selected (and (boundp 'highlight-changes-mode)
		    (null highlight-changes-mode))
     :help "Disable Highlight Changes Mode"]
    ["HCM: Rotate Faces" highlight-changes-rotate-faces
     :active (fboundp 'highlight-changes-rotate-faces)
     :help "Change the faces used by Highlight Changes Mode, recording the age of older changes."]
    ["HCM: Goto Previous Change" highlight-changes-previous-change
     :active (fboundp 'highlight-changes-previous-change)
     :help "Move to previous marked change."]
    ["HCM: Goto Next Change" highlight-changes-next-change
     :active (fboundp 'highlight-changes-next-change)
     :help "Move to next marked change."]
    )
  "List of Menu entries for SW->Tools submenu."
  :group 'smallworld
  :type  'sexp)

(defconst menu-sw-main-menu
  `(,resources-menu-sw
    (,resources-menu-sw-alias-files)
    "---"
    [,resources-menu-sw-gis-select    gis-version-selection
				      :active t
				      :keys "f2 s"]
    [,resources-menu-sw-run-gis       gis
				      :active t
				      :keys "f2 z"]
    [,resources-menu-sw-run-new-gis   gis-new-buffer
				      :active t
				      :keys "C-u f2 z"]
    (,resources-menu-sw-gis-procs)
    "---"
    (,resources-menu-sw-cb)
    (,resources-menu-sw-cb-procs)
    "---"
    (,resources-menu-sw-shell-procs)
    [,resources-menu-sw-list-procs    list-processes t]
    "---"
    ,(append (list resources-menu-sw-tools) menu-sw-tools-submenu)
    (,resources-menu-sw-customize
     [,resources-menu-sw-customize-smallworld sw-customize
					      :active t] ;; :key-sequence nil
     "---"
     [,(format resources-menu-sw-help-view-file "sw_defaults.el") sw-help-view-swdefaults
      :active t]
     [,(format resources-menu-sw-help-view-file "occasional.emacs") sw-help-view-occasional
      :active t]
     [,(format resources-menu-sw-help-view-file "developer.emacs")  sw-help-view-developer
      :active t])
    (,resources-menu-sw-help)))

(defconst menu-sw-cb-menu
  `(,resources-menu-sw-cb
    [,resources-cb-menu-start        cb
				     :active t
				     :keys "f3 f3"]
    [,resources-cb-menu-start-new-cb cb-new-buffer
				     :active t
				     :keys "C-u f3 f3"]
    [,resources-cb-menu-start-method cb-paste-method
				     :active t
				     :keys "f3 m"]
    [,resources-cb-menu-start-class  cb-paste-class
				     :active t
				     :keys "f3 c"]
    [,resources-cb-menu-start-m-c    cb-and-clear
				     :active t
				     :keys "f3 /"]
    "---"                        
    [,resources-cb-menu-jump         cb-jump-to-source
				     :active t
				     :keys "f3 j"]
    "---"                        
    [,resources-menu-sw-customize    cb-customize
				     :active t] ;; :key-sequence nil
    [,resources-menu-sw-help         cb-help
				     :active t
				     :keys "f3 ?"]))

(defconst menu-sw-dev-menu
  `(,resources-menu-sw-dev
    [,resources-menu-sw-dev-get-bug    magik-patch-bug-get
				       :active t
				       :keys "f2 j"]
    [,resources-menu-sw-dev-make-patch magik-patch-make
				       :active t
				       :keys "f2 o"]
    [,resources-menu-sw-dev-make-cn    magik-patch-make-change-note
				       :active t
				       :keys "f2 d"]
    [,resources-menu-sw-dev-make-unit-test  magik-patch-make-unit-test
				       :active t
				       :keys "f2 u"]
    "---"
    [,resources-menu-sw-dev-submit     magik-patch-submit
				       :active magik-patch-mode
				       :keys "f2 i"]
    [,resources-menu-sw-dev-review     magik-patch-review
				       :active magik-patch-mode
				       ]
    [,resources-menu-sw-dev-query      magik-patch-query
				       :active magik-patch-mode
				       ]
    [,resources-menu-sw-dev-approve    magik-patch-approve
				       :active magik-patch-mode
				       ]
    [,resources-menu-sw-dev-unsubmit   magik-patch-unsubmit
				       :active magik-patch-mode
				       ]
    [,resources-menu-sw-dev-unreview   magik-patch-unreview
				       :active magik-patch-mode
				       ]
    [,resources-menu-sw-dev-reject     magik-patch-reject
				       :active magik-patch-mode
				       ]
    [,resources-menu-sw-dev-resurrect  magik-patch-resurrect
				       :active magik-patch-mode
				       ]
    "---"
    [,resources-menu-sw-dev-help       magik-patch-help
				       :active t])) ;; :key-sequence nil

(defun menu-sw-set-menus ()
  "Setup main Smallworld menus."
  (easy-menu-change nil resources-menu-sw (cdr menu-sw-main-menu))
  (easy-menu-change (list resources-menu-sw)
		    resources-menu-sw-cb
		    (cdr menu-sw-cb-menu))
  (and (fboundp 'sw-help-update-submenu)
       (sw-help-update-submenu))

  ;; Due to a minor bug in easy-menu-change, have to set the "No Process" etc.
  ;; strings separately
  (easy-menu-change (list resources-menu-sw)
		    resources-menu-sw-gis-procs
		    (list resources-menu-sw-no-procs))
  (easy-menu-change (list resources-menu-sw)
		    resources-menu-sw-cb-procs
		    (list resources-menu-sw-no-procs))
  (easy-menu-change (list resources-menu-sw)
		    resources-menu-sw-shell-procs
		    (list resources-menu-sw-no-procs))
  
  (and (fboundp 'aliases-update-sw-menu)
       (aliases-update-sw-menu)))

(defun toggle-show-trailing-whitespace ()
  "Set the variable `show-trailing-whitespace'."
  (interactive)
  (setq show-trailing-whitespace
	(not show-trailing-whitespace)))

;;Add appropriate tool-bar items



(provide 'menu-sw)

;;; menu-sw.el ends here
