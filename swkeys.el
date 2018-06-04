;;; swkeys.el -- bind all the SW keys, menus and mouse actions.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'resources)
  (require 'utils-sw)
  (require 'menu-sw)
  (require 'magik)
  (require 'gis)
  (require 'cb))

(defconst swkeys-version "$Revision: 1.34 $")

(defvar magik-patch-mode-map (make-sparse-keymap)
  "Keymap for the Minor mode that handles Magik Patch files")

(defvar deep-print-mode-map (make-sparse-keymap)
  "Keymap for deep-print functionality")

(defvar sw-f2-map (make-sparse-keymap)
  "Global Keymap for the F2 function key")

(defvar magik-f2-map (make-sparse-keymap)
  "Keymap for the F2 function key in Magik buffers")

(defvar gis-f2-map (make-sparse-keymap)
  "Keymap for the F2 function key in GIS command buffers")

(defvar sw-f3-map (make-sparse-keymap)
  "Global Keymap for the F3 function key")

(defvar cb-f3-map (make-sparse-keymap)
  "Keymap for the F3 function key in Class Browser buffers")

(defvar sw-f4-map (make-sparse-keymap)
  "Global Keymap for the F4 function key")

(defvar magik-f4-map (make-sparse-keymap)
  "Keymap for the F4 function key in Magik buffers")

(defvar gis-f4-map (make-sparse-keymap)
  "Keymap for the F4 function key in GIS command buffers")

(defvar sw-f5-map (make-sparse-keymap)
  "Global Keymap for the F5 function key")

(defvar sw-set-keys nil
  "Records whether \\[sw-set-keys] has been executed.
For transition to using default.el rather than .emacs to load code.")

(defun sw-customize ()
  "Open Customization buffer for Smallword Development."
  (interactive)
  (customize-group 'smallworld))

(defun sw-half-scroll-up ()
  (interactive)
  (scroll-up (/ (window-height (selected-window)) 2 )))

(defun sw-half-scroll-down ()
  (interactive)
  (scroll-down (/ (window-height (selected-window)) 2 )))

(defun sw-set-keys ()
  "Setup the default Smallworld key bindings."
  (setq sw-set-keys t)

  ;; ---------------------- top-level globals --------------------------

  (fset 'sw-f2-map    sw-f2-map)
  (fset 'magik-f2-map magik-f2-map)
  (fset 'gis-f2-map   gis-f2-map)

  (fset 'sw-f3-map    sw-f3-map)
  (fset 'cb-f3-map    cb-f3-map)

  (fset 'sw-f4-map    sw-f4-map)
  (fset 'magik-f4-map magik-f4-map)
  (fset 'gis-f4-map   gis-f4-map)

  (fset 'sw-f5-map    sw-f5-map)

  (global-set-key [f1]    'sw-help-info)
  (global-set-key [f2]    'sw-f2-map)
  (global-set-key [f3]    'sw-f3-map)
  (global-set-key [f4]    'sw-f4-map)
  (global-set-key [f5]    'sw-f5-map)

  (global-set-key [f6]    'magik-copy-method)
  (global-set-key [f7]    'magik-transmit-method)
  (global-set-key [f8]    'magik-transmit-region)
  (global-set-key [f9]    'magik-mark-method)
  (global-set-key [prior] 'sw-half-scroll-down)
  (global-set-key [next]  'sw-half-scroll-up)
  (global-set-key [f29]   'sw-half-scroll-down)
  (global-set-key [f35]   'sw-half-scroll-up)

  (global-set-key [f27]   'beginning-of-buffer)
  (global-set-key [f33]   'end-of-buffer)

  (global-set-key "\M-["   'auto-complete)
  (global-set-key "\M-]"   'ac-fuzzy-complete)

  ;; ------------------- F1 globals ----------------------

  ;;(db-set-keys)


  ;; ------------------- F2 globals ----------------------

  ;; these keys have to be defined in the local and global keymaps
  ;; because 19.24 and 19.25 won't display the key-board short-cuts
  ;; otherwise.

  (sw-multi-define-key (list sw-f2-map magik-f2-map) [f7]   'magik-transmit-method)
  (sw-multi-define-key (list sw-f2-map magik-f2-map) [f8]   'magik-transmit-region)
  (sw-multi-define-key (list sw-f2-map magik-f2-map) "\r"   'magik-transmit-thing)
  (sw-multi-define-key (list sw-f2-map magik-f2-map) "#"    'magik-comment-region)
  (sw-multi-define-key (list sw-f2-map magik-f2-map) "\e#"  'magik-uncomment-region)
  (sw-multi-define-key (list sw-f2-map magik-f2-map) "b"    'magik-transmit-buffer)
  (sw-multi-define-key (list sw-f2-map magik-f2-map) "h"    'magik-heading)
  (sw-multi-define-key (list sw-f2-map magik-f2-map) "m"    'magik-transmit-method)
  (sw-multi-define-key (list sw-f2-map magik-f2-map) "r"    'magik-transmit-region)
  (sw-multi-define-key (list sw-f2-map magik-f2-map) "q"    'fill-magik-public-comment)
  (sw-multi-define-key (list sw-f2-map magik-f2-map) "t"    'magik-trace-curr-statement)

  (sw-multi-define-key (list sw-f2-map gis-f2-map magik-f2-map) " "    'explicit-electric-magik-space)
  (sw-multi-define-key (list sw-f2-map gis-f2-map magik-f2-map) "x"    'deep-print)

  (define-key sw-f2-map [f1]   'sw-help-keys)
  (define-key sw-f2-map "["    'toggle-debug)
  (define-key sw-f2-map "\t"   'hippie-expand)
  (define-key sw-f2-map "e"    'electric-magik-mode)
  (define-key sw-f2-map "k"    'sw-reload-dotemacs)
  (define-key sw-f2-map "s"    'gis-version-selection)
  (define-key sw-f2-map "z"    'gis)

  ;; ------------------- F3 globals ----------------------

  (define-key sw-f3-map [f3]   'cb)
  (define-key sw-f3-map "c"    'cb-paste-class)
  (define-key sw-f3-map "j"    'cb-jump-to-source)
  (define-key sw-f3-map "m"    'cb-paste-method)
  (define-key sw-f3-map "/"    'cb-and-clear)
  (define-key sw-f3-map "?"    'cb-help)
  (define-key sw-f3-map "E"    'cb-execute-method-finder)

  ;; ------------------- F4 globals ----------------------

  ;;useful keys that Emacs does not normally bind.
  (define-key sw-f4-map "b"    'bury-buffer)
  (define-key sw-f4-map "g"    'goto-line)
  (define-key sw-f4-map "w"    'compare-windows)

  ;; ------------------- F5 globals ----------------------

  ;; Useful keys that Emacs does not normally bind.
  ;; F5 key for buffer locations, using bookmark and breadcrumb features
  ;; bookmarks use f5 + letter, breadcrumbs use f5 + non-letter keys.
  (define-key sw-f5-map "g"      'goto-line)
  (define-key sw-f5-map "s"      'bookmark-set)
  (define-key sw-f5-map "e"      'edit-bookmarks)
  (define-key sw-f5-map "d"      'bookmark-delete)
  (define-key sw-f5-map "w"      'bookmark-write)
  (define-key sw-f5-map "l"      'bookmark-load)
  (define-key sw-f5-map " "      'bc-set)
  (define-key sw-f5-map [up]     'bc-local-previous)
  (define-key sw-f5-map [down]   'bc-local-next)
  (define-key sw-f5-map [left]   'bc-previous)
  (define-key sw-f5-map [right]  'bc-next)
  (define-key sw-f5-map [delete] 'bc-goto-current)
  (define-key sw-f5-map [return] 'bc-list)

  ;; ---------------------- gis mode -------------------------

  (loop for i from ?  to ?~ do
        (define-key gis-mode-map (char-to-string i) 'gis-insert-char))

  (define-key gis-mode-map [f1]        'gis-help)
  (define-key gis-mode-map [f2]        'gis-f2-map)
  (define-key gis-mode-map [f4]        'gis-f4-map)
  (define-key gis-mode-map "\ep"       'recall-prev-gis-cmd)
  (define-key gis-mode-map "\en"       'recall-next-gis-cmd)
  (define-key gis-mode-map "\r"        'gis-newline)
  (define-key gis-mode-map " "         'gis-electric-magik-space)
  (define-key gis-mode-map "\C-?"      'gis-backward-delete-char)
  (define-key gis-mode-map "\C-a"      'gis-beginning-of-line)
  (define-key gis-mode-map "\C-d"      'gis-delete-char)
  (define-key gis-mode-map "\ed"       'gis-kill-word)
  (define-key gis-mode-map "\e\C-?"    'gis-backward-kill-word)
  (define-key gis-mode-map "\C-k"      'gis-kill-line)
  (define-key gis-mode-map "\C-w"      'gis-kill-region)
  (define-key gis-mode-map [f8]        'gis-send-command-at-point)
  (define-key gis-mode-map "\C-c\C-c"  'gis-kill-process)
  (define-key gis-mode-map "\C-c\C-\\" 'query-quit-shell-subjob)
  (define-key gis-mode-map "\C-c\C-z"  'query-stop-shell-subjob)
  (define-key gis-mode-map "\C-c\C-d"  'query-shell-send-eof)

  (define-key gis-f2-map [up]   'display-gis-history)
  (define-key gis-f2-map "\C-p" 'display-gis-history)
  (define-key gis-f2-map [down] 'undisplay-gis-history)
  (define-key gis-f2-map "\C-n" 'undisplay-gis-history)
  (define-key gis-f2-map "="    'gis-traceback-print)
  (define-key gis-f2-map "f"    'toggle-gis-filter)
  (define-key gis-f2-map "p"    'recall-prev-matching-gis-cmd)
  (define-key gis-f2-map "n"    'recall-next-matching-gis-cmd)

  (define-key gis-f4-map [f4]   'magik-symbol-complete)
  (define-key gis-f4-map [up]   'gis-traceback-up)
  (define-key gis-f4-map [down] 'gis-traceback-down)
  (define-key gis-f4-map "$"    'gis-shell)
  (define-key gis-f4-map "g"    'gis-error-goto)
  (define-key gis-f4-map "m"    'magik-copy-method-to-buffer)
  (define-key gis-f4-map "r"    'magik-copy-region-to-buffer)
  (define-key gis-f4-map "s"    'magik-add-debug-statement)
  (define-key gis-f4-map "w"    'magik-set-work-buffer)
  (define-key gis-f4-map "P"    'gis-traceback-print)
  (define-key gis-f4-map "S"    'gis-traceback-save)

  ;; ------------------------ magik mode -------------------------

  (define-key magik-mode-map [f1]    'magik-help)
  (define-key magik-mode-map [f2]    'magik-f2-map)
  (define-key magik-mode-map [f4]    'magik-f4-map)
  (define-key magik-mode-map "\r"    'magik-newline)
  (define-key magik-mode-map "\n"    'newline)
  (define-key magik-mode-map "\t"    'magik-indent-command)
  (define-key magik-mode-map " "     'electric-magik-space)
  (define-key magik-mode-map "#"     'electric-magik-hash)
  (define-key magik-mode-map "/"     'electric-pragma-/)
  (define-key magik-mode-map "\\"    'electric-pragma-back-/)

  (define-key magik-mode-map "\C-\M-h"   'magik-mark-method) ;standard key mapping
  (sw-define-key magik-mode-map [M-up]   'backward-method)
  (sw-define-key magik-mode-map [M-down] 'forward-method)
  
  (define-key magik-f2-map [up]   'backward-method)
  (define-key magik-f2-map [down] 'forward-method)
  (define-key magik-f2-map "$"    'magik-transmit-$-chunk)

  (define-key magik-f4-map [f4]   'magik-symbol-complete)
  (define-key magik-f4-map "c"    'magik-copy-method)
  (define-key magik-f4-map "e"    'magik-ediff-methods)
  (define-key magik-f4-map [f3]   'cb-magik-ediff-methods)
  (define-key magik-f4-map "m"    'magik-copy-method-to-buffer)
  (define-key magik-f4-map "n"    'magik-set-work-buffer-name)
  (define-key magik-f4-map "r"    'magik-copy-region-to-buffer)
  (define-key magik-f4-map "s"    'magik-add-debug-statement)
  (define-key magik-f4-map "w"    'magik-compare-methods)

  ;; --------------------- magik patch mode -----------------------

  (define-key magik-patch-mode-map "\r"    'magik-newline)
  (define-key magik-patch-mode-map "\n"    'newline)
  (define-key magik-patch-mode-map "\t"    'magik-patch-indent-command)
  (define-key magik-patch-mode-map " "     'magik-patch-electric-space)
  (define-key magik-patch-mode-map "#"     'electric-magik-hash)
  (define-key magik-patch-mode-map "/"     'magik-patch-electric-/)
  (define-key magik-patch-mode-map "\\"    'magik-patch-electric-back-/)

  ;;magik-patch-mode is a Minor mode
  (or (assoc 'magik-patch-mode minor-mode-map-alist)
      (push (cons 'magik-patch-mode magik-patch-mode-map) minor-mode-map-alist))

  ;; ------------------ deep print mode -----------------------------

  (define-key    deep-print-mode-map " "         'bury-buffer)
  (sw-define-key deep-print-mode-map [mouse-2]   'deep-print-mouse-unfold)


  ;; ----------------------- cb mode ------------------------

  (loop for i from ?  to ?~ do
        (define-key cb-mode-map (char-to-string i) 'cb-insert-command))

  (define-key cb-mode-map [f1]        'cb-help)
  (define-key cb-mode-map [f3]        'cb-f3-map)
  (define-key cb-mode-map [delete]    'cb-delete-char)
  (define-key cb-mode-map [backspace] 'cb-backward-delete-char)
  (define-key cb-mode-map "\C-k"      'cb-kill-line)
  (define-key cb-mode-map "\C-y"      'cb-yank)
  (define-key cb-mode-map "\ey"       'cb-yank-pop)
  (define-key cb-mode-map "\C-a"      'cb-beginning-of-line)
  (define-key cb-mode-map "\C-e"      'cb-end-of-line)
  (define-key cb-mode-map "\t"        'cb-tab)
  (define-key cb-mode-map " "         'cb-quit)
  (define-key cb-mode-map ";"         'cb-edit-topics-and-flags)
  (define-key cb-mode-map "/"         'cb-clear)
  (define-key cb-mode-map "\C-b"      'cb-backward-char)
  (define-key cb-mode-map "\C-f"      'cb-forward-char)

  (sw-define-key cb-mode-map [left]    'cb-backward-char)
  (sw-define-key cb-mode-map [right]   'cb-forward-char)
  (sw-define-key cb-mode-map [mouse-2] 'cb-mouse)

  (define-key cb-mode-map [mode-line mouse-1] 'cb-mode-line-click)
  (define-key cb-mode-map [mode-line mouse-2] 'cb-mode-line-click)

  (define-key cb-f3-map [up]   'cb-fold)
  (define-key cb-f3-map [down] 'cb-unfold)
  (define-key cb-f3-map "$"    'cb-gis-shell)
  (define-key cb-f3-map "F"    'cb-toggle-override-flags)
  (define-key cb-f3-map "T"    'cb-toggle-override-topics)
  (define-key cb-f3-map "2"    'cb-toggle-override-200-limit)
  (define-key cb-f3-map "f"    'cb-family)
  (define-key cb-f3-map "g"    'cb-gis)
  (define-key cb-f3-map "h"    'cb-quit)
  (define-key cb-f3-map "j"    'cb-jump-to-source)
  (define-key cb-f3-map "l"    'cb-next-inheritance-setting)
  (define-key cb-f3-map "r"    'cb-reset)
  (define-key cb-f3-map "o"    'cb-toggle-override-flags)
  (define-key cb-f3-map "s"    'cb-edit-topics-and-flags)
  (define-key cb-f3-map "t"    'cb-toggle-all-topics)

  (menu-sw-set-menus))

;; U T I L S
;; _________

;; Define for all the keymaps in KEYMAP-LIST the key KEY to be function FN.
(defun sw-multi-define-key (keymap-list key fn)
  (loop for keymap in keymap-list
        do
        (sw-define-key keymap key fn)))

;; Miscellaneous global keys
(sw-global-set-key [C-f2] 'magik-translate-old-vec-notation)

(provide 'swkeys)

;;; swkeys.el ends here
