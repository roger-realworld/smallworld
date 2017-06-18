;;; sw-autoload: autoload certain SW libraries on demand
;;
;; Consider using update-autoloads-from-directories to generate this file.
;;
;; Some SW functions reside in libraries that can have their loading deferred
;; until the user actually uses the function.
;;
;; This file is typically not byte-compiled since it only contains autoload commands
;; and additions to auto-mode-alist variables.

(defconst sw-autoload-version "$Revision: 1.2 $")
(require 'macros-sw)
(require 'resources)
(require 'misc-sw)
(require 'utils-sw)

;;; menus and keys
(require 'menu-sw)
(require 'swkeys)

;;;MPDE
;;register mpde-install as an autoload function
(autoload 'mpde-install "load_sw_development"
  "Install the MPDE files, ~/.mpde & ~/mpde-boot.el locally and update .emacs to load ~/.mpde."
  t) ;The t indicates an interactive function.

;;; GIS Aliases file mode
(or (assoc "aliases$" auto-mode-alist)
    (push '("aliases$" . aliases-mode) auto-mode-alist))
(or (assoc "aliases.txt$" auto-mode-alist)
    (push '("aliases.txt$" . aliases-mode) auto-mode-alist))
 (autoload 'aliases-mode "aliases"
  "Major mode for editing Magik aliases files."
  t)

;;; CB
(autoload 'cb "cb"
  "Start or resume a Smallworld Class Browser."
  t)

;;; GIS
(autoload 'gis "gis"
  "Run a Gis process in a buffer."
  t)

;;; Magik
(or (assoc "\\.magik$" auto-mode-alist)
    (push '("\\.magik$" . magik-mode) auto-mode-alist))
(autoload 'magik-mode "aliases"
  "Major mode for editing Magik code."
  t)

;;; Magik deep-print functionality
(autoload 'deep-print "deep-print"
  "Interactively explore a data-structure.  Entry point to deep-print-mode"
  t)
(autoload 'deep-print-mode "deep-print"
  "Interactively explore a data-structure.  Entry point to deep-print-mode"
  t)

;;; Dev Tools
(autoload 'dev-tools-vsd-transmit-method "dev-tools"
  "Send the current method to Very Simple Debugger."
  t)
(autoload 'dev-tools-object-inspector "dev-tools"
  "Use the Dev Tools application to inspect the current object."
  t)

(autoload 'dev-tools-traceback-viewer "dev-tools"
  "Use the Dev Tools application to view the last tracebacks."
  t)

;;; Module mode
(or (assoc "module\\.def$" auto-mode-alist)
    (push '("module\\.def$" . module-mode) auto-mode-alist))
(autoload 'module-mode "module"
  "Major mode for editing Magik module.def files."
  t)

;;; Product mode
(or (assoc "product\\.def$" auto-mode-alist)
    (push '("product\\.def$" . product-mode) auto-mode-alist))
(autoload 'product-mode "product"
  "Major mode for editing Magik product.def files."
  t)

;;; Msg mode
(or (assoc "\\.msg$" auto-mode-alist)
    (push '("\\.msg$" . msg-mode) auto-mode-alist))
(or (assoc "\\.hmsg$" auto-mode-alist)
    (push '("\\.hmsg$" . msg-mode) auto-mode-alist))
(autoload 'msg-mode "msg"
  "Major mode for editing Magik Message files."
  t)

;;; Load_list.txt
(or (assoc "load_list\\.txt$" auto-mode-alist)
    (push '("load_list\\.txt$" . loadlist-mode) auto-mode-alist))
(autoload 'loadlist-mode "loadlist"
  "Major mode for editing Magik load_list.txt files."
  t)

;;; gis-version
(autoload 'gis-version-selection "gis-version"
  "Major mode interface for changing the GIS session environment."
  t)

;;; auto-gis
;; Avoid the need to add -load auto-gis when -f auto-gis is on the command line
;; Required to stop Emacs barfing on unknown command line options that must
;; be given before -f auto-gis.
(if (and (boundp 'command-line-functions)
	 (member "auto-gis" command-line-args))
    (add-to-list 'command-line-functions 'auto-gis-command-line-parser))
(autoload 'auto-gis-command-line-parser "auto-gis" nil t)
(autoload 'auto-gis "auto-gis" nil t)

;;; Dynamic TODO mode
(autoload 'todo-dynamic-mode "todo-dynamic"
  "Toggle TODO mode in file buffers.
Display in a dynamic *TODO* buffer lines matching the regexp
in `todo-dynamic-regexp'."
  t) 

;;; swdoc
(or (assoc "\\.idoc$" auto-mode-alist)
    (push '("\\.idoc$" . sw:doc-mode) auto-mode-alist))
(autoload 'sw:doc-mode "swdoc" nil t)

(provide 'sw-autoload)
