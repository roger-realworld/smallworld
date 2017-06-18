;;; load_sw_defs.el -- loads the Smallworld Lisp definitions

;;; Loading this file sets the defuns etc. but tries to avoid
;;; setting user preferences such as key-bindings.  The idea
;;; is that loading this file should not affect any of the
;;; standard Emacs behaviour.

(eval-when-compile (require 'macros-sw)
		   (require 'utils-sw))

(require 'mpde-boot) ;may have been loaded from ~/.mpde

(defconst load_sw_defs-version "$Revision: 1.37 $")

;; Low level utilities, standardisation between Emacsen
(load "macros-sw")
(if xemacs-p (load "sw-xemacs"))

(load "resources")
(load "misc-sw")
(load "utils-sw")

;; Standalone and helper packages for Smallworld Emacs
(load "sw-help")
(load "aliases")
(load "magik-template")
(load "indent-magik")
(load "pragma")

;; Main packages for Smallworld. These rely on one or more of the previous group
(load "magik")
(load "sw-electric")
(load "cb")
(load "gis")

;; High level Smallworld packages that rely on more than one of the main packages
(load "gis-filter")

;; Keymap and menu generation
(load "menu-sw")
(load "swkeys")

;;; Load Magik Patch development code. Either if MPDE already loaded or via mpde-install autoload.
;; Load development tools if MPDE is enabled.
(and (fboundp 'mpde-configurations-available-p)
     (mpde-configurations-available-p)
     (load "load_sw_development"))

(load "sw-autoload")
(load "sw-cus-load")

(provide 'load_sw_defs)

;;; load_sw_defs.el ends here





