;;; load_sw_defs.el -- loads the Smallworld Lisp definitions

;;; Loading this file sets the defuns etc. but tries to avoid
;;; setting user preferences such as key-bindings.  The idea
;;; is that loading this file should not affect any of the
;;; standard Emacs behaviour.

(eval-when-compile (require 'macros-sw)
		   (require 'utils-sw))

(defconst require_sw_defs-version "$Revision: 1.2 $")

(require 'mpde-boot) ;may have been loaded via ~/.mpde

;;; Low level utilities, standardisation between Emacsen
;;Reload macros-sw since it may have been required in old versions of site-start.el
(load "macros-sw")
(if xemacs-p (load "sw-xemacs"))

(require 'resources)
(require 'misc-sw)
(require 'utils-sw)

;;; Standalone and helper packages for Smallworld Emacs
(require 'sw-help)
(require 'aliases)
(require 'magik-template)
(require 'indent-magik)
(require 'pragma)

;;; Main packages for Smallworld. These rely on one or more of the previous group
(require 'magik)
(require 'sw-electric)
(require 'cb)
(require 'gis)

;;; High level Smallworld packages that rely on more than one of the main packages
(require 'gis-filter)

;;; Keymap and menu generation
(require 'menu-sw)
(require 'swkeys)

;;; Load Magik Patch development code. Either if MPDE already loaded or via mpde-install autoload.
;; Load development tools if MPDE is enabled.
(and (fboundp 'mpde-configurations-available-p)
     (mpde-configurations-available-p)
     (require 'require_sw_development))

;;Seems like a good idea to always force the loading of the following:
(load "sw-autoload")
(load "sw-cus-load")

(provide 'require_sw_defs)

;;; load_sw_defs.el ends here





