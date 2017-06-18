;; This file is loaded by load_sw_defs.el when MPDE finds any valid
;; development environment configuration, including the 'master configuration.
;;

;;; Load Code Development libraries.
;; The ordering indicates the dependencies later packages depend on earlier.

(require 'mpde-boot) ;may have been loaded via ~/.mpde

(defconst load_sw_development-version "$Revision: 1.5 $")

(load "mpde-env")

(load "magik-patch")
(load "magik-patch-download")

;;MPDE must be loaded after all packages that register variables with it.
;;     This is to ensure mpde-load-configuration-files will do the right thing.
;;     If you add a package that needs to register with MPDE, the code must be
;;     loaded before here i.e. place a line above or ensure that it is loaded in
;;     a user's ~/.emacs file. Since we now use default.el to load the Smallworld
;;     including this in ~/.emacs is much simpler since default.el always loads
;;     after ~/.emacs (unless inhibit-default-init is t but then the user must know
;;     what they are doing...)
(load "mpde")
(load "mpde-site-lisp" t t)

(provide 'load_sw_development)
