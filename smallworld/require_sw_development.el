;; This file is loaded by load_sw_defs.el when MPDE finds any valid
;; development environment configuration, including the 'master configuration.
;;

;;; Load Code Development libraries.
;; The ordering indicates the dependencies later packages depend on earlier.

(eval-when-compile (require 'cl))

(defconst require_sw_development-version "$Revision: 1.4 $")

(require 'mpde-boot) ;may have been loaded from ~/.mpde

(require 'mpde-env)
(require 'magik-patch)

(require 'mpde)
(require 'mpde-site-lisp nil t)

(provide 'require_sw_development)
