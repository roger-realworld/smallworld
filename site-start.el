;;; site-start.el - emacs startup file.

;;; This file, if it exists in the emacs load path is loaded before
;;; the users' .emacs file.

;;;(if (< emacs-major-version 20)
;;;    (load (concat (file-name-directory load-file-name) "smallworld/site-start-19")
;;;	     nil )
(load (concat (file-name-directory load-file-name) "sw-site-start"))
;;;	   nil ))

;;; Maintainer's site Localisations
;;; Please read CUSTOMISATION_FAQ.txt

;;; Examples of site localisations:

;; SMTP Mail settings
;;(setq send-mail-function     'smtpmail-send-it
;;	smtpmail-local-domain  "LOCAL_DOMAIN"
;;	smtpmail-sendto-domain "SENDTO_DOMAIN"
;;	smtpmail-smtp-server   "SMTP_MAIL_SERVER")
     
;;; For other site localisations, you may also use mpde-site-lisp.el

;;; site-start.el ends here
