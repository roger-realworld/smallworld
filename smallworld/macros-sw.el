;;; macros-sw.el - for suppressing Emacs and XEmacs byte-compiler warnings.


(eval-and-compile
  (defconst emacs19 (= (string-to-number emacs-version) 19))
  (defconst emacs20 (= (string-to-number emacs-version) 20))
  (defconst emacs21 (= (string-to-number emacs-version) 21))
  (defconst emacs22 (= (string-to-number emacs-version) 22))
  (defconst xemacs-p (string-match "XEmacs" emacs-version)))

(defconst macros-sw-version "$Revision: 1.16 $")

(defmacro eval-if (condition &rest expr)
  (and (eval condition)
       (cons 'progn expr)))

(defmacro eval-if-gnu-emacs (&rest expr)
  (cons 'eval-if
	(cons (not xemacs-p) expr)))

(defmacro eval-if-xemacs (&rest expr)
  (cons 'eval-if
	(cons xemacs-p expr)))


(defmacro eval-if-gnu-emacs-1925 (&rest expr)
  (cons 'eval-if
	(cons (string-match "^19\\.25" emacs-version)
	      expr)))

;;macro
(defmacro defmacro-if (condition f args &rest body)
  (and (eval condition)
       (cons 'defmacro
	     (cons f
		   (cons args body)))))

;;defun
(defmacro defun-if (condition f args &rest body)
  (and (eval condition)
       (cons 'defun
	     (cons f
		   (cons args body)))))

(defmacro defun-if-xemacs (f args &rest body)
  (cons 'defun-if
	(cons xemacs-p
	      (cons f
		    (cons args body)))))

(defmacro defun-if-gnu-emacs (f args &rest body)
  (cons 'defun-if
	(cons (not xemacs-p)
	      (cons f
		    (cons args body)))))

;;OS macros
(defmacro running-under-x ()
  (if xemacs-p
      '(eq (device-type) 'x)
    '(eq window-system 'x)))


(defmacro running-under-win32 ()
  '(or (eq window-system 'win32)
       (eq window-system 'w32)))


(defmacro running-under-nt ()     ; or under windows 95!
  '(eq system-type 'windows-nt))

;; Core Emacs functionality

;;;from speedbar.el:
;; From custom web page for compatibility between versions of custom:
(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom) (fboundp 'custom-declare-variable)
	   ;; Some XEmacsen w/ custom don't have :set keyword.
	   ;; This protects them against custom.
	   (fboundp 'custom-initialize-set))
      nil ;; We've got what we needed
    ;; We have the old custom-library, hack around it!
    (defmacro defgroup (&rest args)
      nil)
    (defmacro defface (var values doc &rest args)
      `(progn
	 (defvar ,var (quote ,var))
	 ;; To make colors for your faces you need to set your .Xdefaults
	 ;; or set them up ahead of time in your .emacs file.
	 (make-face ,var)
	 ))
    (defmacro defcustom (var value doc &rest args)
      `(defvar ,var ,value ,doc))
    (defmacro customize-group (&rest args)
      nil)))

(defgroup smallworld nil
  "Smallworld Development group"
  :group 'emacs)

(provide 'macros-sw)


;;; macros-sw.el ends here
