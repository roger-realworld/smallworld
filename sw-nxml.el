;;;nxml-mode requires Emacs 21 and above. 
(require 'cl)

(if (or xemacs-p (< emacs-major-version 21))
    nil ;;NXML code will not load into Emacs versions less than 21.

  ;;Configure our defaults...
  (defvar nxml-child-indent 4)
  (defvar nxml-attribute-indent 4)
  (defvar nxml-slash-auto-complete-flag 1)
  
  (defvar magic-mode-alist nil) ; New at Emacs 22 and used in set-auto-mode.

  (let ((nxml     "\\.\\(xml\\|xsl\\|rng\\|xhtm?l?\\|html?\\|wsdl\\)\\'")
	(rnc      "\\.rnc\\'")
	(magic    "<\\(\\?xml \\|html\\)")
	(default-directory (directory-file-name (file-name-directory load-file-name))))
    (or (assoc nxml auto-mode-alist)
	(push (cons nxml 'nxml-mode) auto-mode-alist))
    (or (assoc rnc  auto-mode-alist)
	(push (cons rnc  'rnc-mode) auto-mode-alist))
    (or (assoc magic  magic-mode-alist)
	(push (cons magic  'nxml-mode) magic-mode-alist)))

  (require 'rnc-mode)
  ;;Load the package to set up autoloads for nXML etc
  ;;This code automatically updates load-path too.
;  (load "rng-auto")
)

(provide 'sw-nxml)
