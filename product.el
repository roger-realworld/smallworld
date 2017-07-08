;;; product.el -- mode for editing Magik product.def files.

(eval-when-compile (require 'cl)
		   (require 'easymenu)
		   (require 'font-lock)
		   (require 'misc-sw)
		   (require 'sw-help)
		   (require 'utils-sw)
		   (require 'magik)
		   (require 'gis))

(defgroup product nil
  "Customise Magik product.def files group."
  :group 'smallworld
  :group 'tools)

(defconst product-version "$Revision: 1.2 $")

(defcustom product-mode-hook nil
  "*Hook to run after Product Mode is set."
  :group 'product
  :type  'hook)

(defvar product-mode-map (make-sparse-keymap)
  "Keymap for Magik product.def files")

(defvar product-f2-map (make-sparse-keymap)
  "Keymap for the F2 function key in Magik product.def buffers")

(fset 'product-f2-map   product-f2-map)

(define-key product-mode-map [f2]    'product-f2-map)

(define-key product-f2-map    "b"    'product-transmit-buffer)

(defvar product-menu nil
  "Keymap for the Magik product.def buffer menu bar")

(easy-menu-define product-menu product-mode-map
  "Menu for Product mode."
  `(,resources-product-menu
    [,resources-product-menu-add-product product-transmit-buffer
	 :active (sw-buffer-mode-list 'gis-mode)
	 :keys "f2 b"]
    [,resources-product-menu-reinitialise-product product-reinitialise
	 :active (sw-buffer-mode-list 'gis-mode)
	 :keys "f2 r"]
    "---"
    [,resources-menu-sw-customize     product-customize   t]
    [,resources-menu-sw-help          product-help        t]))

(define-key product-mode-map [f1] 'product-help)

(defvar product-mode-syntax-table nil
  "Syntax table in use in Product Mode buffers.")

;; Imenu configuration
(defvar product-imenu-generic-expression
  '(
    (nil "^\\(\\sw+\\)\\s-*\n\\(.\\|\n\\)*\nend\\s-*$" 1)
    )
  "Imenu generic expression for Magik Message mode.  See `imenu-generic-expression'.")

;; Font-lock configuration
(defcustom product-font-lock-keywords
  (list 
   '("^end\\s-*$" . font-lock-keyword-face)
   '("^hidden$" . font-lock-keyword-face)
   '("^\\(language\\)\\s-+\\(\\sw+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-type-face))
   '("^\\(\\sw+\\)\\s-*$" . font-lock-variable-name-face)
   '("^\\(\\sw+\\s-*\\sw*\\)\\s-*\\([0-9]*\\s-*[0-9]*\\)"
     (1 font-lock-function-name-face)
     (2 font-lock-constant-face))
   )
  "Default fontification of product.def files."
  :group 'product
  :type 'sexp)

(defun product-help ()
  "Display help on how to use the Product Mode interface."
  (interactive)
  (sw-help-open sw-help-product-id))

(defun product-customize ()
  "Open Customization buffer for Product Mode."
  (interactive)
  (customize-group 'product))

(defun product-mode ()
  "Major mode for editing Magik product.def files.

You can customise Product Mode with the `product-mode-hook'."

  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'require-final-newline)
  (make-local-variable 'font-lock-defaults)

  (use-local-map product-mode-map)
  (easy-menu-add product-menu)
  (set-syntax-table product-mode-syntax-table)

  (setq major-mode 'product-mode
	mode-name resources-product-menu
	require-final-newline t
	imenu-generic-expression product-imenu-generic-expression
	font-lock-defaults
	'(product-font-lock-keywords
	  nil t))

  (run-hooks 'product-mode-hook))

(defun product-name ()
  "Return current Product's name as a string."
  (save-excursion
    (goto-char (point-min))
    (current-word)))

(defun product-reinitialise (&optional gis)
  "Reinitialise this product in GIS."
  (interactive)
  (let* ((gis (sw-get-buffer-mode gis
				  'gis-mode
				  resources-gis-enter-buffer
				  gis-buffer
				  'gis-buffer-alist-prefix-function))
	 (process (barf-if-no-gis gis)))
    (display-buffer gis t)
    (process-send-string
     process
     (concat ;; the .products[] interface is used for backwards compatibility.
      "smallworld_product"
      ".products[:|" (product-name) "|]"
      ".reinitialise()\n$\n"))
    gis))

(defun product-transmit-add-product (filename process)
  "Add the product to the GIS process."
  (process-send-string
   process
   (concat
    (magik-function "smallworld_product.add_product" filename)
    "\n$\n")))

(defun product-transmit-buffer (&optional gis)
  "Send current buffer to GIS."
  (interactive)
  (let* ((gis (sw-get-buffer-mode gis
				  'gis-mode
				  resources-gis-enter-buffer
				  gis-buffer
				  'gis-buffer-alist-prefix-function))
	 (process (barf-if-no-gis gis))
	 (filename (buffer-file-name)))
    (pop-to-buffer gis t)
    (product-transmit-add-product filename process)
    gis))

(defun product-gis-drag-n-drop-load (gis filename)
  "Interface to Drag 'n' Drop GIS mode.
Called by `gis-drag-n-drop-load' when a Product file is dropped."
  (let ((process (barf-if-no-gis gis)))
    (product-transmit-add-product filename process)
    gis))

;;; Package initialisation
(if product-mode-syntax-table
    nil
  (setq product-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?_ "w" product-mode-syntax-table)
  (modify-syntax-entry ?# "<" product-mode-syntax-table)
  (modify-syntax-entry ?\n ">" product-mode-syntax-table))

;;; Package registration

(or (assoc "product\\.def$" auto-mode-alist)
    (push '("product\\.def$" . product-mode) auto-mode-alist))

(provide 'product)


;;; product.el ends here
