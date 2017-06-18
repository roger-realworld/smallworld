;;; Dev Tools interfaces
;;

(require 'magik)
(require 'gis)

;;TODO move key defs to swkeys
(require 'swkeys)
(define-key magik-mode-map "\C-cd" 'dev-tools-vsd-transmit-method)
(define-key sw-f2-map      "w"     'dev-tools-vsd-transmit-method)


(defgroup dev-tools nil
  "Smallworld \"Dev Tools\" interface."
  :group 'smallworld)

(defconst dev-tools-version "$Revision: 1.5 $")

(defun dev-tools-vsd-transmit-method ()
  "Send the current method to the Very Simple Debugger."
  (interactive)
  (save-excursion
    (magik-mark-method)
    (let ((buf (dev-tools-vsd-transmit-string
		(buffer-substring-no-properties (point) (mark))
		(magik-package-line))))
      (and (get-buffer buf)
	   (not (get-buffer-window buf 'visible))
	   (pop-to-buffer buf t)))))

(defun dev-tools-vsd-open_with_file (filename)
  "Loads the magik_tools module if not already loaded."
  (magik-function
   "_proc(filename) 
        _handling sw_module_already_loaded_same_version _with procedure
        _dynamic !current_package!
	sw_module_manager.load_module(:dev_tools_application, _unset, 
                                      :update_image?, _false)
	!current_package![:very_simple_debugger].open_with_file(filename)
_endproc"
   filename))

(defun dev-tools-vsd-transmit-string (str package)
   "send current region via a temp file to the Very Simple Debugger window.
If this command is repeated before the previous file has been processed by Magik,
another file shall be written."
   (interactive "r")
   (magik-transmit-string str package
			  (lambda (f) (dev-tools-vsd-open_with_file f))
			  (lambda (f) (magik-function "system.unlink" f 'false 'true))))

(defun dev-tools-object-inspector (arg &optional buffer)
  "Use the Dev Tools application to inspect the current object."
  (interactive
   (let ((buffer (sw-get-buffer-mode (and (eq major-mode 'gis-mode) (buffer-name (current-buffer)))
				     'gis-mode
				     resources-gis-enter-buffer
				     gis-buffer
				     'gis-buffer-alist-prefix-function)))
     (barf-if-no-gis buffer)
     (list (sw-find-tag-tag (concat resources-deep-print-prompt ": ")) buffer)))
  (if (and (not (equal arg ""))
           (not (equal arg nil)))
      (let ((p (get-buffer-process buffer)))
        (or (and p (eq (process-status p) 'run))
	    (error resources-sw-no-process-error buffer))
	(and (get-buffer buffer)
	   (not (get-buffer-window buffer 'visible))
	   (pop-to-buffer buffer t))
	(process-send-string
	 p
	 (format
	  "_proc(object) 
        _handling sw_module_already_loaded_same_version _with procedure
        _dynamic !current_package!
	sw_module_manager.load_module(:dev_tools_application, _unset, 
                                      :update_image?, _false)
	!current_package![:object_inspector].open( object )
_endproc(%s)\n$\n"
	  arg)))))

(defun dev-tools-traceback-viewer ()
  "Use the Dev Tools application to view the last tracebacks."
  (interactive)
  (or (and gis-process (eq (process-status gis-process) 'run))
	    (error resources-sw-no-process-error (current-buffer)))
  (process-send-string
	 gis-process
	 "traceback_viewer.invoke()\n$\n"
	 ))

(provide 'dev-tools)
