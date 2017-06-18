;; Web Interaction
;;
;;    Code to define MIME type for downloading Patch files from the web directly into Emacs
;;

(eval-when-compile (require 'magik-patch))

(defvar gnuserv-frame)			;shut the compiler up.

(defgroup magik-download nil
  "Internet Browser Interface for downloading Magik Patch files."
  :tag "Magik Download"
  :group 'smallworld)

(defconst magik-download-version "$Revision: 1.3 $")

(defcustom file-download-same-window-p nil
  "*If nil, create the buffer in an 'other' window.
I.e. t will make this new buffer the current buffer,
whereas nil will not steal the focus from your current buffer's window"
  :group 'magik-download
  :type  'boolean)

;;Functionality to interface with the download facility within a web browser
(defcustom magik-download-default-directory mpde-env-proposed-patches-dir
  "*The default directory to store magik files downloaded from the web"
  :group 'magik-download
  :type  'directory)

(defcustom magik-download-save-file-p nil
  "*If t, automatically save the downloaded magik file"
  :group 'magik-download
  :type  'boolean)

;;Generic download function
(defun file-download-mime (file &optional same-window)
  "Generic function for handling files downloaded from Web browsers.

With a prefix arg, ask user use current window to display downloaded file.

Use the variable `file-download-same-window-p' to control the window you want the
downloaded file to appear in."
  (interactive "fFile: \nP")
  (setq same-window (or current-prefix-arg file-download-same-window-p))
  (if same-window
      (switch-to-buffer (get-buffer-create "*downloaded file*"))
    (switch-to-buffer-other-window (get-buffer-create "*downloaded file*")))

  (erase-buffer)
  (insert-file-contents file)
  (goto-char (point-min))
  (sit-for 0.01)
  (raise-frame gnuserv-frame))

(defun magik-download (file &optional same-window)
  "Function used for downloading magik files, usually Magik Patches from the web
See the variables `magik-download-default-directory', `magik-download-save-file-p'
and `file-download-same-window-p' for options on how you want the downloaded magik file
to appear in your buffer"
  (interactive "fFile: \nP")
  (file-download-mime file same-window)
  (if (re-search-forward "^#Change-Number: \\(\\S-+\\)$" nil t)
      (let ((buf-name (concat "P"
			      (buffer-substring (match-beginning 1) (match-end 1))
			      ".magik")))
	(if (get-buffer buf-name)
	    (if (y-or-n-p (format "Buffer %s already exists, create a new buffer? " buf-name))
		(setq buf-name (generate-new-buffer-name buf-name))
	      (kill-buffer buf-name)))

	(rename-buffer buf-name)
	(setq buffer-file-name (concat (file-name-as-directory magik-download-default-directory) (buffer-name)))
	(magik-mode)
	(magik-patch-mode)
	(goto-char (point-min))
	(and magik-download-save-file-p (basic-save-buffer)))))

(defalias 'download-magik 'magik-download)

