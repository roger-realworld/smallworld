;; In load-path, after this directory should come
;; certain subdirectories.  Here we specify them.
(normal-top-level-add-to-load-path
 (cond ((<= emacs-major-version 20)
	nil)
       ((<= emacs-major-version 21)
	'("cedet-1.0" "ecb-2.40" "nxml-mode-20041004" "tabbar-1.3" "tree-widget-2.0" "yasnippet-0.6.1c"))
       ((<= emacs-major-version 22)
	;; Emacs 22:
	;; vc-clearcase-2.2 is written for Emacs 22.
	'("auto-complete-1.3.1" "cedet-1.0" "ecb-2.40" "nxml-mode-20041004" "tabbar-1.3" "tree-widget-2.0" "vc-clearcase-2.2" "yasnippet-0.6.1c"))
       (t
	;; Emacs 23:
	;; vc-clearcase-3.2 is written for Emacs 23.
	;; nxml mode is now part of Emacs
	'("auto-complete-1.3.1" "cedet-1.0" "ecb-2.40" "tabbar-1.3" "tree-widget-2.0" "vc-clearcase-3.2" "yasnippet-0.6.1c"))))

(let ((default-directory (concat (file-name-as-directory default-directory) "cedet-1.0")))
  (load (concat (file-name-as-directory default-directory) "subdirs.el") nil t))
