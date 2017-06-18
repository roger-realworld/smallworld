;; In load-path, after this directory should come
;; certain subdirectories.  Here we specify them.
(normal-top-level-add-to-load-path
	'("auto-complete-1.3.1" "cedet-1.0" "ecb-2.40" "tabbar-1.3" "tree-widget-2.0" "vc-clearcase-3.6" "yasnippet-0.6.1c"))

(let ((default-directory (concat (file-name-as-directory default-directory) "cedet-1.0")))
  (load (concat (file-name-as-directory default-directory) "subdirs.el") nil t))
