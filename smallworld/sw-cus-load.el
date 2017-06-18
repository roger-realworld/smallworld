(defconst sw-cus-load-version "$Revision: 1.1 $")

(put 'smallworld   'custom-loads '("product" "module" "msg" "loadlist"
				   "dev-tools" "deep-print"
				   "gis-version" "auto-gis"
				   "todo-dynamic" "swdoc"))
(put 'product      'custom-loads '("product"))
(put 'module       'custom-loads '("module"))
(put 'msg          'custom-loads '("msg"))
(put 'loadlist     'custom-loads '("loadlist"))
(put 'dev-tools    'custom-loads '("dev-tools"))
(put 'deep-print   'custom-loads '("deep-print"))
(put 'gis-version  'custom-loads '("gis-version"))
(put 'auto-gis     'custom-loads '("auto-gis"))
(put 'todo-dynamic 'custom-loads '("todo-dynamic"))
(put 'swdoc        'custom-loads '("swdoc"))

;; Magik Patch Development Environment is not loaded as part of 'smallworld
;; property list above but is still made accessible if explicitly
;; requested via customize-group or the specific group menu.
(put 'mpde 'custom-loads '("mpde"))

(provide 'sw-cus-load)
