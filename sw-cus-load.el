(defconst sw-cus-load-version "$Revision: 1.1 $")

(put 'smallworld   'custom-loads '("product" "module" "msg" "loadlist"
				   "dev-tools" "deep-print"
				   "gis-version" "auto-gis"
				   "todo-dynamic" "swdoc"))
(put 'product      'custom-loads '("product"))
(put 'module       'custom-loads '("module"))
(put 'msg          'custom-loads '("msg"))
(put 'loadlist     'custom-loads '("loadlist"))
(put 'deep-print   'custom-loads '("deep-print"))
(put 'gis-version  'custom-loads '("gis-version"))
(put 'auto-gis     'custom-loads '("auto-gis"))
(put 'todo-dynamic 'custom-loads '("todo-dynamic"))
(put 'swdoc        'custom-loads '("swdoc"))

(provide 'sw-cus-load)
