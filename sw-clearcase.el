;;; Site specific setup for clearcase.el

;;load the ClearCase path processing functionality
(load "clearcase-path")

;;Load the main Clearcase code by Kevin Esler
(load "clearcase")

;;Load the experimental cleartool package
(if clearcase-servers-online
    (load "cleartool"))

(provide 'sw-clearcase)
