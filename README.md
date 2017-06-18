# smallworld
Magik development support for GE Smallworld

## Installation

These modes should work with Emacs 23+, but we recommend Emacs 25.2+.

These packages are available on [MELPA](http://melpa.org/). To use
rolling releases:

``` emacs-lisp
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
```

Alternatively, if you just want stable releases:

``` emacs-lisp
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
```

