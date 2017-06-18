# Smallworld Emacs Make file for creating byte-compiled Smallworld Elisp files
#
# gmake all     : byte compiles all the feature code for all Emacsen, copying the elc files
#                 to appropriate version numbered subdirectories
# gmake all_elc : byte compiles all the feature code for the latest Emacs.
#                 Used by CUT process.
#                 Delivered code only need support and compiled for the latest Emacs
# gmake clean   : Remove all byte compiled files under the source tree.
#
# Variables:
#  EMACSxx    : Emacs executable for version xx.
#  EMACS      : current/latest/generic Emacs executable to use.
#               Useful for compiling individual features.
#  INSTALLDIR : Location to install .elc files to. Defaults to `pwd`/xx 
#  'Feature'  : Directory based features have variable set to their subdirectory name. 
#

RM=rm -f
RMDIR=rm -fr
MKDIR=mkdir -p
FIND=find

# Emacs executable for smallworld specific code:
#  Emacs 21 is usually chosen because it generates byte-code suitable for earlier Emacsen too.
EMACS=emacs-21.2

EMACS_LOADPATH=.

# Directory based features
DEPRECATED=deprecated
LOCAL     =local
FEATURES  =features
SMALLWORLD=smallworld

SWLISP=$(DEPRECATED) $(LOCAL) $(SMALLWORLD)

# Collections of .el and .elc files
DEPRECATED_EL =$(shell cd $(DEPRECATED) && $(FIND) . -name '*.el')
DEPRECATED_ELC=${DEPRECATED_EL:.el=.elc}

LOCAL_EL =$(shell cd $(LOCAL) && $(FIND) . -name '*.el')
LOCAL_ELC=${LOCAL_EL:.el=.elc}

SMALLWORLD_EL =$(shell $(FIND) $(SMALLWORLD) -name '*.el')
SMALLWORLD_ELC=${SMALLWORLD_EL:.el=.elc}

# Byte compile everything and also byte compile all the features directory for all Emacsen
all: elc features

# Target for anvil CUTTER process
CUTTER: clean elc

# Target for anvil tag CAMBRIDGE process
CAMBRIDGE: ede elc

# Target for anvil tag ESCROW process
ESCROW:
	@echo $(MAKEFILE_LIST) copied for ESCROW purposes

# We do not normally byte compile the local and deprecated directories.
elc: smallworld

# Targets for smallworld lisp directory
.PHONY: smallworld
smallworld: 
	cd $(SMALLWORLD); ${MAKE} EMACS=$(EMACS) elc

# Targets for byte compiling other smallworld directories should we wish to.
.PHONY: deprecated
deprecated:
	cd $(DEPRECATED); ${MAKE} -f ../Makefile EMACS=$(EMACS) EMACS_LOADPATH=.. $(DEPRECATED_ELC)

.PHONY: local
local:
	cd $(LOCAL); ${MAKE} -f ../Makefile EMACS=$(EMACS) EMACS_LOADPATH=.. $(LOCAL_ELC)

# Byte compile all features
features: 
	cd $(FEATURES); ${MAKE} all

# remove elc files.
clean:
	$(FIND) $(SWLISP) -name '*.elc' -exec $(RM) {} \;

# Generic .el -> .elc target.
#   EMACS_LOADPATH allows for provision of an extra load-path location.
.SUFFIXES: .elc .el
.el.elc :
	$(EMACS) -L . -L $(EMACS_LOADPATH) -batch -f batch-byte-compile $<

subdirs.elc:
	@echo $@ is not byte compiled

