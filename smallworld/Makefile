# Smallworld Emacs Make file for creating byte-compiled Smallworld Elisp files
#
# gmake all     : cleans directory then byte compiles all the Smallworld elisp code
# gmake elc     : byte compiles all the Smallworld elisp code
#                 Delivered code only need support and compiled for the latest Emacs
# gmake clean   : Remove all byte compiled files under the source tree.
#
# Variables:
#  EMACS      : current/latest/generic Emacs executable to use.

RM=rm -f

# Emacs executable
EMACS=emacs

# Compilation ordered list of elisp source files.
ELISP=resources.el\
utils-sw.el\
sw-help.el\
aliases.el\
magik-template.el\
indent-magik.el\
pragma.el\
mpde-env.el\
magik.el\
sw-electric.el\
cb.el\
gis.el\
loadlist.el\
module.el\
msg.el\
product.el\
auto-gis.el\
deep-print.el\
dev-tools.el\
gis-filter.el\
gis-version.el\
magik-patch.el\
magik-patch-download.el\
magik-studio.el\
mpde.el\
swdoc.el\
test-harness.el\
menu-sw.el\
swkeys.el\
todo-dynamic.el

ELISP_ELC=${ELISP:.el=.elc}

all: clean elc

# Target for anvil CUTTER process
CUTTER: clean elc

# Target for anvil tag CAMBRIDGE process
CAMBRIDGE: elc

# Target for anvil tag ESCROW process
ESCROW:
	@echo $(MAKEFILE_LIST) copied for ESCROW purposes

elc: $(ELISP_ELC)

clean:
	$(RM) $(ELISP_ELC)

# Generic .el -> .elc target.
#   EMACS_LOADPATH allows for provision of an extra load-path location.
.SUFFIXES: .elc .el
.el.elc :
	$(EMACS) -L . -batch -f batch-byte-compile $<
