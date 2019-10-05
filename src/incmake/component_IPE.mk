# Location of the ESMF makefile fragment for this component:
ipe_mk = $(IPE_BINDIR)/ipe.mk
all_component_mk_files+=$(ipe_mk)

# Location of source code and installation
IPE_SRCDIR?=$(ROOTDIR)/IPE
IPE_BINDIR?=$(ROOTDIR)/IPE/IPE_INSTALL

# Make sure the expected directories exist and are non-empty:
$(call require_dir,$(IPE_SRCDIR),IPE source directory)

ifneq (,$(findstring DEBUG=Y,$(IPE_MAKEOPT)))
  override IPE_CONFOPT += --enable-debug
endif

########################################################################

# Rule for building this component:

build_IPE: $(ipe_mk)

$(ipe_mk): configure
	set -e
	+$(MODULE_LOGIC) ; cd $(IPE_SRCDIR)                   ; \
	test -r Makefile || exec ./configure                    \
	  --enable-coupling --prefix=$(IPE_BINDIR)              \
	  --datarootdir=$(IPE_BINDIR) --libdir=$(IPE_BINDIR)    \
          $(IPE_CONFOPT)
	+$(MODULE_LOGIC) ; cd $(IPE_SRCDIR) ; exec $(MAKE)
	+$(MODULE_LOGIC) ; cd $(IPE_SRCDIR) ; exec $(MAKE) install
	test -d "$(IPE_BINDIR)"

########################################################################

# Rule for cleaning the SRCDIR and BINDIR:

clean_IPE:
	+cd $(IPE_SRCDIR) ; test -f Makefile && exec $(MAKE) -k distclean || echo "Nothing to clean up"

distclean_IPE: clean_IPE
	rm -rf $(IPE_BINDIR) $(ipe_mk)
