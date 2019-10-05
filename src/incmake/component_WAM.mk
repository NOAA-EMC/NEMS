# Location of the ESMF makefile fragment for this component:
wam_mk = $(WAM_BINDIR)/wam.mk
all_component_mk_files+=$(wam_mk)

# Location of source code and installation
WAM_SRCDIR?=$(ROOTDIR)/WAM
WAM_BINDIR?=$(ROOTDIR)/WAM/WAM_INSTALL

# Make sure the expected directories exist and are non-empty:
$(call require_dir,$(WAM_SRCDIR),WAM source directory)

ifneq (,$(findstring DEBUG=Y,$(WAM_MAKEOPT)))
  override WAM_CONFOPT += --enable-debug
endif

########################################################################

# Rule for building this component:

build_WAM: $(wam_mk)

$(wam_mk): configure
	set -e
	+$(MODULE_LOGIC) ; cd $(WAM_SRCDIR)                   ; \
	test -r Makefile || exec ./configure                    \
	  --prefix=$(WAM_BINDIR) --datarootdir=$(WAM_BINDIR)    \
	  --libdir=$(WAM_BINDIR) $(WAM_CONFOPT)
	+$(MODULE_LOGIC) ; cd $(WAM_SRCDIR) ; exec $(MAKE)
	+$(MODULE_LOGIC) ; cd $(WAM_SRCDIR) ; exec $(MAKE) install
	test -d "$(WAM_BINDIR)"

########################################################################

# Rule for cleaning the SRCDIR and BINDIR:

clean_WAM:
	+cd $(WAM_SRCDIR) ; test -f Makefile && exec $(MAKE) -k distclean || echo "Nothing to clean up"

distclean_WAM: clean_WAM
	rm -rf $(WAM_BINDIR) $(wam_mk)
