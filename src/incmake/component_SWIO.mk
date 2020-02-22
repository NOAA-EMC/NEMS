# Location of the ESMF makefile fragment for this component:
swio_mk = $(SWIO_BINDIR)/swio.mk
all_component_mk_files+=$(swio_mk)

# Location of source code and installation
SWIO_SRCDIR?=$(ROOTDIR)/SWIO
SWIO_BINDIR?=$(ROOTDIR)/SWIO/SWIO_INSTALL

# Make sure the expected directories exist and are non-empty:
$(call require_dir,$(SWIO_SRCDIR),SWIO source directory)

ifneq (,$(findstring DEBUG=Y,$(SWIO_MAKEOPT)))
  override SWIO_CONFOPT += --enable-debug
endif

########################################################################

# Rule for building this component:

build_SWIO: $(swio_mk)

$(swio_mk): configure
	set -e
	+$(MODULE_LOGIC) ; cd $(SWIO_SRCDIR)                   ; \
	test -r Makefile || exec ./configure                     \
	  --prefix=$(SWIO_BINDIR)                                \
	  --datarootdir=$(SWIO_BINDIR) --libdir=$(SWIO_BINDIR)   \
          $(SWIO_CONFOPT)
	+$(MODULE_LOGIC) ; cd $(SWIO_SRCDIR) ; exec $(MAKE)
	+$(MODULE_LOGIC) ; cd $(SWIO_SRCDIR) ; exec $(MAKE) install
	test -d "$(SWIO_BINDIR)"

########################################################################

# Rule for cleaning the SRCDIR and BINDIR:

clean_SWIO:
	+cd $(SWIO_SRCDIR) ; test -f Makefile && exec $(MAKE) -k distclean || echo "Nothing to clean up"

distclean_SWIO: clean_SWIO
	rm -rf $(SWIO_BINDIR) $(swio_mk)
