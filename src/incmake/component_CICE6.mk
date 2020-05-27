# Location of the ESMF makefile fragment for this component:
cice6_mk=$(CICE_BINDIR)/cice6.mk
all_component_mk_files+=$(cice6_mk)

# Location of source code and installation
CICE_SRCDIR?=$(ROOTDIR)/CICE6
CICE_UFSDIR?=$(ROOTDIR)/CICE6/forapps/ufs
CICE_BINDIR?=$(ROOTDIR)/CICE6_INSTALL

# NEMS_GRID was found in CICE and defaults to a low-res GSM grid
# This is obsolete and perhaps should be removed.
NEMS_GRID?=T126_mx5

# Make sure the expected directories exist and are non-empty:
$(call require_dir,$(CICE_SRCDIR),CICE source directory)

#ifneq (,$(findstring CMEPS,$(COMPONENTS)))
#CPPCICE = -DCMEPS
#else
CPPCICE =
#endif

CICE_ALL_OPTS=\
  COMP_SRCDIR=$(CICE_SRCDIR) \
  COMP_BINDIR=$(CICE_BINDIR) \
  SITE="$(MACHINE_ID)" \
  SYSTEM_USERDIR="$(CICE_UFSDIR)" \
  BINDIR="$(CICE_BINDIR)" \
  SRCDIR="$(CICE_SRCDIR)" \
  EXEDIR="$(CICE_UFSDIR)" \
  CPPCICE="$(CPPCICE)"  \
  NEMS_GRID="$(NEMS_GRID)"

########################################################################

# Rules for building this component:
$(cice6_mk): configure
	$(MODULE_LOGIC)                                                   ; \
	set -eu                                                           ; \
	export $(CICE_ALL_OPTS) $(CICE_MAKEOPT)                           ; \
	cd $(CICE_UFSDIR)                                                 ; \
	./comp_ice.backend.libcice
	test -f $(cice6_mk)

build_CICE6: $(cice6_mk)

########################################################################

# Rules for cleaning the SRCDIR and BINDIR:

clean_CICE6_SRC: configure
	$(MODULE_LOGIC)                                                   ; \
	set -eu                                                           ; \
	export $(CICE_ALL_OPTS) $(CICE_MAKEOPT)                           ; \
	cd $(CICE_UFSDIR)                                                 ; \
	./comp_ice.backend.clean

clean_CICE6: clean_CICE6_SRC

distclean_CICE6: clean_CICE6
	rm -rf $(CICE_BINDIR) $(cice6_mk)
