# Location of the ESMF makefile fragment for this component:
cdeps_datm_mk = $(ROOTDIR)/CDEPS/DATM_INSTALL/cdeps_datm.mk
all_component_mk_files+=$(cdeps_datm_mk)

# Need this because cmeps_mk is not available at this level
cdeps_mk = $(ROOTDIR)/CDEPS/CDEPS_INSTALL/cdeps.mk

# Location of source code and installation
CDEPS_DATM_BINDIR?=$(ROOTDIR)/CDEPS/DATM_INSTALL
CDEPS_BINDIR?=$(ROOTDIR)/CDEPS/CDEPS_INSTALL

# Make sure we're setting CDEPS=Y if DATM is enabled:
ifeq (,$(findstring CDEPS,$(COMPONENTS)))
  $(error DATM requires CDEPS)
endif

# Rule for building this component:
build_CDEPS_DATM: $(cdeps_datm_mk)

DATM_ALL_FLAGS=\
  COMP_BINDIR="$(CDEPS_DATM_BINDIR)"

$(cdeps_datm_mk): $(cdeps_mk) configure
	mkdir -p $(CDEPS_DATM_BINDIR)
	rm -f $(cdeps_datm_mk)
	@echo "# ESMF self-describing build dependency makefile fragment" > $(cdeps_datm_mk)
	@echo "# src location: $(ROOTDIR)/CDEPS" >> $(cdeps_datm_mk)
	@echo  >> $(cdeps_datm_mk)
	@echo "ESMF_DEP_FRONT     = atm_comp_nuopc" >> $(cdeps_datm_mk)
	@echo "ESMF_DEP_INCPATH   = " >> $(cdeps_datm_mk)
	@echo "ESMF_DEP_CMPL_OBJS = " >> $(cdeps_datm_mk)
	@echo "ESMF_DEP_LINK_OBJS = -L$(CDEPS_BINDIR)/lib -ldatm -ldshr -lstreams -lcdeps_share -lFoX_dom -lFoX_sax -lFoX_common -lFoX_utils -lFoX_fsys -L$(PIO_LIBDIR) -lpiof -lpioc" >> $(cdeps_datm_mk)

	test -d "$(CDEPS_DATM_BINDIR)"
	test -s "$(cdeps_datm_mk)"

# Rule for cleaning the SRCDIR and BINDIR:
clean_CDEPS_DATM:
	@echo "there is nothing to clean for CDEPS provided DATM"

distclean_CDEPS_DATM: clean_CDEPS_DATM
	rm -rf $(CDEPS_DATM_BINDIR) $(cdeps_datm_mk)

