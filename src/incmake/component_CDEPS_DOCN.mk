# Location of the ESMF makefile fragment for this component:
cdeps_docn_mk = $(ROOTDIR)/CDEPS/DOCN_INSTALL/cdeps_docn.mk
all_component_mk_files+=$(cdeps_docn_mk)

# Need this because cmeps_mk is not available at this level
cdeps_mk = $(ROOTDIR)/CDEPS/CDEPS_INSTALL/cdeps.mk

# Location of source code and installation
CDEPS_DOCN_BINDIR?=$(ROOTDIR)/CDEPS/DOCN_INSTALL
CDEPS_BINDIR?=$(ROOTDIR)/CDEPS/CDEPS_INSTALL

# Make sure we're setting CDEPS=Y if DOCN is enabled:
ifeq (,$(findstring CDEPS,$(COMPONENTS)))
  $(error DOCN requires CDEPS)
endif

# Rule for building this component:
build_CDEPS_DOCN: $(cdeps_docn_mk)

DOCN_ALL_FLAGS=\
  COMP_BINDIR="$(CDEPS_DOCN_BINDIR)"

$(cdeps_docn_mk): $(cdeps_mk) configure
	mkdir -p $(CDEPS_DOCN_BINDIR)
	rm -f $(cdeps_docn_mk)
	@echo "# ESMF self-describing build dependency makefile fragment" > $(cdeps_docn_mk)
	@echo "# src location: $(ROOTDIR)/CDEPS" >> $(cdeps_docn_mk)
	@echo  >> $(cdeps_docn_mk)
	@echo "ESMF_DEP_FRONT     = ocn_comp_nuopc" >> $(cdeps_docn_mk)
	@echo "ESMF_DEP_INCPATH   = " >> $(cdeps_docn_mk)
	@echo "ESMF_DEP_CMPL_OBJS = " >> $(cdeps_docn_mk)
	@echo "ESMF_DEP_LINK_OBJS = -L$(CDEPS_BINDIR)/lib -ldocn -ldshr -lstreams -lcdeps_share -lFoX_dom -lFoX_sax -lFoX_common -lFoX_utils -lFoX_fsys -L$(PIO_LIBDIR) -lpiof -lpioc" >> $(cdeps_docn_mk)

	test -d "$(CDEPS_DOCN_BINDIR)"
	test -s "$(cdeps_docn_mk)"

# Rule for cleaning the SRCDIR and BINDIR:
clean_CDEPS_DOCN:
	@echo "there is nothing to clean for CDEPS provided DOCN"

distclean_CDEPS_DOCN: clean_CDEPS_DOCN
	rm -rf $(CDEPS_DOCN_BINDIR) $(cdeps_docn_mk)

