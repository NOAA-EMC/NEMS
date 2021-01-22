# Location of the ESMF makefile fragment for this component:
cdeps_mk = $(ROOTDIR)/CDEPS/CDEPS_INSTALL/cdeps.mk
all_component_mk_files+=$(cdeps_mk)

# Need this because cmeps_mk is not available at this level
cmeps_mk = $(ROOTDIR)/CMEPS/CMEPS_INSTALL/cmeps.mk 

# Location of source code and installation
CDEPS_SRCDIR?=$(ROOTDIR)/CDEPS
CDEPS_BLDDIR?=$(ROOTDIR)/CDEPS/CDEPS_BUILD
CDEPS_BINDIR?=$(ROOTDIR)/CDEPS/CDEPS_INSTALL

# Make sure the expected directories exist and are non-empty:
$(call require_dir,$(CDEPS_SRCDIR),CDEPS source directory)

ifndef CONFIGURE_NEMS_FILE
$(error CONFIGURE_NEMS_FILE not set.)
endif

include $(CONFIGURE_NEMS_FILE)

# Rule for building this component:
build_CDEPS: $(cdeps_mk) 

CDEPS_ALL_OPTS=\
  COMP_SRCDIR="$(CDEPS_SRCDIR)" \
  COMP_BINDIR="$(CDEPS_BINDIR)" \
  MACHINE_ID="$(MACHINE_ID)" \
  FC="$(FC)" \
  CC="$(CC)" \
  CXX="$(CXX)"

# Rule for building CDEPS using cmake build system
$(cdeps_mk): $(cmeps_mk) configure
	$(MODULE_LOGIC); export $(CDEPS_ALL_OPTS); \
	set -e; $(MODULE_LOGIC); mkdir -p $(CDEPS_BLDDIR); \
	cd $(CDEPS_BLDDIR); \
        exec cmake -DCMAKE_BUILD_TYPE=Release \
	-DBLD_STANDALONE=ON \
	-DCMAKE_INSTALL_PREFIX=$(CDEPS_BINDIR) \
	-DCMAKE_Fortran_COMPILER=$(FC) \
	-DCMAKE_C_COMPILER=$(CC) \
	-DCMAKE_CXX_COMPILER=$(CXX) \
        -DPIO=$(PIO_ROOT) \
	-DPIO_Fortran_LIBRARY=$(PIO_LIBDIR) \
	-DPIO_Fortran_INCLUDE_DIR=$(PIO_INC) \
	-DPIO_C_LIBRARY=$(PIO_LIBDIR) \
	-DPIO_C_INCLUDE_DIR=$(PIO_INC) ../
	
	$(MODULE_LOGIC); export $(CDEPS_ALL_OPTS); \
	set -e; $(MODULE_LOGIC) ; cd $(CDEPS_BLDDIR); \
	exec $(MAKE) -j 1  $(CDEPS_ALL_OPTS) install VERBOSE=1
	
	$(MODULE_LOGIC); export $(CDEPS_ALL_OPTS); \
	set -e; $(MODULE_LOGIC) ; cd $(CDEPS_BLDDIR); \
	exec find ./d??? -name "*.a" -exec cp {} ../CDEPS_INSTALL/lib/ \;
	
	$(MODULE_LOGIC); export $(CDEPS_ALL_OPTS); \
        set -e; $(MODULE_LOGIC) ; cd $(CDEPS_BLDDIR); \
        exec find ./fox/lib -name "*.a" -exec cp {} ../CDEPS_INSTALL/lib/ \;
	
	$(MODULE_LOGIC); export $(CDEPS_ALL_OPTS); \
        set -e; $(MODULE_LOGIC) ; cd $(CDEPS_BLDDIR); \
	exec find share/ -name "*.a" -exec cp {} ../CDEPS_INSTALL/lib/ \;
	
	$(MODULE_LOGIC); export $(CDEPS_ALL_OPTS); \
        set -e; $(MODULE_LOGIC) ; cd $(CDEPS_BLDDIR); \
        exec find ./d??? -name "*_comp_nuopc.mod" -exec cp {} ../CDEPS_INSTALL/include/ \;
	
	rm -f $(CDEPS_BINDIR)/cdeps.mk
	@echo "# ESMF self-describing build dependency makefile fragment" > $(CDEPS_BINDIR)/cdeps.mk
	@echo "# src location: $(CDEPS_SRCDIR)" >> $(CDEPS_BINDIR)/cdeps.mk
	@echo  >> $(CDEPS_BINDIR)/cdeps.mk
	@echo "ESMF_DEP_FRONT     = " >> $(CDEPS_BINDIR)/cdeps.mk
	@echo "ESMF_DEP_INCPATH   = $(CDEPS_BINDIR)/include" >> $(CDEPS_BINDIR)/cdeps.mk
	@echo "ESMF_DEP_CMPL_OBJS = " >> $(CDEPS_BINDIR)/cdeps.mk
	@echo "ESMF_DEP_LINK_OBJS = -L$(CDEPS_BINDIR)/lib -ldshr -lstreams -lcdeps_share -lFoX_dom -lFoX_sax -lFoX_common -lFoX_utils -lFoX_fsys -L$(PIO_LIBDIR) -lpiof -lpioc" >> $(CDEPS_BINDIR)/cdeps.mk
	
	test -d "$(CDEPS_BINDIR)"
	test -s "$(cdeps_mk)"

# Rule for cleaning the SRCDIR and BINDIR:
clean_CDEPS: 
	cp -n $(MODULE_DIR)/$(CHOSEN_MODULE) $(CONFDIR)/modules.nems ; \
	$(MODULE_LOGIC); export $(CDEPS_ALL_OPTS); \
	set -e; \
	if [ -d "~/Dropbox" ]; then cd $(CDEPS_BLDDIR); \
	exec $(MAKE) clean; fi

distclean_CDEPS: clean_CDEPS
	rm -rf $(CDEPS_BINDIR)
	rm -rf $(CDEPS_BLDDIR)
	rm -f $(cdeps_mk)
