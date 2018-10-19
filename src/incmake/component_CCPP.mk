# Location of the ESMF makefile fragment for this component:
ccpp_mk=$(CCPP_BINDIR)/ccpp.mk
all_component_mk_files+=$(ccpp_mk)

# Location of source code and installation
CCPP_SRCDIR?=$(ROOTDIR)/ccpp
CCPP_BINDIR?=$(ROOTDIR)/ccpp

# Make sure we have a model that supports CCPP:
ifeq (,$(findstring FV3,$(COMPONENTS)))
  $(error CCPP requires FV3)
else
  # Ensure the model is selected.
  override CCPP_CONFOPT += --model=FV3v1
  override CCPP_MAKEOPT ?= $(FV3_MAKEOPT)
  override CCPP_BUILDOPT ?= $(FV3_BUILDOPT)
endif

ifneq (,$(findstring DEBUG=Y,$(FV3_MAKEOPT)))
  override CCPP_CONFOPT += --debug
endif

# Make sure the expected directories exist and are non-empty:
$(call require_dir,$(CCPP_SRCDIR),CCPP source directory)

########################################################################

# Rule for building this component:
build_CCPP: $(ccpp_mk)

$(ccpp_mk): configure
	$(MODULE_LOGIC) ; \
	set -xue                                                        ; \
	export PATH_CCPP="$(CCPP_SRCDIR)"                               ; \
	export NEMS_CCPP_CPPFLAGS="-DCCPP"                              ; \
	export NEMS_CCPP_LDFLAGS="-L$(CCPP_SRCDIR)/lib -lccpp"          ; \
	cd $(ROOTDIR)                                                   ; \
	./ccpp/framework/scripts/ccpp_prebuild.py $(CCPP_CONFOPT)       ; \
	cd $$PATH_CCPP                                                  ; \
	./build_ccpp.sh ${BUILD_TARGET} "$$PATH_CCPP"                     \
	  "$(CCPP_MAKEOPT)" NO NO                                       ; \
	echo "ESMF_DEP_LINK_OBJS=-L$(CCPP_BINDIR)/lib -lccpp -lccppphys"  \
	                                 > $(ccpp_mk)                   ; \
	test -d "$(CCPP_BINDIR)"/include                                ; \
	test -d "$(CCPP_BINDIR)"/lib

########################################################################

# Rule for cleaning intermediate files
distclean_CCPP: clean_CCPP
	set -x                                             ; \
	cd "$(CCPP_SRCDIR)"                                ; \
	rm -rf "$(CCPP_SRCDIR)/lib"                        ; \
	rm -rf "$(CCPP_SRCDIR)/include"                    ; \
	rm -f $(ccpp_mk)

clean_CCPP:
	set -x                                             ; \
	cd "$(CCPP_SRCDIR)"                                ; \
	rm -rf "$(CCPP_SRCDIR)/build"
