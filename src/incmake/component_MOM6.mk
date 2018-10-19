# Location of the ESMF makefile fragment for this component:
mom6_mk = $(MOM6_BINDIR)/mom6.mk
all_component_mk_files+=$(mom6_mk)

# Location of source code and installation
MOM6_SRCDIR?=$(ROOTDIR)/MOM6
MOM6_BINDIR?=$(ROOTDIR)/MOM6_INSTALL

# Make sure the expected directories exist and are non-empty:
$(call require_dir,$(MOM6_SRCDIR),MOM6 source directory)

# Rule for building this component:
build_MOM6: $(mom6_mk)

MOM6_ALL_OPTS=\
  COMP_SRCDIR="$(MOM6_SRCDIR)" \
  COMP_BINDIR="$(MOM6_BINDIR)" \
  FMS_BINDIR="$(FMS_BINDIR)" \
  MACHINE_ID="$(MACHINE_ID)"

# Workaround: if MOM6 is built twice, it fails because files in
# $(MOM6_SRCDIR)/exec/ already exist.

$(mom6_mk): $(fms_mk) configure
	-rm -fr $(MOM6_SRCDIR)/exec
	$(MODULE_LOGIC) ; export $(MOM6_ALL_OPTS)                     ; \
	set -e                                                        ; \
	cd $(MOM6_SRCDIR)                                             ; \
	./compile.sh --platform $(MACHINE_ID) --fms-dir "$(FMS_BINDIR)"
	+$(MODULE_LOGIC) ; cd $(MOM6_SRCDIR)                          ; \
	  exec $(MAKE) -f makefile.nuopc $(MOM6_ALL_OPTS)               \
	  "FMSDIR=$(FMS_BINDIR)"                                        \
	  "NEMSMOMDIR=$(MOM6_SRCDIR)/exec/$(MACHINE_ID)"                \
	  "INSTALLDIR=$(MOM6_BINDIR)" install
	test -d "$(MOM6_BINDIR)"
	test -s "$(mom6_mk)"

# Rule for cleaning the SRCDIR and BINDIR:
clean_MOM6:
	set -e                                                           ; \
	cd $(MOM6_SRCDIR)                                                ; \
	set +e                                                           ; \
	rm -rf exec src/path_names_shared                                ; \
	find . -name '*.o' -o -name '*.mod' -o -name '*.a' | xargs rm -f

distclean_MOM6: clean_MOM6
	rm -f $(MOM6_SRCDIR)/src/MOM6/config_src/nems_cap/mom5.mk.install
	rm -rf $(MOM6_BINDIR)
	rm -f $(mom6_mk)
