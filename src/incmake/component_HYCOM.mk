# Location of the ESMF makefile fragment for this component:
hycom_mk=$(HYCOM_BINDIR)/hycom.mk
all_component_mk_files+=$(hycom_mk)

# Location of source code and installation
HYCOM_SRCDIR?=$(ROOTDIR)/HYCOM
HYCOM_BINDIR?=$(ROOTDIR)/HYCOM/HYCOM-INSTALL

ifeq ($(MACHINE_ID),linux_gnu)
  HYCOM_ARCH=Alinux-gnu-relo
  HYCOM_TYPE=nuopc
  HYCOM_OPTS=-DEOS_SIG2 -DEOS_17T -DESPC_COUPLE
else ifeq ($(MACHINE_ID),hera)
  HYCOM_ARCH=intelsse-impi-sm-relo
  HYCOM_TYPE=mpi
  HYCOM_OPTS=-DEOS_SIG2 -DEOS_17T -DESPC_COUPLE
else ifeq ($(MACHINE_ID),jet)
  HYCOM_ARCH=intelsse-impi-sm-relo
  HYCOM_TYPE=mpi
  HYCOM_OPTS=-DEOS_SIG2 -DEOS_17T -DESPC_COUPLE
else ifeq ($(MACHINE_ID),orion)
  HYCOM_ARCH=intelsse-impi-sm-relo
  HYCOM_TYPE=mpi
  HYCOM_OPTS=-DEOS_SIG2 -DEOS_17T -DESPC_COUPLE
else ifeq ($(MACHINE_ID),wcoss_cray)
  HYCOM_ARCH=xc40-intel-relo
  HYCOM_TYPE=mpi
  HYCOM_OPTS=-DEOS_SIG2 -DEOS_17T -DESPC_COUPLE
else ifeq ($(MACHINE_ID),wcoss_dell_p3)
  HYCOM_ARCH=intelsse-impi-sm-relo
  HYCOM_TYPE=mpi
  HYCOM_OPTS=-DEOS_SIG2 -DEOS_17T -DESPC_COUPLE
else
  HYCOM_ARCH=Aintelrelo
  HYCOM_TYPE=nuopc
  HYCOM_OPTS=-DEOS_SIG2 -DEOS_17T -DESPC_COUPLE
endif

# Make sure the expected directories exist and are non-empty:
$(call require_dir,$(HYCOM_SRCDIR),HYCOM source directory)

HYCOM_ALL_OPTS=\
  HYCOM_ARCH=$(HYCOM_ARCH) \
  HYCOM_TYPE=$(HYCOM_TYPE) \
  COMP_BINDIR=$(HYCOM_BINDIR) \
  COMP_SRCDIR=$(HYCOM_SRCDIR)

########################################################################

# Rules for building this component:

$(hycom_mk): configure
	+$(MODULE_LOGIC) ; cd "$(HYCOM_SRCDIR)" ; exec $(MAKE)           \
	  ARCH="$(HYCOM_ARCH)" TYPE="$(HYCOM_TYPE)" CPP_EXTRAS="$(HYCOM_OPTS)" \
	  esmf
	+$(MODULE_LOGIC) ; cd "$(HYCOM_SRCDIR)/NUOPC" ; exec $(MAKE)           \
	  ARCH="$(HYCOM_ARCH)" TYPE="$(HYCOM_TYPE)" CPP_EXTRAS="$(HYCOM_OPTS)" \
	  nuopc
	+-$(MODULE_LOGIC) ; cd "$(HYCOM_SRCDIR)/NUOPC" ; exec $(MAKE)          \
	  ARCH="$(HYCOM_ARCH)" TYPE="$(HYCOM_TYPE)" CPP_EXTRAS="$(HYCOM_OPTS)" \
	  DESTDIR=/ INSTDIR="$(HYCOM_BINDIR)" nuopcinstall
	test -d "$(HYCOM_BINDIR)"
	test -s "$(hycom_mk)"

# Note that we do not check the return status of nuopcinstall because
# it always fails.  There is a syntax error in one of the makefile
# rules within HYCOM/sorc/Makefile.  The sole purpose of the command
# is to create the $(hycom_mk) so as long as that is there, the
# nuopcinstall succeeded.

build_HYCOM: $(hycom_mk)

########################################################################

# Rules for cleaning the SRCDIR and BINDIR:

clean_HYCOM:
	+-$(MODULE_LOGIC) ; cd $(HYCOM_SRCDIR)/NUOPC ; exec $(MAKE)            \
	  ARCH="$(HYCOM_ARCH)" TYPE="$(HYCOM_TYPE)" CPP_EXTRAS="$(HYCOM_OPTS)" \
	  nuopcdistclean

distclean_HYCOM: clean_HYCOM
	rm -rf $(HYCOM_BINDIR) $(hycom_mk)
