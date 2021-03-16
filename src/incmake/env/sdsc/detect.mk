########################################################################
#
# Main driver for Intel development machine support
#
########################################################################

ifneq (,$(wildcard /oasis/projects/nsf))
  NEMS_COMPILER?=intel
  $(call add_build_env,comet,env/sdsc/comet.mk)
endif
