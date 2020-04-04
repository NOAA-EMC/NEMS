########################################################################
#
# Main driver for SSEC Wisconsin S4 machine support
#
########################################################################

ifneq (,$(wildcard /data ))
  NEMS_COMPILER?=intel
  $(call add_build_env,s4.$(NEMS_COMPILER),env/ssec/s4.$(NEMS_COMPILER).mk)
endif
