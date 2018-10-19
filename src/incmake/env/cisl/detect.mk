########################################################################
#
# Main driver for CISL machine support
#
########################################################################

ifneq (,$(wildcard /glade))
  $(call add_build_env,cheyenne.intel,env/cisl/cheyenne.intel.mk)
  $(call add_build_env,cheyenne.pgi,env/cisl/cheyenne.pgi.mk)
  $(call add_build_env,cheyenne.gnu,env/cisl/cheyenne.gnu.mk)
endif
