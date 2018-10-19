########################################################################
#
# Main driver for NOAA R&D computing environment support
#
########################################################################

ifneq (,$(and $(wildcard /scratch4),$(wildcard /scratch3)))
  $(call add_build_env,theia.intel,env/rdhpcs/theia.intel.mk)
  $(call add_build_env,theia.gnu,env/rdhpcs/theia.gnu.mk)
  $(call add_build_env,theia.pgi,env/rdhpcs/theia.pgi.mk)
else
  ifneq (,$(and $(wildcard /pan2),$(wildcard /lfs3)))
    $(call add_build_env,jet,env/rdhpcs/jet.mk)
  else
    ifneq (,$(shell hostname | grep -i gaea))
      $(call add_build_env,gaea,env/rdhpcs/gaea.mk)
    endif
  endif
endif
