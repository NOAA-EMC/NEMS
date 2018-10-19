########################################################################
#
# Main driver for supporting build environments in unknown clusters.
# Generally, this uses "uname" to detect the platform.
#
########################################################################

override uname_a=$(shell uname -a)

ifneq (,$(findstring Darwin,$(uname_a)))
  $(call add_build_env,macos.gnu,env/uname/macos.gnu.mk)
  $(call add_build_env,macos.intel,env/uname/macos.intel.mk)
endif

ifneq (,$(findstring Linux,$(uname_a)))
  $(call add_build_env,linux.gnu,env/uname/linux.gnu.mk)
  $(call add_build_env,linux.intel,env/uname/linux.intel.mk)
  $(call add_build_env,linux.pgi,env/uname/linux.pgi.mk)
endif
