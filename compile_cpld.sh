 make app=coupledFV3_MOM6_CICE      distclean
 make app=coupledFV3_MOM6_CICE -j 8 build

#
##############################################################
# This builds the coupled model - app version
# Not a 32-bit build, may need to change later for bit-reproducibility checks
#./NEMS/NEMSAppBuilder rebuild app=coupledFV3_MOM6_CICE
#cd ./NEMS/exe
#mv NEMS.x nems_fv3_mom6_cice5.x
##############################################################
