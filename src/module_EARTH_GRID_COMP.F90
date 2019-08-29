#include "./ESMFVersionDefine.h"

!-----------------------------------------------------------------------
!
      MODULE module_EARTH_GRID_COMP
!
!-----------------------------------------------------------------------
!***  This module contains codes directly related to the EARTH component.
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!  2010-03-24  Black - Created Earth component module.
!  2010-04     Yang  - Added Ensemble capability.
!  2011-05-11  Theurich & Yang - Modified for using the ESMF 5.2.0r_beta_snapshot_07.
!  2011-10-04  Yang - Modified for using the ESMF 5.2.0r library.
!  2012-02     Tripp - Added ESMF superstructure to support an OCN model
!  2013-06     Theurich - Reworked OCN dependency to be NUOPC based
!  2013-07     Theurich - Macro based ESMF error handling
!-----------------------------------------------------------------------
!
!***  The EARTH component lies in the hierarchy seen here:
!
!          Main program
!               |
!               |
!          NEMS component
!               |     |________________________.
!               |                              |
!          EARTH component        Ensemble Coupler component
!              /|\
!             / | \
!          ATM/OCN/ICE/WAV/LND/IPM/HYD .. components
!          |    |   |
!          |    |   (CICE, etc.)
!          |    |
!          |    (MOM5, MOM6, HYCOM, POM, etc.)
!          |
!          CORE component (GSM, NMM, FV3, etc.)
!
!-----------------------------------------------------------------------
!
      USE ESMF

      use NUOPC
      use NUOPC_Driver,                                          &
        Driver_routine_SS             => SetServices,            &
        Driver_label_SetModelServices => label_SetModelServices, &
        Driver_label_SetRunSequence   => label_SetRunSequence,   &
        Driver_label_SetRunClock      => label_SetRunClock,      &
        Driver_label_Finalize         => label_Finalize
#if ESMF_VERSION_MAJOR < 8
      use NUOPC_Connector,  only: conSS      => SetServices
#endif
   ! -   Handle build time ATM options:
#ifdef FRONT_SATM
      use FRONT_SATM,       only: SATM_SS    => SetServices
#endif
#ifdef FRONT_XATM
      use FRONT_XATM,       only: XATM_SS    => SetServices
#endif
#ifdef FRONT_DATAWAM
      use FRONT_DATAWAM,    only: DATAWAM_SS => SetServices
#endif
#ifdef FRONT_GSM
      use FRONT_GSM,        only: GSM_SS     => SetServices
#endif
#ifdef FRONT_NMMB
      use FRONT_NMMB,       only: NMMB_SS    => SetServices
#endif
#ifdef FRONT_FV3
      use FRONT_FV3,        only: FV3_SS     => SetServices
#endif
#ifdef FRONT_DATM
      use FRONT_DATM,       only: DATM_SS    => SetServices
#endif
  ! - Handle build time OCN options:
#ifdef FRONT_SOCN
      use FRONT_SOCN,       only: SOCN_SS    => SetServices
#endif
#ifdef FRONT_XOCN
      use FRONT_XOCN,       only: XOCN_SS    => SetServices
#endif
#ifdef FRONT_HYCOM
      use FRONT_HYCOM,      only: HYCOM_SS   => SetServices
#endif
#ifdef FRONT_MOM5
      use FRONT_MOM5,       only: MOM5_SS    => SetServices
#endif
#ifdef FRONT_MOM6
      use FRONT_MOM6,       only: MOM6_SS    => SetServices
#endif
#ifdef FRONT_POM
      use FRONT_POM,        only: POM_SS     => SetServices
#endif
  ! - Handle build time ICE options:
#ifdef FRONT_SICE
      use FRONT_SICE,       only: SICE_SS    => SetServices
#endif
#ifdef FRONT_XICE
      use FRONT_XICE,       only: XICE_SS  => SetServices
#endif
#ifdef FRONT_CICE
      use FRONT_CICE,       only: CICE_SS  => SetServices
#endif
  ! - Handle build time WAV options:
#ifdef FRONT_SWAV
      use FRONT_SWAV,       only: SWAV_SS  => SetServices
#endif
#ifdef FRONT_XWAV
      use FRONT_XWAV,       only: XWAV_SS  => SetServices
#endif
#ifdef FRONT_WW3
      use FRONT_WW3,        only: WW3_SS  => SetServices
#endif
  ! - Handle build time LND options:
#ifdef FRONT_SLND
      use FRONT_SLND,       only: SLND_SS  => SetServices
#endif
#ifdef FRONT_XLND
      use FRONT_XLND,       only: XLND_SS  => SetServices
#endif
#ifdef FRONT_NOAH
      use FRONT_NOAH,       only: NOAH_SS  => SetServices
#endif
#ifdef FRONT_LIS
      use FRONT_LIS,        only: LIS_SS   => SetServices
#endif
  ! - Handle build time IPM options:
#ifdef FRONT_SIPM
      use FRONT_SIPM,       only: SIPM_SS  => SetServices
#endif
#ifdef FRONT_XIPM
      use FRONT_XIPM,       only: XIPM_SS  => SetServices
#endif
#ifdef FRONT_IPE
      use FRONT_IPE,        only: IPE_SS   => SetServices
#endif
#ifdef FRONT_DATAIPE
      use FRONT_DATAIPE,    only: DATAIPE_SS=> SetServices
#endif
  ! - Handle build time HYD options:
#ifdef FRONT_SHYD
      use FRONT_SHYD,       only: SHYD_SS  => SetServices
#endif
#ifdef FRONT_XHYD
      use FRONT_XHYD,       only: XHYD_SS  => SetServices
#endif
#ifdef FRONT_WRFHYDRO
      use FRONT_WRFHYDRO,   only: WRFHYDRO_SS  => SetServices
#endif
#ifdef FRONT_GSDCHEM
      use FRONT_GSDCHEM,    only: GSDCHEM_SS  => SetServices
#endif
  ! - Mediator
      use module_MEDIATOR,        only: MED_SS     => SetServices
      use module_MEDSpaceWeather, only: MEDSW_SS   => SetServices

      USE module_EARTH_INTERNAL_STATE,ONLY: EARTH_INTERNAL_STATE        &
                                           ,WRAP_EARTH_INTERNAL_STATE
!
!      USE module_ATM_GRID_COMP
!
      USE module_NEMS_UTILS,ONLY: ERR_MSG,MESSAGE_CHECK
!
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
      PRIVATE
!
      PUBLIC :: EARTH_REGISTER
      PUBLIC :: VERBOSE_DIAGNOSTICS
!
!-----------------------------------------------------------------------
!

      LOGICAL, PRIVATE :: flag_verbose_diagnostics = .false.


      CONTAINS

      logical function verbose_diagnostics(set)
        !! Mutator for the verbose diagnostics flag; returns true if
        !! verbose diagnostics should be used, and false otherwise.
        !! If the "set" argument is present, then the flag is set to
        !! the given value.
        logical, optional :: set
        if(present(set)) then
           flag_verbose_diagnostics = set
        endif
        verbose_diagnostics = flag_verbose_diagnostics
      end function verbose_diagnostics

!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
!
      SUBROUTINE EARTH_REGISTER(EARTH_GRID_COMP,RC_REG)
!
!-----------------------------------------------------------------------
!
!------------------------
!***  Argument Variables
!------------------------
!
      TYPE(ESMF_GridComp) :: EARTH_GRID_COMP                               !<-- The EARTH component
!
      INTEGER,INTENT(OUT) :: RC_REG                                        !<-- Error return code
!
!---------------------
!***  Local Variables
!---------------------
!
      INTEGER             :: RC, nf
      type(ESMF_Config)   :: config
!
!     integer, parameter       :: NumFields=251
      integer, parameter       :: NumFields=250
      character(60), parameter :: Field_Name_unit(2,NumFields) = (/                                                                   &
      "air_density_height_lowest                                   ", "kg m-3                                                      ", &
      "mean_zonal_moment_flx                                       ", "N m-2                                                       ", &
      "mean_merid_moment_flx                                       ", "N m-2                                                       ", &
      "mean_sensi_heat_flx                                         ", "W m-2                                                       ", &
      "mean_sensi_heat_flx_atm_into_ice                            ", "W m-2                                                       ", &
      "mean_sensi_heat_flx_atm_into_ocn                            ", "W m-2                                                       ", &
      "mean_laten_heat_flx                                         ", "W m-2                                                       ", &
      "mean_laten_heat_flx_atm_into_ice                            ", "W m-2                                                       ", &
      "mean_laten_heat_flx_atm_into_ocn                            ", "W m-2                                                       ", &
      "mean_down_lw_flx                                            ", "W m-2                                                       ", &
      "mean_down_sw_flx                                            ", "W m-2                                                       ", &
      "mean_fprec_rate                                             ", "kg s m-2                                                    ", &
      "mean_prec_rate                                              ", "kg s m-2                                                    ", &
      "mean_evap_rate                                              ", "kg s m-2                                                    ", &
      "mean_evap_rate_atm_into_ice                                 ", "kg s m-2                                                    ", &
      "mean_evap_rate_atm_into_ocn                                 ", "kg s m-2                                                    ", &
      "inst_zonal_moment_flx                                       ", "N m-2                                                       ", &
      "inst_merid_moment_flx                                       ", "N m-2                                                       ", &
      "inst_sensi_heat_flx                                         ", "W m-2                                                       ", &
      "inst_laten_heat_flx                                         ", "W m-2                                                       ", &
      "inst_down_lw_flx                                            ", "W m-2                                                       ", &
      "inst_down_sw_flx                                            ", "W m-2                                                       ", &
      "inst_temp_height2m                                          ", "K                                                           ", &
      "inst_spec_humid_height2m                                    ", "kg kg-1                                                     ", &
      "inst_u_wind_height10m                                       ", "m s-1                                                       ", &
      "inst_v_wind_height10m                                       ", "m s-1                                                       ", &
      "inst_zonal_wind_height10m                                   ", "m s-1                                                       ", &
      "inst_merid_wind_height10m                                   ", "m s-1                                                       ", &
      "inst_temp_height_surface                                    ", "K                                                           ", &
      "inst_pres_height_surface                                    ", "Pa                                                          ", &
      "inst_surface_height                                         ", "m                                                           ", &
      "mean_down_sw_vis_dir_flx                                    ", "W m-2                                                       ", &
      "mean_down_sw_vis_dif_flx                                    ", "W m-2                                                       ", &
      "mean_down_sw_ir_dir_flx                                     ", "W m-2                                                       ", &
      "mean_down_sw_ir_dif_flx                                     ", "W m-2                                                       ", &
      "inst_down_sw_vis_dir_flx                                    ", "W m-2                                                       ", &
      "inst_down_sw_vis_dif_flx                                    ", "W m-2                                                       ", &
      "inst_down_sw_ir_dir_flx                                     ", "W m-2                                                       ", &
      "inst_down_sw_ir_dif_flx                                     ", "W m-2                                                       ", &
      "mean_net_sw_vis_dir_flx                                     ", "W m-2                                                       ", &
      "mean_net_sw_vis_dif_flx                                     ", "W m-2                                                       ", &
      "mean_net_sw_ir_dir_flx                                      ", "W m-2                                                       ", &
      "mean_net_sw_ir_dif_flx                                      ", "W m-2                                                       ", &
      "inst_net_sw_vis_dir_flx                                     ", "W m-2                                                       ", &
      "inst_net_sw_vis_dif_flx                                     ", "W m-2                                                       ", &
      "inst_net_sw_ir_dir_flx                                      ", "W m-2                                                       ", &
      "inst_net_sw_ir_dif_flx                                      ", "W m-2                                                       ", &
      "mean_salt_rate                                              ", "kg psu m-2 s                                                ", &
      "mean_runoff_rate                                            ", "kg m-2 s                                                    ", &
      "mean_calving_rate                                           ", "kg m-2 s                                                    ", &
      "mean_runoff_heat_flx                                        ", "W m-2                                                       ", &
      "mean_calving_heat_flx                                       ", "W m-2                                                       ", &
      "ice_fraction                                                ", "1                                                           ", &
      "mean_sw_pen_to_ocn                                          ", "W m-2                                                       ", &
      "mean_up_lw_flx                                              ", "W m-2                                                       ", &
      "mass_of_overlying_sea_ice                                   ", "kg                                                          ", &
      "s_surf                                                      ", "psu                                                         ", &
      "freezing_melting_potential                                  ", "W m-2                                                       ", &

! following two added for export from MOM6
! ----------------------------------------
      "accum_heat_frazil                                           ", "W m-2                                                       ", &
      "inst_melt_potential                                         ", "W m-2                                                       ", &
      "u_surf                                                      ", "m s-1                                                       ", &
      "v_surf                                                      ", "m s-1                                                       ", &
      "sea_lev                                                     ", "m                                                           ", &
      "wind_stress_zonal                                           ", "N m-2                                                       ", &
      "wind_stress_merid                                           ", "N m-2                                                       ", &
      "ocn_current_zonal                                           ", "m s-1                                                       ", &
      "ocn_current_merid                                           ", "m s-1                                                       ", &
      "ocn_current_idir                                            ", "m s-1                                                       ", &
      "ocn_current_jdir                                            ", "m s-1                                                       ", &
      "sea_surface_slope_zonal                                     ", "m m-1                                                       ", &
      "sea_surface_slope_merid                                     ", "m m-1                                                       ", &
      "stress_on_air_ice_zonal                                     ", "N m-2                                                       ", &
      "stress_on_air_ice_merid                                     ", "N m-2                                                       ", &
      "stress_on_air_ocn_zonal                                     ", "N m-2                                                       ", &
      "stress_on_air_ocn_merid                                     ", "N m-2                                                       ", &
      "stress_on_ocn_ice_zonal                                     ", "N m-2                                                       ", &
      "stress_on_ocn_ice_merid                                     ", "N m-2                                                       ", &
      "stress_on_ocn_ice_idir                                      ", "N m-2                                                       ", &
      "stress_on_ocn_ice_jdir                                      ", "N m-2                                                       ", &
      "mixed_layer_depth                                           ", "m                                                           ", &
      "mean_net_lw_flx                                             ", "W m-2                                                       ", &
      "mean_net_sw_flx                                             ", "W m-2                                                       ", &
      "mean_up_lw_flx_ice                                          ", "W m-2                                                       ", &
      "mean_up_lw_flx_ocn                                          ", "W m-2                                                       ", &
      "inst_net_lw_flx                                             ", "W m-2                                                       ", &
      "inst_net_sw_flx                                             ", "W m-2                                                       ", &
      "inst_ir_dir_albedo                                          ", "1                                                           ", &
      "inst_ir_dif_albedo                                          ", "1                                                           ", &
      "inst_vis_dir_albedo                                         ", "1                                                           ", &
      "inst_vis_dif_albedo                                         ", "1                                                           ", &
      "inst_ocn_ir_dir_albedo                                      ", "1                                                           ", &
      "inst_ocn_ir_dif_albedo                                      ", "1                                                           ", &
      "inst_ocn_vis_dir_albedo                                     ", "1                                                           ", &
      "inst_ocn_vis_dif_albedo                                     ", "1                                                           ", &
      "inst_ice_ir_dir_albedo                                      ", "1                                                           ", &
      "inst_ice_ir_dif_albedo                                      ", "1                                                           ", &
      "inst_ice_vis_dir_albedo                                     ", "1                                                           ", &
      "inst_ice_vis_dif_albedo                                     ", "1                                                           ", &
      "inst_land_sea_mask                                          ", "1                                                           ", &
      "inst_temp_height_lowest                                     ", "K                                                           ", &
      "inst_spec_humid_height_lowest                               ", "kg kg-1                                                     ", &
      "humidity_2m                                                 ", "kg kg-1                                                     ", &
      "inst_zonal_wind_height_lowest                               ", "m s-1                                                       ", &
      "inst_merid_wind_height_lowest                               ", "m s-1                                                       ", &
      "inst_pres_height_lowest                                     ", "Pa                                                          ", &
      "inst_height_lowest                                          ", "m                                                           ", &
      "ocean_mask                                                  ", "1                                                           ", &
      "ice_mask                                                    ", "1                                                           ", &
      "land_mask                                                   ", "1                                                           ", &

! special HYCOM exports
! ---------------------
      "surface_downward_eastward_stress                            ", "Pa                                                          ", &
      "surface_downward_northward_stress                           ", "Pa                                                          ", &
      "wind_speed_height10m                                        ", "m s-1                                                       ", &
      "wind_speed_squared_10m                                      ", "m2 s-2                                                      ", &
      "friction_speed                                              ", "m s-1                                                       ", &
      "mean_lat_flx                                                ", "W m-2                                                       ", &
      "mean_sens_flx                                               ", "W m-2                                                       ",&
      "water_flux_into_sea_water                                   ", "kg m-2 s-1                                                  ", &
      "frozen_water_flux_into_sea_water                            ", "kg m-2 s-1                                                  ", &
      "surface_temperature                                         ", "K                                                           ", &
      "air_surface_temperature                                     ", "K                                                           ", &
      "sea_ice_surface_temperature                                 ", "K                                                           ", &
      "temperature_2m                                              ", "K                                                           ", &
      "upward_sea_ice_basal_available_heat_flux                    ", "W m-2                                                       ", &

! special HYCOM imports
! ---------------------
      "sea_ice_area_fraction                                       ", "1                                                           ", &
      "downward_x_stress_at_sea_ice_base                           ", "Pa                                                          ", &
      "downward_y_stress_at_sea_ice_base                           ", "Pa                                                          ", &
      "downward_sea_ice_basal_solar_heat_flux                      ", "W m-2                                                       ", &
      "upward_sea_ice_basal_heat_flux                              ", "W m-2                                                       ", &
      "downward_sea_ice_basal_salt_flux                            ", "kg m-2 s-1                                                  ", &
      "downward_sea_ice_basal_water_flux                           ", "kg m-2 s-1                                                  ", &
      "sea_ice_surface_temperature                                 ", "K                                                           ", &
!     "sea_ice_temperature                                         ", "K                                                           ", &
      "sea_ice_thickness                                           ", "m                                                           ", &
      "sea_ice_x_velocity                                          ", "m s-1                                                       ", &
      "sea_ice_y_velocity                                          ", "m s-1                                                       ", &
      "net_heat_flx_to_ocn                                         ", "W m-2                                                       ", &
      "mean_fresh_water_to_ocean_rate                              ", "kg m-2 s-1                                                  ", &
      "mean_ice_volume                                             ", "m                                                           ", &
      "mean_snow_volume                                            ", "m                                                           ", &

! Mass flux of liquid runoff
! --------------------------
      "Foxx_rofl                                                   ", "kg m-2 s-1                                                  ", &
! Mass flux of frozen runoff
! --------------------------
      "Foxx_rofi                                                   ", "kg m-2 s-1                                                  ", &
! Synonyms for HYCOM fields
! -------------------------
      "So_bldepth                                                  ", "m                                                           ", &
!
      ! DCR - Fields added for Regional Application
      ! ATM-OCN-ICE-LND-HYD
      ! List of exisitng fields
      ! ice_mask, inst_down_lw_flx, inst_down_sw_flx, inst_height_lowest,
      ! inst_merid_wind_height_lowest, inst_pres_height_lowest,
      ! inst_pres_height_surface, inst_spec_humid_height_lowest,
      ! inst_temp_height_lowest, inst_temp_height_surface,
      ! inst_zonal_wind_height_lowest, mean_down_lw_flx, mean_down_sw_flx,
      ! mean_fprec_rate, mean_laten_heat_flx, mean_net_lw_flx, mean_net_sw_flx,
      ! mean_prec_rate, mean_sensi_heat_flx

      "aerodynamic_roughness_length                                ", "m                                                           ", &
      "canopy_moisture_storage                                     ", "kg m-2                                                      ", &
      "carbon_dioxide                                              ", "ppmv                                                        ", &
      "cosine_zenith_angle                                         ", "degree                                                      ", &
      "exchange_coefficient_heat                                   ", "W m-2 K-1                                                   ", &
      "exchange_coefficient_heat_height2m                          ", "W m-2 K-1                                                   ", &
      "exchange_coefficient_moisture_height2m                      ", "kg m-2 s-1 Pa-1                                             ", &
      "inst_wind_speed_height_lowest                               ", "m s-1                                                       ", &
      "mean_cprec_rate                                             ", "kg m-2 s-1                                                  ", &
      "mean_grnd_sensi_heat_flx                                    ", "                                                            ", &
      "mean_laten_heat_flx_kinematic                               ", "Kg m-2 s-1                                                  ", &
      "mean_surface_albedo                                         ", "1                                                           ", &
      "mean_surface_skin_temp                                      ", "K                                                           ", &
      "mixing_ratio_surface                                        ", "kg kg-1                                                     ", &
      "root_moisture                                               ", "kg m-2                                                      ", &
      "saturated_mixing_ratio                                      ", "kg kg-1                                                     ", &
      "surface_snow_area_fraction                                  ", "1                                                           ", &
      "surface_snow_thickness                                      ", "m                                                           ", &
      "surface_snow_melt_flux                                      ", "kg m-2                                                      ", &
      "liquid_water_content_of_surface_snow                        ", "m                                                           ", &
      "soil_depth                                                  ", "m                                                           ", &
      "soil_hydraulic_conductivity_at_saturation                   ", "m s-1                                                       ", &
      "moisture_content_of_soil_layer                              ", "kg m-2                                                      ", &
      "moisture_content_of_soil_layer_1                            ", "kg m-2                                                      ", &
      "moisture_content_of_soil_layer_2                            ", "kg m-2                                                      ", &
      "moisture_content_of_soil_layer_3                            ", "kg m-2                                                      ", &
      "moisture_content_of_soil_layer_4                            ", "kg m-2                                                      ", &
      "soil_porosity                                               ", "1                                                           ", &
      "temperature_of_soil_layer                                   ", "K                                                           ", &
      "temperature_of_soil_layer_1                                 ", "K                                                           ", &
      "temperature_of_soil_layer_2                                 ", "K                                                           ", &
      "temperature_of_soil_layer_3                                 ", "K                                                           ", &
      "temperature_of_soil_layer_4                                 ", "K                                                           ", &
      "soil_temperature_bottom                                     ", "K                                                           ", &
      "soil_type                                                   ", "1                                                           ", &
      "soil_moisture_content                                       ", "kg m-2                                                      ", &
      "subsurface_basin_mask                                       ", "1                                                           ", &
      "subsurface_runoff_flux                                      ", "kg m-2 s-1                                                  ", &
      "surface_microwave_emissivity                                ", "1                                                           ", &
      "surface_runoff_flux                                         ", "kg m-2 s-1                                                  ", &
      "vegetation_type                                             ", "1                                                           ", &
      "volume_fraction_of_frozen_water_in_soil                     ", "m3 m-3                                                      ", &
      "liquid_water_content_of_soil_layer                          ", "kg m-2                                                      ", &
      "liquid_water_content_of_soil_layer_1                        ", "kg m-2                                                      ", &
      "liquid_water_content_of_soil_layer_2                        ", "kg m-2                                                      ", &
      "liquid_water_content_of_soil_layer_3                        ", "kg m-2                                                      ", &
      "liquid_water_content_of_soil_layer_4                        ", "kg m-2                                                      ", &
      "volume_fraction_of_total_water_in_soil                      ", "m3 m-3                                                      ", &
      "volume_fraction_of_total_water_in_soil_at_critical_point    ", "m3 m-3                                                      ", &
      "volume_fraction_of_total_water_in_soil_at_field_capacity    ", "m3 m-3                                                      ", &
      "volume_fraction_of_total_water_in_soil_at_wilting_point     ", "m3 m-3                                                      ", &
      "water_surface_height_above_reference_datum                  ", "m                                                           ", &
      "mean_sensi_heat_flx_atm_into_lnd                            ", "W m-2                                                       ", &
      "mean_laten_heat_flx_atm_into_lnd                            ", "W m-2                                                       ", &

! Fields from and to WW3
! ----------------------
      "eastward_wind_at_10m_height                                 ", "m s-1                                                       ", &
      "northward_wind_at_10m_height                                ", "m s-1                                                       ", &
      "eastward_stokes_drift_current                               ", "m s-1                                                       ", &
      "northward_stokes_drift_current                              ", "m s-1                                                       ", &
      "eastward_wave_bottom_current                                ", "m s-1                                                       ", &
      "northward_wave_bottom_current                               ", "m s-1                                                       ", &
      "wave_bottom_current_radian_frequency                        ", "rad s-1                                                     ", &
      "eastward_wave_radiation_stress_gradient                     ", "Pa                                                          ", &
      "northward_wave_radiation_stress_gradient                    ", "Pa                                                          ", &
      "eastward_wave_radiation_stress                              ", "N m-1                                                       ", &
      "eastward_northward_wave_radiation_stress                    ", "N m-1                                                       ", &
      "northward_wave_radiation_stress                             ", "N m-1                                                       ", &
      "wave_induced_charnock_parameter                             ", "1                                                           ", &
      "wave_z0_roughness_length                                    ", "m                                                           ", &

! Fields from WAM to IPE
! ----------------------
      "northward_wind_neutral                                      ", "m s-1                                                       ", &
      "eastward_wind_neutral                                       ", "m s-1                                                       ", &
      "upward_wind_neutral                                         ", "m s-1                                                       ", &
      "temp_neutral                                                ", "K                                                           ", &
      "inst_temp_height_surface                                    ", "K                                                           ", &
      "inst_pres_height_surface                                    ", "Pa                                                          ", &
      "inst_surface_height                                         ", "m                                                           ", &
      "O_Density                                                   ", "m-3                                                         ", &
      "O2_Density                                                  ", "m-3                                                         ", &
      "N2_Density                                                  ", "m-3                                                         ", &
      "height                                                      ", "km                                                          ", &

! Chemistry fields
! ----------------
      "inst_pres_interface                                         ", "Pa                                                          ", &
      "inst_pres_levels                                            ", "Pa                                                          ", &
      "inst_geop_interface                                         ", "m2 s-2                                                      ", &
      "inst_geop_levels                                            ", "m2 s-2                                                      ", &
      "inst_temp_levels                                            ", "K                                                           ", &
      "inst_zonal_wind_levels                                      ", "m s-1                                                       ", &
      "inst_merid_wind_levels                                      ", "m s-1                                                       ", &
      "inst_omega_levels                                           ", "s s-1s                                                      ", &
      "inst_tracer_mass_frac                                       ", "kg kg-1s                                                    ", &
      "inst_tracer_up_surface_flx                                  ", "kg m-2 s-1                                                  ", &
      "inst_tracer_down_surface_flx                                ", "kg m-2 s-1                                                  ", &
      "inst_tracer_clmn_mass_dens                                  ", "g m-2                                                       ", &
      "inst_tracer_up_surface_flx                                  ", "kg m-2 s-1                                                  ", &
      "inst_tracer_down_surface_flx                                ", "kg m-2 s-1                                                  ", &
      "inst_tracer_clmn_mass_dens                                  ", "g m-2                                                       ", &
      "inst_tracer_anth_biom_flx                                   ", "ug m-2 s-1                                                  ", &
      "inst_pbl_height                                             ", "m                                                           ", &
      "surface_cell_area                                           ", "m2                                                          ", &
      "inst_convective_rainfall_amount                             ", "kg m-2                                                      ", &
      "inst_exchange_coefficient_heat_levels                       ", "W m-2 K-1                                                   ", &
      "inst_friction_velocity                                      ", "s-1                                                         ", &
      "inst_rainfall_amount                                        ", "kg m-2                                                      ", &
      "inst_soil_moisture_content                                  ", "kg m-2                                                      ", &
      "inst_up_sensi_heat_flx                                      ", "W m-2                                                       ", &
      "inst_lwe_snow_thickness                                     ", "m                                                           ", &
      "inst_vegetation_area_frac                                   ", "1                                                           ", &
      "inst_surface_roughness                                      ", "1                                                           ", &
 
! Dummy fields
! ------------
      "dummyfield                                                  ", "1                                                           ", &
      "dummyfield1                                                 ", "1                                                           ", &
      "dummyfield2                                                 ", "1                                                           "  &

       /)
      
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      RC_REG = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      ! Derive from NUOPC_Driver
      call NUOPC_CompDerive(EARTH_GRID_COMP, Driver_routine_SS, rc=RC)
      ESMF_ERR_RETURN(RC, RC_REG)

      ! specializations:

      call NUOPC_CompSpecialize(EARTH_GRID_COMP,                          &
                                specLabel = Driver_label_SetModelServices,&
                                specRoutine=SetModelServices, rc=RC)
      ESMF_ERR_RETURN(RC, RC_REG)
      
      call NUOPC_CompSpecialize(EARTH_GRID_COMP,                          &
                                specLabel=Driver_label_SetRunSequence,    &
                                specRoutine=SetRunSequence, rc=RC)
      ESMF_ERR_RETURN(RC, RC_REG)

      ! The NEMS Earth component is currently the top-level driver and
      ! does not need to coordinate Clocks with its parent.

      call ESMF_MethodRemove(EARTH_GRID_COMP, Driver_label_SetRunClock, rc=RC_REG)
      ESMF_ERR_RETURN(RC, RC_REG)

      call NUOPC_CompSpecialize(EARTH_GRID_COMP,                         &
                                specLabel=Driver_label_SetRunClock,      &
                                specRoutine=NUOPC_NoOp, rc=RC_REG)
      ESMF_ERR_RETURN(RC, RC_REG)
      
      call NUOPC_CompSpecialize(EARTH_GRID_COMP,                         &
                                specLabel=Driver_label_Finalize,         &
                                specRoutine=Finalize, rc=RC)
      ESMF_ERR_RETURN(RC, RC_REG)
      
      ! register an internal initialization method
      call NUOPC_CompSetInternalEntryPoint(EARTH_GRID_COMP, ESMF_METHOD_INITIALIZE, &
                                           phaseLabelList=(/"IPDv04p2"/),           &
                                           userRoutine=ModifyCplLists, rc=rc)
      ESMF_ERR_RETURN(RC, RC_REG)

      ! create, open, and set the config
      config = ESMF_ConfigCreate(rc=RC)
      ESMF_ERR_RETURN(RC, RC_REG)
      call ESMF_ConfigLoadFile(config, "nems.configure", rc=RC)
      ESMF_ERR_RETURN(RC, RC_REG)
      call ESMF_GridCompSet(EARTH_GRID_COMP, config=config, rc=RC)
      ESMF_ERR_RETURN(RC, RC_REG)
      
      ! Added the following Field Dictionary block to the EARTH component level
      ! in order to prevent different dictionary definitions in the lower
      ! components. Doing this here isn't without problems because it
      ! potentially makes the components (ATM & OCN) depend on this environment,
      ! which lowers their transferability to other coupled systems. However,
      ! extending the Field Dictionary is a temporary solution anyway (see the
      ! TODO: below), so this isn't going to stay for ever this way.
      
      ! Extend the NUOPC Field Dictionary to hold required entries.
      !TODO: In the long run this section will not be needed when we have
      !TODO: absorbed the needed standard names into the default dictionary.
      ! -> 20 fields identified as exports by the GSM component

      do nf=1,NumFields
        if (.not.NUOPC_FieldDictionaryHasEntry(trim(Field_Name_unit(1,nf)))) then
          call NUOPC_FieldDictionaryAddEntry(standardName=trim(Field_Name_unit(1,nf)), &
                                             canonicalUnits=trim(Field_Name_unit(2,nf)), rc=rc)

          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
        endif
      enddo
 
! Synonyms for HYCOM fields
! -------------------------
      call NUOPC_FieldDictionarySetSyno(standardNames = (/"surface_downward_eastward_stress",&
                                        "mean_zonal_moment_flx           "/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      call NUOPC_FieldDictionarySetSyno(standardNames = (/"surface_downward_northward_stress",&
                                        "mean_merid_moment_flx            "/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      call NUOPC_FieldDictionarySetSyno(standardNames = (/"mean_lat_flx       ",&
                                        "mean_laten_heat_flx"/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      call NUOPC_FieldDictionarySetSyno(standardNames = (/"mean_sens_flx      ",&
                                        "mean_sensi_heat_flx"/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return


! Fields from and to WW3
! ----------------------

      call NUOPC_FieldDictionarySetSyno(standardNames = (/"eastward_wind_at_10m_height",&
                                                          "inst_zonal_wind_height10m  "/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

      call NUOPC_FieldDictionarySetSyno(standardNames = (/"northward_wind_at_10m_height",&
                                        "inst_merid_wind_height10m   "/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

! For MOM6 and WW3 variables to match: 
      call NUOPC_FieldDictionarySetSyno(standardNames = (/"surface_eastward_sea_water_velocity",&
                                                          "ocn_current_zonal                  "/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      call NUOPC_FieldDictionarySetSyno(standardNames = (/"surface_northward_sea_water_velocity",&
                                                          "ocn_current_merid                   "/), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return


!-----------------------------------------------------------------------
!
      END SUBROUTINE EARTH_REGISTER
!
!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
!

      subroutine SetModelServices(driver, rc)
        type(ESMF_GridComp)  :: driver
        integer, intent(out) :: rc

        ! local variables
        integer                         :: localrc, stat, i, j, petCount
        character(ESMF_MAXSTR)          :: name
        type(WRAP_EARTH_INTERNAL_STATE) :: is
        type(ESMF_GridComp)             :: comp
        type(ESMF_Config)               :: config
        character(len=32), allocatable  :: compLabels(:)
        integer, allocatable            :: petList(:)
        character(len=10)               :: value
        character(len=20)               :: model, prefix
        character(len=160)              :: msg
        integer                         :: petListBounds(2)
        integer                         :: componentCount
        type(NUOPC_FreeFormat)          :: attrFF, fdFF

        rc = ESMF_SUCCESS

! query the Component for info
! ----------------------------
        call ESMF_GridCompGet(driver, name=name, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return

! allocate memory for the internal state and store in Component
! -------------------------------------------------------------
        allocate(is%EARTH_INT_STATE, stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat,      &
          msg="Allocation of internal state memory failed.", &
          line=__LINE__, file=trim(name)//":"//__FILE__, rcToReturn=rc)) return

        call ESMF_GridCompSetInternalState(driver, is, rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
        
! get petCount and config
! -----------------------
        call ESMF_GridCompGet(driver, petCount=petCount, config=config, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
        
! read and ingest free format driver attributes
! ---------------------------------------------
        attrFF = NUOPC_FreeFormatCreate(config,                     &
                                        label="EARTH_attributes::", &
                                        relaxedflag=.true., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return

        call NUOPC_CompAttributeIngest(driver, attrFF, addFlag=.true., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return

        call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
        
! dump the current field dictionary into the Log file
! ---------------------------------------------------
        call ESMF_AttributeGet(driver, name="DumpFieldDictionary", &
                               value=value, defaultValue="false", &
                               convention="NUOPC", purpose="Instance", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return

        if (trim(value) == "true") then
          call ESMF_LogWrite("===>===>===>===> Begin Dumping Field Dictionary <===<===<===<===",&
                             ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return

          call NUOPC_FieldDictionaryEgest(fdFF, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return

          call NUOPC_FreeFormatLog(fdFF, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
          call ESMF_LogWrite("===>===>===>===> Done Dumping Field Dictionary <===<===<===<===", &
                             ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
        endif
        
! determine the generic component labels
! --------------------------------------
        componentCount = ESMF_ConfigGetLen(config, label="EARTH_component_list:", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return

        allocate(compLabels(componentCount), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
            msg="Allocation of compLabels failed.",     &
            line=__LINE__, file=trim(name)//":"//__FILE__, rcToReturn=rc)) return

        call ESMF_ConfigGetAttribute(config, valueList=compLabels, &
                                     label="EARTH_component_list:", count=componentCount, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
        
! determine information for each component and add to the driver
! --------------------------------------------------------------
        do i=1, componentCount
! construct component prefix
! --------------------------
          prefix = trim(compLabels(i))
! read in petList bounds
! ----------------------
          call ESMF_ConfigGetAttribute(config, petListBounds, &
                                       label=trim(prefix)//"_petlist_bounds:", default=-1, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
! handle the default situation
! ----------------------------
          if (petListBounds(1) == -1 .or. petListBounds(2) == -1) then
            petListBounds(1) = 0
            petListBounds(2) = petCount - 1
          endif
! read in model instance name
! ---------------------------
          call ESMF_ConfigGetAttribute(config, model, &
                                       label=trim(prefix)//"_model:", default="none", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
! check that there was a model instance specified
! -----------------------------------------------
          if (trim(model) == "none") then ! Error condition: no model was specified
            write (msg, *) "No model was specified for component: ",trim(prefix)
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
          endif
! set petList for this component
! ------------------------------
          allocate(petList(petListBounds(2)-petListBounds(1)+1))
          do j=petListBounds(1), petListBounds(2)
            petList(j-petListBounds(1)+1) = j ! PETs are 0 based
          enddo
          
          if (trim(model) == "satm") then
#ifdef FRONT_SATM
            call NUOPC_DriverAddComp(driver, trim(prefix), SATM_SS, &
                                     petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
                           "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
#endif
          elseif (trim(model) == "xatm") then
#ifdef FRONT_XATM
            call NUOPC_DriverAddComp(driver, trim(prefix), XATM_SS, &
                                     petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
                           "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
#endif
          elseif (trim(model) == "datawam") then
#ifdef FRONT_DATAWAM
            call NUOPC_DriverAddComp(driver, trim(prefix), DATAWAM_SS, &
                                     petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
                           "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
#endif
          elseif (trim(model) == "gsm") then
#ifdef FRONT_GSM
            call NUOPC_DriverAddComp(driver, trim(prefix), GSM_SS, &
                                     petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
                           "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
#endif
          elseif (trim(model) == "nmmb") then
#ifdef FRONT_NMMB
            call NUOPC_DriverAddComp(driver, trim(prefix), NMMB_SS, &
                                     petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
                           "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
#endif
          elseif (trim(model) == "fv3") then
#ifdef FRONT_FV3
            call NUOPC_DriverAddComp(driver, trim(prefix), FV3_SS, &
                                     petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
                           "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
#endif
          elseif (trim(model) == "datm") then
#ifdef FRONT_DATM
            call NUOPC_DriverAddComp(driver, trim(prefix), DATM_SS, &
                                     petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
                           "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
#endif
          elseif (trim(model) == "socn") then
#ifdef FRONT_SOCN
            call NUOPC_DriverAddComp(driver, trim(prefix), SOCN_SS, &
                                     petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
                           "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
#endif
          elseif (trim(model) == "xocn") then
#ifdef FRONT_XOCN
            call NUOPC_DriverAddComp(driver, trim(prefix), XOCN_SS, &
                                     petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
                           "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
#endif
          elseif (trim(model) == "hycom") then
#ifdef FRONT_HYCOM
            call NUOPC_DriverAddComp(driver, trim(prefix), HYCOM_SS, &
                                     petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
                           "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
#endif
          elseif (trim(model) == "mom5") then
#ifdef FRONT_MOM5
            call NUOPC_DriverAddComp(driver, trim(prefix), MOM5_SS, &
                                     petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
                           "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
#endif
          elseif (trim(model) == "mom6") then
#ifdef FRONT_MOM6
            call NUOPC_DriverAddComp(driver, trim(prefix), MOM6_SS, &
                                     petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
                           "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
#endif
          elseif (trim(model) == "pom") then
#ifdef FRONT_POM
            call NUOPC_DriverAddComp(driver, trim(prefix), POM_SS, &
                                     petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
                           "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
#endif
          elseif (trim(model) == "sice") then
#ifdef FRONT_SICE
            call NUOPC_DriverAddComp(driver, trim(prefix), SICE_SS, &
                                     petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
                           "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
#endif
          elseif (trim(model) == "xice") then
#ifdef FRONT_XICE
            call NUOPC_DriverAddComp(driver, trim(prefix), XICE_SS, &
                                     petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
                           "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
#endif
          elseif (trim(model) == "cice") then
#ifdef FRONT_CICE
            call NUOPC_DriverAddComp(driver, trim(prefix), CICE_SS, &
                                     petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
                           "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
#endif
          elseif (trim(model) == "swav") then
#ifdef FRONT_SWAV
            call NUOPC_DriverAddComp(driver, trim(prefix), SWAV_SS, &
                                     petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
                           "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
#endif
          elseif (trim(model) == "xwav") then
#ifdef FRONT_XWAV
            call NUOPC_DriverAddComp(driver, trim(prefix), XWAV_SS, &
                                     petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
                           "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
#endif
          elseif (trim(model) == "ww3") then
#ifdef FRONT_WW3
            call NUOPC_DriverAddComp(driver, trim(prefix), WW3_SS, &
                                     petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
                           "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
#endif
          elseif (trim(model) == "slnd") then
#ifdef FRONT_SLND
            call NUOPC_DriverAddComp(driver, trim(prefix), SLND_SS, &
                                     petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
                           "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
#endif
          elseif (trim(model) == "xlnd") then
#ifdef FRONT_XLND
            call NUOPC_DriverAddComp(driver, trim(prefix), XLND_SS, &
                                     petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
                           "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
#endif
          elseif (trim(model) == "noah") then
#ifdef FRONT_NOAH
            call NUOPC_DriverAddComp(driver, trim(prefix), NOAH_SS, &
                                     petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
                           "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
#endif
          elseif (trim(model) == "lis") then
#ifdef FRONT_LIS
            call NUOPC_DriverAddComp(driver, trim(prefix), LIS_SS, &
                                     petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
                           "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
#endif
          elseif (trim(model) == "sipm") then
#ifdef FRONT_SIPM
            call NUOPC_DriverAddComp(driver, trim(prefix), SIPM_SS, &
                                     petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
                           "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
#endif
          elseif (trim(model) == "xipm") then
#ifdef FRONT_XIPM
            call NUOPC_DriverAddComp(driver, trim(prefix), XIPM_SS, &
                                     petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
                           "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
#endif
          elseif (trim(model) == "ipe") then
#ifdef FRONT_IPE
            call NUOPC_DriverAddComp(driver, trim(prefix), IPE_SS, &
                                     petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
                           "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
#endif
          elseif (trim(model) == "dataipe") then
#ifdef FRONT_DATAIPE
            call NUOPC_DriverAddComp(driver, trim(prefix), DATAIPE_SS, &
                                     petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
                           "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, &
                                  file=__FILE__, rcToReturn=rc)
            return
#endif
          elseif (trim(model) == "shyd") then
#ifdef FRONT_SHYD
            call NUOPC_DriverAddComp(driver, trim(prefix), SHYD_SS, &
                                     petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
                           "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, &
                                  file=__FILE__, rcToReturn=rc)
            return
#endif
          elseif (trim(model) == "xhyd") then
#ifdef FRONT_XHYD
            call NUOPC_DriverAddComp(driver, trim(prefix), XHYD_SS, &
                                     petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
              "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
#endif
          elseif (trim(model) == "wrfhydro") then
#ifdef FRONT_WRFHYDRO
            call NUOPC_DriverAddComp(driver, trim(prefix), WRFHYDRO_SS, &
                                     petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
                           "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
#endif
          elseif (trim(model) == "gsdchem") then
#ifdef FRONT_GSDCHEM
            call NUOPC_DriverAddComp(driver, trim(prefix), GSDCHEM_SS, &
                                     petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
              "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
#endif
          ! - Two mediator choices currently built into NEMS from internal
          elseif (trim(model) == "nems") then
            call NUOPC_DriverAddComp(driver, trim(prefix), MED_SS, &
                                     petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
          elseif (trim(model) == "spaceweather") then
            call NUOPC_DriverAddComp(driver, trim(prefix), MEDSW_SS, &
                                     petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
          else
            ! Error condition: unknown model requested
            write (msg, *) "The requested model '", trim(model), &
                           "' is an invalid choice!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, file=__FILE__, rcToReturn=rc)
            return
          endif
          
! read and ingest free format component attributes
! ------------------------------------------------
          attrFF = NUOPC_FreeFormatCreate(config, &
                                          label=trim(prefix)//"_attributes::", &
                                          relaxedflag=.true., rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return

          call NUOPC_CompAttributeIngest(comp, attrFF, addFlag=.true., rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return

          call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
          
! clean-up
! --------
          deallocate(petList)
          
        enddo

#if ESMF_VERSION_MAJOR < 8        
!TODOgjt: REMOVE THIS BLOCK ONCE SHOWN TO WORK WITHOUT
! SetServices for Connectors
! --------------------------
        call SetFromConfig(driver, mode="setServicesConnectors", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
#endif
! clean-up
! --------
        deallocate(compLabels)
        
      end subroutine

     !-----------------------------------------------------------------------------
  
      subroutine SetRunSequence(driver, rc)
        type(ESMF_GridComp)  :: driver
        integer, intent(out) :: rc
    
! local variables
        character(ESMF_MAXSTR)          :: name
#if ESMF_VERSION_MAJOR >= 8
        type(ESMF_Config)               :: config
        type(NUOPC_FreeFormat)          :: runSeqFF
#endif 

        rc = ESMF_SUCCESS

! query the Component for info
! ----------------------------
        call ESMF_GridCompGet(driver, name=name, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return

#if ESMF_VERSION_MAJOR >= 8
! read free format run sequence from config
! -----------------------------------------
        call ESMF_GridCompGet(driver, config=config, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return

        runSeqFF = NUOPC_FreeFormatCreate(config, label="runSeq::", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return

! ingest FreeFormat run sequence
! ------------------------------
        call NUOPC_DriverIngestRunSequence(driver, runSeqFF, &
                                           autoAddConnectors=.true., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
#else
! access runSeq in the config
! ---------------------------
        call SetFromConfig(driver, mode="setRunSequence", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
#endif

! Diagnostic output
! -----------------
        if(verbose_diagnostics()) then
           call NUOPC_DriverPrint(driver, orderflag=.true., rc=rc)
           if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
        endif
    
      end subroutine
    
      !-----------------------------------------------------------------------------

#if ESMF_VERSION_MAJOR < 8
!TODOgjt: REMOVE THIS BLOCK ONCE SHOWN TO WORK WITHOUT
      subroutine SetFromConfig(driver, mode, rc)
        type(ESMF_GridComp)   :: driver
        character(len=*)      :: mode
        integer, intent(out)  :: rc
    
! local variables
! ---------------
        character(ESMF_MAXSTR)          :: name
        type(ESMF_Config)               :: config
        integer                         :: lineCount, columnCount, i, slotCount
        integer, allocatable            :: count(:)
        character(len=32), allocatable  :: line(:)
        character(len=32)               :: tempString
        logical                         :: phaseFlag
        integer                         :: level, slot, slotHWM
        real(ESMF_KIND_R8)              :: seconds
        integer, allocatable            :: slotStack(:)
        type(ESMF_TimeInterval)         :: timeStep
        type(ESMF_Clock)                :: internalClock, subClock
        character(len=60), allocatable  :: connectorInstance(:)
        integer                         :: connectorCount, j
        type(ESMF_CplComp)              :: conn

        character(len=ESMF_MAXSTR)      :: msgString
        character(len=10)               :: value

! can set to 'max' to recover intro/extro CurrGarbInfo for all connectors 
! -----------------------------------------------------------------------
        character(len=10)               :: defaultVerbosity = "0"
        !character(len=10)              :: defaultVerbosity = "max"

        rc = ESMF_SUCCESS
    
! query the Component for info
! ----------------------------
        call ESMF_GridCompGet(driver, name=name, config=config, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return

! reset config to beginning of runSeq:: block
! -------------------------------------------
        call ESMF_ConfigFindLabel(config, label="runSeq::", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return

        call ESMF_ConfigGetDim(config, lineCount, columnCount, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
    
        allocate(count(lineCount))
    
        if (trim(mode)=="setServicesConnectors") then
          allocate(connectorInstance(lineCount))  ! max number of connectors
          connectorCount = 0                      ! reset
          write(msgString,'(a,i6)')'max number of connectors ',lineCount
          call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
        endif
    
! reset config to beginning of runSeq:: block
! -------------------------------------------
        call ESMF_ConfigFindLabel(config, label="runSeq::", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return

! determine number of entries on each line
! ----------------------------------------
        do i=1, lineCount
          call ESMF_ConfigNextLine(config)
          count(i) = ESMF_ConfigGetLen(config) ! entries on line i
        enddo
    
! reset config to beginning of runSeq:: block
! -------------------------------------------
        call ESMF_ConfigFindLabel(config, label="runSeq::", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return

! read each line and determine slotCount
! --------------------------------------
        slotCount = 0
        do i=1, lineCount
          call ESMF_ConfigNextLine(config)
          allocate(line(count(i)))
          call ESMF_ConfigGetAttribute(config, line, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
      
! process the configuration line
! ------------------------------
          if (size(line) == 1) then
            if (index(trim(line(1)),"@") == 1) then
              slotCount = slotCount + 1
            endif
          elseif ((size(line) == 3) .or. (size(line) == 4)) then
            if (trim(mode) == "setServicesConnectors") then
              ! a connector if the second element is "->"
              if (trim(line(2)) /= "->") then
                call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, &
                  msg="Configuration line incorrectly formatted.", &
                  line=__LINE__, file=__FILE__)
                return
              else
! found a connector entry, see if it is the first instance
! -                  -------------------------------------
                do j=1, connectorCount
                  if (trim(connectorInstance(j)) == &
                      trim(line(1))//trim(line(2))//trim(line(3))) exit
                enddo
                if (j > connectorCount) then
! this is a new Connector instance
! -    -------------------------------------
                  connectorCount = j
                  connectorInstance(j) = trim(line(1))//trim(line(2))//trim(line(3))
                  write(msgString,'(a,i4,a,i4,4a)')'Connector j = ',j,             &
                                                   ' line number ', i,             &
                                                   '  ',trim(connectorInstance(j)),&
                                                   ' Verbosity = ',trim(defaultVerbosity)
                  call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
! SetServices for new Connector instance
! --------------------------------------
                  call NUOPC_DriverAddComp(driver, &
                                           srcCompLabel=trim(line(1)), dstCompLabel=trim(line(3)), &
                                           compSetServicesRoutine=conSS, comp=conn, rc=rc)
                  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return

                  call NUOPC_CompAttributeSet(conn, name="Verbosity", value=defaultVerbosity, &
                                              rc=rc)
                  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return

                  if (size(line) == 4) then
! there are additional connection options specified
! -> set as Attribute for now on the connector object
! --------------------------------------
                    call ESMF_AttributeSet(conn, name="ConnectionOptions", &
                                           value=trim(line(4)), rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
                  endif
                endif
              endif
            endif
          endif
! clean-up
          deallocate(line)
        enddo
        slotCount = (slotCount+1) / 2
        slotCount = max(slotCount, 1) ! at least one slot
    
        if (trim(mode) == "setRunSequence") then
    
          allocate(slotStack(slotCount))

! Replace the default RunSequence with a customized one
          call NUOPC_DriverNewRunSequence(driver, slotCount=slotCount, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

! Get driver intenalClock
          call ESMF_GridCompGet(driver, clock=internalClock, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

! reset config to beginning of runSeq:: block
          call ESMF_ConfigFindLabel(config, label="runSeq::", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return

          level   = 0
          slot    = 0
          slotHWM = 0
          do i=1, lineCount
            call ESMF_ConfigNextLine(config)
            allocate(line(count(i)))
            call ESMF_ConfigGetAttribute(config, line, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
        
! process the configuration line
            if ((size(line) < 1) .or. (size(line) > 4)) then
              call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD,   &
                msg="Configuration line incorrectly formatted.", line=__LINE__, file=__FILE__)
              return
            elseif (size(line) == 1) then
! either a model or a time step indicator
              if (index(trim(line(1)),"@") == 1) then
! time step indicator
                tempString = trim(line(1))
                if (len(trim(tempString)) > 1) then
! entering new time loop level
                  level = level + 1
                  slotStack(level) = slot
                  slot    = slotHWM + 1
                  slotHWM = slotHWM + 1
                  read(tempString(2:len(tempString)), *) seconds
!                 print *, "found time step indicator: ", seconds

                  call ESMF_TimeIntervalSet(timeStep, s_r8=seconds, rc=rc)
                  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

                  if (slot == 1) then ! Set the timeStep of the internalClock
                    call ESMF_ClockSet(internalClock, timeStep=timeStep, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

                  else              ! Insert the link to a new slot, and set the timeStep
                    call NUOPC_DriverAddRunElement(driver, slot=slotStack(level), &
                                                   linkSlot=slot, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return

                    subClock = ESMF_ClockCreate(internalClock, rc=rc)  ! make a copy first
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return

                    call ESMF_ClockSet(subClock, timeStep=timeStep, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return

                    call NUOPC_DriverSetRunSequence(driver, slot=slot, &
                                                    clock = subClock, rc=rc)
                    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
                  endif
                else                ! exiting time loop level
                  slot  = slotStack(level)
                  level = level - 1
                endif
              else                  ! model
                slot = max(slot, 1) ! model outside of a time loop
                call NUOPC_DriverAddRunElement(driver, slot=slot, &
                                               compLabel=trim(line(1)), rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
              endif
            elseif (size(line) == 2) then ! a model with a specific phase label
              call NUOPC_DriverAddRunElement(driver, slot=slot,       &
                                             compLabel=trim(line(1)), &
                                             phaseLabel=trim(line(2)), rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

            elseif ((size(line) == 3) .or. (size(line) == 4)) then
! a connector if the second element is "->", with options if 4th part
! -------------------------------------------------------------------
              if (trim(line(2)) /= "->") then
                call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, &
                  msg="Configuration line incorrectly formatted.", line=__LINE__, file=__FILE__)
                return
              endif
              call NUOPC_DriverAddRunElement(driver, slot=slot,          &
                                             srcCompLabel=trim(line(1)), &
                                             dstCompLabel=trim(line(3)), rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
            endif    
        
            deallocate(line)    ! clean-up
          enddo
          deallocate(slotStack) ! clean-up
        endif

        deallocate(count)       ! clean-up
        if (trim(mode)=="setServicesConnectors") then
          deallocate(connectorInstance)
        endif

      end subroutine
#endif

      !-----------------------------------------------------------------------------

      subroutine Finalize(driver, rc)
        type(ESMF_GridComp)  :: driver
        integer, intent(out) :: rc
    
! local variables
        integer                         :: localrc, stat
        type(WRAP_EARTH_INTERNAL_STATE) :: is
        logical                         :: existflag
        character(ESMF_MAXSTR)          :: name

        rc = ESMF_SUCCESS

! query the Component for info
        call ESMF_GridCompGet(driver, name=name, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
    
! query Component for this internal State
        nullify(is%EARTH_INT_STATE)
        call ESMF_GridCompGetInternalState(driver, is, rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=trim(name)//":"//__FILE__)) return
      
! deallocate internal state memory
        deallocate(is%EARTH_INT_STATE, stat=stat)
        if (ESMF_LogFoundDeallocError(statusToCheck=stat,      &
          msg="Deallocation of internal state memory failed.", &
          line=__LINE__, file=trim(name)//":"//__FILE__, rcToReturn=rc)) return
      
      end subroutine
      
      !-----------------------------------------------------------------------------
  
      recursive subroutine ModifyCplLists(driver, importState, exportState, clock, rc)
        type(ESMF_GridComp)  :: driver
        type(ESMF_State)     :: importState, exportState
        type(ESMF_Clock)     :: clock
        integer, intent(out) :: rc

        character(len=160)              :: name, msg
        type(ESMF_CplComp), pointer     :: connectorList(:)
        integer                         :: i, j, cplListSize
        character(len=160), allocatable :: cplList(:)
        character(len=160)              :: value
        type(WRAP_EARTH_INTERNAL_STATE) :: is

        rc = ESMF_SUCCESS
    
! query Component for this internal State

        nullify(is%EARTH_INT_STATE)
        call ESMF_GridCompGetInternalState(driver, is, rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return

        call ESMF_LogWrite("Driver is in ModifyCplLists()", ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    
        nullify(connectorList)
        call NUOPC_DriverGetComp(driver, compList=connectorList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
    
        write (msg,*) "Found ", size(connectorList), " Connectors."// &
                      " Modifying CplList Attribute...."
        call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
      
        do i=1, size(connectorList)
! query Connector i for its name
          call ESMF_CplCompGet(connectorList(i), name=name, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
! access CplList for Connector i
          call NUOPC_CompAttributeGet(connectorList(i), name="CplList", &
                                      itemCount=cplListSize, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
          if (cplListSize>0) then
            allocate(cplList(cplListSize))
            call NUOPC_CompAttributeGet(connectorList(i), name="CplList", &
                                        valueList=cplList, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
! go through all of the entries in the cplList and add options
            do j=1, cplListSize
              cplList(j) = trim(cplList(j))//":DumpWeights=true"
              cplList(j) = trim(cplList(j))//":SrcTermProcessing=1:TermOrder=SrcSeq"
! add connection options read in from configuration file
              call ESMF_AttributeGet(connectorList(i), name="ConnectionOptions", &
                                     value=value, defaultValue="", rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
              cplList(j) = trim(cplList(j))//trim(value)
            enddo
! store the modified cplList in CplList attribute of connector i
            call NUOPC_CompAttributeSet(connectorList(i), name="CplList", valueList=cplList, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, line=__LINE__, file=__FILE__)) return
            deallocate(cplList)
          endif
        enddo
      
        deallocate(connectorList)
    
      end subroutine

!
!-----------------------------------------------------------------------
!
      END MODULE module_EARTH_GRID_COMP
!
!-----------------------------------------------------------------------
