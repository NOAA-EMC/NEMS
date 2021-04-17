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
!          |    (MOM6, HYCOM, etc.)
!          |
!          CORE component (FV3, etc.)
!
!-----------------------------------------------------------------------
!
      USE ESMF

      use NUOPC
      use NUOPC_Driver, &
        Driver_routine_SS             => SetServices, &
        Driver_label_SetModelServices => label_SetModelServices, &
        Driver_label_SetRunSequence   => label_SetRunSequence, &
        Driver_label_SetRunClock      => label_SetRunClock, &
        Driver_label_Finalize         => label_Finalize
      use NUOPC_Connector, only: conSS => SetServices
  ! - Handle build time ATM options:
#ifdef FRONT_FV3
      use FRONT_FV3,        only: FV3_SS   => SetServices
#endif
#ifdef FRONT_DATM
      use FRONT_DATM,       only: DATM_SS  => SetServices
#endif
  ! - Handle build time OCN options:
#ifdef FRONT_HYCOM
      use FRONT_HYCOM,      only: HYCOM_SS  => SetServices
#endif
#ifdef FRONT_MOM6
      use FRONT_MOM6,       only: MOM6_SS   => SetServices
#endif
  ! - Handle build time ICE options:
#ifdef FRONT_CICE6
      use FRONT_CICE6,      only: CICE6_SS => SetServices
#endif
  ! - Handle build time WAV options:
#ifdef FRONT_WW3
      use FRONT_WW3,        only: WW3_SS  => SetServices
#endif
  ! - Handle build time LND options:
#ifdef FRONT_NOAH
      use FRONT_NOAH,       only: NOAH_SS  => SetServices
#endif
#ifdef FRONT_LIS
      use FRONT_LIS,        only: LIS_SS   => SetServices
#endif
  ! - Handle build time IPM options:
#ifdef FRONT_IPE
      use FRONT_IPE,        only: IPE_SS   => SetServices
#endif
  ! - Handle build time GSDCHEM options:
#ifdef FRONT_GSDCHEM
      use FRONT_GSDCHEM,    only: GSDCHEM_SS  => SetServices
#endif
  ! - Mediator
#ifdef FRONT_CMEPS
      use MED,              only: MED_SS     => SetServices 
#endif

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
           flag_verbose_diagnostics=set
        endif
        verbose_diagnostics=flag_verbose_diagnostics
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
      INTEGER :: RC
      type(ESMF_Config)             :: config
      
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
      ESMF_ERR_RETURN(RC,RC_REG)

      ! specializations:

      call NUOPC_CompSpecialize(EARTH_GRID_COMP, &
        specLabel=Driver_label_SetModelServices, specRoutine=SetModelServices, &
        rc=RC)
      ESMF_ERR_RETURN(RC,RC_REG)
      
      call NUOPC_CompSpecialize(EARTH_GRID_COMP, &
        specLabel=Driver_label_SetRunSequence, specRoutine=SetRunSequence, &
        rc=RC)
      ESMF_ERR_RETURN(RC,RC_REG)

#ifndef JEDI_DRIVER
      ! The NEMS Earth component is currently the top-level driver and
      ! does not need to coordinate Clocks with its parent.
      call ESMF_MethodRemove(EARTH_GRID_COMP, Driver_label_SetRunClock, rc=RC_REG)
      ESMF_ERR_RETURN(RC,RC_REG)
      call NUOPC_CompSpecialize(EARTH_GRID_COMP, &
        specLabel=Driver_label_SetRunClock, specRoutine=NUOPC_NoOp, rc=RC_REG)
      ESMF_ERR_RETURN(RC,RC_REG)
#endif
#if 0
      call NUOPC_CompSpecialize(EARTH_GRID_COMP, &
        specLabel=Driver_label_Finalize, specRoutine=Finalize, &
        rc=RC)
      ESMF_ERR_RETURN(RC,RC_REG)
#endif
      
      ! register an internal initialization method
      call NUOPC_CompSetInternalEntryPoint(EARTH_GRID_COMP, ESMF_METHOD_INITIALIZE, &
        phaseLabelList=(/"IPDv04p2"/), userRoutine=ModifyCplLists, rc=rc)
      ESMF_ERR_RETURN(RC,RC_REG)

      ! create, open, and set the config
      config = ESMF_ConfigCreate(rc=RC)
      ESMF_ERR_RETURN(RC,RC_REG)
      call ESMF_ConfigLoadFile(config, "nems.configure", rc=RC)
      ESMF_ERR_RETURN(RC,RC_REG)
      call ESMF_GridCompSet(EARTH_GRID_COMP, config=config, rc=RC)
      ESMF_ERR_RETURN(RC,RC_REG)

      ! Load the required entries from the fd_nems.yaml file
      call NUOPC_FieldDictionarySetup("fd_nems.yaml", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

!-----------------------------------------------------------------------
!
      END SUBROUTINE EARTH_REGISTER
!
!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
!

      subroutine SetModelServices(driver, rc)
#ifdef CMEPS
        use med_internalstate_mod , only : med_id
#endif
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
#ifdef CMEPS
        logical                         :: read_restart
        character(ESMF_MAXSTR)          :: cvalue
        character(len=5)                :: inst_suffix
        logical                         :: isPresent
#endif
        rc = ESMF_SUCCESS

        ! query the Component for info
        call ESMF_GridCompGet(driver, name=name, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

        ! allocate memory for the internal state and store in Component
        allocate(is%EARTH_INT_STATE, stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg="Allocation of internal state memory failed.", &
          line=__LINE__, file=trim(name)//":"//__FILE__, rcToReturn=rc)) &
          return  ! bail out
        call ESMF_GridCompSetInternalState(driver, is, rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
        
        ! get petCount and config
        call ESMF_GridCompGet(driver, petCount=petCount, config=config, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
        
        ! read and ingest free format driver attributes
        attrFF = NUOPC_FreeFormatCreate(config, label="EARTH_attributes::", &
          relaxedflag=.true., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
        call NUOPC_CompAttributeIngest(driver, attrFF, addFlag=.true., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
        call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
        
        ! dump the current field dictionary into the Log file
        call ESMF_AttributeGet(driver, name="DumpFieldDictionary", &
          value=value, defaultValue="false", &
          convention="NUOPC", purpose="Instance", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
        if (trim(value)=="true") then
          call ESMF_LogWrite( &
            "===>===>===>===> Begin Dumping Field Dictionary <===<===<===<===",&
            ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
          call NUOPC_FieldDictionaryEgest(fdFF, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
          call NUOPC_FreeFormatLog(fdFF, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
          call ESMF_LogWrite( &
            "===>===>===>===> Done Dumping Field Dictionary <===<===<===<===", &
            ESMF_LOGMSG_INFO, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
        endif
        
        ! determine the generic component labels
        componentCount = ESMF_ConfigGetLen(config, &
          label="EARTH_component_list:", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
        allocate(compLabels(componentCount), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, &
          msg="Allocation of compLabels failed.", &
          line=__LINE__, file=trim(name)//":"//__FILE__, rcToReturn=rc)) &
          return  ! bail out
        call ESMF_ConfigGetAttribute(config, valueList=compLabels, &
          label="EARTH_component_list:", count=componentCount, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

#ifdef CMEPS
        inst_suffix = ""

        ! obtain driver attributes (for CMEPS)
        call ReadAttributes(driver, config, "DRIVER_attributes::", formatprint=.true., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

        call ReadAttributes(driver, config, "ALLCOMP_attributes::", formatprint=.true., rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
#endif

        ! determine information for each component and add to the driver
        do i=1, componentCount
          ! construct component prefix
          prefix=trim(compLabels(i))
          ! read in petList bounds
          call ESMF_ConfigGetAttribute(config, petListBounds, &
            label=trim(prefix)//"_petlist_bounds:", default=-1, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
          ! handle the default situation
          if (petListBounds(1)==-1 .or. petListBounds(2)==-1) then
            petListBounds(1) = 0
            petListBounds(2) = petCount - 1
          endif
          ! read in model instance name
          call ESMF_ConfigGetAttribute(config, model, &
            label=trim(prefix)//"_model:", default="none", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
          ! check that there was a model instance specified
          if (trim(model) == "none") then
            ! Error condition: no model was specified
            write (msg, *) "No model was specified for component: ",trim(prefix)
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, &
              file=__FILE__, rcToReturn=rc)
            return  ! bail out
          endif
          ! set petList for this component
          allocate(petList(petListBounds(2)-petListBounds(1)+1))
          do j=petListBounds(1), petListBounds(2)
            petList(j-petListBounds(1)+1) = j ! PETs are 0 based
          enddo

          if (trim(model) == "fv3") then
#ifdef FRONT_FV3
            call NUOPC_DriverAddComp(driver, trim(prefix), FV3_SS, &
              petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
              "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, &
              file=__FILE__, rcToReturn=rc)
            return  ! bail out
#endif
          elseif (trim(model) == "datm") then
#ifdef FRONT_DATM
            call NUOPC_DriverAddComp(driver, trim(prefix), DATM_SS, &
              petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
              "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, &
              file=__FILE__, rcToReturn=rc)
            return  ! bail out
#endif
          elseif (trim(model) == "hycom") then
#ifdef FRONT_HYCOM
            call NUOPC_DriverAddComp(driver, trim(prefix), HYCOM_SS, &
              petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
              "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, &
              file=__FILE__, rcToReturn=rc)
            return  ! bail out
#endif
          elseif (trim(model) == "mom6") then
#ifdef FRONT_MOM6
            call NUOPC_DriverAddComp(driver, trim(prefix), MOM6_SS, &
              petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
              "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, &
              file=__FILE__, rcToReturn=rc)
            return  ! bail out
#endif
          elseif (trim(model) == "cice6") then
#ifdef FRONT_CICE6
            call NUOPC_DriverAddComp(driver, trim(prefix), CICE6_SS, &
              petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
              "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, &
              file=__FILE__, rcToReturn=rc)
            return  ! bail out
#endif
          elseif (trim(model) == "ww3") then
#ifdef FRONT_WW3
            call NUOPC_DriverAddComp(driver, trim(prefix), WW3_SS, &
              petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
              "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, &
              file=__FILE__, rcToReturn=rc)
            return  ! bail out
#endif
          elseif (trim(model) == "noah") then
#ifdef FRONT_NOAH
            call NUOPC_DriverAddComp(driver, trim(prefix), NOAH_SS, &
              petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
              "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, &
              file=__FILE__, rcToReturn=rc)
            return  ! bail out
#endif
          elseif (trim(model) == "lis") then
#ifdef FRONT_LIS
            call NUOPC_DriverAddComp(driver, trim(prefix), LIS_SS, &
              petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
              "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, &
              file=__FILE__, rcToReturn=rc)
            return  ! bail out
#endif
          elseif (trim(model) == "gsdchem") then
#ifdef FRONT_GSDCHEM
            call NUOPC_DriverAddComp(driver, trim(prefix), GSDCHEM_SS, &
              petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//__FILE__)) return  !  bail out
#else
            write (msg, *) "Model '", trim(model), "' was requested, "// &
              "but is not available in the executable!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, &
              file=__FILE__, rcToReturn=rc)
            return  ! bail out
#endif
          elseif (trim(model) == "cmeps") then
#ifdef FRONT_CMEPS
            med_id = i+1
            call NUOPC_DriverAddComp(driver, trim(prefix), MED_SS, &
              petList=petList, comp=comp, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
#endif
          else
            ! Error condition: unknown model requested
            write (msg, *) "The requested model '", trim(model), &
              "' is an invalid choice!"
            call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msg, line=__LINE__, &
              file=__FILE__, rcToReturn=rc)
            return  ! bail out
          endif
          
          ! read and ingest free format component attributes
          attrFF = NUOPC_FreeFormatCreate(config, &
            label=trim(prefix)//"_attributes::", relaxedflag=.true., rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
          call NUOPC_CompAttributeIngest(comp, attrFF, addFlag=.true., rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
          call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
          
          ! clean-up
          deallocate(petList)

#ifdef CMEPS
        ! Perform restarts if appropriate
        call InitRestart(driver, rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

        call AddAttributes(comp, driver, config, i+1, trim(prefix), inst_suffix, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
#endif
        enddo

#if ESMF_VERSION_MAJOR < 8
!TODOgjt: REMOVE THIS BLOCK ONCE SHOWN TO WORK WITHOUT
        ! SetServices for Connectors
        call SetFromConfig(driver, mode="setServicesConnectors", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
#endif

        ! clean-up
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
    call ESMF_GridCompGet(driver, name=name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

#if ESMF_VERSION_MAJOR >= 8
    ! read free format run sequence from config
    call ESMF_GridCompGet(driver, config=config, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    runSeqFF = NUOPC_FreeFormatCreate(config, label="runSeq::", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! ingest FreeFormat run sequence
    call NUOPC_DriverIngestRunSequence(driver, runSeqFF, &
      autoAddConnectors=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
#else
    ! access runSeq in the config
    call SetFromConfig(driver, mode="setRunSequence", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
#endif
    
    ! Diagnostic output
    if(verbose_diagnostics()) then
       call NUOPC_DriverPrint(driver, orderflag=.true., rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
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

    character(len=ESMF_MAXSTR) :: msgString
    character(len=10)          :: value

    !can set to 'max' to recover intro/extro CurrGarbInfo for
    !all connectors
    character(len=10)          :: defaultVerbosity = "0"
    !character(len=10)          :: defaultVerbosity = "max"

    rc = ESMF_SUCCESS
    
    ! query the Component for info
    call ESMF_GridCompGet(driver, name=name, config=config, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! reset config to beginning of runSeq:: block
    call ESMF_ConfigFindLabel(config, label="runSeq::", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    call ESMF_ConfigGetDim(config, lineCount, columnCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    
    allocate(count(lineCount))
    
    if (trim(mode)=="setServicesConnectors") then
      allocate(connectorInstance(lineCount))  ! max number of connectors
      connectorCount = 0 ! reset
    write(msgString,'(a,i6)')'max number of connectors ',lineCount
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    endif
    
    ! reset config to beginning of runSeq:: block
    call ESMF_ConfigFindLabel(config, label="runSeq::", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! determine number of entries on each line
    do i=1, lineCount
      call ESMF_ConfigNextLine(config)
      count(i) = ESMF_ConfigGetLen(config) ! entries on line i
    enddo
    
    ! reset config to beginning of runSeq:: block
    call ESMF_ConfigFindLabel(config, label="runSeq::", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

    ! read each line and determine slotCount
    slotCount = 0
    do i=1, lineCount
      call ESMF_ConfigNextLine(config)
      allocate(line(count(i)))
      call ESMF_ConfigGetAttribute(config, line, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
      
      ! process the configuration line
      if (size(line) == 1) then
        if (index(trim(line(1)),"@") == 1) then
          slotCount = slotCount + 1
        endif
      elseif ((size(line) == 3) .or. (size(line) == 4)) then
        if (trim(mode)=="setServicesConnectors") then
          ! a connector if the second element is "->"
          if (trim(line(2)) /= "->") then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, &
              msg="Configuration line incorrectly formatted.", &
              line=__LINE__, &
              file=__FILE__)
            return  ! bail out
          else
            ! found a connector entry, see if it is the first instance
            do j=1, connectorCount
              if (trim(connectorInstance(j)) == &
                trim(line(1))//trim(line(2))//trim(line(3))) exit
            enddo
            if (j>connectorCount) then
              ! this is a new Connector instance
              connectorCount = j
              connectorInstance(j) = trim(line(1))//trim(line(2))//trim(line(3))
              write(msgString,'(a,i4,a,i4,4a)')'Connector j = ',j,&
                                                ' line number ', i,&
                                                '  ',trim(connectorInstance(j)),&
                                                ' Verbosity = ',trim(defaultVerbosity)
              call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
              ! SetServices for new Connector instance
              call NUOPC_DriverAddComp(driver, &
                srcCompLabel=trim(line(1)), dstCompLabel=trim(line(3)), &
                compSetServicesRoutine=conSS, comp=conn, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail
              call NUOPC_CompAttributeSet(conn, name="Verbosity", value=defaultVerbosity, &
                rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail
              if (size(line) == 4) then
                ! there are additional connection options specified
                ! -> set as Attribute for now on the connector object
                call ESMF_AttributeSet(conn, name="ConnectionOptions", &
                  value=trim(line(4)), rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail
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
    
    if (trim(mode)=="setRunSequence") then
    
      allocate(slotStack(slotCount))

      ! Replace the default RunSequence with a customized one
      call NUOPC_DriverNewRunSequence(driver, slotCount=slotCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! Get driver intenalClock
      call ESMF_GridCompGet(driver, clock=internalClock, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! reset config to beginning of runSeq:: block
      call ESMF_ConfigFindLabel(config, label="runSeq::", rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out

      level = 0
      slot = 0
      slotHWM = 0
      do i=1, lineCount
        call ESMF_ConfigNextLine(config)
        allocate(line(count(i)))
        call ESMF_ConfigGetAttribute(config, line, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
        
        ! process the configuration line
        if ((size(line) < 1) .or. (size(line) > 4)) then
          call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, &
            msg="Configuration line incorrectly formatted.", &
            line=__LINE__, &
            file=__FILE__)
          return  ! bail out
        elseif (size(line) == 1) then
          ! either a model or a time step indicator
          if (index(trim(line(1)),"@") == 1) then
            ! time step indicator
            tempString=trim(line(1))
            if (len(trim(tempString)) > 1) then
              ! entering new time loop level
              level = level + 1
              slotStack(level)=slot
              slot = slotHWM + 1
              slotHWM = slotHWM + 1
              read(tempString(2:len(tempString)), *) seconds
              !print *, "found time step indicator: ", seconds
              call ESMF_TimeIntervalSet(timeStep, s_r8=seconds, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              if (slot==1) then
                ! Set the timeStep of the internalClock
                call ESMF_ClockSet(internalClock, timeStep=timeStep, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, &
                  file=__FILE__)) &
                  return  ! bail out
              else
                ! Insert the link to a new slot, and set the timeStep
                call NUOPC_DriverAddRunElement(driver, slot=slotStack(level), &
                  linkSlot=slot, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
                subClock = ESMF_ClockCreate(internalClock, rc=rc)  ! make a copy first
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
                call ESMF_ClockSet(subClock, timeStep=timeStep, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
                call NUOPC_DriverSetRunSequence(driver, slot=slot, &
                  clock=subClock, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
              endif
            else
              ! exiting time loop level
              slot = slotStack(level)
              level = level - 1
            endif
          else
            ! model
            slot = max(slot, 1) ! model outside of a time loop
            call NUOPC_DriverAddRunElement(driver, slot=slot, &
              compLabel=trim(line(1)), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
          endif
        elseif (size(line) == 2) then
          ! a model with a specific phase label
          call NUOPC_DriverAddRunElement(driver, slot=slot, &
            compLabel=trim(line(1)), phaseLabel=trim(line(2)), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        elseif ((size(line) == 3) .or. (size(line) == 4)) then
          ! a connector if the second element is "->", with options if 4th part
          if (trim(line(2)) /= "->") then
            call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_BAD, &
              msg="Configuration line incorrectly formatted.", &
              line=__LINE__, &
              file=__FILE__)
            return  ! bail out
          endif
          call NUOPC_DriverAddRunElement(driver, slot=slot, &
            srcCompLabel=trim(line(1)), dstCompLabel=trim(line(3)), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        endif
        
        ! clean-up
        deallocate(line)
      enddo
      ! clean-up
      deallocate(slotStack)
    endif

    ! clean-up
    deallocate(count)
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
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
    
    ! query Component for this internal State
    nullify(is%EARTH_INT_STATE)
    call ESMF_GridCompGetInternalState(driver, is, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, file=trim(name)//":"//__FILE__)) return  ! bail out
      
    ! deallocate internal state memory
    deallocate(is%EARTH_INT_STATE, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Deallocation of internal state memory failed.", &
      line=__LINE__, file=trim(name)//":"//__FILE__, rcToReturn=rc)) &
      return  ! bail out
      
  end subroutine
      
  !-----------------------------------------------------------------------------
  
  recursive subroutine ModifyCplLists(driver, importState, exportState, clock, &
    rc)
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
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_LogWrite("Driver is in ModifyCplLists()", ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    nullify(connectorList)
    call NUOPC_DriverGetComp(driver, compList=connectorList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    write (msg,*) "Found ", size(connectorList), " Connectors."// &
      " Modifying CplList Attribute...."
    call ESMF_LogWrite(trim(msg), ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    do i=1, size(connectorList)
      ! query Connector i for its name
      call ESMF_CplCompGet(connectorList(i), name=name, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      ! access CplList for Connector i
      call NUOPC_CompAttributeGet(connectorList(i), name="CplList", &
        itemCount=cplListSize, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      if (cplListSize>0) then
        allocate(cplList(cplListSize))
        call NUOPC_CompAttributeGet(connectorList(i), name="CplList", &
          valueList=cplList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        ! go through all of the entries in the cplList and add options
        do j=1, cplListSize
          cplList(j) = trim(cplList(j))//":DumpWeights=true"
          cplList(j) = trim(cplList(j))//":SrcTermProcessing=1:TermOrder=SrcSeq"
          ! add connection options read in from configuration file
          call ESMF_AttributeGet(connectorList(i), name="ConnectionOptions", &
            value=value, defaultValue="", rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          cplList(j) = trim(cplList(j))//trim(value)
        enddo
        ! store the modified cplList in CplList attribute of connector i
        call NUOPC_CompAttributeSet(connectorList(i), &
          name="CplList", valueList=cplList, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        deallocate(cplList)
      endif
    enddo
      
    deallocate(connectorList)
    
  end subroutine

  !-----------------------------------------------------------------------------

#ifdef CMEPS
  subroutine ReadAttributes(gcomp, config, label, relaxedflag, formatprint, rc)

    use ESMF  , only : ESMF_GridComp, ESMF_Config, ESMF_LogWrite, ESMF_LOGMSG_INFO, ESMF_SUCCESS
    use NUOPC , only : NUOPC_FreeFormatCreate, NUOPC_CompAttributeIngest
    use NUOPC , only : NUOPC_FreeFormatDestroy, NUOPC_FreeFormat

    ! input/output arguments
    type(ESMF_GridComp) , intent(inout)        :: gcomp
    type(ESMF_Config)   , intent(in)           :: config
    character(len=*)    , intent(in)           :: label
    logical             , intent(in), optional :: relaxedflag
    logical             , intent(in), optional :: formatprint
    integer             , intent(inout)        :: rc

    ! local variables
    type(NUOPC_FreeFormat)  :: attrFF
    character(len=*), parameter :: subname = "(module_EARTH_GRID_COMP.F90:ReadAttributes)"
    !-------------------------------------------

    rc = ESMF_SUCCESS

    if (present(relaxedflag)) then
       attrFF = NUOPC_FreeFormatCreate(config, label=trim(label), relaxedflag=.true., rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out
    else
       attrFF = NUOPC_FreeFormatCreate(config, label=trim(label), rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out
    end if

    call NUOPC_CompAttributeIngest(gcomp, attrFF, addFlag=.true., rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_FreeFormatDestroy(attrFF, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine ReadAttributes

  subroutine InitRestart(driver, rc)

    !-----------------------------------------------------
    ! Determine if will restart and read pointer file if appropriate
    !-----------------------------------------------------

    use ESMF         , only : ESMF_GridComp, ESMF_VM, ESMF_GridCompGet, ESMF_VMGet, ESMF_SUCCESS
    use ESMF         , only : ESMF_LogSetError, ESMF_LogWrite, ESMF_LOGMSG_INFO, ESMF_RC_NOT_VALID
    use NUOPC        , only : NUOPC_CompAttributeGet, NUOPC_CompAttributeSet, NUOPC_CompAttributeAdd

    ! input/output variables
    type(ESMF_GridComp)    , intent(inout) :: driver
    integer                , intent(out)   :: rc

    ! local variables
    logical           :: read_restart   ! read the restart file, based on start_type
    character(len=ESMF_MAXSTR) :: cvalue         ! temporary
    character(len=ESMF_MAXSTR) :: rest_case_name ! Short case identification
    character(len=*) , parameter :: subname = "(module_EARTH_GRID_COMP.F90:InitRestart)"
    !-------------------------------------------

    rc = ESMF_SUCCESS
    call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO, rc=rc)

    !-----------------------------------------------------
    ! Carry out restart if appropriate
    !-----------------------------------------------------

    read_restart = IsRestart(driver, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Add rest_case_name and read_restart to driver attributes
    call NUOPC_CompAttributeAdd(driver, attrList=(/'rest_case_name','read_restart  '/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    rest_case_name = ' '
    call NUOPC_CompAttributeSet(driver, name='rest_case_name', value=rest_case_name, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    write(cvalue,*) read_restart
    call NUOPC_CompAttributeSet(driver, name='read_restart', value=trim(cvalue), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine InitRestart

  function IsRestart(gcomp, rc)

    use ESMF         , only : ESMF_GridComp, ESMF_SUCCESS
    use ESMF         , only : ESMF_LogSetError, ESMF_LogWrite, ESMF_LOGMSG_INFO, ESMF_RC_NOT_VALID
    use NUOPC        , only : NUOPC_CompAttributeGet

    ! input/output variables
    logical                                :: IsRestart
    type(ESMF_GridComp)    , intent(inout) :: gcomp
    integer                , intent(out)   :: rc

    ! locals
    character(len=ESMF_MAXSTR)            :: start_type     ! Type of startup
    character(len=ESMF_MAXSTR)            :: msgstr
    character(len=*) , parameter :: start_type_start = "startup"
    character(len=*) , parameter :: start_type_cont  = "continue"
    character(len=*) , parameter :: start_type_brnch = "branch"
    character(len=*) , parameter  :: subname = "(module_EARTH_GRID_COMP.F90:IsRestart)"
    !---------------------------------------

    rc = ESMF_SUCCESS

    ! First Determine if restart is read
    call NUOPC_CompAttributeGet(gcomp, name='start_type', value=start_type, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if ((trim(start_type) /= start_type_start) .and.  &
        (trim(start_type) /= start_type_cont ) .and.  &
        (trim(start_type) /= start_type_brnch)) then
       write (msgstr, *) subname//': start_type invalid = '//trim(start_type)
       call ESMF_LogSetError(ESMF_RC_NOT_VALID, msg=msgstr, line=__LINE__, file=__FILE__, rcToReturn=rc)
       return
    end if

    !TODO: this is hard-wired to CIME start/continue types in terms of gcomp
    IsRestart = .false.
    if (trim(start_type) == trim(start_type_cont) .or. trim(start_type) == trim(start_type_brnch)) then
       IsRestart = .true.
    end if

  end function IsRestart

  subroutine AddAttributes(gcomp, driver, config, compid, compname, inst_suffix, rc)

    ! Add specific set of attributes to components from driver attributes

    use ESMF  , only : ESMF_GridComp, ESMF_Config, ESMF_LogWrite, ESMF_LOGMSG_INFO, ESMF_SUCCESS
    use ESMF  , only : ESMF_LogFoundAllocError, ESMF_ConfigGetLen, ESMF_ConfigGetAttribute
    use NUOPC , only : NUOPC_CompAttributeAdd, NUOPC_CompAttributeGet, NUOPC_CompAttributeSet

    ! input/output parameters
    type(ESMF_GridComp) , intent(inout) :: gcomp
    type(ESMF_GridComp) , intent(in)    :: driver
    type(ESMF_Config)   , intent(inout) :: config
    integer             , intent(in)    :: compid
    character(len=*)    , intent(in)    :: compname
    character(len=*)    , intent(in)    :: inst_suffix
    integer             , intent(inout) :: rc

    ! local variables
    integer                        :: n
    integer                        :: stat
    integer                        :: inst_index
    logical                        :: is_present
    character(len=ESMF_MAXSTR)     :: cvalue
    character(len=32), allocatable :: compLabels(:)
    character(len=32), allocatable :: attrList(:)
    integer                        :: componentCount
    character(len=*), parameter    :: subname = "(module_EARTH_GRID_COMP.F90:AddAttributes)"
    logical                        :: lvalue = .false.
    !-------------------------------------------

    rc = ESMF_Success
    call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO)

    !------
    ! Add compid to gcomp attributes
    !------
    !write(cvalue,*) compid
    !call NUOPC_CompAttributeAdd(gcomp, attrList=(/'MCTID'/), rc=rc)
    !if (chkerr(rc,__LINE__,u_FILE_u)) return
    !call NUOPC_CompAttributeSet(gcomp, name='MCTID', value=trim(cvalue), rc=rc)
    !if (chkerr(rc,__LINE__,u_FILE_u)) return

    !------
    ! Add all the other attributes in AttrList (which have already been added to driver attributes)
    !------
    !allocate(attrList(5))
    !attrList =  (/"read_restart", "orb_eccen   ", "orb_obliqr  ", "orb_lambm0  ", "orb_mvelpp  "/)
    ! TODO: orb_obliqr and orb_lambm0 not exist
    allocate(attrList(1))
    attrList =  (/"read_restart"/)

    call NUOPC_CompAttributeAdd(gcomp, attrList=attrList, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    do n = 1,size(attrList)
       if (trim(attrList(n)) == "read_restart") then
          call NUOPC_CompAttributeGet(driver, name="mediator_read_restart", value=cvalue, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

          read(cvalue,*) lvalue

          if (.not. lvalue) then
            call NUOPC_CompAttributeGet(driver, name=trim(attrList(n)), value=cvalue, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
          end if

          call NUOPC_CompAttributeSet(gcomp, name=trim(attrList(n)), value=trim(cvalue), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
       else
          print*, trim(attrList(n))
          call NUOPC_CompAttributeGet(driver, name=trim(attrList(n)), value=cvalue, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          call NUOPC_CompAttributeSet(gcomp, name=trim(attrList(n)), value=trim(cvalue), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
       end if
    enddo
    deallocate(attrList)

    !------
    ! Add component specific attributes
    !------
    call ReadAttributes(gcomp, config, trim(compname)//"_attributes::", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ReadAttributes(gcomp, config, "ALLCOMP_attributes::", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !------
    ! Add multi-instance specific attributes
    !------
    call NUOPC_CompAttributeAdd(gcomp, attrList=(/'inst_index'/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! add inst_index attribute (inst_index is not required for cime internal components)
    ! for now hard-wire inst_index to 1
    inst_index = 1
    write(cvalue,*) inst_index
    call NUOPC_CompAttributeSet(gcomp, name='inst_index', value=trim(cvalue), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! add inst_suffix attribute
    if (len_trim(inst_suffix) > 0) then
       call NUOPC_CompAttributeAdd(gcomp, attrList=(/'inst_suffix'/), rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out
       call NUOPC_CompAttributeSet(gcomp, name='inst_suffix', value=inst_suffix, rc=rc)
       if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
         line=__LINE__, &
         file=__FILE__)) &
         return  ! bail out
    end if

  end subroutine AddAttributes
#endif
!
!-----------------------------------------------------------------------
!
      END MODULE module_EARTH_GRID_COMP
!
!-----------------------------------------------------------------------
