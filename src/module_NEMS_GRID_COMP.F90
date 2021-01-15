#include "./ESMFVersionDefine.h"

!-----------------------------------------------------------------------
!
      MODULE module_NEMS_GRID_COMP
!
!-----------------------------------------------------------------------
!***  This module contains codes directly related to the NEMS component.
!-----------------------------------------------------------------------
!
!***  The NEMS component lies in the hierarchy seen here:
!
!          Main program
!               |
!               |
!          NEMS component
!               |
!               |
!          EARTH component
!              /|\
!             / | \
!          ATM/OCN/ICE/WAV/LND/IPM/HYD .. components
!          |    |
!          |    |
!          |    |
!          |    (MOM5, HYCOM, etc.)
!          |
!          CORE component (GSM, NMM, FIM, GEN, etc.)
!
!-----------------------------------------------------------------------
!  2011-05-11  Theurich & Yang  - Modified for using the ESMF 5.2.0r_beta_snapshot_07.
!  2011-10-04  Yang  - Modified for using the ESMF 5.2.0r library.
!  2013-07     Theurich - Macro based ESMF error handling
!-----------------------------------------------------------------------
!
      USE ESMF
!
      USE module_NEMS_INTERNAL_STATE,ONLY: NEMS_INTERNAL_STATE          &
                                          ,WRAP_NEMS_INTERNAL_STATE
!
      USE module_EARTH_GRID_COMP
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
      PUBLIC :: NEMS_REGISTER
!
!-----------------------------------------------------------------------
!
      INTEGER :: NUMBER_START                                           &
                ,NUMBER_FINAL
!
      INTEGER :: PE_MEMBER                                                 !<-- Tasks for each member
      INTEGER,DIMENSION(:),ALLOCATABLE :: PETLIST                          !<-- Task list for each member
!
      CHARACTER(ESMF_MAXSTR) :: IMP_EARTH_NAME                             !<-- Import state name of the EARTH components
      CHARACTER(ESMF_MAXSTR) :: EXP_EARTH_NAME                             !<-- Export state name of the EARTH components
      CHARACTER(ESMF_MAXSTR) :: GC_EARTH_NAME                              !<-- Name of the EARTH component
!
      TYPE(NEMS_INTERNAL_STATE),POINTER,SAVE :: NEMS_INT_STATE
      TYPE(WRAP_NEMS_INTERNAL_STATE)   ,SAVE :: WRAP
!
      TYPE(ESMF_Clock), SAVE :: CLOCK_NEMS                                 !<-- The ESMF Clock of the NEMS component
      TYPE(ESMF_Config),SAVE :: CF_NEMS                                    !<-- The configure object of the NEMS component
      TYPE(ESMF_VM),    SAVE :: VM_GLOBAL
      TYPE(ESMF_TIME),  SAVE :: STARTTIME, CURRTIME
!
      TYPE(ESMF_GridComp),SAVE :: EARTH_GRID_COMP                          !<-- EARTH components for each member
      TYPE(ESMF_State),   SAVE :: EARTH_IMP_STATE                          !<-- Import state of the EARTH component
      TYPE(ESMF_State),   SAVE :: EARTH_EXP_STATE                          !<-- Export state of the EARTH component
!
!-----------------------------------------------------------------------
!
      CONTAINS

!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
!
      SUBROUTINE NEMS_REGISTER(NEMS_GRID_COMP,RC_REG)
!
!-----------------------------------------------------------------------
!
!------------------------
!***  Argument Variables
!------------------------
!
      TYPE(ESMF_GridComp) :: NEMS_GRID_COMP                                !<-- The NEMS gridded component
!
      INTEGER,INTENT(OUT) :: RC_REG                                        !<-- Error return code
!
!---------------------
!***  Local Variables
!---------------------
!
      INTEGER :: RC
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      RC_REG = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!***  Register the NEMS Initialize, Run, and Finalize routines.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Set Entry Point for NEMS Initialize"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompSetEntryPoint(NEMS_GRID_COMP                    &  !<-- The NEMS component
                                     ,ESMF_METHOD_INITIALIZE            &  !<-- Subroutine type (Initialize)
                                     ,NEMS_INITIALIZE                   &  !<-- User's subroutine name
                                     ,phase=1                           &
                                     ,rc=RC)
      ESMF_ERR_RETURN(RC,RC_REG)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Set Entry Point for NEMS Run"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompSetEntryPoint(NEMS_GRID_COMP                    &  !<-- The NEMS component
                                     ,ESMF_METHOD_RUN                   &  !<-- Subroutine type (Run)
                                     ,NEMS_RUN                          &  !<-- User's subroutine name
                                     ,phase=1                           &
                                     ,rc=RC)
      ESMF_ERR_RETURN(RC,RC_REG)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Set Entry Point for NEMS Finalize"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompSetEntryPoint(NEMS_GRID_COMP                    &  !<-- The NEMS component
                                     ,ESMF_METHOD_FINALIZE              &  !<-- Subroutine type (Finalize)
                                     ,NEMS_FINALIZE                     &  !<-- User's subroutine name
                                     ,phase=1                           &
                                     ,rc=RC)
      ESMF_ERR_RETURN(RC,RC_REG)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      END SUBROUTINE NEMS_REGISTER
!
!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
!
      SUBROUTINE NEMS_INITIALIZE(NEMS_GRID_COMP                         &
                                ,IMP_STATE                              &
                                ,EXP_STATE                              &
                                ,CLOCK_MAIN                             &
                                ,RC_INIT)
!
!-----------------------------------------------------------------------
!
!------------------------
!***  Argument Variables
!------------------------
!
      TYPE(ESMF_GridComp) :: NEMS_GRID_COMP                                !<-- The NEMS component
!
      TYPE(ESMF_State) :: IMP_STATE                                     &  !<-- The NEMS import state
                         ,EXP_STATE                                        !<-- The NEMS export state
!
      TYPE(ESMF_Clock) :: CLOCK_MAIN                                       !<-- The main Clock
!
      INTEGER,INTENT(OUT) :: RC_INIT                                       !<-- Error return code
!
!-----------------------------------------------------------------------
!
!---------------------
!***  Local Variables
!---------------------
!
      TYPE(ESMF_TimeInterval) :: RUNDURATION, restartOffset
!
      CHARACTER(20) :: PELAB
!
      CHARACTER(ESMF_MAXSTR) EARTH_COMP_NAME &                             !<-- Names of each member's EARTH component
                            ,IMP_EARTH_NAME  &                             !<-- Import state name of the EARTH components
                            ,EXP_EARTH_NAME                                !<-- Export state name of the EARTH components
!
      INTEGER :: I,IJ,J,RC,RC_USER
!
      INTEGER :: MYPE_GLOBAL                                            &
                ,NHOURS_FCST                                            &
                ,NSECONDS_FCST                                          &
                ,PE_MAX                                                 &
                ,TASKS                                                  &
                ,fhrot
!
      INTEGER,DIMENSION(:),ALLOCATABLE :: PETLIST                          !<-- Task list for each ensemble member
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      RC_INIT = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!***  For the moment, use a direct copy of the main Clock within
!***  the NEMS component.
!-----------------------------------------------------------------------
!
      CLOCK_NEMS=CLOCK_MAIN
!
!-----------------------------------------------------------------------
!***  What is the start time on the NEMS clock?
!-----------------------------------------------------------------------

! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK = "Extract the start time of the NEMS clock"
!     CALL ESMF_LogWrite(MESSAGE_CHECK, ESMF_LOGMSG_INFO, rc = RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

      CALL ESMF_ClockGet(clock     = CLOCK_NEMS                         &
                        ,startTime = STARTTIME                          &
                        ,rc = RC)

      ESMF_ERR_RETURN(RC,RC_INIT)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
!-----------------------------------------------------------------------
!***  Allocate the NEMS component's internal state, point at it,
!***  and attach it to the NEMS component.
!-----------------------------------------------------------------------
!
      ALLOCATE(NEMS_INT_STATE,stat=RC)
      wrap%NEMS_INT_STATE=>NEMS_INT_STATE
!
      CALL ESMF_GridCompSetInternalState(NEMS_GRID_COMP                 &  !<--The NEMS component
                                        ,WRAP                           &  !<-- Pointer to the NEMS internal state
                                        ,RC)
      ESMF_ERR_RETURN(RC,RC_INIT)
!
!-----------------------------------------------------------------------
!***  Get the global VM (Virtual Machine).
!***  Obtain the total task count and the local task ID.
!-----------------------------------------------------------------------
!
      CALL ESMF_VMGetGlobal(vm = VM_GLOBAL                              &  !<-- The ESMF global Virtual Machine
                           ,rc = RC)
      ESMF_ERR_RETURN(RC,RC_INIT)
!
      CALL ESMF_VMGet(vm       = VM_GLOBAL                              &  !<-- The ESMF global Virtual Machine
                     ,pecount  = TASKS                                  &  !<-- Total # of MPI tasks
                     ,localpet = MYPE_GLOBAL                            &  !<-- This task's global rank
                     ,rc       = RC)
      ESMF_ERR_RETURN(RC,RC_INIT)

!jm 20140818
!jm   call rsl_lite_error_dup1( mype_global )                              !<-- Enable this for per-MPI task output
!jm                                                                        !<-- obtain other bits from JM

!
!-----------------------------------------------------------------------
!***  Create and load the Configure object which will hold the contents
!***  of the NEMS configure file.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Create/Load the NEMS Configure Object"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CF_NEMS=ESMF_ConfigCreate(rc=RC)
!
      CALL ESMF_ConfigLoadFile(config   = CF_NEMS                       &  !<-- The configure object
                              ,filename = 'model_configure'             &  !<-- The name of the configure file
                              ,rc       = RC)
      ESMF_ERR_RETURN(RC,RC_INIT)
!
!-----------------------------------------------------------------------
!***  For each member create the names of the EARTH components and
!***  ESMF states then fill in the task information.
!-----------------------------------------------------------------------
!
      EARTH_COMP_NAME = "EARTH grid component"
      IMP_EARTH_NAME  = "EARTH import state"
      EXP_EARTH_NAME  = "EARTH export state"
!
      CALL ESMF_ConfigGetAttribute(config = CF_NEMS                     &
                                  ,value  = PE_MEMBER                   &
                                  ,label  = "PE_MEMBER01:"               &
                                  ,rc     = RC)
      ESMF_ERR_RETURN(RC,RC_INIT)
!
!-----------------------------------------------------------------------
!***  Set up the PE list.
!-----------------------------------------------------------------------
!
      ALLOCATE(PETLIST(1:PE_MEMBER))
!
      IJ = 0
      DO I = 1, PE_MEMBER
        PETLIST(I) = IJ
        IJ = IJ + 1
      END DO
!
!-----------------------------------------------------------------------
!***  Create the EARTH grid components.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Create EARTH grid Components"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      EARTH_GRID_COMP = ESMF_GridCompCreate (                      &
                        name         = EARTH_COMP_NAME             &  !<-- Name of element I of the EARTH component array
                       ,petlist      = PETLIST(1:PE_MEMBER)             &  !<-- Element I's PE list
                       ,config       = CF_NEMS                     &  !<-- Associate the NEMS config object with this element
                       ,rc           = RC)
      ESMF_ERR_RETURN(RC,RC_INIT)
!
!-----------------------------------------------------------------------
!***  Register the Initialize, Run, and Finalize routines of
!***  each element in the EARTH component array.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Register EARTH Init, Run, Finalize"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
        CALL ESMF_GridCompSetServices(EARTH_GRID_COMP                   &  !<-- The EARTH gridded components
                                     ,EARTH_REGISTER                    &  !<-- User's name for the Register routine
                                     ,rc=RC)
        ESMF_ERR_RETURN(RC,RC_INIT)
!
!-----------------------------------------------------------------------
!***  Create the EARTH import and export states.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Create the EARTH import states"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      EARTH_IMP_STATE = ESMF_StateCreate(                                 &
                                    name = IMP_EARTH_NAME                 &
                                   ,stateintent = ESMF_STATEINTENT_IMPORT &
                                   ,rc        = RC)
      ESMF_ERR_RETURN(RC,RC_INIT)
!
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Create the EARTH export states"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      EARTH_EXP_STATE = ESMF_StateCreate(                                 &
                                    name   = EXP_EARTH_NAME               &
                                   ,stateintent = ESMF_STATEINTENT_EXPORT &
                                   ,rc          = RC)
      ESMF_ERR_RETURN(RC,RC_INIT)
!
!-----------------------------------------------------------------------
!***  Adjust the currTime of the NEMS clock: CLOCK_NEMS
!***  if the fhrot is > 0
!***  This will correctly set the NEMS and EARTH clocks in case of
!***  Restart-From-History.
!-----------------------------------------------------------------------

      CALL ESMF_ConfigGetAttribute(config = CF_NEMS &
                                   ,value  = fhrot &
                                   ,label  = 'fhrot:' &
                                   ,default = 0 &
                                   ,rc     = RC)
      ESMF_ERR_RETURN(RC,RC_INIT)

      if (fhrot > 0) then
        CALL ESMF_TimeIntervalSet(restartOffset, h=fhrot,rc=RC)
        ESMF_ERR_RETURN(RC,RC_INIT)
        CURRTIME = STARTTIME + restartOffset
        call ESMF_ClockSet(CLOCK_NEMS, currTime=CURRTIME, rc=RC)
        ESMF_ERR_RETURN(RC,RC_INIT)
      endif
!
!-----------------------------------------------------------------------
!***  Execute the Initialize step of each element of the EARTH
!***  component array.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Execute the Initialize step of the EARTH component"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompInitialize(gridcomp    = EARTH_GRID_COMP        &
                                  ,importState = EARTH_IMP_STATE        &
                                  ,exportState = EARTH_EXP_STATE        &
                                  ,clock       = CLOCK_NEMS             &
                                  ,phase       = 1                      &
                                  ,userRc      = RC_USER                &
                                  ,rc          = RC)
      ESMF_ERR_RETURN(RC,RC_INIT)
      ESMF_ERR_RETURN(RC_USER,RC_INIT)
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE NEMS_INITIALIZE
!
!-----------------------------------------------------------------------
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
!-----------------------------------------------------------------------
!
      SUBROUTINE NEMS_RUN(NEMS_GRID_COMP                                &
                         ,IMP_STATE                                     &
                         ,EXP_STATE                                     &
                         ,CLOCK_MAIN                                    &
                         ,RC_RUN)
!
!-----------------------------------------------------------------------
!
!------------------------
!***  Argument Variables
!------------------------
!
      TYPE(ESMF_GridComp) :: NEMS_GRID_COMP                                !<-- The NEMS component
!
      TYPE(ESMF_State) :: IMP_STATE                                     &  !<-- The NEMS import state
                         ,EXP_STATE                                        !<-- The NEMS export state
!
      TYPE(ESMF_Clock) :: CLOCK_MAIN                                       !<-- The main Clock
!
      INTEGER,INTENT(OUT) :: RC_RUN                                        !<-- Error return code
!
!---------------------
!***  Local Variables
!---------------------
!
      INTEGER :: HH,I,J,RC,RC_USER
!
      TYPE(ESMF_Time) :: CURRTIME
!
      TYPE(ESMF_TimeInterval) :: RUNDURATION
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      RC_RUN = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!***  For the moment, use a direct copy of the main Clock within
!***  the NEMS component.
!-----------------------------------------------------------------------
!
      CLOCK_NEMS=CLOCK_MAIN
!
!-----------------------------------------------------------------------
!***  Execute the Run step of each element in the EARTH component
!***  array.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Execute the Run step of the EARTH components"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompRun(gridcomp    = EARTH_GRID_COMP               &
                           ,importState = EARTH_IMP_STATE               &
                           ,exportState = EARTH_EXP_STATE               &
                           ,clock       = CLOCK_NEMS                    &
                           ,phase       = 1                             &
                           ,userRc      = RC_USER                       &
                           ,rc          = RC)
      ESMF_ERR_RETURN(RC,RC_RUN)
      ESMF_ERR_RETURN(RC_USER,RC_RUN)
!
      CALL ESMF_ClockGet(clock       = CLOCK_NEMS                       &
                        ,runDuration = RUNDURATION                      &
                        ,rc          = RC)
      ESMF_ERR_RETURN(RC,RC_RUN)
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE NEMS_RUN
!
!-----------------------------------------------------------------------
!#######################################################################
!-----------------------------------------------------------------------
!
      SUBROUTINE NEMS_FINALIZE(NEMS_GRID_COMP                           &
                              ,IMP_STATE                                &
                              ,EXP_STATE                                &
                              ,CLOCK_MAIN                               &
                              ,RC_FINALIZE)
!
!-----------------------------------------------------------------------
!
!------------------------
!***  Argument Variables
!------------------------
!
      TYPE(ESMF_GridComp) :: NEMS_GRID_COMP                                !<-- The NEMS component
!
      TYPE(ESMF_State) :: IMP_STATE                                     &  !<-- The NEMS import state
                         ,EXP_STATE                                        !<-- The NEMS export state
!
      TYPE(ESMF_Clock) :: CLOCK_MAIN                                       !<-- The main Clock
!
      INTEGER,INTENT(OUT) :: RC_FINALIZE                                   !<-- Error return code
!
!---------------------
!***  Local Variables
!---------------------
!
      INTEGER :: I,RC,RC_USER
!
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
!
      RC_FINALIZE = ESMF_SUCCESS
!
!-----------------------------------------------------------------------
!***  Execute the Finalize step of each element of the
!***  EARTH component array.
!-----------------------------------------------------------------------
!
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
      MESSAGE_CHECK="Execute the Finalize step of the EARTH component"
!     CALL ESMF_LogWrite(MESSAGE_CHECK,ESMF_LOGMSG_INFO,rc=RC)
! ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!
      CALL ESMF_GridCompFinalize(gridcomp    = EARTH_GRID_COMP          &
                                ,importState = EARTH_IMP_STATE          &
                                ,exportState = EARTH_EXP_STATE          &
                                ,clock       = CLOCK_NEMS               &
                                ,phase       = 1                        &
                                ,userRc      = RC_USER                  &
                                ,rc          = RC)
      ESMF_ERR_RETURN(RC,RC_FINALIZE)
      ESMF_ERR_RETURN(RC_USER,RC_FINALIZE)
!
!-----------------------------------------------------------------------
!
      END SUBROUTINE NEMS_FINALIZE
!
!-----------------------------------------------------------------------
!
      END MODULE module_NEMS_GRID_COMP
!
!-----------------------------------------------------------------------
