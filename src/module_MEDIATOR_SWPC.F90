#define LEGACY
module module_MED_SWPC

  !-----------------------------------------------------------------------------
  ! Mediator Component.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Mediator, only: &
    mediator_routine_SS            => SetServices, &
    mediator_label_DataInitialize  => label_DataInitialize, &
    mediator_label_Advance         => label_Advance, &
    mediator_label_Finalize        => label_Finalize, &
    mediator_label_CheckImport     => label_CheckImport, &
    mediator_label_TimestampExport => label_TimestampExport, &
    mediator_label_SetRunClock     => label_SetRunClock

  use module_MED_SWPC_methods
  
  implicit none

  private

  public :: SetServices
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine SetServices(mediator, rc)
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    ! the NUOPC mediator component will register the generic methods
    call NUOPC_CompDerive(mediator, mediator_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! --- Initialization phases --------------------------------------

    ! Provide InitializeP0 to switch from default IPDv00 to IPDv03
    call ESMF_GridCompSetEntryPoint(mediator, ESMF_METHOD_INITIALIZE, &
      userRoutine=InitializeP0, phase=0, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! IPDv03p1: advertise Fields
    call NUOPC_CompSetEntryPoint(mediator, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p1"/), userRoutine=InitializeP1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! IPDv03p3: realize connected Fields with transfer action "provide"
    call NUOPC_CompSetEntryPoint(mediator, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p3"/), userRoutine=InitializeP3, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! IPDv03p4: optionally modify the decomp/distr of transferred Grid/Mesh

    ! IPDv03p5: realize all Fields with transfer action "accept"
    call NUOPC_CompSetEntryPoint(mediator, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv03p5"/), userRoutine=InitializeP5, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! attach specializing method(s)
    call NUOPC_CompSpecialize(mediator, specLabel=mediator_label_Advance, &
      specRoutine=MediatorAdvance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_CompSpecialize(mediator, specLabel=mediator_label_DataInitialize, &
      specRoutine=DataInitialize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_CompSpecialize(mediator, specLabel=mediator_label_Finalize, &
      specRoutine=Finalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine SetServices
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP0(mediator, importState, exportState, clock, rc)
    type(ESMF_GridComp)   :: mediator
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Clock)      :: clock
    integer, intent(out)  :: rc
    
    ! -- local variables
    integer                    :: verbosity
    character(len=ESMF_MAXSTR) :: name, value
    type(ESMF_Config)          :: config

    rc = ESMF_SUCCESS

    ! -- get mediator information
    call NUOPC_CompGet(mediator, name=name, verbosity=verbosity, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__)) &
      return  ! bail out

    ! -- get name of config file
    call ESMF_AttributeGet(mediator, name="ConfigFile", value=value, &
      defaultValue="med.rc", convention="NUOPC", purpose="Instance", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__)) &
      return  ! bail out
    if (btest(verbosity,8)) then
      call ESMF_LogWrite(trim(name)//": ConfigFile = "//trim(value), &
        ESMF_LOGMSG_INFO, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__)) &
        return  ! bail out
    end if

    ! -- load configuration
    config = ESMF_ConfigCreate(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_ConfigLoadFile(config, value, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -- store Config object into mediator's object
    call ESMF_GridCompSet(mediator, config=config, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__)) &
      return  ! bail out

    ! Switch to IPDv03 by filtering all other phaseMap entries
    call NUOPC_CompFilterPhaseMap(mediator, ESMF_METHOD_INITIALIZE, &
      acceptStringList=(/"IPDv03p"/), rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine InitializeP0
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP1(mediator, importState, exportState, clock, rc)
    
    type(ESMF_GridComp)  :: mediator
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    call NamespaceAdd("ATM",importState, &
      (/ &
        "height                                      ", &
        "eastward_wind_neutral:northward_wind_neutral", &
        "upward_wind_neutral                         ", &
        "temp_neutral                                ", &
        "O_Density                                   ", &
        "O2_Density                                  ", &
        "N2_Density                                  "  &
      /), &
      "cannot provide", &
      ungriddedVerticalDim=.true., &
      fieldSep=":", &
      rc=rc) 
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NamespaceAdd("IPM",exportState, &
      (/ &
        "eastward_wind_neutral:northward_wind_neutral", &
        "upward_wind_neutral                         ", &
        "temp_neutral                                ", &
        "O_Density                                   ", &
        "O2_Density                                  ", &
        "N2_Density                                  "  &
      /), &
      "cannot provide", &
      fieldOptions=(/ &
        "none", &
        "none", &
        "none", &
        "16.0", &
        "32.0", &
        "28.0"  &
      /), &
      fieldSep=":", &
      rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NamespaceAdvertise(rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NamespacePrint(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine InitializeP1

  !-----------------------------------------------------------------------------

  subroutine InitializeP3(mediator, importState, exportState, clock, rc)
    ! IPDv03p3: realize connected Fields with transfer action "provide"
    ! and remove Fields that are not connected
    type(ESMF_GridComp)  :: mediator
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc

    ! -- begin
    call NamespaceCheckConnectedFields(rc)
    
  end subroutine InitializeP3
  
  !-----------------------------------------------------------------------------

  subroutine InitializeP5(mediator, importState, exportState, clock, rc)
    ! IPDv03p5: realize all Fields with transfer action "accept"
    type(ESMF_GridComp)  :: mediator
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    ! -- local variables
    logical                     :: meshWrite
    integer                     :: stat
    character(len=ESMF_MAXSTR)  :: filePrefix
    real(ESMF_KIND_R8), pointer :: levels(:)
    type(ESMF_GeomType_Flag)    :: geomtype, localGeomType
    type(ESMF_Grid)             :: grid, localGrid
    type(ESMF_Mesh)             :: mesh, localMesh
    type(ESMF_Array)            :: array

    ! -- begin
    rc = ESMF_SUCCESS

    ! -- if ATM fields are defined on a mesh, the mesh needs to be replaced
    ! -- get min/max height from IPE mesh
    call NamespaceGet("IPM", exportState, mesh=mesh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NamespaceGet("ATM", importState, geomtype=geomtype, &
      grid=grid, mesh=mesh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    nullify(levels)
    call ConfigGet(mediator, levels=levels, meshWrite=meshWrite, &
      filePrefix=filePrefix, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (geomtype == ESMF_GEOMTYPE_GRID) then

      localGrid = GridAddNewCoord(grid, coord=levels, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      if (meshWrite) then
        call ESMF_GridWriteVTK(grid, &
          filename=trim(filePrefix)//".2d", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__)) &
          return
        call ESMF_GridWriteVTK(localGrid, &
          filename=trim(filePrefix)//".3d", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__)) &
          return
      end if

      call NamespaceSetLocalGrid("ATM", localGrid, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

    else if (geomtype == ESMF_GEOMTYPE_MESH) then

      call ReducedT62MeshCreate(mediator, levels, mesh, localMesh, &
        levArray=array, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      if (meshWrite) then
        call ESMF_MeshWrite(mesh, trim(filePrefix)//".2d", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__)) &
          return
        call ESMF_MeshWrite(localMesh, trim(filePrefix)//".3d", rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__)) &
          return
      end if

      call NamespaceUpdateFields("ATM", importState, mesh=mesh, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call NamespaceUpdateFields("ATM", exportState, mesh=mesh, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      call NamespaceSetLocalMesh("ATM", mesh3d=localMesh, mesh2d=mesh, &
        levArray=array, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

    end if

    deallocate(levels, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Unable to free up memory", &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return
   
    call NamespaceRealizeFields(rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine InitializeP5

  !-----------------------------------------------------------------------------

  subroutine DataInitialize(mediator, rc)
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc
    
    ! -- local variable
    type(ESMF_State)         :: importState

    ! -- begin
    rc = ESMF_SUCCESS

    ! -- check if local DE count for coupled fields is supported
    call NamespaceCheckDE(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, &
      msg="Try using a number of PETs at least as big as the largest one &
          &used by the coupled components", &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NamespaceInitializeFields(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! indicate that data initialization is complete (breaking out of init-loop)
    call NUOPC_CompAttributeSet(mediator, &
      name="InitializeDataComplete", value="true", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine DataInitialize

  !-----------------------------------------------------------------------------

  subroutine MediatorAdvance(mediator, rc)
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)      :: clock
    type(ESMF_Field)      :: srcField, dstField, tmpField
    type(ESMF_Array)      :: tmpArray, tnArray
    type(ESMF_State)      :: importState, exportState
    type(ESMF_Time)       :: currTime, stopTime, startTime
    type(rhType), pointer :: rh
    integer(ESMF_KIND_R8) :: advanceCount
    integer               :: item

    real(ESMF_KIND_R8),    parameter :: earthRadius = 6371008.8_ESMF_KIND_R8 !  IUGG Earth Mean Radius (Moritz, 2000)

    ! -- begin
    rc = ESMF_SUCCESS

    ! query the Component for its clock, importState and exportState
    call ESMF_GridCompGet(mediator, clock=clock, &
      importState=importState, exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! HERE THE MEDIATOR ADVANCES: currTime -> currTime + timeStep
    
    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>Advancing Mediator from: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (.not.RouteHandleListIsCreated()) then
#if 0
      ! -- set intermediate grid from field
      call NamespaceSetLocalGridFromField("ATM", importState, "average_height", &
        scale=1._ESMF_KIND_R8/earthRadius, offset=1._ESMF_KIND_R8, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! -- then remove field from importState
      call NamespaceRemoveField("ATM", importState, "average_height", rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

#endif
      ! -- precompute routehandles
      call RouteHandleCreate(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      call RouteHandlePrint(rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

    end if

    ! -- do not regrid imported fields at first coupling time step
    ! -- since they are not available
    call ESMF_ClockGet(clock, advanceCount=advanceCount, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (advanceCount == 0) return

    rh => RouteHandleListGet(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

      ! -- identify field providing time-changing vertical levels
#ifdef LEGACY
      call NamespaceSetRemoteLevelsFromField("ATM", importState, "height", &
        norm=1000._ESMF_KIND_R8, rc=rc)
#else
      call NamespaceSetRemoteLevelsFromField("ATM", importState, "height", &
        scale=1._ESMF_KIND_R8/earthRadius, offset=1._ESMF_KIND_R8, rc=rc)
#endif
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! -- perform necessary computation and regridding
      do while (associated(rh))

      ! -- Fields from WAM to IPE require special treatment
        if (trim(rh % label) == "ATM -> IPM") then
          ! -- vertical profiles of gaseus species must be extrapolated above TOA
          ! -- extrapolated profiles depend upon neutral temperature at TOA
          ! -- therefore the neutral temperature field must be retrieved first
          tmpField = StateGetField(rh % srcState, "temp_neutral", &
#ifdef LEGACY
            options="origin", rc=rc)
#else
            options="native", rc=rc)
#endif
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          call ESMF_FieldGet(tmpField, array=tmpArray, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          tnArray = ESMF_ArrayCreate(tmpArray, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          call FieldPrintMinMax(tmpField, "tmp: temp_neutral", rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

          ! -- only process fields with known destination
          do item = 1, size(rh % dstState % fieldNames)
            call FieldRegrid(rh, trim(rh % dstState % fieldNames(item)), &
              auxArray=tnArray, options=rh % dstState % fieldOptions(item), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
          end do

          call ESMF_ArrayDestroy(tnArray, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        else
          ! -- perform standard regridding w/ linear vertical interpolation
          ! -- only process fields with known destination
          do item = 1, size(rh % dstState % fieldNames)
            call ESMF_StateGet(rh % srcState % self, field=srcField, &
              itemName=trim(rh % dstState % fieldNames(item)), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            call FieldPrintMinMax(srcField, "orig - src:" // trim(rh % dstState % fieldNames(item)), rc)
            call FieldRegrid(rh, trim(rh % dstState % fieldNames(item)), rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
          end do

        end if

        rh => rh % next
      end do

  end subroutine MediatorAdvance

  subroutine Finalize(mediator, rc)

    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc

    ! -- local variables
    logical           :: configIsPresent
    type(ESMF_Config) :: config

    ! -- begin
    rc = ESMF_SUCCESS

    ! -- free up memory
    call RouteHandlePrint(rc=rc)

    ! -- routehandles
    call RouteHandleListRelease(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -- connected components
    call NamespaceDestroy(rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -- check config object
    call ESMF_GridCompGet(mediator, configIsPresent=configIsPresent, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__)) &
      return  ! bail out

    if (configIsPresent) then
      ! -- get mediator's config object
      call ESMF_GridCompGet(mediator, config=config, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__)) &
        return  ! bail out
      ! -- destroy config
      call ESMF_ConfigDestroy(config, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__)) &
        return  ! bail out
    end if
    
  end subroutine Finalize

end module module_MED_SWPC
