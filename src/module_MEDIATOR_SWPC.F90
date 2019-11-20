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
    
    rc = ESMF_SUCCESS

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
    type(ESMF_GeomType_Flag) :: geomtype, localGeomType
    type(ESMF_Grid)          :: grid, localGrid
    type(ESMF_Mesh)          :: mesh, localMesh
    type(ESMF_Array)         :: array
    real(ESMF_KIND_R8), dimension(2) :: coordBounds

    integer, parameter :: numLevels = 188
    real(ESMF_KIND_R8), dimension(numLevels), parameter :: verticalLevels = (/ &
      0.34, 0.39, 0.44, 0.51, 0.58, 0.66, 0.75, 0.86, 0.97, 1.11, 1.26, &
      1.42, 1.61, 1.82, 2.05, 2.31, 2.59, 2.91, 3.25, 3.62, 4.03, 4.46, 4.93, &
      5.42, 5.95, 6.51, 7.09, 7.7, 8.33, 8.99, 9.66, 10.35, 11.04, 11.75, &
      12.47, 13.2, 13.93, 14.67, 15.42, 16.19, 16.97, 17.76, 18.58, 19.42, &
      20.27, 21.15, 22.05, 22.97, 23.92, 24.88, 25.87, 26.88, 27.91, 28.96, &
      30.04, 31.15, 32.29, 33.46, 34.66, 35.9, 37.18, 38.49, 39.85, 41.25, &
      42.68, 44.15, 45.65, 47.18, 48.74, 50.31, 51.9, 53.49, 55.09, 56.69, &
      58.29, 59.88, 61.46, 63.03, 64.58, 66.13, 67.66, 69.18, 70.69, 72.2, &
      73.72, 75.2, 76.69, 78.16, 79.63, 81.1, 82.57, 84.02, 85.48, 86.93, &
      88.37, 89.81, 91.25, 92.68, 94.11, 95.54, 96.97, 98.41, 99.86, 101.33, &
      102.83, 104.38, 105.99, 107.66, 109.44, 111.33, 113.36, 115.61, 118.11, &
      120.92, 124.09, 127.6, 131.47, 135.68, 140.25, 145.17, 150.44, 156.05, &
      161.98, 168.24, 174.8, 181.66, 188.81, 196.24, 203.93, 211.9, 220.13, &
      228.61, 237.34, 246.32, 255.54, 265., 274.69, 284.6, 294.72, 305.04, &
      315.56, 326.27, 337.22, 348.31, 359.6, 371.08, 382.77, 394.69, 406.85, &
      418., 430., 440., 450., 460., 470., 480., 490., 500., 510., 520., 530., 540., 550., &
      560., 570., 580., 590., 600., 610., 620., 630., 640., 650., 660., 670., 680., 690., &
      700., 710., 720., 730., 740., 750., 760., 770., 780., 790., 800. /)

    real(ESMF_KIND_R8),    parameter :: earthRadius = 6371.0088_ESMF_KIND_R8 !  IUGG Earth Mean Radius (Moritz, 2000)

    ! -- begin
    rc = ESMF_SUCCESS

    ! -- if ATM fields are defined on a mesh, the mesh needs to be replaced
    ! -- get min/max height from IPE mesh
    call NamespaceGet("IPM", exportState, mesh=mesh, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call MeshGetBounds(mesh, 3, coordBounds, rc=rc)
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

    if (geomtype == ESMF_GEOMTYPE_GRID) then

      localGrid = GridAddNewCoord(grid, coord=verticalLevels, &
        scale=1._ESMF_KIND_R8/earthRadius, offset=1._ESMF_KIND_R8, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      call NamespaceSetLocalGrid("ATM", localGrid, rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

    else if (geomtype == ESMF_GEOMTYPE_MESH) then

      call initGrids(mediator, mesh, localMesh, minheight=coordBounds(1), &
        hArray=array, rc=rc)
!       heights=verticalLevels, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

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

      call NamespaceSetLocalMesh("ATM", mesh3d=localMesh, mesh2d=mesh, levArray=array, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

    end if
   
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
    
  end subroutine Finalize

end module module_MED_SWPC
