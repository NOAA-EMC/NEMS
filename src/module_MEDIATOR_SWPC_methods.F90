#define LEGACY
module module_MED_SWPC_methods

  !-----------------------------------------------------------------------------
  ! Mediator Methods
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  
  implicit none
  
  type stateType
    type(ESMF_State) :: parent
    type(ESMF_State) :: self
    character(len=ESMF_MAXSTR), dimension(:), pointer :: fieldNames   => null()
    character(len=ESMF_MAXSTR), dimension(:), pointer :: fieldOptions => null()
    character(len=ESMF_MAXSTR) :: trAction
    character(len=1)           :: fieldSep
    type(ESMF_Grid)            :: localGrid
    type(ESMF_Mesh)            :: localMesh
    type(ESMF_Field)           :: uvec(2)
    type(ESMF_Field)           :: localField(2)
    type(ESMF_Field)           :: localIntField(2)
    type(ESMF_Field)           :: localCartField
    type(ESMF_Array)           :: localLevels
    type(ESMF_Array)           :: remoteLevels
    integer                    :: ugDimLength
    integer                    :: fieldMaxRank
    logical                    :: doRotation
    integer, dimension(:), pointer :: fieldDepMap => null()
    type(stateType),   pointer :: next 
  end type stateType

  type compType
    character(len=ESMF_MAXSTR) :: name
    type(stateType),   pointer :: stateList => null()
    type(compType),    pointer :: next      => null()
  end type compType

  type rhType
    character(len=ESMF_MAXSTR) :: label
    type(ESMF_RouteHandle)   :: rh
    type(stateType), pointer :: srcState => null()
    type(stateType), pointer :: dstState => null()
    type(rhType),    pointer :: next
  end type rhType

  type (compType), pointer :: compList => null()
  type (rhType),   pointer :: rhList   => null()

  interface Interpolate
    module procedure VerticalInterpolate1D
    module procedure VerticalInterpolate2D
  end interface Interpolate



  private

  public :: &
    compType, &
    rhType

  public :: &
    ConfigGet,                         &
    FieldGet,                          &
    FieldPrintMinMax,                  &
    FieldRegrid,                       &
    GridAddNewCoord,                   &
    MeshGetBounds,                     &
    NamespaceAdd,                      &
    NamespaceAdjustFields,             &
    NamespaceAdvertise,                &
    NamespaceCheckConnectedFields,     &
    NamespaceCheckDE,                  &
    NamespaceDestroy,                  &
    NamespaceGet,                      &
    NamespaceGetGrid,                  &
    NamespaceGetLocal,                 &
    NamespaceInitializeFields,         &
    NamespacePrint,                    &
    NamespaceRealizeFields,            &
    NamespaceRemoveField,              &
    NamespaceSetLocalGrid,             &
    NamespaceSetLocalGridFromField,    &
    NamespaceSetLocalMesh,             &
    NamespaceSetRemoteLevelsFromField, &
    NamespaceUpdateFields,             &
    ReducedT62MeshCreate,              &
    RouteHandleCreate,                 &
    RouteHandleListGet,                &
    RouteHandleListIsCreated,          &
    RouteHandleListRelease,            &
    RouteHandlePrint,                  &
    StateFilterField,                  &
    StateGetField,                     &
    StateStoreField
  
contains

  ! -- Namespace: Add(Create)/Remove objects/Destroy: begin definition --
  
  subroutine NamespaceAdd(name, state, fieldNames, trAction, ungriddedVerticalDim, &
    fieldOptions, fieldSep, rc)
    character(len=*),               intent(in) :: name
    type(ESMF_State)                           :: state
    character(len=*), dimension(:), intent(in) :: fieldNames
    character(len=*),               intent(in) :: trAction
    logical,             optional,  intent(in) :: ungriddedVerticalDim
    character(len=*), dimension(:), optional, intent(in) :: fieldOptions
    character(len=*),               optional, intent(in) :: fieldSep
    integer,             optional, intent(out) :: rc

    ! -- local variables
    type(compType),  pointer :: p, q
    type(stateType), pointer :: compState, s
    integer :: localrc

    if (present(rc)) rc = ESMF_SUCCESS

    nullify(compState, p, q, s)

    p => compList
    q => compList
    do while (associated(p))
      if (trim(p % name) == name) exit
      q => p
      p => p % next
    end do

    if (.not.associated(p)) then
      allocate(p, stat=localrc)
      if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return
      p % name = name
      nullify(p % next, p % stateList)
      if (associated(q)) then
        q % next => p
      else
        q => p
        compList => p
      end if
    end if

    call StateAdd(compState, state, fieldNames, trAction, &
      ungriddedVerticalDim=ungriddedVerticalDim, &
      fieldOptions=fieldOptions, fieldSep=fieldSep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    if (associated(p % stateList)) then
      s => p % stateList
      do while (associated(s % next))
        s => s % next
      end do
      s % next => compState
    else
      p % stateList => compState
    end if

    nullify(p, q, s, compState)
     
  end subroutine NamespaceAdd

  subroutine StateAdd(s, state, fieldNames, trAction, ungriddedVerticalDim, fieldOptions, fieldSep,  rc)
    type(stateType),                   pointer :: s
    type(ESMF_State)                           :: state
    character(len=*), dimension(:), intent(in) :: fieldNames
    character(len=*),               intent(in) :: trAction
    logical,             optional,  intent(in) :: ungriddedVerticalDim
    character(len=*), dimension(:), optional, intent(in) :: fieldOptions
    character(len=*),               optional, intent(in) :: fieldSep
    integer,             optional, intent(out) :: rc

    ! -- local variables
    integer :: i, item, localCount, pairCount, pos
    integer :: localrc, fieldCount, totalCount, ugDimLength, localMaxRank
    integer,                    dimension(:), allocatable :: localDepMap, tmpMap
    character(len=ESMF_MAXSTR), dimension(:), allocatable :: localNames, localOptions, tmp
    character(len=1) :: localFieldSep

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    ugDimLength = 0
    if (present(ungriddedVerticalDim)) then
      if (ungriddedVerticalDim) ugDimLength = -1
    end if

    ! -- if present, field options must be provided for each field entry
    if (present(fieldOptions)) then
      if (size(fieldNames) /= size(fieldOptions)) then
        call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
          msg="number of field options must match number of fields", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
          return ! bail out
      end if
    end if

    ! -- set field separator string to identify field pairs
    localFieldSep = ":"
    if (present(fieldSep)) localFieldSep = fieldSep(1:1)

    ! -- unpack field pairs to determine full field list
    fieldCount = size(fieldNames)
    pairCount  = count(scan(fieldNames, localFieldSep) /=0)
    localCount = fieldCount + pairCount

    allocate(localNames(localCount), localOptions(localCount), &
      localDepMap(localCount), stat=localrc)
    if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    localOptions = "none"
    localDepMap  = 0
    localMaxRank = 1

    i = 0
    do item = 1, fieldCount
      pos = scan(fieldNames(item), localFieldSep)
      i = i + 1
      if (pos > 0) then
        localNames(i)  = fieldNames(item)(1:pos-1)
        localDepMap(i) = i + 1
        i = i + 1
        localNames(i)  = fieldNames(item)(pos+1:)
        localDepMap(i) = -i - 1
        if (present(fieldOptions)) then
          localOptions(i-1:i) = fieldOptions(item)
        end if
        localMaxRank = 2
      else
        localNames(i) = fieldNames(item)
        if (present(fieldOptions)) &
          localOptions(i) = fieldOptions(item)
      end if
    end do

    ! -- create state if it does not exist
    if (.not.associated(s)) then
      allocate(s, stat=localrc)
      if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return
      ! -- initialize state
      nullify(s % fieldNames, s % fieldOptions, s % next)
      s % trAction     = ""
      s % ugDimLength  = 0
      s % fieldMaxRank = 1
    end if
    if (associated(s % fieldNames)) then
      if (trim(s % trAction) /= trAction) then
        ! -- add fields to same nested state (only one value of trAction is allowed)
        call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
          msg="transferAction must match when adding fields to same state", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
          return ! bail out
      end if 
      if ((s % ugDimLength * ugDimLength == 0) .and. &
          (s % ugDimLength /= ugDimLength)) then
        ! -- add fields to same nested state (only one value of trAction is allowed)
        call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
          msg="field to be added must have ungridded dimension length", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
          return ! bail out
      end if 
      fieldCount = size(s % fieldNames)
      totalCount = fieldCount + localCount
      allocate(tmp(fieldCount), stat=localrc)
      if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return
      ! -- add field names
      tmp(1:fieldCount) = s % fieldNames
      deallocate(s % fieldNames, stat=localrc)
      if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return
      allocate(s % fieldNames(totalCount), stat=localrc)
      if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return
      s % fieldNames(1:fieldCount)      = tmp
      s % fieldNames(fieldCount + 1:)   = localNames
      ! -- add field options
      tmp(1:fieldCount) = s % fieldOptions
      deallocate(s % fieldOptions, stat=localrc)
      if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return
      allocate(s % fieldOptions(totalCount), stat=localrc)
      if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return
      s % fieldOptions(1:fieldCount)      = tmp
      s % fieldOptions(fieldCount + 1:)   = localOptions
      deallocate(tmp, stat=localrc)
      if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return
      ! -- add field depMap
      allocate(tmpMap(fieldCount), stat=localrc)
      if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return
      tmpMap = s % fieldDepMap
      deallocate(s % fieldDepMap, stat=localrc)
      if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return
      allocate(s % fieldDepMap(totalCount), stat=localrc)
      if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return
      s % fieldDepMap(1:fieldCount)      = tmpMap
      s % fieldDepMap(fieldCount + 1:)   = localDepMap
      deallocate(tmpMap, stat=localrc)
      if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return
      s % fieldMaxRank = max(s % fieldMaxRank, localMaxRank)
    else
      allocate(s % fieldNames(localCount), &
               s % fieldOptions(localCount), &
               s % fieldDepMap(localCount), stat=localrc)
      if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return
      s % fieldNames    = localNames
      s % ugDimLength   = ugDimLength
      s % trAction      = trAction
      s % fieldOptions  = localOptions
      s % fieldSep      = localFieldSep
      s % fieldDepMap   = localDepMap
      s % fieldMaxRank  = localMaxRank
      s % doRotation    = .false.
    end if
    s % parent = state
    nullify(s % next)

    deallocate(localNames, localOptions, localDepMap, stat=localrc)
    if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

  end subroutine StateAdd

  subroutine NamespaceRemoveField(name, state, fieldName, rc)

    character(len=*), intent(in) :: name
    type(ESMF_State)             :: state
    character(len=*), intent(in) :: fieldName
    integer,         intent(out) :: rc

    ! -- local variables
    type(compType),  pointer :: p
    type(stateType), pointer :: s
    integer                  :: item, itemCount, itemFound, localrc
    character(len=ESMF_MAXSTR), dimension(:), allocatable :: tmp
      
    ! -- begin
    rc = ESMF_SUCCESS

    itemFound = 0
    p => compList
    do while (associated(p))
      if (trim(p % name) == name) then
        s => p % stateList
        do while (associated(s))
          if (s % parent == state) then
            if (associated(s % fieldNames)) then
              itemCount = size(s % fieldNames)
              do item = 1, itemCount
                if (trim(s % fieldNames(item)) == fieldName) then
                  itemFound = item
                  exit
                end if
              end do
              if (itemFound > 0) then
                if (itemCount == 1) then
                  deallocate(s % fieldNames, s % fieldOptions, stat=localrc)
                  if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                    line=__LINE__, &
                    file=__FILE__, &
                    rcToReturn=rc)) return
                  nullify(s % fieldNames)
                  nullify(s % fieldOptions)
                else
                  itemCount = itemCount - 1
                  allocate(tmp(itemCount), stat=localrc)
                  if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                    line=__LINE__, &
                    file=__FILE__, &
                    rcToReturn=rc)) return
                  tmp(1:itemFound-1) = s % fieldNames(1:itemFound-1)
                  tmp(itemFound:) = s % fieldNames(itemFound+1:)
                  deallocate(s % fieldNames, stat=localrc)
                  if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                    line=__LINE__, &
                    file=__FILE__, &
                    rcToReturn=rc)) return
                  allocate(s % fieldNames(itemCount), stat=localrc)
                  if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                    line=__LINE__, &
                    file=__FILE__, &
                    rcToReturn=rc)) return
                  s % fieldNames = tmp
                  tmp(1:itemFound-1) = s % fieldOptions(1:itemFound-1)
                  tmp(itemFound:) = s % fieldOptions(itemFound+1:)
                  deallocate(s % fieldOptions, stat=localrc)
                  if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                    line=__LINE__, &
                    file=__FILE__, &
                    rcToReturn=rc)) return
                  allocate(s % fieldOptions(itemCount), stat=localrc)
                  if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                    line=__LINE__, &
                    file=__FILE__, &
                    rcToReturn=rc)) return
                  s % fieldOptions = tmp
                  deallocate(tmp, stat=localrc)
                  if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                    line=__LINE__, &
                    file=__FILE__, &
                    rcToReturn=rc)) return
                end if
                call ESMF_StateRemove(s % self, (/ fieldName /), rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, &
                  file=__FILE__)) &
                  return  ! bail out
                return
              end if
            end if
          end if
          s => s % next
        end do
        exit
      end if
      p => p % next
    end do

  end subroutine NamespaceRemoveField

  ! -- Namespace: Add(Create)/Remove objects/Destroy: end definition --

  ! -- Namespace: coupling actions Advertise/Connect/Realize/Initialize: begin definition --

  ! -- Private methods
  subroutine AdjustFieldGeometry(state, fieldName, petCount, rc)

      type(ESMF_State)              :: state
      character(len=*), intent(in)  :: fieldName
      integer,          intent(in)  :: petCount
      integer,          intent(out) :: rc

      ! -- local variables
      type(ESMF_Field)           :: field
      type(ESMF_DistGrid)        :: distgrid, eDistgrid
      type(ESMF_Grid)            :: grid
      type(ESMF_Mesh)            :: mesh, newMesh
      type(ESMF_GeomType_flag)   :: geomtype
      integer                    :: item, deCount, dimCount, tileCount, localrc
      integer, dimension(:,:), allocatable :: minIndexPTile, maxIndexPTile
      character(len=ESMF_MAXSTR) :: transferAction

      integer                    :: i, j
      integer, dimension(:,:), allocatable :: regDecompPTile
      integer, dimension(:), allocatable :: deToTileMap
      real(ESMF_KIND_R8), dimension(:,:), pointer :: p  !!!!!! TEST
      ! -- begin
      rc = ESMF_SUCCESS

      call ESMF_StateGet(state, field=field, &
        itemName=fieldName, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      call NUOPC_GetAttribute(field, name="TransferActionGeomObject", &
        value=transferAction, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      if (trim(transferAction)=="accept") then
        ! the Connector instructed the Mediator to accept geom object
        ! -> find out which type geom object the field holds
        call ESMF_FieldGet(field, geomtype=geomtype, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        if (geomtype == ESMF_GEOMTYPE_GRID) then

          ! empty field holds a Grid with DistGrid
          call ESMF_FieldGet(field, grid=grid, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          ! access the DistGrid
          call ESMF_GridGet(grid, distgrid=distgrid, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        else if (geomtype == ESMF_GEOMTYPE_MESH) then

          ! empty field holds a Mesh with DistGrids
          call ESMF_FieldGet(field, mesh=mesh, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          ! access the DistGrids (use nodal Distgrid in following code)
          call ESMF_MeshGet(mesh, nodalDistgrid=distgrid, &
            elementDistgrid=eDistgrid, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

        else

          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg="Unsupported geom object found in "// fieldName, &
            line=__LINE__, &
            file=__FILE__, &
            rcToReturn=rc)
          return ! bail out

        end if

        ! get DE count
        call ESMF_DistGridGet(distgrid, deCount=deCount, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

        if (deCount /= petCount) then
          ! Create a custom DistGrid, based on the minIndex, maxIndex of the 
          ! accepted DistGrid, but with a default regDecomp for the current VM
          ! that leads to 1DE/PET.
          ! get dimCount and tileCount
          call ESMF_DistGridGet(distgrid, dimCount=dimCount, deCount=deCount, &
            tileCount=tileCount, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

          allocate(minIndexPTile(dimCount, tileCount), &
            maxIndexPTile(dimCount, tileCount), stat=localrc)
          if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__, &
            rcToReturn=rc)) return

          ! get minIndex and maxIndex arrays
          call ESMF_DistGridGet(distgrid, minIndexPTile=minIndexPTile, &
            maxIndexPTile=maxIndexPTile, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

          ! create the new DistGrid with the same minIndexPTile and maxIndexPTile,
          ! but with a default regDecompPTile
          distgrid = ESMF_DistGridCreate(minIndexPTile=minIndexPTile, &
            maxIndexPTile=maxIndexPTile, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out

          if (geomtype == ESMF_GEOMTYPE_GRID) then

            ! Destroy old Grid object
            call ESMF_GridDestroy(grid, rc=rc)    
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            ! Create a new Grid on the new DistGrid and swap it in the Field
            grid = ESMF_GridCreate(distgrid, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            call ESMF_FieldEmptySet(field, grid=grid, rc=rc)    
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out

          else if (geomtype == ESMF_GEOMTYPE_MESH) then

            ! nodal Distgrid available, create new element DIstgrid

            ! get minIndex and maxIndex arrays
            call ESMF_DistGridGet(eDistgrid, minIndexPTile=minIndexPTile, &
              maxIndexPTile=maxIndexPTile, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out

            ! create the new DistGrid with the same minIndexPTile and maxIndexPTile,
            ! but with a default regDecompPTile
            eDistgrid = ESMF_DistGridCreate(minIndexPTile=minIndexPTile, &
              maxIndexPTile=maxIndexPTile, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out

            ! Create a new Grid on the new DistGrid and swap it in the Field
            newMesh = ESMF_MeshCreate(mesh, nodalDistgrid=distgrid, &
              elementDistgrid=eDistgrid, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            call ESMF_FieldEmptySet(field, mesh=newMesh, rc=rc)    
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            ! destroy old Mesh object
            call ESMF_MeshDestroy(mesh, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out

          end if

          ! local clean-up
          deallocate(minIndexPTile, maxIndexPTile, stat=localrc)
          if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__, &
            rcToReturn=rc)) return

        end if

      end if

  end subroutine AdjustFieldGeometry
    
  subroutine RealizeFieldGeometry(s, fieldName, rc)

      type(stateType), intent(inout) :: s
      character(len=*),   intent(in) :: fieldName
      integer,           intent(out) :: rc

      ! -- local variables
      type(ESMF_Field)               :: field
      type(ESMF_FieldStatus_flag)    :: fieldStatus
      logical                        :: isPresent
      integer                        :: itemCount, localrc
      integer, dimension(:), pointer :: ugLBound, ugUBound, gridToFieldMap

      ! -- retrieve field object
      call ESMF_StateGet(s % self, field=field, itemName=trim(fieldName), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! -- check field status
      call ESMF_FieldGet(field, status=fieldStatus, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      if (fieldStatus == ESMF_FIELDSTATUS_GRIDSET) then
        ! the Connector instructed the Mediator to accept geom object
        ! the transferred geom object is already set, allocate memory 
        ! for data by complete
        nullify(ugLBound, ugUBound, gridToFieldMap)
        ! deal with gridToFieldMap
        call ESMF_AttributeGet(field, name="GridToFieldMap", &
          convention="NUOPC", purpose="Instance", &
          itemCount=itemCount, isPresent=isPresent, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        if (itemCount > 0) then
          allocate(gridToFieldMap(itemCount), stat=localrc)
          if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__, &
            rcToReturn=rc)) return
          call ESMF_AttributeGet(field, name="GridToFieldMap", &
            convention="NUOPC", purpose="Instance", &
            valueList=gridToFieldMap, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        endif
        ! deal with ungriddedLBound
        call ESMF_AttributeGet(field, name="UngriddedLBound", &
          convention="NUOPC", purpose="Instance", &
          itemCount=itemCount, isPresent=isPresent, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        if (itemCount > 0) then
          if (s % ugDimLength < 0 .and. itemCount > 1) then
            call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
              msg="Field "//fieldName//" must have ONE ungridded dimension!", &
              line=__LINE__, &
              file=__FILE__, &
              rcToReturn=rc)
            return  ! bail out
          end if
          if (s % ugDimLength == 0) then
            call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
              msg="Field "//fieldName//" must have NO ungridded dimensions!", &
              line=__LINE__, &
              file=__FILE__, &
              rcToReturn=rc)
            return  ! bail out
          end if
          allocate(ugLBound(itemCount), stat=localrc)
          if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__, &
            rcToReturn=rc)) return
          call ESMF_AttributeGet(field, name="UngriddedLBound", &
            convention="NUOPC", purpose="Instance", &
            valueList=ugLBound, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        end if
        ! deal with ungriddedUBound
        call ESMF_AttributeGet(field, name="UngriddedUBound", &
          convention="NUOPC", purpose="Instance", &
          itemCount=itemCount, isPresent=isPresent, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
        if (itemCount > 0) then
          if (s % ugDimLength < 0 .and. itemCount > 1) then
            call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
              msg="Field "//fieldName//" must have ONE ungridded dimension!", &
              line=__LINE__, &
              file=__FILE__, &
              rcToReturn=rc)
            return  ! bail out
          end if
          if (s % ugDimLength == 0) then
            call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
              msg="Field "//fieldName//" must have NO ungridded dimensions!", &
              line=__LINE__, &
              file=__FILE__, &
              rcToReturn=rc)
            return  ! bail out
          end if
          allocate(ugUBound(itemCount), stat=localrc)
          if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__, &
            rcToReturn=rc)) return
          call ESMF_AttributeGet(field, name="UngriddedUBound", &
            convention="NUOPC", purpose="Instance", &
            valueList=ugUBound, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          if (s % ugDimLength < 0) then
            s % ugDimLength = ugUBound(1) - ugLBound(1) + 1
          else if (s % ugDimLength /= ugUBound(1) - ugLBound(1) + 1) then
            call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
              msg="Field "//fieldName//" ungridded dimension has wrong length.", &
              line=__LINE__, &
              file=__FILE__, &
              rcToReturn=rc)
            return  ! bail out
          end if
        end if

        if (associated(ugLBound) .and. associated(ugUBound)) then
          if (associated(gridToFieldMap)) then
            call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, &
              ungriddedLBound=ugLBound, ungriddedUBound=ugUBound, &
              gridToFieldMap=gridToFieldMap, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
          else
            call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, &
              ungriddedLBound=ugLBound, ungriddedUBound=ugUBound, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
          end if
          deallocate(ugLBound, ugUBound, stat=localrc)
          if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__, &
            rcToReturn=rc)) return
        else
          if (associated(gridToFieldMap)) then
            call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, &
              gridToFieldMap=gridToFieldMap, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
          else
            call ESMF_FieldEmptyComplete(field, typekind=ESMF_TYPEKIND_R8, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
          end if
        end if

        if (associated(ugLBound)) then
          deallocate(ugLBound, stat=localrc)
          if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__, &
            rcToReturn=rc)) return
        end if

        if (associated(ugUBound)) then
          deallocate(ugUBound, stat=localrc)
          if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__, &
            rcToReturn=rc)) return
        end if

        if (associated(gridToFieldMap)) then
          deallocate(gridToFieldMap, stat=localrc)
          if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__, &
            rcToReturn=rc)) return
        end if

      end if
      
  end subroutine RealizeFieldGeometry

  ! -- Public methods

  subroutine NamespaceAdvertise(rc)
    integer, intent(out) :: rc

    ! -- local variables
    type(compType),  pointer :: p
    type(stateType), pointer :: s

    ! -- begin
    rc = ESMF_SUCCESS

    nullify(p, s)

    p => compList
    do while (associated(p))
      s => p % stateList
      do while (associated(s))
        call NUOPC_AddNamespace(s % parent, namespace=trim(p % name), &
          nestedState=s % self, rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__)) &
          return
        call NUOPC_Advertise(s % self, StandardNames=s % fieldNames, &
          TransferOfferGeomObject=trim(s % trAction), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__)) &
          return
        s => s % next
      end do
      p => p % next
    end do

    nullify(p, s)
      
  end subroutine NamespaceAdvertise

  subroutine NamespaceCheckConnectedFields(rc)

    integer, intent(out) :: rc

    ! -- local variables
    type (ESMF_Field)          :: field
    type (compType),  pointer  :: p
    type (stateType), pointer  :: s
    character(len=ESMF_MAXSTR) :: stateName
    character(len=ESMF_MAXSTR) :: connectedValue, transferAction
    integer :: item
    
    ! -- begin
    rc = ESMF_SUCCESS

    nullify(p, s)

    p => compList
    do while (associated(p))
      s => p % stateList
      do while (associated(s))
        do item = 1, size(s % fieldNames)
          call ESMF_StateGet(s % self, field=field, &
            itemName=trim(s % fieldNames(item)), rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          call NUOPC_GetAttribute(field, name="Connected", &
            value=connectedValue, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
          if (trim(connectedValue) == "false") then
            call ESMF_StateGet(s % self, name=stateName, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            call ESMF_LogSetError(ESMF_RC_NOT_FOUND, &
              msg="Field "//trim(p % name)//"/"//trim(s % fieldNames(item)) &
              //" in State "//trim(stateName)//" is not connected.", &
              line=__LINE__, &
              file=__FILE__, &
              rcToReturn=rc)
            return ! bail out
          else
            call NUOPC_GetAttribute(field, name="TransferActionGeomObject", &
              value=transferAction, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            if (trim(transferAction)=="provide") then
              ! the Connector instructed the Mediator to provide geom object
              call ESMF_StateGet(s % self, name=stateName, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
              call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
                msg="Cannot fulfill request to provide geom object for "// &
                trim(s % fieldNames(item))//" in State "//trim(stateName), &
                line=__LINE__, &
                file=__FILE__, &
                rcToReturn=rc)
              return ! bail out
            end if
          end if
        end do
        s => s % next
      end do
      p => p % next
    end do
    
  end subroutine NamespaceCheckConnectedFields

  subroutine NamespaceAdjustFields(gridComp, rc) 
    type(ESMF_GridComp), optional              :: gridComp
    integer,             optional, intent(out) :: rc

    ! -- local variables
    type(compType),  pointer :: p
    type(stateType), pointer :: s
    type(ESMF_VM)            :: vm
    integer                  :: item, petCount

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    ! -- get number of PET for current gridded component, if present
    if (present(gridComp)) then

      ! -- get VM for gridded component
      call ESMF_GridCompGet(gridComp, vm=vm, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

      ! -- retrieve PET information
      call ESMF_VMGet(vm, petCount=petCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out

    else

      petCount = -1

    end if

    p => compList
    nullify(s)
    do while (associated(p))
      s => p % stateList
      do while (associated(s))
        do item = 1, size(s % fieldNames)
          call AdjustFieldGeometry(s % self, trim(s % fieldNames(item)), &
            petCount, rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        end do
        s => s % next
      end do
      p => p % next
    end do

    nullify(p, s)

  end subroutine NamespaceAdjustFields

  subroutine NamespaceUpdateFields(name, state, grid, mesh, rc)
    character(len=*),          intent(in)  :: name
    type(ESMF_State),          intent(in)  :: state
    type(ESMF_Grid), optional, intent(in)  :: grid
    type(ESMF_Mesh), optional, intent(in)  :: mesh
    integer,         optional, intent(out) :: rc

    ! -- local variables
    type(compType),  pointer :: p
    type(stateType), pointer :: s
    type(ESMF_Field)         :: field
    logical                  :: isGeom
    integer                  :: item, localrc

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    if (present(grid).or.present(mesh)) then
      p => compList
      nullify(s)
      do while (associated(p))
        if (trim(p % name) == name) then
          s => p % stateList
          do while (associated(s))
            if (s % parent == state) then
              do item = 1, size(s % fieldNames)
                call ESMF_StateGet(s % self, field=field, &
                  itemName=trim(s % fieldNames(item)), rc=localrc)
                if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, &
                  file=__FILE__,  &
                  rcToReturn=rc)) &
                  return  ! bail out
                if (present(grid)) then
                  call ESMF_FieldEmptySet(field, grid=grid, rc=localrc)
                  if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                    line=__LINE__, &
                    file=__FILE__,  &
                    rcToReturn=rc)) &
                    return  ! bail out
                else if (present(mesh)) then
                  call ESMF_FieldEmptySet(field, mesh=mesh, rc=localrc)
                  if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                    line=__LINE__, &
                    file=__FILE__,  &
                    rcToReturn=rc)) &
                    return  ! bail out
                end if
              end do
            end if
            s => s % next
          end do
        end if
        p => p % next
      end do
    else
      call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
        msg="No Grid or Mesh object provided", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return ! bail out
    end if

  end subroutine NamespaceUpdateFields

  subroutine NamespaceRealizeFields(rc)
    integer, intent(out) :: rc

    ! -- local variable
    type(compType),  pointer :: p
    type(stateType), pointer :: s
    integer :: item

    ! -- begin
    rc = ESMF_SUCCESS

    p => compList
    nullify(s)
    do while (associated(p))
      s => p % stateList
      do while (associated(s))
        do item = 1, size(s % fieldNames)
          call RealizeFieldGeometry(s, trim(s % fieldNames(item)), rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        end do
        s => s % next
      end do
      p => p % next
    end do

    nullify(p, s)

  end subroutine NamespaceRealizeFields

  subroutine NamespaceInitializeFields(value, rc)

    real(ESMF_KIND_R8), optional,  intent(in) :: value
    integer,            optional, intent(out) :: rc
    
    ! -- local variables
    integer                     :: localrc, item, rank
    type(compType),     pointer :: p
    type(stateType),    pointer :: s
    real(ESMF_KIND_R8), pointer :: fptr1d(:), fptr2d(:,:), fptr3d(:,:,:)
    real(ESMF_KIND_R8)          :: initValue
    type(ESMF_Field)            :: field
    type(ESMF_StateIntent_flag) :: stateIntent

    real(ESMF_KIND_R8), parameter :: defaultInitValue = 0._ESMF_KIND_R8

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    initValue = defaultInitValue
    if (present(value)) initValue = value

    nullify(s)
    p => compList
    do while (associated(p))
      s => p % stateList
      do while (associated(s))

        call ESMF_StateGet(s % parent, stateintent=stateIntent, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out

        do item = 1, size(s % fieldNames)

          call ESMF_StateGet(s % self, field=field, &
            itemName=trim(s % fieldNames(item)), rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out

#if 0
          call ESMF_FieldGet(field, rank=rank, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out

          select case (rank)
            case(1)
              call ESMF_FieldGet(field, farrayPtr=fptr1d, rc=localrc)
              if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__,  &
                file=__FILE__,  &
                rcToReturn=rc)) &
                return  ! bail out
              fptr1d = initValue
            case(2)
              call ESMF_FieldGet(field, farrayPtr=fptr2d, rc=localrc)
              if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__,  &
                file=__FILE__,  &
                rcToReturn=rc)) &
                return  ! bail out
              fptr2d = initValue
            case(3)
              call ESMF_FieldGet(field, farrayPtr=fptr3d, rc=localrc)
              if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__,  &
                file=__FILE__,  &
                rcToReturn=rc)) &
                return  ! bail out
              fptr3d = initValue
            case default
              call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
                msg="Invalid dimensions for field "//trim(s % fieldNames(item)), &
                line=__LINE__, &
                file=__FILE__, &
                rcToReturn=rc)
                return ! bail out
          end select

#endif
          ! --- mark as updated only if in export state??
          if (stateIntent == ESMF_STATEINTENT_EXPORT) then
            call NUOPC_SetAttribute(field, name="Updated", value="true", rc=localrc)
            if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__,  &
              file=__FILE__,  &
              rcToReturn=rc)) &
              return  ! bail out
          end if

        end do
        s => s % next
      end do
      p => p % next
    end do

    nullify(p, s)

    ! -- initialize internal structures for vector rotation
    call NamespaceSetupRotation(rc)

  end subroutine NamespaceInitializeFields
  
  subroutine NamespaceSetupRotation(rc)
    integer, intent(out) :: rc

    ! -- local variable
    type(compType),  pointer :: p
    type(stateType), pointer :: s
    type(ESMF_Mesh)          :: mesh
    type(ESMF_Field)         :: field
    type(ESMF_GeomType_Flag) :: geomtype
    integer                  :: item

    ! -- begin
    rc = ESMF_SUCCESS

    p => compList
    nullify(s)
    do while (associated(p))
      s => p % stateList
      do while (associated(s))
        if (s % fieldMaxRank > 1) then
          if (ESMF_FieldIsCreated(s % localCartField)) then
            s % doRotation = .true.
          else
            if (ESMF_GridIsCreated(s % localGrid)) then
              ! -- do nothing for now
            else if (ESMF_MeshIsCreated(s % localMesh)) then
              s % doRotation = .true.
            else
              item = size(s % fieldNames)
              if (item > 0) then
                call ESMF_StateGet(s % self, itemName=trim(s % fieldNames(item)), &
                  field=field, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__,  &
                  file=__FILE__)) return  ! bail out
                call ESMF_FieldGet(field, geomtype=geomtype, rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__,  &
                  file=__FILE__)) return  ! bail out
                if (geomtype == ESMF_GEOMTYPE_MESH) then
                  call ESMF_FieldGet(field, mesh=mesh, rc=rc)
                  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                    line=__LINE__,  &
                    file=__FILE__)) return  ! bail out
                  s % localMesh = mesh
                  s % doRotation = .true.
                end if
              end if
            end if
            if (s % doRotation) then
              ! -- create localCartField on Mesh only for now
              s % localCartField = ESMF_FieldCreate(s % localMesh, ESMF_TYPEKIND_R8, &
                gridToFieldMap=(/2/), ungriddedLBound=(/1/), ungriddedUBound=(/3/), &
                meshloc=ESMF_MESHLOC_NODE, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__,  &
                file=__FILE__)) return  ! bail out
            end if
          end if
        end if
        if (s % doRotation) then
          call StateSetLocalVectors(s, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__)) return ! bail out
        else
          ! -- reset fields as scalars if no rotation is performed
          s % fieldDepMap = 0
        end if
        s => s % next
      end do
      p => p % next
    end do

    nullify(p, s)

  end subroutine NamespaceSetupRotation

  subroutine NamespaceCheckDE(rc)

    integer, optional, intent(out) :: rc

    ! -- local variables
    integer                  :: localrc, ldeCount, item
    type(compType),  pointer :: p
    type(stateType), pointer :: s
    type(ESMF_Field)         :: field
    character(ESMF_MAXSTR)   :: errmsg

    integer,       parameter :: localDeCount = 1

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    write(errmsg,'("The SWPC mediator only supports fields with local DE count = ",i0)') &
      localDeCount

    nullify(s)
    p => compList
    do while (associated(p))
      s => p % stateList
      do while (associated(s))

        item = 0
        if (associated(s % fieldNames)) item = size(s % fieldNames)

        if (item > 0) then

          call ESMF_StateGet(s % self, field=field, &
            itemName=trim(s % fieldNames(item)), rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out

          call ESMF_FieldGet(field, localDeCount=ldeCount, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out

          ! -- if field's localDeCount is different than input localDeCount, bail out
          if (ldeCount /= localDeCount) then
            call ESMF_LogSetError(ESMF_RC_VAL_OUTOFRANGE, msg=errmsg, &
              line=__LINE__,  &
              file=__FILE__,  &
              rcToReturn=rc)
            return  ! bail out
          end if

        end if
        s => s % next
      end do
      p => p % next
    end do

    nullify(p, s)

  end subroutine NamespaceCheckDE

  subroutine NamespaceDestroy(nsList, rc)

    type(compType), optional, pointer     :: nsList
    integer,        optional, intent(out) :: rc

    ! -- local variables
    type(compType),  pointer :: p, q
    type(stateType), pointer :: s, r
    integer                  :: localrc

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    if (present(nsList)) then
      p => nsList
    else
      p => compList
    end if

    ! -- connected components
    do while (associated(p))
      q => p
      p => p % next
      s => q % stateList
      do while (associated(s))
        r => s
        s => s % next
        if (associated(r % fieldNames)) then
          deallocate(r % fieldNames, stat=localrc)
          if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__, &
            rcToReturn=rc)) return ! bail out
          nullify(r % fieldNames)
        end if
        if (associated(r % fieldOptions)) then
          deallocate(r % fieldOptions, stat=localrc)
          if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__, &
            rcToReturn=rc)) return ! bail out
          nullify(r % fieldOptions)
        end if
        deallocate(r, stat=localrc)
        if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)) return ! bail out
        nullify(r)
      end do
      deallocate(q, stat=localrc)
      if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return ! bail out
      nullify(q)
    end do

    if (present(nsList)) then
      nullify(nsList)
    else
      nullify(compList)
    end if
    
  end subroutine NamespaceDestroy

  ! -- Namespace: coupling actions Advertise/Connect/Realize/Adjust/Initialize: end definition --

  ! -- Namespace: Create/Get/Set methods: begin definition --

  function NamespaceGetList(rc)
    integer, optional, intent(out) :: rc
    type(compType),        pointer :: NamespaceGetList

    if (present(rc)) rc = ESMF_SUCCESS

    NamespaceGetList => compList

  end function NamespaceGetList

  subroutine NamespaceSetList(nsList, rc)
    integer, optional, intent(out) :: rc
    type(compType),        pointer :: nsList

    if (present(rc)) rc = ESMF_SUCCESS

    compList => nsList

  end subroutine NamespaceSetList

  function NamespaceGetField(name, state, fieldName, rc)
    ! -- input variables
    character(len=*),   intent(in) :: name
    type(ESMF_State),   intent(in) :: state
    character(len=*),   intent(in) :: fieldName
    integer, optional, intent(out) :: rc

    ! -- output variables
    type(ESMF_Field)               :: NamespaceGetField

    ! -- local variables
    integer                  :: item, localrc
    type(compType),  pointer :: p
    type(stateType), pointer :: s

    ! -- begin
    if (present(rc)) rc = ESMF_RC_NOT_FOUND

    nullify(s)
    p => compList
    do while (associated(p))
      if (trim(p % name) == trim(name)) then
        s => p % stateList
        do while (associated(s))
          if (s % parent == state) then
            if (associated(s % fieldNames)) then
              do item = 1, size(s % fieldNames) 
                if (trim(s % fieldNames(item)) == trim(fieldName)) then
                  call ESMF_StateGet(s % self, itemName=trim(fieldName), &
                    field=NamespaceGetField, rc=localrc)
                  if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                    line=__LINE__, &
                    file=__FILE__, &
                    rcToReturn=rc)) return  ! bail out
                  if (present(rc)) rc = ESMF_SUCCESS
                  return
                end if
              end do
            end if
          end if
          s => s % next
        end do
      end if
      p => p % next
    end do

    nullify(p, s)

  end function NamespaceGetField

  subroutine NamespaceGet(name, state, geomtype, grid, mesh, rc)
    character(len=*),                    intent(in) :: name
    type(ESMF_State),                    intent(in) :: state
    type(ESMF_GeomType_Flag), optional, intent(out) :: geomtype
    type(ESMF_Grid),          optional, intent(out) :: grid
    type(ESMF_Mesh),          optional, intent(out) :: mesh
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer                     :: localrc
    type(compType),     pointer :: p
    type(stateType),    pointer :: s
    type(ESMF_Field)            :: field
    type(ESMF_Grid)             :: localGrid
    type(ESMF_Mesh)             :: localMesh
    type(ESMF_GeomType_Flag)    :: localGeomType
    type(ESMF_FieldStatus_Flag) :: fieldStatus

    ! -- begin
    localrc = ESMF_RC_NOT_FOUND
    if (present(rc)) rc = ESMF_RC_NOT_FOUND

    nullify(s)
    p => compList
    do while (associated(p))
      if (trim(p % name) == trim(name)) then
        s => p % stateList
        do while (associated(s))
          if (s % parent == state) then
            if (associated(s % fieldNames)) then
              if (size(s % fieldNames) > 0) then
                call ESMF_StateGet(s % self, itemName=trim(s % fieldNames(1)), &
                  field=field, rc=localrc)
                if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, &
                  file=__FILE__, &
                  rcToReturn=rc)) return  ! bail out
              end if
            end if
          end if
          s => s % next
        end do
      end if
      p => p % next
    end do

    nullify(p, s)

    if (localrc == ESMF_RC_NOT_FOUND) then
      call ESMF_LogSetError(localrc, &
        msg="No field found in namespace "//trim(name), &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return ! bail out
    end if

    ! -- check if field is completed
    call ESMF_FieldGet(field, status=fieldStatus, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE .and. &
        fieldStatus /= ESMF_FIELDSTATUS_GRIDSET) then
      call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
        msg="Field has not been completely created.", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return ! bail out
    end if

    ! -- check if field contains valid grid object
    call ESMF_FieldGet(field, geomtype=localGeomType, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    if (present(geomtype)) geomtype = localGeomType

    if (localGeomType == ESMF_GEOMTYPE_GRID) then
      if (present(grid)) then
        ! -- get grid from field
        call ESMF_FieldGet(field, grid=grid, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)) return  ! bail out
      end if
    else if (localGeomType == ESMF_GEOMTYPE_MESH) then
      if (present(mesh)) then
        ! -- get mesh from field
        call ESMF_FieldGet(field, mesh=mesh, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)) return  ! bail out
      end if
    end if

    ! -- return grid object
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine NamespaceGet

  function NamespaceGetGrid(name, state, rc)
    character(len=*),   intent(in) :: name
    type(ESMF_State)               :: state
    integer, optional, intent(out) :: rc

    type(ESMF_Grid) :: NamespaceGetGrid

    ! -- local variables
    integer                     :: localrc
    type(compType),     pointer :: p
    type(stateType),    pointer :: s
    type(ESMF_Field)            :: field
    type(ESMF_Grid)             :: grid
    type(ESMF_GeomType_Flag)    :: geomtype
    type(ESMF_FieldStatus_Flag) :: fieldStatus

    ! -- begin
    localrc = ESMF_RC_NOT_FOUND
    if (present(rc)) rc = ESMF_RC_NOT_FOUND

    nullify(s)
    p => compList
    do while (associated(p))
      if (trim(p % name) == trim(name)) then
        s => p % stateList
        do while (associated(s))
          if (s % parent == state) then
            if (associated(s % fieldNames)) then
              if (size(s % fieldNames) > 0) then
                call ESMF_StateGet(s % self, itemName=trim(s % fieldNames(1)), &
                  field=field, rc=localrc)
                if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, &
                  file=__FILE__, &
                  rcToReturn=rc)) return  ! bail out
              end if
            end if
          end if
          s => s % next
        end do
      end if
      p => p % next
    end do

    nullify(p, s)

    if (localrc == ESMF_RC_NOT_FOUND) then
      call ESMF_LogSetError(localrc, &
        msg="Field not found.", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return ! bail out
    end if

    ! -- check if field is completed
    call ESMF_FieldGet(field, status=fieldStatus, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) then
      call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
        msg="Field has not been completely created.", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return ! bail out
    end if

    ! -- check if field contains valid grid object
    call ESMF_FieldGet(field, geomtype=geomtype, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    if (geomtype /= ESMF_GEOMTYPE_GRID) then
      call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
        msg="No Grid object found in field ", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return ! bail out
    end if

    ! -- get grid from field
    call ESMF_FieldGet(field, grid=grid, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out

    ! -- return grid object
    if (present(rc)) rc = ESMF_SUCCESS
    NamespaceGetGrid = grid
    
  end function NamespaceGetGrid

!------------------------------------------------------------------------------

  function NamespaceGetMesh(name, state, rc)
    character(len=*),   intent(in) :: name
    type(ESMF_State)               :: state
    integer, optional, intent(out) :: rc

    type(ESMF_Mesh) :: NamespaceGetMesh

    ! -- local variables
    integer                     :: localrc
    type(compType),     pointer :: p
    type(stateType),    pointer :: s
    type(ESMF_Field)            :: field
    type(ESMF_Mesh)             :: mesh
    type(ESMF_GeomType_Flag)    :: geomtype
    type(ESMF_FieldStatus_Flag) :: fieldStatus

    ! -- begin
    localrc = ESMF_RC_NOT_FOUND
    if (present(rc)) rc = ESMF_RC_NOT_FOUND

    nullify(s)
    p => compList
    do while (associated(p))
      if (trim(p % name) == trim(name)) then
        s => p % stateList
        do while (associated(s))
          if (s % parent == state) then
            if (associated(s % fieldNames)) then
              if (size(s % fieldNames) > 0) then
                call ESMF_StateGet(s % self, itemName=trim(s % fieldNames(1)), &
                  field=field, rc=localrc)
                if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__, &
                  file=__FILE__, &
                  rcToReturn=rc)) return  ! bail out
              end if
            end if
          end if
          s => s % next
        end do
      end if
      p => p % next
    end do

    nullify(p, s)

    if (localrc == ESMF_RC_NOT_FOUND) then
      call ESMF_LogSetError(localrc, &
        msg="Field not found.", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return ! bail out
    end if

    ! -- check if field is completed
    call ESMF_FieldGet(field, status=fieldStatus, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) then
      call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
        msg="Field has not been completely created.", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return ! bail out
    end if

    ! -- check if field contains valid mesh object
    call ESMF_FieldGet(field, geomtype=geomtype, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    if (geomtype /= ESMF_GEOMTYPE_MESH) then
      call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
        msg="No Mesh object found in field ", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return ! bail out
    end if

    ! -- get mesh from field
    call ESMF_FieldGet(field, mesh=mesh, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out

    ! -- return mesh object
    if (present(rc)) rc = ESMF_SUCCESS
    NamespaceGetMesh = mesh
    
  end function NamespaceGetMesh

!------------------------------------------------------------------------------

  subroutine NamespaceGetLocal(name, geomtype, grid, mesh, rc)
    character(len=*),          intent(in) :: name
    type(ESMF_GeomType_Flag), intent(out) :: geomtype
    type(ESMF_Grid),          intent(out) :: grid
    type(ESMF_Mesh),          intent(out) :: mesh
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer                     :: localrc
    type(compType),     pointer :: p
    type(stateType),    pointer :: s

    ! -- begin
    localrc = ESMF_RC_NOT_FOUND
    if (present(rc)) rc = ESMF_RC_NOT_FOUND

    geomtype = ESMF_GEOMTYPE_UNINIT

    nullify(s)
    p => compList
    do while (associated(p))
      if (trim(p % name) == trim(name)) then
        s => p % stateList
        do while (associated(s))
          if (ESMF_GridIsCreated(s % localGrid)) then
            grid = s % localGrid
            geomtype = ESMF_GEOMTYPE_GRID
            localrc  = ESMF_SUCCESS
          else if (ESMF_MeshIsCreated(s % localMesh)) then
            mesh = s % localMesh
            geomtype = ESMF_GEOMTYPE_MESH
            localrc  = ESMF_SUCCESS
          end if
          s => s % next
        end do
      end if
      p => p % next
    end do

    nullify(p, s)

    if (localrc == ESMF_RC_NOT_FOUND) then
      call ESMF_LogSetError(localrc, &
        msg="No Grid or Mesh object found in namespace "//trim(name), &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
    else
      call ESMF_LogSetError(localrc, &
        msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
    end if

  end subroutine NamespaceGetLocal

!------------------------------------------------------------------------------

  function GridCreateFromGrid(grid, minIndex, maxIndex, rc)
    ! -- input variables
    type(ESMF_Grid),   intent(in) :: grid
    integer, optional, intent(in) :: minIndex
    integer,           intent(in) :: maxIndex

    ! -- output variables
    type(ESMF_Grid) :: GridCreateFromGrid
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc
    integer :: connectionCount, deCount, dimCount, itemCount, tileCount
    integer :: ldimCount, localDe, localDeCount, minIndx
    integer :: item
    integer :: tileIndexA, tileIndexB
    integer, dimension(:),       pointer :: positionVector,    orientationVector
    integer, dimension(:),       pointer :: newPositionVector, newOrientationVector
    integer, dimension(:),   allocatable :: coordDimCount,    distgridToGridMap
    integer, dimension(:),   allocatable :: newcoordDimCount, newdistgridToGridMap
    integer, dimension(:,:), allocatable :: coordDimMap,    minIndexPTile,    maxIndexPTile
    integer, dimension(:,:), allocatable :: newcoordDimMap, newminIndexPTile, newmaxIndexPTile
    real(ESMF_KIND_R8), dimension(:),     pointer :: fptrIn1d, fptrOut1d
    real(ESMF_KIND_R8), dimension(:,:),   pointer :: fptrIn2d, fptrOut2d
    type(ESMF_DistGridConnection), dimension(:), allocatable :: connectionList, newconnectionList
    type(ESMF_DistGrid)         :: distgrid, newdistgrid
    type(ESMF_Grid)             :: newgrid
    type(ESMF_GeomType_Flag)    :: geomtype
    type(ESMF_Index_Flag)       :: indexflag
    type(ESMF_CoordSys_Flag)    :: coordSys

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    ! -- check additional dimension bounds
    minIndx = 1
    if (present(minIndex)) then
      minIndx = minIndex 
    end if

    if (maxIndex <= minIndx) then
      call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
        msg="maxIndex must be > minIndex", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return ! bail out
    end if

    ! -- get grid parameters and associated DistGrid object
    call ESMF_GridGet(grid, distgrid=distgrid, &
      dimCount=dimCount, coordSys=coordSys, indexflag=indexflag, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out

    if (dimCount /= 2) then
      call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
        msg="Grid object in field MUST have 2 dimensions", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return ! bail out
    end if

    ! -- get 2D distribution information from Grid's DistGrid object
    allocate(coordDimCount(dimCount),  &
      distgridToGridMap(dimCount),     &
      coordDimMap(dimCount,dimCount), &
      stat=localrc)
    if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    call ESMF_GridGet(grid, coordDimCount=coordDimCount, &
      distgridToGridMap=distgridToGridMap, &
      coordDimMap=coordDimMap, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    ! -- increment dimension count by one to build local 3D Grid
    ldimCount = dimCount + 1

    ! -- create mapping arrays for 3D Grid by extending original ones from 2D Grid
    allocate(newcoordDimCount(ldimCount),  &
      newdistgridToGridMap(ldimCount),     &
      newcoordDimMap(ldimCount,ldimCount), &
      stat=localrc)
    if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    newcoordDimCount(1:dimCount)    = coordDimCount
    newcoordDimCount(ldimCount)     = 3

    newdistgridToGridMap(1:dimCount) = distgridToGridMap
    newdistgridToGridMap(ldimCount)  = 3

    newcoordDimMap(1:dimCount,1:dimCount) = coordDimMap
    newcoordDimMap(:, ldimCount) = 1
    newcoordDimMap(ldimCount, :) = (/ 1, 2, 3 /)

    deallocate(coordDimCount, distgridToGridMap, coordDimMap, stat=localrc)
    if (ESMF_LogFoundDeallocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    ! -- in a similar fashion, extend index/tile arrays and connection settings
    ! -- for DistGrid object in new 3D Grid

    ! -- get original DistGrid information
    call ESMF_DistGridGet(distgrid, &
      tileCount=tileCount, connectionCount=connectionCount, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    allocate(minIndexPTile(dimCount, tileCount), &
             maxIndexPTile(dimCount, tileCount), &
             connectionList(connectionCount),    &
             stat=localrc)
    if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    ! -- get original index arrays and connection list
    call ESMF_DistGridGet(distgrid, minIndexPTile=minIndexPTile, &
      maxIndexPTile=maxIndexPTile, connectionList=connectionList, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    ! -- create new index arrays
    allocate(newminIndexPTile(ldimCount, tileCount), &
             newmaxIndexPTile(ldimCount, tileCount), &
             stat=localrc)
    if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    newminIndexPTile(1:dimCount,:) = minIndexPTile
    newmaxIndexPTile(1:dimCount,:) = maxIndexPTile
    newminIndexPTile(ldimCount, :) = minIndx
    newmaxIndexPTile(ldimCount, :) = maxIndex

    deallocate(minIndexPTile, maxIndexPTile, stat=localrc)
    if (ESMF_LogFoundDeallocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    ! -- extend connection list for new Grid
    allocate(newConnectionList(connectionCount), &
             newPositionVector(ldimCount), newOrientationVector(ldimCount), &
             positionVector(dimCount),     orientationVector(dimCount), &
             stat=localrc)
    if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    do item = 1, connectionCount
      ! -- WARNING: this interface needs to be finalized
      call ESMF_DistGridConnectionGet(connectionList(item), &
        tileIndexA=tileIndexA, tileIndexB=tileIndexB, &
        positionVector=positionVector, orientationVector=orientationVector, &
        rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return

      newPositionVector(1:dimCount) = positionVector
      newPositionVector( ldimCount) = 0
      newOrientationVector(1:dimCount) = orientationVector
      newOrientationVector( ldimCount) = 3

      call ESMF_DistGridConnectionSet(newConnectionList(item), &
        tileIndexA=tileIndexA, tileIndexB=tileIndexB, &
        positionVector=newPositionVector, orientationVector=newOrientationVector, &
        rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return
    end do

    deallocate(positionVector, newPositionVector, orientationVector, newOrientationVector, &
      connectionList, stat=localrc)
    if (ESMF_LogFoundDeallocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    ! -- create 3D DistGrid object
    newdistgrid = ESMF_DistGridCreate(minIndexPTile=newminIndexPTile, &
      maxIndexPTile=newmaxIndexPTile, connectionList=newConnectionList, &
      rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    deallocate(newminIndexPTile, newmaxIndexPTile, newconnectionList, stat=localrc)
    if (ESMF_LogFoundDeallocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    ! -- create 3D Grid object
    newgrid = ESMF_GridCreate(newdistgrid, coordDimCount=newcoordDimCount, &
      coordDimMap=newcoordDimMap, coordSys=coordSys, indexflag=indexflag, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    deallocate(newcoordDimMap, stat=localrc)
    if (ESMF_LogFoundDeallocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    ! -- get localDeCount
    call ESMF_GridGet(newgrid, localDeCount=localDeCount, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    ! -- add coordinates to 3D Grid
    call ESMF_GridAddCoord(newgrid, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    ! -- load 2D coordinates
    do item = 1, 2
      select case (newcoordDimCount(item))
        case (1)
          do localDe = 0, localDeCount - 1
            call ESMF_GridGetCoord(grid, coordDim=item, localDE=localDe, &
              farrayPtr=fptrOut1d, rc=localrc)
            if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__, &
              rcToReturn=rc)) return
            call ESMF_GridGetCoord(newgrid, coordDim=item, localDE=localDe, &
              farrayPtr=fptrIn1d, rc=localrc)
            if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__, &
              rcToReturn=rc)) return
            fptrIn1d = fptrOut1d
          end do
        case (2)
          do localDe = 0, localDeCount - 1
            call ESMF_GridGetCoord(grid, coordDim=item, localDE=localDe, &
              farrayPtr=fptrOut2d, rc=localrc)
            if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__, &
              rcToReturn=rc)) return
            call ESMF_GridGetCoord(newgrid, coordDim=item, localDE=localDe, &
              farrayPtr=fptrIn2d, rc=localrc)
            if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__, &
              rcToReturn=rc)) return
            fptrIn2d = fptrOut2d
          end do
        case default
            write(6,'("newcoord: ",i0,2x,"NO COORDINATE SET")') item
            flush 6
      end select
    end do

    deallocate(newcoordDimCount, stat=localrc)
    if (ESMF_LogFoundDeallocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    GridCreateFromGrid = newgrid

  end function GridCreateFromGrid

!------------------------------------------------------------------------------

  function GridAddNewCoord(grid, staggerloc, coord, scale, offset, rc)
    ! -- input variables
    type(ESMF_Grid),                    intent(in) :: grid
    type(ESMF_StaggerLoc),    optional, intent(in) :: staggerloc
    real(ESMF_KIND_R8),                 intent(in) :: coord(:)
    real(ESMF_KIND_R8),       optional, intent(in) :: scale
    real(ESMF_KIND_R8),       optional, intent(in) :: offset

    ! -- output variables
    integer, optional, intent(out) :: rc
    type(ESMF_Grid) :: GridAddNewCoord

    ! -- local variables
    logical :: isPresent
    integer :: localrc
    integer :: localDe, localDeCount
    integer :: k, lsize
    integer :: minIndx, maxIndx
    integer, dimension(3)       :: lbnd, ubnd
    real(ESMF_KIND_R8)          :: scale_factor, add_offset
    real(ESMF_KIND_R8), pointer :: fptrIn3d(:,:,:)
    type(ESMF_Grid)             :: newgrid

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    newgrid = GridCreateFromGrid(grid, maxIndex=size(coord), rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    isPresent = .false.
    call ESMF_GridGetCoord(newgrid, staggerloc=staggerloc, &
      isPresent=isPresent, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    if (.not.isPresent) then
      call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
        msg="This stagger location was not included in the new grid", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return ! bail out
    end if

    ! -- get localDeCount
    call ESMF_GridGet(newgrid, localDeCount=localDeCount, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    ! -- load vertical coordinate
    scale_factor = 1._ESMF_KIND_R8
    add_offset   = 0._ESMF_KIND_R8
    if (present(scale)) scale_factor = scale
    if (present(offset)) add_offset  = offset

    do localDe = 0, localDeCount - 1

      ! -- get coordinate pointer from new grid
      call ESMF_GridGetCoord(newgrid, coordDim=3, localDE=localDe, &
        staggerloc=staggerloc, &
        computationalLBound=lbnd, computationalUBound=ubnd, &
        farrayPtr=fptrIn3d, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return

      ! -- check allocated memory size
      lsize = ubnd(3)-lbnd(3)+1
      if (lsize /= size(coord)) then
        call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
          msg="size of coord array does not match internal coordinate size",&
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return ! bail out
      end if

      do k = 1, lsize
        fptrIn3d(lbnd(1):ubnd(1),lbnd(2):ubnd(2),k+lbnd(3)-1) = scale_factor * coord(k) + add_offset
      end do

    end do

    GridAddNewCoord = newgrid

  end function GridAddNewCoord

!------------------------------------------------------------------------------

  function GridCreateFromField(field, scale, offset, rc)
    ! -- input variables
    type(ESMF_Field), intent(in) :: field
    real(ESMF_KIND_R8), optional, intent(in) :: scale, offset

    ! -- output variables
    type(ESMF_Grid) :: GridCreateFromField
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc
    integer :: localDe, localDeCount
    integer :: item, itemCount
    integer :: ungriddedBound, vSize
    logical :: isPresent
    real(ESMF_KIND_R8)          :: scale_factor, add_offset
    real(ESMF_KIND_R8), dimension(:,:,:), pointer :: fptrIn3d, fptrOut3d
    type(ESMF_Grid)             :: grid, newgrid
    type(ESMF_FieldStatus_Flag) :: fieldStatus
    type(ESMF_GeomType_Flag)    :: geomtype

    character(len=*), dimension(2), parameter :: &
      AttributeList = (/ "UngriddedLBound", "UngriddedUBound" /)

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    ! -- check if field is completed
    call ESMF_FieldGet(field, status=fieldStatus, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out

    if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) then
      call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
        msg="Field has not been completely created.", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return ! bail out
    end if

    ! -- check if field contains valid grid object
    call ESMF_FieldGet(field, geomtype=geomtype, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out

    if (geomtype /= ESMF_GEOMTYPE_GRID) then
      call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
        msg="No Grid object found in field ", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return ! bail out
    end if

    ! -- check if field has ungridded dimension
    vSize = 0
    do item = 1, 2
      call ESMF_AttributeGet(field, name=trim(AttributeList(item)), &
        convention="NUOPC", purpose="Instance", &
        itemCount=itemCount, isPresent=isPresent, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return  ! bail out
      if (itemCount == 1) then
        call ESMF_AttributeGet(field, name=trim(AttributeList(item)), &
          convention="NUOPC", purpose="Instance", &
          value=ungriddedBound, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)) return  ! bail out
        vSize = ungriddedBound - vSize
      else
        call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
          msg="Field must have ONE ungridded dimension!", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
          return  ! bail out
       end if
     end do
     vSize = vSize + 1

    ! -- create 3D grid from field's 2D grid and vertical dimension
    ! -- get original 2D grid from field
    call ESMF_FieldGet(field, grid=grid, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out

    ! -- add vertical dimension
    newgrid = GridCreateFromGrid(grid, minIndex=1, maxIndex=vSize, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out

    ! -- get new grid's localDeCount
    call ESMF_GridGet(newgrid, localDeCount=localDeCount, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    ! -- load vertical coordinate
    scale_factor = 1._ESMF_KIND_R8
    add_offset   = 0._ESMF_KIND_R8
    if (present(scale)) scale_factor = scale
    if (present(offset)) add_offset  = offset

    do localDe = 0, localDeCount - 1

      call ESMF_FieldGet(field, localDE=localDe, farrayPtr=fptrOut3d, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return

      call ESMF_GridGetCoord(newgrid, coordDim=3, localDE=localDe, &
        staggerloc=ESMF_STAGGERLOC_CENTER, &
        farrayPtr=fptrIn3d, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return

      fptrIn3d = scale_factor * fptrOut3d + add_offset

    end do

    GridCreateFromField = newgrid

  end function GridCreateFromField

!------------------------------------------------------------------------------

  subroutine MeshGetBounds(mesh, dim, bounds, rc)
    type(ESMF_Mesh), intent(in) :: mesh
    integer,         intent(in) :: dim
    real(ESMF_KIND_R8), intent(out) :: bounds(2)
    integer, optional,  intent(out) :: rc

    ! -- local variables
    integer :: localrc, numOwnedNodes, spatialDim
    real(ESMF_KIND_R8), dimension(:), allocatable, target :: ownedNodeCoords
    real(ESMF_KIND_R8), dimension(:), pointer :: p
    real(ESMF_KIND_R8), dimension(1) :: sendData, recvData
    type(ESMF_VM) :: vm

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    bounds(2) = huge(0._ESMF_KIND_R8)
    bounds(1) = -bounds(2)

    call ESMF_MeshGet(mesh, spatialDim=spatialDim, &
      numOwnedNodes=numOwnedNodes, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    if (dim > spatialDim) then
      call ESMF_LogSetError(ESMF_RC_ARG_OUTOFRANGE, &
        msg="dim argument is higher than Mesh spatial dimension", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return ! bail out
    end if

    allocate(ownedNodeCoords(spatialDim*numOwnedNodes), stat=localrc)
    if (ESMF_LogFoundAllocError(statusToCheck=localrc, &
      msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) &
      return

    call ESMF_MeshGet(mesh, ownedNodeCoords=ownedNodeCoords, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    call ESMF_VMGetCurrent(vm, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    p => ownedNodeCoords(dim::spatialDim)
    sendData(1) = minval(p)
    recvData    = 0._ESMF_KIND_R8

    call ESMF_VMAllReduce(vm, sendData, recvData, 1, ESMF_REDUCE_MIN, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    bounds(1) = recvData(1)

    sendData(1) = maxval(p)
    recvData    = 0._ESMF_KIND_R8

    call ESMF_VMAllReduce(vm, sendData, recvData, 1, ESMF_REDUCE_MAX, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return

    bounds(2) = recvData(1)
    
  end subroutine MeshGetBounds

!------------------------------------------------------------------------------

  subroutine NamespaceSetLocalGrid(name, grid, rc)
    character(len=*), intent(in) :: name
    type(ESMF_Grid)              :: grid
    integer,         intent(out) :: rc

    ! -- local variables
    type(compType),  pointer :: p
    type(stateType), pointer :: s
    integer                  :: item
    logical                  :: isGridCreated, isFieldCreated

    ! -- begin
    rc = ESMF_SUCCESS

    isGridCreated = ESMF_GridIsCreated(grid, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (isGridCreated) then
      p => compList
      nullify(s)
      do while (associated(p))
        if (trim(p % name) == name) then
          s => p % stateList
          do while (associated(s))
            s % localGrid  = grid
            do item = 1, s % fieldMaxRank
              ! -- if local field exists, destroy and recreate
              isFieldCreated = ESMF_FieldIsCreated(s % localField(item), rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__,  &
                file=__FILE__)) &
                return  ! bail out
              if (isFieldCreated) then
                call ESMF_FieldDestroy(s % localField(item), noGarbage=.true., rc=rc)
                if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__,  &
                  file=__FILE__)) &
                  return  ! bail out
              end if
              s % localField(item) = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R8, &
                staggerloc=ESMF_STAGGERLOC_CENTER, rc=rc)
              if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                line=__LINE__, &
                file=__FILE__)) &
                return  ! bail out
            end do
            s => s % next
          end do
        end if
        p => p % next
      end do
    else
      call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
        msg="Invalid Grid object (not yet created)", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return ! bail out
    end if

  end subroutine NamespaceSetLocalGrid

  subroutine NamespaceSetLocalGridFromField(name, state, fieldName, scale, offset, rc)
    character(len=*), intent(in) :: name
    type(ESMF_State)             :: state
    character(len=*), intent(in) :: fieldName
    real(ESMF_KIND_R8), optional, intent(in) :: scale, offset
    integer,         intent(out) :: rc

    ! -- local variables
    type(ESMF_Field)            :: field
    type(ESMF_Grid)             :: localGrid, grid
    type(ESMF_Mesh)             :: localMesh
    type(ESMF_GeomType_Flag)    :: geomtype
    type(ESMF_FieldStatus_Flag) :: fieldStatus

    ! -- begin
    rc = ESMF_SUCCESS

    field = NamespaceGetField(name, state, fieldName, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! -- check if field is completed
    call ESMF_FieldGet(field, status=fieldStatus, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) then
      call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
        msg="Field has not been completely created.", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return ! bail out
    end if

    ! -- check if field contains valid grid object
    call ESMF_FieldGet(field, geomtype=geomtype, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    if (geomtype /= ESMF_GEOMTYPE_GRID) then
      call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
        msg="No Grid object found in field "// fieldName, &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return ! bail out
    end if

    grid = GridCreateFromField(field, scale=scale, offset=offset, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
     
    call NamespaceSetLocalGrid(name, grid, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

#if 0
    ! -- write 3D Grid object as Mesh (VTK file) for debugging purposes
    localMesh = ESMF_GridToMesh(grid, staggerloc=ESMF_STAGGERLOC_CENTER, &
      isSphere=1, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_MeshWrite(localMesh, 'localmesh', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

#endif

  end subroutine NamespaceSetLocalGridFromField

  subroutine NamespaceSetLocalMesh(name, mesh3d, mesh2d, levArray, levCoord, rc)
    character(len=*),             intent(in)  :: name
    type(ESMF_Mesh),    optional, intent(in)  :: mesh2d, mesh3d
    type(ESMF_Array),   optional, intent(in)  :: levArray
    real(ESMF_KIND_R8), optional, intent(in)  :: levCoord(:)
    integer,            optional, intent(out) :: rc

    ! -- local variables
    type(compType),  pointer    :: p
    type(stateType), pointer    :: s
    logical                     :: proceed, isCreated
    integer                     :: localrc, item
    integer                     :: localDe, localDeCount
    integer, dimension(1)       :: lb, ub
    real(ESMF_KIND_R8), pointer :: fptr(:,:)
    type(ESMF_DistGrid)         :: distgrid

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    proceed = present(mesh3d) .or. &
              (present(mesh2d) .and. (present(levArray) .or. present(levCoord)))

    if (.not.proceed) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="mesh3d or mesh2d and either levArray or levCoord must be provided", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return ! bail out
    end if

    if (proceed) then
      p => compList
      nullify(s)
      do while (associated(p))
        if (trim(p % name) == name) then
          s => p % stateList
          do while (associated(s))
            if (present(mesh3d)) then
              s % localMesh = mesh3d
              do item = 1, s % fieldMaxRank
                ! -- if local field exists, destroy and recreate
                isCreated = ESMF_FieldIsCreated(s % localField(item), rc=localrc)
                if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__,  &
                  file=__FILE__,  &
                  rcToReturn=rc)) return  ! bail out
                if (isCreated) then
                  call ESMF_FieldDestroy(s % localField(item), noGarbage=.true., rc=localrc)
                  if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                    line=__LINE__,  &
                    file=__FILE__,  &
                    rcToReturn=rc)) return  ! bail out
                end if
                s % localField(item) = ESMF_FieldCreate(mesh3d, ESMF_TYPEKIND_R8, &
                  meshloc=ESMF_MESHLOC_NODE, rc=localrc)
                if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__,  &
                  file=__FILE__,  &
                  rcToReturn=rc)) return  ! bail out
              end do
              if (s % fieldMaxRank == 2) then
                ! -- init unit vectors on new mesh
                call StateSetLocalVectors(s, rc=localrc)
                if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__,  &
                  file=__FILE__,  &
                  rcToReturn=rc)) return  ! bail out
              end if
            end if
            if (present(mesh2d)) then
              if (present(levCoord)) then
                call ESMF_MeshGet(mesh2d, nodalDistgrid=distgrid, rc=localrc)
                if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__,  &
                  file=__FILE__,  &
                  rcToReturn=rc)) return  ! bail out
                lb(1) = 1
                ub(1) = size(levCoord)
                s % localLevels = ESMF_ArrayCreate(distgrid, ESMF_TYPEKIND_R8, &
                  undistLBound=lb, undistUBound=ub, rc=localrc)
                if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__,  &
                  file=__FILE__,  &
                  rcToReturn=rc)) return  ! bail out
                call ESMF_ArrayGet(s % localLevels, localDeCount=localDeCount, rc=localrc)
                if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__,  &
                  file=__FILE__,  &
                  rcToReturn=rc)) return  ! bail out
                do localDe = 0, localDeCount-1
                  nullify(fptr)
                  call ESMF_ArrayGet(s % localLevels, localDe=localDe, &
                    farrayPtr=fptr, rc=localrc)
                  if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                    line=__LINE__,  &
                    file=__FILE__,  &
                    rcToReturn=rc)) return  ! bail out
                  do item = lb(1), ub(1)
                    fptr(:,item) = levCoord(item)
                  end do
                end do
              else if (present(levArray)) then
                s % localLevels = levArray
                call ESMF_ArrayGet(levArray, undistLBound=lb, undistUBound=ub, rc=localrc)
                if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__,  &
                  file=__FILE__,  &
                  rcToReturn=rc)) return  ! bail out
              end if

              do item = 1, s % fieldMaxRank
                ! -- if local field exists, destroy and recreate
                isCreated = ESMF_FieldIsCreated(s % localIntField(item), rc=localrc)
                if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__,  &
                  file=__FILE__,  &
                  rcToReturn=rc)) return  ! bail out
                if (isCreated) then
                  call ESMF_FieldDestroy(s % localIntField(item), noGarbage=.true., rc=localrc)
                  if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                    line=__LINE__,  &
                    file=__FILE__,  &
                    rcToReturn=rc)) return  ! bail out
                end if
                s % localIntField(item) = ESMF_FieldCreate(mesh2d, ESMF_TYPEKIND_R8, &
                  meshloc=ESMF_MESHLOC_NODE, &
                  ungriddedLBound=lb, ungriddedUBound=ub, rc=localrc)
                if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__,  &
                  file=__FILE__,  &
                  rcToReturn=rc)) return  ! bail out
              end do
            end if
            s => s % next
          end do
        end if
        p => p % next
      end do
    else
      call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
        msg="Invalid Mesh object (not yet created)", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return ! bail out
    end if

  end subroutine NamespaceSetLocalMesh

  subroutine GetDataFromField(name, state, fieldName, farrayPtr, localDe, rc)
    ! -- input variables
    character(len=*), intent(in) :: name
    type(ESMF_State)             :: state
    character(len=*), intent(in) :: fieldName
    real(ESMF_KIND_R8), dimension(:,:,:), pointer :: farrayPtr
    integer,         optional, intent(in)  :: localDe
    integer,         optional, intent(out) :: rc
    ! -- output variables

    ! -- local variables
    type(compType),  pointer :: p
    type(stateType), pointer :: s
    type(ESMF_Field)         :: field
    integer                  :: item, de

    ! -- begin
    rc = ESMF_SUCCESS

    de = 0
    if (present(localDe)) de = localDe

    nullify(farrayPtr)
       
    nullify(s)
    p => compList
    do while (associated(p))
      if (trim(p % name) == name) then
        s => p % stateList
        do while (associated(s))
          if (s % parent == state) then
            if (associated(s % fieldNames)) then
              do item = 1, size(s % fieldNames) 
                if (trim(s % fieldNames(item)) == fieldName) then
                  call ESMF_StateGet(s % self, field=field, itemName=fieldName, rc=rc)
                  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                    line=__LINE__, &
                    file=__FILE__)) &
                    return  ! bail out
                  call ESMF_FieldGet(field, localDe=de, farrayPtr=farrayPtr, rc=rc)
                  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
                    line=__LINE__, &
                    file=__FILE__)) &
                    return  ! bail out
                  return  ! exit function
                end if
              end do 
            end if
          end if
          s => s % next
        end do
      end if
      p => p % next
    end do

    nullify(p, s)

  end subroutine GetDataFromField

  subroutine NamespaceSetRemoteLevels(name, array, rc)
    character(len=*), intent(in) :: name
    type(ESMF_Array), intent(in) :: array
    integer,         intent(out) :: rc

    ! -- local variables
    type(compType),  pointer :: p
    type(stateType), pointer :: s

    ! -- begin
    rc = ESMF_SUCCESS
  
    nullify(s)
    p => compList
    do while (associated(p))
      if (trim(p % name) == name) then
        s => p % stateList
        do while (associated(s))
          s % remoteLevels = array
          s => s % next
        end do
      end if
      p => p % next
    end do

    nullify(p, s)
     
  end subroutine NamespaceSetRemoteLevels

  subroutine NamespaceSetRemoteLevelsFromField(name, state, fieldName, &
    scale, offset, norm, rc)
    character(len=*), intent(in) :: name
    type(ESMF_State)             :: state
    character(len=*), intent(in) :: fieldName
    real(ESMF_KIND_R8), optional, intent(in) :: scale, offset, norm
    integer,         intent(out) :: rc

    ! -- local variables
    logical            :: update
    integer            :: localDe, localDeCount, rank
    real(ESMF_KIND_R8) :: scale_factor, add_offset, div_by_norm
    real(ESMF_KIND_R8), dimension(:),     pointer :: fptr1d
    real(ESMF_KIND_R8), dimension(:,:),   pointer :: fptr2d
    real(ESMF_KIND_R8), dimension(:,:,:), pointer :: fptr3d
    type(ESMF_Field)   :: field
    type(ESMF_Array)   :: array, localArray

    ! -- begin
    rc = ESMF_SUCCESS

    field = NamespaceGetField(name, state, fieldName, rc)

    call ESMF_FieldGet(field, array=array, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    update = .false.
    scale_factor = 1._ESMF_KIND_R8
    add_offset   = 0._ESMF_KIND_R8
    div_by_norm  = 1._ESMF_KIND_R8
    if (present(scale)) then
      scale_factor = scale
      update = .true.
    end if

    if (present(offset)) then
      add_offset = offset
      update = .true.
    end if

    if (present(norm)) then
      div_by_norm = norm
      update = .true.
    end if

    if (update) then
      localArray = ESMF_ArrayCreate(array, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_ArrayGet(localArray, rank=rank, localDeCount=localDeCount, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      do localDe = 0, localDeCount - 1
        select case (rank)
          case(1)
            call ESMF_ArrayGet(localArray, localDe=localDe, farrayPtr=fptr1d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            fptr1d = scale_factor * fptr1d + add_offset
            fptr1d = fptr1d / div_by_norm
          case(2)
            call ESMF_ArrayGet(localArray, localDe=localDe, farrayPtr=fptr2d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            fptr2d = scale_factor * fptr2d + add_offset
            fptr2d = fptr2d / div_by_norm
          case(3)
            call ESMF_ArrayGet(localArray, localDe=localDe, farrayPtr=fptr3d, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
            fptr3d = scale_factor * fptr3d + add_offset
            fptr3d = fptr3d / div_by_norm
          case default
            call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
              msg="Array rank can only be 1, 2, or 3", &
              line=__LINE__, &
              file=__FILE__, &
              rcToReturn=rc)
            return ! bail out
        end select
      end do
    else
      localArray = array
    end if

    call NamespaceSetRemoteLevels(name, localArray, rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
       
  end subroutine NamespaceSetRemoteLevelsFromField

  ! -- Namespace: Create/Get/Set methods: end definition --

  ! -- Namespace: Field Retrieve/Store/Interpolate/Release methods: begin definition --

  subroutine VerticalInterpolate1D(srcCoord, srcfarray, dstCoord, dstfarray, auxfarray, options, rc)
    real(ESMF_KIND_R8), dimension(:,:),  intent(in) :: srcfarray, srcCoord, dstCoord
    real(ESMF_KIND_R8), dimension(:,:), intent(out) :: dstfarray
    real(ESMF_KIND_R8), dimension(:,:), optional, intent(in) :: auxfarray
    character(len=*), optional, intent(in) :: options
    integer, intent(out) :: rc

    ! -- local variables
    logical :: isFlag
    integer :: i, j, k, localrc
    integer :: lbnd, ubnd
    real(ESMF_KIND_R8) :: auxNorm, rt
#ifdef LEGACY
    integer, parameter :: extrap_start_level = 149
#endif

    ! -- begin
    rc = ESMF_SUCCESS

    ! -- check if src and dst arrays have same horizontal decompositions
    isFlag = .false.
    lbnd = lbound(srcfarray, dim=1)
    isFlag = (lbound(dstfarray, dim=1) /= lbnd) 
    if (.not.isFlag) then
      ubnd = ubound(srcfarray, dim=1)
      isFlag = (ubound(dstfarray, dim=1) /= ubnd)
    end if
    if (isFlag) then
      call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
        msg="src and dst arrays defined on different 1D regions (mismatched lower/upper bounds)", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return ! bail out
    end if

    ! -- pick interpolation method
    auxNorm = 0._ESMF_KIND_R8
    if (present(options)) then
      read(options, *, iostat=localrc) auxNorm
      if (localrc /= 0) auxNorm = 0._ESMF_KIND_R8
    end if
      
    if (auxNorm > 0._ESMF_KIND_R8) then
      if (present(auxfarray)) then
        ! -- log interpolation + extrapolation w/ hypsometric equation
        do i = lbnd, ubnd
#ifdef LEGACY
        ! -- if using extrap_start_level, make sure auxfarray is from original field (no intermediate interpolation)
        ! -- use option "origin" in StateGetField
          rt = auxfarray(i,extrap_start_level)
#else
          rt = auxfarray(i,ubound(auxfarray, dim=2))
#endif
          call LogInterpolate(srcCoord(i,:), srcfarray(i,:), &
                              dstCoord(i,:), dstfarray(i,:), &
                              rt=rt, ms=auxNorm, rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        end do
      else
        ! -- log interpolation, no extrapolation
        do i = lbnd, ubnd
          call LogInterpolate(srcCoord(i,:), srcfarray(i,:), &
                              dstCoord(i,:), dstfarray(i,:), &
                              rc=rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        end do
      end if
    else
      ! -- linear interpolation, extrapolate with constant value
      do i = lbnd, ubnd
#ifdef LEGACY
        call LinearInterpolate(srcCoord(i,:), srcfarray(i,:), &
                               dstCoord(i,:), dstfarray(i,:), rc)
#else
        call PolyInterpolate(srcCoord(i,:), srcfarray(i,:), &
                             dstCoord(i,:), dstfarray(i,:), 1, rc)
#endif
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      end do
    end if

  end subroutine VerticalInterpolate1D

  subroutine VerticalInterpolate2D(srcCoord, srcfarray, dstCoord, dstfarray, auxfarray, options, rc)
    real(ESMF_KIND_R8), dimension(:,:,:),  intent(in) :: srcfarray, srcCoord, dstCoord
    real(ESMF_KIND_R8), dimension(:,:,:), intent(out) :: dstfarray
    real(ESMF_KIND_R8), dimension(:,:,:), optional, intent(in) :: auxfarray
    character(len=*), optional, intent(in) :: options
    integer, intent(out) :: rc

    ! -- local variables
    logical :: isFlag
    integer :: i, j, k, localrc
    integer, dimension(2) :: lbnd, ubnd
    real(ESMF_KIND_R8) :: auxNorm, rt

    ! -- begin
    rc = ESMF_SUCCESS

    ! -- check if src and dst arrays have same horizontal decompositions
    isFlag = .false.
    do i = 1, 2
      lbnd(i) = lbound(srcfarray, dim=i)
      isFlag = (lbound(dstfarray, dim=i) /= lbnd(i)) 
      if (isFlag) exit
      ubnd(i) = ubound(srcfarray, dim=i)
      isFlag = (ubound(dstfarray, dim=i) /= ubnd(i))
      if (isFlag) exit
    end do
    if (isFlag) then
      call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
        msg="src and dst arrays defined on different 2D regions (mismatched lower/upper bounds)", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return ! bail out
    end if

    ! -- pick interpolation method
    auxNorm = 0._ESMF_KIND_R8
    if (present(options)) then
      read(options, *, iostat=localrc) auxNorm
      if (localrc /= 0) auxNorm = 0._ESMF_KIND_R8
    end if
      
    if (auxNorm > 0._ESMF_KIND_R8) then
      if (present(auxfarray)) then
        ! -- log interpolation + extrapolation w/ hypsometric equation
        do j = lbnd(2), ubnd(2)
          do i = lbnd(1), ubnd(1)
            rt = auxfarray(i,j,ubound(auxfarray, dim=3)) / auxNorm
            call LogInterpolate(srcCoord(i,j,:), srcfarray(i,j,:), &
                                dstCoord(i,j,:), dstfarray(i,j,:), &
                                rt=rt, rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
          end do
        end do
      else
        ! -- log interpolation, no extrapolation
        do j = lbnd(2), ubnd(2)
          do i = lbnd(1), ubnd(1)
            call LogInterpolate(srcCoord(i,j,:), srcfarray(i,j,:), &
                                dstCoord(i,j,:), dstfarray(i,j,:), &
                                rc=rc)
            if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__)) &
              return  ! bail out
          end do
        end do
      end if
    else
        ! -- linear interpolation, extrapolate with constant value
      do j = lbnd(2), ubnd(2)
        do i = lbnd(1), ubnd(1)
          call PolyInterpolate(srcCoord(i,j,:), srcfarray(i,j,:), &
                               dstCoord(i,j,:), dstfarray(i,j,:), 1, rc)
          if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) &
            return  ! bail out
        end do
      end do
    end if

  end subroutine VerticalInterpolate2D

  subroutine PolyInterpolate(xs, ys, xd, yd, m, rc)
    real(ESMF_KIND_R8), dimension(:), intent(in)  :: xs, ys, xd
    real(ESMF_KIND_R8), dimension(:), intent(out) :: yd
    integer, intent(in)  :: m
    integer, intent(out) :: rc

    ! -- local variables
    integer :: i, j, k, n, np
    real(ESMF_KIND_R8) :: x, y, dy

    ! -- begin
    rc = ESMF_SUCCESS

    n = m + 1
    np = size(xs)
    do i = 1, size(xd)
      x = xd(i)
      y = 0._ESMF_KIND_R8
      call locate(xs, np, x, j)
      if (j == np) then
        y = ys(np)
      else if (j > 0) then
        k = min(max(j-(n-1)/2,1), np+1-n)
        call polint(xs(k:), ys(k:), n, x, y, dy, rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg="Error in polint", &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      end if
      yd(i) = y
    end do
    
  end subroutine PolyInterpolate

#ifdef LEGACY
  subroutine LinearInterpolate(xs, ys, xd, yd, rc)
    real(ESMF_KIND_R8), dimension(:), intent(in)  :: xs, ys, xd
    real(ESMF_KIND_R8), dimension(:), intent(out) :: yd
    integer, intent(out) :: rc

    ! -- local variables
    integer :: k, kk, l, np, nd
    integer, parameter :: extrap_start_level = 149

    ! np = inlevels
    ! nd = wamdim
    ! varbuf = ys
    ! hgtbuf = xs

    ! -- begin
    rc = ESMF_SUCCESS

    np = size(xs)
    nd = size(xd)
    yd = 0._ESMF_KIND_R8

    kk = 1
    do k = 1, nd
      do while (kk < np .and. xs(kk) < xd(k))
        kk = kk + 1
      end do
      if (kk > extrap_start_level) then
        do l = k, nd
          yd(l)=ys(extrap_start_level)
        end do
        exit
      end if
      if (kk > 1) then
        yd(k)=(ys(kk)*(xd(k)-xs(kk-1))+ &
          ys(kk-1)*(xs(kk)-xd(k)))/(xs(kk)-xs(kk-1))
      else
        yd(k)=ys(kk)
      end if
    end do

  end subroutine LinearInterpolate
#endif

#ifndef LEGACY
  subroutine LogInterpolate(xs, ys, xd, yd, rt, rc)
    ! -- note: both xs and xd are assumed to be "normalized" heights
    ! --       x = 1 + z / earthRadius
    ! -- if absolute heights (km) are used, please set earthRadius = 1
    real(ESMF_KIND_R8), dimension(:), intent(in)  :: xs, ys, xd
    real(ESMF_KIND_R8), dimension(:), intent(out) :: yd
    real(ESMF_KIND_R8), optional, intent(in) :: rt  ! reduced T = T / mass
    integer, intent(out) :: rc

    ! -- local variables
    integer, parameter :: n = 2  ! linear interpolation (2 points)
    integer :: i, itop, j, k, np, nd
    real(ESMF_KIND_R8) :: fact, x, x1, y, y1, dy, ylog(n)

    real(ESMF_KIND_R8), parameter :: log_min = 1.e-10_ESMF_KIND_R8
    real(ESMF_KIND_R8), parameter :: g0 = 9.80665_ESMF_KIND_R8
    real(ESMF_KIND_R8), parameter :: Rgas = 8.3141_ESMF_KIND_R8
    real(ESMF_KIND_R8), parameter :: earthRadius = 6371.2_ESMF_KIND_R8
    real(ESMF_KIND_R8), parameter :: const = 2 * g0 * earthRadius / Rgas
 
    ! -- begin
    rc = ESMF_SUCCESS

    np = size(xs)
    nd = size(xd)
    yd = 0._ESMF_KIND_R8

    ! -- interpolate
    do i = 1, nd
      x = xd(i)
      y = 0._ESMF_KIND_R8
      call locate(xs, np, x, j)
      if (j == np) then
        itop = i
        exit
      else if (j > 0) then
        k = min(max(j-(n-1)/2,1), np+1-n)
        ylog = log(max(ys(k:k+n-1), log_min))
        call polint(xs(k:), ylog, n, x, y, dy, rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg="Error in polint", &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      end if
      yd(i) = exp(y)
    end do

    ! -- extrapolatw with hypsometric equation
    if (present(rt)) then
      if (rt <= 0._ESMF_KIND_R8) then
        call ESMF_LogSetError(ESMF_RC_ARG_OUTOFRANGE, &
          msg="Optional rt argument (T/m) must be > 0", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return ! bail out
      end if 
      x1 = xs(np) 
      y1 = ys(np) 
      fact = const / rt
      do i = itop, nd
        y = fact * (x1-xd(i)) / (x1*x1+xd(i)*xd(i))
        yd(i) = y1 * exp(y)
        x1 = xd(i)
        y1 = yd(i)
      end do
    end if

  end subroutine LogInterpolate

#else
  ! ---------- TEST
  subroutine LogInterpolate(xs, ys, xd, yd, rt, ms, rc)
    ! -- note: both xs and xd are assumed to be "normalized" heights
    ! --       x = 1 + z / earthRadius
    ! -- if absolute heights (km) are used, please set earthRadius = 1
    real(ESMF_KIND_R8), dimension(:), intent(in)  :: xs, ys, xd
    real(ESMF_KIND_R8), dimension(:), intent(out) :: yd
    real(ESMF_KIND_R8), optional, intent(in) :: rt  ! T at TOA
    real(ESMF_KIND_R8), optional, intent(in) :: ms  ! mass
    integer, intent(out) :: rc

    ! -- local variables
    integer :: k, kk, l, np, nd
    real(ESMF_KIND_R8) :: hgt_prev, dist, H_prev, data_prev
    real(ESMF_KIND_R8) :: hgt_curr, H_avg, H_curr, data_curr
    real(ESMF_KIND_R8) :: R, g0, re
    real(ESMF_KIND_R8) :: mass

    integer,            parameter :: extrap_start_level = 149
    real(ESMF_KIND_R8), parameter :: log_min = 1.0E-10
!   real(ESMF_KIND_R8), parameter :: log_min = 1.e-10_ESMF_KIND_R8
!   real(ESMF_KIND_R8), parameter :: re = 6371.2_ESMF_KIND_R8
!   real(ESMF_KIND_R8), parameter :: g0 = 9.80665_ESMF_KIND_R8
!   real(ESMF_KIND_R8), parameter :: R  = 8.3141_ESMF_KIND_R8

    R  = 8.3141
    g0 = 9.80665
    re = 6.3712e03


    ! -- begin
    rc = ESMF_SUCCESS

    np = size(xs)
    nd = size(xd)
    yd = 0._ESMF_KIND_R8

    ! -- interpolate

    ! varbuf = ys
    ! hgtbuf = xs

    if (present(rt)) then
      mass = 1._ESMF_KIND_R8
      if (present(ms)) mass = ms
      kk = 1
      do k = 1, nd
        do while (kk < np .and. xs(kk) < xd(k))
          kk = kk + 1
        end do
        if (kk > extrap_start_level) then
!         hgt_prev=re*(xs(extrap_start_level)-1)
          hgt_prev=xs(extrap_start_level)
          dist=re/(re+hgt_prev)
          H_prev=R*rt/(mass*g0*dist*dist)
          data_prev=ys(extrap_start_level)
          do l = k, nd
!           hgt_curr=re*(xd(l)-1)
            hgt_curr=xd(l)
            dist=re/(re+hgt_curr)
            H_curr=R*rt/(mass*g0*dist*dist)

            ! Extrapolate data to this level
            H_avg=0.5*(H_prev+H_curr)
            ! Temporary fix for H_avg = 0.0?
            if ( H_avg .gt. 0. ) then
              data_curr=data_prev*exp((hgt_prev-hgt_curr)/H_avg)
            else
              data_curr=data_prev
            end if

            ! Set extrapolated data in output array
            yd(l)=data_curr

            ! Set info for next pass
            hgt_prev=hgt_curr
            H_prev=H_curr
            data_prev=data_curr
          enddo
          exit
        endif
        if (kk>1) then
          yd(k)=exp((log(max(ys(kk),log_min))*(xd(k)-xs(kk-1))+ &
            log(max(ys(kk-1),log_min))*(xs(kk)-xd(k)))/(xs(kk)-xs(kk-1)))
        else
          yd(k)=ys(kk)
        endif
      enddo
    end if

  end subroutine LogInterpolate

#endif

  ! -- auxiliary numerical subroutines for interpolation/extrapolation from:
  ! -- W. H. Press, S. A. Teukolsky, W. T. Vetterling, B. P. Flannery,
  ! -- Numerical Recipes in Fortran 77: The Art of Scientific Computing
  ! -- (Vol. 1 of Fortran Numerical Recipes), 2nd Ed., Cambridge Univ. Press
  ! -- 

  subroutine locate(xx, n, x, j)
    implicit none
    integer, intent(in) :: n
    real(ESMF_KIND_R8), intent(in) :: x, xx(n)
    integer, intent(out) :: j

    ! -- local variables
    integer :: jl, jm, ju

    ! -- begin
    jl = 0
    ju = n + 1
    do while (ju-jl.gt.1)
      jm = (ju+jl)/2
      if ((xx(n).ge.xx(1)).eqv.(x.ge.xx(jm))) then
        jl = jm
      else
        ju = jm
      end if
    end do
    if (x.eq.xx(1)) then
      j = 1
    else if (x.eq.xx(n)) then
      j = n - 1
    else
      j = jl
    end if

  end subroutine locate

  subroutine polint(xa, ya, n, x, y, dy, rc)

    implicit none

    integer, intent(in) :: n
    real(ESMF_KIND_R8),    intent(in) :: xa(n), ya(n)
    real(ESMF_KIND_R8),    intent(in) :: x
    real(ESMF_KIND_R8),   intent(out) :: y, dy
    integer, intent(out) :: rc

    ! -- local variables
    integer, parameter :: nmax = 10
    integer :: i, m, ns
    real(ESMF_KIND_R8) :: den, dif, dift, ho, hp, w
    real(ESMF_KIND_R8), dimension(nmax) :: c, d

    ! -- begin
    rc = ESMF_SUCCESS

    ns = 1
    dif = abs(x - xa(1))
    do i = 1, n
      dift = abs(x - xa(i))
      if (dift < dif) then
        ns  = i
        dif = dift
      end if
      c(i) = ya(i)
      d(i) = ya(i)
    end do
    y = ya(ns)
    ns = ns - 1
    do m = 1, n - 1
      do i = 1, n - m
        ho = xa(i)   - x
        hp = xa(i+m) - x
        w  = c(i+1)-d(i)
        den = ho - hp
        if (den == 0.) then
          rc = ESMF_FAILURE
          exit
        end if
        den = w / den
        d(i) = hp * den
        c(i) = ho * den
      end do
      if (2*ns < n - m) then
        dy = c(ns+1)
      else
        dy = d(ns)
        ns = ns - 1
      end if
      y = y + dy
    end do

  end subroutine polint

  subroutine FieldGet(state, fieldName, compNames, compCount, rc)

    ! -- input variables
    type(stateType),            intent(in) :: state
    character(len=*),           intent(in) :: fieldName

    ! -- output
    character(len=*), optional, intent(out) :: compNames(2)
    integer,          optional, intent(out) :: compCount
    integer,          optional, intent(out) :: rc

    ! -- local variables
    integer :: comp, item

    ! -- begin
    if (present(rc)) rc = ESMF_RC_NOT_FOUND

    if (present(compCount)) compCount = 0
    if (present(compNames)) compNames = ""

    if (associated(state % fieldNames)) then
      do item = 1, size(state % fieldNames)
        if (trim(state % fieldNames(item)) == trim(fieldName)) then
          comp = state % fieldDepMap(item)
          if (comp > 0) then
            if (present(compCount)) compCount = 2
            if (present(compNames)) then
              compNames(1) = trim(state % fieldNames(item))
              compNames(2) = trim(state % fieldNames(comp))
            end if
          else if (comp == 0) then
            if (present(compCount)) compCount = 1
            if (present(compNames)) then
              compNames(1) = trim(state % fieldNames(item))
            end if
          end if
          if (present(rc)) rc = ESMF_SUCCESS
          exit
        end if
      end do
    end if

  end subroutine FieldGet


  subroutine FieldRegrid(rh, fieldName, auxArray, options, rc)

    type(rhType)                            :: rh
    character(len=*),           intent(in)  :: fieldName
    type(ESMF_Array), optional, intent(in)  :: auxArray
    character(len=*), optional, intent(in)  :: options
    integer,          optional, intent(out) :: rc

    ! -- local variables
    type(ESMF_Field) :: srcField, dstField
    type(ESMF_Field), dimension(2) :: srcFieldComp, dstFieldComp
    integer :: localrc
    integer :: localDeCount, comp, compCount
    logical :: isSrcCart, isDstCart
    character(len=ESMF_MAXSTR) :: compNames(2)

    ! -- begin 
    if (present(rc)) rc = ESMF_SUCCESS

    call FieldGet(rh % srcState, fieldName, compNames=compNames, compCount=compCount, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

    if (compCount < 1) return

    do comp = 1, compCount
      srcFieldComp(comp) = StateGetField(rh % srcState, compNames(comp), &
        auxArray=auxArray, options=options, component=comp, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out

      ! -- print diagnostic info
      call FieldPrintMinMax(srcFieldComp(comp), "pre  - src:" // trim(compNames(comp)), rc)

      dstFieldComp(comp) = StateGetField(rh % dstState, compNames(comp), component=comp, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out
    end do

    if (compCount == 2) then
      ! -- convert to Cartesian 3D vector
      call Cardinal_to_Cart3D(srcFieldComp(2), srcFieldComp(1), &
        rh % srcState % uvec(2), rh % srcState % uvec(1), &
        rh % srcState % localCartField, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out

      srcField = rh % srcState % localCartField
      dstField = rh % dstState % localCartField
    else
      srcField = srcFieldComp(1)
      dstField = dstFieldComp(1)
    end if

    ! -- perform regrid
    call ESMF_FieldRegrid(srcField=srcField, dstField=dstField, &
      zeroregion=ESMF_REGION_TOTAL, &
#ifdef BFB_REGRID
      termorderflag=ESMF_TERMORDER_SRCSEQ, &
#endif
      routehandle=rh % rh, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

    if (compCount == 2) then
      ! -- convert back to cardinal vectors
      call Cart3D_to_Cardinal(dstField, &
        rh % dstState % uvec(2), rh % dstState % uvec(1), &
        dstFieldComp(2), dstFieldComp(1), rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out
    end if

    do comp = 1, compCount
      ! -- print diagnostic info
      call FieldPrintMinMax(dstFieldComp(comp), "post - dst:" // trim(compNames(comp)), rc)

      call StateStoreField(rh % srcState, srcFieldComp(comp), compNames(comp), rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out

      call StateStoreField(rh % dstState, dstFieldComp(comp), compNames(comp), rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out
    end do

  end subroutine FieldRegrid

  function StateGetField(state, fieldName, auxArray, options, component, rc)

    ! -- input variables
    type(stateType),            intent(inout) :: state
    character(len=*),           intent(in)    :: fieldName
    type(ESMF_Array), optional, intent(in)    :: auxArray
    character(len=*), optional, intent(in)    :: options
    integer,          optional, intent(in)    :: component

    ! -- output 
    type(ESMF_Field)               :: StateGetField
    integer, optional, intent(out) :: rc

    ! -- local variable
    logical :: isFieldCreated
    logical :: isNative, isNoData, isOrigin
    integer :: localrc
    integer :: localDeCount, localComp
    real(ESMF_KIND_R8), pointer :: fptr1d(:), fptr2d(:,:)
    type(ESMF_Field)            :: field
    type(ESMF_StateIntent_Flag) :: stateIntent
    type(ESMF_GeomType_Flag)    :: geomtype, lgeomtype

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    isNoData = .false.
    isNative = .false.
    isOrigin = .false.
    if (present(options)) then
      isNoData = (trim(options) == "nodata")
      isNative = (trim(options) == "native")
      isOrigin = (trim(options) == "origin")
    end if

    localComp = 1
    if (present(component)) then
      if (component < 1) then
        call ESMF_LogSetError(ESMF_RC_ARG_OUTOFRANGE, &
          msg="component must be greater than 0", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return ! bail out
      else if (component > state % fieldMaxRank) then
        call ESMF_LogSetError(ESMF_RC_ARG_OUTOFRANGE, &
          msg="component not found", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return ! bail out
      else
        localComp = component
      end if
    end if

    call ESMF_StateGet(state % parent, stateintent=stateIntent, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

    if (stateIntent == ESMF_STATEINTENT_IMPORT) then
      write(6,'(" - StateGetField: state is Import: getting ",a," ...")') trim(fieldName)
    else if (stateIntent == ESMF_STATEINTENT_EXPORT) then
      write(6,'(" - StateGetField: state is Export: getting ",a," ...")') trim(fieldName)
    else
      write(6,'(" - StateGetField: state is UNKNOWN for field ",a," ...")') trim(fieldName)
      if (present(rc)) rc = ESMF_FAILURE
      return
    end if

    call ESMF_StateGet(state % self, itemName=trim(fieldName), field=field, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

    if (isOrigin) then
      StateGetField = field
      return
    end if

    isFieldCreated = ESMF_FieldIsCreated(state % localField(localComp), rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

    isNoData = isNoData .or. (stateIntent == ESMF_STATEINTENT_EXPORT)

    if (isFieldCreated) then
      if (isNoData) then
        StateGetField = state % localField(localComp)
        return
      end if
      call ESMF_FieldGet(field, geomtype=geomtype, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out
      call ESMF_FieldGet(state % localField(localComp), geomtype=lgeomtype, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out

      if (geomtype /= lgeomtype) then
        call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
          msg="remote and local GeomType must match", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return ! bail out
      end if

      if (geomtype == ESMF_GEOMTYPE_MESH) then

        ! -- interpolate from 2d+1 Mesh to 3d Mesh
        call FieldInterpolate(field, state % localIntField(localComp), &
          srcLevels=state % remoteLevels, &
          dstLevels=state % localLevels, &
          auxArray=auxArray, options=options, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out

        if (isNative) then
          StateGetField = state % localIntField(localComp)
        else
          ! -- assuming 1 DE/PET
          call ESMF_FieldGet(state % localField(localComp), farrayPtr=fptr1d, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) return  ! bail out
          call ESMF_FieldGet(state % localIntField(localComp), farrayPtr=fptr2d, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) return  ! bail out

          fptr1d = reshape(fptr2d, (/ size(fptr1d) /))

          StateGetField = state % localField(localComp)
        end if

      else if (geomtype == ESMF_GEOMTYPE_GRID) then

        ! -- use localLevels only on Mesh to reduce complexity for the time being
        call FieldInterpolate(field, state % localField(localComp), srcLevels=state % remoteLevels, &
          auxArray=auxArray, options=options, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out

        ! -- interpolate from 2d+1 Grid to 3d Grid
!       if (ESMF_ArrayIsCreated(state % localLevels)) then
!         call FieldInterpolate(field, state % localField, srcLevels=state % remoteLevels, &
!           dstLevels=state % localLevels, &
!           auxArray=auxArray, options=options, rc=localrc)
!         if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
!           line=__LINE__,  &
!           file=__FILE__,  &
!           rcToReturn=rc)) return  ! bail out
!       else
!         call FieldInterpolate(field, state % localField, srcLevels=state % remoteLevels, &
!           auxArray=auxArray, options=options, rc=localrc)
!         if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
!           line=__LINE__,  &
!           file=__FILE__,  &
!           rcToReturn=rc)) return  ! bail out
!       end if
        StateGetField = state % localField(localComp)

      else
        call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
          msg="GeomType not recognized", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return ! bail out
      end if

    else
      StateGetField = field
    end if

  end function StateGetField

  subroutine StateStoreField(state, field, fieldName, component, rc)

    ! -- input variables
    type(stateType),   intent(in)  :: state
    type(ESMF_Field),  intent(in)  :: field
    character(len=*),  intent(in)  :: fieldName
    integer, optional, intent(in)  :: component

    ! -- output 
    integer, optional, intent(out) :: rc

    ! -- local variable
    integer                     :: localrc, localComp
    real(ESMF_KIND_R8), pointer :: fptr1d(:), fptr2d(:,:)
    type(ESMF_Field)            :: dstField
    type(ESMF_GeomType_Flag)    :: geomtype
    type(ESMF_StateIntent_Flag) :: stateIntent

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    localComp = 1
    if (present(component)) then
      if (component < 1) then
        call ESMF_LogSetError(ESMF_RC_ARG_OUTOFRANGE, &
          msg="component must be greater than 0", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return ! bail out
      else if (component > state % fieldMaxRank) then
        call ESMF_LogSetError(ESMF_RC_ARG_OUTOFRANGE, &
          msg="component not found", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return ! bail out
      else
        localComp = component
      end if
    end if

    call ESMF_StateGet(state % parent, stateintent=stateIntent, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

    if (stateIntent /= ESMF_STATEINTENT_EXPORT) return

    if (field == state % localField(localComp)) then

      call ESMF_FieldGet(field, geomtype=geomtype, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out

      call ESMF_StateGet(state % self, itemName=trim(fieldName), &
        field=dstField, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out

      if (geomtype == ESMF_GEOMTYPE_MESH) then

        ! -- assuming 1 DE/PET
        call ESMF_FieldGet(state % localField(localComp), farrayPtr=fptr1d, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out
        call ESMF_FieldGet(state % localIntField(localComp), farrayPtr=fptr2d, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out

        fptr2d = reshape(fptr1d, shape(fptr2d))

        ! -- interpolate from 2d+1 Mesh to 3d Mesh
        call FieldInterpolate(state % localIntField(localComp), dstField, &
          srcLevels=state % localLevels, &
          dstLevels=state % remoteLevels, &
          rc=localrc)
!         auxArray=auxArray, options=options, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out

        call FieldPrintMinMax(dstField, "StateGetField:" // trim(fieldName), rc)

      else if (geomtype == ESMF_GEOMTYPE_GRID) then

        ! -- use localLevels only on Mesh to reduce complexity for the time being
        call FieldInterpolate(field, dstField, dstLevels=state % remoteLevels, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out
      end if

    end if

  end subroutine StateStoreField


  subroutine StateSetLocalVectors(state, rc)

    ! -- input/output variables
    type(stateType), intent(inout) :: state
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc, item
    logical :: isCreated

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    isCreated = ESMF_MeshIsCreated(state % localMesh, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

    if (.not.isCreated) then
      call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
        msg="local Mesh must be created before local vectors", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return ! bail out
    end if

    do item = 1, size(state % uvec)
      isCreated = ESMF_FieldIsCreated(state % uvec(item), rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out
      if (isCreated) then
        call ESMF_FieldDestroy(state % uvec(item), noGarbage=.true., rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out
      end if
      ! -- create unit vectors
      state % uvec(item) = ESMF_FieldCreate(state % localMesh, ESMF_TYPEKIND_R8, &
         gridToFieldMap=(/2/), ungriddedLBound=(/1/), ungriddedUBound=(/3/), &
         rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out
    end do

    ! -- set 3D Cartesian unit vectors
    ! -- USE CART VERSION BECAUSE OF A BUG IN MESHREDIST() IN ESMF 7.0.0 and before, once in 7.1.0 USE NON-CART VERSION
    if (ESMF_VERSION_MAJOR > 7) then
      call Set_Field_Cardinal_UVecs(state % uvec(2), state % uvec(1), rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out
    else if ((ESMF_VERSION_MAJOR == 7) .and. (ESMF_VERSION_MINOR > 0)) then
      call Set_Field_Cardinal_UVecs(state % uvec(2), state % uvec(1), rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out
    else
      call Set_Field_Cardinal_UVecsCart(state % uvec(2), state % uvec(1), rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out
    endif

    ! -- now set local Cartesian vector
    isCreated = ESMF_FieldIsCreated(state % localCartField, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out
    if (isCreated) then
      call ESMF_FieldDestroy(state % localCartField, noGarbage=.true., rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out
    end if
    state % localCartField = ESMF_FieldCreate(state % localMesh, ESMF_TYPEKIND_R8, &
      gridToFieldMap=(/2/), ungriddedLBound=(/1/), ungriddedUBound=(/3/), &
      meshloc=ESMF_MESHLOC_NODE, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

  end subroutine StateSetLocalVectors


  subroutine FieldInterpolate(srcField, dstField, srcLevels, dstLevels, auxArray, options, rc)
    type(ESMF_Field),            intent(in) :: srcField
    type(ESMF_Field),         intent(inout) :: dstField
    type(ESMF_Array), optional,  intent(in) :: srcLevels
    type(ESMF_Array), optional,  intent(in) :: dstLevels
    type(ESMF_Array), optional,  intent(in) :: auxArray
    character(len=*), optional,  intent(in) :: options
    integer,          optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc
    integer :: rank, lrank
    integer :: deCount, ldeCount, localDe
    real(ESMF_KIND_R8), dimension(:,:),   pointer :: srcCoord2d, srcData2d, &
                                                     dstCoord2d, dstData2d, auxData2d
    real(ESMF_KIND_R8), dimension(:,:,:), pointer :: srcCoord3d, srcData3d, &
                                                     dstCoord3d, dstData3d, auxData3d
    type(ESMF_Grid)  :: grid
    type(ESMF_Array) :: srcCoordArray, dstCoordArray
    type(ESMF_GeomType_flag) :: geomtype

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    call ESMF_FieldGet(srcField, geomtype=geomtype, rank=rank, &
      localDeCount=deCount, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

    if (rank < 2 .or. rank > 3 ) then
      call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
        msg="rank of source field must be 2 or 3", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return ! bail out
    end if

    if (geomtype == ESMF_GEOMTYPE_MESH) then
      if (deCount /= 1) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="localDeCount of source field must be 1 since field is on Mesh", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return ! bail out
      end if
    end if

    call ESMF_FieldGet(dstField, rank=lrank, localDeCount=ldeCount, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

    if (ldeCount /= deCount) then
      call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
        msg="localDeCount of source and destination field must match", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return ! bail out
    end if

    if (lrank /= rank) then
      call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
        msg="rank of source and destination field must match", &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)
      return ! bail out
    end if

    if (present(srcLevels)) then
      call ESMF_ArrayGet(srcLevels, rank=lrank, localDeCount=ldeCount, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out
      if (lrank /= rank) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="Rank of source levels array must match rank of src/dst field", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return ! bail out
      end if
      if (ldeCount /= deCount) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="localDeCount of srcLevels must match src/dst fields", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return ! bail out
      end if
      srcCoordArray = srcLevels
    else
      ! -- check if src field is on Grid
      call ESMF_FieldGet(srcField, geomtype=geomtype, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return
      if (geomtype == ESMF_GEOMTYPE_GRID) then
        ! -- get src grid
        call ESMF_FieldGet(srcField, grid=grid, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)) return
        ! -- get vertical levels
        call ESMF_GridGetCoord(grid, coordDim=3, &
          staggerloc=ESMF_STAGGERLOC_CENTER, array=srcCoordArray, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)) return
      else
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="if no srcLevels provided, srcField must be on Grid", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return ! bail out
      end if
    end if

    ! -- check for dstLevels
    if (present(dstLevels)) then
      call ESMF_ArrayGet(dstLevels, rank=lrank, localDeCount=ldeCount, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out
      if (lrank /= rank) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="Rank of dstLevels array must match rank of src/dst field", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return ! bail out
      end if
      if (ldeCount /= deCount) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="localDeCount of dstLevels must match src/dst fields", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return ! bail out
      end if
      dstCoordArray = dstLevels
    else
      ! -- check if src field is on Grid
      call ESMF_FieldGet(dstField, geomtype=geomtype, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return
      if (geomtype == ESMF_GEOMTYPE_GRID) then
        ! -- get dst grid
        call ESMF_FieldGet(dstField, grid=grid, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)) return
        ! -- get vertical levels
        call ESMF_GridGetCoord(grid, coordDim=3, &
          staggerloc=ESMF_STAGGERLOC_CENTER, array=dstCoordArray, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)) return
      else
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="if no dstLevels provided, dstField must be on Grid", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return ! bail out
      end if
    end if

    ! -- check for auxArray
    if (present(auxArray)) then
      call ESMF_ArrayGet(auxArray, rank=lrank, localDeCount=ldeCount, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out
      if (lrank /= rank) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="Rank of auxArray array must match rank of src/dst field", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return ! bail out
      end if
      if (ldeCount /= deCount) then
        call ESMF_LogSetError(ESMF_RC_NOT_IMPL, &
          msg="localDeCount of auxArray must match src/dst fields", &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return ! bail out
      end if
    end if

    do localDe = 0, deCount - 1

      select case (rank)
      case (2)

        call ESMF_FieldGet(srcField, localDE=localDe, farrayPtr=srcData2d, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out

        call ESMF_ArrayGet(srcCoordArray, localDE=localDe, farrayPtr=srcCoord2d, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out

        call ESMF_FieldGet(dstField, localDE=localDe, farrayPtr=dstData2d, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out

        call ESMF_ArrayGet(dstCoordArray, localDE=localDe, farrayPtr=dstCoord2d, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out

        if (present(auxArray)) then
          call ESMF_ArrayGet(auxArray, localDE=localDe, farrayPtr=auxData2d, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) return  ! bail out
        else
          nullify(auxData2d)
        end if

        call Interpolate(srcCoord2d, srcData2d, dstCoord2d, dstData2d, &
          auxfarray=auxData2d, options=options, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out

      case (3)

        call ESMF_FieldGet(srcField, localDE=localDe, farrayPtr=srcData3d, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out

        call ESMF_ArrayGet(srcCoordArray, localDE=localDe, farrayPtr=srcCoord3d, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out

        call ESMF_FieldGet(dstField, localDE=localDe, farrayPtr=dstData3d, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out

        call ESMF_ArrayGet(dstCoordArray, localDE=localDe, farrayPtr=dstCoord3d, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out

        if (present(auxArray)) then
          call ESMF_ArrayGet(auxArray, localDE=localDe, farrayPtr=auxData3d, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) return  ! bail out
        else
          nullify(auxData3d)
        end if

        call Interpolate(srcCoord3d, srcData3d, dstCoord3d, dstData3d, &
          auxfarray=auxData3d, options=options, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out

      end select

    end do

  end subroutine FieldInterpolate

  subroutine FieldPrintMinMax(field, label, rc)
    type(ESMF_Field)                        :: field
    character(len=*), optional,  intent(in) :: label
    integer,          optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc, rank
    integer :: localDe, localDeCount
    character(len=ESMF_MAXSTR) :: name
    real(ESMF_KIND_R8), pointer :: fptr1d(:), fptr2d(:,:), fptr3d(:,:,:)
    real(ESMF_KIND_R8) :: fmin, fmax, fieldMin, fieldMax

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    fieldMin = huge(0._ESMF_KIND_R8)
    fieldMax = -fieldMin

    call ESMF_FieldGet(field, name=name, rank=rank, localDeCount=localDeCount, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    do localDe = 0, localDeCount - 1
      select case (rank)
      case(1)
        call ESMF_FieldGet(field, farrayPtr=fptr1d, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out
        fmin = minval(fptr1d)
        fmax = maxval(fptr1d)
      case(2)
        call ESMF_FieldGet(field, farrayPtr=fptr2d, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out
        fmin = minval(fptr2d)
        fmax = maxval(fptr2d)
      case(3)
        call ESMF_FieldGet(field, farrayPtr=fptr3d, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out
        fmin = minval(fptr3d)
        fmax = maxval(fptr3d)
      case default
        call ESMF_LogSetError(ESMF_RC_OBJ_BAD, &
          msg="Rank must be 1, 2, or 3 for field "//trim(name), &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return ! bail out
      end select
      fieldMin = min(fieldMin, fmin)
      fieldMax = max(fieldMax, fmax)
    end do

    write(6,'("FieldPrintMinMax: ",a,1x,a," min = ",g14.6," max = ",g14.6)') &
      trim(label), trim(name), fieldMin, fieldMax

  end subroutine FieldPrintMinMax

  ! -- Namespace: Field Retrieve/Store/Interpolate/Release methods: end definition --

  ! -- Namespace: Utilities: begin definition --

  subroutine NamespacePrint(logUnit, rc)
    integer, intent(in),  optional :: logUnit
    integer, intent(out), optional :: rc

    ! -- local variables
    type(compType),     pointer :: p
    type(stateType),    pointer :: s
    type(ESMF_StateIntent_Flag) :: stateintent
    character(len=ESMF_MAXSTR)  :: stateName, compNames(2)
    integer :: compCount, i, localPet, localrc, lunit
    type(ESMF_VM) :: vm

    integer, parameter :: defaultLogUnit = 6

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    lunit = defaultLogUnit
    if (present(logUnit)) lunit = logUnit

    call ESMF_VMGetCurrent(vm, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out

    call ESMF_VMGet(vm, localPet=localPet, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out

    if (localPet /= 0) return

    nullify(s)
    write(lunit,'("Namespaces defined")')
    write(lunit,'("==================")')
    p => compList
    do while (associated(p))
      write(lunit,'("Namespace: ",a)') trim(p % name)
      s => p % stateList
      do while (associated(s))
        call ESMF_StateGet(s % parent, stateintent=stateintent, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)) return  ! bail out
        call ESMF_StateGet(s % self, name=stateName, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)) return  ! bail out
        if (stateintent == ESMF_STATEINTENT_IMPORT) then
          write(lunit,'("State: ",a,": Import")') trim(stateName)
        else if (stateintent == ESMF_STATEINTENT_EXPORT) then
          write(lunit,'("State: ",a,": Export")') trim(stateName)
        else if (stateintent == ESMF_STATEINTENT_UNSPECIFIED) then
          write(lunit,'("State: ",a,": Intent unspecified")') trim(stateName)
        else
          write(lunit,'("State: ",a,": Intent N/A")') trim(stateName)
        end if
        if (associated(s % fieldNames)) then
          do i = 1, size(s % fieldNames)
            call FieldGet(s, trim(s % fieldNames(i)), compNames=compNames, &
              compCount=compCount, rc=localrc)
            if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
              line=__LINE__, &
              file=__FILE__, &
              rcToReturn=rc)) return  ! bail out
            if (compCount == 1) then
              write(lunit,'(i4,2x,a)') i, trim(compNames(1))
            else if (compCount == 2) then
              write(lunit,'(i4,2x,"(",3a,")")') i, trim(compNames(1)), &
                s % fieldSep, trim(compNames(2))
            end if
          end do
        else
          write(lunit,'("No fields attached")')
        end if
        if (associated(s % fieldOptions)) then
          do i = 1, size(s % fieldOptions)
            write(lunit,'(i4,2x,a)') i, trim(s % fieldOptions(i))
          end do
        else
          write(lunit,'("No field options attached")')
        end if
        if (s % ugDimLength /= 0) then
          write(lunit,'("Length of ungridded dimension: ",i0)') s % ugDimLength
        else
          write(lunit,'("No ungridded dimension")')
        end if
        write(lunit,'("transferAction: ",a)') trim(s % trAction)
        s => s % next
      end do
      p => p % next
    end do
    write(lunit,'("==================")')

    nullify(p, s)

  end subroutine NamespacePrint

  ! -- Namespace: Utilities: end definition --

  ! -- RouteHandle: begin definition --

  function RouteHandleListGet(rc)
    integer, optional, intent(out) :: rc

    ! -- local variables
    type(rhType),          pointer :: RouteHandleListGet

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    RouteHandleListGet => rhList

  end function RouteHandleListGet

  subroutine RouteHandleListSet(routeHandleList, rc)
    integer, optional, intent(out) :: rc
    type(rhType),          pointer :: routeHandleList

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    rhList => routeHandleList

  end subroutine routeHandleListSet

  logical function RouteHandleListIsCreated(rc)
    integer, optional, intent(out) :: rc

    ! -- begin
    RouteHandleListIsCreated = associated(RouteHandleListGet(rc=rc))
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end function RouteHandleListIsCreated

  subroutine RouteHandleCreate(routeHandle, rc)

    type(rhType), optional,     pointer :: routeHandle
    integer,      optional, intent(out) :: rc

    ! -- local variables
    type(compType),     pointer :: p, q
    type(stateType),    pointer :: r, s
    type(rhType),       pointer :: rh, rHandle
    type(ESMF_Field)            :: srcField, dstField
    type(ESMF_StateIntent_flag) :: stateIntent
    character(len=ESMF_MAXSTR)  :: srcName, dstName
    integer                     :: localrc
#ifdef BFB_REGRID
    integer                     :: srcTermProcessing
#endif

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    rh => rhList
    p => compList
    do while (associated(p))
      s => p % stateList
      do while (associated(s))
        call ESMF_StateGet(s % parent, stateintent=stateIntent, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out
        if (stateIntent == ESMF_STATEINTENT_IMPORT) then
          q => compList
          do while (associated(q))
            if (trim(p % name) /= trim(q % name)) then
              r => q % stateList
              do while (associated(r))
                call ESMF_StateGet(r % parent, stateintent=stateIntent, rc=localrc)
                if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                  line=__LINE__,  &
                  file=__FILE__,  &
                  rcToReturn=rc)) return  ! bail out

                if (stateIntent == ESMF_STATEINTENT_EXPORT) then

                  call ESMF_StateGet(s % self, name=srcName, rc=localrc)
                  if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                    line=__LINE__,  &
                    file=__FILE__,  &
                    rcToReturn=rc)) return  ! bail out

                  call ESMF_StateGet(r % self, name=dstName, rc=localrc)
                  if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                    line=__LINE__,  &
                    file=__FILE__,  &
                    rcToReturn=rc)) return  ! bail out

                  srcField = StateGetField(s, s % fieldNames(1), options="nodata", rc=localrc)
                  if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                    line=__LINE__,  &
                    file=__FILE__,  &
                    rcToReturn=rc)) return  ! bail out

                  dstField = StateGetField(r, r % fieldNames(1), options="nodata", rc=localrc)
                  if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                    line=__LINE__,  &
                    file=__FILE__,  &
                    rcToReturn=rc)) return  ! bail out

                  nullify(rHandle)
                  allocate(rHandle, stat=localrc)
                  if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                    line=__LINE__, &
                    file=__FILE__, &
                    rcToReturn=rc)) return ! bail out
                  rHandle % label    = trim(srcName)//" -> "//trim(dstName)
                  rHandle % srcState => s
                  rHandle % dstState => r
                  nullify(rHandle % next)

#ifdef BFB_REGRID
                  srcTermProcessing = 0
                  write(6,'(" - RHStore: start working on RH ...",a, "(srcTermProcessing = ",i0,")")') &
                    trim(rHandle % label), srcTermProcessing
#else
                  write(6,'(" - RHStore: start working on RH ...",a)') trim(rHandle % label)
#endif
                  call ESMF_FieldRegridStore(srcField, dstField, &
                    regridmethod   = ESMF_REGRIDMETHOD_BILINEAR, &
                    unmappedaction = ESMF_UNMAPPEDACTION_IGNORE, &
                    polemethod     = ESMF_POLEMETHOD_NONE,       &
                    lineType       = ESMF_LINETYPE_GREAT_CIRCLE, &
#ifdef BFB_REGRID
                    srcTermProcessing = srcTermProcessing, &
#endif
                    routehandle=rHandle % rh, rc=localrc)
                  if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
                    line=__LINE__,  &
                    file=__FILE__,  &
                    rcToReturn=rc)) return  ! bail out
                  write(6,'(" - RHStore: done working on RH ...",a)') trim(rHandle % label)

                  if (associated(rh)) then
                    rh % next => rHandle
                  else
                    rhList => rHandle
                  end if
                  rh => rHandle
                end if 
                r => r % next
              end do
            end if 
            q => q % next
          end do
        end if
        s => s % next
      end do
      p => p % next
    end do

    if (present(routeHandle)) routeHandle => rhList

  end subroutine RouteHandleCreate

  subroutine RouteHandlePrint(routeHandleList, detailed, rc)
    type(rhType), optional, pointer     :: routeHandleList
    logical,      optional,  intent(in) :: detailed
    integer,      optional, intent(out) :: rc

    ! -- local variables
    integer :: item, localrc
    type(rhType), pointer :: rh

    logical :: printDetails
    integer :: localPet
    type(ESMF_VM) :: vm

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    printDetails = .false.
    if (present(detailed)) printDetails = detailed

    call ESMF_VMGetCurrent(vm, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out

    call ESMF_VMGet(vm, localPet=localPet, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__, &
      rcToReturn=rc)) return  ! bail out

    if (present(routeHandleList)) then
      rh => routeHandleList
    else
      rh => rhList
    end if

    print *, 'RouteHandle Table'
    print *, '================='
    item = 0
    do while (associated(rh))
      item = item + 1
      write(6,'(i4,2x,a,2x,l5)') item, trim(rh % label), &
        ESMF_RouteHandleIsCreated(rh % rh, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return  ! bail out
      if (printDetails) then
        call ESMF_RouteHandlePrint(rh % rh, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)) return  ! bail out
      end if
      rh => rh % next
    end do
    print *, '================='
    
  end subroutine RouteHandlePrint

  subroutine RouteHandleListRelease(routeHandleList, rc)

    type(rhType), optional, pointer     :: routeHandleList
    integer,      optional, intent(out) :: rc

    ! -- local variables
    type(compType),  pointer :: p, q
    type(stateType), pointer :: s, r
    type(rhType),    pointer :: rh, lh
    integer                  :: localrc
    logical                  :: isRHCreated

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    ! -- free up memory
    ! -- routehandles
    if (present(routeHandleList)) then
      rh => routeHandleList
    else
      rh => rhList
    end if

    do while (associated(rh))
      lh => rh
      rh => rh % next
      isRHCreated = ESMF_RouteHandleIsCreated(lh % rh, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) return  ! bail out
      if (isRHCreated) then
        call ESMF_FieldRegridRelease(lh % rh, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out
      end if
      deallocate(lh, stat=localrc)
      if (ESMF_LogFoundAllocError(statusToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__, &
        rcToReturn=rc)) return ! bail out
      nullify(lh)
    end do

    if (present(routeHandleList)) then
      nullify(routeHandleList)
    else
      nullify(rhList)
    end if

  end subroutine RouteHandleListRelease

  ! -- RouteHandle: end definition --

! Fill a Field with 3D Cartesian unit vectors corresponding to cardinal directions
! This assumes that the incoming fields are built on the same Mesh, and have an undistributed dim of 3
subroutine Set_Field_Cardinal_UVecs(north_field, east_field, rc)
  type(ESMF_Field) :: north_field, east_field
  type(ESMF_Mesh) :: mesh
  type(ESMF_VM) :: vm
  integer :: localPet
  integer :: numNodes
  real(ESMF_KIND_R8), allocatable :: nodeCoords(:)
  real(ESMF_KIND_R8), pointer :: north_field_ptr(:,:)
  real(ESMF_KIND_R8), pointer :: east_field_ptr(:,:)
  real(ESMF_KIND_R8) :: lat, lon
  integer :: localDECount, i
  integer :: rc

  ! Error checking
  real(ESMF_KIND_R8) :: max_coord(2), g_max_coord(2)
  real(ESMF_KIND_R8) :: min_coord(2), g_min_coord(2)


  ! debug
  real(ESMF_KIND_R8) :: len

  ! Get mesh
  call ESMF_FieldGet(north_field, mesh=mesh, &
                     localDECount=localDECount, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out

  ! If there are no DEs on this processor, then leave
  if (localDECount .eq. 0) then
     return
  endif

  ! If there is more than 1 DE then complain, because we aren't handling
  ! that case right now
  if (localDECount .gt. 1) then
     return
  endif


  ! Get Coordinates
  call ESMF_MeshGet(mesh, numOwnedNodes=numNodes, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out

   ! Allocate space for coordinates
   allocate(nodeCoords(3*numNodes))

   ! Set interpolated function
   call ESMF_MeshGet(mesh, ownedNodeCoords=nodeCoords, rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out


  ! Get pointer to north field array
  ! (Should only be 1 localDE)
  call ESMF_FieldGet(north_field, 0, north_field_ptr, &
       rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out


  ! Error checking of Field Bounds
  if ((lbound(north_field_ptr,1) .ne. 1) .or. &
       (ubound(north_field_ptr,1) .ne. 3) .or. &
       (lbound(north_field_ptr,2) .ne. 1) .or. &
       (ubound(north_field_ptr,2) .ne. numNodes)) then
     call ESMF_LogSetError(ESMF_RC_VAL_OUTOFRANGE, &
          msg="north Field bounds wrong", &
          line=__LINE__, &
          file=__FILE__,  &
          rcToReturn=rc)
     return
  endif

  ! Get pointer to east field array
  ! (Should only be 1 localDE)
  call ESMF_FieldGet(east_field, 0, east_field_ptr, &
       rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out


  ! Error checking of Field Bounds
  if ((lbound(east_field_ptr, 1) .ne. 1) .or. &
       (ubound(east_field_ptr, 1) .ne. 3) .or. &
       (lbound(east_field_ptr, 2) .ne. 1) .or. &
       (ubound(east_field_ptr, 2) .ne. numNodes)) then
     call ESMF_LogSetError(ESMF_RC_VAL_OUTOFRANGE, &
          msg="east Field bounds wrong", &
          line=__LINE__, &
          file=__FILE__,  &
          rcToReturn=rc)
     return
  endif

  ! For error checking
  min_coord= HUGE(min_coord)
  max_coord=-HUGE(max_coord)

  ! Loop setting unit vectors
  do i=1,numNodes

     ! Get position on sphere
     lon=nodeCoords(3*(i-1)+1)
     lat=nodeCoords(3*(i-1)+2)

     ! Get min/max of coord for error checking
     if (lon < min_coord(1)) min_coord(1)=lon
     if (lon > max_coord(1)) max_coord(1)=lon
     if (lat < min_coord(2)) min_coord(2)=lat
     if (lat > max_coord(2)) max_coord(2)=lat

     ! Convert to radians
     lon=lon*ESMF_COORDSYS_DEG2RAD
     lat=lat*ESMF_COORDSYS_DEG2RAD

     ! Set east vector
     east_field_ptr(1,i)=cos(lon)
     east_field_ptr(2,i)=sin(lon)
     east_field_ptr(3,i)=0.0

     ! Set north vector
     north_field_ptr(1,i)=-sin(lat)*sin(lon)
     north_field_ptr(2,i)= sin(lat)*cos(lon)
     north_field_ptr(3,i)= cos(lat)

  enddo


  ! Get current vm
  call ESMF_VMGetCurrent(vm, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out

  ! set up local pet info
  call ESMF_VMGet(vm, localPet=localPet, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out


  ! Compute global max
  call ESMF_VMAllReduce(vm, max_coord, g_max_coord, 2, &
       ESMF_REDUCE_MAX, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out

  call ESMF_VMAllReduce(vm, min_coord, g_min_coord, 2, &
       ESMF_REDUCE_MIN, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out

  ! Report error
  if ((g_max_coord(1) - g_min_coord(1)) < 20.0) then
     call ESMF_LogSetError(ESMF_RC_VAL_OUTOFRANGE, &
 msg="Longitude range of grid unexpectedly small (< 20 deg) possibly using radians or 7.1.0 snapshot before 16", &
          line=__LINE__, &
          file=__FILE__,  &
          rcToReturn=rc)
     return
  endif

  if ((g_max_coord(2) - g_min_coord(2)) < 20.0) then
     call ESMF_LogSetError(ESMF_RC_VAL_OUTOFRANGE, &
msg="Latitude range of grid unexpectedly small (< 20 deg) possibly using radians or 7.1.0 snapshot before 16", &
          line=__LINE__, &
          file=__FILE__,  &
          rcToReturn=rc)
     return
  endif

  ! Get rid of coordinates
  deallocate(nodeCoords)

  ! return success
  rc=ESMF_SUCCESS

end subroutine Set_Field_Cardinal_UVecs


! This function does the same thing as the above. However, it starts from Cart coordinates in the Mesh.
! This is due to a bug in redisting the mesh. When we switch to 7.1.0 you can get rid of this subroutine and
! use the above for both meshes.
subroutine Set_Field_Cardinal_UVecsCart(north_field, east_field, rc)
  type(ESMF_Field) :: north_field, east_field
  type(ESMF_Mesh) :: mesh
  integer :: numNodes
  real(ESMF_KIND_R8), allocatable :: nodeCoords(:)
  real(ESMF_KIND_R8), pointer :: north_field_ptr(:,:)
  real(ESMF_KIND_R8), pointer :: east_field_ptr(:,:)
  real(ESMF_KIND_R8) :: lat, lon, r
  integer :: localDECount, i
  integer :: rc
  real(ESMF_KIND_R8) :: x,y,z
  real(ESMF_KIND_R8),parameter :: half_pi=1.5707963267949_ESMF_KIND_R8
  real(ESMF_KIND_R8),parameter :: two_pi=6.28318530717959_ESMF_KIND_R8

  ! debug
  real(ESMF_KIND_R8) :: len

  ! Get mesh
  call ESMF_FieldGet(north_field, mesh=mesh, &
                     localDECount=localDECount, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out

  ! If there are no DEs on this processor, then leave
  if (localDECount .eq. 0) then
     return
  endif

  ! If there is more than 1 DE then complain, because we aren't handling
  ! that case right now
  if (localDECount .gt. 1) then
     return
  endif


  ! Get Coordinates
  call ESMF_MeshGet(mesh, numOwnedNodes=numNodes, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out

   ! Allocate space for coordinates
   allocate(nodeCoords(3*numNodes))

   ! Set interpolated function
   call ESMF_MeshGet(mesh, ownedNodeCoords=nodeCoords, rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out


  ! Get pointer to north field array
  ! (Should only be 1 localDE)
  call ESMF_FieldGet(north_field, 0, north_field_ptr, &
       rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out


  ! Error checking of Field Bounds
  if ((lbound(north_field_ptr,1) .ne. 1) .or. &
       (ubound(north_field_ptr,1) .ne. 3) .or. &
       (lbound(north_field_ptr,2) .ne. 1) .or. &
       (ubound(north_field_ptr,2) .ne. numNodes)) then
     call ESMF_LogSetError(ESMF_RC_VAL_OUTOFRANGE, &
          msg="north Field bounds wrong", &
          line=__LINE__, &
          file=__FILE__,  &
          rcToReturn=rc)
     return
  endif

  ! Get pointer to east field array
  ! (Should only be 1 localDE)
  call ESMF_FieldGet(east_field, 0, east_field_ptr, &
       rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out


  ! Error checking of Field Bounds
  if ((lbound(east_field_ptr, 1) .ne. 1) .or. &
       (ubound(east_field_ptr, 1) .ne. 3) .or. &
       (lbound(east_field_ptr, 2) .ne. 1) .or. &
       (ubound(east_field_ptr, 2) .ne. numNodes)) then
     call ESMF_LogSetError(ESMF_RC_VAL_OUTOFRANGE, &
          msg="east Field bounds wrong", &
          line=__LINE__, &
          file=__FILE__,  &
          rcToReturn=rc)
     return
  endif

  ! Loop setting unit vectors
  do i=1,numNodes

     ! Get position on sphere
     x=nodeCoords(3*(i-1)+1)
     y=nodeCoords(3*(i-1)+2)
     z=nodeCoords(3*(i-1)+3)

     ! convert to lon/lat/r
     r=sqrt(x*x+y*y+z*z)

     lon=atan2(y,x)
     if (lon < 0.0) lon = lon + two_pi

     lat=half_pi-acos(z/r)

     ! Set east vector
     east_field_ptr(1,i)=cos(lon)
     east_field_ptr(2,i)=sin(lon)
     east_field_ptr(3,i)=0.0

     ! Set north vector
     north_field_ptr(1,i)=-sin(lat)*sin(lon)
     north_field_ptr(2,i)= sin(lat)*cos(lon)
     north_field_ptr(3,i)= cos(lat)

  enddo

  ! Get rid of coordinates
  deallocate(nodeCoords)

  ! return success
  rc=ESMF_SUCCESS

end subroutine Set_Field_Cardinal_UVecsCart


! Convert Cardinal vectors to one 3D cartesian vector
subroutine Cardinal_to_Cart3D(north_field, east_field, &
                              north_uvec, east_uvec, &
                              cart_vec, rc)
  type(ESMF_Field) :: north_field, east_field
  type(ESMF_Field) :: north_uvec, east_uvec
  type(ESMF_Field) :: cart_vec
  integer :: rc
  real(ESMF_KIND_R8), pointer :: north_field_ptr(:)
  real(ESMF_KIND_R8), pointer :: east_field_ptr(:)
  real(ESMF_KIND_R8), pointer :: north_uvec_ptr(:,:)
  real(ESMF_KIND_R8), pointer :: east_uvec_ptr(:,:)
  real(ESMF_KIND_R8), pointer :: cart_vec_ptr(:,:)
  integer :: localDECount, lDE
  integer :: clbnd(1), cubnd(1), i


  ! Get localDECount
  ! (Asssumes that all the incoming Fields have the same local DE Count)
  call ESMF_FieldGet(north_field, &
                     localDECount=localDECount, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out


  ! Loop over local DEs processing data
  do lDE=0,localDECount-1

     ! Get pointer to north field array
     call ESMF_FieldGet(north_field, lDE, north_field_ptr, &
          computationalLBound=clbnd, computationalUBound=cubnd, &
          rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

     ! Get pointer to east field array
     call ESMF_FieldGet(east_field, lDE, east_field_ptr, &
          rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

     ! Get pointer to north unit vector array
     call ESMF_FieldGet(north_uvec, lDE, north_uvec_ptr, &
          rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

     ! Get pointer to north unit vector array
     call ESMF_FieldGet(east_uvec, lDE, east_uvec_ptr, &
          rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

     ! Get pointer to east unit vector array
     call ESMF_FieldGet(east_uvec, lDE, east_uvec_ptr, &
          rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out


     ! Get pointer to east unit vector array
     call ESMF_FieldGet(cart_vec, lDE, cart_vec_ptr, &
          rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

     ! Loop over points processing
     do i=clbnd(1), cubnd(1)


#if 0
        cart_vec_ptr(1,i)=east_uvec_ptr(1,i)
        cart_vec_ptr(2,i)=east_uvec_ptr(2,i)
        cart_vec_ptr(3,i)=east_uvec_ptr(3,i)
#endif

        cart_vec_ptr(1,i)=east_field_ptr(i)*east_uvec_ptr(1,i)+ &
                          north_field_ptr(i)*north_uvec_ptr(1,i)

        cart_vec_ptr(2,i)=east_field_ptr(i)*east_uvec_ptr(2,i)+ &
                          north_field_ptr(i)*north_uvec_ptr(2,i)

        cart_vec_ptr(3,i)=east_field_ptr(i)*east_uvec_ptr(3,i)+ &
                          north_field_ptr(i)*north_uvec_ptr(3,i)
     enddo
  enddo

  ! return success
  rc=ESMF_SUCCESS

end subroutine Cardinal_to_Cart3D



! Convert 3D cartesian vector to Cardinal vectors
subroutine Cart3D_to_Cardinal(cart_vec, &
                              north_uvec, east_uvec, &
                              north_field, east_field, rc)
  type(ESMF_Field) :: cart_vec
  type(ESMF_Field) :: north_uvec, east_uvec
  type(ESMF_Field) :: north_field, east_field
  integer :: rc
  real(ESMF_KIND_R8), pointer :: north_field_ptr(:)
  real(ESMF_KIND_R8), pointer :: east_field_ptr(:)
  real(ESMF_KIND_R8), pointer :: north_uvec_ptr(:,:)
  real(ESMF_KIND_R8), pointer :: east_uvec_ptr(:,:)
  real(ESMF_KIND_R8), pointer :: cart_vec_ptr(:,:)
  integer :: localDECount, lDE
  integer :: clbnd(1), cubnd(1), i


  ! Get localDECount
  ! (Asssumes that all the incoming Fields have the same local DE Count)
  call ESMF_FieldGet(north_field, &
                     localDECount=localDECount, rc=rc)
  if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out


  ! Loop over local DEs processing data
  do lDE=0,localDECount-1

     ! Get pointer to north field array
     call ESMF_FieldGet(north_field, lDE, north_field_ptr, &
          computationalLBound=clbnd, computationalUBound=cubnd, &
          rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

     ! Get pointer to east field array
     call ESMF_FieldGet(east_field, lDE, east_field_ptr, &
          rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

     ! Get pointer to north unit vector array
     call ESMF_FieldGet(north_uvec, lDE, north_uvec_ptr, &
          rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

     ! Get pointer to north unit vector array
     call ESMF_FieldGet(east_uvec, lDE, east_uvec_ptr, &
          rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

     ! Get pointer to east unit vector array
     call ESMF_FieldGet(east_uvec, lDE, east_uvec_ptr, &
          rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out


     ! Get pointer to east unit vector array
     call ESMF_FieldGet(cart_vec, lDE, cart_vec_ptr, &
          rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out

     ! Loop over points processing
     do i=clbnd(1), cubnd(1)

        east_field_ptr(i)=cart_vec_ptr(1,i)*east_uvec_ptr(1,i)+ &
                          cart_vec_ptr(2,i)*east_uvec_ptr(2,i)+ &
                          cart_vec_ptr(3,i)*east_uvec_ptr(3,i)

        north_field_ptr(i)=cart_vec_ptr(1,i)*north_uvec_ptr(1,i)+ &
                           cart_vec_ptr(2,i)*north_uvec_ptr(2,i)+ &
                           cart_vec_ptr(3,i)*north_uvec_ptr(3,i)

!        if (i .lt. 1000) then
!           write(*,*) i," east=",east_field_ptr(i)," north=",north_field_ptr(i)
!        endif

     enddo
  enddo

  ! return success
  rc=ESMF_SUCCESS

end subroutine Cart3D_to_Cardinal

subroutine StateFilterField(state, fieldName, threshold, rc)

    ! -- input variables
    type(stateType),            intent(inout) :: state
    character(len=*),           intent(in)    :: fieldName
    real(ESMF_KIND_R8),         intent(in)    :: threshold

    ! -- output variables
    integer, optional, intent(out) :: rc

    ! -- local variables
    integer                     :: localrc, rank
    type(ESMF_Field)            :: field
    type(ESMF_GeomType_Flag)    :: geomtype
    real(ESMF_KIND_R8), pointer :: fptr1d(:), fptr2d(:,:), fptr3d(:,:,:)

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    field = StateGetField(state, fieldName, options="origin", rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

    call ESMF_FieldGet(field, rank=rank, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) return  ! bail out

    select case (rank)
      case (1)
        call ESMF_FieldGet(field, farrayPtr=fptr1d, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out
        where (fptr1d < threshold) fptr1d = threshold
      case (2)
        call ESMF_FieldGet(field, farrayPtr=fptr2d, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out
        where (fptr2d < threshold) fptr2d = threshold
      case (3)
        call ESMF_FieldGet(field, farrayPtr=fptr3d, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) return  ! bail out
        where (fptr3d < threshold) fptr3d = threshold
      case default
        ! -- not implemented
    end select

end subroutine StateFilterField

  ! -- Intermediate mesh: begin definition ----------------------------

  function MeshCreateReducedGaussian(ipt_lats_node_a, lats_node_a, &
    lonsperlat, global_lats_a, colrad_a, vm, rc) result (mesh)

    integer,                       intent(in)  :: ipt_lats_node_a
    integer,                       intent(in)  :: lats_node_a
    integer,                       intent(in)  :: lonsperlat(:)
    integer,                       intent(in)  :: global_lats_a(:)
    real(ESMF_KIND_R8),            intent(in)  :: colrad_a(:)
    type(ESMF_VM),       optional, intent(in)  :: vm
    integer,             optional, intent(out) :: rc

    type(ESMF_Mesh) :: mesh

    ! -- local variables
    integer :: localrc, stat
    integer :: localPet, petCount
    integer :: latg, latg2, long
    integer :: i, iHemi, ip1, j, jb, jm1, js, k, kk, kp1, l, l1, m, n
    integer :: id, id1, id2, id3, id4
    integer :: lats_nodes, lats_start
    integer :: lnumNodes, numNodes, numElems, numQuads, numTris
    logical :: isVMCreated
    logical, dimension(:),   allocatable :: localNodes
    integer, dimension(:),   allocatable :: nodeIds, nodeOwners
    integer, dimension(:),   allocatable :: globalToLocalIdMap
    integer, dimension(:),   allocatable :: lnodeIds, lnodeOwners
    integer, dimension(:),   allocatable :: elemIds, elemType, elemConn
    integer, dimension(:),   allocatable :: nodeToPetMap
    integer, dimension(:,:), allocatable :: indexToIdMap
    real(ESMF_KIND_R8) :: dx, x1, x2, x3, x4
    real(ESMF_KIND_R8), dimension(:),   allocatable :: nodeCoords
    real(ESMF_KIND_R8), dimension(:),   allocatable :: lnodeCoords
    real(ESMF_KIND_R8), dimension(:),   allocatable :: y
    real(ESMF_KIND_R8), dimension(:,:), allocatable :: x
    type(ESMF_VM) :: localVM

    ! -- local parameters
    real(ESMF_KIND_R8), parameter :: rad2deg = &
      57.29577951308232087721_ESMF_KIND_R8

    ! begin
    if (present(rc)) rc = ESMF_SUCCESS

    ! perform sanity check on input array sizes
    latg = size(lonsperlat)
    latg2 = latg / 2

    if (size(global_lats_a) /= latg) then
      call ESMF_LogSetError(ESMF_RC_ARG_SIZE, &
        msg="sizes of global lats and lonsperlats arrays must be the same", &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)
      return
    end if

    if (size(colrad_a) /= latg2) then
      call ESMF_LogSetError(ESMF_RC_ARG_SIZE, &
        msg="size of colatitude array is inconsistent", &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)
      return
    end if

    ! get information on parallel environment
    if (present(vm)) then
      isVMCreated = ESMF_VMIsCreated(vm, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return
      if (isVMCreated) then
        ! - use provided VM
        localVM = vm
      else
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="provided VM was not created", &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)
        return
      end if
    else
      ! - retrieve VM from context
      call ESMF_VMGetCurrent(localVM, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return
    end if

    ! - retrieve grid decomposition across PETs
    call ESMF_VMGet(localVM, localPet=localPet, petCount=petCount, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return

    ! - build node-to-PET map on all PETs
    allocate(nodeToPetMap(2*petCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return

    nodeToPetMap = 0

    call ESMF_VMAllGather(localVM, (/ ipt_lats_node_a, lats_node_a /), &
      nodeToPetMap, 2, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return

    ! pre-compute global mesh coordinates, including 2-point halo region
    long = maxval(lonsperlat)

    n = long + 2
    allocate(x(n,latg), y(latg), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return

    x = 0._ESMF_KIND_R8
    y = 0._ESMF_KIND_R8
    do j = 1, latg2
      y(j) =  90._ESMF_KIND_R8 - rad2deg * colrad_a(j)
      dx    = 360._ESMF_KIND_R8 / lonsperlat(j)
      do i = 1, lonsperlat(j) + 2
        x(i,j) = (i-1)*dx
      end do
    end do
    x(:,latg:latg2+1:-1) =  x(:,1:latg2)
    y(  latg:latg2+1:-1) = -y(  1:latg2)

    ! compute global nodes
    numNodes = sum(lonsperlat)

    ! - allocate workspace
    allocate(indexToIdMap(long,latg), nodeIds(numNodes), &
      nodeCoords(2*numNodes), nodeOwners(numNodes), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return

    ! - define global node ids and coordinates
    k = 0
    l = 0
    do j = 1, latg
      do i = 1, lonsperlat(j)
        k = k + 1
        indexToIdMap(i,j) = k
        nodeIds(k) = k
        nodeCoords(l+1) = x(i,j)
        nodeCoords(l+2) = y(j)
        l = l + 2
      end do
    end do

    ! - assign node ownership
    k = 0
    do n = 0, petCount-1
      lats_start = nodeToPetMap(k+1)
      lats_nodes = nodeToPetMap(k+2)
      k = k + 2
      do j = 1, lats_nodes
        l = global_lats_a(lats_start+j-1)
        do i = 1, lonsperlat(l)
          nodeOwners(indexToIdMap(i,l)) = n
        end do
      end do
    end do

    deallocate(nodeToPetMap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return

    allocate(localNodes(numNodes), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return

    ! - assign local nodes based on node ownership first
    do i = 1, numNodes
      id = nodeIds(i)
      localNodes(id) = (nodeOwners(id) == localPet)
    end do

    ! compute global number and type of elements
    numElems = 0
    numQuads = 0
    numTris = 0
    do iHemi = 0, 1
      jb = (latg-1)*iHemi + 2*(1-iHemi)
      js = 1 - 2*iHemi
      do j = jb, latg2, js
        k = 1
        jm1 = j-js
        l  = lonsperlat(j)
        l1 = lonsperlat(jm1)
        do i = 1, l
          x1 = x(i  ,j)
          x2 = x(i+1,j)
          x3 = x(k+1,jm1)
          x4 = x(k  ,jm1)
          ip1 = mod(i  ,l)+1
          kk  = mod(k-1,l1)+1
          kp1 = mod(k  ,l1)+1
          id1 = indexToIdMap(i  ,j)
          id2 = indexToIdMap(ip1,j)
          id3 = indexToIdMap(kp1,jm1)
          id4 = indexToIdMap(kk ,jm1)
          if (x3 > x2) then
            ! - elements are triangles
            if ((nodeOwners(id1) == localPet) .or. &
                (nodeOwners(id2) == localPet) .or. &
                (nodeOwners(id4) == localPet)) then
              ! - assign all element's nodes to the local PET
              numElems = numElems + 1
              numTris = numTris + 1
              localNodes(id1) = .true.
              localNodes(id2) = .true.
              localNodes(id4) = .true.
            end if
            if (x4 < x2) then
              k = k + 1
              if ((nodeOwners(id2) == localPet) .or. &
                  (nodeOwners(id3) == localPet) .or. &
                  (nodeOwners(id4) == localPet)) then
                numElems = numElems + 1
                numTris = numTris + 1
                localNodes(id2) = .true.
                localNodes(id3) = .true.
                localNodes(id4) = .true.
              end if
            end if
          else
            ! - elements are quadrilaters
            k = k + 1
            if ((nodeOwners(id1) == localPet) .or. &
                (nodeOwners(id2) == localPet) .or. &
                (nodeOwners(id3) == localPet) .or. &
                (nodeOwners(id4) == localPet)) then
              numElems = numElems + 1
              numQuads = numQuads + 1
              ! - assign all element's nodes to the local PET
              localNodes(id1) = .true.
              localNodes(id2) = .true.
              localNodes(id3) = .true.
              localNodes(id4) = .true.
            end if
          end if
        end do
      end do
    end do

    ! now compute local nodes
    lnumNodes = count(localNodes)

    ! - allocate local node arrays and global to local node id map
    allocate(lnodeIds(lnumNodes), lnodeCoords(2*lnumNodes), &
      lnodeOwners(lnumNodes), globalToLocalIdMap(numNodes), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return

    globalToLocalIdMap = 0

    k = 0
    l = 0
    n = 0
    do i = 1, numNodes
      id = nodeIds(i)
      n = 2*(id-1)
      if (localNodes(id)) then
        k = k + 1
        globalToLocalIdMap(id) = k
        lnodeIds(k) = id
        lnodeOwners(k) = nodeOwners(id)
        lnodeCoords(l+1) = nodeCoords(n+1)
        lnodeCoords(l+2) = nodeCoords(n+2)
        l = l + 2
      end if
    end do

    ! free up memory used by nodes
    deallocate(nodeIds, nodeCoords, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return

    ! create Mesh object
    mesh = ESMF_MeshCreate(parametricDim=2, spatialDim=2, &
      coordSys=ESMF_COORDSYS_SPH_DEG, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return

    ! add local nodes
    call ESMF_MeshAddNodes(mesh, nodeIds=lnodeIds, &
      nodeCoords=lnodeCoords, nodeOwners=lnodeOwners, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return

    ! free up memory used by nodes
    deallocate(lnodeIds, lnodeCoords, lnodeOwners, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return

    ! define local elements
    ! - allocate work arrays for local elements
    allocate(elemIds(numElems), elemType(numElems), &
      elemConn(3*numTris+4*numQuads), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return

   ! - define Mesh elementa and connectivity
    m = 0
    n = 0
    id = 0
    do iHemi = 0, 1
      jb = (latg-1)*iHemi + 2*(1-iHemi)
      js = 1 - 2*iHemi
      do j = jb, latg2, js
        k = 1
        jm1 = j-js
        l  = lonsperlat(j)
        l1 = lonsperlat(jm1)
        do i = 1, l
          x1 = x(i  ,j)
          x2 = x(i+1,j)
          x3 = x(k+1,jm1)
          x4 = x(k  ,jm1)
          ip1 = mod(i  ,l)+1
          kk  = mod(k-1,l1)+1
          kp1 = mod(k  ,l1)+1
          id1 = indexToIdMap(i  ,j)
          id2 = indexToIdMap(ip1,j)
          id3 = indexToIdMap(kp1,jm1)
          id4 = indexToIdMap(kk ,jm1)
          if (x3 > x2) then
            ! - create triangles
            id = id + 1
            if ((nodeOwners(id1) == localPet) .or. &
                (nodeOwners(id2) == localPet) .or. &
                (nodeOwners(id4) == localPet)) then
              n = n + 1
              elemIds(n) = id
              elemType(n) = ESMF_MESHELEMTYPE_TRI
              elemConn(m + 1) = id1
              elemConn(m + 2) = id2
              elemConn(m + 3) = id4
              m = m + 3
            end if
            if (x4 < x2) then
              k = k + 1
              id = id + 1
              if ((nodeOwners(id2) == localPet) .or. &
                  (nodeOwners(id3) == localPet) .or. &
                  (nodeOwners(id4) == localPet)) then
                n = n + 1
                elemIds(n) = id
                elemType(n) = ESMF_MESHELEMTYPE_TRI
                elemConn(m + 1) = id4
                elemConn(m + 2) = id2
                elemConn(m + 3) = id3
                m = m + 3
              end if
            end if
          else
            ! - create quadrilaters
            k = k + 1
            id = id + 1
            if ((nodeOwners(id1) == localPet) .or. &
                (nodeOwners(id2) == localPet) .or. &
                (nodeOwners(id3) == localPet) .or. &
                (nodeOwners(id4) == localPet)) then
              n = n + 1
              elemIds(n) = id
              elemType(n) = ESMF_MESHELEMTYPE_QUAD
              elemConn(m + 1) = id1
              elemConn(m + 2) = id2
              elemConn(m + 3) = id3
              elemConn(m + 4) = id4
              m = m + 4
            end if
          end if
        end do
      end do
    end do

    ! - convert local element id to local element id
    do i = 1, size(elemConn)
      elemConn(i) = globalToLocalIdMap(elemConn(i))
    end do

    ! - free up memory used for elements definition
    deallocate(globalToLocalIdMap, indexToIdMap, localNodes, &
      nodeOwners, x, y, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return

    ! - add elements to Mesh object
    call ESMF_MeshAddElements(mesh, elementIds=elemIds, &
      elementTypes=elemType, elementConn=elemConn, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return

    ! - free up memory used for local element arrays
    deallocate(elemIds, elemType, elemConn, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return

  end function MeshCreateReducedGaussian

  !-----------------------------------------------------------------------------

  function MeshCreate3DReducedGaussian(ipt_lats_node_a, lats_node_a, &
    lonsperlat, global_lats_a, colrad_a, vlevs, fill_option, vm, rc) result (mesh)

    integer,                       intent(in)  :: ipt_lats_node_a
    integer,                       intent(in)  :: lats_node_a
    integer,                       intent(in)  :: lonsperlat(:)
    integer,                       intent(in)  :: global_lats_a(:)
    real(ESMF_KIND_R8),            intent(in)  :: colrad_a(:)
    real(ESMF_KIND_R8),            intent(in)  :: vlevs(:)
    integer,             optional, intent(in)  :: fill_option
    type(ESMF_VM),       optional, intent(in)  :: vm
    integer,             optional, intent(out) :: rc

    type(ESMF_Mesh) :: mesh

    ! -- local variables
    integer :: localrc, stat
    integer :: localPet, petCount
    integer :: latg, latg2, long, levs
    integer :: i, iHemi, ip1, j, jb, jm1, js, k, kk, kp1, l, l1, m, n, v
    integer :: id, id1, id2, id3, id4, idOffset
    integer :: lats_nodes, lats_start
    integer :: lnumNodes, numNodes, numElems
    integer :: numNodes2D, numElems2D
    integer :: fillOption
    integer, dimension(4) :: p, q
    logical :: isVMCreated
    logical, dimension(:),   allocatable :: localNodes
    integer, dimension(:),   allocatable :: nodeIds, nodeOwners
    integer, dimension(:),   allocatable :: globalToLocalIdMap
    integer, dimension(:),   allocatable :: lnodeIds, lnodeOwners
    integer, dimension(:),   allocatable :: elemIds, elemType, elemConn
    integer, dimension(:),   allocatable :: nodeToPetMap
    integer, dimension(:,:), allocatable :: indexToIdMap
    real(ESMF_KIND_R8) :: dx, x1, x2, x3, x4
    real(ESMF_KIND_R8), dimension(:),   allocatable :: nodeCoords
    real(ESMF_KIND_R8), dimension(:),   allocatable :: lnodeCoords
    real(ESMF_KIND_R8), dimension(:),   allocatable :: y
    real(ESMF_KIND_R8), dimension(:,:), allocatable :: x
    type(ESMF_VM) :: localVM

    ! -- local parameters
    real(ESMF_KIND_R8), parameter :: rad2deg = &
      57.29577951308232087721_ESMF_KIND_R8

    ! begin
    if (present(rc)) rc = ESMF_SUCCESS

    ! fill options are:
    !  0 - no fill
    !  1 - connect circumpolar nodes with triangular elements (zigzag)
    !  2 - add polar nodes and connect them to circumpolar ones
    ! polar gaps are filled with option 1 by default (no additional nodes)
    fillOption = 1
    if (present(fill_option)) then
      if (fill_option < 0 .or. fill_option > 2) then
        call ESMF_LogSetError(ESMF_RC_ARG_OUTOFRANGE, &
          msg="fill_option can be 0, 1, or 2", &
          line=__LINE__,  &
          file=__FILE__,  &
            rcToReturn=rc)
        return
      end if
      fillOption = fill_option
    end if

    ! perform sanity check on input array sizes
    latg = size(lonsperlat)
    latg2 = latg / 2

    if (size(global_lats_a) /= latg) then
      call ESMF_LogSetError(ESMF_RC_ARG_SIZE, &
        msg="sizes of global lats and lonsperlats arrays must be the same", &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)
      return
    end if

    if (size(colrad_a) /= latg2) then
      call ESMF_LogSetError(ESMF_RC_ARG_SIZE, &
        msg="size of colatitude array is inconsistent", &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)
      return
    end if

    ! get information on parallel environment
    if (present(vm)) then
      isVMCreated = ESMF_VMIsCreated(vm, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return
      if (isVMCreated) then
        ! - use provided VM
        localVM = vm
      else
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="provided VM was not created", &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)
        return
      end if
    else
      ! - retrieve VM from context
      call ESMF_VMGetCurrent(localVM, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return
    end if

    ! - retrieve grid decomposition across PETs
    call ESMF_VMGet(localVM, localPet=localPet, petCount=petCount, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return

    ! - build node-to-PET map on all PETs
    allocate(nodeToPetMap(2*petCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return

    nodeToPetMap = 0

    call ESMF_VMAllGather(localVM, (/ ipt_lats_node_a, lats_node_a /), &
      nodeToPetMap, 2, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return

    ! pre-compute global mesh coordinates, including 2-point halo region
    long = maxval(lonsperlat)

    n = long + 2
    allocate(x(n,latg), y(latg), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return

    x = 0._ESMF_KIND_R8
    y = 0._ESMF_KIND_R8
    do j = 1, latg2
      y(j) =  90._ESMF_KIND_R8 - rad2deg * colrad_a(j)
      dx    = 360._ESMF_KIND_R8 / lonsperlat(j)
      do i = 1, lonsperlat(j) + 2
        x(i,j) = (i-1)*dx
      end do
    end do
    x(:,latg:latg2+1:-1) =  x(:,1:latg2)
    y(  latg:latg2+1:-1) = -y(  1:latg2)

    ! compute global nodes
    levs = size(vlevs)
    numNodes2D = sum(lonsperlat)
    numNodes = levs*numNodes2D

    ! - add polar nodes if requested
    if (fillOption == 2) numNodes = numNodes + 2 * levs

    ! - allocate workspace
    allocate(indexToIdMap(long,latg), nodeIds(numNodes), &
      nodeCoords(3*numNodes), nodeOwners(numNodes), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return

    indexToIdMap = 0
    nodeCoords   = 0._ESMF_KIND_R8
    nodeIds      = 0
    nodeOwners   = -1

    ! - define global node ids and coordinates
    k = 0
    l = 0
    do j = 1, latg
      do i = 1, lonsperlat(j)
        k = k + 1
        indexToIdMap(i,j) = k
        do v = 1, levs
          kk = k + (v-1)*numNodes2D
          nodeIds(kk) = kk
          l = 3*(kk-1)
          nodeCoords(l+1) = x(i,j)
          nodeCoords(l+2) = y(j)
          nodeCoords(l+3) = vlevs(v)
        end do
      end do
    end do

    ! - assign node ownership
    k = 0
    do n = 0, petCount-1
      lats_start = nodeToPetMap(k+1)
      lats_nodes = nodeToPetMap(k+2)
      k = k + 2
      do j = 1, lats_nodes
        l = global_lats_a(lats_start+j-1)
        do i = 1, lonsperlat(l)
          do v = 1, levs
            id = indexToIdMap(i,l)+(v-1)*numNodes2D
            nodeOwners(id) = n
          end do
        end do
      end do
    end do

    ! - add polar nodes and assign ownership
    if (fillOption == 2) then
      id = levs * numNodes2D
      l  = 3*id
      n = nodeOwners(indexToIdMap(1,1))
      do iHemi = 1, -1, -2
        do v = 1, levs
          id = id + 1
          nodeIds(id)     = id
          nodeOwners(id)  = n
          nodeCoords(l+1) =  0._ESMF_KIND_R8
          nodeCoords(l+2) = 90._ESMF_KIND_R8 * iHemi
          nodeCoords(l+3) = vlevs(v)
          l = l + 3
        end do
        n = nodeOwners(indexToIdMap(1,latg))
      end do
    end if

    deallocate(nodeToPetMap, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return

    allocate(localNodes(numNodes), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return

    localNodes = .false.

    ! - assign local nodes based on node ownership first
    do i = 1, numNodes
      id = nodeIds(i)
      localNodes(id) = (nodeOwners(id) == localPet)
    end do

    ! compute global number and type of elements
    numElems = 0
    numElems2D = 0
    do iHemi = 0, 1
      jb = (latg-1)*iHemi + 2*(1-iHemi)
      js = 1 - 2*iHemi
      do j = jb, latg2, js
        k = 1
        jm1 = j-js
        l  = lonsperlat(j)
        l1 = lonsperlat(jm1)
        do i = 1, l
          x1 = x(i  ,j)
          x2 = x(i+1,j)
          x3 = x(k+1,jm1)
          x4 = x(k  ,jm1)
          ip1 = mod(i  ,l)+1
          kk  = mod(k-1,l1)+1
          kp1 = mod(k  ,l1)+1
          id1 = indexToIdMap(i  ,j)
          id2 = indexToIdMap(ip1,j)
          id3 = indexToIdMap(kp1,jm1)
          id4 = indexToIdMap(kk ,jm1)
          if (x3 > x2) then
            ! - elements are collapsed hexahedrons with triangular faces
            numElems2D = numElems2D + 1
            if ((nodeOwners(id1) == localPet) .or. &
                (nodeOwners(id2) == localPet) .or. &
                (nodeOwners(id4) == localPet)) then
              ! - assign all element's nodes to the local PET
              numElems = numElems + 1
              localNodes(id1) = .true.
              localNodes(id2) = .true.
              localNodes(id4) = .true.
            end if
            if (x4 < x2) then
              k = k + 1
              numElems2D = numElems2D + 1
              if ((nodeOwners(id2) == localPet) .or. &
                  (nodeOwners(id3) == localPet) .or. &
                  (nodeOwners(id4) == localPet)) then
                numElems = numElems + 1
                localNodes(id2) = .true.
                localNodes(id3) = .true.
                localNodes(id4) = .true.
              end if
            end if
          else
            ! - elements are hexahedrons
            k = k + 1
            numElems2D = numElems2D + 1
            if ((nodeOwners(id1) == localPet) .or. &
                (nodeOwners(id2) == localPet) .or. &
                (nodeOwners(id3) == localPet) .or. &
                (nodeOwners(id4) == localPet)) then
              numElems = numElems + 1
              ! - assign all element's nodes to the local PET
              localNodes(id1) = .true.
              localNodes(id2) = .true.
              localNodes(id3) = .true.
              localNodes(id4) = .true.
            end if
          end if
        end do
      end do
    end do

    ! - add polar elements if requested
    select case (fillOption)
      case (1)
        do j = 1, latg, latg-1
          l = lonsperlat(j)
          id1 = indexToIdMap(1,j)
          numElems2D = numElems2D + l - 2
          if (nodeOwners(id1) == localPet) numElems = numElems + l - 2
        end do
      case (2)
        ! - polar nodes are assigned to the same PET as the circumpolar ones
        id = levs * numNodes2D + 1
        do j = 1, latg, latg-1
          if (nodeOwners(id) == localPet) numElems = numElems + lonsperlat(j)
          id = id + levs
        end do
    end select

    ! now compute local nodes
    do i = 1, numNodes2D
      do v = 2, levs
        id = i + (v-1)*numNodes2D
        localNodes(id) = localNodes(i)
      end do
    end do
    lnumNodes = count(localNodes)

    ! - allocate local node arrays and global to local node id map
    allocate(lnodeIds(lnumNodes), lnodeCoords(3*lnumNodes), &
      lnodeOwners(lnumNodes), globalToLocalIdMap(numNodes), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return

    globalToLocalIdMap = 0
    lnodeIds           = 0
    lnodeCoords        = 0._ESMF_KIND_R8
    lnodeOwners        = -1

    k = 0
    l = 0
    n = 0
    do i = 1, numNodes
      id = nodeIds(i)
      n = 3*(id-1)
      if (localNodes(id)) then
        k = k + 1
        globalToLocalIdMap(id) = k
        lnodeIds(k) = id
        lnodeOwners(k) = nodeOwners(id)
        lnodeCoords(l+1) = nodeCoords(n+1)
        lnodeCoords(l+2) = nodeCoords(n+2)
        lnodeCoords(l+3) = nodeCoords(n+3)
        l = l + 3
      end if
    end do

    ! free up memory used by nodes
    deallocate(nodeIds, nodeCoords, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return

    ! create Mesh object
    mesh = ESMF_MeshCreate(parametricDim=3, spatialDim=3, &
      coordSys=ESMF_COORDSYS_SPH_DEG, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return

    ! add local nodes
    call ESMF_MeshAddNodes(mesh, nodeIds=lnodeIds, &
      nodeCoords=lnodeCoords, nodeOwners=lnodeOwners, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return

    ! free up memory used by nodes
    deallocate(lnodeIds, lnodeCoords, lnodeOwners, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return

    ! define local elements
    ! - allocate work arrays for local elements
    numElems = numElems * (levs-1)
    allocate(elemIds(numElems), elemType(numElems), &
      elemConn(8*numElems), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return

    elemIds  = 0
    elemType = 0
    elemConn = 0

    ! - define Mesh elements and connectivity
    m = 0
    n = 0
    id = 0
    do iHemi = 0, 1
      jb = (latg-1)*iHemi + 2*(1-iHemi)
      js = 1 - 2*iHemi
      do j = jb, latg2, js
        k = 1
        jm1 = j-js
        l  = lonsperlat(j)
        l1 = lonsperlat(jm1)
        do i = 1, l
          x1 = x(i  ,j)
          x2 = x(i+1,j)
          x3 = x(k+1,jm1)
          x4 = x(k  ,jm1)
          ip1 = mod(i  ,l)+1
          kk  = mod(k-1,l1)+1
          kp1 = mod(k  ,l1)+1
          id1 = indexToIdMap(i  ,j)
          id2 = indexToIdMap(ip1,j)
          id3 = indexToIdMap(kp1,jm1)
          id4 = indexToIdMap(kk ,jm1)
          if (x3 > x2) then
            ! - elements are collapsed hexahedrons with triangular faces
            id = id + 1
            if ((nodeOwners(id1) == localPet) .or. &
                (nodeOwners(id2) == localPet) .or. &
                (nodeOwners(id4) == localPet)) then
              do v = 1, levs-1
                n = n + 1
                idOffset = (v-1)*numNodes2D
                elemIds(n) = id + (v-1)*numElems2D
                elemType(n) = ESMF_MESHELEMTYPE_HEX
                elemConn(m + 1) = id1 + idOffset
                elemConn(m + 2) = id2 + idOffset
                elemConn(m + 3) = id4 + idOffset
                elemConn(m + 4) = id4 + idOffset
                m = m + 4
                idOffset = idOffset + numNodes2D
                elemConn(m + 1) = id1 + idOffset
                elemConn(m + 2) = id2 + idOffset
                elemConn(m + 3) = id4 + idOffset
                elemConn(m + 4) = id4 + idOffset
                m = m + 4
              end do
            end if
            if (x4 < x2) then
              k = k + 1
              id = id + 1
              if ((nodeOwners(id2) == localPet) .or. &
                  (nodeOwners(id3) == localPet) .or. &
                  (nodeOwners(id4) == localPet)) then
                do v = 1, levs-1
                  n = n + 1
                  idOffset = (v-1)*numNodes2D
                  elemIds(n) = id + (v-1)*numElems2D
                  elemType(n) = ESMF_MESHELEMTYPE_HEX
                  elemConn(m + 1) = id4 + idOffset
                  elemConn(m + 2) = id2 + idOffset
                  elemConn(m + 3) = id3 + idOffset
                  elemConn(m + 4) = id3 + idOffset
                  m = m + 4
                  idOffset = idOffset + numNodes2D
                  elemConn(m + 1) = id4 + idOffset
                  elemConn(m + 2) = id2 + idOffset
                  elemConn(m + 3) = id3 + idOffset
                  elemConn(m + 4) = id3 + idOffset
                  m = m + 4
                end do
              end if
            end if
          else
            ! - elements are hexahedrons
            k = k + 1
            id = id + 1
            if ((nodeOwners(id1) == localPet) .or. &
                (nodeOwners(id2) == localPet) .or. &
                (nodeOwners(id3) == localPet) .or. &
                (nodeOwners(id4) == localPet)) then
              do v = 1, levs-1
                n = n + 1
                idOffset = (v-1)*numNodes2D
                elemIds(n) = id + (v-1)*numElems2D
                elemType(n) = ESMF_MESHELEMTYPE_HEX
                elemConn(m + 1) = id1 + idOffset
                elemConn(m + 2) = id2 + idOffset
                elemConn(m + 3) = id3 + idOffset
                elemConn(m + 4) = id4 + idOffset
                m = m + 4
                idOffset = idOffset + numNodes2D
                elemConn(m + 1) = id1 + idOffset
                elemConn(m + 2) = id2 + idOffset
                elemConn(m + 3) = id3 + idOffset
                elemConn(m + 4) = id4 + idOffset
                m = m + 4
              end do
            end if
          end if
        end do
      end do
    end do

    ! - add polar elements if requested
    select case (fillOption)
      case (1)
        do iHemi = 0, 1
          id = numElems2D-lonsperlat(latg)+2+(iHemi-1)*(lonsperlat(1)-2)
          j = (latg-1)*iHemi + 1
          if (nodeOwners(indexToIdMap(1,j)) == localPet) then
            l = lonsperlat(j)
            p = (/ 1, 2, l, 1 /)
            do k = 1, l-2
              id = id + 1
              do i = 1, 4
               q(i) = indexToIdMap(p(i),j)
              end do
              do v = 0, levs-2
                n = n + 1
                idOffset    = v*numNodes2D
                elemIds(n)  = id + v*numElems2D
                elemType(n) = ESMF_MESHELEMTYPE_HEX
                do kk = 1, 2
                  do i = 1, 4
                    elemConn(m + i) = q(i) + idOffset
                  end do
                  m = m + 4
                  idOffset = idOffset + numNodes2D
                end do
              end do
              p = perm(p)
            end do
          end if
        end do
      case (2)
        do iHemi = 0, 1
          j = latg*iHemi + (1-iHemi)
          l  = lonsperlat(j)
          id3 = levs * (numNodes2D + iHemi) + 1
          if (nodeOwners(id3) == localPet) then
            id = (levs-1) * (numElems2D + iHemi * lonsperlat(1))
            do i = 1, l
              ip1 = mod(i  ,l)+1
              id1 = indexToIdMap(i  ,j)
              id2 = indexToIdMap(ip1,j)
              id = id + 1
              do v = 1, levs-1
                n = n + 1
                idOffset = (v-1)*numNodes2D
                elemIds(n) = id + (v-1)*l
                elemType(n) = ESMF_MESHELEMTYPE_HEX
                elemConn(m + 1) = id1 + idOffset
                elemConn(m + 2) = id2 + idOffset
                elemConn(m + 3) = id3 + v - 1
                elemConn(m + 4) = id1 + idOffset
                m = m + 4
                idOffset = idOffset + numNodes2D
                elemConn(m + 1) = id1 + idOffset
                elemConn(m + 2) = id2 + idOffset
                elemConn(m + 3) = id3 + v
                elemConn(m + 4) = id1 + idOffset
                m = m + 4
              end do
            end do
          end if
        end do
    end select

    ! - convert local element id to local element id
    do i = 1, size(elemConn)
      elemConn(i) = globalToLocalIdMap(elemConn(i))
    end do

    ! - free up memory used for elements definition
    deallocate(globalToLocalIdMap, indexToIdMap, localNodes, &
      nodeOwners, x, y, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return

    ! - add elements to Mesh object
    call ESMF_MeshAddElements(mesh, elementIds=elemIds, &
      elementTypes=elemType, elementConn=elemConn, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return

    ! - free up memory used for local element arrays
    deallocate(elemIds, elemType, elemConn, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return

  contains

    function perm(a) result (b)
      integer, intent(in) :: a(4)
      integer :: b(4)

      b(1) = p(3)
      b(3) = p(2)
      b(2) = b(1) + sign(1,b(3)-b(1))
      b(4) = b(1)

    end function perm

  end function MeshCreate3DReducedGaussian

  !-----------------------------------------------------------------------------

  subroutine ReducedT62MeshCreate(gcomp, levels, mesh2d, mesh3d, levArray, rc)

    type(ESMF_GridComp)                     :: gcomp
    real(ESMF_KIND_R8),         intent(in)  :: levels(:)
    type(ESMF_Mesh)                         :: mesh3d
    type(ESMF_Mesh)                         :: mesh2d
    type(ESMF_Array), optional              :: levArray
    integer,          optional, intent(out) :: rc

    ! -- local variables
    integer :: localrc, stat
    integer :: localPet, petCount
    integer :: localDe, localDeCount
    integer :: levelCount
    integer :: iprint, item, n, pet
    integer :: lats_node_a
    integer :: ipt_lats_node_a
    integer, dimension(:), allocatable :: nlats
    integer, dimension(:), allocatable :: global_lats_a
    integer, dimension(:), allocatable :: lonsperlat
    real(ESMF_KIND_R8), dimension(:),   allocatable :: colrad_a, wgt_a, wgtcs_a, rcs2_a
    real(ESMF_KIND_R8), dimension(:,:), pointer     :: fptr
    type(ESMF_DistGrid) :: distgrid
    type(ESMF_VM)       :: vm

    ! -- local parameters
    integer, parameter :: latg2 = 47
    integer, parameter :: latg  = 2*latg2

    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    ! -- reduced T62 definitions
    allocate(lonsperlat(latg), global_lats_a(latg), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Unable to allocate memory", &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    lonsperlat(1:latg2) = (/ &
       30,   30,   30,   40,   48,   56,   60,   72,   72,   80, &
       90,   90,   96,  110,  110,  120,  120,  128,  144,  144, &
      144,  144,  154,  160,  160,  168,  168,  180,  180,  180, &
      180,  180,  180,  192,  192,  192,  192,  192,  192,  192, &
      192,  192,  192,  192,  192,  192,  192 /)
    lonsperlat(latg:latg2+1:-1) = lonsperlat(1:latg2)

    global_lats_a = (/ &
      47, 16, 80, 46, 78, 81, 45, 17, 14, 44, 79, 15, 43, 18, 13, &
      42, 77, 82, 41, 76, 83, 40, 74, 12, 39, 75, 11, 38, 22, 84, 37, 73, 85, &
      36, 21, 10, 35, 19, 9, 48, 20, 8, 49, 23, 87, 61, 72, 86, 60, 24, 88, 59, &
      25, 7, 58, 71, 6, 57, 70, 89, 56, 26, 90, 55, 68, 5, 54, 69, 91, 53, 27, &
      4, 52, 30, 93, 51, 31, 92, 50, 32, 1, 34, 33, 3, 29, 28, 2, 64, 63, 94, &
      65, 62, 66, 67 /)

    call ESMF_GridCompGet(gcomp, vm=vm, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    ! -- distribute latitudes across PETs using a round-robin algorithm
    allocate(nlats(petCount), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Unable to allocate memory", &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    nlats = 0
    n = 0
    do while (n < latg)
      do pet = 1, petCount
        if (n < latg) then
          nlats(pet) = nlats(pet) + 1
          n = n + 1
        else
          exit
        end if
      end do
    end do

    lats_node_a = nlats(localPet+1)
    ipt_lats_node_a = 1 + sum(nlats(1:localPet))
    deallocate(nlats, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Unable to free up memory", &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    iprint = 0
    if (localPet == 0) iprint = 1

    allocate(colrad_a(latg2), wgt_a(latg2), wgtcs_a(latg2), &
      rcs2_a(latg2), stat=stat)
    if (ESMF_LogFoundAllocError(statusToCheck=stat, &
      msg="Unable to allocate memory", &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    call gfs_dyn_glats(latg2, colrad_a, wgt_a, wgtcs_a, rcs2_a, iprint)

    deallocate(wgt_a, wgtcs_a, rcs2_a, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Unable to free up memory", &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    mesh2d = MeshCreateReducedGaussian(ipt_lats_node_a, lats_node_a, &
      lonsperlat, global_lats_a, colrad_a, vm=vm, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return

    ! -- create levels Array object if requested
    if (present(levArray)) then
      call ESMF_MeshGet(mesh2d, nodalDistgrid=distgrid, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out
      levelCount = size(levels)
      levArray = ESMF_ArrayCreate(distgrid, ESMF_TYPEKIND_R8, &
        undistLBound=(/ 1 /), undistUBound=(/ levelCount /), rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out
      call ESMF_ArrayGet(levArray, localDeCount=localDeCount, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out
      do localDe = 0, localDeCount-1
        nullify(fptr)
        call ESMF_ArrayGet(levArray, localDe=localDe, &
          farrayPtr=fptr, rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
        do item = 1, levelCount
          fptr(:,item) = levels(item)
        end do
      end do
    end if

    mesh3d = MeshCreate3DReducedGaussian(ipt_lats_node_a, &
      lats_node_a, lonsperlat, global_lats_a, colrad_a, &
      levels, vm=vm, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return

    deallocate(lonsperlat, global_lats_a, colrad_a, stat=stat)
    if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
      msg="Unable to free up memory", &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

  end subroutine ReducedT62MeshCreate

  subroutine gfs_dyn_glats(lgghaf,colrad,wgt,wgtcs,rcs2,iprint)
!
! Jan 2013   Henry Juang  increase precision by kind_qdt_prec=16
!                         to help wgt (Gaussian weighting)

      implicit none
      integer                  iter,k,k1,l2,lgghaf,iprint
!
! increase precision for more significant digit to help wgt
      real(kind=16) drad,dradz,p1,p2,phi,pi,rad,rc
      real(kind=16) rl2,scale,si,sn,w,x
!
      real(kind=8), dimension(lgghaf) ::  colrad, wgt, &
                                                      wgtcs,  rcs2
!
      real(kind=8), parameter :: cons0 = 0.d0, cons1 = 1.d0, &
                                 cons2 = 2.d0, cons4 = 4.d0, &
                                 cons180 = 180.d0, &
                                 cons360 = 360.d0, &
                                 cons0p25 = 0.25d0
      real(kind=16), parameter :: eps = 1.d-20
!
! for better accuracy to select smaller number
!     eps = 1.d-12
!     eps = 1.d-20
!
      if(iprint == 1) print 101
 101  format ('   i   colat   colrad     wgt', 12x, 'wgtcs',10x, 'iter  res')
      si    = cons1
      l2    = 2*lgghaf
      rl2   = l2
      scale = cons2/(rl2*rl2)
      k1    = l2-1
      pi    = atan(si)*cons4
!     dradz = pi / cons360 / 10.0
!  for better accuracy to start iteration
      dradz = pi / float(lgghaf) / 200.0
      rad   = cons0
      do k=1,lgghaf
        iter = 0
        drad = dradz
1       call gfs_dyn_poly(l2,rad,p2)
2       p1 = p2
        iter = iter + 1
        rad = rad + drad
        call gfs_dyn_poly(l2,rad,p2)
        if(sign(si,p1) == sign(si,p2)) go to 2
        if(drad < eps)go to 3
        rad  = rad-drad
        drad = drad * cons0p25
        go to 1
3       continue
        colrad(k) = rad
        phi = rad * cons180 / pi
        call gfs_dyn_poly(k1,rad,p1)
        x        = cos(rad)
        w        =  scale * (cons1 - x*x)/ (p1*p1)
        wgt(k)   = w
        sn       = sin(rad)
        w        = w/(sn*sn)
        wgtcs(k) = w
        rc       = cons1/(sn*sn)
        rcs2(k)  = rc
        call gfs_dyn_poly(l2,rad,p1)
        if(iprint == 1) &
             print 102,k,phi,colrad(k),wgt(k),wgtcs(k),iter,p1
 102    format(1x,i3,2x,f6.2,2x,f10.7,2x,e14.7,2x,e14.7,2x,i4,2x,e14.7)
      enddo
      if(iprint == 1) print 100,lgghaf
100   format(1h ,'shalom from 0.0e0 gfs_dyn_glats for ',i3)
!
      return
  end subroutine

  subroutine gfs_dyn_poly(n,rad,p)
!
      implicit none
!
      integer                  i,n
!
! increase precision for more significant digit to help wgt
      real(kind=16) floati,g,p,rad,x,y1,y2,y3
!
      real(kind=8), parameter ::  cons1 = 1.d0
!
      x  = cos(rad)
      y1 = cons1
      y2 = x
      do i=2,n
        g = x*y2
        floati = i
        y3 = g - y1 + g - (g-y1)/floati
        y1 = y2
        y2 = y3
      enddo
      p = y3
      return
  end subroutine

  ! -- Intermediate mesh: end definition ------------------------------

  ! -- Config: begin definition ---------------------------------------

  subroutine ConfigGet(gcomp, levels, meshWrite, filePrefix, rc)

    type(ESMF_GridComp)                                :: gcomp
    real(ESMF_KIND_R8), pointer, optional              :: levels(:)
    logical,                     optional, intent(out) :: meshWrite
    character(len=ESMF_MAXSTR),  optional, intent(out) :: filePrefix
    integer,                     optional, intent(out) :: rc

    ! -- local variables
    logical :: configIsPresent
    integer :: localrc, stat
    integer :: levelCount, columnCount
    integer :: item
    type(ESMF_Config) :: config


    ! -- begin
    if (present(rc)) rc = ESMF_SUCCESS

    configIsPresent = present(levels) .or. present(meshWrite) &
      .or. present(filePrefix)
    if (.not.configIsPresent) return

    if (present(levels)) then
      if (associated(levels)) then
        call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
          msg="levels pointer must not be associated",&
          line=__LINE__, &
          file=__FILE__, &
          rcToReturn=rc)
        return
      end if
    end if

    ! -- get vertical levels
    call ESMF_GridCompGet(gcomp, configIsPresent=configIsPresent, rc=localrc)
    if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__,  &
      file=__FILE__,  &
      rcToReturn=rc)) &
      return  ! bail out

    if (configIsPresent) then
      call ESMF_GridCompGet(gcomp, config=config, rc=localrc)
      if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)) &
        return  ! bail out
      if (present(levels)) then
        ! -- retrieve number of vertical levels in configuration file
        call ESMF_ConfigGetDim(config, levelCount, columnCount, &
          label="interpolation_levels::", rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
        if (levelCount == 0) then
          call ESMF_LogSetError(ESMF_RC_FILE_READ, msg="missing vertical levels", &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)
          return  ! bail out
        end if
        allocate(levels(levelCount), stat=stat)
        if (ESMF_LogFoundAllocError(statusToCheck=stat, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return
        levels = 0._ESMF_KIND_R8
        ! -- read levels
        call ESMF_ConfigFindLabel(config, "interpolation_levels::", rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
        do item = 1, levelCount
          call ESMF_ConfigNextLine(config, rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
          call ESMF_ConfigGetAttribute(config, levels(item), rc=localrc)
          if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
        end do
        ! -- verify that vertical coordinate is monotonically increasing
        if (any(levels(2:)-levels(1:levelCount-1) <= 0._ESMF_KIND_R8)) then
          call ESMF_LogSetError(ESMF_RC_NOT_VALID, &
            msg="level values must increase strictly monotonically", &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)
          deallocate(levels, stat=stat)
          if (ESMF_LogFoundDeallocError(statusToCheck=stat, &
            msg="Unable to free up memory", &
            line=__LINE__,  &
            file=__FILE__,  &
            rcToReturn=rc)) &
            return  ! bail out
          nullify(levels)
          return
        end if
      end if
      if (present(meshWrite)) then
        ! -- check if mesh needs to be written
        call ESMF_ConfigGetAttribute(config, meshWrite, &
          label="mesh_write:", default=.false., rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
      end if
      if (present(filePrefix)) then
        call ESMF_ConfigGetAttribute(config, filePrefix, &
          label="mesh_file_prefix:", default="med.mesh", rc=localrc)
        if (ESMF_LogFoundError(rcToCheck=localrc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__,  &
          file=__FILE__,  &
          rcToReturn=rc)) &
          return  ! bail out
      end if
    else
      call ESMF_LogSetError(ESMF_RC_NOT_FOUND, msg="missing config object", &
        line=__LINE__,  &
        file=__FILE__,  &
        rcToReturn=rc)
      return  ! bail out
    end if

  end subroutine ConfigGet

  ! -- Config: end definition -----------------------------------------

end module module_MED_SWPC_methods
