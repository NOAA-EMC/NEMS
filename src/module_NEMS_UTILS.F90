#include "ESMFConvenienceMacros.h"

!-----------------------------------------------------------------------
!
      MODULE module_NEMS_UTILS

      USE ESMF

      implicit none

      private
      public :: check_esmf_pet,  message_check

      character(esmf_maxstr) :: message_check

      contains

      subroutine check_esmf_pet(print_esmf)
!
!-----------------------------------------------------------------------
!
  implicit none
  integer :: i,n
  character *256 :: c1,c2
  logical :: opened,print_esmf
!
  do n=101,201
    inquire(n,opened=opened)
    if(.not.opened)then
      open(n,file='model_configure',status='old')  !<-- Open configure file
      exit
    endif
  enddo
!
  print_esmf=.false.
!
  do i=1,10000
    read(n,*,end=22)c1,c2
    if(c1(1:10) == 'print_esmf') then              !<-- Search for print_esmf flag
      if( c2 == 'true'   .or.          &           !<-- Check if print_esmf is true or false
          c2 == '.true.' .or.          &
          c2 == 'TRUE'   .or.          &
          c2 == '.TRUE.' ) print_esmf=.true.
      exit
    endif
  enddo
22  close(n)
    return
!
!-----------------------------------------------------------------------
!
      end subroutine check_esmf_pet
!
!-----------------------------------------------------------------------


      end module module_NEMS_UTILS
