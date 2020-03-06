PROGRAM arrayof_test
USE arrayof_intrinsic_mo
IMPLICIT NONE

TYPE(arrayof_integer_t) :: iarr
!TYPE(arrayof_real_t) :: rarr
INTEGER :: sample

CALL iarr%insert((/3,8,3/))
CALL iarr%insert((/19,8/))
CALL iarr%insert((/190,88/))
CALL iarr%insert(15)
CALL iarr%insert(16)
CALL iarr%insert(21)
PRINT*,iarr%arraysize
sample = iarr%array(4)
PRINT*,iarr%array(1:iarr%arraysize)
!CALL iarr%packarray()

!CALL rarr%insert((/190.,88./))
!CALL rarr%packarray()
!PRINT*,rarr%array

END PROGRAM arrayof_test
