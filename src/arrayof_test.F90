PROGRAM arrayof_test
USE arrayof_intrinsic_mo
IMPLICIT NONE

TYPE(arrayof_integer_t) :: iarr
INTEGER :: sample

CALL iarr%insert((/3,8,3/))
CALL iarr%insert((/190.,88./))
CALL iarr%insert((/190,88/))
CALL iarr%insert(15)
CALL iarr%append(16)
CALL iarr%append(21.)
PRINT*,iarr%arraysize
sample = iarr%array(4)
PRINT*,iarr%array(1:iarr%arraysize)

END PROGRAM arrayof_test
