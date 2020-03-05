MODULE arrayof_intrinsic_mo
USE arrayof_mo
IMPLICIT NONE

TYPE,EXTENDS(arrayof_t) :: arrayof_integer_t
  INTEGER,POINTER :: array(:) => NULL()
  CONTAINS
  PROCEDURE :: copyfrom => copyfrom_integer
  PROCEDURE :: assignp => assignp_integer
END TYPE arrayof_integer_t

TYPE,EXTENDS(arrayof_t) :: arrayof_real_t
  REAL,POINTER :: array(:) => NULL()
  CONTAINS
  PROCEDURE :: copyfrom => copyfrom_real
  PROCEDURE :: assignp => assignp_real
END TYPE arrayof_real_t

PRIVATE
PUBLIC arrayof_integer_t, arrayof_real_t

CONTAINS

SUBROUTINE copyfrom_integer(this, from, start)
CLASS(arrayof_integer_t),INTENT(inout),TARGET :: this
CLASS(*),INTENT(in) :: from(:)
INTEGER,INTENT(in) :: start

SELECT TYPE(from)
TYPE is (INTEGER)
  this%array(start:start+SIZE(from)-1) = from(:)
END SELECT

END SUBROUTINE copyfrom_integer


SUBROUTINE assignp_integer(this)
CLASS(arrayof_integer_t),INTENT(inout),TARGET :: this

CLASS(*),POINTER :: a(:)

a=>this%polyarr(:)
SELECT TYPE(a)
TYPE is (INTEGER)
  this%array => a
END SELECT

END SUBROUTINE assignp_integer


SUBROUTINE copyfrom_real(this, from, start)
CLASS(arrayof_real_t),INTENT(inout),TARGET :: this
CLASS(*),INTENT(in) :: from(:)
INTEGER,INTENT(in) :: start

SELECT TYPE(from)
TYPE is (REAL)
  this%array(start:start+SIZE(from)-1) = from(:)
END SELECT

END SUBROUTINE copyfrom_real


SUBROUTINE assignp_real(this)
CLASS(arrayof_real_t),INTENT(inout),TARGET :: this

CLASS(*),POINTER :: a(:)

a=>this%polyarr(:)
SELECT TYPE(a)
TYPE is (REAL)
  this%array => a
END SELECT

END SUBROUTINE assignp_real

END MODULE arrayof_intrinsic_mo
