MODULE arrayof_intrinsic_mo
USE arrayof_mo
IMPLICIT NONE

TYPE,EXTENDS(arrayof_generic_t) :: arrayof_integer_t
  INTEGER,POINTER :: array(:) => NULL()
  CONTAINS
  PROCEDURE,PRIVATE :: insert_array_integer, insert_scalar_integer
  GENERIC :: insert => insert_array_integer, insert_scalar_integer
  PROCEDURE :: copyfrom => copyfrom_integer
  PROCEDURE :: assignp => assignp_integer
  PROCEDURE :: unassignp => unassignp_integer
END TYPE arrayof_integer_t

TYPE,EXTENDS(arrayof_generic_t) :: arrayof_real_t
  REAL,POINTER :: array(:) => NULL()
  CONTAINS
  PROCEDURE,PRIVATE :: insert_array_real, insert_scalar_real
  GENERIC :: insert => insert_array_real, insert_scalar_real
  PROCEDURE :: copyfrom => copyfrom_real
  PROCEDURE :: assignp => assignp_real
  PROCEDURE :: unassignp => unassignp_real
END TYPE arrayof_real_t

PRIVATE
PUBLIC arrayof_integer_t, arrayof_real_t

CONTAINS


SUBROUTINE insert_array_integer(this, content, pos)
CLASS(arrayof_integer_t),INTENT(inout) :: this
INTEGER,INTENT(in) :: content(:)
INTEGER,INTENT(in),OPTIONAL :: pos

CALL this%arrayof_generic_t%insert_array(content, pos)

END SUBROUTINE insert_array_integer

SUBROUTINE insert_scalar_integer(this, content, pos)
CLASS(arrayof_integer_t),INTENT(inout) :: this
INTEGER,INTENT(in) :: content
INTEGER,INTENT(in),OPTIONAL :: pos

CALL this%arrayof_generic_t%insert_scalar(content, pos)

END SUBROUTINE insert_scalar_integer


SUBROUTINE copyfrom_integer(this, from, start)
CLASS(arrayof_integer_t),INTENT(inout),TARGET :: this
CLASS(*),INTENT(in) :: from(:)
INTEGER,INTENT(in) :: start

SELECT TYPE(from)
TYPE is (INTEGER)
  this%array(start:start+SIZE(from)-1) = from(:)
END SELECT

END SUBROUTINE copyfrom_integer


SUBROUTINE assignp_integer(this, polyarr)
!CLASS(arrayof_integer_t),INTENT(inout),TARGET :: this
CLASS(arrayof_integer_t),INTENT(inout) :: this
CLASS(*),TARGET :: polyarr(:)

SELECT TYPE(polyarr)
TYPE is (INTEGER)
  this%array => polyarr
END SELECT

END SUBROUTINE assignp_integer


SUBROUTINE unassignp_integer(this)
CLASS(arrayof_integer_t),INTENT(inout),TARGET :: this

NULLIFY(this%array)

END SUBROUTINE unassignp_integer

SUBROUTINE insert_array_real(this, content, pos)
CLASS(arrayof_real_t),INTENT(inout) :: this
REAL,INTENT(in) :: content(:)
INTEGER,INTENT(in),OPTIONAL :: pos

CALL this%arrayof_generic_t%insert_array(content, pos)

END SUBROUTINE insert_array_real

SUBROUTINE insert_scalar_real(this, content, pos)
CLASS(arrayof_real_t),INTENT(inout) :: this
REAL,INTENT(in) :: content
INTEGER,INTENT(in),OPTIONAL :: pos

CALL this%arrayof_generic_t%insert_scalar(content, pos)

END SUBROUTINE insert_scalar_real


SUBROUTINE copyfrom_real(this, from, start)
CLASS(arrayof_real_t),INTENT(inout),TARGET :: this
CLASS(*),INTENT(in) :: from(:)
INTEGER,INTENT(in) :: start

SELECT TYPE(from)
TYPE is (REAL)
  this%array(start:start+SIZE(from)-1) = from(:)
END SELECT

END SUBROUTINE copyfrom_real


SUBROUTINE assignp_real(this, polyarr)
CLASS(arrayof_real_t),INTENT(inout) :: this
CLASS(*),TARGET :: polyarr(:)

SELECT TYPE(polyarr)
TYPE is (REAL)
  this%array => polyarr
END SELECT

END SUBROUTINE assignp_real


SUBROUTINE unassignp_real(this)
CLASS(arrayof_real_t),INTENT(inout),TARGET :: this

NULLIFY(this%array)

END SUBROUTINE unassignp_real

END MODULE arrayof_intrinsic_mo
