MODULE arrayof_mo
IMPLICIT NONE

TYPE :: arrayof_generic_t
  CLASS(*),ALLOCATABLE,PRIVATE :: polyarr(:)
  INTEGER :: arraysize=0
  DOUBLE PRECISION :: overalloc=2.D0
  CONTAINS
  PROCEDURE :: insert_array, insert_scalar
  PROCEDURE :: remove
  PROCEDURE :: packarray
  PROCEDURE,PRIVATE :: alloc
  PROCEDURE :: copyfrom => copyfrom_generic
  PROCEDURE :: assignp => assignp_generic
  PROCEDURE :: unassignp => unassignp_generic
END TYPE arrayof_generic_t

PRIVATE
PUBLIC arrayof_generic_t

CONTAINS

SUBROUTINE insert_array(this, content, pos)
CLASS(arrayof_generic_t),INTENT(inout) :: this
CLASS(*),INTENT(in) :: content(:)
!INTEGER, INTENT(in), OPTIONAL :: nelem
INTEGER,INTENT(in),OPTIONAL :: pos

INTEGER :: i, n, p

!IF (PRESENT(content)) THEN ! size of data
n = SIZE(content)
!ELSE IF (PRESENT(nelem)) THEN ! explicit size
!  n = nelem
!ELSE ! default add one element
!  n = 1
!ENDIF
IF (n <= 0) RETURN ! nothing to do

IF (PRESENT(pos)) THEN ! clip p
  p = MAX(1, MIN(pos, this%arraysize+1))
ELSE ! pos not provided, append
  p = this%arraysize + 1
ENDIF
this%arraysize = this%arraysize + n

CALL this%alloc(content(1)) ! ensure to have space
DO i = this%arraysize, p+n, -1 ! push the elements forward starting from p
  CALL this%copyfrom(this%polyarr(i-n:i-n), i)
ENDDO
CALL this%copyfrom(content, p)

END SUBROUTINE insert_array


SUBROUTINE insert_scalar(this, content, pos)
CLASS(arrayof_generic_t),INTENT(inout) :: this
CLASS(*),INTENT(in) :: content
INTEGER,INTENT(in),OPTIONAL :: pos

INTEGER :: i, p
CLASS(*),ALLOCATABLE :: localcontent(:)

IF (PRESENT(pos)) THEN ! clip p
  p = MAX(1, MIN(pos, this%arraysize+1))
ELSE ! pos not provided, append
  p = this%arraysize + 1
ENDIF
this%arraysize = this%arraysize + 1

CALL this%alloc(content) ! ensure to have space
DO i = this%arraysize, p+1, -1 ! push the elements forward starting from p
  CALL this%copyfrom(this%polyarr(i-1:i-1), i)
ENDDO
ALLOCATE(localcontent(1), source=content) ! suboptimal, better way?
CALL this%copyfrom(localcontent, p)

END SUBROUTINE insert_scalar


!SUBROUTINE append_array(this, content)
!CLASS(arrayof_generic_t),INTENT(inout) :: this
!CLASS(*),INTENT(in) :: content(:)
!
!CALL this%insert_array(content)
!
!END SUBROUTINE append_array
!
!
!SUBROUTINE append_scalar(this, content)
!CLASS(arrayof_generic_t),INTENT(inout) :: this
!CLASS(*),INTENT(in) :: content
!
!CALL this%insert_scalar(content)
!
!END SUBROUTINE append_scalar


SUBROUTINE remove(this, nelem, pos, nodestroy)
CLASS(arrayof_generic_t),INTENT(inout) :: this
INTEGER, INTENT(in), OPTIONAL :: nelem
INTEGER, INTENT(in), OPTIONAL :: pos
LOGICAL, INTENT(in), OPTIONAL :: nodestroy

INTEGER :: i, n, p
#ifdef ARRAYOF_ORIGDESTRUCTOR
LOGICAL :: destroy
#endif

IF (this%arraysize <= 0) RETURN ! nothing to do
IF (PRESENT(nelem)) THEN ! explicit size
  n = nelem
  IF (n <= 0) RETURN ! nothing to do
ELSE ! default remove one element
  n = 1
ENDIF

IF (PRESENT(pos)) THEN ! clip p
  p = MAX(1, MIN(pos, this%arraysize-n+1))
ELSE ! pos not provided, cut at the end
  p = this%arraysize - n + 1
ENDIF

! destroy the elements if needed
#ifdef ARRAYOF_ORIGDESTRUCTOR
destroy = .TRUE.
IF (PRESENT(nodestroy)) THEN
  destroy = .NOT.nodestroy
ENDIF
IF (destroy) THEN
  DO i = p, p+n-1
    ARRAYOF_ORIGDESTRUCTOR(this%polyarr(i))
  ENDDO
ENDIF
#endif

this%arraysize = this%arraysize - n
DO i = p, this%arraysize ! push the elements backward starting from p
  CALL this%copyfrom(this%polyarr(i+n:i+n), i)
ENDDO
CALL this%alloc() ! release space if possible

END SUBROUTINE remove


SUBROUTINE packarray(this)
CLASS(arrayof_generic_t),INTENT(inout) :: this

DOUBLE PRECISION :: tmpoveralloc

tmpoveralloc = this%overalloc ! save value
this%overalloc = 1.0D0
CALL this%alloc() ! reallocate exact size
this%overalloc = tmpoveralloc

END SUBROUTINE packarray


SUBROUTINE alloc(this, sample)
CLASS(arrayof_generic_t),INTENT(inout) :: this
CLASS(*),INTENT(in),OPTIONAL :: sample

INTEGER :: newsize, copysize
CLASS(*),ALLOCATABLE :: tmparr(:)

copysize = 0
newsize = MAX(INT(this%arraysize*this%overalloc), this%arraysize)

IF (ALLOCATED(this%polyarr)) THEN ! array already allocated
! space is neither too small nor too big, nothing to do
  IF (SIZE(this%polyarr) >= this%arraysize .AND. SIZE(this%polyarr) <= newsize) RETURN
! if space is too big, reduce newsize
  IF (SIZE(this%polyarr) > newsize) newsize = this%arraysize
! transfer allocation to temporary object and allocate to new size
  CALL this%unassignp()
  CALL MOVE_ALLOC(this%polyarr, tmparr)
  WRITE(0,*)'allocating:',newsize,SIZE(tmparr)
  ALLOCATE(this%polyarr(newsize), mold=tmparr)
  CALL this%assignp(this%polyarr) ! assign typed pointer to polymorphic object
  copysize = MIN(this%arraysize, SIZE(tmparr)) ! restrict to valid intervals
  CALL this%copyfrom(tmparr(1:copysize), 1)
  DEALLOCATE(tmparr) ! destroy temporary
ELSE ! need to allocate from zero
  IF (PRESENT(sample)) THEN
    ALLOCATE(this%polyarr(newsize), mold=sample)
    CALL this%assignp(this%polyarr) ! assign typed pointer to polymorphic object
!  ELSE internal error, should never happen
  ENDIF
ENDIF

END SUBROUTINE alloc


SUBROUTINE copyfrom_generic(this, from, start)
CLASS(arrayof_generic_t),INTENT(inout),TARGET :: this
CLASS(*),INTENT(in) :: from(:)
INTEGER,INTENT(in) :: start

END SUBROUTINE copyfrom_generic

SUBROUTINE assignp_generic(this, polyarr)
!CLASS(arrayof_generic_t),INTENT(inout),TARGET :: this
CLASS(arrayof_generic_t),INTENT(inout) :: this
CLASS(*),TARGET :: polyarr(:)

END SUBROUTINE assignp_generic

SUBROUTINE unassignp_generic(this)
CLASS(arrayof_generic_t),INTENT(inout),TARGET :: this

END SUBROUTINE unassignp_generic

END MODULE arrayof_mo
