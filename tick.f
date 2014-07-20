!*****************************************************************
!
! NAME:
!	tick
! FUNCTION:
!	Fortran 90 replacement for the c timing routine
! SYNTAX:
!	real= tick()
! ON INPUT:
!
! ON OUTPUT:
!	tick: time in milliseconds
! CALLS:
!
! COMMENTS:
!
! LIBRARIES:
!
! REFERENCES:
!
! VERSION(S):
!	1. original version	j. behrens	3/98
!
!*****************************************************************
	FUNCTION tick() RESULT(tack)

	IMPLICIT NONE

	REAL                  :: tack
	INTEGER, DIMENSION(8) :: i_time
	REAL                  :: r_mili, r_secn, r_mint, r_hour
	REAL, PARAMETER       :: r_mil=1/1000.
	REAL, PARAMETER       :: r_sec=1.
	REAL, PARAMETER       :: r_min=60.*r_sec
	REAL, PARAMETER       :: r_hor=60.*r_min

!---------- call fortran 90 timing routine

	CALL date_and_time(values=i_time)

!---------- check for validity

	IF(i_time(1) /= -HUGE(0)) THEN
	  r_mili= REAL(i_time(8))
	  r_secn= REAL(i_time(7))
	  r_mint= REAL(i_time(6))
	  r_hour= REAL(i_time(5))
	  tack= r_hor* r_hour+ r_min* r_mint+ r_sec* r_secn+ r_mil* r_mili
	ELSE
	  tack= 0.
	END IF

	RETURN
	END
