      SUBROUTINE SFSETI (CNP,IVP)
C
      CHARACTER*(*) CNP
      INTEGER IVP
C
C This subroutine is called to give an integer value to a specified
C parameter.
C
C CNP is the name of the parameter whose value is to be set.
C
C IVP is an integer variable containing the desired value.
C
C Declare the labeled common block.
C
      COMMON /SFCOMN/ AID,DBL,ITY,LPA,RDS,IDC,LCH,LDP(8,8)
C
C Declare a local character variable in which to form an error message.
C
      CHARACTER*38 CTM
C
C RLB 3/2010: Previously the integer parameter was converted to a float
C   and the work was delegated off to SFSETR. This provided a sort
C   of "automatic type conversion", allowing the user to set a real
C   parameter using either sfseti() or sfsetr(), as in:
C        CALL SFSETI ('xxx',-9999)
C     or
C        CALL SFSETR ('xxx',-9999.0)
C
C   Color-indices are now either encoded RGBa values, or indices as
C   before. RGBa values are typically large integer values, beyond the
C   precision of floats, and thus this delegation scheme no longer
C   works correctly. The code has been refactored such that the integer
C   cases are now handled directly herein. If no action is found for
C   the CNP, then we delegate over to SFSETR.
C --------------------------------------------------------------------
C
C
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL SFBLDA
C
C Check for an uncleared prior error.
C
      IF (ICFELL('SFSETI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Check for a parameter name that is too short.
C
      IF (.NOT.(LEN(CNP).LT.2)) GO TO 10001
      CTM(1:36)='SFSETR - PARAMETER NAME TOO SHORT - '
      CTM(37:36+LEN(CNP))=CNP
      CALL SETER (CTM(1:36+LEN(CNP)),2,1)
      RETURN
10001 CONTINUE
C
C Set the appropriate parameter.
C
      IF (.NOT.(CNP(1:2).EQ.'CH'.OR.CNP(1:2).EQ.'ch')) GO TO 10002
      LCH=IVP
      GO TO 10003
10002 CONTINUE
      IF (.NOT.(CNP(1:2).EQ.'DC'.OR.CNP(1:2).EQ.'dc')) GO TO 10004
      IDC=MAX(0,IVP)
      GO TO 10003
10004 CONTINUE
      IF (.NOT.(CNP(1:2).EQ.'DO'.OR.CNP(1:2).EQ.'do')) GO TO 10005
      LPA=IVP
      GO TO 10003
10005 CONTINUE
      IF (.NOT.(CNP(1:2).EQ.'TY'.OR.CNP(1:2).EQ.'ty')) GO TO 10006
      ITY=MAX(-4,MIN(2,IVP))
      GO TO 10003
10006 CONTINUE
C         Convert the given value to a real and then use SFSETR to
C         set the parameter.
      CALL SFSETR (CNP,REAL(IVP))
      IF (ICFELL('SFSETI',2).NE.0) RETURN
10003 CONTINUE
C
C Done.
C
      RETURN
C
      END
