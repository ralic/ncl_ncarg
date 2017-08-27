      SUBROUTINE SFSETC (CNP,CVP)
C
      CHARACTER*(*) CNP,CVP
C
C This subroutine is called to give a character value to a specified
C parameter.
C
C CNP is the name of the parameter whose value is to be set.
C
C CVP is a character variable containing the desired value.
C
C
C Declare the labeled common block.
C
      COMMON /SFCOMN/ AID,DBL,ITY,LPA,RDS,IDC,LCH,LDP(8,8)
C
C Declare a local character variable in which to form an error message.
C
      CHARACTER*38 CTM
C
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL SFBLDA
C
C Check for an uncleared prior error.
C
      IF (ICFELL('SFSETC - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Check for a parameter name that is too short.
C
      IF (.NOT.(LEN(CNP).LT.2)) GO TO 10001
      CTM(1:36)='SFSETC - PARAMETER NAME TOO SHORT - '
      CTM(37:36+LEN(CNP))=CNP
      CALL SETER (CTM(1:36+LEN(CNP)),2,1)
      RETURN
10001 CONTINUE
C
C Set the appropriate parameter value.
C
      IF (.NOT.(CNP(1:2).EQ.'CH'.OR.CNP(1:2).EQ.'ch')) GO TO 10002
      LCH=ICHAR(CVP)
      GO TO 10003
10002 CONTINUE
      CTM(1:36)='SFSETC - PARAMETER NAME NOT KNOWN - '
      CTM(37:38)=CNP(1:2)
      CALL SETER (CTM(1:38),3,1)
      RETURN
10003 CONTINUE
C
C Done.
C
      RETURN
C
      END
