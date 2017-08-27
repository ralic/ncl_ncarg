      SUBROUTINE SFSETR (CNP,RVP)
C
      CHARACTER*(*) CNP
C
C This subroutine is called to give a real value to a specified
C parameter.
C
C CNP is the name of the parameter whose value is to be set.
C
C RVP is a real variable containing the desired value.
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
      IF (ICFELL('SFSETR - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
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
      IF (.NOT.(CNP(1:2).EQ.'AN'.OR.CNP(1:2).EQ.'an')) GO TO 10002
      AID=RVP
      GO TO 10003
10002 CONTINUE
      IF (.NOT.(CNP(1:2).EQ.'CH'.OR.CNP(1:2).EQ.'ch')) GO TO 10004
      LCH=INT(RVP)
      GO TO 10003
10004 CONTINUE
      IF (.NOT.(CNP(1:2).EQ.'DC'.OR.CNP(1:2).EQ.'dc')) GO TO 10005
      IDC=MAX(0,INT(RVP))
      GO TO 10003
10005 CONTINUE
      IF (.NOT.(CNP(1:2).EQ.'DO'.OR.CNP(1:2).EQ.'do')) GO TO 10006
      LPA=INT(RVP)
      GO TO 10003
10006 CONTINUE
      IF (.NOT.(CNP(1:2).EQ.'DS'.OR.CNP(1:2).EQ.'ds')) GO TO 10007
      RDS=MAX(.0001,MIN(1.,RVP))
      GO TO 10003
10007 CONTINUE
      IF (.NOT.(CNP(1:2).EQ.'SP'.OR.CNP(1:2).EQ.'sp')) GO TO 10008
      DBL=RVP
      GO TO 10003
10008 CONTINUE
      IF (.NOT.(CNP(1:2).EQ.'TY'.OR.CNP(1:2).EQ.'ty')) GO TO 10009
      ITY=MAX(-4,MIN(2,INT(RVP)))
      GO TO 10003
10009 CONTINUE
      CTM(1:36)='SFSETR - PARAMETER NAME NOT KNOWN - '
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
