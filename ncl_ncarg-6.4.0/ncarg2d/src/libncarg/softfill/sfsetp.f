      SUBROUTINE SFSETP (IDP)
C
C Dimension the argument array.
C
      DIMENSION IDP(8,8)
C
C Declare the labeled common block.
C
      COMMON /SFCOMN/ AID,DBL,ITY,LPA,RDS,IDC,LCH,LDP(8,8)
C
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL SFBLDA
C
C Check for an uncleared prior error.
C
      IF (ICFELL('SFSETP - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Transfer the user's dot-pattern array into the common block.
C
      DO 10001 I=1,8
      DO 10002 J=1,8
      LDP(I,J)=IDP(I,J)
10002 CONTINUE
10001 CONTINUE
C
C Done.
C
      RETURN
C
      END
