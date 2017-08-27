      SUBROUTINE MDPIQM (IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
C
      INTEGER IAM(*),MCS,IAI(*),IAG(*),MAI
      REAL    XCS(*),YCS(*)
C
C Declare required common blocks.  See MAPBDX for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCMC/  IGI1,IGI2,NCRA,NOVS,XCRA(100),YCRA(100)
      INTEGER          IGI1,IGI2,NCRA,NOVS
      REAL             XCRA,YCRA
      SAVE   /MAPCMC/
C
C Check for an uncleared prior error.
C
      IF (ICFELL('MDPIQM - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Terminate the line, if any.
C
      IF (.NOT.(NCRA.GT.1)) GO TO 10000
      CALL ARDRLN (IAM,XCRA,YCRA,NCRA,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPIQM',2).NE.0) RETURN
      NCRA=0
10000 CONTINUE
C
C Done.
C
      RETURN
C
      END
