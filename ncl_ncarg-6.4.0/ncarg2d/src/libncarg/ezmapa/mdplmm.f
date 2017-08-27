      SUBROUTINE MDPLMM (IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      INTEGER IAM(*),MCS,IAI(*),IAG(*),MAI
      REAL    XCS(*),YCS(*)
C
C SVOU is a character variable in which to save the value of the EZMAP
C internal parameter named 'OU'.
C
      CHARACTER*2      SVOU
C
C Check for an uncleared prior error.
C
      IF (ICFELL('MDPLMM - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Call the EZMAPA routine MDPBLM to generate limb lines (if any) and a
C perimeter and draw them masked by an area map.
C
      CALL MDGETC ('OU',SVOU)
      CALL MDSETC ('OU','NO')
      CALL MDPBLM (IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPLMM',2).NE.0) RETURN
      CALL MDSETC ('OU',SVOU)
C
C Done.
C
      RETURN
C
      END
