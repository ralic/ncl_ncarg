      SUBROUTINE MAPIQM (IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      INTEGER IAM(*),MCS,IAI(*),IAG(*),MAI
      REAL    XCS(*),YCS(*)
      IF (ICFELL('MAPIQM - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
      CALL MDPIQM (IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MAPIQM',2).NE.0) RETURN
      RETURN
      END
