      SUBROUTINE MAPBLM (IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      INTEGER IAM(*),MCS,IAI(*),IAG(*),MAI
      REAL    XCS(*),YCS(*)
      IF (ICFELL('MAPBLM - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
      CALL MDPBLM (IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MAPBLM',2).NE.0) RETURN
      RETURN
      END
