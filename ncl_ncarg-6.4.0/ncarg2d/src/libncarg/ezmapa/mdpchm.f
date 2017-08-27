      SUBROUTINE MDPCHM (IPRT,IDPT,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
C
      INTEGER IPRT,IDPT,IAM(*),MCS,IAI(*),IAG(*),MAI
      REAL    XCS(*),YCS(*)
C
C MDPCHM is called by various EZMAP routines to reset the color index,
C and dash pattern before and after drawing parts of a map by means
C of calls to ARDRLN (masked against an area map).
C
C The argument IPRT, if positive, says which part of the map is about
C to be drawn, as follows:
C
C     IPRT    Part of map.
C     ----    ------------
C       1     Perimeter.
C       2     Grid.
C       3     Labelling.
C       4     Limb lines.
C       5     Outline point group, continental.
C       6     Outline point group, U.S.
C       7     Outline point group, country.
C
C A call with IPRT equal to the negative of one of these values asks
C that the color index saved by the last call, with IPRT positive, be
C restored.
C
C When IPRT is positive, IDPT is the dash pattern to be used.  If IPRT
C is negative, IDPT is ignored.
C
C Declare required common blocks.  See MAPBDX for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM4/  GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PDRE,PLA1,PLA2,
     +                 PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLNO,PLTO,ROTA,
     +                 SRCH,XLOW,XROW,YBOW,YTOW,IDOT,IDSH,IDTL,ILCW,
     +                 ILTS,JPRJ,ELPF,INTF,LBLF,PRMF
      DOUBLE PRECISION GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PDRE,PLA1,PLA2,
     +                 PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLNO,PLTO,ROTA,
     +                 SRCH,XLOW,XROW,YBOW,YTOW
      INTEGER          IDOT,IDSH,IDTL,ILCW,ILTS,JPRJ
      LOGICAL          ELPF,INTF,LBLF,PRMF
      SAVE   /MAPCM4/
C
      COMMON /MAPCMQ/  ICIN(8)
      INTEGER          ICIN
      SAVE   /MAPCMQ/
C
C Declare one of the dash-package common blocks, too.
C
      COMMON /SMFLAG/  ISMO
      INTEGER          ISMO
      SAVE   /SMFLAG/
C
C Declare local variables.
C
      INTEGER          IGER,IPLS,IPMS,ISMS,ITXS
C
C Certain variables need to be saved between calls.
C
      SAVE IPLS,IPMS,ISMS,ITXS
C
C Flush all buffers before changing anything.
C
      CALL MDPIQM (IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPCHM',1).NE.0) RETURN
C
C Set/reset color index, dotting, and dash pattern.  The user has the
C last word.
C
      IF (.NOT.(IPRT.GT.0)) GO TO 10000
      ISMS=ISMO
      ISMO=1
      CALL DASHDB (IDPT)
      IF (ICFELL('MDPCHM',2).NE.0) RETURN
      IF (.NOT.(ICIN(IPRT).GE.0)) GO TO 10001
      CALL GQPLCI (IGER,IPLS)
      IF (.NOT.(IGER.NE.0)) GO TO 10002
      CALL SETER ('MDPCHM - ERROR EXIT FROM GQPLCI',3,1)
      RETURN
10002 CONTINUE
      CALL GQPMCI (IGER,IPMS)
      IF (.NOT.(IGER.NE.0)) GO TO 10003
      CALL SETER ('MDPCHM - ERROR EXIT FROM GQPMCI',4,1)
      RETURN
10003 CONTINUE
      CALL GQTXCI (IGER,ITXS)
      IF (.NOT.(IGER.NE.0)) GO TO 10004
      CALL SETER ('MDPCHM - ERROR EXIT FROM GQTXCI',5,1)
      RETURN
10004 CONTINUE
      CALL GSPLCI (ICIN(IPRT))
      CALL GSPMCI (ICIN(IPRT))
      CALL GSTXCI (ICIN(IPRT))
10001 CONTINUE
      CALL HLUMAPUSR (IPRT)
      IF (ICFELL('MDPCHM',6).NE.0) RETURN
      GO TO 10005
10000 CONTINUE
      CALL HLUMAPUSR (IPRT)
      IF (ICFELL('MDPCHM',7).NE.0) RETURN
      IF (.NOT.(ICIN(-IPRT).GE.0)) GO TO 10006
      CALL GSPLCI (IPLS)
      CALL GSPMCI (IPMS)
      CALL GSTXCI (ITXS)
10006 CONTINUE
      CALL DASHDB (IOR(ISHIFT(32767,1),1))
      IF (ICFELL('MDPCHM',8).NE.0) RETURN
      ISMO=ISMS
10005 CONTINUE
C
C Done.
C
      RETURN
C
      END
