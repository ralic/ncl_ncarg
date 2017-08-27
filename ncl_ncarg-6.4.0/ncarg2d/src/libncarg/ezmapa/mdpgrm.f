      SUBROUTINE MDPGRM (IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
C
      INTEGER IAM(*),MCS,IAI(*),IAG(*),MAI
      REAL    XCS(*),YCS(*)
C
C Declare required common blocks.  See MAPBDX for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM0/  COS1,DTOR,DTRH,OOPI,PI,PIOF,PIOT,RTDD,RTOD,
     +                 SROT,SIN1,TOPI,TSRT
      DOUBLE PRECISION COS1,DTOR,DTRH,OOPI,PI,PIOF,PIOT,RTDD,RTOD,
     +                 SROT,SIN1,TOPI,TSRT
      SAVE   /MAPCM0/
C
      COMMON /MAPCM1/  COSO,COSR,SINO,SINR,IPRJ,IROD
      DOUBLE PRECISION COSO,COSR,SINO,SINR
      INTEGER          IPRJ,IROD
      SAVE   /MAPCM1/
C
      COMMON /MAPCM2/  BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                 URNG,VCEN,VMAX,VMIN,VOFF,VRNG,ISSL
      DOUBLE PRECISION BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                 URNG,VCEN,VMAX,VMIN,VOFF,VRNG
      INTEGER          ISSL
      SAVE   /MAPCM2/
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
C Declare local variables.
C
      DOUBLE PRECISION BLAT,BLON,DLAT,DLON,GLAT,GLON,OLAT,RLAT,RLON,
     +                 SLAT,SLON,U,V,X,XLAT,XLON
C
C Declare the type of two local arithmetic statement functions and the
C argument used with them.
C
      DOUBLE PRECISION CEIL,FLOR
C
C The arithmetic statement functions FLOR and CEIL give, respectively,
C the "floor" of X - the largest integer less than or equal to X - and
C the "ceiling" of X - the smallest integer greater than or equal to X.
C
      FLOR(X)=DINT(X+1.D4)-1.D4
      CEIL(X)=-FLOR(-X)
C
C Check for an uncleared prior error.
C
      IF (ICFELL('MDPGRM - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If EZMAP needs initialization, do it.
C
      IF (.NOT.(INTF)) GO TO 10000
      CALL MDPINT
      IF (ICFELL('MDPGRM',2).NE.0) RETURN
10000 CONTINUE
C
C If the grid is suppressed, do nothing.
C
      IF (GRID.LE.0.D0) RETURN
C
C Otherwise, set the latitude and longitude grid spacings.
C
      GLAT=GRID
      GLON=GRID
      IF (GRLA.GT.0.D0) GLAT=GRLA
      IF (GRLO.GT.0.D0) GLON=GRLO
C
C Reset the color index, dotting, and dash pattern for the grid.
C
      CALL MDPCHM (2,IDSH,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPGRM',3).NE.0) RETURN
C
C Transfer the latitude/longitude limits computed by MDPINT to local,
C modifiable variables.
C
      BLAT=BLAM
      BLON=BLOM
      SLAT=SLAM
      SLON=SLOM
C
C For certain azimuthal projections centered at a pole, the latitude
C limit furthest from the pole needs adjustment to make it projectable
C and visible.  Otherwise, we have trouble with portions of meridians
C disappearing.
C
      IF (.NOT.(IPRJ.EQ.3.OR.IPRJ.EQ.4.OR.IPRJ.EQ.6)) GO TO 10001
      IF (.NOT.(PLTO.GT.+89.999999D0)) GO TO 10002
      SLAT=SLAT+SRCH
      IF (IPRJ.EQ.3) SLAT=SLAT+SRCH
10002 CONTINUE
      IF (.NOT.(PLTO.LT.-89.999999D0)) GO TO 10003
      BLAT=BLAT-SRCH
      IF (IPRJ.EQ.3) BLAT=BLAT-SRCH
10003 CONTINUE
10001 CONTINUE
C
C RLON is the smallest longitude for which a meridian is to be drawn,
C XLON the biggest.  Avoid drawing a given meridian twice.
C
      RLON=GLON*FLOR(SLON/GLON)
      XLON=GLON*CEIL(BLON/GLON)
C
      IF (.NOT.(XLON-RLON.GT.359.999999D0)) GO TO 10004
      IF (.NOT.(IPRJ.EQ.1)) GO TO 10005
      RLON=GLON*CEIL((PLNO-179.999999D0)/GLON)
      XLON=GLON*FLOR((PLNO+179.999999D0)/GLON)
      GO TO 10006
10005 CONTINUE
      IF (.NOT.(IPRJ.GE.2.AND.IPRJ.LE.15)) GO TO 10007
      XLON=XLON-GLON
      IF (XLON-RLON.GT.359.999999D0) XLON=XLON-GLON
10006 CONTINUE
10007 CONTINUE
10004 CONTINUE
C
C OLAT is the latitude at which meridians that do not extend all the
C way to the poles are to stop.
C
      IF (.NOT.(IPRJ.EQ.16.OR.IPRJ.EQ.17.OR.IPRJ.EQ.19.OR.IPRJ.EQ.20.OR.
     +IPRJ.EQ.24.OR.IPRJ.EQ.25)) GO TO 10008
      OLAT=90.D0
      GO TO 10009
10008 CONTINUE
      IF (.NOT.(DINT(GRPO/1000.D0).EQ.0D0)) GO TO 10010
      OLAT=GLAT*FLOR(89.999999D0/GLAT)
      GO TO 10011
10010 CONTINUE
      OLAT=GLAT*FLOR(MIN(89.999999D0,DINT(GRPO/1000.D0))/GLAT)
10011 CONTINUE
10009 CONTINUE
C
C Draw the meridians.
C
      RLON=RLON-GLON
  101 RLON=RLON+GLON
      XLAT=OLAT
      IF (.NOT.(MOD(GRPO,1000.D0).GT.0.D0)) GO TO 10012
      IF (MOD(RLON,MOD(GRPO,1000.D0)).EQ.0.D0) XLAT=90.D0
10012 CONTINUE
      RLAT=MAX(SLAT,-XLAT)
      XLAT=MIN(BLAT, XLAT)
      DLAT=(XLAT-RLAT)/CEIL((XLAT-RLAT)/GRDR)
      CALL MDPITM (RLAT,RLON,0,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPGRM',4).NE.0) RETURN
  102 RLAT=RLAT+DLAT
      CALL MDPITM (RLAT,RLON,1,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPGRM',5).NE.0) RETURN
      IF (RLAT.LT.XLAT-.5D0*DLAT) GO TO 102
      CALL MDPIQM (IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPGRM',6).NE.0) RETURN
      IF (RLON.LT.XLON-.5D0*GLON) GO TO 101
C
C Round the latitude limits to appropriate multiples of GLAT.
C
      SLAT=GLAT*FLOR(SLAT/GLAT)
      IF (SLAT.LE.-90.D0) SLAT=SLAT+GLAT
      BLAT=GLAT*CEIL(BLAT/GLAT)
      IF (BLAT.GE.+90.D0) BLAT=BLAT-GLAT
C
C If a fast-path cylindrical equidistant or cylindrical equal-area
C projection is in use and either or both of the poles is within the
C (rectangular) perimeter, arrange for the parallels at -90 and/or +90
C to be drawn.
C
      IF (.NOT.(IPRJ.EQ.16.OR.IPRJ.EQ.20)) GO TO 10013
      CALL MDPTRN (-90.D0,PLNO,U,V)
      IF (ICFELL('MDPGRM',7).NE.0) RETURN
      IF (U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN.AND.V.LE.VMAX)
     +                                                SLAT=SLAT-GLAT
      CALL MDPTRN (+90.D0,PLNO,U,V)
      IF (ICFELL('MDPGRM',8).NE.0) RETURN
      IF (U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN.AND.V.LE.VMAX)
     +                                                BLAT=BLAT+GLAT
10013 CONTINUE
C
C Draw the parallels.
C
      XLAT=SLAT-GLAT
  103 XLAT=XLAT+GLAT
      RLAT=MAX(-90.D0,MIN(+90.D0,XLAT))
      IF (.NOT.(DINT(GRPO/1000.D0).EQ.0.D0.OR.ABS(RLAT).LE.DINT(GRPO/100
     +0.D0))) GO TO 10014
      RLON=FLOR(SLON)
      XLON=MIN(CEIL(BLON),RLON+360.D0)
      DLON=(XLON-RLON)/CEIL((XLON-RLON)/GRDR)
      CALL MDPITM (RLAT,RLON,0,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPGRM',9).NE.0) RETURN
  104 RLON=RLON+DLON
      CALL MDPITM (RLAT,RLON,1,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPGRM',10).NE.0) RETURN
      IF (RLON.LT.XLON-.5D0*DLON) GO TO 104
      CALL MDPIQM (IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPGRM',11).NE.0) RETURN
10014 CONTINUE
      IF (XLAT.LT.BLAT-.5D0*GLAT) GO TO 103
C
C Restore the color index, and dash pattern.
C
      CALL MDPCHM (-2,0,IAM,XCS,YCS,MCS,IAI,IAG,MAI,LPR)
      IF (ICFELL('MDPGRM',12).NE.0) RETURN
C
C Done.
C
      RETURN
C
      END
