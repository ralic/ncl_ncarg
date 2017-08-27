      SUBROUTINE VTTPOM (RPNT,IEDG,ITRI,ISTR,RSTR,SSTR,IDIR,IDBC,
     +                   SLMX,ITER,SLTR,IEND,REND,SEND,IAMA,RTPL)
C
      DIMENSION RPNT(*),IEDG(*),ITRI(*),IAMA(*)
C
      EXTERNAL RTPL
C
C This routine is a copy of VTTSOM that has been modified to allow for
C tracing a line perpendicular to the direction of the flow field.  It
C is used for two purposes: 1) to trace a streamline generator; and 2)
C to trace a short line, perpendicular to a streamline, to see whether
C the streamline is within a specified distance of previously-drawn
C streamlines (referred to as "proximity testing").
C
C This routine, given arrays defining a triangular mesh, at each point
C of which a velocity vector is given, and the location of a particular
C point on a particular triangle of that mesh, traces a line that is
C perpendicular to the velocity vectors, following it until one of a
C set of termination conditions is satisfied.
C
C RPNT is an array of nodes defining vertices of triangles of the mesh.
C
C IEDG is an array of nodes defining edges (pairs of vertices) of the
C triangles of the mesh.
C
C ITRI is an array of nodes defining triangles (triplets of edges) of
C the mesh.
C
C ISTR is the base index, in ITRI, of the triangle node of the triangle
C containing the starting point, and RSTR and SSTR are coordinates of
C the starting point within that triangle (fractional multipliers of
C its first and second sides, respectively).
C
C IDIR is a flag that says in which direction the line is to be traced:
C 0 => toward its beginning; 1 => toward its end.
C
C IDBC is the color index for a color to be used to draw debug stuff.
C
C SLMX is the maximum length of line to be traced.
C
C ITER is a flag that is returned to say how the line terminated:
C
C   ITER=1 => exterior edge of mesh encountered.
C   ITER=3 => angle between velocity vectors exceeded maximum.
C   ITER=4 => line traced for specified distance.
C   ITER=5 => velocity along line dropped below VVMM.
C   ITER=6 => line entered a subtriangle occupied by a streamline.
C   ITER=7 => other (e. g., a degenerate triangle).
C
C SLTR is returned and is the length of line traced before a termination
C condition was encountered.
C
C IEND, REND, and SEND are returned; IEND is the base index, in ITRI,
C of the triangle node of the triangle containing the end point of the
C line and REND and SEND are the coordinates of the end point within
C that triangle.
C
C IAMA is an array containing an area map against which the line is to
C be masked.  If masking is not desired, set IAMA(1) = 0.
C
C RTPL is a routine to be called to draw the line (when it is masked).
C
C Declare all of the VASPACKT common blocks.
C
C
C VTCOM1 contains integer and real variables.
C
      COMMON /VTCOM1/ AHAW,AHLN,AHLR,AHSP,AHSR
      COMMON /VTCOM1/ ANIL,ANM1,ANM2,ANZF,AVEL,CHWM,CXIL,CXZF
      COMMON /VTCOM1/ CYIL,CYZF,DCNU,DCNV,DCNW,DMAX
      COMMON /VTCOM1/ DMIN,DVAL  !  REUSE FOR FLOWMIN AND FLOWMAX?
      COMMON /VTCOM1/ EMAX,EPSI
      COMMON /VTCOM1/ IBIL,IBZF,ICIL,ICLR(255)
      COMMON /VTCOM1/ ICSG,ICST,ICTT,ICTV,ICZF,IDBG,IISP
      COMMON /VTCOM1/ IIWS(2),IIWU,ILBC,IMPF
      COMMON /VTCOM1/ INIL  !  NEEDED? (INFORMATIONAL LABEL INDEX)
      COMMON /VTCOM1/ INIT,IPAI,IPIS
      COMMON /VTCOM1/ IPIL,IPZF,IRNG,IRWS(2),IRWU,ISET,ISTA(625)
      COMMON /VTCOM1/ ISVT,ITBM,IWSO,IZFF,JODP,JOMA
      COMMON /VTCOM1/ JOTZ,LCTM,LEA1,LEA2,LEA3,LEE1,LEE2,LEE3
      COMMON /VTCOM1/ LIWB,LIWK,LIWS(2),LNLG
      COMMON /VTCOM1/ LOEN,LOPN,LOTN
      COMMON /VTCOM1/ LRWK,LRWS(2)
      COMMON /VTCOM1/ LSDD,LSDL,LSDM,LTIL,LTZF,MIRO,NCLR
      COMMON /VTCOM1/ NDGL,NEDG,NEXL,NEXT,NEXU
      COMMON /VTCOM1/ NLBS  !  NEEDED? (NUMBER OF LABELS)
      COMMON /VTCOM1/ NLSD,NLZF,NOMF,NPNT
      COMMON /VTCOM1/ NR04  !  NEEDED? (LABEL-LIST MANAGEMENT)
      COMMON /VTCOM1/ NSDL,NSDR,NTRI,OORV,PCPX,PCPY,PCPZ
      COMMON /VTCOM1/ PITH  !  NEEDED? (STREAMLINE INTERPOLATION)
      COMMON /VTCOM1/ SCFS,SCFU  !  NEEDED? (SCALE FACTORS)
      COMMON /VTCOM1/ SLLN,SLLR,SLPS,SLPR,SLSP,SLSR,SVSP,SVSR
      COMMON /VTCOM1/ TTLL,TTLR,TTSP,TTSR,TVAL(0:256)
      COMMON /VTCOM1/ UVPB,UVPL,UVPR,UVPS,UVPT,UWDB,UWDL
      COMMON /VTCOM1/ UWDR,UWDT,VFRA,VRLN,VRLR,VRMG,VRMR
      COMMON /VTCOM1/ VVMM
      COMMON /VTCOM1/ WCIL,WCZF,WLIL,WLZF
      COMMON /VTCOM1/ WWIL,WWZF
      COMMON /VTCOM1/ XLBC,XMAX,XMIN,XVPL,XVPR,XWDL,XWDR
      COMMON /VTCOM1/ YLBC,YMAX,YMIN,YVPB,YVPT,YWDB,YWDT,ZMAX,ZMIN
C
      EQUIVALENCE (IIWS(1),II01),(LIWS(1),LI01)
      EQUIVALENCE (IIWS(2),II02),(LIWS(2),LI02)
      EQUIVALENCE (IRWS(1),IR01),(LRWS(1),LR01)
      EQUIVALENCE (IRWS(2),IR02),(LRWS(2),LR02)
      SAVE   /VTCOM1/
C
C VTCOM2 holds character parameters.
C
      COMMON /VTCOM2/ CHEX,CTMA,CTMB,FRMT
      COMMON /VTCOM2/ TXIL,TXZF
      CHARACTER*13 CHEX
      CHARACTER*500 CTMA,CTMB
      CHARACTER*8 FRMT
      CHARACTER*128 TXIL
      CHARACTER*64 TXZF
      SAVE   /VTCOM2/
C
C Declare local arrays for user-system polyline coordinates.
C
      PARAMETER (MCPL=100)
C
      DIMENSION XCPL(MCPL),YCPL(MCPL)
C
C Declare local arrays to use in drawing masked polylines.
C
      PARAMETER (MCPF=MCPL,MNOG=64)
      DIMENSION XCPF(MCPF),YCPF(MCPF),IAAI(MNOG),IAGI(MNOG)
C
C Declare a character variable to hold the digits from 0 to 9.
C
      CHARACTER*10 IDIG
C
C Put the digits from 0 to 9 in a single character variable.
C
      DATA IDIG / '0123456789' /
C
C DTOR is a multiplicative constant to convert from degrees to radians.
C
      DATA DTOR / .017453292519943 /
C
C IXOR(IONE,ITWO) is the exclusive OR of the 12-bit masks IONE and ITWO.
C
      IXOR(IONE,ITWO)=IAND(IOR(IONE,ITWO),4095-IAND(IONE,ITWO))
C
C ITBF(IARG) is non-zero if and only if a triangle with blocking-flag
C element IARG is blocked.
C
      ITBF(IARG)=IAND(IXOR(IARG,ITBX),ITBA)
C
C Extract the values of ITBX and ITBA from ITBM.
C
      ITBX=IAND(ISHIFT(ITBM,-12),4095)
      ITBA=IAND(       ITBM     ,4095)
C
C If debugging is turned on, save the initial polyline color and reset
C it to a specified color.
C
      IF (.NOT.(IDBG.NE.0)) GO TO 10001
        CALL GQPLCI (IGER,IPCS)
        IF (.NOT.(IGER.NE.0)) GO TO 10002
          CALL SETER ('VTTPOM - ERROR EXIT FROM GQPLCI',2,1)
          RETURN
10002   CONTINUE
        CALL PLOTIT (0,0,2)
        CALL GSPLCI (IDBC)
      GO TO 10003
10001 CONTINUE
        IPCS=-1
10003 CONTINUE
C
C Compute a test value, based on the maximum allowable angle, to be
C used below.
C
      IF (.NOT.(ANM2.NE.0.)) GO TO 10004
        CSMN=COS(DTOR*ANM2)
        CSMN=CSMN*ABS(CSMN)
10004 CONTINUE
C
C Set some tolerances for the drawing code.
C
      EPSX=ABS(XWDR-XWDL)*EPSI
      EPSY=ABS(YWDT-YWDB)*EPSI
C
      SMLX=.01*ABS(XWDR-XWDL)
      SMLY=.01*ABS(YWDT-YWDB)
C
      PITX=PITH*ABS(XWDR-XWDL)
      PITY=PITH*ABS(YWDT-YWDB)
C
C Initialize the pointer to the current triangle and find the base
C indices of the nodes defining its points.
C
      IIII=ISTR
C
C Find the base indices of point 1 (that edges 1 and 2 have in common),
C point 2 (that edges 2 and 3 have in common), and point 3 (that edges
C 3 and 1 have in common).
C
      IF (.NOT.(IEDG(ITRI(IIII+2)+1).NE.IEDG(ITRI(IIII+3)+1).AND.IEDG(IT
     +RI(IIII+2)+1).NE.IEDG(ITRI(IIII+3)+2))) GO TO 10005
        IPP1=IEDG(ITRI(IIII+2)+1)
        IPP2=IEDG(ITRI(IIII+2)+2)
      GO TO 10006
10005 CONTINUE
        IPP1=IEDG(ITRI(IIII+2)+2)
        IPP2=IEDG(ITRI(IIII+2)+1)
10006 CONTINUE
C
      IF (.NOT.(IEDG(ITRI(IIII+1)+1).NE.IPP1)) GO TO 10007
        IPP3=IEDG(ITRI(IIII+1)+1)
      GO TO 10008
10007 CONTINUE
        IPP3=IEDG(ITRI(IIII+1)+2)
10008 CONTINUE
C
C Initialize the starting point values.
C
      RVAL=RSTR
      SVAL=SSTR
C
C If debugging is turned on, mark the starting point of the line in a
C specified color.
C
      IF (.NOT.(IDBG.NE.0.AND.ITBF(ITRI(IIII+4)).EQ.0)) GO TO 10009
        CALL HLUVTMXYZ (IMPF,
     +                  RPNT(IPP1+1)+RVAL*(RPNT(IPP2+1)-RPNT(IPP1+1))+
     +                               SVAL*(RPNT(IPP3+1)-RPNT(IPP1+1)),
     +                  RPNT(IPP1+2)+RVAL*(RPNT(IPP2+2)-RPNT(IPP1+2))+
     +                               SVAL*(RPNT(IPP3+2)-RPNT(IPP1+2)),
     +                  RPNT(IPP1+3)+RVAL*(RPNT(IPP2+3)-RPNT(IPP1+3))+
     +                               SVAL*(RPNT(IPP3+3)-RPNT(IPP1+3)),
     +                  XPOS,YPOS)
        IF (.NOT.(XPOS.NE.OORV)) GO TO 10010
          CALL GQFACI (IGER,ISFC)
          CALL GSFACI (IDBC)
          CALL VTDREL (CUFX(XPOS),CUFY(YPOS),.0004,.0004,0.,10.,1)
          CALL GSFACI (ISFC)
10010   CONTINUE
10009 CONTINUE
C
C SLTR keeps track of the length of line traced already.
C
      SLTR=0.
C
C IPRJ is non-zero if and only if the streamline is to be projected
C from the triangular mesh to the image plane, either because the line
C is to be drawn there or because testing needs to be done there.
C
      IF (.NOT.(IDBG.EQ.0)) GO TO 10011
        IPRJ=0
      GO TO 10012
10011 CONTINUE
        IPRJ=1
10012 CONTINUE
C
C NCPL keeps track of the number of points in the coordinate arrays.
C
      NCPL=0
C
C RUDN keeps track of the ratio of segment length in the user coordinate
C system to segment length in the data coordinate system.
C
      RUDN=0.
C
C ICTB is non-zero if and only if the current triangle is blocked and
C ILTB is non-zero if and only if the last triangle was blocked.
C
      ICTB=1
C
C START TRACING LINE INSIDE TRIANGLE ----------------------------------
C
C Initializing - move ICTB to ILTB and recompute the correct value for
C the new triangle.
C
  101 ILTB=ICTB
      ICTB=ITBF(ITRI(IIII+4))
C
C Extract values from the point arrays describing the current triangle,
C including the coordinates of its vertices, the components of the
C velocity vectors at its vertices, and the values of the quantities
C to be used to determine the color of the line drawn.
C
      UCP1=RPNT(IPP1+1)
      VCP1=RPNT(IPP1+2)
      WCP1=RPNT(IPP1+3)
      UCP2=RPNT(IPP2+1)
      VCP2=RPNT(IPP2+2)
      WCP2=RPNT(IPP2+3)
      UCP3=RPNT(IPP3+1)
      VCP3=RPNT(IPP3+2)
      WCP3=RPNT(IPP3+3)
C
      IF (.NOT.(IDIR.EQ.0)) GO TO 10013
        UVP1=-RPNT(IPP1+4)
        VVP1=-RPNT(IPP1+5)
        WVP1=-RPNT(IPP1+6)
        UVP2=-RPNT(IPP2+4)
        VVP2=-RPNT(IPP2+5)
        WVP2=-RPNT(IPP2+6)
        UVP3=-RPNT(IPP3+4)
        VVP3=-RPNT(IPP3+5)
        WVP3=-RPNT(IPP3+6)
      GO TO 10014
10013 CONTINUE
        UVP1=+RPNT(IPP1+4)
        VVP1=+RPNT(IPP1+5)
        WVP1=+RPNT(IPP1+6)
        UVP2=+RPNT(IPP2+4)
        VVP2=+RPNT(IPP2+5)
        WVP2=+RPNT(IPP2+6)
        UVP3=+RPNT(IPP3+4)
        VVP3=+RPNT(IPP3+5)
        WVP3=+RPNT(IPP3+6)
10014 CONTINUE
C
      VMG1=SQRT(UVP1**2+VVP1**2+WVP1**2)
C
      IF (.NOT.(VMG1.NE.0.)) GO TO 10015
        UVP1=.001*EMAX*UVP1/VMG1
        VVP1=.001*EMAX*VVP1/VMG1
        WVP1=.001*EMAX*WVP1/VMG1
10015 CONTINUE
C
      VMG2=SQRT(UVP2**2+VVP2**2+WVP2**2)
C
      IF (.NOT.(VMG2.NE.0.)) GO TO 10016
        UVP2=.001*EMAX*UVP2/VMG2
        VVP2=.001*EMAX*VVP2/VMG2
        WVP2=.001*EMAX*WVP2/VMG2
10016 CONTINUE
C
      VMG3=SQRT(UVP3**2+VVP3**2+WVP3**2)
C
      IF (.NOT.(VMG3.NE.0.)) GO TO 10017
        UVP3=.001*EMAX*UVP3/VMG3
        VVP3=.001*EMAX*VVP3/VMG3
        WVP3=.001*EMAX*WVP3/VMG3
10017 CONTINUE
C
C Compute the coefficients A, B, C, and D in the equation defining the
C plane of the triangle (Ax+By+Cz+D=0).
C
      A=(VCP2-VCP1)*(WCP3-WCP1)-(VCP3-VCP1)*(WCP2-WCP1)
      B=(WCP2-WCP1)*(UCP3-UCP1)-(WCP3-WCP1)*(UCP2-UCP1)
      C=(UCP2-UCP1)*(VCP3-VCP1)-(UCP3-UCP1)*(VCP2-VCP1)
      D=-(A*UCP1+B*VCP1+C*WCP1)
C
C Compute the direction cosines of the normal to the triangle.  If they
C are not well-defined, take an error exit.
C
      DNOM=SQRT(A**2+B**2+C**2)
C
      IF (.NOT.(DNOM.NE.0.)) GO TO 10018
 
        DCNU=A/DNOM
        DCNV=B/DNOM
        DCNW=C/DNOM
C
      GO TO 10019
10018 CONTINUE
C
        ITER=7
        GO TO 104
C
10019 CONTINUE
C
C (09/29/2005) The following code resulted in discontinuities in the
C definitions of the velocity vectors along the edges of the triangles
C of the mesh.  I found a formulation that didn't have this problem,
C but I'm leaving the original code here, commented out, for possible
C future reference.
C
C For each velocity vector, use the parametric equations for a line that
C passes through the end of it and is perpendicular to the plane of the
C triangle to find the coordinates of the point where it intersects the
C triangle; use that to compute the components of the projection of the
C velocity vector into the plane of the triangle.
C
C       T=-(A*(UCP1+UVP1)+B*(VCP1+VVP1)+C*(WCP1+WVP1)+D)/
C    +                                                  (A**2+B**2+C**2)
C       UPP1=UVP1+A*T
C       VPP1=VVP1+B*T
C       WPP1=WVP1+C*T
C
C       T=-(A*(UCP2+UVP2)+B*(VCP2+VVP2)+C*(WCP2+WVP2)+D)/
C    +                                                  (A**2+B**2+C**2)
C       UPP2=UVP2+A*T
C       VPP2=VVP2+B*T
C       WPP2=WVP2+C*T
C
C       T=-(A*(UCP3+UVP3)+B*(VCP3+VVP3)+C*(WCP3+WVP3)+D)/
C    +                                                  (A**2+B**2+C**2)
C       UPP3=UVP3+A*T
C       VPP3=VVP3+B*T
C       WPP3=WVP3+C*T
C
C (09/29/2005) This is the alternate formulation.  Instead of a line
C perpendicular to the plane of the triangle, we use one that passes
C through a user-specified center point.  This ought to work okay for
C triangular meshes that are meant to represent the surface of a globe
C (using the center of the globe as the center point), which is the
C case of most interest to us, but it will not work for an arbitrary
C triangular mesh.  This is a problem I'll have to look into later.
C
      T=-((UCP1+UVP1)*A+(VCP1+VVP1)*B+(WCP1+WVP1)*C+D)/
     +   ((UCP1+UVP1-PCPX)*A+(VCP1+VVP1-PCPY)*B+(WCP1+WVP1-PCPZ)*C)
C
      UPP1=UVP1+(UCP1+UVP1-PCPX)*T
      VPP1=VVP1+(VCP1+VVP1-PCPY)*T
      WPP1=WVP1+(WCP1+WVP1-PCPZ)*T
C
      T=-((UCP2+UVP2)*A+(VCP2+VVP2)*B+(WCP2+WVP2)*C+D)/
     +   ((UCP2+UVP2-PCPX)*A+(VCP2+VVP2-PCPY)*B+(WCP2+WVP2-PCPZ)*C)
C
      UPP2=UVP2+(UCP2+UVP2-PCPX)*T
      VPP2=VVP2+(VCP2+VVP2-PCPY)*T
      WPP2=WVP2+(WCP2+WVP2-PCPZ)*T
C
      T=-((UCP3+UVP3)*A+(VCP3+VVP3)*B+(WCP3+WVP3)*C+D)/
     +   ((UCP3+UVP3-PCPX)*A+(VCP3+VVP3-PCPY)*B+(WCP3+WVP3-PCPZ)*C)
C
      UPP3=UVP3+(UCP3+UVP3-PCPX)*T
      VPP3=VVP3+(VCP3+VVP3-PCPY)*T
      WPP3=WVP3+(WCP3+WVP3-PCPZ)*T
C
C Modify these values, using cross products with the normal to the
C triangle to generate vectors perpendicular to the velocity vectors.
C
      UPPT=DCNV*WPP1-DCNW*VPP1
      VPPT=DCNW*UPP1-DCNU*WPP1
      WPPT=DCNU*VPP1-DCNV*UPP1
C
      UPP1=UPPT
      VPP1=VPPT
      WPP1=WPPT
C
      UPPT=DCNV*WPP2-DCNW*VPP2
      VPPT=DCNW*UPP2-DCNU*WPP2
      WPPT=DCNU*VPP2-DCNV*UPP2
C
      UPP2=UPPT
      VPP2=VPPT
      WPP2=WPPT
C
      UPPT=DCNV*WPP3-DCNW*VPP3
      VPPT=DCNW*UPP3-DCNU*WPP3
      WPPT=DCNU*VPP3-DCNV*UPP3
C
      UPP3=UPPT
      VPP3=VPPT
      WPP3=WPPT
C
C For each of the projected velocity vectors at the three vertices of
C the triangle, find R and S such that the vector may be expressed as R
C times the vector from V1 to V2 plus S times the vector from V1 to V3.
C There are three possible ways to compute these; we use the one that
C minimizes the probability of dividing zero by zero.
C
      DNUV=((UCP2-UCP1)*(VCP3-VCP1)-(VCP2-VCP1)*(UCP3-UCP1))
      DNVW=((VCP2-VCP1)*(WCP3-WCP1)-(WCP2-WCP1)*(VCP3-VCP1))
      DNWU=((WCP2-WCP1)*(UCP3-UCP1)-(UCP2-UCP1)*(WCP3-WCP1))
C
      IF (.NOT.(ABS(DNUV).GT.ABS(DNVW).AND.ABS(DNUV).GT.ABS(DNWU)))
     +GO TO 10020
        RVV1=(      UPP1 *(VCP3-VCP1)-      VPP1 *(UCP3-UCP1))/DNUV
        SVV1=((UCP2-UCP1)*      VPP1 -(VCP2-VCP1)*      UPP1 )/DNUV
        RVV2=(      UPP2 *(VCP3-VCP1)-      VPP2 *(UCP3-UCP1))/DNUV
        SVV2=((UCP2-UCP1)*      VPP2 -(VCP2-VCP1)*      UPP2 )/DNUV
        RVV3=(      UPP3 *(VCP3-VCP1)-      VPP3 *(UCP3-UCP1))/DNUV
        SVV3=((UCP2-UCP1)*      VPP3 -(VCP2-VCP1)*      UPP3 )/DNUV
      GO TO 10021
10020 CONTINUE
      IF (.NOT.(ABS(DNVW).GT.ABS(DNWU).AND.ABS(DNVW).GT.ABS(DNUV)))
     +GO TO 10022
        RVV1=(      VPP1 *(WCP3-WCP1)-      WPP1 *(VCP3-VCP1))/DNVW
        SVV1=((VCP2-VCP1)*      WPP1 -(WCP2-WCP1)*      VPP1 )/DNVW
        RVV2=(      VPP2 *(WCP3-WCP1)-      WPP2 *(VCP3-VCP1))/DNVW
        SVV2=((VCP2-VCP1)*      WPP2 -(WCP2-WCP1)*      VPP2 )/DNVW
        RVV3=(      VPP3 *(WCP3-WCP1)-      WPP3 *(VCP3-VCP1))/DNVW
        SVV3=((VCP2-VCP1)*      WPP3 -(WCP2-WCP1)*      VPP3 )/DNVW
      GO TO 10021
10022 CONTINUE
        RVV1=(      WPP1 *(UCP3-UCP1)-      UPP1 *(WCP3-WCP1))/DNWU
        SVV1=((WCP2-WCP1)*      UPP1 -(UCP2-UCP1)*      WPP1 )/DNWU
        RVV2=(      WPP2 *(UCP3-UCP1)-      UPP2 *(WCP3-WCP1))/DNWU
        SVV2=((WCP2-WCP1)*      UPP2 -(UCP2-UCP1)*      WPP2 )/DNWU
        RVV3=(      WPP3 *(UCP3-UCP1)-      UPP3 *(WCP3-WCP1))/DNWU
        SVV3=((WCP2-WCP1)*      UPP3 -(UCP2-UCP1)*      WPP3 )/DNWU
10021 CONTINUE
C
C See which subtriangle the line starts in and check the subtriangle
C mask to see if a streamline has passed through it already.  If so,
C terminate the line.
C
      INDR=MAX(0,MIN(4,INT(5.*RVAL)))
      INDS=MAX(0,MIN(4,INT(5.*SVAL)))
      INDT=MAX(0,MIN(4,INT(5.*(RVAL+SVAL))))
      IOSB=2*(5*INDR+INDS)-INDR*INDR+MOD(INDR+INDS+INDT,2)
      IF (.NOT.(IAND(ITRI(IIII+5),ISHIFT(1,3+IOSB)).NE.0)) GO TO 10023
        ITER=6
        GO TO 104
10023 CONTINUE
C
C If the streamline is being projected ...
C
      IF (.NOT.(IPRJ.NE.0)) GO TO 10024
C
C ... and the last triangle was blocked (or non-existent) and this one
C is not blocked, compute values associated with the first point of a
C new segment of streamline.  (If both triangles are unblocked, values
C computed would be identical to what we already had, but for round-off
C differences that could cause problems.)
C
        IF (.NOT.(ILTB.NE.0.AND.ICTB.EQ.0)) GO TO 10025
          UCND=UCP1+RVAL*(UCP2-UCP1)+SVAL*(UCP3-UCP1)
          VCND=VCP1+RVAL*(VCP2-VCP1)+SVAL*(VCP3-VCP1)
          WCND=WCP1+RVAL*(WCP2-WCP1)+SVAL*(WCP3-WCP1)
          L10027=    1
          GO TO 10027
10026     CONTINUE
10025   CONTINUE
C
C ... and, if the new triangle is blocked and there's something in the
C polyline buffer, clear the buffer.
C
        IF (.NOT.(ICTB.NE.0.AND.NCPL.NE.0)) GO TO 10028
          L10030=    1
          GO TO 10030
10029     CONTINUE
10028   CONTINUE
C
10024 CONTINUE
C
C Initialize a flag which keeps track of whether the line has terminated
C inside the triangle, and, if so, how.
C
      JUMP=0
C
C CONTINUE TRACING LINE INSIDE TRIANGLE -------------------------------
C
C Trace the line in the requested direction.  SLNE is the length of
C line to be traced before the next event, and SLNS is a saved copy
C of SLNE.
C
  102 SLNE=SLMX-SLTR
      SLNS=SLNE
C
C Take a step along the line and check for termination.  Helpful
C comments about some of the tests below are to be found in the
C file called "VectorMath".
C
  103 RVVN=RVV1+RVAL*(RVV2-RVV1)+SVAL*(RVV3-RVV1)
      SVVN=SVV1+RVAL*(SVV2-SVV1)+SVAL*(SVV3-SVV1)
C
      UPPN=RVVN*(UCP2-UCP1)+SVVN*(UCP3-UCP1)
      VPPN=RVVN*(VCP2-VCP1)+SVVN*(VCP3-VCP1)
      WPPN=RVVN*(WCP2-WCP1)+SVVN*(WCP3-WCP1)
C
      VMAG=SQRT(UPPN**2+VPPN**2+WPPN**2)
C
      IF (.NOT.(VMAG.GT.VVMM)) GO TO 10031
C
        TEMP=MIN(SLNE,SLPR)/VMAG
C
C Check for exit through the side joining V1 and V3.
C
        IF (.NOT.(RVAL+TEMP*RVVN.LT.0..AND.(RVAL.GT..001.OR.ABS(RVVN).GT
     +..001*ABS(RVVN+2.*SVVN)))) GO TO 10032
          JUMP=1
          TEMP=-RVAL/RVVN
10032   CONTINUE
C
C Check for exit through the side joining V1 and V2.
C
        IF (.NOT.(SVAL+TEMP*SVVN.LT.0..AND.(SVAL.GT..001.OR.ABS(SVVN).GT
     +..001*ABS(2.*RVVN+SVVN)))) GO TO 10033
          JUMP=2
          TEMP=-SVAL/SVVN
10033   CONTINUE
C
C Check for exit through the side joining V2 and V3.
C
        IF (.NOT.(1.-RVAL-SVAL-TEMP*(RVVN+SVVN).LT.0..AND.(1.-RVAL-SVAL.
     +GT..001.OR.ABS(RVVN+SVVN).GT..001*ABS(RVVN-SVVN)))) GO TO 10034
          JUMP=3
          TEMP=(1.-RVAL-SVAL)/(RVVN+SVVN)
10034   CONTINUE
C
        RVAL=MAX(0.,MIN(1.,RVAL+TEMP*RVVN))
        SVAL=MAX(0.,MIN(1.,SVAL+TEMP*SVVN))
C
C If the streamline is being projected and the current triangle is not
C blocked, save information about the previous point, generate values
C for the new point, and process the line segment joining them.
C
        IF (.NOT.(IPRJ.NE.0.AND.ICTB.EQ.0)) GO TO 10035
          UCOD=UCND
          VCOD=VCND
          WCOD=WCND
          XCOU=XCNU
          YCOU=YCNU
          IVOU=IVNU
          UCND=UCP1+RVAL*(UCP2-UCP1)+SVAL*(UCP3-UCP1)
          VCND=VCP1+RVAL*(VCP2-VCP1)+SVAL*(VCP3-VCP1)
          WCND=WCP1+RVAL*(WCP2-WCP1)+SVAL*(WCP3-WCP1)
          L10027=    2
          GO TO 10027
10036     CONTINUE
          L10038=    1
          GO TO 10038
10037     CONTINUE
10035   CONTINUE
C
C Reduce the streamline length to be traced before the next event.
C
        SLNE=SLNE-VMAG*TEMP
C
C If the line is now of the desired length at the next event, terminate
C it.
C
        IF (.NOT.(SLNE.LT..01*SLPR)) GO TO 10039
          SLNE=0.
          JUMP=4
10039   CONTINUE
C
C See which subtriangle the line is entering and check the subtriangle
C mask to see if a streamline has passed through it already.  If so,
C terminate the line.
C
        IOSL=IOSB
        INDR=MAX(0,MIN(4,INT(5.*RVAL)))
        INDS=MAX(0,MIN(4,INT(5.*SVAL)))
        INDT=MAX(0,MIN(4,INT(5.*(RVAL+SVAL))))
        IOSB=2*(5*INDR+INDS)-INDR*INDR+MOD(INDR+INDS+INDT,2)
        IF (.NOT.(IAND(ITRI(IIII+5),ISHIFT(ISTA(25*IOSL+IOSB+1),3)).NE.0
     +))GO TO 10040
          JUMP=6
10040   CONTINUE
C
      GO TO 10041
10031 CONTINUE
C
C The velocity is too low.
C
        JUMP=5
C
10041 CONTINUE
C
      IF (JUMP.EQ.0) GO TO 103
C
      IF (JUMP.EQ.4) JUMP=0
C
C Update various line-length quantities (line length in buffer and line
C length traced already).
C
      SLIB=SLNS-SLNE
      SLTR=SLTR+SLIB
C
C If the length of the line has hit the maximum, terminate it.
C
      IF (.NOT.(SLTR.GE.SLMX)) GO TO 10042
        ITER=4
        GO TO 104
10042 CONTINUE
C
C If there is more of the line to be traced in the current triangle,
C jump back to continue tracing it.
C
      IF (JUMP.EQ.0) GO TO 102
C
C Otherwise, the line terminated inside the triangle.  If that happened
C because RVAL became zero, move to the triangle, if any, that lies on
C the other side of edge 1 (joining vertices 3 and 1 of the triangle).
C
      IF (.NOT.(JUMP.EQ.1)) GO TO 10043
C
        IF (.NOT.(IEDG(ITRI(IIII+1)+1).EQ.IPP1.AND.IEDG(ITRI(IIII+1)+2).
     +EQ.IPP3)) GO TO 10044
          INEW=IEDG(ITRI(IIII+1)+3)
        GO TO 10045
10044   CONTINUE
        IF (.NOT.(IEDG(ITRI(IIII+1)+1).EQ.IPP3.AND.IEDG(ITRI(IIII+1)+2).
     +EQ.IPP1)) GO TO 10046
          INEW=IEDG(ITRI(IIII+1)+4)
        GO TO 10045
10046   CONTINUE
          INEW=-1
10045   CONTINUE
        IF (.NOT.(INEW.LT.0)) GO TO 10047
          ITER=1
          GO TO 104
10047   CONTINUE
        IIII=LOTN*((INEW-1)/LOTN)
        IPP2=IPP3
        IF (.NOT.(IEDG(ITRI(IIII+1)+1).NE.IPP1.AND.IEDG(ITRI(IIII+1)+1).
     +NE.IPP2)) GO TO 10048
          IPP3=IEDG(ITRI(IIII+1)+1)
        GO TO 10049
10048   CONTINUE
        IF (.NOT.(IEDG(ITRI(IIII+1)+2).NE.IPP1.AND.IEDG(ITRI(IIII+1)+2).
     +NE.IPP2)) GO TO 10050
          IPP3=IEDG(ITRI(IIII+1)+2)
        GO TO 10049
10050   CONTINUE
        IF (.NOT.(IEDG(ITRI(IIII+2)+1).NE.IPP1.AND.IEDG(ITRI(IIII+2)+1).
     +NE.IPP2)) GO TO 10051
          IPP3=IEDG(ITRI(IIII+2)+1)
        GO TO 10049
10051   CONTINUE
          IPP3=IEDG(ITRI(IIII+2)+2)
10049   CONTINUE
        RVAL=SVAL
        SVAL=0.
C
C If the line terminated because SVAL became zero, move to the triangle,
C if any, that lies on the other side of edge 2 (joining vertices 1 and
C 2 of the triangle).
C
      GO TO 10052
10043 CONTINUE
      IF (.NOT.(JUMP.EQ.2)) GO TO 10053
C
        IF (.NOT.(IEDG(ITRI(IIII+2)+1).EQ.IPP1.AND.IEDG(ITRI(IIII+2)+2).
     +EQ.IPP2)) GO TO 10054
          INEW=IEDG(ITRI(IIII+2)+4)
        GO TO 10055
10054   CONTINUE
        IF (.NOT.(IEDG(ITRI(IIII+2)+1).EQ.IPP2.AND.IEDG(ITRI(IIII+2)+2).
     +EQ.IPP1)) GO TO 10056
          INEW=IEDG(ITRI(IIII+2)+3)
        GO TO 10055
10056   CONTINUE
          INEW=-1
10055   CONTINUE
        IF (.NOT.(INEW.LT.0)) GO TO 10057
          ITER=1
          GO TO 104
10057   CONTINUE
        IIII=LOTN*((INEW-1)/LOTN)
        IPP3=IPP2
        IF (.NOT.(IEDG(ITRI(IIII+1)+1).NE.IPP1.AND.IEDG(ITRI(IIII+1)+1).
     +NE.IPP3)) GO TO 10058
          IPP2=IEDG(ITRI(IIII+1)+1)
        GO TO 10059
10058   CONTINUE
        IF (.NOT.(IEDG(ITRI(IIII+1)+2).NE.IPP1.AND.IEDG(ITRI(IIII+1)+2).
     +NE.IPP3)) GO TO 10060
          IPP2=IEDG(ITRI(IIII+1)+2)
        GO TO 10059
10060   CONTINUE
        IF (.NOT.(IEDG(ITRI(IIII+2)+1).NE.IPP1.AND.IEDG(ITRI(IIII+2)+1).
     +NE.IPP3)) GO TO 10061
          IPP2=IEDG(ITRI(IIII+2)+1)
        GO TO 10059
10061   CONTINUE
          IPP2=IEDG(ITRI(IIII+2)+2)
10059   CONTINUE
        SVAL=RVAL
        RVAL=0.
C
C If the line terminated because RVAL+SVAL became equal to one, move to
C the triangle, if any, that lies on the other side of edge 3 (joining
C vertices 2 and 3 of the triangle).
C
      GO TO 10052
10053 CONTINUE
      IF (.NOT.(JUMP.EQ.3)) GO TO 10062
C
        IF (.NOT.(IEDG(ITRI(IIII+3)+1).EQ.IPP2.AND.IEDG(ITRI(IIII+3)+2).
     +EQ.IPP3)) GO TO 10063
          INEW=IEDG(ITRI(IIII+3)+4)
        GO TO 10064
10063   CONTINUE
        IF (.NOT.(IEDG(ITRI(IIII+3)+1).EQ.IPP3.AND.IEDG(ITRI(IIII+3)+2).
     +EQ.IPP2)) GO TO 10065
          INEW=IEDG(ITRI(IIII+3)+3)
        GO TO 10064
10065   CONTINUE
          INEW=-1
10064   CONTINUE
        IF (.NOT.(INEW.LT.0)) GO TO 10066
          ITER=1
          GO TO 104
10066   CONTINUE
        IIII=LOTN*((INEW-1)/LOTN)
        IPP1=IPP2
        IF (.NOT.(IEDG(ITRI(IIII+1)+1).NE.IPP1.AND.IEDG(ITRI(IIII+1)+1).
     +NE.IPP3)) GO TO 10067
          IPP2=IEDG(ITRI(IIII+1)+1)
        GO TO 10068
10067   CONTINUE
        IF (.NOT.(IEDG(ITRI(IIII+1)+2).NE.IPP1.AND.IEDG(ITRI(IIII+1)+2).
     +NE.IPP3)) GO TO 10069
          IPP2=IEDG(ITRI(IIII+1)+2)
        GO TO 10068
10069   CONTINUE
        IF (.NOT.(IEDG(ITRI(IIII+2)+1).NE.IPP1.AND.IEDG(ITRI(IIII+2)+1).
     +NE.IPP3)) GO TO 10070
          IPP2=IEDG(ITRI(IIII+2)+1)
        GO TO 10068
10070   CONTINUE
          IPP2=IEDG(ITRI(IIII+2)+2)
10068   CONTINUE
        SVAL=1.-RVAL
        RVAL=0.
C
10052 CONTINUE
10062 CONTINUE
C
C If we just moved into a new triangle, we may need to recompute the
C values of the pointers to its vertices and of the coordinates of the
C point in the triangle.
C
      IF (.NOT.(JUMP.LT.4)) GO TO 10071
C
C Get a pointer to what should be point 1 of the triangle.  It will
C match one of the pointers we already have.
C
        IF (.NOT.(IEDG(ITRI(IIII+1)+1).EQ.IEDG(ITRI(IIII+2)+1).OR.IEDG(I
     +TRI(IIII+1)+1).EQ.IEDG(ITRI(IIII+2)+2))) GO TO 10072
          IPPT=IEDG(ITRI(IIII+1)+1)
        GO TO 10073
10072   CONTINUE
          IPPT=IEDG(ITRI(IIII+1)+2)
10073   CONTINUE
C
C Adjust the pointers and the values of RVAL and SVAL appropriately.
C
        IF (.NOT.(IPPT.NE.IPP1)) GO TO 10074
          RTMP=RVAL
          STMP=SVAL
          IF (.NOT.(IPPT.EQ.IPP2)) GO TO 10075
            IPP2=IPP3
            IPP3=IPP1
            IPP1=IPPT
            RVAL=STMP
            SVAL=1.-RTMP-STMP
          GO TO 10076
10075     CONTINUE
            IPP3=IPP2
            IPP2=IPP1
            IPP1=IPPT
            RVAL=1.-RTMP-STMP
            SVAL=RTMP
10076     CONTINUE
10074   CONTINUE
C
C Also, if we have been directed to examine the angles between the
C velocity vectors at its vertices, do that.
C
        IF (.NOT.(ANM2.NE.0.)) GO TO 10077
C
C Compute the squares of the cosines of the angles between the velocity
C vectors at pairs of vertices of the triangle.
C
          DNM1=(RPNT(IPP1+4)**2+RPNT(IPP1+5)**2+RPNT(IPP1+6)**2)*
     +         (RPNT(IPP2+4)**2+RPNT(IPP2+5)**2+RPNT(IPP2+6)**2)
          IF (.NOT.(DNM1.EQ.0.)) GO TO 10078
            ITER=7
            GO TO 104
10078     CONTINUE
          CSA1=(RPNT(IPP1+4)*RPNT(IPP2+4)+
     +          RPNT(IPP1+5)*RPNT(IPP2+5)+
     +          RPNT(IPP1+6)*RPNT(IPP2+6))
          CSA1=CSA1*ABS(CSA1)/DNM1
C
          DNM2=(RPNT(IPP2+4)**2+RPNT(IPP2+5)**2+RPNT(IPP2+6)**2)*
     +         (RPNT(IPP3+4)**2+RPNT(IPP3+5)**2+RPNT(IPP3+6)**2)
          IF (.NOT.(DNM2.EQ.0.)) GO TO 10079
            ITER=7
            GO TO 104
10079     CONTINUE
          CSA2=(RPNT(IPP2+4)*RPNT(IPP3+4)+
     +          RPNT(IPP2+5)*RPNT(IPP3+5)+
     +          RPNT(IPP2+6)*RPNT(IPP3+6))
          CSA2=CSA2*ABS(CSA2)/DNM2
C
          DNM3=(RPNT(IPP3+4)**2+RPNT(IPP3+5)**2+RPNT(IPP3+6)**2)*
     +         (RPNT(IPP1+4)**2+RPNT(IPP1+5)**2+RPNT(IPP1+6)**2)
          IF (.NOT.(DNM3.EQ.0.)) GO TO 10080
            ITER=7
            GO TO 104
10080     CONTINUE
          CSA3=(RPNT(IPP3+4)*RPNT(IPP1+4)+
     +          RPNT(IPP3+5)*RPNT(IPP1+5)+
     +          RPNT(IPP3+6)*RPNT(IPP1+6))
          CSA3=CSA3*ABS(CSA3)/DNM3
C
          IF (.NOT.(MIN(CSA1,CSA2,CSA3).LE.CSMN)) GO TO 10081
            ITER=3
            GO TO 104
10081     CONTINUE
C
10077   CONTINUE
C
C Jump back to continue tracing the streamline in the new triangle.
C
        GO TO 101
C
C Otherwise, ...
C
10071 CONTINUE
C
C ... transfer the termination condition flag within the triangle to
C the appropriate return variable and drop through to the return from
C this routine.
C
        ITER=JUMP
C
C
C Common exit point.  Process any remaining portion of the curve.
C
  104 CONTINUE
      IF (.NOT.(NCPL.NE.0)) GO TO 10082
        L10030=    2
        GO TO 10030
10083   CONTINUE
10082 CONTINUE
C
C If the polyline color index was changed above, restore it.
C
      IF (IPCS.GE.0) CALL GSPLCI (IPCS)
C
C Return the final trace position to the caller.
C
      IEND=IIII
      REND=RVAL
      SEND=SVAL
C
C If debugging is turned on and the line is being drawn, mark the
C termination point.
C
      IF (.NOT.(IDBG.NE.0.AND.ICTB.EQ.0)) GO TO 10084
        CALL HLUVTMXYZ (IMPF,UCND,VCND,WCND,XPOS,YPOS)
        IF (.NOT.(XPOS.NE.OORV)) GO TO 10085
          CALL GQFACI (IGER,ISFC)
          CALL GSFACI (IDBC)
          CALL VTDREL (CUFX(XPOS),CUFY(YPOS),.0004,.0004,0.,10.,1)
          CALL GSFACI (ISFC)
          CALL PLCHHQ (XPOS,YPOS,IDIG(ITER+1:ITER+1),.0004,0.,0.)
10085   CONTINUE
10084 CONTINUE
C
C Done.
C
      RETURN
C
C The following internal procedure, given a line segment, adds visible
C portions of it to the coordinate arrays.
C
10038 CONTINUE
C
C If point interpolation is turned on, do the first IPIS segments.
C
        IF (.NOT.(IPIS.NE.0)) GO TO 10086
          USOD=UCOD
          VSOD=VCOD
          WSOD=WCOD
          USND=UCND
          VSND=VCND
          WSND=WCND
          XSNU=XCNU
          YSNU=YCNU
          ISNU=IVNU
            INTP = 1
            GO TO 10089
10087       CONTINUE
            INTP =INTP +1
10089       CONTINUE
            IF (INTP .GT.(ABS(IPIS))) GO TO 10088
            UCND=USOD+(REAL(INTP)/REAL(ABS(IPIS)+1))*(USND-USOD)
            VCND=VSOD+(REAL(INTP)/REAL(ABS(IPIS)+1))*(VSND-VSOD)
            WCND=WSOD+(REAL(INTP)/REAL(ABS(IPIS)+1))*(WSND-WSOD)
            L10027=    3
            GO TO 10027
10090       CONTINUE
            IF (.NOT.(IPIS.GT.0.OR.IVNU.NE.IVOU)) GO TO 10091
              L10093=    1
              GO TO 10093
10092         CONTINUE
              UCOD=UCND
              VCOD=VCND
              WCOD=WCND
              XCOU=XCNU
              YCOU=YCNU
              IVOU=IVNU
10091       CONTINUE
          GO TO 10087
10088     CONTINUE
          UCND=USND
          VCND=VSND
          WCND=WSND
          XCNU=XSNU
          YCNU=YSNU
          IVNU=ISNU
10086   CONTINUE
C
C Finish off the job.
C
        L10093=    2
        GO TO 10093
10094   CONTINUE
C
      GO TO (10037) , L10038
C
C The following internal procedure examines the points (UCOD,VCOD,WCOD),
C which projects into (XCOU,YCOU), and (UCND,VCND,WCND), which projects
C into (XCNU,YCNU), either of which may be visible or invisible in the
C projection space, and adds visible portions of the line segment
C between them to the polyline being built.
C
10093 CONTINUE
C
        IF (.NOT.(UCND.NE.UCOD.OR.VCND.NE.VCOD.OR.WCND.NE.WCOD))
     +  GO TO 10095
C
          IF (.NOT.(NCPL.EQ.0)) GO TO 10096
            IF (.NOT.(IVOU.NE.0)) GO TO 10097
              IF (.NOT.(IMPF.NE.0.AND.PITH.GT.0.)) GO TO 10098
                UCLD=UCOD
                VCLD=VCOD
                WCLD=WCOD
                XCLU=XCOU
                YCLU=YCOU
10098         CONTINUE
              NCPL=1
              XCPL(1)=XCOU
              YCPL(1)=YCOU
            GO TO 10099
10097       CONTINUE
            IF (.NOT.(IVNU.NE.0)) GO TO 10100
              UCID=UCOD
              VCID=VCOD
              WCID=WCOD
              UCVD=UCND
              VCVD=VCND
              WCVD=WCND
              XCVU=XCNU
              YCVU=YCNU
              L10102=    1
              GO TO 10102
10101         CONTINUE
              L10104=    1
              GO TO 10104
10103         CONTINUE
              UCOD=UCVD
              VCOD=VCVD
              WCOD=WCVD
              XCOU=XCVU
              YCOU=YCVU
              IVOU=1
10099       CONTINUE
10100       CONTINUE
          GO TO 10105
10096     CONTINUE
          IF (.NOT.(NCPL.EQ.MCPL)) GO TO 10106
            L10108=    1
            GO TO 10108
10107       CONTINUE
10105     CONTINUE
10106     CONTINUE
C
          IF (.NOT.(IVNU.NE.0)) GO TO 10109
            L10111=    1
            GO TO 10111
10110       CONTINUE
          GO TO 10112
10109     CONTINUE
          IF (.NOT.(IVOU.NE.0)) GO TO 10113
            UCVD=UCOD
            VCVD=VCOD
            WCVD=WCOD
            XCVU=XCOU
            YCVU=YCOU
            UCID=UCND
            VCID=VCND
            WCID=WCND
            L10102=    2
            GO TO 10102
10114       CONTINUE
            UKND=UCND
            VKND=VCND
            WKND=WCND
            XKNU=XCNU
            YKNU=YCNU
            UCND=UCVD
            VCND=VCVD
            WCND=WCVD
            XCNU=XCVU
            YCNU=YCVU
            L10111=    2
            GO TO 10111
10115       CONTINUE
            UCND=UKND
            VCND=VKND
            WCND=WKND
            XCNU=XKNU
            YCNU=YKNU
            L10030=    3
            GO TO 10030
10116       CONTINUE
10112     CONTINUE
10113     CONTINUE
C
10095   CONTINUE
C
      GO TO (10092,10094) , L10093
C
C The following internal procedure outputs the next point; if mapping
C is being done and there is a sufficiently large jump in the mapped
C position of the point, we check for a discontinuity in the mapping.
C Similarly, if mapping is being done and point interpolation is
C activated, we check for a large enough jump in the mapped position
C to justify interpolating points.
C
10111 CONTINUE
        IF (.NOT.(IMPF.NE.0.AND.(UCND.NE.UCOD.OR.VCND.NE.VCOD.OR.WCND.NE
     +.WCOD))) GO TO 10117
          RUDO=RUDN
          RUDN=(ABS(XCNU-XCOU)+ABS(YCNU-YCOU))/
     +         (ABS(UCND-UCOD)+ABS(VCND-VCOD)+ABS(WCND-WCOD))
          IF (.NOT.(RUDN.GT.2.*RUDO)) GO TO 10118
            L10120=    1
            GO TO 10120
10119       CONTINUE
10118     CONTINUE
          IF (.NOT.(PITH.GT.0.)) GO TO 10121
            UCTD=UCND
            VCTD=VCND
            WCTD=WCND
            XCTU=XCNU
            YCTU=YCNU
            L10123=    1
            GO TO 10123
10122       CONTINUE
10121     CONTINUE
10117   CONTINUE
        NCPL=NCPL+1
        XCPL(NCPL)=XCNU
        YCPL(NCPL)=YCNU
      GO TO (10110,10115) , L10111
C
C The following internal procedure is invoked when an unusually large
C jump in the position of mapped points on the curve is seen.  It
C checks for a possible discontinuity in the mapping function (as
C can happen, for example, when a cylindrical equidistant projection
C is being used); if there is such a discontinuity, we must generate
C a final point on one side of it, dump the polyline, and then start
C a new polyline on the other side.
C
10120 CONTINUE
        UC1D=UCOD
        VC1D=VCOD
        WC1D=WCOD
        XC1U=XCOU
        YC1U=YCOU
        UC2D=UCND
        VC2D=VCND
        WC2D=WCND
        XC2U=XCNU
        YC2U=YCNU
        ITMP=0
10124   CONTINUE
          DSTO=ABS(XC2U-XC1U)+ABS(YC2U-YC1U)
          UC3D=(UC1D+UC2D)/2.
          VC3D=(VC1D+VC2D)/2.
          WC3D=(WC1D+WC2D)/2.
          CALL HLUCTMXYZ (IMPF,UC3D,VC3D,WC3D,XC3U,YC3U)
          IF (ICFELL('VTTPOM',2).NE.0) GO TO 104
          IF (.NOT.(OORV.EQ.0..OR.(XC3U.NE.OORV.AND.YC3U.NE.OORV)))
     +    GO TO 10125
            DST1=ABS(XC3U-XC1U)+ABS(YC3U-YC1U)
            DST2=ABS(XC3U-XC2U)+ABS(YC3U-YC2U)
            IF (.NOT.(MIN(DST1,DST2).GT.DSTO)) GO TO 10126
              ITMP=1000
              GO TO 10127
10126       CONTINUE
            IF (.NOT.(DST1.LT.DST2)) GO TO 10128
              IF (UC3D.EQ.UC1D.AND.VC3D.EQ.VC1D.AND.WC3D.EQ.WC1D) GO TO
     +10127
              UC1D=UC3D
              VC1D=VC3D
              WC1D=WC3D
              XC1U=XC3U
              YC1U=YC3U
            GO TO 10129
10128       CONTINUE
              IF (UC3D.EQ.UC2D.AND.VC3D.EQ.VC2D.AND.WC3D.EQ.WC2D) GO TO
     +10127
              UC2D=UC3D
              VC2D=VC3D
              WC2D=WC3D
              XC2U=XC3U
              YC2U=YC3U
10129       CONTINUE
            ITMP=ITMP+1
            IF (ITMP.EQ.64) GO TO 10127
          GO TO 10130
10125     CONTINUE
            UCVD=UCOD
            VCVD=VCOD
            WCVD=WCOD
            XCVU=XCOU
            YCVU=YCOU
            UCID=UC3D
            VCID=VC3D
            WCID=WC3D
            L10102=    3
            GO TO 10102
10131       CONTINUE
            L10104=    2
            GO TO 10104
10132       CONTINUE
            L10030=    4
            GO TO 10030
10133       CONTINUE
            UCID=UC3D
            VCID=VC3D
            WCID=WC3D
            UCVD=UCND
            VCVD=VCND
            WCVD=WCND
            XCVU=XCNU
            YCVU=YCNU
            L10102=    4
            GO TO 10102
10134       CONTINUE
            L10104=    3
            GO TO 10104
10135       CONTINUE
            ITMP=1000
            GO TO 10127
10130     CONTINUE
        GO TO 10124
10127   CONTINUE
        IF (.NOT.(ITMP.NE.1000.AND.(ABS(XC1U-XC2U).GT.SMLX.OR.ABS(YC1U-Y
     +C2U).GT.SMLY))) GO TO 10136
          IF (.NOT.(IMPF.NE.0.AND.PITH.GT.0.)) GO TO 10137
            UCTD=UC1D
            VCTD=VC1D
            WCTD=WC1D
            XCTU=XC1U
            YCTU=YC1U
            L10123=    2
            GO TO 10123
10138       CONTINUE
10137     CONTINUE
          NCPL=NCPL+1
          XCPL(NCPL)=XC1U
          YCPL(NCPL)=YC1U
          L10030=    5
          GO TO 10030
10139     CONTINUE
          IF (.NOT.(IMPF.NE.0.AND.PITH.GT.0.)) GO TO 10140
            UCLD=UC2D
            VCLD=VC2D
            WCLD=WC2D
            XCLU=XC2U
            YCLU=YC2U
10140     CONTINUE
          NCPL=1
          XCPL(1)=XC2U
          YCPL(1)=YC2U
10136   CONTINUE
      GO TO (10119) , L10120
C
C Given two points in the data-array-index coordinate system, one of
C which maps to a visible point and the other of which maps to an
C invisible point, this internal routine searches the line between
C them for a point near the edge of visibility.
C
10102 CONTINUE
        ITMP=0
10141   CONTINUE
          UCHD=(UCVD+UCID)/2.
          VCHD=(VCVD+VCID)/2.
          WCHD=(WCVD+WCID)/2.
          CALL HLUCTMXYZ (IMPF,UCHD,VCHD,WCHD,XCHU,YCHU)
          IF (ICFELL('VTTPOM',3).NE.0) GO TO 104
          IF (.NOT.(XCHU.NE.OORV.AND.YCHU.NE.OORV)) GO TO 10142
            IF (UCHD.EQ.UCVD.AND.VCHD.EQ.VCVD.AND.WCHD.EQ.WCVD) GO TO 10
     +143
            UCVD=UCHD
            VCVD=VCHD
            WCVD=WCHD
            XCVU=XCHU
            YCVU=YCHU
          GO TO 10144
10142     CONTINUE
            IF (UCHD.EQ.UCID.AND.VCHD.EQ.VCID.AND.WCHD.EQ.WCID) GO TO 10
     +143
            UCID=UCHD
            VCID=VCHD
            WCID=WCHD
10144     CONTINUE
          ITMP=ITMP+1
          IF (ITMP.EQ.64) GO TO 10143
        GO TO 10141
10143   CONTINUE
      GO TO (10101,10114,10131,10134) , L10102
C
C The following internal procedure outputs a visible edge point found
C by the previous internal procedure.
C
10104 CONTINUE
        IF (.NOT.(PITH.GT.0.)) GO TO 10145
          IF (.NOT.(NCPL.EQ.0)) GO TO 10146
            UCLD=UCVD
            VCLD=VCVD
            WCLD=WCVD
            XCLU=XCVU
            YCLU=YCVU
          GO TO 10147
10146     CONTINUE
            UCTD=UCVD
            VCTD=VCVD
            WCTD=WCVD
            XCTU=XCVU
            YCTU=YCVU
            L10123=    3
            GO TO 10123
10148       CONTINUE
10147     CONTINUE
10145   CONTINUE
        NCPL=NCPL+1
        XCPL(NCPL)=XCVU
        YCPL(NCPL)=YCVU
      GO TO (10103,10132,10135) , L10104
C
C The following internal procedure is invoked when mapping is being
C done and a new point is about to be added to the polyline buffer.
C It checks for a jump (using a user-defined threshold value) in
C the mapped coordinates of the point and, if such a jump is found,
C interpolates some points in between.  The assumption is made that
C all points in between are visible; if that is found not to be the
C case, no attempt is made to rectify the situation: the user probably
C screwed up the definition of the mapping function.
C
10123 CONTINUE
10149   CONTINUE
        IF (.NOT.(ABS(XCTU-XCLU).GT.PITX.OR.ABS(YCTU-YCLU).GT.PITY))
     +  GO TO 10150
          IFND=0
          UCQD=0.
          VCQD=0.
          WCQD=0.
          RDST=.50
          RSTP=.25
10151     CONTINUE
            UCPD=UCLD+RDST*(UCTD-UCLD)
            VCPD=VCLD+RDST*(VCTD-VCLD)
            WCPD=WCLD+RDST*(WCTD-WCLD)
            CALL HLUCTMXYZ (IMPF,UCPD,VCPD,WCPD,XCPU,YCPU)
            IF (ICFELL('VTTPOM',4).NE.0) GO TO 104
            IF (OORV.NE.0..AND.(XCPU.EQ.OORV.OR.YCPU.EQ.OORV)) GO TO 101
     +52
            IF (.NOT.(ABS(XCPU-XCLU).LT.PITX.AND.ABS(YCPU-YCLU).LT.PITY)
     +)     GO TO 10153
              IFND=1
              UCQD=UCPD
              VCQD=VCPD
              WCQD=WCPD
              XCQU=XCPU
              YCQU=YCPU
              IF (ABS(XCQU-XCLU).GT..5*PITX.OR.ABS(YCQU-YCLU).GT..5*PITY
     +)       GO TO 10152
              RDST=RDST+RSTP
            GO TO 10154
10153       CONTINUE
              RDST=RDST-RSTP
10154       CONTINUE
            RSTP=RSTP/2.
            IF (RSTP.LT..0001) GO TO 10152
          GO TO 10151
10152     CONTINUE
          IF (.NOT.(IFND.NE.0.AND.(UCQD.NE.UCLD.OR.VCQD.NE.VCLD.OR.WCQD.
     +NE.WCLD))) GO TO 10155
            NCPL=NCPL+1
            XCPL(NCPL)=XCQU
            YCPL(NCPL)=YCQU
            IF (.NOT.(NCPL.EQ.MCPL)) GO TO 10156
              L10108=    2
              GO TO 10108
10157         CONTINUE
10156       CONTINUE
            UCLD=UCQD
            VCLD=VCQD
            WCLD=WCQD
            XCLU=XCQU
            YCLU=YCQU
          GO TO 10158
10155     CONTINUE
            UCLD=UCTD
            VCLD=VCTD
            WCLD=WCTD
            XCLU=XCTU
            YCLU=YCTU
10158     CONTINUE
        GO TO 10149
10150   CONTINUE
        UCLD=UCTD
        VCLD=VCTD
        WCLD=WCTD
        XCLU=XCTU
        YCLU=YCTU
      GO TO (10122,10138,10148) , L10123
C
C The following internal procedure is given the data-system coordinates
C of a point (UCND,VCND,WCND) and computes the user-system coordinates
C of the point's projection (XCNU,YCNU).  It also sets a flag indicating
C whether the projection point is visible or not.
C
10027 CONTINUE
C
        IF (.NOT.(IMPF.EQ.0)) GO TO 10159
          XCNU=UCND
          YCNU=VCND
          IVNU=1
        GO TO 10160
10159   CONTINUE
          CALL HLUCTMXYZ (IMPF,UCND,VCND,WCND,XCNU,YCNU)
          IF (ICFELL('VTTPOM',5).NE.0) GO TO 104
          IF (.NOT.((OORV.NE.0.).AND.(XCNU.EQ.OORV.OR.YCNU.EQ.OORV)))
     +    GO TO 10161
            IVNU=0
          GO TO 10162
10161     CONTINUE
            IVNU=1
10162     CONTINUE
10160   CONTINUE
C
      GO TO (10026,10036,10090) , L10027
C
C The following internal procedure draws the part of the curve defined
C by the contents of the polyline buffer and then removes all but the
C last point from the buffer.
C
10108 CONTINUE
C
        I=1
C
10163   CONTINUE
          I=I+1
          IF (I.GT.NCPL) GO TO 10164
          IF (.NOT.(ABS(XCPL(I)-XCPL(I-1)).LT.EPSX.AND.ABS(YCPL(I)-YCPL(
     +I-1)).LT.EPSY)) GO TO 10165
            IF (.NOT.(I.NE.NCPL)) GO TO 10166
              DO 10167 J=I+1,NCPL
                XCPL(J-1)=XCPL(J)
                YCPL(J-1)=YCPL(J)
10167         CONTINUE
            GO TO 10168
10166       CONTINUE
              XCPL(NCPL-1)=XCPL(NCPL)
              YCPL(NCPL-1)=YCPL(NCPL)
10168       CONTINUE
            I=I-1
            NCPL=NCPL-1
10165     CONTINUE
        GO TO 10163
10164   CONTINUE
C
        IF (.NOT.(NCPL.GT.1)) GO TO 10169
          IF (.NOT.(IAMA(1).EQ.0)) GO TO 10170
            CALL CURVE (XCPL,YCPL,NCPL)
          GO TO 10171
10170     CONTINUE
            CALL ARDRLN (IAMA,XCPL,YCPL,NCPL,
     +                        XCPF,YCPF,MCPF,
     +                        IAAI,IAGI,MNOG,RTPL)
10171     CONTINUE
10169   CONTINUE
C
        XCPL(1)=XCPL(NCPL)
        YCPL(1)=YCPL(NCPL)
        NCPL=1
C
      GO TO (10107,10157) , L10108
C
C The following internal procedure draws the part of the curve defined
C by the contents of the polyline buffer and then clears the buffer.
C
10030 CONTINUE
C
        I=1
C
10172   CONTINUE
          I=I+1
          IF (I.GT.NCPL) GO TO 10173
          IF (.NOT.(ABS(XCPL(I)-XCPL(I-1)).LT.EPSX.AND.ABS(YCPL(I)-YCPL(
     +I-1)).LT.EPSY)) GO TO 10174
            IF (.NOT.(I.NE.NCPL)) GO TO 10175
              DO 10176 J=I+1,NCPL
                XCPL(J-1)=XCPL(J)
                YCPL(J-1)=YCPL(J)
10176         CONTINUE
            GO TO 10177
10175       CONTINUE
              XCPL(NCPL-1)=XCPL(NCPL)
              YCPL(NCPL-1)=YCPL(NCPL)
10177       CONTINUE
            I=I-1
            NCPL=NCPL-1
10174     CONTINUE
        GO TO 10172
10173   CONTINUE
C
        IF (.NOT.(NCPL.GT.1)) GO TO 10178
          IF (.NOT.(IAMA(1).EQ.0)) GO TO 10179
            CALL CURVE (XCPL,YCPL,NCPL)
          GO TO 10180
10179     CONTINUE
            CALL ARDRLN (IAMA,XCPL,YCPL,NCPL,
     +                        XCPF,YCPF,MCPF,
     +                        IAAI,IAGI,MNOG,RTPL)
10180     CONTINUE
10178   CONTINUE
C
        NCPL=0
        RUDN=0.
C
      GO TO (10029,10083,10116,10133,10139) , L10030
C
      END
