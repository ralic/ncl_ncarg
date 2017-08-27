      SUBROUTINE VTTSOM (IDRW,RPNT,IEDG,ITRI,ISTR,RSTR,SSTR,IDIR,
     +                   SLMX,ITER,SLTR,IEND,REND,SEND,IAMA,RTPL)
C
      DIMENSION RPNT(*),IEDG(*),ITRI(*),IAMA(*)
C
      EXTERNAL RTPL
C
C This routine, given arrays defining a triangular mesh, at each point
C of which a velocity vector is given, and the location of a particular
C starting point on a particular triangle of that mesh, traces and/or
C draws a streamline (a line that is everywhere parallel to the velocity
C vectors), continuing until one of a set of termination conditions is
C satisfied, and then returns the location of the final point of the
C line to the caller.
C
C IDRW determines the mode in which VTTSOM is to operate: If IDRW = 0,
C the streamline is traced and proximity termination tests are done,
C but the line is not actually drawn.  If IDRW is not 0, the line is
C traced and drawn; no proximity termination tests are done.  If IDRW
C = 1, no arrowheads are drawn; if IDRW = 2, only one arrowhead is
C drawn, at the end of the streamline; if IDRW = 3, arrowheads are
C drawn at positions along the streamline which are AHSR units apart.
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
C SLMX is the maximum length of streamline to be traced.
C
C ITER is a flag that is returned to say how the line terminated:
C
C   ITER=1 => exterior edge of mesh encountered.
C   ITER=2 => triangle crossed by seven or more streamlines entered.
C   ITER=3 => angle between velocity vectors exceeded maximum.
C   ITER=4 => line traced for specified distance.
C   ITER=5 => velocity along line dropped below VVMM.
C   ITER=6 => proximity termination test failure.
C   ITER=7 => other (e. g., a degenerate triangle).
C
C SLTR is returned and is the length of streamline traced before a
C termination condition was encountered.
C
C IEND, REND, and SEND are returned; IEND is the base index, in ITRI,
C of the triangle node of the triangle containing the end point of the
C line and REND and SEND are the coordinates of the end point within
C that triangle.
C
C IAMA is an array containing an area map against which the streamline
C is to be masked.  If masking is not desired, set IAMA(1) = 0.
C
C RTPL is a routine to be called to draw the streamline (when it is
C masked).
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
C Declare variables to hold coordinates of subtriangle vertices.
C
      DIMENSION UTRI(4),VTRI(4),WTRI(4)
C
C Declare a variable to be used to somewhat randomize the positions of
C arrowheads on the streamline.  Its value is computed during one call
C for use during that call and the next.
C
      SAVE AHPR
C
C Declare a character variable to hold the digits from 0 to 9.
C
      CHARACTER*10 IDIG
C
C Put the digits from 0 to 9 in a character variable.
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
C If a streamline is to be drawn, compute a quantity AHPR between 0 and
C 1 to be used to somewhat randomize the positions of arrowheads on it.
C Note that this code assumes pairs of calls, one to draw the part of
C the streamline toward its beginning and another to draw the part of
C the streamline toward its end.
C
      IF (.NOT.(IDRW.NE.0)) GO TO 10001
        IF (.NOT.(IDIR.EQ.0)) GO TO 10002
          AHPR=VTRAND()
        GO TO 10003
10002   CONTINUE
          AHPR=1.-AHPR
10003   CONTINUE
10001 CONTINUE
C
C If a streamline is to be drawn and either debugging output is turned
C on or the streamline is to be colored, save the initial polyline
C color and initialize variables that keep track of the current
C polyline color setting.
C
      IF (.NOT.(IDRW.NE.0.AND.(IDBG.NE.0.OR.(ICTV.NE.0.AND.NCLR.NE.0))))
     +GO TO 10004
        CALL GQPLCI (IGER,IPCS)
        IF (.NOT.(IGER.NE.0)) GO TO 10005
          CALL SETER ('VTTSOM - ERROR EXIT FROM GQPLCI',1,1)
          RETURN
10005   CONTINUE
        IPCC=IPCS
        IF (.NOT.(ICTV.NE.0.AND.NCLR.NE.0)) GO TO 10006
          IPCV=1
          ICVL=(NCLR+1)/2
        GO TO 10007
10006   CONTINUE
          IPCV=0
10007   CONTINUE
      GO TO 10008
10004 CONTINUE
        IPCS=-1
        IPCV=0
10008 CONTINUE
C
C Compute a test value, based on the maximum allowable angle, to be
C used below.
C
      IF (.NOT.(ANM2.NE.0.)) GO TO 10009
        CSMN=COS(DTOR*ANM2)
        CSMN=CSMN*ABS(CSMN)
10009 CONTINUE
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
C indices of the nodes defining its vertices.
C
      IIII=ISTR
C
C Find the base indices of point 1 (that edges 1 and 2 have in common),
C point 2 (that edges 2 and 3 have in common), and point 3 (that edges
C 3 and 1 have in common).
C
      IF (.NOT.(IEDG(ITRI(IIII+2)+1).NE.IEDG(ITRI(IIII+3)+1).AND.IEDG(IT
     +RI(IIII+2)+1).NE.IEDG(ITRI(IIII+3)+2))) GO TO 10010
        IPP1=IEDG(ITRI(IIII+2)+1)
        IPP2=IEDG(ITRI(IIII+2)+2)
      GO TO 10011
10010 CONTINUE
        IPP1=IEDG(ITRI(IIII+2)+2)
        IPP2=IEDG(ITRI(IIII+2)+1)
10011 CONTINUE
C
      IF (.NOT.(IEDG(ITRI(IIII+1)+1).NE.IPP1)) GO TO 10012
        IPP3=IEDG(ITRI(IIII+1)+1)
      GO TO 10013
10012 CONTINUE
        IPP3=IEDG(ITRI(IIII+1)+2)
10013 CONTINUE
C
C Initialize the starting point values.
C
      RVAL=RSTR
      SVAL=SSTR
C
C SLTR keeps track of the length of line traced already, SLAR the length
C of line to be drawn before adding the next arrowhead, and SLTT the
C length of line to be drawn before performing the next proximity tests
C for termination of the streamline.
C
      SLTR=0.
C
      IF (.NOT.(IDRW.EQ.0)) GO TO 10014
        SLAR=2.*SLMX
        SLTT=TTSR
      GO TO 10015
10014 CONTINUE
      IF (.NOT.(IDRW.EQ.1)) GO TO 10016
        SLAR=2.*SLMX
        SLTT=2.*SLMX
      GO TO 10015
10016 CONTINUE
      IF (.NOT.(IDRW.EQ.2)) GO TO 10017
        IF (.NOT.(IDIR.EQ.0)) GO TO 10018
          SLAR=0.
        GO TO 10019
10018   CONTINUE
          SLAR=SLMX
10019   CONTINUE
        SLTT=2.*SLMX
      GO TO 10015
10017 CONTINUE
        SLAR=AHSR*AHPR
        SLTT=2.*SLMX
10015 CONTINUE
C
C IPRJ is non-zero if and only if the streamline is to be projected
C from the triangular mesh to the image plane, either because the line
C is to be drawn there or because testing needs to be done there.
C
      IF (.NOT.(IDRW.EQ.0)) GO TO 10020
        IPRJ=0
      GO TO 10021
10020 CONTINUE
        IPRJ=1
10021 CONTINUE
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
C IDST is non-zero if and only if subtriangles are to be drawn for
C debugging purposes and the triangle is not blocked.
C
      IF (.NOT.(IDRW.EQ.0.OR.IDBG.EQ.0.OR.ICTB.NE.0)) GO TO 10022
        IDST=0
      GO TO 10023
10022 CONTINUE
        IDST=1
10023 CONTINUE
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
      IF (.NOT.(IDIR.EQ.0)) GO TO 10024
        UVP1=-RPNT(IPP1+4)
        VVP1=-RPNT(IPP1+5)
        WVP1=-RPNT(IPP1+6)
        UVP2=-RPNT(IPP2+4)
        VVP2=-RPNT(IPP2+5)
        WVP2=-RPNT(IPP2+6)
        UVP3=-RPNT(IPP3+4)
        VVP3=-RPNT(IPP3+5)
        WVP3=-RPNT(IPP3+6)
      GO TO 10025
10024 CONTINUE
        UVP1=+RPNT(IPP1+4)
        VVP1=+RPNT(IPP1+5)
        WVP1=+RPNT(IPP1+6)
        UVP2=+RPNT(IPP2+4)
        VVP2=+RPNT(IPP2+5)
        WVP2=+RPNT(IPP2+6)
        UVP3=+RPNT(IPP3+4)
        VVP3=+RPNT(IPP3+5)
        WVP3=+RPNT(IPP3+6)
10025 CONTINUE
C
      VMG1=SQRT(UVP1**2+VVP1**2+WVP1**2)
C
      IF (.NOT.(VMG1.NE.0.)) GO TO 10026
        UVP1=.001*EMAX*UVP1/VMG1
        VVP1=.001*EMAX*VVP1/VMG1
        WVP1=.001*EMAX*WVP1/VMG1
10026 CONTINUE
C
      VMG2=SQRT(UVP2**2+VVP2**2+WVP2**2)
C
      IF (.NOT.(VMG2.NE.0.)) GO TO 10027
        UVP2=.001*EMAX*UVP2/VMG2
        VVP2=.001*EMAX*VVP2/VMG2
        WVP2=.001*EMAX*WVP2/VMG2
10027 CONTINUE
C
      VMG3=SQRT(UVP3**2+VVP3**2+WVP3**2)
C
      IF (.NOT.(VMG3.NE.0.)) GO TO 10028
        UVP3=.001*EMAX*UVP3/VMG3
        VVP3=.001*EMAX*VVP3/VMG3
        WVP3=.001*EMAX*WVP3/VMG3
10028 CONTINUE
C
      IF (.NOT.(ICTV.EQ.0)) GO TO 10029
        CVP1=0.
        CVP2=0.
        CVP3=0.
      GO TO 10030
10029 CONTINUE
      IF (.NOT.(ABS(ICTV).LE.LOPN)) GO TO 10031
        CVP1=RPNT(IPP1+ABS(ICTV))
        CVP2=RPNT(IPP2+ABS(ICTV))
        CVP3=RPNT(IPP3+ABS(ICTV))
      GO TO 10030
10031 CONTINUE
        CVP1=SQRT(RPNT(IPP1+4)**2+RPNT(IPP1+5)**2+RPNT(IPP1+6)**2)
        CVP2=SQRT(RPNT(IPP2+4)**2+RPNT(IPP2+5)**2+RPNT(IPP2+6)**2)
        CVP3=SQRT(RPNT(IPP3+4)**2+RPNT(IPP3+5)**2+RPNT(IPP3+6)**2)
10030 CONTINUE
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
      IF (.NOT.(DNOM.NE.0.)) GO TO 10032
 
        DCNU=A/DNOM
        DCNV=B/DNOM
        DCNW=C/DNOM
C
      GO TO 10033
10032 CONTINUE
C
        ITER=7
        GO TO 106
C
10033 CONTINUE
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
     +GO TO 10034
        RVV1=(      UPP1 *(VCP3-VCP1)-      VPP1 *(UCP3-UCP1))/DNUV
        SVV1=((UCP2-UCP1)*      VPP1 -(VCP2-VCP1)*      UPP1 )/DNUV
        RVV2=(      UPP2 *(VCP3-VCP1)-      VPP2 *(UCP3-UCP1))/DNUV
        SVV2=((UCP2-UCP1)*      VPP2 -(VCP2-VCP1)*      UPP2 )/DNUV
        RVV3=(      UPP3 *(VCP3-VCP1)-      VPP3 *(UCP3-UCP1))/DNUV
        SVV3=((UCP2-UCP1)*      VPP3 -(VCP2-VCP1)*      UPP3 )/DNUV
      GO TO 10035
10034 CONTINUE
      IF (.NOT.(ABS(DNVW).GT.ABS(DNWU).AND.ABS(DNVW).GT.ABS(DNUV)))
     +GO TO 10036
        RVV1=(      VPP1 *(WCP3-WCP1)-      WPP1 *(VCP3-VCP1))/DNVW
        SVV1=((VCP2-VCP1)*      WPP1 -(WCP2-WCP1)*      VPP1 )/DNVW
        RVV2=(      VPP2 *(WCP3-WCP1)-      WPP2 *(VCP3-VCP1))/DNVW
        SVV2=((VCP2-VCP1)*      WPP2 -(WCP2-WCP1)*      VPP2 )/DNVW
        RVV3=(      VPP3 *(WCP3-WCP1)-      WPP3 *(VCP3-VCP1))/DNVW
        SVV3=((VCP2-VCP1)*      WPP3 -(WCP2-WCP1)*      VPP3 )/DNVW
      GO TO 10035
10036 CONTINUE
        RVV1=(      WPP1 *(UCP3-UCP1)-      UPP1 *(WCP3-WCP1))/DNWU
        SVV1=((WCP2-WCP1)*      UPP1 -(UCP2-UCP1)*      WPP1 )/DNWU
        RVV2=(      WPP2 *(UCP3-UCP1)-      UPP2 *(WCP3-WCP1))/DNWU
        SVV2=((WCP2-WCP1)*      UPP2 -(UCP2-UCP1)*      WPP2 )/DNWU
        RVV3=(      WPP3 *(UCP3-UCP1)-      UPP3 *(WCP3-WCP1))/DNWU
        SVV3=((WCP2-WCP1)*      UPP3 -(UCP2-UCP1)*      WPP3 )/DNWU
10035 CONTINUE
C
C See which subtriangle the line starts in.
C
      INDR=MAX(0,MIN(4,INT(5.*RVAL)))
      INDS=MAX(0,MIN(4,INT(5.*SVAL)))
      INDT=MAX(0,MIN(4,INT(5.*(RVAL+SVAL))))
      IOSB=2*(5*INDR+INDS)-INDR*INDR+MOD(INDR+INDS+INDT,2)
C
C If the streamline is being projected ...
C
      IF (.NOT.(IPRJ.NE.0)) GO TO 10037
C
C ... and the last triangle was blocked (or non-existent) and this one
C is not blocked, compute values associated with the first point of a
C new segment of streamline.  (If both triangles are unblocked, values
C computed would be identical to what we already had, but for round-off
C differences that could cause problems.)
C
        IF (.NOT.(ILTB.NE.0.AND.ICTB.EQ.0)) GO TO 10038
          UCND=UCP1+RVAL*(UCP2-UCP1)+SVAL*(UCP3-UCP1)
          VCND=VCP1+RVAL*(VCP2-VCP1)+SVAL*(VCP3-VCP1)
          WCND=WCP1+RVAL*(WCP2-WCP1)+SVAL*(WCP3-WCP1)
          CCND=CVP1+RVAL*(CVP2-CVP1)+SVAL*(CVP3-CVP1)
          L10040=    1
          GO TO 10040
10039     CONTINUE
10038   CONTINUE
C
C ... and, if the new triangle is blocked and there's something in the
C polyline buffer, clear the buffer.
C
        IF (.NOT.(ICTB.NE.0.AND.NCPL.NE.0)) GO TO 10041
          L10043=    1
          GO TO 10043
10042     CONTINUE
10041   CONTINUE
C
10037 CONTINUE
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
  102 SLNE=MIN(SLMX-SLTR,SLAR,SLTT)
      SLNS=SLNE
C
C Jump if the next event is to happen immediately.  (This can only
C happen if the event is drawing an arrowhead.)
C
      IF (SLNE.EQ.0.) GO TO 105
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
      IF (.NOT.(VMAG.GT.VVMM)) GO TO 10044
C
        TEMP=MIN(SLNE,SLPR)/VMAG
C
C Check for exit through the side joining V1 and V3.
C
        IF (.NOT.(RVAL+TEMP*RVVN.LT.0..AND.(RVAL.GT..001.OR.ABS(RVVN).GT
     +..001*ABS(RVVN+2.*SVVN)))) GO TO 10045
          JUMP=1
          TEMP=-RVAL/RVVN
10045   CONTINUE
C
C Check for exit through the side joining V1 and V2.
C
        IF (.NOT.(SVAL+TEMP*SVVN.LT.0..AND.(SVAL.GT..001.OR.ABS(SVVN).GT
     +..001*ABS(2.*RVVN+SVVN)))) GO TO 10046
          JUMP=2
          TEMP=-SVAL/SVVN
10046   CONTINUE
C
C Check for exit through the side joining V2 and V3.
C
        IF (.NOT.(1.-RVAL-SVAL-TEMP*(RVVN+SVVN).LT.0..AND.(1.-RVAL-SVAL.
     +GT..001.OR.ABS(RVVN+SVVN).GT..001*ABS(RVVN-SVVN)))) GO TO 10047
          JUMP=3
          TEMP=(1.-RVAL-SVAL)/(RVVN+SVVN)
10047   CONTINUE
C
        RVAL=MAX(0.,MIN(1.,RVAL+TEMP*RVVN))
        SVAL=MAX(0.,MIN(1.,SVAL+TEMP*SVVN))
C
C If the streamline is being projected and the current triangle is not
C blocked, save information about the previous point, generate values
C for the new point, and process the line segment joining them.
C
        IF (.NOT.(IPRJ.NE.0.AND.ICTB.EQ.0)) GO TO 10048
          UCOD=UCND
          VCOD=VCND
          WCOD=WCND
          CCOD=CCND
          XCOU=XCNU
          YCOU=YCNU
          IVOU=IVNU
          UCND=UCP1+RVAL*(UCP2-UCP1)+SVAL*(UCP3-UCP1)
          VCND=VCP1+RVAL*(VCP2-VCP1)+SVAL*(VCP3-VCP1)
          WCND=WCP1+RVAL*(WCP2-WCP1)+SVAL*(WCP3-WCP1)
          CCND=CVP1+RVAL*(CVP2-CVP1)+SVAL*(CVP3-CVP1)
          L10040=    2
          GO TO 10040
10049     CONTINUE
          L10051=    1
          GO TO 10051
10050     CONTINUE
10048   CONTINUE
C
C Reduce the streamline length to be traced before the next event.
C
        SLNE=SLNE-VMAG*TEMP
C
C If the line is now of the desired length at the next event, terminate
C it.
C
        IF (.NOT.(SLNE.LT..01*SLPR)) GO TO 10052
          SLNE=0.
          JUMP=4
10052   CONTINUE
C
C If the streamline is actually being drawn, see which subtriangle the
C line is entering and update the appropriate mask bits.
C
        IF (.NOT.(IDRW.NE.0)) GO TO 10053
          IOSL=IOSB
          INDR=MAX(0,MIN(4,INT(5.*RVAL)))
          INDS=MAX(0,MIN(4,INT(5.*SVAL)))
          INDT=MAX(0,MIN(4,INT(5.*(RVAL+SVAL))))
          IOSB=2*(5*INDR+INDS)-INDR*INDR+MOD(INDR+INDS+INDT,2)
          IF (.NOT.(IDST.NE.0)) GO TO 10054
            ISTO=ISHIFT(ITRI(IIII+5),-3)
            ISTN=ISTA(25*IOSL+IOSB+1)
            DO 104 I=0,24
            IF (IAND(ISTO,ISHIFT(1,I)).NE.0) GO TO 104
            IF (IAND(ISTN,ISHIFT(1,I)).EQ.0) GO TO 104
            IF (.NOT.(I.LE.8)) GO TO 10055
              INDR=0
              INDS=I/2
              INDT=(I+1)/2
            GO TO 10056
10055       CONTINUE
            IF (.NOT.(I.LE.15)) GO TO 10057
              INDR=1
              INDS=(I-9)/2
              INDT=(I-6)/2
            GO TO 10056
10057       CONTINUE
            IF (.NOT.(I.LE.20)) GO TO 10058
              INDR=2
              INDS=(I-16)/2
              INDT=(I-11)/2
            GO TO 10056
10058       CONTINUE
            IF (.NOT.(I.LE.23)) GO TO 10059
              INDR=3
              INDS=(I-21)/2
              INDT=(I-14)/2
            GO TO 10056
10059       CONTINUE
              INDR=4
              INDS=0
              INDT=4
10056       CONTINUE
            IF (.NOT.(MOD(INDR+INDS+INDT,2).EQ.0)) GO TO 10060
              RVL1=REAL(INDR  )/5.
              RVL2=REAL(INDR+1)/5.
              RVL3=REAL(INDR  )/5.
              SVL1=REAL(INDS  )/5.
              SVL2=REAL(INDS  )/5.
              SVL3=REAL(INDS+1)/5.
            GO TO 10061
10060       CONTINUE
              RVL1=REAL(INDR+1)/5.
              RVL2=REAL(INDR+1)/5.
              RVL3=REAL(INDR  )/5.
              SVL1=REAL(INDS  )/5.
              SVL2=REAL(INDS+1)/5.
              SVL3=REAL(INDS+1)/5.
10061       CONTINUE
            UTRI(1)=UCP1+RVL1*(UCP2-UCP1)+SVL1*(UCP3-UCP1)
            VTRI(1)=VCP1+RVL1*(VCP2-VCP1)+SVL1*(VCP3-VCP1)
            WTRI(1)=WCP1+RVL1*(WCP2-WCP1)+SVL1*(WCP3-WCP1)
            UTRI(2)=UCP1+RVL2*(UCP2-UCP1)+SVL2*(UCP3-UCP1)
            VTRI(2)=VCP1+RVL2*(VCP2-VCP1)+SVL2*(VCP3-VCP1)
            WTRI(2)=WCP1+RVL2*(WCP2-WCP1)+SVL2*(WCP3-WCP1)
            UTRI(3)=UCP1+RVL3*(UCP2-UCP1)+SVL3*(UCP3-UCP1)
            VTRI(3)=VCP1+RVL3*(VCP2-VCP1)+SVL3*(VCP3-VCP1)
            WTRI(3)=WCP1+RVL3*(WCP2-WCP1)+SVL3*(WCP3-WCP1)
            UTRI(4)=UTRI(1)
            VTRI(4)=VTRI(1)
            WTRI(4)=WTRI(1)
            IF (.NOT.(IPCC.NE.ICST)) GO TO 10062
              CALL GSPLCI (ICST)
10062       CONTINUE
            CALL VTCUDR (UTRI,VTRI,WTRI,UTRI,4,0,0,IAMA,RTPL)
            IF (.NOT.(IPCC.NE.ICST)) GO TO 10063
              CALL GSPLCI (IPCC)
10063       CONTINUE
  104       CONTINUE
10054     CONTINUE
          ITRI(IIII+5)=IOR(ITRI(IIII+5),
     +                     ISHIFT(ISTA(25*IOSL+IOSB+1),3))
10053   CONTINUE
C
      GO TO 10064
10044 CONTINUE
C
C The velocity is too low.
C
        JUMP=5
C
10064 CONTINUE
C
      IF (JUMP.EQ.0) GO TO 103
C
      IF (JUMP.EQ.4) JUMP=0
C
C Update various line-length quantities (line length in buffer, line
C length traced already, line length to next arrowhead, and line length
C to next termination test).
C
  105 SLIB=SLNS-SLNE
      SLTR=SLTR+SLIB
      SLAR=SLAR-SLIB
      SLTT=SLTT-SLIB
C
C If we're at a point where an arrowhead is to be drawn, do it.
C
      IF (.NOT.(SLAR.LT..01*SLPR)) GO TO 10065
        IF (.NOT.(ICTB.EQ.0)) GO TO 10066
          CALL VTTLOM (RPNT,IEDG,ITRI,IIII,RVAL,SVAL,180.-AHAW/2.,
     +                 AHLR,ISTP,SLT2,IAMA,RTPL)
          CALL VTTLOM (RPNT,IEDG,ITRI,IIII,RVAL,SVAL,AHAW/2.-180.,
     +                 AHLR,ISTP,SLT2,IAMA,RTPL)
10066   CONTINUE
        SLAR=SLAR+AHSR
10065 CONTINUE
C
C If we're at a point where a termination test is to be done, do it.
C
      IF (.NOT.(SLTT.LT..01*SLPR)) GO TO 10067
        CALL VTTPOM (RPNT,IEDG,ITRI,IIII,RVAL,SVAL,
     +               0,ICTT,TTLR,ISTP,SLT2,IDUM,RDUM,SDUM,IAMA,RTPL)
        IF (.NOT.(SLT2.LT..99*TTLR)) GO TO 10068
          ITER=6
          GO TO 106
10068   CONTINUE
        CALL VTTPOM (RPNT,IEDG,ITRI,IIII,RVAL,SVAL,
     +               1,ICTT,TTLR,ISTP,SLT2,IDUM,RDUM,SDUM,IAMA,RTPL)
        IF (.NOT.(SLT2.LT..99*TTLR)) GO TO 10069
          ITER=6
          GO TO 106
10069   CONTINUE
        SLTT=SLTT+TTSR
10067 CONTINUE
C
C If the length of the line has hit the maximum, terminate it.
C
      IF (.NOT.(SLTR.GE.SLMX)) GO TO 10070
        ITER=4
        GO TO 106
10070 CONTINUE
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
      IF (.NOT.(JUMP.EQ.1)) GO TO 10071
C
        IF (.NOT.(IEDG(ITRI(IIII+1)+1).EQ.IPP1.AND.IEDG(ITRI(IIII+1)+2).
     +EQ.IPP3)) GO TO 10072
          INEW=IEDG(ITRI(IIII+1)+3)
        GO TO 10073
10072   CONTINUE
        IF (.NOT.(IEDG(ITRI(IIII+1)+1).EQ.IPP3.AND.IEDG(ITRI(IIII+1)+2).
     +EQ.IPP1)) GO TO 10074
          INEW=IEDG(ITRI(IIII+1)+4)
        GO TO 10073
10074   CONTINUE
          INEW=-1
10073   CONTINUE
        IF (.NOT.(INEW.LT.0)) GO TO 10075
          ITER=1
          GO TO 106
10075   CONTINUE
        IIII=LOTN*((INEW-1)/LOTN)
        IF (.NOT.(IAND(ITRI(IIII+5),7).EQ.7)) GO TO 10076
          ITER=2
          GO TO 106
10076   CONTINUE
        IF (IDRW.NE.0) ITRI(IIII+5)=ITRI(IIII+5)+1
        IPP2=IPP3
        IF (.NOT.(IEDG(ITRI(IIII+1)+1).NE.IPP1.AND.IEDG(ITRI(IIII+1)+1).
     +NE.IPP2)) GO TO 10077
          IPP3=IEDG(ITRI(IIII+1)+1)
        GO TO 10078
10077   CONTINUE
        IF (.NOT.(IEDG(ITRI(IIII+1)+2).NE.IPP1.AND.IEDG(ITRI(IIII+1)+2).
     +NE.IPP2)) GO TO 10079
          IPP3=IEDG(ITRI(IIII+1)+2)
        GO TO 10078
10079   CONTINUE
        IF (.NOT.(IEDG(ITRI(IIII+2)+1).NE.IPP1.AND.IEDG(ITRI(IIII+2)+1).
     +NE.IPP2)) GO TO 10080
          IPP3=IEDG(ITRI(IIII+2)+1)
        GO TO 10078
10080   CONTINUE
          IPP3=IEDG(ITRI(IIII+2)+2)
10078   CONTINUE
        RVAL=SVAL
        SVAL=0.
C
C If the line terminated because SVAL became zero, move to the triangle,
C if any, that lies on the other side of edge 2 (joining vertices 1 and
C 2 of the triangle).
C
      GO TO 10081
10071 CONTINUE
      IF (.NOT.(JUMP.EQ.2)) GO TO 10082
C
        IF (.NOT.(IEDG(ITRI(IIII+2)+1).EQ.IPP1.AND.IEDG(ITRI(IIII+2)+2).
     +EQ.IPP2)) GO TO 10083
          INEW=IEDG(ITRI(IIII+2)+4)
        GO TO 10084
10083   CONTINUE
        IF (.NOT.(IEDG(ITRI(IIII+2)+1).EQ.IPP2.AND.IEDG(ITRI(IIII+2)+2).
     +EQ.IPP1)) GO TO 10085
          INEW=IEDG(ITRI(IIII+2)+3)
        GO TO 10084
10085   CONTINUE
          INEW=-1
10084   CONTINUE
        IF (.NOT.(INEW.LT.0)) GO TO 10086
          ITER=1
          GO TO 106
10086   CONTINUE
        IIII=LOTN*((INEW-1)/LOTN)
        IF (.NOT.(IAND(ITRI(IIII+5),7).EQ.7)) GO TO 10087
          ITER=2
          GO TO 106
10087   CONTINUE
        IF (IDRW.NE.0) ITRI(IIII+5)=ITRI(IIII+5)+1
        IPP3=IPP2
        IF (.NOT.(IEDG(ITRI(IIII+1)+1).NE.IPP1.AND.IEDG(ITRI(IIII+1)+1).
     +NE.IPP3)) GO TO 10088
          IPP2=IEDG(ITRI(IIII+1)+1)
        GO TO 10089
10088   CONTINUE
        IF (.NOT.(IEDG(ITRI(IIII+1)+2).NE.IPP1.AND.IEDG(ITRI(IIII+1)+2).
     +NE.IPP3)) GO TO 10090
          IPP2=IEDG(ITRI(IIII+1)+2)
        GO TO 10089
10090   CONTINUE
        IF (.NOT.(IEDG(ITRI(IIII+2)+1).NE.IPP1.AND.IEDG(ITRI(IIII+2)+1).
     +NE.IPP3)) GO TO 10091
          IPP2=IEDG(ITRI(IIII+2)+1)
        GO TO 10089
10091   CONTINUE
          IPP2=IEDG(ITRI(IIII+2)+2)
10089   CONTINUE
        SVAL=RVAL
        RVAL=0.
C
C If the line terminated because RVAL+SVAL became equal to one, move to
C the triangle, if any, that lies on the other side of edge 3 (joining
C vertices 2 and 3 of the triangle).
C
      GO TO 10081
10082 CONTINUE
      IF (.NOT.(JUMP.EQ.3)) GO TO 10092
C
        IF (.NOT.(IEDG(ITRI(IIII+3)+1).EQ.IPP2.AND.IEDG(ITRI(IIII+3)+2).
     +EQ.IPP3)) GO TO 10093
          INEW=IEDG(ITRI(IIII+3)+4)
        GO TO 10094
10093   CONTINUE
        IF (.NOT.(IEDG(ITRI(IIII+3)+1).EQ.IPP3.AND.IEDG(ITRI(IIII+3)+2).
     +EQ.IPP2)) GO TO 10095
          INEW=IEDG(ITRI(IIII+3)+3)
        GO TO 10094
10095   CONTINUE
          INEW=-1
10094   CONTINUE
        IF (.NOT.(INEW.LT.0)) GO TO 10096
          ITER=1
          GO TO 106
10096   CONTINUE
        IIII=LOTN*((INEW-1)/LOTN)
        IF (.NOT.(IAND(ITRI(IIII+5),7).EQ.7)) GO TO 10097
          ITER=2
          GO TO 106
10097   CONTINUE
        IF (IDRW.NE.0) ITRI(IIII+5)=ITRI(IIII+5)+1
        IPP1=IPP2
        IF (.NOT.(IEDG(ITRI(IIII+1)+1).NE.IPP1.AND.IEDG(ITRI(IIII+1)+1).
     +NE.IPP3)) GO TO 10098
          IPP2=IEDG(ITRI(IIII+1)+1)
        GO TO 10099
10098   CONTINUE
        IF (.NOT.(IEDG(ITRI(IIII+1)+2).NE.IPP1.AND.IEDG(ITRI(IIII+1)+2).
     +NE.IPP3)) GO TO 10100
          IPP2=IEDG(ITRI(IIII+1)+2)
        GO TO 10099
10100   CONTINUE
        IF (.NOT.(IEDG(ITRI(IIII+2)+1).NE.IPP1.AND.IEDG(ITRI(IIII+2)+1).
     +NE.IPP3)) GO TO 10101
          IPP2=IEDG(ITRI(IIII+2)+1)
        GO TO 10099
10101   CONTINUE
          IPP2=IEDG(ITRI(IIII+2)+2)
10099   CONTINUE
        SVAL=1.-RVAL
        RVAL=0.
C
10081 CONTINUE
10092 CONTINUE
C
C If we just moved into a new triangle, we may need to recompute the
C values of the pointers to its vertices and of the coordinates of the
C point in the triangle.
C
      IF (.NOT.(JUMP.LT.4)) GO TO 10102
C
C Get a pointer to what should be point 1 of the triangle.  It will
C match one of the pointers we already have.
C
        IF (.NOT.(IEDG(ITRI(IIII+1)+1).EQ.IEDG(ITRI(IIII+2)+1).OR.IEDG(I
     +TRI(IIII+1)+1).EQ.IEDG(ITRI(IIII+2)+2))) GO TO 10103
          IPPT=IEDG(ITRI(IIII+1)+1)
        GO TO 10104
10103   CONTINUE
          IPPT=IEDG(ITRI(IIII+1)+2)
10104   CONTINUE
C
C Adjust the pointers and the values of RVAL and SVAL appropriately.
C
        IF (.NOT.(IPPT.NE.IPP1)) GO TO 10105
          RTMP=RVAL
          STMP=SVAL
          IF (.NOT.(IPPT.EQ.IPP2)) GO TO 10106
            IPP2=IPP3
            IPP3=IPP1
            IPP1=IPPT
            RVAL=STMP
            SVAL=1.-RTMP-STMP
          GO TO 10107
10106     CONTINUE
            IPP3=IPP2
            IPP2=IPP1
            IPP1=IPPT
            RVAL=1.-RTMP-STMP
            SVAL=RTMP
10107     CONTINUE
10105   CONTINUE
C
C Also, if we have been directed to examine the angles between the
C velocity vectors at its vertices, do that.
C
        IF (.NOT.(ANM2.NE.0.)) GO TO 10108
C
C Compute the squares of the cosines of the angles between the velocity
C vectors at pairs of vertices of the triangle.
C
          DNM1=(RPNT(IPP1+4)**2+RPNT(IPP1+5)**2+RPNT(IPP1+6)**2)*
     +         (RPNT(IPP2+4)**2+RPNT(IPP2+5)**2+RPNT(IPP2+6)**2)
          IF (.NOT.(DNM1.EQ.0.)) GO TO 10109
            ITER=7
            GO TO 106
10109     CONTINUE
          CSA1=(RPNT(IPP1+4)*RPNT(IPP2+4)+
     +          RPNT(IPP1+5)*RPNT(IPP2+5)+
     +          RPNT(IPP1+6)*RPNT(IPP2+6))
          CSA1=CSA1*ABS(CSA1)/DNM1
C
          DNM2=(RPNT(IPP2+4)**2+RPNT(IPP2+5)**2+RPNT(IPP2+6)**2)*
     +         (RPNT(IPP3+4)**2+RPNT(IPP3+5)**2+RPNT(IPP3+6)**2)
          IF (.NOT.(DNM2.EQ.0.)) GO TO 10110
            ITER=7
            GO TO 106
10110     CONTINUE
          CSA2=(RPNT(IPP2+4)*RPNT(IPP3+4)+
     +          RPNT(IPP2+5)*RPNT(IPP3+5)+
     +          RPNT(IPP2+6)*RPNT(IPP3+6))
          CSA2=CSA2*ABS(CSA2)/DNM2
C
          DNM3=(RPNT(IPP3+4)**2+RPNT(IPP3+5)**2+RPNT(IPP3+6)**2)*
     +         (RPNT(IPP1+4)**2+RPNT(IPP1+5)**2+RPNT(IPP1+6)**2)
          IF (.NOT.(DNM3.EQ.0.)) GO TO 10111
            ITER=7
            GO TO 106
10111     CONTINUE
          CSA3=(RPNT(IPP3+4)*RPNT(IPP1+4)+
     +          RPNT(IPP3+5)*RPNT(IPP1+5)+
     +          RPNT(IPP3+6)*RPNT(IPP1+6))
          CSA3=CSA3*ABS(CSA3)/DNM3
C
          IF (.NOT.(MIN(CSA1,CSA2,CSA3).LE.CSMN)) GO TO 10112
            ITER=3
            GO TO 106
10112     CONTINUE
C
10108   CONTINUE
C
C Jump back to continue tracing the streamline in the new triangle.
C
        GO TO 101
C
C Otherwise, ...
C
10102 CONTINUE
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
  106 CONTINUE
      IF (.NOT.(NCPL.NE.0)) GO TO 10113
        L10043=    2
        GO TO 10043
10114   CONTINUE
10113 CONTINUE
C
C If the polyline color index was saved above, restore it.
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
      IF (.NOT.(IDBG.NE.0.AND.IDRW.NE.0.AND.ICTB.EQ.0)) GO TO 10115
        CALL HLUVTMXYZ (IMPF,UCND,VCND,WCND,XPOS,YPOS)
        IF (.NOT.(XPOS.NE.OORV)) GO TO 10116
          CALL GQFACI (IGER,ISFC)
          CALL GSFACI (0)
          CALL VTDREL (CUFX(XPOS),CUFY(YPOS),.0004,.0004,0.,10.,1)
          CALL GSFACI (ISFC)
          CALL PLCHHQ (XPOS,YPOS,IDIG(ITER+1:ITER+1),.0004,0.,0.)
10116   CONTINUE
10115 CONTINUE
C
C Done.
C
      RETURN
C
C The following internal procedure, given a line segment, adds visible
C portions of it to the coordinate arrays.
C
10051 CONTINUE
C
C If point interpolation is turned on, do the first IPIS segments.
C
        IF (.NOT.(IPIS.NE.0)) GO TO 10117
          USOD=UCOD
          VSOD=VCOD
          WSOD=WCOD
          CSOD=CCOD
          USND=UCND
          VSND=VCND
          WSND=WCND
          CSND=CCND
          XSNU=XCNU
          YSNU=YCNU
          ISNU=IVNU
            INTP = 1
            GO TO 10120
10118       CONTINUE
            INTP =INTP +1
10120       CONTINUE
            IF (INTP .GT.(ABS(IPIS))) GO TO 10119
            UCND=USOD+(REAL(INTP)/REAL(ABS(IPIS)+1))*(USND-USOD)
            VCND=VSOD+(REAL(INTP)/REAL(ABS(IPIS)+1))*(VSND-VSOD)
            WCND=WSOD+(REAL(INTP)/REAL(ABS(IPIS)+1))*(WSND-WSOD)
            CCND=CSOD+(REAL(INTP)/REAL(ABS(IPIS)+1))*(CSND-CSOD)
            L10040=    3
            GO TO 10040
10121       CONTINUE
            IF (.NOT.(IPIS.GT.0.OR.IVNU.NE.IVOU)) GO TO 10122
              L10124=    1
              GO TO 10124
10123         CONTINUE
              UCOD=UCND
              VCOD=VCND
              WCOD=WCND
              CCOD=CCND
              XCOU=XCNU
              YCOU=YCNU
              IVOU=IVNU
10122       CONTINUE
          GO TO 10118
10119     CONTINUE
          UCND=USND
          VCND=VSND
          WCND=WSND
          CCND=CSND
          XCNU=XSNU
          YCNU=YSNU
          IVNU=ISNU
10117   CONTINUE
C
C Finish off the job.
C
        L10124=    2
        GO TO 10124
10125   CONTINUE
C
      GO TO (10050) , L10051
C
C The following internal procedure examines the points (UCOD,VCOD,WCOD),
C which projects into (XCOU,YCOU), and (UCND,VCND,WCND), which projects
C into (XCNU,YCNU), either of which may be visible or invisible in the
C projection space, and adds visible portions of the line segment
C between them to the polyline being built.
C
10124 CONTINUE
C
        IF (.NOT.(UCND.NE.UCOD.OR.VCND.NE.VCOD.OR.WCND.NE.WCOD))
     +  GO TO 10126
C
          IF (.NOT.(NCPL.EQ.0)) GO TO 10127
            IF (.NOT.(IVOU.NE.0)) GO TO 10128
              IF (.NOT.(IMPF.NE.0.AND.PITH.GT.0.)) GO TO 10129
                UCLD=UCOD
                VCLD=VCOD
                WCLD=WCOD
                CCLD=CCOD
                XCLU=XCOU
                YCLU=YCOU
10129         CONTINUE
              NCPL=1
              XCPL(1)=XCOU
              YCPL(1)=YCOU
              IF (.NOT.(IPCV.NE.0)) GO TO 10130
                CVAL=CCOD
                L10132=    1
                GO TO 10132
10131           CONTINUE
                IPCD=IPCI
10130         CONTINUE
            GO TO 10133
10128       CONTINUE
            IF (.NOT.(IVNU.NE.0)) GO TO 10134
              UCID=UCOD
              VCID=VCOD
              WCID=WCOD
              CCID=CCOD
              UCVD=UCND
              VCVD=VCND
              WCVD=WCND
              CCVD=CCND
              XCVU=XCNU
              YCVU=YCNU
              L10136=    1
              GO TO 10136
10135         CONTINUE
              L10138=    1
              GO TO 10138
10137         CONTINUE
              UCOD=UCVD
              VCOD=VCVD
              WCOD=WCVD
              CCOD=CCVD
              XCOU=XCVU
              YCOU=YCVU
              IVOU=1
10133       CONTINUE
10134       CONTINUE
          GO TO 10139
10127     CONTINUE
          IF (.NOT.(NCPL.EQ.MCPL)) GO TO 10140
            L10142=    1
            GO TO 10142
10141       CONTINUE
10139     CONTINUE
10140     CONTINUE
C
          IF (.NOT.(IVNU.NE.0)) GO TO 10143
            L10145=    1
            GO TO 10145
10144       CONTINUE
          GO TO 10146
10143     CONTINUE
          IF (.NOT.(IVOU.NE.0)) GO TO 10147
            UCVD=UCOD
            VCVD=VCOD
            WCVD=WCOD
            CCVD=CCOD
            XCVU=XCOU
            YCVU=YCOU
            UCID=UCND
            VCID=VCND
            WCID=WCND
            CCID=CCND
            L10136=    2
            GO TO 10136
10148       CONTINUE
            UKND=UCND
            VKND=VCND
            WKND=WCND
            CKND=CCND
            XKNU=XCNU
            YKNU=YCNU
            UCND=UCVD
            VCND=VCVD
            WCND=WCVD
            CCND=CCVD
            XCNU=XCVU
            YCNU=YCVU
            L10145=    2
            GO TO 10145
10149       CONTINUE
            UCND=UKND
            VCND=VKND
            WCND=WKND
            CCND=CKND
            XCNU=XKNU
            YCNU=YKNU
            L10043=    3
            GO TO 10043
10150       CONTINUE
10146     CONTINUE
10147     CONTINUE
C
10126   CONTINUE
C
      GO TO (10123,10125) , L10124
C
C The following internal procedure outputs the next point; if mapping
C is being done and there is a sufficiently large jump in the mapped
C position of the point, we check for a discontinuity in the mapping.
C Similarly, if mapping is being done and point interpolation is
C activated, we check for a large enough jump in the mapped position
C to justify interpolating points.
C
10145 CONTINUE
        IF (.NOT.(IMPF.NE.0.AND.(UCND.NE.UCOD.OR.VCND.NE.VCOD.OR.WCND.NE
     +.WCOD))) GO TO 10151
          RUDO=RUDN
          RUDN=(ABS(XCNU-XCOU)+ABS(YCNU-YCOU))/
     +         (ABS(UCND-UCOD)+ABS(VCND-VCOD)+ABS(WCND-WCOD))
          IF (.NOT.(RUDN.GT.2.*RUDO)) GO TO 10152
            L10154=    1
            GO TO 10154
10153       CONTINUE
10152     CONTINUE
          IF (.NOT.(PITH.GT.0.)) GO TO 10155
            UCTD=UCND
            VCTD=VCND
            WCTD=WCND
            CCTD=CCND
            XCTU=XCNU
            YCTU=YCNU
            L10157=    1
            GO TO 10157
10156       CONTINUE
10155     CONTINUE
10151   CONTINUE
        NCPL=NCPL+1
        XCPL(NCPL)=XCNU
        YCPL(NCPL)=YCNU
        IF (.NOT.(IPCV.NE.0)) GO TO 10158
          CVAL=CCND
          L10132=    2
          GO TO 10132
10159     CONTINUE
          IF (.NOT.(IPCI.NE.IPCD)) GO TO 10160
            L10142=    2
            GO TO 10142
10161       CONTINUE
            IPCD=IPCI
10160     CONTINUE
10158   CONTINUE
      GO TO (10144,10149) , L10145
C
C The following internal procedure is invoked when an unusually large
C jump in the position of mapped points on the curve is seen.  It
C checks for a possible discontinuity in the mapping function (as
C can happen, for example, when a cylindrical equidistant projection
C is being used); if there is such a discontinuity, we must generate
C a final point on one side of it, dump the polyline, and then start
C a new polyline on the other side.
C
10154 CONTINUE
        UC1D=UCOD
        VC1D=VCOD
        WC1D=WCOD
        CC1D=CCOD
        XC1U=XCOU
        YC1U=YCOU
        UC2D=UCND
        VC2D=VCND
        WC2D=WCND
        CC2D=CCND
        XC2U=XCNU
        YC2U=YCNU
        ITMP=0
10162   CONTINUE
          DSTO=ABS(XC2U-XC1U)+ABS(YC2U-YC1U)
          UC3D=(UC1D+UC2D)/2.
          VC3D=(VC1D+VC2D)/2.
          WC3D=(WC1D+WC2D)/2.
          CC3D=(CC1D+CC2D)/2.
          CALL HLUCTMXYZ (IMPF,UC3D,VC3D,WC3D,XC3U,YC3U)
          IF (ICFELL('VTTSOM',2).NE.0) GO TO 106
          IF (.NOT.(OORV.EQ.0..OR.(XC3U.NE.OORV.AND.YC3U.NE.OORV)))
     +    GO TO 10163
            DST1=ABS(XC3U-XC1U)+ABS(YC3U-YC1U)
            DST2=ABS(XC3U-XC2U)+ABS(YC3U-YC2U)
            IF (.NOT.(MIN(DST1,DST2).GT.DSTO)) GO TO 10164
              ITMP=1000
              GO TO 10165
10164       CONTINUE
            IF (.NOT.(DST1.LT.DST2)) GO TO 10166
              IF (UC3D.EQ.UC1D.AND.VC3D.EQ.VC1D.AND.WC3D.EQ.WC1D) GO TO
     +10165
              UC1D=UC3D
              VC1D=VC3D
              WC1D=WC3D
              CC1D=CC3D
              XC1U=XC3U
              YC1U=YC3U
            GO TO 10167
10166       CONTINUE
              IF (UC3D.EQ.UC2D.AND.VC3D.EQ.VC2D.AND.WC3D.EQ.WC2D) GO TO
     +10165
              UC2D=UC3D
              VC2D=VC3D
              WC2D=WC3D
              CC2D=CC3D
              XC2U=XC3U
              YC2U=YC3U
10167       CONTINUE
            ITMP=ITMP+1
            IF (ITMP.EQ.64) GO TO 10165
          GO TO 10168
10163     CONTINUE
            UCVD=UCOD
            VCVD=VCOD
            WCVD=WCOD
            CCVD=CCOD
            XCVU=XCOU
            YCVU=YCOU
            UCID=UC3D
            VCID=VC3D
            WCID=WC3D
            CCID=CC3D
            L10136=    3
            GO TO 10136
10169       CONTINUE
            L10138=    2
            GO TO 10138
10170       CONTINUE
            L10043=    4
            GO TO 10043
10171       CONTINUE
            UCID=UC3D
            VCID=VC3D
            WCID=WC3D
            CCID=CC3D
            UCVD=UCND
            VCVD=VCND
            WCVD=WCND
            CCVD=CCND
            XCVU=XCNU
            YCVU=YCNU
            L10136=    4
            GO TO 10136
10172       CONTINUE
            L10138=    3
            GO TO 10138
10173       CONTINUE
            ITMP=1000
            GO TO 10165
10168     CONTINUE
        GO TO 10162
10165   CONTINUE
        IF (.NOT.(ITMP.NE.1000.AND.(ABS(XC1U-XC2U).GT.SMLX.OR.ABS(YC1U-Y
     +C2U).GT.SMLY))) GO TO 10174
          IF (.NOT.(IMPF.NE.0.AND.PITH.GT.0.)) GO TO 10175
            UCTD=UC1D
            VCTD=VC1D
            WCTD=WC1D
            CCTD=CC1D
            XCTU=XC1U
            YCTU=YC1U
            L10157=    2
            GO TO 10157
10176       CONTINUE
10175     CONTINUE
          NCPL=NCPL+1
          XCPL(NCPL)=XC1U
          YCPL(NCPL)=YC1U
          L10043=    5
          GO TO 10043
10177     CONTINUE
          IF (.NOT.(IMPF.NE.0.AND.PITH.GT.0.)) GO TO 10178
            UCLD=UC2D
            VCLD=VC2D
            WCLD=WC2D
            CCLD=CC2D
            XCLU=XC2U
            YCLU=YC2U
10178     CONTINUE
          NCPL=1
          XCPL(1)=XC2U
          YCPL(1)=YC2U
          IF (.NOT.(IPCV.NE.0)) GO TO 10179
            CVAL=CC2D
            L10132=    3
            GO TO 10132
10180       CONTINUE
            IPCD=IPCI
10179     CONTINUE
10174   CONTINUE
      GO TO (10153) , L10154
C
C Given two points in the data-array-index coordinate system, one of
C which maps to a visible point and the other of which maps to an
C invisible point, this internal routine searches the line between
C them for a point near the edge of visibility.
C
10136 CONTINUE
        ITMP=0
10181   CONTINUE
          UCHD=(UCVD+UCID)/2.
          VCHD=(VCVD+VCID)/2.
          WCHD=(WCVD+WCID)/2.
          CCHD=(CCVD+CCID)/2.
          CALL HLUCTMXYZ (IMPF,UCHD,VCHD,WCHD,XCHU,YCHU)
          IF (ICFELL('VTTSOM',3).NE.0) GO TO 106
          IF (.NOT.(XCHU.NE.OORV.AND.YCHU.NE.OORV)) GO TO 10182
            IF (UCHD.EQ.UCVD.AND.VCHD.EQ.VCVD.AND.WCHD.EQ.WCVD) GO TO 10
     +183
            UCVD=UCHD
            VCVD=VCHD
            WCVD=WCHD
            CCVD=CCHD
            XCVU=XCHU
            YCVU=YCHU
          GO TO 10184
10182     CONTINUE
            IF (UCHD.EQ.UCID.AND.VCHD.EQ.VCID.AND.WCHD.EQ.WCID) GO TO 10
     +183
            UCID=UCHD
            VCID=VCHD
            WCID=WCHD
            CCID=CCHD
10184     CONTINUE
          ITMP=ITMP+1
          IF (ITMP.EQ.64) GO TO 10183
        GO TO 10181
10183   CONTINUE
      GO TO (10135,10148,10169,10172) , L10136
C
C The following internal procedure outputs a visible edge point found
C by the previous internal procedure.
C
10138 CONTINUE
        IF (.NOT.(PITH.GT.0.)) GO TO 10185
          IF (.NOT.(NCPL.EQ.0)) GO TO 10186
            UCLD=UCVD
            VCLD=VCVD
            WCLD=WCVD
            CCLD=CCVD
            XCLU=XCVU
            YCLU=YCVU
          GO TO 10187
10186     CONTINUE
            UCTD=UCVD
            VCTD=VCVD
            WCTD=WCVD
            CCTD=CCVD
            XCTU=XCVU
            YCTU=YCVU
            L10157=    3
            GO TO 10157
10188       CONTINUE
10187     CONTINUE
10185   CONTINUE
        NCPL=NCPL+1
        XCPL(NCPL)=XCVU
        YCPL(NCPL)=YCVU
        IF (.NOT.(IPCV.NE.0)) GO TO 10189
          CVAL=CCVD
          L10132=    4
          GO TO 10132
10190     CONTINUE
          IF (.NOT.(IPCI.NE.IPCD)) GO TO 10191
            L10142=    3
            GO TO 10142
10192       CONTINUE
            IPCD=IPCI
10191     CONTINUE
10189   CONTINUE
      GO TO (10137,10170,10173) , L10138
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
10157 CONTINUE
10193   CONTINUE
        IF (.NOT.(ABS(XCTU-XCLU).GT.PITX.OR.ABS(YCTU-YCLU).GT.PITY))
     +  GO TO 10194
          IFND=0
          UCQD=0.
          VCQD=0.
          WCQD=0.
          CCQD=0.
          RDST=.50
          RSTP=.25
10195     CONTINUE
            UCPD=UCLD+RDST*(UCTD-UCLD)
            VCPD=VCLD+RDST*(VCTD-VCLD)
            WCPD=WCLD+RDST*(WCTD-WCLD)
            CCPD=CCLD+RDST*(CCTD-CCLD)
            CALL HLUCTMXYZ (IMPF,UCPD,VCPD,WCPD,XCPU,YCPU)
            IF (ICFELL('VTTSOM',4).NE.0) GO TO 106
            IF (OORV.NE.0..AND.(XCPU.EQ.OORV.OR.YCPU.EQ.OORV)) GO TO 101
     +96
            IF (.NOT.(ABS(XCPU-XCLU).LT.PITX.AND.ABS(YCPU-YCLU).LT.PITY)
     +)     GO TO 10197
              IFND=1
              UCQD=UCPD
              VCQD=VCPD
              WCQD=WCPD
              CCQD=CCPD
              XCQU=XCPU
              YCQU=YCPU
              IF (ABS(XCQU-XCLU).GT..5*PITX.OR.ABS(YCQU-YCLU).GT..5*PITY
     +)       GO TO 10196
              RDST=RDST+RSTP
            GO TO 10198
10197       CONTINUE
              RDST=RDST-RSTP
10198       CONTINUE
            RSTP=RSTP/2.
            IF (RSTP.LT..0001) GO TO 10196
          GO TO 10195
10196     CONTINUE
          IF (.NOT.(IFND.NE.0.AND.(UCQD.NE.UCLD.OR.VCQD.NE.VCLD.OR.WCQD.
     +NE.WCLD))) GO TO 10199
            NCPL=NCPL+1
            XCPL(NCPL)=XCQU
            YCPL(NCPL)=YCQU
            IF (.NOT.(IPCV.NE.0)) GO TO 10200
              CVAL=CCQD
              L10132=    5
              GO TO 10132
10201         CONTINUE
              IF (.NOT.(IPCI.NE.IPCD)) GO TO 10202
                L10142=    4
                GO TO 10142
10203           CONTINUE
                IPCD=IPCI
10202         CONTINUE
10200       CONTINUE
            IF (.NOT.(NCPL.EQ.MCPL)) GO TO 10204
              L10142=    5
              GO TO 10142
10205         CONTINUE
10204       CONTINUE
            UCLD=UCQD
            VCLD=VCQD
            WCLD=WCQD
            CCLD=CCQD
            XCLU=XCQU
            YCLU=YCQU
          GO TO 10206
10199     CONTINUE
            UCLD=UCTD
            VCLD=VCTD
            WCLD=WCTD
            CCLD=CCTD
            XCLU=XCTU
            YCLU=YCTU
10206     CONTINUE
        GO TO 10193
10194   CONTINUE
        UCLD=UCTD
        VCLD=VCTD
        WCLD=WCTD
        CCLD=CCTD
        XCLU=XCTU
        YCLU=YCTU
      GO TO (10156,10176,10188) , L10157
C
C The following internal procedure is given the data-system coordinates
C of a point (UCND,VCND,WCND) and computes the user-system coordinates
C of the point's projection (XCNU,YCNU).  It also sets a flag indicating
C whether the projection point is visible or not.
C
10040 CONTINUE
C
        IF (.NOT.(IMPF.EQ.0)) GO TO 10207
          XCNU=UCND
          YCNU=VCND
          IVNU=1
        GO TO 10208
10207   CONTINUE
          CALL HLUCTMXYZ (IMPF,UCND,VCND,WCND,XCNU,YCNU)
          IF (ICFELL('VTTSOM',5).NE.0) GO TO 106
          IF (.NOT.((OORV.NE.0.).AND.(XCNU.EQ.OORV.OR.YCNU.EQ.OORV)))
     +    GO TO 10209
            IVNU=0
          GO TO 10210
10209     CONTINUE
            IVNU=1
10210     CONTINUE
10208   CONTINUE
C
      GO TO (10039,10049,10121) , L10040
C
C The following internal procedure, given a value (CVAL), computes a
C polyline color index (IPCI) to be used to get a desired color for a
C streamline being drawn.
C
10132 CONTINUE
10211   CONTINUE
        IF (.NOT.(ICVL.GT.1.AND.CVAL.LT.TVAL(ICVL))) GO TO 10212
          ICVL=ICVL-1
        GO TO 10211
10212   CONTINUE
10213   CONTINUE
        IF (.NOT.(ICVL.LT.NCLR.AND.CVAL.GE.TVAL(ICVL+1))) GO TO 10214
          ICVL=ICVL+1
        GO TO 10213
10214   CONTINUE
        IPCI=ICLR(ICVL)
      GO TO (10131,10159,10180,10190,10201) , L10132
C
C The following internal procedure draws the part of the curve defined
C by the contents of the polyline buffer and then removes all but the
C last point from the buffer.  IPCC is the polyline color currently
C in use and IPCD the polyline color desired for the curve.
C
10142 CONTINUE
C
        I=1
C
10215   CONTINUE
          I=I+1
          IF (I.GT.NCPL) GO TO 10216
          IF (.NOT.(ABS(XCPL(I)-XCPL(I-1)).LT.EPSX.AND.ABS(YCPL(I)-YCPL(
     +I-1)).LT.EPSY)) GO TO 10217
            IF (.NOT.(I.NE.NCPL)) GO TO 10218
              DO 10219 J=I+1,NCPL
                XCPL(J-1)=XCPL(J)
                YCPL(J-1)=YCPL(J)
10219         CONTINUE
            GO TO 10220
10218       CONTINUE
              XCPL(NCPL-1)=XCPL(NCPL)
              YCPL(NCPL-1)=YCPL(NCPL)
10220       CONTINUE
            I=I-1
            NCPL=NCPL-1
10217     CONTINUE
        GO TO 10215
10216   CONTINUE
C
        IF (.NOT.(NCPL.GT.1)) GO TO 10221
          IF (.NOT.(IPCV.NE.0)) GO TO 10222
            IF (.NOT.(IPCC.NE.IPCD)) GO TO 10223
              CALL GSPLCI (IPCD)
              IPCC=IPCD
10223       CONTINUE
10222     CONTINUE
          IF (.NOT.(IAMA(1).EQ.0)) GO TO 10224
            CALL CURVE (XCPL,YCPL,NCPL)
          GO TO 10225
10224     CONTINUE
            CALL ARDRLN (IAMA,XCPL,YCPL,NCPL,
     +                        XCPF,YCPF,MCPF,
     +                        IAAI,IAGI,MNOG,RTPL)
10225     CONTINUE
10221   CONTINUE
C
        XCPL(1)=XCPL(NCPL)
        YCPL(1)=YCPL(NCPL)
        NCPL=1
C
      GO TO (10141,10161,10192,10203,10205) , L10142
C
C The following internal procedure draws the part of the curve defined
C by the contents of the polyline buffer and then clears the buffer.
C IPCC is the polyline color currently in use and IPCD the polyline
C color desired for the curve.
C
10043 CONTINUE
C
        I=1
C
10226   CONTINUE
          I=I+1
          IF (I.GT.NCPL) GO TO 10227
          IF (.NOT.(ABS(XCPL(I)-XCPL(I-1)).LT.EPSX.AND.ABS(YCPL(I)-YCPL(
     +I-1)).LT.EPSY)) GO TO 10228
            IF (.NOT.(I.NE.NCPL)) GO TO 10229
              DO 10230 J=I+1,NCPL
                XCPL(J-1)=XCPL(J)
                YCPL(J-1)=YCPL(J)
10230         CONTINUE
            GO TO 10231
10229       CONTINUE
              XCPL(NCPL-1)=XCPL(NCPL)
              YCPL(NCPL-1)=YCPL(NCPL)
10231       CONTINUE
            I=I-1
            NCPL=NCPL-1
10228     CONTINUE
        GO TO 10226
10227   CONTINUE
C
        IF (.NOT.(NCPL.GT.1)) GO TO 10232
          IF (.NOT.(IPCV.NE.0)) GO TO 10233
            IF (.NOT.(IPCC.NE.IPCD)) GO TO 10234
              CALL GSPLCI (IPCD)
              IPCC=IPCD
10234       CONTINUE
10233     CONTINUE
          IF (.NOT.(IAMA(1).EQ.0)) GO TO 10235
            CALL CURVE (XCPL,YCPL,NCPL)
          GO TO 10236
10235     CONTINUE
            CALL ARDRLN (IAMA,XCPL,YCPL,NCPL,
     +                        XCPF,YCPF,MCPF,
     +                        IAAI,IAGI,MNOG,RTPL)
10236     CONTINUE
10232   CONTINUE
C
        NCPL=0
        RUDN=0.
C
      GO TO (10042,10114,10150,10171,10177) , L10043
C
      END
