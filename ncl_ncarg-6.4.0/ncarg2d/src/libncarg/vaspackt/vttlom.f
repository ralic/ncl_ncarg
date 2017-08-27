      SUBROUTINE VTTLOM (RPNT,IEDG,ITRI,ISTR,RSTR,SSTR,ANGD,SLMX,
     +                   ITER,SLTR,IAMA,RTPL)
C
      DIMENSION RPNT(*),IEDG(*),ITRI(*),IAMA(*)
C
      EXTERNAL RTPL
C
C This routine, given arrays defining a triangular mesh, at each point
C of which a velocity vector is given, and the location of a particular
C starting point on a particular triangle of that mesh, draws a line
C that everywhere makes a specified angle with the velocity vectors,
C continuing until one of a set of termination conditions is satisfied.
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
C ANGD is the angle, in degrees, that the line drawn is to make with
C the velocity vectors.
C
C SLMX is the maximum length of streamline to be traced.
C
C ITER is a flag that is returned to say how the line terminated:
C
C   ITER=1 => exterior edge of mesh encountered.
C   ITER=4 => line traced for specified distance.
C   ITER=5 => velocity along line dropped below VVMM.
C   ITER=7 => other (e. g., a degenerate triangle).
C
C SLTR is returned and is the length of streamline traced before a
C termination condition was encountered.
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
C If the line is to be colored, save the initial polyline color and
C initialize the variables that keep track of coloring.
C
      IF (.NOT.(ICTV.NE.0.AND.NCLR.NE.0)) GO TO 10001
        CALL GQPLCI (IGER,IPCS)
        IF (.NOT.(IGER.NE.0)) GO TO 10002
          CALL SETER ('VTTLOM - ERROR EXIT FROM GQPLCI',1,1)
          RETURN
10002   CONTINUE
        IPCC=IPCS
        ICVL=(NCLR+1)/2
      GO TO 10003
10001 CONTINUE
        IPCS=-1
        IPCC=-1
        IPCD=-1
10003 CONTINUE
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
     +RI(IIII+2)+1).NE.IEDG(ITRI(IIII+3)+2))) GO TO 10004
        IPP1=IEDG(ITRI(IIII+2)+1)
        IPP2=IEDG(ITRI(IIII+2)+2)
      GO TO 10005
10004 CONTINUE
        IPP1=IEDG(ITRI(IIII+2)+2)
        IPP2=IEDG(ITRI(IIII+2)+1)
10005 CONTINUE
C
      IF (.NOT.(IEDG(ITRI(IIII+1)+1).NE.IPP1)) GO TO 10006
        IPP3=IEDG(ITRI(IIII+1)+1)
      GO TO 10007
10006 CONTINUE
        IPP3=IEDG(ITRI(IIII+1)+2)
10007 CONTINUE
C
C Initialize the starting point values.
C
      RVAL=RSTR
      SVAL=SSTR
C
C SLTR keeps track of the length of line traced already.
C
      SLTR=0.
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
      IF (.NOT.(IDIR.EQ.0)) GO TO 10008
        UVP1=-RPNT(IPP1+4)
        VVP1=-RPNT(IPP1+5)
        WVP1=-RPNT(IPP1+6)
        UVP2=-RPNT(IPP2+4)
        VVP2=-RPNT(IPP2+5)
        WVP2=-RPNT(IPP2+6)
        UVP3=-RPNT(IPP3+4)
        VVP3=-RPNT(IPP3+5)
        WVP3=-RPNT(IPP3+6)
      GO TO 10009
10008 CONTINUE
        UVP1=+RPNT(IPP1+4)
        VVP1=+RPNT(IPP1+5)
        WVP1=+RPNT(IPP1+6)
        UVP2=+RPNT(IPP2+4)
        VVP2=+RPNT(IPP2+5)
        WVP2=+RPNT(IPP2+6)
        UVP3=+RPNT(IPP3+4)
        VVP3=+RPNT(IPP3+5)
        WVP3=+RPNT(IPP3+6)
10009 CONTINUE
C
      VMG1=SQRT(UVP1**2+VVP1**2+WVP1**2)
C
      IF (.NOT.(VMG1.NE.0.)) GO TO 10010
        UVP1=.001*EMAX*UVP1/VMG1
        VVP1=.001*EMAX*VVP1/VMG1
        WVP1=.001*EMAX*WVP1/VMG1
10010 CONTINUE
C
      VMG2=SQRT(UVP2**2+VVP2**2+WVP2**2)
C
      IF (.NOT.(VMG2.NE.0.)) GO TO 10011
        UVP2=.001*EMAX*UVP2/VMG2
        VVP2=.001*EMAX*VVP2/VMG2
        WVP2=.001*EMAX*WVP2/VMG2
10011 CONTINUE
C
      VMG3=SQRT(UVP3**2+VVP3**2+WVP3**2)
C
      IF (.NOT.(VMG3.NE.0.)) GO TO 10012
        UVP3=.001*EMAX*UVP3/VMG3
        VVP3=.001*EMAX*VVP3/VMG3
        WVP3=.001*EMAX*WVP3/VMG3
10012 CONTINUE
C
      IF (.NOT.(ICTV.EQ.0)) GO TO 10013
        CVP1=0.
        CVP2=0.
        CVP3=0.
      GO TO 10014
10013 CONTINUE
      IF (.NOT.(ABS(ICTV).LE.LOPN)) GO TO 10015
        CVP1=RPNT(IPP1+ABS(ICTV))
        CVP2=RPNT(IPP2+ABS(ICTV))
        CVP3=RPNT(IPP3+ABS(ICTV))
      GO TO 10014
10015 CONTINUE
        CVP1=SQRT(RPNT(IPP1+4)**2+RPNT(IPP1+5)**2+RPNT(IPP1+6)**2)
        CVP2=SQRT(RPNT(IPP2+4)**2+RPNT(IPP2+5)**2+RPNT(IPP2+6)**2)
        CVP3=SQRT(RPNT(IPP3+4)**2+RPNT(IPP3+5)**2+RPNT(IPP3+6)**2)
10014 CONTINUE
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
      IF (.NOT.(DNOM.NE.0.)) GO TO 10016
 
        DCNU=A/DNOM
        DCNV=B/DNOM
        DCNW=C/DNOM
C
      GO TO 10017
10016 CONTINUE
C
        ITER=7
        GO TO 104
C
10017 CONTINUE
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
C Use cross products with the normal to the triangle to generate
C vectors perpendicular to the velocity vectors.
C
      UQP1=DCNV*WPP1-DCNW*VPP1
      VQP1=DCNW*UPP1-DCNU*WPP1
      WQP1=DCNU*VPP1-DCNV*UPP1
C
      UQP2=DCNV*WPP2-DCNW*VPP2
      VQP2=DCNW*UPP2-DCNU*WPP2
      WQP2=DCNU*VPP2-DCNV*UPP2
C
      UQP3=DCNV*WPP3-DCNW*VPP3
      VQP3=DCNW*UPP3-DCNU*WPP3
      WQP3=DCNU*VPP3-DCNV*UPP3
C
C Now generate vectors at the specified angle to the velocity vectors.
C
      UPP1=UPP1*COS(DTOR*ANGD)+UQP1*SIN(DTOR*ANGD)
      VPP1=VPP1*COS(DTOR*ANGD)+VQP1*SIN(DTOR*ANGD)
      WPP1=WPP1*COS(DTOR*ANGD)+WQP1*SIN(DTOR*ANGD)
C
      UPP2=UPP2*COS(DTOR*ANGD)+UQP2*SIN(DTOR*ANGD)
      VPP2=VPP2*COS(DTOR*ANGD)+VQP2*SIN(DTOR*ANGD)
      WPP2=WPP2*COS(DTOR*ANGD)+WQP2*SIN(DTOR*ANGD)
C
      UPP3=UPP3*COS(DTOR*ANGD)+UQP3*SIN(DTOR*ANGD)
      VPP3=VPP3*COS(DTOR*ANGD)+VQP3*SIN(DTOR*ANGD)
      WPP3=WPP3*COS(DTOR*ANGD)+WQP3*SIN(DTOR*ANGD)
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
     +GO TO 10018
        RVV1=(      UPP1 *(VCP3-VCP1)-      VPP1 *(UCP3-UCP1))/DNUV
        SVV1=((UCP2-UCP1)*      VPP1 -(VCP2-VCP1)*      UPP1 )/DNUV
        RVV2=(      UPP2 *(VCP3-VCP1)-      VPP2 *(UCP3-UCP1))/DNUV
        SVV2=((UCP2-UCP1)*      VPP2 -(VCP2-VCP1)*      UPP2 )/DNUV
        RVV3=(      UPP3 *(VCP3-VCP1)-      VPP3 *(UCP3-UCP1))/DNUV
        SVV3=((UCP2-UCP1)*      VPP3 -(VCP2-VCP1)*      UPP3 )/DNUV
      GO TO 10019
10018 CONTINUE
      IF (.NOT.(ABS(DNVW).GT.ABS(DNWU).AND.ABS(DNVW).GT.ABS(DNUV)))
     +GO TO 10020
        RVV1=(      VPP1 *(WCP3-WCP1)-      WPP1 *(VCP3-VCP1))/DNVW
        SVV1=((VCP2-VCP1)*      WPP1 -(WCP2-WCP1)*      VPP1 )/DNVW
        RVV2=(      VPP2 *(WCP3-WCP1)-      WPP2 *(VCP3-VCP1))/DNVW
        SVV2=((VCP2-VCP1)*      WPP2 -(WCP2-WCP1)*      VPP2 )/DNVW
        RVV3=(      VPP3 *(WCP3-WCP1)-      WPP3 *(VCP3-VCP1))/DNVW
        SVV3=((VCP2-VCP1)*      WPP3 -(WCP2-WCP1)*      VPP3 )/DNVW
      GO TO 10019
10020 CONTINUE
        RVV1=(      WPP1 *(UCP3-UCP1)-      UPP1 *(WCP3-WCP1))/DNWU
        SVV1=((WCP2-WCP1)*      UPP1 -(UCP2-UCP1)*      WPP1 )/DNWU
        RVV2=(      WPP2 *(UCP3-UCP1)-      UPP2 *(WCP3-WCP1))/DNWU
        SVV2=((WCP2-WCP1)*      UPP2 -(UCP2-UCP1)*      WPP2 )/DNWU
        RVV3=(      WPP3 *(UCP3-UCP1)-      UPP3 *(WCP3-WCP1))/DNWU
        SVV3=((WCP2-WCP1)*      UPP3 -(UCP2-UCP1)*      WPP3 )/DNWU
10019 CONTINUE
C
C If the last triangle was blocked (or non-existent) and this one is
C not blocked, compute values associated with the first point of a new
C segment of streamline.
C
      IF (.NOT.(ILTB.NE.0.AND.ICTB.EQ.0)) GO TO 10021
        UCND=UCP1+RVAL*(UCP2-UCP1)+SVAL*(UCP3-UCP1)
        VCND=VCP1+RVAL*(VCP2-VCP1)+SVAL*(VCP3-VCP1)
        WCND=WCP1+RVAL*(WCP2-WCP1)+SVAL*(WCP3-WCP1)
        CCND=CVP1+RVAL*(CVP2-CVP1)+SVAL*(CVP3-CVP1)
        L10023=    1
        GO TO 10023
10022   CONTINUE
10021 CONTINUE
C
C If the new triangle is blocked and there's something in the polyline
C buffer, clear the buffer.
C
      IF (.NOT.(ICTB.NE.0.AND.NCPL.NE.0)) GO TO 10024
        L10026=    1
        GO TO 10026
10025   CONTINUE
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
      IF (.NOT.(VMAG.GT.VVMM)) GO TO 10027
C
        TEMP=MIN(SLNE,SLPR)/VMAG
C
C Check for exit through the side joining V1 and V3.
C
        IF (.NOT.(RVAL+TEMP*RVVN.LT.0..AND.(RVAL.GT..001.OR.ABS(RVVN).GT
     +..001*ABS(RVVN+2.*SVVN)))) GO TO 10028
          JUMP=1
          TEMP=-RVAL/RVVN
10028   CONTINUE
C
C Check for exit through the side joining V1 and V2.
C
        IF (.NOT.(SVAL+TEMP*SVVN.LT.0..AND.(SVAL.GT..001.OR.ABS(SVVN).GT
     +..001*ABS(2.*RVVN+SVVN)))) GO TO 10029
          JUMP=2
          TEMP=-SVAL/SVVN
10029   CONTINUE
C
C Check for exit through the side joining V2 and V3.
C
        IF (.NOT.(1.-RVAL-SVAL-TEMP*(RVVN+SVVN).LT.0..AND.(1.-RVAL-SVAL.
     +GT..001.OR.ABS(RVVN+SVVN).GT..001*ABS(RVVN-SVVN)))) GO TO 10030
          JUMP=3
          TEMP=(1.-RVAL-SVAL)/(RVVN+SVVN)
10030   CONTINUE
C
        RVAL=MAX(0.,MIN(1.,RVAL+TEMP*RVVN))
        SVAL=MAX(0.,MIN(1.,SVAL+TEMP*SVVN))
C
C If the current triangle is not blocked, save information about the
C previous point, generate values for the new point, and process the
C line segment joining them.
C
        IF (.NOT.(ICTB.EQ.0)) GO TO 10031
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
          L10023=    2
          GO TO 10023
10032     CONTINUE
          L10034=    1
          GO TO 10034
10033     CONTINUE
10031   CONTINUE
C
C Reduce the streamline length to be traced before the next event.
C
        SLNE=SLNE-VMAG*TEMP
C
C If the line is now of the desired length at the next event, terminate
C it.
C
        IF (.NOT.(SLNE.LT..01*SLPR)) GO TO 10035
          SLNE=0.
          JUMP=4
10035   CONTINUE
C
      GO TO 10036
10027 CONTINUE
C
C The velocity is too low.
C
        JUMP=5
C
10036 CONTINUE
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
      IF (.NOT.(SLTR.GE.SLMX)) GO TO 10037
        ITER=4
        GO TO 104
10037 CONTINUE
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
      IF (.NOT.(JUMP.EQ.1)) GO TO 10038
C
        IF (.NOT.(IEDG(ITRI(IIII+1)+1).EQ.IPP1.AND.IEDG(ITRI(IIII+1)+2).
     +EQ.IPP3)) GO TO 10039
          INEW=IEDG(ITRI(IIII+1)+3)
        GO TO 10040
10039   CONTINUE
        IF (.NOT.(IEDG(ITRI(IIII+1)+1).EQ.IPP3.AND.IEDG(ITRI(IIII+1)+2).
     +EQ.IPP1)) GO TO 10041
          INEW=IEDG(ITRI(IIII+1)+4)
        GO TO 10040
10041   CONTINUE
          INEW=-1
10040   CONTINUE
        IF (.NOT.(INEW.LT.0)) GO TO 10042
          ITER=1
          GO TO 104
10042   CONTINUE
        IIII=LOTN*((INEW-1)/LOTN)
        IPP2=IPP3
        IF (.NOT.(IEDG(ITRI(IIII+1)+1).NE.IPP1.AND.IEDG(ITRI(IIII+1)+1).
     +NE.IPP2)) GO TO 10043
          IPP3=IEDG(ITRI(IIII+1)+1)
        GO TO 10044
10043   CONTINUE
        IF (.NOT.(IEDG(ITRI(IIII+1)+2).NE.IPP1.AND.IEDG(ITRI(IIII+1)+2).
     +NE.IPP2)) GO TO 10045
          IPP3=IEDG(ITRI(IIII+1)+2)
        GO TO 10044
10045   CONTINUE
        IF (.NOT.(IEDG(ITRI(IIII+2)+1).NE.IPP1.AND.IEDG(ITRI(IIII+2)+1).
     +NE.IPP2)) GO TO 10046
          IPP3=IEDG(ITRI(IIII+2)+1)
        GO TO 10044
10046   CONTINUE
          IPP3=IEDG(ITRI(IIII+2)+2)
10044   CONTINUE
        RVAL=SVAL
        SVAL=0.
C
C If the line terminated because SVAL became zero, move to the triangle,
C if any, that lies on the other side of edge 2 (joining vertices 1 and
C 2 of the triangle).
C
      GO TO 10047
10038 CONTINUE
      IF (.NOT.(JUMP.EQ.2)) GO TO 10048
C
        IF (.NOT.(IEDG(ITRI(IIII+2)+1).EQ.IPP1.AND.IEDG(ITRI(IIII+2)+2).
     +EQ.IPP2)) GO TO 10049
          INEW=IEDG(ITRI(IIII+2)+4)
        GO TO 10050
10049   CONTINUE
        IF (.NOT.(IEDG(ITRI(IIII+2)+1).EQ.IPP2.AND.IEDG(ITRI(IIII+2)+2).
     +EQ.IPP1)) GO TO 10051
          INEW=IEDG(ITRI(IIII+2)+3)
        GO TO 10050
10051   CONTINUE
          INEW=-1
10050   CONTINUE
        IF (.NOT.(INEW.LT.0)) GO TO 10052
          ITER=1
          GO TO 104
10052   CONTINUE
        IIII=LOTN*((INEW-1)/LOTN)
        IPP3=IPP2
        IF (.NOT.(IEDG(ITRI(IIII+1)+1).NE.IPP1.AND.IEDG(ITRI(IIII+1)+1).
     +NE.IPP3)) GO TO 10053
          IPP2=IEDG(ITRI(IIII+1)+1)
        GO TO 10054
10053   CONTINUE
        IF (.NOT.(IEDG(ITRI(IIII+1)+2).NE.IPP1.AND.IEDG(ITRI(IIII+1)+2).
     +NE.IPP3)) GO TO 10055
          IPP2=IEDG(ITRI(IIII+1)+2)
        GO TO 10054
10055   CONTINUE
        IF (.NOT.(IEDG(ITRI(IIII+2)+1).NE.IPP1.AND.IEDG(ITRI(IIII+2)+1).
     +NE.IPP3)) GO TO 10056
          IPP2=IEDG(ITRI(IIII+2)+1)
        GO TO 10054
10056   CONTINUE
          IPP2=IEDG(ITRI(IIII+2)+2)
10054   CONTINUE
        SVAL=RVAL
        RVAL=0.
C
C If the line terminated because RVAL+SVAL became equal to one, move to
C the triangle, if any, that lies on the other side of edge 3 (joining
C vertices 2 and 3 of the triangle).
C
      GO TO 10047
10048 CONTINUE
      IF (.NOT.(JUMP.EQ.3)) GO TO 10057
C
        IF (.NOT.(IEDG(ITRI(IIII+3)+1).EQ.IPP2.AND.IEDG(ITRI(IIII+3)+2).
     +EQ.IPP3)) GO TO 10058
          INEW=IEDG(ITRI(IIII+3)+4)
        GO TO 10059
10058   CONTINUE
        IF (.NOT.(IEDG(ITRI(IIII+3)+1).EQ.IPP3.AND.IEDG(ITRI(IIII+3)+2).
     +EQ.IPP2)) GO TO 10060
          INEW=IEDG(ITRI(IIII+3)+3)
        GO TO 10059
10060   CONTINUE
          INEW=-1
10059   CONTINUE
        IF (.NOT.(INEW.LT.0)) GO TO 10061
          ITER=1
          GO TO 104
10061   CONTINUE
        IIII=LOTN*((INEW-1)/LOTN)
        IPP1=IPP2
        IF (.NOT.(IEDG(ITRI(IIII+1)+1).NE.IPP1.AND.IEDG(ITRI(IIII+1)+1).
     +NE.IPP3)) GO TO 10062
          IPP2=IEDG(ITRI(IIII+1)+1)
        GO TO 10063
10062   CONTINUE
        IF (.NOT.(IEDG(ITRI(IIII+1)+2).NE.IPP1.AND.IEDG(ITRI(IIII+1)+2).
     +NE.IPP3)) GO TO 10064
          IPP2=IEDG(ITRI(IIII+1)+2)
        GO TO 10063
10064   CONTINUE
        IF (.NOT.(IEDG(ITRI(IIII+2)+1).NE.IPP1.AND.IEDG(ITRI(IIII+2)+1).
     +NE.IPP3)) GO TO 10065
          IPP2=IEDG(ITRI(IIII+2)+1)
        GO TO 10063
10065   CONTINUE
          IPP2=IEDG(ITRI(IIII+2)+2)
10063   CONTINUE
        SVAL=1.-RVAL
        RVAL=0.
C
10047 CONTINUE
10057 CONTINUE
C
C If we just moved into a new triangle, we may need to recompute the
C values of the pointers to its vertices and of the coordinates of the
C point in the triangle.
C
      IF (.NOT.(JUMP.LT.4)) GO TO 10066
C
C Get a pointer to what should be point 1 of the triangle.  It will
C match one of the pointers we already have.
C
        IF (.NOT.(IEDG(ITRI(IIII+1)+1).EQ.IEDG(ITRI(IIII+2)+1).OR.IEDG(I
     +TRI(IIII+1)+1).EQ.IEDG(ITRI(IIII+2)+2))) GO TO 10067
          IPPT=IEDG(ITRI(IIII+1)+1)
        GO TO 10068
10067   CONTINUE
          IPPT=IEDG(ITRI(IIII+1)+2)
10068   CONTINUE
C
C Adjust the pointers and the values of RVAL and SVAL appropriately.
C
        IF (.NOT.(IPPT.NE.IPP1)) GO TO 10069
          RTMP=RVAL
          STMP=SVAL
          IF (.NOT.(IPPT.EQ.IPP2)) GO TO 10070
            IPP2=IPP3
            IPP3=IPP1
            IPP1=IPPT
            RVAL=STMP
            SVAL=1.-RTMP-STMP
          GO TO 10071
10070     CONTINUE
            IPP3=IPP2
            IPP2=IPP1
            IPP1=IPPT
            RVAL=1.-RTMP-STMP
            SVAL=RTMP
10071     CONTINUE
10069   CONTINUE
C
C Jump back to continue tracing the line in the new triangle.
C
        GO TO 101
C
C Otherwise, ...
C
10066 CONTINUE
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
      IF (.NOT.(NCPL.NE.0)) GO TO 10072
        L10026=    2
        GO TO 10026
10073   CONTINUE
10072 CONTINUE
C
C If the curve was colored, restore the saved polyline color.
C
      IF (IPCS.GE.0) CALL GSPLCI (IPCS)
C
C Done.
C
      RETURN
C
C The following internal procedure, given a line segment, adds visible
C portions of it to the coordinate arrays.
C
10034 CONTINUE
C
C If point interpolation is turned on, do the first IPIS segments.
C
        IF (.NOT.(IPIS.NE.0)) GO TO 10074
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
            GO TO 10077
10075       CONTINUE
            INTP =INTP +1
10077       CONTINUE
            IF (INTP .GT.(ABS(IPIS))) GO TO 10076
            UCND=USOD+(REAL(INTP)/REAL(ABS(IPIS)+1))*(USND-USOD)
            VCND=VSOD+(REAL(INTP)/REAL(ABS(IPIS)+1))*(VSND-VSOD)
            WCND=WSOD+(REAL(INTP)/REAL(ABS(IPIS)+1))*(WSND-WSOD)
            CCND=CSOD+(REAL(INTP)/REAL(ABS(IPIS)+1))*(CSND-CSOD)
            L10023=    3
            GO TO 10023
10078       CONTINUE
            IF (.NOT.(IPIS.GT.0.OR.IVNU.NE.IVOU)) GO TO 10079
              L10081=    1
              GO TO 10081
10080         CONTINUE
              UCOD=UCND
              VCOD=VCND
              WCOD=WCND
              CCOD=CCND
              XCOU=XCNU
              YCOU=YCNU
              IVOU=IVNU
10079       CONTINUE
          GO TO 10075
10076     CONTINUE
          UCND=USND
          VCND=VSND
          WCND=WSND
          CCND=CSND
          XCNU=XSNU
          YCNU=YSNU
          IVNU=ISNU
10074   CONTINUE
C
C Finish off the job.
C
        L10081=    2
        GO TO 10081
10082   CONTINUE
C
      GO TO (10033) , L10034
C
C The following internal procedure examines the points (UCOD,VCOD,WCOD),
C which projects into (XCOU,YCOU), and (UCND,VCND,WCND), which projects
C into (XCNU,YCNU), either of which may be visible or invisible in the
C projection space, and adds visible portions of the line segment
C between them to the polyline being built.
C
10081 CONTINUE
C
        IF (.NOT.(UCND.NE.UCOD.OR.VCND.NE.VCOD.OR.WCND.NE.WCOD))
     +  GO TO 10083
C
          IF (.NOT.(NCPL.EQ.0)) GO TO 10084
            IF (.NOT.(IVOU.NE.0)) GO TO 10085
              IF (.NOT.(IMPF.NE.0.AND.PITH.GT.0.)) GO TO 10086
                UCLD=UCOD
                VCLD=VCOD
                WCLD=WCOD
                CCLD=CCOD
                XCLU=XCOU
                YCLU=YCOU
10086         CONTINUE
              NCPL=1
              XCPL(1)=XCOU
              YCPL(1)=YCOU
              IF (.NOT.(IPCS.GE.0)) GO TO 10087
                CVAL=CCOD
                L10089=    1
                GO TO 10089
10088           CONTINUE
                IPCD=IPCI
10087         CONTINUE
            GO TO 10090
10085       CONTINUE
            IF (.NOT.(IVNU.NE.0)) GO TO 10091
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
              L10093=    1
              GO TO 10093
10092         CONTINUE
              L10095=    1
              GO TO 10095
10094         CONTINUE
              UCOD=UCVD
              VCOD=VCVD
              WCOD=WCVD
              CCOD=CCVD
              XCOU=XCVU
              YCOU=YCVU
              IVOU=1
10090       CONTINUE
10091       CONTINUE
          GO TO 10096
10084     CONTINUE
          IF (.NOT.(NCPL.EQ.MCPL)) GO TO 10097
            L10099=    1
            GO TO 10099
10098       CONTINUE
10096     CONTINUE
10097     CONTINUE
C
          IF (.NOT.(IVNU.NE.0)) GO TO 10100
            L10102=    1
            GO TO 10102
10101       CONTINUE
          GO TO 10103
10100     CONTINUE
          IF (.NOT.(IVOU.NE.0)) GO TO 10104
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
            L10093=    2
            GO TO 10093
10105       CONTINUE
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
            L10102=    2
            GO TO 10102
10106       CONTINUE
            UCND=UKND
            VCND=VKND
            WCND=WKND
            CCND=CKND
            XCNU=XKNU
            YCNU=YKNU
            L10026=    3
            GO TO 10026
10107       CONTINUE
10103     CONTINUE
10104     CONTINUE
C
10083   CONTINUE
C
      GO TO (10080,10082) , L10081
C
C The following internal procedure outputs the next point; if mapping
C is being done and there is a sufficiently large jump in the mapped
C position of the point, we check for a discontinuity in the mapping.
C Similarly, if mapping is being done and point interpolation is
C activated, we check for a large enough jump in the mapped position
C to justify interpolating points.
C
10102 CONTINUE
        IF (.NOT.(IMPF.NE.0.AND.(UCND.NE.UCOD.OR.VCND.NE.VCOD.OR.WCND.NE
     +.WCOD))) GO TO 10108
          RUDO=RUDN
          RUDN=(ABS(XCNU-XCOU)+ABS(YCNU-YCOU))/
     +         (ABS(UCND-UCOD)+ABS(VCND-VCOD)+ABS(WCND-WCOD))
          IF (.NOT.(RUDN.GT.2.*RUDO)) GO TO 10109
            L10111=    1
            GO TO 10111
10110       CONTINUE
10109     CONTINUE
          IF (.NOT.(PITH.GT.0.)) GO TO 10112
            UCTD=UCND
            VCTD=VCND
            WCTD=WCND
            CCTD=CCND
            XCTU=XCNU
            YCTU=YCNU
            L10114=    1
            GO TO 10114
10113       CONTINUE
10112     CONTINUE
10108   CONTINUE
        NCPL=NCPL+1
        XCPL(NCPL)=XCNU
        YCPL(NCPL)=YCNU
        IF (.NOT.(IPCS.GE.0)) GO TO 10115
          CVAL=CCND
          L10089=    2
          GO TO 10089
10116     CONTINUE
          IF (.NOT.(IPCI.NE.IPCD)) GO TO 10117
            L10099=    2
            GO TO 10099
10118       CONTINUE
            IPCD=IPCI
10117     CONTINUE
10115   CONTINUE
      GO TO (10101,10106) , L10102
C
C The following internal procedure is invoked when an unusually large
C jump in the position of mapped points on the curve is seen.  It
C checks for a possible discontinuity in the mapping function (as
C can happen, for example, when a cylindrical equidistant projection
C is being used); if there is such a discontinuity, we must generate
C a final point on one side of it, dump the polyline, and then start
C a new polyline on the other side.
C
10111 CONTINUE
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
10119   CONTINUE
          DSTO=ABS(XC2U-XC1U)+ABS(YC2U-YC1U)
          UC3D=(UC1D+UC2D)/2.
          VC3D=(VC1D+VC2D)/2.
          WC3D=(WC1D+WC2D)/2.
          CC3D=(CC1D+CC2D)/2.
          CALL HLUCTMXYZ (IMPF,UC3D,VC3D,WC3D,XC3U,YC3U)
          IF (ICFELL('VTTLOM',2).NE.0) GO TO 104
          IF (.NOT.(OORV.EQ.0..OR.(XC3U.NE.OORV.AND.YC3U.NE.OORV)))
     +    GO TO 10120
            DST1=ABS(XC3U-XC1U)+ABS(YC3U-YC1U)
            DST2=ABS(XC3U-XC2U)+ABS(YC3U-YC2U)
            IF (.NOT.(MIN(DST1,DST2).GT.DSTO)) GO TO 10121
              ITMP=1000
              GO TO 10122
10121       CONTINUE
            IF (.NOT.(DST1.LT.DST2)) GO TO 10123
              IF (UC3D.EQ.UC1D.AND.VC3D.EQ.VC1D.AND.WC3D.EQ.WC1D) GO TO
     +10122
              UC1D=UC3D
              VC1D=VC3D
              WC1D=WC3D
              CC1D=CC3D
              XC1U=XC3U
              YC1U=YC3U
            GO TO 10124
10123       CONTINUE
              IF (UC3D.EQ.UC2D.AND.VC3D.EQ.VC2D.AND.WC3D.EQ.WC2D) GO TO
     +10122
              UC2D=UC3D
              VC2D=VC3D
              WC2D=WC3D
              CC2D=CC3D
              XC2U=XC3U
              YC2U=YC3U
10124       CONTINUE
            ITMP=ITMP+1
            IF (ITMP.EQ.64) GO TO 10122
          GO TO 10125
10120     CONTINUE
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
            L10093=    3
            GO TO 10093
10126       CONTINUE
            L10095=    2
            GO TO 10095
10127       CONTINUE
            L10026=    4
            GO TO 10026
10128       CONTINUE
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
            L10093=    4
            GO TO 10093
10129       CONTINUE
            L10095=    3
            GO TO 10095
10130       CONTINUE
            ITMP=1000
            GO TO 10122
10125     CONTINUE
        GO TO 10119
10122   CONTINUE
        IF (.NOT.(ITMP.NE.1000.AND.(ABS(XC1U-XC2U).GT.SMLX.OR.ABS(YC1U-Y
     +C2U).GT.SMLY))) GO TO 10131
          IF (.NOT.(IMPF.NE.0.AND.PITH.GT.0.)) GO TO 10132
            UCTD=UC1D
            VCTD=VC1D
            WCTD=WC1D
            CCTD=CC1D
            XCTU=XC1U
            YCTU=YC1U
            L10114=    2
            GO TO 10114
10133       CONTINUE
10132     CONTINUE
          NCPL=NCPL+1
          XCPL(NCPL)=XC1U
          YCPL(NCPL)=YC1U
          L10026=    5
          GO TO 10026
10134     CONTINUE
          IF (.NOT.(IMPF.NE.0.AND.PITH.GT.0.)) GO TO 10135
            UCLD=UC2D
            VCLD=VC2D
            WCLD=WC2D
            CCLD=CC2D
            XCLU=XC2U
            YCLU=YC2U
10135     CONTINUE
          NCPL=1
          XCPL(1)=XC2U
          YCPL(1)=YC2U
          IF (.NOT.(IPCS.GE.0)) GO TO 10136
            CVAL=CC2D
            L10089=    3
            GO TO 10089
10137       CONTINUE
            IPCD=IPCI
10136     CONTINUE
10131   CONTINUE
      GO TO (10110) , L10111
C
C Given two points in the data-array-index coordinate system, one of
C which maps to a visible point and the other of which maps to an
C invisible point, this internal routine searches the line between
C them for a point near the edge of visibility.
C
10093 CONTINUE
        ITMP=0
10138   CONTINUE
          UCHD=(UCVD+UCID)/2.
          VCHD=(VCVD+VCID)/2.
          WCHD=(WCVD+WCID)/2.
          CCHD=(CCVD+CCID)/2.
          CALL HLUCTMXYZ (IMPF,UCHD,VCHD,WCHD,XCHU,YCHU)
          IF (ICFELL('VTTLOM',3).NE.0) GO TO 104
          IF (.NOT.(XCHU.NE.OORV.AND.YCHU.NE.OORV)) GO TO 10139
            IF (UCHD.EQ.UCVD.AND.VCHD.EQ.VCVD.AND.WCHD.EQ.WCVD) GO TO 10
     +140
            UCVD=UCHD
            VCVD=VCHD
            WCVD=WCHD
            CCVD=CCHD
            XCVU=XCHU
            YCVU=YCHU
          GO TO 10141
10139     CONTINUE
            IF (UCHD.EQ.UCID.AND.VCHD.EQ.VCID.AND.WCHD.EQ.WCID) GO TO 10
     +140
            UCID=UCHD
            VCID=VCHD
            WCID=WCHD
            CCID=CCHD
10141     CONTINUE
          ITMP=ITMP+1
          IF (ITMP.EQ.64) GO TO 10140
        GO TO 10138
10140   CONTINUE
      GO TO (10092,10105,10126,10129) , L10093
C
C The following internal procedure outputs a visible edge point found
C by the previous internal procedure.
C
10095 CONTINUE
        IF (.NOT.(PITH.GT.0.)) GO TO 10142
          IF (.NOT.(NCPL.EQ.0)) GO TO 10143
            UCLD=UCVD
            VCLD=VCVD
            WCLD=WCVD
            CCLD=CCVD
            XCLU=XCVU
            YCLU=YCVU
          GO TO 10144
10143     CONTINUE
            UCTD=UCVD
            VCTD=VCVD
            WCTD=WCVD
            CCTD=CCVD
            XCTU=XCVU
            YCTU=YCVU
            L10114=    3
            GO TO 10114
10145       CONTINUE
10144     CONTINUE
10142   CONTINUE
        NCPL=NCPL+1
        XCPL(NCPL)=XCVU
        YCPL(NCPL)=YCVU
        IF (.NOT.(IPCS.GE.0)) GO TO 10146
          CVAL=CCVD
          L10089=    4
          GO TO 10089
10147     CONTINUE
          IF (.NOT.(IPCI.NE.IPCD)) GO TO 10148
            L10099=    3
            GO TO 10099
10149       CONTINUE
            IPCD=IPCI
10148     CONTINUE
10146   CONTINUE
      GO TO (10094,10127,10130) , L10095
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
10114 CONTINUE
10150   CONTINUE
        IF (.NOT.(ABS(XCTU-XCLU).GT.PITX.OR.ABS(YCTU-YCLU).GT.PITY))
     +  GO TO 10151
          IFND=0
          UCQD=0.
          VCQD=0.
          WCQD=0.
          CCQD=0.
          RDST=.50
          RSTP=.25
10152     CONTINUE
            UCPD=UCLD+RDST*(UCTD-UCLD)
            VCPD=VCLD+RDST*(VCTD-VCLD)
            WCPD=WCLD+RDST*(WCTD-WCLD)
            CCPD=CCLD+RDST*(CCTD-CCLD)
            CALL HLUCTMXYZ (IMPF,UCPD,VCPD,WCPD,XCPU,YCPU)
            IF (ICFELL('VTTLOM',4).NE.0) GO TO 104
            IF (OORV.NE.0..AND.(XCPU.EQ.OORV.OR.YCPU.EQ.OORV)) GO TO 101
     +53
            IF (.NOT.(ABS(XCPU-XCLU).LT.PITX.AND.ABS(YCPU-YCLU).LT.PITY)
     +)     GO TO 10154
              IFND=1
              UCQD=UCPD
              VCQD=VCPD
              WCQD=WCPD
              CCQD=CCPD
              XCQU=XCPU
              YCQU=YCPU
              IF (ABS(XCQU-XCLU).GT..5*PITX.OR.ABS(YCQU-YCLU).GT..5*PITY
     +)       GO TO 10153
              RDST=RDST+RSTP
            GO TO 10155
10154       CONTINUE
              RDST=RDST-RSTP
10155       CONTINUE
            RSTP=RSTP/2.
            IF (RSTP.LT..0001) GO TO 10153
          GO TO 10152
10153     CONTINUE
          IF (.NOT.(IFND.NE.0.AND.(UCQD.NE.UCLD.OR.VCQD.NE.VCLD.OR.WCQD.
     +NE.WCLD))) GO TO 10156
            NCPL=NCPL+1
            XCPL(NCPL)=XCQU
            YCPL(NCPL)=YCQU
            IF (.NOT.(IPCS.GE.0)) GO TO 10157
              CVAL=CCQD
              L10089=    5
              GO TO 10089
10158         CONTINUE
              IF (.NOT.(IPCI.NE.IPCD)) GO TO 10159
                L10099=    4
                GO TO 10099
10160           CONTINUE
                IPCD=IPCI
10159         CONTINUE
10157       CONTINUE
            IF (.NOT.(NCPL.EQ.MCPL)) GO TO 10161
              L10099=    5
              GO TO 10099
10162         CONTINUE
10161       CONTINUE
            UCLD=UCQD
            VCLD=VCQD
            WCLD=WCQD
            CCLD=CCQD
            XCLU=XCQU
            YCLU=YCQU
          GO TO 10163
10156     CONTINUE
            UCLD=UCTD
            VCLD=VCTD
            WCLD=WCTD
            CCLD=CCTD
            XCLU=XCTU
            YCLU=YCTU
10163     CONTINUE
        GO TO 10150
10151   CONTINUE
        UCLD=UCTD
        VCLD=VCTD
        WCLD=WCTD
        CCLD=CCTD
        XCLU=XCTU
        YCLU=YCTU
      GO TO (10113,10133,10145) , L10114
C
C The following internal procedure is given the data-system coordinates
C of a point (UCND,VCND,WCND) and computes the user-system coordinates
C of the point's projection (XCNU,YCNU).  It also sets a flag indicating
C whether the projection point is visible or not.
C
10023 CONTINUE
C
        IF (.NOT.(IMPF.EQ.0)) GO TO 10164
          XCNU=UCND
          YCNU=VCND
          IVNU=1
        GO TO 10165
10164   CONTINUE
          CALL HLUCTMXYZ (IMPF,UCND,VCND,WCND,XCNU,YCNU)
          IF (ICFELL('VTTLOM',5).NE.0) GO TO 104
          IF (.NOT.((OORV.NE.0.).AND.(XCNU.EQ.OORV.OR.YCNU.EQ.OORV)))
     +    GO TO 10166
            IVNU=0
          GO TO 10167
10166     CONTINUE
            IVNU=1
10167     CONTINUE
10165   CONTINUE
C
      GO TO (10022,10032,10078) , L10023
C
C The following internal procedure, given a value (CVAL), computes a
C polyline color index (IPCI) to be used to get a desired color for a
C streamline being drawn.
C
10089 CONTINUE
10168   CONTINUE
        IF (.NOT.(ICVL.GT.1.AND.CVAL.LT.TVAL(ICVL))) GO TO 10169
          ICVL=ICVL-1
        GO TO 10168
10169   CONTINUE
10170   CONTINUE
        IF (.NOT.(ICVL.LT.NCLR.AND.CVAL.GE.TVAL(ICVL+1))) GO TO 10171
          ICVL=ICVL+1
        GO TO 10170
10171   CONTINUE
        IPCI=ICLR(ICVL)
      GO TO (10088,10116,10137,10147,10158) , L10089
C
C The following internal procedure draws the part of the curve defined
C by the contents of the polyline buffer and then removes all but the
C last point from the buffer.  IPCC is the polyline color currently
C in use and IPCD the polyline color desired for the curve.
C
10099 CONTINUE
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
          IF (.NOT.(IPCC.NE.IPCD)) GO TO 10179
            CALL GSPLCI (IPCD)
            IPCC=IPCD
10179     CONTINUE
          IF (.NOT.(IAMA(1).EQ.0)) GO TO 10180
            CALL CURVE (XCPL,YCPL,NCPL)
          GO TO 10181
10180     CONTINUE
            CALL ARDRLN (IAMA,XCPL,YCPL,NCPL,
     +                        XCPF,YCPF,MCPF,
     +                        IAAI,IAGI,MNOG,RTPL)
10181     CONTINUE
10178   CONTINUE
C
        XCPL(1)=XCPL(NCPL)
        YCPL(1)=YCPL(NCPL)
        NCPL=1
C
      GO TO (10098,10118,10149,10160,10162) , L10099
C
C The following internal procedure draws the part of the curve defined
C by the contents of the polyline buffer and then clears the buffer.
C IPCC is the polyline color currently in use and IPCD the polyline
C color desired for the curve.
C
10026 CONTINUE
C
        I=1
C
10182   CONTINUE
          I=I+1
          IF (I.GT.NCPL) GO TO 10183
          IF (.NOT.(ABS(XCPL(I)-XCPL(I-1)).LT.EPSX.AND.ABS(YCPL(I)-YCPL(
     +I-1)).LT.EPSY)) GO TO 10184
            IF (.NOT.(I.NE.NCPL)) GO TO 10185
              DO 10186 J=I+1,NCPL
                XCPL(J-1)=XCPL(J)
                YCPL(J-1)=YCPL(J)
10186         CONTINUE
            GO TO 10187
10185       CONTINUE
              XCPL(NCPL-1)=XCPL(NCPL)
              YCPL(NCPL-1)=YCPL(NCPL)
10187       CONTINUE
            I=I-1
            NCPL=NCPL-1
10184     CONTINUE
        GO TO 10182
10183   CONTINUE
C
        IF (.NOT.(NCPL.GT.1)) GO TO 10188
          IF (.NOT.(IPCC.NE.IPCD)) GO TO 10189
            CALL GSPLCI (IPCD)
            IPCC=IPCD
10189     CONTINUE
          IF (.NOT.(IAMA(1).EQ.0)) GO TO 10190
            CALL CURVE (XCPL,YCPL,NCPL)
          GO TO 10191
10190     CONTINUE
            CALL ARDRLN (IAMA,XCPL,YCPL,NCPL,
     +                        XCPF,YCPF,MCPF,
     +                        IAAI,IAGI,MNOG,RTPL)
10191     CONTINUE
10188   CONTINUE
C
        NCPL=0
        RUDN=0.
C
      GO TO (10025,10073,10107,10128,10134) , L10026
C
      END
