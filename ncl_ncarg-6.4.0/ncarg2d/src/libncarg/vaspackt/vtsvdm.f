      SUBROUTINE VTSVDM (RPNT,IEDG,ITRI,RWRK,IWRK,IAMA,RTPL)
C
      DIMENSION RPNT(*),IEDG(*),ITRI(*),RWRK(*),IWRK(*),IAMA(*)
C
      EXTERNAL RTPL
C
C This routine draws simple vectors.
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
C Declare arrays for use in calling VTCUDR.
C
      DIMENSION UCRV(2),VCRV(2),WCRV(2),CCRV(2)
C
C The following common block allows us to initialize the "seed" for the
C random number generator VTRAND.  This makes it possible to generate
C the same set of vectors for a pair of stereo views of a field.
C
      COMMON /VTSEED/ SEED
      DOUBLE PRECISION SEED
      SAVE   /VTSEED/
C
C Declare an array to be used in searching and marking all triangles
C within a specified radius of some particular triangle.
C
      DIMENSION ISTK(2,10)
C
C Define a constant used to convert from degrees to radians.
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
C Check for an uncleared prior error.
C
      IF (ICFELL('VTSVDM - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Compute "realized" values of various vector parameters.
C
      CALL VTRVSP
C
C Set a flag that says whether or not vectors will be colored.
C
      IF (.NOT.(ICTV.EQ.0.OR.NCLR.EQ.0)) GO TO 10001
        ICOL=0
      GO TO 10002
10001 CONTINUE
        ICOL=1
10002 CONTINUE
C
C Decide whether to use a simple algorithm that draws a vector in each
C unblocked triangle or a more complicated algorithm that attempts to
C cull some of them.
C
      IF (.NOT.(ISVT.EQ.0)) GO TO 10003
C
C Simple algorithm - just loop through all of the triangle nodes and
C draw a vector at the center of each triangle.
C
          IIII = 0
          GO TO 10006
10004     CONTINUE
          IIII =IIII +LOTN
10006     CONTINUE
          IF (LOTN) 10007,10008,10009
10007     CONTINUE
          IF (IIII .LT.(NTRI-LOTN)) GO TO 10005
          GO TO 10008
10009     CONTINUE
          IF (IIII .GT.(NTRI-LOTN)) GO TO 10005
10008     CONTINUE
C
          L10011=    1
          GO TO 10011
10010     CONTINUE
C
        GO TO 10004
10005   CONTINUE
C
        GO TO 103
C
10003 CONTINUE
C
C More complicated algorithm - first, initialize the "seed" for the
C random number generator used.
C
        SEED=0.D0
C
        DO 10012 I=1,IRNG
          TEMP=VTRAND()
10012   CONTINUE
C
C Get the required integer workspace.
C
        CALL VTGIWS (IWRK,1,NTRI/LOTN,IWSE)
        IF (IWSE.NE.0.OR.ICFELL('VTSVDM',2).NE.0) GO TO 102
C
C Zero the utility flags in all of the triangle nodes.
C
        DO 10013 I=0,NTRI-LOTN,LOTN
          ITRI(I+5)=0
10013   CONTINUE
C
C Generate an index array putting the triangles in random order.
C
        NIND=NTRI/LOTN
C
        DO 10014 I=1,NIND
          IWRK(II01+I)=I
10014   CONTINUE
C
        DO 10015 I=1,NIND-1
          J=MAX(I,MIN(NIND,I+INT(REAL(NIND-I+1)*VTRAND())))
          IF (.NOT.(I.NE.J)) GO TO 10016
            ITMP=IWRK(II01+I)
            IWRK(II01+I)=IWRK(II01+J)
            IWRK(II01+J)=ITMP
10016     CONTINUE
10015   CONTINUE
C
C Loop through the triangles in random order.
C
          I = 1
          GO TO 10019
10017     CONTINUE
          I =I +1
10019     CONTINUE
          IF (I .GT.(NIND)) GO TO 10018
C
          IIII=LOTN*(II01+IWRK(I)-1)
C
C Use the current triangle only if it has not been marked yet.
C
          IF (.NOT.(ITRI(IIII+5).EQ.0)) GO TO 10020
C
C Draw a vector at the center of the current triangle.
C
            L10011=    2
            GO TO 10011
10021       CONTINUE
C
C Save the coordinates of the center of the triangle.
C
            UCCS=UCCT
            VCCS=VCCT
            WCCS=WCCT
C
C Mark all triangles within ISVT steps of this one (where a "step" is
C just from one triangle to an adjacent triangle) and having centers
C within SVSR units of the center of this one.  (If SVSR is zero, we
C don't bother with computing the distance - we just mark all triangles
C within ISVT steps.)  The algorithm used here may not be the most
C efficient one; it was used because it could be implemented without
C a lot of extra memory.
C
C Initialize the stack.
C
            ILEV=1
            ISTK(1,ILEV)=IIII
            ISTK(2,ILEV)=0
C
C Try the next path from the current triangle to another one.
C
  101       CONTINUE
            IF (.NOT.(ISTK(2,ILEV).LT.3)) GO TO 10022
              ISTK(2,ILEV)=ISTK(2,ILEV)+1
              ITMP=ITRI(ISTK(1,ILEV)+ISTK(2,ILEV))
              IF (IEDG(ITMP+3).LT.0.OR.IEDG(ITMP+4).LT.0) GO TO 101
              IF (.NOT.(LOTN*((IEDG(ITMP+3)-1)/LOTN).NE.ISTK(1,ILEV)))
     +        GO TO 10023
                ITMP=LOTN*((IEDG(ITMP+3)-1)/LOTN)
              GO TO 10024
10023         CONTINUE
                ITMP=LOTN*((IEDG(ITMP+4)-1)/LOTN)
10024         CONTINUE
              IF (.NOT.(ITRI(ITMP+5).EQ.0)) GO TO 10025
                IF (.NOT.(SVSR.EQ.0.)) GO TO 10026
                  ITRI(ITMP+5)=1
                GO TO 10027
10026           CONTINUE
                  IPP1=IEDG(ITRI(ITMP+1)+1)
                  IPP2=IEDG(ITRI(ITMP+1)+2)
                  IPP3=IEDG(ITRI(ITMP+2)+1)
                  IF (IPP3.EQ.IPP1.OR.IPP3.EQ.IPP2)
     +                                       IPP3=IEDG(ITRI(ITMP+2)+2)
                  UCCT=(RPNT(IPP1+1)+RPNT(IPP2+1)+RPNT(IPP3+1))/3.
                  VCCT=(RPNT(IPP1+2)+RPNT(IPP2+2)+RPNT(IPP3+2))/3.
                  WCCT=(RPNT(IPP1+3)+RPNT(IPP2+3)+RPNT(IPP3+3))/3.
                  IF (SQRT((UCCT-UCCS)**2+
     +                     (VCCT-VCCS)**2+
     +                     (WCCT-WCCS)**2).LT.SVSR) ITRI(ITMP+5)=1
10027           CONTINUE
10025         CONTINUE
              IF (.NOT.(ILEV.LT.ISVT)) GO TO 10028
                ILEV=ILEV+1
                ISTK(1,ILEV)=ITMP
                ISTK(2,ILEV)=0
10028         CONTINUE
              GO TO 101
10022       CONTINUE
              IF (.NOT.(ILEV.GT.1)) GO TO 10029
                ILEV=ILEV-1
                GO TO 101
10029         CONTINUE
C
10020     CONTINUE
C
        GO TO 10017
10018   CONTINUE
C
C
C Release the integer workspace acquired above.
C
  102 LI01=0
C
C Done.
C
  103 RETURN
C
C The following internal procedure draws a simple vector centered on
C the triangle whose base address is IIII.
C
10011 CONTINUE
C
C Find the indices of the nodes of the vertices of the triangle (in no
C particular order).
C
        IPP1=IEDG(ITRI(IIII+1)+1)
        IPP2=IEDG(ITRI(IIII+1)+2)
        IPP3=IEDG(ITRI(IIII+2)+1)
        IF (IPP3.EQ.IPP1.OR.IPP3.EQ.IPP2) IPP3=IEDG(ITRI(IIII+2)+2)
C
C Extract the coordinates of the vertices of the triangle.
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
C Compute the coordinates of the center of the triangle.
C
        UCCT=(UCP1+UCP2+UCP3)/3.
        VCCT=(VCP1+VCP2+VCP3)/3.
        WCCT=(WCP1+WCP2+WCP3)/3.
C
C Skip the triangle if it's blocked.
C
        IF (ITBF(ITRI(IIII+4)).NE.0) GO TO 104
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
C are not well-defined, skip the triangle.
C
        DNOM=SQRT(A**2+B**2+C**2)
C
        IF (DNOM.EQ.0.) GO TO 104
C
        DCNU=A/DNOM
        DCNV=B/DNOM
        DCNW=C/DNOM
C
C Compute the components of the velocity vector at the center of the
C triangle and its magnitude.
C
        UCVV=(RPNT(IPP1+4)+RPNT(IPP2+4)+RPNT(IPP3+4))/3.
        VCVV=(RPNT(IPP1+5)+RPNT(IPP2+5)+RPNT(IPP3+5))/3.
        WCVV=(RPNT(IPP1+6)+RPNT(IPP2+6)+RPNT(IPP3+6))/3.
C
        VMAG=SQRT(UCVV**2+VCVV**2+WCVV**2)
C
        VLEN=VFRA*VRLR+(1.-VFRA)*VRLR*(VMAG/VRMR)
C
        VLEN=VLEN/2.
C
C Draw the simple vector.  WE MAY WANT TO HAVE OTHER CHOICES HERE.  FOR
C EXAMPLE, IT MAY BE DESIRABLE TO FIND THE ORIENTATION OF THE VECTOR BY
C PROJECTING THE END POINTS OF A TINY PORTION OF IT AND THEN USE THAT
C FOR A VECTOR OF A SPECIFIED SIZE IN THE PROJECTION SPACE.
C
        UCRV(1)=UCCT-VLEN*UCVV/VMAG
        VCRV(1)=VCCT-VLEN*VCVV/VMAG
        WCRV(1)=WCCT-VLEN*WCVV/VMAG
        CCRV(1)=VMAG
C
        UCRV(2)=UCCT+VLEN*UCVV/VMAG
        VCRV(2)=VCCT+VLEN*VCVV/VMAG
        WCRV(2)=WCCT+VLEN*WCVV/VMAG
        CCRV(2)=VMAG
C
        CALL VTCUDR (UCRV,VCRV,WCRV,CCRV,2,ICOL,1,IAMA,RTPL)
C
  104 CONTINUE
      GO TO (10010,10021) , L10011
C
      END
