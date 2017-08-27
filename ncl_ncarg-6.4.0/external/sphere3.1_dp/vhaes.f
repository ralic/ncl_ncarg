c
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c  .                                                             .
c  .                  copyright (c) 1998 by UCAR                 .
c  .                                                             .
c  .       University Corporation for Atmospheric Research       .
c  .                                                             .
c  .                      all rights reserved                    .
c  .                                                             .
c  .                                                             .
c  .                         SPHEREPACK                          .
c  .                                                             .
c  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
c
c
c
c ... file vhaes.f
c
c     this file contains code and documentation for subroutines
c     vhaes and vhaesi
c
c ... files which must be loaded with vhaes.f
c
c     sphcom.f, hrfft.f
c
c
c     subroutine vhaes(nlat,nlon,ityp,nt,v,w,idvw,jdvw,br,bi,cr,ci,
c    +                 mdab,ndab,wvhaes,lvhaes,work,lwork,ierror)
c
c     subroutine vhaes performs the vector spherical harmonic analysis
c     on the vector field (v,w) and stores the result in the arrays
c     br, bi, cr, and ci. v(i,j) and w(i,j) are the colatitudinal
c     (measured from the north pole) and east longitudinal components
c     respectively, located at colatitude theta(i) = (i-1)*pi/(nlat-1)
c     and longitude phi(j) = (j-1)*2*pi/nlon. the spectral
c     representation of (v,w) is given at output parameters v,w in
c     subroutine vhses.
c
c     input parameters
c
c     nlat   the number of colatitudes on the full sphere including the
c            poles. for example, nlat = 37 for a five degree grid.
c            nlat determines the grid increment in colatitude as
c            pi/(nlat-1).  if nlat is odd the equator is located at
c            grid point i=(nlat+1)/2. if nlat is even the equator is
c            located half way between points i=nlat/2 and i=nlat/2+1.
c            nlat must be at least 3. note: on the half sphere, the
c            number of grid points in the colatitudinal direction is
c            nlat/2 if nlat is even or (nlat+1)/2 if nlat is odd.
c
c     nlon   the number of distinct londitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater
c            than zero. the axisymmetric case corresponds to nlon=1.
c            the efficiency of the computation is improved when nlon
c            is a product of small prime numbers.
c
c     ityp   = 0  no symmetries exist about the equator. the analysis
c                 is performed on the entire sphere.  i.e. on the
c                 arrays v(i,j),w(i,j) for i=1,...,nlat and
c                 j=1,...,nlon.
c
c            = 1  no symmetries exist about the equator. the analysis
c                 is performed on the entire sphere.  i.e. on the
c                 arrays v(i,j),w(i,j) for i=1,...,nlat and
c                 j=1,...,nlon. the curl of (v,w) is zero. that is,
c                 (d/dtheta (sin(theta) w) - dv/dphi)/sin(theta) = 0.
c                 the coefficients cr and ci are zero.
c
c            = 2  no symmetries exist about the equator. the analysis
c                 is performed on the entire sphere.  i.e. on the
c                 arrays v(i,j),w(i,j) for i=1,...,nlat and
c                 j=1,...,nlon. the divergence of (v,w) is zero. i.e.,
c                 (d/dtheta (sin(theta) v) + dw/dphi)/sin(theta) = 0.
c                 the coefficients br and bi are zero.
c
c            = 3  v is symmetric and w is antisymmetric about the
c                 equator. the analysis is performed on the northern
c                 hemisphere only.  i.e., if nlat is odd the analysis
c                 is performed on the arrays v(i,j),w(i,j) for
c                 i=1,...,(nlat+1)/2 and j=1,...,nlon. if nlat is
c                 even the analysis is performed on the the arrays
c                 v(i,j),w(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c            = 4  v is symmetric and w is antisymmetric about the
c                 equator. the analysis is performed on the northern
c                 hemisphere only.  i.e., if nlat is odd the analysis
c                 is performed on the arrays v(i,j),w(i,j) for
c                 i=1,...,(nlat+1)/2 and j=1,...,nlon. if nlat is
c                 even the analysis is performed on the the arrays
c                 v(i,j),w(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c                 the curl of (v,w) is zero. that is,
c                 (d/dtheta (sin(theta) w) - dv/dphi)/sin(theta) = 0.
c                 the coefficients cr and ci are zero.
c
c            = 5  v is symmetric and w is antisymmetric about the
c                 equator. the analysis is performed on the northern
c                 hemisphere only.  i.e., if nlat is odd the analysis
c                 is performed on the arrays v(i,j),w(i,j) for
c                 i=1,...,(nlat+1)/2 and j=1,...,nlon. if nlat is
c                 even the analysis is performed on the the arrays
c                 v(i,j),w(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c                 the divergence of (v,w) is zero. i.e.,
c                 (d/dtheta (sin(theta) v) + dw/dphi)/sin(theta) = 0.
c                 the coefficients br and bi are zero.
c
c            = 6  v is antisymmetric and w is symmetric about the
c                 equator. the analysis is performed on the northern
c                 hemisphere only.  i.e., if nlat is odd the analysis
c                 is performed on the arrays v(i,j),w(i,j) for
c                 i=1,...,(nlat+1)/2 and j=1,...,nlon. if nlat is
c                 even the analysis is performed on the the arrays
c                 v(i,j),w(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c
c            = 7  v is antisymmetric and w is symmetric about the
c                 equator. the analysis is performed on the northern
c                 hemisphere only.  i.e., if nlat is odd the analysis
c                 is performed on the arrays v(i,j),w(i,j) for
c                 i=1,...,(nlat+1)/2 and j=1,...,nlon. if nlat is
c                 even the analysis is performed on the the arrays
c                 v(i,j),w(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c                 the curl of (v,w) is zero. that is,
c                 (d/dtheta (sin(theta) w) - dv/dphi)/sin(theta) = 0.
c                 the coefficients cr and ci are zero.
c
c            = 8  v is antisymmetric and w is symmetric about the
c                 equator. the analysis is performed on the northern
c                 hemisphere only.  i.e., if nlat is odd the analysis
c                 is performed on the arrays v(i,j),w(i,j) for
c                 i=1,...,(nlat+1)/2 and j=1,...,nlon. if nlat is
c                 even the analysis is performed on the the arrays
c                 v(i,j),w(i,j) for i=1,...,nlat/2 and j=1,...,nlon.
c                 the divergence of (v,w) is zero. i.e.,
c                 (d/dtheta (sin(theta) v) + dw/dphi)/sin(theta) = 0.
c                 the coefficients br and bi are zero.
c
c
c     nt     the number of analyses.  in the program that calls vhaes,
c            the arrays v,w,br,bi,cr, and ci can be three dimensional
c            in which case multiple analyses will be performed.
c            the third index is the analysis index which assumes the
c            values k=1,...,nt.  for a single analysis set nt=1. the
c            discription of the remaining parameters is simplified
c            by assuming that nt=1 or that all the arrays are two
c            dimensional.
c
c     v,w    two or three dimensional arrays (see input parameter nt)
c            that contain the vector function to be analyzed.
c            v is the colatitudnal component and w is the east
c            longitudinal component. v(i,j),w(i,j) contain the
c            components at colatitude theta(i) = (i-1)*pi/(nlat-1)
c            and longitude phi(j) = (j-1)*2*pi/nlon. the index ranges
c            are defined above at the input parameter ityp.
c
c     idvw   the first dimension of the arrays v,w as it appears in
c            the program that calls vhaes. if ityp .le. 2 then idvw
c            must be at least nlat.  if ityp .gt. 2 and nlat is
c            even then idvw must be at least nlat/2. if ityp .gt. 2
c            and nlat is odd then idvw must be at least (nlat+1)/2.
c
c     jdvw   the second dimension of the arrays v,w as it appears in
c            the program that calls vhaes. jdvw must be at least nlon.
c
c     mdab   the first dimension of the arrays br,bi,cr, and ci as it
c            appears in the program that calls vhaes. mdab must be at
c            least min0(nlat,nlon/2) if nlon is even or at least
c            min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     ndab   the second dimension of the arrays br,bi,cr, and ci as it
c            appears in the program that calls vhaes. ndab must be at
c            least nlat.
c
c     lvhaes an array which must be initialized by subroutine vhaesi.
c            once initialized, wvhaes can be used repeatedly by vhaes
c            as long as nlon and nlat remain unchanged.  wvhaes must
c            not be altered between calls of vhaes.
c
c     lvhaes the dimension of the array wvhaes as it appears in the
c            program that calls vhaes. define
c
c               l1 = min0(nlat,nlon/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lvhaes must be at least
c
c            l1*l2(nlat+nlat-l1+1)+nlon+15
c
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls vhaes. define
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            if ityp .le. 2 then lwork must be at least
c
c                       (2*nt+1)*nlat*nlon
c
c            if ityp .gt. 2 then lwork must be at least
c
c                        (2*nt+1)*l2*nlon
c
c     **************************************************************
c
c     output parameters
c
c     br,bi  two or three dimensional arrays (see input parameter nt)
c     cr,ci  that contain the vector spherical harmonic coefficients
c            in the spectral representation of v(i,j) and w(i,j) given
c            in the discription of subroutine vhses. br(mp1,np1),
c            bi(mp1,np1),cr(mp1,np1), and ci(mp1,np1) are computed
c            for mp1=1,...,mmax and np1=mp1,...,nlat except for np1=nlat
c            and odd mp1. mmax=min0(nlat,nlon/2) if nlon is even or
c            mmax=min0(nlat,(nlon+1)/2) if nlon is odd.
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of ityp
c            = 4  error in the specification of nt
c            = 5  error in the specification of idvw
c            = 6  error in the specification of jdvw
c            = 7  error in the specification of mdab
c            = 8  error in the specification of ndab
c            = 9  error in the specification of lvhaes
c            = 10 error in the specification of lwork
c
c ********************************************************
c
c     subroutine vhaesi(nlat,nlon,wvhaes,lvhaes,work,lwork,dwork,
c    +                  ldwork,ierror)
c
c     subroutine vhaesi initializes the array wvhaes which can then be
c     used repeatedly by subroutine vhaes until nlat or nlon is changed.
c
c     input parameters
c
c     nlat   the number of colatitudes on the full sphere including the
c            poles. for example, nlat = 37 for a five degree grid.
c            nlat determines the grid increment in colatitude as
c            pi/(nlat-1).  if nlat is odd the equator is located at
c            grid point i=(nlat+1)/2. if nlat is even the equator is
c            located half way between points i=nlat/2 and i=nlat/2+1.
c            nlat must be at least 3. note: on the half sphere, the
c            number of grid points in the colatitudinal direction is
c            nlat/2 if nlat is even or (nlat+1)/2 if nlat is odd.
c
c     nlon   the number of distinct londitude points.  nlon determines
c            the grid increment in longitude as 2*pi/nlon. for example
c            nlon = 72 for a five degree grid. nlon must be greater
c            than zero. the axisymmetric case corresponds to nlon=1.
c            the efficiency of the computation is improved when nlon
c            is a product of small prime numbers.
c
c     lvhaes the dimension of the array wvhaes as it appears in the
c            program that calls vhaes. define
c
c               l1 = min0(nlat,nlon/2) if nlon is even or
c               l1 = min0(nlat,(nlon+1)/2) if nlon is odd
c
c            and
c
c               l2 = nlat/2        if nlat is even or
c               l2 = (nlat+1)/2    if nlat is odd
c
c            then lvhaes must be at least
c
c               l1*l2*(nlat+nlat-l1+1)+nlon+15
c
c
c     work   a work array that does not have to be saved.
c
c     lwork  the dimension of the array work as it appears in the
c            program that calls vhaes. lwork must be at least
c
c              3*(max0(l1-2,0)*(nlat+nlat-l1-1))/2+5*l2*nlat
c
c     dwork  an unsaved double precision work space
c
c     ldwork the length of the array dwork as it appears in the
c            program that calls vhaesi.  ldwork must be at least
c            2*(nlat+1)
c
c
c     **************************************************************
c
c     output parameters
c
c     wvhaes an array which is initialized for use by subroutine vhaes.
c            once initialized, wvhaes can be used repeatedly by vhaes
c            as long as nlat or nlon remain unchanged.  wvhaes must not
c            be altered between calls of vhaes.
c
c
c     ierror = 0  no errors
c            = 1  error in the specification of nlat
c            = 2  error in the specification of nlon
c            = 3  error in the specification of lvhaes
c            = 4  error in the specification of lwork
c            = 5  error in the specification of ldwork
c
c
      SUBROUTINE DVHAES(NLAT,NLON,ITYP,NT,V,W,IDVW,JDVW,BR,BI,CR,CI,
     +                  MDAB,NDAB,WVHAES,LVHAES,WORK,LWORK,IERROR)
      DOUBLE PRECISION V
      DOUBLE PRECISION W
      DOUBLE PRECISION BR
      DOUBLE PRECISION BI
      DOUBLE PRECISION CR
      DOUBLE PRECISION CI
      DOUBLE PRECISION WVHAES
      DOUBLE PRECISION WORK
      DIMENSION V(IDVW,JDVW,1),W(IDVW,JDVW,1),BR(MDAB,NDAB,1),
     +          BI(MDAB,NDAB,1),CR(MDAB,NDAB,1),CI(MDAB,NDAB,1),WORK(1),
     +          WVHAES(1)

      IERROR = 1
      IF (NLAT.LT.3) RETURN
      IERROR = 2
      IF (NLON.LT.1) RETURN
      IERROR = 3
      IF (ITYP.LT.0 .OR. ITYP.GT.8) RETURN
      IERROR = 4
      IF (NT.LT.0) RETURN
      IERROR = 5
      IMID = (NLAT+1)/2
      IF ((ITYP.LE.2.AND.IDVW.LT.NLAT) .OR.
     +    (ITYP.GT.2.AND.IDVW.LT.IMID)) RETURN
      IERROR = 6
      IF (JDVW.LT.NLON) RETURN
      IERROR = 7
      MMAX = MIN0(NLAT, (NLON+1)/2)
      IF (MDAB.LT.MMAX) RETURN
      IERROR = 8
      IF (NDAB.LT.NLAT) RETURN
      IERROR = 9
      IDZ = (MMAX* (NLAT+NLAT-MMAX+1))/2
      LZIMN = IDZ*IMID
      IF (LVHAES.LT.LZIMN+LZIMN+NLON+15) RETURN
      IERROR = 10
      IDV = NLAT
      IF (ITYP.GT.2) IDV = IMID
      LNL = NT*IDV*NLON
      IF (LWORK.LT.LNL+LNL+IDV*NLON) RETURN
      IERROR = 0
      IST = 0
      IF (ITYP.LE.2) IST = IMID
      IW1 = IST + 1
      IW2 = LNL + 1
      IW3 = IW2 + IST
      IW4 = IW2 + LNL
      JW1 = LZIMN + 1
      JW2 = JW1 + LZIMN
      CALL DVHAES1(NLAT,NLON,ITYP,NT,IMID,IDVW,JDVW,V,W,MDAB,NDAB,BR,
     +             BI,CR,CI,IDV,WORK,WORK(IW1),WORK(IW2),WORK(IW3),
     +             WORK(IW4),IDZ,WVHAES,WVHAES(JW1),WVHAES(JW2))
      RETURN
      END

      SUBROUTINE DVHAES1(NLAT,NLON,ITYP,NT,IMID,IDVW,JDVW,V,W,MDAB,NDAB,
     +                  BR,BI,CR,CI,IDV,VE,VO,WE,WO,WORK,IDZ,ZV,ZW,
     +                  WRFFT)
      DOUBLE PRECISION V
      DOUBLE PRECISION W
      DOUBLE PRECISION BR
      DOUBLE PRECISION BI
      DOUBLE PRECISION CR
      DOUBLE PRECISION CI
      DOUBLE PRECISION VE
      DOUBLE PRECISION VO
      DOUBLE PRECISION WE
      DOUBLE PRECISION WO
      DOUBLE PRECISION WORK
      DOUBLE PRECISION ZV
      DOUBLE PRECISION ZW
      DOUBLE PRECISION WRFFT
      DOUBLE PRECISION TSN
      DOUBLE PRECISION FSN
      DIMENSION V(IDVW,JDVW,1),W(IDVW,JDVW,1),BR(MDAB,NDAB,1),
     +          BI(MDAB,NDAB,1),CR(MDAB,NDAB,1),CI(MDAB,NDAB,1),
     +          VE(IDV,NLON,1),VO(IDV,NLON,1),WE(IDV,NLON,1),
     +          WO(IDV,NLON,1),WORK(1),WRFFT(1),ZV(IDZ,1),ZW(IDZ,1)

      NLP1 = NLAT + 1
      TSN = 2.D0/NLON
      FSN = 4.D0/NLON
      MLAT = MOD(NLAT,2)
      MLON = MOD(NLON,2)
      MMAX = MIN0(NLAT, (NLON+1)/2)
      IMM1 = IMID
      IF (MLAT.NE.0) IMM1 = IMID - 1
      IF (ITYP.GT.2) GO TO 3
      DO 5 K = 1,NT
          DO 5 I = 1,IMM1
              DO 5 J = 1,NLON
                  VE(I,J,K) = TSN* (V(I,J,K)+V(NLP1-I,J,K))
                  VO(I,J,K) = TSN* (V(I,J,K)-V(NLP1-I,J,K))
                  WE(I,J,K) = TSN* (W(I,J,K)+W(NLP1-I,J,K))
                  WO(I,J,K) = TSN* (W(I,J,K)-W(NLP1-I,J,K))
    5 CONTINUE
      GO TO 2
    3 DO 8 K = 1,NT
          DO 8 I = 1,IMM1
              DO 8 J = 1,NLON
                  VE(I,J,K) = FSN*V(I,J,K)
                  VO(I,J,K) = FSN*V(I,J,K)
                  WE(I,J,K) = FSN*W(I,J,K)
                  WO(I,J,K) = FSN*W(I,J,K)
    8 CONTINUE
    2 IF (MLAT.EQ.0) GO TO 7
      DO 6 K = 1,NT
          DO 6 J = 1,NLON
              VE(IMID,J,K) = TSN*V(IMID,J,K)
              WE(IMID,J,K) = TSN*W(IMID,J,K)
    6 CONTINUE
    7 DO 9 K = 1,NT
          CALL DHRFFTF(IDV,NLON,VE(1,1,K),IDV,WRFFT,WORK)
          CALL DHRFFTF(IDV,NLON,WE(1,1,K),IDV,WRFFT,WORK)
    9 CONTINUE
      NDO1 = NLAT
      NDO2 = NLAT
      IF (MLAT.NE.0) NDO1 = NLAT - 1
      IF (MLAT.EQ.0) NDO2 = NLAT - 1
      IF (ITYP.EQ.2 .OR. ITYP.EQ.5 .OR. ITYP.EQ.8) GO TO 11
      DO 10 K = 1,NT
          DO 10 MP1 = 1,MMAX
              DO 10 NP1 = MP1,NLAT
                  BR(MP1,NP1,K) = 0.D0
                  BI(MP1,NP1,K) = 0.D0
   10 CONTINUE
   11 IF (ITYP.EQ.1 .OR. ITYP.EQ.4 .OR. ITYP.EQ.7) GO TO 13
      DO 12 K = 1,NT
          DO 12 MP1 = 1,MMAX
              DO 12 NP1 = MP1,NLAT
                  CR(MP1,NP1,K) = 0.D0
                  CI(MP1,NP1,K) = 0.D0
   12 CONTINUE
   13 ITYPP = ITYP + 1
      GO TO (1,100,200,300,400,500,600,700,800) ITYPP
c
c     case ityp=0 ,  no symmetries
c
c     case m=0
c
    1 DO 15 K = 1,NT
          DO 15 I = 1,IMID
              DO 15 NP1 = 2,NDO2,2
                  BR(1,NP1,K) = BR(1,NP1,K) + ZV(NP1,I)*VE(I,1,K)
                  CR(1,NP1,K) = CR(1,NP1,K) - ZV(NP1,I)*WE(I,1,K)
   15 CONTINUE
      DO 16 K = 1,NT
          DO 16 I = 1,IMM1
              DO 16 NP1 = 3,NDO1,2
                  BR(1,NP1,K) = BR(1,NP1,K) + ZV(NP1,I)*VO(I,1,K)
                  CR(1,NP1,K) = CR(1,NP1,K) - ZV(NP1,I)*WO(I,1,K)
   16 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) RETURN
      DO 20 MP1 = 2,MMAX
          M = MP1 - 1
          MB = M* (NLAT-1) - (M* (M-1))/2
          MP2 = MP1 + 1
          IF (MP1.GT.NDO1) GO TO 17
          DO 23 K = 1,NT
              DO 23 I = 1,IMM1
                  DO 23 NP1 = MP1,NDO1,2
                      BR(MP1,NP1,K) = BR(MP1,NP1,K) +
     +                                ZV(NP1+MB,I)*VO(I,2*MP1-2,K) +
     +                                ZW(NP1+MB,I)*WE(I,2*MP1-1,K)
                      BI(MP1,NP1,K) = BI(MP1,NP1,K) +
     +                                ZV(NP1+MB,I)*VO(I,2*MP1-1,K) -
     +                                ZW(NP1+MB,I)*WE(I,2*MP1-2,K)
                      CR(MP1,NP1,K) = CR(MP1,NP1,K) -
     +                                ZV(NP1+MB,I)*WO(I,2*MP1-2,K) +
     +                                ZW(NP1+MB,I)*VE(I,2*MP1-1,K)
                      CI(MP1,NP1,K) = CI(MP1,NP1,K) -
     +                                ZV(NP1+MB,I)*WO(I,2*MP1-1,K) -
     +                                ZW(NP1+MB,I)*VE(I,2*MP1-2,K)
   23     CONTINUE
          IF (MLAT.EQ.0) GO TO 17
          DO 24 K = 1,NT
              DO 24 NP1 = MP1,NDO1,2
                  BR(MP1,NP1,K) = BR(MP1,NP1,K) +
     +                            ZW(NP1+MB,IMID)*WE(IMID,2*MP1-1,K)
                  BI(MP1,NP1,K) = BI(MP1,NP1,K) -
     +                            ZW(NP1+MB,IMID)*WE(IMID,2*MP1-2,K)
                  CR(MP1,NP1,K) = CR(MP1,NP1,K) +
     +                            ZW(NP1+MB,IMID)*VE(IMID,2*MP1-1,K)
                  CI(MP1,NP1,K) = CI(MP1,NP1,K) -
     +                            ZW(NP1+MB,IMID)*VE(IMID,2*MP1-2,K)
   24     CONTINUE
   17     IF (MP2.GT.NDO2) GO TO 20
          DO 21 K = 1,NT
              DO 21 I = 1,IMM1
                  DO 21 NP1 = MP2,NDO2,2
                      BR(MP1,NP1,K) = BR(MP1,NP1,K) +
     +                                ZV(NP1+MB,I)*VE(I,2*MP1-2,K) +
     +                                ZW(NP1+MB,I)*WO(I,2*MP1-1,K)
                      BI(MP1,NP1,K) = BI(MP1,NP1,K) +
     +                                ZV(NP1+MB,I)*VE(I,2*MP1-1,K) -
     +                                ZW(NP1+MB,I)*WO(I,2*MP1-2,K)
                      CR(MP1,NP1,K) = CR(MP1,NP1,K) -
     +                                ZV(NP1+MB,I)*WE(I,2*MP1-2,K) +
     +                                ZW(NP1+MB,I)*VO(I,2*MP1-1,K)
                      CI(MP1,NP1,K) = CI(MP1,NP1,K) -
     +                                ZV(NP1+MB,I)*WE(I,2*MP1-1,K) -
     +                                ZW(NP1+MB,I)*VO(I,2*MP1-2,K)
   21     CONTINUE
          IF (MLAT.EQ.0) GO TO 20
          DO 22 K = 1,NT
              DO 22 NP1 = MP2,NDO2,2
                  BR(MP1,NP1,K) = BR(MP1,NP1,K) +
     +                            ZV(NP1+MB,IMID)*VE(IMID,2*MP1-2,K)
                  BI(MP1,NP1,K) = BI(MP1,NP1,K) +
     +                            ZV(NP1+MB,IMID)*VE(IMID,2*MP1-1,K)
                  CR(MP1,NP1,K) = CR(MP1,NP1,K) -
     +                            ZV(NP1+MB,IMID)*WE(IMID,2*MP1-2,K)
                  CI(MP1,NP1,K) = CI(MP1,NP1,K) -
     +                            ZV(NP1+MB,IMID)*WE(IMID,2*MP1-1,K)
   22     CONTINUE
   20 CONTINUE
      RETURN
c
c     case ityp=1 ,  no symmetries but cr and ci equal zero
c
c     case m=0
c
  100 DO 115 K = 1,NT
          DO 115 I = 1,IMID
              DO 115 NP1 = 2,NDO2,2
                  BR(1,NP1,K) = BR(1,NP1,K) + ZV(NP1,I)*VE(I,1,K)
  115 CONTINUE
      DO 116 K = 1,NT
          DO 116 I = 1,IMM1
              DO 116 NP1 = 3,NDO1,2
                  BR(1,NP1,K) = BR(1,NP1,K) + ZV(NP1,I)*VO(I,1,K)
  116 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) RETURN
      DO 120 MP1 = 2,MMAX
          M = MP1 - 1
          MB = M* (NLAT-1) - (M* (M-1))/2
          MP2 = MP1 + 1
          IF (MP1.GT.NDO1) GO TO 117
          DO 123 K = 1,NT
              DO 123 I = 1,IMM1
                  DO 123 NP1 = MP1,NDO1,2
                      BR(MP1,NP1,K) = BR(MP1,NP1,K) +
     +                                ZV(NP1+MB,I)*VO(I,2*MP1-2,K) +
     +                                ZW(NP1+MB,I)*WE(I,2*MP1-1,K)
                      BI(MP1,NP1,K) = BI(MP1,NP1,K) +
     +                                ZV(NP1+MB,I)*VO(I,2*MP1-1,K) -
     +                                ZW(NP1+MB,I)*WE(I,2*MP1-2,K)
  123     CONTINUE
          IF (MLAT.EQ.0) GO TO 117
          DO 124 K = 1,NT
              DO 124 NP1 = MP1,NDO1,2
                  BR(MP1,NP1,K) = BR(MP1,NP1,K) +
     +                            ZW(NP1+MB,IMID)*WE(IMID,2*MP1-1,K)
                  BI(MP1,NP1,K) = BI(MP1,NP1,K) -
     +                            ZW(NP1+MB,IMID)*WE(IMID,2*MP1-2,K)
  124     CONTINUE
  117     IF (MP2.GT.NDO2) GO TO 120
          DO 121 K = 1,NT
              DO 121 I = 1,IMM1
                  DO 121 NP1 = MP2,NDO2,2
                      BR(MP1,NP1,K) = BR(MP1,NP1,K) +
     +                                ZV(NP1+MB,I)*VE(I,2*MP1-2,K) +
     +                                ZW(NP1+MB,I)*WO(I,2*MP1-1,K)
                      BI(MP1,NP1,K) = BI(MP1,NP1,K) +
     +                                ZV(NP1+MB,I)*VE(I,2*MP1-1,K) -
     +                                ZW(NP1+MB,I)*WO(I,2*MP1-2,K)
  121     CONTINUE
          IF (MLAT.EQ.0) GO TO 120
          DO 122 K = 1,NT
              DO 122 NP1 = MP2,NDO2,2
                  BR(MP1,NP1,K) = BR(MP1,NP1,K) +
     +                            ZV(NP1+MB,IMID)*VE(IMID,2*MP1-2,K)
                  BI(MP1,NP1,K) = BI(MP1,NP1,K) +
     +                            ZV(NP1+MB,IMID)*VE(IMID,2*MP1-1,K)
  122     CONTINUE
  120 CONTINUE
      RETURN
c
c     case ityp=2 ,  no symmetries but br and bi equal zero
c
c     case m=0
c
  200 DO 215 K = 1,NT
          DO 215 I = 1,IMID
              DO 215 NP1 = 2,NDO2,2
                  CR(1,NP1,K) = CR(1,NP1,K) - ZV(NP1,I)*WE(I,1,K)
  215 CONTINUE
      DO 216 K = 1,NT
          DO 216 I = 1,IMM1
              DO 216 NP1 = 3,NDO1,2
                  CR(1,NP1,K) = CR(1,NP1,K) - ZV(NP1,I)*WO(I,1,K)
  216 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) RETURN
      DO 220 MP1 = 2,MMAX
          M = MP1 - 1
          MB = M* (NLAT-1) - (M* (M-1))/2
          MP2 = MP1 + 1
          IF (MP1.GT.NDO1) GO TO 217
          DO 223 K = 1,NT
              DO 223 I = 1,IMM1
                  DO 223 NP1 = MP1,NDO1,2
                      CR(MP1,NP1,K) = CR(MP1,NP1,K) -
     +                                ZV(NP1+MB,I)*WO(I,2*MP1-2,K) +
     +                                ZW(NP1+MB,I)*VE(I,2*MP1-1,K)
                      CI(MP1,NP1,K) = CI(MP1,NP1,K) -
     +                                ZV(NP1+MB,I)*WO(I,2*MP1-1,K) -
     +                                ZW(NP1+MB,I)*VE(I,2*MP1-2,K)
  223     CONTINUE
          IF (MLAT.EQ.0) GO TO 217
          DO 224 K = 1,NT
              DO 224 NP1 = MP1,NDO1,2
                  CR(MP1,NP1,K) = CR(MP1,NP1,K) +
     +                            ZW(NP1+MB,IMID)*VE(IMID,2*MP1-1,K)
                  CI(MP1,NP1,K) = CI(MP1,NP1,K) -
     +                            ZW(NP1+MB,IMID)*VE(IMID,2*MP1-2,K)
  224     CONTINUE
  217     IF (MP2.GT.NDO2) GO TO 220
          DO 221 K = 1,NT
              DO 221 I = 1,IMM1
                  DO 221 NP1 = MP2,NDO2,2
                      CR(MP1,NP1,K) = CR(MP1,NP1,K) -
     +                                ZV(NP1+MB,I)*WE(I,2*MP1-2,K) +
     +                                ZW(NP1+MB,I)*VO(I,2*MP1-1,K)
                      CI(MP1,NP1,K) = CI(MP1,NP1,K) -
     +                                ZV(NP1+MB,I)*WE(I,2*MP1-1,K) -
     +                                ZW(NP1+MB,I)*VO(I,2*MP1-2,K)
  221     CONTINUE
          IF (MLAT.EQ.0) GO TO 220
          DO 222 K = 1,NT
              DO 222 NP1 = MP2,NDO2,2
                  CR(MP1,NP1,K) = CR(MP1,NP1,K) -
     +                            ZV(NP1+MB,IMID)*WE(IMID,2*MP1-2,K)
                  CI(MP1,NP1,K) = CI(MP1,NP1,K) -
     +                            ZV(NP1+MB,IMID)*WE(IMID,2*MP1-1,K)
  222     CONTINUE
  220 CONTINUE
      RETURN
c
c     case ityp=3 ,  v even , w odd
c
c     case m=0
c
  300 DO 315 K = 1,NT
          DO 315 I = 1,IMID
              DO 315 NP1 = 2,NDO2,2
                  BR(1,NP1,K) = BR(1,NP1,K) + ZV(NP1,I)*VE(I,1,K)
  315 CONTINUE
      DO 316 K = 1,NT
          DO 316 I = 1,IMM1
              DO 316 NP1 = 3,NDO1,2
                  CR(1,NP1,K) = CR(1,NP1,K) - ZV(NP1,I)*WO(I,1,K)
  316 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) RETURN
      DO 320 MP1 = 2,MMAX
          M = MP1 - 1
          MB = M* (NLAT-1) - (M* (M-1))/2
          MP2 = MP1 + 1
          IF (MP1.GT.NDO1) GO TO 317
          DO 323 K = 1,NT
              DO 323 I = 1,IMM1
                  DO 323 NP1 = MP1,NDO1,2
                      CR(MP1,NP1,K) = CR(MP1,NP1,K) -
     +                                ZV(NP1+MB,I)*WO(I,2*MP1-2,K) +
     +                                ZW(NP1+MB,I)*VE(I,2*MP1-1,K)
                      CI(MP1,NP1,K) = CI(MP1,NP1,K) -
     +                                ZV(NP1+MB,I)*WO(I,2*MP1-1,K) -
     +                                ZW(NP1+MB,I)*VE(I,2*MP1-2,K)
  323     CONTINUE
          IF (MLAT.EQ.0) GO TO 317
          DO 324 K = 1,NT
              DO 324 NP1 = MP1,NDO1,2
                  CR(MP1,NP1,K) = CR(MP1,NP1,K) +
     +                            ZW(NP1+MB,IMID)*VE(IMID,2*MP1-1,K)
                  CI(MP1,NP1,K) = CI(MP1,NP1,K) -
     +                            ZW(NP1+MB,IMID)*VE(IMID,2*MP1-2,K)
  324     CONTINUE
  317     IF (MP2.GT.NDO2) GO TO 320
          DO 321 K = 1,NT
              DO 321 I = 1,IMM1
                  DO 321 NP1 = MP2,NDO2,2
                      BR(MP1,NP1,K) = BR(MP1,NP1,K) +
     +                                ZV(NP1+MB,I)*VE(I,2*MP1-2,K) +
     +                                ZW(NP1+MB,I)*WO(I,2*MP1-1,K)
                      BI(MP1,NP1,K) = BI(MP1,NP1,K) +
     +                                ZV(NP1+MB,I)*VE(I,2*MP1-1,K) -
     +                                ZW(NP1+MB,I)*WO(I,2*MP1-2,K)
  321     CONTINUE
          IF (MLAT.EQ.0) GO TO 320
          DO 322 K = 1,NT
              DO 322 NP1 = MP2,NDO2,2
                  BR(MP1,NP1,K) = BR(MP1,NP1,K) +
     +                            ZV(NP1+MB,IMID)*VE(IMID,2*MP1-2,K)
                  BI(MP1,NP1,K) = BI(MP1,NP1,K) +
     +                            ZV(NP1+MB,IMID)*VE(IMID,2*MP1-1,K)
  322     CONTINUE
  320 CONTINUE
      RETURN
c
c     case ityp=4 ,  v even, w odd, and cr and ci equal 0.
c
c     case m=0
c
  400 DO 415 K = 1,NT
          DO 415 I = 1,IMID
              DO 415 NP1 = 2,NDO2,2
                  BR(1,NP1,K) = BR(1,NP1,K) + ZV(NP1,I)*VE(I,1,K)
  415 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) RETURN
      DO 420 MP1 = 2,MMAX
          M = MP1 - 1
          MB = M* (NLAT-1) - (M* (M-1))/2
          MP2 = MP1 + 1
          IF (MP2.GT.NDO2) GO TO 420
          DO 421 K = 1,NT
              DO 421 I = 1,IMM1
                  DO 421 NP1 = MP2,NDO2,2
                      BR(MP1,NP1,K) = BR(MP1,NP1,K) +
     +                                ZV(NP1+MB,I)*VE(I,2*MP1-2,K) +
     +                                ZW(NP1+MB,I)*WO(I,2*MP1-1,K)
                      BI(MP1,NP1,K) = BI(MP1,NP1,K) +
     +                                ZV(NP1+MB,I)*VE(I,2*MP1-1,K) -
     +                                ZW(NP1+MB,I)*WO(I,2*MP1-2,K)
  421     CONTINUE
          IF (MLAT.EQ.0) GO TO 420
          DO 422 K = 1,NT
              DO 422 NP1 = MP2,NDO2,2
                  BR(MP1,NP1,K) = BR(MP1,NP1,K) +
     +                            ZV(NP1+MB,IMID)*VE(IMID,2*MP1-2,K)
                  BI(MP1,NP1,K) = BI(MP1,NP1,K) +
     +                            ZV(NP1+MB,IMID)*VE(IMID,2*MP1-1,K)
  422     CONTINUE
  420 CONTINUE
      RETURN
c
c     case ityp=5   v even, w odd, and br and bi equal zero
c
c     case m=0
c
  500 DO 516 K = 1,NT
          DO 516 I = 1,IMM1
              DO 516 NP1 = 3,NDO1,2
                  CR(1,NP1,K) = CR(1,NP1,K) - ZV(NP1,I)*WO(I,1,K)
  516 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) RETURN
      DO 520 MP1 = 2,MMAX
          M = MP1 - 1
          MB = M* (NLAT-1) - (M* (M-1))/2
          MP2 = MP1 + 1
          IF (MP1.GT.NDO1) GO TO 520
          DO 523 K = 1,NT
              DO 523 I = 1,IMM1
                  DO 523 NP1 = MP1,NDO1,2
                      CR(MP1,NP1,K) = CR(MP1,NP1,K) -
     +                                ZV(NP1+MB,I)*WO(I,2*MP1-2,K) +
     +                                ZW(NP1+MB,I)*VE(I,2*MP1-1,K)
                      CI(MP1,NP1,K) = CI(MP1,NP1,K) -
     +                                ZV(NP1+MB,I)*WO(I,2*MP1-1,K) -
     +                                ZW(NP1+MB,I)*VE(I,2*MP1-2,K)
  523     CONTINUE
          IF (MLAT.EQ.0) GO TO 520
          DO 524 K = 1,NT
              DO 524 NP1 = MP1,NDO1,2
                  CR(MP1,NP1,K) = CR(MP1,NP1,K) +
     +                            ZW(NP1+MB,IMID)*VE(IMID,2*MP1-1,K)
                  CI(MP1,NP1,K) = CI(MP1,NP1,K) -
     +                            ZW(NP1+MB,IMID)*VE(IMID,2*MP1-2,K)
  524     CONTINUE
  520 CONTINUE
      RETURN
c
c     case ityp=6 ,  v odd , w even
c
c     case m=0
c
  600 DO 615 K = 1,NT
          DO 615 I = 1,IMID
              DO 615 NP1 = 2,NDO2,2
                  CR(1,NP1,K) = CR(1,NP1,K) - ZV(NP1,I)*WE(I,1,K)
  615 CONTINUE
      DO 616 K = 1,NT
          DO 616 I = 1,IMM1
              DO 616 NP1 = 3,NDO1,2
                  BR(1,NP1,K) = BR(1,NP1,K) + ZV(NP1,I)*VO(I,1,K)
  616 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) RETURN
      DO 620 MP1 = 2,MMAX
          M = MP1 - 1
          MB = M* (NLAT-1) - (M* (M-1))/2
          MP2 = MP1 + 1
          IF (MP1.GT.NDO1) GO TO 617
          DO 623 K = 1,NT
              DO 623 I = 1,IMM1
                  DO 623 NP1 = MP1,NDO1,2
                      BR(MP1,NP1,K) = BR(MP1,NP1,K) +
     +                                ZV(NP1+MB,I)*VO(I,2*MP1-2,K) +
     +                                ZW(NP1+MB,I)*WE(I,2*MP1-1,K)
                      BI(MP1,NP1,K) = BI(MP1,NP1,K) +
     +                                ZV(NP1+MB,I)*VO(I,2*MP1-1,K) -
     +                                ZW(NP1+MB,I)*WE(I,2*MP1-2,K)
  623     CONTINUE
          IF (MLAT.EQ.0) GO TO 617
          DO 624 K = 1,NT
              DO 624 NP1 = MP1,NDO1,2
                  BR(MP1,NP1,K) = BR(MP1,NP1,K) +
     +                            ZW(NP1+MB,IMID)*WE(IMID,2*MP1-1,K)
                  BI(MP1,NP1,K) = BI(MP1,NP1,K) -
     +                            ZW(NP1+MB,IMID)*WE(IMID,2*MP1-2,K)
  624     CONTINUE
  617     IF (MP2.GT.NDO2) GO TO 620
          DO 621 K = 1,NT
              DO 621 I = 1,IMM1
                  DO 621 NP1 = MP2,NDO2,2
                      CR(MP1,NP1,K) = CR(MP1,NP1,K) -
     +                                ZV(NP1+MB,I)*WE(I,2*MP1-2,K) +
     +                                ZW(NP1+MB,I)*VO(I,2*MP1-1,K)
                      CI(MP1,NP1,K) = CI(MP1,NP1,K) -
     +                                ZV(NP1+MB,I)*WE(I,2*MP1-1,K) -
     +                                ZW(NP1+MB,I)*VO(I,2*MP1-2,K)
  621     CONTINUE
          IF (MLAT.EQ.0) GO TO 620
          DO 622 K = 1,NT
              DO 622 NP1 = MP2,NDO2,2
                  CR(MP1,NP1,K) = CR(MP1,NP1,K) -
     +                            ZV(NP1+MB,IMID)*WE(IMID,2*MP1-2,K)
                  CI(MP1,NP1,K) = CI(MP1,NP1,K) -
     +                            ZV(NP1+MB,IMID)*WE(IMID,2*MP1-1,K)
  622     CONTINUE
  620 CONTINUE
      RETURN
c
c     case ityp=7   v odd, w even, and cr and ci equal zero
c
c     case m=0
c
  700 DO 716 K = 1,NT
          DO 716 I = 1,IMM1
              DO 716 NP1 = 3,NDO1,2
                  BR(1,NP1,K) = BR(1,NP1,K) + ZV(NP1,I)*VO(I,1,K)
  716 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) RETURN
      DO 720 MP1 = 2,MMAX
          M = MP1 - 1
          MB = M* (NLAT-1) - (M* (M-1))/2
          MP2 = MP1 + 1
          IF (MP1.GT.NDO1) GO TO 720
          DO 723 K = 1,NT
              DO 723 I = 1,IMM1
                  DO 723 NP1 = MP1,NDO1,2
                      BR(MP1,NP1,K) = BR(MP1,NP1,K) +
     +                                ZV(NP1+MB,I)*VO(I,2*MP1-2,K) +
     +                                ZW(NP1+MB,I)*WE(I,2*MP1-1,K)
                      BI(MP1,NP1,K) = BI(MP1,NP1,K) +
     +                                ZV(NP1+MB,I)*VO(I,2*MP1-1,K) -
     +                                ZW(NP1+MB,I)*WE(I,2*MP1-2,K)
  723     CONTINUE
          IF (MLAT.EQ.0) GO TO 720
          DO 724 K = 1,NT
              DO 724 NP1 = MP1,NDO1,2
                  BR(MP1,NP1,K) = BR(MP1,NP1,K) +
     +                            ZW(NP1+MB,IMID)*WE(IMID,2*MP1-1,K)
                  BI(MP1,NP1,K) = BI(MP1,NP1,K) -
     +                            ZW(NP1+MB,IMID)*WE(IMID,2*MP1-2,K)
  724     CONTINUE
  720 CONTINUE
      RETURN
c
c     case ityp=8   v odd, w even, and both br and bi equal zero
c
c     case m=0
c
  800 DO 815 K = 1,NT
          DO 815 I = 1,IMID
              DO 815 NP1 = 2,NDO2,2
                  CR(1,NP1,K) = CR(1,NP1,K) - ZV(NP1,I)*WE(I,1,K)
  815 CONTINUE
c
c     case m = 1 through nlat-1
c
      IF (MMAX.LT.2) RETURN
      DO 820 MP1 = 2,MMAX
          M = MP1 - 1
          MB = M* (NLAT-1) - (M* (M-1))/2
          MP2 = MP1 + 1
          IF (MP2.GT.NDO2) GO TO 820
          DO 821 K = 1,NT
              DO 821 I = 1,IMM1
                  DO 821 NP1 = MP2,NDO2,2
                      CR(MP1,NP1,K) = CR(MP1,NP1,K) -
     +                                ZV(NP1+MB,I)*WE(I,2*MP1-2,K) +
     +                                ZW(NP1+MB,I)*VO(I,2*MP1-1,K)
                      CI(MP1,NP1,K) = CI(MP1,NP1,K) -
     +                                ZV(NP1+MB,I)*WE(I,2*MP1-1,K) -
     +                                ZW(NP1+MB,I)*VO(I,2*MP1-2,K)
  821     CONTINUE
          IF (MLAT.EQ.0) GO TO 820
          DO 822 K = 1,NT
              DO 822 NP1 = MP2,NDO2,2
                  CR(MP1,NP1,K) = CR(MP1,NP1,K) -
     +                            ZV(NP1+MB,IMID)*WE(IMID,2*MP1-2,K)
                  CI(MP1,NP1,K) = CI(MP1,NP1,K) -
     +                            ZV(NP1+MB,IMID)*WE(IMID,2*MP1-1,K)
  822     CONTINUE
  820 CONTINUE
      RETURN
      END
c
c     dwork must be of length at least 2*(nlat+1)
c
      SUBROUTINE DVHAESI(NLAT,NLON,WVHAES,LVHAES,WORK,LWORK,DWORK,
     +                   LDWORK,IERROR)
      DOUBLE PRECISION WVHAES
      DOUBLE PRECISION WORK
      DIMENSION WVHAES(LVHAES),WORK(LWORK)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION DWORK(LDWORK)

      IERROR = 1
      IF (NLAT.LT.3) RETURN
      IERROR = 2
      IF (NLON.LT.1) RETURN
      IERROR = 3
      MMAX = MIN0(NLAT, (NLON+1)/2)
      IMID = (NLAT+1)/2
      LZIMN = (IMID*MMAX* (NLAT+NLAT-MMAX+1))/2
      IF (LVHAES.LT.LZIMN+LZIMN+NLON+15) RETURN
      IERROR = 4
      LABC = 3* (MAX0(MMAX-2,0)* (NLAT+NLAT-MMAX-1))/2
      IF (LWORK.LT.5*NLAT*IMID+LABC) RETURN
      IERROR = 5
      IF (LDWORK.LT.2* (NLAT+1)) RETURN
      IERROR = 0
      IW1 = 3*NLAT*IMID + 1
      IDZ = (MMAX* (NLAT+NLAT-MMAX+1))/2
      CALL DVEA1(NLAT,NLON,IMID,WVHAES,WVHAES(LZIMN+1),IDZ,WORK,
     +          WORK(IW1),DWORK)
      CALL DHRFFTI(NLON,WVHAES(2*LZIMN+1))
      RETURN
      END
      SUBROUTINE DVEA1(NLAT,NLON,IMID,ZV,ZW,IDZ,ZIN,WZVIN,DWORK)
      DOUBLE PRECISION ZV
      DOUBLE PRECISION ZW
      DOUBLE PRECISION ZIN
      DOUBLE PRECISION WZVIN
      DIMENSION ZV(IDZ,1),ZW(IDZ,1),ZIN(IMID,NLAT,3),WZVIN(1)
C*PT*WARNING* Already double-precision
      DOUBLE PRECISION DWORK(*)

      MMAX = MIN0(NLAT, (NLON+1)/2)
      CALL DZVINIT(NLAT,NLON,WZVIN,DWORK)
      DO 33 MP1 = 1,MMAX
          M = MP1 - 1
          CALL DZVIN(0,NLAT,NLON,M,ZIN,I3,WZVIN)
          DO 33 NP1 = MP1,NLAT
              MN = M* (NLAT-1) - (M* (M-1))/2 + NP1
              DO 33 I = 1,IMID
                  ZV(MN,I) = ZIN(I,NP1,I3)
   33 CONTINUE
      CALL DZWINIT(NLAT,NLON,WZVIN,DWORK)
      DO 34 MP1 = 1,MMAX
          M = MP1 - 1
          CALL DZWIN(0,NLAT,NLON,M,ZIN,I3,WZVIN)
          DO 34 NP1 = MP1,NLAT
              MN = M* (NLAT-1) - (M* (M-1))/2 + NP1
              DO 34 I = 1,IMID
                  ZW(MN,I) = ZIN(I,NP1,I3)
   34 CONTINUE
      RETURN
      END
