      SUBROUTINE ARDBDA (X1,Y1,X2,Y2,IL,IR,IF,IG)
C
C The routine ARDBDA is called by ARDBPA, below, to draw an arrow from
C the point (X1,Y1) to the point (X2,Y2), in the fractional coordinate
C system.  The left and right area identifiers IL and IR are written
C in the proper positions relative to the arrow.  If IF is less than
C or equal to zero, the group identifier IG is written on the arrow.
C In order to prevent too many arrowheads from appearing, we keep track
C of the cumulative distance along edges being drawn (in DT).
C
C Declare the AREAS common block.
C
C
C ARCOMN contains variables which are used by all the AREAS routines.
C
      COMMON /ARCOMN/ IAD,IAU,ILC,RLC,ILM,RLM,ILP,RLP,IBS,RBS,DBS,IDB,
     +                IDC,IDI,IRC(16),RLA,RWA,RDI,RSI
      SAVE   /ARCOMN/
C
C Declare a local common block used to communicate with ARDBPA.
C
      COMMON /ARCOM1/ DT
C
C Define character variables required to write the area identifiers.
C
      CHARACTER*7 CS
      CHARACTER*1 IC
C
C Check for an uncleared prior error.
C
      IF (ICFELL('ARDBDA - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Draw the body of the arrow.
C
      CALL PLOTIF(X1,Y1,0)
      IF (ICFELL('ARDBDA',2).NE.0) RETURN
      CALL PLOTIF(X2,Y2,1)
      IF (ICFELL('ARDBDA',3).NE.0) RETURN
C
C Compute the length of the arrow.  If it's zero, quit.
C
      DX=X2-X1
      DY=Y2-Y1
      DP=SQRT(DX*DX+DY*DY)
C
      IF (DP.EQ.0.) RETURN
C
C If area identifiers are to be written and they are in a reasonable
C range (less than 1,000,000 in absolute value), write them on either
C side of the arrow.
C
      IF (.NOT.(RDI.GT.0..AND.RSI.GT.0..AND.ABS(IL).LT.1000000.AND.ABS(I
     +R).LT.1000000)) GO TO 10001
C
        XC=.5*(X1+X2)
        YC=.5*(Y1+Y2)
        XL=XC-RDI*DY/DP
        YL=YC+RDI*DX/DP
        WRITE (CS,'(I7)') IL
        NC=0
        DO 101 I=1,7
        IC=CS(I:I)
        IF (.NOT.(IC.NE.' ')) GO TO 10002
          NC=NC+1
          CS(NC:NC)=IC
10002   CONTINUE
  101   CONTINUE
        CALL PLCHLQ (XL,YL,CS(1:NC),RSI,0.,0.)
        IF (ICFELL('ARDBDA',4).NE.0) RETURN
C
        XR=XC+RDI*DY/DP
        YR=YC-RDI*DX/DP
        WRITE (CS,'(I7)') IR
        NC=0
        DO 102 I=1,7
        IC=CS(I:I)
        IF (.NOT.(IC.NE.' ')) GO TO 10003
          NC=NC+1
          CS(NC:NC)=IC
10003   CONTINUE
  102   CONTINUE
        CALL PLCHLQ (XR,YR,CS(1:NC),RSI,0.,0.)
        IF (ICFELL('ARDBDA',5).NE.0) RETURN
C
10001 CONTINUE
C
C If all groups of edges are being put on the same plot, write the
C group identifier on the arrow.
C
      IF (.NOT.(RSI.GT.0..AND.IF.LE.0.AND.IG.LT.1000000)) GO TO 10004
        XC=.5*(X1+X2)
        YC=.5*(Y1+Y2)
        WRITE (CS,'(I6)') IG
        NC=0
        DO 103 I=1,6
        IC=CS(I:I)
        IF (.NOT.(IC.NE.' ')) GO TO 10005
          NC=NC+1
          CS(NC:NC)=IC
10005   CONTINUE
  103   CONTINUE
        CALL PLCHLQ (XC,YC,CS(1:NC),RSI,0.,0.)
        IF (ICFELL('ARDBDA',6).NE.0) RETURN
10004 CONTINUE
C
C If an arrowhead is to be drawn, do that now, making sure that the
C cumulative length of the edge being drawn is great enough.
C
      IF (.NOT.(RLA.GT.0..AND.RWA.GT.0.)) GO TO 10006
        DT=DT+DP
        IF(DT.LE.RLA) RETURN
        DT=0.
        B=(DP-RLA)/DP
        A=1.-B
        XT=A*X1+B*X2
        YT=A*Y1+B*Y2
        X3=XT-RWA*DY/DP
        Y3=YT+RWA*DX/DP
        X4=XT+RWA*DY/DP
        Y4=YT-RWA*DX/DP
        CALL PLOTIF (X3,Y3,0)
        IF (ICFELL('ARDBDA',7).NE.0) RETURN
        CALL PLOTIF (X2,Y2,1)
        IF (ICFELL('ARDBDA',8).NE.0) RETURN
        CALL PLOTIF (X4,Y4,1)
        IF (ICFELL('ARDBDA',9).NE.0) RETURN
10006 CONTINUE
C
C Done.
C
      RETURN
C
      END
