      SUBROUTINE VTTMTX (NTTO,TBUF,MBUF,NBUF,
     +                   EPST,
     +                   IPPP,MPPP,NPPP,
     +                   IPPE,MPPE,NPPE,
     +                   RPNT,MPNT,NPNT,LOPN,
     +                   IEDG,MEDG,NEDG,LOEN,
     +                   ITRI,MTRI,NTRI,LOTN)
C
      DIMENSION TBUF(18,MBUF)
      DIMENSION IPPP(3,MPPP),IPPE(2,MPPE)
      DIMENSION RPNT(MPNT),IEDG(MEDG),ITRI(MTRI)
C
C The routine VTTMTX is called to process NTTO randomly-selected
C triangles from among the NBUF stored in the array TBUF, leaving the
C remaining NBUF-NTTO triangles at the beginning of the array.  New
C points are added to the point list in the array RPNT, new edges are
C added to the edge list in the array IEDG, and new triangles are
C added to the triangle list in the array ITRI.  The arrays IPPP and
C IPPE are used to keep tree-sorted lists of the points and the edges,
C respectively, so that no duplicate points or edges will be created.
C The argument EPST is an epsilon used in testing whether or not two
C coordinate values are the same or not.
C
      DO 102 I=1,NTTO
C
C Pick a value of IBUF between 1 and NTTO, inclusive.  The buffered
C triangle with index IBUF will be processed.
C
      IBUF=1+MAX(0,MIN(NBUF-1,INT(REAL(NBUF)*VTFRAN())))
C
C Use the function ICAVPX to get indices for each of the three points
C of the triangle in the point list and form the base indices (IPP1,
C IPP2, and IPP3) of the three points in the point list.
C
      IPP1=(ICAVPX(TBUF( 1,IBUF),
     +             TBUF( 2,IBUF),
     +             TBUF( 3,IBUF),
     +             TBUF( 4,IBUF),
     +             TBUF( 5,IBUF),
     +             TBUF( 6,IBUF),
     +             RPNT,LOPN,IPPP,MPPP,NPPP,EPST)-1)*LOPN
C
      IF (ICFELL('VTTMTX',1).NE.0) RETURN
C
      IPP2=(ICAVPX(TBUF( 7,IBUF),
     +             TBUF( 8,IBUF),
     +             TBUF( 9,IBUF),
     +             TBUF(10,IBUF),
     +             TBUF(11,IBUF),
     +             TBUF(12,IBUF),
     +             RPNT,LOPN,IPPP,MPPP,NPPP,EPST)-1)*LOPN
C
      IF (ICFELL('VTTMTX',2).NE.0) RETURN
C
      IPP3=(ICAVPX(TBUF(13,IBUF),
     +             TBUF(14,IBUF),
     +             TBUF(15,IBUF),
     +             TBUF(16,IBUF),
     +             TBUF(17,IBUF),
     +             TBUF(18,IBUF),
     +             RPNT,LOPN,IPPP,MPPP,NPPP,EPST)-1)*LOPN
C
      IF (ICFELL('VTTMTX',3).NE.0) RETURN
C
C Use the function ICAEDG to get indices for each of the three edges of
C the triangle in the edge list and form the base indices (IPE1, IPE2,
C and IPE3) of the three edges in the edge list.  At the same time, set
C the pointer from each edge node into the new triangle we're about to
C create (to the left or to the right, as appropriate).
C
      IPE1=(ICAEDG(IPP1,IPP2,IEDG,LOEN,IPPE,MPPE,NPPE,RPNT)-1)*LOEN
C
      IF (ICFELL('VTTMTX',4).NE.0) RETURN
C
      IF (IEDG(IPE1+1).EQ.IPP1) THEN
        IEDG(IPE1+3)=NTRI+1
      ELSE
        IEDG(IPE1+4)=NTRI+1
      END IF
C
      IPE2=(ICAEDG(IPP2,IPP3,IEDG,LOEN,IPPE,MPPE,NPPE,RPNT)-1)*LOEN
C
      IF (ICFELL('VTTMTX',5).NE.0) RETURN
C
      IF (IEDG(IPE2+1).EQ.IPP2) THEN
        IEDG(IPE2+3)=NTRI+2
      ELSE
        IEDG(IPE2+4)=NTRI+2
      END IF
C
      IPE3=(ICAEDG(IPP3,IPP1,IEDG,LOEN,IPPE,MPPE,NPPE,RPNT)-1)*LOEN
C
      IF (ICFELL('VTTMTX',6).NE.0) RETURN
C
      IF (IEDG(IPE3+1).EQ.IPP3) THEN
        IEDG(IPE3+3)=NTRI+3
      ELSE
        IEDG(IPE3+4)=NTRI+3
      END IF
C
C Add the new triangle to the triangle list.
C
      IF (NTRI+LOTN.GT.MTRI) THEN
        CALL SETER ('VTTMTX - TRIANGLE ARRAY IS TOO SMALL',7,1)
        RETURN
      ELSE
        IPTT=NTRI
        NTRI=NTRI+LOTN
        ITRI(IPTT+1)=IPE1
        ITRI(IPTT+2)=IPE2
        ITRI(IPTT+3)=IPE3
        ITRI(IPTT+4)=0
      END IF
C
C Copy the last triangle in the triangle buffer to the vacated slot left
C by the one just processed.
C
      IF (IBUF.NE.NBUF) THEN
        DO 101 J=1,12
        TBUF(J,IBUF)=TBUF(J,NBUF)
  101   CONTINUE
      END IF
C
C Reduce the count of the number of triangles in the buffer.
C
      NBUF=NBUF-1
C
C Continue looping until NTTO triangles have been processed.
C
  102 CONTINUE
C
C Set the pointers that tell the caller how many points and edges were
C created.
C
      NPNT=NPPP*LOPN
      NEDG=NPPE*LOEN
C
C Done.
C
      RETURN
C
      END
