      FUNCTION ICAPNX (XCRD,YCRD,ZCRD,DVAL,RPNT,LOPN,IPPP,MPPP,NPPP,
     +                                                         EPST)
C
      DIMENSION RPNT(LOPN,MPPP),IPPP(3,MPPP)
C
C This function, given the X, Y, and Z coordinates of a point and the
C field data value at that point, searches the point list for a point
C having nearly the same coordinates (within the epsilon specified by
C the value of EPST).  If such a point exists, its index is returned;
C if not, such a point is created and its index is returned.  The
C search is effected using a tree-sort technique, the pointers for
C which are kept in the array IPPP.  Each node contains three pointers:
C 1) a forward pointer to a list of lesser values; 2) a forward pointer
C to a list of greater values, and 3) a backward pointer to the parent.
C
C Initialize.
C
      ITMP=0
C
C If there are any points in the point list at all, ...
C
      IF (NPPP.NE.0) THEN
C
C initialize the search index to point to the first one, and loop.
C
        ITMP=1
C
C If the search index is that of the point we want, return it.
C
  101   IF (ABS(XCRD-RPNT(1,ITMP)).LE.EPST.AND.ABS(YCRD-RPNT(2,ITMP)).LE
     +.EPST.AND.ABS(ZCRD-RPNT(3,ITMP)).LE.EPST) THEN
C
C 101     IF      (ABS(XCRD.EQ.RPNT(1,ITMP)).AND.
C    +             ABS(YCRD.EQ.RPNT(2,ITMP)).AND.
C    +             ABS(ZCRD.EQ.RPNT(3,ITMP)))
C
          ICAPNX=ITMP
C
          RETURN
C
C If the point we want would precede the one pointed at by the search
C index, reset the search index to look at lesser elements (if any),
C and loop back to continue the search.  If the pointer is null, reset
C it to point to a new element that we will create.
C
        ELSE IF ((XCRD.LT.RPNT(1,ITMP)).OR.(XCRD.EQ.RPNT(1,ITMP).AND.YCR
     +D.LT.RPNT(2,ITMP)).OR.(XCRD.EQ.RPNT(1,ITMP).AND.YCRD.EQ.RPNT(2,ITM
     +P).AND.ZCRD.LT.RPNT(3,ITMP))) THEN
C
          IF (IPPP(1,ITMP).NE.0) THEN
            ITMP=IPPP(1,ITMP)
            GO TO 101
          END IF
C
          INEW=1
C
C If the point we want would follow the one pointed at by the search
C index, reset the search index to look at greater elements (if any),
C and loop back to continue the search.  If the pointer is null, reset
C it to point to a new element that we will create.
C
        ELSE IF ((XCRD.GT.RPNT(1,ITMP)).OR.(XCRD.EQ.RPNT(1,ITMP).AND.YCR
     +D.GT.RPNT(2,ITMP)).OR.(XCRD.EQ.RPNT(1,ITMP).AND.YCRD.EQ.RPNT(2,ITM
     +P).AND.ZCRD.GT.RPNT(3,ITMP))) THEN
C
          IF (IPPP(2,ITMP).NE.0) THEN
            ITMP=IPPP(2,ITMP)
            GO TO 101
          END IF
C
          INEW=2
C
        END IF
C
C No point with approximately the right X, Y, and Z coordinates was
C found.  Search backward through the list looking for near matches.
C
        IBAK=ITMP
C
10001   CONTINUE
          IF (IPPP(1,IBAK).NE.0) THEN
            IBAK=IPPP(1,IBAK)
10002       CONTINUE
            IF (.NOT.(IPPP(2,IBAK).NE.0)) GO TO 10003
              IBAK=IPPP(2,IBAK)
            GO TO 10002
10003       CONTINUE
          ELSE
10004       CONTINUE
              IF (IPPP(3,IBAK).EQ.0) GO TO 10005
              ISAV=IBAK
              IBAK=IPPP(3,IBAK)
            IF (.NOT.(IPPP(2,IBAK).EQ.ISAV)) GO TO 10004
          END IF
          IF (RPNT(1,IBAK).LT.XCRD-EPST) GO TO 10005
          IF (ABS(XCRD-RPNT(1,IBAK)).LE.EPST.AND.ABS(YCRD-RPNT(2,IBAK)).
     +LE.EPST.AND.ABS(ZCRD-RPNT(3,IBAK)).LE.EPST) THEN
            ICAPNX=IBAK
            RETURN
          END IF
        GO TO 10001
10005   CONTINUE
C
C No point with approximately the right X, Y, and Z coordinates was
C found.  Search forward through the list looking for near matches.
C
        IFOR=ITMP
C
10006   CONTINUE
          IF (IPPP(2,IFOR).NE.0) THEN
            IFOR=IPPP(2,IFOR)
10007       CONTINUE
            IF (.NOT.(IPPP(1,IFOR).NE.0)) GO TO 10008
              IFOR=IPPP(1,IFOR)
            GO TO 10007
10008       CONTINUE
          ELSE
10009       CONTINUE
              IF (IPPP(3,IFOR).EQ.0) GO TO 10010
              ISAV=IFOR
              IFOR=IPPP(3,IFOR)
            IF (.NOT.(IPPP(1,IFOR).EQ.ISAV)) GO TO 10009
          END IF
          IF (RPNT(1,IFOR).GT.XCRD+EPST) GO TO 10010
          IF (ABS(XCRD-RPNT(1,IFOR)).LE.EPST.AND.ABS(YCRD-RPNT(2,IFOR)).
     +LE.EPST.AND.ABS(ZCRD-RPNT(3,IFOR)).LE.EPST) THEN
            ICAPNX=IFOR
            RETURN
          END IF
        GO TO 10006
10010   CONTINUE
C
      END IF
C
C Create a new point in the point list (if there's room, of course), and
C return its index to the caller.
C
      IF (NPPP.GE.MPPP) THEN
C
        CALL SETER ('ICAPNX - POINT ARRAY IS TOO SMALL',1,1)
        ICAPNX=-1
        RETURN
C
      ELSE
C
        NPPP=NPPP+1
C
        IF (ITMP.NE.0) IPPP(INEW,ITMP)=NPPP
C
        IPPP(1,NPPP)=0
        IPPP(2,NPPP)=0
        IPPP(3,NPPP)=ITMP
C
        RPNT(1,NPPP)=XCRD
        RPNT(2,NPPP)=YCRD
        RPNT(3,NPPP)=ZCRD
        RPNT(4,NPPP)=DVAL
C
        ICAPNX=NPPP
C
      END IF
C
C Done.
C
      RETURN
C
      END
