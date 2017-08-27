      FUNCTION HLUICAPNT (XCRD,YCRD,ZCRD,DVAL,RPNT,LOPN,IPPP,MPPP,NPPP)
C
      DIMENSION RPNT(LOPN,MPPP),IPPP(2,MPPP)
      INTEGER *8 IKEY1,IKEY2
 
C      save maxdepth,itdepth
C
C Modified by dbrown to use mangled integer keys.
C Improves performance (same change as made earlier to ICAEDG). 12/11/20
C
C
C This function, given the X, Y, and Z coordinates of a point and the
C field data value at that point, searches the point list for a point
C having the same coordinates.  If such a point exists, its index is
C returned; if not, such a point is created and its index is returned.
C The search is effected using a tree-sort technique, the pointers for
C which are kept in the array IPPP.
C
C If there are any points in the point list at all, ...
C
C      idepth = 0
C      if (nppp .eq. 0) then
C         itdepth = 0
C      end if
      IF (NPPP.NE.0) THEN
C
        CALL MANGLE(XCRD,YCRD,IKEY1)
C
C initialize a search index to point to the first one, and loop.
C
        ITMP=1
C
C If the search index is of the point we want, return its index.
C
 101    CALL MANGLE(RPNT(1,ITMP),RPNT(2,ITMP), IKEY2)
        IF (IKEY1 .EQ. IKEY2) THEN
C
          HLUICAPNT=ITMP
C
          RETURN
        END IF
C
C If the point we want would precede the one pointed at by the search
C index, reset the search index to look at lesser elements (if any),
C and loop back to continue the search.  If the pointer is null, reset
C it to point to a new element that we will create.
C
C        idepth = idepth + 1
        IF (IKEY1 .LT. IKEY2) THEN
C
          IF (IPPP(1,ITMP).NE.0) THEN
            ITMP=IPPP(1,ITMP)
            GO TO 101
          END IF
C
          IPPP(1,ITMP)=NPPP+1
C
C If the point we want would follow the one pointed at by the search
C index, reset the search index to look at greater elements (if any),
C and loop back to continue the search.  If the pointer is null, reset
C it to point to a new element that we will create.
C
        ELSE IF (IKEY1 .GT. IKEY2) THEN
C
          IF (IPPP(2,ITMP).NE.0) THEN
            ITMP=IPPP(2,ITMP)
            GO TO 101
          END IF
C
          IPPP(2,ITMP)=NPPP+1
C
        END IF
C
      END IF
C
C      output for monitoring performance of the key method
C
C      itdepth = itdepth + idepth
C      if (idepth .gt. maxdepth) then
C         maxdepth = idepth
C         write(*,*) 'current maxdepth:', maxdepth,
C     +              ' avg depth: ', itdepth / (nppp + 1),
C     +              'point count is: ', (nppp + 1)
C      else if (mod((nppp+1),100) .eq. 0) then
C         write(*,*) ' avg depth: ', itdepth, itdepth / (nppp + 1)
C      end if
C
C Create a new point in the point list (if there's room, of course), and
C return its index to the caller.
C
      IF (NPPP.GE.MPPP) THEN
C
        CALL SETER ('ICAPNT - POINT ARRAY IS TOO SMALL',1,1)
        HLUICAPNT=-1
        RETURN
C
      ELSE
C
        NPPP=NPPP+1
C
        IPPP(1,NPPP)=0
        IPPP(2,NPPP)=0
C
        RPNT(1,NPPP)=XCRD
        RPNT(2,NPPP)=YCRD
        RPNT(3,NPPP)=ZCRD
        RPNT(4,NPPP)=DVAL
C
        HLUICAPNT=NPPP
C
      END IF
C
C Done.
C
      RETURN
C
      END
