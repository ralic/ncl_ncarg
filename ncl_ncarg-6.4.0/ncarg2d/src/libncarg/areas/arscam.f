      SUBROUTINE ARSCAM (IAM,XCS,YCS,MCS,IAI,IAG,MAI,APR)
C
      DIMENSION IAM(*),XCS(MCS),YCS(MCS),IAI(MAI),IAG(MAI)
C
C The routine ARSCAM is called to scan an area map created by calls to
C ARINAM and AREDAM.  For each subarea of the map, the user routine APR
C is called.
C
C IAM is the array holding the area map, created by prior calls to the
C routines ARINAM and AREDAM.
C
C The arrays XCS and YCS are used, in a call to APR, to hold the X
C and Y coordinates of the points defining a particular subarea.  Each
C is dimensioned MCS.
C
C The arrays IAG and IAI are used, in a call to APR, to hold the
C group and area identifiers of the subarea defined by XCS and YCS.
C Each is dimensioned MAI.
C
C APR is the user's area-processing routine.  It must be declared in
C an EXTERNAL statement in the routine which calls ARSCAM.  It will be
C called using a FORTRAN statement like
C
C       CALL APR (XCS,YCS,NCS,IAI,IAG,NAI)
C
C where XCS and YCS hold the normalized device coordinates of NCS points
C defining a single subarea (point number NCS being a duplicate of point
C number 1) and IAI and IAG hold NAI area-identifier/group-identifier
C pairs for that subarea.
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
C Define some double-precision variables.
C
      DOUBLE PRECISION DP1,DP2
C
C Define the arrays which determine the multiple-precision operations
C to be done by ARMPIA.
C
      DIMENSION IO1(4,8),IO2(4,4)
C
      DATA IO1 / 1 ,  1 ,  0 ,  0 ,
     +           1 ,  2 ,  0 ,  0 ,
     +           1 ,  3 ,  0 ,  0 ,
     +           1 ,  4 ,  0 ,  0 ,
     +           4 ,  5 ,  1 ,  2 ,
     +           4 ,  6 ,  3 ,  4 ,
     +           3 ,  7 ,  5 ,  6 ,
     +           5 ,  7 ,  0 ,  0 /
      DATA IO2 / 4 ,  5 ,  1 ,  4 ,
     +           4 ,  6 ,  2 ,  3 ,
     +           2 ,  7 ,  5 ,  6 ,
     +           5 ,  7 ,  0 ,  0 /
C
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL ARBLDA
C
C Check for an uncleared prior error.
C
      IF (ICFELL('ARSCAM - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Pull out the length of the area map and check for initialization.
C
      LAM=IAM(1)
C
      IF (.NOT.(IAU.EQ.0.OR.IAM(LAM).NE.LAM)) GO TO 10001
        CALL SETER ('ARSCAM - INITIALIZATION DONE IMPROPERLY',2,1)
        RETURN
10001 CONTINUE
C
C Save the current user-system mapping and reset it as required for
C calls to APR.
C
      CALL GETSET (FFL,FFR,FFB,FFT,FUL,FUR,FUB,FUT,ILL)
      IF (ICFELL('ARSCAM',3).NE.0) RETURN
      CALL SET    (FFL,FFR,FFB,FFT,FFL,FFR,FFB,FFT,  1)
      IF (ICFELL('ARSCAM',4).NE.0) RETURN
C
C Initialize IPX, which is used to position nodes in coordinate order.
C
      IPX=8
C
C If it has not already been done, find points of intersection and
C incorporate them into the map and then adjust area identifiers.
C
      IF (.NOT.(IAM(4).EQ.0)) GO TO 10002
        CALL ARPRAM (IAM,0,0,0)
        IF (ICFELL('ARSCAM',5).NE.0) RETURN
10002 CONTINUE
C
C We first make a pass over the entire area map, looking for holes and
C eliminating them by the insertion of some temporary connecting lines.
C
C Save the pointer to the last word of the last node, so that we can
C remove the nodes implementing the temporary connecting lines before
C returning to the caller.
C
      ILW=IAM(5)
C
C Each pass through the following loop traces the boundary of one
C connected loop.
C
C
      IPT=8
C
10003 CONTINUE
C
C Move to the right across the area map, looking for an edge segment
C that has not yet been completely processed.  If no such segment can
C be found, all subareas have been done.
C
10004   CONTINUE
        IF (.NOT.(IAM(IPT).GE.3.OR.IAM(IPT+7).LE.1)) GO TO 10005
          IPT=IAM(IPT+5)
          IF (IPT.EQ.18) GO TO 10006
        GO TO 10004
10005   CONTINUE
C
C Decide whether to scan the subarea to the left of the edge being
C traced (IPU=1) or the one to the right (IPU=2).
C
        IF (.NOT.(IAM(IPT).EQ.0.OR.IAM(IPT).EQ.2)) GO TO 10007
          IPU=1
        GO TO 10008
10007   CONTINUE
          IPU=2
10008   CONTINUE
C
C IPQ points to the node defining the beginning of the edge segment,
C IPR to the node defining the end of the edge segment, and IPS to
C the node defining the beginning of the edge, so that we can tell
C when we've gone all the way around it.
C
        IPQ=IAM(IPT+4)
        IPR=IPT
        IPS=IPQ
        IPM=IPR
        IPV=IPU
C
C Jira NCL_32: Handle special case of IPP in starting segment is a
C node inserted due to splitting a segment to insert a hole-connector.
C In that case, we want to use original segment's coords for angle
C calculation.
        IF (.NOT.(IPQ.GT.ILW)) GO TO 10009
          IF (IAM(IPQ+3).NE.IPT) IFRMXY=IAM(IPQ+3)
          IF (IAM(IPQ+4).NE.IPT) IFRMXY=IAM(IPQ+4)
        GO TO 10010
10009   CONTINUE
          IFRMXY=0
10010   CONTINUE
C
C We need to keep track of the highest point found along the loop and
C the total change in direction.  Initialize the variables involved.
C
        IPH=IPQ
        ANT=0.
C
C Each pass through the following loop moves one step along the edge of
C the subarea.
C
10011   CONTINUE
C
C Update the pointer to the highest point found along the loop.
C
          IF (IAM(IPR+2).GT.IAM(IPH+2)) IPH=IPR
C
C Move IPQ to IPP and IPR to IPQ.
C
          IPP=IPQ
          IPQ=IPR
C
C Get the coordinates of the ends of the edge segment for use in
C computing change in direction to a possible next point.
C
C Jira NCL_32: if IPP is from a segment that was split to insert a
C hole-connector, use original segment's coords. in angle calculation.
          IF (.NOT.(IFRMXY.GT.0)) GO TO 10012
            IXP=IAM(IFRMXY+1)
            IYP=IAM(IFRMXY+2)
            IFRMXY=0
          GO TO 10013
10012     CONTINUE
            IXP=IAM(IPP+1)
            IYP=IAM(IPP+2)
10013     CONTINUE
          IXQ=IAM(IPQ+1)
          IYQ=IAM(IPQ+2)
          FXP=REAL(IXP)
          FYP=REAL(IYP)
          FXQ=REAL(IXQ)
          FYQ=REAL(IYQ)
C
C Back up IPR to the beginning of the group of nodes which have the
C same X and Y coordinates as it does.
C
10014     CONTINUE
          IF (.NOT.(IAM(IPR+1).EQ.IAM(IAM(IPR+6)+1).AND.IAM(IPR+2).EQ.IA
     +M(IAM(IPR+6)+2))) GO TO 10015
            IPR=IAM(IPR+6)
          GO TO 10014
10015     CONTINUE
C
C Go through the group of nodes, examining all the possible ways to
C move from the current position to a new one.  Pick the direction
C which is leftmost (if IPU=1) or rightmost (if IPU=2).
C
          IP1=IPR
          IP2=IPR
          IPR=0
          IF (.NOT.(IPU.EQ.1)) GO TO 10016
            ANM=-3.14159265358979
          GO TO 10017
10016     CONTINUE
            ANM=+3.14159265358979
10017     CONTINUE
C
10018     CONTINUE
          IF (.NOT.(IAM(IP2+1).EQ.IAM(IP1+1).AND.IAM(IP2+2).EQ.IAM(IP1+2
     +))) GO TO 10019
            IF (.NOT.(IAM(IAM(IP2+3)+7).GT.1.AND.(IAM(IAM(IP2+3)+1).NE.I
     +AM(IPP+1).OR.IAM(IAM(IP2+3)+2).NE.IAM(IPP+2)))) GO TO 10020
C               Jira NCL_32: If test node is due to splitting a
C               a line-segment to insert a hole-connector, use the
C               original segment's end points for angle calculation.
              IF (.NOT.(IP2.LT.ILW.AND.IAM(IP2+3).GT.ILW)) GO TO 10021
                IXR=IAM(IAM(IAM(IP2+3)+3)+1)
                IYR=IAM(IAM(IAM(IP2+3)+3)+2)
              GO TO 10022
10021         CONTINUE
                IXR=IAM(IAM(IP2+3)+1)
                IYR=IAM(IAM(IP2+3)+2)
10022         CONTINUE
              FXR=REAL(IXR)
              FYR=REAL(IYR)
C
              IF (.NOT.(IAU.EQ.1)) GO TO 10023
                ANG=ARRAT2((FXQ-FXP)*(FYR-FYQ)-(FYQ-FYP)*(FXR-FXQ),
     +                     (FXQ-FXP)*(FXR-FXQ)+(FYQ-FYP)*(FYR-FYQ))
              GO TO 10024
10023         CONTINUE
              IF (.NOT.(IAU.EQ.2)) GO TO 10025
                ANG=ARDAT2(DBLE(IXQ-IXP)*DBLE(IYR-IYQ)-
     +                     DBLE(IYQ-IYP)*DBLE(IXR-IXQ),
     +                     DBLE(IXQ-IXP)*DBLE(IXR-IXQ)+
     +                     DBLE(IYQ-IYP)*DBLE(IYR-IYQ))
              GO TO 10024
10025         CONTINUE
                IO1(3,1)=IXQ-IXP
                IO1(3,2)=IYR-IYQ
                IO1(3,3)=IYQ-IYP
                IO1(3,4)=IXR-IXQ
                CALL ARMPIA (IO1,DP1,IER)
                IF (.NOT.(IER.NE.0)) GO TO 10026
                  GO TO 10028
10026           CONTINUE
                CALL ARMPIA (IO2,DP2,IER)
                IF (.NOT.(IER.NE.0)) GO TO 10029
                  GO TO 10028
10029           CONTINUE
                ANG=ARDAT2(DP1,DP2)
10024         CONTINUE
C
              IF (.NOT.(IPU.EQ.1)) GO TO 10031
                IF (.NOT.(ANG.GT.ANM)) GO TO 10032
                  IPR=IAM(IP2+3)
                  ANM=ANG
                  IPM=IPR
                  IPV=1
C Jira NCL_32: case where potential next IPP is a node inserted to
C create a hole-connector. We want the original segment's coords for
C angle calculation.
C Note: because the first node (bottom) of the first hole-connector
C points *back* to a user-inserted node (i.e., IAM(IP2+$PD$).LT.ILW),
C we need to exclude that one special case from the following test;
C hence the factor (ILW+1).
                  IF (.NOT.(IP2.GT.(ILW+1).AND.IAM(IP2+4).LT.ILW)) GO TO
     + 10033
                    IFRMXY = IAM(IP2+4)
                  GO TO 10034
10033             CONTINUE
                    IFRMXY=0
10034             CONTINUE
10032           CONTINUE
              GO TO 10035
10031         CONTINUE
                IF (.NOT.(ANG.LT.ANM)) GO TO 10036
                  IPR=IAM(IP2+3)
                  ANM=ANG
                  IPM=IPR
                  IPV=2
C Jira NCL_32: case where potential next IPP is a node inserted to
C create a hole-connector. We want the original segment's coords for
C angle calculation.
C Note: because the first node (bottom) of the first hole-connector
C points *back* to a user-inserted node (i.e., IAM(IP2+$PD$).LT.ILW),
C we need to exclude that one special case from the following test;
C hence the factor (ILW+1).
                  IF (.NOT.(IP2.GT.(ILW+1).AND.IAM(IP2+4).LT.ILW)) GO TO
     + 10037
                    IFRMXY = IAM(IP2+4)
                  GO TO 10038
10037             CONTINUE
                    IFRMXY=0
10038             CONTINUE
10036           CONTINUE
10035         CONTINUE
10020       CONTINUE
            IF (.NOT.(IAM(IP2+7).GT.1.AND.(IAM(IAM(IP2+4)+1).NE.IAM(IPP+
     +1).OR.IAM(IAM(IP2+4)+2).NE.IAM(IPP+2)))) GO TO 10039
C               Jira NCL_32: If test node is due to splitting a
C               a line-segment to insert a hole-connector, use the
C               original segment's end points for angle calculation.
              IF (.NOT.(IP2.LT.ILW.AND.IAM(IP2+4).GT.ILW)) GO TO 10040
                IXR=IAM(IAM(IAM(IP2+4)+4)+1)
                IYR=IAM(IAM(IAM(IP2+4)+4)+2)
              GO TO 10041
10040         CONTINUE
                IXR=IAM(IAM(IP2+4)+1)
                IYR=IAM(IAM(IP2+4)+2)
10041         CONTINUE
              FXR=REAL(IXR)
              FYR=REAL(IYR)
C
              IF (.NOT.(IAU.EQ.1)) GO TO 10042
                ANG=ARRAT2((FXQ-FXP)*(FYR-FYQ)-(FYQ-FYP)*(FXR-FXQ),
     +                     (FXQ-FXP)*(FXR-FXQ)+(FYQ-FYP)*(FYR-FYQ))
              GO TO 10043
10042         CONTINUE
              IF (.NOT.(IAU.EQ.2)) GO TO 10044
                ANG=ARDAT2(DBLE(IXQ-IXP)*DBLE(IYR-IYQ)-
     +                     DBLE(IYQ-IYP)*DBLE(IXR-IXQ),
     +                     DBLE(IXQ-IXP)*DBLE(IXR-IXQ)+
     +                     DBLE(IYQ-IYP)*DBLE(IYR-IYQ))
              GO TO 10043
10044         CONTINUE
                IO1(3,1)=IXQ-IXP
                IO1(3,2)=IYR-IYQ
                IO1(3,3)=IYQ-IYP
                IO1(3,4)=IXR-IXQ
                CALL ARMPIA (IO1,DP1,IER)
                IF (.NOT.(IER.NE.0)) GO TO 10045
                  GO TO 10028
10045           CONTINUE
                CALL ARMPIA (IO2,DP2,IER)
                IF (.NOT.(IER.NE.0)) GO TO 10047
                  GO TO 10028
10047           CONTINUE
                ANG=ARDAT2(DP1,DP2)
10043         CONTINUE
C
              IF (.NOT.(IPU.EQ.1)) GO TO 10049
                IF (.NOT.(ANG.GT.ANM)) GO TO 10050
                  IPR=IAM(IP2+4)
                  ANM=ANG
                  IPM=IP2
                  IPV=2
C Jira NCL_32: case where potential next IPP is a node inserted to
C create a hole-connector. We want the original segment's coords for
C angle calculation.
                  IF (.NOT.(IP2.GT.ILW.AND.IAM(IP2+3).LT.ILW)) GO TO 100
     +51
                    IFRMXY=IAM(IP2+3)
                  GO TO 10052
10051             CONTINUE
                    IFRMXY=0
10052             CONTINUE
10050           CONTINUE
              GO TO 10053
10049         CONTINUE
                IF (.NOT.(ANG.LT.ANM)) GO TO 10054
                  IPR=IAM(IP2+4)
                  ANM=ANG
                  IPM=IP2
                  IPV=1
C Jira NCL_32: case where potential next IPP is a node inserted to
C create a hole-connector. We want the original segment's coords for
C angle calculation.
                  IF (.NOT.(IP2.GT.ILW.AND.IAM(IP2+3).LT.ILW)) GO TO 100
     +55
                    IFRMXY=IAM(IP2+3)
                  GO TO 10056
10055             CONTINUE
                    IFRMXY=0
10056             CONTINUE
10054           CONTINUE
10053         CONTINUE
10039       CONTINUE
            IP2=IAM(IP2+5)
          GO TO 10018
10019     CONTINUE
C
C If no possible exit was found, reverse direction.
C
          IF (.NOT.(IPR.EQ.0)) GO TO 10057
            IPR=IPP
            IPV=3-IPV
            IF (.NOT.(IPU.EQ.1)) GO TO 10058
              ANM=+3.14159265358979
            GO TO 10059
10058       CONTINUE
              ANM=-3.14159265358979
10059       CONTINUE
10057     CONTINUE
C
C Update the total angular change.
C
          ANT=ANT+ANM
C
C Update the markers for the edge segment picked.
C
          IF (.NOT.(IPV.EQ.1)) GO TO 10060
            IF (.NOT.(MOD(IAM(IPM),2).EQ.0)) GO TO 10061
              IAM(IPM)=IAM(IPM)+1
            GO TO 10062
10061       CONTINUE
              CALL SETER ('ARSCAM - ALGORITHM FAILURE',6,1)
              RETURN
10062       CONTINUE
          GO TO 10063
10060     CONTINUE
            IF (.NOT.(MOD(IAM(IPM)/2,2).EQ.0)) GO TO 10064
              IAM(IPM)=IAM(IPM)+2
            GO TO 10065
10064       CONTINUE
              CALL SETER ('ARSCAM - ALGORITHM FAILURE',7,1)
              RETURN
10065       CONTINUE
10063     CONTINUE
C
C Exit if we're passing the start of the subarea.
C
          IF (IAM(IPQ+1).EQ.IAM(IPS+1).AND.IAM(IPQ+2).EQ.IAM(IPS+2).AND.
     +IAM(IPR+1).EQ.IAM(IPT+1).AND.IAM(IPR+2).EQ.IAM(IPT+2)) GO TO 10066
C
        GO TO 10011
10066   CONTINUE
C
C If the closed loop just traced was a hole, insert a temporary
C connecting line to get rid of the hole.
C
        IF (.NOT.((IPU.EQ.1.AND.ANT.LT.0.).OR.(IPU.EQ.2.AND.ANT.GT.0.)))
     +  GO TO 10067
          IOF=0
          XCI=REAL(IAM(IPH+1))
          YCI=REAL(IAM(IPH+2))
          YCO=RLP
          IP1=IPH
10068     CONTINUE
          IF (.NOT.(IAM(IAM(IP1+5)+1).EQ.IAM(IPH+1))) GO TO 10069
            IP1=IAM(IP1+5)
          GO TO 10068
10069     CONTINUE
10070     CONTINUE
          IF (.NOT.(IAM(IP1+1).GE.IAM(IPH+1)-IAM(2))) GO TO 10071
            IF (.NOT.(IAM(IP1+7).GT.1.AND.IAM(IAM(IP1+4)+1).GT.IAM(IP1+1
     +).AND.IAM(IAM(IP1+4)+1).GE.IAM(IPH+1))) GO TO 10072
              IF (.NOT.(IAU.EQ.1)) GO TO 10073
                YTM=REAL(IAM(IP1+2))+
     +          (XCI-REAL(IAM(IP1+1)))*
     +       (REAL(IAM(IAM(IP1+4)+2)-IAM(IP1+2))/                      R
     +EAL(IAM(IAM(IP1+4)+1)-IAM(IP1+1)))
              GO TO 10074
10073         CONTINUE
                YTM=REAL(DBLE(IAM(IP1+2))+
     +          (DBLE(XCI)-DBLE(IAM(IP1+1)))*
     +       (DBLE(IAM(IAM(IP1+4)+2)-IAM(IP1+2))/                      D
     +BLE(IAM(IAM(IP1+4)+1)-IAM(IP1+1))))
10074         CONTINUE
              IF (.NOT.(YTM.GT.YCI.AND.YTM.LT.YCO)) GO TO 10075
                IOF=IP1
                YCO=YTM
10075         CONTINUE
10072       CONTINUE
            IF (.NOT.(IAM(IAM(IP1+3)+7).GT.1.AND.IAM(IAM(IP1+3)+1).GT.IA
     +M(IP1+1).AND.IAM(IAM(IP1+3)+1).GE.IAM(IPH+1))) GO TO 10076
              IF (.NOT.(IAU.EQ.1)) GO TO 10077
                YTM=REAL(IAM(IP1+2))+
     +          (XCI-REAL(IAM(IP1+1)))*
     +       (REAL(IAM(IAM(IP1+3)+2)-IAM(IP1+2))/                      R
     +EAL(IAM(IAM(IP1+3)+1)-IAM(IP1+1)))
              GO TO 10078
10077         CONTINUE
                YTM=REAL(DBLE(IAM(IP1+2))+
     +          (DBLE(XCI)-DBLE(IAM(IP1+1)))*
     +       (DBLE(IAM(IAM(IP1+3)+2)-IAM(IP1+2))/                      D
     +BLE(IAM(IAM(IP1+3)+1)-IAM(IP1+1))))
10078         CONTINUE
              IF (.NOT.(YTM.GT.YCI.AND.YTM.LT.YCO)) GO TO 10079
                IOF=IAM(IP1+3)
                YCO=YTM
10079         CONTINUE
10076       CONTINUE
            IP1=IAM(IP1+6)
          GO TO 10070
10071     CONTINUE
          IF (.NOT.(IOF.NE.0)) GO TO 10080
            IX0=IAM(IPH+1)
            IY0=IAM(IPH+2)
            IF (.NOT.(INT(YCO+.5).NE.IY0)) GO TO 10081
              IPI=18
              L10083=    1
              GO TO 10083
10082         CONTINUE
              IY0=INT(YCO+.5)
              L10083=    2
              GO TO 10083
10084         CONTINUE
              IAM(IPN+7)=1
10081       CONTINUE
            IF (.NOT.((IX0.NE.IAM(IOF+1).OR.IY0.NE.IAM(IOF+2)).AND.(IX0.
     +NE.IAM(IAM(IOF+4)+1).OR.IY0.NE.IAM(IAM(IOF+4)+2)))) GO TO 10085
              IPI=IOF
              L10083=    3
              GO TO 10083
10086         CONTINUE
              IAM(IPN)=IAM(IPI)
              IAM(IPN+7)=IAM(IPI+7)
              IAM(IPN+8)=IAM(IPI+8)
              IAM(IPN+9)=IAM(IPI+9)
10085       CONTINUE
10080     CONTINUE
C
10067   CONTINUE
C
      GO TO 10003
10006 CONTINUE
C
C Zero the markers for all the nodes.
C
      DO 10087 IPT=8,IAM(5)-9,10
        IAM(IPT)=0
10087 CONTINUE
C
C
C Now, make a pass through the area map, tracing one subarea at a time
C and calling the routine APR to do with it what the user wants.
C
C Each pass through the following loop traces the boundary of one
C subarea.
C
C
      IPT=8
C
10088 CONTINUE
C
C Move to the right across the area map, looking for an edge segment
C that has not yet been completely processed.  If no such segment can
C be found, all subareas have been done.
C
10089   CONTINUE
        IF (.NOT.(IAM(IPT).GE.3.OR.IAM(IPT+7).LE.1)) GO TO 10090
          IPT=IAM(IPT+5)
          IF (IPT.EQ.18) GO TO 10091
        GO TO 10089
10090   CONTINUE
C
C Decide whether to scan the subarea to the left of the edge being
C traced (IPU=1) or the one to the right (IPU=2).
C
        IF (.NOT.(IAM(IPT).EQ.0.OR.IAM(IPT).EQ.2)) GO TO 10092
          IPU=1
          IAQ=IPT+8
        GO TO 10093
10092   CONTINUE
          IPU=2
          IAQ=IPT+9
10093   CONTINUE
C
C Store the first group identifier and area identifier for the subarea
C and clear the flag that is set when all identifiers have been found.
C
        NAI=1
        IAI(NAI)=IAM(IAQ)
        IF (IAI(NAI).GE.IAM(6)) IAI(NAI)=IAM(IAI(NAI))/2
        IAG(NAI)=IAM(IAM(IPT+7))/2
C
        IAF=0
C
C IPQ points to the node defining the beginning of the edge segment,
C IPR to the node defining the end of the edge segment, and IPS to
C the node defining the beginning of the edge, so that we can tell
C when we've gone all the way around it.
C
        IPQ=IAM(IPT+4)
        IPR=IPT
        IPS=IPQ
        IPM=IPR
        IPV=IPU
C
C Put the first point in the list defining this subarea.
C
        NCS=1
        XCS(1)=REAL(IAM(IPQ+1))/RLC
        YCS(1)=REAL(IAM(IPQ+2))/RLC
C
C Jira NCL_32: Handle special case of IPP in starting segment is a
C node inserted due to splitting a segment to insert a hole-connector.
C In that case, we want to use original segment's coords for angle
C calculation.
        IF (.NOT.(IPQ.GT.ILW)) GO TO 10094
          IF (IAM(IPQ+3).NE.IPT) IFRMXY=IAM(IPQ+3)
          IF (IAM(IPQ+4).NE.IPT) IFRMXY=IAM(IPQ+4)
        GO TO 10095
10094   CONTINUE
          IFRMXY=0
10095   CONTINUE
 
C
C Each pass through the following loop moves one step along the edge of
C the subarea.
C
10096   CONTINUE
C
C Add the end of the current segment to the description of the subarea.
C
          IF (.NOT.(NCS.LT.MCS)) GO TO 10097
            NCS=NCS+1
            XCS(NCS)=REAL(IAM(IPR+1))/RLC
            YCS(NCS)=REAL(IAM(IPR+2))/RLC
          GO TO 10098
10097     CONTINUE
            CALL SETER ('ARSCAM - MCS TOO SMALL',8,1)
            RETURN
10098     CONTINUE
C
C If the group/area identifier information is incomplete and the current
C edge segment spans a portion of the X axis of non-zero length, scan
C outwards from the subarea for group/area identifier information.
C
          IF (.NOT.(IAF.EQ.0.AND.IAM(IPQ+1).NE.IAM(IPR+1))) GO TO 10099
C
            IF (.NOT.(IAM(IPQ+1).LT.IAM(IPR+1))) GO TO 10100
              IP1=IPQ
              IP2=IPR
              IDU=3-IPU
            GO TO 10101
10100       CONTINUE
              IP1=IPR
              IP2=IPQ
              IDU=IPU
10101       CONTINUE
C
            IXC=INT(.5*(REAL(IAM(IP1+1))+REAL(IAM(IP2+1))))
            XCO=REAL(IXC)+.5
            IF (.NOT.(IAU.EQ.1)) GO TO 10102
              YCO=REAL(IAM(IP1+2))+(XCO-REAL(IAM(IP1+1)))*
     +     (REAL(IAM(IP2+2)-IAM(IP1+2))/
     +REAL(IAM(IP2+1)-IAM(IP1+1)))
            GO TO 10103
10102       CONTINUE
              YCO=REAL(DBLE(IAM(IP1+2))+
     +        (DBLE(XCO)-DBLE(IAM(IP1+1)))*
     +     (DBLE(IAM(IP2+2)-IAM(IP1+2))/
     +DBLE(IAM(IP2+1)-IAM(IP1+1))))
10103       CONTINUE
C
            IGI=LAM
C
10104       CONTINUE
            IF (.NOT.(IGI.GT.IAM(6))) GO TO 10105
              IGI=IGI-1
              IF (.NOT.(MOD(IAM(IGI),2).EQ.0.AND.IAM(IPT+7).NE.IGI)) GO
     +TO 10106
                IAF=0
                IF (.NOT.(IDU.EQ.1)) GO TO 10107
                  YCI=RLP
                GO TO 10108
10107           CONTINUE
                  YCI=-1.
10108           CONTINUE
                IP3=IAM(IP2+6)
10109           CONTINUE
                IF (.NOT.(IAM(IP3+1)+IAM(2).GT.IXC)) GO TO 10110
                  IF (.NOT.(IAM(IP3+1).LE.IXC)) GO TO 10111
                    IF (.NOT.(ABS(IAM(IP3+7)).EQ.IGI.AND.IAM(IP3+1).LT.I
     +AM(IAM(IP3+4)+1).AND.IXC.LT.IAM(IAM(IP3+4)+1))) GO TO 10112
                      IF (.NOT.(IAU.EQ.1)) GO TO 10113
                        YTM=REAL(IAM(IP3+2))+
     +                  (XCO-REAL(IAM(IP3+1)))*
     +               (REAL(IAM(IAM(IP3+4)+2)-
     +                   IAM(IP3      +2))/
     +       REAL(IAM(IAM(IP3+4)+1)-
     +          IAM(IP3      +1)))
                      GO TO 10114
10113                 CONTINUE
                        YTM=REAL(DBLE(IAM(IP3+2))+
     +                  (DBLE(XCO)-DBLE(IAM(IP3+1)))*
     +               (DBLE(IAM(IAM(IP3+4)+2)-
     +                   IAM(IP3      +2))/
     +       DBLE(IAM(IAM(IP3+4)+1)-
     +          IAM(IP3      +1))))
10114                 CONTINUE
                      IF (.NOT.(IDU.EQ.1.AND.YTM.LT.YCI.AND.YTM.GE.YCO))
     + GO TO 10115
                        IAF=IP3
                        IAQ=IAF+8
                        YCI=YTM
10115                 CONTINUE
                      IF (.NOT.(IDU.EQ.2.AND.YTM.GT.YCI.AND.YTM.LE.YCO))
     + GO TO 10116
                        IAF=IP3
                        IAQ=IAF+9
                        YCI=YTM
10116                 CONTINUE
10112               CONTINUE
                    IF (.NOT.(ABS(IAM(IAM(IP3+3)+7)).EQ.IGI.AND.IAM(IP3+
     +1).LT.IAM(IAM(IP3+3)+1).AND.IXC.LT.IAM(IAM(IP3+3)+1))) GO TO 10117
                      IF (.NOT.(IAU.EQ.1)) GO TO 10118
                        YTM=REAL(IAM(IP3+2))+
     +                  (XCO-REAL(IAM(IP3+1)))*
     +               (REAL(IAM(IAM(IP3+3)+2)-
     +                   IAM(IP3      +2))/
     +       REAL(IAM(IAM(IP3+3)+1)-
     +          IAM(IP3      +1)))
                      GO TO 10119
10118                 CONTINUE
                        YTM=REAL(DBLE(IAM(IP3+2))+
     +                  (DBLE(XCO)-DBLE(IAM(IP3+1)))*
     +               (DBLE(IAM(IAM(IP3+3)+2)-
     +                   IAM(IP3      +2))/
     +       DBLE(IAM(IAM(IP3+3)+1)-
     +          IAM(IP3      +1))))
10119                 CONTINUE
                      IF (.NOT.(IDU.EQ.1.AND.YTM.LT.YCI.AND.YTM.GE.YCO))
     + GO TO 10120
                        IAF=IAM(IP3+3)
                        IAQ=IAF+9
                        YCI=YTM
10120                 CONTINUE
                      IF (.NOT.(IDU.EQ.2.AND.YTM.GT.YCI.AND.YTM.LE.YCO))
     + GO TO 10121
                        IAF=IAM(IP3+3)
                        IAQ=IAF+8
                        YCI=YTM
10121                 CONTINUE
10117               CONTINUE
10111             CONTINUE
                  IP3=IAM(IP3+6)
                GO TO 10109
10110           CONTINUE
                IF (.NOT.(IAF.NE.0)) GO TO 10122
                  IF (.NOT.(NAI.LT.MAI)) GO TO 10123
                    NAI=NAI+1
                    IF (.NOT.(IAM(IAQ).LT.IAM(6))) GO TO 10124
                      IAI(NAI)=IAM(IAQ)
                    GO TO 10125
10124               CONTINUE
                      IAI(NAI)=IAM(IAM(IAQ))/2
10125               CONTINUE
                    IAG(NAI)=IAM(IGI)/2
                  GO TO 10126
10123             CONTINUE
                    CALL SETER ('ARSCAM - MAI TOO SMALL',9,1)
                    RETURN
10126             CONTINUE
10122           CONTINUE
10106         CONTINUE
            GO TO 10104
10105       CONTINUE
            IAF=1
10099     CONTINUE
C
C Move IPQ to IPP and IPR to IPQ.
C
          IPP=IPQ
          IPQ=IPR
C
C Get the coordinates of the ends of the edge segment for use in
C computing change in direction to a possible next point.
C
C Jira NCL_32: if IPP is from a segment that was split to insert a
C hole-connector, use original segment's coords. in angle calculation.
          IF (.NOT.(IFRMXY.GT.0)) GO TO 10127
            IXP=IAM(IFRMXY+1)
            IYP=IAM(IFRMXY+2)
            IFRMXY=0
          GO TO 10128
10127     CONTINUE
            IXP=IAM(IPP+1)
            IYP=IAM(IPP+2)
10128     CONTINUE
          IXQ=IAM(IPQ+1)
          IYQ=IAM(IPQ+2)
          FXP=REAL(IXP)
          FYP=REAL(IYP)
          FXQ=REAL(IXQ)
          FYQ=REAL(IYQ)
C
C Back up IPR to the beginning of the group of nodes which have the
C same X and Y coordinates as it does.
C
10129     CONTINUE
          IF (.NOT.(IAM(IPR+1).EQ.IAM(IAM(IPR+6)+1).AND.IAM(IPR+2).EQ.IA
     +M(IAM(IPR+6)+2))) GO TO 10130
            IPR=IAM(IPR+6)
          GO TO 10129
10130     CONTINUE
C
C If there is only one node in the group, the exit path is obvious.
C
          IF (.NOT.(IAM(IPR+1).NE.IAM(IAM(IPR+5)+1).OR.IAM(IPR+2).NE.IAM
     +(IAM(IPR+5)+2))) GO TO 10131
            IF (.NOT.(IAM(IAM(IPR+3)+1).NE.IAM(IPP+1).OR.IAM(IAM(IPR+3)+
     +2).NE.IAM(IPP+2))) GO TO 10132
              IF (.NOT.(IAM(IAM(IPR+3)+7).GT.0)) GO TO 10133
                IPM=IAM(IPR+3)
                IPR=IPM
                IPV=IPU
              GO TO 10134
10133         CONTINUE
                IPR=0
10134         CONTINUE
            GO TO 10135
10132       CONTINUE
              IF (.NOT.(IAM(IPR+7).GT.0)) GO TO 10136
                IPM=IPR
                IPR=IAM(IPR+4)
                IPV=3-IPU
              GO TO 10137
10136         CONTINUE
                IPR=0
10137         CONTINUE
10135       CONTINUE
C
C Otherwise, go through the group of nodes, examining all the possible
C ways to move from the current position to a new one.  Pick the
C direction which is leftmost (if IPU=1) or rightmost (if IPU=2).
C
          GO TO 10138
10131     CONTINUE
C
            IP1=IPR
            IP2=IPR
            IPR=0
            IF (.NOT.(IPU.EQ.1)) GO TO 10139
              ANM=-3.14159265358979
            GO TO 10140
10139       CONTINUE
              ANM=+3.14159265358979
10140       CONTINUE
C
10141       CONTINUE
            IF (.NOT.(IAM(IP2+1).EQ.IAM(IP1+1).AND.IAM(IP2+2).EQ.IAM(IP1
     ++2))) GO TO 10142
              IF (.NOT.(IAM(IAM(IP2+3)+7).GT.0.AND.(IAM(IAM(IP2+3)+1).NE
     +.IAM(IPP+1).OR.IAM(IAM(IP2+3)+2).NE.IAM(IPP+2)))) GO TO 10143
C                 Jira NCL_32: If test node is due to splitting a
C                 a line-segment to insert a hole-connector, use the
C                 original segment's end points for angle calculation.
                IF (.NOT.(IP2.LT.ILW.AND.IAM(IP2+3).GT.ILW)) GO TO 10144
                  IXR=IAM(IAM(IAM(IP2+3)+3)+1)
                  IYR=IAM(IAM(IAM(IP2+3)+3)+2)
                GO TO 10145
10144           CONTINUE
                  IXR=IAM(IAM(IP2+3)+1)
                  IYR=IAM(IAM(IP2+3)+2)
10145           CONTINUE
                FXR=REAL(IXR)
                FYR=REAL(IYR)
C
                IF (.NOT.(IAU.EQ.1)) GO TO 10146
                  ANG=ARRAT2((FXQ-FXP)*(FYR-FYQ)-(FYQ-FYP)*(FXR-FXQ),
     +                       (FXQ-FXP)*(FXR-FXQ)+(FYQ-FYP)*(FYR-FYQ))
                GO TO 10147
10146           CONTINUE
                IF (.NOT.(IAU.EQ.2)) GO TO 10148
                  ANG=ARDAT2(DBLE(IXQ-IXP)*DBLE(IYR-IYQ)-
     +                       DBLE(IYQ-IYP)*DBLE(IXR-IXQ),
     +                       DBLE(IXQ-IXP)*DBLE(IXR-IXQ)+
     +                       DBLE(IYQ-IYP)*DBLE(IYR-IYQ))
                GO TO 10147
10148           CONTINUE
                  IO1(3,1)=IXQ-IXP
                  IO1(3,2)=IYR-IYQ
                  IO1(3,3)=IYQ-IYP
                  IO1(3,4)=IXR-IXQ
                  CALL ARMPIA (IO1,DP1,IER)
                  IF (.NOT.(IER.NE.0)) GO TO 10149
                    GO TO 10028
10149             CONTINUE
                  CALL ARMPIA (IO2,DP2,IER)
                  IF (.NOT.(IER.NE.0)) GO TO 10151
                    GO TO 10028
10151             CONTINUE
                  ANG=ARDAT2(DP1,DP2)
10147           CONTINUE
C
                IF (.NOT.(IPU.EQ.1)) GO TO 10153
C Jira NCL_32: jamison test case, where angle along an original
C segment and angle along temp segment is the same, we prefer the
C original.
                  IF (.NOT.(ANG.EQ.ANM.AND.IPR.GT.ILW)) GO TO 10154
                    IPR=IAM(IP2+3)
                    ANM=ANG
                    IPM=IPR
                    IPV=1
                    IFRMXY=0
10154             CONTINUE
                  IF (.NOT.(ANG.GT.ANM)) GO TO 10155
                    IPR=IAM(IP2+3)
                    ANM=ANG
                    IPM=IPR
                    IPV=1
C Jira NCL_32: case where potential next IPP is a node inserted to
C create a hole-connector. We want the original segment's coords for
C angle calculation.
C Note: because the first node (bottom) of the first hole-connector
C points *back* to a user-inserted node (i.e., IAM(IP2+$PD$).LT.ILW),
C we need to exclude that one special case from the following test;
C hence the factor (ILW+1).
                    IF (.NOT.(IP2.GT.(ILW+1).AND.IAM(IP2+4).LT.ILW)) GO
     +TO 10156
                      IFRMXY = IAM(IP2+4)
                    GO TO 10157
10156               CONTINUE
                      IFRMXY=0
10157               CONTINUE
10155             CONTINUE
                GO TO 10158
10153           CONTINUE
                  IF (.NOT.(ANG.LT.ANM)) GO TO 10159
                    IPR=IAM(IP2+3)
                    ANM=ANG
                    IPM=IPR
                    IPV=2
C Jira NCL_32: case where potential next IPP is a node inserted to
C create a hole-connector. We want the original segment's coords for
C angle calculation.
C Note: because the first node (bottom) of the first hole-connector
C points *back* to a user-inserted node (i.e., IAM(IP2+$PD$).LT.ILW),
C we need to exclude that one special case from the following test;
C hence the factor (ILW+1).
                    IF (.NOT.(IP2.GT.(ILW+1).AND.IAM(IP2+4).LT.ILW)) GO
     +TO 10160
                      IFRMXY = IAM(IP2+4)
                    GO TO 10161
10160               CONTINUE
                      IFRMXY=0
10161               CONTINUE
10159             CONTINUE
10158           CONTINUE
10143         CONTINUE
              IF (.NOT.(IAM(IP2+7).GT.0.AND.(IAM(IAM(IP2+4)+1).NE.IAM(IP
     +P+1).OR.IAM(IAM(IP2+4)+2).NE.IAM(IPP+2)))) GO TO 10162
C                 Jira NCL_32: If test node is due to splitting a
C                 a line-segment to insert a hole-connector, use the
C                 original segment's end points for angle calculation.
                IF (.NOT.(IP2.LT.ILW.AND.IAM(IP2+4).GT.ILW)) GO TO 10163
                  IXR=IAM(IAM(IAM(IP2+4)+4)+1)
                  IYR=IAM(IAM(IAM(IP2+4)+4)+2)
                GO TO 10164
10163           CONTINUE
                  IXR=IAM(IAM(IP2+4)+1)
                  IYR=IAM(IAM(IP2+4)+2)
10164           CONTINUE
                FXR=REAL(IXR)
                FYR=REAL(IYR)
C
                IF (.NOT.(IAU.EQ.1)) GO TO 10165
                  ANG=ARRAT2((FXQ-FXP)*(FYR-FYQ)-(FYQ-FYP)*(FXR-FXQ),
     +                       (FXQ-FXP)*(FXR-FXQ)+(FYQ-FYP)*(FYR-FYQ))
                GO TO 10166
10165           CONTINUE
                IF (.NOT.(IAU.EQ.2)) GO TO 10167
                  ANG=ARDAT2(DBLE(IXQ-IXP)*DBLE(IYR-IYQ)-
     +                       DBLE(IYQ-IYP)*DBLE(IXR-IXQ),
     +                       DBLE(IXQ-IXP)*DBLE(IXR-IXQ)+
     +                       DBLE(IYQ-IYP)*DBLE(IYR-IYQ))
                GO TO 10166
10167           CONTINUE
                  IO1(3,1)=IXQ-IXP
                  IO1(3,2)=IYR-IYQ
                  IO1(3,3)=IYQ-IYP
                  IO1(3,4)=IXR-IXQ
                  CALL ARMPIA (IO1,DP1,IER)
                  IF (.NOT.(IER.NE.0)) GO TO 10168
                    GO TO 10028
10168             CONTINUE
                  CALL ARMPIA (IO2,DP2,IER)
                  IF (.NOT.(IER.NE.0)) GO TO 10170
                    GO TO 10028
10170             CONTINUE
                  ANG=ARDAT2(DP1,DP2)
10166           CONTINUE
C
                IF (.NOT.(IPU.EQ.1)) GO TO 10172
C Jira NCL_32: jamison test case, where angle along an original
C segment and angle along temp segment is the same, we prefer the
C original.
                  IF (.NOT.(ANG.EQ.ANM.AND.IPR.GT.ILW)) GO TO 10173
                    IPR=IAM(IP2+4)
                    ANM=ANG
                    IPM=IP2
                    IPV=2
                    IFRMXY=0
10173             CONTINUE
                  IF (.NOT.(ANG.GT.ANM)) GO TO 10174
                    IPR=IAM(IP2+4)
                    ANM=ANG
                    IPM=IP2
                    IPV=2
C Jira NCL_32: case where potential next IPP is a node inserted to
C create a hole-connector. We want the original segment's coords for
C angle calculation.
                    IF (.NOT.(IP2.GT.ILW.AND.IAM(IP2+3).LT.ILW)) GO TO 1
     +0175
                      IFRMXY=IAM(IP2+3)
                    GO TO 10176
10175               CONTINUE
                      IFRMXY=0
10176               CONTINUE
10174             CONTINUE
                GO TO 10177
10172           CONTINUE
                  IF (.NOT.(ANG.LT.ANM)) GO TO 10178
                    IPR=IAM(IP2+4)
                    ANM=ANG
                    IPM=IP2
                    IPV=1
C Jira NCL_32: case where potential next IPP is a node inserted to
C create a hole-connector. We want the original segment's coords for
C angle calculation.
                    IF (.NOT.(IP2.GT.ILW.AND.IAM(IP2+3).LT.ILW)) GO TO 1
     +0179
                      IFRMXY=IAM(IP2+3)
                    GO TO 10180
10179               CONTINUE
                      IFRMXY=0
10180               CONTINUE
10178             CONTINUE
10177           CONTINUE
10162         CONTINUE
              IP2=IAM(IP2+5)
            GO TO 10141
10142       CONTINUE
C
10138     CONTINUE
C
C If no possible exit was found, reverse direction.
C
          IF (.NOT.(IPR.EQ.0)) GO TO 10181
            IPR=IPP
            IPV=3-IPV
10181     CONTINUE
C
C Update the markers for the edge segment picked.
C
          IF (.NOT.(IPV.EQ.1)) GO TO 10182
            IF (.NOT.(MOD(IAM(IPM),2).EQ.0)) GO TO 10183
              IAM(IPM)=IAM(IPM)+1
            GO TO 10184
10183       CONTINUE
              CALL SETER ('ARSCAM - ALGORITHM FAILURE',10,1)
              RETURN
10184       CONTINUE
          GO TO 10185
10182     CONTINUE
            IF (.NOT.(MOD(IAM(IPM)/2,2).EQ.0)) GO TO 10186
              IAM(IPM)=IAM(IPM)+2
            GO TO 10187
10186       CONTINUE
              CALL SETER ('ARSCAM - ALGORITHM FAILURE',11,1)
              RETURN
10187       CONTINUE
10185     CONTINUE
C
C Exit if we're passing the start of the subarea.
C
          IF (IAM(IPQ+1).EQ.IAM(IPS+1).AND.IAM(IPQ+2).EQ.IAM(IPS+2).AND.
     +IAM(IPR+1).EQ.IAM(IPT+1).AND.IAM(IPR+2).EQ.IAM(IPT+2)) GO TO 10188
C
        GO TO 10096
10188   CONTINUE
C
C A complete subarea has been found.  Let the user do what he wants
C with it.
C
        IF (.NOT.(NAI.EQ.IAM(7))) GO TO 10189
          IDI=IPU
          CALL APR (XCS,YCS,NCS,IAI,IAG,NAI)
          IF (ICFELL('ARSCAM',12).NE.0) RETURN
        GO TO 10190
10189   CONTINUE
          IF (.NOT.(IAF.NE.0)) GO TO 10191
            CALL SETER ('ARSCAM - ALGORITHM FAILURE',13,1)
            RETURN
10191     CONTINUE
10190   CONTINUE
C
      GO TO 10088
10091 CONTINUE
C
C Delete the nodes used to put in the temporary connecting lines.
C
      IPT=IAM(5)-9
10192 CONTINUE
      IF (.NOT.(IPT.GT.ILW)) GO TO 10193
        IAM(IAM(IPT+4)+3)=IAM(IPT+3)
        IAM(IAM(IPT+3)+4)=IAM(IPT+4)
        IAM(IAM(IPT+6)+5)=IAM(IPT+5)
        IAM(IAM(IPT+5)+6)=IAM(IPT+6)
        IPT=IPT-10
      GO TO 10192
10193 CONTINUE
      IAM(5)=ILW
C
C Zero the markers in all the remaining nodes.
C
      DO 10194 IPT=8,IAM(5)-9,10
        IAM(IPT)=0
10194 CONTINUE
C
C
C Done.  Restore the SET parameters and return to the caller.
C
      CALL SET (FFL,FFR,FFB,FFT,FUL,FUR,FUB,FUT,ILL)
      IF (ICFELL('ARSCAM',14).NE.0) RETURN
C
      RETURN
C
C This internal procedure adds a new point in the existing part of the
C area map.
C
10083 CONTINUE
        IPN=IAM(5)+1
        IF (.NOT.(IAM(5)+10.GE.IAM(6))) GO TO 10195
          CALL SETER ('ARSCAM - AREA-MAP ARRAY OVERFLOW',15,1)
          RETURN
10195   CONTINUE
        IAM(5)=IAM(5)+10
        IAM(IPN)=0
        IAM(IPN+1)=IX0
        IAM(IPN+2)=IY0
        IAM(IPN+3)=IPI
        IAM(IPN+4)=IAM(IPI+4)
        IAM(IAM(IPI+4)+3)=IPN
        IAM(IPI+4)=IPN
10196   CONTINUE
          IF (.NOT.(IAM(IPN+1).LT.IAM(IPX+1))) GO TO 10197
            IPX=IAM(IPX+6)
          GO TO 10198
10197     CONTINUE
          IF (.NOT.(IAM(IPN+1).GT.IAM(IAM(IPX+5)+1))) GO TO 10199
            IPX=IAM(IPX+5)
          GO TO 10198
10199     CONTINUE
10200       CONTINUE
              IF (.NOT.(IAM(IPN+1).EQ.IAM(IPX+1).AND.IAM(IPN+2).LT.IAM(I
     +PX+2))) GO TO 10201
                IPX=IAM(IPX+6)
              GO TO 10202
10201         CONTINUE
              IF (.NOT.(IAM(IPN+1).EQ.IAM(IAM(IPX+5)+1).AND.IAM(IPN+2).G
     +T.IAM(IAM(IPX+5)+2))) GO TO 10203
                IPX=IAM(IPX+5)
              GO TO 10202
10203         CONTINUE
                GO TO 10204
10202         CONTINUE
            GO TO 10200
10204       CONTINUE
            GO TO 10205
10198     CONTINUE
        GO TO 10196
10205   CONTINUE
        IAM(IPN+5)=IAM(IPX+5)
        IAM(IPN+6)=IAM(IAM(IPX+5)+6)
        IAM(IAM(IPX+5)+6)=IPN
        IAM(IPX+5)=IPN
        IAM(IPN+7)=0
        IAM(IPN+8)=0
        IAM(IPN+9)=0
      GO TO (10082,10084,10086) , L10083
C
C This internal procedure is called when an error occurs in ARMPIA.
C
10028 CONTINUE
        CALL SETER
     +  ('ARSCAM/ARMPIA - MULTIPLE-PRECISION QUANTITY IS TOO BIG',
     +                                                       16,1)
        RETURN
C
      END
