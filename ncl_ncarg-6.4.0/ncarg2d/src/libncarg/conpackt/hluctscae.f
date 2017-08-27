      SUBROUTINE HLUCTSCAE (ICRA,ICA1,ICAM,ICAN,XCPF,YCPF,XCQF,YCQF,
     +                                          IND1,IND2,ICAF,IAID)
C
      DIMENSION ICRA(ICA1,*)
C
C This routine stands between CONPACKT and the user call-back routine
C CTSCAE.  When HLUs are not in use, this version of the routine gets
C loaded, so that CTSCAE is called.  When HLUs are in use, another
C version gets loaded; it either does the appropriate thing for the
C purposes of the HLUs or calls CTSCAE.
C
      CALL CTSCAE (ICRA,ICA1,ICAM,ICAN,XCPF,YCPF,XCQF,YCQF,
     +                                 IND1,IND2,ICAF,IAID)
C
      RETURN
C
      END
