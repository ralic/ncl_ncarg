      SUBROUTINE HLUCTCHCL (IFLG)
C
C This routine stands between CONPACKT and the user call-back routine
C CTCHCL.  When HLUs are not in use, this version of the routine gets
C loaded, so that CTCHCL is called.  When HLUs are in use, another
C version gets loaded; it either does the appropriate thing for the
C purposes of the HLUs or calls CTCHCL.
C
      CALL CTCHCL (IFLG)
C
      RETURN
C
      END
