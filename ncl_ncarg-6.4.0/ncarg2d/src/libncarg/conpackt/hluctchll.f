      SUBROUTINE HLUCTCHLL (IFLG)
C
C This routine stands between CONPACKT and the user call-back routine
C CTCHLL.  When HLUs are not in use, this version of the routine gets
C loaded, so that CTCHLL is called.  When HLUs are in use, another
C version gets loaded; it either does the appropriate thing for the
C purposes of the HLUs or calls CTCHLL.
C
      CALL CTCHLL (IFLG)
C
      RETURN
C
      END
