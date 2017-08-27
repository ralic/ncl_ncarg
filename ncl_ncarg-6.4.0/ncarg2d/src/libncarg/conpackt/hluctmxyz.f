      SUBROUTINE HLUCTMXYZ (IMAP,XINP,YINP,ZINP,XOTP,YOTP)
C
C This routine stands between CONPACKT and the user call-back routine
C CTMXYZ.  When HLUs are not in use, this version of the routine gets
C loaded, so that CTMXYZ is called.  When HLUs are in use, another
C version gets loaded; it either does the appropriate thing for the
C purposes of the HLUs or calls CTMXYZ.
C
      CALL CTMXYZ (IMAP,XINP,YINP,ZINP,XOTP,YOTP)
C
      RETURN
C
      END
