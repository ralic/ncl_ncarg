      SUBROUTINE HLUVTMXYZ (IMAP,XINP,YINP,ZINP,XOTP,YOTP)
C
C This routine stands between VASPACKT and the user call-back routine
C VTMXYZ.  When HLUs are not in use, this version of the routine gets
C loaded, so that VTMXYZ is called.  When HLUs are in use, another
C version gets loaded; it either does the appropriate thing for the
C purposes of the HLUs or calls VTMXYZ.
C
      CALL VTMXYZ (IMAP,XINP,YINP,ZINP,XOTP,YOTP)
C
      RETURN
C
      END
