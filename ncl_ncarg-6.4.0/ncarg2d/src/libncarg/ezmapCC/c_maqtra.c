/*
 *	$Id: c_maqtra.c,v 1.2 2008-07-23 16:16:48 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

#include <ncarg/ncargC.h>

extern void NGCALLF(maqtra,MAQTRA)(float*,float*,float*,float*);

void c_maqtra
#ifdef NeedFuncProto
(
    float rlat,
    float rlon,
    float *uval,
    float *vval
)
#else
(rlat,rlon,uval,vval)
    float rlat;
    float rlon;
    float *uval;
    float *vval;
#endif
{
    NGCALLF(maqtra,MAQTRA)(&rlat,&rlon,uval,vval);
}
