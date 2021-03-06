/*
 *	$Id: sunraster.h,v 1.4 2008-07-27 03:22:40 haley Exp $
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

#include	<pixrect/pixrect_hs.h>

#define	TILE_SIZE		16	/* hatch pattern replication tile size*/
#define	MAX_POLYGON_POINTS	1024	/* max polygon handled by hardware */

#define	POINT_BUF_ALLOCED	20
static	struct	{
	struct	pr_pos	*p;
	int	size;
	} pointBuf = {NULL, 0};

#define	INITIAL_COLOR_ALLOCATION	2
#define	MAX_COLOR	256
#define	MAX_COLOR_INDEX	256
#define	UNALLOCATED	-1

typedef	struct	{
	unsigned char	red,
			green,
			blue;
	} Rgb;

static	struct	{
	int	default_i[MAX_COLOR_INDEX];
	int	current_i[MAX_COLOR_INDEX];
	Rgb	default_rgb[MAX_COLOR_INDEX];
	Rgb	current_rgb[MAX_COLOR_INDEX];
	int	default_index;
	int	current_index;
	int	*next_new_index;
	Rgb	*rgb;
	int	*index;
	} color_tab;

