#########################################################################
#                                                                       #
#       This Makefile was created by the "ymake" utility.               #
#       If you wish to make changes in the "Makefile" or "makefile",    #
#       do so by making changes to "yMakefile" or "ymakefile"           #
#       and executing "make local-Makefile".                            #
#                                                                       #
#########################################################################

# ########################################################################
#
# In main Template
#
# ########################################################################

SYSTEM_INCLUDE		= "Darwin_Intel"
DATE                    = $(shell date +%d%b%Y_%H%M)

OS1_DEF		=	-DDarwin
OS2_DEF		=
OS_DEF		=	$(OS1_DEF) $(OS2_DEF)
ARCH_DEF	=	-Dx86_64
VENDOR_DEF	=	-DApple

MAJOR_DEF	=	-DMAJOR=17
MINOR_DEF	=	-DMINOR=0

SUBMINOR_DEF	=	-DSUBMINOR=0

# ########################################################################
#
# Including Site - first time
#
# ########################################################################

# ########################################################################
#
# After Including Site - first time
#
# ########################################################################

# ########################################################################
#
# Including "Darwin_Intel"
#
# ########################################################################

FC = $(F77)

# ########################################################################
#
# After Including "Darwin_Intel"
#
# ########################################################################

# ########################################################################
#
# Including Site - second time
#
# ########################################################################

# ########################################################################
#
# After Including Site - second time
#
# ########################################################################

YMAKEROOT		= /usr/local/Cellar/ncl_ncarg/6.4.0
BINROOT			= /usr/local/Cellar/ncl_ncarg/6.4.0/bin
LIBROOT			= /usr/local/Cellar/ncl_ncarg/6.4.0/lib
INCROOT			= /usr/local/Cellar/ncl_ncarg/6.4.0/include
MANROOT			= /usr/local/Cellar/ncl_ncarg/6.4.0/man
TMPROOT			= /tmp

ROOTPATH		= /usr/local/Cellar/ncl_ncarg/6.4.0
BINPATH			= /usr/local/Cellar/ncl_ncarg/6.4.0/bin
LIBPATH			= /usr/local/Cellar/ncl_ncarg/6.4.0/lib
INCPATH			= /usr/local/Cellar/ncl_ncarg/6.4.0/include
MANPATH			= /usr/local/Cellar/ncl_ncarg/6.4.0/man

LIBSEARCH		= -L/opt/X11/lib
INCSEARCH		= -I/opt/X11/include
LIBSEARCHUSER		= -L/opt/X11/lib
INCSEARCHUSER		= -I/opt/X11/include
PYTHONVERSION		=
PYTHONLIBSEARCH		=
PYTHONINCSEARCH		=
PYTHONPKGSDIR           =
PYTHONBINDIR            =

LIB_SEARCH		= $(LIBSEARCH)
INC_SEARCH		= $(INCSEARCH)

TOP			= ../../../../.
CURRENT_DIR		= ./ncarview/src/lib/libncarg_ras

BINDIR			= bin
LIBDIR			= lib
INCDIR			= include
MANDIR			= man
DOCDIR			= doc
NGTMPDIR		= tmp
CONFIGDIR		= config

MANLPATH		= $(MANPATH)/manl
MAN1PATH		= $(MANPATH)/man1
MAN3PATH		= $(MANPATH)/man3
MAN5PATH		= $(MANPATH)/man5
MAN6PATH		= $(MANPATH)/man6
MAN8PATH		= $(MANPATH)/man8

MANL_SECTION		= l
MAN1_SECTION		= 1
MAN3_SECTION		= 3
MAN5_SECTION		= 5
MAN6_SECTION		= 6
MAN8_SECTION		= 8

CONFIG_DIR		= $(CONFIGSRC)

XLIB			= -lXpm -lX11 -lXext
EXTENSIONLIB		= -lXext
XTOOLLIB		= -lXt -lSM -lICE
XMULIB			= -lXmu
XAWLIB			= -lXaw
XMOTIFLIB		= -lXm

XMOTIFCLIENTLIBS	= $(XMOTIFLIB) $(XTOOLLIB) $(XLIB)
XATHENACLIENTLIBS	= $(XAWLIB) $(XTOOLLIB) $(EXTENSIONLIB) $(XLIB)
XMUATHENACLIENTLIBS	= $(XAWLIB) $(XMULIB) $(XTOOLLIB) $(EXTENSIONLIB) $(XLIB)

CTOFLIBS	= -lgfortran -lquadmath
CTOFLIBSUSER	= -lgfortran -lquadmath
MAKE		= make

CC		= gcc
CC_LD		= gcc
CDYNAMIC	=
CSTATIC		=

CCOPTIONS	= -ansi -fPIC -m64 -Wall -fopenmp -std=c99 $(EXTRA_CCOPTIONS)

BUILDINCDIR	= $(TOP)/include

BUILDINCTOP	= ./..

_LIBROOTSEARCH	=	-L$(LIBROOT)
_EXECLIBSEARCH	=	$(RPATHFLAG)$(TOP)/shared:$(LIBROOT)
_SHLIBSEARCH	=	-rpath $(TOP)/shared:$(LIBROOT)

_INCROOTSEARCH	=	-I$(INCROOT)

SHAREDLIBFLAGS =

TOP_INCLUDES	= -I$(BUILDINCDIR)
LDLIBS		= $(_LIBROOTSEARCH) $(LIB_SEARCH) $(EXTRA_LIBS)
CC_LDOPTIONS	= $(CCOPTIONS) $(COPT) $(XTRA_CLDFLAGS) $(SHAREDLIBFLAGS)
FC_LDOPTIONS	= $(FCOPTIONS) $(FOPT) $(XTRA_FLDFLAGS) $(SHAREDLIBFLAGS)

EXTRA_LDLIBS	= $(SYS_LIBS) $(EXTRA_SYS_LIBS) $(DEV_SYS_LIBS)

ALLINCLUDES	= $(EXTRA_INCLUDES) $(TOP_INCLUDES) $(INC_SEARCH)
STD_DEFINES	= -DSYSV -D_POSIX_SOURCE -D_XOPEN_SOURCE -DByteSwapped -D__UNIXOS2__ -D_DARWIN_C_SOURCE
XTRA_CDEFINES	= $(EXTRA_CDEFINES) $(EXTRA_DEFINES)
ALLDEFINES	= $(ALLINCLUDES) $(DEV_DEFINES) $(XTRA_CDEFINES) $(STD_DEFINES) $(PROJECTDEFINES) $(DEFINES)
CFLAGS		= $(CCOPTIONS) $(COPT) $(ALLDEFINES)
XTRA_CLDFLAGS	= $(EXTRA_CLDFLAGS) $(EXTRA_LDFLAGS)
EXTRA_SYS_LIBS	=
EXTRA_EXPORT_FLAGS =

DEPEND		= makedepend
CPP		= '/usr/bin/cpp -traditional'
FCPP		= $(CONFIG_DIR)/fcpp $(CPP)

F77		= gfortran
F77_LD		= gfortran
FCOPTIONS	= -fPIC -fno-range-check -m64 -Wall -fopenmp $(EXTRA_FCOPTIONS)
NGCALLF = reg\#\#_

EXE_SUFFIX	=

XTRA_FDEFINES	= $(EXTRA_FDEFINES) $(EXTRA_DEFINES)
FFLAGS	= $(FCOPTIONS) $(FOPT) $(XTRA_FDEFINES)
XTRA_FLDFLAGS	= $(EXTRA_FLDFLAGS) $(EXTRA_LDFLAGS)

LD		= ld
LD_REL		= ld
RPATHFLAG	= -Wl,-rpath,
AR		= ar
AR_SRC		= ar
AR_SRC_QUICK	= $(AR_SRC) q
AR_SRC_CHECK	= $(AR_SRC) vru
AR_REG		= $(AR) vru
AR_QUICK	= $(AR) q

RM		= rm -f
LEX		= flex
YACC		= yacc
LEXLIB		= -ll
CAT		= cat
CP		= cp
MV		= mv -f
LN		= ln -s
RANLIB		= ranlib
NO_OPT		= touch -c
TBL		= tbl
NROFF		= nroff -man
COL		= col

LINT		= lint
INSTALL		= $(CONFIG_DIR)/ymake-install -c
INSTALL_RM	= $(RM)

INSTALL_BIN	= 0755
INSTALL_MAN	= 0644
INSTALL_DOC	= 0644
INSTALL_APP	= 0644
INSTALL_LIB	= 0644
INSTALL_DB	= 0644
INSTALL_INC	= 0644

CONFIGSRC	= $(TOP)/$(CONFIGDIR)
NEWCONFIGSRC	= ../$(CONFIGSRC)

_YMAKE_EXTRA	=

YMAKE		= $(CONFIGSRC)/ymake -config $(CONFIGSRC) $(_YMAKE_EXTRA)
NEWYMAKE	= $(NEWCONFIGSRC)/ymake -config $(NEWCONFIGSRC) $(_YMAKE_EXTRA)
YMAKEDEP	= $(CONFIGSRC)/ymake-filter

SHELL		= /bin/sh
CALLCONV	= $(CONFIG_DIR)/callconv
MKDIRHIER	= mkdir -p

# ########################################################################
#
# Including Project file
#
# ########################################################################

NGVERSION       = 6.4.0
NCLVERSION      = 6.4.0

NCARGDIR		= ncarg
HLUDIR			= hlu
HLUEXDIR		= hluex
NCLDIR			= ncl
NCLEXDIR		= nclex
PYNGLEXDIR		= pynglex
PYNGLDIR		= PyNGL
NGODIR			= ngo
NFPDIR			= nfp
NFPFORTDIR		= nfpfort
DATADIR			= data
GRIB2CODETABLEDIR	= grib2_codetables
DBDIR			= database
FONTCAPDIR		= fontcaps
GRAPHCAPDIR		= graphcaps
EXAMPLESDIR		= examples
TUTORIALDIR		= tutorial
TESTSDIR		= tests
RESDIR			= resfiles
ROBJDIR			= robj
WWWDIR			= ngwww
XAPPDIR			= xapp

LIBNCARGROOT		= $(LIBROOT)/$(NCARGDIR)
CONFIGROOT		= $(LIBNCARGROOT)/$(CONFIGDIR)
DBROOT			= $(LIBNCARGROOT)/$(DBDIR)
FONTCAPROOT		= $(LIBNCARGROOT)/$(FONTCAPDIR)
GRAPHCAPROOT		= $(LIBNCARGROOT)/$(GRAPHCAPDIR)
DATAROOT		= $(LIBNCARGROOT)/$(DATADIR)
DOCROOT			= $(LIBNCARGROOT)/$(DOCDIR)
EXAMPLESROOT		= $(LIBNCARGROOT)/$(EXAMPLESDIR)
ROBJROOT		= $(LIBNCARGROOT)/$(ROBJDIR)
TESTSROOT		= $(LIBNCARGROOT)/$(TESTSDIR)
TUTORIALROOT		= $(LIBNCARGROOT)/$(TUTORIALDIR)
WWWROOT			= $(LIBNCARGROOT)/$(WWWDIR)
XAPPROOT		= $(LIBNCARGROOT)/$(XAPPDIR)
HLUEXROOT		= $(LIBNCARGROOT)/$(HLUEXDIR)
RESFILEROOT		= $(LIBNCARGROOT)/$(RESDIR)
NCLEXROOT		= $(LIBNCARGROOT)/$(NCLEXDIR)
GRIB2CODETABLEROOT	= $(LIBNCARGROOT)/$(GRIB2CODETABLEDIR)
LIBNCARGPATH		= $(LIBPATH)/$(NCARGDIR)
CONFIGPATH		= $(LIBNCARGPATH)/$(CONFIGDIR)
DBPATH			= $(LIBNCARGPATH)/$(DBDIR)
FONTCAPPATH		= $(LIBNCARGPATH)/$(FONTCAPDIR)
GRAPHCAPPATH		= $(LIBNCARGPATH)/$(GRAPHCAPDIR)
DATAPATH		= $(LIBNCARGPATH)/$(DATADIR)
DOCPATH			= $(LIBNCARGPATH)/$(DOCDIR)
EXAMPLESPATH		= $(LIBNCARGPATH)/$(EXAMPLESDIR)
ROBJPATH		= $(LIBNCARGPATH)/$(ROBJDIR)
TESTSPATH		= $(LIBNCARGPATH)/$(TESTSDIR)
TUTORIALPATH		= $(LIBNCARGPATH)/$(TUTORIALDIR)
WWWPATH			= $(LIBNCARGPATH)/$(WWWDIR)
XAPPPATH		= $(LIBNCARGPATH)/$(XAPPDIR)
LIBNCLPATH		= $(LIBNCARGPATH)/$(NCLDIR)
HLUEXPATH		= $(LIBNCARGPATH)/$(HLUEXDIR)
RESFILEPATH		= $(LIBNCARGPATH)/$(RESDIR)
NCLEXPATH		= $(LIBNCARGPATH)/$(NCLEXDIR)
NCARGINCPATH		= $(INCPATH)/$(NCARGDIR)
HLUINCPATH		= $(NCARGINCPATH)/$(HLUDIR)
NCLINCPATH		= $(NCARGINCPATH)/$(NCLDIR)
NGOINCPATH		= $(NCARGINCPATH)/$(NGODIR)
PYNGLDOCPATH		= webdoc/pyngl
PYNGLBINPATH		= $(PYTHONBINDIR)
PYNGLNCARGPATH		= $(PYTHONPKGSDIR)/$(PYNGLDIR)/$(NCARGDIR)
USRRESFILE		= "~/.hluresfile"
NCARGURL		= "ngurl"
NCARGSITEFILE   = "ncargsite"

ROOTPATHCHECK		= "/lib/ncarg/fontcaps/font1"

NGENV_DESCRIPT	=  		{"tmp","TMPDIR",NULL,"$(TMPROOT)",NULL,	 		"fontcap","FONTCAP",NULL,NULL,NULL,		 		"graphcap","GRAPHCAP",NULL,NULL,NULL,		 		"root",NULL,NULL,"$(ROOTPATH)",$(ROOTPATHCHECK), 		"usrresfile",NULL,NULL,$(USRRESFILE),NULL,	 		"gks_output",NULL,NULL,"gmeta",NULL,		 		"bin",NULL,"root",NULL,NULL,			 		"lib",NULL,"root",NULL,NULL,			 		"include",NULL,"root",NULL,NULL,		 		"man",NULL,"root",NULL,NULL,			 		"ncarg",NULL,"lib",NULL,NULL,			 		"config",NULL,"ncarg",NULL,NULL,		 		"data",NULL,"ncarg",NULL,NULL,			 		"database",NULL,"ncarg",NULL,NULL,		 		"doc",NULL,"ncarg",NULL,NULL,			 		"fontcaps",NULL,"ncarg",NULL,NULL,		 		"graphcaps",NULL,"ncarg",NULL,NULL,		 		"colormaps",NULL,"ncarg",NULL,NULL,		 		"rangs","NCARG_RANGS","database",NULL,NULL,	 		"udunits","NCARG_UDUNITS","ncarg",NULL,NULL,	 		"psadilookup","PSADILOOKUP_PATH",NULL,NULL,NULL, 		"examples",NULL,"ncarg",NULL,NULL,		 		"hluex",NULL,"ncarg",NULL,NULL,			 		"nclex",NULL,"ncarg",NULL,NULL,			 		"pynglex",NULL,"ncarg",NULL,NULL,		 		"resfiles",NULL,"ncarg",NULL,NULL,		 		"tests",NULL,"ncarg",NULL,NULL,			 		"tutorial",NULL,"ncarg",NULL,NULL,		 		"xapp",NULL,"ncarg",NULL,NULL,			 		"ngwww",NULL,"ncarg",NULL,NULL,			 		"ngurl",NULL,"ngwww",NULL,NULL,			 		"sysresfile",NULL,"ncarg",NULL,NULL,		 		"sysappres",NULL,"ncarg",NULL,NULL,		 		"grib2_codetables","NIO_GRIB2_CODETABLES","ncarg",NULL,NULL,  		"ftfonts",NULL,"database",NULL,NULL,             		NULL}

NGENVDEF	=	-DENV_DEFS=$(NGENV_DESCRIPT)

COMMONSRC	= $(TOP)/common/src
NCARG2DSRC	= $(TOP)/ncarg2d/src
NGMATHSRC	= $(TOP)/ngmath/src
NGMATHLIBSRC= $(NGMATHSRC)/lib
NVIEWSRC	= $(TOP)/ncarview/src
NVIEWBINSRC	= $(NVIEWSRC)/bin
NVIEWLIBSRC	= $(NVIEWSRC)/lib
NISRC		= $(TOP)/ni/src
NIBINSRC	= $(NISRC)/bin
NILIBSRC	= $(NISRC)/lib
EXTERNALSRC	= $(TOP)/external
PYNIOSRC        = $(TOP)/pynio
NCARGCLIBSRC	= $(COMMONSRC)/libncarg_c
NCARGLIBSRC	= $(NCARG2DSRC)/libncarg
NCARGGKSLIBSRC	= $(NCARG2DSRC)/libncarg_gks
NCARGOPTLIBSRC	= $(NCARG2DSRC)/liboptional
CGMLIBSRC	= $(NVIEWLIBSRC)/libcgm
CTRANSLIBSRC	= $(NVIEWLIBSRC)/libctrans
ICTRANSLIBSRC	= $(NVIEWLIBSRC)/libictrans
NCARGRASLIBSRC	= $(NVIEWLIBSRC)/libncarg_ras
HLULIBSRC	= $(NILIBSRC)/hlu
BLASSRC	        = $(EXTERNALSRC)/blas
LAPACKSRC       = $(EXTERNALSRC)/lapack
SPHERESRC	= $(EXTERNALSRC)/sphere3.1_dp
FFTPACK5SRC	= $(EXTERNALSRC)/fftpack5_dp
NGOLIBSRC	= $(NILIBSRC)/ngo
XCBLIBSRC	= $(NILIBSRC)/xcb
NFPLIBSRC	= $(NILIBSRC)/nfp
NFPFORTLIBSRC	= $(NILIBSRC)/nfpfort
NCLLIBSRC	= $(NISRC)/ncl
NIOLIBSRC	= $(NILIBSRC)/nio

NCARGCLIB	= -L$(NCARGCLIBSRC) -lncarg_c
NCARGLIB	= -L$(NCARGLIBSRC) -lncarg
NCARGGKSLIB	= -L$(NCARGGKSLIBSRC) -lncarg_gks
NCARGGKSXQFLIB	= -L$(NCARGGKSLIBSRC) -lncarg_gks_xq_fix
NGMATHLIB	= -L$(NGMATHLIBSRC) -lngmath
CGMLIB		= -L$(CGMLIBSRC) -lcgm
NCARGRASLIB	= -L$(NCARGRASLIBSRC) -lncarg_ras
HLULIB		= -L$(HLULIBSRC) -lhlu
NGOLIB		= -L$(NGOLIBSRC) -lngo
NFPLIB		= -L$(NFPLIBSRC) -lnfp
NFPFORTLIB	= -L$(NFPFORTLIBSRC) -lnfpfort
NIOLIB		= -L$(NIOLIBSRC) -lnio
XCBLIB		= -L$(XCBLIBSRC) -lXcb
NCLLIB		= -L$(NCLLIBSRC) -lncl
NCLAPILIB	= -L$(NCLLIBSRC) -lnclapi
BLASLIB         = -L$(BLASSRC) -lblas_ncl
LAPACKLIB       = -L$(LAPACKSRC) -llapack_ncl
SPHERELIB	= -L$(SPHERESRC) -lsphere3.1_dp
FFTPACK5LIB	= -L$(FFTPACK5SRC) -lfftpack5_dp

CTRANSLIB	= $(CTRANSLIBSRC)/libctrans.o
ICTRANS_LIB	= $(ICTRANSLIBSRC)/libictrans_.o

DEPNCARGCLIB	= $(NCARGCLIBSRC)/libncarg_c.a
DEPNCARGLIB	= $(NCARGLIBSRC)/libncarg.a
DEPNCARGGKSLIB	= $(NCARGGKSLIBSRC)/libncarg_gks.a
DEPNCARGGKSXQFLIB= $(NCARGGKSLIBSRC)/libncarg_gks_xq_fix.a
DEPNGMATHLIB	= $(NGMATHLIBSRC)/libngmath.a
DEPCGMLIB	= $(CGMLIBSRC)/libcgm.a
DEPNCARGRASLIB	= $(NCARGRASLIBSRC)/libncarg_ras.a
DEPHLULIB	= $(HLULIBSRC)/libhlu.a
DEPNGOLIB	= $(NGOLIBSRC)/libngo.a
DEPNFPLIB	= $(NFPLIBSRC)/libnfp.a
DEPNFPFORTLIB	= $(NFPFORTLIBSRC)/libnfpfort.a
DEPNIOLIB	= $(NIOLIBSRC)/libnio.a
DEPNCLLIB	= $(NCLLIBSRC)/libncl.a
DEPNCLAPILIB	= $(NCLLIBSRC)/libnclapi.a

DEPCTRANSLIB	= $(CTRANSLIB)
DEPICTRANS_LIB	= $(ICTRANS_LIB)

ICTRANSLIB	= $(ICTRANSLIBSRC)/libictrans.o
DEPICTRANSLIB	= $(ICTRANSLIB)

NCARGLIBS	= $(NCARGLIB) $(NCARGGKSLIB) $(NCARGCLIB)
NCARGXQFLIBS	= $(NCARGLIB) $(NCARGGKSXQFLIB) $(NCARGCLIB)
NGMATHLIBS	= $(NGMATHLIB)
HLULIBS		= $(HLULIB)
NCLLIBS		= $(NCLAPILIB)
NFPLIBS		= $(NFPLIB) $(NFPFORTLIB)
NIOLIBS		= $(NIOLIB)
EXTERNALLIBS    = $(SPHERELIB) $(FFTPACK5LIB) $(LAPACKLIB) $(BLASLIB)

DEPNCARGLIBS	= $(DEPNCARGLIB) $(DEPNCARGGKSLIB) $(DEPNCARGCLIB)
DEPNCARGXQFLIBS	= $(DEPNCARGLIB) $(DEPNCARGGKSXQFLIB) $(DEPNCARGCLIB)
DEPHLULIBS	= $(DEPHLULIB)
DEPNCLLIBS	= $(DEPHLULIB) $(DEPNCLAPILIB)
DEPNFPLIBS	= $(DEPNFPLIB) $(DEPNFPFORTLIB)
DEPNIOLIBS	= $(DEPNIOLIB)

GRAPHCSRC	= $(COMMONSRC)/graphcap
FONTCSRC	= $(COMMONSRC)/fontcap

GRAPHC		= $(GRAPHCSRC)/graphc
FONTC		= $(FONTCSRC)/fontc
IFTRAN      = $(NCARGLIBSRC)/Iftran/Iftran
FSPLIT      = $(NCARGLIBSRC)/Iftran/Fsplit
NYACC      = $(NISRC)/nyacc/nyacc

NETCDFLIB	=

NCDFLIBS	=
PNGLIB		= -lpng -lz
CAIROLIB	= -lcairo -lXrender -lfontconfig -lpixman-1 -lfreetype -lexpat -lpng -lz -liconv -lbz2
CAIROLIBUSER	= -lcairo -lXrender -lfontconfig -lpixman-1 -lfreetype -lexpat -lpng -lz -liconv -lbz2
HDFLIB		= -lmfhdf -ldf -ljpeg -lz
NETCDF4LIB      =
HDFEOSLIB	=
HDFEOS5LIB	=
HDF5LIB		=
GDALLIB		=
GRIB2LIB	=
EEMDLIB		= -lgsl -lgslcblas
UDUNITSLIB	=
V5DLIB      	=
OpenCLLIB	= OpenCLlib
GRIDSPECLIB     =
XDRLIB		=
HDFDEFINES	= -DDARWIN
VDEFINE		= -DNGVERSION='"$(NGVERSION)"'  -DNCLVERSION='"$(NCLVERSION)"'
PROJECTDEFINES	= -DNeedFuncProto
GKS_BUFFER_SIZE	= 0
PERL = /bin/perl
NCLWEBROOT      = /web/ncl
PYNGLWEBROOT      = /web/pyngl

# ########################################################################
#
# After Including Project file
#
# ########################################################################

# ########################################################################
#
# Including Rules file
#
# ########################################################################

# ########################################################################
#
# After Including Rules file
#
# ########################################################################

.SUFFIXES: .f.sed .xml .shtml

.f.sed.o:
	@$(FCPP) $< $*.f
	$(F77) $(FFLAGS) -c $*.f
	@$(RM) $*.f

# ########################################################################
#
#	local y(mM)akefile is included HERE.
#
# ########################################################################

#	$Id: yMakefile,v 1.52 2007-07-18 18:49:23 haley Exp $
#
#	Makefile for libraster - libraries for dealing with
#	a variety of raster image formats.
#

# Build the Parallax video frame buffer driver only for local systems.

MYNAME		= libncarg_ras
INCLUDES	= ncarg_ras.h

LOCLIB		= $(NCARGRASLIB) $(NCARGCLIB) -ldf

HDF_OBJS =	hdf.o
HDF_SRCS =	hdf.c
HDF_DEFS = 	-DBuildRasterHDF $(HDFDEFINES)

HPPCL_OBJS =	hppcl.o
HPPCL_SRCS =	hppcl.c
HPPCL_DEFS =	-DBuildRasterHPPCL

NRIF_OBJS =	nrif.o
NRIF_SRCS =	nrif.c
NRIF_DEFS = 	-DBuildRasterNrif

SUN_OBJS =	sunraster.o
SUN_SRCS =	sunraster.c
SUN_DEFS = 	-DBuildRasterSun

XWD_OBJS =	xwd.o
XWD_SRCS =	xwd.c
XWD_DEFS = 	-DBuildRasterXWD

AVS_OBJS =	avsraster.o
AVS_SRCS =	avsraster.c
AVS_DEFS =	-DBuildRasterAVS

SGI_OBJS =	sgiraster.o
SGI_SRCS =	sgiraster.c
SGI_DEFS =	-DBuildRasterSGI

ABEKAS_OBJS =	abekas.o
ABEKAS_SRCS =	abekas.c
ABEKAS_DEFS =	-DBuildRasterAbekas

BINARY_OBJS =	binary.o
BINARY_SRCS =	binary.c
BINARY_DEFS =	-DBuildRasterBinary

YUV_OBJS =	yuv.o
YUV_SRCS =	yuv.c
YUV_DEFS =	-DBuildRasterYUV

EXTRA_DEFINES =	$(HDF_DEFS) $(HPPCL_DEFS) $(NRIF_DEFS) $(SUN_DEFS)  		$(XWD_DEFS) $(PARALLAX_DEFS) $(AVS_DEFS) $(SGI_DEFS)  		$(ABEKAS_DEFS) $(NETCDF_DEFS) $(BINARY_DEFS) $(YUV_DEFS)  		-DNGTMPDIR='"$(NGTMPDIR)"' $(ARCH_DEF) $(VENDOR_DEF)

DEVICE_OBJS =	$(HDF_OBJS) $(HPPCL_OBJS) $(NRIF_OBJS) $(SUN_OBJS)  		$(XWD_OBJS) $(PARALLAX_OBJS) $(AVS_OBJS) $(SGI_OBJS)  		$(ABEKAS_OBJS) $(NETCDF_OBJS) $(BINARY_OBJS) $(YUV_OBJS)

DEVICE_SRCS =	$(HDF_SRCS) $(HPPCL_SRCS) $(NRIF_SRCS) $(SUN_SRCS)  		$(XWD_SRCS) $(PARALLAX_SRCS) $(AVS_SRCS) $(SGI_SRCS)  		$(ABEKAS_SRCS) $(NETCDF_SRCS) $(BINARY_SRCS) $(YUV_SRCS)

GENERIC_OBJS =	raster.o cleartext.o error.o misc.o palette.o  		dither.o resample.o composite.o ntsc.o color.o  		rasterop.o generic.o memory.o $(DEVICE_OBJS)

GENERIC_SRCS =	raster.c cleartext.c error.c misc.c palette.c  		dither.c resample.c composite.c ntsc.c color.c  		rasterop.c generic.c memory.c $(DEVICE_SRCS)

OBJS		= $(GENERIC_OBJS)
SRCS		= $(GENERIC_SRCS)

all::

all-local:: $(MYNAME).a

$(MYNAME).a:: $(OBJS)
	-$(RM) $(MYNAME).a
	@$(AR_QUICK) $(MYNAME).a $(OBJS)
	$(RANLIB) $(MYNAME).a

all::

clean-local::
	-@$(RM) $(MYNAME).a $(OBJS)

all::

all-local:: $(MYNAME).a

install-local:: $(MYNAME).a
	@if (test ! -d $(LIBPATH)); then ($(MKDIRHIER) $(LIBPATH)); fi
	@for name in $(MYNAME).a; do \
	if (test -f $$name.$(EXE_SUFFIX)) then \
	(echo "Installing $$name.$(EXE_SUFFIX)";\
	$(INSTALL) -m $(INSTALL_LIB) $$name.$(EXE_SUFFIX) $(LIBPATH));\
	else \
	(echo "Installing $$name";\
	$(INSTALL) -m $(INSTALL_LIB) $$name $(LIBPATH)); fi \
	done

all::

install-local::
	$(RANLIB) $(LIBPATH)/$(MYNAME).a

all::

includes-local::
	@if (test ! -d $(BUILDINCDIR)/ncarg); then ($(MKDIRHIER) $(BUILDINCDIR)/ncarg); fi
	@cd $(BUILDINCDIR)/ncarg;\
	$(RM) $(INCLUDES); \
	for i in $(INCLUDES); do \
	(echo "$(LN) dstupdir/$(BUILDINCTOP)/$(CURRENT_DIR)/$$i .";\
	$(LN) ../$(BUILDINCTOP)/$(CURRENT_DIR)/$$i .;)\
	done

all::

all-local:: $(INCLUDES)

install-local:: $(INCLUDES)
	@if (test ! -d $(NCARGINCPATH)); then ($(MKDIRHIER) $(NCARGINCPATH)); fi
	@for name in $(INCLUDES); do \
	if (test -f $$name.$(EXE_SUFFIX)) then \
	(echo "Installing $$name.$(EXE_SUFFIX)";\
	$(INSTALL) -m $(INSTALL_INC) $$name.$(EXE_SUFFIX) $(NCARGINCPATH));\
	else \
	(echo "Installing $$name";\
	$(INSTALL) -m $(INSTALL_INC) $$name $(NCARGINCPATH)); fi \
	done

all::

depend-local:: $(SRCS)
	$(DEPEND) -s "# DO NOT DELETE" -- $(ALLDEFINES) -- $(SRCS)

hdf.o:		Makefile
raster.o:	Makefile

test: rasls.o rascopy.o
	$(CC) $(CFLAGS) -o rasls rasls.o $(LOCLIB) -lmalloc
	$(CC) $(CFLAGS) -o rascopy rascopy.o $(LOCLIB) -lmalloc

lint:
	lint -I$(CINC) $(SRCS) 1> lint.out 2>&1

# ########################################################################
#
#	after local y(mM)akefile included.
#
# ########################################################################

# ########################################################################
#
#	The Rest of this file is generated from the Template file.
#
# ########################################################################

COPT	= -O
FOPT	= -O

all:: all-local

clean-local::
	@echo "cleaning in $(CURRENT_DIR)"

all::

clean-local::
	-@$(RM) core Makefile.bak

local-Makefile:: $(YMAKEDEP)

$(YMAKEDEP):
	@(cd $(CONFIGSRC); if (test -f Makefile) then \
	echo "checking ymake in $(CONFIGSRC) first..."; $(MAKE) all; else \
	echo "making ymake from Makefile.ini in $(CONFIGSRC) first..."; \
	$(MAKE) -f Makefile.ini clean all; fi; \
	echo "continuing in $(CURRENT_DIR)")

local-Makefile::
	@echo "Making Makefile in $(CURRENT_DIR)"
	@$(YMAKE) -Topdir $(TOP) -Curdir $(CURRENT_DIR)

me: local-Makefile

all-local::

install-local::
	@echo "installing in $(CURRENT_DIR)"

depend-local::

includes-local::

preformatted-local::

install:: install-local

Makefiles::

includes:: includes-local

depend:: depend-local

clean:: clean-local

dist-clean:: clean-local
	@$(RM) Makefile
	@echo "dist-clean in $(CURRENT_DIR)"

preformatted:: preformatted-local

# DO NOT DELETE

raster.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
raster.o: /usr/include/sys/_symbol_aliasing.h
raster.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
raster.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
raster.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
raster.o: /usr/include/i386/_types.h
raster.o: /usr/include/sys/_pthread/_pthread_types.h
raster.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
raster.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
raster.o: /usr/include/sys/_types/_int16_t.h
raster.o: /usr/include/sys/_types/_int32_t.h
raster.o: /usr/include/sys/_types/_int64_t.h
raster.o: /usr/include/sys/_types/_u_int8_t.h
raster.o: /usr/include/sys/_types/_u_int16_t.h
raster.o: /usr/include/sys/_types/_u_int32_t.h
raster.o: /usr/include/sys/_types/_u_int64_t.h
raster.o: /usr/include/sys/_types/_intptr_t.h
raster.o: /usr/include/sys/_types/_uintptr_t.h
raster.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
raster.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
raster.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
raster.o: /usr/include/secure/_common.h /usr/include/string.h
raster.o: /usr/include/sys/_types/_rsize_t.h
raster.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
raster.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
raster.o: /usr/include/fcntl.h /usr/include/sys/fcntl.h
raster.o: /usr/include/sys/_types/_mode_t.h /usr/include/sys/_types/_pid_t.h
raster.o: /usr/include/sys/_types/_o_sync.h
raster.o: /usr/include/sys/_types/_o_dsync.h
raster.o: /usr/include/sys/_types/_seek_set.h
raster.o: /usr/include/sys/_types/_s_ifmt.h
raster.o: /usr/include/sys/_types/_timespec.h
raster.o: /usr/include/sys/_types/_filesec_t.h /usr/include/errno.h
raster.o: /usr/include/sys/errno.h ../../../.././include/ncarg/c.h
raster.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
raster.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
raster.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
raster.o: /usr/include/libkern/i386/_OSByteOrder.h
raster.o: /usr/include/sys/_types/_u_char.h
raster.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
raster.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
raster.o: /usr/include/sys/_types/_blkcnt_t.h
raster.o: /usr/include/sys/_types/_blksize_t.h
raster.o: /usr/include/sys/_types/_gid_t.h
raster.o: /usr/include/sys/_types/_in_addr_t.h
raster.o: /usr/include/sys/_types/_in_port_t.h
raster.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
raster.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_nlink_t.h
raster.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_uid_t.h
raster.o: /usr/include/sys/_types/_clock_t.h
raster.o: /usr/include/sys/_types/_time_t.h
raster.o: /usr/include/sys/_types/_useconds_t.h
raster.o: /usr/include/sys/_types/_suseconds_t.h
raster.o: /usr/include/sys/_types/_fd_def.h
raster.o: /usr/include/sys/_types/_fd_setsize.h
raster.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
raster.o: /usr/include/sys/_types/_fd_zero.h
raster.o: /usr/include/sys/_types/_fd_isset.h
raster.o: /usr/include/sys/_types/_fd_copy.h
raster.o: /usr/include/sys/_pthread/_pthread_attr_t.h
raster.o: /usr/include/sys/_pthread/_pthread_cond_t.h
raster.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
raster.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
raster.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
raster.o: /usr/include/sys/_pthread/_pthread_once_t.h
raster.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
raster.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
raster.o: /usr/include/sys/_pthread/_pthread_t.h
raster.o: /usr/include/sys/_pthread/_pthread_key_t.h
raster.o: /usr/include/sys/_types/_fsblkcnt_t.h
raster.o: /usr/include/sys/_types/_fsfilcnt_t.h ncarg_ras.h
raster.o: /usr/include/sys/stat.h raster.h options.h devices.h yuv.h
cleartext.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
cleartext.o: /usr/include/sys/_symbol_aliasing.h
cleartext.o: /usr/include/sys/_posix_availability.h
cleartext.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
cleartext.o: /usr/include/_types.h /usr/include/sys/_types.h
cleartext.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
cleartext.o: /usr/include/sys/_pthread/_pthread_types.h
cleartext.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
cleartext.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
cleartext.o: /usr/include/sys/_types/_int16_t.h
cleartext.o: /usr/include/sys/_types/_int32_t.h
cleartext.o: /usr/include/sys/_types/_int64_t.h
cleartext.o: /usr/include/sys/_types/_u_int8_t.h
cleartext.o: /usr/include/sys/_types/_u_int16_t.h
cleartext.o: /usr/include/sys/_types/_u_int32_t.h
cleartext.o: /usr/include/sys/_types/_u_int64_t.h
cleartext.o: /usr/include/sys/_types/_intptr_t.h
cleartext.o: /usr/include/sys/_types/_uintptr_t.h
cleartext.o: /usr/include/sys/_types/_size_t.h
cleartext.o: /usr/include/sys/_types/_null.h /usr/include/sys/stdio.h
cleartext.o: /usr/include/sys/_types/_off_t.h
cleartext.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
cleartext.o: /usr/include/secure/_common.h /usr/include/stdlib.h
cleartext.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
cleartext.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
cleartext.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
cleartext.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
cleartext.o: /usr/include/i386/_mcontext.h
cleartext.o: /usr/include/mach/machine/_structs.h
cleartext.o: /usr/include/mach/i386/_structs.h
cleartext.o: /usr/include/sys/_pthread/_pthread_attr_t.h
cleartext.o: /usr/include/sys/_types/_sigaltstack.h
cleartext.o: /usr/include/sys/_types/_ucontext.h
cleartext.o: /usr/include/sys/_types/_sigset_t.h
cleartext.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
cleartext.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
cleartext.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
cleartext.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
cleartext.o: /usr/include/_types/_uintmax_t.h
cleartext.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
cleartext.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
cleartext.o: /usr/include/libkern/_OSByteOrder.h
cleartext.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
cleartext.o: /usr/include/sys/_types/_ct_rune_t.h
cleartext.o: /usr/include/sys/_types/_rune_t.h
cleartext.o: /usr/include/sys/_types/_wchar_t.h
cleartext.o: /usr/include/sys/_types/_dev_t.h
cleartext.o: /usr/include/sys/_types/_mode_t.h /usr/include/errno.h
cleartext.o: /usr/include/sys/errno.h /usr/include/sys/_types/_errno_t.h
cleartext.o: /usr/include/fcntl.h /usr/include/sys/fcntl.h
cleartext.o: /usr/include/sys/_types/_o_sync.h
cleartext.o: /usr/include/sys/_types/_o_dsync.h
cleartext.o: /usr/include/sys/_types/_seek_set.h
cleartext.o: /usr/include/sys/_types/_s_ifmt.h
cleartext.o: /usr/include/sys/_types/_timespec.h
cleartext.o: /usr/include/sys/_types/_filesec_t.h /usr/include/string.h
cleartext.o: /usr/include/sys/_types/_rsize_t.h /usr/include/strings.h
cleartext.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
cleartext.o: ncarg_ras.h /usr/include/sys/types.h
cleartext.o: /usr/include/sys/_types/_u_char.h
cleartext.o: /usr/include/sys/_types/_u_short.h
cleartext.o: /usr/include/sys/_types/_u_int.h
cleartext.o: /usr/include/sys/_types/_caddr_t.h
cleartext.o: /usr/include/sys/_types/_blkcnt_t.h
cleartext.o: /usr/include/sys/_types/_blksize_t.h
cleartext.o: /usr/include/sys/_types/_gid_t.h
cleartext.o: /usr/include/sys/_types/_in_addr_t.h
cleartext.o: /usr/include/sys/_types/_in_port_t.h
cleartext.o: /usr/include/sys/_types/_ino_t.h
cleartext.o: /usr/include/sys/_types/_ino64_t.h
cleartext.o: /usr/include/sys/_types/_key_t.h
cleartext.o: /usr/include/sys/_types/_nlink_t.h
cleartext.o: /usr/include/sys/_types/_clock_t.h
cleartext.o: /usr/include/sys/_types/_time_t.h
cleartext.o: /usr/include/sys/_types/_useconds_t.h
cleartext.o: /usr/include/sys/_types/_suseconds_t.h
cleartext.o: /usr/include/sys/_types/_fd_def.h
cleartext.o: /usr/include/sys/_types/_fd_setsize.h
cleartext.o: /usr/include/sys/_types/_fd_set.h
cleartext.o: /usr/include/sys/_types/_fd_clr.h
cleartext.o: /usr/include/sys/_types/_fd_zero.h
cleartext.o: /usr/include/sys/_types/_fd_isset.h
cleartext.o: /usr/include/sys/_types/_fd_copy.h
cleartext.o: /usr/include/sys/_pthread/_pthread_cond_t.h
cleartext.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
cleartext.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
cleartext.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
cleartext.o: /usr/include/sys/_pthread/_pthread_once_t.h
cleartext.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
cleartext.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
cleartext.o: /usr/include/sys/_pthread/_pthread_t.h
cleartext.o: /usr/include/sys/_pthread/_pthread_key_t.h
cleartext.o: /usr/include/sys/_types/_fsblkcnt_t.h
cleartext.o: /usr/include/sys/_types/_fsfilcnt_t.h /usr/include/sys/stat.h
cleartext.o: ../../../.././include/ncarg/c.h
error.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
error.o: /usr/include/sys/_symbol_aliasing.h
error.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
error.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
error.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
error.o: /usr/include/i386/_types.h
error.o: /usr/include/sys/_pthread/_pthread_types.h
error.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
error.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
error.o: /usr/include/sys/_types/_int16_t.h
error.o: /usr/include/sys/_types/_int32_t.h
error.o: /usr/include/sys/_types/_int64_t.h
error.o: /usr/include/sys/_types/_u_int8_t.h
error.o: /usr/include/sys/_types/_u_int16_t.h
error.o: /usr/include/sys/_types/_u_int32_t.h
error.o: /usr/include/sys/_types/_u_int64_t.h
error.o: /usr/include/sys/_types/_intptr_t.h
error.o: /usr/include/sys/_types/_uintptr_t.h
error.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
error.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
error.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
error.o: /usr/include/secure/_common.h /usr/include/errno.h
error.o: /usr/include/sys/errno.h /usr/include/sys/_types/_errno_t.h
error.o: /usr/include/string.h /usr/include/sys/_types/_rsize_t.h
error.o: /usr/include/strings.h /usr/include/secure/_strings.h
error.o: /usr/include/secure/_string.h ncarg_ras.h /usr/include/sys/types.h
error.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
error.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
error.o: /usr/include/libkern/_OSByteOrder.h
error.o: /usr/include/libkern/i386/_OSByteOrder.h
error.o: /usr/include/sys/_types/_u_char.h /usr/include/sys/_types/_u_short.h
error.o: /usr/include/sys/_types/_u_int.h /usr/include/sys/_types/_caddr_t.h
error.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_blkcnt_t.h
error.o: /usr/include/sys/_types/_blksize_t.h
error.o: /usr/include/sys/_types/_gid_t.h
error.o: /usr/include/sys/_types/_in_addr_t.h
error.o: /usr/include/sys/_types/_in_port_t.h
error.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
error.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
error.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
error.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
error.o: /usr/include/sys/_types/_clock_t.h /usr/include/sys/_types/_time_t.h
error.o: /usr/include/sys/_types/_useconds_t.h
error.o: /usr/include/sys/_types/_suseconds_t.h
error.o: /usr/include/sys/_types/_fd_def.h
error.o: /usr/include/sys/_types/_fd_setsize.h
error.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
error.o: /usr/include/sys/_types/_fd_zero.h
error.o: /usr/include/sys/_types/_fd_isset.h
error.o: /usr/include/sys/_types/_fd_copy.h
error.o: /usr/include/sys/_pthread/_pthread_attr_t.h
error.o: /usr/include/sys/_pthread/_pthread_cond_t.h
error.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
error.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
error.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
error.o: /usr/include/sys/_pthread/_pthread_once_t.h
error.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
error.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
error.o: /usr/include/sys/_pthread/_pthread_t.h
error.o: /usr/include/sys/_pthread/_pthread_key_t.h
error.o: /usr/include/sys/_types/_fsblkcnt_t.h
error.o: /usr/include/sys/_types/_fsfilcnt_t.h /usr/include/sys/stat.h
error.o: /usr/include/sys/_types/_timespec.h
error.o: /usr/include/sys/_types/_s_ifmt.h
error.o: /usr/include/sys/_types/_filesec_t.h ../../../.././include/ncarg/c.h
error.o: error.h
misc.o: ../../../.././include/ncarg/c.h /usr/include/stdio.h
misc.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
misc.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
misc.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
misc.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
misc.o: /usr/include/i386/_types.h /usr/include/sys/_pthread/_pthread_types.h
misc.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
misc.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
misc.o: /usr/include/sys/_types/_int16_t.h /usr/include/sys/_types/_int32_t.h
misc.o: /usr/include/sys/_types/_int64_t.h
misc.o: /usr/include/sys/_types/_u_int8_t.h
misc.o: /usr/include/sys/_types/_u_int16_t.h
misc.o: /usr/include/sys/_types/_u_int32_t.h
misc.o: /usr/include/sys/_types/_u_int64_t.h
misc.o: /usr/include/sys/_types/_intptr_t.h
misc.o: /usr/include/sys/_types/_uintptr_t.h
misc.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
misc.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
misc.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
misc.o: /usr/include/secure/_common.h /usr/include/sys/types.h
misc.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
misc.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
misc.o: /usr/include/libkern/_OSByteOrder.h
misc.o: /usr/include/libkern/i386/_OSByteOrder.h
misc.o: /usr/include/sys/_types/_u_char.h /usr/include/sys/_types/_u_short.h
misc.o: /usr/include/sys/_types/_u_int.h /usr/include/sys/_types/_caddr_t.h
misc.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_blkcnt_t.h
misc.o: /usr/include/sys/_types/_blksize_t.h /usr/include/sys/_types/_gid_t.h
misc.o: /usr/include/sys/_types/_in_addr_t.h
misc.o: /usr/include/sys/_types/_in_port_t.h /usr/include/sys/_types/_ino_t.h
misc.o: /usr/include/sys/_types/_ino64_t.h /usr/include/sys/_types/_key_t.h
misc.o: /usr/include/sys/_types/_mode_t.h /usr/include/sys/_types/_nlink_t.h
misc.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
misc.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/_types/_clock_t.h
misc.o: /usr/include/sys/_types/_time_t.h
misc.o: /usr/include/sys/_types/_useconds_t.h
misc.o: /usr/include/sys/_types/_suseconds_t.h
misc.o: /usr/include/sys/_types/_rsize_t.h /usr/include/sys/_types/_errno_t.h
misc.o: /usr/include/sys/_types/_fd_def.h
misc.o: /usr/include/sys/_types/_fd_setsize.h
misc.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
misc.o: /usr/include/sys/_types/_fd_zero.h
misc.o: /usr/include/sys/_types/_fd_isset.h
misc.o: /usr/include/sys/_types/_fd_copy.h
misc.o: /usr/include/sys/_pthread/_pthread_attr_t.h
misc.o: /usr/include/sys/_pthread/_pthread_cond_t.h
misc.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
misc.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
misc.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
misc.o: /usr/include/sys/_pthread/_pthread_once_t.h
misc.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
misc.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
misc.o: /usr/include/sys/_pthread/_pthread_t.h
misc.o: /usr/include/sys/_pthread/_pthread_key_t.h
misc.o: /usr/include/sys/_types/_fsblkcnt_t.h
misc.o: /usr/include/sys/_types/_fsfilcnt_t.h ncarg_ras.h
misc.o: /usr/include/sys/stat.h /usr/include/sys/_types/_timespec.h
misc.o: /usr/include/sys/_types/_s_ifmt.h
misc.o: /usr/include/sys/_types/_filesec_t.h
palette.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
palette.o: /usr/include/sys/_symbol_aliasing.h
palette.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
palette.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
palette.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
palette.o: /usr/include/i386/_types.h
palette.o: /usr/include/sys/_pthread/_pthread_types.h
palette.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
palette.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
palette.o: /usr/include/sys/_types/_int16_t.h
palette.o: /usr/include/sys/_types/_int32_t.h
palette.o: /usr/include/sys/_types/_int64_t.h
palette.o: /usr/include/sys/_types/_u_int8_t.h
palette.o: /usr/include/sys/_types/_u_int16_t.h
palette.o: /usr/include/sys/_types/_u_int32_t.h
palette.o: /usr/include/sys/_types/_u_int64_t.h
palette.o: /usr/include/sys/_types/_intptr_t.h
palette.o: /usr/include/sys/_types/_uintptr_t.h
palette.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
palette.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
palette.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
palette.o: /usr/include/secure/_common.h /usr/include/errno.h
palette.o: /usr/include/sys/errno.h /usr/include/sys/_types/_errno_t.h
palette.o: /usr/include/string.h /usr/include/sys/_types/_rsize_t.h
palette.o: /usr/include/strings.h /usr/include/secure/_strings.h
palette.o: /usr/include/secure/_string.h ncarg_ras.h /usr/include/sys/types.h
palette.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
palette.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
palette.o: /usr/include/libkern/_OSByteOrder.h
palette.o: /usr/include/libkern/i386/_OSByteOrder.h
palette.o: /usr/include/sys/_types/_u_char.h
palette.o: /usr/include/sys/_types/_u_short.h
palette.o: /usr/include/sys/_types/_u_int.h
palette.o: /usr/include/sys/_types/_caddr_t.h
palette.o: /usr/include/sys/_types/_dev_t.h
palette.o: /usr/include/sys/_types/_blkcnt_t.h
palette.o: /usr/include/sys/_types/_blksize_t.h
palette.o: /usr/include/sys/_types/_gid_t.h
palette.o: /usr/include/sys/_types/_in_addr_t.h
palette.o: /usr/include/sys/_types/_in_port_t.h
palette.o: /usr/include/sys/_types/_ino_t.h
palette.o: /usr/include/sys/_types/_ino64_t.h
palette.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
palette.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
palette.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
palette.o: /usr/include/sys/_types/_clock_t.h
palette.o: /usr/include/sys/_types/_time_t.h
palette.o: /usr/include/sys/_types/_useconds_t.h
palette.o: /usr/include/sys/_types/_suseconds_t.h
palette.o: /usr/include/sys/_types/_fd_def.h
palette.o: /usr/include/sys/_types/_fd_setsize.h
palette.o: /usr/include/sys/_types/_fd_set.h
palette.o: /usr/include/sys/_types/_fd_clr.h
palette.o: /usr/include/sys/_types/_fd_zero.h
palette.o: /usr/include/sys/_types/_fd_isset.h
palette.o: /usr/include/sys/_types/_fd_copy.h
palette.o: /usr/include/sys/_pthread/_pthread_attr_t.h
palette.o: /usr/include/sys/_pthread/_pthread_cond_t.h
palette.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
palette.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
palette.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
palette.o: /usr/include/sys/_pthread/_pthread_once_t.h
palette.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
palette.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
palette.o: /usr/include/sys/_pthread/_pthread_t.h
palette.o: /usr/include/sys/_pthread/_pthread_key_t.h
palette.o: /usr/include/sys/_types/_fsblkcnt_t.h
palette.o: /usr/include/sys/_types/_fsfilcnt_t.h /usr/include/sys/stat.h
palette.o: /usr/include/sys/_types/_timespec.h
palette.o: /usr/include/sys/_types/_s_ifmt.h
palette.o: /usr/include/sys/_types/_filesec_t.h
palette.o: ../../../.././include/ncarg/c.h
dither.o: /usr/include/stdlib.h /usr/include/Availability.h
dither.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
dither.o: /usr/include/sys/_types.h /usr/include/sys/cdefs.h
dither.o: /usr/include/sys/_symbol_aliasing.h
dither.o: /usr/include/sys/_posix_availability.h
dither.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
dither.o: /usr/include/sys/_pthread/_pthread_types.h /usr/include/sys/wait.h
dither.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_id_t.h
dither.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
dither.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
dither.o: /usr/include/machine/_mcontext.h /usr/include/i386/_mcontext.h
dither.o: /usr/include/mach/machine/_structs.h
dither.o: /usr/include/mach/i386/_structs.h /usr/include/machine/types.h
dither.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
dither.o: /usr/include/sys/_types/_int16_t.h
dither.o: /usr/include/sys/_types/_int32_t.h
dither.o: /usr/include/sys/_types/_int64_t.h
dither.o: /usr/include/sys/_types/_u_int8_t.h
dither.o: /usr/include/sys/_types/_u_int16_t.h
dither.o: /usr/include/sys/_types/_u_int32_t.h
dither.o: /usr/include/sys/_types/_u_int64_t.h
dither.o: /usr/include/sys/_types/_intptr_t.h
dither.o: /usr/include/sys/_types/_uintptr_t.h
dither.o: /usr/include/sys/_pthread/_pthread_attr_t.h
dither.o: /usr/include/sys/_types/_sigaltstack.h
dither.o: /usr/include/sys/_types/_ucontext.h
dither.o: /usr/include/sys/_types/_sigset_t.h
dither.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_uid_t.h
dither.o: /usr/include/sys/resource.h /usr/include/stdint.h
dither.o: /usr/include/_types/_uint8_t.h /usr/include/_types/_uint16_t.h
dither.o: /usr/include/_types/_uint32_t.h /usr/include/_types/_uint64_t.h
dither.o: /usr/include/_types/_intmax_t.h /usr/include/_types/_uintmax_t.h
dither.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
dither.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
dither.o: /usr/include/libkern/_OSByteOrder.h
dither.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
dither.o: /usr/include/sys/_types/_ct_rune_t.h
dither.o: /usr/include/sys/_types/_rune_t.h
dither.o: /usr/include/sys/_types/_wchar_t.h /usr/include/sys/_types/_null.h
dither.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_mode_t.h
dither.o: options.h ncarg_ras.h /usr/include/stdio.h
dither.o: /usr/include/sys/_types/_va_list.h /usr/include/sys/stdio.h
dither.o: /usr/include/sys/_types/_off_t.h /usr/include/sys/_types/_ssize_t.h
dither.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
dither.o: /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
dither.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
dither.o: /usr/include/sys/_types/_caddr_t.h
dither.o: /usr/include/sys/_types/_blkcnt_t.h
dither.o: /usr/include/sys/_types/_blksize_t.h
dither.o: /usr/include/sys/_types/_gid_t.h
dither.o: /usr/include/sys/_types/_in_addr_t.h
dither.o: /usr/include/sys/_types/_in_port_t.h
dither.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
dither.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_nlink_t.h
dither.o: /usr/include/sys/_types/_clock_t.h
dither.o: /usr/include/sys/_types/_time_t.h
dither.o: /usr/include/sys/_types/_useconds_t.h
dither.o: /usr/include/sys/_types/_suseconds_t.h
dither.o: /usr/include/sys/_types/_rsize_t.h
dither.o: /usr/include/sys/_types/_errno_t.h
dither.o: /usr/include/sys/_types/_fd_def.h
dither.o: /usr/include/sys/_types/_fd_setsize.h
dither.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
dither.o: /usr/include/sys/_types/_fd_zero.h
dither.o: /usr/include/sys/_types/_fd_isset.h
dither.o: /usr/include/sys/_types/_fd_copy.h
dither.o: /usr/include/sys/_pthread/_pthread_cond_t.h
dither.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
dither.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
dither.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
dither.o: /usr/include/sys/_pthread/_pthread_once_t.h
dither.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
dither.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
dither.o: /usr/include/sys/_pthread/_pthread_t.h
dither.o: /usr/include/sys/_pthread/_pthread_key_t.h
dither.o: /usr/include/sys/_types/_fsblkcnt_t.h
dither.o: /usr/include/sys/_types/_fsfilcnt_t.h /usr/include/sys/stat.h
dither.o: /usr/include/sys/_types/_timespec.h
dither.o: /usr/include/sys/_types/_s_ifmt.h
dither.o: /usr/include/sys/_types/_filesec_t.h
dither.o: ../../../.././include/ncarg/c.h
resample.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
resample.o: /usr/include/sys/_symbol_aliasing.h
resample.o: /usr/include/sys/_posix_availability.h
resample.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
resample.o: /usr/include/_types.h /usr/include/sys/_types.h
resample.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
resample.o: /usr/include/sys/_pthread/_pthread_types.h
resample.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
resample.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
resample.o: /usr/include/sys/_types/_int16_t.h
resample.o: /usr/include/sys/_types/_int32_t.h
resample.o: /usr/include/sys/_types/_int64_t.h
resample.o: /usr/include/sys/_types/_u_int8_t.h
resample.o: /usr/include/sys/_types/_u_int16_t.h
resample.o: /usr/include/sys/_types/_u_int32_t.h
resample.o: /usr/include/sys/_types/_u_int64_t.h
resample.o: /usr/include/sys/_types/_intptr_t.h
resample.o: /usr/include/sys/_types/_uintptr_t.h
resample.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
resample.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
resample.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
resample.o: /usr/include/secure/_common.h /usr/include/stdlib.h
resample.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
resample.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
resample.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
resample.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
resample.o: /usr/include/i386/_mcontext.h
resample.o: /usr/include/mach/machine/_structs.h
resample.o: /usr/include/mach/i386/_structs.h
resample.o: /usr/include/sys/_pthread/_pthread_attr_t.h
resample.o: /usr/include/sys/_types/_sigaltstack.h
resample.o: /usr/include/sys/_types/_ucontext.h
resample.o: /usr/include/sys/_types/_sigset_t.h
resample.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
resample.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
resample.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
resample.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
resample.o: /usr/include/_types/_uintmax_t.h
resample.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
resample.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
resample.o: /usr/include/libkern/_OSByteOrder.h
resample.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
resample.o: /usr/include/sys/_types/_ct_rune_t.h
resample.o: /usr/include/sys/_types/_rune_t.h
resample.o: /usr/include/sys/_types/_wchar_t.h
resample.o: /usr/include/sys/_types/_dev_t.h
resample.o: /usr/include/sys/_types/_mode_t.h /usr/include/string.h
resample.o: /usr/include/sys/_types/_rsize_t.h
resample.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
resample.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
resample.o: /usr/include/math.h ncarg_ras.h /usr/include/sys/types.h
resample.o: /usr/include/sys/_types/_u_char.h
resample.o: /usr/include/sys/_types/_u_short.h
resample.o: /usr/include/sys/_types/_u_int.h
resample.o: /usr/include/sys/_types/_caddr_t.h
resample.o: /usr/include/sys/_types/_blkcnt_t.h
resample.o: /usr/include/sys/_types/_blksize_t.h
resample.o: /usr/include/sys/_types/_gid_t.h
resample.o: /usr/include/sys/_types/_in_addr_t.h
resample.o: /usr/include/sys/_types/_in_port_t.h
resample.o: /usr/include/sys/_types/_ino_t.h
resample.o: /usr/include/sys/_types/_ino64_t.h
resample.o: /usr/include/sys/_types/_key_t.h
resample.o: /usr/include/sys/_types/_nlink_t.h
resample.o: /usr/include/sys/_types/_clock_t.h
resample.o: /usr/include/sys/_types/_time_t.h
resample.o: /usr/include/sys/_types/_useconds_t.h
resample.o: /usr/include/sys/_types/_suseconds_t.h
resample.o: /usr/include/sys/_types/_fd_def.h
resample.o: /usr/include/sys/_types/_fd_setsize.h
resample.o: /usr/include/sys/_types/_fd_set.h
resample.o: /usr/include/sys/_types/_fd_clr.h
resample.o: /usr/include/sys/_types/_fd_zero.h
resample.o: /usr/include/sys/_types/_fd_isset.h
resample.o: /usr/include/sys/_types/_fd_copy.h
resample.o: /usr/include/sys/_pthread/_pthread_cond_t.h
resample.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
resample.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
resample.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
resample.o: /usr/include/sys/_pthread/_pthread_once_t.h
resample.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
resample.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
resample.o: /usr/include/sys/_pthread/_pthread_t.h
resample.o: /usr/include/sys/_pthread/_pthread_key_t.h
resample.o: /usr/include/sys/_types/_fsblkcnt_t.h
resample.o: /usr/include/sys/_types/_fsfilcnt_t.h /usr/include/sys/stat.h
resample.o: /usr/include/sys/_types/_timespec.h
resample.o: /usr/include/sys/_types/_s_ifmt.h
resample.o: /usr/include/sys/_types/_filesec_t.h
resample.o: ../../../.././include/ncarg/c.h
composite.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
composite.o: /usr/include/sys/_symbol_aliasing.h
composite.o: /usr/include/sys/_posix_availability.h
composite.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
composite.o: /usr/include/_types.h /usr/include/sys/_types.h
composite.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
composite.o: /usr/include/sys/_pthread/_pthread_types.h
composite.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
composite.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
composite.o: /usr/include/sys/_types/_int16_t.h
composite.o: /usr/include/sys/_types/_int32_t.h
composite.o: /usr/include/sys/_types/_int64_t.h
composite.o: /usr/include/sys/_types/_u_int8_t.h
composite.o: /usr/include/sys/_types/_u_int16_t.h
composite.o: /usr/include/sys/_types/_u_int32_t.h
composite.o: /usr/include/sys/_types/_u_int64_t.h
composite.o: /usr/include/sys/_types/_intptr_t.h
composite.o: /usr/include/sys/_types/_uintptr_t.h
composite.o: /usr/include/sys/_types/_size_t.h
composite.o: /usr/include/sys/_types/_null.h /usr/include/sys/stdio.h
composite.o: /usr/include/sys/_types/_off_t.h
composite.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
composite.o: /usr/include/secure/_common.h ncarg_ras.h
composite.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
composite.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
composite.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
composite.o: /usr/include/libkern/i386/_OSByteOrder.h
composite.o: /usr/include/sys/_types/_u_char.h
composite.o: /usr/include/sys/_types/_u_short.h
composite.o: /usr/include/sys/_types/_u_int.h
composite.o: /usr/include/sys/_types/_caddr_t.h
composite.o: /usr/include/sys/_types/_dev_t.h
composite.o: /usr/include/sys/_types/_blkcnt_t.h
composite.o: /usr/include/sys/_types/_blksize_t.h
composite.o: /usr/include/sys/_types/_gid_t.h
composite.o: /usr/include/sys/_types/_in_addr_t.h
composite.o: /usr/include/sys/_types/_in_port_t.h
composite.o: /usr/include/sys/_types/_ino_t.h
composite.o: /usr/include/sys/_types/_ino64_t.h
composite.o: /usr/include/sys/_types/_key_t.h
composite.o: /usr/include/sys/_types/_mode_t.h
composite.o: /usr/include/sys/_types/_nlink_t.h
composite.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
composite.o: /usr/include/sys/_types/_uid_t.h
composite.o: /usr/include/sys/_types/_clock_t.h
composite.o: /usr/include/sys/_types/_time_t.h
composite.o: /usr/include/sys/_types/_useconds_t.h
composite.o: /usr/include/sys/_types/_suseconds_t.h
composite.o: /usr/include/sys/_types/_rsize_t.h
composite.o: /usr/include/sys/_types/_errno_t.h
composite.o: /usr/include/sys/_types/_fd_def.h
composite.o: /usr/include/sys/_types/_fd_setsize.h
composite.o: /usr/include/sys/_types/_fd_set.h
composite.o: /usr/include/sys/_types/_fd_clr.h
composite.o: /usr/include/sys/_types/_fd_zero.h
composite.o: /usr/include/sys/_types/_fd_isset.h
composite.o: /usr/include/sys/_types/_fd_copy.h
composite.o: /usr/include/sys/_pthread/_pthread_attr_t.h
composite.o: /usr/include/sys/_pthread/_pthread_cond_t.h
composite.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
composite.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
composite.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
composite.o: /usr/include/sys/_pthread/_pthread_once_t.h
composite.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
composite.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
composite.o: /usr/include/sys/_pthread/_pthread_t.h
composite.o: /usr/include/sys/_pthread/_pthread_key_t.h
composite.o: /usr/include/sys/_types/_fsblkcnt_t.h
composite.o: /usr/include/sys/_types/_fsfilcnt_t.h /usr/include/sys/stat.h
composite.o: /usr/include/sys/_types/_timespec.h
composite.o: /usr/include/sys/_types/_s_ifmt.h
composite.o: /usr/include/sys/_types/_filesec_t.h
composite.o: ../../../.././include/ncarg/c.h
ntsc.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
ntsc.o: /usr/include/sys/_symbol_aliasing.h
ntsc.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
ntsc.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
ntsc.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
ntsc.o: /usr/include/i386/_types.h /usr/include/sys/_pthread/_pthread_types.h
ntsc.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
ntsc.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
ntsc.o: /usr/include/sys/_types/_int16_t.h /usr/include/sys/_types/_int32_t.h
ntsc.o: /usr/include/sys/_types/_int64_t.h
ntsc.o: /usr/include/sys/_types/_u_int8_t.h
ntsc.o: /usr/include/sys/_types/_u_int16_t.h
ntsc.o: /usr/include/sys/_types/_u_int32_t.h
ntsc.o: /usr/include/sys/_types/_u_int64_t.h
ntsc.o: /usr/include/sys/_types/_intptr_t.h
ntsc.o: /usr/include/sys/_types/_uintptr_t.h
ntsc.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
ntsc.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
ntsc.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
ntsc.o: /usr/include/secure/_common.h /usr/include/math.h ncarg_ras.h
ntsc.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
ntsc.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
ntsc.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
ntsc.o: /usr/include/libkern/i386/_OSByteOrder.h
ntsc.o: /usr/include/sys/_types/_u_char.h /usr/include/sys/_types/_u_short.h
ntsc.o: /usr/include/sys/_types/_u_int.h /usr/include/sys/_types/_caddr_t.h
ntsc.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_blkcnt_t.h
ntsc.o: /usr/include/sys/_types/_blksize_t.h /usr/include/sys/_types/_gid_t.h
ntsc.o: /usr/include/sys/_types/_in_addr_t.h
ntsc.o: /usr/include/sys/_types/_in_port_t.h /usr/include/sys/_types/_ino_t.h
ntsc.o: /usr/include/sys/_types/_ino64_t.h /usr/include/sys/_types/_key_t.h
ntsc.o: /usr/include/sys/_types/_mode_t.h /usr/include/sys/_types/_nlink_t.h
ntsc.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
ntsc.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/_types/_clock_t.h
ntsc.o: /usr/include/sys/_types/_time_t.h
ntsc.o: /usr/include/sys/_types/_useconds_t.h
ntsc.o: /usr/include/sys/_types/_suseconds_t.h
ntsc.o: /usr/include/sys/_types/_rsize_t.h /usr/include/sys/_types/_errno_t.h
ntsc.o: /usr/include/sys/_types/_fd_def.h
ntsc.o: /usr/include/sys/_types/_fd_setsize.h
ntsc.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
ntsc.o: /usr/include/sys/_types/_fd_zero.h
ntsc.o: /usr/include/sys/_types/_fd_isset.h
ntsc.o: /usr/include/sys/_types/_fd_copy.h
ntsc.o: /usr/include/sys/_pthread/_pthread_attr_t.h
ntsc.o: /usr/include/sys/_pthread/_pthread_cond_t.h
ntsc.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
ntsc.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
ntsc.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
ntsc.o: /usr/include/sys/_pthread/_pthread_once_t.h
ntsc.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
ntsc.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
ntsc.o: /usr/include/sys/_pthread/_pthread_t.h
ntsc.o: /usr/include/sys/_pthread/_pthread_key_t.h
ntsc.o: /usr/include/sys/_types/_fsblkcnt_t.h
ntsc.o: /usr/include/sys/_types/_fsfilcnt_t.h /usr/include/sys/stat.h
ntsc.o: /usr/include/sys/_types/_timespec.h /usr/include/sys/_types/_s_ifmt.h
ntsc.o: /usr/include/sys/_types/_filesec_t.h ../../../.././include/ncarg/c.h
ntsc.o: ntsc.h
color.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
color.o: /usr/include/sys/_symbol_aliasing.h
color.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
color.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
color.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
color.o: /usr/include/i386/_types.h
color.o: /usr/include/sys/_pthread/_pthread_types.h
color.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
color.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
color.o: /usr/include/sys/_types/_int16_t.h
color.o: /usr/include/sys/_types/_int32_t.h
color.o: /usr/include/sys/_types/_int64_t.h
color.o: /usr/include/sys/_types/_u_int8_t.h
color.o: /usr/include/sys/_types/_u_int16_t.h
color.o: /usr/include/sys/_types/_u_int32_t.h
color.o: /usr/include/sys/_types/_u_int64_t.h
color.o: /usr/include/sys/_types/_intptr_t.h
color.o: /usr/include/sys/_types/_uintptr_t.h
color.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
color.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
color.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
color.o: /usr/include/secure/_common.h /usr/include/string.h
color.o: /usr/include/sys/_types/_rsize_t.h
color.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
color.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
color.o: /usr/include/math.h ncarg_ras.h /usr/include/sys/types.h
color.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
color.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
color.o: /usr/include/libkern/_OSByteOrder.h
color.o: /usr/include/libkern/i386/_OSByteOrder.h
color.o: /usr/include/sys/_types/_u_char.h /usr/include/sys/_types/_u_short.h
color.o: /usr/include/sys/_types/_u_int.h /usr/include/sys/_types/_caddr_t.h
color.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_blkcnt_t.h
color.o: /usr/include/sys/_types/_blksize_t.h
color.o: /usr/include/sys/_types/_gid_t.h
color.o: /usr/include/sys/_types/_in_addr_t.h
color.o: /usr/include/sys/_types/_in_port_t.h
color.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
color.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
color.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
color.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
color.o: /usr/include/sys/_types/_clock_t.h /usr/include/sys/_types/_time_t.h
color.o: /usr/include/sys/_types/_useconds_t.h
color.o: /usr/include/sys/_types/_suseconds_t.h
color.o: /usr/include/sys/_types/_fd_def.h
color.o: /usr/include/sys/_types/_fd_setsize.h
color.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
color.o: /usr/include/sys/_types/_fd_zero.h
color.o: /usr/include/sys/_types/_fd_isset.h
color.o: /usr/include/sys/_types/_fd_copy.h
color.o: /usr/include/sys/_pthread/_pthread_attr_t.h
color.o: /usr/include/sys/_pthread/_pthread_cond_t.h
color.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
color.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
color.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
color.o: /usr/include/sys/_pthread/_pthread_once_t.h
color.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
color.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
color.o: /usr/include/sys/_pthread/_pthread_t.h
color.o: /usr/include/sys/_pthread/_pthread_key_t.h
color.o: /usr/include/sys/_types/_fsblkcnt_t.h
color.o: /usr/include/sys/_types/_fsfilcnt_t.h /usr/include/sys/stat.h
color.o: /usr/include/sys/_types/_timespec.h
color.o: /usr/include/sys/_types/_s_ifmt.h
color.o: /usr/include/sys/_types/_filesec_t.h ../../../.././include/ncarg/c.h
rasterop.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
rasterop.o: /usr/include/sys/_symbol_aliasing.h
rasterop.o: /usr/include/sys/_posix_availability.h
rasterop.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
rasterop.o: /usr/include/_types.h /usr/include/sys/_types.h
rasterop.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
rasterop.o: /usr/include/sys/_pthread/_pthread_types.h
rasterop.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
rasterop.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
rasterop.o: /usr/include/sys/_types/_int16_t.h
rasterop.o: /usr/include/sys/_types/_int32_t.h
rasterop.o: /usr/include/sys/_types/_int64_t.h
rasterop.o: /usr/include/sys/_types/_u_int8_t.h
rasterop.o: /usr/include/sys/_types/_u_int16_t.h
rasterop.o: /usr/include/sys/_types/_u_int32_t.h
rasterop.o: /usr/include/sys/_types/_u_int64_t.h
rasterop.o: /usr/include/sys/_types/_intptr_t.h
rasterop.o: /usr/include/sys/_types/_uintptr_t.h
rasterop.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
rasterop.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
rasterop.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
rasterop.o: /usr/include/secure/_common.h /usr/include/stdlib.h
rasterop.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
rasterop.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
rasterop.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
rasterop.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
rasterop.o: /usr/include/i386/_mcontext.h
rasterop.o: /usr/include/mach/machine/_structs.h
rasterop.o: /usr/include/mach/i386/_structs.h
rasterop.o: /usr/include/sys/_pthread/_pthread_attr_t.h
rasterop.o: /usr/include/sys/_types/_sigaltstack.h
rasterop.o: /usr/include/sys/_types/_ucontext.h
rasterop.o: /usr/include/sys/_types/_sigset_t.h
rasterop.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
rasterop.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
rasterop.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
rasterop.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
rasterop.o: /usr/include/_types/_uintmax_t.h
rasterop.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
rasterop.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
rasterop.o: /usr/include/libkern/_OSByteOrder.h
rasterop.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
rasterop.o: /usr/include/sys/_types/_ct_rune_t.h
rasterop.o: /usr/include/sys/_types/_rune_t.h
rasterop.o: /usr/include/sys/_types/_wchar_t.h
rasterop.o: /usr/include/sys/_types/_dev_t.h
rasterop.o: /usr/include/sys/_types/_mode_t.h /usr/include/memory.h
rasterop.o: /usr/include/string.h /usr/include/sys/_types/_rsize_t.h
rasterop.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
rasterop.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
rasterop.o: /usr/include/errno.h /usr/include/sys/errno.h ncarg_ras.h
rasterop.o: /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
rasterop.o: /usr/include/sys/_types/_u_short.h
rasterop.o: /usr/include/sys/_types/_u_int.h
rasterop.o: /usr/include/sys/_types/_caddr_t.h
rasterop.o: /usr/include/sys/_types/_blkcnt_t.h
rasterop.o: /usr/include/sys/_types/_blksize_t.h
rasterop.o: /usr/include/sys/_types/_gid_t.h
rasterop.o: /usr/include/sys/_types/_in_addr_t.h
rasterop.o: /usr/include/sys/_types/_in_port_t.h
rasterop.o: /usr/include/sys/_types/_ino_t.h
rasterop.o: /usr/include/sys/_types/_ino64_t.h
rasterop.o: /usr/include/sys/_types/_key_t.h
rasterop.o: /usr/include/sys/_types/_nlink_t.h
rasterop.o: /usr/include/sys/_types/_clock_t.h
rasterop.o: /usr/include/sys/_types/_time_t.h
rasterop.o: /usr/include/sys/_types/_useconds_t.h
rasterop.o: /usr/include/sys/_types/_suseconds_t.h
rasterop.o: /usr/include/sys/_types/_fd_def.h
rasterop.o: /usr/include/sys/_types/_fd_setsize.h
rasterop.o: /usr/include/sys/_types/_fd_set.h
rasterop.o: /usr/include/sys/_types/_fd_clr.h
rasterop.o: /usr/include/sys/_types/_fd_zero.h
rasterop.o: /usr/include/sys/_types/_fd_isset.h
rasterop.o: /usr/include/sys/_types/_fd_copy.h
rasterop.o: /usr/include/sys/_pthread/_pthread_cond_t.h
rasterop.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
rasterop.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
rasterop.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
rasterop.o: /usr/include/sys/_pthread/_pthread_once_t.h
rasterop.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
rasterop.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
rasterop.o: /usr/include/sys/_pthread/_pthread_t.h
rasterop.o: /usr/include/sys/_pthread/_pthread_key_t.h
rasterop.o: /usr/include/sys/_types/_fsblkcnt_t.h
rasterop.o: /usr/include/sys/_types/_fsfilcnt_t.h /usr/include/sys/stat.h
rasterop.o: /usr/include/sys/_types/_timespec.h
rasterop.o: /usr/include/sys/_types/_s_ifmt.h
rasterop.o: /usr/include/sys/_types/_filesec_t.h
rasterop.o: ../../../.././include/ncarg/c.h
generic.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
generic.o: /usr/include/sys/_symbol_aliasing.h
generic.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
generic.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
generic.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
generic.o: /usr/include/i386/_types.h
generic.o: /usr/include/sys/_pthread/_pthread_types.h
generic.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
generic.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
generic.o: /usr/include/sys/_types/_int16_t.h
generic.o: /usr/include/sys/_types/_int32_t.h
generic.o: /usr/include/sys/_types/_int64_t.h
generic.o: /usr/include/sys/_types/_u_int8_t.h
generic.o: /usr/include/sys/_types/_u_int16_t.h
generic.o: /usr/include/sys/_types/_u_int32_t.h
generic.o: /usr/include/sys/_types/_u_int64_t.h
generic.o: /usr/include/sys/_types/_intptr_t.h
generic.o: /usr/include/sys/_types/_uintptr_t.h
generic.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
generic.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
generic.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
generic.o: /usr/include/secure/_common.h /usr/include/stdlib.h
generic.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
generic.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
generic.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
generic.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
generic.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
generic.o: /usr/include/mach/i386/_structs.h
generic.o: /usr/include/sys/_pthread/_pthread_attr_t.h
generic.o: /usr/include/sys/_types/_sigaltstack.h
generic.o: /usr/include/sys/_types/_ucontext.h
generic.o: /usr/include/sys/_types/_sigset_t.h
generic.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
generic.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
generic.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
generic.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
generic.o: /usr/include/_types/_uintmax_t.h
generic.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
generic.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
generic.o: /usr/include/libkern/_OSByteOrder.h
generic.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
generic.o: /usr/include/sys/_types/_ct_rune_t.h
generic.o: /usr/include/sys/_types/_rune_t.h
generic.o: /usr/include/sys/_types/_wchar_t.h
generic.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_mode_t.h
generic.o: /usr/include/errno.h /usr/include/sys/errno.h
generic.o: /usr/include/sys/_types/_errno_t.h ../../../.././include/ncarg/c.h
generic.o: /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
generic.o: /usr/include/sys/_types/_u_short.h
generic.o: /usr/include/sys/_types/_u_int.h
generic.o: /usr/include/sys/_types/_caddr_t.h
generic.o: /usr/include/sys/_types/_blkcnt_t.h
generic.o: /usr/include/sys/_types/_blksize_t.h
generic.o: /usr/include/sys/_types/_gid_t.h
generic.o: /usr/include/sys/_types/_in_addr_t.h
generic.o: /usr/include/sys/_types/_in_port_t.h
generic.o: /usr/include/sys/_types/_ino_t.h
generic.o: /usr/include/sys/_types/_ino64_t.h
generic.o: /usr/include/sys/_types/_key_t.h
generic.o: /usr/include/sys/_types/_nlink_t.h
generic.o: /usr/include/sys/_types/_clock_t.h
generic.o: /usr/include/sys/_types/_time_t.h
generic.o: /usr/include/sys/_types/_useconds_t.h
generic.o: /usr/include/sys/_types/_suseconds_t.h
generic.o: /usr/include/sys/_types/_rsize_t.h
generic.o: /usr/include/sys/_types/_fd_def.h
generic.o: /usr/include/sys/_types/_fd_setsize.h
generic.o: /usr/include/sys/_types/_fd_set.h
generic.o: /usr/include/sys/_types/_fd_clr.h
generic.o: /usr/include/sys/_types/_fd_zero.h
generic.o: /usr/include/sys/_types/_fd_isset.h
generic.o: /usr/include/sys/_types/_fd_copy.h
generic.o: /usr/include/sys/_pthread/_pthread_cond_t.h
generic.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
generic.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
generic.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
generic.o: /usr/include/sys/_pthread/_pthread_once_t.h
generic.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
generic.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
generic.o: /usr/include/sys/_pthread/_pthread_t.h
generic.o: /usr/include/sys/_pthread/_pthread_key_t.h
generic.o: /usr/include/sys/_types/_fsblkcnt_t.h
generic.o: /usr/include/sys/_types/_fsfilcnt_t.h ncarg_ras.h
generic.o: /usr/include/sys/stat.h /usr/include/sys/_types/_timespec.h
generic.o: /usr/include/sys/_types/_s_ifmt.h
generic.o: /usr/include/sys/_types/_filesec_t.h
memory.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
memory.o: /usr/include/sys/_symbol_aliasing.h
memory.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
memory.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
memory.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
memory.o: /usr/include/i386/_types.h
memory.o: /usr/include/sys/_pthread/_pthread_types.h
memory.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
memory.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
memory.o: /usr/include/sys/_types/_int16_t.h
memory.o: /usr/include/sys/_types/_int32_t.h
memory.o: /usr/include/sys/_types/_int64_t.h
memory.o: /usr/include/sys/_types/_u_int8_t.h
memory.o: /usr/include/sys/_types/_u_int16_t.h
memory.o: /usr/include/sys/_types/_u_int32_t.h
memory.o: /usr/include/sys/_types/_u_int64_t.h
memory.o: /usr/include/sys/_types/_intptr_t.h
memory.o: /usr/include/sys/_types/_uintptr_t.h
memory.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
memory.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
memory.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
memory.o: /usr/include/secure/_common.h /usr/include/stdlib.h
memory.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
memory.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
memory.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
memory.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
memory.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
memory.o: /usr/include/mach/i386/_structs.h
memory.o: /usr/include/sys/_pthread/_pthread_attr_t.h
memory.o: /usr/include/sys/_types/_sigaltstack.h
memory.o: /usr/include/sys/_types/_ucontext.h
memory.o: /usr/include/sys/_types/_sigset_t.h
memory.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
memory.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
memory.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
memory.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
memory.o: /usr/include/_types/_uintmax_t.h /usr/include/sys/_types/_timeval.h
memory.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
memory.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
memory.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
memory.o: /usr/include/sys/_types/_ct_rune_t.h
memory.o: /usr/include/sys/_types/_rune_t.h
memory.o: /usr/include/sys/_types/_wchar_t.h /usr/include/sys/_types/_dev_t.h
memory.o: /usr/include/sys/_types/_mode_t.h ncarg_ras.h
memory.o: /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
memory.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
memory.o: /usr/include/sys/_types/_caddr_t.h
memory.o: /usr/include/sys/_types/_blkcnt_t.h
memory.o: /usr/include/sys/_types/_blksize_t.h
memory.o: /usr/include/sys/_types/_gid_t.h
memory.o: /usr/include/sys/_types/_in_addr_t.h
memory.o: /usr/include/sys/_types/_in_port_t.h
memory.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
memory.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_nlink_t.h
memory.o: /usr/include/sys/_types/_clock_t.h
memory.o: /usr/include/sys/_types/_time_t.h
memory.o: /usr/include/sys/_types/_useconds_t.h
memory.o: /usr/include/sys/_types/_suseconds_t.h
memory.o: /usr/include/sys/_types/_rsize_t.h
memory.o: /usr/include/sys/_types/_errno_t.h
memory.o: /usr/include/sys/_types/_fd_def.h
memory.o: /usr/include/sys/_types/_fd_setsize.h
memory.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
memory.o: /usr/include/sys/_types/_fd_zero.h
memory.o: /usr/include/sys/_types/_fd_isset.h
memory.o: /usr/include/sys/_types/_fd_copy.h
memory.o: /usr/include/sys/_pthread/_pthread_cond_t.h
memory.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
memory.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
memory.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
memory.o: /usr/include/sys/_pthread/_pthread_once_t.h
memory.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
memory.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
memory.o: /usr/include/sys/_pthread/_pthread_t.h
memory.o: /usr/include/sys/_pthread/_pthread_key_t.h
memory.o: /usr/include/sys/_types/_fsblkcnt_t.h
memory.o: /usr/include/sys/_types/_fsfilcnt_t.h /usr/include/sys/stat.h
memory.o: /usr/include/sys/_types/_timespec.h
memory.o: /usr/include/sys/_types/_s_ifmt.h
memory.o: /usr/include/sys/_types/_filesec_t.h
memory.o: ../../../.././include/ncarg/c.h
hdf.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
hdf.o: /usr/include/sys/_symbol_aliasing.h
hdf.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
hdf.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
hdf.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
hdf.o: /usr/include/i386/_types.h /usr/include/sys/_pthread/_pthread_types.h
hdf.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
hdf.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
hdf.o: /usr/include/sys/_types/_int16_t.h /usr/include/sys/_types/_int32_t.h
hdf.o: /usr/include/sys/_types/_int64_t.h /usr/include/sys/_types/_u_int8_t.h
hdf.o: /usr/include/sys/_types/_u_int16_t.h
hdf.o: /usr/include/sys/_types/_u_int32_t.h
hdf.o: /usr/include/sys/_types/_u_int64_t.h
hdf.o: /usr/include/sys/_types/_intptr_t.h
hdf.o: /usr/include/sys/_types/_uintptr_t.h /usr/include/sys/_types/_size_t.h
hdf.o: /usr/include/sys/_types/_null.h /usr/include/sys/stdio.h
hdf.o: /usr/include/sys/_types/_off_t.h /usr/include/sys/_types/_ssize_t.h
hdf.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
hdf.o: /usr/include/errno.h /usr/include/sys/errno.h
hdf.o: /usr/include/sys/_types/_errno_t.h /usr/include/stdlib.h
hdf.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
hdf.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
hdf.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
hdf.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
hdf.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
hdf.o: /usr/include/mach/i386/_structs.h
hdf.o: /usr/include/sys/_pthread/_pthread_attr_t.h
hdf.o: /usr/include/sys/_types/_sigaltstack.h
hdf.o: /usr/include/sys/_types/_ucontext.h
hdf.o: /usr/include/sys/_types/_sigset_t.h /usr/include/sys/_types/_uid_t.h
hdf.o: /usr/include/sys/resource.h /usr/include/stdint.h
hdf.o: /usr/include/_types/_uint8_t.h /usr/include/_types/_uint16_t.h
hdf.o: /usr/include/_types/_uint32_t.h /usr/include/_types/_uint64_t.h
hdf.o: /usr/include/_types/_intmax_t.h /usr/include/_types/_uintmax_t.h
hdf.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
hdf.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
hdf.o: /usr/include/libkern/_OSByteOrder.h
hdf.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
hdf.o: /usr/include/sys/_types/_ct_rune_t.h /usr/include/sys/_types/_rune_t.h
hdf.o: /usr/include/sys/_types/_wchar_t.h /usr/include/sys/_types/_dev_t.h
hdf.o: /usr/include/sys/_types/_mode_t.h /usr/include/fcntl.h
hdf.o: /usr/include/sys/fcntl.h /usr/include/sys/_types/_o_sync.h
hdf.o: /usr/include/sys/_types/_o_dsync.h /usr/include/sys/_types/_seek_set.h
hdf.o: /usr/include/sys/_types/_s_ifmt.h /usr/include/sys/_types/_timespec.h
hdf.o: /usr/include/sys/_types/_filesec_t.h /usr/include/string.h
hdf.o: /usr/include/sys/_types/_rsize_t.h /usr/include/strings.h
hdf.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
hdf.o: ../../../.././include/ncarg/c.h /usr/include/sys/types.h
hdf.o: /usr/include/sys/_types/_u_char.h /usr/include/sys/_types/_u_short.h
hdf.o: /usr/include/sys/_types/_u_int.h /usr/include/sys/_types/_caddr_t.h
hdf.o: /usr/include/sys/_types/_blkcnt_t.h
hdf.o: /usr/include/sys/_types/_blksize_t.h /usr/include/sys/_types/_gid_t.h
hdf.o: /usr/include/sys/_types/_in_addr_t.h
hdf.o: /usr/include/sys/_types/_in_port_t.h /usr/include/sys/_types/_ino_t.h
hdf.o: /usr/include/sys/_types/_ino64_t.h /usr/include/sys/_types/_key_t.h
hdf.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_clock_t.h
hdf.o: /usr/include/sys/_types/_time_t.h
hdf.o: /usr/include/sys/_types/_useconds_t.h
hdf.o: /usr/include/sys/_types/_suseconds_t.h
hdf.o: /usr/include/sys/_types/_fd_def.h
hdf.o: /usr/include/sys/_types/_fd_setsize.h
hdf.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
hdf.o: /usr/include/sys/_types/_fd_zero.h /usr/include/sys/_types/_fd_isset.h
hdf.o: /usr/include/sys/_types/_fd_copy.h
hdf.o: /usr/include/sys/_pthread/_pthread_cond_t.h
hdf.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
hdf.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
hdf.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
hdf.o: /usr/include/sys/_pthread/_pthread_once_t.h
hdf.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
hdf.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
hdf.o: /usr/include/sys/_pthread/_pthread_t.h
hdf.o: /usr/include/sys/_pthread/_pthread_key_t.h
hdf.o: /usr/include/sys/_types/_fsblkcnt_t.h
hdf.o: /usr/include/sys/_types/_fsfilcnt_t.h ncarg_ras.h
hdf.o: /usr/include/sys/stat.h hdfP.h options.h
hppcl.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
hppcl.o: /usr/include/sys/_symbol_aliasing.h
hppcl.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
hppcl.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
hppcl.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
hppcl.o: /usr/include/i386/_types.h
hppcl.o: /usr/include/sys/_pthread/_pthread_types.h
hppcl.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
hppcl.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
hppcl.o: /usr/include/sys/_types/_int16_t.h
hppcl.o: /usr/include/sys/_types/_int32_t.h
hppcl.o: /usr/include/sys/_types/_int64_t.h
hppcl.o: /usr/include/sys/_types/_u_int8_t.h
hppcl.o: /usr/include/sys/_types/_u_int16_t.h
hppcl.o: /usr/include/sys/_types/_u_int32_t.h
hppcl.o: /usr/include/sys/_types/_u_int64_t.h
hppcl.o: /usr/include/sys/_types/_intptr_t.h
hppcl.o: /usr/include/sys/_types/_uintptr_t.h
hppcl.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
hppcl.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
hppcl.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
hppcl.o: /usr/include/secure/_common.h /usr/include/stdlib.h
hppcl.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
hppcl.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
hppcl.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
hppcl.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
hppcl.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
hppcl.o: /usr/include/mach/i386/_structs.h
hppcl.o: /usr/include/sys/_pthread/_pthread_attr_t.h
hppcl.o: /usr/include/sys/_types/_sigaltstack.h
hppcl.o: /usr/include/sys/_types/_ucontext.h
hppcl.o: /usr/include/sys/_types/_sigset_t.h /usr/include/sys/_types/_uid_t.h
hppcl.o: /usr/include/sys/resource.h /usr/include/stdint.h
hppcl.o: /usr/include/_types/_uint8_t.h /usr/include/_types/_uint16_t.h
hppcl.o: /usr/include/_types/_uint32_t.h /usr/include/_types/_uint64_t.h
hppcl.o: /usr/include/_types/_intmax_t.h /usr/include/_types/_uintmax_t.h
hppcl.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
hppcl.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
hppcl.o: /usr/include/libkern/_OSByteOrder.h
hppcl.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
hppcl.o: /usr/include/sys/_types/_ct_rune_t.h
hppcl.o: /usr/include/sys/_types/_rune_t.h /usr/include/sys/_types/_wchar_t.h
hppcl.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_mode_t.h
hppcl.o: /usr/include/string.h /usr/include/sys/_types/_rsize_t.h
hppcl.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
hppcl.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
hppcl.o: /usr/include/errno.h /usr/include/sys/errno.h /usr/include/fcntl.h
hppcl.o: /usr/include/sys/fcntl.h /usr/include/sys/_types/_o_sync.h
hppcl.o: /usr/include/sys/_types/_o_dsync.h
hppcl.o: /usr/include/sys/_types/_seek_set.h
hppcl.o: /usr/include/sys/_types/_s_ifmt.h
hppcl.o: /usr/include/sys/_types/_timespec.h
hppcl.o: /usr/include/sys/_types/_filesec_t.h /usr/include/sys/types.h
hppcl.o: /usr/include/sys/_types/_u_char.h /usr/include/sys/_types/_u_short.h
hppcl.o: /usr/include/sys/_types/_u_int.h /usr/include/sys/_types/_caddr_t.h
hppcl.o: /usr/include/sys/_types/_blkcnt_t.h
hppcl.o: /usr/include/sys/_types/_blksize_t.h
hppcl.o: /usr/include/sys/_types/_gid_t.h
hppcl.o: /usr/include/sys/_types/_in_addr_t.h
hppcl.o: /usr/include/sys/_types/_in_port_t.h
hppcl.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
hppcl.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_nlink_t.h
hppcl.o: /usr/include/sys/_types/_clock_t.h /usr/include/sys/_types/_time_t.h
hppcl.o: /usr/include/sys/_types/_useconds_t.h
hppcl.o: /usr/include/sys/_types/_suseconds_t.h
hppcl.o: /usr/include/sys/_types/_fd_def.h
hppcl.o: /usr/include/sys/_types/_fd_setsize.h
hppcl.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
hppcl.o: /usr/include/sys/_types/_fd_zero.h
hppcl.o: /usr/include/sys/_types/_fd_isset.h
hppcl.o: /usr/include/sys/_types/_fd_copy.h
hppcl.o: /usr/include/sys/_pthread/_pthread_cond_t.h
hppcl.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
hppcl.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
hppcl.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
hppcl.o: /usr/include/sys/_pthread/_pthread_once_t.h
hppcl.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
hppcl.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
hppcl.o: /usr/include/sys/_pthread/_pthread_t.h
hppcl.o: /usr/include/sys/_pthread/_pthread_key_t.h
hppcl.o: /usr/include/sys/_types/_fsblkcnt_t.h
hppcl.o: /usr/include/sys/_types/_fsfilcnt_t.h ncarg_ras.h
hppcl.o: /usr/include/sys/stat.h ../../../.././include/ncarg/c.h options.h
hppcl.o: hppcl.h
nrif.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
nrif.o: /usr/include/sys/_symbol_aliasing.h
nrif.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
nrif.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
nrif.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
nrif.o: /usr/include/i386/_types.h /usr/include/sys/_pthread/_pthread_types.h
nrif.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
nrif.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
nrif.o: /usr/include/sys/_types/_int16_t.h /usr/include/sys/_types/_int32_t.h
nrif.o: /usr/include/sys/_types/_int64_t.h
nrif.o: /usr/include/sys/_types/_u_int8_t.h
nrif.o: /usr/include/sys/_types/_u_int16_t.h
nrif.o: /usr/include/sys/_types/_u_int32_t.h
nrif.o: /usr/include/sys/_types/_u_int64_t.h
nrif.o: /usr/include/sys/_types/_intptr_t.h
nrif.o: /usr/include/sys/_types/_uintptr_t.h
nrif.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
nrif.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
nrif.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
nrif.o: /usr/include/secure/_common.h /usr/include/stdlib.h
nrif.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
nrif.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
nrif.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
nrif.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
nrif.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
nrif.o: /usr/include/mach/i386/_structs.h
nrif.o: /usr/include/sys/_pthread/_pthread_attr_t.h
nrif.o: /usr/include/sys/_types/_sigaltstack.h
nrif.o: /usr/include/sys/_types/_ucontext.h
nrif.o: /usr/include/sys/_types/_sigset_t.h /usr/include/sys/_types/_uid_t.h
nrif.o: /usr/include/sys/resource.h /usr/include/stdint.h
nrif.o: /usr/include/_types/_uint8_t.h /usr/include/_types/_uint16_t.h
nrif.o: /usr/include/_types/_uint32_t.h /usr/include/_types/_uint64_t.h
nrif.o: /usr/include/_types/_intmax_t.h /usr/include/_types/_uintmax_t.h
nrif.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
nrif.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
nrif.o: /usr/include/libkern/_OSByteOrder.h
nrif.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
nrif.o: /usr/include/sys/_types/_ct_rune_t.h
nrif.o: /usr/include/sys/_types/_rune_t.h /usr/include/sys/_types/_wchar_t.h
nrif.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_mode_t.h
nrif.o: /usr/include/string.h /usr/include/sys/_types/_rsize_t.h
nrif.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
nrif.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
nrif.o: /usr/include/errno.h /usr/include/sys/errno.h /usr/include/fcntl.h
nrif.o: /usr/include/sys/fcntl.h /usr/include/sys/_types/_o_sync.h
nrif.o: /usr/include/sys/_types/_o_dsync.h
nrif.o: /usr/include/sys/_types/_seek_set.h /usr/include/sys/_types/_s_ifmt.h
nrif.o: /usr/include/sys/_types/_timespec.h
nrif.o: /usr/include/sys/_types/_filesec_t.h ncarg_ras.h
nrif.o: /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
nrif.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
nrif.o: /usr/include/sys/_types/_caddr_t.h
nrif.o: /usr/include/sys/_types/_blkcnt_t.h
nrif.o: /usr/include/sys/_types/_blksize_t.h /usr/include/sys/_types/_gid_t.h
nrif.o: /usr/include/sys/_types/_in_addr_t.h
nrif.o: /usr/include/sys/_types/_in_port_t.h /usr/include/sys/_types/_ino_t.h
nrif.o: /usr/include/sys/_types/_ino64_t.h /usr/include/sys/_types/_key_t.h
nrif.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_clock_t.h
nrif.o: /usr/include/sys/_types/_time_t.h
nrif.o: /usr/include/sys/_types/_useconds_t.h
nrif.o: /usr/include/sys/_types/_suseconds_t.h
nrif.o: /usr/include/sys/_types/_fd_def.h
nrif.o: /usr/include/sys/_types/_fd_setsize.h
nrif.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
nrif.o: /usr/include/sys/_types/_fd_zero.h
nrif.o: /usr/include/sys/_types/_fd_isset.h
nrif.o: /usr/include/sys/_types/_fd_copy.h
nrif.o: /usr/include/sys/_pthread/_pthread_cond_t.h
nrif.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
nrif.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
nrif.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
nrif.o: /usr/include/sys/_pthread/_pthread_once_t.h
nrif.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
nrif.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
nrif.o: /usr/include/sys/_pthread/_pthread_t.h
nrif.o: /usr/include/sys/_pthread/_pthread_key_t.h
nrif.o: /usr/include/sys/_types/_fsblkcnt_t.h
nrif.o: /usr/include/sys/_types/_fsfilcnt_t.h /usr/include/sys/stat.h
nrif.o: ../../../.././include/ncarg/c.h options.h nrif.h
sunraster.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
sunraster.o: /usr/include/sys/_symbol_aliasing.h
sunraster.o: /usr/include/sys/_posix_availability.h
sunraster.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
sunraster.o: /usr/include/_types.h /usr/include/sys/_types.h
sunraster.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
sunraster.o: /usr/include/sys/_pthread/_pthread_types.h
sunraster.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
sunraster.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
sunraster.o: /usr/include/sys/_types/_int16_t.h
sunraster.o: /usr/include/sys/_types/_int32_t.h
sunraster.o: /usr/include/sys/_types/_int64_t.h
sunraster.o: /usr/include/sys/_types/_u_int8_t.h
sunraster.o: /usr/include/sys/_types/_u_int16_t.h
sunraster.o: /usr/include/sys/_types/_u_int32_t.h
sunraster.o: /usr/include/sys/_types/_u_int64_t.h
sunraster.o: /usr/include/sys/_types/_intptr_t.h
sunraster.o: /usr/include/sys/_types/_uintptr_t.h
sunraster.o: /usr/include/sys/_types/_size_t.h
sunraster.o: /usr/include/sys/_types/_null.h /usr/include/sys/stdio.h
sunraster.o: /usr/include/sys/_types/_off_t.h
sunraster.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
sunraster.o: /usr/include/secure/_common.h /usr/include/errno.h
sunraster.o: /usr/include/sys/errno.h /usr/include/sys/_types/_errno_t.h
sunraster.o: /usr/include/fcntl.h /usr/include/sys/fcntl.h
sunraster.o: /usr/include/sys/_types/_mode_t.h
sunraster.o: /usr/include/sys/_types/_pid_t.h
sunraster.o: /usr/include/sys/_types/_o_sync.h
sunraster.o: /usr/include/sys/_types/_o_dsync.h
sunraster.o: /usr/include/sys/_types/_seek_set.h
sunraster.o: /usr/include/sys/_types/_s_ifmt.h
sunraster.o: /usr/include/sys/_types/_timespec.h
sunraster.o: /usr/include/sys/_types/_filesec_t.h /usr/include/string.h
sunraster.o: /usr/include/sys/_types/_rsize_t.h /usr/include/strings.h
sunraster.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
sunraster.o: /usr/include/stdlib.h /usr/include/sys/wait.h
sunraster.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
sunraster.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
sunraster.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
sunraster.o: /usr/include/i386/_mcontext.h
sunraster.o: /usr/include/mach/machine/_structs.h
sunraster.o: /usr/include/mach/i386/_structs.h
sunraster.o: /usr/include/sys/_pthread/_pthread_attr_t.h
sunraster.o: /usr/include/sys/_types/_sigaltstack.h
sunraster.o: /usr/include/sys/_types/_ucontext.h
sunraster.o: /usr/include/sys/_types/_sigset_t.h
sunraster.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
sunraster.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
sunraster.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
sunraster.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
sunraster.o: /usr/include/_types/_uintmax_t.h
sunraster.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
sunraster.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
sunraster.o: /usr/include/libkern/_OSByteOrder.h
sunraster.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
sunraster.o: /usr/include/sys/_types/_ct_rune_t.h
sunraster.o: /usr/include/sys/_types/_rune_t.h
sunraster.o: /usr/include/sys/_types/_wchar_t.h
sunraster.o: /usr/include/sys/_types/_dev_t.h ncarg_ras.h
sunraster.o: /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
sunraster.o: /usr/include/sys/_types/_u_short.h
sunraster.o: /usr/include/sys/_types/_u_int.h
sunraster.o: /usr/include/sys/_types/_caddr_t.h
sunraster.o: /usr/include/sys/_types/_blkcnt_t.h
sunraster.o: /usr/include/sys/_types/_blksize_t.h
sunraster.o: /usr/include/sys/_types/_gid_t.h
sunraster.o: /usr/include/sys/_types/_in_addr_t.h
sunraster.o: /usr/include/sys/_types/_in_port_t.h
sunraster.o: /usr/include/sys/_types/_ino_t.h
sunraster.o: /usr/include/sys/_types/_ino64_t.h
sunraster.o: /usr/include/sys/_types/_key_t.h
sunraster.o: /usr/include/sys/_types/_nlink_t.h
sunraster.o: /usr/include/sys/_types/_clock_t.h
sunraster.o: /usr/include/sys/_types/_time_t.h
sunraster.o: /usr/include/sys/_types/_useconds_t.h
sunraster.o: /usr/include/sys/_types/_suseconds_t.h
sunraster.o: /usr/include/sys/_types/_fd_def.h
sunraster.o: /usr/include/sys/_types/_fd_setsize.h
sunraster.o: /usr/include/sys/_types/_fd_set.h
sunraster.o: /usr/include/sys/_types/_fd_clr.h
sunraster.o: /usr/include/sys/_types/_fd_zero.h
sunraster.o: /usr/include/sys/_types/_fd_isset.h
sunraster.o: /usr/include/sys/_types/_fd_copy.h
sunraster.o: /usr/include/sys/_pthread/_pthread_cond_t.h
sunraster.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
sunraster.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
sunraster.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
sunraster.o: /usr/include/sys/_pthread/_pthread_once_t.h
sunraster.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
sunraster.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
sunraster.o: /usr/include/sys/_pthread/_pthread_t.h
sunraster.o: /usr/include/sys/_pthread/_pthread_key_t.h
sunraster.o: /usr/include/sys/_types/_fsblkcnt_t.h
sunraster.o: /usr/include/sys/_types/_fsfilcnt_t.h /usr/include/sys/stat.h
sunraster.o: ../../../.././include/ncarg/c.h sunraster.h
xwd.o: /usr/include/stdlib.h /usr/include/Availability.h
xwd.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
xwd.o: /usr/include/sys/_types.h /usr/include/sys/cdefs.h
xwd.o: /usr/include/sys/_symbol_aliasing.h
xwd.o: /usr/include/sys/_posix_availability.h /usr/include/machine/_types.h
xwd.o: /usr/include/i386/_types.h /usr/include/sys/_pthread/_pthread_types.h
xwd.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
xwd.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
xwd.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
xwd.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
xwd.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
xwd.o: /usr/include/mach/i386/_structs.h /usr/include/machine/types.h
xwd.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
xwd.o: /usr/include/sys/_types/_int16_t.h /usr/include/sys/_types/_int32_t.h
xwd.o: /usr/include/sys/_types/_int64_t.h /usr/include/sys/_types/_u_int8_t.h
xwd.o: /usr/include/sys/_types/_u_int16_t.h
xwd.o: /usr/include/sys/_types/_u_int32_t.h
xwd.o: /usr/include/sys/_types/_u_int64_t.h
xwd.o: /usr/include/sys/_types/_intptr_t.h
xwd.o: /usr/include/sys/_types/_uintptr_t.h
xwd.o: /usr/include/sys/_pthread/_pthread_attr_t.h
xwd.o: /usr/include/sys/_types/_sigaltstack.h
xwd.o: /usr/include/sys/_types/_ucontext.h
xwd.o: /usr/include/sys/_types/_sigset_t.h /usr/include/sys/_types/_size_t.h
xwd.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
xwd.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
xwd.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
xwd.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
xwd.o: /usr/include/_types/_uintmax_t.h /usr/include/sys/_types/_timeval.h
xwd.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
xwd.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
xwd.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
xwd.o: /usr/include/sys/_types/_ct_rune_t.h /usr/include/sys/_types/_rune_t.h
xwd.o: /usr/include/sys/_types/_wchar_t.h /usr/include/sys/_types/_null.h
xwd.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_mode_t.h
xwd.o: /usr/include/errno.h /usr/include/sys/errno.h
xwd.o: /usr/include/sys/_types/_errno_t.h /usr/include/fcntl.h
xwd.o: /usr/include/sys/fcntl.h /usr/include/sys/_types/_off_t.h
xwd.o: /usr/include/sys/_types/_o_sync.h /usr/include/sys/_types/_o_dsync.h
xwd.o: /usr/include/sys/_types/_seek_set.h /usr/include/sys/_types/_s_ifmt.h
xwd.o: /usr/include/sys/_types/_timespec.h
xwd.o: /usr/include/sys/_types/_filesec_t.h /usr/include/sys/types.h
xwd.o: /usr/include/sys/_types/_u_char.h /usr/include/sys/_types/_u_short.h
xwd.o: /usr/include/sys/_types/_u_int.h /usr/include/sys/_types/_caddr_t.h
xwd.o: /usr/include/sys/_types/_blkcnt_t.h
xwd.o: /usr/include/sys/_types/_blksize_t.h /usr/include/sys/_types/_gid_t.h
xwd.o: /usr/include/sys/_types/_in_addr_t.h
xwd.o: /usr/include/sys/_types/_in_port_t.h /usr/include/sys/_types/_ino_t.h
xwd.o: /usr/include/sys/_types/_ino64_t.h /usr/include/sys/_types/_key_t.h
xwd.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_clock_t.h
xwd.o: /usr/include/sys/_types/_ssize_t.h /usr/include/sys/_types/_time_t.h
xwd.o: /usr/include/sys/_types/_useconds_t.h
xwd.o: /usr/include/sys/_types/_suseconds_t.h
xwd.o: /usr/include/sys/_types/_rsize_t.h /usr/include/sys/_types/_fd_def.h
xwd.o: /usr/include/sys/_types/_fd_setsize.h
xwd.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
xwd.o: /usr/include/sys/_types/_fd_zero.h /usr/include/sys/_types/_fd_isset.h
xwd.o: /usr/include/sys/_types/_fd_copy.h
xwd.o: /usr/include/sys/_pthread/_pthread_cond_t.h
xwd.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
xwd.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
xwd.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
xwd.o: /usr/include/sys/_pthread/_pthread_once_t.h
xwd.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
xwd.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
xwd.o: /usr/include/sys/_pthread/_pthread_t.h
xwd.o: /usr/include/sys/_pthread/_pthread_key_t.h
xwd.o: /usr/include/sys/_types/_fsblkcnt_t.h
xwd.o: /usr/include/sys/_types/_fsfilcnt_t.h /opt/X11/include/X11/Intrinsic.h
xwd.o: /opt/X11/include/X11/Xlib.h /opt/X11/include/X11/X.h
xwd.o: /opt/X11/include/X11/Xfuncproto.h /opt/X11/include/X11/Xosdefs.h
xwd.o: /usr/include/stddef.h /usr/include/sys/_types/_offsetof.h
xwd.o: /usr/include/sys/_types/_ptrdiff_t.h /usr/include/sys/_types/_wint_t.h
xwd.o: /opt/X11/include/X11/Xutil.h /opt/X11/include/X11/keysym.h
xwd.o: /opt/X11/include/X11/keysymdef.h /opt/X11/include/X11/Xresource.h
xwd.o: /usr/include/string.h /usr/include/strings.h
xwd.o: /usr/include/secure/_strings.h /usr/include/secure/_common.h
xwd.o: /usr/include/secure/_string.h /opt/X11/include/X11/Core.h
xwd.o: /opt/X11/include/X11/Composite.h /opt/X11/include/X11/Constraint.h
xwd.o: /opt/X11/include/X11/Object.h /opt/X11/include/X11/RectObj.h
xwd.o: /opt/X11/include/X11/XWDFile.h /opt/X11/include/X11/Xmd.h ncarg_ras.h
xwd.o: /usr/include/stdio.h /usr/include/sys/_types/_va_list.h
xwd.o: /usr/include/sys/stdio.h /usr/include/secure/_stdio.h
xwd.o: /usr/include/sys/stat.h ../../../.././include/ncarg/c.h
avsraster.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
avsraster.o: /usr/include/sys/_symbol_aliasing.h
avsraster.o: /usr/include/sys/_posix_availability.h
avsraster.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
avsraster.o: /usr/include/_types.h /usr/include/sys/_types.h
avsraster.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
avsraster.o: /usr/include/sys/_pthread/_pthread_types.h
avsraster.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
avsraster.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
avsraster.o: /usr/include/sys/_types/_int16_t.h
avsraster.o: /usr/include/sys/_types/_int32_t.h
avsraster.o: /usr/include/sys/_types/_int64_t.h
avsraster.o: /usr/include/sys/_types/_u_int8_t.h
avsraster.o: /usr/include/sys/_types/_u_int16_t.h
avsraster.o: /usr/include/sys/_types/_u_int32_t.h
avsraster.o: /usr/include/sys/_types/_u_int64_t.h
avsraster.o: /usr/include/sys/_types/_intptr_t.h
avsraster.o: /usr/include/sys/_types/_uintptr_t.h
avsraster.o: /usr/include/sys/_types/_size_t.h
avsraster.o: /usr/include/sys/_types/_null.h /usr/include/sys/stdio.h
avsraster.o: /usr/include/sys/_types/_off_t.h
avsraster.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
avsraster.o: /usr/include/secure/_common.h /usr/include/stdlib.h
avsraster.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
avsraster.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
avsraster.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
avsraster.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
avsraster.o: /usr/include/i386/_mcontext.h
avsraster.o: /usr/include/mach/machine/_structs.h
avsraster.o: /usr/include/mach/i386/_structs.h
avsraster.o: /usr/include/sys/_pthread/_pthread_attr_t.h
avsraster.o: /usr/include/sys/_types/_sigaltstack.h
avsraster.o: /usr/include/sys/_types/_ucontext.h
avsraster.o: /usr/include/sys/_types/_sigset_t.h
avsraster.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
avsraster.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
avsraster.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
avsraster.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
avsraster.o: /usr/include/_types/_uintmax_t.h
avsraster.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
avsraster.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
avsraster.o: /usr/include/libkern/_OSByteOrder.h
avsraster.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
avsraster.o: /usr/include/sys/_types/_ct_rune_t.h
avsraster.o: /usr/include/sys/_types/_rune_t.h
avsraster.o: /usr/include/sys/_types/_wchar_t.h
avsraster.o: /usr/include/sys/_types/_dev_t.h
avsraster.o: /usr/include/sys/_types/_mode_t.h /usr/include/errno.h
avsraster.o: /usr/include/sys/errno.h /usr/include/sys/_types/_errno_t.h
avsraster.o: /usr/include/fcntl.h /usr/include/sys/fcntl.h
avsraster.o: /usr/include/sys/_types/_o_sync.h
avsraster.o: /usr/include/sys/_types/_o_dsync.h
avsraster.o: /usr/include/sys/_types/_seek_set.h
avsraster.o: /usr/include/sys/_types/_s_ifmt.h
avsraster.o: /usr/include/sys/_types/_timespec.h
avsraster.o: /usr/include/sys/_types/_filesec_t.h /usr/include/string.h
avsraster.o: /usr/include/sys/_types/_rsize_t.h /usr/include/strings.h
avsraster.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
avsraster.o: ncarg_ras.h /usr/include/sys/types.h
avsraster.o: /usr/include/sys/_types/_u_char.h
avsraster.o: /usr/include/sys/_types/_u_short.h
avsraster.o: /usr/include/sys/_types/_u_int.h
avsraster.o: /usr/include/sys/_types/_caddr_t.h
avsraster.o: /usr/include/sys/_types/_blkcnt_t.h
avsraster.o: /usr/include/sys/_types/_blksize_t.h
avsraster.o: /usr/include/sys/_types/_gid_t.h
avsraster.o: /usr/include/sys/_types/_in_addr_t.h
avsraster.o: /usr/include/sys/_types/_in_port_t.h
avsraster.o: /usr/include/sys/_types/_ino_t.h
avsraster.o: /usr/include/sys/_types/_ino64_t.h
avsraster.o: /usr/include/sys/_types/_key_t.h
avsraster.o: /usr/include/sys/_types/_nlink_t.h
avsraster.o: /usr/include/sys/_types/_clock_t.h
avsraster.o: /usr/include/sys/_types/_time_t.h
avsraster.o: /usr/include/sys/_types/_useconds_t.h
avsraster.o: /usr/include/sys/_types/_suseconds_t.h
avsraster.o: /usr/include/sys/_types/_fd_def.h
avsraster.o: /usr/include/sys/_types/_fd_setsize.h
avsraster.o: /usr/include/sys/_types/_fd_set.h
avsraster.o: /usr/include/sys/_types/_fd_clr.h
avsraster.o: /usr/include/sys/_types/_fd_zero.h
avsraster.o: /usr/include/sys/_types/_fd_isset.h
avsraster.o: /usr/include/sys/_types/_fd_copy.h
avsraster.o: /usr/include/sys/_pthread/_pthread_cond_t.h
avsraster.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
avsraster.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
avsraster.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
avsraster.o: /usr/include/sys/_pthread/_pthread_once_t.h
avsraster.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
avsraster.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
avsraster.o: /usr/include/sys/_pthread/_pthread_t.h
avsraster.o: /usr/include/sys/_pthread/_pthread_key_t.h
avsraster.o: /usr/include/sys/_types/_fsblkcnt_t.h
avsraster.o: /usr/include/sys/_types/_fsfilcnt_t.h /usr/include/sys/stat.h
avsraster.o: ../../../.././include/ncarg/c.h avsraster.h
sgiraster.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
sgiraster.o: /usr/include/sys/_symbol_aliasing.h
sgiraster.o: /usr/include/sys/_posix_availability.h
sgiraster.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
sgiraster.o: /usr/include/_types.h /usr/include/sys/_types.h
sgiraster.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
sgiraster.o: /usr/include/sys/_pthread/_pthread_types.h
sgiraster.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
sgiraster.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
sgiraster.o: /usr/include/sys/_types/_int16_t.h
sgiraster.o: /usr/include/sys/_types/_int32_t.h
sgiraster.o: /usr/include/sys/_types/_int64_t.h
sgiraster.o: /usr/include/sys/_types/_u_int8_t.h
sgiraster.o: /usr/include/sys/_types/_u_int16_t.h
sgiraster.o: /usr/include/sys/_types/_u_int32_t.h
sgiraster.o: /usr/include/sys/_types/_u_int64_t.h
sgiraster.o: /usr/include/sys/_types/_intptr_t.h
sgiraster.o: /usr/include/sys/_types/_uintptr_t.h
sgiraster.o: /usr/include/sys/_types/_size_t.h
sgiraster.o: /usr/include/sys/_types/_null.h /usr/include/sys/stdio.h
sgiraster.o: /usr/include/sys/_types/_off_t.h
sgiraster.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
sgiraster.o: /usr/include/secure/_common.h /usr/include/stdlib.h
sgiraster.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
sgiraster.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
sgiraster.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
sgiraster.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
sgiraster.o: /usr/include/i386/_mcontext.h
sgiraster.o: /usr/include/mach/machine/_structs.h
sgiraster.o: /usr/include/mach/i386/_structs.h
sgiraster.o: /usr/include/sys/_pthread/_pthread_attr_t.h
sgiraster.o: /usr/include/sys/_types/_sigaltstack.h
sgiraster.o: /usr/include/sys/_types/_ucontext.h
sgiraster.o: /usr/include/sys/_types/_sigset_t.h
sgiraster.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
sgiraster.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
sgiraster.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
sgiraster.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
sgiraster.o: /usr/include/_types/_uintmax_t.h
sgiraster.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
sgiraster.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
sgiraster.o: /usr/include/libkern/_OSByteOrder.h
sgiraster.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
sgiraster.o: /usr/include/sys/_types/_ct_rune_t.h
sgiraster.o: /usr/include/sys/_types/_rune_t.h
sgiraster.o: /usr/include/sys/_types/_wchar_t.h
sgiraster.o: /usr/include/sys/_types/_dev_t.h
sgiraster.o: /usr/include/sys/_types/_mode_t.h /usr/include/errno.h
sgiraster.o: /usr/include/sys/errno.h /usr/include/sys/_types/_errno_t.h
sgiraster.o: /usr/include/fcntl.h /usr/include/sys/fcntl.h
sgiraster.o: /usr/include/sys/_types/_o_sync.h
sgiraster.o: /usr/include/sys/_types/_o_dsync.h
sgiraster.o: /usr/include/sys/_types/_seek_set.h
sgiraster.o: /usr/include/sys/_types/_s_ifmt.h
sgiraster.o: /usr/include/sys/_types/_timespec.h
sgiraster.o: /usr/include/sys/_types/_filesec_t.h /usr/include/string.h
sgiraster.o: /usr/include/sys/_types/_rsize_t.h /usr/include/strings.h
sgiraster.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
sgiraster.o: ncarg_ras.h /usr/include/sys/types.h
sgiraster.o: /usr/include/sys/_types/_u_char.h
sgiraster.o: /usr/include/sys/_types/_u_short.h
sgiraster.o: /usr/include/sys/_types/_u_int.h
sgiraster.o: /usr/include/sys/_types/_caddr_t.h
sgiraster.o: /usr/include/sys/_types/_blkcnt_t.h
sgiraster.o: /usr/include/sys/_types/_blksize_t.h
sgiraster.o: /usr/include/sys/_types/_gid_t.h
sgiraster.o: /usr/include/sys/_types/_in_addr_t.h
sgiraster.o: /usr/include/sys/_types/_in_port_t.h
sgiraster.o: /usr/include/sys/_types/_ino_t.h
sgiraster.o: /usr/include/sys/_types/_ino64_t.h
sgiraster.o: /usr/include/sys/_types/_key_t.h
sgiraster.o: /usr/include/sys/_types/_nlink_t.h
sgiraster.o: /usr/include/sys/_types/_clock_t.h
sgiraster.o: /usr/include/sys/_types/_time_t.h
sgiraster.o: /usr/include/sys/_types/_useconds_t.h
sgiraster.o: /usr/include/sys/_types/_suseconds_t.h
sgiraster.o: /usr/include/sys/_types/_fd_def.h
sgiraster.o: /usr/include/sys/_types/_fd_setsize.h
sgiraster.o: /usr/include/sys/_types/_fd_set.h
sgiraster.o: /usr/include/sys/_types/_fd_clr.h
sgiraster.o: /usr/include/sys/_types/_fd_zero.h
sgiraster.o: /usr/include/sys/_types/_fd_isset.h
sgiraster.o: /usr/include/sys/_types/_fd_copy.h
sgiraster.o: /usr/include/sys/_pthread/_pthread_cond_t.h
sgiraster.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
sgiraster.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
sgiraster.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
sgiraster.o: /usr/include/sys/_pthread/_pthread_once_t.h
sgiraster.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
sgiraster.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
sgiraster.o: /usr/include/sys/_pthread/_pthread_t.h
sgiraster.o: /usr/include/sys/_pthread/_pthread_key_t.h
sgiraster.o: /usr/include/sys/_types/_fsblkcnt_t.h
sgiraster.o: /usr/include/sys/_types/_fsfilcnt_t.h /usr/include/sys/stat.h
sgiraster.o: ../../../.././include/ncarg/c.h options.h sgiraster.h
abekas.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
abekas.o: /usr/include/sys/_symbol_aliasing.h
abekas.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
abekas.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
abekas.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
abekas.o: /usr/include/i386/_types.h
abekas.o: /usr/include/sys/_pthread/_pthread_types.h
abekas.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
abekas.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
abekas.o: /usr/include/sys/_types/_int16_t.h
abekas.o: /usr/include/sys/_types/_int32_t.h
abekas.o: /usr/include/sys/_types/_int64_t.h
abekas.o: /usr/include/sys/_types/_u_int8_t.h
abekas.o: /usr/include/sys/_types/_u_int16_t.h
abekas.o: /usr/include/sys/_types/_u_int32_t.h
abekas.o: /usr/include/sys/_types/_u_int64_t.h
abekas.o: /usr/include/sys/_types/_intptr_t.h
abekas.o: /usr/include/sys/_types/_uintptr_t.h
abekas.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
abekas.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
abekas.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
abekas.o: /usr/include/secure/_common.h /usr/include/stdlib.h
abekas.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
abekas.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
abekas.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
abekas.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
abekas.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
abekas.o: /usr/include/mach/i386/_structs.h
abekas.o: /usr/include/sys/_pthread/_pthread_attr_t.h
abekas.o: /usr/include/sys/_types/_sigaltstack.h
abekas.o: /usr/include/sys/_types/_ucontext.h
abekas.o: /usr/include/sys/_types/_sigset_t.h
abekas.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
abekas.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
abekas.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
abekas.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
abekas.o: /usr/include/_types/_uintmax_t.h /usr/include/sys/_types/_timeval.h
abekas.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
abekas.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
abekas.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
abekas.o: /usr/include/sys/_types/_ct_rune_t.h
abekas.o: /usr/include/sys/_types/_rune_t.h
abekas.o: /usr/include/sys/_types/_wchar_t.h /usr/include/sys/_types/_dev_t.h
abekas.o: /usr/include/sys/_types/_mode_t.h /usr/include/string.h
abekas.o: /usr/include/sys/_types/_rsize_t.h
abekas.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
abekas.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
abekas.o: /usr/include/errno.h /usr/include/sys/errno.h /usr/include/fcntl.h
abekas.o: /usr/include/sys/fcntl.h /usr/include/sys/_types/_o_sync.h
abekas.o: /usr/include/sys/_types/_o_dsync.h
abekas.o: /usr/include/sys/_types/_seek_set.h
abekas.o: /usr/include/sys/_types/_s_ifmt.h
abekas.o: /usr/include/sys/_types/_timespec.h
abekas.o: /usr/include/sys/_types/_filesec_t.h ncarg_ras.h
abekas.o: /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
abekas.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
abekas.o: /usr/include/sys/_types/_caddr_t.h
abekas.o: /usr/include/sys/_types/_blkcnt_t.h
abekas.o: /usr/include/sys/_types/_blksize_t.h
abekas.o: /usr/include/sys/_types/_gid_t.h
abekas.o: /usr/include/sys/_types/_in_addr_t.h
abekas.o: /usr/include/sys/_types/_in_port_t.h
abekas.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
abekas.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_nlink_t.h
abekas.o: /usr/include/sys/_types/_clock_t.h
abekas.o: /usr/include/sys/_types/_time_t.h
abekas.o: /usr/include/sys/_types/_useconds_t.h
abekas.o: /usr/include/sys/_types/_suseconds_t.h
abekas.o: /usr/include/sys/_types/_fd_def.h
abekas.o: /usr/include/sys/_types/_fd_setsize.h
abekas.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
abekas.o: /usr/include/sys/_types/_fd_zero.h
abekas.o: /usr/include/sys/_types/_fd_isset.h
abekas.o: /usr/include/sys/_types/_fd_copy.h
abekas.o: /usr/include/sys/_pthread/_pthread_cond_t.h
abekas.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
abekas.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
abekas.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
abekas.o: /usr/include/sys/_pthread/_pthread_once_t.h
abekas.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
abekas.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
abekas.o: /usr/include/sys/_pthread/_pthread_t.h
abekas.o: /usr/include/sys/_pthread/_pthread_key_t.h
abekas.o: /usr/include/sys/_types/_fsblkcnt_t.h
abekas.o: /usr/include/sys/_types/_fsfilcnt_t.h /usr/include/sys/stat.h
abekas.o: ../../../.././include/ncarg/c.h options.h abekas.h
binary.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
binary.o: /usr/include/sys/_symbol_aliasing.h
binary.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
binary.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
binary.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
binary.o: /usr/include/i386/_types.h
binary.o: /usr/include/sys/_pthread/_pthread_types.h
binary.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
binary.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
binary.o: /usr/include/sys/_types/_int16_t.h
binary.o: /usr/include/sys/_types/_int32_t.h
binary.o: /usr/include/sys/_types/_int64_t.h
binary.o: /usr/include/sys/_types/_u_int8_t.h
binary.o: /usr/include/sys/_types/_u_int16_t.h
binary.o: /usr/include/sys/_types/_u_int32_t.h
binary.o: /usr/include/sys/_types/_u_int64_t.h
binary.o: /usr/include/sys/_types/_intptr_t.h
binary.o: /usr/include/sys/_types/_uintptr_t.h
binary.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
binary.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
binary.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
binary.o: /usr/include/secure/_common.h /usr/include/stdlib.h
binary.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
binary.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
binary.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
binary.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
binary.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
binary.o: /usr/include/mach/i386/_structs.h
binary.o: /usr/include/sys/_pthread/_pthread_attr_t.h
binary.o: /usr/include/sys/_types/_sigaltstack.h
binary.o: /usr/include/sys/_types/_ucontext.h
binary.o: /usr/include/sys/_types/_sigset_t.h
binary.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
binary.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
binary.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
binary.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
binary.o: /usr/include/_types/_uintmax_t.h /usr/include/sys/_types/_timeval.h
binary.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
binary.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
binary.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
binary.o: /usr/include/sys/_types/_ct_rune_t.h
binary.o: /usr/include/sys/_types/_rune_t.h
binary.o: /usr/include/sys/_types/_wchar_t.h /usr/include/sys/_types/_dev_t.h
binary.o: /usr/include/sys/_types/_mode_t.h /usr/include/errno.h
binary.o: /usr/include/sys/errno.h /usr/include/sys/_types/_errno_t.h
binary.o: /usr/include/fcntl.h /usr/include/sys/fcntl.h
binary.o: /usr/include/sys/_types/_o_sync.h
binary.o: /usr/include/sys/_types/_o_dsync.h
binary.o: /usr/include/sys/_types/_seek_set.h
binary.o: /usr/include/sys/_types/_s_ifmt.h
binary.o: /usr/include/sys/_types/_timespec.h
binary.o: /usr/include/sys/_types/_filesec_t.h /usr/include/string.h
binary.o: /usr/include/sys/_types/_rsize_t.h /usr/include/strings.h
binary.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
binary.o: ncarg_ras.h /usr/include/sys/types.h
binary.o: /usr/include/sys/_types/_u_char.h
binary.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
binary.o: /usr/include/sys/_types/_caddr_t.h
binary.o: /usr/include/sys/_types/_blkcnt_t.h
binary.o: /usr/include/sys/_types/_blksize_t.h
binary.o: /usr/include/sys/_types/_gid_t.h
binary.o: /usr/include/sys/_types/_in_addr_t.h
binary.o: /usr/include/sys/_types/_in_port_t.h
binary.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
binary.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_nlink_t.h
binary.o: /usr/include/sys/_types/_clock_t.h
binary.o: /usr/include/sys/_types/_time_t.h
binary.o: /usr/include/sys/_types/_useconds_t.h
binary.o: /usr/include/sys/_types/_suseconds_t.h
binary.o: /usr/include/sys/_types/_fd_def.h
binary.o: /usr/include/sys/_types/_fd_setsize.h
binary.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
binary.o: /usr/include/sys/_types/_fd_zero.h
binary.o: /usr/include/sys/_types/_fd_isset.h
binary.o: /usr/include/sys/_types/_fd_copy.h
binary.o: /usr/include/sys/_pthread/_pthread_cond_t.h
binary.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
binary.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
binary.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
binary.o: /usr/include/sys/_pthread/_pthread_once_t.h
binary.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
binary.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
binary.o: /usr/include/sys/_pthread/_pthread_t.h
binary.o: /usr/include/sys/_pthread/_pthread_key_t.h
binary.o: /usr/include/sys/_types/_fsblkcnt_t.h
binary.o: /usr/include/sys/_types/_fsfilcnt_t.h /usr/include/sys/stat.h
binary.o: ../../../.././include/ncarg/c.h options.h
yuv.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
yuv.o: /usr/include/sys/_symbol_aliasing.h
yuv.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
yuv.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
yuv.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
yuv.o: /usr/include/i386/_types.h /usr/include/sys/_pthread/_pthread_types.h
yuv.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
yuv.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
yuv.o: /usr/include/sys/_types/_int16_t.h /usr/include/sys/_types/_int32_t.h
yuv.o: /usr/include/sys/_types/_int64_t.h /usr/include/sys/_types/_u_int8_t.h
yuv.o: /usr/include/sys/_types/_u_int16_t.h
yuv.o: /usr/include/sys/_types/_u_int32_t.h
yuv.o: /usr/include/sys/_types/_u_int64_t.h
yuv.o: /usr/include/sys/_types/_intptr_t.h
yuv.o: /usr/include/sys/_types/_uintptr_t.h /usr/include/sys/_types/_size_t.h
yuv.o: /usr/include/sys/_types/_null.h /usr/include/sys/stdio.h
yuv.o: /usr/include/sys/_types/_off_t.h /usr/include/sys/_types/_ssize_t.h
yuv.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
yuv.o: /usr/include/stdlib.h /usr/include/sys/wait.h
yuv.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_id_t.h
yuv.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
yuv.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
yuv.o: /usr/include/machine/_mcontext.h /usr/include/i386/_mcontext.h
yuv.o: /usr/include/mach/machine/_structs.h /usr/include/mach/i386/_structs.h
yuv.o: /usr/include/sys/_pthread/_pthread_attr_t.h
yuv.o: /usr/include/sys/_types/_sigaltstack.h
yuv.o: /usr/include/sys/_types/_ucontext.h
yuv.o: /usr/include/sys/_types/_sigset_t.h /usr/include/sys/_types/_uid_t.h
yuv.o: /usr/include/sys/resource.h /usr/include/stdint.h
yuv.o: /usr/include/_types/_uint8_t.h /usr/include/_types/_uint16_t.h
yuv.o: /usr/include/_types/_uint32_t.h /usr/include/_types/_uint64_t.h
yuv.o: /usr/include/_types/_intmax_t.h /usr/include/_types/_uintmax_t.h
yuv.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
yuv.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
yuv.o: /usr/include/libkern/_OSByteOrder.h
yuv.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
yuv.o: /usr/include/sys/_types/_ct_rune_t.h /usr/include/sys/_types/_rune_t.h
yuv.o: /usr/include/sys/_types/_wchar_t.h /usr/include/sys/_types/_dev_t.h
yuv.o: /usr/include/sys/_types/_mode_t.h /usr/include/errno.h
yuv.o: /usr/include/sys/errno.h /usr/include/sys/_types/_errno_t.h
yuv.o: /usr/include/fcntl.h /usr/include/sys/fcntl.h
yuv.o: /usr/include/sys/_types/_o_sync.h /usr/include/sys/_types/_o_dsync.h
yuv.o: /usr/include/sys/_types/_seek_set.h /usr/include/sys/_types/_s_ifmt.h
yuv.o: /usr/include/sys/_types/_timespec.h
yuv.o: /usr/include/sys/_types/_filesec_t.h /usr/include/string.h
yuv.o: /usr/include/sys/_types/_rsize_t.h /usr/include/strings.h
yuv.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h options.h
yuv.o: yuv.h ncarg_ras.h /usr/include/sys/types.h
yuv.o: /usr/include/sys/_types/_u_char.h /usr/include/sys/_types/_u_short.h
yuv.o: /usr/include/sys/_types/_u_int.h /usr/include/sys/_types/_caddr_t.h
yuv.o: /usr/include/sys/_types/_blkcnt_t.h
yuv.o: /usr/include/sys/_types/_blksize_t.h /usr/include/sys/_types/_gid_t.h
yuv.o: /usr/include/sys/_types/_in_addr_t.h
yuv.o: /usr/include/sys/_types/_in_port_t.h /usr/include/sys/_types/_ino_t.h
yuv.o: /usr/include/sys/_types/_ino64_t.h /usr/include/sys/_types/_key_t.h
yuv.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_clock_t.h
yuv.o: /usr/include/sys/_types/_time_t.h
yuv.o: /usr/include/sys/_types/_useconds_t.h
yuv.o: /usr/include/sys/_types/_suseconds_t.h
yuv.o: /usr/include/sys/_types/_fd_def.h
yuv.o: /usr/include/sys/_types/_fd_setsize.h
yuv.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
yuv.o: /usr/include/sys/_types/_fd_zero.h /usr/include/sys/_types/_fd_isset.h
yuv.o: /usr/include/sys/_types/_fd_copy.h
yuv.o: /usr/include/sys/_pthread/_pthread_cond_t.h
yuv.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
yuv.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
yuv.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
yuv.o: /usr/include/sys/_pthread/_pthread_once_t.h
yuv.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
yuv.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
yuv.o: /usr/include/sys/_pthread/_pthread_t.h
yuv.o: /usr/include/sys/_pthread/_pthread_key_t.h
yuv.o: /usr/include/sys/_types/_fsblkcnt_t.h
yuv.o: /usr/include/sys/_types/_fsfilcnt_t.h /usr/include/sys/stat.h
yuv.o: ../../../.././include/ncarg/c.h
