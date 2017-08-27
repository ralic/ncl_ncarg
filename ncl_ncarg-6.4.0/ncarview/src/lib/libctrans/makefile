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
CURRENT_DIR		= ./ncarview/src/lib/libctrans

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

#
#	$Id: yMakefile,v 1.28 2001-04-12 15:51:28 haley Exp $
#

MYNAME		= libctrans.o
INCLUDES	= ctrans.h

X11SRCS	= $(DX11SRCS)
X11OBJS	= $(DX11OBJS)
X11DEFINES = -DX11

CTXTSRCS = $(DCTXTSRCS)
CTXTOBJS = $(DCTXTOBJS)
CTXTDEFINES = -DCTXT

SRCS	=  	$(GENSRCS) $(X11SRCS) $(CGISRCS) $(CTXTSRCS)  	$(SUNVSRCS) $(SUNRSRCS) $(DEVSRCS) $(VMSSRCS)

OBJS	=  	$(GENOBJS) $(X11OBJS) $(CGIOBJS) $(CTXTOBJS)  	$(SUNVOBJS) $(SUNROBJS) $(DEVOBJS) $(VMSOBJS)

FCGCDIRDEF	=	-DFONTCAPDIR='"$(FONTCAPDIR)"'  			-DGRAPHCAPDIR='"$(GRAPHCAPDIR)"'

EXTRA_DEFINES = $(CGIDEFINES) $(X11DEFINES) $(SUNDEFINES) $(CTXTDEFINES) $(FCGCDIRDEF) $(VDEFINE)

# Generic code and libraries for all versions of ctrans.

GENSRCS =   	bitops.c buffer.c clip.c ctrans.c default.c  	fill.c fontlist.c format.c gcap.c gcread.c in.c rast.c  	readfont.c rgbtohls.c text.c translate.c env.c  	soft_fill.c cell.c commondev.c gcapdev.c gcaprast.c  	rastdev.c hatch.c pal.c ctrans_api.c cgmmisc.c misc.c

GENOBJS	=  	bitops.o buffer.o clip.o ctrans.o default.o  	fill.o fontlist.o format.o gcap.o gcread.o in.o rast.o  	readfont.o rgbtohls.o text.o translate.o env.o  	soft_fill.o cell.o commondev.o gcapdev.o gcaprast.o  	rastdev.o hatch.o pal.o ctrans_api.o cgmmisc.o misc.o

DEVSRCS	= devices.c
DEVOBJS	= devices.o

# Declare code and Libraries for SunCGI.

DCGISRCS = cgi.c cgi_cell_sim.c

DCGIOBJS = cgi.o cgi_cell_sim.o

# Declare code and macros for X11R3

DX11SRCS =  	X11_class0.c X11_class1.c X11_class3.c  	X11_class4.c X11_class6.c X11_class7.c  	Xcrm.c xresources.c

DX11OBJS =  	X11_class0.o X11_class1.o X11_class3.o  	X11_class4.o X11_class6.o X11_class7.o  	Xcrm.o xresources.o

DSUNVSRCS =  	sun_view.c

DSUNVOBJS =  	sun_view.o

DSUNRSRCS =  	sunraster.c

DSUNROBJS =  	sunraster.o

# Declare code for Clear Text driver.

DCTXTSRCS = clear_text.c

DCTXTOBJS = clear_text.o

all::

all-local:: $(MYNAME)

$(MYNAME): $(OBJS)
	$(LD_REL) -r -o $(MYNAME) $(OBJS)
all::

all-local:: $(MYNAME)

install-local:: $(MYNAME)
	@if (test ! -d $(ROBJPATH)); then ($(MKDIRHIER) $(ROBJPATH)); fi
	@for name in $(MYNAME); do \
	if (test -f $$name.$(EXE_SUFFIX)) then \
	(echo "Installing $$name.$(EXE_SUFFIX)";\
	$(INSTALL) -m $(INSTALL_LIB) $$name.$(EXE_SUFFIX) $(ROBJPATH));\
	else \
	(echo "Installing $$name";\
	$(INSTALL) -m $(INSTALL_LIB) $$name $(ROBJPATH)); fi \
	done

all::

clean-local::
	-@$(RM) $(MYNAME) $(OBJS)

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

lint:
	$(LINT) $(CINC) $(SRCS) 1> lint.out 2>&1

env.o:	env.c Makefile

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

bitops.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
bitops.o: /usr/include/sys/_symbol_aliasing.h
bitops.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
bitops.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
bitops.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
bitops.o: /usr/include/i386/_types.h
bitops.o: /usr/include/sys/_pthread/_pthread_types.h
bitops.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
bitops.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
bitops.o: /usr/include/sys/_types/_int16_t.h
bitops.o: /usr/include/sys/_types/_int32_t.h
bitops.o: /usr/include/sys/_types/_int64_t.h
bitops.o: /usr/include/sys/_types/_u_int8_t.h
bitops.o: /usr/include/sys/_types/_u_int16_t.h
bitops.o: /usr/include/sys/_types/_u_int32_t.h
bitops.o: /usr/include/sys/_types/_u_int64_t.h
bitops.o: /usr/include/sys/_types/_intptr_t.h
bitops.o: /usr/include/sys/_types/_uintptr_t.h
bitops.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
bitops.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
bitops.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
bitops.o: /usr/include/secure/_common.h ../../../.././include/ncarg/c.h
bitops.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
bitops.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
bitops.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
bitops.o: /usr/include/libkern/i386/_OSByteOrder.h
bitops.o: /usr/include/sys/_types/_u_char.h
bitops.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
bitops.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
bitops.o: /usr/include/sys/_types/_blkcnt_t.h
bitops.o: /usr/include/sys/_types/_blksize_t.h
bitops.o: /usr/include/sys/_types/_gid_t.h
bitops.o: /usr/include/sys/_types/_in_addr_t.h
bitops.o: /usr/include/sys/_types/_in_port_t.h
bitops.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
bitops.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
bitops.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
bitops.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
bitops.o: /usr/include/sys/_types/_clock_t.h
bitops.o: /usr/include/sys/_types/_time_t.h
bitops.o: /usr/include/sys/_types/_useconds_t.h
bitops.o: /usr/include/sys/_types/_suseconds_t.h
bitops.o: /usr/include/sys/_types/_rsize_t.h
bitops.o: /usr/include/sys/_types/_errno_t.h
bitops.o: /usr/include/sys/_types/_fd_def.h
bitops.o: /usr/include/sys/_types/_fd_setsize.h
bitops.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
bitops.o: /usr/include/sys/_types/_fd_zero.h
bitops.o: /usr/include/sys/_types/_fd_isset.h
bitops.o: /usr/include/sys/_types/_fd_copy.h
bitops.o: /usr/include/sys/_pthread/_pthread_attr_t.h
bitops.o: /usr/include/sys/_pthread/_pthread_cond_t.h
bitops.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
bitops.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
bitops.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
bitops.o: /usr/include/sys/_pthread/_pthread_once_t.h
bitops.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
bitops.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
bitops.o: /usr/include/sys/_pthread/_pthread_t.h
bitops.o: /usr/include/sys/_pthread/_pthread_key_t.h
bitops.o: /usr/include/sys/_types/_fsblkcnt_t.h
bitops.o: /usr/include/sys/_types/_fsfilcnt_t.h default.h
bitops.o: ../../../.././include/ncarg/cgmdef.h cgmc.h text.h
buffer.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
buffer.o: /usr/include/sys/_symbol_aliasing.h
buffer.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
buffer.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
buffer.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
buffer.o: /usr/include/i386/_types.h
buffer.o: /usr/include/sys/_pthread/_pthread_types.h
buffer.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
buffer.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
buffer.o: /usr/include/sys/_types/_int16_t.h
buffer.o: /usr/include/sys/_types/_int32_t.h
buffer.o: /usr/include/sys/_types/_int64_t.h
buffer.o: /usr/include/sys/_types/_u_int8_t.h
buffer.o: /usr/include/sys/_types/_u_int16_t.h
buffer.o: /usr/include/sys/_types/_u_int32_t.h
buffer.o: /usr/include/sys/_types/_u_int64_t.h
buffer.o: /usr/include/sys/_types/_intptr_t.h
buffer.o: /usr/include/sys/_types/_uintptr_t.h
buffer.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
buffer.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
buffer.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
buffer.o: /usr/include/secure/_common.h /usr/include/stdlib.h
buffer.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
buffer.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
buffer.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
buffer.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
buffer.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
buffer.o: /usr/include/mach/i386/_structs.h
buffer.o: /usr/include/sys/_pthread/_pthread_attr_t.h
buffer.o: /usr/include/sys/_types/_sigaltstack.h
buffer.o: /usr/include/sys/_types/_ucontext.h
buffer.o: /usr/include/sys/_types/_sigset_t.h
buffer.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
buffer.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
buffer.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
buffer.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
buffer.o: /usr/include/_types/_uintmax_t.h /usr/include/sys/_types/_timeval.h
buffer.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
buffer.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
buffer.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
buffer.o: /usr/include/sys/_types/_ct_rune_t.h
buffer.o: /usr/include/sys/_types/_rune_t.h
buffer.o: /usr/include/sys/_types/_wchar_t.h /usr/include/sys/_types/_dev_t.h
buffer.o: /usr/include/sys/_types/_mode_t.h /usr/include/fcntl.h
buffer.o: /usr/include/sys/fcntl.h /usr/include/sys/_types/_o_sync.h
buffer.o: /usr/include/sys/_types/_o_dsync.h
buffer.o: /usr/include/sys/_types/_seek_set.h
buffer.o: /usr/include/sys/_types/_s_ifmt.h
buffer.o: /usr/include/sys/_types/_timespec.h
buffer.o: /usr/include/sys/_types/_filesec_t.h /usr/include/errno.h
buffer.o: /usr/include/sys/errno.h /usr/include/sys/_types/_errno_t.h
buffer.o: ../../../.././include/ncarg/c.h /usr/include/sys/types.h
buffer.o: /usr/include/sys/_types/_u_char.h
buffer.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
buffer.o: /usr/include/sys/_types/_caddr_t.h
buffer.o: /usr/include/sys/_types/_blkcnt_t.h
buffer.o: /usr/include/sys/_types/_blksize_t.h
buffer.o: /usr/include/sys/_types/_gid_t.h
buffer.o: /usr/include/sys/_types/_in_addr_t.h
buffer.o: /usr/include/sys/_types/_in_port_t.h
buffer.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
buffer.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_nlink_t.h
buffer.o: /usr/include/sys/_types/_clock_t.h
buffer.o: /usr/include/sys/_types/_time_t.h
buffer.o: /usr/include/sys/_types/_useconds_t.h
buffer.o: /usr/include/sys/_types/_suseconds_t.h
buffer.o: /usr/include/sys/_types/_rsize_t.h
buffer.o: /usr/include/sys/_types/_fd_def.h
buffer.o: /usr/include/sys/_types/_fd_setsize.h
buffer.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
buffer.o: /usr/include/sys/_types/_fd_zero.h
buffer.o: /usr/include/sys/_types/_fd_isset.h
buffer.o: /usr/include/sys/_types/_fd_copy.h
buffer.o: /usr/include/sys/_pthread/_pthread_cond_t.h
buffer.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
buffer.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
buffer.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
buffer.o: /usr/include/sys/_pthread/_pthread_once_t.h
buffer.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
buffer.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
buffer.o: /usr/include/sys/_pthread/_pthread_t.h
buffer.o: /usr/include/sys/_pthread/_pthread_key_t.h
buffer.o: /usr/include/sys/_types/_fsblkcnt_t.h
buffer.o: /usr/include/sys/_types/_fsfilcnt_t.h ctrandef.h
clip.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
clip.o: /usr/include/sys/_symbol_aliasing.h
clip.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
clip.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
clip.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
clip.o: /usr/include/i386/_types.h /usr/include/sys/_pthread/_pthread_types.h
clip.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
clip.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
clip.o: /usr/include/sys/_types/_int16_t.h /usr/include/sys/_types/_int32_t.h
clip.o: /usr/include/sys/_types/_int64_t.h
clip.o: /usr/include/sys/_types/_u_int8_t.h
clip.o: /usr/include/sys/_types/_u_int16_t.h
clip.o: /usr/include/sys/_types/_u_int32_t.h
clip.o: /usr/include/sys/_types/_u_int64_t.h
clip.o: /usr/include/sys/_types/_intptr_t.h
clip.o: /usr/include/sys/_types/_uintptr_t.h
clip.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
clip.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
clip.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
clip.o: /usr/include/secure/_common.h ../../../.././include/ncarg/c.h
clip.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
clip.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
clip.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
clip.o: /usr/include/libkern/i386/_OSByteOrder.h
clip.o: /usr/include/sys/_types/_u_char.h /usr/include/sys/_types/_u_short.h
clip.o: /usr/include/sys/_types/_u_int.h /usr/include/sys/_types/_caddr_t.h
clip.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_blkcnt_t.h
clip.o: /usr/include/sys/_types/_blksize_t.h /usr/include/sys/_types/_gid_t.h
clip.o: /usr/include/sys/_types/_in_addr_t.h
clip.o: /usr/include/sys/_types/_in_port_t.h /usr/include/sys/_types/_ino_t.h
clip.o: /usr/include/sys/_types/_ino64_t.h /usr/include/sys/_types/_key_t.h
clip.o: /usr/include/sys/_types/_mode_t.h /usr/include/sys/_types/_nlink_t.h
clip.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
clip.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/_types/_clock_t.h
clip.o: /usr/include/sys/_types/_time_t.h
clip.o: /usr/include/sys/_types/_useconds_t.h
clip.o: /usr/include/sys/_types/_suseconds_t.h
clip.o: /usr/include/sys/_types/_rsize_t.h /usr/include/sys/_types/_errno_t.h
clip.o: /usr/include/sys/_types/_fd_def.h
clip.o: /usr/include/sys/_types/_fd_setsize.h
clip.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
clip.o: /usr/include/sys/_types/_fd_zero.h
clip.o: /usr/include/sys/_types/_fd_isset.h
clip.o: /usr/include/sys/_types/_fd_copy.h
clip.o: /usr/include/sys/_pthread/_pthread_attr_t.h
clip.o: /usr/include/sys/_pthread/_pthread_cond_t.h
clip.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
clip.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
clip.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
clip.o: /usr/include/sys/_pthread/_pthread_once_t.h
clip.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
clip.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
clip.o: /usr/include/sys/_pthread/_pthread_t.h
clip.o: /usr/include/sys/_pthread/_pthread_key_t.h
clip.o: /usr/include/sys/_types/_fsblkcnt_t.h
clip.o: /usr/include/sys/_types/_fsfilcnt_t.h default.h
clip.o: ../../../.././include/ncarg/cgmdef.h cgmc.h text.h translate.h
clip.o: /usr/include/stdlib.h /usr/include/sys/wait.h
clip.o: /usr/include/sys/signal.h /usr/include/machine/signal.h
clip.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
clip.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
clip.o: /usr/include/mach/i386/_structs.h
clip.o: /usr/include/sys/_types/_sigaltstack.h
clip.o: /usr/include/sys/_types/_ucontext.h
clip.o: /usr/include/sys/_types/_sigset_t.h /usr/include/sys/resource.h
clip.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
clip.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
clip.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
clip.o: /usr/include/_types/_uintmax_t.h /usr/include/sys/_types/_timeval.h
clip.o: /usr/include/alloca.h /usr/include/sys/_types/_ct_rune_t.h
clip.o: /usr/include/sys/_types/_rune_t.h /usr/include/sys/_types/_wchar_t.h
clip.o: ctrandef.h
ctrans.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
ctrans.o: /usr/include/sys/_symbol_aliasing.h
ctrans.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
ctrans.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
ctrans.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
ctrans.o: /usr/include/i386/_types.h
ctrans.o: /usr/include/sys/_pthread/_pthread_types.h
ctrans.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
ctrans.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
ctrans.o: /usr/include/sys/_types/_int16_t.h
ctrans.o: /usr/include/sys/_types/_int32_t.h
ctrans.o: /usr/include/sys/_types/_int64_t.h
ctrans.o: /usr/include/sys/_types/_u_int8_t.h
ctrans.o: /usr/include/sys/_types/_u_int16_t.h
ctrans.o: /usr/include/sys/_types/_u_int32_t.h
ctrans.o: /usr/include/sys/_types/_u_int64_t.h
ctrans.o: /usr/include/sys/_types/_intptr_t.h
ctrans.o: /usr/include/sys/_types/_uintptr_t.h
ctrans.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
ctrans.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
ctrans.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
ctrans.o: /usr/include/secure/_common.h /usr/include/stdlib.h
ctrans.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
ctrans.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
ctrans.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
ctrans.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
ctrans.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
ctrans.o: /usr/include/mach/i386/_structs.h
ctrans.o: /usr/include/sys/_pthread/_pthread_attr_t.h
ctrans.o: /usr/include/sys/_types/_sigaltstack.h
ctrans.o: /usr/include/sys/_types/_ucontext.h
ctrans.o: /usr/include/sys/_types/_sigset_t.h
ctrans.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
ctrans.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
ctrans.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
ctrans.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
ctrans.o: /usr/include/_types/_uintmax_t.h /usr/include/sys/_types/_timeval.h
ctrans.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
ctrans.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
ctrans.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
ctrans.o: /usr/include/sys/_types/_ct_rune_t.h
ctrans.o: /usr/include/sys/_types/_rune_t.h
ctrans.o: /usr/include/sys/_types/_wchar_t.h /usr/include/sys/_types/_dev_t.h
ctrans.o: /usr/include/sys/_types/_mode_t.h /usr/include/string.h
ctrans.o: /usr/include/sys/_types/_rsize_t.h
ctrans.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
ctrans.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
ctrans.o: /usr/include/ctype.h /usr/include/runetype.h
ctrans.o: /usr/include/sys/_types/_wint_t.h /usr/include/errno.h
ctrans.o: /usr/include/sys/errno.h ../../../.././include/ncarg/c.h
ctrans.o: /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
ctrans.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
ctrans.o: /usr/include/sys/_types/_caddr_t.h
ctrans.o: /usr/include/sys/_types/_blkcnt_t.h
ctrans.o: /usr/include/sys/_types/_blksize_t.h
ctrans.o: /usr/include/sys/_types/_gid_t.h
ctrans.o: /usr/include/sys/_types/_in_addr_t.h
ctrans.o: /usr/include/sys/_types/_in_port_t.h
ctrans.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
ctrans.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_nlink_t.h
ctrans.o: /usr/include/sys/_types/_clock_t.h
ctrans.o: /usr/include/sys/_types/_time_t.h
ctrans.o: /usr/include/sys/_types/_useconds_t.h
ctrans.o: /usr/include/sys/_types/_suseconds_t.h
ctrans.o: /usr/include/sys/_types/_fd_def.h
ctrans.o: /usr/include/sys/_types/_fd_setsize.h
ctrans.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
ctrans.o: /usr/include/sys/_types/_fd_zero.h
ctrans.o: /usr/include/sys/_types/_fd_isset.h
ctrans.o: /usr/include/sys/_types/_fd_copy.h
ctrans.o: /usr/include/sys/_pthread/_pthread_cond_t.h
ctrans.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
ctrans.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
ctrans.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
ctrans.o: /usr/include/sys/_pthread/_pthread_once_t.h
ctrans.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
ctrans.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
ctrans.o: /usr/include/sys/_pthread/_pthread_t.h
ctrans.o: /usr/include/sys/_pthread/_pthread_key_t.h
ctrans.o: /usr/include/sys/_types/_fsblkcnt_t.h
ctrans.o: /usr/include/sys/_types/_fsfilcnt_t.h
ctrans.o: ../../../.././include/ncarg/cgm_tools.h
ctrans.o: ../../../.././include/ncarg/cgmdef.h ctrans.h cgmc.h defines.h
ctrans.o: translate.h devices.h ctrandef.h graphcap.h fontlist.h
default.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
default.o: /usr/include/sys/_symbol_aliasing.h
default.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
default.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
default.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
default.o: /usr/include/i386/_types.h
default.o: /usr/include/sys/_pthread/_pthread_types.h
default.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
default.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
default.o: /usr/include/sys/_types/_int16_t.h
default.o: /usr/include/sys/_types/_int32_t.h
default.o: /usr/include/sys/_types/_int64_t.h
default.o: /usr/include/sys/_types/_u_int8_t.h
default.o: /usr/include/sys/_types/_u_int16_t.h
default.o: /usr/include/sys/_types/_u_int32_t.h
default.o: /usr/include/sys/_types/_u_int64_t.h
default.o: /usr/include/sys/_types/_intptr_t.h
default.o: /usr/include/sys/_types/_uintptr_t.h
default.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
default.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
default.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
default.o: /usr/include/secure/_common.h /usr/include/stdlib.h
default.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
default.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
default.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
default.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
default.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
default.o: /usr/include/mach/i386/_structs.h
default.o: /usr/include/sys/_pthread/_pthread_attr_t.h
default.o: /usr/include/sys/_types/_sigaltstack.h
default.o: /usr/include/sys/_types/_ucontext.h
default.o: /usr/include/sys/_types/_sigset_t.h
default.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
default.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
default.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
default.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
default.o: /usr/include/_types/_uintmax_t.h
default.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
default.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
default.o: /usr/include/libkern/_OSByteOrder.h
default.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
default.o: /usr/include/sys/_types/_ct_rune_t.h
default.o: /usr/include/sys/_types/_rune_t.h
default.o: /usr/include/sys/_types/_wchar_t.h
default.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_mode_t.h
default.o: /usr/include/errno.h /usr/include/sys/errno.h
default.o: /usr/include/sys/_types/_errno_t.h ../../../.././include/ncarg/c.h
default.o: /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
default.o: /usr/include/sys/_types/_u_short.h
default.o: /usr/include/sys/_types/_u_int.h
default.o: /usr/include/sys/_types/_caddr_t.h
default.o: /usr/include/sys/_types/_blkcnt_t.h
default.o: /usr/include/sys/_types/_blksize_t.h
default.o: /usr/include/sys/_types/_gid_t.h
default.o: /usr/include/sys/_types/_in_addr_t.h
default.o: /usr/include/sys/_types/_in_port_t.h
default.o: /usr/include/sys/_types/_ino_t.h
default.o: /usr/include/sys/_types/_ino64_t.h
default.o: /usr/include/sys/_types/_key_t.h
default.o: /usr/include/sys/_types/_nlink_t.h
default.o: /usr/include/sys/_types/_clock_t.h
default.o: /usr/include/sys/_types/_time_t.h
default.o: /usr/include/sys/_types/_useconds_t.h
default.o: /usr/include/sys/_types/_suseconds_t.h
default.o: /usr/include/sys/_types/_rsize_t.h
default.o: /usr/include/sys/_types/_fd_def.h
default.o: /usr/include/sys/_types/_fd_setsize.h
default.o: /usr/include/sys/_types/_fd_set.h
default.o: /usr/include/sys/_types/_fd_clr.h
default.o: /usr/include/sys/_types/_fd_zero.h
default.o: /usr/include/sys/_types/_fd_isset.h
default.o: /usr/include/sys/_types/_fd_copy.h
default.o: /usr/include/sys/_pthread/_pthread_cond_t.h
default.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
default.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
default.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
default.o: /usr/include/sys/_pthread/_pthread_once_t.h
default.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
default.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
default.o: /usr/include/sys/_pthread/_pthread_t.h
default.o: /usr/include/sys/_pthread/_pthread_key_t.h
default.o: /usr/include/sys/_types/_fsblkcnt_t.h
default.o: /usr/include/sys/_types/_fsfilcnt_t.h cgmc.h default.h
default.o: ../../../.././include/ncarg/cgmdef.h text.h ctrandef.h
fill.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
fill.o: /usr/include/sys/_symbol_aliasing.h
fill.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
fill.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
fill.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
fill.o: /usr/include/i386/_types.h /usr/include/sys/_pthread/_pthread_types.h
fill.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
fill.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
fill.o: /usr/include/sys/_types/_int16_t.h /usr/include/sys/_types/_int32_t.h
fill.o: /usr/include/sys/_types/_int64_t.h
fill.o: /usr/include/sys/_types/_u_int8_t.h
fill.o: /usr/include/sys/_types/_u_int16_t.h
fill.o: /usr/include/sys/_types/_u_int32_t.h
fill.o: /usr/include/sys/_types/_u_int64_t.h
fill.o: /usr/include/sys/_types/_intptr_t.h
fill.o: /usr/include/sys/_types/_uintptr_t.h
fill.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
fill.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
fill.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
fill.o: /usr/include/secure/_common.h /usr/include/stdlib.h
fill.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
fill.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
fill.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
fill.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
fill.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
fill.o: /usr/include/mach/i386/_structs.h
fill.o: /usr/include/sys/_pthread/_pthread_attr_t.h
fill.o: /usr/include/sys/_types/_sigaltstack.h
fill.o: /usr/include/sys/_types/_ucontext.h
fill.o: /usr/include/sys/_types/_sigset_t.h /usr/include/sys/_types/_uid_t.h
fill.o: /usr/include/sys/resource.h /usr/include/stdint.h
fill.o: /usr/include/_types/_uint8_t.h /usr/include/_types/_uint16_t.h
fill.o: /usr/include/_types/_uint32_t.h /usr/include/_types/_uint64_t.h
fill.o: /usr/include/_types/_intmax_t.h /usr/include/_types/_uintmax_t.h
fill.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
fill.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
fill.o: /usr/include/libkern/_OSByteOrder.h
fill.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
fill.o: /usr/include/sys/_types/_ct_rune_t.h
fill.o: /usr/include/sys/_types/_rune_t.h /usr/include/sys/_types/_wchar_t.h
fill.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_mode_t.h
fill.o: /usr/include/ctype.h /usr/include/runetype.h
fill.o: /usr/include/sys/_types/_wint_t.h /usr/include/errno.h
fill.o: /usr/include/sys/errno.h /usr/include/sys/_types/_errno_t.h
fill.o: ../../../.././include/ncarg/c.h /usr/include/sys/types.h
fill.o: /usr/include/sys/_types/_u_char.h /usr/include/sys/_types/_u_short.h
fill.o: /usr/include/sys/_types/_u_int.h /usr/include/sys/_types/_caddr_t.h
fill.o: /usr/include/sys/_types/_blkcnt_t.h
fill.o: /usr/include/sys/_types/_blksize_t.h /usr/include/sys/_types/_gid_t.h
fill.o: /usr/include/sys/_types/_in_addr_t.h
fill.o: /usr/include/sys/_types/_in_port_t.h /usr/include/sys/_types/_ino_t.h
fill.o: /usr/include/sys/_types/_ino64_t.h /usr/include/sys/_types/_key_t.h
fill.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_clock_t.h
fill.o: /usr/include/sys/_types/_time_t.h
fill.o: /usr/include/sys/_types/_useconds_t.h
fill.o: /usr/include/sys/_types/_suseconds_t.h
fill.o: /usr/include/sys/_types/_rsize_t.h /usr/include/sys/_types/_fd_def.h
fill.o: /usr/include/sys/_types/_fd_setsize.h
fill.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
fill.o: /usr/include/sys/_types/_fd_zero.h
fill.o: /usr/include/sys/_types/_fd_isset.h
fill.o: /usr/include/sys/_types/_fd_copy.h
fill.o: /usr/include/sys/_pthread/_pthread_cond_t.h
fill.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
fill.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
fill.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
fill.o: /usr/include/sys/_pthread/_pthread_once_t.h
fill.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
fill.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
fill.o: /usr/include/sys/_pthread/_pthread_t.h
fill.o: /usr/include/sys/_pthread/_pthread_key_t.h
fill.o: /usr/include/sys/_types/_fsblkcnt_t.h
fill.o: /usr/include/sys/_types/_fsfilcnt_t.h
fill.o: ../../../.././include/ncarg/cgm_tools.h
fill.o: ../../../.././include/ncarg/cgmdef.h fill.h cgmc.h in.h default.h
fill.o: text.h ctrandef.h bitops.h
fontlist.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
fontlist.o: /usr/include/sys/_symbol_aliasing.h
fontlist.o: /usr/include/sys/_posix_availability.h
fontlist.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
fontlist.o: /usr/include/_types.h /usr/include/sys/_types.h
fontlist.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
fontlist.o: /usr/include/sys/_pthread/_pthread_types.h
fontlist.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
fontlist.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
fontlist.o: /usr/include/sys/_types/_int16_t.h
fontlist.o: /usr/include/sys/_types/_int32_t.h
fontlist.o: /usr/include/sys/_types/_int64_t.h
fontlist.o: /usr/include/sys/_types/_u_int8_t.h
fontlist.o: /usr/include/sys/_types/_u_int16_t.h
fontlist.o: /usr/include/sys/_types/_u_int32_t.h
fontlist.o: /usr/include/sys/_types/_u_int64_t.h
fontlist.o: /usr/include/sys/_types/_intptr_t.h
fontlist.o: /usr/include/sys/_types/_uintptr_t.h
fontlist.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
fontlist.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
fontlist.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
fontlist.o: /usr/include/secure/_common.h /usr/include/string.h
fontlist.o: /usr/include/sys/_types/_rsize_t.h
fontlist.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
fontlist.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
fontlist.o: /usr/include/stdlib.h /usr/include/sys/wait.h
fontlist.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_id_t.h
fontlist.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
fontlist.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
fontlist.o: /usr/include/machine/_mcontext.h /usr/include/i386/_mcontext.h
fontlist.o: /usr/include/mach/machine/_structs.h
fontlist.o: /usr/include/mach/i386/_structs.h
fontlist.o: /usr/include/sys/_pthread/_pthread_attr_t.h
fontlist.o: /usr/include/sys/_types/_sigaltstack.h
fontlist.o: /usr/include/sys/_types/_ucontext.h
fontlist.o: /usr/include/sys/_types/_sigset_t.h
fontlist.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
fontlist.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
fontlist.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
fontlist.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
fontlist.o: /usr/include/_types/_uintmax_t.h
fontlist.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
fontlist.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
fontlist.o: /usr/include/libkern/_OSByteOrder.h
fontlist.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
fontlist.o: /usr/include/sys/_types/_ct_rune_t.h
fontlist.o: /usr/include/sys/_types/_rune_t.h
fontlist.o: /usr/include/sys/_types/_wchar_t.h
fontlist.o: /usr/include/sys/_types/_dev_t.h
fontlist.o: /usr/include/sys/_types/_mode_t.h /usr/include/errno.h
fontlist.o: /usr/include/sys/errno.h ../../../.././include/ncarg/c.h
fontlist.o: /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
fontlist.o: /usr/include/sys/_types/_u_short.h
fontlist.o: /usr/include/sys/_types/_u_int.h
fontlist.o: /usr/include/sys/_types/_caddr_t.h
fontlist.o: /usr/include/sys/_types/_blkcnt_t.h
fontlist.o: /usr/include/sys/_types/_blksize_t.h
fontlist.o: /usr/include/sys/_types/_gid_t.h
fontlist.o: /usr/include/sys/_types/_in_addr_t.h
fontlist.o: /usr/include/sys/_types/_in_port_t.h
fontlist.o: /usr/include/sys/_types/_ino_t.h
fontlist.o: /usr/include/sys/_types/_ino64_t.h
fontlist.o: /usr/include/sys/_types/_key_t.h
fontlist.o: /usr/include/sys/_types/_nlink_t.h
fontlist.o: /usr/include/sys/_types/_clock_t.h
fontlist.o: /usr/include/sys/_types/_time_t.h
fontlist.o: /usr/include/sys/_types/_useconds_t.h
fontlist.o: /usr/include/sys/_types/_suseconds_t.h
fontlist.o: /usr/include/sys/_types/_fd_def.h
fontlist.o: /usr/include/sys/_types/_fd_setsize.h
fontlist.o: /usr/include/sys/_types/_fd_set.h
fontlist.o: /usr/include/sys/_types/_fd_clr.h
fontlist.o: /usr/include/sys/_types/_fd_zero.h
fontlist.o: /usr/include/sys/_types/_fd_isset.h
fontlist.o: /usr/include/sys/_types/_fd_copy.h
fontlist.o: /usr/include/sys/_pthread/_pthread_cond_t.h
fontlist.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
fontlist.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
fontlist.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
fontlist.o: /usr/include/sys/_pthread/_pthread_once_t.h
fontlist.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
fontlist.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
fontlist.o: /usr/include/sys/_pthread/_pthread_t.h
fontlist.o: /usr/include/sys/_pthread/_pthread_key_t.h
fontlist.o: /usr/include/sys/_types/_fsblkcnt_t.h
fontlist.o: /usr/include/sys/_types/_fsfilcnt_t.h default.h
fontlist.o: ../../../.././include/ncarg/cgmdef.h cgmc.h text.h defines.h
fontlist.o: ctrans.h ../../../.././include/ncarg/cgm_tools.h fontlist.h
format.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
format.o: /usr/include/sys/_symbol_aliasing.h
format.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
format.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
format.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
format.o: /usr/include/i386/_types.h
format.o: /usr/include/sys/_pthread/_pthread_types.h
format.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
format.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
format.o: /usr/include/sys/_types/_int16_t.h
format.o: /usr/include/sys/_types/_int32_t.h
format.o: /usr/include/sys/_types/_int64_t.h
format.o: /usr/include/sys/_types/_u_int8_t.h
format.o: /usr/include/sys/_types/_u_int16_t.h
format.o: /usr/include/sys/_types/_u_int32_t.h
format.o: /usr/include/sys/_types/_u_int64_t.h
format.o: /usr/include/sys/_types/_intptr_t.h
format.o: /usr/include/sys/_types/_uintptr_t.h
format.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
format.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
format.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
format.o: /usr/include/secure/_common.h /usr/include/stdlib.h
format.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
format.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
format.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
format.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
format.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
format.o: /usr/include/mach/i386/_structs.h
format.o: /usr/include/sys/_pthread/_pthread_attr_t.h
format.o: /usr/include/sys/_types/_sigaltstack.h
format.o: /usr/include/sys/_types/_ucontext.h
format.o: /usr/include/sys/_types/_sigset_t.h
format.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
format.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
format.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
format.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
format.o: /usr/include/_types/_uintmax_t.h /usr/include/sys/_types/_timeval.h
format.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
format.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
format.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
format.o: /usr/include/sys/_types/_ct_rune_t.h
format.o: /usr/include/sys/_types/_rune_t.h
format.o: /usr/include/sys/_types/_wchar_t.h /usr/include/sys/_types/_dev_t.h
format.o: /usr/include/sys/_types/_mode_t.h ../../../.././include/ncarg/c.h
format.o: /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
format.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
format.o: /usr/include/sys/_types/_caddr_t.h
format.o: /usr/include/sys/_types/_blkcnt_t.h
format.o: /usr/include/sys/_types/_blksize_t.h
format.o: /usr/include/sys/_types/_gid_t.h
format.o: /usr/include/sys/_types/_in_addr_t.h
format.o: /usr/include/sys/_types/_in_port_t.h
format.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
format.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_nlink_t.h
format.o: /usr/include/sys/_types/_clock_t.h
format.o: /usr/include/sys/_types/_time_t.h
format.o: /usr/include/sys/_types/_useconds_t.h
format.o: /usr/include/sys/_types/_suseconds_t.h
format.o: /usr/include/sys/_types/_rsize_t.h
format.o: /usr/include/sys/_types/_errno_t.h
format.o: /usr/include/sys/_types/_fd_def.h
format.o: /usr/include/sys/_types/_fd_setsize.h
format.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
format.o: /usr/include/sys/_types/_fd_zero.h
format.o: /usr/include/sys/_types/_fd_isset.h
format.o: /usr/include/sys/_types/_fd_copy.h
format.o: /usr/include/sys/_pthread/_pthread_cond_t.h
format.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
format.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
format.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
format.o: /usr/include/sys/_pthread/_pthread_once_t.h
format.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
format.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
format.o: /usr/include/sys/_pthread/_pthread_t.h
format.o: /usr/include/sys/_pthread/_pthread_key_t.h
format.o: /usr/include/sys/_types/_fsblkcnt_t.h
format.o: /usr/include/sys/_types/_fsfilcnt_t.h cgmc.h defines.h graphcap.h
format.o: bitops.h format.h
gcap.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
gcap.o: /usr/include/sys/_symbol_aliasing.h
gcap.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
gcap.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
gcap.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
gcap.o: /usr/include/i386/_types.h /usr/include/sys/_pthread/_pthread_types.h
gcap.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
gcap.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
gcap.o: /usr/include/sys/_types/_int16_t.h /usr/include/sys/_types/_int32_t.h
gcap.o: /usr/include/sys/_types/_int64_t.h
gcap.o: /usr/include/sys/_types/_u_int8_t.h
gcap.o: /usr/include/sys/_types/_u_int16_t.h
gcap.o: /usr/include/sys/_types/_u_int32_t.h
gcap.o: /usr/include/sys/_types/_u_int64_t.h
gcap.o: /usr/include/sys/_types/_intptr_t.h
gcap.o: /usr/include/sys/_types/_uintptr_t.h
gcap.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
gcap.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
gcap.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
gcap.o: /usr/include/secure/_common.h /usr/include/math.h
gcap.o: /usr/include/errno.h /usr/include/sys/errno.h
gcap.o: /usr/include/sys/_types/_errno_t.h /usr/include/sys/types.h
gcap.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
gcap.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
gcap.o: /usr/include/libkern/_OSByteOrder.h
gcap.o: /usr/include/libkern/i386/_OSByteOrder.h
gcap.o: /usr/include/sys/_types/_u_char.h /usr/include/sys/_types/_u_short.h
gcap.o: /usr/include/sys/_types/_u_int.h /usr/include/sys/_types/_caddr_t.h
gcap.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_blkcnt_t.h
gcap.o: /usr/include/sys/_types/_blksize_t.h /usr/include/sys/_types/_gid_t.h
gcap.o: /usr/include/sys/_types/_in_addr_t.h
gcap.o: /usr/include/sys/_types/_in_port_t.h /usr/include/sys/_types/_ino_t.h
gcap.o: /usr/include/sys/_types/_ino64_t.h /usr/include/sys/_types/_key_t.h
gcap.o: /usr/include/sys/_types/_mode_t.h /usr/include/sys/_types/_nlink_t.h
gcap.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
gcap.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/_types/_clock_t.h
gcap.o: /usr/include/sys/_types/_time_t.h
gcap.o: /usr/include/sys/_types/_useconds_t.h
gcap.o: /usr/include/sys/_types/_suseconds_t.h
gcap.o: /usr/include/sys/_types/_rsize_t.h /usr/include/sys/_types/_fd_def.h
gcap.o: /usr/include/sys/_types/_fd_setsize.h
gcap.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
gcap.o: /usr/include/sys/_types/_fd_zero.h
gcap.o: /usr/include/sys/_types/_fd_isset.h
gcap.o: /usr/include/sys/_types/_fd_copy.h
gcap.o: /usr/include/sys/_pthread/_pthread_attr_t.h
gcap.o: /usr/include/sys/_pthread/_pthread_cond_t.h
gcap.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
gcap.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
gcap.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
gcap.o: /usr/include/sys/_pthread/_pthread_once_t.h
gcap.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
gcap.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
gcap.o: /usr/include/sys/_pthread/_pthread_t.h
gcap.o: /usr/include/sys/_pthread/_pthread_key_t.h
gcap.o: /usr/include/sys/_types/_fsblkcnt_t.h
gcap.o: /usr/include/sys/_types/_fsfilcnt_t.h /usr/include/unistd.h
gcap.o: /usr/include/sys/unistd.h /usr/include/sys/_types/_posix_vdisable.h
gcap.o: /usr/include/sys/_types/_seek_set.h /usr/include/_types/_uint64_t.h
gcap.o: /usr/include/_types/_uint32_t.h /usr/include/sys/select.h
gcap.o: /usr/include/sys/_types/_timespec.h
gcap.o: /usr/include/sys/_types/_timeval.h
gcap.o: /usr/include/sys/_types/_sigset_t.h /usr/include/sys/_select.h
gcap.o: /usr/include/sys/_types/_uuid_t.h /usr/include/gethostuuid.h
gcap.o: ../../../.././include/ncarg/c.h cgmc.h graphcap.h default.h
gcap.o: ../../../.././include/ncarg/cgmdef.h text.h ctrandef.h soft_fill.h
gcap.o: translate.h /usr/include/stdlib.h /usr/include/sys/wait.h
gcap.o: /usr/include/sys/signal.h /usr/include/machine/signal.h
gcap.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
gcap.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
gcap.o: /usr/include/mach/i386/_structs.h
gcap.o: /usr/include/sys/_types/_sigaltstack.h
gcap.o: /usr/include/sys/_types/_ucontext.h /usr/include/sys/resource.h
gcap.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
gcap.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_intmax_t.h
gcap.o: /usr/include/_types/_uintmax_t.h /usr/include/alloca.h
gcap.o: /usr/include/sys/_types/_ct_rune_t.h
gcap.o: /usr/include/sys/_types/_rune_t.h /usr/include/sys/_types/_wchar_t.h
gcap.o: format.h gcapdev.h
gcread.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
gcread.o: /usr/include/sys/_symbol_aliasing.h
gcread.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
gcread.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
gcread.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
gcread.o: /usr/include/i386/_types.h
gcread.o: /usr/include/sys/_pthread/_pthread_types.h
gcread.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
gcread.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
gcread.o: /usr/include/sys/_types/_int16_t.h
gcread.o: /usr/include/sys/_types/_int32_t.h
gcread.o: /usr/include/sys/_types/_int64_t.h
gcread.o: /usr/include/sys/_types/_u_int8_t.h
gcread.o: /usr/include/sys/_types/_u_int16_t.h
gcread.o: /usr/include/sys/_types/_u_int32_t.h
gcread.o: /usr/include/sys/_types/_u_int64_t.h
gcread.o: /usr/include/sys/_types/_intptr_t.h
gcread.o: /usr/include/sys/_types/_uintptr_t.h
gcread.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
gcread.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
gcread.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
gcread.o: /usr/include/secure/_common.h /usr/include/fcntl.h
gcread.o: /usr/include/sys/fcntl.h /usr/include/sys/_types/_mode_t.h
gcread.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_o_sync.h
gcread.o: /usr/include/sys/_types/_o_dsync.h
gcread.o: /usr/include/sys/_types/_seek_set.h
gcread.o: /usr/include/sys/_types/_s_ifmt.h
gcread.o: /usr/include/sys/_types/_timespec.h
gcread.o: /usr/include/sys/_types/_filesec_t.h /usr/include/errno.h
gcread.o: /usr/include/sys/errno.h /usr/include/sys/_types/_errno_t.h
gcread.o: graphcap.h ctrandef.h
in.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
in.o: /usr/include/sys/_symbol_aliasing.h
in.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
in.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
in.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
in.o: /usr/include/i386/_types.h /usr/include/sys/_pthread/_pthread_types.h
in.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
in.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
in.o: /usr/include/sys/_types/_int16_t.h /usr/include/sys/_types/_int32_t.h
in.o: /usr/include/sys/_types/_int64_t.h /usr/include/sys/_types/_u_int8_t.h
in.o: /usr/include/sys/_types/_u_int16_t.h
in.o: /usr/include/sys/_types/_u_int32_t.h
in.o: /usr/include/sys/_types/_u_int64_t.h
in.o: /usr/include/sys/_types/_intptr_t.h
in.o: /usr/include/sys/_types/_uintptr_t.h /usr/include/sys/_types/_size_t.h
in.o: /usr/include/sys/_types/_null.h /usr/include/sys/stdio.h
in.o: /usr/include/sys/_types/_off_t.h /usr/include/sys/_types/_ssize_t.h
in.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
in.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
in.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
in.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
in.o: /usr/include/libkern/i386/_OSByteOrder.h
in.o: /usr/include/sys/_types/_u_char.h /usr/include/sys/_types/_u_short.h
in.o: /usr/include/sys/_types/_u_int.h /usr/include/sys/_types/_caddr_t.h
in.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_blkcnt_t.h
in.o: /usr/include/sys/_types/_blksize_t.h /usr/include/sys/_types/_gid_t.h
in.o: /usr/include/sys/_types/_in_addr_t.h
in.o: /usr/include/sys/_types/_in_port_t.h /usr/include/sys/_types/_ino_t.h
in.o: /usr/include/sys/_types/_ino64_t.h /usr/include/sys/_types/_key_t.h
in.o: /usr/include/sys/_types/_mode_t.h /usr/include/sys/_types/_nlink_t.h
in.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
in.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/_types/_clock_t.h
in.o: /usr/include/sys/_types/_time_t.h /usr/include/sys/_types/_useconds_t.h
in.o: /usr/include/sys/_types/_suseconds_t.h
in.o: /usr/include/sys/_types/_rsize_t.h /usr/include/sys/_types/_errno_t.h
in.o: /usr/include/sys/_types/_fd_def.h /usr/include/sys/_types/_fd_setsize.h
in.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
in.o: /usr/include/sys/_types/_fd_zero.h /usr/include/sys/_types/_fd_isset.h
in.o: /usr/include/sys/_types/_fd_copy.h
in.o: /usr/include/sys/_pthread/_pthread_attr_t.h
in.o: /usr/include/sys/_pthread/_pthread_cond_t.h
in.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
in.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
in.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
in.o: /usr/include/sys/_pthread/_pthread_once_t.h
in.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
in.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
in.o: /usr/include/sys/_pthread/_pthread_t.h
in.o: /usr/include/sys/_pthread/_pthread_key_t.h
in.o: /usr/include/sys/_types/_fsblkcnt_t.h
in.o: /usr/include/sys/_types/_fsfilcnt_t.h /usr/include/sys/file.h
in.o: /usr/include/sys/fcntl.h /usr/include/sys/_types/_o_sync.h
in.o: /usr/include/sys/_types/_o_dsync.h /usr/include/sys/_types/_seek_set.h
in.o: /usr/include/sys/_types/_s_ifmt.h /usr/include/sys/_types/_timespec.h
in.o: /usr/include/sys/_types/_filesec_t.h /usr/include/sys/unistd.h
in.o: /usr/include/sys/_types/_posix_vdisable.h
in.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_uint32_t.h
in.o: /usr/include/sys/queue.h /usr/include/errno.h /usr/include/sys/errno.h
in.o: ../../../.././include/ncarg/cgm_tools.h ../../../.././include/ncarg/c.h
in.o: ../../../.././include/ncarg/cgmdef.h in.h cgmc.h bitops.h fill.h
rast.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
rast.o: /usr/include/sys/_symbol_aliasing.h
rast.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
rast.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
rast.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
rast.o: /usr/include/i386/_types.h /usr/include/sys/_pthread/_pthread_types.h
rast.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
rast.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
rast.o: /usr/include/sys/_types/_int16_t.h /usr/include/sys/_types/_int32_t.h
rast.o: /usr/include/sys/_types/_int64_t.h
rast.o: /usr/include/sys/_types/_u_int8_t.h
rast.o: /usr/include/sys/_types/_u_int16_t.h
rast.o: /usr/include/sys/_types/_u_int32_t.h
rast.o: /usr/include/sys/_types/_u_int64_t.h
rast.o: /usr/include/sys/_types/_intptr_t.h
rast.o: /usr/include/sys/_types/_uintptr_t.h
rast.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
rast.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
rast.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
rast.o: /usr/include/secure/_common.h /usr/include/stdlib.h
rast.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
rast.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
rast.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
rast.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
rast.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
rast.o: /usr/include/mach/i386/_structs.h
rast.o: /usr/include/sys/_pthread/_pthread_attr_t.h
rast.o: /usr/include/sys/_types/_sigaltstack.h
rast.o: /usr/include/sys/_types/_ucontext.h
rast.o: /usr/include/sys/_types/_sigset_t.h /usr/include/sys/_types/_uid_t.h
rast.o: /usr/include/sys/resource.h /usr/include/stdint.h
rast.o: /usr/include/_types/_uint8_t.h /usr/include/_types/_uint16_t.h
rast.o: /usr/include/_types/_uint32_t.h /usr/include/_types/_uint64_t.h
rast.o: /usr/include/_types/_intmax_t.h /usr/include/_types/_uintmax_t.h
rast.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
rast.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
rast.o: /usr/include/libkern/_OSByteOrder.h
rast.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
rast.o: /usr/include/sys/_types/_ct_rune_t.h
rast.o: /usr/include/sys/_types/_rune_t.h /usr/include/sys/_types/_wchar_t.h
rast.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_mode_t.h
rast.o: /usr/include/string.h /usr/include/sys/_types/_rsize_t.h
rast.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
rast.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
rast.o: /usr/include/math.h /usr/include/sys/types.h
rast.o: /usr/include/sys/_types/_u_char.h /usr/include/sys/_types/_u_short.h
rast.o: /usr/include/sys/_types/_u_int.h /usr/include/sys/_types/_caddr_t.h
rast.o: /usr/include/sys/_types/_blkcnt_t.h
rast.o: /usr/include/sys/_types/_blksize_t.h /usr/include/sys/_types/_gid_t.h
rast.o: /usr/include/sys/_types/_in_addr_t.h
rast.o: /usr/include/sys/_types/_in_port_t.h /usr/include/sys/_types/_ino_t.h
rast.o: /usr/include/sys/_types/_ino64_t.h /usr/include/sys/_types/_key_t.h
rast.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_clock_t.h
rast.o: /usr/include/sys/_types/_time_t.h
rast.o: /usr/include/sys/_types/_useconds_t.h
rast.o: /usr/include/sys/_types/_suseconds_t.h
rast.o: /usr/include/sys/_types/_fd_def.h
rast.o: /usr/include/sys/_types/_fd_setsize.h
rast.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
rast.o: /usr/include/sys/_types/_fd_zero.h
rast.o: /usr/include/sys/_types/_fd_isset.h
rast.o: /usr/include/sys/_types/_fd_copy.h
rast.o: /usr/include/sys/_pthread/_pthread_cond_t.h
rast.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
rast.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
rast.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
rast.o: /usr/include/sys/_pthread/_pthread_once_t.h
rast.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
rast.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
rast.o: /usr/include/sys/_pthread/_pthread_t.h
rast.o: /usr/include/sys/_pthread/_pthread_key_t.h
rast.o: /usr/include/sys/_types/_fsblkcnt_t.h
rast.o: /usr/include/sys/_types/_fsfilcnt_t.h /usr/include/errno.h
rast.o: /usr/include/sys/errno.h ../../../.././include/ncarg/c.h
rast.o: ../../../.././include/ncarg/ncarg_ras.h /usr/include/sys/stat.h
rast.o: /usr/include/sys/_types/_timespec.h /usr/include/sys/_types/_s_ifmt.h
rast.o: /usr/include/sys/_types/_filesec_t.h cgmc.h rast.h translate.h
rast.o: ctrandef.h default.h ../../../.././include/ncarg/cgmdef.h text.h
rast.o: devices.h
readfont.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
readfont.o: /usr/include/sys/_symbol_aliasing.h
readfont.o: /usr/include/sys/_posix_availability.h
readfont.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
readfont.o: /usr/include/_types.h /usr/include/sys/_types.h
readfont.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
readfont.o: /usr/include/sys/_pthread/_pthread_types.h
readfont.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
readfont.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
readfont.o: /usr/include/sys/_types/_int16_t.h
readfont.o: /usr/include/sys/_types/_int32_t.h
readfont.o: /usr/include/sys/_types/_int64_t.h
readfont.o: /usr/include/sys/_types/_u_int8_t.h
readfont.o: /usr/include/sys/_types/_u_int16_t.h
readfont.o: /usr/include/sys/_types/_u_int32_t.h
readfont.o: /usr/include/sys/_types/_u_int64_t.h
readfont.o: /usr/include/sys/_types/_intptr_t.h
readfont.o: /usr/include/sys/_types/_uintptr_t.h
readfont.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
readfont.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
readfont.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
readfont.o: /usr/include/secure/_common.h /usr/include/stdlib.h
readfont.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
readfont.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
readfont.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
readfont.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
readfont.o: /usr/include/i386/_mcontext.h
readfont.o: /usr/include/mach/machine/_structs.h
readfont.o: /usr/include/mach/i386/_structs.h
readfont.o: /usr/include/sys/_pthread/_pthread_attr_t.h
readfont.o: /usr/include/sys/_types/_sigaltstack.h
readfont.o: /usr/include/sys/_types/_ucontext.h
readfont.o: /usr/include/sys/_types/_sigset_t.h
readfont.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
readfont.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
readfont.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
readfont.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
readfont.o: /usr/include/_types/_uintmax_t.h
readfont.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
readfont.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
readfont.o: /usr/include/libkern/_OSByteOrder.h
readfont.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
readfont.o: /usr/include/sys/_types/_ct_rune_t.h
readfont.o: /usr/include/sys/_types/_rune_t.h
readfont.o: /usr/include/sys/_types/_wchar_t.h
readfont.o: /usr/include/sys/_types/_dev_t.h
readfont.o: /usr/include/sys/_types/_mode_t.h /usr/include/errno.h
readfont.o: /usr/include/sys/errno.h /usr/include/sys/_types/_errno_t.h
readfont.o: ../../../.././include/ncarg/c.h /usr/include/sys/types.h
readfont.o: /usr/include/sys/_types/_u_char.h
readfont.o: /usr/include/sys/_types/_u_short.h
readfont.o: /usr/include/sys/_types/_u_int.h
readfont.o: /usr/include/sys/_types/_caddr_t.h
readfont.o: /usr/include/sys/_types/_blkcnt_t.h
readfont.o: /usr/include/sys/_types/_blksize_t.h
readfont.o: /usr/include/sys/_types/_gid_t.h
readfont.o: /usr/include/sys/_types/_in_addr_t.h
readfont.o: /usr/include/sys/_types/_in_port_t.h
readfont.o: /usr/include/sys/_types/_ino_t.h
readfont.o: /usr/include/sys/_types/_ino64_t.h
readfont.o: /usr/include/sys/_types/_key_t.h
readfont.o: /usr/include/sys/_types/_nlink_t.h
readfont.o: /usr/include/sys/_types/_clock_t.h
readfont.o: /usr/include/sys/_types/_time_t.h
readfont.o: /usr/include/sys/_types/_useconds_t.h
readfont.o: /usr/include/sys/_types/_suseconds_t.h
readfont.o: /usr/include/sys/_types/_rsize_t.h
readfont.o: /usr/include/sys/_types/_fd_def.h
readfont.o: /usr/include/sys/_types/_fd_setsize.h
readfont.o: /usr/include/sys/_types/_fd_set.h
readfont.o: /usr/include/sys/_types/_fd_clr.h
readfont.o: /usr/include/sys/_types/_fd_zero.h
readfont.o: /usr/include/sys/_types/_fd_isset.h
readfont.o: /usr/include/sys/_types/_fd_copy.h
readfont.o: /usr/include/sys/_pthread/_pthread_cond_t.h
readfont.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
readfont.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
readfont.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
readfont.o: /usr/include/sys/_pthread/_pthread_once_t.h
readfont.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
readfont.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
readfont.o: /usr/include/sys/_pthread/_pthread_t.h
readfont.o: /usr/include/sys/_pthread/_pthread_key_t.h
readfont.o: /usr/include/sys/_types/_fsblkcnt_t.h
readfont.o: /usr/include/sys/_types/_fsfilcnt_t.h readfont.h
text.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
text.o: /usr/include/sys/_symbol_aliasing.h
text.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
text.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
text.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
text.o: /usr/include/i386/_types.h /usr/include/sys/_pthread/_pthread_types.h
text.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
text.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
text.o: /usr/include/sys/_types/_int16_t.h /usr/include/sys/_types/_int32_t.h
text.o: /usr/include/sys/_types/_int64_t.h
text.o: /usr/include/sys/_types/_u_int8_t.h
text.o: /usr/include/sys/_types/_u_int16_t.h
text.o: /usr/include/sys/_types/_u_int32_t.h
text.o: /usr/include/sys/_types/_u_int64_t.h
text.o: /usr/include/sys/_types/_intptr_t.h
text.o: /usr/include/sys/_types/_uintptr_t.h
text.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
text.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
text.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
text.o: /usr/include/secure/_common.h /usr/include/stdlib.h
text.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
text.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
text.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
text.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
text.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
text.o: /usr/include/mach/i386/_structs.h
text.o: /usr/include/sys/_pthread/_pthread_attr_t.h
text.o: /usr/include/sys/_types/_sigaltstack.h
text.o: /usr/include/sys/_types/_ucontext.h
text.o: /usr/include/sys/_types/_sigset_t.h /usr/include/sys/_types/_uid_t.h
text.o: /usr/include/sys/resource.h /usr/include/stdint.h
text.o: /usr/include/_types/_uint8_t.h /usr/include/_types/_uint16_t.h
text.o: /usr/include/_types/_uint32_t.h /usr/include/_types/_uint64_t.h
text.o: /usr/include/_types/_intmax_t.h /usr/include/_types/_uintmax_t.h
text.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
text.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
text.o: /usr/include/libkern/_OSByteOrder.h
text.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
text.o: /usr/include/sys/_types/_ct_rune_t.h
text.o: /usr/include/sys/_types/_rune_t.h /usr/include/sys/_types/_wchar_t.h
text.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_mode_t.h
text.o: /usr/include/string.h /usr/include/sys/_types/_rsize_t.h
text.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
text.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
text.o: /usr/include/math.h /usr/include/errno.h /usr/include/sys/errno.h
text.o: ../../../.././include/ncarg/c.h /usr/include/sys/types.h
text.o: /usr/include/sys/_types/_u_char.h /usr/include/sys/_types/_u_short.h
text.o: /usr/include/sys/_types/_u_int.h /usr/include/sys/_types/_caddr_t.h
text.o: /usr/include/sys/_types/_blkcnt_t.h
text.o: /usr/include/sys/_types/_blksize_t.h /usr/include/sys/_types/_gid_t.h
text.o: /usr/include/sys/_types/_in_addr_t.h
text.o: /usr/include/sys/_types/_in_port_t.h /usr/include/sys/_types/_ino_t.h
text.o: /usr/include/sys/_types/_ino64_t.h /usr/include/sys/_types/_key_t.h
text.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_clock_t.h
text.o: /usr/include/sys/_types/_time_t.h
text.o: /usr/include/sys/_types/_useconds_t.h
text.o: /usr/include/sys/_types/_suseconds_t.h
text.o: /usr/include/sys/_types/_fd_def.h
text.o: /usr/include/sys/_types/_fd_setsize.h
text.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
text.o: /usr/include/sys/_types/_fd_zero.h
text.o: /usr/include/sys/_types/_fd_isset.h
text.o: /usr/include/sys/_types/_fd_copy.h
text.o: /usr/include/sys/_pthread/_pthread_cond_t.h
text.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
text.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
text.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
text.o: /usr/include/sys/_pthread/_pthread_once_t.h
text.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
text.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
text.o: /usr/include/sys/_pthread/_pthread_t.h
text.o: /usr/include/sys/_pthread/_pthread_key_t.h
text.o: /usr/include/sys/_types/_fsblkcnt_t.h
text.o: /usr/include/sys/_types/_fsfilcnt_t.h ctrans.h
text.o: ../../../.././include/ncarg/cgm_tools.h
text.o: ../../../.././include/ncarg/cgmdef.h cgmc.h ctrandef.h text.h
text.o: readfont.h default.h fontlist.h
translate.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
translate.o: /usr/include/sys/_symbol_aliasing.h
translate.o: /usr/include/sys/_posix_availability.h
translate.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
translate.o: /usr/include/_types.h /usr/include/sys/_types.h
translate.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
translate.o: /usr/include/sys/_pthread/_pthread_types.h
translate.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
translate.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
translate.o: /usr/include/sys/_types/_int16_t.h
translate.o: /usr/include/sys/_types/_int32_t.h
translate.o: /usr/include/sys/_types/_int64_t.h
translate.o: /usr/include/sys/_types/_u_int8_t.h
translate.o: /usr/include/sys/_types/_u_int16_t.h
translate.o: /usr/include/sys/_types/_u_int32_t.h
translate.o: /usr/include/sys/_types/_u_int64_t.h
translate.o: /usr/include/sys/_types/_intptr_t.h
translate.o: /usr/include/sys/_types/_uintptr_t.h
translate.o: /usr/include/sys/_types/_size_t.h
translate.o: /usr/include/sys/_types/_null.h /usr/include/sys/stdio.h
translate.o: /usr/include/sys/_types/_off_t.h
translate.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
translate.o: /usr/include/secure/_common.h translate.h /usr/include/stdlib.h
translate.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
translate.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
translate.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
translate.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
translate.o: /usr/include/i386/_mcontext.h
translate.o: /usr/include/mach/machine/_structs.h
translate.o: /usr/include/mach/i386/_structs.h
translate.o: /usr/include/sys/_pthread/_pthread_attr_t.h
translate.o: /usr/include/sys/_types/_sigaltstack.h
translate.o: /usr/include/sys/_types/_ucontext.h
translate.o: /usr/include/sys/_types/_sigset_t.h
translate.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
translate.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
translate.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
translate.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
translate.o: /usr/include/_types/_uintmax_t.h
translate.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
translate.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
translate.o: /usr/include/libkern/_OSByteOrder.h
translate.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
translate.o: /usr/include/sys/_types/_ct_rune_t.h
translate.o: /usr/include/sys/_types/_rune_t.h
translate.o: /usr/include/sys/_types/_wchar_t.h
translate.o: /usr/include/sys/_types/_dev_t.h
translate.o: /usr/include/sys/_types/_mode_t.h
translate.o: ../../../.././include/ncarg/c.h /usr/include/sys/types.h
translate.o: /usr/include/sys/_types/_u_char.h
translate.o: /usr/include/sys/_types/_u_short.h
translate.o: /usr/include/sys/_types/_u_int.h
translate.o: /usr/include/sys/_types/_caddr_t.h
translate.o: /usr/include/sys/_types/_blkcnt_t.h
translate.o: /usr/include/sys/_types/_blksize_t.h
translate.o: /usr/include/sys/_types/_gid_t.h
translate.o: /usr/include/sys/_types/_in_addr_t.h
translate.o: /usr/include/sys/_types/_in_port_t.h
translate.o: /usr/include/sys/_types/_ino_t.h
translate.o: /usr/include/sys/_types/_ino64_t.h
translate.o: /usr/include/sys/_types/_key_t.h
translate.o: /usr/include/sys/_types/_nlink_t.h
translate.o: /usr/include/sys/_types/_clock_t.h
translate.o: /usr/include/sys/_types/_time_t.h
translate.o: /usr/include/sys/_types/_useconds_t.h
translate.o: /usr/include/sys/_types/_suseconds_t.h
translate.o: /usr/include/sys/_types/_rsize_t.h
translate.o: /usr/include/sys/_types/_errno_t.h
translate.o: /usr/include/sys/_types/_fd_def.h
translate.o: /usr/include/sys/_types/_fd_setsize.h
translate.o: /usr/include/sys/_types/_fd_set.h
translate.o: /usr/include/sys/_types/_fd_clr.h
translate.o: /usr/include/sys/_types/_fd_zero.h
translate.o: /usr/include/sys/_types/_fd_isset.h
translate.o: /usr/include/sys/_types/_fd_copy.h
translate.o: /usr/include/sys/_pthread/_pthread_cond_t.h
translate.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
translate.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
translate.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
translate.o: /usr/include/sys/_pthread/_pthread_once_t.h
translate.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
translate.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
translate.o: /usr/include/sys/_pthread/_pthread_t.h
translate.o: /usr/include/sys/_pthread/_pthread_key_t.h
translate.o: /usr/include/sys/_types/_fsblkcnt_t.h
translate.o: /usr/include/sys/_types/_fsfilcnt_t.h default.h
translate.o: ../../../.././include/ncarg/cgmdef.h cgmc.h text.h
env.o: /usr/include/ctype.h /usr/include/sys/cdefs.h
env.o: /usr/include/sys/_symbol_aliasing.h
env.o: /usr/include/sys/_posix_availability.h /usr/include/runetype.h
env.o: /usr/include/_types.h /usr/include/sys/_types.h
env.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
env.o: /usr/include/sys/_pthread/_pthread_types.h
env.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_ct_rune_t.h
env.o: /usr/include/sys/_types/_rune_t.h /usr/include/sys/_types/_wchar_t.h
env.o: /usr/include/sys/_types/_wint_t.h /usr/include/stdio.h
env.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
env.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
env.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
env.o: /usr/include/sys/_types/_int16_t.h /usr/include/sys/_types/_int32_t.h
env.o: /usr/include/sys/_types/_int64_t.h /usr/include/sys/_types/_u_int8_t.h
env.o: /usr/include/sys/_types/_u_int16_t.h
env.o: /usr/include/sys/_types/_u_int32_t.h
env.o: /usr/include/sys/_types/_u_int64_t.h
env.o: /usr/include/sys/_types/_intptr_t.h
env.o: /usr/include/sys/_types/_uintptr_t.h /usr/include/sys/_types/_null.h
env.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
env.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
env.o: /usr/include/secure/_common.h /usr/include/stdlib.h
env.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
env.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
env.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
env.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
env.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
env.o: /usr/include/mach/i386/_structs.h
env.o: /usr/include/sys/_pthread/_pthread_attr_t.h
env.o: /usr/include/sys/_types/_sigaltstack.h
env.o: /usr/include/sys/_types/_ucontext.h
env.o: /usr/include/sys/_types/_sigset_t.h /usr/include/sys/_types/_uid_t.h
env.o: /usr/include/sys/resource.h /usr/include/stdint.h
env.o: /usr/include/_types/_uint8_t.h /usr/include/_types/_uint16_t.h
env.o: /usr/include/_types/_uint32_t.h /usr/include/_types/_uint64_t.h
env.o: /usr/include/_types/_intmax_t.h /usr/include/_types/_uintmax_t.h
env.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
env.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
env.o: /usr/include/libkern/_OSByteOrder.h
env.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
env.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_mode_t.h
env.o: /usr/include/string.h /usr/include/sys/_types/_rsize_t.h
env.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
env.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
env.o: /usr/include/errno.h /usr/include/sys/errno.h
env.o: ../../../.././include/ncarg/c.h /usr/include/sys/types.h
env.o: /usr/include/sys/_types/_u_char.h /usr/include/sys/_types/_u_short.h
env.o: /usr/include/sys/_types/_u_int.h /usr/include/sys/_types/_caddr_t.h
env.o: /usr/include/sys/_types/_blkcnt_t.h
env.o: /usr/include/sys/_types/_blksize_t.h /usr/include/sys/_types/_gid_t.h
env.o: /usr/include/sys/_types/_in_addr_t.h
env.o: /usr/include/sys/_types/_in_port_t.h /usr/include/sys/_types/_ino_t.h
env.o: /usr/include/sys/_types/_ino64_t.h /usr/include/sys/_types/_key_t.h
env.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_clock_t.h
env.o: /usr/include/sys/_types/_time_t.h
env.o: /usr/include/sys/_types/_useconds_t.h
env.o: /usr/include/sys/_types/_suseconds_t.h
env.o: /usr/include/sys/_types/_fd_def.h
env.o: /usr/include/sys/_types/_fd_setsize.h
env.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
env.o: /usr/include/sys/_types/_fd_zero.h /usr/include/sys/_types/_fd_isset.h
env.o: /usr/include/sys/_types/_fd_copy.h
env.o: /usr/include/sys/_pthread/_pthread_cond_t.h
env.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
env.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
env.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
env.o: /usr/include/sys/_pthread/_pthread_once_t.h
env.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
env.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
env.o: /usr/include/sys/_pthread/_pthread_t.h
env.o: /usr/include/sys/_pthread/_pthread_key_t.h
env.o: /usr/include/sys/_types/_fsblkcnt_t.h
env.o: /usr/include/sys/_types/_fsfilcnt_t.h
soft_fill.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
soft_fill.o: /usr/include/sys/_symbol_aliasing.h
soft_fill.o: /usr/include/sys/_posix_availability.h
soft_fill.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
soft_fill.o: /usr/include/_types.h /usr/include/sys/_types.h
soft_fill.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
soft_fill.o: /usr/include/sys/_pthread/_pthread_types.h
soft_fill.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
soft_fill.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
soft_fill.o: /usr/include/sys/_types/_int16_t.h
soft_fill.o: /usr/include/sys/_types/_int32_t.h
soft_fill.o: /usr/include/sys/_types/_int64_t.h
soft_fill.o: /usr/include/sys/_types/_u_int8_t.h
soft_fill.o: /usr/include/sys/_types/_u_int16_t.h
soft_fill.o: /usr/include/sys/_types/_u_int32_t.h
soft_fill.o: /usr/include/sys/_types/_u_int64_t.h
soft_fill.o: /usr/include/sys/_types/_intptr_t.h
soft_fill.o: /usr/include/sys/_types/_uintptr_t.h
soft_fill.o: /usr/include/sys/_types/_size_t.h
soft_fill.o: /usr/include/sys/_types/_null.h /usr/include/sys/stdio.h
soft_fill.o: /usr/include/sys/_types/_off_t.h
soft_fill.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
soft_fill.o: /usr/include/secure/_common.h /usr/include/stdlib.h
soft_fill.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
soft_fill.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
soft_fill.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
soft_fill.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
soft_fill.o: /usr/include/i386/_mcontext.h
soft_fill.o: /usr/include/mach/machine/_structs.h
soft_fill.o: /usr/include/mach/i386/_structs.h
soft_fill.o: /usr/include/sys/_pthread/_pthread_attr_t.h
soft_fill.o: /usr/include/sys/_types/_sigaltstack.h
soft_fill.o: /usr/include/sys/_types/_ucontext.h
soft_fill.o: /usr/include/sys/_types/_sigset_t.h
soft_fill.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
soft_fill.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
soft_fill.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
soft_fill.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
soft_fill.o: /usr/include/_types/_uintmax_t.h
soft_fill.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
soft_fill.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
soft_fill.o: /usr/include/libkern/_OSByteOrder.h
soft_fill.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
soft_fill.o: /usr/include/sys/_types/_ct_rune_t.h
soft_fill.o: /usr/include/sys/_types/_rune_t.h
soft_fill.o: /usr/include/sys/_types/_wchar_t.h
soft_fill.o: /usr/include/sys/_types/_dev_t.h
soft_fill.o: /usr/include/sys/_types/_mode_t.h /usr/include/errno.h
soft_fill.o: /usr/include/sys/errno.h /usr/include/sys/_types/_errno_t.h
soft_fill.o: ../../../.././include/ncarg/c.h /usr/include/sys/types.h
soft_fill.o: /usr/include/sys/_types/_u_char.h
soft_fill.o: /usr/include/sys/_types/_u_short.h
soft_fill.o: /usr/include/sys/_types/_u_int.h
soft_fill.o: /usr/include/sys/_types/_caddr_t.h
soft_fill.o: /usr/include/sys/_types/_blkcnt_t.h
soft_fill.o: /usr/include/sys/_types/_blksize_t.h
soft_fill.o: /usr/include/sys/_types/_gid_t.h
soft_fill.o: /usr/include/sys/_types/_in_addr_t.h
soft_fill.o: /usr/include/sys/_types/_in_port_t.h
soft_fill.o: /usr/include/sys/_types/_ino_t.h
soft_fill.o: /usr/include/sys/_types/_ino64_t.h
soft_fill.o: /usr/include/sys/_types/_key_t.h
soft_fill.o: /usr/include/sys/_types/_nlink_t.h
soft_fill.o: /usr/include/sys/_types/_clock_t.h
soft_fill.o: /usr/include/sys/_types/_time_t.h
soft_fill.o: /usr/include/sys/_types/_useconds_t.h
soft_fill.o: /usr/include/sys/_types/_suseconds_t.h
soft_fill.o: /usr/include/sys/_types/_rsize_t.h
soft_fill.o: /usr/include/sys/_types/_fd_def.h
soft_fill.o: /usr/include/sys/_types/_fd_setsize.h
soft_fill.o: /usr/include/sys/_types/_fd_set.h
soft_fill.o: /usr/include/sys/_types/_fd_clr.h
soft_fill.o: /usr/include/sys/_types/_fd_zero.h
soft_fill.o: /usr/include/sys/_types/_fd_isset.h
soft_fill.o: /usr/include/sys/_types/_fd_copy.h
soft_fill.o: /usr/include/sys/_pthread/_pthread_cond_t.h
soft_fill.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
soft_fill.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
soft_fill.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
soft_fill.o: /usr/include/sys/_pthread/_pthread_once_t.h
soft_fill.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
soft_fill.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
soft_fill.o: /usr/include/sys/_pthread/_pthread_t.h
soft_fill.o: /usr/include/sys/_pthread/_pthread_key_t.h
soft_fill.o: /usr/include/sys/_types/_fsblkcnt_t.h
soft_fill.o: /usr/include/sys/_types/_fsfilcnt_t.h ctrandef.h cgmc.h
soft_fill.o: soft_fill.h
cell.o: cgmc.h ../../../.././include/ncarg/c.h /usr/include/stdio.h
cell.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
cell.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
cell.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
cell.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
cell.o: /usr/include/i386/_types.h /usr/include/sys/_pthread/_pthread_types.h
cell.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
cell.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
cell.o: /usr/include/sys/_types/_int16_t.h /usr/include/sys/_types/_int32_t.h
cell.o: /usr/include/sys/_types/_int64_t.h
cell.o: /usr/include/sys/_types/_u_int8_t.h
cell.o: /usr/include/sys/_types/_u_int16_t.h
cell.o: /usr/include/sys/_types/_u_int32_t.h
cell.o: /usr/include/sys/_types/_u_int64_t.h
cell.o: /usr/include/sys/_types/_intptr_t.h
cell.o: /usr/include/sys/_types/_uintptr_t.h
cell.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
cell.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
cell.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
cell.o: /usr/include/secure/_common.h /usr/include/sys/types.h
cell.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
cell.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
cell.o: /usr/include/libkern/_OSByteOrder.h
cell.o: /usr/include/libkern/i386/_OSByteOrder.h
cell.o: /usr/include/sys/_types/_u_char.h /usr/include/sys/_types/_u_short.h
cell.o: /usr/include/sys/_types/_u_int.h /usr/include/sys/_types/_caddr_t.h
cell.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_blkcnt_t.h
cell.o: /usr/include/sys/_types/_blksize_t.h /usr/include/sys/_types/_gid_t.h
cell.o: /usr/include/sys/_types/_in_addr_t.h
cell.o: /usr/include/sys/_types/_in_port_t.h /usr/include/sys/_types/_ino_t.h
cell.o: /usr/include/sys/_types/_ino64_t.h /usr/include/sys/_types/_key_t.h
cell.o: /usr/include/sys/_types/_mode_t.h /usr/include/sys/_types/_nlink_t.h
cell.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
cell.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/_types/_clock_t.h
cell.o: /usr/include/sys/_types/_time_t.h
cell.o: /usr/include/sys/_types/_useconds_t.h
cell.o: /usr/include/sys/_types/_suseconds_t.h
cell.o: /usr/include/sys/_types/_rsize_t.h /usr/include/sys/_types/_errno_t.h
cell.o: /usr/include/sys/_types/_fd_def.h
cell.o: /usr/include/sys/_types/_fd_setsize.h
cell.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
cell.o: /usr/include/sys/_types/_fd_zero.h
cell.o: /usr/include/sys/_types/_fd_isset.h
cell.o: /usr/include/sys/_types/_fd_copy.h
cell.o: /usr/include/sys/_pthread/_pthread_attr_t.h
cell.o: /usr/include/sys/_pthread/_pthread_cond_t.h
cell.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
cell.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
cell.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
cell.o: /usr/include/sys/_pthread/_pthread_once_t.h
cell.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
cell.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
cell.o: /usr/include/sys/_pthread/_pthread_t.h
cell.o: /usr/include/sys/_pthread/_pthread_key_t.h
cell.o: /usr/include/sys/_types/_fsblkcnt_t.h
cell.o: /usr/include/sys/_types/_fsfilcnt_t.h ctrandef.h
commondev.o: /usr/include/math.h /usr/include/sys/cdefs.h
commondev.o: /usr/include/sys/_symbol_aliasing.h
commondev.o: /usr/include/sys/_posix_availability.h
commondev.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
commondev.o: /usr/include/stdio.h /usr/include/_types.h
commondev.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
commondev.o: /usr/include/i386/_types.h
commondev.o: /usr/include/sys/_pthread/_pthread_types.h
commondev.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
commondev.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
commondev.o: /usr/include/sys/_types/_int16_t.h
commondev.o: /usr/include/sys/_types/_int32_t.h
commondev.o: /usr/include/sys/_types/_int64_t.h
commondev.o: /usr/include/sys/_types/_u_int8_t.h
commondev.o: /usr/include/sys/_types/_u_int16_t.h
commondev.o: /usr/include/sys/_types/_u_int32_t.h
commondev.o: /usr/include/sys/_types/_u_int64_t.h
commondev.o: /usr/include/sys/_types/_intptr_t.h
commondev.o: /usr/include/sys/_types/_uintptr_t.h
commondev.o: /usr/include/sys/_types/_size_t.h
commondev.o: /usr/include/sys/_types/_null.h /usr/include/sys/stdio.h
commondev.o: /usr/include/sys/_types/_off_t.h
commondev.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
commondev.o: /usr/include/secure/_common.h /usr/include/stdlib.h
commondev.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
commondev.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
commondev.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
commondev.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
commondev.o: /usr/include/i386/_mcontext.h
commondev.o: /usr/include/mach/machine/_structs.h
commondev.o: /usr/include/mach/i386/_structs.h
commondev.o: /usr/include/sys/_pthread/_pthread_attr_t.h
commondev.o: /usr/include/sys/_types/_sigaltstack.h
commondev.o: /usr/include/sys/_types/_ucontext.h
commondev.o: /usr/include/sys/_types/_sigset_t.h
commondev.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
commondev.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
commondev.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
commondev.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
commondev.o: /usr/include/_types/_uintmax_t.h
commondev.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
commondev.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
commondev.o: /usr/include/libkern/_OSByteOrder.h
commondev.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
commondev.o: /usr/include/sys/_types/_ct_rune_t.h
commondev.o: /usr/include/sys/_types/_rune_t.h
commondev.o: /usr/include/sys/_types/_wchar_t.h
commondev.o: /usr/include/sys/_types/_dev_t.h
commondev.o: /usr/include/sys/_types/_mode_t.h /usr/include/errno.h
commondev.o: /usr/include/sys/errno.h /usr/include/sys/_types/_errno_t.h
commondev.o: ../../../.././include/ncarg/c.h /usr/include/sys/types.h
commondev.o: /usr/include/sys/_types/_u_char.h
commondev.o: /usr/include/sys/_types/_u_short.h
commondev.o: /usr/include/sys/_types/_u_int.h
commondev.o: /usr/include/sys/_types/_caddr_t.h
commondev.o: /usr/include/sys/_types/_blkcnt_t.h
commondev.o: /usr/include/sys/_types/_blksize_t.h
commondev.o: /usr/include/sys/_types/_gid_t.h
commondev.o: /usr/include/sys/_types/_in_addr_t.h
commondev.o: /usr/include/sys/_types/_in_port_t.h
commondev.o: /usr/include/sys/_types/_ino_t.h
commondev.o: /usr/include/sys/_types/_ino64_t.h
commondev.o: /usr/include/sys/_types/_key_t.h
commondev.o: /usr/include/sys/_types/_nlink_t.h
commondev.o: /usr/include/sys/_types/_clock_t.h
commondev.o: /usr/include/sys/_types/_time_t.h
commondev.o: /usr/include/sys/_types/_useconds_t.h
commondev.o: /usr/include/sys/_types/_suseconds_t.h
commondev.o: /usr/include/sys/_types/_rsize_t.h
commondev.o: /usr/include/sys/_types/_fd_def.h
commondev.o: /usr/include/sys/_types/_fd_setsize.h
commondev.o: /usr/include/sys/_types/_fd_set.h
commondev.o: /usr/include/sys/_types/_fd_clr.h
commondev.o: /usr/include/sys/_types/_fd_zero.h
commondev.o: /usr/include/sys/_types/_fd_isset.h
commondev.o: /usr/include/sys/_types/_fd_copy.h
commondev.o: /usr/include/sys/_pthread/_pthread_cond_t.h
commondev.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
commondev.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
commondev.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
commondev.o: /usr/include/sys/_pthread/_pthread_once_t.h
commondev.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
commondev.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
commondev.o: /usr/include/sys/_pthread/_pthread_t.h
commondev.o: /usr/include/sys/_pthread/_pthread_key_t.h
commondev.o: /usr/include/sys/_types/_fsblkcnt_t.h
commondev.o: /usr/include/sys/_types/_fsfilcnt_t.h commondev.h gcapdev.h
commondev.o: rastdev.h cgmc.h soft_fill.h default.h
commondev.o: ../../../.././include/ncarg/cgmdef.h text.h translate.h
commondev.o: ctrandef.h hatch.h
gcapdev.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
gcapdev.o: /usr/include/sys/_symbol_aliasing.h
gcapdev.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
gcapdev.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
gcapdev.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
gcapdev.o: /usr/include/i386/_types.h
gcapdev.o: /usr/include/sys/_pthread/_pthread_types.h
gcapdev.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
gcapdev.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
gcapdev.o: /usr/include/sys/_types/_int16_t.h
gcapdev.o: /usr/include/sys/_types/_int32_t.h
gcapdev.o: /usr/include/sys/_types/_int64_t.h
gcapdev.o: /usr/include/sys/_types/_u_int8_t.h
gcapdev.o: /usr/include/sys/_types/_u_int16_t.h
gcapdev.o: /usr/include/sys/_types/_u_int32_t.h
gcapdev.o: /usr/include/sys/_types/_u_int64_t.h
gcapdev.o: /usr/include/sys/_types/_intptr_t.h
gcapdev.o: /usr/include/sys/_types/_uintptr_t.h
gcapdev.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
gcapdev.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
gcapdev.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
gcapdev.o: /usr/include/secure/_common.h /usr/include/stdlib.h
gcapdev.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
gcapdev.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
gcapdev.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
gcapdev.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
gcapdev.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
gcapdev.o: /usr/include/mach/i386/_structs.h
gcapdev.o: /usr/include/sys/_pthread/_pthread_attr_t.h
gcapdev.o: /usr/include/sys/_types/_sigaltstack.h
gcapdev.o: /usr/include/sys/_types/_ucontext.h
gcapdev.o: /usr/include/sys/_types/_sigset_t.h
gcapdev.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
gcapdev.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
gcapdev.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
gcapdev.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
gcapdev.o: /usr/include/_types/_uintmax_t.h
gcapdev.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
gcapdev.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
gcapdev.o: /usr/include/libkern/_OSByteOrder.h
gcapdev.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
gcapdev.o: /usr/include/sys/_types/_ct_rune_t.h
gcapdev.o: /usr/include/sys/_types/_rune_t.h
gcapdev.o: /usr/include/sys/_types/_wchar_t.h
gcapdev.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_mode_t.h
gcapdev.o: /usr/include/errno.h /usr/include/sys/errno.h
gcapdev.o: /usr/include/sys/_types/_errno_t.h ../../../.././include/ncarg/c.h
gcapdev.o: /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
gcapdev.o: /usr/include/sys/_types/_u_short.h
gcapdev.o: /usr/include/sys/_types/_u_int.h
gcapdev.o: /usr/include/sys/_types/_caddr_t.h
gcapdev.o: /usr/include/sys/_types/_blkcnt_t.h
gcapdev.o: /usr/include/sys/_types/_blksize_t.h
gcapdev.o: /usr/include/sys/_types/_gid_t.h
gcapdev.o: /usr/include/sys/_types/_in_addr_t.h
gcapdev.o: /usr/include/sys/_types/_in_port_t.h
gcapdev.o: /usr/include/sys/_types/_ino_t.h
gcapdev.o: /usr/include/sys/_types/_ino64_t.h
gcapdev.o: /usr/include/sys/_types/_key_t.h
gcapdev.o: /usr/include/sys/_types/_nlink_t.h
gcapdev.o: /usr/include/sys/_types/_clock_t.h
gcapdev.o: /usr/include/sys/_types/_time_t.h
gcapdev.o: /usr/include/sys/_types/_useconds_t.h
gcapdev.o: /usr/include/sys/_types/_suseconds_t.h
gcapdev.o: /usr/include/sys/_types/_rsize_t.h
gcapdev.o: /usr/include/sys/_types/_fd_def.h
gcapdev.o: /usr/include/sys/_types/_fd_setsize.h
gcapdev.o: /usr/include/sys/_types/_fd_set.h
gcapdev.o: /usr/include/sys/_types/_fd_clr.h
gcapdev.o: /usr/include/sys/_types/_fd_zero.h
gcapdev.o: /usr/include/sys/_types/_fd_isset.h
gcapdev.o: /usr/include/sys/_types/_fd_copy.h
gcapdev.o: /usr/include/sys/_pthread/_pthread_cond_t.h
gcapdev.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
gcapdev.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
gcapdev.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
gcapdev.o: /usr/include/sys/_pthread/_pthread_once_t.h
gcapdev.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
gcapdev.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
gcapdev.o: /usr/include/sys/_pthread/_pthread_t.h
gcapdev.o: /usr/include/sys/_pthread/_pthread_key_t.h
gcapdev.o: /usr/include/sys/_types/_fsblkcnt_t.h
gcapdev.o: /usr/include/sys/_types/_fsfilcnt_t.h ctrans.h
gcapdev.o: ../../../.././include/ncarg/cgm_tools.h
gcapdev.o: ../../../.././include/ncarg/cgmdef.h graphcap.h cgmc.h soft_fill.h
gcapdev.o: translate.h ctrandef.h default.h text.h format.h
gcaprast.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
gcaprast.o: /usr/include/sys/_symbol_aliasing.h
gcaprast.o: /usr/include/sys/_posix_availability.h
gcaprast.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
gcaprast.o: /usr/include/_types.h /usr/include/sys/_types.h
gcaprast.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
gcaprast.o: /usr/include/sys/_pthread/_pthread_types.h
gcaprast.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
gcaprast.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
gcaprast.o: /usr/include/sys/_types/_int16_t.h
gcaprast.o: /usr/include/sys/_types/_int32_t.h
gcaprast.o: /usr/include/sys/_types/_int64_t.h
gcaprast.o: /usr/include/sys/_types/_u_int8_t.h
gcaprast.o: /usr/include/sys/_types/_u_int16_t.h
gcaprast.o: /usr/include/sys/_types/_u_int32_t.h
gcaprast.o: /usr/include/sys/_types/_u_int64_t.h
gcaprast.o: /usr/include/sys/_types/_intptr_t.h
gcaprast.o: /usr/include/sys/_types/_uintptr_t.h
gcaprast.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
gcaprast.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
gcaprast.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
gcaprast.o: /usr/include/secure/_common.h /usr/include/stdlib.h
gcaprast.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
gcaprast.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
gcaprast.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
gcaprast.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
gcaprast.o: /usr/include/i386/_mcontext.h
gcaprast.o: /usr/include/mach/machine/_structs.h
gcaprast.o: /usr/include/mach/i386/_structs.h
gcaprast.o: /usr/include/sys/_pthread/_pthread_attr_t.h
gcaprast.o: /usr/include/sys/_types/_sigaltstack.h
gcaprast.o: /usr/include/sys/_types/_ucontext.h
gcaprast.o: /usr/include/sys/_types/_sigset_t.h
gcaprast.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
gcaprast.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
gcaprast.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
gcaprast.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
gcaprast.o: /usr/include/_types/_uintmax_t.h
gcaprast.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
gcaprast.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
gcaprast.o: /usr/include/libkern/_OSByteOrder.h
gcaprast.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
gcaprast.o: /usr/include/sys/_types/_ct_rune_t.h
gcaprast.o: /usr/include/sys/_types/_rune_t.h
gcaprast.o: /usr/include/sys/_types/_wchar_t.h
gcaprast.o: /usr/include/sys/_types/_dev_t.h
gcaprast.o: /usr/include/sys/_types/_mode_t.h /usr/include/errno.h
gcaprast.o: /usr/include/sys/errno.h /usr/include/sys/_types/_errno_t.h
gcaprast.o: ../../../.././include/ncarg/c.h /usr/include/sys/types.h
gcaprast.o: /usr/include/sys/_types/_u_char.h
gcaprast.o: /usr/include/sys/_types/_u_short.h
gcaprast.o: /usr/include/sys/_types/_u_int.h
gcaprast.o: /usr/include/sys/_types/_caddr_t.h
gcaprast.o: /usr/include/sys/_types/_blkcnt_t.h
gcaprast.o: /usr/include/sys/_types/_blksize_t.h
gcaprast.o: /usr/include/sys/_types/_gid_t.h
gcaprast.o: /usr/include/sys/_types/_in_addr_t.h
gcaprast.o: /usr/include/sys/_types/_in_port_t.h
gcaprast.o: /usr/include/sys/_types/_ino_t.h
gcaprast.o: /usr/include/sys/_types/_ino64_t.h
gcaprast.o: /usr/include/sys/_types/_key_t.h
gcaprast.o: /usr/include/sys/_types/_nlink_t.h
gcaprast.o: /usr/include/sys/_types/_clock_t.h
gcaprast.o: /usr/include/sys/_types/_time_t.h
gcaprast.o: /usr/include/sys/_types/_useconds_t.h
gcaprast.o: /usr/include/sys/_types/_suseconds_t.h
gcaprast.o: /usr/include/sys/_types/_rsize_t.h
gcaprast.o: /usr/include/sys/_types/_fd_def.h
gcaprast.o: /usr/include/sys/_types/_fd_setsize.h
gcaprast.o: /usr/include/sys/_types/_fd_set.h
gcaprast.o: /usr/include/sys/_types/_fd_clr.h
gcaprast.o: /usr/include/sys/_types/_fd_zero.h
gcaprast.o: /usr/include/sys/_types/_fd_isset.h
gcaprast.o: /usr/include/sys/_types/_fd_copy.h
gcaprast.o: /usr/include/sys/_pthread/_pthread_cond_t.h
gcaprast.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
gcaprast.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
gcaprast.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
gcaprast.o: /usr/include/sys/_pthread/_pthread_once_t.h
gcaprast.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
gcaprast.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
gcaprast.o: /usr/include/sys/_pthread/_pthread_t.h
gcaprast.o: /usr/include/sys/_pthread/_pthread_key_t.h
gcaprast.o: /usr/include/sys/_types/_fsblkcnt_t.h
gcaprast.o: /usr/include/sys/_types/_fsfilcnt_t.h cgmc.h defines.h graphcap.h
gcaprast.o: ctrandef.h translate.h bitops.h
rastdev.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
rastdev.o: /usr/include/sys/_symbol_aliasing.h
rastdev.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
rastdev.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
rastdev.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
rastdev.o: /usr/include/i386/_types.h
rastdev.o: /usr/include/sys/_pthread/_pthread_types.h
rastdev.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
rastdev.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
rastdev.o: /usr/include/sys/_types/_int16_t.h
rastdev.o: /usr/include/sys/_types/_int32_t.h
rastdev.o: /usr/include/sys/_types/_int64_t.h
rastdev.o: /usr/include/sys/_types/_u_int8_t.h
rastdev.o: /usr/include/sys/_types/_u_int16_t.h
rastdev.o: /usr/include/sys/_types/_u_int32_t.h
rastdev.o: /usr/include/sys/_types/_u_int64_t.h
rastdev.o: /usr/include/sys/_types/_intptr_t.h
rastdev.o: /usr/include/sys/_types/_uintptr_t.h
rastdev.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
rastdev.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
rastdev.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
rastdev.o: /usr/include/secure/_common.h
rastdev.o: ../../../.././include/ncarg/ncarg_ras.h /usr/include/sys/types.h
rastdev.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
rastdev.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
rastdev.o: /usr/include/libkern/_OSByteOrder.h
rastdev.o: /usr/include/libkern/i386/_OSByteOrder.h
rastdev.o: /usr/include/sys/_types/_u_char.h
rastdev.o: /usr/include/sys/_types/_u_short.h
rastdev.o: /usr/include/sys/_types/_u_int.h
rastdev.o: /usr/include/sys/_types/_caddr_t.h
rastdev.o: /usr/include/sys/_types/_dev_t.h
rastdev.o: /usr/include/sys/_types/_blkcnt_t.h
rastdev.o: /usr/include/sys/_types/_blksize_t.h
rastdev.o: /usr/include/sys/_types/_gid_t.h
rastdev.o: /usr/include/sys/_types/_in_addr_t.h
rastdev.o: /usr/include/sys/_types/_in_port_t.h
rastdev.o: /usr/include/sys/_types/_ino_t.h
rastdev.o: /usr/include/sys/_types/_ino64_t.h
rastdev.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
rastdev.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
rastdev.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
rastdev.o: /usr/include/sys/_types/_clock_t.h
rastdev.o: /usr/include/sys/_types/_time_t.h
rastdev.o: /usr/include/sys/_types/_useconds_t.h
rastdev.o: /usr/include/sys/_types/_suseconds_t.h
rastdev.o: /usr/include/sys/_types/_rsize_t.h
rastdev.o: /usr/include/sys/_types/_errno_t.h
rastdev.o: /usr/include/sys/_types/_fd_def.h
rastdev.o: /usr/include/sys/_types/_fd_setsize.h
rastdev.o: /usr/include/sys/_types/_fd_set.h
rastdev.o: /usr/include/sys/_types/_fd_clr.h
rastdev.o: /usr/include/sys/_types/_fd_zero.h
rastdev.o: /usr/include/sys/_types/_fd_isset.h
rastdev.o: /usr/include/sys/_types/_fd_copy.h
rastdev.o: /usr/include/sys/_pthread/_pthread_attr_t.h
rastdev.o: /usr/include/sys/_pthread/_pthread_cond_t.h
rastdev.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
rastdev.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
rastdev.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
rastdev.o: /usr/include/sys/_pthread/_pthread_once_t.h
rastdev.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
rastdev.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
rastdev.o: /usr/include/sys/_pthread/_pthread_t.h
rastdev.o: /usr/include/sys/_pthread/_pthread_key_t.h
rastdev.o: /usr/include/sys/_types/_fsblkcnt_t.h
rastdev.o: /usr/include/sys/_types/_fsfilcnt_t.h /usr/include/sys/stat.h
rastdev.o: /usr/include/sys/_types/_timespec.h
rastdev.o: /usr/include/sys/_types/_s_ifmt.h
rastdev.o: /usr/include/sys/_types/_filesec_t.h
rastdev.o: ../../../.././include/ncarg/c.h rast.h cgmc.h default.h
rastdev.o: ../../../.././include/ncarg/cgmdef.h text.h ctrandef.h translate.h
rastdev.o: /usr/include/stdlib.h /usr/include/sys/wait.h
rastdev.o: /usr/include/sys/signal.h /usr/include/machine/signal.h
rastdev.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
rastdev.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
rastdev.o: /usr/include/mach/i386/_structs.h
rastdev.o: /usr/include/sys/_types/_sigaltstack.h
rastdev.o: /usr/include/sys/_types/_ucontext.h
rastdev.o: /usr/include/sys/_types/_sigset_t.h /usr/include/sys/resource.h
rastdev.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
rastdev.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
rastdev.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
rastdev.o: /usr/include/_types/_uintmax_t.h
rastdev.o: /usr/include/sys/_types/_timeval.h /usr/include/alloca.h
rastdev.o: /usr/include/sys/_types/_ct_rune_t.h
rastdev.o: /usr/include/sys/_types/_rune_t.h
rastdev.o: /usr/include/sys/_types/_wchar_t.h
hatch.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
hatch.o: /usr/include/sys/_symbol_aliasing.h
hatch.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
hatch.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
hatch.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
hatch.o: /usr/include/i386/_types.h
hatch.o: /usr/include/sys/_pthread/_pthread_types.h
hatch.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
hatch.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
hatch.o: /usr/include/sys/_types/_int16_t.h
hatch.o: /usr/include/sys/_types/_int32_t.h
hatch.o: /usr/include/sys/_types/_int64_t.h
hatch.o: /usr/include/sys/_types/_u_int8_t.h
hatch.o: /usr/include/sys/_types/_u_int16_t.h
hatch.o: /usr/include/sys/_types/_u_int32_t.h
hatch.o: /usr/include/sys/_types/_u_int64_t.h
hatch.o: /usr/include/sys/_types/_intptr_t.h
hatch.o: /usr/include/sys/_types/_uintptr_t.h
hatch.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
hatch.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
hatch.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
hatch.o: /usr/include/secure/_common.h /usr/include/math.h default.h
hatch.o: ../../../.././include/ncarg/c.h /usr/include/sys/types.h
hatch.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
hatch.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
hatch.o: /usr/include/libkern/_OSByteOrder.h
hatch.o: /usr/include/libkern/i386/_OSByteOrder.h
hatch.o: /usr/include/sys/_types/_u_char.h /usr/include/sys/_types/_u_short.h
hatch.o: /usr/include/sys/_types/_u_int.h /usr/include/sys/_types/_caddr_t.h
hatch.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_blkcnt_t.h
hatch.o: /usr/include/sys/_types/_blksize_t.h
hatch.o: /usr/include/sys/_types/_gid_t.h
hatch.o: /usr/include/sys/_types/_in_addr_t.h
hatch.o: /usr/include/sys/_types/_in_port_t.h
hatch.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
hatch.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
hatch.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
hatch.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
hatch.o: /usr/include/sys/_types/_clock_t.h /usr/include/sys/_types/_time_t.h
hatch.o: /usr/include/sys/_types/_useconds_t.h
hatch.o: /usr/include/sys/_types/_suseconds_t.h
hatch.o: /usr/include/sys/_types/_rsize_t.h
hatch.o: /usr/include/sys/_types/_errno_t.h /usr/include/sys/_types/_fd_def.h
hatch.o: /usr/include/sys/_types/_fd_setsize.h
hatch.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
hatch.o: /usr/include/sys/_types/_fd_zero.h
hatch.o: /usr/include/sys/_types/_fd_isset.h
hatch.o: /usr/include/sys/_types/_fd_copy.h
hatch.o: /usr/include/sys/_pthread/_pthread_attr_t.h
hatch.o: /usr/include/sys/_pthread/_pthread_cond_t.h
hatch.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
hatch.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
hatch.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
hatch.o: /usr/include/sys/_pthread/_pthread_once_t.h
hatch.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
hatch.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
hatch.o: /usr/include/sys/_pthread/_pthread_t.h
hatch.o: /usr/include/sys/_pthread/_pthread_key_t.h
hatch.o: /usr/include/sys/_types/_fsblkcnt_t.h
hatch.o: /usr/include/sys/_types/_fsfilcnt_t.h
hatch.o: ../../../.././include/ncarg/cgmdef.h cgmc.h text.h soft_fill.h
hatch.o: translate.h /usr/include/stdlib.h /usr/include/sys/wait.h
hatch.o: /usr/include/sys/signal.h /usr/include/machine/signal.h
hatch.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
hatch.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
hatch.o: /usr/include/mach/i386/_structs.h
hatch.o: /usr/include/sys/_types/_sigaltstack.h
hatch.o: /usr/include/sys/_types/_ucontext.h
hatch.o: /usr/include/sys/_types/_sigset_t.h /usr/include/sys/resource.h
hatch.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
hatch.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
hatch.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
hatch.o: /usr/include/_types/_uintmax_t.h /usr/include/sys/_types/_timeval.h
hatch.o: /usr/include/alloca.h /usr/include/sys/_types/_ct_rune_t.h
hatch.o: /usr/include/sys/_types/_rune_t.h /usr/include/sys/_types/_wchar_t.h
hatch.o: ctrandef.h hatch.h commondev.h
pal.o: /usr/include/stdlib.h /usr/include/Availability.h
pal.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
pal.o: /usr/include/sys/_types.h /usr/include/sys/cdefs.h
pal.o: /usr/include/sys/_symbol_aliasing.h
pal.o: /usr/include/sys/_posix_availability.h /usr/include/machine/_types.h
pal.o: /usr/include/i386/_types.h /usr/include/sys/_pthread/_pthread_types.h
pal.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
pal.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
pal.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
pal.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
pal.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
pal.o: /usr/include/mach/i386/_structs.h /usr/include/machine/types.h
pal.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
pal.o: /usr/include/sys/_types/_int16_t.h /usr/include/sys/_types/_int32_t.h
pal.o: /usr/include/sys/_types/_int64_t.h /usr/include/sys/_types/_u_int8_t.h
pal.o: /usr/include/sys/_types/_u_int16_t.h
pal.o: /usr/include/sys/_types/_u_int32_t.h
pal.o: /usr/include/sys/_types/_u_int64_t.h
pal.o: /usr/include/sys/_types/_intptr_t.h
pal.o: /usr/include/sys/_types/_uintptr_t.h
pal.o: /usr/include/sys/_pthread/_pthread_attr_t.h
pal.o: /usr/include/sys/_types/_sigaltstack.h
pal.o: /usr/include/sys/_types/_ucontext.h
pal.o: /usr/include/sys/_types/_sigset_t.h /usr/include/sys/_types/_size_t.h
pal.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
pal.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
pal.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
pal.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
pal.o: /usr/include/_types/_uintmax_t.h /usr/include/sys/_types/_timeval.h
pal.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
pal.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
pal.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
pal.o: /usr/include/sys/_types/_ct_rune_t.h /usr/include/sys/_types/_rune_t.h
pal.o: /usr/include/sys/_types/_wchar_t.h /usr/include/sys/_types/_null.h
pal.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_mode_t.h
pal.o: /usr/include/errno.h /usr/include/sys/errno.h
pal.o: /usr/include/sys/_types/_errno_t.h ../../../.././include/ncarg/c.h
pal.o: /usr/include/stdio.h /usr/include/sys/_types/_va_list.h
pal.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
pal.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
pal.o: /usr/include/secure/_common.h /usr/include/sys/types.h
pal.o: /usr/include/sys/_types/_u_char.h /usr/include/sys/_types/_u_short.h
pal.o: /usr/include/sys/_types/_u_int.h /usr/include/sys/_types/_caddr_t.h
pal.o: /usr/include/sys/_types/_blkcnt_t.h
pal.o: /usr/include/sys/_types/_blksize_t.h /usr/include/sys/_types/_gid_t.h
pal.o: /usr/include/sys/_types/_in_addr_t.h
pal.o: /usr/include/sys/_types/_in_port_t.h /usr/include/sys/_types/_ino_t.h
pal.o: /usr/include/sys/_types/_ino64_t.h /usr/include/sys/_types/_key_t.h
pal.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_clock_t.h
pal.o: /usr/include/sys/_types/_time_t.h
pal.o: /usr/include/sys/_types/_useconds_t.h
pal.o: /usr/include/sys/_types/_suseconds_t.h
pal.o: /usr/include/sys/_types/_rsize_t.h /usr/include/sys/_types/_fd_def.h
pal.o: /usr/include/sys/_types/_fd_setsize.h
pal.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
pal.o: /usr/include/sys/_types/_fd_zero.h /usr/include/sys/_types/_fd_isset.h
pal.o: /usr/include/sys/_types/_fd_copy.h
pal.o: /usr/include/sys/_pthread/_pthread_cond_t.h
pal.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
pal.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
pal.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
pal.o: /usr/include/sys/_pthread/_pthread_once_t.h
pal.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
pal.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
pal.o: /usr/include/sys/_pthread/_pthread_t.h
pal.o: /usr/include/sys/_pthread/_pthread_key_t.h
pal.o: /usr/include/sys/_types/_fsblkcnt_t.h
pal.o: /usr/include/sys/_types/_fsfilcnt_t.h
pal.o: ../../../.././include/ncarg/ncarg_ras.h /usr/include/sys/stat.h
pal.o: /usr/include/sys/_types/_timespec.h /usr/include/sys/_types/_s_ifmt.h
pal.o: /usr/include/sys/_types/_filesec_t.h cgmc.h default.h
pal.o: ../../../.././include/ncarg/cgmdef.h text.h
ctrans_api.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
ctrans_api.o: /usr/include/sys/_symbol_aliasing.h
ctrans_api.o: /usr/include/sys/_posix_availability.h
ctrans_api.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
ctrans_api.o: /usr/include/_types.h /usr/include/sys/_types.h
ctrans_api.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
ctrans_api.o: /usr/include/sys/_pthread/_pthread_types.h
ctrans_api.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
ctrans_api.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
ctrans_api.o: /usr/include/sys/_types/_int16_t.h
ctrans_api.o: /usr/include/sys/_types/_int32_t.h
ctrans_api.o: /usr/include/sys/_types/_int64_t.h
ctrans_api.o: /usr/include/sys/_types/_u_int8_t.h
ctrans_api.o: /usr/include/sys/_types/_u_int16_t.h
ctrans_api.o: /usr/include/sys/_types/_u_int32_t.h
ctrans_api.o: /usr/include/sys/_types/_u_int64_t.h
ctrans_api.o: /usr/include/sys/_types/_intptr_t.h
ctrans_api.o: /usr/include/sys/_types/_uintptr_t.h
ctrans_api.o: /usr/include/sys/_types/_size_t.h
ctrans_api.o: /usr/include/sys/_types/_null.h /usr/include/sys/stdio.h
ctrans_api.o: /usr/include/sys/_types/_off_t.h
ctrans_api.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
ctrans_api.o: /usr/include/secure/_common.h /usr/include/fcntl.h
ctrans_api.o: /usr/include/sys/fcntl.h /usr/include/sys/_types/_mode_t.h
ctrans_api.o: /usr/include/sys/_types/_pid_t.h
ctrans_api.o: /usr/include/sys/_types/_o_sync.h
ctrans_api.o: /usr/include/sys/_types/_o_dsync.h
ctrans_api.o: /usr/include/sys/_types/_seek_set.h
ctrans_api.o: /usr/include/sys/_types/_s_ifmt.h
ctrans_api.o: /usr/include/sys/_types/_timespec.h
ctrans_api.o: /usr/include/sys/_types/_filesec_t.h /usr/include/errno.h
ctrans_api.o: /usr/include/sys/errno.h /usr/include/sys/_types/_errno_t.h
ctrans_api.o: /usr/include/string.h /usr/include/sys/_types/_rsize_t.h
ctrans_api.o: /usr/include/strings.h /usr/include/secure/_strings.h
ctrans_api.o: /usr/include/secure/_string.h ../../../.././include/ncarg/c.h
ctrans_api.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
ctrans_api.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
ctrans_api.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
ctrans_api.o: /usr/include/libkern/i386/_OSByteOrder.h
ctrans_api.o: /usr/include/sys/_types/_u_char.h
ctrans_api.o: /usr/include/sys/_types/_u_short.h
ctrans_api.o: /usr/include/sys/_types/_u_int.h
ctrans_api.o: /usr/include/sys/_types/_caddr_t.h
ctrans_api.o: /usr/include/sys/_types/_dev_t.h
ctrans_api.o: /usr/include/sys/_types/_blkcnt_t.h
ctrans_api.o: /usr/include/sys/_types/_blksize_t.h
ctrans_api.o: /usr/include/sys/_types/_gid_t.h
ctrans_api.o: /usr/include/sys/_types/_in_addr_t.h
ctrans_api.o: /usr/include/sys/_types/_in_port_t.h
ctrans_api.o: /usr/include/sys/_types/_ino_t.h
ctrans_api.o: /usr/include/sys/_types/_ino64_t.h
ctrans_api.o: /usr/include/sys/_types/_key_t.h
ctrans_api.o: /usr/include/sys/_types/_nlink_t.h
ctrans_api.o: /usr/include/sys/_types/_id_t.h
ctrans_api.o: /usr/include/sys/_types/_uid_t.h
ctrans_api.o: /usr/include/sys/_types/_clock_t.h
ctrans_api.o: /usr/include/sys/_types/_time_t.h
ctrans_api.o: /usr/include/sys/_types/_useconds_t.h
ctrans_api.o: /usr/include/sys/_types/_suseconds_t.h
ctrans_api.o: /usr/include/sys/_types/_fd_def.h
ctrans_api.o: /usr/include/sys/_types/_fd_setsize.h
ctrans_api.o: /usr/include/sys/_types/_fd_set.h
ctrans_api.o: /usr/include/sys/_types/_fd_clr.h
ctrans_api.o: /usr/include/sys/_types/_fd_zero.h
ctrans_api.o: /usr/include/sys/_types/_fd_isset.h
ctrans_api.o: /usr/include/sys/_types/_fd_copy.h
ctrans_api.o: /usr/include/sys/_pthread/_pthread_attr_t.h
ctrans_api.o: /usr/include/sys/_pthread/_pthread_cond_t.h
ctrans_api.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
ctrans_api.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
ctrans_api.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
ctrans_api.o: /usr/include/sys/_pthread/_pthread_once_t.h
ctrans_api.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
ctrans_api.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
ctrans_api.o: /usr/include/sys/_pthread/_pthread_t.h
ctrans_api.o: /usr/include/sys/_pthread/_pthread_key_t.h
ctrans_api.o: /usr/include/sys/_types/_fsblkcnt_t.h
ctrans_api.o: /usr/include/sys/_types/_fsfilcnt_t.h
ctrans_api.o: ../../../.././include/ncarg/cgm_tools.h
ctrans_api.o: ../../../.././include/ncarg/cgmdef.h ctrans.h cgmc.h defines.h
ctrans_api.o: devices.h ctrandef.h
cgmmisc.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
cgmmisc.o: /usr/include/sys/_symbol_aliasing.h
cgmmisc.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
cgmmisc.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
cgmmisc.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
cgmmisc.o: /usr/include/i386/_types.h
cgmmisc.o: /usr/include/sys/_pthread/_pthread_types.h
cgmmisc.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
cgmmisc.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
cgmmisc.o: /usr/include/sys/_types/_int16_t.h
cgmmisc.o: /usr/include/sys/_types/_int32_t.h
cgmmisc.o: /usr/include/sys/_types/_int64_t.h
cgmmisc.o: /usr/include/sys/_types/_u_int8_t.h
cgmmisc.o: /usr/include/sys/_types/_u_int16_t.h
cgmmisc.o: /usr/include/sys/_types/_u_int32_t.h
cgmmisc.o: /usr/include/sys/_types/_u_int64_t.h
cgmmisc.o: /usr/include/sys/_types/_intptr_t.h
cgmmisc.o: /usr/include/sys/_types/_uintptr_t.h
cgmmisc.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
cgmmisc.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
cgmmisc.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
cgmmisc.o: /usr/include/secure/_common.h cgmc.h
cgmmisc.o: ../../../.././include/ncarg/c.h /usr/include/sys/types.h
cgmmisc.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
cgmmisc.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
cgmmisc.o: /usr/include/libkern/_OSByteOrder.h
cgmmisc.o: /usr/include/libkern/i386/_OSByteOrder.h
cgmmisc.o: /usr/include/sys/_types/_u_char.h
cgmmisc.o: /usr/include/sys/_types/_u_short.h
cgmmisc.o: /usr/include/sys/_types/_u_int.h
cgmmisc.o: /usr/include/sys/_types/_caddr_t.h
cgmmisc.o: /usr/include/sys/_types/_dev_t.h
cgmmisc.o: /usr/include/sys/_types/_blkcnt_t.h
cgmmisc.o: /usr/include/sys/_types/_blksize_t.h
cgmmisc.o: /usr/include/sys/_types/_gid_t.h
cgmmisc.o: /usr/include/sys/_types/_in_addr_t.h
cgmmisc.o: /usr/include/sys/_types/_in_port_t.h
cgmmisc.o: /usr/include/sys/_types/_ino_t.h
cgmmisc.o: /usr/include/sys/_types/_ino64_t.h
cgmmisc.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
cgmmisc.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
cgmmisc.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
cgmmisc.o: /usr/include/sys/_types/_clock_t.h
cgmmisc.o: /usr/include/sys/_types/_time_t.h
cgmmisc.o: /usr/include/sys/_types/_useconds_t.h
cgmmisc.o: /usr/include/sys/_types/_suseconds_t.h
cgmmisc.o: /usr/include/sys/_types/_rsize_t.h
cgmmisc.o: /usr/include/sys/_types/_errno_t.h
cgmmisc.o: /usr/include/sys/_types/_fd_def.h
cgmmisc.o: /usr/include/sys/_types/_fd_setsize.h
cgmmisc.o: /usr/include/sys/_types/_fd_set.h
cgmmisc.o: /usr/include/sys/_types/_fd_clr.h
cgmmisc.o: /usr/include/sys/_types/_fd_zero.h
cgmmisc.o: /usr/include/sys/_types/_fd_isset.h
cgmmisc.o: /usr/include/sys/_types/_fd_copy.h
cgmmisc.o: /usr/include/sys/_pthread/_pthread_attr_t.h
cgmmisc.o: /usr/include/sys/_pthread/_pthread_cond_t.h
cgmmisc.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
cgmmisc.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
cgmmisc.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
cgmmisc.o: /usr/include/sys/_pthread/_pthread_once_t.h
cgmmisc.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
cgmmisc.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
cgmmisc.o: /usr/include/sys/_pthread/_pthread_t.h
cgmmisc.o: /usr/include/sys/_pthread/_pthread_key_t.h
cgmmisc.o: /usr/include/sys/_types/_fsblkcnt_t.h
cgmmisc.o: /usr/include/sys/_types/_fsfilcnt_t.h
X11_class0.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
X11_class0.o: /usr/include/sys/_symbol_aliasing.h
X11_class0.o: /usr/include/sys/_posix_availability.h
X11_class0.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
X11_class0.o: /usr/include/_types.h /usr/include/sys/_types.h
X11_class0.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
X11_class0.o: /usr/include/sys/_pthread/_pthread_types.h
X11_class0.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
X11_class0.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
X11_class0.o: /usr/include/sys/_types/_int16_t.h
X11_class0.o: /usr/include/sys/_types/_int32_t.h
X11_class0.o: /usr/include/sys/_types/_int64_t.h
X11_class0.o: /usr/include/sys/_types/_u_int8_t.h
X11_class0.o: /usr/include/sys/_types/_u_int16_t.h
X11_class0.o: /usr/include/sys/_types/_u_int32_t.h
X11_class0.o: /usr/include/sys/_types/_u_int64_t.h
X11_class0.o: /usr/include/sys/_types/_intptr_t.h
X11_class0.o: /usr/include/sys/_types/_uintptr_t.h
X11_class0.o: /usr/include/sys/_types/_size_t.h
X11_class0.o: /usr/include/sys/_types/_null.h /usr/include/sys/stdio.h
X11_class0.o: /usr/include/sys/_types/_off_t.h
X11_class0.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
X11_class0.o: /usr/include/secure/_common.h /usr/include/stdlib.h
X11_class0.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
X11_class0.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
X11_class0.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
X11_class0.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
X11_class0.o: /usr/include/i386/_mcontext.h
X11_class0.o: /usr/include/mach/machine/_structs.h
X11_class0.o: /usr/include/mach/i386/_structs.h
X11_class0.o: /usr/include/sys/_pthread/_pthread_attr_t.h
X11_class0.o: /usr/include/sys/_types/_sigaltstack.h
X11_class0.o: /usr/include/sys/_types/_ucontext.h
X11_class0.o: /usr/include/sys/_types/_sigset_t.h
X11_class0.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
X11_class0.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
X11_class0.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
X11_class0.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
X11_class0.o: /usr/include/_types/_uintmax_t.h
X11_class0.o: /usr/include/sys/_types/_timeval.h
X11_class0.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
X11_class0.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
X11_class0.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
X11_class0.o: /usr/include/sys/_types/_ct_rune_t.h
X11_class0.o: /usr/include/sys/_types/_rune_t.h
X11_class0.o: /usr/include/sys/_types/_wchar_t.h
X11_class0.o: /usr/include/sys/_types/_dev_t.h
X11_class0.o: /usr/include/sys/_types/_mode_t.h /usr/include/string.h
X11_class0.o: /usr/include/sys/_types/_rsize_t.h
X11_class0.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
X11_class0.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
X11_class0.o: /opt/X11/include/X11/Xlib.h /usr/include/sys/types.h
X11_class0.o: /usr/include/sys/_types/_u_char.h
X11_class0.o: /usr/include/sys/_types/_u_short.h
X11_class0.o: /usr/include/sys/_types/_u_int.h
X11_class0.o: /usr/include/sys/_types/_caddr_t.h
X11_class0.o: /usr/include/sys/_types/_blkcnt_t.h
X11_class0.o: /usr/include/sys/_types/_blksize_t.h
X11_class0.o: /usr/include/sys/_types/_gid_t.h
X11_class0.o: /usr/include/sys/_types/_in_addr_t.h
X11_class0.o: /usr/include/sys/_types/_in_port_t.h
X11_class0.o: /usr/include/sys/_types/_ino_t.h
X11_class0.o: /usr/include/sys/_types/_ino64_t.h
X11_class0.o: /usr/include/sys/_types/_key_t.h
X11_class0.o: /usr/include/sys/_types/_nlink_t.h
X11_class0.o: /usr/include/sys/_types/_clock_t.h
X11_class0.o: /usr/include/sys/_types/_time_t.h
X11_class0.o: /usr/include/sys/_types/_useconds_t.h
X11_class0.o: /usr/include/sys/_types/_suseconds_t.h
X11_class0.o: /usr/include/sys/_types/_fd_def.h
X11_class0.o: /usr/include/sys/_types/_fd_setsize.h
X11_class0.o: /usr/include/sys/_types/_fd_set.h
X11_class0.o: /usr/include/sys/_types/_fd_clr.h
X11_class0.o: /usr/include/sys/_types/_fd_zero.h
X11_class0.o: /usr/include/sys/_types/_fd_isset.h
X11_class0.o: /usr/include/sys/_types/_fd_copy.h
X11_class0.o: /usr/include/sys/_pthread/_pthread_cond_t.h
X11_class0.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
X11_class0.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
X11_class0.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
X11_class0.o: /usr/include/sys/_pthread/_pthread_once_t.h
X11_class0.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
X11_class0.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
X11_class0.o: /usr/include/sys/_pthread/_pthread_t.h
X11_class0.o: /usr/include/sys/_pthread/_pthread_key_t.h
X11_class0.o: /usr/include/sys/_types/_fsblkcnt_t.h
X11_class0.o: /usr/include/sys/_types/_fsfilcnt_t.h /opt/X11/include/X11/X.h
X11_class0.o: /opt/X11/include/X11/Xfuncproto.h
X11_class0.o: /opt/X11/include/X11/Xosdefs.h /usr/include/stddef.h
X11_class0.o: /usr/include/sys/_types/_offsetof.h
X11_class0.o: /usr/include/sys/_types/_ptrdiff_t.h
X11_class0.o: /usr/include/sys/_types/_wint_t.h /opt/X11/include/X11/Xutil.h
X11_class0.o: /opt/X11/include/X11/keysym.h /opt/X11/include/X11/keysymdef.h
X11_class0.o: /opt/X11/include/X11/Xresource.h /opt/X11/include/X11/Xatom.h
X11_class0.o: ../../../.././include/ncarg/c.h cgmc.h default.h
X11_class0.o: ../../../.././include/ncarg/cgmdef.h text.h Xdefs.h ctrandef.h
X11_class0.o: translate.h Xcrm.h xresources.h ncaricon.bit
X11_class1.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
X11_class1.o: /usr/include/sys/_symbol_aliasing.h
X11_class1.o: /usr/include/sys/_posix_availability.h
X11_class1.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
X11_class1.o: /usr/include/_types.h /usr/include/sys/_types.h
X11_class1.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
X11_class1.o: /usr/include/sys/_pthread/_pthread_types.h
X11_class1.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
X11_class1.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
X11_class1.o: /usr/include/sys/_types/_int16_t.h
X11_class1.o: /usr/include/sys/_types/_int32_t.h
X11_class1.o: /usr/include/sys/_types/_int64_t.h
X11_class1.o: /usr/include/sys/_types/_u_int8_t.h
X11_class1.o: /usr/include/sys/_types/_u_int16_t.h
X11_class1.o: /usr/include/sys/_types/_u_int32_t.h
X11_class1.o: /usr/include/sys/_types/_u_int64_t.h
X11_class1.o: /usr/include/sys/_types/_intptr_t.h
X11_class1.o: /usr/include/sys/_types/_uintptr_t.h
X11_class1.o: /usr/include/sys/_types/_size_t.h
X11_class1.o: /usr/include/sys/_types/_null.h /usr/include/sys/stdio.h
X11_class1.o: /usr/include/sys/_types/_off_t.h
X11_class1.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
X11_class1.o: /usr/include/secure/_common.h /usr/include/errno.h
X11_class1.o: /usr/include/sys/errno.h /usr/include/sys/_types/_errno_t.h
X11_class1.o: /opt/X11/include/X11/Xlib.h /usr/include/sys/types.h
X11_class1.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
X11_class1.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
X11_class1.o: /usr/include/libkern/_OSByteOrder.h
X11_class1.o: /usr/include/libkern/i386/_OSByteOrder.h
X11_class1.o: /usr/include/sys/_types/_u_char.h
X11_class1.o: /usr/include/sys/_types/_u_short.h
X11_class1.o: /usr/include/sys/_types/_u_int.h
X11_class1.o: /usr/include/sys/_types/_caddr_t.h
X11_class1.o: /usr/include/sys/_types/_dev_t.h
X11_class1.o: /usr/include/sys/_types/_blkcnt_t.h
X11_class1.o: /usr/include/sys/_types/_blksize_t.h
X11_class1.o: /usr/include/sys/_types/_gid_t.h
X11_class1.o: /usr/include/sys/_types/_in_addr_t.h
X11_class1.o: /usr/include/sys/_types/_in_port_t.h
X11_class1.o: /usr/include/sys/_types/_ino_t.h
X11_class1.o: /usr/include/sys/_types/_ino64_t.h
X11_class1.o: /usr/include/sys/_types/_key_t.h
X11_class1.o: /usr/include/sys/_types/_mode_t.h
X11_class1.o: /usr/include/sys/_types/_nlink_t.h
X11_class1.o: /usr/include/sys/_types/_id_t.h
X11_class1.o: /usr/include/sys/_types/_pid_t.h
X11_class1.o: /usr/include/sys/_types/_uid_t.h
X11_class1.o: /usr/include/sys/_types/_clock_t.h
X11_class1.o: /usr/include/sys/_types/_time_t.h
X11_class1.o: /usr/include/sys/_types/_useconds_t.h
X11_class1.o: /usr/include/sys/_types/_suseconds_t.h
X11_class1.o: /usr/include/sys/_types/_rsize_t.h
X11_class1.o: /usr/include/sys/_types/_fd_def.h
X11_class1.o: /usr/include/sys/_types/_fd_setsize.h
X11_class1.o: /usr/include/sys/_types/_fd_set.h
X11_class1.o: /usr/include/sys/_types/_fd_clr.h
X11_class1.o: /usr/include/sys/_types/_fd_zero.h
X11_class1.o: /usr/include/sys/_types/_fd_isset.h
X11_class1.o: /usr/include/sys/_types/_fd_copy.h
X11_class1.o: /usr/include/sys/_pthread/_pthread_attr_t.h
X11_class1.o: /usr/include/sys/_pthread/_pthread_cond_t.h
X11_class1.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
X11_class1.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
X11_class1.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
X11_class1.o: /usr/include/sys/_pthread/_pthread_once_t.h
X11_class1.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
X11_class1.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
X11_class1.o: /usr/include/sys/_pthread/_pthread_t.h
X11_class1.o: /usr/include/sys/_pthread/_pthread_key_t.h
X11_class1.o: /usr/include/sys/_types/_fsblkcnt_t.h
X11_class1.o: /usr/include/sys/_types/_fsfilcnt_t.h /opt/X11/include/X11/X.h
X11_class1.o: /opt/X11/include/X11/Xfuncproto.h
X11_class1.o: /opt/X11/include/X11/Xosdefs.h /usr/include/stddef.h
X11_class1.o: /usr/include/sys/_types/_offsetof.h
X11_class1.o: /usr/include/sys/_types/_ptrdiff_t.h
X11_class1.o: /usr/include/sys/_types/_wchar_t.h
X11_class1.o: /usr/include/sys/_types/_wint_t.h /opt/X11/include/X11/Xutil.h
X11_class1.o: /opt/X11/include/X11/keysym.h /opt/X11/include/X11/keysymdef.h
X11_class1.o: ../../../.././include/ncarg/c.h cgmc.h
X11_class3.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
X11_class3.o: /usr/include/sys/_symbol_aliasing.h
X11_class3.o: /usr/include/sys/_posix_availability.h
X11_class3.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
X11_class3.o: /usr/include/_types.h /usr/include/sys/_types.h
X11_class3.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
X11_class3.o: /usr/include/sys/_pthread/_pthread_types.h
X11_class3.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
X11_class3.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
X11_class3.o: /usr/include/sys/_types/_int16_t.h
X11_class3.o: /usr/include/sys/_types/_int32_t.h
X11_class3.o: /usr/include/sys/_types/_int64_t.h
X11_class3.o: /usr/include/sys/_types/_u_int8_t.h
X11_class3.o: /usr/include/sys/_types/_u_int16_t.h
X11_class3.o: /usr/include/sys/_types/_u_int32_t.h
X11_class3.o: /usr/include/sys/_types/_u_int64_t.h
X11_class3.o: /usr/include/sys/_types/_intptr_t.h
X11_class3.o: /usr/include/sys/_types/_uintptr_t.h
X11_class3.o: /usr/include/sys/_types/_size_t.h
X11_class3.o: /usr/include/sys/_types/_null.h /usr/include/sys/stdio.h
X11_class3.o: /usr/include/sys/_types/_off_t.h
X11_class3.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
X11_class3.o: /usr/include/secure/_common.h /usr/include/errno.h
X11_class3.o: /usr/include/sys/errno.h /usr/include/sys/_types/_errno_t.h
X11_class3.o: /opt/X11/include/X11/Xlib.h /usr/include/sys/types.h
X11_class3.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
X11_class3.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
X11_class3.o: /usr/include/libkern/_OSByteOrder.h
X11_class3.o: /usr/include/libkern/i386/_OSByteOrder.h
X11_class3.o: /usr/include/sys/_types/_u_char.h
X11_class3.o: /usr/include/sys/_types/_u_short.h
X11_class3.o: /usr/include/sys/_types/_u_int.h
X11_class3.o: /usr/include/sys/_types/_caddr_t.h
X11_class3.o: /usr/include/sys/_types/_dev_t.h
X11_class3.o: /usr/include/sys/_types/_blkcnt_t.h
X11_class3.o: /usr/include/sys/_types/_blksize_t.h
X11_class3.o: /usr/include/sys/_types/_gid_t.h
X11_class3.o: /usr/include/sys/_types/_in_addr_t.h
X11_class3.o: /usr/include/sys/_types/_in_port_t.h
X11_class3.o: /usr/include/sys/_types/_ino_t.h
X11_class3.o: /usr/include/sys/_types/_ino64_t.h
X11_class3.o: /usr/include/sys/_types/_key_t.h
X11_class3.o: /usr/include/sys/_types/_mode_t.h
X11_class3.o: /usr/include/sys/_types/_nlink_t.h
X11_class3.o: /usr/include/sys/_types/_id_t.h
X11_class3.o: /usr/include/sys/_types/_pid_t.h
X11_class3.o: /usr/include/sys/_types/_uid_t.h
X11_class3.o: /usr/include/sys/_types/_clock_t.h
X11_class3.o: /usr/include/sys/_types/_time_t.h
X11_class3.o: /usr/include/sys/_types/_useconds_t.h
X11_class3.o: /usr/include/sys/_types/_suseconds_t.h
X11_class3.o: /usr/include/sys/_types/_rsize_t.h
X11_class3.o: /usr/include/sys/_types/_fd_def.h
X11_class3.o: /usr/include/sys/_types/_fd_setsize.h
X11_class3.o: /usr/include/sys/_types/_fd_set.h
X11_class3.o: /usr/include/sys/_types/_fd_clr.h
X11_class3.o: /usr/include/sys/_types/_fd_zero.h
X11_class3.o: /usr/include/sys/_types/_fd_isset.h
X11_class3.o: /usr/include/sys/_types/_fd_copy.h
X11_class3.o: /usr/include/sys/_pthread/_pthread_attr_t.h
X11_class3.o: /usr/include/sys/_pthread/_pthread_cond_t.h
X11_class3.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
X11_class3.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
X11_class3.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
X11_class3.o: /usr/include/sys/_pthread/_pthread_once_t.h
X11_class3.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
X11_class3.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
X11_class3.o: /usr/include/sys/_pthread/_pthread_t.h
X11_class3.o: /usr/include/sys/_pthread/_pthread_key_t.h
X11_class3.o: /usr/include/sys/_types/_fsblkcnt_t.h
X11_class3.o: /usr/include/sys/_types/_fsfilcnt_t.h /opt/X11/include/X11/X.h
X11_class3.o: /opt/X11/include/X11/Xfuncproto.h
X11_class3.o: /opt/X11/include/X11/Xosdefs.h /usr/include/stddef.h
X11_class3.o: /usr/include/sys/_types/_offsetof.h
X11_class3.o: /usr/include/sys/_types/_ptrdiff_t.h
X11_class3.o: /usr/include/sys/_types/_wchar_t.h
X11_class3.o: /usr/include/sys/_types/_wint_t.h /opt/X11/include/X11/Xutil.h
X11_class3.o: /opt/X11/include/X11/keysym.h /opt/X11/include/X11/keysymdef.h
X11_class3.o: ../../../.././include/ncarg/c.h default.h
X11_class3.o: ../../../.././include/ncarg/cgmdef.h cgmc.h text.h Xdefs.h
X11_class3.o: ctrandef.h translate.h /usr/include/stdlib.h
X11_class3.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
X11_class3.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
X11_class3.o: /usr/include/machine/_mcontext.h /usr/include/i386/_mcontext.h
X11_class3.o: /usr/include/mach/machine/_structs.h
X11_class3.o: /usr/include/mach/i386/_structs.h
X11_class3.o: /usr/include/sys/_types/_sigaltstack.h
X11_class3.o: /usr/include/sys/_types/_ucontext.h
X11_class3.o: /usr/include/sys/_types/_sigset_t.h /usr/include/sys/resource.h
X11_class3.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
X11_class3.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
X11_class3.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
X11_class3.o: /usr/include/_types/_uintmax_t.h
X11_class3.o: /usr/include/sys/_types/_timeval.h /usr/include/alloca.h
X11_class3.o: /usr/include/sys/_types/_ct_rune_t.h
X11_class3.o: /usr/include/sys/_types/_rune_t.h
X11_class4.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
X11_class4.o: /usr/include/sys/_symbol_aliasing.h
X11_class4.o: /usr/include/sys/_posix_availability.h
X11_class4.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
X11_class4.o: /usr/include/_types.h /usr/include/sys/_types.h
X11_class4.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
X11_class4.o: /usr/include/sys/_pthread/_pthread_types.h
X11_class4.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
X11_class4.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
X11_class4.o: /usr/include/sys/_types/_int16_t.h
X11_class4.o: /usr/include/sys/_types/_int32_t.h
X11_class4.o: /usr/include/sys/_types/_int64_t.h
X11_class4.o: /usr/include/sys/_types/_u_int8_t.h
X11_class4.o: /usr/include/sys/_types/_u_int16_t.h
X11_class4.o: /usr/include/sys/_types/_u_int32_t.h
X11_class4.o: /usr/include/sys/_types/_u_int64_t.h
X11_class4.o: /usr/include/sys/_types/_intptr_t.h
X11_class4.o: /usr/include/sys/_types/_uintptr_t.h
X11_class4.o: /usr/include/sys/_types/_size_t.h
X11_class4.o: /usr/include/sys/_types/_null.h /usr/include/sys/stdio.h
X11_class4.o: /usr/include/sys/_types/_off_t.h
X11_class4.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
X11_class4.o: /usr/include/secure/_common.h /usr/include/stdlib.h
X11_class4.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
X11_class4.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
X11_class4.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
X11_class4.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
X11_class4.o: /usr/include/i386/_mcontext.h
X11_class4.o: /usr/include/mach/machine/_structs.h
X11_class4.o: /usr/include/mach/i386/_structs.h
X11_class4.o: /usr/include/sys/_pthread/_pthread_attr_t.h
X11_class4.o: /usr/include/sys/_types/_sigaltstack.h
X11_class4.o: /usr/include/sys/_types/_ucontext.h
X11_class4.o: /usr/include/sys/_types/_sigset_t.h
X11_class4.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
X11_class4.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
X11_class4.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
X11_class4.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
X11_class4.o: /usr/include/_types/_uintmax_t.h
X11_class4.o: /usr/include/sys/_types/_timeval.h
X11_class4.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
X11_class4.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
X11_class4.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
X11_class4.o: /usr/include/sys/_types/_ct_rune_t.h
X11_class4.o: /usr/include/sys/_types/_rune_t.h
X11_class4.o: /usr/include/sys/_types/_wchar_t.h
X11_class4.o: /usr/include/sys/_types/_dev_t.h
X11_class4.o: /usr/include/sys/_types/_mode_t.h /usr/include/math.h
X11_class4.o: /usr/include/errno.h /usr/include/sys/errno.h
X11_class4.o: /usr/include/sys/_types/_errno_t.h /opt/X11/include/X11/Xlib.h
X11_class4.o: /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
X11_class4.o: /usr/include/sys/_types/_u_short.h
X11_class4.o: /usr/include/sys/_types/_u_int.h
X11_class4.o: /usr/include/sys/_types/_caddr_t.h
X11_class4.o: /usr/include/sys/_types/_blkcnt_t.h
X11_class4.o: /usr/include/sys/_types/_blksize_t.h
X11_class4.o: /usr/include/sys/_types/_gid_t.h
X11_class4.o: /usr/include/sys/_types/_in_addr_t.h
X11_class4.o: /usr/include/sys/_types/_in_port_t.h
X11_class4.o: /usr/include/sys/_types/_ino_t.h
X11_class4.o: /usr/include/sys/_types/_ino64_t.h
X11_class4.o: /usr/include/sys/_types/_key_t.h
X11_class4.o: /usr/include/sys/_types/_nlink_t.h
X11_class4.o: /usr/include/sys/_types/_clock_t.h
X11_class4.o: /usr/include/sys/_types/_time_t.h
X11_class4.o: /usr/include/sys/_types/_useconds_t.h
X11_class4.o: /usr/include/sys/_types/_suseconds_t.h
X11_class4.o: /usr/include/sys/_types/_rsize_t.h
X11_class4.o: /usr/include/sys/_types/_fd_def.h
X11_class4.o: /usr/include/sys/_types/_fd_setsize.h
X11_class4.o: /usr/include/sys/_types/_fd_set.h
X11_class4.o: /usr/include/sys/_types/_fd_clr.h
X11_class4.o: /usr/include/sys/_types/_fd_zero.h
X11_class4.o: /usr/include/sys/_types/_fd_isset.h
X11_class4.o: /usr/include/sys/_types/_fd_copy.h
X11_class4.o: /usr/include/sys/_pthread/_pthread_cond_t.h
X11_class4.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
X11_class4.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
X11_class4.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
X11_class4.o: /usr/include/sys/_pthread/_pthread_once_t.h
X11_class4.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
X11_class4.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
X11_class4.o: /usr/include/sys/_pthread/_pthread_t.h
X11_class4.o: /usr/include/sys/_pthread/_pthread_key_t.h
X11_class4.o: /usr/include/sys/_types/_fsblkcnt_t.h
X11_class4.o: /usr/include/sys/_types/_fsfilcnt_t.h /opt/X11/include/X11/X.h
X11_class4.o: /opt/X11/include/X11/Xfuncproto.h
X11_class4.o: /opt/X11/include/X11/Xosdefs.h /usr/include/stddef.h
X11_class4.o: /usr/include/sys/_types/_offsetof.h
X11_class4.o: /usr/include/sys/_types/_ptrdiff_t.h
X11_class4.o: /usr/include/sys/_types/_wint_t.h /opt/X11/include/X11/Xutil.h
X11_class4.o: /opt/X11/include/X11/keysym.h /opt/X11/include/X11/keysymdef.h
X11_class4.o: ../../../.././include/ncarg/c.h
X11_class4.o: ../../../.././include/ncarg/cgm_tools.h
X11_class4.o: ../../../.././include/ncarg/cgmdef.h cgmc.h default.h text.h
X11_class4.o: Xdefs.h ctrandef.h Xcrm.h soft_fill.h translate.h
X11_class6.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
X11_class6.o: /usr/include/sys/_symbol_aliasing.h
X11_class6.o: /usr/include/sys/_posix_availability.h
X11_class6.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
X11_class6.o: /usr/include/_types.h /usr/include/sys/_types.h
X11_class6.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
X11_class6.o: /usr/include/sys/_pthread/_pthread_types.h
X11_class6.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
X11_class6.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
X11_class6.o: /usr/include/sys/_types/_int16_t.h
X11_class6.o: /usr/include/sys/_types/_int32_t.h
X11_class6.o: /usr/include/sys/_types/_int64_t.h
X11_class6.o: /usr/include/sys/_types/_u_int8_t.h
X11_class6.o: /usr/include/sys/_types/_u_int16_t.h
X11_class6.o: /usr/include/sys/_types/_u_int32_t.h
X11_class6.o: /usr/include/sys/_types/_u_int64_t.h
X11_class6.o: /usr/include/sys/_types/_intptr_t.h
X11_class6.o: /usr/include/sys/_types/_uintptr_t.h
X11_class6.o: /usr/include/sys/_types/_size_t.h
X11_class6.o: /usr/include/sys/_types/_null.h /usr/include/sys/stdio.h
X11_class6.o: /usr/include/sys/_types/_off_t.h
X11_class6.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
X11_class6.o: /usr/include/secure/_common.h /opt/X11/include/X11/Xlib.h
X11_class6.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
X11_class6.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
X11_class6.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
X11_class6.o: /usr/include/libkern/i386/_OSByteOrder.h
X11_class6.o: /usr/include/sys/_types/_u_char.h
X11_class6.o: /usr/include/sys/_types/_u_short.h
X11_class6.o: /usr/include/sys/_types/_u_int.h
X11_class6.o: /usr/include/sys/_types/_caddr_t.h
X11_class6.o: /usr/include/sys/_types/_dev_t.h
X11_class6.o: /usr/include/sys/_types/_blkcnt_t.h
X11_class6.o: /usr/include/sys/_types/_blksize_t.h
X11_class6.o: /usr/include/sys/_types/_gid_t.h
X11_class6.o: /usr/include/sys/_types/_in_addr_t.h
X11_class6.o: /usr/include/sys/_types/_in_port_t.h
X11_class6.o: /usr/include/sys/_types/_ino_t.h
X11_class6.o: /usr/include/sys/_types/_ino64_t.h
X11_class6.o: /usr/include/sys/_types/_key_t.h
X11_class6.o: /usr/include/sys/_types/_mode_t.h
X11_class6.o: /usr/include/sys/_types/_nlink_t.h
X11_class6.o: /usr/include/sys/_types/_id_t.h
X11_class6.o: /usr/include/sys/_types/_pid_t.h
X11_class6.o: /usr/include/sys/_types/_uid_t.h
X11_class6.o: /usr/include/sys/_types/_clock_t.h
X11_class6.o: /usr/include/sys/_types/_time_t.h
X11_class6.o: /usr/include/sys/_types/_useconds_t.h
X11_class6.o: /usr/include/sys/_types/_suseconds_t.h
X11_class6.o: /usr/include/sys/_types/_rsize_t.h
X11_class6.o: /usr/include/sys/_types/_errno_t.h
X11_class6.o: /usr/include/sys/_types/_fd_def.h
X11_class6.o: /usr/include/sys/_types/_fd_setsize.h
X11_class6.o: /usr/include/sys/_types/_fd_set.h
X11_class6.o: /usr/include/sys/_types/_fd_clr.h
X11_class6.o: /usr/include/sys/_types/_fd_zero.h
X11_class6.o: /usr/include/sys/_types/_fd_isset.h
X11_class6.o: /usr/include/sys/_types/_fd_copy.h
X11_class6.o: /usr/include/sys/_pthread/_pthread_attr_t.h
X11_class6.o: /usr/include/sys/_pthread/_pthread_cond_t.h
X11_class6.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
X11_class6.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
X11_class6.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
X11_class6.o: /usr/include/sys/_pthread/_pthread_once_t.h
X11_class6.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
X11_class6.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
X11_class6.o: /usr/include/sys/_pthread/_pthread_t.h
X11_class6.o: /usr/include/sys/_pthread/_pthread_key_t.h
X11_class6.o: /usr/include/sys/_types/_fsblkcnt_t.h
X11_class6.o: /usr/include/sys/_types/_fsfilcnt_t.h /opt/X11/include/X11/X.h
X11_class6.o: /opt/X11/include/X11/Xfuncproto.h
X11_class6.o: /opt/X11/include/X11/Xosdefs.h /usr/include/stddef.h
X11_class6.o: /usr/include/sys/_types/_offsetof.h
X11_class6.o: /usr/include/sys/_types/_ptrdiff_t.h
X11_class6.o: /usr/include/sys/_types/_wchar_t.h
X11_class6.o: /usr/include/sys/_types/_wint_t.h /opt/X11/include/X11/Xutil.h
X11_class6.o: /opt/X11/include/X11/keysym.h /opt/X11/include/X11/keysymdef.h
X11_class6.o: ../../../.././include/ncarg/c.h cgmc.h
X11_class7.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
X11_class7.o: /usr/include/sys/_symbol_aliasing.h
X11_class7.o: /usr/include/sys/_posix_availability.h
X11_class7.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
X11_class7.o: /usr/include/_types.h /usr/include/sys/_types.h
X11_class7.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
X11_class7.o: /usr/include/sys/_pthread/_pthread_types.h
X11_class7.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
X11_class7.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
X11_class7.o: /usr/include/sys/_types/_int16_t.h
X11_class7.o: /usr/include/sys/_types/_int32_t.h
X11_class7.o: /usr/include/sys/_types/_int64_t.h
X11_class7.o: /usr/include/sys/_types/_u_int8_t.h
X11_class7.o: /usr/include/sys/_types/_u_int16_t.h
X11_class7.o: /usr/include/sys/_types/_u_int32_t.h
X11_class7.o: /usr/include/sys/_types/_u_int64_t.h
X11_class7.o: /usr/include/sys/_types/_intptr_t.h
X11_class7.o: /usr/include/sys/_types/_uintptr_t.h
X11_class7.o: /usr/include/sys/_types/_size_t.h
X11_class7.o: /usr/include/sys/_types/_null.h /usr/include/sys/stdio.h
X11_class7.o: /usr/include/sys/_types/_off_t.h
X11_class7.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
X11_class7.o: /usr/include/secure/_common.h /usr/include/errno.h
X11_class7.o: /usr/include/sys/errno.h /usr/include/sys/_types/_errno_t.h
X11_class7.o: /opt/X11/include/X11/Xlib.h /usr/include/sys/types.h
X11_class7.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
X11_class7.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
X11_class7.o: /usr/include/libkern/_OSByteOrder.h
X11_class7.o: /usr/include/libkern/i386/_OSByteOrder.h
X11_class7.o: /usr/include/sys/_types/_u_char.h
X11_class7.o: /usr/include/sys/_types/_u_short.h
X11_class7.o: /usr/include/sys/_types/_u_int.h
X11_class7.o: /usr/include/sys/_types/_caddr_t.h
X11_class7.o: /usr/include/sys/_types/_dev_t.h
X11_class7.o: /usr/include/sys/_types/_blkcnt_t.h
X11_class7.o: /usr/include/sys/_types/_blksize_t.h
X11_class7.o: /usr/include/sys/_types/_gid_t.h
X11_class7.o: /usr/include/sys/_types/_in_addr_t.h
X11_class7.o: /usr/include/sys/_types/_in_port_t.h
X11_class7.o: /usr/include/sys/_types/_ino_t.h
X11_class7.o: /usr/include/sys/_types/_ino64_t.h
X11_class7.o: /usr/include/sys/_types/_key_t.h
X11_class7.o: /usr/include/sys/_types/_mode_t.h
X11_class7.o: /usr/include/sys/_types/_nlink_t.h
X11_class7.o: /usr/include/sys/_types/_id_t.h
X11_class7.o: /usr/include/sys/_types/_pid_t.h
X11_class7.o: /usr/include/sys/_types/_uid_t.h
X11_class7.o: /usr/include/sys/_types/_clock_t.h
X11_class7.o: /usr/include/sys/_types/_time_t.h
X11_class7.o: /usr/include/sys/_types/_useconds_t.h
X11_class7.o: /usr/include/sys/_types/_suseconds_t.h
X11_class7.o: /usr/include/sys/_types/_rsize_t.h
X11_class7.o: /usr/include/sys/_types/_fd_def.h
X11_class7.o: /usr/include/sys/_types/_fd_setsize.h
X11_class7.o: /usr/include/sys/_types/_fd_set.h
X11_class7.o: /usr/include/sys/_types/_fd_clr.h
X11_class7.o: /usr/include/sys/_types/_fd_zero.h
X11_class7.o: /usr/include/sys/_types/_fd_isset.h
X11_class7.o: /usr/include/sys/_types/_fd_copy.h
X11_class7.o: /usr/include/sys/_pthread/_pthread_attr_t.h
X11_class7.o: /usr/include/sys/_pthread/_pthread_cond_t.h
X11_class7.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
X11_class7.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
X11_class7.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
X11_class7.o: /usr/include/sys/_pthread/_pthread_once_t.h
X11_class7.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
X11_class7.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
X11_class7.o: /usr/include/sys/_pthread/_pthread_t.h
X11_class7.o: /usr/include/sys/_pthread/_pthread_key_t.h
X11_class7.o: /usr/include/sys/_types/_fsblkcnt_t.h
X11_class7.o: /usr/include/sys/_types/_fsfilcnt_t.h /opt/X11/include/X11/X.h
X11_class7.o: /opt/X11/include/X11/Xfuncproto.h
X11_class7.o: /opt/X11/include/X11/Xosdefs.h /usr/include/stddef.h
X11_class7.o: /usr/include/sys/_types/_offsetof.h
X11_class7.o: /usr/include/sys/_types/_ptrdiff_t.h
X11_class7.o: /usr/include/sys/_types/_wchar_t.h
X11_class7.o: /usr/include/sys/_types/_wint_t.h /opt/X11/include/X11/Xutil.h
X11_class7.o: /opt/X11/include/X11/keysym.h /opt/X11/include/X11/keysymdef.h
X11_class7.o: ../../../.././include/ncarg/c.h cgmc.h
Xcrm.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
Xcrm.o: /usr/include/sys/_symbol_aliasing.h
Xcrm.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
Xcrm.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
Xcrm.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
Xcrm.o: /usr/include/i386/_types.h /usr/include/sys/_pthread/_pthread_types.h
Xcrm.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
Xcrm.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
Xcrm.o: /usr/include/sys/_types/_int16_t.h /usr/include/sys/_types/_int32_t.h
Xcrm.o: /usr/include/sys/_types/_int64_t.h
Xcrm.o: /usr/include/sys/_types/_u_int8_t.h
Xcrm.o: /usr/include/sys/_types/_u_int16_t.h
Xcrm.o: /usr/include/sys/_types/_u_int32_t.h
Xcrm.o: /usr/include/sys/_types/_u_int64_t.h
Xcrm.o: /usr/include/sys/_types/_intptr_t.h
Xcrm.o: /usr/include/sys/_types/_uintptr_t.h
Xcrm.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
Xcrm.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
Xcrm.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
Xcrm.o: /usr/include/secure/_common.h /usr/include/stdlib.h
Xcrm.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
Xcrm.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
Xcrm.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
Xcrm.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
Xcrm.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
Xcrm.o: /usr/include/mach/i386/_structs.h
Xcrm.o: /usr/include/sys/_pthread/_pthread_attr_t.h
Xcrm.o: /usr/include/sys/_types/_sigaltstack.h
Xcrm.o: /usr/include/sys/_types/_ucontext.h
Xcrm.o: /usr/include/sys/_types/_sigset_t.h /usr/include/sys/_types/_uid_t.h
Xcrm.o: /usr/include/sys/resource.h /usr/include/stdint.h
Xcrm.o: /usr/include/_types/_uint8_t.h /usr/include/_types/_uint16_t.h
Xcrm.o: /usr/include/_types/_uint32_t.h /usr/include/_types/_uint64_t.h
Xcrm.o: /usr/include/_types/_intmax_t.h /usr/include/_types/_uintmax_t.h
Xcrm.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
Xcrm.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
Xcrm.o: /usr/include/libkern/_OSByteOrder.h
Xcrm.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
Xcrm.o: /usr/include/sys/_types/_ct_rune_t.h
Xcrm.o: /usr/include/sys/_types/_rune_t.h /usr/include/sys/_types/_wchar_t.h
Xcrm.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_mode_t.h
Xcrm.o: /usr/include/errno.h /usr/include/sys/errno.h
Xcrm.o: /usr/include/sys/_types/_errno_t.h /usr/include/math.h
Xcrm.o: /opt/X11/include/X11/Xlib.h /usr/include/sys/types.h
Xcrm.o: /usr/include/sys/_types/_u_char.h /usr/include/sys/_types/_u_short.h
Xcrm.o: /usr/include/sys/_types/_u_int.h /usr/include/sys/_types/_caddr_t.h
Xcrm.o: /usr/include/sys/_types/_blkcnt_t.h
Xcrm.o: /usr/include/sys/_types/_blksize_t.h /usr/include/sys/_types/_gid_t.h
Xcrm.o: /usr/include/sys/_types/_in_addr_t.h
Xcrm.o: /usr/include/sys/_types/_in_port_t.h /usr/include/sys/_types/_ino_t.h
Xcrm.o: /usr/include/sys/_types/_ino64_t.h /usr/include/sys/_types/_key_t.h
Xcrm.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_clock_t.h
Xcrm.o: /usr/include/sys/_types/_time_t.h
Xcrm.o: /usr/include/sys/_types/_useconds_t.h
Xcrm.o: /usr/include/sys/_types/_suseconds_t.h
Xcrm.o: /usr/include/sys/_types/_rsize_t.h /usr/include/sys/_types/_fd_def.h
Xcrm.o: /usr/include/sys/_types/_fd_setsize.h
Xcrm.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
Xcrm.o: /usr/include/sys/_types/_fd_zero.h
Xcrm.o: /usr/include/sys/_types/_fd_isset.h
Xcrm.o: /usr/include/sys/_types/_fd_copy.h
Xcrm.o: /usr/include/sys/_pthread/_pthread_cond_t.h
Xcrm.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
Xcrm.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
Xcrm.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
Xcrm.o: /usr/include/sys/_pthread/_pthread_once_t.h
Xcrm.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
Xcrm.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
Xcrm.o: /usr/include/sys/_pthread/_pthread_t.h
Xcrm.o: /usr/include/sys/_pthread/_pthread_key_t.h
Xcrm.o: /usr/include/sys/_types/_fsblkcnt_t.h
Xcrm.o: /usr/include/sys/_types/_fsfilcnt_t.h /opt/X11/include/X11/X.h
Xcrm.o: /opt/X11/include/X11/Xfuncproto.h /opt/X11/include/X11/Xosdefs.h
Xcrm.o: /usr/include/stddef.h /usr/include/sys/_types/_offsetof.h
Xcrm.o: /usr/include/sys/_types/_ptrdiff_t.h
Xcrm.o: /usr/include/sys/_types/_wint_t.h /opt/X11/include/X11/Xutil.h
Xcrm.o: /opt/X11/include/X11/keysym.h /opt/X11/include/X11/keysymdef.h
Xcrm.o: ../../../.././include/ncarg/c.h cgmc.h devices.h Xdefs.h Xcrm.h
Xcrm.o: default.h ../../../.././include/ncarg/cgmdef.h text.h ctrandef.h
xresources.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
xresources.o: /usr/include/sys/_symbol_aliasing.h
xresources.o: /usr/include/sys/_posix_availability.h
xresources.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
xresources.o: /usr/include/_types.h /usr/include/sys/_types.h
xresources.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
xresources.o: /usr/include/sys/_pthread/_pthread_types.h
xresources.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
xresources.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
xresources.o: /usr/include/sys/_types/_int16_t.h
xresources.o: /usr/include/sys/_types/_int32_t.h
xresources.o: /usr/include/sys/_types/_int64_t.h
xresources.o: /usr/include/sys/_types/_u_int8_t.h
xresources.o: /usr/include/sys/_types/_u_int16_t.h
xresources.o: /usr/include/sys/_types/_u_int32_t.h
xresources.o: /usr/include/sys/_types/_u_int64_t.h
xresources.o: /usr/include/sys/_types/_intptr_t.h
xresources.o: /usr/include/sys/_types/_uintptr_t.h
xresources.o: /usr/include/sys/_types/_size_t.h
xresources.o: /usr/include/sys/_types/_null.h /usr/include/sys/stdio.h
xresources.o: /usr/include/sys/_types/_off_t.h
xresources.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
xresources.o: /usr/include/secure/_common.h /usr/include/stdlib.h
xresources.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
xresources.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
xresources.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
xresources.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
xresources.o: /usr/include/i386/_mcontext.h
xresources.o: /usr/include/mach/machine/_structs.h
xresources.o: /usr/include/mach/i386/_structs.h
xresources.o: /usr/include/sys/_pthread/_pthread_attr_t.h
xresources.o: /usr/include/sys/_types/_sigaltstack.h
xresources.o: /usr/include/sys/_types/_ucontext.h
xresources.o: /usr/include/sys/_types/_sigset_t.h
xresources.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
xresources.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
xresources.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
xresources.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
xresources.o: /usr/include/_types/_uintmax_t.h
xresources.o: /usr/include/sys/_types/_timeval.h
xresources.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
xresources.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
xresources.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
xresources.o: /usr/include/sys/_types/_ct_rune_t.h
xresources.o: /usr/include/sys/_types/_rune_t.h
xresources.o: /usr/include/sys/_types/_wchar_t.h
xresources.o: /usr/include/sys/_types/_dev_t.h
xresources.o: /usr/include/sys/_types/_mode_t.h /usr/include/string.h
xresources.o: /usr/include/sys/_types/_rsize_t.h
xresources.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
xresources.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
xresources.o: /opt/X11/include/X11/Xlib.h /usr/include/sys/types.h
xresources.o: /usr/include/sys/_types/_u_char.h
xresources.o: /usr/include/sys/_types/_u_short.h
xresources.o: /usr/include/sys/_types/_u_int.h
xresources.o: /usr/include/sys/_types/_caddr_t.h
xresources.o: /usr/include/sys/_types/_blkcnt_t.h
xresources.o: /usr/include/sys/_types/_blksize_t.h
xresources.o: /usr/include/sys/_types/_gid_t.h
xresources.o: /usr/include/sys/_types/_in_addr_t.h
xresources.o: /usr/include/sys/_types/_in_port_t.h
xresources.o: /usr/include/sys/_types/_ino_t.h
xresources.o: /usr/include/sys/_types/_ino64_t.h
xresources.o: /usr/include/sys/_types/_key_t.h
xresources.o: /usr/include/sys/_types/_nlink_t.h
xresources.o: /usr/include/sys/_types/_clock_t.h
xresources.o: /usr/include/sys/_types/_time_t.h
xresources.o: /usr/include/sys/_types/_useconds_t.h
xresources.o: /usr/include/sys/_types/_suseconds_t.h
xresources.o: /usr/include/sys/_types/_fd_def.h
xresources.o: /usr/include/sys/_types/_fd_setsize.h
xresources.o: /usr/include/sys/_types/_fd_set.h
xresources.o: /usr/include/sys/_types/_fd_clr.h
xresources.o: /usr/include/sys/_types/_fd_zero.h
xresources.o: /usr/include/sys/_types/_fd_isset.h
xresources.o: /usr/include/sys/_types/_fd_copy.h
xresources.o: /usr/include/sys/_pthread/_pthread_cond_t.h
xresources.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
xresources.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
xresources.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
xresources.o: /usr/include/sys/_pthread/_pthread_once_t.h
xresources.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
xresources.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
xresources.o: /usr/include/sys/_pthread/_pthread_t.h
xresources.o: /usr/include/sys/_pthread/_pthread_key_t.h
xresources.o: /usr/include/sys/_types/_fsblkcnt_t.h
xresources.o: /usr/include/sys/_types/_fsfilcnt_t.h /opt/X11/include/X11/X.h
xresources.o: /opt/X11/include/X11/Xfuncproto.h
xresources.o: /opt/X11/include/X11/Xosdefs.h /usr/include/stddef.h
xresources.o: /usr/include/sys/_types/_offsetof.h
xresources.o: /usr/include/sys/_types/_ptrdiff_t.h
xresources.o: /usr/include/sys/_types/_wint_t.h
xresources.o: /opt/X11/include/X11/Xresource.h xresources.h
clear_text.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
clear_text.o: /usr/include/sys/_symbol_aliasing.h
clear_text.o: /usr/include/sys/_posix_availability.h
clear_text.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
clear_text.o: /usr/include/_types.h /usr/include/sys/_types.h
clear_text.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
clear_text.o: /usr/include/sys/_pthread/_pthread_types.h
clear_text.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
clear_text.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
clear_text.o: /usr/include/sys/_types/_int16_t.h
clear_text.o: /usr/include/sys/_types/_int32_t.h
clear_text.o: /usr/include/sys/_types/_int64_t.h
clear_text.o: /usr/include/sys/_types/_u_int8_t.h
clear_text.o: /usr/include/sys/_types/_u_int16_t.h
clear_text.o: /usr/include/sys/_types/_u_int32_t.h
clear_text.o: /usr/include/sys/_types/_u_int64_t.h
clear_text.o: /usr/include/sys/_types/_intptr_t.h
clear_text.o: /usr/include/sys/_types/_uintptr_t.h
clear_text.o: /usr/include/sys/_types/_size_t.h
clear_text.o: /usr/include/sys/_types/_null.h /usr/include/sys/stdio.h
clear_text.o: /usr/include/sys/_types/_off_t.h
clear_text.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
clear_text.o: /usr/include/secure/_common.h /usr/include/sys/types.h
clear_text.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
clear_text.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
clear_text.o: /usr/include/libkern/_OSByteOrder.h
clear_text.o: /usr/include/libkern/i386/_OSByteOrder.h
clear_text.o: /usr/include/sys/_types/_u_char.h
clear_text.o: /usr/include/sys/_types/_u_short.h
clear_text.o: /usr/include/sys/_types/_u_int.h
clear_text.o: /usr/include/sys/_types/_caddr_t.h
clear_text.o: /usr/include/sys/_types/_dev_t.h
clear_text.o: /usr/include/sys/_types/_blkcnt_t.h
clear_text.o: /usr/include/sys/_types/_blksize_t.h
clear_text.o: /usr/include/sys/_types/_gid_t.h
clear_text.o: /usr/include/sys/_types/_in_addr_t.h
clear_text.o: /usr/include/sys/_types/_in_port_t.h
clear_text.o: /usr/include/sys/_types/_ino_t.h
clear_text.o: /usr/include/sys/_types/_ino64_t.h
clear_text.o: /usr/include/sys/_types/_key_t.h
clear_text.o: /usr/include/sys/_types/_mode_t.h
clear_text.o: /usr/include/sys/_types/_nlink_t.h
clear_text.o: /usr/include/sys/_types/_id_t.h
clear_text.o: /usr/include/sys/_types/_pid_t.h
clear_text.o: /usr/include/sys/_types/_uid_t.h
clear_text.o: /usr/include/sys/_types/_clock_t.h
clear_text.o: /usr/include/sys/_types/_time_t.h
clear_text.o: /usr/include/sys/_types/_useconds_t.h
clear_text.o: /usr/include/sys/_types/_suseconds_t.h
clear_text.o: /usr/include/sys/_types/_rsize_t.h
clear_text.o: /usr/include/sys/_types/_errno_t.h
clear_text.o: /usr/include/sys/_types/_fd_def.h
clear_text.o: /usr/include/sys/_types/_fd_setsize.h
clear_text.o: /usr/include/sys/_types/_fd_set.h
clear_text.o: /usr/include/sys/_types/_fd_clr.h
clear_text.o: /usr/include/sys/_types/_fd_zero.h
clear_text.o: /usr/include/sys/_types/_fd_isset.h
clear_text.o: /usr/include/sys/_types/_fd_copy.h
clear_text.o: /usr/include/sys/_pthread/_pthread_attr_t.h
clear_text.o: /usr/include/sys/_pthread/_pthread_cond_t.h
clear_text.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
clear_text.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
clear_text.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
clear_text.o: /usr/include/sys/_pthread/_pthread_once_t.h
clear_text.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
clear_text.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
clear_text.o: /usr/include/sys/_pthread/_pthread_t.h
clear_text.o: /usr/include/sys/_pthread/_pthread_key_t.h
clear_text.o: /usr/include/sys/_types/_fsblkcnt_t.h
clear_text.o: /usr/include/sys/_types/_fsfilcnt_t.h
clear_text.o: ../../../.././include/ncarg/c.h
clear_text.o: ../../../.././include/ncarg/cgm_tools.h
clear_text.o: ../../../.././include/ncarg/cgmdef.h cgmc.h text.h default.h
devices.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
devices.o: /usr/include/sys/_symbol_aliasing.h
devices.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
devices.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
devices.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
devices.o: /usr/include/i386/_types.h
devices.o: /usr/include/sys/_pthread/_pthread_types.h
devices.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
devices.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
devices.o: /usr/include/sys/_types/_int16_t.h
devices.o: /usr/include/sys/_types/_int32_t.h
devices.o: /usr/include/sys/_types/_int64_t.h
devices.o: /usr/include/sys/_types/_u_int8_t.h
devices.o: /usr/include/sys/_types/_u_int16_t.h
devices.o: /usr/include/sys/_types/_u_int32_t.h
devices.o: /usr/include/sys/_types/_u_int64_t.h
devices.o: /usr/include/sys/_types/_intptr_t.h
devices.o: /usr/include/sys/_types/_uintptr_t.h
devices.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
devices.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
devices.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
devices.o: /usr/include/secure/_common.h
devices.o: ../../../.././include/ncarg/cgm_tools.h
devices.o: ../../../.././include/ncarg/c.h /usr/include/sys/types.h
devices.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
devices.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
devices.o: /usr/include/libkern/_OSByteOrder.h
devices.o: /usr/include/libkern/i386/_OSByteOrder.h
devices.o: /usr/include/sys/_types/_u_char.h
devices.o: /usr/include/sys/_types/_u_short.h
devices.o: /usr/include/sys/_types/_u_int.h
devices.o: /usr/include/sys/_types/_caddr_t.h
devices.o: /usr/include/sys/_types/_dev_t.h
devices.o: /usr/include/sys/_types/_blkcnt_t.h
devices.o: /usr/include/sys/_types/_blksize_t.h
devices.o: /usr/include/sys/_types/_gid_t.h
devices.o: /usr/include/sys/_types/_in_addr_t.h
devices.o: /usr/include/sys/_types/_in_port_t.h
devices.o: /usr/include/sys/_types/_ino_t.h
devices.o: /usr/include/sys/_types/_ino64_t.h
devices.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
devices.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
devices.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
devices.o: /usr/include/sys/_types/_clock_t.h
devices.o: /usr/include/sys/_types/_time_t.h
devices.o: /usr/include/sys/_types/_useconds_t.h
devices.o: /usr/include/sys/_types/_suseconds_t.h
devices.o: /usr/include/sys/_types/_rsize_t.h
devices.o: /usr/include/sys/_types/_errno_t.h
devices.o: /usr/include/sys/_types/_fd_def.h
devices.o: /usr/include/sys/_types/_fd_setsize.h
devices.o: /usr/include/sys/_types/_fd_set.h
devices.o: /usr/include/sys/_types/_fd_clr.h
devices.o: /usr/include/sys/_types/_fd_zero.h
devices.o: /usr/include/sys/_types/_fd_isset.h
devices.o: /usr/include/sys/_types/_fd_copy.h
devices.o: /usr/include/sys/_pthread/_pthread_attr_t.h
devices.o: /usr/include/sys/_pthread/_pthread_cond_t.h
devices.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
devices.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
devices.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
devices.o: /usr/include/sys/_pthread/_pthread_once_t.h
devices.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
devices.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
devices.o: /usr/include/sys/_pthread/_pthread_t.h
devices.o: /usr/include/sys/_pthread/_pthread_key_t.h
devices.o: /usr/include/sys/_types/_fsblkcnt_t.h
devices.o: /usr/include/sys/_types/_fsfilcnt_t.h
devices.o: ../../../.././include/ncarg/cgmdef.h cgmc.h devices.h
