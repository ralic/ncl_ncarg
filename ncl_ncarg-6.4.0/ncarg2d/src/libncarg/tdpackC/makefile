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
CURRENT_DIR		= ./ncarg2d/src/libncarg/tdpackC

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
#       $Id: yMakefile,v 1.5 2003-11-24 21:03:43 kennison Exp $
#

MYNAME  = tdpackC

SRCS =  c_tdclrs.c c_tddtri.c c_tdez2d.c c_tdez3d.c c_tdgeti.c c_tdgetr.c  	c_tdgrds.c c_tdgrid.c c_tdgtrs.c c_tdinit.c c_tditri.c c_tdlbla.c  	c_tdlbls.c c_tdline.c c_tdlnpa.c c_tdmtri.c c_tdotri.c c_tdpara.c  	c_tdplch.c c_tdprpa.c c_tdprpi.c c_tdprpt.c c_tdseti.c c_tdsetr.c  	c_tdsort.c c_tdstri.c c_tdstrs.c c_tdctri.c c_tdlndp.c c_tdlpdp.c  	c_tdcurv.c c_tdcudp.c

OBJS =  c_tdclrs.o c_tddtri.o c_tdez2d.o c_tdez3d.o c_tdgeti.o c_tdgetr.o  	c_tdgrds.o c_tdgrid.o c_tdgtrs.o c_tdinit.o c_tditri.o c_tdlbla.o  	c_tdlbls.o c_tdline.o c_tdlnpa.o c_tdmtri.o c_tdotri.o c_tdpara.o  	c_tdplch.o c_tdprpa.o c_tdprpi.o c_tdprpt.o c_tdseti.o c_tdsetr.o  	c_tdsort.o c_tdstri.o c_tdstrs.o c_tdctri.o c_tdlndp.o c_tdlpdp.o  	c_tdcurv.o c_tdcudp.o

EXTRA_CDEFINES = $(OS_DEF)

all::

all-local::../libncarg.a

../libncarg.a:: $(OBJS)
	$(AR_REG) ../libncarg.a $(OBJS)

all::

clean-local::
	-@$(RM) $(OBJS)

all::

all-local::../libNGmisc.a

../libNGmisc.a:: $(OBJS)
	$(AR_REG) ../libNGmisc.a $(OBJS)

all::

clean-local::
	-@$(RM) $(OBJS)

all::

depend-local:: $(SRCS)
	$(DEPEND) -s "# DO NOT DELETE" -- $(ALLDEFINES) -- $(SRCS)

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

c_tdclrs.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_tdclrs.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_tdclrs.o: /usr/include/sys/_posix_availability.h
c_tdclrs.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_tdclrs.o: /usr/include/_types.h /usr/include/sys/_types.h
c_tdclrs.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_tdclrs.o: /usr/include/sys/_pthread/_pthread_types.h
c_tdclrs.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_tdclrs.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_tdclrs.o: /usr/include/sys/_types/_int16_t.h
c_tdclrs.o: /usr/include/sys/_types/_int32_t.h
c_tdclrs.o: /usr/include/sys/_types/_int64_t.h
c_tdclrs.o: /usr/include/sys/_types/_u_int8_t.h
c_tdclrs.o: /usr/include/sys/_types/_u_int16_t.h
c_tdclrs.o: /usr/include/sys/_types/_u_int32_t.h
c_tdclrs.o: /usr/include/sys/_types/_u_int64_t.h
c_tdclrs.o: /usr/include/sys/_types/_intptr_t.h
c_tdclrs.o: /usr/include/sys/_types/_uintptr_t.h
c_tdclrs.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_tdclrs.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_tdclrs.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_tdclrs.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_tdclrs.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_tdclrs.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_tdclrs.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_tdclrs.o: /usr/include/libkern/i386/_OSByteOrder.h
c_tdclrs.o: /usr/include/sys/_types/_u_char.h
c_tdclrs.o: /usr/include/sys/_types/_u_short.h
c_tdclrs.o: /usr/include/sys/_types/_u_int.h
c_tdclrs.o: /usr/include/sys/_types/_caddr_t.h
c_tdclrs.o: /usr/include/sys/_types/_dev_t.h
c_tdclrs.o: /usr/include/sys/_types/_blkcnt_t.h
c_tdclrs.o: /usr/include/sys/_types/_blksize_t.h
c_tdclrs.o: /usr/include/sys/_types/_gid_t.h
c_tdclrs.o: /usr/include/sys/_types/_in_addr_t.h
c_tdclrs.o: /usr/include/sys/_types/_in_port_t.h
c_tdclrs.o: /usr/include/sys/_types/_ino_t.h
c_tdclrs.o: /usr/include/sys/_types/_ino64_t.h
c_tdclrs.o: /usr/include/sys/_types/_key_t.h
c_tdclrs.o: /usr/include/sys/_types/_mode_t.h
c_tdclrs.o: /usr/include/sys/_types/_nlink_t.h
c_tdclrs.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_tdclrs.o: /usr/include/sys/_types/_uid_t.h
c_tdclrs.o: /usr/include/sys/_types/_clock_t.h
c_tdclrs.o: /usr/include/sys/_types/_time_t.h
c_tdclrs.o: /usr/include/sys/_types/_useconds_t.h
c_tdclrs.o: /usr/include/sys/_types/_suseconds_t.h
c_tdclrs.o: /usr/include/sys/_types/_rsize_t.h
c_tdclrs.o: /usr/include/sys/_types/_errno_t.h
c_tdclrs.o: /usr/include/sys/_types/_fd_def.h
c_tdclrs.o: /usr/include/sys/_types/_fd_setsize.h
c_tdclrs.o: /usr/include/sys/_types/_fd_set.h
c_tdclrs.o: /usr/include/sys/_types/_fd_clr.h
c_tdclrs.o: /usr/include/sys/_types/_fd_zero.h
c_tdclrs.o: /usr/include/sys/_types/_fd_isset.h
c_tdclrs.o: /usr/include/sys/_types/_fd_copy.h
c_tdclrs.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_tdclrs.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_tdclrs.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_tdclrs.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_tdclrs.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_tdclrs.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_tdclrs.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_tdclrs.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_tdclrs.o: /usr/include/sys/_pthread/_pthread_t.h
c_tdclrs.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_tdclrs.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_tdclrs.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_tdclrs.o: ../../../.././include/ncarg/c.h
c_tddtri.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_tddtri.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_tddtri.o: /usr/include/sys/_posix_availability.h
c_tddtri.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_tddtri.o: /usr/include/_types.h /usr/include/sys/_types.h
c_tddtri.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_tddtri.o: /usr/include/sys/_pthread/_pthread_types.h
c_tddtri.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_tddtri.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_tddtri.o: /usr/include/sys/_types/_int16_t.h
c_tddtri.o: /usr/include/sys/_types/_int32_t.h
c_tddtri.o: /usr/include/sys/_types/_int64_t.h
c_tddtri.o: /usr/include/sys/_types/_u_int8_t.h
c_tddtri.o: /usr/include/sys/_types/_u_int16_t.h
c_tddtri.o: /usr/include/sys/_types/_u_int32_t.h
c_tddtri.o: /usr/include/sys/_types/_u_int64_t.h
c_tddtri.o: /usr/include/sys/_types/_intptr_t.h
c_tddtri.o: /usr/include/sys/_types/_uintptr_t.h
c_tddtri.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_tddtri.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_tddtri.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_tddtri.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_tddtri.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_tddtri.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_tddtri.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_tddtri.o: /usr/include/libkern/i386/_OSByteOrder.h
c_tddtri.o: /usr/include/sys/_types/_u_char.h
c_tddtri.o: /usr/include/sys/_types/_u_short.h
c_tddtri.o: /usr/include/sys/_types/_u_int.h
c_tddtri.o: /usr/include/sys/_types/_caddr_t.h
c_tddtri.o: /usr/include/sys/_types/_dev_t.h
c_tddtri.o: /usr/include/sys/_types/_blkcnt_t.h
c_tddtri.o: /usr/include/sys/_types/_blksize_t.h
c_tddtri.o: /usr/include/sys/_types/_gid_t.h
c_tddtri.o: /usr/include/sys/_types/_in_addr_t.h
c_tddtri.o: /usr/include/sys/_types/_in_port_t.h
c_tddtri.o: /usr/include/sys/_types/_ino_t.h
c_tddtri.o: /usr/include/sys/_types/_ino64_t.h
c_tddtri.o: /usr/include/sys/_types/_key_t.h
c_tddtri.o: /usr/include/sys/_types/_mode_t.h
c_tddtri.o: /usr/include/sys/_types/_nlink_t.h
c_tddtri.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_tddtri.o: /usr/include/sys/_types/_uid_t.h
c_tddtri.o: /usr/include/sys/_types/_clock_t.h
c_tddtri.o: /usr/include/sys/_types/_time_t.h
c_tddtri.o: /usr/include/sys/_types/_useconds_t.h
c_tddtri.o: /usr/include/sys/_types/_suseconds_t.h
c_tddtri.o: /usr/include/sys/_types/_rsize_t.h
c_tddtri.o: /usr/include/sys/_types/_errno_t.h
c_tddtri.o: /usr/include/sys/_types/_fd_def.h
c_tddtri.o: /usr/include/sys/_types/_fd_setsize.h
c_tddtri.o: /usr/include/sys/_types/_fd_set.h
c_tddtri.o: /usr/include/sys/_types/_fd_clr.h
c_tddtri.o: /usr/include/sys/_types/_fd_zero.h
c_tddtri.o: /usr/include/sys/_types/_fd_isset.h
c_tddtri.o: /usr/include/sys/_types/_fd_copy.h
c_tddtri.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_tddtri.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_tddtri.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_tddtri.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_tddtri.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_tddtri.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_tddtri.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_tddtri.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_tddtri.o: /usr/include/sys/_pthread/_pthread_t.h
c_tddtri.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_tddtri.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_tddtri.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_tddtri.o: ../../../.././include/ncarg/c.h
c_tdez2d.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_tdez2d.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_tdez2d.o: /usr/include/sys/_posix_availability.h
c_tdez2d.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_tdez2d.o: /usr/include/_types.h /usr/include/sys/_types.h
c_tdez2d.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_tdez2d.o: /usr/include/sys/_pthread/_pthread_types.h
c_tdez2d.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_tdez2d.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_tdez2d.o: /usr/include/sys/_types/_int16_t.h
c_tdez2d.o: /usr/include/sys/_types/_int32_t.h
c_tdez2d.o: /usr/include/sys/_types/_int64_t.h
c_tdez2d.o: /usr/include/sys/_types/_u_int8_t.h
c_tdez2d.o: /usr/include/sys/_types/_u_int16_t.h
c_tdez2d.o: /usr/include/sys/_types/_u_int32_t.h
c_tdez2d.o: /usr/include/sys/_types/_u_int64_t.h
c_tdez2d.o: /usr/include/sys/_types/_intptr_t.h
c_tdez2d.o: /usr/include/sys/_types/_uintptr_t.h
c_tdez2d.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_tdez2d.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_tdez2d.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_tdez2d.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_tdez2d.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_tdez2d.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_tdez2d.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_tdez2d.o: /usr/include/libkern/i386/_OSByteOrder.h
c_tdez2d.o: /usr/include/sys/_types/_u_char.h
c_tdez2d.o: /usr/include/sys/_types/_u_short.h
c_tdez2d.o: /usr/include/sys/_types/_u_int.h
c_tdez2d.o: /usr/include/sys/_types/_caddr_t.h
c_tdez2d.o: /usr/include/sys/_types/_dev_t.h
c_tdez2d.o: /usr/include/sys/_types/_blkcnt_t.h
c_tdez2d.o: /usr/include/sys/_types/_blksize_t.h
c_tdez2d.o: /usr/include/sys/_types/_gid_t.h
c_tdez2d.o: /usr/include/sys/_types/_in_addr_t.h
c_tdez2d.o: /usr/include/sys/_types/_in_port_t.h
c_tdez2d.o: /usr/include/sys/_types/_ino_t.h
c_tdez2d.o: /usr/include/sys/_types/_ino64_t.h
c_tdez2d.o: /usr/include/sys/_types/_key_t.h
c_tdez2d.o: /usr/include/sys/_types/_mode_t.h
c_tdez2d.o: /usr/include/sys/_types/_nlink_t.h
c_tdez2d.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_tdez2d.o: /usr/include/sys/_types/_uid_t.h
c_tdez2d.o: /usr/include/sys/_types/_clock_t.h
c_tdez2d.o: /usr/include/sys/_types/_time_t.h
c_tdez2d.o: /usr/include/sys/_types/_useconds_t.h
c_tdez2d.o: /usr/include/sys/_types/_suseconds_t.h
c_tdez2d.o: /usr/include/sys/_types/_rsize_t.h
c_tdez2d.o: /usr/include/sys/_types/_errno_t.h
c_tdez2d.o: /usr/include/sys/_types/_fd_def.h
c_tdez2d.o: /usr/include/sys/_types/_fd_setsize.h
c_tdez2d.o: /usr/include/sys/_types/_fd_set.h
c_tdez2d.o: /usr/include/sys/_types/_fd_clr.h
c_tdez2d.o: /usr/include/sys/_types/_fd_zero.h
c_tdez2d.o: /usr/include/sys/_types/_fd_isset.h
c_tdez2d.o: /usr/include/sys/_types/_fd_copy.h
c_tdez2d.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_tdez2d.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_tdez2d.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_tdez2d.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_tdez2d.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_tdez2d.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_tdez2d.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_tdez2d.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_tdez2d.o: /usr/include/sys/_pthread/_pthread_t.h
c_tdez2d.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_tdez2d.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_tdez2d.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_tdez2d.o: ../../../.././include/ncarg/c.h
c_tdez3d.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_tdez3d.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_tdez3d.o: /usr/include/sys/_posix_availability.h
c_tdez3d.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_tdez3d.o: /usr/include/_types.h /usr/include/sys/_types.h
c_tdez3d.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_tdez3d.o: /usr/include/sys/_pthread/_pthread_types.h
c_tdez3d.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_tdez3d.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_tdez3d.o: /usr/include/sys/_types/_int16_t.h
c_tdez3d.o: /usr/include/sys/_types/_int32_t.h
c_tdez3d.o: /usr/include/sys/_types/_int64_t.h
c_tdez3d.o: /usr/include/sys/_types/_u_int8_t.h
c_tdez3d.o: /usr/include/sys/_types/_u_int16_t.h
c_tdez3d.o: /usr/include/sys/_types/_u_int32_t.h
c_tdez3d.o: /usr/include/sys/_types/_u_int64_t.h
c_tdez3d.o: /usr/include/sys/_types/_intptr_t.h
c_tdez3d.o: /usr/include/sys/_types/_uintptr_t.h
c_tdez3d.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_tdez3d.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_tdez3d.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_tdez3d.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_tdez3d.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_tdez3d.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_tdez3d.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_tdez3d.o: /usr/include/libkern/i386/_OSByteOrder.h
c_tdez3d.o: /usr/include/sys/_types/_u_char.h
c_tdez3d.o: /usr/include/sys/_types/_u_short.h
c_tdez3d.o: /usr/include/sys/_types/_u_int.h
c_tdez3d.o: /usr/include/sys/_types/_caddr_t.h
c_tdez3d.o: /usr/include/sys/_types/_dev_t.h
c_tdez3d.o: /usr/include/sys/_types/_blkcnt_t.h
c_tdez3d.o: /usr/include/sys/_types/_blksize_t.h
c_tdez3d.o: /usr/include/sys/_types/_gid_t.h
c_tdez3d.o: /usr/include/sys/_types/_in_addr_t.h
c_tdez3d.o: /usr/include/sys/_types/_in_port_t.h
c_tdez3d.o: /usr/include/sys/_types/_ino_t.h
c_tdez3d.o: /usr/include/sys/_types/_ino64_t.h
c_tdez3d.o: /usr/include/sys/_types/_key_t.h
c_tdez3d.o: /usr/include/sys/_types/_mode_t.h
c_tdez3d.o: /usr/include/sys/_types/_nlink_t.h
c_tdez3d.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_tdez3d.o: /usr/include/sys/_types/_uid_t.h
c_tdez3d.o: /usr/include/sys/_types/_clock_t.h
c_tdez3d.o: /usr/include/sys/_types/_time_t.h
c_tdez3d.o: /usr/include/sys/_types/_useconds_t.h
c_tdez3d.o: /usr/include/sys/_types/_suseconds_t.h
c_tdez3d.o: /usr/include/sys/_types/_rsize_t.h
c_tdez3d.o: /usr/include/sys/_types/_errno_t.h
c_tdez3d.o: /usr/include/sys/_types/_fd_def.h
c_tdez3d.o: /usr/include/sys/_types/_fd_setsize.h
c_tdez3d.o: /usr/include/sys/_types/_fd_set.h
c_tdez3d.o: /usr/include/sys/_types/_fd_clr.h
c_tdez3d.o: /usr/include/sys/_types/_fd_zero.h
c_tdez3d.o: /usr/include/sys/_types/_fd_isset.h
c_tdez3d.o: /usr/include/sys/_types/_fd_copy.h
c_tdez3d.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_tdez3d.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_tdez3d.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_tdez3d.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_tdez3d.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_tdez3d.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_tdez3d.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_tdez3d.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_tdez3d.o: /usr/include/sys/_pthread/_pthread_t.h
c_tdez3d.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_tdez3d.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_tdez3d.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_tdez3d.o: ../../../.././include/ncarg/c.h
c_tdgeti.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_tdgeti.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_tdgeti.o: /usr/include/sys/_posix_availability.h
c_tdgeti.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_tdgeti.o: /usr/include/_types.h /usr/include/sys/_types.h
c_tdgeti.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_tdgeti.o: /usr/include/sys/_pthread/_pthread_types.h
c_tdgeti.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_tdgeti.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_tdgeti.o: /usr/include/sys/_types/_int16_t.h
c_tdgeti.o: /usr/include/sys/_types/_int32_t.h
c_tdgeti.o: /usr/include/sys/_types/_int64_t.h
c_tdgeti.o: /usr/include/sys/_types/_u_int8_t.h
c_tdgeti.o: /usr/include/sys/_types/_u_int16_t.h
c_tdgeti.o: /usr/include/sys/_types/_u_int32_t.h
c_tdgeti.o: /usr/include/sys/_types/_u_int64_t.h
c_tdgeti.o: /usr/include/sys/_types/_intptr_t.h
c_tdgeti.o: /usr/include/sys/_types/_uintptr_t.h
c_tdgeti.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_tdgeti.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_tdgeti.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_tdgeti.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_tdgeti.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_tdgeti.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_tdgeti.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_tdgeti.o: /usr/include/libkern/i386/_OSByteOrder.h
c_tdgeti.o: /usr/include/sys/_types/_u_char.h
c_tdgeti.o: /usr/include/sys/_types/_u_short.h
c_tdgeti.o: /usr/include/sys/_types/_u_int.h
c_tdgeti.o: /usr/include/sys/_types/_caddr_t.h
c_tdgeti.o: /usr/include/sys/_types/_dev_t.h
c_tdgeti.o: /usr/include/sys/_types/_blkcnt_t.h
c_tdgeti.o: /usr/include/sys/_types/_blksize_t.h
c_tdgeti.o: /usr/include/sys/_types/_gid_t.h
c_tdgeti.o: /usr/include/sys/_types/_in_addr_t.h
c_tdgeti.o: /usr/include/sys/_types/_in_port_t.h
c_tdgeti.o: /usr/include/sys/_types/_ino_t.h
c_tdgeti.o: /usr/include/sys/_types/_ino64_t.h
c_tdgeti.o: /usr/include/sys/_types/_key_t.h
c_tdgeti.o: /usr/include/sys/_types/_mode_t.h
c_tdgeti.o: /usr/include/sys/_types/_nlink_t.h
c_tdgeti.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_tdgeti.o: /usr/include/sys/_types/_uid_t.h
c_tdgeti.o: /usr/include/sys/_types/_clock_t.h
c_tdgeti.o: /usr/include/sys/_types/_time_t.h
c_tdgeti.o: /usr/include/sys/_types/_useconds_t.h
c_tdgeti.o: /usr/include/sys/_types/_suseconds_t.h
c_tdgeti.o: /usr/include/sys/_types/_rsize_t.h
c_tdgeti.o: /usr/include/sys/_types/_errno_t.h
c_tdgeti.o: /usr/include/sys/_types/_fd_def.h
c_tdgeti.o: /usr/include/sys/_types/_fd_setsize.h
c_tdgeti.o: /usr/include/sys/_types/_fd_set.h
c_tdgeti.o: /usr/include/sys/_types/_fd_clr.h
c_tdgeti.o: /usr/include/sys/_types/_fd_zero.h
c_tdgeti.o: /usr/include/sys/_types/_fd_isset.h
c_tdgeti.o: /usr/include/sys/_types/_fd_copy.h
c_tdgeti.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_tdgeti.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_tdgeti.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_tdgeti.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_tdgeti.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_tdgeti.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_tdgeti.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_tdgeti.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_tdgeti.o: /usr/include/sys/_pthread/_pthread_t.h
c_tdgeti.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_tdgeti.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_tdgeti.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_tdgeti.o: ../../../.././include/ncarg/c.h
c_tdgetr.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_tdgetr.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_tdgetr.o: /usr/include/sys/_posix_availability.h
c_tdgetr.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_tdgetr.o: /usr/include/_types.h /usr/include/sys/_types.h
c_tdgetr.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_tdgetr.o: /usr/include/sys/_pthread/_pthread_types.h
c_tdgetr.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_tdgetr.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_tdgetr.o: /usr/include/sys/_types/_int16_t.h
c_tdgetr.o: /usr/include/sys/_types/_int32_t.h
c_tdgetr.o: /usr/include/sys/_types/_int64_t.h
c_tdgetr.o: /usr/include/sys/_types/_u_int8_t.h
c_tdgetr.o: /usr/include/sys/_types/_u_int16_t.h
c_tdgetr.o: /usr/include/sys/_types/_u_int32_t.h
c_tdgetr.o: /usr/include/sys/_types/_u_int64_t.h
c_tdgetr.o: /usr/include/sys/_types/_intptr_t.h
c_tdgetr.o: /usr/include/sys/_types/_uintptr_t.h
c_tdgetr.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_tdgetr.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_tdgetr.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_tdgetr.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_tdgetr.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_tdgetr.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_tdgetr.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_tdgetr.o: /usr/include/libkern/i386/_OSByteOrder.h
c_tdgetr.o: /usr/include/sys/_types/_u_char.h
c_tdgetr.o: /usr/include/sys/_types/_u_short.h
c_tdgetr.o: /usr/include/sys/_types/_u_int.h
c_tdgetr.o: /usr/include/sys/_types/_caddr_t.h
c_tdgetr.o: /usr/include/sys/_types/_dev_t.h
c_tdgetr.o: /usr/include/sys/_types/_blkcnt_t.h
c_tdgetr.o: /usr/include/sys/_types/_blksize_t.h
c_tdgetr.o: /usr/include/sys/_types/_gid_t.h
c_tdgetr.o: /usr/include/sys/_types/_in_addr_t.h
c_tdgetr.o: /usr/include/sys/_types/_in_port_t.h
c_tdgetr.o: /usr/include/sys/_types/_ino_t.h
c_tdgetr.o: /usr/include/sys/_types/_ino64_t.h
c_tdgetr.o: /usr/include/sys/_types/_key_t.h
c_tdgetr.o: /usr/include/sys/_types/_mode_t.h
c_tdgetr.o: /usr/include/sys/_types/_nlink_t.h
c_tdgetr.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_tdgetr.o: /usr/include/sys/_types/_uid_t.h
c_tdgetr.o: /usr/include/sys/_types/_clock_t.h
c_tdgetr.o: /usr/include/sys/_types/_time_t.h
c_tdgetr.o: /usr/include/sys/_types/_useconds_t.h
c_tdgetr.o: /usr/include/sys/_types/_suseconds_t.h
c_tdgetr.o: /usr/include/sys/_types/_rsize_t.h
c_tdgetr.o: /usr/include/sys/_types/_errno_t.h
c_tdgetr.o: /usr/include/sys/_types/_fd_def.h
c_tdgetr.o: /usr/include/sys/_types/_fd_setsize.h
c_tdgetr.o: /usr/include/sys/_types/_fd_set.h
c_tdgetr.o: /usr/include/sys/_types/_fd_clr.h
c_tdgetr.o: /usr/include/sys/_types/_fd_zero.h
c_tdgetr.o: /usr/include/sys/_types/_fd_isset.h
c_tdgetr.o: /usr/include/sys/_types/_fd_copy.h
c_tdgetr.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_tdgetr.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_tdgetr.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_tdgetr.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_tdgetr.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_tdgetr.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_tdgetr.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_tdgetr.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_tdgetr.o: /usr/include/sys/_pthread/_pthread_t.h
c_tdgetr.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_tdgetr.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_tdgetr.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_tdgetr.o: ../../../.././include/ncarg/c.h
c_tdgrds.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_tdgrds.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_tdgrds.o: /usr/include/sys/_posix_availability.h
c_tdgrds.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_tdgrds.o: /usr/include/_types.h /usr/include/sys/_types.h
c_tdgrds.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_tdgrds.o: /usr/include/sys/_pthread/_pthread_types.h
c_tdgrds.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_tdgrds.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_tdgrds.o: /usr/include/sys/_types/_int16_t.h
c_tdgrds.o: /usr/include/sys/_types/_int32_t.h
c_tdgrds.o: /usr/include/sys/_types/_int64_t.h
c_tdgrds.o: /usr/include/sys/_types/_u_int8_t.h
c_tdgrds.o: /usr/include/sys/_types/_u_int16_t.h
c_tdgrds.o: /usr/include/sys/_types/_u_int32_t.h
c_tdgrds.o: /usr/include/sys/_types/_u_int64_t.h
c_tdgrds.o: /usr/include/sys/_types/_intptr_t.h
c_tdgrds.o: /usr/include/sys/_types/_uintptr_t.h
c_tdgrds.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_tdgrds.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_tdgrds.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_tdgrds.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_tdgrds.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_tdgrds.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_tdgrds.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_tdgrds.o: /usr/include/libkern/i386/_OSByteOrder.h
c_tdgrds.o: /usr/include/sys/_types/_u_char.h
c_tdgrds.o: /usr/include/sys/_types/_u_short.h
c_tdgrds.o: /usr/include/sys/_types/_u_int.h
c_tdgrds.o: /usr/include/sys/_types/_caddr_t.h
c_tdgrds.o: /usr/include/sys/_types/_dev_t.h
c_tdgrds.o: /usr/include/sys/_types/_blkcnt_t.h
c_tdgrds.o: /usr/include/sys/_types/_blksize_t.h
c_tdgrds.o: /usr/include/sys/_types/_gid_t.h
c_tdgrds.o: /usr/include/sys/_types/_in_addr_t.h
c_tdgrds.o: /usr/include/sys/_types/_in_port_t.h
c_tdgrds.o: /usr/include/sys/_types/_ino_t.h
c_tdgrds.o: /usr/include/sys/_types/_ino64_t.h
c_tdgrds.o: /usr/include/sys/_types/_key_t.h
c_tdgrds.o: /usr/include/sys/_types/_mode_t.h
c_tdgrds.o: /usr/include/sys/_types/_nlink_t.h
c_tdgrds.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_tdgrds.o: /usr/include/sys/_types/_uid_t.h
c_tdgrds.o: /usr/include/sys/_types/_clock_t.h
c_tdgrds.o: /usr/include/sys/_types/_time_t.h
c_tdgrds.o: /usr/include/sys/_types/_useconds_t.h
c_tdgrds.o: /usr/include/sys/_types/_suseconds_t.h
c_tdgrds.o: /usr/include/sys/_types/_rsize_t.h
c_tdgrds.o: /usr/include/sys/_types/_errno_t.h
c_tdgrds.o: /usr/include/sys/_types/_fd_def.h
c_tdgrds.o: /usr/include/sys/_types/_fd_setsize.h
c_tdgrds.o: /usr/include/sys/_types/_fd_set.h
c_tdgrds.o: /usr/include/sys/_types/_fd_clr.h
c_tdgrds.o: /usr/include/sys/_types/_fd_zero.h
c_tdgrds.o: /usr/include/sys/_types/_fd_isset.h
c_tdgrds.o: /usr/include/sys/_types/_fd_copy.h
c_tdgrds.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_tdgrds.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_tdgrds.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_tdgrds.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_tdgrds.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_tdgrds.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_tdgrds.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_tdgrds.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_tdgrds.o: /usr/include/sys/_pthread/_pthread_t.h
c_tdgrds.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_tdgrds.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_tdgrds.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_tdgrds.o: ../../../.././include/ncarg/c.h
c_tdgrid.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_tdgrid.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_tdgrid.o: /usr/include/sys/_posix_availability.h
c_tdgrid.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_tdgrid.o: /usr/include/_types.h /usr/include/sys/_types.h
c_tdgrid.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_tdgrid.o: /usr/include/sys/_pthread/_pthread_types.h
c_tdgrid.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_tdgrid.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_tdgrid.o: /usr/include/sys/_types/_int16_t.h
c_tdgrid.o: /usr/include/sys/_types/_int32_t.h
c_tdgrid.o: /usr/include/sys/_types/_int64_t.h
c_tdgrid.o: /usr/include/sys/_types/_u_int8_t.h
c_tdgrid.o: /usr/include/sys/_types/_u_int16_t.h
c_tdgrid.o: /usr/include/sys/_types/_u_int32_t.h
c_tdgrid.o: /usr/include/sys/_types/_u_int64_t.h
c_tdgrid.o: /usr/include/sys/_types/_intptr_t.h
c_tdgrid.o: /usr/include/sys/_types/_uintptr_t.h
c_tdgrid.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_tdgrid.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_tdgrid.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_tdgrid.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_tdgrid.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_tdgrid.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_tdgrid.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_tdgrid.o: /usr/include/libkern/i386/_OSByteOrder.h
c_tdgrid.o: /usr/include/sys/_types/_u_char.h
c_tdgrid.o: /usr/include/sys/_types/_u_short.h
c_tdgrid.o: /usr/include/sys/_types/_u_int.h
c_tdgrid.o: /usr/include/sys/_types/_caddr_t.h
c_tdgrid.o: /usr/include/sys/_types/_dev_t.h
c_tdgrid.o: /usr/include/sys/_types/_blkcnt_t.h
c_tdgrid.o: /usr/include/sys/_types/_blksize_t.h
c_tdgrid.o: /usr/include/sys/_types/_gid_t.h
c_tdgrid.o: /usr/include/sys/_types/_in_addr_t.h
c_tdgrid.o: /usr/include/sys/_types/_in_port_t.h
c_tdgrid.o: /usr/include/sys/_types/_ino_t.h
c_tdgrid.o: /usr/include/sys/_types/_ino64_t.h
c_tdgrid.o: /usr/include/sys/_types/_key_t.h
c_tdgrid.o: /usr/include/sys/_types/_mode_t.h
c_tdgrid.o: /usr/include/sys/_types/_nlink_t.h
c_tdgrid.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_tdgrid.o: /usr/include/sys/_types/_uid_t.h
c_tdgrid.o: /usr/include/sys/_types/_clock_t.h
c_tdgrid.o: /usr/include/sys/_types/_time_t.h
c_tdgrid.o: /usr/include/sys/_types/_useconds_t.h
c_tdgrid.o: /usr/include/sys/_types/_suseconds_t.h
c_tdgrid.o: /usr/include/sys/_types/_rsize_t.h
c_tdgrid.o: /usr/include/sys/_types/_errno_t.h
c_tdgrid.o: /usr/include/sys/_types/_fd_def.h
c_tdgrid.o: /usr/include/sys/_types/_fd_setsize.h
c_tdgrid.o: /usr/include/sys/_types/_fd_set.h
c_tdgrid.o: /usr/include/sys/_types/_fd_clr.h
c_tdgrid.o: /usr/include/sys/_types/_fd_zero.h
c_tdgrid.o: /usr/include/sys/_types/_fd_isset.h
c_tdgrid.o: /usr/include/sys/_types/_fd_copy.h
c_tdgrid.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_tdgrid.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_tdgrid.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_tdgrid.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_tdgrid.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_tdgrid.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_tdgrid.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_tdgrid.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_tdgrid.o: /usr/include/sys/_pthread/_pthread_t.h
c_tdgrid.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_tdgrid.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_tdgrid.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_tdgrid.o: ../../../.././include/ncarg/c.h
c_tdgtrs.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_tdgtrs.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_tdgtrs.o: /usr/include/sys/_posix_availability.h
c_tdgtrs.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_tdgtrs.o: /usr/include/_types.h /usr/include/sys/_types.h
c_tdgtrs.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_tdgtrs.o: /usr/include/sys/_pthread/_pthread_types.h
c_tdgtrs.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_tdgtrs.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_tdgtrs.o: /usr/include/sys/_types/_int16_t.h
c_tdgtrs.o: /usr/include/sys/_types/_int32_t.h
c_tdgtrs.o: /usr/include/sys/_types/_int64_t.h
c_tdgtrs.o: /usr/include/sys/_types/_u_int8_t.h
c_tdgtrs.o: /usr/include/sys/_types/_u_int16_t.h
c_tdgtrs.o: /usr/include/sys/_types/_u_int32_t.h
c_tdgtrs.o: /usr/include/sys/_types/_u_int64_t.h
c_tdgtrs.o: /usr/include/sys/_types/_intptr_t.h
c_tdgtrs.o: /usr/include/sys/_types/_uintptr_t.h
c_tdgtrs.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_tdgtrs.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_tdgtrs.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_tdgtrs.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_tdgtrs.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_tdgtrs.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_tdgtrs.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_tdgtrs.o: /usr/include/libkern/i386/_OSByteOrder.h
c_tdgtrs.o: /usr/include/sys/_types/_u_char.h
c_tdgtrs.o: /usr/include/sys/_types/_u_short.h
c_tdgtrs.o: /usr/include/sys/_types/_u_int.h
c_tdgtrs.o: /usr/include/sys/_types/_caddr_t.h
c_tdgtrs.o: /usr/include/sys/_types/_dev_t.h
c_tdgtrs.o: /usr/include/sys/_types/_blkcnt_t.h
c_tdgtrs.o: /usr/include/sys/_types/_blksize_t.h
c_tdgtrs.o: /usr/include/sys/_types/_gid_t.h
c_tdgtrs.o: /usr/include/sys/_types/_in_addr_t.h
c_tdgtrs.o: /usr/include/sys/_types/_in_port_t.h
c_tdgtrs.o: /usr/include/sys/_types/_ino_t.h
c_tdgtrs.o: /usr/include/sys/_types/_ino64_t.h
c_tdgtrs.o: /usr/include/sys/_types/_key_t.h
c_tdgtrs.o: /usr/include/sys/_types/_mode_t.h
c_tdgtrs.o: /usr/include/sys/_types/_nlink_t.h
c_tdgtrs.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_tdgtrs.o: /usr/include/sys/_types/_uid_t.h
c_tdgtrs.o: /usr/include/sys/_types/_clock_t.h
c_tdgtrs.o: /usr/include/sys/_types/_time_t.h
c_tdgtrs.o: /usr/include/sys/_types/_useconds_t.h
c_tdgtrs.o: /usr/include/sys/_types/_suseconds_t.h
c_tdgtrs.o: /usr/include/sys/_types/_rsize_t.h
c_tdgtrs.o: /usr/include/sys/_types/_errno_t.h
c_tdgtrs.o: /usr/include/sys/_types/_fd_def.h
c_tdgtrs.o: /usr/include/sys/_types/_fd_setsize.h
c_tdgtrs.o: /usr/include/sys/_types/_fd_set.h
c_tdgtrs.o: /usr/include/sys/_types/_fd_clr.h
c_tdgtrs.o: /usr/include/sys/_types/_fd_zero.h
c_tdgtrs.o: /usr/include/sys/_types/_fd_isset.h
c_tdgtrs.o: /usr/include/sys/_types/_fd_copy.h
c_tdgtrs.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_tdgtrs.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_tdgtrs.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_tdgtrs.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_tdgtrs.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_tdgtrs.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_tdgtrs.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_tdgtrs.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_tdgtrs.o: /usr/include/sys/_pthread/_pthread_t.h
c_tdgtrs.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_tdgtrs.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_tdgtrs.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_tdgtrs.o: ../../../.././include/ncarg/c.h
c_tdinit.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_tdinit.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_tdinit.o: /usr/include/sys/_posix_availability.h
c_tdinit.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_tdinit.o: /usr/include/_types.h /usr/include/sys/_types.h
c_tdinit.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_tdinit.o: /usr/include/sys/_pthread/_pthread_types.h
c_tdinit.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_tdinit.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_tdinit.o: /usr/include/sys/_types/_int16_t.h
c_tdinit.o: /usr/include/sys/_types/_int32_t.h
c_tdinit.o: /usr/include/sys/_types/_int64_t.h
c_tdinit.o: /usr/include/sys/_types/_u_int8_t.h
c_tdinit.o: /usr/include/sys/_types/_u_int16_t.h
c_tdinit.o: /usr/include/sys/_types/_u_int32_t.h
c_tdinit.o: /usr/include/sys/_types/_u_int64_t.h
c_tdinit.o: /usr/include/sys/_types/_intptr_t.h
c_tdinit.o: /usr/include/sys/_types/_uintptr_t.h
c_tdinit.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_tdinit.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_tdinit.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_tdinit.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_tdinit.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_tdinit.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_tdinit.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_tdinit.o: /usr/include/libkern/i386/_OSByteOrder.h
c_tdinit.o: /usr/include/sys/_types/_u_char.h
c_tdinit.o: /usr/include/sys/_types/_u_short.h
c_tdinit.o: /usr/include/sys/_types/_u_int.h
c_tdinit.o: /usr/include/sys/_types/_caddr_t.h
c_tdinit.o: /usr/include/sys/_types/_dev_t.h
c_tdinit.o: /usr/include/sys/_types/_blkcnt_t.h
c_tdinit.o: /usr/include/sys/_types/_blksize_t.h
c_tdinit.o: /usr/include/sys/_types/_gid_t.h
c_tdinit.o: /usr/include/sys/_types/_in_addr_t.h
c_tdinit.o: /usr/include/sys/_types/_in_port_t.h
c_tdinit.o: /usr/include/sys/_types/_ino_t.h
c_tdinit.o: /usr/include/sys/_types/_ino64_t.h
c_tdinit.o: /usr/include/sys/_types/_key_t.h
c_tdinit.o: /usr/include/sys/_types/_mode_t.h
c_tdinit.o: /usr/include/sys/_types/_nlink_t.h
c_tdinit.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_tdinit.o: /usr/include/sys/_types/_uid_t.h
c_tdinit.o: /usr/include/sys/_types/_clock_t.h
c_tdinit.o: /usr/include/sys/_types/_time_t.h
c_tdinit.o: /usr/include/sys/_types/_useconds_t.h
c_tdinit.o: /usr/include/sys/_types/_suseconds_t.h
c_tdinit.o: /usr/include/sys/_types/_rsize_t.h
c_tdinit.o: /usr/include/sys/_types/_errno_t.h
c_tdinit.o: /usr/include/sys/_types/_fd_def.h
c_tdinit.o: /usr/include/sys/_types/_fd_setsize.h
c_tdinit.o: /usr/include/sys/_types/_fd_set.h
c_tdinit.o: /usr/include/sys/_types/_fd_clr.h
c_tdinit.o: /usr/include/sys/_types/_fd_zero.h
c_tdinit.o: /usr/include/sys/_types/_fd_isset.h
c_tdinit.o: /usr/include/sys/_types/_fd_copy.h
c_tdinit.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_tdinit.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_tdinit.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_tdinit.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_tdinit.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_tdinit.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_tdinit.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_tdinit.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_tdinit.o: /usr/include/sys/_pthread/_pthread_t.h
c_tdinit.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_tdinit.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_tdinit.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_tdinit.o: ../../../.././include/ncarg/c.h
c_tditri.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_tditri.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_tditri.o: /usr/include/sys/_posix_availability.h
c_tditri.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_tditri.o: /usr/include/_types.h /usr/include/sys/_types.h
c_tditri.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_tditri.o: /usr/include/sys/_pthread/_pthread_types.h
c_tditri.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_tditri.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_tditri.o: /usr/include/sys/_types/_int16_t.h
c_tditri.o: /usr/include/sys/_types/_int32_t.h
c_tditri.o: /usr/include/sys/_types/_int64_t.h
c_tditri.o: /usr/include/sys/_types/_u_int8_t.h
c_tditri.o: /usr/include/sys/_types/_u_int16_t.h
c_tditri.o: /usr/include/sys/_types/_u_int32_t.h
c_tditri.o: /usr/include/sys/_types/_u_int64_t.h
c_tditri.o: /usr/include/sys/_types/_intptr_t.h
c_tditri.o: /usr/include/sys/_types/_uintptr_t.h
c_tditri.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_tditri.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_tditri.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_tditri.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_tditri.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_tditri.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_tditri.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_tditri.o: /usr/include/libkern/i386/_OSByteOrder.h
c_tditri.o: /usr/include/sys/_types/_u_char.h
c_tditri.o: /usr/include/sys/_types/_u_short.h
c_tditri.o: /usr/include/sys/_types/_u_int.h
c_tditri.o: /usr/include/sys/_types/_caddr_t.h
c_tditri.o: /usr/include/sys/_types/_dev_t.h
c_tditri.o: /usr/include/sys/_types/_blkcnt_t.h
c_tditri.o: /usr/include/sys/_types/_blksize_t.h
c_tditri.o: /usr/include/sys/_types/_gid_t.h
c_tditri.o: /usr/include/sys/_types/_in_addr_t.h
c_tditri.o: /usr/include/sys/_types/_in_port_t.h
c_tditri.o: /usr/include/sys/_types/_ino_t.h
c_tditri.o: /usr/include/sys/_types/_ino64_t.h
c_tditri.o: /usr/include/sys/_types/_key_t.h
c_tditri.o: /usr/include/sys/_types/_mode_t.h
c_tditri.o: /usr/include/sys/_types/_nlink_t.h
c_tditri.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_tditri.o: /usr/include/sys/_types/_uid_t.h
c_tditri.o: /usr/include/sys/_types/_clock_t.h
c_tditri.o: /usr/include/sys/_types/_time_t.h
c_tditri.o: /usr/include/sys/_types/_useconds_t.h
c_tditri.o: /usr/include/sys/_types/_suseconds_t.h
c_tditri.o: /usr/include/sys/_types/_rsize_t.h
c_tditri.o: /usr/include/sys/_types/_errno_t.h
c_tditri.o: /usr/include/sys/_types/_fd_def.h
c_tditri.o: /usr/include/sys/_types/_fd_setsize.h
c_tditri.o: /usr/include/sys/_types/_fd_set.h
c_tditri.o: /usr/include/sys/_types/_fd_clr.h
c_tditri.o: /usr/include/sys/_types/_fd_zero.h
c_tditri.o: /usr/include/sys/_types/_fd_isset.h
c_tditri.o: /usr/include/sys/_types/_fd_copy.h
c_tditri.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_tditri.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_tditri.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_tditri.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_tditri.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_tditri.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_tditri.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_tditri.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_tditri.o: /usr/include/sys/_pthread/_pthread_t.h
c_tditri.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_tditri.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_tditri.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_tditri.o: ../../../.././include/ncarg/c.h
c_tdlbla.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_tdlbla.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_tdlbla.o: /usr/include/sys/_posix_availability.h
c_tdlbla.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_tdlbla.o: /usr/include/_types.h /usr/include/sys/_types.h
c_tdlbla.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_tdlbla.o: /usr/include/sys/_pthread/_pthread_types.h
c_tdlbla.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_tdlbla.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_tdlbla.o: /usr/include/sys/_types/_int16_t.h
c_tdlbla.o: /usr/include/sys/_types/_int32_t.h
c_tdlbla.o: /usr/include/sys/_types/_int64_t.h
c_tdlbla.o: /usr/include/sys/_types/_u_int8_t.h
c_tdlbla.o: /usr/include/sys/_types/_u_int16_t.h
c_tdlbla.o: /usr/include/sys/_types/_u_int32_t.h
c_tdlbla.o: /usr/include/sys/_types/_u_int64_t.h
c_tdlbla.o: /usr/include/sys/_types/_intptr_t.h
c_tdlbla.o: /usr/include/sys/_types/_uintptr_t.h
c_tdlbla.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_tdlbla.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_tdlbla.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_tdlbla.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_tdlbla.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_tdlbla.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_tdlbla.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_tdlbla.o: /usr/include/libkern/i386/_OSByteOrder.h
c_tdlbla.o: /usr/include/sys/_types/_u_char.h
c_tdlbla.o: /usr/include/sys/_types/_u_short.h
c_tdlbla.o: /usr/include/sys/_types/_u_int.h
c_tdlbla.o: /usr/include/sys/_types/_caddr_t.h
c_tdlbla.o: /usr/include/sys/_types/_dev_t.h
c_tdlbla.o: /usr/include/sys/_types/_blkcnt_t.h
c_tdlbla.o: /usr/include/sys/_types/_blksize_t.h
c_tdlbla.o: /usr/include/sys/_types/_gid_t.h
c_tdlbla.o: /usr/include/sys/_types/_in_addr_t.h
c_tdlbla.o: /usr/include/sys/_types/_in_port_t.h
c_tdlbla.o: /usr/include/sys/_types/_ino_t.h
c_tdlbla.o: /usr/include/sys/_types/_ino64_t.h
c_tdlbla.o: /usr/include/sys/_types/_key_t.h
c_tdlbla.o: /usr/include/sys/_types/_mode_t.h
c_tdlbla.o: /usr/include/sys/_types/_nlink_t.h
c_tdlbla.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_tdlbla.o: /usr/include/sys/_types/_uid_t.h
c_tdlbla.o: /usr/include/sys/_types/_clock_t.h
c_tdlbla.o: /usr/include/sys/_types/_time_t.h
c_tdlbla.o: /usr/include/sys/_types/_useconds_t.h
c_tdlbla.o: /usr/include/sys/_types/_suseconds_t.h
c_tdlbla.o: /usr/include/sys/_types/_rsize_t.h
c_tdlbla.o: /usr/include/sys/_types/_errno_t.h
c_tdlbla.o: /usr/include/sys/_types/_fd_def.h
c_tdlbla.o: /usr/include/sys/_types/_fd_setsize.h
c_tdlbla.o: /usr/include/sys/_types/_fd_set.h
c_tdlbla.o: /usr/include/sys/_types/_fd_clr.h
c_tdlbla.o: /usr/include/sys/_types/_fd_zero.h
c_tdlbla.o: /usr/include/sys/_types/_fd_isset.h
c_tdlbla.o: /usr/include/sys/_types/_fd_copy.h
c_tdlbla.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_tdlbla.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_tdlbla.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_tdlbla.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_tdlbla.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_tdlbla.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_tdlbla.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_tdlbla.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_tdlbla.o: /usr/include/sys/_pthread/_pthread_t.h
c_tdlbla.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_tdlbla.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_tdlbla.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_tdlbla.o: ../../../.././include/ncarg/c.h
c_tdlbls.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_tdlbls.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_tdlbls.o: /usr/include/sys/_posix_availability.h
c_tdlbls.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_tdlbls.o: /usr/include/_types.h /usr/include/sys/_types.h
c_tdlbls.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_tdlbls.o: /usr/include/sys/_pthread/_pthread_types.h
c_tdlbls.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_tdlbls.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_tdlbls.o: /usr/include/sys/_types/_int16_t.h
c_tdlbls.o: /usr/include/sys/_types/_int32_t.h
c_tdlbls.o: /usr/include/sys/_types/_int64_t.h
c_tdlbls.o: /usr/include/sys/_types/_u_int8_t.h
c_tdlbls.o: /usr/include/sys/_types/_u_int16_t.h
c_tdlbls.o: /usr/include/sys/_types/_u_int32_t.h
c_tdlbls.o: /usr/include/sys/_types/_u_int64_t.h
c_tdlbls.o: /usr/include/sys/_types/_intptr_t.h
c_tdlbls.o: /usr/include/sys/_types/_uintptr_t.h
c_tdlbls.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_tdlbls.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_tdlbls.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_tdlbls.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_tdlbls.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_tdlbls.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_tdlbls.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_tdlbls.o: /usr/include/libkern/i386/_OSByteOrder.h
c_tdlbls.o: /usr/include/sys/_types/_u_char.h
c_tdlbls.o: /usr/include/sys/_types/_u_short.h
c_tdlbls.o: /usr/include/sys/_types/_u_int.h
c_tdlbls.o: /usr/include/sys/_types/_caddr_t.h
c_tdlbls.o: /usr/include/sys/_types/_dev_t.h
c_tdlbls.o: /usr/include/sys/_types/_blkcnt_t.h
c_tdlbls.o: /usr/include/sys/_types/_blksize_t.h
c_tdlbls.o: /usr/include/sys/_types/_gid_t.h
c_tdlbls.o: /usr/include/sys/_types/_in_addr_t.h
c_tdlbls.o: /usr/include/sys/_types/_in_port_t.h
c_tdlbls.o: /usr/include/sys/_types/_ino_t.h
c_tdlbls.o: /usr/include/sys/_types/_ino64_t.h
c_tdlbls.o: /usr/include/sys/_types/_key_t.h
c_tdlbls.o: /usr/include/sys/_types/_mode_t.h
c_tdlbls.o: /usr/include/sys/_types/_nlink_t.h
c_tdlbls.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_tdlbls.o: /usr/include/sys/_types/_uid_t.h
c_tdlbls.o: /usr/include/sys/_types/_clock_t.h
c_tdlbls.o: /usr/include/sys/_types/_time_t.h
c_tdlbls.o: /usr/include/sys/_types/_useconds_t.h
c_tdlbls.o: /usr/include/sys/_types/_suseconds_t.h
c_tdlbls.o: /usr/include/sys/_types/_rsize_t.h
c_tdlbls.o: /usr/include/sys/_types/_errno_t.h
c_tdlbls.o: /usr/include/sys/_types/_fd_def.h
c_tdlbls.o: /usr/include/sys/_types/_fd_setsize.h
c_tdlbls.o: /usr/include/sys/_types/_fd_set.h
c_tdlbls.o: /usr/include/sys/_types/_fd_clr.h
c_tdlbls.o: /usr/include/sys/_types/_fd_zero.h
c_tdlbls.o: /usr/include/sys/_types/_fd_isset.h
c_tdlbls.o: /usr/include/sys/_types/_fd_copy.h
c_tdlbls.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_tdlbls.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_tdlbls.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_tdlbls.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_tdlbls.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_tdlbls.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_tdlbls.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_tdlbls.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_tdlbls.o: /usr/include/sys/_pthread/_pthread_t.h
c_tdlbls.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_tdlbls.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_tdlbls.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_tdlbls.o: ../../../.././include/ncarg/c.h
c_tdline.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_tdline.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_tdline.o: /usr/include/sys/_posix_availability.h
c_tdline.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_tdline.o: /usr/include/_types.h /usr/include/sys/_types.h
c_tdline.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_tdline.o: /usr/include/sys/_pthread/_pthread_types.h
c_tdline.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_tdline.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_tdline.o: /usr/include/sys/_types/_int16_t.h
c_tdline.o: /usr/include/sys/_types/_int32_t.h
c_tdline.o: /usr/include/sys/_types/_int64_t.h
c_tdline.o: /usr/include/sys/_types/_u_int8_t.h
c_tdline.o: /usr/include/sys/_types/_u_int16_t.h
c_tdline.o: /usr/include/sys/_types/_u_int32_t.h
c_tdline.o: /usr/include/sys/_types/_u_int64_t.h
c_tdline.o: /usr/include/sys/_types/_intptr_t.h
c_tdline.o: /usr/include/sys/_types/_uintptr_t.h
c_tdline.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_tdline.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_tdline.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_tdline.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_tdline.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_tdline.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_tdline.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_tdline.o: /usr/include/libkern/i386/_OSByteOrder.h
c_tdline.o: /usr/include/sys/_types/_u_char.h
c_tdline.o: /usr/include/sys/_types/_u_short.h
c_tdline.o: /usr/include/sys/_types/_u_int.h
c_tdline.o: /usr/include/sys/_types/_caddr_t.h
c_tdline.o: /usr/include/sys/_types/_dev_t.h
c_tdline.o: /usr/include/sys/_types/_blkcnt_t.h
c_tdline.o: /usr/include/sys/_types/_blksize_t.h
c_tdline.o: /usr/include/sys/_types/_gid_t.h
c_tdline.o: /usr/include/sys/_types/_in_addr_t.h
c_tdline.o: /usr/include/sys/_types/_in_port_t.h
c_tdline.o: /usr/include/sys/_types/_ino_t.h
c_tdline.o: /usr/include/sys/_types/_ino64_t.h
c_tdline.o: /usr/include/sys/_types/_key_t.h
c_tdline.o: /usr/include/sys/_types/_mode_t.h
c_tdline.o: /usr/include/sys/_types/_nlink_t.h
c_tdline.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_tdline.o: /usr/include/sys/_types/_uid_t.h
c_tdline.o: /usr/include/sys/_types/_clock_t.h
c_tdline.o: /usr/include/sys/_types/_time_t.h
c_tdline.o: /usr/include/sys/_types/_useconds_t.h
c_tdline.o: /usr/include/sys/_types/_suseconds_t.h
c_tdline.o: /usr/include/sys/_types/_rsize_t.h
c_tdline.o: /usr/include/sys/_types/_errno_t.h
c_tdline.o: /usr/include/sys/_types/_fd_def.h
c_tdline.o: /usr/include/sys/_types/_fd_setsize.h
c_tdline.o: /usr/include/sys/_types/_fd_set.h
c_tdline.o: /usr/include/sys/_types/_fd_clr.h
c_tdline.o: /usr/include/sys/_types/_fd_zero.h
c_tdline.o: /usr/include/sys/_types/_fd_isset.h
c_tdline.o: /usr/include/sys/_types/_fd_copy.h
c_tdline.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_tdline.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_tdline.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_tdline.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_tdline.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_tdline.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_tdline.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_tdline.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_tdline.o: /usr/include/sys/_pthread/_pthread_t.h
c_tdline.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_tdline.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_tdline.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_tdline.o: ../../../.././include/ncarg/c.h
c_tdlnpa.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_tdlnpa.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_tdlnpa.o: /usr/include/sys/_posix_availability.h
c_tdlnpa.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_tdlnpa.o: /usr/include/_types.h /usr/include/sys/_types.h
c_tdlnpa.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_tdlnpa.o: /usr/include/sys/_pthread/_pthread_types.h
c_tdlnpa.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_tdlnpa.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_tdlnpa.o: /usr/include/sys/_types/_int16_t.h
c_tdlnpa.o: /usr/include/sys/_types/_int32_t.h
c_tdlnpa.o: /usr/include/sys/_types/_int64_t.h
c_tdlnpa.o: /usr/include/sys/_types/_u_int8_t.h
c_tdlnpa.o: /usr/include/sys/_types/_u_int16_t.h
c_tdlnpa.o: /usr/include/sys/_types/_u_int32_t.h
c_tdlnpa.o: /usr/include/sys/_types/_u_int64_t.h
c_tdlnpa.o: /usr/include/sys/_types/_intptr_t.h
c_tdlnpa.o: /usr/include/sys/_types/_uintptr_t.h
c_tdlnpa.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_tdlnpa.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_tdlnpa.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_tdlnpa.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_tdlnpa.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_tdlnpa.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_tdlnpa.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_tdlnpa.o: /usr/include/libkern/i386/_OSByteOrder.h
c_tdlnpa.o: /usr/include/sys/_types/_u_char.h
c_tdlnpa.o: /usr/include/sys/_types/_u_short.h
c_tdlnpa.o: /usr/include/sys/_types/_u_int.h
c_tdlnpa.o: /usr/include/sys/_types/_caddr_t.h
c_tdlnpa.o: /usr/include/sys/_types/_dev_t.h
c_tdlnpa.o: /usr/include/sys/_types/_blkcnt_t.h
c_tdlnpa.o: /usr/include/sys/_types/_blksize_t.h
c_tdlnpa.o: /usr/include/sys/_types/_gid_t.h
c_tdlnpa.o: /usr/include/sys/_types/_in_addr_t.h
c_tdlnpa.o: /usr/include/sys/_types/_in_port_t.h
c_tdlnpa.o: /usr/include/sys/_types/_ino_t.h
c_tdlnpa.o: /usr/include/sys/_types/_ino64_t.h
c_tdlnpa.o: /usr/include/sys/_types/_key_t.h
c_tdlnpa.o: /usr/include/sys/_types/_mode_t.h
c_tdlnpa.o: /usr/include/sys/_types/_nlink_t.h
c_tdlnpa.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_tdlnpa.o: /usr/include/sys/_types/_uid_t.h
c_tdlnpa.o: /usr/include/sys/_types/_clock_t.h
c_tdlnpa.o: /usr/include/sys/_types/_time_t.h
c_tdlnpa.o: /usr/include/sys/_types/_useconds_t.h
c_tdlnpa.o: /usr/include/sys/_types/_suseconds_t.h
c_tdlnpa.o: /usr/include/sys/_types/_rsize_t.h
c_tdlnpa.o: /usr/include/sys/_types/_errno_t.h
c_tdlnpa.o: /usr/include/sys/_types/_fd_def.h
c_tdlnpa.o: /usr/include/sys/_types/_fd_setsize.h
c_tdlnpa.o: /usr/include/sys/_types/_fd_set.h
c_tdlnpa.o: /usr/include/sys/_types/_fd_clr.h
c_tdlnpa.o: /usr/include/sys/_types/_fd_zero.h
c_tdlnpa.o: /usr/include/sys/_types/_fd_isset.h
c_tdlnpa.o: /usr/include/sys/_types/_fd_copy.h
c_tdlnpa.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_tdlnpa.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_tdlnpa.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_tdlnpa.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_tdlnpa.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_tdlnpa.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_tdlnpa.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_tdlnpa.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_tdlnpa.o: /usr/include/sys/_pthread/_pthread_t.h
c_tdlnpa.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_tdlnpa.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_tdlnpa.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_tdlnpa.o: ../../../.././include/ncarg/c.h
c_tdmtri.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_tdmtri.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_tdmtri.o: /usr/include/sys/_posix_availability.h
c_tdmtri.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_tdmtri.o: /usr/include/_types.h /usr/include/sys/_types.h
c_tdmtri.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_tdmtri.o: /usr/include/sys/_pthread/_pthread_types.h
c_tdmtri.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_tdmtri.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_tdmtri.o: /usr/include/sys/_types/_int16_t.h
c_tdmtri.o: /usr/include/sys/_types/_int32_t.h
c_tdmtri.o: /usr/include/sys/_types/_int64_t.h
c_tdmtri.o: /usr/include/sys/_types/_u_int8_t.h
c_tdmtri.o: /usr/include/sys/_types/_u_int16_t.h
c_tdmtri.o: /usr/include/sys/_types/_u_int32_t.h
c_tdmtri.o: /usr/include/sys/_types/_u_int64_t.h
c_tdmtri.o: /usr/include/sys/_types/_intptr_t.h
c_tdmtri.o: /usr/include/sys/_types/_uintptr_t.h
c_tdmtri.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_tdmtri.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_tdmtri.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_tdmtri.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_tdmtri.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_tdmtri.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_tdmtri.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_tdmtri.o: /usr/include/libkern/i386/_OSByteOrder.h
c_tdmtri.o: /usr/include/sys/_types/_u_char.h
c_tdmtri.o: /usr/include/sys/_types/_u_short.h
c_tdmtri.o: /usr/include/sys/_types/_u_int.h
c_tdmtri.o: /usr/include/sys/_types/_caddr_t.h
c_tdmtri.o: /usr/include/sys/_types/_dev_t.h
c_tdmtri.o: /usr/include/sys/_types/_blkcnt_t.h
c_tdmtri.o: /usr/include/sys/_types/_blksize_t.h
c_tdmtri.o: /usr/include/sys/_types/_gid_t.h
c_tdmtri.o: /usr/include/sys/_types/_in_addr_t.h
c_tdmtri.o: /usr/include/sys/_types/_in_port_t.h
c_tdmtri.o: /usr/include/sys/_types/_ino_t.h
c_tdmtri.o: /usr/include/sys/_types/_ino64_t.h
c_tdmtri.o: /usr/include/sys/_types/_key_t.h
c_tdmtri.o: /usr/include/sys/_types/_mode_t.h
c_tdmtri.o: /usr/include/sys/_types/_nlink_t.h
c_tdmtri.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_tdmtri.o: /usr/include/sys/_types/_uid_t.h
c_tdmtri.o: /usr/include/sys/_types/_clock_t.h
c_tdmtri.o: /usr/include/sys/_types/_time_t.h
c_tdmtri.o: /usr/include/sys/_types/_useconds_t.h
c_tdmtri.o: /usr/include/sys/_types/_suseconds_t.h
c_tdmtri.o: /usr/include/sys/_types/_rsize_t.h
c_tdmtri.o: /usr/include/sys/_types/_errno_t.h
c_tdmtri.o: /usr/include/sys/_types/_fd_def.h
c_tdmtri.o: /usr/include/sys/_types/_fd_setsize.h
c_tdmtri.o: /usr/include/sys/_types/_fd_set.h
c_tdmtri.o: /usr/include/sys/_types/_fd_clr.h
c_tdmtri.o: /usr/include/sys/_types/_fd_zero.h
c_tdmtri.o: /usr/include/sys/_types/_fd_isset.h
c_tdmtri.o: /usr/include/sys/_types/_fd_copy.h
c_tdmtri.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_tdmtri.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_tdmtri.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_tdmtri.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_tdmtri.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_tdmtri.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_tdmtri.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_tdmtri.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_tdmtri.o: /usr/include/sys/_pthread/_pthread_t.h
c_tdmtri.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_tdmtri.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_tdmtri.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_tdmtri.o: ../../../.././include/ncarg/c.h
c_tdotri.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_tdotri.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_tdotri.o: /usr/include/sys/_posix_availability.h
c_tdotri.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_tdotri.o: /usr/include/_types.h /usr/include/sys/_types.h
c_tdotri.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_tdotri.o: /usr/include/sys/_pthread/_pthread_types.h
c_tdotri.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_tdotri.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_tdotri.o: /usr/include/sys/_types/_int16_t.h
c_tdotri.o: /usr/include/sys/_types/_int32_t.h
c_tdotri.o: /usr/include/sys/_types/_int64_t.h
c_tdotri.o: /usr/include/sys/_types/_u_int8_t.h
c_tdotri.o: /usr/include/sys/_types/_u_int16_t.h
c_tdotri.o: /usr/include/sys/_types/_u_int32_t.h
c_tdotri.o: /usr/include/sys/_types/_u_int64_t.h
c_tdotri.o: /usr/include/sys/_types/_intptr_t.h
c_tdotri.o: /usr/include/sys/_types/_uintptr_t.h
c_tdotri.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_tdotri.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_tdotri.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_tdotri.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_tdotri.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_tdotri.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_tdotri.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_tdotri.o: /usr/include/libkern/i386/_OSByteOrder.h
c_tdotri.o: /usr/include/sys/_types/_u_char.h
c_tdotri.o: /usr/include/sys/_types/_u_short.h
c_tdotri.o: /usr/include/sys/_types/_u_int.h
c_tdotri.o: /usr/include/sys/_types/_caddr_t.h
c_tdotri.o: /usr/include/sys/_types/_dev_t.h
c_tdotri.o: /usr/include/sys/_types/_blkcnt_t.h
c_tdotri.o: /usr/include/sys/_types/_blksize_t.h
c_tdotri.o: /usr/include/sys/_types/_gid_t.h
c_tdotri.o: /usr/include/sys/_types/_in_addr_t.h
c_tdotri.o: /usr/include/sys/_types/_in_port_t.h
c_tdotri.o: /usr/include/sys/_types/_ino_t.h
c_tdotri.o: /usr/include/sys/_types/_ino64_t.h
c_tdotri.o: /usr/include/sys/_types/_key_t.h
c_tdotri.o: /usr/include/sys/_types/_mode_t.h
c_tdotri.o: /usr/include/sys/_types/_nlink_t.h
c_tdotri.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_tdotri.o: /usr/include/sys/_types/_uid_t.h
c_tdotri.o: /usr/include/sys/_types/_clock_t.h
c_tdotri.o: /usr/include/sys/_types/_time_t.h
c_tdotri.o: /usr/include/sys/_types/_useconds_t.h
c_tdotri.o: /usr/include/sys/_types/_suseconds_t.h
c_tdotri.o: /usr/include/sys/_types/_rsize_t.h
c_tdotri.o: /usr/include/sys/_types/_errno_t.h
c_tdotri.o: /usr/include/sys/_types/_fd_def.h
c_tdotri.o: /usr/include/sys/_types/_fd_setsize.h
c_tdotri.o: /usr/include/sys/_types/_fd_set.h
c_tdotri.o: /usr/include/sys/_types/_fd_clr.h
c_tdotri.o: /usr/include/sys/_types/_fd_zero.h
c_tdotri.o: /usr/include/sys/_types/_fd_isset.h
c_tdotri.o: /usr/include/sys/_types/_fd_copy.h
c_tdotri.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_tdotri.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_tdotri.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_tdotri.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_tdotri.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_tdotri.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_tdotri.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_tdotri.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_tdotri.o: /usr/include/sys/_pthread/_pthread_t.h
c_tdotri.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_tdotri.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_tdotri.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_tdotri.o: ../../../.././include/ncarg/c.h
c_tdpara.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_tdpara.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_tdpara.o: /usr/include/sys/_posix_availability.h
c_tdpara.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_tdpara.o: /usr/include/_types.h /usr/include/sys/_types.h
c_tdpara.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_tdpara.o: /usr/include/sys/_pthread/_pthread_types.h
c_tdpara.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_tdpara.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_tdpara.o: /usr/include/sys/_types/_int16_t.h
c_tdpara.o: /usr/include/sys/_types/_int32_t.h
c_tdpara.o: /usr/include/sys/_types/_int64_t.h
c_tdpara.o: /usr/include/sys/_types/_u_int8_t.h
c_tdpara.o: /usr/include/sys/_types/_u_int16_t.h
c_tdpara.o: /usr/include/sys/_types/_u_int32_t.h
c_tdpara.o: /usr/include/sys/_types/_u_int64_t.h
c_tdpara.o: /usr/include/sys/_types/_intptr_t.h
c_tdpara.o: /usr/include/sys/_types/_uintptr_t.h
c_tdpara.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_tdpara.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_tdpara.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_tdpara.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_tdpara.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_tdpara.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_tdpara.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_tdpara.o: /usr/include/libkern/i386/_OSByteOrder.h
c_tdpara.o: /usr/include/sys/_types/_u_char.h
c_tdpara.o: /usr/include/sys/_types/_u_short.h
c_tdpara.o: /usr/include/sys/_types/_u_int.h
c_tdpara.o: /usr/include/sys/_types/_caddr_t.h
c_tdpara.o: /usr/include/sys/_types/_dev_t.h
c_tdpara.o: /usr/include/sys/_types/_blkcnt_t.h
c_tdpara.o: /usr/include/sys/_types/_blksize_t.h
c_tdpara.o: /usr/include/sys/_types/_gid_t.h
c_tdpara.o: /usr/include/sys/_types/_in_addr_t.h
c_tdpara.o: /usr/include/sys/_types/_in_port_t.h
c_tdpara.o: /usr/include/sys/_types/_ino_t.h
c_tdpara.o: /usr/include/sys/_types/_ino64_t.h
c_tdpara.o: /usr/include/sys/_types/_key_t.h
c_tdpara.o: /usr/include/sys/_types/_mode_t.h
c_tdpara.o: /usr/include/sys/_types/_nlink_t.h
c_tdpara.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_tdpara.o: /usr/include/sys/_types/_uid_t.h
c_tdpara.o: /usr/include/sys/_types/_clock_t.h
c_tdpara.o: /usr/include/sys/_types/_time_t.h
c_tdpara.o: /usr/include/sys/_types/_useconds_t.h
c_tdpara.o: /usr/include/sys/_types/_suseconds_t.h
c_tdpara.o: /usr/include/sys/_types/_rsize_t.h
c_tdpara.o: /usr/include/sys/_types/_errno_t.h
c_tdpara.o: /usr/include/sys/_types/_fd_def.h
c_tdpara.o: /usr/include/sys/_types/_fd_setsize.h
c_tdpara.o: /usr/include/sys/_types/_fd_set.h
c_tdpara.o: /usr/include/sys/_types/_fd_clr.h
c_tdpara.o: /usr/include/sys/_types/_fd_zero.h
c_tdpara.o: /usr/include/sys/_types/_fd_isset.h
c_tdpara.o: /usr/include/sys/_types/_fd_copy.h
c_tdpara.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_tdpara.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_tdpara.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_tdpara.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_tdpara.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_tdpara.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_tdpara.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_tdpara.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_tdpara.o: /usr/include/sys/_pthread/_pthread_t.h
c_tdpara.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_tdpara.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_tdpara.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_tdpara.o: ../../../.././include/ncarg/c.h
c_tdplch.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_tdplch.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_tdplch.o: /usr/include/sys/_posix_availability.h
c_tdplch.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_tdplch.o: /usr/include/_types.h /usr/include/sys/_types.h
c_tdplch.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_tdplch.o: /usr/include/sys/_pthread/_pthread_types.h
c_tdplch.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_tdplch.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_tdplch.o: /usr/include/sys/_types/_int16_t.h
c_tdplch.o: /usr/include/sys/_types/_int32_t.h
c_tdplch.o: /usr/include/sys/_types/_int64_t.h
c_tdplch.o: /usr/include/sys/_types/_u_int8_t.h
c_tdplch.o: /usr/include/sys/_types/_u_int16_t.h
c_tdplch.o: /usr/include/sys/_types/_u_int32_t.h
c_tdplch.o: /usr/include/sys/_types/_u_int64_t.h
c_tdplch.o: /usr/include/sys/_types/_intptr_t.h
c_tdplch.o: /usr/include/sys/_types/_uintptr_t.h
c_tdplch.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_tdplch.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_tdplch.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_tdplch.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_tdplch.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_tdplch.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_tdplch.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_tdplch.o: /usr/include/libkern/i386/_OSByteOrder.h
c_tdplch.o: /usr/include/sys/_types/_u_char.h
c_tdplch.o: /usr/include/sys/_types/_u_short.h
c_tdplch.o: /usr/include/sys/_types/_u_int.h
c_tdplch.o: /usr/include/sys/_types/_caddr_t.h
c_tdplch.o: /usr/include/sys/_types/_dev_t.h
c_tdplch.o: /usr/include/sys/_types/_blkcnt_t.h
c_tdplch.o: /usr/include/sys/_types/_blksize_t.h
c_tdplch.o: /usr/include/sys/_types/_gid_t.h
c_tdplch.o: /usr/include/sys/_types/_in_addr_t.h
c_tdplch.o: /usr/include/sys/_types/_in_port_t.h
c_tdplch.o: /usr/include/sys/_types/_ino_t.h
c_tdplch.o: /usr/include/sys/_types/_ino64_t.h
c_tdplch.o: /usr/include/sys/_types/_key_t.h
c_tdplch.o: /usr/include/sys/_types/_mode_t.h
c_tdplch.o: /usr/include/sys/_types/_nlink_t.h
c_tdplch.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_tdplch.o: /usr/include/sys/_types/_uid_t.h
c_tdplch.o: /usr/include/sys/_types/_clock_t.h
c_tdplch.o: /usr/include/sys/_types/_time_t.h
c_tdplch.o: /usr/include/sys/_types/_useconds_t.h
c_tdplch.o: /usr/include/sys/_types/_suseconds_t.h
c_tdplch.o: /usr/include/sys/_types/_rsize_t.h
c_tdplch.o: /usr/include/sys/_types/_errno_t.h
c_tdplch.o: /usr/include/sys/_types/_fd_def.h
c_tdplch.o: /usr/include/sys/_types/_fd_setsize.h
c_tdplch.o: /usr/include/sys/_types/_fd_set.h
c_tdplch.o: /usr/include/sys/_types/_fd_clr.h
c_tdplch.o: /usr/include/sys/_types/_fd_zero.h
c_tdplch.o: /usr/include/sys/_types/_fd_isset.h
c_tdplch.o: /usr/include/sys/_types/_fd_copy.h
c_tdplch.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_tdplch.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_tdplch.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_tdplch.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_tdplch.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_tdplch.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_tdplch.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_tdplch.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_tdplch.o: /usr/include/sys/_pthread/_pthread_t.h
c_tdplch.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_tdplch.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_tdplch.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_tdplch.o: ../../../.././include/ncarg/c.h
c_tdprpa.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_tdprpa.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_tdprpa.o: /usr/include/sys/_posix_availability.h
c_tdprpa.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_tdprpa.o: /usr/include/_types.h /usr/include/sys/_types.h
c_tdprpa.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_tdprpa.o: /usr/include/sys/_pthread/_pthread_types.h
c_tdprpa.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_tdprpa.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_tdprpa.o: /usr/include/sys/_types/_int16_t.h
c_tdprpa.o: /usr/include/sys/_types/_int32_t.h
c_tdprpa.o: /usr/include/sys/_types/_int64_t.h
c_tdprpa.o: /usr/include/sys/_types/_u_int8_t.h
c_tdprpa.o: /usr/include/sys/_types/_u_int16_t.h
c_tdprpa.o: /usr/include/sys/_types/_u_int32_t.h
c_tdprpa.o: /usr/include/sys/_types/_u_int64_t.h
c_tdprpa.o: /usr/include/sys/_types/_intptr_t.h
c_tdprpa.o: /usr/include/sys/_types/_uintptr_t.h
c_tdprpa.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_tdprpa.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_tdprpa.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_tdprpa.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_tdprpa.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_tdprpa.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_tdprpa.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_tdprpa.o: /usr/include/libkern/i386/_OSByteOrder.h
c_tdprpa.o: /usr/include/sys/_types/_u_char.h
c_tdprpa.o: /usr/include/sys/_types/_u_short.h
c_tdprpa.o: /usr/include/sys/_types/_u_int.h
c_tdprpa.o: /usr/include/sys/_types/_caddr_t.h
c_tdprpa.o: /usr/include/sys/_types/_dev_t.h
c_tdprpa.o: /usr/include/sys/_types/_blkcnt_t.h
c_tdprpa.o: /usr/include/sys/_types/_blksize_t.h
c_tdprpa.o: /usr/include/sys/_types/_gid_t.h
c_tdprpa.o: /usr/include/sys/_types/_in_addr_t.h
c_tdprpa.o: /usr/include/sys/_types/_in_port_t.h
c_tdprpa.o: /usr/include/sys/_types/_ino_t.h
c_tdprpa.o: /usr/include/sys/_types/_ino64_t.h
c_tdprpa.o: /usr/include/sys/_types/_key_t.h
c_tdprpa.o: /usr/include/sys/_types/_mode_t.h
c_tdprpa.o: /usr/include/sys/_types/_nlink_t.h
c_tdprpa.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_tdprpa.o: /usr/include/sys/_types/_uid_t.h
c_tdprpa.o: /usr/include/sys/_types/_clock_t.h
c_tdprpa.o: /usr/include/sys/_types/_time_t.h
c_tdprpa.o: /usr/include/sys/_types/_useconds_t.h
c_tdprpa.o: /usr/include/sys/_types/_suseconds_t.h
c_tdprpa.o: /usr/include/sys/_types/_rsize_t.h
c_tdprpa.o: /usr/include/sys/_types/_errno_t.h
c_tdprpa.o: /usr/include/sys/_types/_fd_def.h
c_tdprpa.o: /usr/include/sys/_types/_fd_setsize.h
c_tdprpa.o: /usr/include/sys/_types/_fd_set.h
c_tdprpa.o: /usr/include/sys/_types/_fd_clr.h
c_tdprpa.o: /usr/include/sys/_types/_fd_zero.h
c_tdprpa.o: /usr/include/sys/_types/_fd_isset.h
c_tdprpa.o: /usr/include/sys/_types/_fd_copy.h
c_tdprpa.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_tdprpa.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_tdprpa.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_tdprpa.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_tdprpa.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_tdprpa.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_tdprpa.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_tdprpa.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_tdprpa.o: /usr/include/sys/_pthread/_pthread_t.h
c_tdprpa.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_tdprpa.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_tdprpa.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_tdprpa.o: ../../../.././include/ncarg/c.h
c_tdprpi.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_tdprpi.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_tdprpi.o: /usr/include/sys/_posix_availability.h
c_tdprpi.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_tdprpi.o: /usr/include/_types.h /usr/include/sys/_types.h
c_tdprpi.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_tdprpi.o: /usr/include/sys/_pthread/_pthread_types.h
c_tdprpi.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_tdprpi.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_tdprpi.o: /usr/include/sys/_types/_int16_t.h
c_tdprpi.o: /usr/include/sys/_types/_int32_t.h
c_tdprpi.o: /usr/include/sys/_types/_int64_t.h
c_tdprpi.o: /usr/include/sys/_types/_u_int8_t.h
c_tdprpi.o: /usr/include/sys/_types/_u_int16_t.h
c_tdprpi.o: /usr/include/sys/_types/_u_int32_t.h
c_tdprpi.o: /usr/include/sys/_types/_u_int64_t.h
c_tdprpi.o: /usr/include/sys/_types/_intptr_t.h
c_tdprpi.o: /usr/include/sys/_types/_uintptr_t.h
c_tdprpi.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_tdprpi.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_tdprpi.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_tdprpi.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_tdprpi.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_tdprpi.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_tdprpi.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_tdprpi.o: /usr/include/libkern/i386/_OSByteOrder.h
c_tdprpi.o: /usr/include/sys/_types/_u_char.h
c_tdprpi.o: /usr/include/sys/_types/_u_short.h
c_tdprpi.o: /usr/include/sys/_types/_u_int.h
c_tdprpi.o: /usr/include/sys/_types/_caddr_t.h
c_tdprpi.o: /usr/include/sys/_types/_dev_t.h
c_tdprpi.o: /usr/include/sys/_types/_blkcnt_t.h
c_tdprpi.o: /usr/include/sys/_types/_blksize_t.h
c_tdprpi.o: /usr/include/sys/_types/_gid_t.h
c_tdprpi.o: /usr/include/sys/_types/_in_addr_t.h
c_tdprpi.o: /usr/include/sys/_types/_in_port_t.h
c_tdprpi.o: /usr/include/sys/_types/_ino_t.h
c_tdprpi.o: /usr/include/sys/_types/_ino64_t.h
c_tdprpi.o: /usr/include/sys/_types/_key_t.h
c_tdprpi.o: /usr/include/sys/_types/_mode_t.h
c_tdprpi.o: /usr/include/sys/_types/_nlink_t.h
c_tdprpi.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_tdprpi.o: /usr/include/sys/_types/_uid_t.h
c_tdprpi.o: /usr/include/sys/_types/_clock_t.h
c_tdprpi.o: /usr/include/sys/_types/_time_t.h
c_tdprpi.o: /usr/include/sys/_types/_useconds_t.h
c_tdprpi.o: /usr/include/sys/_types/_suseconds_t.h
c_tdprpi.o: /usr/include/sys/_types/_rsize_t.h
c_tdprpi.o: /usr/include/sys/_types/_errno_t.h
c_tdprpi.o: /usr/include/sys/_types/_fd_def.h
c_tdprpi.o: /usr/include/sys/_types/_fd_setsize.h
c_tdprpi.o: /usr/include/sys/_types/_fd_set.h
c_tdprpi.o: /usr/include/sys/_types/_fd_clr.h
c_tdprpi.o: /usr/include/sys/_types/_fd_zero.h
c_tdprpi.o: /usr/include/sys/_types/_fd_isset.h
c_tdprpi.o: /usr/include/sys/_types/_fd_copy.h
c_tdprpi.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_tdprpi.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_tdprpi.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_tdprpi.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_tdprpi.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_tdprpi.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_tdprpi.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_tdprpi.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_tdprpi.o: /usr/include/sys/_pthread/_pthread_t.h
c_tdprpi.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_tdprpi.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_tdprpi.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_tdprpi.o: ../../../.././include/ncarg/c.h
c_tdprpt.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_tdprpt.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_tdprpt.o: /usr/include/sys/_posix_availability.h
c_tdprpt.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_tdprpt.o: /usr/include/_types.h /usr/include/sys/_types.h
c_tdprpt.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_tdprpt.o: /usr/include/sys/_pthread/_pthread_types.h
c_tdprpt.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_tdprpt.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_tdprpt.o: /usr/include/sys/_types/_int16_t.h
c_tdprpt.o: /usr/include/sys/_types/_int32_t.h
c_tdprpt.o: /usr/include/sys/_types/_int64_t.h
c_tdprpt.o: /usr/include/sys/_types/_u_int8_t.h
c_tdprpt.o: /usr/include/sys/_types/_u_int16_t.h
c_tdprpt.o: /usr/include/sys/_types/_u_int32_t.h
c_tdprpt.o: /usr/include/sys/_types/_u_int64_t.h
c_tdprpt.o: /usr/include/sys/_types/_intptr_t.h
c_tdprpt.o: /usr/include/sys/_types/_uintptr_t.h
c_tdprpt.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_tdprpt.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_tdprpt.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_tdprpt.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_tdprpt.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_tdprpt.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_tdprpt.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_tdprpt.o: /usr/include/libkern/i386/_OSByteOrder.h
c_tdprpt.o: /usr/include/sys/_types/_u_char.h
c_tdprpt.o: /usr/include/sys/_types/_u_short.h
c_tdprpt.o: /usr/include/sys/_types/_u_int.h
c_tdprpt.o: /usr/include/sys/_types/_caddr_t.h
c_tdprpt.o: /usr/include/sys/_types/_dev_t.h
c_tdprpt.o: /usr/include/sys/_types/_blkcnt_t.h
c_tdprpt.o: /usr/include/sys/_types/_blksize_t.h
c_tdprpt.o: /usr/include/sys/_types/_gid_t.h
c_tdprpt.o: /usr/include/sys/_types/_in_addr_t.h
c_tdprpt.o: /usr/include/sys/_types/_in_port_t.h
c_tdprpt.o: /usr/include/sys/_types/_ino_t.h
c_tdprpt.o: /usr/include/sys/_types/_ino64_t.h
c_tdprpt.o: /usr/include/sys/_types/_key_t.h
c_tdprpt.o: /usr/include/sys/_types/_mode_t.h
c_tdprpt.o: /usr/include/sys/_types/_nlink_t.h
c_tdprpt.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_tdprpt.o: /usr/include/sys/_types/_uid_t.h
c_tdprpt.o: /usr/include/sys/_types/_clock_t.h
c_tdprpt.o: /usr/include/sys/_types/_time_t.h
c_tdprpt.o: /usr/include/sys/_types/_useconds_t.h
c_tdprpt.o: /usr/include/sys/_types/_suseconds_t.h
c_tdprpt.o: /usr/include/sys/_types/_rsize_t.h
c_tdprpt.o: /usr/include/sys/_types/_errno_t.h
c_tdprpt.o: /usr/include/sys/_types/_fd_def.h
c_tdprpt.o: /usr/include/sys/_types/_fd_setsize.h
c_tdprpt.o: /usr/include/sys/_types/_fd_set.h
c_tdprpt.o: /usr/include/sys/_types/_fd_clr.h
c_tdprpt.o: /usr/include/sys/_types/_fd_zero.h
c_tdprpt.o: /usr/include/sys/_types/_fd_isset.h
c_tdprpt.o: /usr/include/sys/_types/_fd_copy.h
c_tdprpt.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_tdprpt.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_tdprpt.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_tdprpt.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_tdprpt.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_tdprpt.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_tdprpt.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_tdprpt.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_tdprpt.o: /usr/include/sys/_pthread/_pthread_t.h
c_tdprpt.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_tdprpt.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_tdprpt.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_tdprpt.o: ../../../.././include/ncarg/c.h
c_tdseti.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_tdseti.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_tdseti.o: /usr/include/sys/_posix_availability.h
c_tdseti.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_tdseti.o: /usr/include/_types.h /usr/include/sys/_types.h
c_tdseti.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_tdseti.o: /usr/include/sys/_pthread/_pthread_types.h
c_tdseti.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_tdseti.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_tdseti.o: /usr/include/sys/_types/_int16_t.h
c_tdseti.o: /usr/include/sys/_types/_int32_t.h
c_tdseti.o: /usr/include/sys/_types/_int64_t.h
c_tdseti.o: /usr/include/sys/_types/_u_int8_t.h
c_tdseti.o: /usr/include/sys/_types/_u_int16_t.h
c_tdseti.o: /usr/include/sys/_types/_u_int32_t.h
c_tdseti.o: /usr/include/sys/_types/_u_int64_t.h
c_tdseti.o: /usr/include/sys/_types/_intptr_t.h
c_tdseti.o: /usr/include/sys/_types/_uintptr_t.h
c_tdseti.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_tdseti.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_tdseti.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_tdseti.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_tdseti.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_tdseti.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_tdseti.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_tdseti.o: /usr/include/libkern/i386/_OSByteOrder.h
c_tdseti.o: /usr/include/sys/_types/_u_char.h
c_tdseti.o: /usr/include/sys/_types/_u_short.h
c_tdseti.o: /usr/include/sys/_types/_u_int.h
c_tdseti.o: /usr/include/sys/_types/_caddr_t.h
c_tdseti.o: /usr/include/sys/_types/_dev_t.h
c_tdseti.o: /usr/include/sys/_types/_blkcnt_t.h
c_tdseti.o: /usr/include/sys/_types/_blksize_t.h
c_tdseti.o: /usr/include/sys/_types/_gid_t.h
c_tdseti.o: /usr/include/sys/_types/_in_addr_t.h
c_tdseti.o: /usr/include/sys/_types/_in_port_t.h
c_tdseti.o: /usr/include/sys/_types/_ino_t.h
c_tdseti.o: /usr/include/sys/_types/_ino64_t.h
c_tdseti.o: /usr/include/sys/_types/_key_t.h
c_tdseti.o: /usr/include/sys/_types/_mode_t.h
c_tdseti.o: /usr/include/sys/_types/_nlink_t.h
c_tdseti.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_tdseti.o: /usr/include/sys/_types/_uid_t.h
c_tdseti.o: /usr/include/sys/_types/_clock_t.h
c_tdseti.o: /usr/include/sys/_types/_time_t.h
c_tdseti.o: /usr/include/sys/_types/_useconds_t.h
c_tdseti.o: /usr/include/sys/_types/_suseconds_t.h
c_tdseti.o: /usr/include/sys/_types/_rsize_t.h
c_tdseti.o: /usr/include/sys/_types/_errno_t.h
c_tdseti.o: /usr/include/sys/_types/_fd_def.h
c_tdseti.o: /usr/include/sys/_types/_fd_setsize.h
c_tdseti.o: /usr/include/sys/_types/_fd_set.h
c_tdseti.o: /usr/include/sys/_types/_fd_clr.h
c_tdseti.o: /usr/include/sys/_types/_fd_zero.h
c_tdseti.o: /usr/include/sys/_types/_fd_isset.h
c_tdseti.o: /usr/include/sys/_types/_fd_copy.h
c_tdseti.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_tdseti.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_tdseti.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_tdseti.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_tdseti.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_tdseti.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_tdseti.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_tdseti.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_tdseti.o: /usr/include/sys/_pthread/_pthread_t.h
c_tdseti.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_tdseti.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_tdseti.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_tdseti.o: ../../../.././include/ncarg/c.h
c_tdsetr.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_tdsetr.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_tdsetr.o: /usr/include/sys/_posix_availability.h
c_tdsetr.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_tdsetr.o: /usr/include/_types.h /usr/include/sys/_types.h
c_tdsetr.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_tdsetr.o: /usr/include/sys/_pthread/_pthread_types.h
c_tdsetr.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_tdsetr.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_tdsetr.o: /usr/include/sys/_types/_int16_t.h
c_tdsetr.o: /usr/include/sys/_types/_int32_t.h
c_tdsetr.o: /usr/include/sys/_types/_int64_t.h
c_tdsetr.o: /usr/include/sys/_types/_u_int8_t.h
c_tdsetr.o: /usr/include/sys/_types/_u_int16_t.h
c_tdsetr.o: /usr/include/sys/_types/_u_int32_t.h
c_tdsetr.o: /usr/include/sys/_types/_u_int64_t.h
c_tdsetr.o: /usr/include/sys/_types/_intptr_t.h
c_tdsetr.o: /usr/include/sys/_types/_uintptr_t.h
c_tdsetr.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_tdsetr.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_tdsetr.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_tdsetr.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_tdsetr.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_tdsetr.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_tdsetr.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_tdsetr.o: /usr/include/libkern/i386/_OSByteOrder.h
c_tdsetr.o: /usr/include/sys/_types/_u_char.h
c_tdsetr.o: /usr/include/sys/_types/_u_short.h
c_tdsetr.o: /usr/include/sys/_types/_u_int.h
c_tdsetr.o: /usr/include/sys/_types/_caddr_t.h
c_tdsetr.o: /usr/include/sys/_types/_dev_t.h
c_tdsetr.o: /usr/include/sys/_types/_blkcnt_t.h
c_tdsetr.o: /usr/include/sys/_types/_blksize_t.h
c_tdsetr.o: /usr/include/sys/_types/_gid_t.h
c_tdsetr.o: /usr/include/sys/_types/_in_addr_t.h
c_tdsetr.o: /usr/include/sys/_types/_in_port_t.h
c_tdsetr.o: /usr/include/sys/_types/_ino_t.h
c_tdsetr.o: /usr/include/sys/_types/_ino64_t.h
c_tdsetr.o: /usr/include/sys/_types/_key_t.h
c_tdsetr.o: /usr/include/sys/_types/_mode_t.h
c_tdsetr.o: /usr/include/sys/_types/_nlink_t.h
c_tdsetr.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_tdsetr.o: /usr/include/sys/_types/_uid_t.h
c_tdsetr.o: /usr/include/sys/_types/_clock_t.h
c_tdsetr.o: /usr/include/sys/_types/_time_t.h
c_tdsetr.o: /usr/include/sys/_types/_useconds_t.h
c_tdsetr.o: /usr/include/sys/_types/_suseconds_t.h
c_tdsetr.o: /usr/include/sys/_types/_rsize_t.h
c_tdsetr.o: /usr/include/sys/_types/_errno_t.h
c_tdsetr.o: /usr/include/sys/_types/_fd_def.h
c_tdsetr.o: /usr/include/sys/_types/_fd_setsize.h
c_tdsetr.o: /usr/include/sys/_types/_fd_set.h
c_tdsetr.o: /usr/include/sys/_types/_fd_clr.h
c_tdsetr.o: /usr/include/sys/_types/_fd_zero.h
c_tdsetr.o: /usr/include/sys/_types/_fd_isset.h
c_tdsetr.o: /usr/include/sys/_types/_fd_copy.h
c_tdsetr.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_tdsetr.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_tdsetr.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_tdsetr.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_tdsetr.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_tdsetr.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_tdsetr.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_tdsetr.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_tdsetr.o: /usr/include/sys/_pthread/_pthread_t.h
c_tdsetr.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_tdsetr.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_tdsetr.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_tdsetr.o: ../../../.././include/ncarg/c.h
c_tdsort.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_tdsort.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_tdsort.o: /usr/include/sys/_posix_availability.h
c_tdsort.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_tdsort.o: /usr/include/_types.h /usr/include/sys/_types.h
c_tdsort.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_tdsort.o: /usr/include/sys/_pthread/_pthread_types.h
c_tdsort.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_tdsort.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_tdsort.o: /usr/include/sys/_types/_int16_t.h
c_tdsort.o: /usr/include/sys/_types/_int32_t.h
c_tdsort.o: /usr/include/sys/_types/_int64_t.h
c_tdsort.o: /usr/include/sys/_types/_u_int8_t.h
c_tdsort.o: /usr/include/sys/_types/_u_int16_t.h
c_tdsort.o: /usr/include/sys/_types/_u_int32_t.h
c_tdsort.o: /usr/include/sys/_types/_u_int64_t.h
c_tdsort.o: /usr/include/sys/_types/_intptr_t.h
c_tdsort.o: /usr/include/sys/_types/_uintptr_t.h
c_tdsort.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_tdsort.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_tdsort.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_tdsort.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_tdsort.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_tdsort.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_tdsort.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_tdsort.o: /usr/include/libkern/i386/_OSByteOrder.h
c_tdsort.o: /usr/include/sys/_types/_u_char.h
c_tdsort.o: /usr/include/sys/_types/_u_short.h
c_tdsort.o: /usr/include/sys/_types/_u_int.h
c_tdsort.o: /usr/include/sys/_types/_caddr_t.h
c_tdsort.o: /usr/include/sys/_types/_dev_t.h
c_tdsort.o: /usr/include/sys/_types/_blkcnt_t.h
c_tdsort.o: /usr/include/sys/_types/_blksize_t.h
c_tdsort.o: /usr/include/sys/_types/_gid_t.h
c_tdsort.o: /usr/include/sys/_types/_in_addr_t.h
c_tdsort.o: /usr/include/sys/_types/_in_port_t.h
c_tdsort.o: /usr/include/sys/_types/_ino_t.h
c_tdsort.o: /usr/include/sys/_types/_ino64_t.h
c_tdsort.o: /usr/include/sys/_types/_key_t.h
c_tdsort.o: /usr/include/sys/_types/_mode_t.h
c_tdsort.o: /usr/include/sys/_types/_nlink_t.h
c_tdsort.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_tdsort.o: /usr/include/sys/_types/_uid_t.h
c_tdsort.o: /usr/include/sys/_types/_clock_t.h
c_tdsort.o: /usr/include/sys/_types/_time_t.h
c_tdsort.o: /usr/include/sys/_types/_useconds_t.h
c_tdsort.o: /usr/include/sys/_types/_suseconds_t.h
c_tdsort.o: /usr/include/sys/_types/_rsize_t.h
c_tdsort.o: /usr/include/sys/_types/_errno_t.h
c_tdsort.o: /usr/include/sys/_types/_fd_def.h
c_tdsort.o: /usr/include/sys/_types/_fd_setsize.h
c_tdsort.o: /usr/include/sys/_types/_fd_set.h
c_tdsort.o: /usr/include/sys/_types/_fd_clr.h
c_tdsort.o: /usr/include/sys/_types/_fd_zero.h
c_tdsort.o: /usr/include/sys/_types/_fd_isset.h
c_tdsort.o: /usr/include/sys/_types/_fd_copy.h
c_tdsort.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_tdsort.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_tdsort.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_tdsort.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_tdsort.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_tdsort.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_tdsort.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_tdsort.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_tdsort.o: /usr/include/sys/_pthread/_pthread_t.h
c_tdsort.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_tdsort.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_tdsort.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_tdsort.o: ../../../.././include/ncarg/c.h
c_tdstri.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_tdstri.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_tdstri.o: /usr/include/sys/_posix_availability.h
c_tdstri.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_tdstri.o: /usr/include/_types.h /usr/include/sys/_types.h
c_tdstri.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_tdstri.o: /usr/include/sys/_pthread/_pthread_types.h
c_tdstri.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_tdstri.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_tdstri.o: /usr/include/sys/_types/_int16_t.h
c_tdstri.o: /usr/include/sys/_types/_int32_t.h
c_tdstri.o: /usr/include/sys/_types/_int64_t.h
c_tdstri.o: /usr/include/sys/_types/_u_int8_t.h
c_tdstri.o: /usr/include/sys/_types/_u_int16_t.h
c_tdstri.o: /usr/include/sys/_types/_u_int32_t.h
c_tdstri.o: /usr/include/sys/_types/_u_int64_t.h
c_tdstri.o: /usr/include/sys/_types/_intptr_t.h
c_tdstri.o: /usr/include/sys/_types/_uintptr_t.h
c_tdstri.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_tdstri.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_tdstri.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_tdstri.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_tdstri.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_tdstri.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_tdstri.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_tdstri.o: /usr/include/libkern/i386/_OSByteOrder.h
c_tdstri.o: /usr/include/sys/_types/_u_char.h
c_tdstri.o: /usr/include/sys/_types/_u_short.h
c_tdstri.o: /usr/include/sys/_types/_u_int.h
c_tdstri.o: /usr/include/sys/_types/_caddr_t.h
c_tdstri.o: /usr/include/sys/_types/_dev_t.h
c_tdstri.o: /usr/include/sys/_types/_blkcnt_t.h
c_tdstri.o: /usr/include/sys/_types/_blksize_t.h
c_tdstri.o: /usr/include/sys/_types/_gid_t.h
c_tdstri.o: /usr/include/sys/_types/_in_addr_t.h
c_tdstri.o: /usr/include/sys/_types/_in_port_t.h
c_tdstri.o: /usr/include/sys/_types/_ino_t.h
c_tdstri.o: /usr/include/sys/_types/_ino64_t.h
c_tdstri.o: /usr/include/sys/_types/_key_t.h
c_tdstri.o: /usr/include/sys/_types/_mode_t.h
c_tdstri.o: /usr/include/sys/_types/_nlink_t.h
c_tdstri.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_tdstri.o: /usr/include/sys/_types/_uid_t.h
c_tdstri.o: /usr/include/sys/_types/_clock_t.h
c_tdstri.o: /usr/include/sys/_types/_time_t.h
c_tdstri.o: /usr/include/sys/_types/_useconds_t.h
c_tdstri.o: /usr/include/sys/_types/_suseconds_t.h
c_tdstri.o: /usr/include/sys/_types/_rsize_t.h
c_tdstri.o: /usr/include/sys/_types/_errno_t.h
c_tdstri.o: /usr/include/sys/_types/_fd_def.h
c_tdstri.o: /usr/include/sys/_types/_fd_setsize.h
c_tdstri.o: /usr/include/sys/_types/_fd_set.h
c_tdstri.o: /usr/include/sys/_types/_fd_clr.h
c_tdstri.o: /usr/include/sys/_types/_fd_zero.h
c_tdstri.o: /usr/include/sys/_types/_fd_isset.h
c_tdstri.o: /usr/include/sys/_types/_fd_copy.h
c_tdstri.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_tdstri.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_tdstri.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_tdstri.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_tdstri.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_tdstri.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_tdstri.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_tdstri.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_tdstri.o: /usr/include/sys/_pthread/_pthread_t.h
c_tdstri.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_tdstri.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_tdstri.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_tdstri.o: ../../../.././include/ncarg/c.h
c_tdstrs.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_tdstrs.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_tdstrs.o: /usr/include/sys/_posix_availability.h
c_tdstrs.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_tdstrs.o: /usr/include/_types.h /usr/include/sys/_types.h
c_tdstrs.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_tdstrs.o: /usr/include/sys/_pthread/_pthread_types.h
c_tdstrs.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_tdstrs.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_tdstrs.o: /usr/include/sys/_types/_int16_t.h
c_tdstrs.o: /usr/include/sys/_types/_int32_t.h
c_tdstrs.o: /usr/include/sys/_types/_int64_t.h
c_tdstrs.o: /usr/include/sys/_types/_u_int8_t.h
c_tdstrs.o: /usr/include/sys/_types/_u_int16_t.h
c_tdstrs.o: /usr/include/sys/_types/_u_int32_t.h
c_tdstrs.o: /usr/include/sys/_types/_u_int64_t.h
c_tdstrs.o: /usr/include/sys/_types/_intptr_t.h
c_tdstrs.o: /usr/include/sys/_types/_uintptr_t.h
c_tdstrs.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_tdstrs.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_tdstrs.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_tdstrs.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_tdstrs.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_tdstrs.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_tdstrs.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_tdstrs.o: /usr/include/libkern/i386/_OSByteOrder.h
c_tdstrs.o: /usr/include/sys/_types/_u_char.h
c_tdstrs.o: /usr/include/sys/_types/_u_short.h
c_tdstrs.o: /usr/include/sys/_types/_u_int.h
c_tdstrs.o: /usr/include/sys/_types/_caddr_t.h
c_tdstrs.o: /usr/include/sys/_types/_dev_t.h
c_tdstrs.o: /usr/include/sys/_types/_blkcnt_t.h
c_tdstrs.o: /usr/include/sys/_types/_blksize_t.h
c_tdstrs.o: /usr/include/sys/_types/_gid_t.h
c_tdstrs.o: /usr/include/sys/_types/_in_addr_t.h
c_tdstrs.o: /usr/include/sys/_types/_in_port_t.h
c_tdstrs.o: /usr/include/sys/_types/_ino_t.h
c_tdstrs.o: /usr/include/sys/_types/_ino64_t.h
c_tdstrs.o: /usr/include/sys/_types/_key_t.h
c_tdstrs.o: /usr/include/sys/_types/_mode_t.h
c_tdstrs.o: /usr/include/sys/_types/_nlink_t.h
c_tdstrs.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_tdstrs.o: /usr/include/sys/_types/_uid_t.h
c_tdstrs.o: /usr/include/sys/_types/_clock_t.h
c_tdstrs.o: /usr/include/sys/_types/_time_t.h
c_tdstrs.o: /usr/include/sys/_types/_useconds_t.h
c_tdstrs.o: /usr/include/sys/_types/_suseconds_t.h
c_tdstrs.o: /usr/include/sys/_types/_rsize_t.h
c_tdstrs.o: /usr/include/sys/_types/_errno_t.h
c_tdstrs.o: /usr/include/sys/_types/_fd_def.h
c_tdstrs.o: /usr/include/sys/_types/_fd_setsize.h
c_tdstrs.o: /usr/include/sys/_types/_fd_set.h
c_tdstrs.o: /usr/include/sys/_types/_fd_clr.h
c_tdstrs.o: /usr/include/sys/_types/_fd_zero.h
c_tdstrs.o: /usr/include/sys/_types/_fd_isset.h
c_tdstrs.o: /usr/include/sys/_types/_fd_copy.h
c_tdstrs.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_tdstrs.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_tdstrs.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_tdstrs.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_tdstrs.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_tdstrs.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_tdstrs.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_tdstrs.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_tdstrs.o: /usr/include/sys/_pthread/_pthread_t.h
c_tdstrs.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_tdstrs.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_tdstrs.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_tdstrs.o: ../../../.././include/ncarg/c.h
c_tdctri.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_tdctri.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_tdctri.o: /usr/include/sys/_posix_availability.h
c_tdctri.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_tdctri.o: /usr/include/_types.h /usr/include/sys/_types.h
c_tdctri.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_tdctri.o: /usr/include/sys/_pthread/_pthread_types.h
c_tdctri.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_tdctri.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_tdctri.o: /usr/include/sys/_types/_int16_t.h
c_tdctri.o: /usr/include/sys/_types/_int32_t.h
c_tdctri.o: /usr/include/sys/_types/_int64_t.h
c_tdctri.o: /usr/include/sys/_types/_u_int8_t.h
c_tdctri.o: /usr/include/sys/_types/_u_int16_t.h
c_tdctri.o: /usr/include/sys/_types/_u_int32_t.h
c_tdctri.o: /usr/include/sys/_types/_u_int64_t.h
c_tdctri.o: /usr/include/sys/_types/_intptr_t.h
c_tdctri.o: /usr/include/sys/_types/_uintptr_t.h
c_tdctri.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_tdctri.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_tdctri.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_tdctri.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_tdctri.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_tdctri.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_tdctri.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_tdctri.o: /usr/include/libkern/i386/_OSByteOrder.h
c_tdctri.o: /usr/include/sys/_types/_u_char.h
c_tdctri.o: /usr/include/sys/_types/_u_short.h
c_tdctri.o: /usr/include/sys/_types/_u_int.h
c_tdctri.o: /usr/include/sys/_types/_caddr_t.h
c_tdctri.o: /usr/include/sys/_types/_dev_t.h
c_tdctri.o: /usr/include/sys/_types/_blkcnt_t.h
c_tdctri.o: /usr/include/sys/_types/_blksize_t.h
c_tdctri.o: /usr/include/sys/_types/_gid_t.h
c_tdctri.o: /usr/include/sys/_types/_in_addr_t.h
c_tdctri.o: /usr/include/sys/_types/_in_port_t.h
c_tdctri.o: /usr/include/sys/_types/_ino_t.h
c_tdctri.o: /usr/include/sys/_types/_ino64_t.h
c_tdctri.o: /usr/include/sys/_types/_key_t.h
c_tdctri.o: /usr/include/sys/_types/_mode_t.h
c_tdctri.o: /usr/include/sys/_types/_nlink_t.h
c_tdctri.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_tdctri.o: /usr/include/sys/_types/_uid_t.h
c_tdctri.o: /usr/include/sys/_types/_clock_t.h
c_tdctri.o: /usr/include/sys/_types/_time_t.h
c_tdctri.o: /usr/include/sys/_types/_useconds_t.h
c_tdctri.o: /usr/include/sys/_types/_suseconds_t.h
c_tdctri.o: /usr/include/sys/_types/_rsize_t.h
c_tdctri.o: /usr/include/sys/_types/_errno_t.h
c_tdctri.o: /usr/include/sys/_types/_fd_def.h
c_tdctri.o: /usr/include/sys/_types/_fd_setsize.h
c_tdctri.o: /usr/include/sys/_types/_fd_set.h
c_tdctri.o: /usr/include/sys/_types/_fd_clr.h
c_tdctri.o: /usr/include/sys/_types/_fd_zero.h
c_tdctri.o: /usr/include/sys/_types/_fd_isset.h
c_tdctri.o: /usr/include/sys/_types/_fd_copy.h
c_tdctri.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_tdctri.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_tdctri.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_tdctri.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_tdctri.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_tdctri.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_tdctri.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_tdctri.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_tdctri.o: /usr/include/sys/_pthread/_pthread_t.h
c_tdctri.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_tdctri.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_tdctri.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_tdctri.o: ../../../.././include/ncarg/c.h
c_tdlndp.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_tdlndp.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_tdlndp.o: /usr/include/sys/_posix_availability.h
c_tdlndp.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_tdlndp.o: /usr/include/_types.h /usr/include/sys/_types.h
c_tdlndp.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_tdlndp.o: /usr/include/sys/_pthread/_pthread_types.h
c_tdlndp.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_tdlndp.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_tdlndp.o: /usr/include/sys/_types/_int16_t.h
c_tdlndp.o: /usr/include/sys/_types/_int32_t.h
c_tdlndp.o: /usr/include/sys/_types/_int64_t.h
c_tdlndp.o: /usr/include/sys/_types/_u_int8_t.h
c_tdlndp.o: /usr/include/sys/_types/_u_int16_t.h
c_tdlndp.o: /usr/include/sys/_types/_u_int32_t.h
c_tdlndp.o: /usr/include/sys/_types/_u_int64_t.h
c_tdlndp.o: /usr/include/sys/_types/_intptr_t.h
c_tdlndp.o: /usr/include/sys/_types/_uintptr_t.h
c_tdlndp.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_tdlndp.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_tdlndp.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_tdlndp.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_tdlndp.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_tdlndp.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_tdlndp.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_tdlndp.o: /usr/include/libkern/i386/_OSByteOrder.h
c_tdlndp.o: /usr/include/sys/_types/_u_char.h
c_tdlndp.o: /usr/include/sys/_types/_u_short.h
c_tdlndp.o: /usr/include/sys/_types/_u_int.h
c_tdlndp.o: /usr/include/sys/_types/_caddr_t.h
c_tdlndp.o: /usr/include/sys/_types/_dev_t.h
c_tdlndp.o: /usr/include/sys/_types/_blkcnt_t.h
c_tdlndp.o: /usr/include/sys/_types/_blksize_t.h
c_tdlndp.o: /usr/include/sys/_types/_gid_t.h
c_tdlndp.o: /usr/include/sys/_types/_in_addr_t.h
c_tdlndp.o: /usr/include/sys/_types/_in_port_t.h
c_tdlndp.o: /usr/include/sys/_types/_ino_t.h
c_tdlndp.o: /usr/include/sys/_types/_ino64_t.h
c_tdlndp.o: /usr/include/sys/_types/_key_t.h
c_tdlndp.o: /usr/include/sys/_types/_mode_t.h
c_tdlndp.o: /usr/include/sys/_types/_nlink_t.h
c_tdlndp.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_tdlndp.o: /usr/include/sys/_types/_uid_t.h
c_tdlndp.o: /usr/include/sys/_types/_clock_t.h
c_tdlndp.o: /usr/include/sys/_types/_time_t.h
c_tdlndp.o: /usr/include/sys/_types/_useconds_t.h
c_tdlndp.o: /usr/include/sys/_types/_suseconds_t.h
c_tdlndp.o: /usr/include/sys/_types/_rsize_t.h
c_tdlndp.o: /usr/include/sys/_types/_errno_t.h
c_tdlndp.o: /usr/include/sys/_types/_fd_def.h
c_tdlndp.o: /usr/include/sys/_types/_fd_setsize.h
c_tdlndp.o: /usr/include/sys/_types/_fd_set.h
c_tdlndp.o: /usr/include/sys/_types/_fd_clr.h
c_tdlndp.o: /usr/include/sys/_types/_fd_zero.h
c_tdlndp.o: /usr/include/sys/_types/_fd_isset.h
c_tdlndp.o: /usr/include/sys/_types/_fd_copy.h
c_tdlndp.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_tdlndp.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_tdlndp.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_tdlndp.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_tdlndp.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_tdlndp.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_tdlndp.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_tdlndp.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_tdlndp.o: /usr/include/sys/_pthread/_pthread_t.h
c_tdlndp.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_tdlndp.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_tdlndp.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_tdlndp.o: ../../../.././include/ncarg/c.h
c_tdlpdp.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_tdlpdp.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_tdlpdp.o: /usr/include/sys/_posix_availability.h
c_tdlpdp.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_tdlpdp.o: /usr/include/_types.h /usr/include/sys/_types.h
c_tdlpdp.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_tdlpdp.o: /usr/include/sys/_pthread/_pthread_types.h
c_tdlpdp.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_tdlpdp.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_tdlpdp.o: /usr/include/sys/_types/_int16_t.h
c_tdlpdp.o: /usr/include/sys/_types/_int32_t.h
c_tdlpdp.o: /usr/include/sys/_types/_int64_t.h
c_tdlpdp.o: /usr/include/sys/_types/_u_int8_t.h
c_tdlpdp.o: /usr/include/sys/_types/_u_int16_t.h
c_tdlpdp.o: /usr/include/sys/_types/_u_int32_t.h
c_tdlpdp.o: /usr/include/sys/_types/_u_int64_t.h
c_tdlpdp.o: /usr/include/sys/_types/_intptr_t.h
c_tdlpdp.o: /usr/include/sys/_types/_uintptr_t.h
c_tdlpdp.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_tdlpdp.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_tdlpdp.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_tdlpdp.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_tdlpdp.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_tdlpdp.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_tdlpdp.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_tdlpdp.o: /usr/include/libkern/i386/_OSByteOrder.h
c_tdlpdp.o: /usr/include/sys/_types/_u_char.h
c_tdlpdp.o: /usr/include/sys/_types/_u_short.h
c_tdlpdp.o: /usr/include/sys/_types/_u_int.h
c_tdlpdp.o: /usr/include/sys/_types/_caddr_t.h
c_tdlpdp.o: /usr/include/sys/_types/_dev_t.h
c_tdlpdp.o: /usr/include/sys/_types/_blkcnt_t.h
c_tdlpdp.o: /usr/include/sys/_types/_blksize_t.h
c_tdlpdp.o: /usr/include/sys/_types/_gid_t.h
c_tdlpdp.o: /usr/include/sys/_types/_in_addr_t.h
c_tdlpdp.o: /usr/include/sys/_types/_in_port_t.h
c_tdlpdp.o: /usr/include/sys/_types/_ino_t.h
c_tdlpdp.o: /usr/include/sys/_types/_ino64_t.h
c_tdlpdp.o: /usr/include/sys/_types/_key_t.h
c_tdlpdp.o: /usr/include/sys/_types/_mode_t.h
c_tdlpdp.o: /usr/include/sys/_types/_nlink_t.h
c_tdlpdp.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_tdlpdp.o: /usr/include/sys/_types/_uid_t.h
c_tdlpdp.o: /usr/include/sys/_types/_clock_t.h
c_tdlpdp.o: /usr/include/sys/_types/_time_t.h
c_tdlpdp.o: /usr/include/sys/_types/_useconds_t.h
c_tdlpdp.o: /usr/include/sys/_types/_suseconds_t.h
c_tdlpdp.o: /usr/include/sys/_types/_rsize_t.h
c_tdlpdp.o: /usr/include/sys/_types/_errno_t.h
c_tdlpdp.o: /usr/include/sys/_types/_fd_def.h
c_tdlpdp.o: /usr/include/sys/_types/_fd_setsize.h
c_tdlpdp.o: /usr/include/sys/_types/_fd_set.h
c_tdlpdp.o: /usr/include/sys/_types/_fd_clr.h
c_tdlpdp.o: /usr/include/sys/_types/_fd_zero.h
c_tdlpdp.o: /usr/include/sys/_types/_fd_isset.h
c_tdlpdp.o: /usr/include/sys/_types/_fd_copy.h
c_tdlpdp.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_tdlpdp.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_tdlpdp.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_tdlpdp.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_tdlpdp.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_tdlpdp.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_tdlpdp.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_tdlpdp.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_tdlpdp.o: /usr/include/sys/_pthread/_pthread_t.h
c_tdlpdp.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_tdlpdp.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_tdlpdp.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_tdlpdp.o: ../../../.././include/ncarg/c.h
c_tdcurv.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_tdcurv.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_tdcurv.o: /usr/include/sys/_posix_availability.h
c_tdcurv.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_tdcurv.o: /usr/include/_types.h /usr/include/sys/_types.h
c_tdcurv.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_tdcurv.o: /usr/include/sys/_pthread/_pthread_types.h
c_tdcurv.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_tdcurv.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_tdcurv.o: /usr/include/sys/_types/_int16_t.h
c_tdcurv.o: /usr/include/sys/_types/_int32_t.h
c_tdcurv.o: /usr/include/sys/_types/_int64_t.h
c_tdcurv.o: /usr/include/sys/_types/_u_int8_t.h
c_tdcurv.o: /usr/include/sys/_types/_u_int16_t.h
c_tdcurv.o: /usr/include/sys/_types/_u_int32_t.h
c_tdcurv.o: /usr/include/sys/_types/_u_int64_t.h
c_tdcurv.o: /usr/include/sys/_types/_intptr_t.h
c_tdcurv.o: /usr/include/sys/_types/_uintptr_t.h
c_tdcurv.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_tdcurv.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_tdcurv.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_tdcurv.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_tdcurv.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_tdcurv.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_tdcurv.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_tdcurv.o: /usr/include/libkern/i386/_OSByteOrder.h
c_tdcurv.o: /usr/include/sys/_types/_u_char.h
c_tdcurv.o: /usr/include/sys/_types/_u_short.h
c_tdcurv.o: /usr/include/sys/_types/_u_int.h
c_tdcurv.o: /usr/include/sys/_types/_caddr_t.h
c_tdcurv.o: /usr/include/sys/_types/_dev_t.h
c_tdcurv.o: /usr/include/sys/_types/_blkcnt_t.h
c_tdcurv.o: /usr/include/sys/_types/_blksize_t.h
c_tdcurv.o: /usr/include/sys/_types/_gid_t.h
c_tdcurv.o: /usr/include/sys/_types/_in_addr_t.h
c_tdcurv.o: /usr/include/sys/_types/_in_port_t.h
c_tdcurv.o: /usr/include/sys/_types/_ino_t.h
c_tdcurv.o: /usr/include/sys/_types/_ino64_t.h
c_tdcurv.o: /usr/include/sys/_types/_key_t.h
c_tdcurv.o: /usr/include/sys/_types/_mode_t.h
c_tdcurv.o: /usr/include/sys/_types/_nlink_t.h
c_tdcurv.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_tdcurv.o: /usr/include/sys/_types/_uid_t.h
c_tdcurv.o: /usr/include/sys/_types/_clock_t.h
c_tdcurv.o: /usr/include/sys/_types/_time_t.h
c_tdcurv.o: /usr/include/sys/_types/_useconds_t.h
c_tdcurv.o: /usr/include/sys/_types/_suseconds_t.h
c_tdcurv.o: /usr/include/sys/_types/_rsize_t.h
c_tdcurv.o: /usr/include/sys/_types/_errno_t.h
c_tdcurv.o: /usr/include/sys/_types/_fd_def.h
c_tdcurv.o: /usr/include/sys/_types/_fd_setsize.h
c_tdcurv.o: /usr/include/sys/_types/_fd_set.h
c_tdcurv.o: /usr/include/sys/_types/_fd_clr.h
c_tdcurv.o: /usr/include/sys/_types/_fd_zero.h
c_tdcurv.o: /usr/include/sys/_types/_fd_isset.h
c_tdcurv.o: /usr/include/sys/_types/_fd_copy.h
c_tdcurv.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_tdcurv.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_tdcurv.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_tdcurv.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_tdcurv.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_tdcurv.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_tdcurv.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_tdcurv.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_tdcurv.o: /usr/include/sys/_pthread/_pthread_t.h
c_tdcurv.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_tdcurv.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_tdcurv.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_tdcurv.o: ../../../.././include/ncarg/c.h
c_tdcudp.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_tdcudp.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_tdcudp.o: /usr/include/sys/_posix_availability.h
c_tdcudp.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_tdcudp.o: /usr/include/_types.h /usr/include/sys/_types.h
c_tdcudp.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_tdcudp.o: /usr/include/sys/_pthread/_pthread_types.h
c_tdcudp.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_tdcudp.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_tdcudp.o: /usr/include/sys/_types/_int16_t.h
c_tdcudp.o: /usr/include/sys/_types/_int32_t.h
c_tdcudp.o: /usr/include/sys/_types/_int64_t.h
c_tdcudp.o: /usr/include/sys/_types/_u_int8_t.h
c_tdcudp.o: /usr/include/sys/_types/_u_int16_t.h
c_tdcudp.o: /usr/include/sys/_types/_u_int32_t.h
c_tdcudp.o: /usr/include/sys/_types/_u_int64_t.h
c_tdcudp.o: /usr/include/sys/_types/_intptr_t.h
c_tdcudp.o: /usr/include/sys/_types/_uintptr_t.h
c_tdcudp.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_tdcudp.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_tdcudp.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_tdcudp.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_tdcudp.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_tdcudp.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_tdcudp.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_tdcudp.o: /usr/include/libkern/i386/_OSByteOrder.h
c_tdcudp.o: /usr/include/sys/_types/_u_char.h
c_tdcudp.o: /usr/include/sys/_types/_u_short.h
c_tdcudp.o: /usr/include/sys/_types/_u_int.h
c_tdcudp.o: /usr/include/sys/_types/_caddr_t.h
c_tdcudp.o: /usr/include/sys/_types/_dev_t.h
c_tdcudp.o: /usr/include/sys/_types/_blkcnt_t.h
c_tdcudp.o: /usr/include/sys/_types/_blksize_t.h
c_tdcudp.o: /usr/include/sys/_types/_gid_t.h
c_tdcudp.o: /usr/include/sys/_types/_in_addr_t.h
c_tdcudp.o: /usr/include/sys/_types/_in_port_t.h
c_tdcudp.o: /usr/include/sys/_types/_ino_t.h
c_tdcudp.o: /usr/include/sys/_types/_ino64_t.h
c_tdcudp.o: /usr/include/sys/_types/_key_t.h
c_tdcudp.o: /usr/include/sys/_types/_mode_t.h
c_tdcudp.o: /usr/include/sys/_types/_nlink_t.h
c_tdcudp.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_tdcudp.o: /usr/include/sys/_types/_uid_t.h
c_tdcudp.o: /usr/include/sys/_types/_clock_t.h
c_tdcudp.o: /usr/include/sys/_types/_time_t.h
c_tdcudp.o: /usr/include/sys/_types/_useconds_t.h
c_tdcudp.o: /usr/include/sys/_types/_suseconds_t.h
c_tdcudp.o: /usr/include/sys/_types/_rsize_t.h
c_tdcudp.o: /usr/include/sys/_types/_errno_t.h
c_tdcudp.o: /usr/include/sys/_types/_fd_def.h
c_tdcudp.o: /usr/include/sys/_types/_fd_setsize.h
c_tdcudp.o: /usr/include/sys/_types/_fd_set.h
c_tdcudp.o: /usr/include/sys/_types/_fd_clr.h
c_tdcudp.o: /usr/include/sys/_types/_fd_zero.h
c_tdcudp.o: /usr/include/sys/_types/_fd_isset.h
c_tdcudp.o: /usr/include/sys/_types/_fd_copy.h
c_tdcudp.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_tdcudp.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_tdcudp.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_tdcudp.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_tdcudp.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_tdcudp.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_tdcudp.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_tdcudp.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_tdcudp.o: /usr/include/sys/_pthread/_pthread_t.h
c_tdcudp.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_tdcudp.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_tdcudp.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_tdcudp.o: ../../../.././include/ncarg/c.h
