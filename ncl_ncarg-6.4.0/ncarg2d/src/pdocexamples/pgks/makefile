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
CURRENT_DIR		= ./ncarg2d/src/pdocexamples/pgks

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
#	$Id: yMakefile,v 1.7 2004-06-23 16:04:42 haley Exp $
#
MYNAME	= pgks

FSRCS =	pgkex01.f pgkex02.f pgkex03.f pgkex04.f pgkex05.f pgkex06.f  		pgkex07.f pgkex08.f pgkex09.f pgkex10.f pgkex11.f pgkex12.f  		pgkex13.f pgkex14.f pgkex15.f pgkex16.f pgkex17.f pgkex18.f  		pgkex19.f pgkex20.f pgkex21.f pgkex22.f pgkex23.f pgkex24.f  		pgkex25.f pgkex26.f pgkex27.f

CSRCS = c_gcell.c

FOBJS =	pgkex01.o pgkex02.o pgkex03.o pgkex04.o pgkex05.o pgkex06.o  		pgkex07.o pgkex08.o pgkex09.o pgkex10.o pgkex11.o pgkex12.o  		pgkex13.o pgkex14.o pgkex15.o pgkex16.o pgkex17.o pgkex18.o  		pgkex19.o pgkex20.o pgkex21.o pgkex22.o pgkex23.o pgkex24.o  		pgkex25.o pgkex26.o pgkex27.o

SRCS = $(FSRCS) $(CSRCS)

PROG_LIBS		= $(NCARGLIBS)
SYS_LIBS		= -lm
EXTRA_LIBS		= $(XLIB)

all::

all-local:: $(SRCS)

install-local:: $(SRCS)
	@if (test ! -d $(EXAMPLESPATH)); then ($(MKDIRHIER) $(EXAMPLESPATH)); fi
	@for name in $(SRCS); do \
	if (test -f $$name.$(EXE_SUFFIX)) then \
	(echo "Installing $$name.$(EXE_SUFFIX)";\
	$(INSTALL) -m $(INSTALL_LIB) $$name.$(EXE_SUFFIX) $(EXAMPLESPATH));\
	else \
	(echo "Installing $$name";\
	$(INSTALL) -m $(INSTALL_LIB) $$name $(EXAMPLESPATH)); fi \
	done

examples:: pgkex01

pgkex01: pgkex01.o
	$(F77_LD) $(FC_LDOPTIONS) -o pgkex01 pgkex01.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) pgkex01 pgkex01.o

all::

clean-local::
	-@$(RM) pgkex01 pgkex01.o

examples:: pgkex02

pgkex02: pgkex02.o
	$(F77_LD) $(FC_LDOPTIONS) -o pgkex02 pgkex02.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) pgkex02 pgkex02.o

all::

clean-local::
	-@$(RM) pgkex02 pgkex02.o

examples:: pgkex03

pgkex03: pgkex03.o
	$(F77_LD) $(FC_LDOPTIONS) -o pgkex03 pgkex03.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) pgkex03 pgkex03.o

all::

clean-local::
	-@$(RM) pgkex03 pgkex03.o

examples:: pgkex04

pgkex04: pgkex04.o
	$(F77_LD) $(FC_LDOPTIONS) -o pgkex04 pgkex04.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) pgkex04 pgkex04.o

all::

clean-local::
	-@$(RM) pgkex04 pgkex04.o

examples:: pgkex05

pgkex05: pgkex05.o
	$(F77_LD) $(FC_LDOPTIONS) -o pgkex05 pgkex05.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) pgkex05 pgkex05.o

all::

clean-local::
	-@$(RM) pgkex05 pgkex05.o

examples:: pgkex06

pgkex06: pgkex06.o
	$(F77_LD) $(FC_LDOPTIONS) -o pgkex06 pgkex06.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) pgkex06 pgkex06.o

all::

clean-local::
	-@$(RM) pgkex06 pgkex06.o

examples:: pgkex07

pgkex07: pgkex07.o
	$(F77_LD) $(FC_LDOPTIONS) -o pgkex07 pgkex07.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) pgkex07 pgkex07.o

all::

clean-local::
	-@$(RM) pgkex07 pgkex07.o

examples:: pgkex08

pgkex08: pgkex08.o
	$(F77_LD) $(FC_LDOPTIONS) -o pgkex08 pgkex08.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) pgkex08 pgkex08.o

all::

clean-local::
	-@$(RM) pgkex08 pgkex08.o

examples:: pgkex09

pgkex09: pgkex09.o
	$(F77_LD) $(FC_LDOPTIONS) -o pgkex09 pgkex09.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) pgkex09 pgkex09.o

all::

clean-local::
	-@$(RM) pgkex09 pgkex09.o

examples:: pgkex10

pgkex10: pgkex10.o
	$(F77_LD) $(FC_LDOPTIONS) -o pgkex10 pgkex10.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) pgkex10 pgkex10.o

all::

clean-local::
	-@$(RM) pgkex10 pgkex10.o

examples:: pgkex11

pgkex11: pgkex11.o
	$(F77_LD) $(FC_LDOPTIONS) -o pgkex11 pgkex11.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) pgkex11 pgkex11.o

all::

clean-local::
	-@$(RM) pgkex11 pgkex11.o

examples:: pgkex12

pgkex12: pgkex12.o
	$(F77_LD) $(FC_LDOPTIONS) -o pgkex12 pgkex12.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) pgkex12 pgkex12.o

all::

clean-local::
	-@$(RM) pgkex12 pgkex12.o

examples:: pgkex13

pgkex13: pgkex13.o
	$(F77_LD) $(FC_LDOPTIONS) -o pgkex13 pgkex13.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) pgkex13 pgkex13.o

all::

clean-local::
	-@$(RM) pgkex13 pgkex13.o

examples:: pgkex14

pgkex14: pgkex14.o
	$(F77_LD) $(FC_LDOPTIONS) -o pgkex14 pgkex14.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) pgkex14 pgkex14.o

all::

clean-local::
	-@$(RM) pgkex14 pgkex14.o

examples:: pgkex15

pgkex15: pgkex15.o
	$(F77_LD) $(FC_LDOPTIONS) -o pgkex15 pgkex15.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) pgkex15 pgkex15.o

all::

clean-local::
	-@$(RM) pgkex15 pgkex15.o

examples:: pgkex16

pgkex16: pgkex16.o
	$(F77_LD) $(FC_LDOPTIONS) -o pgkex16 pgkex16.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) pgkex16 pgkex16.o

all::

clean-local::
	-@$(RM) pgkex16 pgkex16.o

examples:: pgkex17

pgkex17: pgkex17.o
	$(F77_LD) $(FC_LDOPTIONS) -o pgkex17 pgkex17.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) pgkex17 pgkex17.o

all::

clean-local::
	-@$(RM) pgkex17 pgkex17.o

examples:: pgkex18

pgkex18: pgkex18.o
	$(F77_LD) $(FC_LDOPTIONS) -o pgkex18 pgkex18.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) pgkex18 pgkex18.o

all::

clean-local::
	-@$(RM) pgkex18 pgkex18.o

examples:: pgkex19

pgkex19: pgkex19.o
	$(F77_LD) $(FC_LDOPTIONS) -o pgkex19 pgkex19.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) pgkex19 pgkex19.o

all::

clean-local::
	-@$(RM) pgkex19 pgkex19.o

examples:: pgkex20

pgkex20: pgkex20.o
	$(F77_LD) $(FC_LDOPTIONS) -o pgkex20 pgkex20.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) pgkex20 pgkex20.o

all::

clean-local::
	-@$(RM) pgkex20 pgkex20.o

examples:: pgkex21

pgkex21: pgkex21.o
	$(F77_LD) $(FC_LDOPTIONS) -o pgkex21 pgkex21.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) pgkex21 pgkex21.o

all::

clean-local::
	-@$(RM) pgkex21 pgkex21.o

examples:: pgkex22

pgkex22: pgkex22.o
	$(F77_LD) $(FC_LDOPTIONS) -o pgkex22 pgkex22.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) pgkex22 pgkex22.o

all::

clean-local::
	-@$(RM) pgkex22 pgkex22.o

examples:: pgkex23

pgkex23: pgkex23.o
	$(F77_LD) $(FC_LDOPTIONS) -o pgkex23 pgkex23.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) pgkex23 pgkex23.o

all::

clean-local::
	-@$(RM) pgkex23 pgkex23.o

examples:: pgkex24

pgkex24: pgkex24.o
	$(F77_LD) $(FC_LDOPTIONS) -o pgkex24 pgkex24.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) pgkex24 pgkex24.o

all::

clean-local::
	-@$(RM) pgkex24 pgkex24.o

examples:: pgkex25

pgkex25: pgkex25.o
	$(F77_LD) $(FC_LDOPTIONS) -o pgkex25 pgkex25.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) pgkex25 pgkex25.o

all::

clean-local::
	-@$(RM) pgkex25 pgkex25.o

examples:: pgkex26

pgkex26: pgkex26.o
	$(F77_LD) $(FC_LDOPTIONS) -o pgkex26 pgkex26.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) pgkex26 pgkex26.o

all::

clean-local::
	-@$(RM) pgkex26 pgkex26.o

examples:: pgkex27

pgkex27: pgkex27.o
	$(F77_LD) $(FC_LDOPTIONS) -o pgkex27 pgkex27.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) pgkex27 pgkex27.o

all::

clean-local::
	-@$(RM) pgkex27 pgkex27.o

examples:: c_gcell

c_gcell: c_gcell.o
	$(CC_LD) $(CC_LDOPTIONS) -o c_gcell c_gcell.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) c_gcell c_gcell.o

all::

clean-local::
	-@$(RM) c_gcell c_gcell.o

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

