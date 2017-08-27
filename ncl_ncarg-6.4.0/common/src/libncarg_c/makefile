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

TOP			= ../../../.
CURRENT_DIR		= ./common/src/libncarg_c

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
#
#	$Id: yMakefile,v 1.58 2010-02-25 18:19:50 haley Exp $
#
#
#	Makefile for libncarg_c -- NCAR G common utilities
#
#

MYNAME		= libncarg_c

EXCSRCS = bcopyswap.c logic32.c
EXFSRCS = gbytes.f sbytes.f
EXOBJS  = sbytes.o gbytes.o bcopyswap.o logic32.o

FCPP_SRCS = argget.f
FCPP_OBJS = argget.o

HDRS	= c.h
CSRCS	= $(EXCSRCS) options.c misc.c ncarg_path.c version.c  	error.c atoargv.c usleep.c env.c trnspprt.c  	rwchinfl.c sdbm.c mangle.c

FSRCS	= $(EXFSRCS) $(FCPP_SRCS) lblank.f iargct.f i1mach.f r1mach.f  	d1mach.f

OBJS	= $(EXOBJS) $(FCPP_OBJS) options.o misc.o ncarg_path.o  	version.o error.o atoargv.o usleep.o env.o trnspprt.o lblank.o  	iargct.o rwchinfl.o sdbm.o i1mach.o r1mach.o d1mach.o mangle.o

EXTRA_CDEFINES =	$(OS_DEF) $(VENDOR_DEF) $(VDEFINE) $(MY_DEFINES) -DNCARGURL='$(NCARGURL)'

REQLIBS = $(CTOFLIBS) -lc

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

all-local:: $(HDRS)

install-local:: $(HDRS)
	@if (test ! -d $(NCARGINCPATH)); then ($(MKDIRHIER) $(NCARGINCPATH)); fi
	@for name in $(HDRS); do \
	if (test -f $$name.$(EXE_SUFFIX)) then \
	(echo "Installing $$name.$(EXE_SUFFIX)";\
	$(INSTALL) -m $(INSTALL_INC) $$name.$(EXE_SUFFIX) $(NCARGINCPATH));\
	else \
	(echo "Installing $$name";\
	$(INSTALL) -m $(INSTALL_INC) $$name $(NCARGINCPATH)); fi \
	done

includes-local:: c.h

all::

includes-local::
	@if (test ! -d $(BUILDINCDIR)/ncarg); then ($(MKDIRHIER) $(BUILDINCDIR)/ncarg); fi
	@cd $(BUILDINCDIR)/ncarg;\
	$(RM) $(HDRS); \
	for i in $(HDRS); do \
	(echo "$(LN) dstupdir/$(BUILDINCTOP)/$(CURRENT_DIR)/$$i .";\
	$(LN) ../$(BUILDINCTOP)/$(CURRENT_DIR)/$$i .;)\
	done

all::

depend-local:: $(CSRCS)
	$(DEPEND) -s "# DO NOT DELETE" -- $(ALLDEFINES) -- $(CSRCS)

usleep.o: usleep.c
	$(CC) $(CCOPTIONS) $(COPT) -c usleep.c

sdbm.o: sdbm.c
	$(CC) $(CCOPTIONS) $(COPT) -c sdbm.c

all::

clean-local::
	-@$(RM) ncarg_path.c c.h

ncarg_path.c: ncarg_path.c.sed Makefile
	@sed  	-e 's+SED_ENV_DEFS+$(NGENV_DESCRIPT)+'  	<ncarg_path.c.sed >ncarg_path.c

c.h: c.h.sed Makefile
	@sed  	-e 's+SED_NGCALLF+$(NGCALLF)+'  	<c.h.sed >c.h

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

bcopyswap.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
bcopyswap.o: /usr/include/sys/_symbol_aliasing.h
bcopyswap.o: /usr/include/sys/_posix_availability.h
bcopyswap.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
bcopyswap.o: /usr/include/_types.h /usr/include/sys/_types.h
bcopyswap.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
bcopyswap.o: /usr/include/sys/_pthread/_pthread_types.h
bcopyswap.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
bcopyswap.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
bcopyswap.o: /usr/include/sys/_types/_int16_t.h
bcopyswap.o: /usr/include/sys/_types/_int32_t.h
bcopyswap.o: /usr/include/sys/_types/_int64_t.h
bcopyswap.o: /usr/include/sys/_types/_u_int8_t.h
bcopyswap.o: /usr/include/sys/_types/_u_int16_t.h
bcopyswap.o: /usr/include/sys/_types/_u_int32_t.h
bcopyswap.o: /usr/include/sys/_types/_u_int64_t.h
bcopyswap.o: /usr/include/sys/_types/_intptr_t.h
bcopyswap.o: /usr/include/sys/_types/_uintptr_t.h
bcopyswap.o: /usr/include/sys/_types/_size_t.h
bcopyswap.o: /usr/include/sys/_types/_null.h /usr/include/sys/stdio.h
bcopyswap.o: /usr/include/sys/_types/_off_t.h
bcopyswap.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
bcopyswap.o: /usr/include/secure/_common.h /usr/include/stdlib.h
bcopyswap.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
bcopyswap.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
bcopyswap.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
bcopyswap.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
bcopyswap.o: /usr/include/i386/_mcontext.h
bcopyswap.o: /usr/include/mach/machine/_structs.h
bcopyswap.o: /usr/include/mach/i386/_structs.h
bcopyswap.o: /usr/include/sys/_pthread/_pthread_attr_t.h
bcopyswap.o: /usr/include/sys/_types/_sigaltstack.h
bcopyswap.o: /usr/include/sys/_types/_ucontext.h
bcopyswap.o: /usr/include/sys/_types/_sigset_t.h
bcopyswap.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
bcopyswap.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
bcopyswap.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
bcopyswap.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
bcopyswap.o: /usr/include/_types/_uintmax_t.h
bcopyswap.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
bcopyswap.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
bcopyswap.o: /usr/include/libkern/_OSByteOrder.h
bcopyswap.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
bcopyswap.o: /usr/include/sys/_types/_ct_rune_t.h
bcopyswap.o: /usr/include/sys/_types/_rune_t.h
bcopyswap.o: /usr/include/sys/_types/_wchar_t.h
bcopyswap.o: /usr/include/sys/_types/_dev_t.h
bcopyswap.o: /usr/include/sys/_types/_mode_t.h
logic32.o: ../../.././include/ncarg/c.h /usr/include/stdio.h
logic32.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
logic32.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
logic32.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
logic32.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
logic32.o: /usr/include/i386/_types.h
logic32.o: /usr/include/sys/_pthread/_pthread_types.h
logic32.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
logic32.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
logic32.o: /usr/include/sys/_types/_int16_t.h
logic32.o: /usr/include/sys/_types/_int32_t.h
logic32.o: /usr/include/sys/_types/_int64_t.h
logic32.o: /usr/include/sys/_types/_u_int8_t.h
logic32.o: /usr/include/sys/_types/_u_int16_t.h
logic32.o: /usr/include/sys/_types/_u_int32_t.h
logic32.o: /usr/include/sys/_types/_u_int64_t.h
logic32.o: /usr/include/sys/_types/_intptr_t.h
logic32.o: /usr/include/sys/_types/_uintptr_t.h
logic32.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
logic32.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
logic32.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
logic32.o: /usr/include/secure/_common.h /usr/include/sys/types.h
logic32.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
logic32.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
logic32.o: /usr/include/libkern/_OSByteOrder.h
logic32.o: /usr/include/libkern/i386/_OSByteOrder.h
logic32.o: /usr/include/sys/_types/_u_char.h
logic32.o: /usr/include/sys/_types/_u_short.h
logic32.o: /usr/include/sys/_types/_u_int.h
logic32.o: /usr/include/sys/_types/_caddr_t.h
logic32.o: /usr/include/sys/_types/_dev_t.h
logic32.o: /usr/include/sys/_types/_blkcnt_t.h
logic32.o: /usr/include/sys/_types/_blksize_t.h
logic32.o: /usr/include/sys/_types/_gid_t.h
logic32.o: /usr/include/sys/_types/_in_addr_t.h
logic32.o: /usr/include/sys/_types/_in_port_t.h
logic32.o: /usr/include/sys/_types/_ino_t.h
logic32.o: /usr/include/sys/_types/_ino64_t.h
logic32.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
logic32.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
logic32.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
logic32.o: /usr/include/sys/_types/_clock_t.h
logic32.o: /usr/include/sys/_types/_time_t.h
logic32.o: /usr/include/sys/_types/_useconds_t.h
logic32.o: /usr/include/sys/_types/_suseconds_t.h
logic32.o: /usr/include/sys/_types/_rsize_t.h
logic32.o: /usr/include/sys/_types/_errno_t.h
logic32.o: /usr/include/sys/_types/_fd_def.h
logic32.o: /usr/include/sys/_types/_fd_setsize.h
logic32.o: /usr/include/sys/_types/_fd_set.h
logic32.o: /usr/include/sys/_types/_fd_clr.h
logic32.o: /usr/include/sys/_types/_fd_zero.h
logic32.o: /usr/include/sys/_types/_fd_isset.h
logic32.o: /usr/include/sys/_types/_fd_copy.h
logic32.o: /usr/include/sys/_pthread/_pthread_attr_t.h
logic32.o: /usr/include/sys/_pthread/_pthread_cond_t.h
logic32.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
logic32.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
logic32.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
logic32.o: /usr/include/sys/_pthread/_pthread_once_t.h
logic32.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
logic32.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
logic32.o: /usr/include/sys/_pthread/_pthread_t.h
logic32.o: /usr/include/sys/_pthread/_pthread_key_t.h
logic32.o: /usr/include/sys/_types/_fsblkcnt_t.h
logic32.o: /usr/include/sys/_types/_fsfilcnt_t.h
options.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
options.o: /usr/include/sys/_symbol_aliasing.h
options.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
options.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
options.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
options.o: /usr/include/i386/_types.h
options.o: /usr/include/sys/_pthread/_pthread_types.h
options.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
options.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
options.o: /usr/include/sys/_types/_int16_t.h
options.o: /usr/include/sys/_types/_int32_t.h
options.o: /usr/include/sys/_types/_int64_t.h
options.o: /usr/include/sys/_types/_u_int8_t.h
options.o: /usr/include/sys/_types/_u_int16_t.h
options.o: /usr/include/sys/_types/_u_int32_t.h
options.o: /usr/include/sys/_types/_u_int64_t.h
options.o: /usr/include/sys/_types/_intptr_t.h
options.o: /usr/include/sys/_types/_uintptr_t.h
options.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
options.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
options.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
options.o: /usr/include/secure/_common.h /usr/include/stdlib.h
options.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
options.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
options.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
options.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
options.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
options.o: /usr/include/mach/i386/_structs.h
options.o: /usr/include/sys/_pthread/_pthread_attr_t.h
options.o: /usr/include/sys/_types/_sigaltstack.h
options.o: /usr/include/sys/_types/_ucontext.h
options.o: /usr/include/sys/_types/_sigset_t.h
options.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
options.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
options.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
options.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
options.o: /usr/include/_types/_uintmax_t.h
options.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
options.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
options.o: /usr/include/libkern/_OSByteOrder.h
options.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
options.o: /usr/include/sys/_types/_ct_rune_t.h
options.o: /usr/include/sys/_types/_rune_t.h
options.o: /usr/include/sys/_types/_wchar_t.h
options.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_mode_t.h
options.o: /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
options.o: /usr/include/sys/_types/_u_short.h
options.o: /usr/include/sys/_types/_u_int.h
options.o: /usr/include/sys/_types/_caddr_t.h
options.o: /usr/include/sys/_types/_blkcnt_t.h
options.o: /usr/include/sys/_types/_blksize_t.h
options.o: /usr/include/sys/_types/_gid_t.h
options.o: /usr/include/sys/_types/_in_addr_t.h
options.o: /usr/include/sys/_types/_in_port_t.h
options.o: /usr/include/sys/_types/_ino_t.h
options.o: /usr/include/sys/_types/_ino64_t.h
options.o: /usr/include/sys/_types/_key_t.h
options.o: /usr/include/sys/_types/_nlink_t.h
options.o: /usr/include/sys/_types/_clock_t.h
options.o: /usr/include/sys/_types/_time_t.h
options.o: /usr/include/sys/_types/_useconds_t.h
options.o: /usr/include/sys/_types/_suseconds_t.h
options.o: /usr/include/sys/_types/_rsize_t.h
options.o: /usr/include/sys/_types/_errno_t.h
options.o: /usr/include/sys/_types/_fd_def.h
options.o: /usr/include/sys/_types/_fd_setsize.h
options.o: /usr/include/sys/_types/_fd_set.h
options.o: /usr/include/sys/_types/_fd_clr.h
options.o: /usr/include/sys/_types/_fd_zero.h
options.o: /usr/include/sys/_types/_fd_isset.h
options.o: /usr/include/sys/_types/_fd_copy.h
options.o: /usr/include/sys/_pthread/_pthread_cond_t.h
options.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
options.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
options.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
options.o: /usr/include/sys/_pthread/_pthread_once_t.h
options.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
options.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
options.o: /usr/include/sys/_pthread/_pthread_t.h
options.o: /usr/include/sys/_pthread/_pthread_key_t.h
options.o: /usr/include/sys/_types/_fsblkcnt_t.h
options.o: /usr/include/sys/_types/_fsfilcnt_t.h /usr/include/errno.h
options.o: /usr/include/sys/errno.h /usr/include/string.h
options.o: /usr/include/strings.h /usr/include/secure/_strings.h
options.o: /usr/include/secure/_string.h /usr/include/ctype.h
options.o: /usr/include/runetype.h /usr/include/sys/_types/_wint_t.h c.h
options.o: options.h
misc.o: /usr/include/ctype.h /usr/include/sys/cdefs.h
misc.o: /usr/include/sys/_symbol_aliasing.h
misc.o: /usr/include/sys/_posix_availability.h /usr/include/runetype.h
misc.o: /usr/include/_types.h /usr/include/sys/_types.h
misc.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
misc.o: /usr/include/sys/_pthread/_pthread_types.h
misc.o: /usr/include/sys/_types/_size_t.h
misc.o: /usr/include/sys/_types/_ct_rune_t.h
misc.o: /usr/include/sys/_types/_rune_t.h /usr/include/sys/_types/_wchar_t.h
misc.o: /usr/include/sys/_types/_wint_t.h /usr/include/stdlib.h
misc.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
misc.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
misc.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
misc.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
misc.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
misc.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
misc.o: /usr/include/mach/i386/_structs.h /usr/include/machine/types.h
misc.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
misc.o: /usr/include/sys/_types/_int16_t.h /usr/include/sys/_types/_int32_t.h
misc.o: /usr/include/sys/_types/_int64_t.h
misc.o: /usr/include/sys/_types/_u_int8_t.h
misc.o: /usr/include/sys/_types/_u_int16_t.h
misc.o: /usr/include/sys/_types/_u_int32_t.h
misc.o: /usr/include/sys/_types/_u_int64_t.h
misc.o: /usr/include/sys/_types/_intptr_t.h
misc.o: /usr/include/sys/_types/_uintptr_t.h
misc.o: /usr/include/sys/_pthread/_pthread_attr_t.h
misc.o: /usr/include/sys/_types/_sigaltstack.h
misc.o: /usr/include/sys/_types/_ucontext.h
misc.o: /usr/include/sys/_types/_sigset_t.h /usr/include/sys/_types/_uid_t.h
misc.o: /usr/include/sys/resource.h /usr/include/stdint.h
misc.o: /usr/include/_types/_uint8_t.h /usr/include/_types/_uint16_t.h
misc.o: /usr/include/_types/_uint32_t.h /usr/include/_types/_uint64_t.h
misc.o: /usr/include/_types/_intmax_t.h /usr/include/_types/_uintmax_t.h
misc.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
misc.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
misc.o: /usr/include/libkern/_OSByteOrder.h
misc.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
misc.o: /usr/include/sys/_types/_null.h /usr/include/sys/_types/_dev_t.h
misc.o: /usr/include/sys/_types/_mode_t.h /usr/include/string.h
misc.o: /usr/include/sys/_types/_rsize_t.h /usr/include/sys/_types/_errno_t.h
misc.o: /usr/include/sys/_types/_ssize_t.h /usr/include/strings.h
misc.o: /usr/include/secure/_strings.h /usr/include/secure/_common.h
misc.o: /usr/include/secure/_string.h /usr/include/errno.h
misc.o: /usr/include/sys/errno.h c.h /usr/include/stdio.h
misc.o: /usr/include/sys/_types/_va_list.h /usr/include/sys/stdio.h
misc.o: /usr/include/sys/_types/_off_t.h /usr/include/secure/_stdio.h
misc.o: /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
misc.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
misc.o: /usr/include/sys/_types/_caddr_t.h
misc.o: /usr/include/sys/_types/_blkcnt_t.h
misc.o: /usr/include/sys/_types/_blksize_t.h /usr/include/sys/_types/_gid_t.h
misc.o: /usr/include/sys/_types/_in_addr_t.h
misc.o: /usr/include/sys/_types/_in_port_t.h /usr/include/sys/_types/_ino_t.h
misc.o: /usr/include/sys/_types/_ino64_t.h /usr/include/sys/_types/_key_t.h
misc.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_clock_t.h
misc.o: /usr/include/sys/_types/_time_t.h
misc.o: /usr/include/sys/_types/_useconds_t.h
misc.o: /usr/include/sys/_types/_suseconds_t.h
misc.o: /usr/include/sys/_types/_fd_def.h
misc.o: /usr/include/sys/_types/_fd_setsize.h
misc.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
misc.o: /usr/include/sys/_types/_fd_zero.h
misc.o: /usr/include/sys/_types/_fd_isset.h
misc.o: /usr/include/sys/_types/_fd_copy.h
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
misc.o: /usr/include/sys/_types/_fsfilcnt_t.h
ncarg_path.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
ncarg_path.o: /usr/include/sys/_symbol_aliasing.h
ncarg_path.o: /usr/include/sys/_posix_availability.h
ncarg_path.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
ncarg_path.o: /usr/include/_types.h /usr/include/sys/_types.h
ncarg_path.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
ncarg_path.o: /usr/include/sys/_pthread/_pthread_types.h
ncarg_path.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
ncarg_path.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
ncarg_path.o: /usr/include/sys/_types/_int16_t.h
ncarg_path.o: /usr/include/sys/_types/_int32_t.h
ncarg_path.o: /usr/include/sys/_types/_int64_t.h
ncarg_path.o: /usr/include/sys/_types/_u_int8_t.h
ncarg_path.o: /usr/include/sys/_types/_u_int16_t.h
ncarg_path.o: /usr/include/sys/_types/_u_int32_t.h
ncarg_path.o: /usr/include/sys/_types/_u_int64_t.h
ncarg_path.o: /usr/include/sys/_types/_intptr_t.h
ncarg_path.o: /usr/include/sys/_types/_uintptr_t.h
ncarg_path.o: /usr/include/sys/_types/_size_t.h
ncarg_path.o: /usr/include/sys/_types/_null.h /usr/include/sys/stdio.h
ncarg_path.o: /usr/include/sys/_types/_off_t.h
ncarg_path.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
ncarg_path.o: /usr/include/secure/_common.h /usr/include/stdlib.h
ncarg_path.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
ncarg_path.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
ncarg_path.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
ncarg_path.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
ncarg_path.o: /usr/include/i386/_mcontext.h
ncarg_path.o: /usr/include/mach/machine/_structs.h
ncarg_path.o: /usr/include/mach/i386/_structs.h
ncarg_path.o: /usr/include/sys/_pthread/_pthread_attr_t.h
ncarg_path.o: /usr/include/sys/_types/_sigaltstack.h
ncarg_path.o: /usr/include/sys/_types/_ucontext.h
ncarg_path.o: /usr/include/sys/_types/_sigset_t.h
ncarg_path.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
ncarg_path.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
ncarg_path.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
ncarg_path.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
ncarg_path.o: /usr/include/_types/_uintmax_t.h
ncarg_path.o: /usr/include/sys/_types/_timeval.h
ncarg_path.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
ncarg_path.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
ncarg_path.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
ncarg_path.o: /usr/include/sys/_types/_ct_rune_t.h
ncarg_path.o: /usr/include/sys/_types/_rune_t.h
ncarg_path.o: /usr/include/sys/_types/_wchar_t.h
ncarg_path.o: /usr/include/sys/_types/_dev_t.h
ncarg_path.o: /usr/include/sys/_types/_mode_t.h /usr/include/string.h
ncarg_path.o: /usr/include/sys/_types/_rsize_t.h
ncarg_path.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
ncarg_path.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
ncarg_path.o: /usr/include/ctype.h /usr/include/runetype.h
ncarg_path.o: /usr/include/sys/_types/_wint_t.h /usr/include/sys/types.h
ncarg_path.o: /usr/include/sys/_types/_u_char.h
ncarg_path.o: /usr/include/sys/_types/_u_short.h
ncarg_path.o: /usr/include/sys/_types/_u_int.h
ncarg_path.o: /usr/include/sys/_types/_caddr_t.h
ncarg_path.o: /usr/include/sys/_types/_blkcnt_t.h
ncarg_path.o: /usr/include/sys/_types/_blksize_t.h
ncarg_path.o: /usr/include/sys/_types/_gid_t.h
ncarg_path.o: /usr/include/sys/_types/_in_addr_t.h
ncarg_path.o: /usr/include/sys/_types/_in_port_t.h
ncarg_path.o: /usr/include/sys/_types/_ino_t.h
ncarg_path.o: /usr/include/sys/_types/_ino64_t.h
ncarg_path.o: /usr/include/sys/_types/_key_t.h
ncarg_path.o: /usr/include/sys/_types/_nlink_t.h
ncarg_path.o: /usr/include/sys/_types/_clock_t.h
ncarg_path.o: /usr/include/sys/_types/_time_t.h
ncarg_path.o: /usr/include/sys/_types/_useconds_t.h
ncarg_path.o: /usr/include/sys/_types/_suseconds_t.h
ncarg_path.o: /usr/include/sys/_types/_fd_def.h
ncarg_path.o: /usr/include/sys/_types/_fd_setsize.h
ncarg_path.o: /usr/include/sys/_types/_fd_set.h
ncarg_path.o: /usr/include/sys/_types/_fd_clr.h
ncarg_path.o: /usr/include/sys/_types/_fd_zero.h
ncarg_path.o: /usr/include/sys/_types/_fd_isset.h
ncarg_path.o: /usr/include/sys/_types/_fd_copy.h
ncarg_path.o: /usr/include/sys/_pthread/_pthread_cond_t.h
ncarg_path.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
ncarg_path.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
ncarg_path.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
ncarg_path.o: /usr/include/sys/_pthread/_pthread_once_t.h
ncarg_path.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
ncarg_path.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
ncarg_path.o: /usr/include/sys/_pthread/_pthread_t.h
ncarg_path.o: /usr/include/sys/_pthread/_pthread_key_t.h
ncarg_path.o: /usr/include/sys/_types/_fsblkcnt_t.h
ncarg_path.o: /usr/include/sys/_types/_fsfilcnt_t.h /usr/include/sys/stat.h
ncarg_path.o: /usr/include/sys/_types/_timespec.h
ncarg_path.o: /usr/include/sys/_types/_s_ifmt.h
ncarg_path.o: /usr/include/sys/_types/_filesec_t.h /usr/include/unistd.h
ncarg_path.o: /usr/include/sys/unistd.h
ncarg_path.o: /usr/include/sys/_types/_posix_vdisable.h
ncarg_path.o: /usr/include/sys/_types/_seek_set.h /usr/include/sys/select.h
ncarg_path.o: /usr/include/sys/_select.h /usr/include/sys/_types/_uuid_t.h
ncarg_path.o: /usr/include/gethostuuid.h /usr/include/pwd.h
ncarg_path.o: /usr/include/uuid/uuid.h /usr/include/errno.h
ncarg_path.o: /usr/include/sys/errno.h ../../.././include/ncarg/c.h
ncarg_path.o: ncarg_path.h
version.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
version.o: /usr/include/sys/_symbol_aliasing.h
version.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
version.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
version.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
version.o: /usr/include/i386/_types.h
version.o: /usr/include/sys/_pthread/_pthread_types.h
version.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
version.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
version.o: /usr/include/sys/_types/_int16_t.h
version.o: /usr/include/sys/_types/_int32_t.h
version.o: /usr/include/sys/_types/_int64_t.h
version.o: /usr/include/sys/_types/_u_int8_t.h
version.o: /usr/include/sys/_types/_u_int16_t.h
version.o: /usr/include/sys/_types/_u_int32_t.h
version.o: /usr/include/sys/_types/_u_int64_t.h
version.o: /usr/include/sys/_types/_intptr_t.h
version.o: /usr/include/sys/_types/_uintptr_t.h
version.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
version.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
version.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
version.o: /usr/include/secure/_common.h c.h /usr/include/sys/types.h
version.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
version.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
version.o: /usr/include/libkern/_OSByteOrder.h
version.o: /usr/include/libkern/i386/_OSByteOrder.h
version.o: /usr/include/sys/_types/_u_char.h
version.o: /usr/include/sys/_types/_u_short.h
version.o: /usr/include/sys/_types/_u_int.h
version.o: /usr/include/sys/_types/_caddr_t.h
version.o: /usr/include/sys/_types/_dev_t.h
version.o: /usr/include/sys/_types/_blkcnt_t.h
version.o: /usr/include/sys/_types/_blksize_t.h
version.o: /usr/include/sys/_types/_gid_t.h
version.o: /usr/include/sys/_types/_in_addr_t.h
version.o: /usr/include/sys/_types/_in_port_t.h
version.o: /usr/include/sys/_types/_ino_t.h
version.o: /usr/include/sys/_types/_ino64_t.h
version.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
version.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
version.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
version.o: /usr/include/sys/_types/_clock_t.h
version.o: /usr/include/sys/_types/_time_t.h
version.o: /usr/include/sys/_types/_useconds_t.h
version.o: /usr/include/sys/_types/_suseconds_t.h
version.o: /usr/include/sys/_types/_rsize_t.h
version.o: /usr/include/sys/_types/_errno_t.h
version.o: /usr/include/sys/_types/_fd_def.h
version.o: /usr/include/sys/_types/_fd_setsize.h
version.o: /usr/include/sys/_types/_fd_set.h
version.o: /usr/include/sys/_types/_fd_clr.h
version.o: /usr/include/sys/_types/_fd_zero.h
version.o: /usr/include/sys/_types/_fd_isset.h
version.o: /usr/include/sys/_types/_fd_copy.h
version.o: /usr/include/sys/_pthread/_pthread_attr_t.h
version.o: /usr/include/sys/_pthread/_pthread_cond_t.h
version.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
version.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
version.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
version.o: /usr/include/sys/_pthread/_pthread_once_t.h
version.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
version.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
version.o: /usr/include/sys/_pthread/_pthread_t.h
version.o: /usr/include/sys/_pthread/_pthread_key_t.h
version.o: /usr/include/sys/_types/_fsblkcnt_t.h
version.o: /usr/include/sys/_types/_fsfilcnt_t.h
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
error.o: /usr/include/secure/_common.h /usr/include/string.h
error.o: /usr/include/sys/_types/_rsize_t.h
error.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
error.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
error.o: /usr/include/errno.h /usr/include/sys/errno.h c.h
error.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
error.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
error.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
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
error.o: /usr/include/sys/_types/_fsfilcnt_t.h
atoargv.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
atoargv.o: /usr/include/sys/_symbol_aliasing.h
atoargv.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
atoargv.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
atoargv.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
atoargv.o: /usr/include/i386/_types.h
atoargv.o: /usr/include/sys/_pthread/_pthread_types.h
atoargv.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
atoargv.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
atoargv.o: /usr/include/sys/_types/_int16_t.h
atoargv.o: /usr/include/sys/_types/_int32_t.h
atoargv.o: /usr/include/sys/_types/_int64_t.h
atoargv.o: /usr/include/sys/_types/_u_int8_t.h
atoargv.o: /usr/include/sys/_types/_u_int16_t.h
atoargv.o: /usr/include/sys/_types/_u_int32_t.h
atoargv.o: /usr/include/sys/_types/_u_int64_t.h
atoargv.o: /usr/include/sys/_types/_intptr_t.h
atoargv.o: /usr/include/sys/_types/_uintptr_t.h
atoargv.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
atoargv.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
atoargv.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
atoargv.o: /usr/include/secure/_common.h /usr/include/stdlib.h
atoargv.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
atoargv.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
atoargv.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
atoargv.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
atoargv.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
atoargv.o: /usr/include/mach/i386/_structs.h
atoargv.o: /usr/include/sys/_pthread/_pthread_attr_t.h
atoargv.o: /usr/include/sys/_types/_sigaltstack.h
atoargv.o: /usr/include/sys/_types/_ucontext.h
atoargv.o: /usr/include/sys/_types/_sigset_t.h
atoargv.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
atoargv.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
atoargv.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
atoargv.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
atoargv.o: /usr/include/_types/_uintmax_t.h
atoargv.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
atoargv.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
atoargv.o: /usr/include/libkern/_OSByteOrder.h
atoargv.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
atoargv.o: /usr/include/sys/_types/_ct_rune_t.h
atoargv.o: /usr/include/sys/_types/_rune_t.h
atoargv.o: /usr/include/sys/_types/_wchar_t.h
atoargv.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_mode_t.h
atoargv.o: /usr/include/string.h /usr/include/sys/_types/_rsize_t.h
atoargv.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
atoargv.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
atoargv.o: /usr/include/ctype.h /usr/include/runetype.h
atoargv.o: /usr/include/sys/_types/_wint_t.h /usr/include/errno.h
atoargv.o: /usr/include/sys/errno.h c.h /usr/include/sys/types.h
atoargv.o: /usr/include/sys/_types/_u_char.h
atoargv.o: /usr/include/sys/_types/_u_short.h
atoargv.o: /usr/include/sys/_types/_u_int.h
atoargv.o: /usr/include/sys/_types/_caddr_t.h
atoargv.o: /usr/include/sys/_types/_blkcnt_t.h
atoargv.o: /usr/include/sys/_types/_blksize_t.h
atoargv.o: /usr/include/sys/_types/_gid_t.h
atoargv.o: /usr/include/sys/_types/_in_addr_t.h
atoargv.o: /usr/include/sys/_types/_in_port_t.h
atoargv.o: /usr/include/sys/_types/_ino_t.h
atoargv.o: /usr/include/sys/_types/_ino64_t.h
atoargv.o: /usr/include/sys/_types/_key_t.h
atoargv.o: /usr/include/sys/_types/_nlink_t.h
atoargv.o: /usr/include/sys/_types/_clock_t.h
atoargv.o: /usr/include/sys/_types/_time_t.h
atoargv.o: /usr/include/sys/_types/_useconds_t.h
atoargv.o: /usr/include/sys/_types/_suseconds_t.h
atoargv.o: /usr/include/sys/_types/_fd_def.h
atoargv.o: /usr/include/sys/_types/_fd_setsize.h
atoargv.o: /usr/include/sys/_types/_fd_set.h
atoargv.o: /usr/include/sys/_types/_fd_clr.h
atoargv.o: /usr/include/sys/_types/_fd_zero.h
atoargv.o: /usr/include/sys/_types/_fd_isset.h
atoargv.o: /usr/include/sys/_types/_fd_copy.h
atoargv.o: /usr/include/sys/_pthread/_pthread_cond_t.h
atoargv.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
atoargv.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
atoargv.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
atoargv.o: /usr/include/sys/_pthread/_pthread_once_t.h
atoargv.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
atoargv.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
atoargv.o: /usr/include/sys/_pthread/_pthread_t.h
atoargv.o: /usr/include/sys/_pthread/_pthread_key_t.h
atoargv.o: /usr/include/sys/_types/_fsblkcnt_t.h
atoargv.o: /usr/include/sys/_types/_fsfilcnt_t.h
usleep.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
usleep.o: /usr/include/sys/_symbol_aliasing.h
usleep.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
usleep.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
usleep.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
usleep.o: /usr/include/i386/_types.h
usleep.o: /usr/include/sys/_pthread/_pthread_types.h
usleep.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
usleep.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
usleep.o: /usr/include/sys/_types/_int16_t.h
usleep.o: /usr/include/sys/_types/_int32_t.h
usleep.o: /usr/include/sys/_types/_int64_t.h
usleep.o: /usr/include/sys/_types/_u_int8_t.h
usleep.o: /usr/include/sys/_types/_u_int16_t.h
usleep.o: /usr/include/sys/_types/_u_int32_t.h
usleep.o: /usr/include/sys/_types/_u_int64_t.h
usleep.o: /usr/include/sys/_types/_intptr_t.h
usleep.o: /usr/include/sys/_types/_uintptr_t.h
usleep.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
usleep.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
usleep.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
usleep.o: /usr/include/secure/_common.h /usr/include/sys/types.h
usleep.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
usleep.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
usleep.o: /usr/include/libkern/_OSByteOrder.h
usleep.o: /usr/include/libkern/i386/_OSByteOrder.h
usleep.o: /usr/include/sys/_types/_u_char.h
usleep.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
usleep.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
usleep.o: /usr/include/sys/_types/_blkcnt_t.h
usleep.o: /usr/include/sys/_types/_blksize_t.h
usleep.o: /usr/include/sys/_types/_gid_t.h
usleep.o: /usr/include/sys/_types/_in_addr_t.h
usleep.o: /usr/include/sys/_types/_in_port_t.h
usleep.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
usleep.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
usleep.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
usleep.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
usleep.o: /usr/include/sys/_types/_clock_t.h
usleep.o: /usr/include/sys/_types/_time_t.h
usleep.o: /usr/include/sys/_types/_useconds_t.h
usleep.o: /usr/include/sys/_types/_suseconds_t.h
usleep.o: /usr/include/sys/_types/_rsize_t.h
usleep.o: /usr/include/sys/_types/_errno_t.h
usleep.o: /usr/include/sys/_types/_fd_def.h
usleep.o: /usr/include/sys/_types/_fd_setsize.h
usleep.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
usleep.o: /usr/include/sys/_types/_fd_zero.h
usleep.o: /usr/include/sys/_types/_fd_isset.h
usleep.o: /usr/include/sys/_types/_fd_copy.h
usleep.o: /usr/include/sys/_pthread/_pthread_attr_t.h
usleep.o: /usr/include/sys/_pthread/_pthread_cond_t.h
usleep.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
usleep.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
usleep.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
usleep.o: /usr/include/sys/_pthread/_pthread_once_t.h
usleep.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
usleep.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
usleep.o: /usr/include/sys/_pthread/_pthread_t.h
usleep.o: /usr/include/sys/_pthread/_pthread_key_t.h
usleep.o: /usr/include/sys/_types/_fsblkcnt_t.h
usleep.o: /usr/include/sys/_types/_fsfilcnt_t.h /usr/include/sys/time.h
usleep.o: /usr/include/sys/_types/_timespec.h
usleep.o: /usr/include/sys/_types/_timeval.h
usleep.o: /usr/include/sys/_types/_timeval64.h /usr/include/time.h
usleep.o: /usr/include/sys/_select.h /usr/include/sys/param.h
usleep.o: /usr/include/sys/syslimits.h /usr/include/machine/param.h
usleep.o: /usr/include/i386/param.h /usr/include/i386/_param.h
usleep.o: /usr/include/limits.h /usr/include/machine/limits.h
usleep.o: /usr/include/i386/limits.h /usr/include/i386/_limits.h
usleep.o: /usr/include/sys/signal.h /usr/include/machine/signal.h
usleep.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
usleep.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
usleep.o: /usr/include/mach/i386/_structs.h
usleep.o: /usr/include/sys/_types/_sigaltstack.h
usleep.o: /usr/include/sys/_types/_ucontext.h
usleep.o: /usr/include/sys/_types/_sigset_t.h /usr/include/unistd.h
usleep.o: /usr/include/sys/unistd.h /usr/include/sys/_types/_posix_vdisable.h
usleep.o: /usr/include/sys/_types/_seek_set.h /usr/include/_types/_uint64_t.h
usleep.o: /usr/include/_types/_uint32_t.h /usr/include/sys/select.h
usleep.o: /usr/include/sys/_types/_uuid_t.h /usr/include/gethostuuid.h
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
env.o: ../../.././include/ncarg/c.h /usr/include/sys/types.h
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
env.o: /usr/include/sys/_types/_fsfilcnt_t.h ncarg_path.h
trnspprt.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
trnspprt.o: /usr/include/sys/_symbol_aliasing.h
trnspprt.o: /usr/include/sys/_posix_availability.h
trnspprt.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
trnspprt.o: /usr/include/_types.h /usr/include/sys/_types.h
trnspprt.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
trnspprt.o: /usr/include/sys/_pthread/_pthread_types.h
trnspprt.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
trnspprt.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
trnspprt.o: /usr/include/sys/_types/_int16_t.h
trnspprt.o: /usr/include/sys/_types/_int32_t.h
trnspprt.o: /usr/include/sys/_types/_int64_t.h
trnspprt.o: /usr/include/sys/_types/_u_int8_t.h
trnspprt.o: /usr/include/sys/_types/_u_int16_t.h
trnspprt.o: /usr/include/sys/_types/_u_int32_t.h
trnspprt.o: /usr/include/sys/_types/_u_int64_t.h
trnspprt.o: /usr/include/sys/_types/_intptr_t.h
trnspprt.o: /usr/include/sys/_types/_uintptr_t.h
trnspprt.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
trnspprt.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
trnspprt.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
trnspprt.o: /usr/include/secure/_common.h /usr/include/stdlib.h
trnspprt.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
trnspprt.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
trnspprt.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
trnspprt.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
trnspprt.o: /usr/include/i386/_mcontext.h
trnspprt.o: /usr/include/mach/machine/_structs.h
trnspprt.o: /usr/include/mach/i386/_structs.h
trnspprt.o: /usr/include/sys/_pthread/_pthread_attr_t.h
trnspprt.o: /usr/include/sys/_types/_sigaltstack.h
trnspprt.o: /usr/include/sys/_types/_ucontext.h
trnspprt.o: /usr/include/sys/_types/_sigset_t.h
trnspprt.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
trnspprt.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
trnspprt.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
trnspprt.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
trnspprt.o: /usr/include/_types/_uintmax_t.h
trnspprt.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
trnspprt.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
trnspprt.o: /usr/include/libkern/_OSByteOrder.h
trnspprt.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
trnspprt.o: /usr/include/sys/_types/_ct_rune_t.h
trnspprt.o: /usr/include/sys/_types/_rune_t.h
trnspprt.o: /usr/include/sys/_types/_wchar_t.h
trnspprt.o: /usr/include/sys/_types/_dev_t.h
trnspprt.o: /usr/include/sys/_types/_mode_t.h /usr/include/unistd.h
trnspprt.o: /usr/include/sys/unistd.h
trnspprt.o: /usr/include/sys/_types/_posix_vdisable.h
trnspprt.o: /usr/include/sys/_types/_seek_set.h
trnspprt.o: /usr/include/sys/_types/_gid_t.h
trnspprt.o: /usr/include/sys/_types/_useconds_t.h /usr/include/sys/select.h
trnspprt.o: /usr/include/sys/_types/_fd_def.h
trnspprt.o: /usr/include/sys/_types/_timespec.h
trnspprt.o: /usr/include/sys/_types/_time_t.h
trnspprt.o: /usr/include/sys/_types/_suseconds_t.h
trnspprt.o: /usr/include/sys/_types/_fd_setsize.h
trnspprt.o: /usr/include/sys/_types/_fd_set.h
trnspprt.o: /usr/include/sys/_types/_fd_clr.h
trnspprt.o: /usr/include/sys/_types/_fd_isset.h
trnspprt.o: /usr/include/sys/_types/_fd_zero.h
trnspprt.o: /usr/include/sys/_types/_fd_copy.h /usr/include/sys/_select.h
trnspprt.o: /usr/include/sys/_types/_uuid_t.h /usr/include/gethostuuid.h
trnspprt.o: /usr/include/fcntl.h /usr/include/sys/fcntl.h
trnspprt.o: /usr/include/sys/_types/_o_sync.h
trnspprt.o: /usr/include/sys/_types/_o_dsync.h
trnspprt.o: /usr/include/sys/_types/_s_ifmt.h
trnspprt.o: /usr/include/sys/_types/_filesec_t.h ../../.././include/ncarg/c.h
trnspprt.o: /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
trnspprt.o: /usr/include/sys/_types/_u_short.h
trnspprt.o: /usr/include/sys/_types/_u_int.h
trnspprt.o: /usr/include/sys/_types/_caddr_t.h
trnspprt.o: /usr/include/sys/_types/_blkcnt_t.h
trnspprt.o: /usr/include/sys/_types/_blksize_t.h
trnspprt.o: /usr/include/sys/_types/_in_addr_t.h
trnspprt.o: /usr/include/sys/_types/_in_port_t.h
trnspprt.o: /usr/include/sys/_types/_ino_t.h
trnspprt.o: /usr/include/sys/_types/_ino64_t.h
trnspprt.o: /usr/include/sys/_types/_key_t.h
trnspprt.o: /usr/include/sys/_types/_nlink_t.h
trnspprt.o: /usr/include/sys/_types/_clock_t.h
trnspprt.o: /usr/include/sys/_types/_rsize_t.h
trnspprt.o: /usr/include/sys/_types/_errno_t.h
trnspprt.o: /usr/include/sys/_pthread/_pthread_cond_t.h
trnspprt.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
trnspprt.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
trnspprt.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
trnspprt.o: /usr/include/sys/_pthread/_pthread_once_t.h
trnspprt.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
trnspprt.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
trnspprt.o: /usr/include/sys/_pthread/_pthread_t.h
trnspprt.o: /usr/include/sys/_pthread/_pthread_key_t.h
trnspprt.o: /usr/include/sys/_types/_fsblkcnt_t.h
trnspprt.o: /usr/include/sys/_types/_fsfilcnt_t.h
rwchinfl.o: ../../.././include/ncarg/c.h /usr/include/stdio.h
rwchinfl.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
rwchinfl.o: /usr/include/sys/_posix_availability.h
rwchinfl.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
rwchinfl.o: /usr/include/_types.h /usr/include/sys/_types.h
rwchinfl.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
rwchinfl.o: /usr/include/sys/_pthread/_pthread_types.h
rwchinfl.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
rwchinfl.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
rwchinfl.o: /usr/include/sys/_types/_int16_t.h
rwchinfl.o: /usr/include/sys/_types/_int32_t.h
rwchinfl.o: /usr/include/sys/_types/_int64_t.h
rwchinfl.o: /usr/include/sys/_types/_u_int8_t.h
rwchinfl.o: /usr/include/sys/_types/_u_int16_t.h
rwchinfl.o: /usr/include/sys/_types/_u_int32_t.h
rwchinfl.o: /usr/include/sys/_types/_u_int64_t.h
rwchinfl.o: /usr/include/sys/_types/_intptr_t.h
rwchinfl.o: /usr/include/sys/_types/_uintptr_t.h
rwchinfl.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
rwchinfl.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
rwchinfl.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
rwchinfl.o: /usr/include/secure/_common.h /usr/include/sys/types.h
rwchinfl.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
rwchinfl.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
rwchinfl.o: /usr/include/libkern/_OSByteOrder.h
rwchinfl.o: /usr/include/libkern/i386/_OSByteOrder.h
rwchinfl.o: /usr/include/sys/_types/_u_char.h
rwchinfl.o: /usr/include/sys/_types/_u_short.h
rwchinfl.o: /usr/include/sys/_types/_u_int.h
rwchinfl.o: /usr/include/sys/_types/_caddr_t.h
rwchinfl.o: /usr/include/sys/_types/_dev_t.h
rwchinfl.o: /usr/include/sys/_types/_blkcnt_t.h
rwchinfl.o: /usr/include/sys/_types/_blksize_t.h
rwchinfl.o: /usr/include/sys/_types/_gid_t.h
rwchinfl.o: /usr/include/sys/_types/_in_addr_t.h
rwchinfl.o: /usr/include/sys/_types/_in_port_t.h
rwchinfl.o: /usr/include/sys/_types/_ino_t.h
rwchinfl.o: /usr/include/sys/_types/_ino64_t.h
rwchinfl.o: /usr/include/sys/_types/_key_t.h
rwchinfl.o: /usr/include/sys/_types/_mode_t.h
rwchinfl.o: /usr/include/sys/_types/_nlink_t.h
rwchinfl.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
rwchinfl.o: /usr/include/sys/_types/_uid_t.h
rwchinfl.o: /usr/include/sys/_types/_clock_t.h
rwchinfl.o: /usr/include/sys/_types/_time_t.h
rwchinfl.o: /usr/include/sys/_types/_useconds_t.h
rwchinfl.o: /usr/include/sys/_types/_suseconds_t.h
rwchinfl.o: /usr/include/sys/_types/_rsize_t.h
rwchinfl.o: /usr/include/sys/_types/_errno_t.h
rwchinfl.o: /usr/include/sys/_types/_fd_def.h
rwchinfl.o: /usr/include/sys/_types/_fd_setsize.h
rwchinfl.o: /usr/include/sys/_types/_fd_set.h
rwchinfl.o: /usr/include/sys/_types/_fd_clr.h
rwchinfl.o: /usr/include/sys/_types/_fd_zero.h
rwchinfl.o: /usr/include/sys/_types/_fd_isset.h
rwchinfl.o: /usr/include/sys/_types/_fd_copy.h
rwchinfl.o: /usr/include/sys/_pthread/_pthread_attr_t.h
rwchinfl.o: /usr/include/sys/_pthread/_pthread_cond_t.h
rwchinfl.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
rwchinfl.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
rwchinfl.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
rwchinfl.o: /usr/include/sys/_pthread/_pthread_once_t.h
rwchinfl.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
rwchinfl.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
rwchinfl.o: /usr/include/sys/_pthread/_pthread_t.h
rwchinfl.o: /usr/include/sys/_pthread/_pthread_key_t.h
rwchinfl.o: /usr/include/sys/_types/_fsblkcnt_t.h
rwchinfl.o: /usr/include/sys/_types/_fsfilcnt_t.h /usr/include/stdlib.h
rwchinfl.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
rwchinfl.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
rwchinfl.o: /usr/include/machine/_mcontext.h /usr/include/i386/_mcontext.h
rwchinfl.o: /usr/include/mach/machine/_structs.h
rwchinfl.o: /usr/include/mach/i386/_structs.h
rwchinfl.o: /usr/include/sys/_types/_sigaltstack.h
rwchinfl.o: /usr/include/sys/_types/_ucontext.h
rwchinfl.o: /usr/include/sys/_types/_sigset_t.h /usr/include/sys/resource.h
rwchinfl.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
rwchinfl.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
rwchinfl.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
rwchinfl.o: /usr/include/_types/_uintmax_t.h
rwchinfl.o: /usr/include/sys/_types/_timeval.h /usr/include/alloca.h
rwchinfl.o: /usr/include/sys/_types/_ct_rune_t.h
rwchinfl.o: /usr/include/sys/_types/_rune_t.h
rwchinfl.o: /usr/include/sys/_types/_wchar_t.h /usr/include/sys/file.h
rwchinfl.o: /usr/include/sys/fcntl.h /usr/include/sys/_types/_o_sync.h
rwchinfl.o: /usr/include/sys/_types/_o_dsync.h
rwchinfl.o: /usr/include/sys/_types/_seek_set.h
rwchinfl.o: /usr/include/sys/_types/_s_ifmt.h
rwchinfl.o: /usr/include/sys/_types/_timespec.h
rwchinfl.o: /usr/include/sys/_types/_filesec_t.h /usr/include/sys/unistd.h
rwchinfl.o: /usr/include/sys/_types/_posix_vdisable.h
rwchinfl.o: /usr/include/sys/queue.h /usr/include/fcntl.h
rwchinfl.o: /usr/include/unistd.h /usr/include/sys/select.h
rwchinfl.o: /usr/include/sys/_select.h /usr/include/sys/_types/_uuid_t.h
rwchinfl.o: /usr/include/gethostuuid.h
sdbm.o: c.h /usr/include/stdio.h /usr/include/sys/cdefs.h
sdbm.o: /usr/include/sys/_symbol_aliasing.h
sdbm.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
sdbm.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
sdbm.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
sdbm.o: /usr/include/i386/_types.h /usr/include/sys/_pthread/_pthread_types.h
sdbm.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
sdbm.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
sdbm.o: /usr/include/sys/_types/_int16_t.h /usr/include/sys/_types/_int32_t.h
sdbm.o: /usr/include/sys/_types/_int64_t.h
sdbm.o: /usr/include/sys/_types/_u_int8_t.h
sdbm.o: /usr/include/sys/_types/_u_int16_t.h
sdbm.o: /usr/include/sys/_types/_u_int32_t.h
sdbm.o: /usr/include/sys/_types/_u_int64_t.h
sdbm.o: /usr/include/sys/_types/_intptr_t.h
sdbm.o: /usr/include/sys/_types/_uintptr_t.h
sdbm.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
sdbm.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
sdbm.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
sdbm.o: /usr/include/secure/_common.h /usr/include/sys/types.h
sdbm.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
sdbm.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
sdbm.o: /usr/include/libkern/_OSByteOrder.h
sdbm.o: /usr/include/libkern/i386/_OSByteOrder.h
sdbm.o: /usr/include/sys/_types/_u_char.h /usr/include/sys/_types/_u_short.h
sdbm.o: /usr/include/sys/_types/_u_int.h /usr/include/sys/_types/_caddr_t.h
sdbm.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_blkcnt_t.h
sdbm.o: /usr/include/sys/_types/_blksize_t.h /usr/include/sys/_types/_gid_t.h
sdbm.o: /usr/include/sys/_types/_in_addr_t.h
sdbm.o: /usr/include/sys/_types/_in_port_t.h /usr/include/sys/_types/_ino_t.h
sdbm.o: /usr/include/sys/_types/_ino64_t.h /usr/include/sys/_types/_key_t.h
sdbm.o: /usr/include/sys/_types/_mode_t.h /usr/include/sys/_types/_nlink_t.h
sdbm.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
sdbm.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/_types/_clock_t.h
sdbm.o: /usr/include/sys/_types/_time_t.h
sdbm.o: /usr/include/sys/_types/_useconds_t.h
sdbm.o: /usr/include/sys/_types/_suseconds_t.h
sdbm.o: /usr/include/sys/_types/_rsize_t.h /usr/include/sys/_types/_errno_t.h
sdbm.o: /usr/include/sys/_types/_fd_def.h
sdbm.o: /usr/include/sys/_types/_fd_setsize.h
sdbm.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
sdbm.o: /usr/include/sys/_types/_fd_zero.h
sdbm.o: /usr/include/sys/_types/_fd_isset.h
sdbm.o: /usr/include/sys/_types/_fd_copy.h
sdbm.o: /usr/include/sys/_pthread/_pthread_attr_t.h
sdbm.o: /usr/include/sys/_pthread/_pthread_cond_t.h
sdbm.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
sdbm.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
sdbm.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
sdbm.o: /usr/include/sys/_pthread/_pthread_once_t.h
sdbm.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
sdbm.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
sdbm.o: /usr/include/sys/_pthread/_pthread_t.h
sdbm.o: /usr/include/sys/_pthread/_pthread_key_t.h
sdbm.o: /usr/include/sys/_types/_fsblkcnt_t.h
sdbm.o: /usr/include/sys/_types/_fsfilcnt_t.h /usr/include/stdlib.h
sdbm.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
sdbm.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
sdbm.o: /usr/include/machine/_mcontext.h /usr/include/i386/_mcontext.h
sdbm.o: /usr/include/mach/machine/_structs.h
sdbm.o: /usr/include/mach/i386/_structs.h
sdbm.o: /usr/include/sys/_types/_sigaltstack.h
sdbm.o: /usr/include/sys/_types/_ucontext.h
sdbm.o: /usr/include/sys/_types/_sigset_t.h /usr/include/sys/resource.h
sdbm.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
sdbm.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
sdbm.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
sdbm.o: /usr/include/_types/_uintmax_t.h /usr/include/sys/_types/_timeval.h
sdbm.o: /usr/include/alloca.h /usr/include/sys/_types/_ct_rune_t.h
sdbm.o: /usr/include/sys/_types/_rune_t.h /usr/include/sys/_types/_wchar_t.h
sdbm.o: /usr/include/unistd.h /usr/include/sys/unistd.h
sdbm.o: /usr/include/sys/_types/_posix_vdisable.h
sdbm.o: /usr/include/sys/_types/_seek_set.h /usr/include/sys/select.h
sdbm.o: /usr/include/sys/_types/_timespec.h /usr/include/sys/_select.h
sdbm.o: /usr/include/sys/_types/_uuid_t.h /usr/include/gethostuuid.h
sdbm.o: /usr/include/sys/stat.h /usr/include/sys/_types/_s_ifmt.h
sdbm.o: /usr/include/sys/_types/_filesec_t.h /usr/include/fcntl.h
sdbm.o: /usr/include/sys/fcntl.h /usr/include/sys/_types/_o_sync.h
sdbm.o: /usr/include/sys/_types/_o_dsync.h /usr/include/memory.h
sdbm.o: /usr/include/string.h /usr/include/strings.h
sdbm.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
sdbm.o: /usr/include/errno.h /usr/include/sys/errno.h /usr/include/stddef.h
sdbm.o: /usr/include/sys/_types/_offsetof.h
sdbm.o: /usr/include/sys/_types/_ptrdiff_t.h
sdbm.o: /usr/include/sys/_types/_wint_t.h
mangle.o: ../../.././include/ncarg/c.h /usr/include/stdio.h
mangle.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
mangle.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
mangle.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
mangle.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
mangle.o: /usr/include/i386/_types.h
mangle.o: /usr/include/sys/_pthread/_pthread_types.h
mangle.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
mangle.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
mangle.o: /usr/include/sys/_types/_int16_t.h
mangle.o: /usr/include/sys/_types/_int32_t.h
mangle.o: /usr/include/sys/_types/_int64_t.h
mangle.o: /usr/include/sys/_types/_u_int8_t.h
mangle.o: /usr/include/sys/_types/_u_int16_t.h
mangle.o: /usr/include/sys/_types/_u_int32_t.h
mangle.o: /usr/include/sys/_types/_u_int64_t.h
mangle.o: /usr/include/sys/_types/_intptr_t.h
mangle.o: /usr/include/sys/_types/_uintptr_t.h
mangle.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
mangle.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
mangle.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
mangle.o: /usr/include/secure/_common.h /usr/include/sys/types.h
mangle.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
mangle.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
mangle.o: /usr/include/libkern/_OSByteOrder.h
mangle.o: /usr/include/libkern/i386/_OSByteOrder.h
mangle.o: /usr/include/sys/_types/_u_char.h
mangle.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
mangle.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
mangle.o: /usr/include/sys/_types/_blkcnt_t.h
mangle.o: /usr/include/sys/_types/_blksize_t.h
mangle.o: /usr/include/sys/_types/_gid_t.h
mangle.o: /usr/include/sys/_types/_in_addr_t.h
mangle.o: /usr/include/sys/_types/_in_port_t.h
mangle.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
mangle.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
mangle.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
mangle.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
mangle.o: /usr/include/sys/_types/_clock_t.h
mangle.o: /usr/include/sys/_types/_time_t.h
mangle.o: /usr/include/sys/_types/_useconds_t.h
mangle.o: /usr/include/sys/_types/_suseconds_t.h
mangle.o: /usr/include/sys/_types/_rsize_t.h
mangle.o: /usr/include/sys/_types/_errno_t.h
mangle.o: /usr/include/sys/_types/_fd_def.h
mangle.o: /usr/include/sys/_types/_fd_setsize.h
mangle.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
mangle.o: /usr/include/sys/_types/_fd_zero.h
mangle.o: /usr/include/sys/_types/_fd_isset.h
mangle.o: /usr/include/sys/_types/_fd_copy.h
mangle.o: /usr/include/sys/_pthread/_pthread_attr_t.h
mangle.o: /usr/include/sys/_pthread/_pthread_cond_t.h
mangle.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
mangle.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
mangle.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
mangle.o: /usr/include/sys/_pthread/_pthread_once_t.h
mangle.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
mangle.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
mangle.o: /usr/include/sys/_pthread/_pthread_t.h
mangle.o: /usr/include/sys/_pthread/_pthread_key_t.h
mangle.o: /usr/include/sys/_types/_fsblkcnt_t.h
mangle.o: /usr/include/sys/_types/_fsfilcnt_t.h
