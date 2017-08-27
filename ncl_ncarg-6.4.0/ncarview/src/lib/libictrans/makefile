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
CURRENT_DIR		= ./ncarview/src/lib/libictrans

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
#	$Id: yMakefile,v 1.24 2003-12-09 17:04:34 grubin Exp $
#

MYNAME1		= libictrans.o

#
# name of the spooler translator
#
SPOOL_TRANS	= ctrans

EXTRA_CDEFINES	=	-DSPOOL_TRANS='"$(SPOOL_TRANS)"' -DNGTMPDIR='"$(NGTMPDIR)"' 			-DBINDIR='"$(BINDIR)"' -DLIBDIR='"$(LIBDIR)"' 			-DNCARGDIR='"$(NCARGDIR)"'

EXTENSION_OBJS	= ictrans_mem.o ictrans_wks.o fort_ictrans.o

EXTENSION_SRCS	= ictrans_mem.c ictrans_wks.c

GENERIC_OBJS	= commands.o cmd_tab.o get_cmd.o  		glob.o spooler.o ictrans.o lex_stuff.o input.o

LEX_OBJS = lex.yy_linux.o

GENERIC_SRCS	= commands.c cmd_tab.c get_cmd.c  		glob.c spooler.c ictrans.c input.c

OBJS	= $(GENERIC_OBJS) $(LEX_OBJS) $(EXTENSION_OBJS)
SRCS	= $(GENERIC_SRCS) $(EXTENSION_SRCS)

all::

all-local:: $(MYNAME1)

$(MYNAME1): $(OBJS)
	$(LD_REL) -r -o $(MYNAME1) $(OBJS)
all::

all-local:: $(MYNAME1)

install-local:: $(MYNAME1)
	@if (test ! -d $(ROBJPATH)); then ($(MKDIRHIER) $(ROBJPATH)); fi
	@for name in $(MYNAME1); do \
	if (test -f $$name.$(EXE_SUFFIX)) then \
	(echo "Installing $$name.$(EXE_SUFFIX)";\
	$(INSTALL) -m $(INSTALL_LIB) $$name.$(EXE_SUFFIX) $(ROBJPATH));\
	else \
	(echo "Installing $$name";\
	$(INSTALL) -m $(INSTALL_LIB) $$name $(ROBJPATH)); fi \
	done

all::

clean-local::
	-@$(RM) $(MYNAME1) $(OBJS)

all::

depend-local:: $(SRCS)
	$(DEPEND) -s "# DO NOT DELETE" -- $(ALLDEFINES) -- $(SRCS)

all::

clean-local::
	-@$(RM) lex.yy.c lex.yy.c.sed sedscript

lint: $(SRCS)
	lint $(CFLAGS) 1> lint.out 2>&1

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

commands.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
commands.o: /usr/include/sys/_symbol_aliasing.h
commands.o: /usr/include/sys/_posix_availability.h
commands.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
commands.o: /usr/include/_types.h /usr/include/sys/_types.h
commands.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
commands.o: /usr/include/sys/_pthread/_pthread_types.h
commands.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
commands.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
commands.o: /usr/include/sys/_types/_int16_t.h
commands.o: /usr/include/sys/_types/_int32_t.h
commands.o: /usr/include/sys/_types/_int64_t.h
commands.o: /usr/include/sys/_types/_u_int8_t.h
commands.o: /usr/include/sys/_types/_u_int16_t.h
commands.o: /usr/include/sys/_types/_u_int32_t.h
commands.o: /usr/include/sys/_types/_u_int64_t.h
commands.o: /usr/include/sys/_types/_intptr_t.h
commands.o: /usr/include/sys/_types/_uintptr_t.h
commands.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
commands.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
commands.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
commands.o: /usr/include/secure/_common.h /usr/include/stdlib.h
commands.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
commands.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
commands.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
commands.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
commands.o: /usr/include/i386/_mcontext.h
commands.o: /usr/include/mach/machine/_structs.h
commands.o: /usr/include/mach/i386/_structs.h
commands.o: /usr/include/sys/_pthread/_pthread_attr_t.h
commands.o: /usr/include/sys/_types/_sigaltstack.h
commands.o: /usr/include/sys/_types/_ucontext.h
commands.o: /usr/include/sys/_types/_sigset_t.h
commands.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
commands.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
commands.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
commands.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
commands.o: /usr/include/_types/_uintmax_t.h
commands.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
commands.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
commands.o: /usr/include/libkern/_OSByteOrder.h
commands.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
commands.o: /usr/include/sys/_types/_ct_rune_t.h
commands.o: /usr/include/sys/_types/_rune_t.h
commands.o: /usr/include/sys/_types/_wchar_t.h
commands.o: /usr/include/sys/_types/_dev_t.h
commands.o: /usr/include/sys/_types/_mode_t.h /usr/include/string.h
commands.o: /usr/include/sys/_types/_rsize_t.h
commands.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
commands.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
commands.o: /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
commands.o: /usr/include/sys/_types/_u_short.h
commands.o: /usr/include/sys/_types/_u_int.h
commands.o: /usr/include/sys/_types/_caddr_t.h
commands.o: /usr/include/sys/_types/_blkcnt_t.h
commands.o: /usr/include/sys/_types/_blksize_t.h
commands.o: /usr/include/sys/_types/_gid_t.h
commands.o: /usr/include/sys/_types/_in_addr_t.h
commands.o: /usr/include/sys/_types/_in_port_t.h
commands.o: /usr/include/sys/_types/_ino_t.h
commands.o: /usr/include/sys/_types/_ino64_t.h
commands.o: /usr/include/sys/_types/_key_t.h
commands.o: /usr/include/sys/_types/_nlink_t.h
commands.o: /usr/include/sys/_types/_clock_t.h
commands.o: /usr/include/sys/_types/_time_t.h
commands.o: /usr/include/sys/_types/_useconds_t.h
commands.o: /usr/include/sys/_types/_suseconds_t.h
commands.o: /usr/include/sys/_types/_fd_def.h
commands.o: /usr/include/sys/_types/_fd_setsize.h
commands.o: /usr/include/sys/_types/_fd_set.h
commands.o: /usr/include/sys/_types/_fd_clr.h
commands.o: /usr/include/sys/_types/_fd_zero.h
commands.o: /usr/include/sys/_types/_fd_isset.h
commands.o: /usr/include/sys/_types/_fd_copy.h
commands.o: /usr/include/sys/_pthread/_pthread_cond_t.h
commands.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
commands.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
commands.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
commands.o: /usr/include/sys/_pthread/_pthread_once_t.h
commands.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
commands.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
commands.o: /usr/include/sys/_pthread/_pthread_t.h
commands.o: /usr/include/sys/_pthread/_pthread_key_t.h
commands.o: /usr/include/sys/_types/_fsblkcnt_t.h
commands.o: /usr/include/sys/_types/_fsfilcnt_t.h /usr/include/signal.h
commands.o: /usr/include/fcntl.h /usr/include/sys/fcntl.h
commands.o: /usr/include/sys/_types/_o_sync.h
commands.o: /usr/include/sys/_types/_o_dsync.h
commands.o: /usr/include/sys/_types/_seek_set.h
commands.o: /usr/include/sys/_types/_s_ifmt.h
commands.o: /usr/include/sys/_types/_timespec.h
commands.o: /usr/include/sys/_types/_filesec_t.h /usr/include/ctype.h
commands.o: /usr/include/runetype.h /usr/include/sys/_types/_wint_t.h
commands.o: /usr/include/sys/stat.h /usr/include/errno.h
commands.o: /usr/include/sys/errno.h ../../../.././include/ncarg/c.h
commands.o: ../../../.././include/ncarg/cgm_tools.h
commands.o: ../../../.././include/ncarg/cgmdef.h
commands.o: ../../../.././include/ncarg/ctrans.h ictrans.h glob.h
cmd_tab.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
cmd_tab.o: /usr/include/sys/_symbol_aliasing.h
cmd_tab.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
cmd_tab.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
cmd_tab.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
cmd_tab.o: /usr/include/i386/_types.h
cmd_tab.o: /usr/include/sys/_pthread/_pthread_types.h
cmd_tab.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
cmd_tab.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
cmd_tab.o: /usr/include/sys/_types/_int16_t.h
cmd_tab.o: /usr/include/sys/_types/_int32_t.h
cmd_tab.o: /usr/include/sys/_types/_int64_t.h
cmd_tab.o: /usr/include/sys/_types/_u_int8_t.h
cmd_tab.o: /usr/include/sys/_types/_u_int16_t.h
cmd_tab.o: /usr/include/sys/_types/_u_int32_t.h
cmd_tab.o: /usr/include/sys/_types/_u_int64_t.h
cmd_tab.o: /usr/include/sys/_types/_intptr_t.h
cmd_tab.o: /usr/include/sys/_types/_uintptr_t.h
cmd_tab.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
cmd_tab.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
cmd_tab.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
cmd_tab.o: /usr/include/secure/_common.h ictrans.h
cmd_tab.o: ../../../.././include/ncarg/ctrans.h
cmd_tab.o: ../../../.././include/ncarg/c.h /usr/include/sys/types.h
cmd_tab.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
cmd_tab.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
cmd_tab.o: /usr/include/libkern/_OSByteOrder.h
cmd_tab.o: /usr/include/libkern/i386/_OSByteOrder.h
cmd_tab.o: /usr/include/sys/_types/_u_char.h
cmd_tab.o: /usr/include/sys/_types/_u_short.h
cmd_tab.o: /usr/include/sys/_types/_u_int.h
cmd_tab.o: /usr/include/sys/_types/_caddr_t.h
cmd_tab.o: /usr/include/sys/_types/_dev_t.h
cmd_tab.o: /usr/include/sys/_types/_blkcnt_t.h
cmd_tab.o: /usr/include/sys/_types/_blksize_t.h
cmd_tab.o: /usr/include/sys/_types/_gid_t.h
cmd_tab.o: /usr/include/sys/_types/_in_addr_t.h
cmd_tab.o: /usr/include/sys/_types/_in_port_t.h
cmd_tab.o: /usr/include/sys/_types/_ino_t.h
cmd_tab.o: /usr/include/sys/_types/_ino64_t.h
cmd_tab.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
cmd_tab.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
cmd_tab.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
cmd_tab.o: /usr/include/sys/_types/_clock_t.h
cmd_tab.o: /usr/include/sys/_types/_time_t.h
cmd_tab.o: /usr/include/sys/_types/_useconds_t.h
cmd_tab.o: /usr/include/sys/_types/_suseconds_t.h
cmd_tab.o: /usr/include/sys/_types/_rsize_t.h
cmd_tab.o: /usr/include/sys/_types/_errno_t.h
cmd_tab.o: /usr/include/sys/_types/_fd_def.h
cmd_tab.o: /usr/include/sys/_types/_fd_setsize.h
cmd_tab.o: /usr/include/sys/_types/_fd_set.h
cmd_tab.o: /usr/include/sys/_types/_fd_clr.h
cmd_tab.o: /usr/include/sys/_types/_fd_zero.h
cmd_tab.o: /usr/include/sys/_types/_fd_isset.h
cmd_tab.o: /usr/include/sys/_types/_fd_copy.h
cmd_tab.o: /usr/include/sys/_pthread/_pthread_attr_t.h
cmd_tab.o: /usr/include/sys/_pthread/_pthread_cond_t.h
cmd_tab.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
cmd_tab.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
cmd_tab.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
cmd_tab.o: /usr/include/sys/_pthread/_pthread_once_t.h
cmd_tab.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
cmd_tab.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
cmd_tab.o: /usr/include/sys/_pthread/_pthread_t.h
cmd_tab.o: /usr/include/sys/_pthread/_pthread_key_t.h
cmd_tab.o: /usr/include/sys/_types/_fsblkcnt_t.h
cmd_tab.o: /usr/include/sys/_types/_fsfilcnt_t.h
cmd_tab.o: ../../../.././include/ncarg/cgm_tools.h
cmd_tab.o: ../../../.././include/ncarg/cgmdef.h
get_cmd.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
get_cmd.o: /usr/include/sys/_symbol_aliasing.h
get_cmd.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
get_cmd.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
get_cmd.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
get_cmd.o: /usr/include/i386/_types.h
get_cmd.o: /usr/include/sys/_pthread/_pthread_types.h
get_cmd.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
get_cmd.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
get_cmd.o: /usr/include/sys/_types/_int16_t.h
get_cmd.o: /usr/include/sys/_types/_int32_t.h
get_cmd.o: /usr/include/sys/_types/_int64_t.h
get_cmd.o: /usr/include/sys/_types/_u_int8_t.h
get_cmd.o: /usr/include/sys/_types/_u_int16_t.h
get_cmd.o: /usr/include/sys/_types/_u_int32_t.h
get_cmd.o: /usr/include/sys/_types/_u_int64_t.h
get_cmd.o: /usr/include/sys/_types/_intptr_t.h
get_cmd.o: /usr/include/sys/_types/_uintptr_t.h
get_cmd.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
get_cmd.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
get_cmd.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
get_cmd.o: /usr/include/secure/_common.h /usr/include/stdlib.h
get_cmd.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
get_cmd.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
get_cmd.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
get_cmd.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
get_cmd.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
get_cmd.o: /usr/include/mach/i386/_structs.h
get_cmd.o: /usr/include/sys/_pthread/_pthread_attr_t.h
get_cmd.o: /usr/include/sys/_types/_sigaltstack.h
get_cmd.o: /usr/include/sys/_types/_ucontext.h
get_cmd.o: /usr/include/sys/_types/_sigset_t.h
get_cmd.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
get_cmd.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
get_cmd.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
get_cmd.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
get_cmd.o: /usr/include/_types/_uintmax_t.h
get_cmd.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
get_cmd.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
get_cmd.o: /usr/include/libkern/_OSByteOrder.h
get_cmd.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
get_cmd.o: /usr/include/sys/_types/_ct_rune_t.h
get_cmd.o: /usr/include/sys/_types/_rune_t.h
get_cmd.o: /usr/include/sys/_types/_wchar_t.h
get_cmd.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_mode_t.h
get_cmd.o: /usr/include/string.h /usr/include/sys/_types/_rsize_t.h
get_cmd.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
get_cmd.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
get_cmd.o: /usr/include/ctype.h /usr/include/runetype.h
get_cmd.o: /usr/include/sys/_types/_wint_t.h ../../../.././include/ncarg/c.h
get_cmd.o: /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
get_cmd.o: /usr/include/sys/_types/_u_short.h
get_cmd.o: /usr/include/sys/_types/_u_int.h
get_cmd.o: /usr/include/sys/_types/_caddr_t.h
get_cmd.o: /usr/include/sys/_types/_blkcnt_t.h
get_cmd.o: /usr/include/sys/_types/_blksize_t.h
get_cmd.o: /usr/include/sys/_types/_gid_t.h
get_cmd.o: /usr/include/sys/_types/_in_addr_t.h
get_cmd.o: /usr/include/sys/_types/_in_port_t.h
get_cmd.o: /usr/include/sys/_types/_ino_t.h
get_cmd.o: /usr/include/sys/_types/_ino64_t.h
get_cmd.o: /usr/include/sys/_types/_key_t.h
get_cmd.o: /usr/include/sys/_types/_nlink_t.h
get_cmd.o: /usr/include/sys/_types/_clock_t.h
get_cmd.o: /usr/include/sys/_types/_time_t.h
get_cmd.o: /usr/include/sys/_types/_useconds_t.h
get_cmd.o: /usr/include/sys/_types/_suseconds_t.h
get_cmd.o: /usr/include/sys/_types/_fd_def.h
get_cmd.o: /usr/include/sys/_types/_fd_setsize.h
get_cmd.o: /usr/include/sys/_types/_fd_set.h
get_cmd.o: /usr/include/sys/_types/_fd_clr.h
get_cmd.o: /usr/include/sys/_types/_fd_zero.h
get_cmd.o: /usr/include/sys/_types/_fd_isset.h
get_cmd.o: /usr/include/sys/_types/_fd_copy.h
get_cmd.o: /usr/include/sys/_pthread/_pthread_cond_t.h
get_cmd.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
get_cmd.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
get_cmd.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
get_cmd.o: /usr/include/sys/_pthread/_pthread_once_t.h
get_cmd.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
get_cmd.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
get_cmd.o: /usr/include/sys/_pthread/_pthread_t.h
get_cmd.o: /usr/include/sys/_pthread/_pthread_key_t.h
get_cmd.o: /usr/include/sys/_types/_fsblkcnt_t.h
get_cmd.o: /usr/include/sys/_types/_fsfilcnt_t.h lex.h ictrans.h
get_cmd.o: ../../../.././include/ncarg/ctrans.h
get_cmd.o: ../../../.././include/ncarg/cgm_tools.h
get_cmd.o: ../../../.././include/ncarg/cgmdef.h get_cmd.h
glob.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
glob.o: /usr/include/sys/_symbol_aliasing.h
glob.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
glob.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
glob.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
glob.o: /usr/include/i386/_types.h /usr/include/sys/_pthread/_pthread_types.h
glob.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
glob.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
glob.o: /usr/include/sys/_types/_int16_t.h /usr/include/sys/_types/_int32_t.h
glob.o: /usr/include/sys/_types/_int64_t.h
glob.o: /usr/include/sys/_types/_u_int8_t.h
glob.o: /usr/include/sys/_types/_u_int16_t.h
glob.o: /usr/include/sys/_types/_u_int32_t.h
glob.o: /usr/include/sys/_types/_u_int64_t.h
glob.o: /usr/include/sys/_types/_intptr_t.h
glob.o: /usr/include/sys/_types/_uintptr_t.h
glob.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
glob.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
glob.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
glob.o: /usr/include/secure/_common.h /usr/include/stdlib.h
glob.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
glob.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
glob.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
glob.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
glob.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
glob.o: /usr/include/mach/i386/_structs.h
glob.o: /usr/include/sys/_pthread/_pthread_attr_t.h
glob.o: /usr/include/sys/_types/_sigaltstack.h
glob.o: /usr/include/sys/_types/_ucontext.h
glob.o: /usr/include/sys/_types/_sigset_t.h /usr/include/sys/_types/_uid_t.h
glob.o: /usr/include/sys/resource.h /usr/include/stdint.h
glob.o: /usr/include/_types/_uint8_t.h /usr/include/_types/_uint16_t.h
glob.o: /usr/include/_types/_uint32_t.h /usr/include/_types/_uint64_t.h
glob.o: /usr/include/_types/_intmax_t.h /usr/include/_types/_uintmax_t.h
glob.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
glob.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
glob.o: /usr/include/libkern/_OSByteOrder.h
glob.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
glob.o: /usr/include/sys/_types/_ct_rune_t.h
glob.o: /usr/include/sys/_types/_rune_t.h /usr/include/sys/_types/_wchar_t.h
glob.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_mode_t.h
glob.o: /usr/include/string.h /usr/include/sys/_types/_rsize_t.h
glob.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
glob.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
glob.o: /usr/include/ctype.h /usr/include/runetype.h
glob.o: /usr/include/sys/_types/_wint_t.h ../../../.././include/ncarg/c.h
glob.o: /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
glob.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
glob.o: /usr/include/sys/_types/_caddr_t.h
glob.o: /usr/include/sys/_types/_blkcnt_t.h
glob.o: /usr/include/sys/_types/_blksize_t.h /usr/include/sys/_types/_gid_t.h
glob.o: /usr/include/sys/_types/_in_addr_t.h
glob.o: /usr/include/sys/_types/_in_port_t.h /usr/include/sys/_types/_ino_t.h
glob.o: /usr/include/sys/_types/_ino64_t.h /usr/include/sys/_types/_key_t.h
glob.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_clock_t.h
glob.o: /usr/include/sys/_types/_time_t.h
glob.o: /usr/include/sys/_types/_useconds_t.h
glob.o: /usr/include/sys/_types/_suseconds_t.h
glob.o: /usr/include/sys/_types/_fd_def.h
glob.o: /usr/include/sys/_types/_fd_setsize.h
glob.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
glob.o: /usr/include/sys/_types/_fd_zero.h
glob.o: /usr/include/sys/_types/_fd_isset.h
glob.o: /usr/include/sys/_types/_fd_copy.h
glob.o: /usr/include/sys/_pthread/_pthread_cond_t.h
glob.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
glob.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
glob.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
glob.o: /usr/include/sys/_pthread/_pthread_once_t.h
glob.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
glob.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
glob.o: /usr/include/sys/_pthread/_pthread_t.h
glob.o: /usr/include/sys/_pthread/_pthread_key_t.h
glob.o: /usr/include/sys/_types/_fsblkcnt_t.h
glob.o: /usr/include/sys/_types/_fsfilcnt_t.h ictrans.h
glob.o: ../../../.././include/ncarg/ctrans.h
glob.o: ../../../.././include/ncarg/cgm_tools.h
glob.o: ../../../.././include/ncarg/cgmdef.h glob.h
spooler.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
spooler.o: /usr/include/sys/_symbol_aliasing.h
spooler.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
spooler.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
spooler.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
spooler.o: /usr/include/i386/_types.h
spooler.o: /usr/include/sys/_pthread/_pthread_types.h
spooler.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
spooler.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
spooler.o: /usr/include/sys/_types/_int16_t.h
spooler.o: /usr/include/sys/_types/_int32_t.h
spooler.o: /usr/include/sys/_types/_int64_t.h
spooler.o: /usr/include/sys/_types/_u_int8_t.h
spooler.o: /usr/include/sys/_types/_u_int16_t.h
spooler.o: /usr/include/sys/_types/_u_int32_t.h
spooler.o: /usr/include/sys/_types/_u_int64_t.h
spooler.o: /usr/include/sys/_types/_intptr_t.h
spooler.o: /usr/include/sys/_types/_uintptr_t.h
spooler.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
spooler.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
spooler.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
spooler.o: /usr/include/secure/_common.h /usr/include/stdlib.h
spooler.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
spooler.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
spooler.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
spooler.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
spooler.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
spooler.o: /usr/include/mach/i386/_structs.h
spooler.o: /usr/include/sys/_pthread/_pthread_attr_t.h
spooler.o: /usr/include/sys/_types/_sigaltstack.h
spooler.o: /usr/include/sys/_types/_ucontext.h
spooler.o: /usr/include/sys/_types/_sigset_t.h
spooler.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
spooler.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
spooler.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
spooler.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
spooler.o: /usr/include/_types/_uintmax_t.h
spooler.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
spooler.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
spooler.o: /usr/include/libkern/_OSByteOrder.h
spooler.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
spooler.o: /usr/include/sys/_types/_ct_rune_t.h
spooler.o: /usr/include/sys/_types/_rune_t.h
spooler.o: /usr/include/sys/_types/_wchar_t.h
spooler.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_mode_t.h
spooler.o: /usr/include/signal.h /usr/include/sys/_pthread/_pthread_t.h
spooler.o: /usr/include/string.h /usr/include/sys/_types/_rsize_t.h
spooler.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
spooler.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
spooler.o: /usr/include/ctype.h /usr/include/runetype.h
spooler.o: /usr/include/sys/_types/_wint_t.h /usr/include/fcntl.h
spooler.o: /usr/include/sys/fcntl.h /usr/include/sys/_types/_o_sync.h
spooler.o: /usr/include/sys/_types/_o_dsync.h
spooler.o: /usr/include/sys/_types/_seek_set.h
spooler.o: /usr/include/sys/_types/_s_ifmt.h
spooler.o: /usr/include/sys/_types/_timespec.h
spooler.o: /usr/include/sys/_types/_filesec_t.h
spooler.o: ../../../.././include/ncarg/c.h /usr/include/sys/types.h
spooler.o: /usr/include/sys/_types/_u_char.h
spooler.o: /usr/include/sys/_types/_u_short.h
spooler.o: /usr/include/sys/_types/_u_int.h
spooler.o: /usr/include/sys/_types/_caddr_t.h
spooler.o: /usr/include/sys/_types/_blkcnt_t.h
spooler.o: /usr/include/sys/_types/_blksize_t.h
spooler.o: /usr/include/sys/_types/_gid_t.h
spooler.o: /usr/include/sys/_types/_in_addr_t.h
spooler.o: /usr/include/sys/_types/_in_port_t.h
spooler.o: /usr/include/sys/_types/_ino_t.h
spooler.o: /usr/include/sys/_types/_ino64_t.h
spooler.o: /usr/include/sys/_types/_key_t.h
spooler.o: /usr/include/sys/_types/_nlink_t.h
spooler.o: /usr/include/sys/_types/_clock_t.h
spooler.o: /usr/include/sys/_types/_time_t.h
spooler.o: /usr/include/sys/_types/_useconds_t.h
spooler.o: /usr/include/sys/_types/_suseconds_t.h
spooler.o: /usr/include/sys/_types/_fd_def.h
spooler.o: /usr/include/sys/_types/_fd_setsize.h
spooler.o: /usr/include/sys/_types/_fd_set.h
spooler.o: /usr/include/sys/_types/_fd_clr.h
spooler.o: /usr/include/sys/_types/_fd_zero.h
spooler.o: /usr/include/sys/_types/_fd_isset.h
spooler.o: /usr/include/sys/_types/_fd_copy.h
spooler.o: /usr/include/sys/_pthread/_pthread_cond_t.h
spooler.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
spooler.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
spooler.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
spooler.o: /usr/include/sys/_pthread/_pthread_once_t.h
spooler.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
spooler.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
spooler.o: /usr/include/sys/_pthread/_pthread_key_t.h
spooler.o: /usr/include/sys/_types/_fsblkcnt_t.h
spooler.o: /usr/include/sys/_types/_fsfilcnt_t.h ictrans.h
spooler.o: ../../../.././include/ncarg/ctrans.h
spooler.o: ../../../.././include/ncarg/cgm_tools.h
spooler.o: ../../../.././include/ncarg/cgmdef.h spooler.h
ictrans.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
ictrans.o: /usr/include/sys/_symbol_aliasing.h
ictrans.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
ictrans.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
ictrans.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
ictrans.o: /usr/include/i386/_types.h
ictrans.o: /usr/include/sys/_pthread/_pthread_types.h
ictrans.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
ictrans.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
ictrans.o: /usr/include/sys/_types/_int16_t.h
ictrans.o: /usr/include/sys/_types/_int32_t.h
ictrans.o: /usr/include/sys/_types/_int64_t.h
ictrans.o: /usr/include/sys/_types/_u_int8_t.h
ictrans.o: /usr/include/sys/_types/_u_int16_t.h
ictrans.o: /usr/include/sys/_types/_u_int32_t.h
ictrans.o: /usr/include/sys/_types/_u_int64_t.h
ictrans.o: /usr/include/sys/_types/_intptr_t.h
ictrans.o: /usr/include/sys/_types/_uintptr_t.h
ictrans.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
ictrans.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
ictrans.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
ictrans.o: /usr/include/secure/_common.h /usr/include/stdlib.h
ictrans.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
ictrans.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
ictrans.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
ictrans.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
ictrans.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
ictrans.o: /usr/include/mach/i386/_structs.h
ictrans.o: /usr/include/sys/_pthread/_pthread_attr_t.h
ictrans.o: /usr/include/sys/_types/_sigaltstack.h
ictrans.o: /usr/include/sys/_types/_ucontext.h
ictrans.o: /usr/include/sys/_types/_sigset_t.h
ictrans.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
ictrans.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
ictrans.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
ictrans.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
ictrans.o: /usr/include/_types/_uintmax_t.h
ictrans.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
ictrans.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
ictrans.o: /usr/include/libkern/_OSByteOrder.h
ictrans.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
ictrans.o: /usr/include/sys/_types/_ct_rune_t.h
ictrans.o: /usr/include/sys/_types/_rune_t.h
ictrans.o: /usr/include/sys/_types/_wchar_t.h
ictrans.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_mode_t.h
ictrans.o: /usr/include/string.h /usr/include/sys/_types/_rsize_t.h
ictrans.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
ictrans.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
ictrans.o: /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
ictrans.o: /usr/include/sys/_types/_u_short.h
ictrans.o: /usr/include/sys/_types/_u_int.h
ictrans.o: /usr/include/sys/_types/_caddr_t.h
ictrans.o: /usr/include/sys/_types/_blkcnt_t.h
ictrans.o: /usr/include/sys/_types/_blksize_t.h
ictrans.o: /usr/include/sys/_types/_gid_t.h
ictrans.o: /usr/include/sys/_types/_in_addr_t.h
ictrans.o: /usr/include/sys/_types/_in_port_t.h
ictrans.o: /usr/include/sys/_types/_ino_t.h
ictrans.o: /usr/include/sys/_types/_ino64_t.h
ictrans.o: /usr/include/sys/_types/_key_t.h
ictrans.o: /usr/include/sys/_types/_nlink_t.h
ictrans.o: /usr/include/sys/_types/_clock_t.h
ictrans.o: /usr/include/sys/_types/_time_t.h
ictrans.o: /usr/include/sys/_types/_useconds_t.h
ictrans.o: /usr/include/sys/_types/_suseconds_t.h
ictrans.o: /usr/include/sys/_types/_fd_def.h
ictrans.o: /usr/include/sys/_types/_fd_setsize.h
ictrans.o: /usr/include/sys/_types/_fd_set.h
ictrans.o: /usr/include/sys/_types/_fd_clr.h
ictrans.o: /usr/include/sys/_types/_fd_zero.h
ictrans.o: /usr/include/sys/_types/_fd_isset.h
ictrans.o: /usr/include/sys/_types/_fd_copy.h
ictrans.o: /usr/include/sys/_pthread/_pthread_cond_t.h
ictrans.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
ictrans.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
ictrans.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
ictrans.o: /usr/include/sys/_pthread/_pthread_once_t.h
ictrans.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
ictrans.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
ictrans.o: /usr/include/sys/_pthread/_pthread_t.h
ictrans.o: /usr/include/sys/_pthread/_pthread_key_t.h
ictrans.o: /usr/include/sys/_types/_fsblkcnt_t.h
ictrans.o: /usr/include/sys/_types/_fsfilcnt_t.h /usr/include/sys/time.h
ictrans.o: /usr/include/sys/_types/_timespec.h
ictrans.o: /usr/include/sys/_types/_timeval64.h /usr/include/time.h
ictrans.o: /usr/include/sys/_select.h ../../../.././include/ncarg/cgm_tools.h
ictrans.o: ../../../.././include/ncarg/c.h
ictrans.o: ../../../.././include/ncarg/cgmdef.h ictrans.h
ictrans.o: ../../../.././include/ncarg/ctrans.h lex.h get_cmd.h
input.o: /usr/include/string.h /usr/include/_types.h
input.o: /usr/include/sys/_types.h /usr/include/sys/cdefs.h
input.o: /usr/include/sys/_symbol_aliasing.h
input.o: /usr/include/sys/_posix_availability.h /usr/include/machine/_types.h
input.o: /usr/include/i386/_types.h
input.o: /usr/include/sys/_pthread/_pthread_types.h
input.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
input.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
input.o: /usr/include/sys/_types/_rsize_t.h /usr/include/machine/types.h
input.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
input.o: /usr/include/sys/_types/_int16_t.h
input.o: /usr/include/sys/_types/_int32_t.h
input.o: /usr/include/sys/_types/_int64_t.h
input.o: /usr/include/sys/_types/_u_int8_t.h
input.o: /usr/include/sys/_types/_u_int16_t.h
input.o: /usr/include/sys/_types/_u_int32_t.h
input.o: /usr/include/sys/_types/_u_int64_t.h
input.o: /usr/include/sys/_types/_intptr_t.h
input.o: /usr/include/sys/_types/_uintptr_t.h
input.o: /usr/include/sys/_types/_errno_t.h
input.o: /usr/include/sys/_types/_ssize_t.h /usr/include/strings.h
input.o: /usr/include/secure/_strings.h /usr/include/secure/_common.h
input.o: /usr/include/secure/_string.h
ictrans_mem.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
ictrans_mem.o: /usr/include/sys/_symbol_aliasing.h
ictrans_mem.o: /usr/include/sys/_posix_availability.h
ictrans_mem.o: /usr/include/Availability.h
ictrans_mem.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
ictrans_mem.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
ictrans_mem.o: /usr/include/i386/_types.h
ictrans_mem.o: /usr/include/sys/_pthread/_pthread_types.h
ictrans_mem.o: /usr/include/sys/_types/_va_list.h
ictrans_mem.o: /usr/include/machine/types.h /usr/include/i386/types.h
ictrans_mem.o: /usr/include/sys/_types/_int8_t.h
ictrans_mem.o: /usr/include/sys/_types/_int16_t.h
ictrans_mem.o: /usr/include/sys/_types/_int32_t.h
ictrans_mem.o: /usr/include/sys/_types/_int64_t.h
ictrans_mem.o: /usr/include/sys/_types/_u_int8_t.h
ictrans_mem.o: /usr/include/sys/_types/_u_int16_t.h
ictrans_mem.o: /usr/include/sys/_types/_u_int32_t.h
ictrans_mem.o: /usr/include/sys/_types/_u_int64_t.h
ictrans_mem.o: /usr/include/sys/_types/_intptr_t.h
ictrans_mem.o: /usr/include/sys/_types/_uintptr_t.h
ictrans_mem.o: /usr/include/sys/_types/_size_t.h
ictrans_mem.o: /usr/include/sys/_types/_null.h /usr/include/sys/stdio.h
ictrans_mem.o: /usr/include/sys/_types/_off_t.h
ictrans_mem.o: /usr/include/sys/_types/_ssize_t.h
ictrans_mem.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
ictrans_mem.o: /usr/include/stdlib.h /usr/include/sys/wait.h
ictrans_mem.o: /usr/include/sys/_types/_pid_t.h
ictrans_mem.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
ictrans_mem.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
ictrans_mem.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
ictrans_mem.o: /usr/include/i386/_mcontext.h
ictrans_mem.o: /usr/include/mach/machine/_structs.h
ictrans_mem.o: /usr/include/mach/i386/_structs.h
ictrans_mem.o: /usr/include/sys/_pthread/_pthread_attr_t.h
ictrans_mem.o: /usr/include/sys/_types/_sigaltstack.h
ictrans_mem.o: /usr/include/sys/_types/_ucontext.h
ictrans_mem.o: /usr/include/sys/_types/_sigset_t.h
ictrans_mem.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
ictrans_mem.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
ictrans_mem.o: /usr/include/_types/_uint16_t.h
ictrans_mem.o: /usr/include/_types/_uint32_t.h
ictrans_mem.o: /usr/include/_types/_uint64_t.h
ictrans_mem.o: /usr/include/_types/_intmax_t.h
ictrans_mem.o: /usr/include/_types/_uintmax_t.h
ictrans_mem.o: /usr/include/sys/_types/_timeval.h
ictrans_mem.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
ictrans_mem.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
ictrans_mem.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
ictrans_mem.o: /usr/include/sys/_types/_ct_rune_t.h
ictrans_mem.o: /usr/include/sys/_types/_rune_t.h
ictrans_mem.o: /usr/include/sys/_types/_wchar_t.h
ictrans_mem.o: /usr/include/sys/_types/_dev_t.h
ictrans_mem.o: /usr/include/sys/_types/_mode_t.h /usr/include/ctype.h
ictrans_mem.o: /usr/include/runetype.h /usr/include/sys/_types/_wint_t.h
ictrans_mem.o: /usr/include/string.h /usr/include/sys/_types/_rsize_t.h
ictrans_mem.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
ictrans_mem.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
ictrans_mem.o: /usr/include/fcntl.h /usr/include/sys/fcntl.h
ictrans_mem.o: /usr/include/sys/_types/_o_sync.h
ictrans_mem.o: /usr/include/sys/_types/_o_dsync.h
ictrans_mem.o: /usr/include/sys/_types/_seek_set.h
ictrans_mem.o: /usr/include/sys/_types/_s_ifmt.h
ictrans_mem.o: /usr/include/sys/_types/_timespec.h
ictrans_mem.o: /usr/include/sys/_types/_filesec_t.h
ictrans_mem.o: ../../../.././include/ncarg/c.h /usr/include/sys/types.h
ictrans_mem.o: /usr/include/sys/_types/_u_char.h
ictrans_mem.o: /usr/include/sys/_types/_u_short.h
ictrans_mem.o: /usr/include/sys/_types/_u_int.h
ictrans_mem.o: /usr/include/sys/_types/_caddr_t.h
ictrans_mem.o: /usr/include/sys/_types/_blkcnt_t.h
ictrans_mem.o: /usr/include/sys/_types/_blksize_t.h
ictrans_mem.o: /usr/include/sys/_types/_gid_t.h
ictrans_mem.o: /usr/include/sys/_types/_in_addr_t.h
ictrans_mem.o: /usr/include/sys/_types/_in_port_t.h
ictrans_mem.o: /usr/include/sys/_types/_ino_t.h
ictrans_mem.o: /usr/include/sys/_types/_ino64_t.h
ictrans_mem.o: /usr/include/sys/_types/_key_t.h
ictrans_mem.o: /usr/include/sys/_types/_nlink_t.h
ictrans_mem.o: /usr/include/sys/_types/_clock_t.h
ictrans_mem.o: /usr/include/sys/_types/_time_t.h
ictrans_mem.o: /usr/include/sys/_types/_useconds_t.h
ictrans_mem.o: /usr/include/sys/_types/_suseconds_t.h
ictrans_mem.o: /usr/include/sys/_types/_fd_def.h
ictrans_mem.o: /usr/include/sys/_types/_fd_setsize.h
ictrans_mem.o: /usr/include/sys/_types/_fd_set.h
ictrans_mem.o: /usr/include/sys/_types/_fd_clr.h
ictrans_mem.o: /usr/include/sys/_types/_fd_zero.h
ictrans_mem.o: /usr/include/sys/_types/_fd_isset.h
ictrans_mem.o: /usr/include/sys/_types/_fd_copy.h
ictrans_mem.o: /usr/include/sys/_pthread/_pthread_cond_t.h
ictrans_mem.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
ictrans_mem.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
ictrans_mem.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
ictrans_mem.o: /usr/include/sys/_pthread/_pthread_once_t.h
ictrans_mem.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
ictrans_mem.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
ictrans_mem.o: /usr/include/sys/_pthread/_pthread_t.h
ictrans_mem.o: /usr/include/sys/_pthread/_pthread_key_t.h
ictrans_mem.o: /usr/include/sys/_types/_fsblkcnt_t.h
ictrans_mem.o: /usr/include/sys/_types/_fsfilcnt_t.h
ictrans_mem.o: ../../../.././include/ncarg/cgm_tools.h
ictrans_mem.o: ../../../.././include/ncarg/cgmdef.h ictrans_mem.h
ictrans_wks.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
ictrans_wks.o: /usr/include/sys/_symbol_aliasing.h
ictrans_wks.o: /usr/include/sys/_posix_availability.h
ictrans_wks.o: /usr/include/Availability.h
ictrans_wks.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
ictrans_wks.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
ictrans_wks.o: /usr/include/i386/_types.h
ictrans_wks.o: /usr/include/sys/_pthread/_pthread_types.h
ictrans_wks.o: /usr/include/sys/_types/_va_list.h
ictrans_wks.o: /usr/include/machine/types.h /usr/include/i386/types.h
ictrans_wks.o: /usr/include/sys/_types/_int8_t.h
ictrans_wks.o: /usr/include/sys/_types/_int16_t.h
ictrans_wks.o: /usr/include/sys/_types/_int32_t.h
ictrans_wks.o: /usr/include/sys/_types/_int64_t.h
ictrans_wks.o: /usr/include/sys/_types/_u_int8_t.h
ictrans_wks.o: /usr/include/sys/_types/_u_int16_t.h
ictrans_wks.o: /usr/include/sys/_types/_u_int32_t.h
ictrans_wks.o: /usr/include/sys/_types/_u_int64_t.h
ictrans_wks.o: /usr/include/sys/_types/_intptr_t.h
ictrans_wks.o: /usr/include/sys/_types/_uintptr_t.h
ictrans_wks.o: /usr/include/sys/_types/_size_t.h
ictrans_wks.o: /usr/include/sys/_types/_null.h /usr/include/sys/stdio.h
ictrans_wks.o: /usr/include/sys/_types/_off_t.h
ictrans_wks.o: /usr/include/sys/_types/_ssize_t.h
ictrans_wks.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
ictrans_wks.o: /usr/include/fcntl.h /usr/include/sys/fcntl.h
ictrans_wks.o: /usr/include/sys/_types/_mode_t.h
ictrans_wks.o: /usr/include/sys/_types/_pid_t.h
ictrans_wks.o: /usr/include/sys/_types/_o_sync.h
ictrans_wks.o: /usr/include/sys/_types/_o_dsync.h
ictrans_wks.o: /usr/include/sys/_types/_seek_set.h
ictrans_wks.o: /usr/include/sys/_types/_s_ifmt.h
ictrans_wks.o: /usr/include/sys/_types/_timespec.h
ictrans_wks.o: /usr/include/sys/_types/_filesec_t.h /usr/include/sys/types.h
ictrans_wks.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
ictrans_wks.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
ictrans_wks.o: /usr/include/libkern/_OSByteOrder.h
ictrans_wks.o: /usr/include/libkern/i386/_OSByteOrder.h
ictrans_wks.o: /usr/include/sys/_types/_u_char.h
ictrans_wks.o: /usr/include/sys/_types/_u_short.h
ictrans_wks.o: /usr/include/sys/_types/_u_int.h
ictrans_wks.o: /usr/include/sys/_types/_caddr_t.h
ictrans_wks.o: /usr/include/sys/_types/_dev_t.h
ictrans_wks.o: /usr/include/sys/_types/_blkcnt_t.h
ictrans_wks.o: /usr/include/sys/_types/_blksize_t.h
ictrans_wks.o: /usr/include/sys/_types/_gid_t.h
ictrans_wks.o: /usr/include/sys/_types/_in_addr_t.h
ictrans_wks.o: /usr/include/sys/_types/_in_port_t.h
ictrans_wks.o: /usr/include/sys/_types/_ino_t.h
ictrans_wks.o: /usr/include/sys/_types/_ino64_t.h
ictrans_wks.o: /usr/include/sys/_types/_key_t.h
ictrans_wks.o: /usr/include/sys/_types/_nlink_t.h
ictrans_wks.o: /usr/include/sys/_types/_id_t.h
ictrans_wks.o: /usr/include/sys/_types/_uid_t.h
ictrans_wks.o: /usr/include/sys/_types/_clock_t.h
ictrans_wks.o: /usr/include/sys/_types/_time_t.h
ictrans_wks.o: /usr/include/sys/_types/_useconds_t.h
ictrans_wks.o: /usr/include/sys/_types/_suseconds_t.h
ictrans_wks.o: /usr/include/sys/_types/_rsize_t.h
ictrans_wks.o: /usr/include/sys/_types/_errno_t.h
ictrans_wks.o: /usr/include/sys/_types/_fd_def.h
ictrans_wks.o: /usr/include/sys/_types/_fd_setsize.h
ictrans_wks.o: /usr/include/sys/_types/_fd_set.h
ictrans_wks.o: /usr/include/sys/_types/_fd_clr.h
ictrans_wks.o: /usr/include/sys/_types/_fd_zero.h
ictrans_wks.o: /usr/include/sys/_types/_fd_isset.h
ictrans_wks.o: /usr/include/sys/_types/_fd_copy.h
ictrans_wks.o: /usr/include/sys/_pthread/_pthread_attr_t.h
ictrans_wks.o: /usr/include/sys/_pthread/_pthread_cond_t.h
ictrans_wks.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
ictrans_wks.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
ictrans_wks.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
ictrans_wks.o: /usr/include/sys/_pthread/_pthread_once_t.h
ictrans_wks.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
ictrans_wks.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
ictrans_wks.o: /usr/include/sys/_pthread/_pthread_t.h
ictrans_wks.o: /usr/include/sys/_pthread/_pthread_key_t.h
ictrans_wks.o: /usr/include/sys/_types/_fsblkcnt_t.h
ictrans_wks.o: /usr/include/sys/_types/_fsfilcnt_t.h /usr/include/sys/file.h
ictrans_wks.o: /usr/include/sys/unistd.h
ictrans_wks.o: /usr/include/sys/_types/_posix_vdisable.h
ictrans_wks.o: /usr/include/_types/_uint64_t.h
ictrans_wks.o: /usr/include/_types/_uint32_t.h /usr/include/sys/queue.h
ictrans_wks.o: /usr/include/unistd.h /usr/include/sys/select.h
ictrans_wks.o: /usr/include/sys/_types/_timeval.h
ictrans_wks.o: /usr/include/sys/_types/_sigset_t.h /usr/include/sys/_select.h
ictrans_wks.o: /usr/include/sys/_types/_uuid_t.h /usr/include/gethostuuid.h
ictrans_wks.o: ../../../.././include/ncarg/c.h
ictrans_wks.o: ../../../.././include/ncarg/cgm_tools.h
ictrans_wks.o: ../../../.././include/ncarg/cgmdef.h ictrans_wks.h
