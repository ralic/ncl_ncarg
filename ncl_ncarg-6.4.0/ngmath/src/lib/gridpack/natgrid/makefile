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

TOP			= ../../../../../.
CURRENT_DIR		= ./ngmath/src/lib/gridpack/natgrid

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
#       $Id: yMakefile,v 1.6 2005-07-29 23:19:56 fred Exp $
#

MYNAME  = natgrid

SOURCES = natgrid.c  natgrids.c nncrunchd.c nnerror.c nnuserd.c    	  natgridd.c nncrunch.c nncrunchs.c nnuser.c  nnusers.c     	  nnuser_f.f nnculld.c  nnculls.c

OBJECTS = natgrid.o  natgrids.o nncrunchd.o nnerror.o nnuserd.o   	  natgridd.o nncrunch.o nncrunchs.o nnuser.o  nnusers.o   	  nnuser_f.o nnculld.o  nnculls.o

all::

all-local::../../libngmath.a

../../libngmath.a:: $(OBJECTS)
	$(AR_REG) ../../libngmath.a $(OBJECTS)

all::

clean-local::
	-@$(RM) $(OBJECTS)

all::

depend-local:: $(SOURCES)
	$(DEPEND) -s "# DO NOT DELETE" -- $(ALLDEFINES) -- $(SOURCES)

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

natgrid.o: nnmhead.h /usr/include/stdio.h /usr/include/sys/cdefs.h
natgrid.o: /usr/include/sys/_symbol_aliasing.h
natgrid.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
natgrid.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
natgrid.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
natgrid.o: /usr/include/i386/_types.h
natgrid.o: /usr/include/sys/_pthread/_pthread_types.h
natgrid.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
natgrid.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
natgrid.o: /usr/include/sys/_types/_int16_t.h
natgrid.o: /usr/include/sys/_types/_int32_t.h
natgrid.o: /usr/include/sys/_types/_int64_t.h
natgrid.o: /usr/include/sys/_types/_u_int8_t.h
natgrid.o: /usr/include/sys/_types/_u_int16_t.h
natgrid.o: /usr/include/sys/_types/_u_int32_t.h
natgrid.o: /usr/include/sys/_types/_u_int64_t.h
natgrid.o: /usr/include/sys/_types/_intptr_t.h
natgrid.o: /usr/include/sys/_types/_uintptr_t.h
natgrid.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
natgrid.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
natgrid.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
natgrid.o: /usr/include/secure/_common.h /usr/include/stdlib.h
natgrid.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
natgrid.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
natgrid.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
natgrid.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
natgrid.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
natgrid.o: /usr/include/mach/i386/_structs.h
natgrid.o: /usr/include/sys/_pthread/_pthread_attr_t.h
natgrid.o: /usr/include/sys/_types/_sigaltstack.h
natgrid.o: /usr/include/sys/_types/_ucontext.h
natgrid.o: /usr/include/sys/_types/_sigset_t.h
natgrid.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
natgrid.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
natgrid.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
natgrid.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
natgrid.o: /usr/include/_types/_uintmax_t.h
natgrid.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
natgrid.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
natgrid.o: /usr/include/libkern/_OSByteOrder.h
natgrid.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
natgrid.o: /usr/include/sys/_types/_ct_rune_t.h
natgrid.o: /usr/include/sys/_types/_rune_t.h
natgrid.o: /usr/include/sys/_types/_wchar_t.h
natgrid.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_mode_t.h
natgrid.o: /usr/include/math.h nnghead.h /usr/include/time.h
natgrid.o: /usr/include/sys/_types/_clock_t.h
natgrid.o: /usr/include/sys/_types/_time_t.h
natgrid.o: /usr/include/sys/_types/_timespec.h nntypes.h nntpvrs.h nnexver.h
natgrids.o: /usr/include/string.h /usr/include/_types.h
natgrids.o: /usr/include/sys/_types.h /usr/include/sys/cdefs.h
natgrids.o: /usr/include/sys/_symbol_aliasing.h
natgrids.o: /usr/include/sys/_posix_availability.h
natgrids.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
natgrids.o: /usr/include/sys/_pthread/_pthread_types.h
natgrids.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
natgrids.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
natgrids.o: /usr/include/sys/_types/_rsize_t.h /usr/include/machine/types.h
natgrids.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
natgrids.o: /usr/include/sys/_types/_int16_t.h
natgrids.o: /usr/include/sys/_types/_int32_t.h
natgrids.o: /usr/include/sys/_types/_int64_t.h
natgrids.o: /usr/include/sys/_types/_u_int8_t.h
natgrids.o: /usr/include/sys/_types/_u_int16_t.h
natgrids.o: /usr/include/sys/_types/_u_int32_t.h
natgrids.o: /usr/include/sys/_types/_u_int64_t.h
natgrids.o: /usr/include/sys/_types/_intptr_t.h
natgrids.o: /usr/include/sys/_types/_uintptr_t.h
natgrids.o: /usr/include/sys/_types/_errno_t.h
natgrids.o: /usr/include/sys/_types/_ssize_t.h /usr/include/strings.h
natgrids.o: /usr/include/secure/_strings.h /usr/include/secure/_common.h
natgrids.o: /usr/include/secure/_string.h /usr/include/stdio.h
natgrids.o: /usr/include/sys/_types/_va_list.h /usr/include/sys/stdio.h
natgrids.o: /usr/include/sys/_types/_off_t.h /usr/include/secure/_stdio.h
natgrids.o: /usr/include/stdlib.h /usr/include/sys/wait.h
natgrids.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_id_t.h
natgrids.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
natgrids.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
natgrids.o: /usr/include/machine/_mcontext.h /usr/include/i386/_mcontext.h
natgrids.o: /usr/include/mach/machine/_structs.h
natgrids.o: /usr/include/mach/i386/_structs.h
natgrids.o: /usr/include/sys/_pthread/_pthread_attr_t.h
natgrids.o: /usr/include/sys/_types/_sigaltstack.h
natgrids.o: /usr/include/sys/_types/_ucontext.h
natgrids.o: /usr/include/sys/_types/_sigset_t.h
natgrids.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
natgrids.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
natgrids.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
natgrids.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
natgrids.o: /usr/include/_types/_uintmax_t.h
natgrids.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
natgrids.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
natgrids.o: /usr/include/libkern/_OSByteOrder.h
natgrids.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
natgrids.o: /usr/include/sys/_types/_ct_rune_t.h
natgrids.o: /usr/include/sys/_types/_rune_t.h
natgrids.o: /usr/include/sys/_types/_wchar_t.h
natgrids.o: /usr/include/sys/_types/_dev_t.h
natgrids.o: /usr/include/sys/_types/_mode_t.h
natgrids.o: ../../../../.././include/ncarg/ngmath.h /usr/include/stddef.h
natgrids.o: /usr/include/sys/_types/_offsetof.h
natgrids.o: /usr/include/sys/_types/_ptrdiff_t.h
natgrids.o: /usr/include/sys/_types/_wint_t.h /usr/include/math.h nnghead.h
natgrids.o: /usr/include/time.h /usr/include/sys/_types/_clock_t.h
natgrids.o: /usr/include/sys/_types/_time_t.h
natgrids.o: /usr/include/sys/_types/_timespec.h nngheads.h nnexver.h
nncrunchd.o: nncheadd.h ../../../../.././include/ncarg/ngmath.h
nncrunchd.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
nncrunchd.o: /usr/include/sys/_symbol_aliasing.h
nncrunchd.o: /usr/include/sys/_posix_availability.h
nncrunchd.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
nncrunchd.o: /usr/include/_types.h /usr/include/sys/_types.h
nncrunchd.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
nncrunchd.o: /usr/include/sys/_pthread/_pthread_types.h
nncrunchd.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
nncrunchd.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
nncrunchd.o: /usr/include/sys/_types/_int16_t.h
nncrunchd.o: /usr/include/sys/_types/_int32_t.h
nncrunchd.o: /usr/include/sys/_types/_int64_t.h
nncrunchd.o: /usr/include/sys/_types/_u_int8_t.h
nncrunchd.o: /usr/include/sys/_types/_u_int16_t.h
nncrunchd.o: /usr/include/sys/_types/_u_int32_t.h
nncrunchd.o: /usr/include/sys/_types/_u_int64_t.h
nncrunchd.o: /usr/include/sys/_types/_intptr_t.h
nncrunchd.o: /usr/include/sys/_types/_uintptr_t.h
nncrunchd.o: /usr/include/sys/_types/_size_t.h
nncrunchd.o: /usr/include/sys/_types/_null.h /usr/include/sys/stdio.h
nncrunchd.o: /usr/include/sys/_types/_off_t.h
nncrunchd.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
nncrunchd.o: /usr/include/secure/_common.h /usr/include/stddef.h
nncrunchd.o: /usr/include/sys/_types/_offsetof.h
nncrunchd.o: /usr/include/sys/_types/_ptrdiff_t.h
nncrunchd.o: /usr/include/sys/_types/_rsize_t.h
nncrunchd.o: /usr/include/sys/_types/_wchar_t.h
nncrunchd.o: /usr/include/sys/_types/_wint_t.h /usr/include/string.h
nncrunchd.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
nncrunchd.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
nncrunchd.o: /usr/include/stdlib.h /usr/include/sys/wait.h
nncrunchd.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_id_t.h
nncrunchd.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
nncrunchd.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
nncrunchd.o: /usr/include/machine/_mcontext.h /usr/include/i386/_mcontext.h
nncrunchd.o: /usr/include/mach/machine/_structs.h
nncrunchd.o: /usr/include/mach/i386/_structs.h
nncrunchd.o: /usr/include/sys/_pthread/_pthread_attr_t.h
nncrunchd.o: /usr/include/sys/_types/_sigaltstack.h
nncrunchd.o: /usr/include/sys/_types/_ucontext.h
nncrunchd.o: /usr/include/sys/_types/_sigset_t.h
nncrunchd.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
nncrunchd.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
nncrunchd.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
nncrunchd.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
nncrunchd.o: /usr/include/_types/_uintmax_t.h
nncrunchd.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
nncrunchd.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
nncrunchd.o: /usr/include/libkern/_OSByteOrder.h
nncrunchd.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
nncrunchd.o: /usr/include/sys/_types/_ct_rune_t.h
nncrunchd.o: /usr/include/sys/_types/_rune_t.h
nncrunchd.o: /usr/include/sys/_types/_dev_t.h
nncrunchd.o: /usr/include/sys/_types/_mode_t.h /usr/include/math.h nnchead.h
nncrunchd.o: nntypes.h nnexver.h nnuheadd.h nnuhead.h
nnerror.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
nnerror.o: /usr/include/sys/_symbol_aliasing.h
nnerror.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
nnerror.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
nnerror.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
nnerror.o: /usr/include/i386/_types.h
nnerror.o: /usr/include/sys/_pthread/_pthread_types.h
nnerror.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
nnerror.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
nnerror.o: /usr/include/sys/_types/_int16_t.h
nnerror.o: /usr/include/sys/_types/_int32_t.h
nnerror.o: /usr/include/sys/_types/_int64_t.h
nnerror.o: /usr/include/sys/_types/_u_int8_t.h
nnerror.o: /usr/include/sys/_types/_u_int16_t.h
nnerror.o: /usr/include/sys/_types/_u_int32_t.h
nnerror.o: /usr/include/sys/_types/_u_int64_t.h
nnerror.o: /usr/include/sys/_types/_intptr_t.h
nnerror.o: /usr/include/sys/_types/_uintptr_t.h
nnerror.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
nnerror.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
nnerror.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
nnerror.o: /usr/include/secure/_common.h /usr/include/stdlib.h
nnerror.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
nnerror.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
nnerror.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
nnerror.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
nnerror.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
nnerror.o: /usr/include/mach/i386/_structs.h
nnerror.o: /usr/include/sys/_pthread/_pthread_attr_t.h
nnerror.o: /usr/include/sys/_types/_sigaltstack.h
nnerror.o: /usr/include/sys/_types/_ucontext.h
nnerror.o: /usr/include/sys/_types/_sigset_t.h
nnerror.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
nnerror.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
nnerror.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
nnerror.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
nnerror.o: /usr/include/_types/_uintmax_t.h
nnerror.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
nnerror.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
nnerror.o: /usr/include/libkern/_OSByteOrder.h
nnerror.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
nnerror.o: /usr/include/sys/_types/_ct_rune_t.h
nnerror.o: /usr/include/sys/_types/_rune_t.h
nnerror.o: /usr/include/sys/_types/_wchar_t.h
nnerror.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_mode_t.h
nnuserd.o: nnuheadd.h ../../../../.././include/ncarg/ngmath.h
nnuserd.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
nnuserd.o: /usr/include/sys/_symbol_aliasing.h
nnuserd.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
nnuserd.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
nnuserd.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
nnuserd.o: /usr/include/i386/_types.h
nnuserd.o: /usr/include/sys/_pthread/_pthread_types.h
nnuserd.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
nnuserd.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
nnuserd.o: /usr/include/sys/_types/_int16_t.h
nnuserd.o: /usr/include/sys/_types/_int32_t.h
nnuserd.o: /usr/include/sys/_types/_int64_t.h
nnuserd.o: /usr/include/sys/_types/_u_int8_t.h
nnuserd.o: /usr/include/sys/_types/_u_int16_t.h
nnuserd.o: /usr/include/sys/_types/_u_int32_t.h
nnuserd.o: /usr/include/sys/_types/_u_int64_t.h
nnuserd.o: /usr/include/sys/_types/_intptr_t.h
nnuserd.o: /usr/include/sys/_types/_uintptr_t.h
nnuserd.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
nnuserd.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
nnuserd.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
nnuserd.o: /usr/include/secure/_common.h /usr/include/stddef.h
nnuserd.o: /usr/include/sys/_types/_offsetof.h
nnuserd.o: /usr/include/sys/_types/_ptrdiff_t.h
nnuserd.o: /usr/include/sys/_types/_rsize_t.h
nnuserd.o: /usr/include/sys/_types/_wchar_t.h
nnuserd.o: /usr/include/sys/_types/_wint_t.h /usr/include/string.h
nnuserd.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
nnuserd.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
nnuserd.o: /usr/include/stdlib.h /usr/include/sys/wait.h
nnuserd.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_id_t.h
nnuserd.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
nnuserd.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
nnuserd.o: /usr/include/machine/_mcontext.h /usr/include/i386/_mcontext.h
nnuserd.o: /usr/include/mach/machine/_structs.h
nnuserd.o: /usr/include/mach/i386/_structs.h
nnuserd.o: /usr/include/sys/_pthread/_pthread_attr_t.h
nnuserd.o: /usr/include/sys/_types/_sigaltstack.h
nnuserd.o: /usr/include/sys/_types/_ucontext.h
nnuserd.o: /usr/include/sys/_types/_sigset_t.h
nnuserd.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
nnuserd.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
nnuserd.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
nnuserd.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
nnuserd.o: /usr/include/_types/_uintmax_t.h
nnuserd.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
nnuserd.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
nnuserd.o: /usr/include/libkern/_OSByteOrder.h
nnuserd.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
nnuserd.o: /usr/include/sys/_types/_ct_rune_t.h
nnuserd.o: /usr/include/sys/_types/_rune_t.h /usr/include/sys/_types/_dev_t.h
nnuserd.o: /usr/include/sys/_types/_mode_t.h /usr/include/math.h nnuhead.h
natgridd.o: /usr/include/string.h /usr/include/_types.h
natgridd.o: /usr/include/sys/_types.h /usr/include/sys/cdefs.h
natgridd.o: /usr/include/sys/_symbol_aliasing.h
natgridd.o: /usr/include/sys/_posix_availability.h
natgridd.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
natgridd.o: /usr/include/sys/_pthread/_pthread_types.h
natgridd.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
natgridd.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
natgridd.o: /usr/include/sys/_types/_rsize_t.h /usr/include/machine/types.h
natgridd.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
natgridd.o: /usr/include/sys/_types/_int16_t.h
natgridd.o: /usr/include/sys/_types/_int32_t.h
natgridd.o: /usr/include/sys/_types/_int64_t.h
natgridd.o: /usr/include/sys/_types/_u_int8_t.h
natgridd.o: /usr/include/sys/_types/_u_int16_t.h
natgridd.o: /usr/include/sys/_types/_u_int32_t.h
natgridd.o: /usr/include/sys/_types/_u_int64_t.h
natgridd.o: /usr/include/sys/_types/_intptr_t.h
natgridd.o: /usr/include/sys/_types/_uintptr_t.h
natgridd.o: /usr/include/sys/_types/_errno_t.h
natgridd.o: /usr/include/sys/_types/_ssize_t.h /usr/include/strings.h
natgridd.o: /usr/include/secure/_strings.h /usr/include/secure/_common.h
natgridd.o: /usr/include/secure/_string.h /usr/include/stdio.h
natgridd.o: /usr/include/sys/_types/_va_list.h /usr/include/sys/stdio.h
natgridd.o: /usr/include/sys/_types/_off_t.h /usr/include/secure/_stdio.h
natgridd.o: /usr/include/stdlib.h /usr/include/sys/wait.h
natgridd.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_id_t.h
natgridd.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
natgridd.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
natgridd.o: /usr/include/machine/_mcontext.h /usr/include/i386/_mcontext.h
natgridd.o: /usr/include/mach/machine/_structs.h
natgridd.o: /usr/include/mach/i386/_structs.h
natgridd.o: /usr/include/sys/_pthread/_pthread_attr_t.h
natgridd.o: /usr/include/sys/_types/_sigaltstack.h
natgridd.o: /usr/include/sys/_types/_ucontext.h
natgridd.o: /usr/include/sys/_types/_sigset_t.h
natgridd.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
natgridd.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
natgridd.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
natgridd.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
natgridd.o: /usr/include/_types/_uintmax_t.h
natgridd.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
natgridd.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
natgridd.o: /usr/include/libkern/_OSByteOrder.h
natgridd.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
natgridd.o: /usr/include/sys/_types/_ct_rune_t.h
natgridd.o: /usr/include/sys/_types/_rune_t.h
natgridd.o: /usr/include/sys/_types/_wchar_t.h
natgridd.o: /usr/include/sys/_types/_dev_t.h
natgridd.o: /usr/include/sys/_types/_mode_t.h
natgridd.o: ../../../../.././include/ncarg/ngmath.h /usr/include/stddef.h
natgridd.o: /usr/include/sys/_types/_offsetof.h
natgridd.o: /usr/include/sys/_types/_ptrdiff_t.h
natgridd.o: /usr/include/sys/_types/_wint_t.h /usr/include/math.h nnghead.h
natgridd.o: /usr/include/time.h /usr/include/sys/_types/_clock_t.h
natgridd.o: /usr/include/sys/_types/_time_t.h
natgridd.o: /usr/include/sys/_types/_timespec.h nngheadd.h
nncrunch.o: /usr/include/stdlib.h /usr/include/Availability.h
nncrunch.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
nncrunch.o: /usr/include/sys/_types.h /usr/include/sys/cdefs.h
nncrunch.o: /usr/include/sys/_symbol_aliasing.h
nncrunch.o: /usr/include/sys/_posix_availability.h
nncrunch.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
nncrunch.o: /usr/include/sys/_pthread/_pthread_types.h
nncrunch.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
nncrunch.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
nncrunch.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
nncrunch.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
nncrunch.o: /usr/include/i386/_mcontext.h
nncrunch.o: /usr/include/mach/machine/_structs.h
nncrunch.o: /usr/include/mach/i386/_structs.h /usr/include/machine/types.h
nncrunch.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
nncrunch.o: /usr/include/sys/_types/_int16_t.h
nncrunch.o: /usr/include/sys/_types/_int32_t.h
nncrunch.o: /usr/include/sys/_types/_int64_t.h
nncrunch.o: /usr/include/sys/_types/_u_int8_t.h
nncrunch.o: /usr/include/sys/_types/_u_int16_t.h
nncrunch.o: /usr/include/sys/_types/_u_int32_t.h
nncrunch.o: /usr/include/sys/_types/_u_int64_t.h
nncrunch.o: /usr/include/sys/_types/_intptr_t.h
nncrunch.o: /usr/include/sys/_types/_uintptr_t.h
nncrunch.o: /usr/include/sys/_pthread/_pthread_attr_t.h
nncrunch.o: /usr/include/sys/_types/_sigaltstack.h
nncrunch.o: /usr/include/sys/_types/_ucontext.h
nncrunch.o: /usr/include/sys/_types/_sigset_t.h
nncrunch.o: /usr/include/sys/_types/_size_t.h
nncrunch.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
nncrunch.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
nncrunch.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
nncrunch.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
nncrunch.o: /usr/include/_types/_uintmax_t.h
nncrunch.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
nncrunch.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
nncrunch.o: /usr/include/libkern/_OSByteOrder.h
nncrunch.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
nncrunch.o: /usr/include/sys/_types/_ct_rune_t.h
nncrunch.o: /usr/include/sys/_types/_rune_t.h
nncrunch.o: /usr/include/sys/_types/_wchar_t.h
nncrunch.o: /usr/include/sys/_types/_null.h /usr/include/sys/_types/_dev_t.h
nncrunch.o: /usr/include/sys/_types/_mode_t.h /usr/include/math.h
nncrunch.o: ../../../../.././include/ncarg/ngmath.h /usr/include/stdio.h
nncrunch.o: /usr/include/sys/_types/_va_list.h /usr/include/sys/stdio.h
nncrunch.o: /usr/include/sys/_types/_off_t.h
nncrunch.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
nncrunch.o: /usr/include/secure/_common.h /usr/include/stddef.h
nncrunch.o: /usr/include/sys/_types/_offsetof.h
nncrunch.o: /usr/include/sys/_types/_ptrdiff_t.h
nncrunch.o: /usr/include/sys/_types/_rsize_t.h
nncrunch.o: /usr/include/sys/_types/_wint_t.h /usr/include/string.h
nncrunch.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
nncrunch.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
nncrunch.o: nncheads.h nnchead.h nntypes.h nnexver.h nnuheads.h nnuhead.h
nncrunchs.o: nncheads.h ../../../../.././include/ncarg/ngmath.h
nncrunchs.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
nncrunchs.o: /usr/include/sys/_symbol_aliasing.h
nncrunchs.o: /usr/include/sys/_posix_availability.h
nncrunchs.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
nncrunchs.o: /usr/include/_types.h /usr/include/sys/_types.h
nncrunchs.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
nncrunchs.o: /usr/include/sys/_pthread/_pthread_types.h
nncrunchs.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
nncrunchs.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
nncrunchs.o: /usr/include/sys/_types/_int16_t.h
nncrunchs.o: /usr/include/sys/_types/_int32_t.h
nncrunchs.o: /usr/include/sys/_types/_int64_t.h
nncrunchs.o: /usr/include/sys/_types/_u_int8_t.h
nncrunchs.o: /usr/include/sys/_types/_u_int16_t.h
nncrunchs.o: /usr/include/sys/_types/_u_int32_t.h
nncrunchs.o: /usr/include/sys/_types/_u_int64_t.h
nncrunchs.o: /usr/include/sys/_types/_intptr_t.h
nncrunchs.o: /usr/include/sys/_types/_uintptr_t.h
nncrunchs.o: /usr/include/sys/_types/_size_t.h
nncrunchs.o: /usr/include/sys/_types/_null.h /usr/include/sys/stdio.h
nncrunchs.o: /usr/include/sys/_types/_off_t.h
nncrunchs.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
nncrunchs.o: /usr/include/secure/_common.h /usr/include/stddef.h
nncrunchs.o: /usr/include/sys/_types/_offsetof.h
nncrunchs.o: /usr/include/sys/_types/_ptrdiff_t.h
nncrunchs.o: /usr/include/sys/_types/_rsize_t.h
nncrunchs.o: /usr/include/sys/_types/_wchar_t.h
nncrunchs.o: /usr/include/sys/_types/_wint_t.h /usr/include/string.h
nncrunchs.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
nncrunchs.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
nncrunchs.o: /usr/include/stdlib.h /usr/include/sys/wait.h
nncrunchs.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_id_t.h
nncrunchs.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
nncrunchs.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
nncrunchs.o: /usr/include/machine/_mcontext.h /usr/include/i386/_mcontext.h
nncrunchs.o: /usr/include/mach/machine/_structs.h
nncrunchs.o: /usr/include/mach/i386/_structs.h
nncrunchs.o: /usr/include/sys/_pthread/_pthread_attr_t.h
nncrunchs.o: /usr/include/sys/_types/_sigaltstack.h
nncrunchs.o: /usr/include/sys/_types/_ucontext.h
nncrunchs.o: /usr/include/sys/_types/_sigset_t.h
nncrunchs.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
nncrunchs.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
nncrunchs.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
nncrunchs.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
nncrunchs.o: /usr/include/_types/_uintmax_t.h
nncrunchs.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
nncrunchs.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
nncrunchs.o: /usr/include/libkern/_OSByteOrder.h
nncrunchs.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
nncrunchs.o: /usr/include/sys/_types/_ct_rune_t.h
nncrunchs.o: /usr/include/sys/_types/_rune_t.h
nncrunchs.o: /usr/include/sys/_types/_dev_t.h
nncrunchs.o: /usr/include/sys/_types/_mode_t.h /usr/include/math.h nnchead.h
nncrunchs.o: nntypes.h nnexver.h nnuheads.h nnuhead.h
nnuser.o: nnuheads.h ../../../../.././include/ncarg/ngmath.h
nnuser.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
nnuser.o: /usr/include/sys/_symbol_aliasing.h
nnuser.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
nnuser.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
nnuser.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
nnuser.o: /usr/include/i386/_types.h
nnuser.o: /usr/include/sys/_pthread/_pthread_types.h
nnuser.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
nnuser.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
nnuser.o: /usr/include/sys/_types/_int16_t.h
nnuser.o: /usr/include/sys/_types/_int32_t.h
nnuser.o: /usr/include/sys/_types/_int64_t.h
nnuser.o: /usr/include/sys/_types/_u_int8_t.h
nnuser.o: /usr/include/sys/_types/_u_int16_t.h
nnuser.o: /usr/include/sys/_types/_u_int32_t.h
nnuser.o: /usr/include/sys/_types/_u_int64_t.h
nnuser.o: /usr/include/sys/_types/_intptr_t.h
nnuser.o: /usr/include/sys/_types/_uintptr_t.h
nnuser.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
nnuser.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
nnuser.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
nnuser.o: /usr/include/secure/_common.h /usr/include/stddef.h
nnuser.o: /usr/include/sys/_types/_offsetof.h
nnuser.o: /usr/include/sys/_types/_ptrdiff_t.h
nnuser.o: /usr/include/sys/_types/_rsize_t.h
nnuser.o: /usr/include/sys/_types/_wchar_t.h
nnuser.o: /usr/include/sys/_types/_wint_t.h /usr/include/string.h
nnuser.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
nnuser.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
nnuser.o: /usr/include/stdlib.h /usr/include/sys/wait.h
nnuser.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_id_t.h
nnuser.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
nnuser.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
nnuser.o: /usr/include/machine/_mcontext.h /usr/include/i386/_mcontext.h
nnuser.o: /usr/include/mach/machine/_structs.h
nnuser.o: /usr/include/mach/i386/_structs.h
nnuser.o: /usr/include/sys/_pthread/_pthread_attr_t.h
nnuser.o: /usr/include/sys/_types/_sigaltstack.h
nnuser.o: /usr/include/sys/_types/_ucontext.h
nnuser.o: /usr/include/sys/_types/_sigset_t.h
nnuser.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
nnuser.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
nnuser.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
nnuser.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
nnuser.o: /usr/include/_types/_uintmax_t.h /usr/include/sys/_types/_timeval.h
nnuser.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
nnuser.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
nnuser.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
nnuser.o: /usr/include/sys/_types/_ct_rune_t.h
nnuser.o: /usr/include/sys/_types/_rune_t.h /usr/include/sys/_types/_dev_t.h
nnuser.o: /usr/include/sys/_types/_mode_t.h /usr/include/math.h nnuhead.h
nnusers.o: nnuheads.h ../../../../.././include/ncarg/ngmath.h
nnusers.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
nnusers.o: /usr/include/sys/_symbol_aliasing.h
nnusers.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
nnusers.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
nnusers.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
nnusers.o: /usr/include/i386/_types.h
nnusers.o: /usr/include/sys/_pthread/_pthread_types.h
nnusers.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
nnusers.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
nnusers.o: /usr/include/sys/_types/_int16_t.h
nnusers.o: /usr/include/sys/_types/_int32_t.h
nnusers.o: /usr/include/sys/_types/_int64_t.h
nnusers.o: /usr/include/sys/_types/_u_int8_t.h
nnusers.o: /usr/include/sys/_types/_u_int16_t.h
nnusers.o: /usr/include/sys/_types/_u_int32_t.h
nnusers.o: /usr/include/sys/_types/_u_int64_t.h
nnusers.o: /usr/include/sys/_types/_intptr_t.h
nnusers.o: /usr/include/sys/_types/_uintptr_t.h
nnusers.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
nnusers.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
nnusers.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
nnusers.o: /usr/include/secure/_common.h /usr/include/stddef.h
nnusers.o: /usr/include/sys/_types/_offsetof.h
nnusers.o: /usr/include/sys/_types/_ptrdiff_t.h
nnusers.o: /usr/include/sys/_types/_rsize_t.h
nnusers.o: /usr/include/sys/_types/_wchar_t.h
nnusers.o: /usr/include/sys/_types/_wint_t.h /usr/include/string.h
nnusers.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
nnusers.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
nnusers.o: /usr/include/stdlib.h /usr/include/sys/wait.h
nnusers.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_id_t.h
nnusers.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
nnusers.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
nnusers.o: /usr/include/machine/_mcontext.h /usr/include/i386/_mcontext.h
nnusers.o: /usr/include/mach/machine/_structs.h
nnusers.o: /usr/include/mach/i386/_structs.h
nnusers.o: /usr/include/sys/_pthread/_pthread_attr_t.h
nnusers.o: /usr/include/sys/_types/_sigaltstack.h
nnusers.o: /usr/include/sys/_types/_ucontext.h
nnusers.o: /usr/include/sys/_types/_sigset_t.h
nnusers.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
nnusers.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
nnusers.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
nnusers.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
nnusers.o: /usr/include/_types/_uintmax_t.h
nnusers.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
nnusers.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
nnusers.o: /usr/include/libkern/_OSByteOrder.h
nnusers.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
nnusers.o: /usr/include/sys/_types/_ct_rune_t.h
nnusers.o: /usr/include/sys/_types/_rune_t.h /usr/include/sys/_types/_dev_t.h
nnusers.o: /usr/include/sys/_types/_mode_t.h /usr/include/math.h nnuhead.h
nnculld.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
nnculld.o: /usr/include/sys/_symbol_aliasing.h
nnculld.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
nnculld.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
nnculld.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
nnculld.o: /usr/include/i386/_types.h
nnculld.o: /usr/include/sys/_pthread/_pthread_types.h
nnculld.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
nnculld.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
nnculld.o: /usr/include/sys/_types/_int16_t.h
nnculld.o: /usr/include/sys/_types/_int32_t.h
nnculld.o: /usr/include/sys/_types/_int64_t.h
nnculld.o: /usr/include/sys/_types/_u_int8_t.h
nnculld.o: /usr/include/sys/_types/_u_int16_t.h
nnculld.o: /usr/include/sys/_types/_u_int32_t.h
nnculld.o: /usr/include/sys/_types/_u_int64_t.h
nnculld.o: /usr/include/sys/_types/_intptr_t.h
nnculld.o: /usr/include/sys/_types/_uintptr_t.h
nnculld.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
nnculld.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
nnculld.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
nnculld.o: /usr/include/secure/_common.h /usr/include/string.h
nnculld.o: /usr/include/sys/_types/_rsize_t.h
nnculld.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
nnculld.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
nnculld.o: ../../../../.././include/ncarg/ngmath.h /usr/include/stddef.h
nnculld.o: /usr/include/sys/_types/_offsetof.h
nnculld.o: /usr/include/sys/_types/_ptrdiff_t.h
nnculld.o: /usr/include/sys/_types/_wchar_t.h
nnculld.o: /usr/include/sys/_types/_wint_t.h /usr/include/stdlib.h
nnculld.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
nnculld.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
nnculld.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
nnculld.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
nnculld.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
nnculld.o: /usr/include/mach/i386/_structs.h
nnculld.o: /usr/include/sys/_pthread/_pthread_attr_t.h
nnculld.o: /usr/include/sys/_types/_sigaltstack.h
nnculld.o: /usr/include/sys/_types/_ucontext.h
nnculld.o: /usr/include/sys/_types/_sigset_t.h
nnculld.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
nnculld.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
nnculld.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
nnculld.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
nnculld.o: /usr/include/_types/_uintmax_t.h
nnculld.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
nnculld.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
nnculld.o: /usr/include/libkern/_OSByteOrder.h
nnculld.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
nnculld.o: /usr/include/sys/_types/_ct_rune_t.h
nnculld.o: /usr/include/sys/_types/_rune_t.h /usr/include/sys/_types/_dev_t.h
nnculld.o: /usr/include/sys/_types/_mode_t.h /usr/include/math.h
nnculls.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
nnculls.o: /usr/include/sys/_symbol_aliasing.h
nnculls.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
nnculls.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
nnculls.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
nnculls.o: /usr/include/i386/_types.h
nnculls.o: /usr/include/sys/_pthread/_pthread_types.h
nnculls.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
nnculls.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
nnculls.o: /usr/include/sys/_types/_int16_t.h
nnculls.o: /usr/include/sys/_types/_int32_t.h
nnculls.o: /usr/include/sys/_types/_int64_t.h
nnculls.o: /usr/include/sys/_types/_u_int8_t.h
nnculls.o: /usr/include/sys/_types/_u_int16_t.h
nnculls.o: /usr/include/sys/_types/_u_int32_t.h
nnculls.o: /usr/include/sys/_types/_u_int64_t.h
nnculls.o: /usr/include/sys/_types/_intptr_t.h
nnculls.o: /usr/include/sys/_types/_uintptr_t.h
nnculls.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
nnculls.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
nnculls.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
nnculls.o: /usr/include/secure/_common.h /usr/include/string.h
nnculls.o: /usr/include/sys/_types/_rsize_t.h
nnculls.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
nnculls.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
nnculls.o: ../../../../.././include/ncarg/ngmath.h /usr/include/stddef.h
nnculls.o: /usr/include/sys/_types/_offsetof.h
nnculls.o: /usr/include/sys/_types/_ptrdiff_t.h
nnculls.o: /usr/include/sys/_types/_wchar_t.h
nnculls.o: /usr/include/sys/_types/_wint_t.h /usr/include/stdlib.h
nnculls.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
nnculls.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
nnculls.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
nnculls.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
nnculls.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
nnculls.o: /usr/include/mach/i386/_structs.h
nnculls.o: /usr/include/sys/_pthread/_pthread_attr_t.h
nnculls.o: /usr/include/sys/_types/_sigaltstack.h
nnculls.o: /usr/include/sys/_types/_ucontext.h
nnculls.o: /usr/include/sys/_types/_sigset_t.h
nnculls.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
nnculls.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
nnculls.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
nnculls.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
nnculls.o: /usr/include/_types/_uintmax_t.h
nnculls.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
nnculls.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
nnculls.o: /usr/include/libkern/_OSByteOrder.h
nnculls.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
nnculls.o: /usr/include/sys/_types/_ct_rune_t.h
nnculls.o: /usr/include/sys/_types/_rune_t.h /usr/include/sys/_types/_dev_t.h
nnculls.o: /usr/include/sys/_types/_mode_t.h /usr/include/math.h
