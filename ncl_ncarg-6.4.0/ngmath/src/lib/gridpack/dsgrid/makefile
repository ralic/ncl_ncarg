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
CURRENT_DIR		= ./ngmath/src/lib/gridpack/dsgrid

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
#       $Id: yMakefile,v 1.1 1997-08-05 23:15:57 fred Exp $
#

MYNAME  = dsgrid

SOURCES = dsgrid3d.c dsgrid2d.c dsgrid3s.c dsgrid2s.c  dsuser.c   	  dsusers.c  dsuserd.c  dssprtd.c  dssprts.c   dserror.c  	  dsuser_f.f

OBJECTS = dsgrid3d.o dsgrid2d.o dsgrid3s.o dsgrid2s.o  dsuser.o   	  dsusers.o  dsuserd.o  dssprtd.o  dssprts.o   dserror.o  	  dsuser_f.o

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

dsgrid3d.o: ../../../../.././include/ncarg/ngmath.h /usr/include/stdio.h
dsgrid3d.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
dsgrid3d.o: /usr/include/sys/_posix_availability.h
dsgrid3d.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
dsgrid3d.o: /usr/include/_types.h /usr/include/sys/_types.h
dsgrid3d.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
dsgrid3d.o: /usr/include/sys/_pthread/_pthread_types.h
dsgrid3d.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
dsgrid3d.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
dsgrid3d.o: /usr/include/sys/_types/_int16_t.h
dsgrid3d.o: /usr/include/sys/_types/_int32_t.h
dsgrid3d.o: /usr/include/sys/_types/_int64_t.h
dsgrid3d.o: /usr/include/sys/_types/_u_int8_t.h
dsgrid3d.o: /usr/include/sys/_types/_u_int16_t.h
dsgrid3d.o: /usr/include/sys/_types/_u_int32_t.h
dsgrid3d.o: /usr/include/sys/_types/_u_int64_t.h
dsgrid3d.o: /usr/include/sys/_types/_intptr_t.h
dsgrid3d.o: /usr/include/sys/_types/_uintptr_t.h
dsgrid3d.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
dsgrid3d.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
dsgrid3d.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
dsgrid3d.o: /usr/include/secure/_common.h /usr/include/stddef.h
dsgrid3d.o: /usr/include/sys/_types/_offsetof.h
dsgrid3d.o: /usr/include/sys/_types/_ptrdiff_t.h
dsgrid3d.o: /usr/include/sys/_types/_rsize_t.h
dsgrid3d.o: /usr/include/sys/_types/_wchar_t.h
dsgrid3d.o: /usr/include/sys/_types/_wint_t.h /usr/include/string.h
dsgrid3d.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
dsgrid3d.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
dsgrid3d.o: /usr/include/stdlib.h /usr/include/sys/wait.h
dsgrid3d.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_id_t.h
dsgrid3d.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
dsgrid3d.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
dsgrid3d.o: /usr/include/machine/_mcontext.h /usr/include/i386/_mcontext.h
dsgrid3d.o: /usr/include/mach/machine/_structs.h
dsgrid3d.o: /usr/include/mach/i386/_structs.h
dsgrid3d.o: /usr/include/sys/_pthread/_pthread_attr_t.h
dsgrid3d.o: /usr/include/sys/_types/_sigaltstack.h
dsgrid3d.o: /usr/include/sys/_types/_ucontext.h
dsgrid3d.o: /usr/include/sys/_types/_sigset_t.h
dsgrid3d.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
dsgrid3d.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
dsgrid3d.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
dsgrid3d.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
dsgrid3d.o: /usr/include/_types/_uintmax_t.h
dsgrid3d.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
dsgrid3d.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
dsgrid3d.o: /usr/include/libkern/_OSByteOrder.h
dsgrid3d.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
dsgrid3d.o: /usr/include/sys/_types/_ct_rune_t.h
dsgrid3d.o: /usr/include/sys/_types/_rune_t.h
dsgrid3d.o: /usr/include/sys/_types/_dev_t.h
dsgrid3d.o: /usr/include/sys/_types/_mode_t.h /usr/include/math.h dstypes.h
dsgrid3d.o: dsproto.h dsuhead.h
dsgrid2d.o: ../../../../.././include/ncarg/ngmath.h /usr/include/stdio.h
dsgrid2d.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
dsgrid2d.o: /usr/include/sys/_posix_availability.h
dsgrid2d.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
dsgrid2d.o: /usr/include/_types.h /usr/include/sys/_types.h
dsgrid2d.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
dsgrid2d.o: /usr/include/sys/_pthread/_pthread_types.h
dsgrid2d.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
dsgrid2d.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
dsgrid2d.o: /usr/include/sys/_types/_int16_t.h
dsgrid2d.o: /usr/include/sys/_types/_int32_t.h
dsgrid2d.o: /usr/include/sys/_types/_int64_t.h
dsgrid2d.o: /usr/include/sys/_types/_u_int8_t.h
dsgrid2d.o: /usr/include/sys/_types/_u_int16_t.h
dsgrid2d.o: /usr/include/sys/_types/_u_int32_t.h
dsgrid2d.o: /usr/include/sys/_types/_u_int64_t.h
dsgrid2d.o: /usr/include/sys/_types/_intptr_t.h
dsgrid2d.o: /usr/include/sys/_types/_uintptr_t.h
dsgrid2d.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
dsgrid2d.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
dsgrid2d.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
dsgrid2d.o: /usr/include/secure/_common.h /usr/include/stddef.h
dsgrid2d.o: /usr/include/sys/_types/_offsetof.h
dsgrid2d.o: /usr/include/sys/_types/_ptrdiff_t.h
dsgrid2d.o: /usr/include/sys/_types/_rsize_t.h
dsgrid2d.o: /usr/include/sys/_types/_wchar_t.h
dsgrid2d.o: /usr/include/sys/_types/_wint_t.h /usr/include/string.h
dsgrid2d.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
dsgrid2d.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
dsgrid2d.o: /usr/include/stdlib.h /usr/include/sys/wait.h
dsgrid2d.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_id_t.h
dsgrid2d.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
dsgrid2d.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
dsgrid2d.o: /usr/include/machine/_mcontext.h /usr/include/i386/_mcontext.h
dsgrid2d.o: /usr/include/mach/machine/_structs.h
dsgrid2d.o: /usr/include/mach/i386/_structs.h
dsgrid2d.o: /usr/include/sys/_pthread/_pthread_attr_t.h
dsgrid2d.o: /usr/include/sys/_types/_sigaltstack.h
dsgrid2d.o: /usr/include/sys/_types/_ucontext.h
dsgrid2d.o: /usr/include/sys/_types/_sigset_t.h
dsgrid2d.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
dsgrid2d.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
dsgrid2d.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
dsgrid2d.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
dsgrid2d.o: /usr/include/_types/_uintmax_t.h
dsgrid2d.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
dsgrid2d.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
dsgrid2d.o: /usr/include/libkern/_OSByteOrder.h
dsgrid2d.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
dsgrid2d.o: /usr/include/sys/_types/_ct_rune_t.h
dsgrid2d.o: /usr/include/sys/_types/_rune_t.h
dsgrid2d.o: /usr/include/sys/_types/_dev_t.h
dsgrid2d.o: /usr/include/sys/_types/_mode_t.h /usr/include/math.h dstypes.h
dsgrid2d.o: dsproto.h dsuhead.h
dsgrid3s.o: ../../../../.././include/ncarg/ngmath.h /usr/include/stdio.h
dsgrid3s.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
dsgrid3s.o: /usr/include/sys/_posix_availability.h
dsgrid3s.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
dsgrid3s.o: /usr/include/_types.h /usr/include/sys/_types.h
dsgrid3s.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
dsgrid3s.o: /usr/include/sys/_pthread/_pthread_types.h
dsgrid3s.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
dsgrid3s.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
dsgrid3s.o: /usr/include/sys/_types/_int16_t.h
dsgrid3s.o: /usr/include/sys/_types/_int32_t.h
dsgrid3s.o: /usr/include/sys/_types/_int64_t.h
dsgrid3s.o: /usr/include/sys/_types/_u_int8_t.h
dsgrid3s.o: /usr/include/sys/_types/_u_int16_t.h
dsgrid3s.o: /usr/include/sys/_types/_u_int32_t.h
dsgrid3s.o: /usr/include/sys/_types/_u_int64_t.h
dsgrid3s.o: /usr/include/sys/_types/_intptr_t.h
dsgrid3s.o: /usr/include/sys/_types/_uintptr_t.h
dsgrid3s.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
dsgrid3s.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
dsgrid3s.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
dsgrid3s.o: /usr/include/secure/_common.h /usr/include/stddef.h
dsgrid3s.o: /usr/include/sys/_types/_offsetof.h
dsgrid3s.o: /usr/include/sys/_types/_ptrdiff_t.h
dsgrid3s.o: /usr/include/sys/_types/_rsize_t.h
dsgrid3s.o: /usr/include/sys/_types/_wchar_t.h
dsgrid3s.o: /usr/include/sys/_types/_wint_t.h /usr/include/string.h
dsgrid3s.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
dsgrid3s.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
dsgrid3s.o: /usr/include/stdlib.h /usr/include/sys/wait.h
dsgrid3s.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_id_t.h
dsgrid3s.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
dsgrid3s.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
dsgrid3s.o: /usr/include/machine/_mcontext.h /usr/include/i386/_mcontext.h
dsgrid3s.o: /usr/include/mach/machine/_structs.h
dsgrid3s.o: /usr/include/mach/i386/_structs.h
dsgrid3s.o: /usr/include/sys/_pthread/_pthread_attr_t.h
dsgrid3s.o: /usr/include/sys/_types/_sigaltstack.h
dsgrid3s.o: /usr/include/sys/_types/_ucontext.h
dsgrid3s.o: /usr/include/sys/_types/_sigset_t.h
dsgrid3s.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
dsgrid3s.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
dsgrid3s.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
dsgrid3s.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
dsgrid3s.o: /usr/include/_types/_uintmax_t.h
dsgrid3s.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
dsgrid3s.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
dsgrid3s.o: /usr/include/libkern/_OSByteOrder.h
dsgrid3s.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
dsgrid3s.o: /usr/include/sys/_types/_ct_rune_t.h
dsgrid3s.o: /usr/include/sys/_types/_rune_t.h
dsgrid3s.o: /usr/include/sys/_types/_dev_t.h
dsgrid3s.o: /usr/include/sys/_types/_mode_t.h /usr/include/math.h dstypes.h
dsgrid3s.o: dsproto.h dsuhead.h
dsgrid2s.o: ../../../../.././include/ncarg/ngmath.h /usr/include/stdio.h
dsgrid2s.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
dsgrid2s.o: /usr/include/sys/_posix_availability.h
dsgrid2s.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
dsgrid2s.o: /usr/include/_types.h /usr/include/sys/_types.h
dsgrid2s.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
dsgrid2s.o: /usr/include/sys/_pthread/_pthread_types.h
dsgrid2s.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
dsgrid2s.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
dsgrid2s.o: /usr/include/sys/_types/_int16_t.h
dsgrid2s.o: /usr/include/sys/_types/_int32_t.h
dsgrid2s.o: /usr/include/sys/_types/_int64_t.h
dsgrid2s.o: /usr/include/sys/_types/_u_int8_t.h
dsgrid2s.o: /usr/include/sys/_types/_u_int16_t.h
dsgrid2s.o: /usr/include/sys/_types/_u_int32_t.h
dsgrid2s.o: /usr/include/sys/_types/_u_int64_t.h
dsgrid2s.o: /usr/include/sys/_types/_intptr_t.h
dsgrid2s.o: /usr/include/sys/_types/_uintptr_t.h
dsgrid2s.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
dsgrid2s.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
dsgrid2s.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
dsgrid2s.o: /usr/include/secure/_common.h /usr/include/stddef.h
dsgrid2s.o: /usr/include/sys/_types/_offsetof.h
dsgrid2s.o: /usr/include/sys/_types/_ptrdiff_t.h
dsgrid2s.o: /usr/include/sys/_types/_rsize_t.h
dsgrid2s.o: /usr/include/sys/_types/_wchar_t.h
dsgrid2s.o: /usr/include/sys/_types/_wint_t.h /usr/include/string.h
dsgrid2s.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
dsgrid2s.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
dsgrid2s.o: /usr/include/stdlib.h /usr/include/sys/wait.h
dsgrid2s.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_id_t.h
dsgrid2s.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
dsgrid2s.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
dsgrid2s.o: /usr/include/machine/_mcontext.h /usr/include/i386/_mcontext.h
dsgrid2s.o: /usr/include/mach/machine/_structs.h
dsgrid2s.o: /usr/include/mach/i386/_structs.h
dsgrid2s.o: /usr/include/sys/_pthread/_pthread_attr_t.h
dsgrid2s.o: /usr/include/sys/_types/_sigaltstack.h
dsgrid2s.o: /usr/include/sys/_types/_ucontext.h
dsgrid2s.o: /usr/include/sys/_types/_sigset_t.h
dsgrid2s.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
dsgrid2s.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
dsgrid2s.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
dsgrid2s.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
dsgrid2s.o: /usr/include/_types/_uintmax_t.h
dsgrid2s.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
dsgrid2s.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
dsgrid2s.o: /usr/include/libkern/_OSByteOrder.h
dsgrid2s.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
dsgrid2s.o: /usr/include/sys/_types/_ct_rune_t.h
dsgrid2s.o: /usr/include/sys/_types/_rune_t.h
dsgrid2s.o: /usr/include/sys/_types/_dev_t.h
dsgrid2s.o: /usr/include/sys/_types/_mode_t.h /usr/include/math.h dstypes.h
dsgrid2s.o: dsproto.h dsuhead.h
dsuser.o: ../../../../.././include/ncarg/ngmath.h /usr/include/stdio.h
dsuser.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
dsuser.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
dsuser.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
dsuser.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
dsuser.o: /usr/include/i386/_types.h
dsuser.o: /usr/include/sys/_pthread/_pthread_types.h
dsuser.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
dsuser.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
dsuser.o: /usr/include/sys/_types/_int16_t.h
dsuser.o: /usr/include/sys/_types/_int32_t.h
dsuser.o: /usr/include/sys/_types/_int64_t.h
dsuser.o: /usr/include/sys/_types/_u_int8_t.h
dsuser.o: /usr/include/sys/_types/_u_int16_t.h
dsuser.o: /usr/include/sys/_types/_u_int32_t.h
dsuser.o: /usr/include/sys/_types/_u_int64_t.h
dsuser.o: /usr/include/sys/_types/_intptr_t.h
dsuser.o: /usr/include/sys/_types/_uintptr_t.h
dsuser.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
dsuser.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
dsuser.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
dsuser.o: /usr/include/secure/_common.h /usr/include/stddef.h
dsuser.o: /usr/include/sys/_types/_offsetof.h
dsuser.o: /usr/include/sys/_types/_ptrdiff_t.h
dsuser.o: /usr/include/sys/_types/_rsize_t.h
dsuser.o: /usr/include/sys/_types/_wchar_t.h
dsuser.o: /usr/include/sys/_types/_wint_t.h /usr/include/string.h
dsuser.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
dsuser.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
dsuser.o: /usr/include/stdlib.h /usr/include/sys/wait.h
dsuser.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_id_t.h
dsuser.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
dsuser.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
dsuser.o: /usr/include/machine/_mcontext.h /usr/include/i386/_mcontext.h
dsuser.o: /usr/include/mach/machine/_structs.h
dsuser.o: /usr/include/mach/i386/_structs.h
dsuser.o: /usr/include/sys/_pthread/_pthread_attr_t.h
dsuser.o: /usr/include/sys/_types/_sigaltstack.h
dsuser.o: /usr/include/sys/_types/_ucontext.h
dsuser.o: /usr/include/sys/_types/_sigset_t.h
dsuser.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
dsuser.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
dsuser.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
dsuser.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
dsuser.o: /usr/include/_types/_uintmax_t.h /usr/include/sys/_types/_timeval.h
dsuser.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
dsuser.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
dsuser.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
dsuser.o: /usr/include/sys/_types/_ct_rune_t.h
dsuser.o: /usr/include/sys/_types/_rune_t.h /usr/include/sys/_types/_dev_t.h
dsuser.o: /usr/include/sys/_types/_mode_t.h /usr/include/math.h dstypes.h
dsuser.o: dsproto.h dsuhead.h
dsusers.o: ../../../../.././include/ncarg/ngmath.h /usr/include/stdio.h
dsusers.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
dsusers.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
dsusers.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
dsusers.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
dsusers.o: /usr/include/i386/_types.h
dsusers.o: /usr/include/sys/_pthread/_pthread_types.h
dsusers.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
dsusers.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
dsusers.o: /usr/include/sys/_types/_int16_t.h
dsusers.o: /usr/include/sys/_types/_int32_t.h
dsusers.o: /usr/include/sys/_types/_int64_t.h
dsusers.o: /usr/include/sys/_types/_u_int8_t.h
dsusers.o: /usr/include/sys/_types/_u_int16_t.h
dsusers.o: /usr/include/sys/_types/_u_int32_t.h
dsusers.o: /usr/include/sys/_types/_u_int64_t.h
dsusers.o: /usr/include/sys/_types/_intptr_t.h
dsusers.o: /usr/include/sys/_types/_uintptr_t.h
dsusers.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
dsusers.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
dsusers.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
dsusers.o: /usr/include/secure/_common.h /usr/include/stddef.h
dsusers.o: /usr/include/sys/_types/_offsetof.h
dsusers.o: /usr/include/sys/_types/_ptrdiff_t.h
dsusers.o: /usr/include/sys/_types/_rsize_t.h
dsusers.o: /usr/include/sys/_types/_wchar_t.h
dsusers.o: /usr/include/sys/_types/_wint_t.h /usr/include/string.h
dsusers.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
dsusers.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
dsusers.o: /usr/include/stdlib.h /usr/include/sys/wait.h
dsusers.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_id_t.h
dsusers.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
dsusers.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
dsusers.o: /usr/include/machine/_mcontext.h /usr/include/i386/_mcontext.h
dsusers.o: /usr/include/mach/machine/_structs.h
dsusers.o: /usr/include/mach/i386/_structs.h
dsusers.o: /usr/include/sys/_pthread/_pthread_attr_t.h
dsusers.o: /usr/include/sys/_types/_sigaltstack.h
dsusers.o: /usr/include/sys/_types/_ucontext.h
dsusers.o: /usr/include/sys/_types/_sigset_t.h
dsusers.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
dsusers.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
dsusers.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
dsusers.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
dsusers.o: /usr/include/_types/_uintmax_t.h
dsusers.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
dsusers.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
dsusers.o: /usr/include/libkern/_OSByteOrder.h
dsusers.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
dsusers.o: /usr/include/sys/_types/_ct_rune_t.h
dsusers.o: /usr/include/sys/_types/_rune_t.h /usr/include/sys/_types/_dev_t.h
dsusers.o: /usr/include/sys/_types/_mode_t.h /usr/include/math.h dstypes.h
dsusers.o: dsproto.h dsuhead.h
dsuserd.o: ../../../../.././include/ncarg/ngmath.h /usr/include/stdio.h
dsuserd.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
dsuserd.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
dsuserd.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
dsuserd.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
dsuserd.o: /usr/include/i386/_types.h
dsuserd.o: /usr/include/sys/_pthread/_pthread_types.h
dsuserd.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
dsuserd.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
dsuserd.o: /usr/include/sys/_types/_int16_t.h
dsuserd.o: /usr/include/sys/_types/_int32_t.h
dsuserd.o: /usr/include/sys/_types/_int64_t.h
dsuserd.o: /usr/include/sys/_types/_u_int8_t.h
dsuserd.o: /usr/include/sys/_types/_u_int16_t.h
dsuserd.o: /usr/include/sys/_types/_u_int32_t.h
dsuserd.o: /usr/include/sys/_types/_u_int64_t.h
dsuserd.o: /usr/include/sys/_types/_intptr_t.h
dsuserd.o: /usr/include/sys/_types/_uintptr_t.h
dsuserd.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
dsuserd.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
dsuserd.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
dsuserd.o: /usr/include/secure/_common.h /usr/include/stddef.h
dsuserd.o: /usr/include/sys/_types/_offsetof.h
dsuserd.o: /usr/include/sys/_types/_ptrdiff_t.h
dsuserd.o: /usr/include/sys/_types/_rsize_t.h
dsuserd.o: /usr/include/sys/_types/_wchar_t.h
dsuserd.o: /usr/include/sys/_types/_wint_t.h /usr/include/string.h
dsuserd.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
dsuserd.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
dsuserd.o: /usr/include/stdlib.h /usr/include/sys/wait.h
dsuserd.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_id_t.h
dsuserd.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
dsuserd.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
dsuserd.o: /usr/include/machine/_mcontext.h /usr/include/i386/_mcontext.h
dsuserd.o: /usr/include/mach/machine/_structs.h
dsuserd.o: /usr/include/mach/i386/_structs.h
dsuserd.o: /usr/include/sys/_pthread/_pthread_attr_t.h
dsuserd.o: /usr/include/sys/_types/_sigaltstack.h
dsuserd.o: /usr/include/sys/_types/_ucontext.h
dsuserd.o: /usr/include/sys/_types/_sigset_t.h
dsuserd.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
dsuserd.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
dsuserd.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
dsuserd.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
dsuserd.o: /usr/include/_types/_uintmax_t.h
dsuserd.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
dsuserd.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
dsuserd.o: /usr/include/libkern/_OSByteOrder.h
dsuserd.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
dsuserd.o: /usr/include/sys/_types/_ct_rune_t.h
dsuserd.o: /usr/include/sys/_types/_rune_t.h /usr/include/sys/_types/_dev_t.h
dsuserd.o: /usr/include/sys/_types/_mode_t.h /usr/include/math.h dstypes.h
dsuserd.o: dsproto.h dsuhead.h
dssprtd.o: ../../../../.././include/ncarg/ngmath.h /usr/include/stdio.h
dssprtd.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
dssprtd.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
dssprtd.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
dssprtd.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
dssprtd.o: /usr/include/i386/_types.h
dssprtd.o: /usr/include/sys/_pthread/_pthread_types.h
dssprtd.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
dssprtd.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
dssprtd.o: /usr/include/sys/_types/_int16_t.h
dssprtd.o: /usr/include/sys/_types/_int32_t.h
dssprtd.o: /usr/include/sys/_types/_int64_t.h
dssprtd.o: /usr/include/sys/_types/_u_int8_t.h
dssprtd.o: /usr/include/sys/_types/_u_int16_t.h
dssprtd.o: /usr/include/sys/_types/_u_int32_t.h
dssprtd.o: /usr/include/sys/_types/_u_int64_t.h
dssprtd.o: /usr/include/sys/_types/_intptr_t.h
dssprtd.o: /usr/include/sys/_types/_uintptr_t.h
dssprtd.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
dssprtd.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
dssprtd.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
dssprtd.o: /usr/include/secure/_common.h /usr/include/stddef.h
dssprtd.o: /usr/include/sys/_types/_offsetof.h
dssprtd.o: /usr/include/sys/_types/_ptrdiff_t.h
dssprtd.o: /usr/include/sys/_types/_rsize_t.h
dssprtd.o: /usr/include/sys/_types/_wchar_t.h
dssprtd.o: /usr/include/sys/_types/_wint_t.h /usr/include/string.h
dssprtd.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
dssprtd.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
dssprtd.o: /usr/include/stdlib.h /usr/include/sys/wait.h
dssprtd.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_id_t.h
dssprtd.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
dssprtd.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
dssprtd.o: /usr/include/machine/_mcontext.h /usr/include/i386/_mcontext.h
dssprtd.o: /usr/include/mach/machine/_structs.h
dssprtd.o: /usr/include/mach/i386/_structs.h
dssprtd.o: /usr/include/sys/_pthread/_pthread_attr_t.h
dssprtd.o: /usr/include/sys/_types/_sigaltstack.h
dssprtd.o: /usr/include/sys/_types/_ucontext.h
dssprtd.o: /usr/include/sys/_types/_sigset_t.h
dssprtd.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
dssprtd.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
dssprtd.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
dssprtd.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
dssprtd.o: /usr/include/_types/_uintmax_t.h
dssprtd.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
dssprtd.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
dssprtd.o: /usr/include/libkern/_OSByteOrder.h
dssprtd.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
dssprtd.o: /usr/include/sys/_types/_ct_rune_t.h
dssprtd.o: /usr/include/sys/_types/_rune_t.h /usr/include/sys/_types/_dev_t.h
dssprtd.o: /usr/include/sys/_types/_mode_t.h /usr/include/math.h dstypes.h
dssprtd.o: dsproto.h dsgvars.h
dssprts.o: ../../../../.././include/ncarg/ngmath.h /usr/include/stdio.h
dssprts.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
dssprts.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
dssprts.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
dssprts.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
dssprts.o: /usr/include/i386/_types.h
dssprts.o: /usr/include/sys/_pthread/_pthread_types.h
dssprts.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
dssprts.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
dssprts.o: /usr/include/sys/_types/_int16_t.h
dssprts.o: /usr/include/sys/_types/_int32_t.h
dssprts.o: /usr/include/sys/_types/_int64_t.h
dssprts.o: /usr/include/sys/_types/_u_int8_t.h
dssprts.o: /usr/include/sys/_types/_u_int16_t.h
dssprts.o: /usr/include/sys/_types/_u_int32_t.h
dssprts.o: /usr/include/sys/_types/_u_int64_t.h
dssprts.o: /usr/include/sys/_types/_intptr_t.h
dssprts.o: /usr/include/sys/_types/_uintptr_t.h
dssprts.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
dssprts.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
dssprts.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
dssprts.o: /usr/include/secure/_common.h /usr/include/stddef.h
dssprts.o: /usr/include/sys/_types/_offsetof.h
dssprts.o: /usr/include/sys/_types/_ptrdiff_t.h
dssprts.o: /usr/include/sys/_types/_rsize_t.h
dssprts.o: /usr/include/sys/_types/_wchar_t.h
dssprts.o: /usr/include/sys/_types/_wint_t.h /usr/include/string.h
dssprts.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
dssprts.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
dssprts.o: /usr/include/stdlib.h /usr/include/sys/wait.h
dssprts.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_id_t.h
dssprts.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
dssprts.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
dssprts.o: /usr/include/machine/_mcontext.h /usr/include/i386/_mcontext.h
dssprts.o: /usr/include/mach/machine/_structs.h
dssprts.o: /usr/include/mach/i386/_structs.h
dssprts.o: /usr/include/sys/_pthread/_pthread_attr_t.h
dssprts.o: /usr/include/sys/_types/_sigaltstack.h
dssprts.o: /usr/include/sys/_types/_ucontext.h
dssprts.o: /usr/include/sys/_types/_sigset_t.h
dssprts.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
dssprts.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
dssprts.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
dssprts.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
dssprts.o: /usr/include/_types/_uintmax_t.h
dssprts.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
dssprts.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
dssprts.o: /usr/include/libkern/_OSByteOrder.h
dssprts.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
dssprts.o: /usr/include/sys/_types/_ct_rune_t.h
dssprts.o: /usr/include/sys/_types/_rune_t.h /usr/include/sys/_types/_dev_t.h
dssprts.o: /usr/include/sys/_types/_mode_t.h /usr/include/math.h dstypes.h
dssprts.o: dsproto.h dsuhead.h
dserror.o: ../../../../.././include/ncarg/ngmath.h /usr/include/stdio.h
dserror.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
dserror.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
dserror.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
dserror.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
dserror.o: /usr/include/i386/_types.h
dserror.o: /usr/include/sys/_pthread/_pthread_types.h
dserror.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
dserror.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
dserror.o: /usr/include/sys/_types/_int16_t.h
dserror.o: /usr/include/sys/_types/_int32_t.h
dserror.o: /usr/include/sys/_types/_int64_t.h
dserror.o: /usr/include/sys/_types/_u_int8_t.h
dserror.o: /usr/include/sys/_types/_u_int16_t.h
dserror.o: /usr/include/sys/_types/_u_int32_t.h
dserror.o: /usr/include/sys/_types/_u_int64_t.h
dserror.o: /usr/include/sys/_types/_intptr_t.h
dserror.o: /usr/include/sys/_types/_uintptr_t.h
dserror.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
dserror.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
dserror.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
dserror.o: /usr/include/secure/_common.h /usr/include/stddef.h
dserror.o: /usr/include/sys/_types/_offsetof.h
dserror.o: /usr/include/sys/_types/_ptrdiff_t.h
dserror.o: /usr/include/sys/_types/_rsize_t.h
dserror.o: /usr/include/sys/_types/_wchar_t.h
dserror.o: /usr/include/sys/_types/_wint_t.h /usr/include/string.h
dserror.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
dserror.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
dserror.o: /usr/include/stdlib.h /usr/include/sys/wait.h
dserror.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_id_t.h
dserror.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
dserror.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
dserror.o: /usr/include/machine/_mcontext.h /usr/include/i386/_mcontext.h
dserror.o: /usr/include/mach/machine/_structs.h
dserror.o: /usr/include/mach/i386/_structs.h
dserror.o: /usr/include/sys/_pthread/_pthread_attr_t.h
dserror.o: /usr/include/sys/_types/_sigaltstack.h
dserror.o: /usr/include/sys/_types/_ucontext.h
dserror.o: /usr/include/sys/_types/_sigset_t.h
dserror.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
dserror.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
dserror.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
dserror.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
dserror.o: /usr/include/_types/_uintmax_t.h
dserror.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
dserror.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
dserror.o: /usr/include/libkern/_OSByteOrder.h
dserror.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
dserror.o: /usr/include/sys/_types/_ct_rune_t.h
dserror.o: /usr/include/sys/_types/_rune_t.h /usr/include/sys/_types/_dev_t.h
dserror.o: /usr/include/sys/_types/_mode_t.h /usr/include/math.h dstypes.h
dserror.o: dsproto.h
