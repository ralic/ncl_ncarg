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
CURRENT_DIR		= ./ncarg2d/src/libncarg/supportC

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
#	$Id: yMakefile,v 1.2 1997-04-11 17:58:55 haley Exp $
#

MYNAME	= supportC

SRCS = c_encd.c c_entsr.c c_nerro.c c_eprin.c c_errof.c c_msbsf1.c  	c_msbsf2.c c_msceez.c c_mskrv1.c c_mskrv2.c c_msntvl.c c_msshch.c  	c_mssrf1.c c_mssrf2.c c_mstrms.c c_q8qst4.c c_retsr.c c_semess.c  	c_seter.c c_icloem.c c_icfell.c c_idbvip.c c_idsfft.c

OBJS = c_encd.o c_entsr.o c_nerro.o c_eprin.o c_errof.o c_msbsf1.o  	c_msbsf2.o c_msceez.o c_mskrv1.o c_mskrv2.o c_msntvl.o c_msshch.o  	c_mssrf1.o c_mssrf2.o c_mstrms.o c_q8qst4.o c_retsr.o c_semess.o  	c_seter.o c_icloem.o c_icfell.o c_idbvip.o c_idsfft.o

EXTRA_CDEFINES = $(OS_DEF)

all::

all-local::../libncarg.a

../libncarg.a:: $(OBJS)
	$(AR_REG) ../libncarg.a $(OBJS)

all::

clean-local::
	-@$(RM) $(OBJS)

all::

all-local::../libNGlluC.a

../libNGlluC.a:: $(OBJS)
	$(AR_REG) ../libNGlluC.a $(OBJS)

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

c_encd.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_encd.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_encd.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_encd.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_encd.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_encd.o: /usr/include/i386/_types.h
c_encd.o: /usr/include/sys/_pthread/_pthread_types.h
c_encd.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_encd.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_encd.o: /usr/include/sys/_types/_int16_t.h
c_encd.o: /usr/include/sys/_types/_int32_t.h
c_encd.o: /usr/include/sys/_types/_int64_t.h
c_encd.o: /usr/include/sys/_types/_u_int8_t.h
c_encd.o: /usr/include/sys/_types/_u_int16_t.h
c_encd.o: /usr/include/sys/_types/_u_int32_t.h
c_encd.o: /usr/include/sys/_types/_u_int64_t.h
c_encd.o: /usr/include/sys/_types/_intptr_t.h
c_encd.o: /usr/include/sys/_types/_uintptr_t.h
c_encd.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_encd.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_encd.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_encd.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_encd.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_encd.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_encd.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_encd.o: /usr/include/libkern/i386/_OSByteOrder.h
c_encd.o: /usr/include/sys/_types/_u_char.h
c_encd.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
c_encd.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
c_encd.o: /usr/include/sys/_types/_blkcnt_t.h
c_encd.o: /usr/include/sys/_types/_blksize_t.h
c_encd.o: /usr/include/sys/_types/_gid_t.h
c_encd.o: /usr/include/sys/_types/_in_addr_t.h
c_encd.o: /usr/include/sys/_types/_in_port_t.h
c_encd.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
c_encd.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_encd.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_encd.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_encd.o: /usr/include/sys/_types/_clock_t.h
c_encd.o: /usr/include/sys/_types/_time_t.h
c_encd.o: /usr/include/sys/_types/_useconds_t.h
c_encd.o: /usr/include/sys/_types/_suseconds_t.h
c_encd.o: /usr/include/sys/_types/_rsize_t.h
c_encd.o: /usr/include/sys/_types/_errno_t.h
c_encd.o: /usr/include/sys/_types/_fd_def.h
c_encd.o: /usr/include/sys/_types/_fd_setsize.h
c_encd.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
c_encd.o: /usr/include/sys/_types/_fd_zero.h
c_encd.o: /usr/include/sys/_types/_fd_isset.h
c_encd.o: /usr/include/sys/_types/_fd_copy.h
c_encd.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_encd.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_encd.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_encd.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_encd.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_encd.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_encd.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_encd.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_encd.o: /usr/include/sys/_pthread/_pthread_t.h
c_encd.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_encd.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_encd.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_encd.o: ../../../.././include/ncarg/c.h
c_entsr.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_entsr.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_entsr.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_entsr.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_entsr.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_entsr.o: /usr/include/i386/_types.h
c_entsr.o: /usr/include/sys/_pthread/_pthread_types.h
c_entsr.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_entsr.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_entsr.o: /usr/include/sys/_types/_int16_t.h
c_entsr.o: /usr/include/sys/_types/_int32_t.h
c_entsr.o: /usr/include/sys/_types/_int64_t.h
c_entsr.o: /usr/include/sys/_types/_u_int8_t.h
c_entsr.o: /usr/include/sys/_types/_u_int16_t.h
c_entsr.o: /usr/include/sys/_types/_u_int32_t.h
c_entsr.o: /usr/include/sys/_types/_u_int64_t.h
c_entsr.o: /usr/include/sys/_types/_intptr_t.h
c_entsr.o: /usr/include/sys/_types/_uintptr_t.h
c_entsr.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_entsr.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_entsr.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_entsr.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_entsr.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_entsr.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_entsr.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_entsr.o: /usr/include/libkern/i386/_OSByteOrder.h
c_entsr.o: /usr/include/sys/_types/_u_char.h
c_entsr.o: /usr/include/sys/_types/_u_short.h
c_entsr.o: /usr/include/sys/_types/_u_int.h
c_entsr.o: /usr/include/sys/_types/_caddr_t.h
c_entsr.o: /usr/include/sys/_types/_dev_t.h
c_entsr.o: /usr/include/sys/_types/_blkcnt_t.h
c_entsr.o: /usr/include/sys/_types/_blksize_t.h
c_entsr.o: /usr/include/sys/_types/_gid_t.h
c_entsr.o: /usr/include/sys/_types/_in_addr_t.h
c_entsr.o: /usr/include/sys/_types/_in_port_t.h
c_entsr.o: /usr/include/sys/_types/_ino_t.h
c_entsr.o: /usr/include/sys/_types/_ino64_t.h
c_entsr.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_entsr.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_entsr.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_entsr.o: /usr/include/sys/_types/_clock_t.h
c_entsr.o: /usr/include/sys/_types/_time_t.h
c_entsr.o: /usr/include/sys/_types/_useconds_t.h
c_entsr.o: /usr/include/sys/_types/_suseconds_t.h
c_entsr.o: /usr/include/sys/_types/_rsize_t.h
c_entsr.o: /usr/include/sys/_types/_errno_t.h
c_entsr.o: /usr/include/sys/_types/_fd_def.h
c_entsr.o: /usr/include/sys/_types/_fd_setsize.h
c_entsr.o: /usr/include/sys/_types/_fd_set.h
c_entsr.o: /usr/include/sys/_types/_fd_clr.h
c_entsr.o: /usr/include/sys/_types/_fd_zero.h
c_entsr.o: /usr/include/sys/_types/_fd_isset.h
c_entsr.o: /usr/include/sys/_types/_fd_copy.h
c_entsr.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_entsr.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_entsr.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_entsr.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_entsr.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_entsr.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_entsr.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_entsr.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_entsr.o: /usr/include/sys/_pthread/_pthread_t.h
c_entsr.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_entsr.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_entsr.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_entsr.o: ../../../.././include/ncarg/c.h
c_nerro.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_nerro.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_nerro.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_nerro.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_nerro.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_nerro.o: /usr/include/i386/_types.h
c_nerro.o: /usr/include/sys/_pthread/_pthread_types.h
c_nerro.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_nerro.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_nerro.o: /usr/include/sys/_types/_int16_t.h
c_nerro.o: /usr/include/sys/_types/_int32_t.h
c_nerro.o: /usr/include/sys/_types/_int64_t.h
c_nerro.o: /usr/include/sys/_types/_u_int8_t.h
c_nerro.o: /usr/include/sys/_types/_u_int16_t.h
c_nerro.o: /usr/include/sys/_types/_u_int32_t.h
c_nerro.o: /usr/include/sys/_types/_u_int64_t.h
c_nerro.o: /usr/include/sys/_types/_intptr_t.h
c_nerro.o: /usr/include/sys/_types/_uintptr_t.h
c_nerro.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_nerro.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_nerro.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_nerro.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_nerro.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_nerro.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_nerro.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_nerro.o: /usr/include/libkern/i386/_OSByteOrder.h
c_nerro.o: /usr/include/sys/_types/_u_char.h
c_nerro.o: /usr/include/sys/_types/_u_short.h
c_nerro.o: /usr/include/sys/_types/_u_int.h
c_nerro.o: /usr/include/sys/_types/_caddr_t.h
c_nerro.o: /usr/include/sys/_types/_dev_t.h
c_nerro.o: /usr/include/sys/_types/_blkcnt_t.h
c_nerro.o: /usr/include/sys/_types/_blksize_t.h
c_nerro.o: /usr/include/sys/_types/_gid_t.h
c_nerro.o: /usr/include/sys/_types/_in_addr_t.h
c_nerro.o: /usr/include/sys/_types/_in_port_t.h
c_nerro.o: /usr/include/sys/_types/_ino_t.h
c_nerro.o: /usr/include/sys/_types/_ino64_t.h
c_nerro.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_nerro.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_nerro.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_nerro.o: /usr/include/sys/_types/_clock_t.h
c_nerro.o: /usr/include/sys/_types/_time_t.h
c_nerro.o: /usr/include/sys/_types/_useconds_t.h
c_nerro.o: /usr/include/sys/_types/_suseconds_t.h
c_nerro.o: /usr/include/sys/_types/_rsize_t.h
c_nerro.o: /usr/include/sys/_types/_errno_t.h
c_nerro.o: /usr/include/sys/_types/_fd_def.h
c_nerro.o: /usr/include/sys/_types/_fd_setsize.h
c_nerro.o: /usr/include/sys/_types/_fd_set.h
c_nerro.o: /usr/include/sys/_types/_fd_clr.h
c_nerro.o: /usr/include/sys/_types/_fd_zero.h
c_nerro.o: /usr/include/sys/_types/_fd_isset.h
c_nerro.o: /usr/include/sys/_types/_fd_copy.h
c_nerro.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_nerro.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_nerro.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_nerro.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_nerro.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_nerro.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_nerro.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_nerro.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_nerro.o: /usr/include/sys/_pthread/_pthread_t.h
c_nerro.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_nerro.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_nerro.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_nerro.o: ../../../.././include/ncarg/c.h
c_eprin.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_eprin.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_eprin.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_eprin.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_eprin.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_eprin.o: /usr/include/i386/_types.h
c_eprin.o: /usr/include/sys/_pthread/_pthread_types.h
c_eprin.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_eprin.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_eprin.o: /usr/include/sys/_types/_int16_t.h
c_eprin.o: /usr/include/sys/_types/_int32_t.h
c_eprin.o: /usr/include/sys/_types/_int64_t.h
c_eprin.o: /usr/include/sys/_types/_u_int8_t.h
c_eprin.o: /usr/include/sys/_types/_u_int16_t.h
c_eprin.o: /usr/include/sys/_types/_u_int32_t.h
c_eprin.o: /usr/include/sys/_types/_u_int64_t.h
c_eprin.o: /usr/include/sys/_types/_intptr_t.h
c_eprin.o: /usr/include/sys/_types/_uintptr_t.h
c_eprin.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_eprin.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_eprin.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_eprin.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_eprin.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_eprin.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_eprin.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_eprin.o: /usr/include/libkern/i386/_OSByteOrder.h
c_eprin.o: /usr/include/sys/_types/_u_char.h
c_eprin.o: /usr/include/sys/_types/_u_short.h
c_eprin.o: /usr/include/sys/_types/_u_int.h
c_eprin.o: /usr/include/sys/_types/_caddr_t.h
c_eprin.o: /usr/include/sys/_types/_dev_t.h
c_eprin.o: /usr/include/sys/_types/_blkcnt_t.h
c_eprin.o: /usr/include/sys/_types/_blksize_t.h
c_eprin.o: /usr/include/sys/_types/_gid_t.h
c_eprin.o: /usr/include/sys/_types/_in_addr_t.h
c_eprin.o: /usr/include/sys/_types/_in_port_t.h
c_eprin.o: /usr/include/sys/_types/_ino_t.h
c_eprin.o: /usr/include/sys/_types/_ino64_t.h
c_eprin.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_eprin.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_eprin.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_eprin.o: /usr/include/sys/_types/_clock_t.h
c_eprin.o: /usr/include/sys/_types/_time_t.h
c_eprin.o: /usr/include/sys/_types/_useconds_t.h
c_eprin.o: /usr/include/sys/_types/_suseconds_t.h
c_eprin.o: /usr/include/sys/_types/_rsize_t.h
c_eprin.o: /usr/include/sys/_types/_errno_t.h
c_eprin.o: /usr/include/sys/_types/_fd_def.h
c_eprin.o: /usr/include/sys/_types/_fd_setsize.h
c_eprin.o: /usr/include/sys/_types/_fd_set.h
c_eprin.o: /usr/include/sys/_types/_fd_clr.h
c_eprin.o: /usr/include/sys/_types/_fd_zero.h
c_eprin.o: /usr/include/sys/_types/_fd_isset.h
c_eprin.o: /usr/include/sys/_types/_fd_copy.h
c_eprin.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_eprin.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_eprin.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_eprin.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_eprin.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_eprin.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_eprin.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_eprin.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_eprin.o: /usr/include/sys/_pthread/_pthread_t.h
c_eprin.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_eprin.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_eprin.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_eprin.o: ../../../.././include/ncarg/c.h
c_errof.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_errof.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_errof.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_errof.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_errof.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_errof.o: /usr/include/i386/_types.h
c_errof.o: /usr/include/sys/_pthread/_pthread_types.h
c_errof.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_errof.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_errof.o: /usr/include/sys/_types/_int16_t.h
c_errof.o: /usr/include/sys/_types/_int32_t.h
c_errof.o: /usr/include/sys/_types/_int64_t.h
c_errof.o: /usr/include/sys/_types/_u_int8_t.h
c_errof.o: /usr/include/sys/_types/_u_int16_t.h
c_errof.o: /usr/include/sys/_types/_u_int32_t.h
c_errof.o: /usr/include/sys/_types/_u_int64_t.h
c_errof.o: /usr/include/sys/_types/_intptr_t.h
c_errof.o: /usr/include/sys/_types/_uintptr_t.h
c_errof.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_errof.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_errof.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_errof.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_errof.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_errof.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_errof.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_errof.o: /usr/include/libkern/i386/_OSByteOrder.h
c_errof.o: /usr/include/sys/_types/_u_char.h
c_errof.o: /usr/include/sys/_types/_u_short.h
c_errof.o: /usr/include/sys/_types/_u_int.h
c_errof.o: /usr/include/sys/_types/_caddr_t.h
c_errof.o: /usr/include/sys/_types/_dev_t.h
c_errof.o: /usr/include/sys/_types/_blkcnt_t.h
c_errof.o: /usr/include/sys/_types/_blksize_t.h
c_errof.o: /usr/include/sys/_types/_gid_t.h
c_errof.o: /usr/include/sys/_types/_in_addr_t.h
c_errof.o: /usr/include/sys/_types/_in_port_t.h
c_errof.o: /usr/include/sys/_types/_ino_t.h
c_errof.o: /usr/include/sys/_types/_ino64_t.h
c_errof.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_errof.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_errof.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_errof.o: /usr/include/sys/_types/_clock_t.h
c_errof.o: /usr/include/sys/_types/_time_t.h
c_errof.o: /usr/include/sys/_types/_useconds_t.h
c_errof.o: /usr/include/sys/_types/_suseconds_t.h
c_errof.o: /usr/include/sys/_types/_rsize_t.h
c_errof.o: /usr/include/sys/_types/_errno_t.h
c_errof.o: /usr/include/sys/_types/_fd_def.h
c_errof.o: /usr/include/sys/_types/_fd_setsize.h
c_errof.o: /usr/include/sys/_types/_fd_set.h
c_errof.o: /usr/include/sys/_types/_fd_clr.h
c_errof.o: /usr/include/sys/_types/_fd_zero.h
c_errof.o: /usr/include/sys/_types/_fd_isset.h
c_errof.o: /usr/include/sys/_types/_fd_copy.h
c_errof.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_errof.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_errof.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_errof.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_errof.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_errof.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_errof.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_errof.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_errof.o: /usr/include/sys/_pthread/_pthread_t.h
c_errof.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_errof.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_errof.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_errof.o: ../../../.././include/ncarg/c.h
c_msbsf1.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_msbsf1.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_msbsf1.o: /usr/include/sys/_posix_availability.h
c_msbsf1.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_msbsf1.o: /usr/include/_types.h /usr/include/sys/_types.h
c_msbsf1.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_msbsf1.o: /usr/include/sys/_pthread/_pthread_types.h
c_msbsf1.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_msbsf1.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_msbsf1.o: /usr/include/sys/_types/_int16_t.h
c_msbsf1.o: /usr/include/sys/_types/_int32_t.h
c_msbsf1.o: /usr/include/sys/_types/_int64_t.h
c_msbsf1.o: /usr/include/sys/_types/_u_int8_t.h
c_msbsf1.o: /usr/include/sys/_types/_u_int16_t.h
c_msbsf1.o: /usr/include/sys/_types/_u_int32_t.h
c_msbsf1.o: /usr/include/sys/_types/_u_int64_t.h
c_msbsf1.o: /usr/include/sys/_types/_intptr_t.h
c_msbsf1.o: /usr/include/sys/_types/_uintptr_t.h
c_msbsf1.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_msbsf1.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_msbsf1.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_msbsf1.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_msbsf1.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_msbsf1.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_msbsf1.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_msbsf1.o: /usr/include/libkern/i386/_OSByteOrder.h
c_msbsf1.o: /usr/include/sys/_types/_u_char.h
c_msbsf1.o: /usr/include/sys/_types/_u_short.h
c_msbsf1.o: /usr/include/sys/_types/_u_int.h
c_msbsf1.o: /usr/include/sys/_types/_caddr_t.h
c_msbsf1.o: /usr/include/sys/_types/_dev_t.h
c_msbsf1.o: /usr/include/sys/_types/_blkcnt_t.h
c_msbsf1.o: /usr/include/sys/_types/_blksize_t.h
c_msbsf1.o: /usr/include/sys/_types/_gid_t.h
c_msbsf1.o: /usr/include/sys/_types/_in_addr_t.h
c_msbsf1.o: /usr/include/sys/_types/_in_port_t.h
c_msbsf1.o: /usr/include/sys/_types/_ino_t.h
c_msbsf1.o: /usr/include/sys/_types/_ino64_t.h
c_msbsf1.o: /usr/include/sys/_types/_key_t.h
c_msbsf1.o: /usr/include/sys/_types/_mode_t.h
c_msbsf1.o: /usr/include/sys/_types/_nlink_t.h
c_msbsf1.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_msbsf1.o: /usr/include/sys/_types/_uid_t.h
c_msbsf1.o: /usr/include/sys/_types/_clock_t.h
c_msbsf1.o: /usr/include/sys/_types/_time_t.h
c_msbsf1.o: /usr/include/sys/_types/_useconds_t.h
c_msbsf1.o: /usr/include/sys/_types/_suseconds_t.h
c_msbsf1.o: /usr/include/sys/_types/_rsize_t.h
c_msbsf1.o: /usr/include/sys/_types/_errno_t.h
c_msbsf1.o: /usr/include/sys/_types/_fd_def.h
c_msbsf1.o: /usr/include/sys/_types/_fd_setsize.h
c_msbsf1.o: /usr/include/sys/_types/_fd_set.h
c_msbsf1.o: /usr/include/sys/_types/_fd_clr.h
c_msbsf1.o: /usr/include/sys/_types/_fd_zero.h
c_msbsf1.o: /usr/include/sys/_types/_fd_isset.h
c_msbsf1.o: /usr/include/sys/_types/_fd_copy.h
c_msbsf1.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_msbsf1.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_msbsf1.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_msbsf1.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_msbsf1.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_msbsf1.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_msbsf1.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_msbsf1.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_msbsf1.o: /usr/include/sys/_pthread/_pthread_t.h
c_msbsf1.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_msbsf1.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_msbsf1.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_msbsf1.o: ../../../.././include/ncarg/c.h
c_msbsf2.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_msbsf2.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_msbsf2.o: /usr/include/sys/_posix_availability.h
c_msbsf2.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_msbsf2.o: /usr/include/_types.h /usr/include/sys/_types.h
c_msbsf2.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_msbsf2.o: /usr/include/sys/_pthread/_pthread_types.h
c_msbsf2.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_msbsf2.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_msbsf2.o: /usr/include/sys/_types/_int16_t.h
c_msbsf2.o: /usr/include/sys/_types/_int32_t.h
c_msbsf2.o: /usr/include/sys/_types/_int64_t.h
c_msbsf2.o: /usr/include/sys/_types/_u_int8_t.h
c_msbsf2.o: /usr/include/sys/_types/_u_int16_t.h
c_msbsf2.o: /usr/include/sys/_types/_u_int32_t.h
c_msbsf2.o: /usr/include/sys/_types/_u_int64_t.h
c_msbsf2.o: /usr/include/sys/_types/_intptr_t.h
c_msbsf2.o: /usr/include/sys/_types/_uintptr_t.h
c_msbsf2.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_msbsf2.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_msbsf2.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_msbsf2.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_msbsf2.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_msbsf2.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_msbsf2.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_msbsf2.o: /usr/include/libkern/i386/_OSByteOrder.h
c_msbsf2.o: /usr/include/sys/_types/_u_char.h
c_msbsf2.o: /usr/include/sys/_types/_u_short.h
c_msbsf2.o: /usr/include/sys/_types/_u_int.h
c_msbsf2.o: /usr/include/sys/_types/_caddr_t.h
c_msbsf2.o: /usr/include/sys/_types/_dev_t.h
c_msbsf2.o: /usr/include/sys/_types/_blkcnt_t.h
c_msbsf2.o: /usr/include/sys/_types/_blksize_t.h
c_msbsf2.o: /usr/include/sys/_types/_gid_t.h
c_msbsf2.o: /usr/include/sys/_types/_in_addr_t.h
c_msbsf2.o: /usr/include/sys/_types/_in_port_t.h
c_msbsf2.o: /usr/include/sys/_types/_ino_t.h
c_msbsf2.o: /usr/include/sys/_types/_ino64_t.h
c_msbsf2.o: /usr/include/sys/_types/_key_t.h
c_msbsf2.o: /usr/include/sys/_types/_mode_t.h
c_msbsf2.o: /usr/include/sys/_types/_nlink_t.h
c_msbsf2.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_msbsf2.o: /usr/include/sys/_types/_uid_t.h
c_msbsf2.o: /usr/include/sys/_types/_clock_t.h
c_msbsf2.o: /usr/include/sys/_types/_time_t.h
c_msbsf2.o: /usr/include/sys/_types/_useconds_t.h
c_msbsf2.o: /usr/include/sys/_types/_suseconds_t.h
c_msbsf2.o: /usr/include/sys/_types/_rsize_t.h
c_msbsf2.o: /usr/include/sys/_types/_errno_t.h
c_msbsf2.o: /usr/include/sys/_types/_fd_def.h
c_msbsf2.o: /usr/include/sys/_types/_fd_setsize.h
c_msbsf2.o: /usr/include/sys/_types/_fd_set.h
c_msbsf2.o: /usr/include/sys/_types/_fd_clr.h
c_msbsf2.o: /usr/include/sys/_types/_fd_zero.h
c_msbsf2.o: /usr/include/sys/_types/_fd_isset.h
c_msbsf2.o: /usr/include/sys/_types/_fd_copy.h
c_msbsf2.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_msbsf2.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_msbsf2.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_msbsf2.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_msbsf2.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_msbsf2.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_msbsf2.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_msbsf2.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_msbsf2.o: /usr/include/sys/_pthread/_pthread_t.h
c_msbsf2.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_msbsf2.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_msbsf2.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_msbsf2.o: ../../../.././include/ncarg/c.h
c_msceez.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_msceez.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_msceez.o: /usr/include/sys/_posix_availability.h
c_msceez.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_msceez.o: /usr/include/_types.h /usr/include/sys/_types.h
c_msceez.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_msceez.o: /usr/include/sys/_pthread/_pthread_types.h
c_msceez.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_msceez.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_msceez.o: /usr/include/sys/_types/_int16_t.h
c_msceez.o: /usr/include/sys/_types/_int32_t.h
c_msceez.o: /usr/include/sys/_types/_int64_t.h
c_msceez.o: /usr/include/sys/_types/_u_int8_t.h
c_msceez.o: /usr/include/sys/_types/_u_int16_t.h
c_msceez.o: /usr/include/sys/_types/_u_int32_t.h
c_msceez.o: /usr/include/sys/_types/_u_int64_t.h
c_msceez.o: /usr/include/sys/_types/_intptr_t.h
c_msceez.o: /usr/include/sys/_types/_uintptr_t.h
c_msceez.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_msceez.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_msceez.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_msceez.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_msceez.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_msceez.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_msceez.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_msceez.o: /usr/include/libkern/i386/_OSByteOrder.h
c_msceez.o: /usr/include/sys/_types/_u_char.h
c_msceez.o: /usr/include/sys/_types/_u_short.h
c_msceez.o: /usr/include/sys/_types/_u_int.h
c_msceez.o: /usr/include/sys/_types/_caddr_t.h
c_msceez.o: /usr/include/sys/_types/_dev_t.h
c_msceez.o: /usr/include/sys/_types/_blkcnt_t.h
c_msceez.o: /usr/include/sys/_types/_blksize_t.h
c_msceez.o: /usr/include/sys/_types/_gid_t.h
c_msceez.o: /usr/include/sys/_types/_in_addr_t.h
c_msceez.o: /usr/include/sys/_types/_in_port_t.h
c_msceez.o: /usr/include/sys/_types/_ino_t.h
c_msceez.o: /usr/include/sys/_types/_ino64_t.h
c_msceez.o: /usr/include/sys/_types/_key_t.h
c_msceez.o: /usr/include/sys/_types/_mode_t.h
c_msceez.o: /usr/include/sys/_types/_nlink_t.h
c_msceez.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_msceez.o: /usr/include/sys/_types/_uid_t.h
c_msceez.o: /usr/include/sys/_types/_clock_t.h
c_msceez.o: /usr/include/sys/_types/_time_t.h
c_msceez.o: /usr/include/sys/_types/_useconds_t.h
c_msceez.o: /usr/include/sys/_types/_suseconds_t.h
c_msceez.o: /usr/include/sys/_types/_rsize_t.h
c_msceez.o: /usr/include/sys/_types/_errno_t.h
c_msceez.o: /usr/include/sys/_types/_fd_def.h
c_msceez.o: /usr/include/sys/_types/_fd_setsize.h
c_msceez.o: /usr/include/sys/_types/_fd_set.h
c_msceez.o: /usr/include/sys/_types/_fd_clr.h
c_msceez.o: /usr/include/sys/_types/_fd_zero.h
c_msceez.o: /usr/include/sys/_types/_fd_isset.h
c_msceez.o: /usr/include/sys/_types/_fd_copy.h
c_msceez.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_msceez.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_msceez.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_msceez.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_msceez.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_msceez.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_msceez.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_msceez.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_msceez.o: /usr/include/sys/_pthread/_pthread_t.h
c_msceez.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_msceez.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_msceez.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_msceez.o: ../../../.././include/ncarg/c.h
c_mskrv1.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mskrv1.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mskrv1.o: /usr/include/sys/_posix_availability.h
c_mskrv1.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mskrv1.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mskrv1.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mskrv1.o: /usr/include/sys/_pthread/_pthread_types.h
c_mskrv1.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mskrv1.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mskrv1.o: /usr/include/sys/_types/_int16_t.h
c_mskrv1.o: /usr/include/sys/_types/_int32_t.h
c_mskrv1.o: /usr/include/sys/_types/_int64_t.h
c_mskrv1.o: /usr/include/sys/_types/_u_int8_t.h
c_mskrv1.o: /usr/include/sys/_types/_u_int16_t.h
c_mskrv1.o: /usr/include/sys/_types/_u_int32_t.h
c_mskrv1.o: /usr/include/sys/_types/_u_int64_t.h
c_mskrv1.o: /usr/include/sys/_types/_intptr_t.h
c_mskrv1.o: /usr/include/sys/_types/_uintptr_t.h
c_mskrv1.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mskrv1.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mskrv1.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mskrv1.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mskrv1.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mskrv1.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mskrv1.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mskrv1.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mskrv1.o: /usr/include/sys/_types/_u_char.h
c_mskrv1.o: /usr/include/sys/_types/_u_short.h
c_mskrv1.o: /usr/include/sys/_types/_u_int.h
c_mskrv1.o: /usr/include/sys/_types/_caddr_t.h
c_mskrv1.o: /usr/include/sys/_types/_dev_t.h
c_mskrv1.o: /usr/include/sys/_types/_blkcnt_t.h
c_mskrv1.o: /usr/include/sys/_types/_blksize_t.h
c_mskrv1.o: /usr/include/sys/_types/_gid_t.h
c_mskrv1.o: /usr/include/sys/_types/_in_addr_t.h
c_mskrv1.o: /usr/include/sys/_types/_in_port_t.h
c_mskrv1.o: /usr/include/sys/_types/_ino_t.h
c_mskrv1.o: /usr/include/sys/_types/_ino64_t.h
c_mskrv1.o: /usr/include/sys/_types/_key_t.h
c_mskrv1.o: /usr/include/sys/_types/_mode_t.h
c_mskrv1.o: /usr/include/sys/_types/_nlink_t.h
c_mskrv1.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mskrv1.o: /usr/include/sys/_types/_uid_t.h
c_mskrv1.o: /usr/include/sys/_types/_clock_t.h
c_mskrv1.o: /usr/include/sys/_types/_time_t.h
c_mskrv1.o: /usr/include/sys/_types/_useconds_t.h
c_mskrv1.o: /usr/include/sys/_types/_suseconds_t.h
c_mskrv1.o: /usr/include/sys/_types/_rsize_t.h
c_mskrv1.o: /usr/include/sys/_types/_errno_t.h
c_mskrv1.o: /usr/include/sys/_types/_fd_def.h
c_mskrv1.o: /usr/include/sys/_types/_fd_setsize.h
c_mskrv1.o: /usr/include/sys/_types/_fd_set.h
c_mskrv1.o: /usr/include/sys/_types/_fd_clr.h
c_mskrv1.o: /usr/include/sys/_types/_fd_zero.h
c_mskrv1.o: /usr/include/sys/_types/_fd_isset.h
c_mskrv1.o: /usr/include/sys/_types/_fd_copy.h
c_mskrv1.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mskrv1.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mskrv1.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mskrv1.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mskrv1.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mskrv1.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mskrv1.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mskrv1.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mskrv1.o: /usr/include/sys/_pthread/_pthread_t.h
c_mskrv1.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mskrv1.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mskrv1.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mskrv1.o: ../../../.././include/ncarg/c.h
c_mskrv2.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mskrv2.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mskrv2.o: /usr/include/sys/_posix_availability.h
c_mskrv2.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mskrv2.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mskrv2.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mskrv2.o: /usr/include/sys/_pthread/_pthread_types.h
c_mskrv2.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mskrv2.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mskrv2.o: /usr/include/sys/_types/_int16_t.h
c_mskrv2.o: /usr/include/sys/_types/_int32_t.h
c_mskrv2.o: /usr/include/sys/_types/_int64_t.h
c_mskrv2.o: /usr/include/sys/_types/_u_int8_t.h
c_mskrv2.o: /usr/include/sys/_types/_u_int16_t.h
c_mskrv2.o: /usr/include/sys/_types/_u_int32_t.h
c_mskrv2.o: /usr/include/sys/_types/_u_int64_t.h
c_mskrv2.o: /usr/include/sys/_types/_intptr_t.h
c_mskrv2.o: /usr/include/sys/_types/_uintptr_t.h
c_mskrv2.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mskrv2.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mskrv2.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mskrv2.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mskrv2.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mskrv2.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mskrv2.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mskrv2.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mskrv2.o: /usr/include/sys/_types/_u_char.h
c_mskrv2.o: /usr/include/sys/_types/_u_short.h
c_mskrv2.o: /usr/include/sys/_types/_u_int.h
c_mskrv2.o: /usr/include/sys/_types/_caddr_t.h
c_mskrv2.o: /usr/include/sys/_types/_dev_t.h
c_mskrv2.o: /usr/include/sys/_types/_blkcnt_t.h
c_mskrv2.o: /usr/include/sys/_types/_blksize_t.h
c_mskrv2.o: /usr/include/sys/_types/_gid_t.h
c_mskrv2.o: /usr/include/sys/_types/_in_addr_t.h
c_mskrv2.o: /usr/include/sys/_types/_in_port_t.h
c_mskrv2.o: /usr/include/sys/_types/_ino_t.h
c_mskrv2.o: /usr/include/sys/_types/_ino64_t.h
c_mskrv2.o: /usr/include/sys/_types/_key_t.h
c_mskrv2.o: /usr/include/sys/_types/_mode_t.h
c_mskrv2.o: /usr/include/sys/_types/_nlink_t.h
c_mskrv2.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mskrv2.o: /usr/include/sys/_types/_uid_t.h
c_mskrv2.o: /usr/include/sys/_types/_clock_t.h
c_mskrv2.o: /usr/include/sys/_types/_time_t.h
c_mskrv2.o: /usr/include/sys/_types/_useconds_t.h
c_mskrv2.o: /usr/include/sys/_types/_suseconds_t.h
c_mskrv2.o: /usr/include/sys/_types/_rsize_t.h
c_mskrv2.o: /usr/include/sys/_types/_errno_t.h
c_mskrv2.o: /usr/include/sys/_types/_fd_def.h
c_mskrv2.o: /usr/include/sys/_types/_fd_setsize.h
c_mskrv2.o: /usr/include/sys/_types/_fd_set.h
c_mskrv2.o: /usr/include/sys/_types/_fd_clr.h
c_mskrv2.o: /usr/include/sys/_types/_fd_zero.h
c_mskrv2.o: /usr/include/sys/_types/_fd_isset.h
c_mskrv2.o: /usr/include/sys/_types/_fd_copy.h
c_mskrv2.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mskrv2.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mskrv2.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mskrv2.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mskrv2.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mskrv2.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mskrv2.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mskrv2.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mskrv2.o: /usr/include/sys/_pthread/_pthread_t.h
c_mskrv2.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mskrv2.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mskrv2.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mskrv2.o: ../../../.././include/ncarg/c.h
c_msntvl.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_msntvl.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_msntvl.o: /usr/include/sys/_posix_availability.h
c_msntvl.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_msntvl.o: /usr/include/_types.h /usr/include/sys/_types.h
c_msntvl.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_msntvl.o: /usr/include/sys/_pthread/_pthread_types.h
c_msntvl.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_msntvl.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_msntvl.o: /usr/include/sys/_types/_int16_t.h
c_msntvl.o: /usr/include/sys/_types/_int32_t.h
c_msntvl.o: /usr/include/sys/_types/_int64_t.h
c_msntvl.o: /usr/include/sys/_types/_u_int8_t.h
c_msntvl.o: /usr/include/sys/_types/_u_int16_t.h
c_msntvl.o: /usr/include/sys/_types/_u_int32_t.h
c_msntvl.o: /usr/include/sys/_types/_u_int64_t.h
c_msntvl.o: /usr/include/sys/_types/_intptr_t.h
c_msntvl.o: /usr/include/sys/_types/_uintptr_t.h
c_msntvl.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_msntvl.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_msntvl.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_msntvl.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_msntvl.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_msntvl.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_msntvl.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_msntvl.o: /usr/include/libkern/i386/_OSByteOrder.h
c_msntvl.o: /usr/include/sys/_types/_u_char.h
c_msntvl.o: /usr/include/sys/_types/_u_short.h
c_msntvl.o: /usr/include/sys/_types/_u_int.h
c_msntvl.o: /usr/include/sys/_types/_caddr_t.h
c_msntvl.o: /usr/include/sys/_types/_dev_t.h
c_msntvl.o: /usr/include/sys/_types/_blkcnt_t.h
c_msntvl.o: /usr/include/sys/_types/_blksize_t.h
c_msntvl.o: /usr/include/sys/_types/_gid_t.h
c_msntvl.o: /usr/include/sys/_types/_in_addr_t.h
c_msntvl.o: /usr/include/sys/_types/_in_port_t.h
c_msntvl.o: /usr/include/sys/_types/_ino_t.h
c_msntvl.o: /usr/include/sys/_types/_ino64_t.h
c_msntvl.o: /usr/include/sys/_types/_key_t.h
c_msntvl.o: /usr/include/sys/_types/_mode_t.h
c_msntvl.o: /usr/include/sys/_types/_nlink_t.h
c_msntvl.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_msntvl.o: /usr/include/sys/_types/_uid_t.h
c_msntvl.o: /usr/include/sys/_types/_clock_t.h
c_msntvl.o: /usr/include/sys/_types/_time_t.h
c_msntvl.o: /usr/include/sys/_types/_useconds_t.h
c_msntvl.o: /usr/include/sys/_types/_suseconds_t.h
c_msntvl.o: /usr/include/sys/_types/_rsize_t.h
c_msntvl.o: /usr/include/sys/_types/_errno_t.h
c_msntvl.o: /usr/include/sys/_types/_fd_def.h
c_msntvl.o: /usr/include/sys/_types/_fd_setsize.h
c_msntvl.o: /usr/include/sys/_types/_fd_set.h
c_msntvl.o: /usr/include/sys/_types/_fd_clr.h
c_msntvl.o: /usr/include/sys/_types/_fd_zero.h
c_msntvl.o: /usr/include/sys/_types/_fd_isset.h
c_msntvl.o: /usr/include/sys/_types/_fd_copy.h
c_msntvl.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_msntvl.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_msntvl.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_msntvl.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_msntvl.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_msntvl.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_msntvl.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_msntvl.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_msntvl.o: /usr/include/sys/_pthread/_pthread_t.h
c_msntvl.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_msntvl.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_msntvl.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_msntvl.o: ../../../.././include/ncarg/c.h
c_msshch.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_msshch.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_msshch.o: /usr/include/sys/_posix_availability.h
c_msshch.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_msshch.o: /usr/include/_types.h /usr/include/sys/_types.h
c_msshch.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_msshch.o: /usr/include/sys/_pthread/_pthread_types.h
c_msshch.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_msshch.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_msshch.o: /usr/include/sys/_types/_int16_t.h
c_msshch.o: /usr/include/sys/_types/_int32_t.h
c_msshch.o: /usr/include/sys/_types/_int64_t.h
c_msshch.o: /usr/include/sys/_types/_u_int8_t.h
c_msshch.o: /usr/include/sys/_types/_u_int16_t.h
c_msshch.o: /usr/include/sys/_types/_u_int32_t.h
c_msshch.o: /usr/include/sys/_types/_u_int64_t.h
c_msshch.o: /usr/include/sys/_types/_intptr_t.h
c_msshch.o: /usr/include/sys/_types/_uintptr_t.h
c_msshch.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_msshch.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_msshch.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_msshch.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_msshch.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_msshch.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_msshch.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_msshch.o: /usr/include/libkern/i386/_OSByteOrder.h
c_msshch.o: /usr/include/sys/_types/_u_char.h
c_msshch.o: /usr/include/sys/_types/_u_short.h
c_msshch.o: /usr/include/sys/_types/_u_int.h
c_msshch.o: /usr/include/sys/_types/_caddr_t.h
c_msshch.o: /usr/include/sys/_types/_dev_t.h
c_msshch.o: /usr/include/sys/_types/_blkcnt_t.h
c_msshch.o: /usr/include/sys/_types/_blksize_t.h
c_msshch.o: /usr/include/sys/_types/_gid_t.h
c_msshch.o: /usr/include/sys/_types/_in_addr_t.h
c_msshch.o: /usr/include/sys/_types/_in_port_t.h
c_msshch.o: /usr/include/sys/_types/_ino_t.h
c_msshch.o: /usr/include/sys/_types/_ino64_t.h
c_msshch.o: /usr/include/sys/_types/_key_t.h
c_msshch.o: /usr/include/sys/_types/_mode_t.h
c_msshch.o: /usr/include/sys/_types/_nlink_t.h
c_msshch.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_msshch.o: /usr/include/sys/_types/_uid_t.h
c_msshch.o: /usr/include/sys/_types/_clock_t.h
c_msshch.o: /usr/include/sys/_types/_time_t.h
c_msshch.o: /usr/include/sys/_types/_useconds_t.h
c_msshch.o: /usr/include/sys/_types/_suseconds_t.h
c_msshch.o: /usr/include/sys/_types/_rsize_t.h
c_msshch.o: /usr/include/sys/_types/_errno_t.h
c_msshch.o: /usr/include/sys/_types/_fd_def.h
c_msshch.o: /usr/include/sys/_types/_fd_setsize.h
c_msshch.o: /usr/include/sys/_types/_fd_set.h
c_msshch.o: /usr/include/sys/_types/_fd_clr.h
c_msshch.o: /usr/include/sys/_types/_fd_zero.h
c_msshch.o: /usr/include/sys/_types/_fd_isset.h
c_msshch.o: /usr/include/sys/_types/_fd_copy.h
c_msshch.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_msshch.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_msshch.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_msshch.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_msshch.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_msshch.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_msshch.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_msshch.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_msshch.o: /usr/include/sys/_pthread/_pthread_t.h
c_msshch.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_msshch.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_msshch.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_msshch.o: ../../../.././include/ncarg/c.h
c_mssrf1.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mssrf1.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mssrf1.o: /usr/include/sys/_posix_availability.h
c_mssrf1.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mssrf1.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mssrf1.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mssrf1.o: /usr/include/sys/_pthread/_pthread_types.h
c_mssrf1.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mssrf1.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mssrf1.o: /usr/include/sys/_types/_int16_t.h
c_mssrf1.o: /usr/include/sys/_types/_int32_t.h
c_mssrf1.o: /usr/include/sys/_types/_int64_t.h
c_mssrf1.o: /usr/include/sys/_types/_u_int8_t.h
c_mssrf1.o: /usr/include/sys/_types/_u_int16_t.h
c_mssrf1.o: /usr/include/sys/_types/_u_int32_t.h
c_mssrf1.o: /usr/include/sys/_types/_u_int64_t.h
c_mssrf1.o: /usr/include/sys/_types/_intptr_t.h
c_mssrf1.o: /usr/include/sys/_types/_uintptr_t.h
c_mssrf1.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mssrf1.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mssrf1.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mssrf1.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mssrf1.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mssrf1.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mssrf1.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mssrf1.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mssrf1.o: /usr/include/sys/_types/_u_char.h
c_mssrf1.o: /usr/include/sys/_types/_u_short.h
c_mssrf1.o: /usr/include/sys/_types/_u_int.h
c_mssrf1.o: /usr/include/sys/_types/_caddr_t.h
c_mssrf1.o: /usr/include/sys/_types/_dev_t.h
c_mssrf1.o: /usr/include/sys/_types/_blkcnt_t.h
c_mssrf1.o: /usr/include/sys/_types/_blksize_t.h
c_mssrf1.o: /usr/include/sys/_types/_gid_t.h
c_mssrf1.o: /usr/include/sys/_types/_in_addr_t.h
c_mssrf1.o: /usr/include/sys/_types/_in_port_t.h
c_mssrf1.o: /usr/include/sys/_types/_ino_t.h
c_mssrf1.o: /usr/include/sys/_types/_ino64_t.h
c_mssrf1.o: /usr/include/sys/_types/_key_t.h
c_mssrf1.o: /usr/include/sys/_types/_mode_t.h
c_mssrf1.o: /usr/include/sys/_types/_nlink_t.h
c_mssrf1.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mssrf1.o: /usr/include/sys/_types/_uid_t.h
c_mssrf1.o: /usr/include/sys/_types/_clock_t.h
c_mssrf1.o: /usr/include/sys/_types/_time_t.h
c_mssrf1.o: /usr/include/sys/_types/_useconds_t.h
c_mssrf1.o: /usr/include/sys/_types/_suseconds_t.h
c_mssrf1.o: /usr/include/sys/_types/_rsize_t.h
c_mssrf1.o: /usr/include/sys/_types/_errno_t.h
c_mssrf1.o: /usr/include/sys/_types/_fd_def.h
c_mssrf1.o: /usr/include/sys/_types/_fd_setsize.h
c_mssrf1.o: /usr/include/sys/_types/_fd_set.h
c_mssrf1.o: /usr/include/sys/_types/_fd_clr.h
c_mssrf1.o: /usr/include/sys/_types/_fd_zero.h
c_mssrf1.o: /usr/include/sys/_types/_fd_isset.h
c_mssrf1.o: /usr/include/sys/_types/_fd_copy.h
c_mssrf1.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mssrf1.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mssrf1.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mssrf1.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mssrf1.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mssrf1.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mssrf1.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mssrf1.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mssrf1.o: /usr/include/sys/_pthread/_pthread_t.h
c_mssrf1.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mssrf1.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mssrf1.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mssrf1.o: ../../../.././include/ncarg/c.h
c_mssrf2.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mssrf2.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mssrf2.o: /usr/include/sys/_posix_availability.h
c_mssrf2.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mssrf2.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mssrf2.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mssrf2.o: /usr/include/sys/_pthread/_pthread_types.h
c_mssrf2.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mssrf2.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mssrf2.o: /usr/include/sys/_types/_int16_t.h
c_mssrf2.o: /usr/include/sys/_types/_int32_t.h
c_mssrf2.o: /usr/include/sys/_types/_int64_t.h
c_mssrf2.o: /usr/include/sys/_types/_u_int8_t.h
c_mssrf2.o: /usr/include/sys/_types/_u_int16_t.h
c_mssrf2.o: /usr/include/sys/_types/_u_int32_t.h
c_mssrf2.o: /usr/include/sys/_types/_u_int64_t.h
c_mssrf2.o: /usr/include/sys/_types/_intptr_t.h
c_mssrf2.o: /usr/include/sys/_types/_uintptr_t.h
c_mssrf2.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mssrf2.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mssrf2.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mssrf2.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mssrf2.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mssrf2.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mssrf2.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mssrf2.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mssrf2.o: /usr/include/sys/_types/_u_char.h
c_mssrf2.o: /usr/include/sys/_types/_u_short.h
c_mssrf2.o: /usr/include/sys/_types/_u_int.h
c_mssrf2.o: /usr/include/sys/_types/_caddr_t.h
c_mssrf2.o: /usr/include/sys/_types/_dev_t.h
c_mssrf2.o: /usr/include/sys/_types/_blkcnt_t.h
c_mssrf2.o: /usr/include/sys/_types/_blksize_t.h
c_mssrf2.o: /usr/include/sys/_types/_gid_t.h
c_mssrf2.o: /usr/include/sys/_types/_in_addr_t.h
c_mssrf2.o: /usr/include/sys/_types/_in_port_t.h
c_mssrf2.o: /usr/include/sys/_types/_ino_t.h
c_mssrf2.o: /usr/include/sys/_types/_ino64_t.h
c_mssrf2.o: /usr/include/sys/_types/_key_t.h
c_mssrf2.o: /usr/include/sys/_types/_mode_t.h
c_mssrf2.o: /usr/include/sys/_types/_nlink_t.h
c_mssrf2.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mssrf2.o: /usr/include/sys/_types/_uid_t.h
c_mssrf2.o: /usr/include/sys/_types/_clock_t.h
c_mssrf2.o: /usr/include/sys/_types/_time_t.h
c_mssrf2.o: /usr/include/sys/_types/_useconds_t.h
c_mssrf2.o: /usr/include/sys/_types/_suseconds_t.h
c_mssrf2.o: /usr/include/sys/_types/_rsize_t.h
c_mssrf2.o: /usr/include/sys/_types/_errno_t.h
c_mssrf2.o: /usr/include/sys/_types/_fd_def.h
c_mssrf2.o: /usr/include/sys/_types/_fd_setsize.h
c_mssrf2.o: /usr/include/sys/_types/_fd_set.h
c_mssrf2.o: /usr/include/sys/_types/_fd_clr.h
c_mssrf2.o: /usr/include/sys/_types/_fd_zero.h
c_mssrf2.o: /usr/include/sys/_types/_fd_isset.h
c_mssrf2.o: /usr/include/sys/_types/_fd_copy.h
c_mssrf2.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mssrf2.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mssrf2.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mssrf2.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mssrf2.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mssrf2.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mssrf2.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mssrf2.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mssrf2.o: /usr/include/sys/_pthread/_pthread_t.h
c_mssrf2.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mssrf2.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mssrf2.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mssrf2.o: ../../../.././include/ncarg/c.h
c_mstrms.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mstrms.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mstrms.o: /usr/include/sys/_posix_availability.h
c_mstrms.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mstrms.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mstrms.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mstrms.o: /usr/include/sys/_pthread/_pthread_types.h
c_mstrms.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mstrms.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mstrms.o: /usr/include/sys/_types/_int16_t.h
c_mstrms.o: /usr/include/sys/_types/_int32_t.h
c_mstrms.o: /usr/include/sys/_types/_int64_t.h
c_mstrms.o: /usr/include/sys/_types/_u_int8_t.h
c_mstrms.o: /usr/include/sys/_types/_u_int16_t.h
c_mstrms.o: /usr/include/sys/_types/_u_int32_t.h
c_mstrms.o: /usr/include/sys/_types/_u_int64_t.h
c_mstrms.o: /usr/include/sys/_types/_intptr_t.h
c_mstrms.o: /usr/include/sys/_types/_uintptr_t.h
c_mstrms.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mstrms.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mstrms.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mstrms.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mstrms.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mstrms.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mstrms.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mstrms.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mstrms.o: /usr/include/sys/_types/_u_char.h
c_mstrms.o: /usr/include/sys/_types/_u_short.h
c_mstrms.o: /usr/include/sys/_types/_u_int.h
c_mstrms.o: /usr/include/sys/_types/_caddr_t.h
c_mstrms.o: /usr/include/sys/_types/_dev_t.h
c_mstrms.o: /usr/include/sys/_types/_blkcnt_t.h
c_mstrms.o: /usr/include/sys/_types/_blksize_t.h
c_mstrms.o: /usr/include/sys/_types/_gid_t.h
c_mstrms.o: /usr/include/sys/_types/_in_addr_t.h
c_mstrms.o: /usr/include/sys/_types/_in_port_t.h
c_mstrms.o: /usr/include/sys/_types/_ino_t.h
c_mstrms.o: /usr/include/sys/_types/_ino64_t.h
c_mstrms.o: /usr/include/sys/_types/_key_t.h
c_mstrms.o: /usr/include/sys/_types/_mode_t.h
c_mstrms.o: /usr/include/sys/_types/_nlink_t.h
c_mstrms.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mstrms.o: /usr/include/sys/_types/_uid_t.h
c_mstrms.o: /usr/include/sys/_types/_clock_t.h
c_mstrms.o: /usr/include/sys/_types/_time_t.h
c_mstrms.o: /usr/include/sys/_types/_useconds_t.h
c_mstrms.o: /usr/include/sys/_types/_suseconds_t.h
c_mstrms.o: /usr/include/sys/_types/_rsize_t.h
c_mstrms.o: /usr/include/sys/_types/_errno_t.h
c_mstrms.o: /usr/include/sys/_types/_fd_def.h
c_mstrms.o: /usr/include/sys/_types/_fd_setsize.h
c_mstrms.o: /usr/include/sys/_types/_fd_set.h
c_mstrms.o: /usr/include/sys/_types/_fd_clr.h
c_mstrms.o: /usr/include/sys/_types/_fd_zero.h
c_mstrms.o: /usr/include/sys/_types/_fd_isset.h
c_mstrms.o: /usr/include/sys/_types/_fd_copy.h
c_mstrms.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mstrms.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mstrms.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mstrms.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mstrms.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mstrms.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mstrms.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mstrms.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mstrms.o: /usr/include/sys/_pthread/_pthread_t.h
c_mstrms.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mstrms.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mstrms.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mstrms.o: ../../../.././include/ncarg/c.h
c_q8qst4.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_q8qst4.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_q8qst4.o: /usr/include/sys/_posix_availability.h
c_q8qst4.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_q8qst4.o: /usr/include/_types.h /usr/include/sys/_types.h
c_q8qst4.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_q8qst4.o: /usr/include/sys/_pthread/_pthread_types.h
c_q8qst4.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_q8qst4.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_q8qst4.o: /usr/include/sys/_types/_int16_t.h
c_q8qst4.o: /usr/include/sys/_types/_int32_t.h
c_q8qst4.o: /usr/include/sys/_types/_int64_t.h
c_q8qst4.o: /usr/include/sys/_types/_u_int8_t.h
c_q8qst4.o: /usr/include/sys/_types/_u_int16_t.h
c_q8qst4.o: /usr/include/sys/_types/_u_int32_t.h
c_q8qst4.o: /usr/include/sys/_types/_u_int64_t.h
c_q8qst4.o: /usr/include/sys/_types/_intptr_t.h
c_q8qst4.o: /usr/include/sys/_types/_uintptr_t.h
c_q8qst4.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_q8qst4.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_q8qst4.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_q8qst4.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_q8qst4.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_q8qst4.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_q8qst4.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_q8qst4.o: /usr/include/libkern/i386/_OSByteOrder.h
c_q8qst4.o: /usr/include/sys/_types/_u_char.h
c_q8qst4.o: /usr/include/sys/_types/_u_short.h
c_q8qst4.o: /usr/include/sys/_types/_u_int.h
c_q8qst4.o: /usr/include/sys/_types/_caddr_t.h
c_q8qst4.o: /usr/include/sys/_types/_dev_t.h
c_q8qst4.o: /usr/include/sys/_types/_blkcnt_t.h
c_q8qst4.o: /usr/include/sys/_types/_blksize_t.h
c_q8qst4.o: /usr/include/sys/_types/_gid_t.h
c_q8qst4.o: /usr/include/sys/_types/_in_addr_t.h
c_q8qst4.o: /usr/include/sys/_types/_in_port_t.h
c_q8qst4.o: /usr/include/sys/_types/_ino_t.h
c_q8qst4.o: /usr/include/sys/_types/_ino64_t.h
c_q8qst4.o: /usr/include/sys/_types/_key_t.h
c_q8qst4.o: /usr/include/sys/_types/_mode_t.h
c_q8qst4.o: /usr/include/sys/_types/_nlink_t.h
c_q8qst4.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_q8qst4.o: /usr/include/sys/_types/_uid_t.h
c_q8qst4.o: /usr/include/sys/_types/_clock_t.h
c_q8qst4.o: /usr/include/sys/_types/_time_t.h
c_q8qst4.o: /usr/include/sys/_types/_useconds_t.h
c_q8qst4.o: /usr/include/sys/_types/_suseconds_t.h
c_q8qst4.o: /usr/include/sys/_types/_rsize_t.h
c_q8qst4.o: /usr/include/sys/_types/_errno_t.h
c_q8qst4.o: /usr/include/sys/_types/_fd_def.h
c_q8qst4.o: /usr/include/sys/_types/_fd_setsize.h
c_q8qst4.o: /usr/include/sys/_types/_fd_set.h
c_q8qst4.o: /usr/include/sys/_types/_fd_clr.h
c_q8qst4.o: /usr/include/sys/_types/_fd_zero.h
c_q8qst4.o: /usr/include/sys/_types/_fd_isset.h
c_q8qst4.o: /usr/include/sys/_types/_fd_copy.h
c_q8qst4.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_q8qst4.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_q8qst4.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_q8qst4.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_q8qst4.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_q8qst4.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_q8qst4.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_q8qst4.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_q8qst4.o: /usr/include/sys/_pthread/_pthread_t.h
c_q8qst4.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_q8qst4.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_q8qst4.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_q8qst4.o: ../../../.././include/ncarg/c.h
c_retsr.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_retsr.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_retsr.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_retsr.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_retsr.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_retsr.o: /usr/include/i386/_types.h
c_retsr.o: /usr/include/sys/_pthread/_pthread_types.h
c_retsr.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_retsr.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_retsr.o: /usr/include/sys/_types/_int16_t.h
c_retsr.o: /usr/include/sys/_types/_int32_t.h
c_retsr.o: /usr/include/sys/_types/_int64_t.h
c_retsr.o: /usr/include/sys/_types/_u_int8_t.h
c_retsr.o: /usr/include/sys/_types/_u_int16_t.h
c_retsr.o: /usr/include/sys/_types/_u_int32_t.h
c_retsr.o: /usr/include/sys/_types/_u_int64_t.h
c_retsr.o: /usr/include/sys/_types/_intptr_t.h
c_retsr.o: /usr/include/sys/_types/_uintptr_t.h
c_retsr.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_retsr.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_retsr.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_retsr.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_retsr.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_retsr.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_retsr.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_retsr.o: /usr/include/libkern/i386/_OSByteOrder.h
c_retsr.o: /usr/include/sys/_types/_u_char.h
c_retsr.o: /usr/include/sys/_types/_u_short.h
c_retsr.o: /usr/include/sys/_types/_u_int.h
c_retsr.o: /usr/include/sys/_types/_caddr_t.h
c_retsr.o: /usr/include/sys/_types/_dev_t.h
c_retsr.o: /usr/include/sys/_types/_blkcnt_t.h
c_retsr.o: /usr/include/sys/_types/_blksize_t.h
c_retsr.o: /usr/include/sys/_types/_gid_t.h
c_retsr.o: /usr/include/sys/_types/_in_addr_t.h
c_retsr.o: /usr/include/sys/_types/_in_port_t.h
c_retsr.o: /usr/include/sys/_types/_ino_t.h
c_retsr.o: /usr/include/sys/_types/_ino64_t.h
c_retsr.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_retsr.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_retsr.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_retsr.o: /usr/include/sys/_types/_clock_t.h
c_retsr.o: /usr/include/sys/_types/_time_t.h
c_retsr.o: /usr/include/sys/_types/_useconds_t.h
c_retsr.o: /usr/include/sys/_types/_suseconds_t.h
c_retsr.o: /usr/include/sys/_types/_rsize_t.h
c_retsr.o: /usr/include/sys/_types/_errno_t.h
c_retsr.o: /usr/include/sys/_types/_fd_def.h
c_retsr.o: /usr/include/sys/_types/_fd_setsize.h
c_retsr.o: /usr/include/sys/_types/_fd_set.h
c_retsr.o: /usr/include/sys/_types/_fd_clr.h
c_retsr.o: /usr/include/sys/_types/_fd_zero.h
c_retsr.o: /usr/include/sys/_types/_fd_isset.h
c_retsr.o: /usr/include/sys/_types/_fd_copy.h
c_retsr.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_retsr.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_retsr.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_retsr.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_retsr.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_retsr.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_retsr.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_retsr.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_retsr.o: /usr/include/sys/_pthread/_pthread_t.h
c_retsr.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_retsr.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_retsr.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_retsr.o: ../../../.././include/ncarg/c.h
c_semess.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_semess.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_semess.o: /usr/include/sys/_posix_availability.h
c_semess.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_semess.o: /usr/include/_types.h /usr/include/sys/_types.h
c_semess.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_semess.o: /usr/include/sys/_pthread/_pthread_types.h
c_semess.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_semess.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_semess.o: /usr/include/sys/_types/_int16_t.h
c_semess.o: /usr/include/sys/_types/_int32_t.h
c_semess.o: /usr/include/sys/_types/_int64_t.h
c_semess.o: /usr/include/sys/_types/_u_int8_t.h
c_semess.o: /usr/include/sys/_types/_u_int16_t.h
c_semess.o: /usr/include/sys/_types/_u_int32_t.h
c_semess.o: /usr/include/sys/_types/_u_int64_t.h
c_semess.o: /usr/include/sys/_types/_intptr_t.h
c_semess.o: /usr/include/sys/_types/_uintptr_t.h
c_semess.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_semess.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_semess.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_semess.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_semess.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_semess.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_semess.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_semess.o: /usr/include/libkern/i386/_OSByteOrder.h
c_semess.o: /usr/include/sys/_types/_u_char.h
c_semess.o: /usr/include/sys/_types/_u_short.h
c_semess.o: /usr/include/sys/_types/_u_int.h
c_semess.o: /usr/include/sys/_types/_caddr_t.h
c_semess.o: /usr/include/sys/_types/_dev_t.h
c_semess.o: /usr/include/sys/_types/_blkcnt_t.h
c_semess.o: /usr/include/sys/_types/_blksize_t.h
c_semess.o: /usr/include/sys/_types/_gid_t.h
c_semess.o: /usr/include/sys/_types/_in_addr_t.h
c_semess.o: /usr/include/sys/_types/_in_port_t.h
c_semess.o: /usr/include/sys/_types/_ino_t.h
c_semess.o: /usr/include/sys/_types/_ino64_t.h
c_semess.o: /usr/include/sys/_types/_key_t.h
c_semess.o: /usr/include/sys/_types/_mode_t.h
c_semess.o: /usr/include/sys/_types/_nlink_t.h
c_semess.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_semess.o: /usr/include/sys/_types/_uid_t.h
c_semess.o: /usr/include/sys/_types/_clock_t.h
c_semess.o: /usr/include/sys/_types/_time_t.h
c_semess.o: /usr/include/sys/_types/_useconds_t.h
c_semess.o: /usr/include/sys/_types/_suseconds_t.h
c_semess.o: /usr/include/sys/_types/_rsize_t.h
c_semess.o: /usr/include/sys/_types/_errno_t.h
c_semess.o: /usr/include/sys/_types/_fd_def.h
c_semess.o: /usr/include/sys/_types/_fd_setsize.h
c_semess.o: /usr/include/sys/_types/_fd_set.h
c_semess.o: /usr/include/sys/_types/_fd_clr.h
c_semess.o: /usr/include/sys/_types/_fd_zero.h
c_semess.o: /usr/include/sys/_types/_fd_isset.h
c_semess.o: /usr/include/sys/_types/_fd_copy.h
c_semess.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_semess.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_semess.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_semess.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_semess.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_semess.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_semess.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_semess.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_semess.o: /usr/include/sys/_pthread/_pthread_t.h
c_semess.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_semess.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_semess.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_semess.o: ../../../.././include/ncarg/c.h
c_seter.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_seter.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_seter.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_seter.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_seter.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_seter.o: /usr/include/i386/_types.h
c_seter.o: /usr/include/sys/_pthread/_pthread_types.h
c_seter.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_seter.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_seter.o: /usr/include/sys/_types/_int16_t.h
c_seter.o: /usr/include/sys/_types/_int32_t.h
c_seter.o: /usr/include/sys/_types/_int64_t.h
c_seter.o: /usr/include/sys/_types/_u_int8_t.h
c_seter.o: /usr/include/sys/_types/_u_int16_t.h
c_seter.o: /usr/include/sys/_types/_u_int32_t.h
c_seter.o: /usr/include/sys/_types/_u_int64_t.h
c_seter.o: /usr/include/sys/_types/_intptr_t.h
c_seter.o: /usr/include/sys/_types/_uintptr_t.h
c_seter.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_seter.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_seter.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_seter.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_seter.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_seter.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_seter.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_seter.o: /usr/include/libkern/i386/_OSByteOrder.h
c_seter.o: /usr/include/sys/_types/_u_char.h
c_seter.o: /usr/include/sys/_types/_u_short.h
c_seter.o: /usr/include/sys/_types/_u_int.h
c_seter.o: /usr/include/sys/_types/_caddr_t.h
c_seter.o: /usr/include/sys/_types/_dev_t.h
c_seter.o: /usr/include/sys/_types/_blkcnt_t.h
c_seter.o: /usr/include/sys/_types/_blksize_t.h
c_seter.o: /usr/include/sys/_types/_gid_t.h
c_seter.o: /usr/include/sys/_types/_in_addr_t.h
c_seter.o: /usr/include/sys/_types/_in_port_t.h
c_seter.o: /usr/include/sys/_types/_ino_t.h
c_seter.o: /usr/include/sys/_types/_ino64_t.h
c_seter.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_seter.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_seter.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_seter.o: /usr/include/sys/_types/_clock_t.h
c_seter.o: /usr/include/sys/_types/_time_t.h
c_seter.o: /usr/include/sys/_types/_useconds_t.h
c_seter.o: /usr/include/sys/_types/_suseconds_t.h
c_seter.o: /usr/include/sys/_types/_rsize_t.h
c_seter.o: /usr/include/sys/_types/_errno_t.h
c_seter.o: /usr/include/sys/_types/_fd_def.h
c_seter.o: /usr/include/sys/_types/_fd_setsize.h
c_seter.o: /usr/include/sys/_types/_fd_set.h
c_seter.o: /usr/include/sys/_types/_fd_clr.h
c_seter.o: /usr/include/sys/_types/_fd_zero.h
c_seter.o: /usr/include/sys/_types/_fd_isset.h
c_seter.o: /usr/include/sys/_types/_fd_copy.h
c_seter.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_seter.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_seter.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_seter.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_seter.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_seter.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_seter.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_seter.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_seter.o: /usr/include/sys/_pthread/_pthread_t.h
c_seter.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_seter.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_seter.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_seter.o: ../../../.././include/ncarg/c.h
c_icloem.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_icloem.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_icloem.o: /usr/include/sys/_posix_availability.h
c_icloem.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_icloem.o: /usr/include/_types.h /usr/include/sys/_types.h
c_icloem.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_icloem.o: /usr/include/sys/_pthread/_pthread_types.h
c_icloem.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_icloem.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_icloem.o: /usr/include/sys/_types/_int16_t.h
c_icloem.o: /usr/include/sys/_types/_int32_t.h
c_icloem.o: /usr/include/sys/_types/_int64_t.h
c_icloem.o: /usr/include/sys/_types/_u_int8_t.h
c_icloem.o: /usr/include/sys/_types/_u_int16_t.h
c_icloem.o: /usr/include/sys/_types/_u_int32_t.h
c_icloem.o: /usr/include/sys/_types/_u_int64_t.h
c_icloem.o: /usr/include/sys/_types/_intptr_t.h
c_icloem.o: /usr/include/sys/_types/_uintptr_t.h
c_icloem.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_icloem.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_icloem.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_icloem.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_icloem.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_icloem.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_icloem.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_icloem.o: /usr/include/libkern/i386/_OSByteOrder.h
c_icloem.o: /usr/include/sys/_types/_u_char.h
c_icloem.o: /usr/include/sys/_types/_u_short.h
c_icloem.o: /usr/include/sys/_types/_u_int.h
c_icloem.o: /usr/include/sys/_types/_caddr_t.h
c_icloem.o: /usr/include/sys/_types/_dev_t.h
c_icloem.o: /usr/include/sys/_types/_blkcnt_t.h
c_icloem.o: /usr/include/sys/_types/_blksize_t.h
c_icloem.o: /usr/include/sys/_types/_gid_t.h
c_icloem.o: /usr/include/sys/_types/_in_addr_t.h
c_icloem.o: /usr/include/sys/_types/_in_port_t.h
c_icloem.o: /usr/include/sys/_types/_ino_t.h
c_icloem.o: /usr/include/sys/_types/_ino64_t.h
c_icloem.o: /usr/include/sys/_types/_key_t.h
c_icloem.o: /usr/include/sys/_types/_mode_t.h
c_icloem.o: /usr/include/sys/_types/_nlink_t.h
c_icloem.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_icloem.o: /usr/include/sys/_types/_uid_t.h
c_icloem.o: /usr/include/sys/_types/_clock_t.h
c_icloem.o: /usr/include/sys/_types/_time_t.h
c_icloem.o: /usr/include/sys/_types/_useconds_t.h
c_icloem.o: /usr/include/sys/_types/_suseconds_t.h
c_icloem.o: /usr/include/sys/_types/_rsize_t.h
c_icloem.o: /usr/include/sys/_types/_errno_t.h
c_icloem.o: /usr/include/sys/_types/_fd_def.h
c_icloem.o: /usr/include/sys/_types/_fd_setsize.h
c_icloem.o: /usr/include/sys/_types/_fd_set.h
c_icloem.o: /usr/include/sys/_types/_fd_clr.h
c_icloem.o: /usr/include/sys/_types/_fd_zero.h
c_icloem.o: /usr/include/sys/_types/_fd_isset.h
c_icloem.o: /usr/include/sys/_types/_fd_copy.h
c_icloem.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_icloem.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_icloem.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_icloem.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_icloem.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_icloem.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_icloem.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_icloem.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_icloem.o: /usr/include/sys/_pthread/_pthread_t.h
c_icloem.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_icloem.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_icloem.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_icloem.o: ../../../.././include/ncarg/c.h
c_icfell.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_icfell.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_icfell.o: /usr/include/sys/_posix_availability.h
c_icfell.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_icfell.o: /usr/include/_types.h /usr/include/sys/_types.h
c_icfell.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_icfell.o: /usr/include/sys/_pthread/_pthread_types.h
c_icfell.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_icfell.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_icfell.o: /usr/include/sys/_types/_int16_t.h
c_icfell.o: /usr/include/sys/_types/_int32_t.h
c_icfell.o: /usr/include/sys/_types/_int64_t.h
c_icfell.o: /usr/include/sys/_types/_u_int8_t.h
c_icfell.o: /usr/include/sys/_types/_u_int16_t.h
c_icfell.o: /usr/include/sys/_types/_u_int32_t.h
c_icfell.o: /usr/include/sys/_types/_u_int64_t.h
c_icfell.o: /usr/include/sys/_types/_intptr_t.h
c_icfell.o: /usr/include/sys/_types/_uintptr_t.h
c_icfell.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_icfell.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_icfell.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_icfell.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_icfell.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_icfell.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_icfell.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_icfell.o: /usr/include/libkern/i386/_OSByteOrder.h
c_icfell.o: /usr/include/sys/_types/_u_char.h
c_icfell.o: /usr/include/sys/_types/_u_short.h
c_icfell.o: /usr/include/sys/_types/_u_int.h
c_icfell.o: /usr/include/sys/_types/_caddr_t.h
c_icfell.o: /usr/include/sys/_types/_dev_t.h
c_icfell.o: /usr/include/sys/_types/_blkcnt_t.h
c_icfell.o: /usr/include/sys/_types/_blksize_t.h
c_icfell.o: /usr/include/sys/_types/_gid_t.h
c_icfell.o: /usr/include/sys/_types/_in_addr_t.h
c_icfell.o: /usr/include/sys/_types/_in_port_t.h
c_icfell.o: /usr/include/sys/_types/_ino_t.h
c_icfell.o: /usr/include/sys/_types/_ino64_t.h
c_icfell.o: /usr/include/sys/_types/_key_t.h
c_icfell.o: /usr/include/sys/_types/_mode_t.h
c_icfell.o: /usr/include/sys/_types/_nlink_t.h
c_icfell.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_icfell.o: /usr/include/sys/_types/_uid_t.h
c_icfell.o: /usr/include/sys/_types/_clock_t.h
c_icfell.o: /usr/include/sys/_types/_time_t.h
c_icfell.o: /usr/include/sys/_types/_useconds_t.h
c_icfell.o: /usr/include/sys/_types/_suseconds_t.h
c_icfell.o: /usr/include/sys/_types/_rsize_t.h
c_icfell.o: /usr/include/sys/_types/_errno_t.h
c_icfell.o: /usr/include/sys/_types/_fd_def.h
c_icfell.o: /usr/include/sys/_types/_fd_setsize.h
c_icfell.o: /usr/include/sys/_types/_fd_set.h
c_icfell.o: /usr/include/sys/_types/_fd_clr.h
c_icfell.o: /usr/include/sys/_types/_fd_zero.h
c_icfell.o: /usr/include/sys/_types/_fd_isset.h
c_icfell.o: /usr/include/sys/_types/_fd_copy.h
c_icfell.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_icfell.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_icfell.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_icfell.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_icfell.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_icfell.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_icfell.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_icfell.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_icfell.o: /usr/include/sys/_pthread/_pthread_t.h
c_icfell.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_icfell.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_icfell.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_icfell.o: ../../../.././include/ncarg/c.h
c_idbvip.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_idbvip.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_idbvip.o: /usr/include/sys/_posix_availability.h
c_idbvip.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_idbvip.o: /usr/include/_types.h /usr/include/sys/_types.h
c_idbvip.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_idbvip.o: /usr/include/sys/_pthread/_pthread_types.h
c_idbvip.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_idbvip.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_idbvip.o: /usr/include/sys/_types/_int16_t.h
c_idbvip.o: /usr/include/sys/_types/_int32_t.h
c_idbvip.o: /usr/include/sys/_types/_int64_t.h
c_idbvip.o: /usr/include/sys/_types/_u_int8_t.h
c_idbvip.o: /usr/include/sys/_types/_u_int16_t.h
c_idbvip.o: /usr/include/sys/_types/_u_int32_t.h
c_idbvip.o: /usr/include/sys/_types/_u_int64_t.h
c_idbvip.o: /usr/include/sys/_types/_intptr_t.h
c_idbvip.o: /usr/include/sys/_types/_uintptr_t.h
c_idbvip.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_idbvip.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_idbvip.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_idbvip.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_idbvip.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_idbvip.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_idbvip.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_idbvip.o: /usr/include/libkern/i386/_OSByteOrder.h
c_idbvip.o: /usr/include/sys/_types/_u_char.h
c_idbvip.o: /usr/include/sys/_types/_u_short.h
c_idbvip.o: /usr/include/sys/_types/_u_int.h
c_idbvip.o: /usr/include/sys/_types/_caddr_t.h
c_idbvip.o: /usr/include/sys/_types/_dev_t.h
c_idbvip.o: /usr/include/sys/_types/_blkcnt_t.h
c_idbvip.o: /usr/include/sys/_types/_blksize_t.h
c_idbvip.o: /usr/include/sys/_types/_gid_t.h
c_idbvip.o: /usr/include/sys/_types/_in_addr_t.h
c_idbvip.o: /usr/include/sys/_types/_in_port_t.h
c_idbvip.o: /usr/include/sys/_types/_ino_t.h
c_idbvip.o: /usr/include/sys/_types/_ino64_t.h
c_idbvip.o: /usr/include/sys/_types/_key_t.h
c_idbvip.o: /usr/include/sys/_types/_mode_t.h
c_idbvip.o: /usr/include/sys/_types/_nlink_t.h
c_idbvip.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_idbvip.o: /usr/include/sys/_types/_uid_t.h
c_idbvip.o: /usr/include/sys/_types/_clock_t.h
c_idbvip.o: /usr/include/sys/_types/_time_t.h
c_idbvip.o: /usr/include/sys/_types/_useconds_t.h
c_idbvip.o: /usr/include/sys/_types/_suseconds_t.h
c_idbvip.o: /usr/include/sys/_types/_rsize_t.h
c_idbvip.o: /usr/include/sys/_types/_errno_t.h
c_idbvip.o: /usr/include/sys/_types/_fd_def.h
c_idbvip.o: /usr/include/sys/_types/_fd_setsize.h
c_idbvip.o: /usr/include/sys/_types/_fd_set.h
c_idbvip.o: /usr/include/sys/_types/_fd_clr.h
c_idbvip.o: /usr/include/sys/_types/_fd_zero.h
c_idbvip.o: /usr/include/sys/_types/_fd_isset.h
c_idbvip.o: /usr/include/sys/_types/_fd_copy.h
c_idbvip.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_idbvip.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_idbvip.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_idbvip.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_idbvip.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_idbvip.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_idbvip.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_idbvip.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_idbvip.o: /usr/include/sys/_pthread/_pthread_t.h
c_idbvip.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_idbvip.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_idbvip.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_idbvip.o: ../../../.././include/ncarg/c.h
c_idsfft.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_idsfft.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_idsfft.o: /usr/include/sys/_posix_availability.h
c_idsfft.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_idsfft.o: /usr/include/_types.h /usr/include/sys/_types.h
c_idsfft.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_idsfft.o: /usr/include/sys/_pthread/_pthread_types.h
c_idsfft.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_idsfft.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_idsfft.o: /usr/include/sys/_types/_int16_t.h
c_idsfft.o: /usr/include/sys/_types/_int32_t.h
c_idsfft.o: /usr/include/sys/_types/_int64_t.h
c_idsfft.o: /usr/include/sys/_types/_u_int8_t.h
c_idsfft.o: /usr/include/sys/_types/_u_int16_t.h
c_idsfft.o: /usr/include/sys/_types/_u_int32_t.h
c_idsfft.o: /usr/include/sys/_types/_u_int64_t.h
c_idsfft.o: /usr/include/sys/_types/_intptr_t.h
c_idsfft.o: /usr/include/sys/_types/_uintptr_t.h
c_idsfft.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_idsfft.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_idsfft.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_idsfft.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_idsfft.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_idsfft.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_idsfft.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_idsfft.o: /usr/include/libkern/i386/_OSByteOrder.h
c_idsfft.o: /usr/include/sys/_types/_u_char.h
c_idsfft.o: /usr/include/sys/_types/_u_short.h
c_idsfft.o: /usr/include/sys/_types/_u_int.h
c_idsfft.o: /usr/include/sys/_types/_caddr_t.h
c_idsfft.o: /usr/include/sys/_types/_dev_t.h
c_idsfft.o: /usr/include/sys/_types/_blkcnt_t.h
c_idsfft.o: /usr/include/sys/_types/_blksize_t.h
c_idsfft.o: /usr/include/sys/_types/_gid_t.h
c_idsfft.o: /usr/include/sys/_types/_in_addr_t.h
c_idsfft.o: /usr/include/sys/_types/_in_port_t.h
c_idsfft.o: /usr/include/sys/_types/_ino_t.h
c_idsfft.o: /usr/include/sys/_types/_ino64_t.h
c_idsfft.o: /usr/include/sys/_types/_key_t.h
c_idsfft.o: /usr/include/sys/_types/_mode_t.h
c_idsfft.o: /usr/include/sys/_types/_nlink_t.h
c_idsfft.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_idsfft.o: /usr/include/sys/_types/_uid_t.h
c_idsfft.o: /usr/include/sys/_types/_clock_t.h
c_idsfft.o: /usr/include/sys/_types/_time_t.h
c_idsfft.o: /usr/include/sys/_types/_useconds_t.h
c_idsfft.o: /usr/include/sys/_types/_suseconds_t.h
c_idsfft.o: /usr/include/sys/_types/_rsize_t.h
c_idsfft.o: /usr/include/sys/_types/_errno_t.h
c_idsfft.o: /usr/include/sys/_types/_fd_def.h
c_idsfft.o: /usr/include/sys/_types/_fd_setsize.h
c_idsfft.o: /usr/include/sys/_types/_fd_set.h
c_idsfft.o: /usr/include/sys/_types/_fd_clr.h
c_idsfft.o: /usr/include/sys/_types/_fd_zero.h
c_idsfft.o: /usr/include/sys/_types/_fd_isset.h
c_idsfft.o: /usr/include/sys/_types/_fd_copy.h
c_idsfft.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_idsfft.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_idsfft.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_idsfft.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_idsfft.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_idsfft.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_idsfft.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_idsfft.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_idsfft.o: /usr/include/sys/_pthread/_pthread_t.h
c_idsfft.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_idsfft.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_idsfft.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_idsfft.o: ../../../.././include/ncarg/c.h
