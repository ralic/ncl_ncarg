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
CURRENT_DIR		= ./ncarg2d/src/libncarg/conpacktC

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
#       $Id: yMakefile,v 1.3 2004-03-26 22:06:21 kennison Exp $
#

MYNAME	= conpacktC

SRCS =  c_ctback.c c_ctcica.c c_ctclam.c c_ctcldm.c c_ctcldr.c c_ctcltr.c  	c_ctgetc.c c_ctgeti.c c_ctgetr.c c_ctlbam.c c_ctlbdr.c c_ctmesh.c  	c_ctmviw.c c_ctmvrw.c c_ctpkcl.c c_ctpklb.c c_ctrset.c c_ctsetc.c  	c_ctseti.c c_ctsetr.c c_cttdbf.c c_cttdbm.c c_cttdca.c c_cttddm.c  	c_cttmrg.c c_cttmtl.c

OBJS =  c_ctback.o c_ctcica.o c_ctclam.o c_ctcldm.o c_ctcldr.o c_ctcltr.o  	c_ctgetc.o c_ctgeti.o c_ctgetr.o c_ctlbam.o c_ctlbdr.o c_ctmesh.o  	c_ctmviw.o c_ctmvrw.o c_ctpkcl.o c_ctpklb.o c_ctrset.o c_ctsetc.o  	c_ctseti.o c_ctsetr.o c_cttdbf.o c_cttdbm.o c_cttdca.o c_cttddm.o  	c_cttmrg.o c_cttmtl.o

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

c_ctback.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_ctback.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_ctback.o: /usr/include/sys/_posix_availability.h
c_ctback.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_ctback.o: /usr/include/_types.h /usr/include/sys/_types.h
c_ctback.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_ctback.o: /usr/include/sys/_pthread/_pthread_types.h
c_ctback.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_ctback.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_ctback.o: /usr/include/sys/_types/_int16_t.h
c_ctback.o: /usr/include/sys/_types/_int32_t.h
c_ctback.o: /usr/include/sys/_types/_int64_t.h
c_ctback.o: /usr/include/sys/_types/_u_int8_t.h
c_ctback.o: /usr/include/sys/_types/_u_int16_t.h
c_ctback.o: /usr/include/sys/_types/_u_int32_t.h
c_ctback.o: /usr/include/sys/_types/_u_int64_t.h
c_ctback.o: /usr/include/sys/_types/_intptr_t.h
c_ctback.o: /usr/include/sys/_types/_uintptr_t.h
c_ctback.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_ctback.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_ctback.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_ctback.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_ctback.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_ctback.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_ctback.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_ctback.o: /usr/include/libkern/i386/_OSByteOrder.h
c_ctback.o: /usr/include/sys/_types/_u_char.h
c_ctback.o: /usr/include/sys/_types/_u_short.h
c_ctback.o: /usr/include/sys/_types/_u_int.h
c_ctback.o: /usr/include/sys/_types/_caddr_t.h
c_ctback.o: /usr/include/sys/_types/_dev_t.h
c_ctback.o: /usr/include/sys/_types/_blkcnt_t.h
c_ctback.o: /usr/include/sys/_types/_blksize_t.h
c_ctback.o: /usr/include/sys/_types/_gid_t.h
c_ctback.o: /usr/include/sys/_types/_in_addr_t.h
c_ctback.o: /usr/include/sys/_types/_in_port_t.h
c_ctback.o: /usr/include/sys/_types/_ino_t.h
c_ctback.o: /usr/include/sys/_types/_ino64_t.h
c_ctback.o: /usr/include/sys/_types/_key_t.h
c_ctback.o: /usr/include/sys/_types/_mode_t.h
c_ctback.o: /usr/include/sys/_types/_nlink_t.h
c_ctback.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_ctback.o: /usr/include/sys/_types/_uid_t.h
c_ctback.o: /usr/include/sys/_types/_clock_t.h
c_ctback.o: /usr/include/sys/_types/_time_t.h
c_ctback.o: /usr/include/sys/_types/_useconds_t.h
c_ctback.o: /usr/include/sys/_types/_suseconds_t.h
c_ctback.o: /usr/include/sys/_types/_rsize_t.h
c_ctback.o: /usr/include/sys/_types/_errno_t.h
c_ctback.o: /usr/include/sys/_types/_fd_def.h
c_ctback.o: /usr/include/sys/_types/_fd_setsize.h
c_ctback.o: /usr/include/sys/_types/_fd_set.h
c_ctback.o: /usr/include/sys/_types/_fd_clr.h
c_ctback.o: /usr/include/sys/_types/_fd_zero.h
c_ctback.o: /usr/include/sys/_types/_fd_isset.h
c_ctback.o: /usr/include/sys/_types/_fd_copy.h
c_ctback.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_ctback.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_ctback.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_ctback.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_ctback.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_ctback.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_ctback.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_ctback.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_ctback.o: /usr/include/sys/_pthread/_pthread_t.h
c_ctback.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_ctback.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_ctback.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_ctback.o: ../../../.././include/ncarg/c.h
c_ctcica.o: /usr/include/stdlib.h /usr/include/Availability.h
c_ctcica.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_ctcica.o: /usr/include/sys/_types.h /usr/include/sys/cdefs.h
c_ctcica.o: /usr/include/sys/_symbol_aliasing.h
c_ctcica.o: /usr/include/sys/_posix_availability.h
c_ctcica.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_ctcica.o: /usr/include/sys/_pthread/_pthread_types.h
c_ctcica.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
c_ctcica.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
c_ctcica.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
c_ctcica.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
c_ctcica.o: /usr/include/i386/_mcontext.h
c_ctcica.o: /usr/include/mach/machine/_structs.h
c_ctcica.o: /usr/include/mach/i386/_structs.h /usr/include/machine/types.h
c_ctcica.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_ctcica.o: /usr/include/sys/_types/_int16_t.h
c_ctcica.o: /usr/include/sys/_types/_int32_t.h
c_ctcica.o: /usr/include/sys/_types/_int64_t.h
c_ctcica.o: /usr/include/sys/_types/_u_int8_t.h
c_ctcica.o: /usr/include/sys/_types/_u_int16_t.h
c_ctcica.o: /usr/include/sys/_types/_u_int32_t.h
c_ctcica.o: /usr/include/sys/_types/_u_int64_t.h
c_ctcica.o: /usr/include/sys/_types/_intptr_t.h
c_ctcica.o: /usr/include/sys/_types/_uintptr_t.h
c_ctcica.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_ctcica.o: /usr/include/sys/_types/_sigaltstack.h
c_ctcica.o: /usr/include/sys/_types/_ucontext.h
c_ctcica.o: /usr/include/sys/_types/_sigset_t.h
c_ctcica.o: /usr/include/sys/_types/_size_t.h
c_ctcica.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
c_ctcica.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
c_ctcica.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
c_ctcica.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
c_ctcica.o: /usr/include/_types/_uintmax_t.h
c_ctcica.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
c_ctcica.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
c_ctcica.o: /usr/include/libkern/_OSByteOrder.h
c_ctcica.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
c_ctcica.o: /usr/include/sys/_types/_ct_rune_t.h
c_ctcica.o: /usr/include/sys/_types/_rune_t.h
c_ctcica.o: /usr/include/sys/_types/_wchar_t.h
c_ctcica.o: /usr/include/sys/_types/_null.h /usr/include/sys/_types/_dev_t.h
c_ctcica.o: /usr/include/sys/_types/_mode_t.h
c_ctcica.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_ctcica.o: /usr/include/sys/_types/_va_list.h /usr/include/sys/stdio.h
c_ctcica.o: /usr/include/sys/_types/_off_t.h
c_ctcica.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_ctcica.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_ctcica.o: /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
c_ctcica.o: /usr/include/sys/_types/_u_short.h
c_ctcica.o: /usr/include/sys/_types/_u_int.h
c_ctcica.o: /usr/include/sys/_types/_caddr_t.h
c_ctcica.o: /usr/include/sys/_types/_blkcnt_t.h
c_ctcica.o: /usr/include/sys/_types/_blksize_t.h
c_ctcica.o: /usr/include/sys/_types/_gid_t.h
c_ctcica.o: /usr/include/sys/_types/_in_addr_t.h
c_ctcica.o: /usr/include/sys/_types/_in_port_t.h
c_ctcica.o: /usr/include/sys/_types/_ino_t.h
c_ctcica.o: /usr/include/sys/_types/_ino64_t.h
c_ctcica.o: /usr/include/sys/_types/_key_t.h
c_ctcica.o: /usr/include/sys/_types/_nlink_t.h
c_ctcica.o: /usr/include/sys/_types/_clock_t.h
c_ctcica.o: /usr/include/sys/_types/_time_t.h
c_ctcica.o: /usr/include/sys/_types/_useconds_t.h
c_ctcica.o: /usr/include/sys/_types/_suseconds_t.h
c_ctcica.o: /usr/include/sys/_types/_rsize_t.h
c_ctcica.o: /usr/include/sys/_types/_errno_t.h
c_ctcica.o: /usr/include/sys/_types/_fd_def.h
c_ctcica.o: /usr/include/sys/_types/_fd_setsize.h
c_ctcica.o: /usr/include/sys/_types/_fd_set.h
c_ctcica.o: /usr/include/sys/_types/_fd_clr.h
c_ctcica.o: /usr/include/sys/_types/_fd_zero.h
c_ctcica.o: /usr/include/sys/_types/_fd_isset.h
c_ctcica.o: /usr/include/sys/_types/_fd_copy.h
c_ctcica.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_ctcica.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_ctcica.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_ctcica.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_ctcica.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_ctcica.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_ctcica.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_ctcica.o: /usr/include/sys/_pthread/_pthread_t.h
c_ctcica.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_ctcica.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_ctcica.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_ctcica.o: ../../../.././include/ncarg/c.h
c_ctclam.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_ctclam.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_ctclam.o: /usr/include/sys/_posix_availability.h
c_ctclam.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_ctclam.o: /usr/include/_types.h /usr/include/sys/_types.h
c_ctclam.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_ctclam.o: /usr/include/sys/_pthread/_pthread_types.h
c_ctclam.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_ctclam.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_ctclam.o: /usr/include/sys/_types/_int16_t.h
c_ctclam.o: /usr/include/sys/_types/_int32_t.h
c_ctclam.o: /usr/include/sys/_types/_int64_t.h
c_ctclam.o: /usr/include/sys/_types/_u_int8_t.h
c_ctclam.o: /usr/include/sys/_types/_u_int16_t.h
c_ctclam.o: /usr/include/sys/_types/_u_int32_t.h
c_ctclam.o: /usr/include/sys/_types/_u_int64_t.h
c_ctclam.o: /usr/include/sys/_types/_intptr_t.h
c_ctclam.o: /usr/include/sys/_types/_uintptr_t.h
c_ctclam.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_ctclam.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_ctclam.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_ctclam.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_ctclam.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_ctclam.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_ctclam.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_ctclam.o: /usr/include/libkern/i386/_OSByteOrder.h
c_ctclam.o: /usr/include/sys/_types/_u_char.h
c_ctclam.o: /usr/include/sys/_types/_u_short.h
c_ctclam.o: /usr/include/sys/_types/_u_int.h
c_ctclam.o: /usr/include/sys/_types/_caddr_t.h
c_ctclam.o: /usr/include/sys/_types/_dev_t.h
c_ctclam.o: /usr/include/sys/_types/_blkcnt_t.h
c_ctclam.o: /usr/include/sys/_types/_blksize_t.h
c_ctclam.o: /usr/include/sys/_types/_gid_t.h
c_ctclam.o: /usr/include/sys/_types/_in_addr_t.h
c_ctclam.o: /usr/include/sys/_types/_in_port_t.h
c_ctclam.o: /usr/include/sys/_types/_ino_t.h
c_ctclam.o: /usr/include/sys/_types/_ino64_t.h
c_ctclam.o: /usr/include/sys/_types/_key_t.h
c_ctclam.o: /usr/include/sys/_types/_mode_t.h
c_ctclam.o: /usr/include/sys/_types/_nlink_t.h
c_ctclam.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_ctclam.o: /usr/include/sys/_types/_uid_t.h
c_ctclam.o: /usr/include/sys/_types/_clock_t.h
c_ctclam.o: /usr/include/sys/_types/_time_t.h
c_ctclam.o: /usr/include/sys/_types/_useconds_t.h
c_ctclam.o: /usr/include/sys/_types/_suseconds_t.h
c_ctclam.o: /usr/include/sys/_types/_rsize_t.h
c_ctclam.o: /usr/include/sys/_types/_errno_t.h
c_ctclam.o: /usr/include/sys/_types/_fd_def.h
c_ctclam.o: /usr/include/sys/_types/_fd_setsize.h
c_ctclam.o: /usr/include/sys/_types/_fd_set.h
c_ctclam.o: /usr/include/sys/_types/_fd_clr.h
c_ctclam.o: /usr/include/sys/_types/_fd_zero.h
c_ctclam.o: /usr/include/sys/_types/_fd_isset.h
c_ctclam.o: /usr/include/sys/_types/_fd_copy.h
c_ctclam.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_ctclam.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_ctclam.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_ctclam.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_ctclam.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_ctclam.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_ctclam.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_ctclam.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_ctclam.o: /usr/include/sys/_pthread/_pthread_t.h
c_ctclam.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_ctclam.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_ctclam.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_ctclam.o: ../../../.././include/ncarg/c.h
c_ctcldm.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_ctcldm.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_ctcldm.o: /usr/include/sys/_posix_availability.h
c_ctcldm.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_ctcldm.o: /usr/include/_types.h /usr/include/sys/_types.h
c_ctcldm.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_ctcldm.o: /usr/include/sys/_pthread/_pthread_types.h
c_ctcldm.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_ctcldm.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_ctcldm.o: /usr/include/sys/_types/_int16_t.h
c_ctcldm.o: /usr/include/sys/_types/_int32_t.h
c_ctcldm.o: /usr/include/sys/_types/_int64_t.h
c_ctcldm.o: /usr/include/sys/_types/_u_int8_t.h
c_ctcldm.o: /usr/include/sys/_types/_u_int16_t.h
c_ctcldm.o: /usr/include/sys/_types/_u_int32_t.h
c_ctcldm.o: /usr/include/sys/_types/_u_int64_t.h
c_ctcldm.o: /usr/include/sys/_types/_intptr_t.h
c_ctcldm.o: /usr/include/sys/_types/_uintptr_t.h
c_ctcldm.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_ctcldm.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_ctcldm.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_ctcldm.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_ctcldm.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_ctcldm.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_ctcldm.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_ctcldm.o: /usr/include/libkern/i386/_OSByteOrder.h
c_ctcldm.o: /usr/include/sys/_types/_u_char.h
c_ctcldm.o: /usr/include/sys/_types/_u_short.h
c_ctcldm.o: /usr/include/sys/_types/_u_int.h
c_ctcldm.o: /usr/include/sys/_types/_caddr_t.h
c_ctcldm.o: /usr/include/sys/_types/_dev_t.h
c_ctcldm.o: /usr/include/sys/_types/_blkcnt_t.h
c_ctcldm.o: /usr/include/sys/_types/_blksize_t.h
c_ctcldm.o: /usr/include/sys/_types/_gid_t.h
c_ctcldm.o: /usr/include/sys/_types/_in_addr_t.h
c_ctcldm.o: /usr/include/sys/_types/_in_port_t.h
c_ctcldm.o: /usr/include/sys/_types/_ino_t.h
c_ctcldm.o: /usr/include/sys/_types/_ino64_t.h
c_ctcldm.o: /usr/include/sys/_types/_key_t.h
c_ctcldm.o: /usr/include/sys/_types/_mode_t.h
c_ctcldm.o: /usr/include/sys/_types/_nlink_t.h
c_ctcldm.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_ctcldm.o: /usr/include/sys/_types/_uid_t.h
c_ctcldm.o: /usr/include/sys/_types/_clock_t.h
c_ctcldm.o: /usr/include/sys/_types/_time_t.h
c_ctcldm.o: /usr/include/sys/_types/_useconds_t.h
c_ctcldm.o: /usr/include/sys/_types/_suseconds_t.h
c_ctcldm.o: /usr/include/sys/_types/_rsize_t.h
c_ctcldm.o: /usr/include/sys/_types/_errno_t.h
c_ctcldm.o: /usr/include/sys/_types/_fd_def.h
c_ctcldm.o: /usr/include/sys/_types/_fd_setsize.h
c_ctcldm.o: /usr/include/sys/_types/_fd_set.h
c_ctcldm.o: /usr/include/sys/_types/_fd_clr.h
c_ctcldm.o: /usr/include/sys/_types/_fd_zero.h
c_ctcldm.o: /usr/include/sys/_types/_fd_isset.h
c_ctcldm.o: /usr/include/sys/_types/_fd_copy.h
c_ctcldm.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_ctcldm.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_ctcldm.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_ctcldm.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_ctcldm.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_ctcldm.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_ctcldm.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_ctcldm.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_ctcldm.o: /usr/include/sys/_pthread/_pthread_t.h
c_ctcldm.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_ctcldm.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_ctcldm.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_ctcldm.o: ../../../.././include/ncarg/c.h
c_ctcldr.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_ctcldr.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_ctcldr.o: /usr/include/sys/_posix_availability.h
c_ctcldr.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_ctcldr.o: /usr/include/_types.h /usr/include/sys/_types.h
c_ctcldr.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_ctcldr.o: /usr/include/sys/_pthread/_pthread_types.h
c_ctcldr.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_ctcldr.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_ctcldr.o: /usr/include/sys/_types/_int16_t.h
c_ctcldr.o: /usr/include/sys/_types/_int32_t.h
c_ctcldr.o: /usr/include/sys/_types/_int64_t.h
c_ctcldr.o: /usr/include/sys/_types/_u_int8_t.h
c_ctcldr.o: /usr/include/sys/_types/_u_int16_t.h
c_ctcldr.o: /usr/include/sys/_types/_u_int32_t.h
c_ctcldr.o: /usr/include/sys/_types/_u_int64_t.h
c_ctcldr.o: /usr/include/sys/_types/_intptr_t.h
c_ctcldr.o: /usr/include/sys/_types/_uintptr_t.h
c_ctcldr.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_ctcldr.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_ctcldr.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_ctcldr.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_ctcldr.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_ctcldr.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_ctcldr.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_ctcldr.o: /usr/include/libkern/i386/_OSByteOrder.h
c_ctcldr.o: /usr/include/sys/_types/_u_char.h
c_ctcldr.o: /usr/include/sys/_types/_u_short.h
c_ctcldr.o: /usr/include/sys/_types/_u_int.h
c_ctcldr.o: /usr/include/sys/_types/_caddr_t.h
c_ctcldr.o: /usr/include/sys/_types/_dev_t.h
c_ctcldr.o: /usr/include/sys/_types/_blkcnt_t.h
c_ctcldr.o: /usr/include/sys/_types/_blksize_t.h
c_ctcldr.o: /usr/include/sys/_types/_gid_t.h
c_ctcldr.o: /usr/include/sys/_types/_in_addr_t.h
c_ctcldr.o: /usr/include/sys/_types/_in_port_t.h
c_ctcldr.o: /usr/include/sys/_types/_ino_t.h
c_ctcldr.o: /usr/include/sys/_types/_ino64_t.h
c_ctcldr.o: /usr/include/sys/_types/_key_t.h
c_ctcldr.o: /usr/include/sys/_types/_mode_t.h
c_ctcldr.o: /usr/include/sys/_types/_nlink_t.h
c_ctcldr.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_ctcldr.o: /usr/include/sys/_types/_uid_t.h
c_ctcldr.o: /usr/include/sys/_types/_clock_t.h
c_ctcldr.o: /usr/include/sys/_types/_time_t.h
c_ctcldr.o: /usr/include/sys/_types/_useconds_t.h
c_ctcldr.o: /usr/include/sys/_types/_suseconds_t.h
c_ctcldr.o: /usr/include/sys/_types/_rsize_t.h
c_ctcldr.o: /usr/include/sys/_types/_errno_t.h
c_ctcldr.o: /usr/include/sys/_types/_fd_def.h
c_ctcldr.o: /usr/include/sys/_types/_fd_setsize.h
c_ctcldr.o: /usr/include/sys/_types/_fd_set.h
c_ctcldr.o: /usr/include/sys/_types/_fd_clr.h
c_ctcldr.o: /usr/include/sys/_types/_fd_zero.h
c_ctcldr.o: /usr/include/sys/_types/_fd_isset.h
c_ctcldr.o: /usr/include/sys/_types/_fd_copy.h
c_ctcldr.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_ctcldr.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_ctcldr.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_ctcldr.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_ctcldr.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_ctcldr.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_ctcldr.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_ctcldr.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_ctcldr.o: /usr/include/sys/_pthread/_pthread_t.h
c_ctcldr.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_ctcldr.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_ctcldr.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_ctcldr.o: ../../../.././include/ncarg/c.h
c_ctcltr.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_ctcltr.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_ctcltr.o: /usr/include/sys/_posix_availability.h
c_ctcltr.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_ctcltr.o: /usr/include/_types.h /usr/include/sys/_types.h
c_ctcltr.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_ctcltr.o: /usr/include/sys/_pthread/_pthread_types.h
c_ctcltr.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_ctcltr.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_ctcltr.o: /usr/include/sys/_types/_int16_t.h
c_ctcltr.o: /usr/include/sys/_types/_int32_t.h
c_ctcltr.o: /usr/include/sys/_types/_int64_t.h
c_ctcltr.o: /usr/include/sys/_types/_u_int8_t.h
c_ctcltr.o: /usr/include/sys/_types/_u_int16_t.h
c_ctcltr.o: /usr/include/sys/_types/_u_int32_t.h
c_ctcltr.o: /usr/include/sys/_types/_u_int64_t.h
c_ctcltr.o: /usr/include/sys/_types/_intptr_t.h
c_ctcltr.o: /usr/include/sys/_types/_uintptr_t.h
c_ctcltr.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_ctcltr.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_ctcltr.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_ctcltr.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_ctcltr.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_ctcltr.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_ctcltr.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_ctcltr.o: /usr/include/libkern/i386/_OSByteOrder.h
c_ctcltr.o: /usr/include/sys/_types/_u_char.h
c_ctcltr.o: /usr/include/sys/_types/_u_short.h
c_ctcltr.o: /usr/include/sys/_types/_u_int.h
c_ctcltr.o: /usr/include/sys/_types/_caddr_t.h
c_ctcltr.o: /usr/include/sys/_types/_dev_t.h
c_ctcltr.o: /usr/include/sys/_types/_blkcnt_t.h
c_ctcltr.o: /usr/include/sys/_types/_blksize_t.h
c_ctcltr.o: /usr/include/sys/_types/_gid_t.h
c_ctcltr.o: /usr/include/sys/_types/_in_addr_t.h
c_ctcltr.o: /usr/include/sys/_types/_in_port_t.h
c_ctcltr.o: /usr/include/sys/_types/_ino_t.h
c_ctcltr.o: /usr/include/sys/_types/_ino64_t.h
c_ctcltr.o: /usr/include/sys/_types/_key_t.h
c_ctcltr.o: /usr/include/sys/_types/_mode_t.h
c_ctcltr.o: /usr/include/sys/_types/_nlink_t.h
c_ctcltr.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_ctcltr.o: /usr/include/sys/_types/_uid_t.h
c_ctcltr.o: /usr/include/sys/_types/_clock_t.h
c_ctcltr.o: /usr/include/sys/_types/_time_t.h
c_ctcltr.o: /usr/include/sys/_types/_useconds_t.h
c_ctcltr.o: /usr/include/sys/_types/_suseconds_t.h
c_ctcltr.o: /usr/include/sys/_types/_rsize_t.h
c_ctcltr.o: /usr/include/sys/_types/_errno_t.h
c_ctcltr.o: /usr/include/sys/_types/_fd_def.h
c_ctcltr.o: /usr/include/sys/_types/_fd_setsize.h
c_ctcltr.o: /usr/include/sys/_types/_fd_set.h
c_ctcltr.o: /usr/include/sys/_types/_fd_clr.h
c_ctcltr.o: /usr/include/sys/_types/_fd_zero.h
c_ctcltr.o: /usr/include/sys/_types/_fd_isset.h
c_ctcltr.o: /usr/include/sys/_types/_fd_copy.h
c_ctcltr.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_ctcltr.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_ctcltr.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_ctcltr.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_ctcltr.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_ctcltr.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_ctcltr.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_ctcltr.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_ctcltr.o: /usr/include/sys/_pthread/_pthread_t.h
c_ctcltr.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_ctcltr.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_ctcltr.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_ctcltr.o: ../../../.././include/ncarg/c.h
c_ctgetc.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_ctgetc.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_ctgetc.o: /usr/include/sys/_posix_availability.h
c_ctgetc.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_ctgetc.o: /usr/include/_types.h /usr/include/sys/_types.h
c_ctgetc.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_ctgetc.o: /usr/include/sys/_pthread/_pthread_types.h
c_ctgetc.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_ctgetc.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_ctgetc.o: /usr/include/sys/_types/_int16_t.h
c_ctgetc.o: /usr/include/sys/_types/_int32_t.h
c_ctgetc.o: /usr/include/sys/_types/_int64_t.h
c_ctgetc.o: /usr/include/sys/_types/_u_int8_t.h
c_ctgetc.o: /usr/include/sys/_types/_u_int16_t.h
c_ctgetc.o: /usr/include/sys/_types/_u_int32_t.h
c_ctgetc.o: /usr/include/sys/_types/_u_int64_t.h
c_ctgetc.o: /usr/include/sys/_types/_intptr_t.h
c_ctgetc.o: /usr/include/sys/_types/_uintptr_t.h
c_ctgetc.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_ctgetc.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_ctgetc.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_ctgetc.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_ctgetc.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_ctgetc.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_ctgetc.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_ctgetc.o: /usr/include/libkern/i386/_OSByteOrder.h
c_ctgetc.o: /usr/include/sys/_types/_u_char.h
c_ctgetc.o: /usr/include/sys/_types/_u_short.h
c_ctgetc.o: /usr/include/sys/_types/_u_int.h
c_ctgetc.o: /usr/include/sys/_types/_caddr_t.h
c_ctgetc.o: /usr/include/sys/_types/_dev_t.h
c_ctgetc.o: /usr/include/sys/_types/_blkcnt_t.h
c_ctgetc.o: /usr/include/sys/_types/_blksize_t.h
c_ctgetc.o: /usr/include/sys/_types/_gid_t.h
c_ctgetc.o: /usr/include/sys/_types/_in_addr_t.h
c_ctgetc.o: /usr/include/sys/_types/_in_port_t.h
c_ctgetc.o: /usr/include/sys/_types/_ino_t.h
c_ctgetc.o: /usr/include/sys/_types/_ino64_t.h
c_ctgetc.o: /usr/include/sys/_types/_key_t.h
c_ctgetc.o: /usr/include/sys/_types/_mode_t.h
c_ctgetc.o: /usr/include/sys/_types/_nlink_t.h
c_ctgetc.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_ctgetc.o: /usr/include/sys/_types/_uid_t.h
c_ctgetc.o: /usr/include/sys/_types/_clock_t.h
c_ctgetc.o: /usr/include/sys/_types/_time_t.h
c_ctgetc.o: /usr/include/sys/_types/_useconds_t.h
c_ctgetc.o: /usr/include/sys/_types/_suseconds_t.h
c_ctgetc.o: /usr/include/sys/_types/_rsize_t.h
c_ctgetc.o: /usr/include/sys/_types/_errno_t.h
c_ctgetc.o: /usr/include/sys/_types/_fd_def.h
c_ctgetc.o: /usr/include/sys/_types/_fd_setsize.h
c_ctgetc.o: /usr/include/sys/_types/_fd_set.h
c_ctgetc.o: /usr/include/sys/_types/_fd_clr.h
c_ctgetc.o: /usr/include/sys/_types/_fd_zero.h
c_ctgetc.o: /usr/include/sys/_types/_fd_isset.h
c_ctgetc.o: /usr/include/sys/_types/_fd_copy.h
c_ctgetc.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_ctgetc.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_ctgetc.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_ctgetc.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_ctgetc.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_ctgetc.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_ctgetc.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_ctgetc.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_ctgetc.o: /usr/include/sys/_pthread/_pthread_t.h
c_ctgetc.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_ctgetc.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_ctgetc.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_ctgetc.o: ../../../.././include/ncarg/c.h
c_ctgeti.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_ctgeti.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_ctgeti.o: /usr/include/sys/_posix_availability.h
c_ctgeti.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_ctgeti.o: /usr/include/_types.h /usr/include/sys/_types.h
c_ctgeti.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_ctgeti.o: /usr/include/sys/_pthread/_pthread_types.h
c_ctgeti.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_ctgeti.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_ctgeti.o: /usr/include/sys/_types/_int16_t.h
c_ctgeti.o: /usr/include/sys/_types/_int32_t.h
c_ctgeti.o: /usr/include/sys/_types/_int64_t.h
c_ctgeti.o: /usr/include/sys/_types/_u_int8_t.h
c_ctgeti.o: /usr/include/sys/_types/_u_int16_t.h
c_ctgeti.o: /usr/include/sys/_types/_u_int32_t.h
c_ctgeti.o: /usr/include/sys/_types/_u_int64_t.h
c_ctgeti.o: /usr/include/sys/_types/_intptr_t.h
c_ctgeti.o: /usr/include/sys/_types/_uintptr_t.h
c_ctgeti.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_ctgeti.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_ctgeti.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_ctgeti.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_ctgeti.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_ctgeti.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_ctgeti.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_ctgeti.o: /usr/include/libkern/i386/_OSByteOrder.h
c_ctgeti.o: /usr/include/sys/_types/_u_char.h
c_ctgeti.o: /usr/include/sys/_types/_u_short.h
c_ctgeti.o: /usr/include/sys/_types/_u_int.h
c_ctgeti.o: /usr/include/sys/_types/_caddr_t.h
c_ctgeti.o: /usr/include/sys/_types/_dev_t.h
c_ctgeti.o: /usr/include/sys/_types/_blkcnt_t.h
c_ctgeti.o: /usr/include/sys/_types/_blksize_t.h
c_ctgeti.o: /usr/include/sys/_types/_gid_t.h
c_ctgeti.o: /usr/include/sys/_types/_in_addr_t.h
c_ctgeti.o: /usr/include/sys/_types/_in_port_t.h
c_ctgeti.o: /usr/include/sys/_types/_ino_t.h
c_ctgeti.o: /usr/include/sys/_types/_ino64_t.h
c_ctgeti.o: /usr/include/sys/_types/_key_t.h
c_ctgeti.o: /usr/include/sys/_types/_mode_t.h
c_ctgeti.o: /usr/include/sys/_types/_nlink_t.h
c_ctgeti.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_ctgeti.o: /usr/include/sys/_types/_uid_t.h
c_ctgeti.o: /usr/include/sys/_types/_clock_t.h
c_ctgeti.o: /usr/include/sys/_types/_time_t.h
c_ctgeti.o: /usr/include/sys/_types/_useconds_t.h
c_ctgeti.o: /usr/include/sys/_types/_suseconds_t.h
c_ctgeti.o: /usr/include/sys/_types/_rsize_t.h
c_ctgeti.o: /usr/include/sys/_types/_errno_t.h
c_ctgeti.o: /usr/include/sys/_types/_fd_def.h
c_ctgeti.o: /usr/include/sys/_types/_fd_setsize.h
c_ctgeti.o: /usr/include/sys/_types/_fd_set.h
c_ctgeti.o: /usr/include/sys/_types/_fd_clr.h
c_ctgeti.o: /usr/include/sys/_types/_fd_zero.h
c_ctgeti.o: /usr/include/sys/_types/_fd_isset.h
c_ctgeti.o: /usr/include/sys/_types/_fd_copy.h
c_ctgeti.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_ctgeti.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_ctgeti.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_ctgeti.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_ctgeti.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_ctgeti.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_ctgeti.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_ctgeti.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_ctgeti.o: /usr/include/sys/_pthread/_pthread_t.h
c_ctgeti.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_ctgeti.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_ctgeti.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_ctgeti.o: ../../../.././include/ncarg/c.h
c_ctgetr.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_ctgetr.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_ctgetr.o: /usr/include/sys/_posix_availability.h
c_ctgetr.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_ctgetr.o: /usr/include/_types.h /usr/include/sys/_types.h
c_ctgetr.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_ctgetr.o: /usr/include/sys/_pthread/_pthread_types.h
c_ctgetr.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_ctgetr.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_ctgetr.o: /usr/include/sys/_types/_int16_t.h
c_ctgetr.o: /usr/include/sys/_types/_int32_t.h
c_ctgetr.o: /usr/include/sys/_types/_int64_t.h
c_ctgetr.o: /usr/include/sys/_types/_u_int8_t.h
c_ctgetr.o: /usr/include/sys/_types/_u_int16_t.h
c_ctgetr.o: /usr/include/sys/_types/_u_int32_t.h
c_ctgetr.o: /usr/include/sys/_types/_u_int64_t.h
c_ctgetr.o: /usr/include/sys/_types/_intptr_t.h
c_ctgetr.o: /usr/include/sys/_types/_uintptr_t.h
c_ctgetr.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_ctgetr.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_ctgetr.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_ctgetr.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_ctgetr.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_ctgetr.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_ctgetr.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_ctgetr.o: /usr/include/libkern/i386/_OSByteOrder.h
c_ctgetr.o: /usr/include/sys/_types/_u_char.h
c_ctgetr.o: /usr/include/sys/_types/_u_short.h
c_ctgetr.o: /usr/include/sys/_types/_u_int.h
c_ctgetr.o: /usr/include/sys/_types/_caddr_t.h
c_ctgetr.o: /usr/include/sys/_types/_dev_t.h
c_ctgetr.o: /usr/include/sys/_types/_blkcnt_t.h
c_ctgetr.o: /usr/include/sys/_types/_blksize_t.h
c_ctgetr.o: /usr/include/sys/_types/_gid_t.h
c_ctgetr.o: /usr/include/sys/_types/_in_addr_t.h
c_ctgetr.o: /usr/include/sys/_types/_in_port_t.h
c_ctgetr.o: /usr/include/sys/_types/_ino_t.h
c_ctgetr.o: /usr/include/sys/_types/_ino64_t.h
c_ctgetr.o: /usr/include/sys/_types/_key_t.h
c_ctgetr.o: /usr/include/sys/_types/_mode_t.h
c_ctgetr.o: /usr/include/sys/_types/_nlink_t.h
c_ctgetr.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_ctgetr.o: /usr/include/sys/_types/_uid_t.h
c_ctgetr.o: /usr/include/sys/_types/_clock_t.h
c_ctgetr.o: /usr/include/sys/_types/_time_t.h
c_ctgetr.o: /usr/include/sys/_types/_useconds_t.h
c_ctgetr.o: /usr/include/sys/_types/_suseconds_t.h
c_ctgetr.o: /usr/include/sys/_types/_rsize_t.h
c_ctgetr.o: /usr/include/sys/_types/_errno_t.h
c_ctgetr.o: /usr/include/sys/_types/_fd_def.h
c_ctgetr.o: /usr/include/sys/_types/_fd_setsize.h
c_ctgetr.o: /usr/include/sys/_types/_fd_set.h
c_ctgetr.o: /usr/include/sys/_types/_fd_clr.h
c_ctgetr.o: /usr/include/sys/_types/_fd_zero.h
c_ctgetr.o: /usr/include/sys/_types/_fd_isset.h
c_ctgetr.o: /usr/include/sys/_types/_fd_copy.h
c_ctgetr.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_ctgetr.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_ctgetr.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_ctgetr.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_ctgetr.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_ctgetr.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_ctgetr.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_ctgetr.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_ctgetr.o: /usr/include/sys/_pthread/_pthread_t.h
c_ctgetr.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_ctgetr.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_ctgetr.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_ctgetr.o: ../../../.././include/ncarg/c.h
c_ctlbam.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_ctlbam.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_ctlbam.o: /usr/include/sys/_posix_availability.h
c_ctlbam.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_ctlbam.o: /usr/include/_types.h /usr/include/sys/_types.h
c_ctlbam.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_ctlbam.o: /usr/include/sys/_pthread/_pthread_types.h
c_ctlbam.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_ctlbam.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_ctlbam.o: /usr/include/sys/_types/_int16_t.h
c_ctlbam.o: /usr/include/sys/_types/_int32_t.h
c_ctlbam.o: /usr/include/sys/_types/_int64_t.h
c_ctlbam.o: /usr/include/sys/_types/_u_int8_t.h
c_ctlbam.o: /usr/include/sys/_types/_u_int16_t.h
c_ctlbam.o: /usr/include/sys/_types/_u_int32_t.h
c_ctlbam.o: /usr/include/sys/_types/_u_int64_t.h
c_ctlbam.o: /usr/include/sys/_types/_intptr_t.h
c_ctlbam.o: /usr/include/sys/_types/_uintptr_t.h
c_ctlbam.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_ctlbam.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_ctlbam.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_ctlbam.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_ctlbam.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_ctlbam.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_ctlbam.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_ctlbam.o: /usr/include/libkern/i386/_OSByteOrder.h
c_ctlbam.o: /usr/include/sys/_types/_u_char.h
c_ctlbam.o: /usr/include/sys/_types/_u_short.h
c_ctlbam.o: /usr/include/sys/_types/_u_int.h
c_ctlbam.o: /usr/include/sys/_types/_caddr_t.h
c_ctlbam.o: /usr/include/sys/_types/_dev_t.h
c_ctlbam.o: /usr/include/sys/_types/_blkcnt_t.h
c_ctlbam.o: /usr/include/sys/_types/_blksize_t.h
c_ctlbam.o: /usr/include/sys/_types/_gid_t.h
c_ctlbam.o: /usr/include/sys/_types/_in_addr_t.h
c_ctlbam.o: /usr/include/sys/_types/_in_port_t.h
c_ctlbam.o: /usr/include/sys/_types/_ino_t.h
c_ctlbam.o: /usr/include/sys/_types/_ino64_t.h
c_ctlbam.o: /usr/include/sys/_types/_key_t.h
c_ctlbam.o: /usr/include/sys/_types/_mode_t.h
c_ctlbam.o: /usr/include/sys/_types/_nlink_t.h
c_ctlbam.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_ctlbam.o: /usr/include/sys/_types/_uid_t.h
c_ctlbam.o: /usr/include/sys/_types/_clock_t.h
c_ctlbam.o: /usr/include/sys/_types/_time_t.h
c_ctlbam.o: /usr/include/sys/_types/_useconds_t.h
c_ctlbam.o: /usr/include/sys/_types/_suseconds_t.h
c_ctlbam.o: /usr/include/sys/_types/_rsize_t.h
c_ctlbam.o: /usr/include/sys/_types/_errno_t.h
c_ctlbam.o: /usr/include/sys/_types/_fd_def.h
c_ctlbam.o: /usr/include/sys/_types/_fd_setsize.h
c_ctlbam.o: /usr/include/sys/_types/_fd_set.h
c_ctlbam.o: /usr/include/sys/_types/_fd_clr.h
c_ctlbam.o: /usr/include/sys/_types/_fd_zero.h
c_ctlbam.o: /usr/include/sys/_types/_fd_isset.h
c_ctlbam.o: /usr/include/sys/_types/_fd_copy.h
c_ctlbam.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_ctlbam.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_ctlbam.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_ctlbam.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_ctlbam.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_ctlbam.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_ctlbam.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_ctlbam.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_ctlbam.o: /usr/include/sys/_pthread/_pthread_t.h
c_ctlbam.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_ctlbam.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_ctlbam.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_ctlbam.o: ../../../.././include/ncarg/c.h
c_ctlbdr.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_ctlbdr.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_ctlbdr.o: /usr/include/sys/_posix_availability.h
c_ctlbdr.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_ctlbdr.o: /usr/include/_types.h /usr/include/sys/_types.h
c_ctlbdr.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_ctlbdr.o: /usr/include/sys/_pthread/_pthread_types.h
c_ctlbdr.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_ctlbdr.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_ctlbdr.o: /usr/include/sys/_types/_int16_t.h
c_ctlbdr.o: /usr/include/sys/_types/_int32_t.h
c_ctlbdr.o: /usr/include/sys/_types/_int64_t.h
c_ctlbdr.o: /usr/include/sys/_types/_u_int8_t.h
c_ctlbdr.o: /usr/include/sys/_types/_u_int16_t.h
c_ctlbdr.o: /usr/include/sys/_types/_u_int32_t.h
c_ctlbdr.o: /usr/include/sys/_types/_u_int64_t.h
c_ctlbdr.o: /usr/include/sys/_types/_intptr_t.h
c_ctlbdr.o: /usr/include/sys/_types/_uintptr_t.h
c_ctlbdr.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_ctlbdr.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_ctlbdr.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_ctlbdr.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_ctlbdr.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_ctlbdr.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_ctlbdr.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_ctlbdr.o: /usr/include/libkern/i386/_OSByteOrder.h
c_ctlbdr.o: /usr/include/sys/_types/_u_char.h
c_ctlbdr.o: /usr/include/sys/_types/_u_short.h
c_ctlbdr.o: /usr/include/sys/_types/_u_int.h
c_ctlbdr.o: /usr/include/sys/_types/_caddr_t.h
c_ctlbdr.o: /usr/include/sys/_types/_dev_t.h
c_ctlbdr.o: /usr/include/sys/_types/_blkcnt_t.h
c_ctlbdr.o: /usr/include/sys/_types/_blksize_t.h
c_ctlbdr.o: /usr/include/sys/_types/_gid_t.h
c_ctlbdr.o: /usr/include/sys/_types/_in_addr_t.h
c_ctlbdr.o: /usr/include/sys/_types/_in_port_t.h
c_ctlbdr.o: /usr/include/sys/_types/_ino_t.h
c_ctlbdr.o: /usr/include/sys/_types/_ino64_t.h
c_ctlbdr.o: /usr/include/sys/_types/_key_t.h
c_ctlbdr.o: /usr/include/sys/_types/_mode_t.h
c_ctlbdr.o: /usr/include/sys/_types/_nlink_t.h
c_ctlbdr.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_ctlbdr.o: /usr/include/sys/_types/_uid_t.h
c_ctlbdr.o: /usr/include/sys/_types/_clock_t.h
c_ctlbdr.o: /usr/include/sys/_types/_time_t.h
c_ctlbdr.o: /usr/include/sys/_types/_useconds_t.h
c_ctlbdr.o: /usr/include/sys/_types/_suseconds_t.h
c_ctlbdr.o: /usr/include/sys/_types/_rsize_t.h
c_ctlbdr.o: /usr/include/sys/_types/_errno_t.h
c_ctlbdr.o: /usr/include/sys/_types/_fd_def.h
c_ctlbdr.o: /usr/include/sys/_types/_fd_setsize.h
c_ctlbdr.o: /usr/include/sys/_types/_fd_set.h
c_ctlbdr.o: /usr/include/sys/_types/_fd_clr.h
c_ctlbdr.o: /usr/include/sys/_types/_fd_zero.h
c_ctlbdr.o: /usr/include/sys/_types/_fd_isset.h
c_ctlbdr.o: /usr/include/sys/_types/_fd_copy.h
c_ctlbdr.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_ctlbdr.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_ctlbdr.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_ctlbdr.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_ctlbdr.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_ctlbdr.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_ctlbdr.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_ctlbdr.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_ctlbdr.o: /usr/include/sys/_pthread/_pthread_t.h
c_ctlbdr.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_ctlbdr.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_ctlbdr.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_ctlbdr.o: ../../../.././include/ncarg/c.h
c_ctmesh.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_ctmesh.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_ctmesh.o: /usr/include/sys/_posix_availability.h
c_ctmesh.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_ctmesh.o: /usr/include/_types.h /usr/include/sys/_types.h
c_ctmesh.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_ctmesh.o: /usr/include/sys/_pthread/_pthread_types.h
c_ctmesh.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_ctmesh.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_ctmesh.o: /usr/include/sys/_types/_int16_t.h
c_ctmesh.o: /usr/include/sys/_types/_int32_t.h
c_ctmesh.o: /usr/include/sys/_types/_int64_t.h
c_ctmesh.o: /usr/include/sys/_types/_u_int8_t.h
c_ctmesh.o: /usr/include/sys/_types/_u_int16_t.h
c_ctmesh.o: /usr/include/sys/_types/_u_int32_t.h
c_ctmesh.o: /usr/include/sys/_types/_u_int64_t.h
c_ctmesh.o: /usr/include/sys/_types/_intptr_t.h
c_ctmesh.o: /usr/include/sys/_types/_uintptr_t.h
c_ctmesh.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_ctmesh.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_ctmesh.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_ctmesh.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_ctmesh.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_ctmesh.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_ctmesh.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_ctmesh.o: /usr/include/libkern/i386/_OSByteOrder.h
c_ctmesh.o: /usr/include/sys/_types/_u_char.h
c_ctmesh.o: /usr/include/sys/_types/_u_short.h
c_ctmesh.o: /usr/include/sys/_types/_u_int.h
c_ctmesh.o: /usr/include/sys/_types/_caddr_t.h
c_ctmesh.o: /usr/include/sys/_types/_dev_t.h
c_ctmesh.o: /usr/include/sys/_types/_blkcnt_t.h
c_ctmesh.o: /usr/include/sys/_types/_blksize_t.h
c_ctmesh.o: /usr/include/sys/_types/_gid_t.h
c_ctmesh.o: /usr/include/sys/_types/_in_addr_t.h
c_ctmesh.o: /usr/include/sys/_types/_in_port_t.h
c_ctmesh.o: /usr/include/sys/_types/_ino_t.h
c_ctmesh.o: /usr/include/sys/_types/_ino64_t.h
c_ctmesh.o: /usr/include/sys/_types/_key_t.h
c_ctmesh.o: /usr/include/sys/_types/_mode_t.h
c_ctmesh.o: /usr/include/sys/_types/_nlink_t.h
c_ctmesh.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_ctmesh.o: /usr/include/sys/_types/_uid_t.h
c_ctmesh.o: /usr/include/sys/_types/_clock_t.h
c_ctmesh.o: /usr/include/sys/_types/_time_t.h
c_ctmesh.o: /usr/include/sys/_types/_useconds_t.h
c_ctmesh.o: /usr/include/sys/_types/_suseconds_t.h
c_ctmesh.o: /usr/include/sys/_types/_rsize_t.h
c_ctmesh.o: /usr/include/sys/_types/_errno_t.h
c_ctmesh.o: /usr/include/sys/_types/_fd_def.h
c_ctmesh.o: /usr/include/sys/_types/_fd_setsize.h
c_ctmesh.o: /usr/include/sys/_types/_fd_set.h
c_ctmesh.o: /usr/include/sys/_types/_fd_clr.h
c_ctmesh.o: /usr/include/sys/_types/_fd_zero.h
c_ctmesh.o: /usr/include/sys/_types/_fd_isset.h
c_ctmesh.o: /usr/include/sys/_types/_fd_copy.h
c_ctmesh.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_ctmesh.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_ctmesh.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_ctmesh.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_ctmesh.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_ctmesh.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_ctmesh.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_ctmesh.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_ctmesh.o: /usr/include/sys/_pthread/_pthread_t.h
c_ctmesh.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_ctmesh.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_ctmesh.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_ctmesh.o: ../../../.././include/ncarg/c.h
c_ctmviw.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_ctmviw.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_ctmviw.o: /usr/include/sys/_posix_availability.h
c_ctmviw.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_ctmviw.o: /usr/include/_types.h /usr/include/sys/_types.h
c_ctmviw.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_ctmviw.o: /usr/include/sys/_pthread/_pthread_types.h
c_ctmviw.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_ctmviw.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_ctmviw.o: /usr/include/sys/_types/_int16_t.h
c_ctmviw.o: /usr/include/sys/_types/_int32_t.h
c_ctmviw.o: /usr/include/sys/_types/_int64_t.h
c_ctmviw.o: /usr/include/sys/_types/_u_int8_t.h
c_ctmviw.o: /usr/include/sys/_types/_u_int16_t.h
c_ctmviw.o: /usr/include/sys/_types/_u_int32_t.h
c_ctmviw.o: /usr/include/sys/_types/_u_int64_t.h
c_ctmviw.o: /usr/include/sys/_types/_intptr_t.h
c_ctmviw.o: /usr/include/sys/_types/_uintptr_t.h
c_ctmviw.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_ctmviw.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_ctmviw.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_ctmviw.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_ctmviw.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_ctmviw.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_ctmviw.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_ctmviw.o: /usr/include/libkern/i386/_OSByteOrder.h
c_ctmviw.o: /usr/include/sys/_types/_u_char.h
c_ctmviw.o: /usr/include/sys/_types/_u_short.h
c_ctmviw.o: /usr/include/sys/_types/_u_int.h
c_ctmviw.o: /usr/include/sys/_types/_caddr_t.h
c_ctmviw.o: /usr/include/sys/_types/_dev_t.h
c_ctmviw.o: /usr/include/sys/_types/_blkcnt_t.h
c_ctmviw.o: /usr/include/sys/_types/_blksize_t.h
c_ctmviw.o: /usr/include/sys/_types/_gid_t.h
c_ctmviw.o: /usr/include/sys/_types/_in_addr_t.h
c_ctmviw.o: /usr/include/sys/_types/_in_port_t.h
c_ctmviw.o: /usr/include/sys/_types/_ino_t.h
c_ctmviw.o: /usr/include/sys/_types/_ino64_t.h
c_ctmviw.o: /usr/include/sys/_types/_key_t.h
c_ctmviw.o: /usr/include/sys/_types/_mode_t.h
c_ctmviw.o: /usr/include/sys/_types/_nlink_t.h
c_ctmviw.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_ctmviw.o: /usr/include/sys/_types/_uid_t.h
c_ctmviw.o: /usr/include/sys/_types/_clock_t.h
c_ctmviw.o: /usr/include/sys/_types/_time_t.h
c_ctmviw.o: /usr/include/sys/_types/_useconds_t.h
c_ctmviw.o: /usr/include/sys/_types/_suseconds_t.h
c_ctmviw.o: /usr/include/sys/_types/_rsize_t.h
c_ctmviw.o: /usr/include/sys/_types/_errno_t.h
c_ctmviw.o: /usr/include/sys/_types/_fd_def.h
c_ctmviw.o: /usr/include/sys/_types/_fd_setsize.h
c_ctmviw.o: /usr/include/sys/_types/_fd_set.h
c_ctmviw.o: /usr/include/sys/_types/_fd_clr.h
c_ctmviw.o: /usr/include/sys/_types/_fd_zero.h
c_ctmviw.o: /usr/include/sys/_types/_fd_isset.h
c_ctmviw.o: /usr/include/sys/_types/_fd_copy.h
c_ctmviw.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_ctmviw.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_ctmviw.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_ctmviw.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_ctmviw.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_ctmviw.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_ctmviw.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_ctmviw.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_ctmviw.o: /usr/include/sys/_pthread/_pthread_t.h
c_ctmviw.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_ctmviw.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_ctmviw.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_ctmviw.o: ../../../.././include/ncarg/c.h
c_ctmvrw.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_ctmvrw.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_ctmvrw.o: /usr/include/sys/_posix_availability.h
c_ctmvrw.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_ctmvrw.o: /usr/include/_types.h /usr/include/sys/_types.h
c_ctmvrw.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_ctmvrw.o: /usr/include/sys/_pthread/_pthread_types.h
c_ctmvrw.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_ctmvrw.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_ctmvrw.o: /usr/include/sys/_types/_int16_t.h
c_ctmvrw.o: /usr/include/sys/_types/_int32_t.h
c_ctmvrw.o: /usr/include/sys/_types/_int64_t.h
c_ctmvrw.o: /usr/include/sys/_types/_u_int8_t.h
c_ctmvrw.o: /usr/include/sys/_types/_u_int16_t.h
c_ctmvrw.o: /usr/include/sys/_types/_u_int32_t.h
c_ctmvrw.o: /usr/include/sys/_types/_u_int64_t.h
c_ctmvrw.o: /usr/include/sys/_types/_intptr_t.h
c_ctmvrw.o: /usr/include/sys/_types/_uintptr_t.h
c_ctmvrw.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_ctmvrw.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_ctmvrw.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_ctmvrw.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_ctmvrw.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_ctmvrw.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_ctmvrw.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_ctmvrw.o: /usr/include/libkern/i386/_OSByteOrder.h
c_ctmvrw.o: /usr/include/sys/_types/_u_char.h
c_ctmvrw.o: /usr/include/sys/_types/_u_short.h
c_ctmvrw.o: /usr/include/sys/_types/_u_int.h
c_ctmvrw.o: /usr/include/sys/_types/_caddr_t.h
c_ctmvrw.o: /usr/include/sys/_types/_dev_t.h
c_ctmvrw.o: /usr/include/sys/_types/_blkcnt_t.h
c_ctmvrw.o: /usr/include/sys/_types/_blksize_t.h
c_ctmvrw.o: /usr/include/sys/_types/_gid_t.h
c_ctmvrw.o: /usr/include/sys/_types/_in_addr_t.h
c_ctmvrw.o: /usr/include/sys/_types/_in_port_t.h
c_ctmvrw.o: /usr/include/sys/_types/_ino_t.h
c_ctmvrw.o: /usr/include/sys/_types/_ino64_t.h
c_ctmvrw.o: /usr/include/sys/_types/_key_t.h
c_ctmvrw.o: /usr/include/sys/_types/_mode_t.h
c_ctmvrw.o: /usr/include/sys/_types/_nlink_t.h
c_ctmvrw.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_ctmvrw.o: /usr/include/sys/_types/_uid_t.h
c_ctmvrw.o: /usr/include/sys/_types/_clock_t.h
c_ctmvrw.o: /usr/include/sys/_types/_time_t.h
c_ctmvrw.o: /usr/include/sys/_types/_useconds_t.h
c_ctmvrw.o: /usr/include/sys/_types/_suseconds_t.h
c_ctmvrw.o: /usr/include/sys/_types/_rsize_t.h
c_ctmvrw.o: /usr/include/sys/_types/_errno_t.h
c_ctmvrw.o: /usr/include/sys/_types/_fd_def.h
c_ctmvrw.o: /usr/include/sys/_types/_fd_setsize.h
c_ctmvrw.o: /usr/include/sys/_types/_fd_set.h
c_ctmvrw.o: /usr/include/sys/_types/_fd_clr.h
c_ctmvrw.o: /usr/include/sys/_types/_fd_zero.h
c_ctmvrw.o: /usr/include/sys/_types/_fd_isset.h
c_ctmvrw.o: /usr/include/sys/_types/_fd_copy.h
c_ctmvrw.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_ctmvrw.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_ctmvrw.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_ctmvrw.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_ctmvrw.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_ctmvrw.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_ctmvrw.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_ctmvrw.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_ctmvrw.o: /usr/include/sys/_pthread/_pthread_t.h
c_ctmvrw.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_ctmvrw.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_ctmvrw.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_ctmvrw.o: ../../../.././include/ncarg/c.h
c_ctpkcl.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_ctpkcl.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_ctpkcl.o: /usr/include/sys/_posix_availability.h
c_ctpkcl.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_ctpkcl.o: /usr/include/_types.h /usr/include/sys/_types.h
c_ctpkcl.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_ctpkcl.o: /usr/include/sys/_pthread/_pthread_types.h
c_ctpkcl.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_ctpkcl.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_ctpkcl.o: /usr/include/sys/_types/_int16_t.h
c_ctpkcl.o: /usr/include/sys/_types/_int32_t.h
c_ctpkcl.o: /usr/include/sys/_types/_int64_t.h
c_ctpkcl.o: /usr/include/sys/_types/_u_int8_t.h
c_ctpkcl.o: /usr/include/sys/_types/_u_int16_t.h
c_ctpkcl.o: /usr/include/sys/_types/_u_int32_t.h
c_ctpkcl.o: /usr/include/sys/_types/_u_int64_t.h
c_ctpkcl.o: /usr/include/sys/_types/_intptr_t.h
c_ctpkcl.o: /usr/include/sys/_types/_uintptr_t.h
c_ctpkcl.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_ctpkcl.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_ctpkcl.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_ctpkcl.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_ctpkcl.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_ctpkcl.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_ctpkcl.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_ctpkcl.o: /usr/include/libkern/i386/_OSByteOrder.h
c_ctpkcl.o: /usr/include/sys/_types/_u_char.h
c_ctpkcl.o: /usr/include/sys/_types/_u_short.h
c_ctpkcl.o: /usr/include/sys/_types/_u_int.h
c_ctpkcl.o: /usr/include/sys/_types/_caddr_t.h
c_ctpkcl.o: /usr/include/sys/_types/_dev_t.h
c_ctpkcl.o: /usr/include/sys/_types/_blkcnt_t.h
c_ctpkcl.o: /usr/include/sys/_types/_blksize_t.h
c_ctpkcl.o: /usr/include/sys/_types/_gid_t.h
c_ctpkcl.o: /usr/include/sys/_types/_in_addr_t.h
c_ctpkcl.o: /usr/include/sys/_types/_in_port_t.h
c_ctpkcl.o: /usr/include/sys/_types/_ino_t.h
c_ctpkcl.o: /usr/include/sys/_types/_ino64_t.h
c_ctpkcl.o: /usr/include/sys/_types/_key_t.h
c_ctpkcl.o: /usr/include/sys/_types/_mode_t.h
c_ctpkcl.o: /usr/include/sys/_types/_nlink_t.h
c_ctpkcl.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_ctpkcl.o: /usr/include/sys/_types/_uid_t.h
c_ctpkcl.o: /usr/include/sys/_types/_clock_t.h
c_ctpkcl.o: /usr/include/sys/_types/_time_t.h
c_ctpkcl.o: /usr/include/sys/_types/_useconds_t.h
c_ctpkcl.o: /usr/include/sys/_types/_suseconds_t.h
c_ctpkcl.o: /usr/include/sys/_types/_rsize_t.h
c_ctpkcl.o: /usr/include/sys/_types/_errno_t.h
c_ctpkcl.o: /usr/include/sys/_types/_fd_def.h
c_ctpkcl.o: /usr/include/sys/_types/_fd_setsize.h
c_ctpkcl.o: /usr/include/sys/_types/_fd_set.h
c_ctpkcl.o: /usr/include/sys/_types/_fd_clr.h
c_ctpkcl.o: /usr/include/sys/_types/_fd_zero.h
c_ctpkcl.o: /usr/include/sys/_types/_fd_isset.h
c_ctpkcl.o: /usr/include/sys/_types/_fd_copy.h
c_ctpkcl.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_ctpkcl.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_ctpkcl.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_ctpkcl.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_ctpkcl.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_ctpkcl.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_ctpkcl.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_ctpkcl.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_ctpkcl.o: /usr/include/sys/_pthread/_pthread_t.h
c_ctpkcl.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_ctpkcl.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_ctpkcl.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_ctpkcl.o: ../../../.././include/ncarg/c.h
c_ctpklb.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_ctpklb.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_ctpklb.o: /usr/include/sys/_posix_availability.h
c_ctpklb.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_ctpklb.o: /usr/include/_types.h /usr/include/sys/_types.h
c_ctpklb.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_ctpklb.o: /usr/include/sys/_pthread/_pthread_types.h
c_ctpklb.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_ctpklb.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_ctpklb.o: /usr/include/sys/_types/_int16_t.h
c_ctpklb.o: /usr/include/sys/_types/_int32_t.h
c_ctpklb.o: /usr/include/sys/_types/_int64_t.h
c_ctpklb.o: /usr/include/sys/_types/_u_int8_t.h
c_ctpklb.o: /usr/include/sys/_types/_u_int16_t.h
c_ctpklb.o: /usr/include/sys/_types/_u_int32_t.h
c_ctpklb.o: /usr/include/sys/_types/_u_int64_t.h
c_ctpklb.o: /usr/include/sys/_types/_intptr_t.h
c_ctpklb.o: /usr/include/sys/_types/_uintptr_t.h
c_ctpklb.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_ctpklb.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_ctpklb.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_ctpklb.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_ctpklb.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_ctpklb.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_ctpklb.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_ctpklb.o: /usr/include/libkern/i386/_OSByteOrder.h
c_ctpklb.o: /usr/include/sys/_types/_u_char.h
c_ctpklb.o: /usr/include/sys/_types/_u_short.h
c_ctpklb.o: /usr/include/sys/_types/_u_int.h
c_ctpklb.o: /usr/include/sys/_types/_caddr_t.h
c_ctpklb.o: /usr/include/sys/_types/_dev_t.h
c_ctpklb.o: /usr/include/sys/_types/_blkcnt_t.h
c_ctpklb.o: /usr/include/sys/_types/_blksize_t.h
c_ctpklb.o: /usr/include/sys/_types/_gid_t.h
c_ctpklb.o: /usr/include/sys/_types/_in_addr_t.h
c_ctpklb.o: /usr/include/sys/_types/_in_port_t.h
c_ctpklb.o: /usr/include/sys/_types/_ino_t.h
c_ctpklb.o: /usr/include/sys/_types/_ino64_t.h
c_ctpklb.o: /usr/include/sys/_types/_key_t.h
c_ctpklb.o: /usr/include/sys/_types/_mode_t.h
c_ctpklb.o: /usr/include/sys/_types/_nlink_t.h
c_ctpklb.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_ctpklb.o: /usr/include/sys/_types/_uid_t.h
c_ctpklb.o: /usr/include/sys/_types/_clock_t.h
c_ctpklb.o: /usr/include/sys/_types/_time_t.h
c_ctpklb.o: /usr/include/sys/_types/_useconds_t.h
c_ctpklb.o: /usr/include/sys/_types/_suseconds_t.h
c_ctpklb.o: /usr/include/sys/_types/_rsize_t.h
c_ctpklb.o: /usr/include/sys/_types/_errno_t.h
c_ctpklb.o: /usr/include/sys/_types/_fd_def.h
c_ctpklb.o: /usr/include/sys/_types/_fd_setsize.h
c_ctpklb.o: /usr/include/sys/_types/_fd_set.h
c_ctpklb.o: /usr/include/sys/_types/_fd_clr.h
c_ctpklb.o: /usr/include/sys/_types/_fd_zero.h
c_ctpklb.o: /usr/include/sys/_types/_fd_isset.h
c_ctpklb.o: /usr/include/sys/_types/_fd_copy.h
c_ctpklb.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_ctpklb.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_ctpklb.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_ctpklb.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_ctpklb.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_ctpklb.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_ctpklb.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_ctpklb.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_ctpklb.o: /usr/include/sys/_pthread/_pthread_t.h
c_ctpklb.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_ctpklb.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_ctpklb.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_ctpklb.o: ../../../.././include/ncarg/c.h
c_ctrset.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_ctrset.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_ctrset.o: /usr/include/sys/_posix_availability.h
c_ctrset.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_ctrset.o: /usr/include/_types.h /usr/include/sys/_types.h
c_ctrset.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_ctrset.o: /usr/include/sys/_pthread/_pthread_types.h
c_ctrset.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_ctrset.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_ctrset.o: /usr/include/sys/_types/_int16_t.h
c_ctrset.o: /usr/include/sys/_types/_int32_t.h
c_ctrset.o: /usr/include/sys/_types/_int64_t.h
c_ctrset.o: /usr/include/sys/_types/_u_int8_t.h
c_ctrset.o: /usr/include/sys/_types/_u_int16_t.h
c_ctrset.o: /usr/include/sys/_types/_u_int32_t.h
c_ctrset.o: /usr/include/sys/_types/_u_int64_t.h
c_ctrset.o: /usr/include/sys/_types/_intptr_t.h
c_ctrset.o: /usr/include/sys/_types/_uintptr_t.h
c_ctrset.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_ctrset.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_ctrset.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_ctrset.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_ctrset.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_ctrset.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_ctrset.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_ctrset.o: /usr/include/libkern/i386/_OSByteOrder.h
c_ctrset.o: /usr/include/sys/_types/_u_char.h
c_ctrset.o: /usr/include/sys/_types/_u_short.h
c_ctrset.o: /usr/include/sys/_types/_u_int.h
c_ctrset.o: /usr/include/sys/_types/_caddr_t.h
c_ctrset.o: /usr/include/sys/_types/_dev_t.h
c_ctrset.o: /usr/include/sys/_types/_blkcnt_t.h
c_ctrset.o: /usr/include/sys/_types/_blksize_t.h
c_ctrset.o: /usr/include/sys/_types/_gid_t.h
c_ctrset.o: /usr/include/sys/_types/_in_addr_t.h
c_ctrset.o: /usr/include/sys/_types/_in_port_t.h
c_ctrset.o: /usr/include/sys/_types/_ino_t.h
c_ctrset.o: /usr/include/sys/_types/_ino64_t.h
c_ctrset.o: /usr/include/sys/_types/_key_t.h
c_ctrset.o: /usr/include/sys/_types/_mode_t.h
c_ctrset.o: /usr/include/sys/_types/_nlink_t.h
c_ctrset.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_ctrset.o: /usr/include/sys/_types/_uid_t.h
c_ctrset.o: /usr/include/sys/_types/_clock_t.h
c_ctrset.o: /usr/include/sys/_types/_time_t.h
c_ctrset.o: /usr/include/sys/_types/_useconds_t.h
c_ctrset.o: /usr/include/sys/_types/_suseconds_t.h
c_ctrset.o: /usr/include/sys/_types/_rsize_t.h
c_ctrset.o: /usr/include/sys/_types/_errno_t.h
c_ctrset.o: /usr/include/sys/_types/_fd_def.h
c_ctrset.o: /usr/include/sys/_types/_fd_setsize.h
c_ctrset.o: /usr/include/sys/_types/_fd_set.h
c_ctrset.o: /usr/include/sys/_types/_fd_clr.h
c_ctrset.o: /usr/include/sys/_types/_fd_zero.h
c_ctrset.o: /usr/include/sys/_types/_fd_isset.h
c_ctrset.o: /usr/include/sys/_types/_fd_copy.h
c_ctrset.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_ctrset.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_ctrset.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_ctrset.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_ctrset.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_ctrset.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_ctrset.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_ctrset.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_ctrset.o: /usr/include/sys/_pthread/_pthread_t.h
c_ctrset.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_ctrset.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_ctrset.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_ctrset.o: ../../../.././include/ncarg/c.h
c_ctsetc.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_ctsetc.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_ctsetc.o: /usr/include/sys/_posix_availability.h
c_ctsetc.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_ctsetc.o: /usr/include/_types.h /usr/include/sys/_types.h
c_ctsetc.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_ctsetc.o: /usr/include/sys/_pthread/_pthread_types.h
c_ctsetc.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_ctsetc.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_ctsetc.o: /usr/include/sys/_types/_int16_t.h
c_ctsetc.o: /usr/include/sys/_types/_int32_t.h
c_ctsetc.o: /usr/include/sys/_types/_int64_t.h
c_ctsetc.o: /usr/include/sys/_types/_u_int8_t.h
c_ctsetc.o: /usr/include/sys/_types/_u_int16_t.h
c_ctsetc.o: /usr/include/sys/_types/_u_int32_t.h
c_ctsetc.o: /usr/include/sys/_types/_u_int64_t.h
c_ctsetc.o: /usr/include/sys/_types/_intptr_t.h
c_ctsetc.o: /usr/include/sys/_types/_uintptr_t.h
c_ctsetc.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_ctsetc.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_ctsetc.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_ctsetc.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_ctsetc.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_ctsetc.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_ctsetc.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_ctsetc.o: /usr/include/libkern/i386/_OSByteOrder.h
c_ctsetc.o: /usr/include/sys/_types/_u_char.h
c_ctsetc.o: /usr/include/sys/_types/_u_short.h
c_ctsetc.o: /usr/include/sys/_types/_u_int.h
c_ctsetc.o: /usr/include/sys/_types/_caddr_t.h
c_ctsetc.o: /usr/include/sys/_types/_dev_t.h
c_ctsetc.o: /usr/include/sys/_types/_blkcnt_t.h
c_ctsetc.o: /usr/include/sys/_types/_blksize_t.h
c_ctsetc.o: /usr/include/sys/_types/_gid_t.h
c_ctsetc.o: /usr/include/sys/_types/_in_addr_t.h
c_ctsetc.o: /usr/include/sys/_types/_in_port_t.h
c_ctsetc.o: /usr/include/sys/_types/_ino_t.h
c_ctsetc.o: /usr/include/sys/_types/_ino64_t.h
c_ctsetc.o: /usr/include/sys/_types/_key_t.h
c_ctsetc.o: /usr/include/sys/_types/_mode_t.h
c_ctsetc.o: /usr/include/sys/_types/_nlink_t.h
c_ctsetc.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_ctsetc.o: /usr/include/sys/_types/_uid_t.h
c_ctsetc.o: /usr/include/sys/_types/_clock_t.h
c_ctsetc.o: /usr/include/sys/_types/_time_t.h
c_ctsetc.o: /usr/include/sys/_types/_useconds_t.h
c_ctsetc.o: /usr/include/sys/_types/_suseconds_t.h
c_ctsetc.o: /usr/include/sys/_types/_rsize_t.h
c_ctsetc.o: /usr/include/sys/_types/_errno_t.h
c_ctsetc.o: /usr/include/sys/_types/_fd_def.h
c_ctsetc.o: /usr/include/sys/_types/_fd_setsize.h
c_ctsetc.o: /usr/include/sys/_types/_fd_set.h
c_ctsetc.o: /usr/include/sys/_types/_fd_clr.h
c_ctsetc.o: /usr/include/sys/_types/_fd_zero.h
c_ctsetc.o: /usr/include/sys/_types/_fd_isset.h
c_ctsetc.o: /usr/include/sys/_types/_fd_copy.h
c_ctsetc.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_ctsetc.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_ctsetc.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_ctsetc.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_ctsetc.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_ctsetc.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_ctsetc.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_ctsetc.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_ctsetc.o: /usr/include/sys/_pthread/_pthread_t.h
c_ctsetc.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_ctsetc.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_ctsetc.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_ctsetc.o: ../../../.././include/ncarg/c.h
c_ctseti.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_ctseti.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_ctseti.o: /usr/include/sys/_posix_availability.h
c_ctseti.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_ctseti.o: /usr/include/_types.h /usr/include/sys/_types.h
c_ctseti.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_ctseti.o: /usr/include/sys/_pthread/_pthread_types.h
c_ctseti.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_ctseti.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_ctseti.o: /usr/include/sys/_types/_int16_t.h
c_ctseti.o: /usr/include/sys/_types/_int32_t.h
c_ctseti.o: /usr/include/sys/_types/_int64_t.h
c_ctseti.o: /usr/include/sys/_types/_u_int8_t.h
c_ctseti.o: /usr/include/sys/_types/_u_int16_t.h
c_ctseti.o: /usr/include/sys/_types/_u_int32_t.h
c_ctseti.o: /usr/include/sys/_types/_u_int64_t.h
c_ctseti.o: /usr/include/sys/_types/_intptr_t.h
c_ctseti.o: /usr/include/sys/_types/_uintptr_t.h
c_ctseti.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_ctseti.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_ctseti.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_ctseti.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_ctseti.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_ctseti.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_ctseti.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_ctseti.o: /usr/include/libkern/i386/_OSByteOrder.h
c_ctseti.o: /usr/include/sys/_types/_u_char.h
c_ctseti.o: /usr/include/sys/_types/_u_short.h
c_ctseti.o: /usr/include/sys/_types/_u_int.h
c_ctseti.o: /usr/include/sys/_types/_caddr_t.h
c_ctseti.o: /usr/include/sys/_types/_dev_t.h
c_ctseti.o: /usr/include/sys/_types/_blkcnt_t.h
c_ctseti.o: /usr/include/sys/_types/_blksize_t.h
c_ctseti.o: /usr/include/sys/_types/_gid_t.h
c_ctseti.o: /usr/include/sys/_types/_in_addr_t.h
c_ctseti.o: /usr/include/sys/_types/_in_port_t.h
c_ctseti.o: /usr/include/sys/_types/_ino_t.h
c_ctseti.o: /usr/include/sys/_types/_ino64_t.h
c_ctseti.o: /usr/include/sys/_types/_key_t.h
c_ctseti.o: /usr/include/sys/_types/_mode_t.h
c_ctseti.o: /usr/include/sys/_types/_nlink_t.h
c_ctseti.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_ctseti.o: /usr/include/sys/_types/_uid_t.h
c_ctseti.o: /usr/include/sys/_types/_clock_t.h
c_ctseti.o: /usr/include/sys/_types/_time_t.h
c_ctseti.o: /usr/include/sys/_types/_useconds_t.h
c_ctseti.o: /usr/include/sys/_types/_suseconds_t.h
c_ctseti.o: /usr/include/sys/_types/_rsize_t.h
c_ctseti.o: /usr/include/sys/_types/_errno_t.h
c_ctseti.o: /usr/include/sys/_types/_fd_def.h
c_ctseti.o: /usr/include/sys/_types/_fd_setsize.h
c_ctseti.o: /usr/include/sys/_types/_fd_set.h
c_ctseti.o: /usr/include/sys/_types/_fd_clr.h
c_ctseti.o: /usr/include/sys/_types/_fd_zero.h
c_ctseti.o: /usr/include/sys/_types/_fd_isset.h
c_ctseti.o: /usr/include/sys/_types/_fd_copy.h
c_ctseti.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_ctseti.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_ctseti.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_ctseti.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_ctseti.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_ctseti.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_ctseti.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_ctseti.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_ctseti.o: /usr/include/sys/_pthread/_pthread_t.h
c_ctseti.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_ctseti.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_ctseti.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_ctseti.o: ../../../.././include/ncarg/c.h
c_ctsetr.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_ctsetr.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_ctsetr.o: /usr/include/sys/_posix_availability.h
c_ctsetr.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_ctsetr.o: /usr/include/_types.h /usr/include/sys/_types.h
c_ctsetr.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_ctsetr.o: /usr/include/sys/_pthread/_pthread_types.h
c_ctsetr.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_ctsetr.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_ctsetr.o: /usr/include/sys/_types/_int16_t.h
c_ctsetr.o: /usr/include/sys/_types/_int32_t.h
c_ctsetr.o: /usr/include/sys/_types/_int64_t.h
c_ctsetr.o: /usr/include/sys/_types/_u_int8_t.h
c_ctsetr.o: /usr/include/sys/_types/_u_int16_t.h
c_ctsetr.o: /usr/include/sys/_types/_u_int32_t.h
c_ctsetr.o: /usr/include/sys/_types/_u_int64_t.h
c_ctsetr.o: /usr/include/sys/_types/_intptr_t.h
c_ctsetr.o: /usr/include/sys/_types/_uintptr_t.h
c_ctsetr.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_ctsetr.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_ctsetr.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_ctsetr.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_ctsetr.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_ctsetr.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_ctsetr.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_ctsetr.o: /usr/include/libkern/i386/_OSByteOrder.h
c_ctsetr.o: /usr/include/sys/_types/_u_char.h
c_ctsetr.o: /usr/include/sys/_types/_u_short.h
c_ctsetr.o: /usr/include/sys/_types/_u_int.h
c_ctsetr.o: /usr/include/sys/_types/_caddr_t.h
c_ctsetr.o: /usr/include/sys/_types/_dev_t.h
c_ctsetr.o: /usr/include/sys/_types/_blkcnt_t.h
c_ctsetr.o: /usr/include/sys/_types/_blksize_t.h
c_ctsetr.o: /usr/include/sys/_types/_gid_t.h
c_ctsetr.o: /usr/include/sys/_types/_in_addr_t.h
c_ctsetr.o: /usr/include/sys/_types/_in_port_t.h
c_ctsetr.o: /usr/include/sys/_types/_ino_t.h
c_ctsetr.o: /usr/include/sys/_types/_ino64_t.h
c_ctsetr.o: /usr/include/sys/_types/_key_t.h
c_ctsetr.o: /usr/include/sys/_types/_mode_t.h
c_ctsetr.o: /usr/include/sys/_types/_nlink_t.h
c_ctsetr.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_ctsetr.o: /usr/include/sys/_types/_uid_t.h
c_ctsetr.o: /usr/include/sys/_types/_clock_t.h
c_ctsetr.o: /usr/include/sys/_types/_time_t.h
c_ctsetr.o: /usr/include/sys/_types/_useconds_t.h
c_ctsetr.o: /usr/include/sys/_types/_suseconds_t.h
c_ctsetr.o: /usr/include/sys/_types/_rsize_t.h
c_ctsetr.o: /usr/include/sys/_types/_errno_t.h
c_ctsetr.o: /usr/include/sys/_types/_fd_def.h
c_ctsetr.o: /usr/include/sys/_types/_fd_setsize.h
c_ctsetr.o: /usr/include/sys/_types/_fd_set.h
c_ctsetr.o: /usr/include/sys/_types/_fd_clr.h
c_ctsetr.o: /usr/include/sys/_types/_fd_zero.h
c_ctsetr.o: /usr/include/sys/_types/_fd_isset.h
c_ctsetr.o: /usr/include/sys/_types/_fd_copy.h
c_ctsetr.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_ctsetr.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_ctsetr.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_ctsetr.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_ctsetr.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_ctsetr.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_ctsetr.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_ctsetr.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_ctsetr.o: /usr/include/sys/_pthread/_pthread_t.h
c_ctsetr.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_ctsetr.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_ctsetr.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_ctsetr.o: ../../../.././include/ncarg/c.h
c_cttdbf.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_cttdbf.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_cttdbf.o: /usr/include/sys/_posix_availability.h
c_cttdbf.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_cttdbf.o: /usr/include/_types.h /usr/include/sys/_types.h
c_cttdbf.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_cttdbf.o: /usr/include/sys/_pthread/_pthread_types.h
c_cttdbf.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_cttdbf.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_cttdbf.o: /usr/include/sys/_types/_int16_t.h
c_cttdbf.o: /usr/include/sys/_types/_int32_t.h
c_cttdbf.o: /usr/include/sys/_types/_int64_t.h
c_cttdbf.o: /usr/include/sys/_types/_u_int8_t.h
c_cttdbf.o: /usr/include/sys/_types/_u_int16_t.h
c_cttdbf.o: /usr/include/sys/_types/_u_int32_t.h
c_cttdbf.o: /usr/include/sys/_types/_u_int64_t.h
c_cttdbf.o: /usr/include/sys/_types/_intptr_t.h
c_cttdbf.o: /usr/include/sys/_types/_uintptr_t.h
c_cttdbf.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_cttdbf.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_cttdbf.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_cttdbf.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_cttdbf.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_cttdbf.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_cttdbf.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_cttdbf.o: /usr/include/libkern/i386/_OSByteOrder.h
c_cttdbf.o: /usr/include/sys/_types/_u_char.h
c_cttdbf.o: /usr/include/sys/_types/_u_short.h
c_cttdbf.o: /usr/include/sys/_types/_u_int.h
c_cttdbf.o: /usr/include/sys/_types/_caddr_t.h
c_cttdbf.o: /usr/include/sys/_types/_dev_t.h
c_cttdbf.o: /usr/include/sys/_types/_blkcnt_t.h
c_cttdbf.o: /usr/include/sys/_types/_blksize_t.h
c_cttdbf.o: /usr/include/sys/_types/_gid_t.h
c_cttdbf.o: /usr/include/sys/_types/_in_addr_t.h
c_cttdbf.o: /usr/include/sys/_types/_in_port_t.h
c_cttdbf.o: /usr/include/sys/_types/_ino_t.h
c_cttdbf.o: /usr/include/sys/_types/_ino64_t.h
c_cttdbf.o: /usr/include/sys/_types/_key_t.h
c_cttdbf.o: /usr/include/sys/_types/_mode_t.h
c_cttdbf.o: /usr/include/sys/_types/_nlink_t.h
c_cttdbf.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_cttdbf.o: /usr/include/sys/_types/_uid_t.h
c_cttdbf.o: /usr/include/sys/_types/_clock_t.h
c_cttdbf.o: /usr/include/sys/_types/_time_t.h
c_cttdbf.o: /usr/include/sys/_types/_useconds_t.h
c_cttdbf.o: /usr/include/sys/_types/_suseconds_t.h
c_cttdbf.o: /usr/include/sys/_types/_rsize_t.h
c_cttdbf.o: /usr/include/sys/_types/_errno_t.h
c_cttdbf.o: /usr/include/sys/_types/_fd_def.h
c_cttdbf.o: /usr/include/sys/_types/_fd_setsize.h
c_cttdbf.o: /usr/include/sys/_types/_fd_set.h
c_cttdbf.o: /usr/include/sys/_types/_fd_clr.h
c_cttdbf.o: /usr/include/sys/_types/_fd_zero.h
c_cttdbf.o: /usr/include/sys/_types/_fd_isset.h
c_cttdbf.o: /usr/include/sys/_types/_fd_copy.h
c_cttdbf.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_cttdbf.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_cttdbf.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_cttdbf.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_cttdbf.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_cttdbf.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_cttdbf.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_cttdbf.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_cttdbf.o: /usr/include/sys/_pthread/_pthread_t.h
c_cttdbf.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_cttdbf.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_cttdbf.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_cttdbf.o: ../../../.././include/ncarg/c.h
c_cttdbm.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_cttdbm.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_cttdbm.o: /usr/include/sys/_posix_availability.h
c_cttdbm.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_cttdbm.o: /usr/include/_types.h /usr/include/sys/_types.h
c_cttdbm.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_cttdbm.o: /usr/include/sys/_pthread/_pthread_types.h
c_cttdbm.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_cttdbm.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_cttdbm.o: /usr/include/sys/_types/_int16_t.h
c_cttdbm.o: /usr/include/sys/_types/_int32_t.h
c_cttdbm.o: /usr/include/sys/_types/_int64_t.h
c_cttdbm.o: /usr/include/sys/_types/_u_int8_t.h
c_cttdbm.o: /usr/include/sys/_types/_u_int16_t.h
c_cttdbm.o: /usr/include/sys/_types/_u_int32_t.h
c_cttdbm.o: /usr/include/sys/_types/_u_int64_t.h
c_cttdbm.o: /usr/include/sys/_types/_intptr_t.h
c_cttdbm.o: /usr/include/sys/_types/_uintptr_t.h
c_cttdbm.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_cttdbm.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_cttdbm.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_cttdbm.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_cttdbm.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_cttdbm.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_cttdbm.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_cttdbm.o: /usr/include/libkern/i386/_OSByteOrder.h
c_cttdbm.o: /usr/include/sys/_types/_u_char.h
c_cttdbm.o: /usr/include/sys/_types/_u_short.h
c_cttdbm.o: /usr/include/sys/_types/_u_int.h
c_cttdbm.o: /usr/include/sys/_types/_caddr_t.h
c_cttdbm.o: /usr/include/sys/_types/_dev_t.h
c_cttdbm.o: /usr/include/sys/_types/_blkcnt_t.h
c_cttdbm.o: /usr/include/sys/_types/_blksize_t.h
c_cttdbm.o: /usr/include/sys/_types/_gid_t.h
c_cttdbm.o: /usr/include/sys/_types/_in_addr_t.h
c_cttdbm.o: /usr/include/sys/_types/_in_port_t.h
c_cttdbm.o: /usr/include/sys/_types/_ino_t.h
c_cttdbm.o: /usr/include/sys/_types/_ino64_t.h
c_cttdbm.o: /usr/include/sys/_types/_key_t.h
c_cttdbm.o: /usr/include/sys/_types/_mode_t.h
c_cttdbm.o: /usr/include/sys/_types/_nlink_t.h
c_cttdbm.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_cttdbm.o: /usr/include/sys/_types/_uid_t.h
c_cttdbm.o: /usr/include/sys/_types/_clock_t.h
c_cttdbm.o: /usr/include/sys/_types/_time_t.h
c_cttdbm.o: /usr/include/sys/_types/_useconds_t.h
c_cttdbm.o: /usr/include/sys/_types/_suseconds_t.h
c_cttdbm.o: /usr/include/sys/_types/_rsize_t.h
c_cttdbm.o: /usr/include/sys/_types/_errno_t.h
c_cttdbm.o: /usr/include/sys/_types/_fd_def.h
c_cttdbm.o: /usr/include/sys/_types/_fd_setsize.h
c_cttdbm.o: /usr/include/sys/_types/_fd_set.h
c_cttdbm.o: /usr/include/sys/_types/_fd_clr.h
c_cttdbm.o: /usr/include/sys/_types/_fd_zero.h
c_cttdbm.o: /usr/include/sys/_types/_fd_isset.h
c_cttdbm.o: /usr/include/sys/_types/_fd_copy.h
c_cttdbm.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_cttdbm.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_cttdbm.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_cttdbm.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_cttdbm.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_cttdbm.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_cttdbm.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_cttdbm.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_cttdbm.o: /usr/include/sys/_pthread/_pthread_t.h
c_cttdbm.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_cttdbm.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_cttdbm.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_cttdbm.o: ../../../.././include/ncarg/c.h
c_cttdca.o: /usr/include/stdlib.h /usr/include/Availability.h
c_cttdca.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_cttdca.o: /usr/include/sys/_types.h /usr/include/sys/cdefs.h
c_cttdca.o: /usr/include/sys/_symbol_aliasing.h
c_cttdca.o: /usr/include/sys/_posix_availability.h
c_cttdca.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_cttdca.o: /usr/include/sys/_pthread/_pthread_types.h
c_cttdca.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
c_cttdca.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
c_cttdca.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
c_cttdca.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
c_cttdca.o: /usr/include/i386/_mcontext.h
c_cttdca.o: /usr/include/mach/machine/_structs.h
c_cttdca.o: /usr/include/mach/i386/_structs.h /usr/include/machine/types.h
c_cttdca.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_cttdca.o: /usr/include/sys/_types/_int16_t.h
c_cttdca.o: /usr/include/sys/_types/_int32_t.h
c_cttdca.o: /usr/include/sys/_types/_int64_t.h
c_cttdca.o: /usr/include/sys/_types/_u_int8_t.h
c_cttdca.o: /usr/include/sys/_types/_u_int16_t.h
c_cttdca.o: /usr/include/sys/_types/_u_int32_t.h
c_cttdca.o: /usr/include/sys/_types/_u_int64_t.h
c_cttdca.o: /usr/include/sys/_types/_intptr_t.h
c_cttdca.o: /usr/include/sys/_types/_uintptr_t.h
c_cttdca.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_cttdca.o: /usr/include/sys/_types/_sigaltstack.h
c_cttdca.o: /usr/include/sys/_types/_ucontext.h
c_cttdca.o: /usr/include/sys/_types/_sigset_t.h
c_cttdca.o: /usr/include/sys/_types/_size_t.h
c_cttdca.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
c_cttdca.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
c_cttdca.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
c_cttdca.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
c_cttdca.o: /usr/include/_types/_uintmax_t.h
c_cttdca.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
c_cttdca.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
c_cttdca.o: /usr/include/libkern/_OSByteOrder.h
c_cttdca.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
c_cttdca.o: /usr/include/sys/_types/_ct_rune_t.h
c_cttdca.o: /usr/include/sys/_types/_rune_t.h
c_cttdca.o: /usr/include/sys/_types/_wchar_t.h
c_cttdca.o: /usr/include/sys/_types/_null.h /usr/include/sys/_types/_dev_t.h
c_cttdca.o: /usr/include/sys/_types/_mode_t.h
c_cttdca.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_cttdca.o: /usr/include/sys/_types/_va_list.h /usr/include/sys/stdio.h
c_cttdca.o: /usr/include/sys/_types/_off_t.h
c_cttdca.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_cttdca.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_cttdca.o: /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
c_cttdca.o: /usr/include/sys/_types/_u_short.h
c_cttdca.o: /usr/include/sys/_types/_u_int.h
c_cttdca.o: /usr/include/sys/_types/_caddr_t.h
c_cttdca.o: /usr/include/sys/_types/_blkcnt_t.h
c_cttdca.o: /usr/include/sys/_types/_blksize_t.h
c_cttdca.o: /usr/include/sys/_types/_gid_t.h
c_cttdca.o: /usr/include/sys/_types/_in_addr_t.h
c_cttdca.o: /usr/include/sys/_types/_in_port_t.h
c_cttdca.o: /usr/include/sys/_types/_ino_t.h
c_cttdca.o: /usr/include/sys/_types/_ino64_t.h
c_cttdca.o: /usr/include/sys/_types/_key_t.h
c_cttdca.o: /usr/include/sys/_types/_nlink_t.h
c_cttdca.o: /usr/include/sys/_types/_clock_t.h
c_cttdca.o: /usr/include/sys/_types/_time_t.h
c_cttdca.o: /usr/include/sys/_types/_useconds_t.h
c_cttdca.o: /usr/include/sys/_types/_suseconds_t.h
c_cttdca.o: /usr/include/sys/_types/_rsize_t.h
c_cttdca.o: /usr/include/sys/_types/_errno_t.h
c_cttdca.o: /usr/include/sys/_types/_fd_def.h
c_cttdca.o: /usr/include/sys/_types/_fd_setsize.h
c_cttdca.o: /usr/include/sys/_types/_fd_set.h
c_cttdca.o: /usr/include/sys/_types/_fd_clr.h
c_cttdca.o: /usr/include/sys/_types/_fd_zero.h
c_cttdca.o: /usr/include/sys/_types/_fd_isset.h
c_cttdca.o: /usr/include/sys/_types/_fd_copy.h
c_cttdca.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_cttdca.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_cttdca.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_cttdca.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_cttdca.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_cttdca.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_cttdca.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_cttdca.o: /usr/include/sys/_pthread/_pthread_t.h
c_cttdca.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_cttdca.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_cttdca.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_cttdca.o: ../../../.././include/ncarg/c.h
c_cttddm.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_cttddm.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_cttddm.o: /usr/include/sys/_posix_availability.h
c_cttddm.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_cttddm.o: /usr/include/_types.h /usr/include/sys/_types.h
c_cttddm.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_cttddm.o: /usr/include/sys/_pthread/_pthread_types.h
c_cttddm.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_cttddm.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_cttddm.o: /usr/include/sys/_types/_int16_t.h
c_cttddm.o: /usr/include/sys/_types/_int32_t.h
c_cttddm.o: /usr/include/sys/_types/_int64_t.h
c_cttddm.o: /usr/include/sys/_types/_u_int8_t.h
c_cttddm.o: /usr/include/sys/_types/_u_int16_t.h
c_cttddm.o: /usr/include/sys/_types/_u_int32_t.h
c_cttddm.o: /usr/include/sys/_types/_u_int64_t.h
c_cttddm.o: /usr/include/sys/_types/_intptr_t.h
c_cttddm.o: /usr/include/sys/_types/_uintptr_t.h
c_cttddm.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_cttddm.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_cttddm.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_cttddm.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_cttddm.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_cttddm.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_cttddm.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_cttddm.o: /usr/include/libkern/i386/_OSByteOrder.h
c_cttddm.o: /usr/include/sys/_types/_u_char.h
c_cttddm.o: /usr/include/sys/_types/_u_short.h
c_cttddm.o: /usr/include/sys/_types/_u_int.h
c_cttddm.o: /usr/include/sys/_types/_caddr_t.h
c_cttddm.o: /usr/include/sys/_types/_dev_t.h
c_cttddm.o: /usr/include/sys/_types/_blkcnt_t.h
c_cttddm.o: /usr/include/sys/_types/_blksize_t.h
c_cttddm.o: /usr/include/sys/_types/_gid_t.h
c_cttddm.o: /usr/include/sys/_types/_in_addr_t.h
c_cttddm.o: /usr/include/sys/_types/_in_port_t.h
c_cttddm.o: /usr/include/sys/_types/_ino_t.h
c_cttddm.o: /usr/include/sys/_types/_ino64_t.h
c_cttddm.o: /usr/include/sys/_types/_key_t.h
c_cttddm.o: /usr/include/sys/_types/_mode_t.h
c_cttddm.o: /usr/include/sys/_types/_nlink_t.h
c_cttddm.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_cttddm.o: /usr/include/sys/_types/_uid_t.h
c_cttddm.o: /usr/include/sys/_types/_clock_t.h
c_cttddm.o: /usr/include/sys/_types/_time_t.h
c_cttddm.o: /usr/include/sys/_types/_useconds_t.h
c_cttddm.o: /usr/include/sys/_types/_suseconds_t.h
c_cttddm.o: /usr/include/sys/_types/_rsize_t.h
c_cttddm.o: /usr/include/sys/_types/_errno_t.h
c_cttddm.o: /usr/include/sys/_types/_fd_def.h
c_cttddm.o: /usr/include/sys/_types/_fd_setsize.h
c_cttddm.o: /usr/include/sys/_types/_fd_set.h
c_cttddm.o: /usr/include/sys/_types/_fd_clr.h
c_cttddm.o: /usr/include/sys/_types/_fd_zero.h
c_cttddm.o: /usr/include/sys/_types/_fd_isset.h
c_cttddm.o: /usr/include/sys/_types/_fd_copy.h
c_cttddm.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_cttddm.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_cttddm.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_cttddm.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_cttddm.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_cttddm.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_cttddm.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_cttddm.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_cttddm.o: /usr/include/sys/_pthread/_pthread_t.h
c_cttddm.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_cttddm.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_cttddm.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_cttddm.o: ../../../.././include/ncarg/c.h
c_cttmrg.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_cttmrg.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_cttmrg.o: /usr/include/sys/_posix_availability.h
c_cttmrg.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_cttmrg.o: /usr/include/_types.h /usr/include/sys/_types.h
c_cttmrg.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_cttmrg.o: /usr/include/sys/_pthread/_pthread_types.h
c_cttmrg.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_cttmrg.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_cttmrg.o: /usr/include/sys/_types/_int16_t.h
c_cttmrg.o: /usr/include/sys/_types/_int32_t.h
c_cttmrg.o: /usr/include/sys/_types/_int64_t.h
c_cttmrg.o: /usr/include/sys/_types/_u_int8_t.h
c_cttmrg.o: /usr/include/sys/_types/_u_int16_t.h
c_cttmrg.o: /usr/include/sys/_types/_u_int32_t.h
c_cttmrg.o: /usr/include/sys/_types/_u_int64_t.h
c_cttmrg.o: /usr/include/sys/_types/_intptr_t.h
c_cttmrg.o: /usr/include/sys/_types/_uintptr_t.h
c_cttmrg.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_cttmrg.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_cttmrg.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_cttmrg.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_cttmrg.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_cttmrg.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_cttmrg.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_cttmrg.o: /usr/include/libkern/i386/_OSByteOrder.h
c_cttmrg.o: /usr/include/sys/_types/_u_char.h
c_cttmrg.o: /usr/include/sys/_types/_u_short.h
c_cttmrg.o: /usr/include/sys/_types/_u_int.h
c_cttmrg.o: /usr/include/sys/_types/_caddr_t.h
c_cttmrg.o: /usr/include/sys/_types/_dev_t.h
c_cttmrg.o: /usr/include/sys/_types/_blkcnt_t.h
c_cttmrg.o: /usr/include/sys/_types/_blksize_t.h
c_cttmrg.o: /usr/include/sys/_types/_gid_t.h
c_cttmrg.o: /usr/include/sys/_types/_in_addr_t.h
c_cttmrg.o: /usr/include/sys/_types/_in_port_t.h
c_cttmrg.o: /usr/include/sys/_types/_ino_t.h
c_cttmrg.o: /usr/include/sys/_types/_ino64_t.h
c_cttmrg.o: /usr/include/sys/_types/_key_t.h
c_cttmrg.o: /usr/include/sys/_types/_mode_t.h
c_cttmrg.o: /usr/include/sys/_types/_nlink_t.h
c_cttmrg.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_cttmrg.o: /usr/include/sys/_types/_uid_t.h
c_cttmrg.o: /usr/include/sys/_types/_clock_t.h
c_cttmrg.o: /usr/include/sys/_types/_time_t.h
c_cttmrg.o: /usr/include/sys/_types/_useconds_t.h
c_cttmrg.o: /usr/include/sys/_types/_suseconds_t.h
c_cttmrg.o: /usr/include/sys/_types/_rsize_t.h
c_cttmrg.o: /usr/include/sys/_types/_errno_t.h
c_cttmrg.o: /usr/include/sys/_types/_fd_def.h
c_cttmrg.o: /usr/include/sys/_types/_fd_setsize.h
c_cttmrg.o: /usr/include/sys/_types/_fd_set.h
c_cttmrg.o: /usr/include/sys/_types/_fd_clr.h
c_cttmrg.o: /usr/include/sys/_types/_fd_zero.h
c_cttmrg.o: /usr/include/sys/_types/_fd_isset.h
c_cttmrg.o: /usr/include/sys/_types/_fd_copy.h
c_cttmrg.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_cttmrg.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_cttmrg.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_cttmrg.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_cttmrg.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_cttmrg.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_cttmrg.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_cttmrg.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_cttmrg.o: /usr/include/sys/_pthread/_pthread_t.h
c_cttmrg.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_cttmrg.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_cttmrg.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_cttmrg.o: ../../../.././include/ncarg/c.h
c_cttmtl.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_cttmtl.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_cttmtl.o: /usr/include/sys/_posix_availability.h
c_cttmtl.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_cttmtl.o: /usr/include/_types.h /usr/include/sys/_types.h
c_cttmtl.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_cttmtl.o: /usr/include/sys/_pthread/_pthread_types.h
c_cttmtl.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_cttmtl.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_cttmtl.o: /usr/include/sys/_types/_int16_t.h
c_cttmtl.o: /usr/include/sys/_types/_int32_t.h
c_cttmtl.o: /usr/include/sys/_types/_int64_t.h
c_cttmtl.o: /usr/include/sys/_types/_u_int8_t.h
c_cttmtl.o: /usr/include/sys/_types/_u_int16_t.h
c_cttmtl.o: /usr/include/sys/_types/_u_int32_t.h
c_cttmtl.o: /usr/include/sys/_types/_u_int64_t.h
c_cttmtl.o: /usr/include/sys/_types/_intptr_t.h
c_cttmtl.o: /usr/include/sys/_types/_uintptr_t.h
c_cttmtl.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_cttmtl.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_cttmtl.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_cttmtl.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_cttmtl.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_cttmtl.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_cttmtl.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_cttmtl.o: /usr/include/libkern/i386/_OSByteOrder.h
c_cttmtl.o: /usr/include/sys/_types/_u_char.h
c_cttmtl.o: /usr/include/sys/_types/_u_short.h
c_cttmtl.o: /usr/include/sys/_types/_u_int.h
c_cttmtl.o: /usr/include/sys/_types/_caddr_t.h
c_cttmtl.o: /usr/include/sys/_types/_dev_t.h
c_cttmtl.o: /usr/include/sys/_types/_blkcnt_t.h
c_cttmtl.o: /usr/include/sys/_types/_blksize_t.h
c_cttmtl.o: /usr/include/sys/_types/_gid_t.h
c_cttmtl.o: /usr/include/sys/_types/_in_addr_t.h
c_cttmtl.o: /usr/include/sys/_types/_in_port_t.h
c_cttmtl.o: /usr/include/sys/_types/_ino_t.h
c_cttmtl.o: /usr/include/sys/_types/_ino64_t.h
c_cttmtl.o: /usr/include/sys/_types/_key_t.h
c_cttmtl.o: /usr/include/sys/_types/_mode_t.h
c_cttmtl.o: /usr/include/sys/_types/_nlink_t.h
c_cttmtl.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_cttmtl.o: /usr/include/sys/_types/_uid_t.h
c_cttmtl.o: /usr/include/sys/_types/_clock_t.h
c_cttmtl.o: /usr/include/sys/_types/_time_t.h
c_cttmtl.o: /usr/include/sys/_types/_useconds_t.h
c_cttmtl.o: /usr/include/sys/_types/_suseconds_t.h
c_cttmtl.o: /usr/include/sys/_types/_rsize_t.h
c_cttmtl.o: /usr/include/sys/_types/_errno_t.h
c_cttmtl.o: /usr/include/sys/_types/_fd_def.h
c_cttmtl.o: /usr/include/sys/_types/_fd_setsize.h
c_cttmtl.o: /usr/include/sys/_types/_fd_set.h
c_cttmtl.o: /usr/include/sys/_types/_fd_clr.h
c_cttmtl.o: /usr/include/sys/_types/_fd_zero.h
c_cttmtl.o: /usr/include/sys/_types/_fd_isset.h
c_cttmtl.o: /usr/include/sys/_types/_fd_copy.h
c_cttmtl.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_cttmtl.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_cttmtl.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_cttmtl.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_cttmtl.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_cttmtl.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_cttmtl.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_cttmtl.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_cttmtl.o: /usr/include/sys/_pthread/_pthread_t.h
c_cttmtl.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_cttmtl.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_cttmtl.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_cttmtl.o: ../../../.././include/ncarg/c.h
