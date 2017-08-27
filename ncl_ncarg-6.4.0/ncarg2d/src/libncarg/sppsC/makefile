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
CURRENT_DIR		= ./ncarg2d/src/libncarg/sppsC

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
#	$Id: yMakefile,v 1.1 1997-04-11 17:44:30 haley Exp $
#

MYNAME	= sppsC

SRCS = c_clsgks.c c_curve.c c_fl2int.c c_frame.c c_frstpt.c c_getset.c  	c_getusv.c c_line.c c_opngks.c c_plotif.c c_point.c c_points.c c_set.c  	c_setusv.c c_sflush.c c_vector.c c_wtstr.c c_plotit.c c_pwrit.c c_cfux.c  	c_cfuy.c c_cmfx.c c_cmfy.c c_cufx.c c_cufy.c c_cpux.c c_cpuy.c c_cpfx.c  	c_cpfy.c c_cmux.c c_cmuy.c c_kfpx.c c_kfpy.c c_kfmx.c c_kfmy.c c_kupx.c  	c_kupy.c c_kumx.c c_kumy.c c_kpmx.c c_kpmy.c c_kmpx.c c_kmpy.c

OBJS = c_clsgks.o c_curve.o c_fl2int.o c_frame.o c_frstpt.o c_getset.o  	c_getusv.o c_line.o c_opngks.o c_plotif.o c_point.o c_points.o c_set.o  	c_setusv.o c_sflush.o c_vector.o c_wtstr.o c_plotit.o c_pwrit.o c_cfux.o  	c_cfuy.o c_cmfx.o c_cmfy.o c_cufx.o c_cufy.o c_cpux.o c_cpuy.o c_cpfx.o  	c_cpfy.o c_cmux.o c_cmuy.o c_kfpx.o c_kfpy.o c_kfmx.o c_kfmy.o c_kupx.o  	c_kupy.o c_kumx.o c_kumy.o c_kpmx.o c_kpmy.o c_kmpx.o c_kmpy.o

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

c_clsgks.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_clsgks.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_clsgks.o: /usr/include/sys/_posix_availability.h
c_clsgks.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_clsgks.o: /usr/include/_types.h /usr/include/sys/_types.h
c_clsgks.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_clsgks.o: /usr/include/sys/_pthread/_pthread_types.h
c_clsgks.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_clsgks.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_clsgks.o: /usr/include/sys/_types/_int16_t.h
c_clsgks.o: /usr/include/sys/_types/_int32_t.h
c_clsgks.o: /usr/include/sys/_types/_int64_t.h
c_clsgks.o: /usr/include/sys/_types/_u_int8_t.h
c_clsgks.o: /usr/include/sys/_types/_u_int16_t.h
c_clsgks.o: /usr/include/sys/_types/_u_int32_t.h
c_clsgks.o: /usr/include/sys/_types/_u_int64_t.h
c_clsgks.o: /usr/include/sys/_types/_intptr_t.h
c_clsgks.o: /usr/include/sys/_types/_uintptr_t.h
c_clsgks.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_clsgks.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_clsgks.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_clsgks.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_clsgks.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_clsgks.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_clsgks.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_clsgks.o: /usr/include/libkern/i386/_OSByteOrder.h
c_clsgks.o: /usr/include/sys/_types/_u_char.h
c_clsgks.o: /usr/include/sys/_types/_u_short.h
c_clsgks.o: /usr/include/sys/_types/_u_int.h
c_clsgks.o: /usr/include/sys/_types/_caddr_t.h
c_clsgks.o: /usr/include/sys/_types/_dev_t.h
c_clsgks.o: /usr/include/sys/_types/_blkcnt_t.h
c_clsgks.o: /usr/include/sys/_types/_blksize_t.h
c_clsgks.o: /usr/include/sys/_types/_gid_t.h
c_clsgks.o: /usr/include/sys/_types/_in_addr_t.h
c_clsgks.o: /usr/include/sys/_types/_in_port_t.h
c_clsgks.o: /usr/include/sys/_types/_ino_t.h
c_clsgks.o: /usr/include/sys/_types/_ino64_t.h
c_clsgks.o: /usr/include/sys/_types/_key_t.h
c_clsgks.o: /usr/include/sys/_types/_mode_t.h
c_clsgks.o: /usr/include/sys/_types/_nlink_t.h
c_clsgks.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_clsgks.o: /usr/include/sys/_types/_uid_t.h
c_clsgks.o: /usr/include/sys/_types/_clock_t.h
c_clsgks.o: /usr/include/sys/_types/_time_t.h
c_clsgks.o: /usr/include/sys/_types/_useconds_t.h
c_clsgks.o: /usr/include/sys/_types/_suseconds_t.h
c_clsgks.o: /usr/include/sys/_types/_rsize_t.h
c_clsgks.o: /usr/include/sys/_types/_errno_t.h
c_clsgks.o: /usr/include/sys/_types/_fd_def.h
c_clsgks.o: /usr/include/sys/_types/_fd_setsize.h
c_clsgks.o: /usr/include/sys/_types/_fd_set.h
c_clsgks.o: /usr/include/sys/_types/_fd_clr.h
c_clsgks.o: /usr/include/sys/_types/_fd_zero.h
c_clsgks.o: /usr/include/sys/_types/_fd_isset.h
c_clsgks.o: /usr/include/sys/_types/_fd_copy.h
c_clsgks.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_clsgks.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_clsgks.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_clsgks.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_clsgks.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_clsgks.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_clsgks.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_clsgks.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_clsgks.o: /usr/include/sys/_pthread/_pthread_t.h
c_clsgks.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_clsgks.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_clsgks.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_clsgks.o: ../../../.././include/ncarg/c.h
c_curve.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_curve.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_curve.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_curve.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_curve.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_curve.o: /usr/include/i386/_types.h
c_curve.o: /usr/include/sys/_pthread/_pthread_types.h
c_curve.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_curve.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_curve.o: /usr/include/sys/_types/_int16_t.h
c_curve.o: /usr/include/sys/_types/_int32_t.h
c_curve.o: /usr/include/sys/_types/_int64_t.h
c_curve.o: /usr/include/sys/_types/_u_int8_t.h
c_curve.o: /usr/include/sys/_types/_u_int16_t.h
c_curve.o: /usr/include/sys/_types/_u_int32_t.h
c_curve.o: /usr/include/sys/_types/_u_int64_t.h
c_curve.o: /usr/include/sys/_types/_intptr_t.h
c_curve.o: /usr/include/sys/_types/_uintptr_t.h
c_curve.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_curve.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_curve.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_curve.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_curve.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_curve.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_curve.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_curve.o: /usr/include/libkern/i386/_OSByteOrder.h
c_curve.o: /usr/include/sys/_types/_u_char.h
c_curve.o: /usr/include/sys/_types/_u_short.h
c_curve.o: /usr/include/sys/_types/_u_int.h
c_curve.o: /usr/include/sys/_types/_caddr_t.h
c_curve.o: /usr/include/sys/_types/_dev_t.h
c_curve.o: /usr/include/sys/_types/_blkcnt_t.h
c_curve.o: /usr/include/sys/_types/_blksize_t.h
c_curve.o: /usr/include/sys/_types/_gid_t.h
c_curve.o: /usr/include/sys/_types/_in_addr_t.h
c_curve.o: /usr/include/sys/_types/_in_port_t.h
c_curve.o: /usr/include/sys/_types/_ino_t.h
c_curve.o: /usr/include/sys/_types/_ino64_t.h
c_curve.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_curve.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_curve.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_curve.o: /usr/include/sys/_types/_clock_t.h
c_curve.o: /usr/include/sys/_types/_time_t.h
c_curve.o: /usr/include/sys/_types/_useconds_t.h
c_curve.o: /usr/include/sys/_types/_suseconds_t.h
c_curve.o: /usr/include/sys/_types/_rsize_t.h
c_curve.o: /usr/include/sys/_types/_errno_t.h
c_curve.o: /usr/include/sys/_types/_fd_def.h
c_curve.o: /usr/include/sys/_types/_fd_setsize.h
c_curve.o: /usr/include/sys/_types/_fd_set.h
c_curve.o: /usr/include/sys/_types/_fd_clr.h
c_curve.o: /usr/include/sys/_types/_fd_zero.h
c_curve.o: /usr/include/sys/_types/_fd_isset.h
c_curve.o: /usr/include/sys/_types/_fd_copy.h
c_curve.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_curve.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_curve.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_curve.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_curve.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_curve.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_curve.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_curve.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_curve.o: /usr/include/sys/_pthread/_pthread_t.h
c_curve.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_curve.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_curve.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_curve.o: ../../../.././include/ncarg/c.h
c_fl2int.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_fl2int.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_fl2int.o: /usr/include/sys/_posix_availability.h
c_fl2int.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_fl2int.o: /usr/include/_types.h /usr/include/sys/_types.h
c_fl2int.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_fl2int.o: /usr/include/sys/_pthread/_pthread_types.h
c_fl2int.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_fl2int.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_fl2int.o: /usr/include/sys/_types/_int16_t.h
c_fl2int.o: /usr/include/sys/_types/_int32_t.h
c_fl2int.o: /usr/include/sys/_types/_int64_t.h
c_fl2int.o: /usr/include/sys/_types/_u_int8_t.h
c_fl2int.o: /usr/include/sys/_types/_u_int16_t.h
c_fl2int.o: /usr/include/sys/_types/_u_int32_t.h
c_fl2int.o: /usr/include/sys/_types/_u_int64_t.h
c_fl2int.o: /usr/include/sys/_types/_intptr_t.h
c_fl2int.o: /usr/include/sys/_types/_uintptr_t.h
c_fl2int.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_fl2int.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_fl2int.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_fl2int.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_fl2int.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_fl2int.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_fl2int.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_fl2int.o: /usr/include/libkern/i386/_OSByteOrder.h
c_fl2int.o: /usr/include/sys/_types/_u_char.h
c_fl2int.o: /usr/include/sys/_types/_u_short.h
c_fl2int.o: /usr/include/sys/_types/_u_int.h
c_fl2int.o: /usr/include/sys/_types/_caddr_t.h
c_fl2int.o: /usr/include/sys/_types/_dev_t.h
c_fl2int.o: /usr/include/sys/_types/_blkcnt_t.h
c_fl2int.o: /usr/include/sys/_types/_blksize_t.h
c_fl2int.o: /usr/include/sys/_types/_gid_t.h
c_fl2int.o: /usr/include/sys/_types/_in_addr_t.h
c_fl2int.o: /usr/include/sys/_types/_in_port_t.h
c_fl2int.o: /usr/include/sys/_types/_ino_t.h
c_fl2int.o: /usr/include/sys/_types/_ino64_t.h
c_fl2int.o: /usr/include/sys/_types/_key_t.h
c_fl2int.o: /usr/include/sys/_types/_mode_t.h
c_fl2int.o: /usr/include/sys/_types/_nlink_t.h
c_fl2int.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_fl2int.o: /usr/include/sys/_types/_uid_t.h
c_fl2int.o: /usr/include/sys/_types/_clock_t.h
c_fl2int.o: /usr/include/sys/_types/_time_t.h
c_fl2int.o: /usr/include/sys/_types/_useconds_t.h
c_fl2int.o: /usr/include/sys/_types/_suseconds_t.h
c_fl2int.o: /usr/include/sys/_types/_rsize_t.h
c_fl2int.o: /usr/include/sys/_types/_errno_t.h
c_fl2int.o: /usr/include/sys/_types/_fd_def.h
c_fl2int.o: /usr/include/sys/_types/_fd_setsize.h
c_fl2int.o: /usr/include/sys/_types/_fd_set.h
c_fl2int.o: /usr/include/sys/_types/_fd_clr.h
c_fl2int.o: /usr/include/sys/_types/_fd_zero.h
c_fl2int.o: /usr/include/sys/_types/_fd_isset.h
c_fl2int.o: /usr/include/sys/_types/_fd_copy.h
c_fl2int.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_fl2int.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_fl2int.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_fl2int.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_fl2int.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_fl2int.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_fl2int.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_fl2int.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_fl2int.o: /usr/include/sys/_pthread/_pthread_t.h
c_fl2int.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_fl2int.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_fl2int.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_fl2int.o: ../../../.././include/ncarg/c.h
c_frame.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_frame.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_frame.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_frame.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_frame.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_frame.o: /usr/include/i386/_types.h
c_frame.o: /usr/include/sys/_pthread/_pthread_types.h
c_frame.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_frame.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_frame.o: /usr/include/sys/_types/_int16_t.h
c_frame.o: /usr/include/sys/_types/_int32_t.h
c_frame.o: /usr/include/sys/_types/_int64_t.h
c_frame.o: /usr/include/sys/_types/_u_int8_t.h
c_frame.o: /usr/include/sys/_types/_u_int16_t.h
c_frame.o: /usr/include/sys/_types/_u_int32_t.h
c_frame.o: /usr/include/sys/_types/_u_int64_t.h
c_frame.o: /usr/include/sys/_types/_intptr_t.h
c_frame.o: /usr/include/sys/_types/_uintptr_t.h
c_frame.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_frame.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_frame.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_frame.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_frame.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_frame.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_frame.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_frame.o: /usr/include/libkern/i386/_OSByteOrder.h
c_frame.o: /usr/include/sys/_types/_u_char.h
c_frame.o: /usr/include/sys/_types/_u_short.h
c_frame.o: /usr/include/sys/_types/_u_int.h
c_frame.o: /usr/include/sys/_types/_caddr_t.h
c_frame.o: /usr/include/sys/_types/_dev_t.h
c_frame.o: /usr/include/sys/_types/_blkcnt_t.h
c_frame.o: /usr/include/sys/_types/_blksize_t.h
c_frame.o: /usr/include/sys/_types/_gid_t.h
c_frame.o: /usr/include/sys/_types/_in_addr_t.h
c_frame.o: /usr/include/sys/_types/_in_port_t.h
c_frame.o: /usr/include/sys/_types/_ino_t.h
c_frame.o: /usr/include/sys/_types/_ino64_t.h
c_frame.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_frame.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_frame.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_frame.o: /usr/include/sys/_types/_clock_t.h
c_frame.o: /usr/include/sys/_types/_time_t.h
c_frame.o: /usr/include/sys/_types/_useconds_t.h
c_frame.o: /usr/include/sys/_types/_suseconds_t.h
c_frame.o: /usr/include/sys/_types/_rsize_t.h
c_frame.o: /usr/include/sys/_types/_errno_t.h
c_frame.o: /usr/include/sys/_types/_fd_def.h
c_frame.o: /usr/include/sys/_types/_fd_setsize.h
c_frame.o: /usr/include/sys/_types/_fd_set.h
c_frame.o: /usr/include/sys/_types/_fd_clr.h
c_frame.o: /usr/include/sys/_types/_fd_zero.h
c_frame.o: /usr/include/sys/_types/_fd_isset.h
c_frame.o: /usr/include/sys/_types/_fd_copy.h
c_frame.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_frame.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_frame.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_frame.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_frame.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_frame.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_frame.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_frame.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_frame.o: /usr/include/sys/_pthread/_pthread_t.h
c_frame.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_frame.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_frame.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_frame.o: ../../../.././include/ncarg/c.h
c_frstpt.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_frstpt.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_frstpt.o: /usr/include/sys/_posix_availability.h
c_frstpt.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_frstpt.o: /usr/include/_types.h /usr/include/sys/_types.h
c_frstpt.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_frstpt.o: /usr/include/sys/_pthread/_pthread_types.h
c_frstpt.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_frstpt.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_frstpt.o: /usr/include/sys/_types/_int16_t.h
c_frstpt.o: /usr/include/sys/_types/_int32_t.h
c_frstpt.o: /usr/include/sys/_types/_int64_t.h
c_frstpt.o: /usr/include/sys/_types/_u_int8_t.h
c_frstpt.o: /usr/include/sys/_types/_u_int16_t.h
c_frstpt.o: /usr/include/sys/_types/_u_int32_t.h
c_frstpt.o: /usr/include/sys/_types/_u_int64_t.h
c_frstpt.o: /usr/include/sys/_types/_intptr_t.h
c_frstpt.o: /usr/include/sys/_types/_uintptr_t.h
c_frstpt.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_frstpt.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_frstpt.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_frstpt.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_frstpt.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_frstpt.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_frstpt.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_frstpt.o: /usr/include/libkern/i386/_OSByteOrder.h
c_frstpt.o: /usr/include/sys/_types/_u_char.h
c_frstpt.o: /usr/include/sys/_types/_u_short.h
c_frstpt.o: /usr/include/sys/_types/_u_int.h
c_frstpt.o: /usr/include/sys/_types/_caddr_t.h
c_frstpt.o: /usr/include/sys/_types/_dev_t.h
c_frstpt.o: /usr/include/sys/_types/_blkcnt_t.h
c_frstpt.o: /usr/include/sys/_types/_blksize_t.h
c_frstpt.o: /usr/include/sys/_types/_gid_t.h
c_frstpt.o: /usr/include/sys/_types/_in_addr_t.h
c_frstpt.o: /usr/include/sys/_types/_in_port_t.h
c_frstpt.o: /usr/include/sys/_types/_ino_t.h
c_frstpt.o: /usr/include/sys/_types/_ino64_t.h
c_frstpt.o: /usr/include/sys/_types/_key_t.h
c_frstpt.o: /usr/include/sys/_types/_mode_t.h
c_frstpt.o: /usr/include/sys/_types/_nlink_t.h
c_frstpt.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_frstpt.o: /usr/include/sys/_types/_uid_t.h
c_frstpt.o: /usr/include/sys/_types/_clock_t.h
c_frstpt.o: /usr/include/sys/_types/_time_t.h
c_frstpt.o: /usr/include/sys/_types/_useconds_t.h
c_frstpt.o: /usr/include/sys/_types/_suseconds_t.h
c_frstpt.o: /usr/include/sys/_types/_rsize_t.h
c_frstpt.o: /usr/include/sys/_types/_errno_t.h
c_frstpt.o: /usr/include/sys/_types/_fd_def.h
c_frstpt.o: /usr/include/sys/_types/_fd_setsize.h
c_frstpt.o: /usr/include/sys/_types/_fd_set.h
c_frstpt.o: /usr/include/sys/_types/_fd_clr.h
c_frstpt.o: /usr/include/sys/_types/_fd_zero.h
c_frstpt.o: /usr/include/sys/_types/_fd_isset.h
c_frstpt.o: /usr/include/sys/_types/_fd_copy.h
c_frstpt.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_frstpt.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_frstpt.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_frstpt.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_frstpt.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_frstpt.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_frstpt.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_frstpt.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_frstpt.o: /usr/include/sys/_pthread/_pthread_t.h
c_frstpt.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_frstpt.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_frstpt.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_frstpt.o: ../../../.././include/ncarg/c.h
c_getset.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_getset.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_getset.o: /usr/include/sys/_posix_availability.h
c_getset.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_getset.o: /usr/include/_types.h /usr/include/sys/_types.h
c_getset.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_getset.o: /usr/include/sys/_pthread/_pthread_types.h
c_getset.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_getset.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_getset.o: /usr/include/sys/_types/_int16_t.h
c_getset.o: /usr/include/sys/_types/_int32_t.h
c_getset.o: /usr/include/sys/_types/_int64_t.h
c_getset.o: /usr/include/sys/_types/_u_int8_t.h
c_getset.o: /usr/include/sys/_types/_u_int16_t.h
c_getset.o: /usr/include/sys/_types/_u_int32_t.h
c_getset.o: /usr/include/sys/_types/_u_int64_t.h
c_getset.o: /usr/include/sys/_types/_intptr_t.h
c_getset.o: /usr/include/sys/_types/_uintptr_t.h
c_getset.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_getset.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_getset.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_getset.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_getset.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_getset.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_getset.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_getset.o: /usr/include/libkern/i386/_OSByteOrder.h
c_getset.o: /usr/include/sys/_types/_u_char.h
c_getset.o: /usr/include/sys/_types/_u_short.h
c_getset.o: /usr/include/sys/_types/_u_int.h
c_getset.o: /usr/include/sys/_types/_caddr_t.h
c_getset.o: /usr/include/sys/_types/_dev_t.h
c_getset.o: /usr/include/sys/_types/_blkcnt_t.h
c_getset.o: /usr/include/sys/_types/_blksize_t.h
c_getset.o: /usr/include/sys/_types/_gid_t.h
c_getset.o: /usr/include/sys/_types/_in_addr_t.h
c_getset.o: /usr/include/sys/_types/_in_port_t.h
c_getset.o: /usr/include/sys/_types/_ino_t.h
c_getset.o: /usr/include/sys/_types/_ino64_t.h
c_getset.o: /usr/include/sys/_types/_key_t.h
c_getset.o: /usr/include/sys/_types/_mode_t.h
c_getset.o: /usr/include/sys/_types/_nlink_t.h
c_getset.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_getset.o: /usr/include/sys/_types/_uid_t.h
c_getset.o: /usr/include/sys/_types/_clock_t.h
c_getset.o: /usr/include/sys/_types/_time_t.h
c_getset.o: /usr/include/sys/_types/_useconds_t.h
c_getset.o: /usr/include/sys/_types/_suseconds_t.h
c_getset.o: /usr/include/sys/_types/_rsize_t.h
c_getset.o: /usr/include/sys/_types/_errno_t.h
c_getset.o: /usr/include/sys/_types/_fd_def.h
c_getset.o: /usr/include/sys/_types/_fd_setsize.h
c_getset.o: /usr/include/sys/_types/_fd_set.h
c_getset.o: /usr/include/sys/_types/_fd_clr.h
c_getset.o: /usr/include/sys/_types/_fd_zero.h
c_getset.o: /usr/include/sys/_types/_fd_isset.h
c_getset.o: /usr/include/sys/_types/_fd_copy.h
c_getset.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_getset.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_getset.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_getset.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_getset.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_getset.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_getset.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_getset.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_getset.o: /usr/include/sys/_pthread/_pthread_t.h
c_getset.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_getset.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_getset.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_getset.o: ../../../.././include/ncarg/c.h
c_getusv.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_getusv.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_getusv.o: /usr/include/sys/_posix_availability.h
c_getusv.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_getusv.o: /usr/include/_types.h /usr/include/sys/_types.h
c_getusv.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_getusv.o: /usr/include/sys/_pthread/_pthread_types.h
c_getusv.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_getusv.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_getusv.o: /usr/include/sys/_types/_int16_t.h
c_getusv.o: /usr/include/sys/_types/_int32_t.h
c_getusv.o: /usr/include/sys/_types/_int64_t.h
c_getusv.o: /usr/include/sys/_types/_u_int8_t.h
c_getusv.o: /usr/include/sys/_types/_u_int16_t.h
c_getusv.o: /usr/include/sys/_types/_u_int32_t.h
c_getusv.o: /usr/include/sys/_types/_u_int64_t.h
c_getusv.o: /usr/include/sys/_types/_intptr_t.h
c_getusv.o: /usr/include/sys/_types/_uintptr_t.h
c_getusv.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_getusv.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_getusv.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_getusv.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_getusv.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_getusv.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_getusv.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_getusv.o: /usr/include/libkern/i386/_OSByteOrder.h
c_getusv.o: /usr/include/sys/_types/_u_char.h
c_getusv.o: /usr/include/sys/_types/_u_short.h
c_getusv.o: /usr/include/sys/_types/_u_int.h
c_getusv.o: /usr/include/sys/_types/_caddr_t.h
c_getusv.o: /usr/include/sys/_types/_dev_t.h
c_getusv.o: /usr/include/sys/_types/_blkcnt_t.h
c_getusv.o: /usr/include/sys/_types/_blksize_t.h
c_getusv.o: /usr/include/sys/_types/_gid_t.h
c_getusv.o: /usr/include/sys/_types/_in_addr_t.h
c_getusv.o: /usr/include/sys/_types/_in_port_t.h
c_getusv.o: /usr/include/sys/_types/_ino_t.h
c_getusv.o: /usr/include/sys/_types/_ino64_t.h
c_getusv.o: /usr/include/sys/_types/_key_t.h
c_getusv.o: /usr/include/sys/_types/_mode_t.h
c_getusv.o: /usr/include/sys/_types/_nlink_t.h
c_getusv.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_getusv.o: /usr/include/sys/_types/_uid_t.h
c_getusv.o: /usr/include/sys/_types/_clock_t.h
c_getusv.o: /usr/include/sys/_types/_time_t.h
c_getusv.o: /usr/include/sys/_types/_useconds_t.h
c_getusv.o: /usr/include/sys/_types/_suseconds_t.h
c_getusv.o: /usr/include/sys/_types/_rsize_t.h
c_getusv.o: /usr/include/sys/_types/_errno_t.h
c_getusv.o: /usr/include/sys/_types/_fd_def.h
c_getusv.o: /usr/include/sys/_types/_fd_setsize.h
c_getusv.o: /usr/include/sys/_types/_fd_set.h
c_getusv.o: /usr/include/sys/_types/_fd_clr.h
c_getusv.o: /usr/include/sys/_types/_fd_zero.h
c_getusv.o: /usr/include/sys/_types/_fd_isset.h
c_getusv.o: /usr/include/sys/_types/_fd_copy.h
c_getusv.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_getusv.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_getusv.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_getusv.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_getusv.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_getusv.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_getusv.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_getusv.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_getusv.o: /usr/include/sys/_pthread/_pthread_t.h
c_getusv.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_getusv.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_getusv.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_getusv.o: ../../../.././include/ncarg/c.h
c_line.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_line.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_line.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_line.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_line.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_line.o: /usr/include/i386/_types.h
c_line.o: /usr/include/sys/_pthread/_pthread_types.h
c_line.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_line.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_line.o: /usr/include/sys/_types/_int16_t.h
c_line.o: /usr/include/sys/_types/_int32_t.h
c_line.o: /usr/include/sys/_types/_int64_t.h
c_line.o: /usr/include/sys/_types/_u_int8_t.h
c_line.o: /usr/include/sys/_types/_u_int16_t.h
c_line.o: /usr/include/sys/_types/_u_int32_t.h
c_line.o: /usr/include/sys/_types/_u_int64_t.h
c_line.o: /usr/include/sys/_types/_intptr_t.h
c_line.o: /usr/include/sys/_types/_uintptr_t.h
c_line.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_line.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_line.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_line.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_line.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_line.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_line.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_line.o: /usr/include/libkern/i386/_OSByteOrder.h
c_line.o: /usr/include/sys/_types/_u_char.h
c_line.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
c_line.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
c_line.o: /usr/include/sys/_types/_blkcnt_t.h
c_line.o: /usr/include/sys/_types/_blksize_t.h
c_line.o: /usr/include/sys/_types/_gid_t.h
c_line.o: /usr/include/sys/_types/_in_addr_t.h
c_line.o: /usr/include/sys/_types/_in_port_t.h
c_line.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
c_line.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_line.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_line.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_line.o: /usr/include/sys/_types/_clock_t.h
c_line.o: /usr/include/sys/_types/_time_t.h
c_line.o: /usr/include/sys/_types/_useconds_t.h
c_line.o: /usr/include/sys/_types/_suseconds_t.h
c_line.o: /usr/include/sys/_types/_rsize_t.h
c_line.o: /usr/include/sys/_types/_errno_t.h
c_line.o: /usr/include/sys/_types/_fd_def.h
c_line.o: /usr/include/sys/_types/_fd_setsize.h
c_line.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
c_line.o: /usr/include/sys/_types/_fd_zero.h
c_line.o: /usr/include/sys/_types/_fd_isset.h
c_line.o: /usr/include/sys/_types/_fd_copy.h
c_line.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_line.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_line.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_line.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_line.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_line.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_line.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_line.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_line.o: /usr/include/sys/_pthread/_pthread_t.h
c_line.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_line.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_line.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_line.o: ../../../.././include/ncarg/c.h
c_opngks.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_opngks.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_opngks.o: /usr/include/sys/_posix_availability.h
c_opngks.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_opngks.o: /usr/include/_types.h /usr/include/sys/_types.h
c_opngks.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_opngks.o: /usr/include/sys/_pthread/_pthread_types.h
c_opngks.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_opngks.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_opngks.o: /usr/include/sys/_types/_int16_t.h
c_opngks.o: /usr/include/sys/_types/_int32_t.h
c_opngks.o: /usr/include/sys/_types/_int64_t.h
c_opngks.o: /usr/include/sys/_types/_u_int8_t.h
c_opngks.o: /usr/include/sys/_types/_u_int16_t.h
c_opngks.o: /usr/include/sys/_types/_u_int32_t.h
c_opngks.o: /usr/include/sys/_types/_u_int64_t.h
c_opngks.o: /usr/include/sys/_types/_intptr_t.h
c_opngks.o: /usr/include/sys/_types/_uintptr_t.h
c_opngks.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_opngks.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_opngks.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_opngks.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_opngks.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_opngks.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_opngks.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_opngks.o: /usr/include/libkern/i386/_OSByteOrder.h
c_opngks.o: /usr/include/sys/_types/_u_char.h
c_opngks.o: /usr/include/sys/_types/_u_short.h
c_opngks.o: /usr/include/sys/_types/_u_int.h
c_opngks.o: /usr/include/sys/_types/_caddr_t.h
c_opngks.o: /usr/include/sys/_types/_dev_t.h
c_opngks.o: /usr/include/sys/_types/_blkcnt_t.h
c_opngks.o: /usr/include/sys/_types/_blksize_t.h
c_opngks.o: /usr/include/sys/_types/_gid_t.h
c_opngks.o: /usr/include/sys/_types/_in_addr_t.h
c_opngks.o: /usr/include/sys/_types/_in_port_t.h
c_opngks.o: /usr/include/sys/_types/_ino_t.h
c_opngks.o: /usr/include/sys/_types/_ino64_t.h
c_opngks.o: /usr/include/sys/_types/_key_t.h
c_opngks.o: /usr/include/sys/_types/_mode_t.h
c_opngks.o: /usr/include/sys/_types/_nlink_t.h
c_opngks.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_opngks.o: /usr/include/sys/_types/_uid_t.h
c_opngks.o: /usr/include/sys/_types/_clock_t.h
c_opngks.o: /usr/include/sys/_types/_time_t.h
c_opngks.o: /usr/include/sys/_types/_useconds_t.h
c_opngks.o: /usr/include/sys/_types/_suseconds_t.h
c_opngks.o: /usr/include/sys/_types/_rsize_t.h
c_opngks.o: /usr/include/sys/_types/_errno_t.h
c_opngks.o: /usr/include/sys/_types/_fd_def.h
c_opngks.o: /usr/include/sys/_types/_fd_setsize.h
c_opngks.o: /usr/include/sys/_types/_fd_set.h
c_opngks.o: /usr/include/sys/_types/_fd_clr.h
c_opngks.o: /usr/include/sys/_types/_fd_zero.h
c_opngks.o: /usr/include/sys/_types/_fd_isset.h
c_opngks.o: /usr/include/sys/_types/_fd_copy.h
c_opngks.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_opngks.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_opngks.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_opngks.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_opngks.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_opngks.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_opngks.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_opngks.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_opngks.o: /usr/include/sys/_pthread/_pthread_t.h
c_opngks.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_opngks.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_opngks.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_opngks.o: ../../../.././include/ncarg/c.h
c_plotif.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_plotif.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_plotif.o: /usr/include/sys/_posix_availability.h
c_plotif.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_plotif.o: /usr/include/_types.h /usr/include/sys/_types.h
c_plotif.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_plotif.o: /usr/include/sys/_pthread/_pthread_types.h
c_plotif.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_plotif.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_plotif.o: /usr/include/sys/_types/_int16_t.h
c_plotif.o: /usr/include/sys/_types/_int32_t.h
c_plotif.o: /usr/include/sys/_types/_int64_t.h
c_plotif.o: /usr/include/sys/_types/_u_int8_t.h
c_plotif.o: /usr/include/sys/_types/_u_int16_t.h
c_plotif.o: /usr/include/sys/_types/_u_int32_t.h
c_plotif.o: /usr/include/sys/_types/_u_int64_t.h
c_plotif.o: /usr/include/sys/_types/_intptr_t.h
c_plotif.o: /usr/include/sys/_types/_uintptr_t.h
c_plotif.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_plotif.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_plotif.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_plotif.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_plotif.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_plotif.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_plotif.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_plotif.o: /usr/include/libkern/i386/_OSByteOrder.h
c_plotif.o: /usr/include/sys/_types/_u_char.h
c_plotif.o: /usr/include/sys/_types/_u_short.h
c_plotif.o: /usr/include/sys/_types/_u_int.h
c_plotif.o: /usr/include/sys/_types/_caddr_t.h
c_plotif.o: /usr/include/sys/_types/_dev_t.h
c_plotif.o: /usr/include/sys/_types/_blkcnt_t.h
c_plotif.o: /usr/include/sys/_types/_blksize_t.h
c_plotif.o: /usr/include/sys/_types/_gid_t.h
c_plotif.o: /usr/include/sys/_types/_in_addr_t.h
c_plotif.o: /usr/include/sys/_types/_in_port_t.h
c_plotif.o: /usr/include/sys/_types/_ino_t.h
c_plotif.o: /usr/include/sys/_types/_ino64_t.h
c_plotif.o: /usr/include/sys/_types/_key_t.h
c_plotif.o: /usr/include/sys/_types/_mode_t.h
c_plotif.o: /usr/include/sys/_types/_nlink_t.h
c_plotif.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_plotif.o: /usr/include/sys/_types/_uid_t.h
c_plotif.o: /usr/include/sys/_types/_clock_t.h
c_plotif.o: /usr/include/sys/_types/_time_t.h
c_plotif.o: /usr/include/sys/_types/_useconds_t.h
c_plotif.o: /usr/include/sys/_types/_suseconds_t.h
c_plotif.o: /usr/include/sys/_types/_rsize_t.h
c_plotif.o: /usr/include/sys/_types/_errno_t.h
c_plotif.o: /usr/include/sys/_types/_fd_def.h
c_plotif.o: /usr/include/sys/_types/_fd_setsize.h
c_plotif.o: /usr/include/sys/_types/_fd_set.h
c_plotif.o: /usr/include/sys/_types/_fd_clr.h
c_plotif.o: /usr/include/sys/_types/_fd_zero.h
c_plotif.o: /usr/include/sys/_types/_fd_isset.h
c_plotif.o: /usr/include/sys/_types/_fd_copy.h
c_plotif.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_plotif.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_plotif.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_plotif.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_plotif.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_plotif.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_plotif.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_plotif.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_plotif.o: /usr/include/sys/_pthread/_pthread_t.h
c_plotif.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_plotif.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_plotif.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_plotif.o: ../../../.././include/ncarg/c.h
c_point.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_point.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_point.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_point.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_point.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_point.o: /usr/include/i386/_types.h
c_point.o: /usr/include/sys/_pthread/_pthread_types.h
c_point.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_point.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_point.o: /usr/include/sys/_types/_int16_t.h
c_point.o: /usr/include/sys/_types/_int32_t.h
c_point.o: /usr/include/sys/_types/_int64_t.h
c_point.o: /usr/include/sys/_types/_u_int8_t.h
c_point.o: /usr/include/sys/_types/_u_int16_t.h
c_point.o: /usr/include/sys/_types/_u_int32_t.h
c_point.o: /usr/include/sys/_types/_u_int64_t.h
c_point.o: /usr/include/sys/_types/_intptr_t.h
c_point.o: /usr/include/sys/_types/_uintptr_t.h
c_point.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_point.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_point.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_point.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_point.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_point.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_point.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_point.o: /usr/include/libkern/i386/_OSByteOrder.h
c_point.o: /usr/include/sys/_types/_u_char.h
c_point.o: /usr/include/sys/_types/_u_short.h
c_point.o: /usr/include/sys/_types/_u_int.h
c_point.o: /usr/include/sys/_types/_caddr_t.h
c_point.o: /usr/include/sys/_types/_dev_t.h
c_point.o: /usr/include/sys/_types/_blkcnt_t.h
c_point.o: /usr/include/sys/_types/_blksize_t.h
c_point.o: /usr/include/sys/_types/_gid_t.h
c_point.o: /usr/include/sys/_types/_in_addr_t.h
c_point.o: /usr/include/sys/_types/_in_port_t.h
c_point.o: /usr/include/sys/_types/_ino_t.h
c_point.o: /usr/include/sys/_types/_ino64_t.h
c_point.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_point.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_point.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_point.o: /usr/include/sys/_types/_clock_t.h
c_point.o: /usr/include/sys/_types/_time_t.h
c_point.o: /usr/include/sys/_types/_useconds_t.h
c_point.o: /usr/include/sys/_types/_suseconds_t.h
c_point.o: /usr/include/sys/_types/_rsize_t.h
c_point.o: /usr/include/sys/_types/_errno_t.h
c_point.o: /usr/include/sys/_types/_fd_def.h
c_point.o: /usr/include/sys/_types/_fd_setsize.h
c_point.o: /usr/include/sys/_types/_fd_set.h
c_point.o: /usr/include/sys/_types/_fd_clr.h
c_point.o: /usr/include/sys/_types/_fd_zero.h
c_point.o: /usr/include/sys/_types/_fd_isset.h
c_point.o: /usr/include/sys/_types/_fd_copy.h
c_point.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_point.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_point.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_point.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_point.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_point.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_point.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_point.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_point.o: /usr/include/sys/_pthread/_pthread_t.h
c_point.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_point.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_point.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_point.o: ../../../.././include/ncarg/c.h
c_points.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_points.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_points.o: /usr/include/sys/_posix_availability.h
c_points.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_points.o: /usr/include/_types.h /usr/include/sys/_types.h
c_points.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_points.o: /usr/include/sys/_pthread/_pthread_types.h
c_points.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_points.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_points.o: /usr/include/sys/_types/_int16_t.h
c_points.o: /usr/include/sys/_types/_int32_t.h
c_points.o: /usr/include/sys/_types/_int64_t.h
c_points.o: /usr/include/sys/_types/_u_int8_t.h
c_points.o: /usr/include/sys/_types/_u_int16_t.h
c_points.o: /usr/include/sys/_types/_u_int32_t.h
c_points.o: /usr/include/sys/_types/_u_int64_t.h
c_points.o: /usr/include/sys/_types/_intptr_t.h
c_points.o: /usr/include/sys/_types/_uintptr_t.h
c_points.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_points.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_points.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_points.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_points.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_points.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_points.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_points.o: /usr/include/libkern/i386/_OSByteOrder.h
c_points.o: /usr/include/sys/_types/_u_char.h
c_points.o: /usr/include/sys/_types/_u_short.h
c_points.o: /usr/include/sys/_types/_u_int.h
c_points.o: /usr/include/sys/_types/_caddr_t.h
c_points.o: /usr/include/sys/_types/_dev_t.h
c_points.o: /usr/include/sys/_types/_blkcnt_t.h
c_points.o: /usr/include/sys/_types/_blksize_t.h
c_points.o: /usr/include/sys/_types/_gid_t.h
c_points.o: /usr/include/sys/_types/_in_addr_t.h
c_points.o: /usr/include/sys/_types/_in_port_t.h
c_points.o: /usr/include/sys/_types/_ino_t.h
c_points.o: /usr/include/sys/_types/_ino64_t.h
c_points.o: /usr/include/sys/_types/_key_t.h
c_points.o: /usr/include/sys/_types/_mode_t.h
c_points.o: /usr/include/sys/_types/_nlink_t.h
c_points.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_points.o: /usr/include/sys/_types/_uid_t.h
c_points.o: /usr/include/sys/_types/_clock_t.h
c_points.o: /usr/include/sys/_types/_time_t.h
c_points.o: /usr/include/sys/_types/_useconds_t.h
c_points.o: /usr/include/sys/_types/_suseconds_t.h
c_points.o: /usr/include/sys/_types/_rsize_t.h
c_points.o: /usr/include/sys/_types/_errno_t.h
c_points.o: /usr/include/sys/_types/_fd_def.h
c_points.o: /usr/include/sys/_types/_fd_setsize.h
c_points.o: /usr/include/sys/_types/_fd_set.h
c_points.o: /usr/include/sys/_types/_fd_clr.h
c_points.o: /usr/include/sys/_types/_fd_zero.h
c_points.o: /usr/include/sys/_types/_fd_isset.h
c_points.o: /usr/include/sys/_types/_fd_copy.h
c_points.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_points.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_points.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_points.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_points.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_points.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_points.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_points.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_points.o: /usr/include/sys/_pthread/_pthread_t.h
c_points.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_points.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_points.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_points.o: ../../../.././include/ncarg/c.h
c_set.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_set.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_set.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_set.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_set.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_set.o: /usr/include/i386/_types.h
c_set.o: /usr/include/sys/_pthread/_pthread_types.h
c_set.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_set.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_set.o: /usr/include/sys/_types/_int16_t.h
c_set.o: /usr/include/sys/_types/_int32_t.h
c_set.o: /usr/include/sys/_types/_int64_t.h
c_set.o: /usr/include/sys/_types/_u_int8_t.h
c_set.o: /usr/include/sys/_types/_u_int16_t.h
c_set.o: /usr/include/sys/_types/_u_int32_t.h
c_set.o: /usr/include/sys/_types/_u_int64_t.h
c_set.o: /usr/include/sys/_types/_intptr_t.h
c_set.o: /usr/include/sys/_types/_uintptr_t.h
c_set.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_set.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_set.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_set.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_set.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_set.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_set.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_set.o: /usr/include/libkern/i386/_OSByteOrder.h
c_set.o: /usr/include/sys/_types/_u_char.h /usr/include/sys/_types/_u_short.h
c_set.o: /usr/include/sys/_types/_u_int.h /usr/include/sys/_types/_caddr_t.h
c_set.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_blkcnt_t.h
c_set.o: /usr/include/sys/_types/_blksize_t.h
c_set.o: /usr/include/sys/_types/_gid_t.h
c_set.o: /usr/include/sys/_types/_in_addr_t.h
c_set.o: /usr/include/sys/_types/_in_port_t.h
c_set.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
c_set.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_set.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_set.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_set.o: /usr/include/sys/_types/_clock_t.h /usr/include/sys/_types/_time_t.h
c_set.o: /usr/include/sys/_types/_useconds_t.h
c_set.o: /usr/include/sys/_types/_suseconds_t.h
c_set.o: /usr/include/sys/_types/_rsize_t.h
c_set.o: /usr/include/sys/_types/_errno_t.h /usr/include/sys/_types/_fd_def.h
c_set.o: /usr/include/sys/_types/_fd_setsize.h
c_set.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
c_set.o: /usr/include/sys/_types/_fd_zero.h
c_set.o: /usr/include/sys/_types/_fd_isset.h
c_set.o: /usr/include/sys/_types/_fd_copy.h
c_set.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_set.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_set.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_set.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_set.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_set.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_set.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_set.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_set.o: /usr/include/sys/_pthread/_pthread_t.h
c_set.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_set.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_set.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_set.o: ../../../.././include/ncarg/c.h
c_setusv.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_setusv.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_setusv.o: /usr/include/sys/_posix_availability.h
c_setusv.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_setusv.o: /usr/include/_types.h /usr/include/sys/_types.h
c_setusv.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_setusv.o: /usr/include/sys/_pthread/_pthread_types.h
c_setusv.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_setusv.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_setusv.o: /usr/include/sys/_types/_int16_t.h
c_setusv.o: /usr/include/sys/_types/_int32_t.h
c_setusv.o: /usr/include/sys/_types/_int64_t.h
c_setusv.o: /usr/include/sys/_types/_u_int8_t.h
c_setusv.o: /usr/include/sys/_types/_u_int16_t.h
c_setusv.o: /usr/include/sys/_types/_u_int32_t.h
c_setusv.o: /usr/include/sys/_types/_u_int64_t.h
c_setusv.o: /usr/include/sys/_types/_intptr_t.h
c_setusv.o: /usr/include/sys/_types/_uintptr_t.h
c_setusv.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_setusv.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_setusv.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_setusv.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_setusv.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_setusv.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_setusv.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_setusv.o: /usr/include/libkern/i386/_OSByteOrder.h
c_setusv.o: /usr/include/sys/_types/_u_char.h
c_setusv.o: /usr/include/sys/_types/_u_short.h
c_setusv.o: /usr/include/sys/_types/_u_int.h
c_setusv.o: /usr/include/sys/_types/_caddr_t.h
c_setusv.o: /usr/include/sys/_types/_dev_t.h
c_setusv.o: /usr/include/sys/_types/_blkcnt_t.h
c_setusv.o: /usr/include/sys/_types/_blksize_t.h
c_setusv.o: /usr/include/sys/_types/_gid_t.h
c_setusv.o: /usr/include/sys/_types/_in_addr_t.h
c_setusv.o: /usr/include/sys/_types/_in_port_t.h
c_setusv.o: /usr/include/sys/_types/_ino_t.h
c_setusv.o: /usr/include/sys/_types/_ino64_t.h
c_setusv.o: /usr/include/sys/_types/_key_t.h
c_setusv.o: /usr/include/sys/_types/_mode_t.h
c_setusv.o: /usr/include/sys/_types/_nlink_t.h
c_setusv.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_setusv.o: /usr/include/sys/_types/_uid_t.h
c_setusv.o: /usr/include/sys/_types/_clock_t.h
c_setusv.o: /usr/include/sys/_types/_time_t.h
c_setusv.o: /usr/include/sys/_types/_useconds_t.h
c_setusv.o: /usr/include/sys/_types/_suseconds_t.h
c_setusv.o: /usr/include/sys/_types/_rsize_t.h
c_setusv.o: /usr/include/sys/_types/_errno_t.h
c_setusv.o: /usr/include/sys/_types/_fd_def.h
c_setusv.o: /usr/include/sys/_types/_fd_setsize.h
c_setusv.o: /usr/include/sys/_types/_fd_set.h
c_setusv.o: /usr/include/sys/_types/_fd_clr.h
c_setusv.o: /usr/include/sys/_types/_fd_zero.h
c_setusv.o: /usr/include/sys/_types/_fd_isset.h
c_setusv.o: /usr/include/sys/_types/_fd_copy.h
c_setusv.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_setusv.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_setusv.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_setusv.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_setusv.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_setusv.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_setusv.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_setusv.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_setusv.o: /usr/include/sys/_pthread/_pthread_t.h
c_setusv.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_setusv.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_setusv.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_setusv.o: ../../../.././include/ncarg/c.h
c_sflush.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_sflush.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_sflush.o: /usr/include/sys/_posix_availability.h
c_sflush.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_sflush.o: /usr/include/_types.h /usr/include/sys/_types.h
c_sflush.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_sflush.o: /usr/include/sys/_pthread/_pthread_types.h
c_sflush.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_sflush.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_sflush.o: /usr/include/sys/_types/_int16_t.h
c_sflush.o: /usr/include/sys/_types/_int32_t.h
c_sflush.o: /usr/include/sys/_types/_int64_t.h
c_sflush.o: /usr/include/sys/_types/_u_int8_t.h
c_sflush.o: /usr/include/sys/_types/_u_int16_t.h
c_sflush.o: /usr/include/sys/_types/_u_int32_t.h
c_sflush.o: /usr/include/sys/_types/_u_int64_t.h
c_sflush.o: /usr/include/sys/_types/_intptr_t.h
c_sflush.o: /usr/include/sys/_types/_uintptr_t.h
c_sflush.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_sflush.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_sflush.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_sflush.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_sflush.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_sflush.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_sflush.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_sflush.o: /usr/include/libkern/i386/_OSByteOrder.h
c_sflush.o: /usr/include/sys/_types/_u_char.h
c_sflush.o: /usr/include/sys/_types/_u_short.h
c_sflush.o: /usr/include/sys/_types/_u_int.h
c_sflush.o: /usr/include/sys/_types/_caddr_t.h
c_sflush.o: /usr/include/sys/_types/_dev_t.h
c_sflush.o: /usr/include/sys/_types/_blkcnt_t.h
c_sflush.o: /usr/include/sys/_types/_blksize_t.h
c_sflush.o: /usr/include/sys/_types/_gid_t.h
c_sflush.o: /usr/include/sys/_types/_in_addr_t.h
c_sflush.o: /usr/include/sys/_types/_in_port_t.h
c_sflush.o: /usr/include/sys/_types/_ino_t.h
c_sflush.o: /usr/include/sys/_types/_ino64_t.h
c_sflush.o: /usr/include/sys/_types/_key_t.h
c_sflush.o: /usr/include/sys/_types/_mode_t.h
c_sflush.o: /usr/include/sys/_types/_nlink_t.h
c_sflush.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_sflush.o: /usr/include/sys/_types/_uid_t.h
c_sflush.o: /usr/include/sys/_types/_clock_t.h
c_sflush.o: /usr/include/sys/_types/_time_t.h
c_sflush.o: /usr/include/sys/_types/_useconds_t.h
c_sflush.o: /usr/include/sys/_types/_suseconds_t.h
c_sflush.o: /usr/include/sys/_types/_rsize_t.h
c_sflush.o: /usr/include/sys/_types/_errno_t.h
c_sflush.o: /usr/include/sys/_types/_fd_def.h
c_sflush.o: /usr/include/sys/_types/_fd_setsize.h
c_sflush.o: /usr/include/sys/_types/_fd_set.h
c_sflush.o: /usr/include/sys/_types/_fd_clr.h
c_sflush.o: /usr/include/sys/_types/_fd_zero.h
c_sflush.o: /usr/include/sys/_types/_fd_isset.h
c_sflush.o: /usr/include/sys/_types/_fd_copy.h
c_sflush.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_sflush.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_sflush.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_sflush.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_sflush.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_sflush.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_sflush.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_sflush.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_sflush.o: /usr/include/sys/_pthread/_pthread_t.h
c_sflush.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_sflush.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_sflush.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_sflush.o: ../../../.././include/ncarg/c.h
c_vector.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_vector.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_vector.o: /usr/include/sys/_posix_availability.h
c_vector.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_vector.o: /usr/include/_types.h /usr/include/sys/_types.h
c_vector.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_vector.o: /usr/include/sys/_pthread/_pthread_types.h
c_vector.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_vector.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_vector.o: /usr/include/sys/_types/_int16_t.h
c_vector.o: /usr/include/sys/_types/_int32_t.h
c_vector.o: /usr/include/sys/_types/_int64_t.h
c_vector.o: /usr/include/sys/_types/_u_int8_t.h
c_vector.o: /usr/include/sys/_types/_u_int16_t.h
c_vector.o: /usr/include/sys/_types/_u_int32_t.h
c_vector.o: /usr/include/sys/_types/_u_int64_t.h
c_vector.o: /usr/include/sys/_types/_intptr_t.h
c_vector.o: /usr/include/sys/_types/_uintptr_t.h
c_vector.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_vector.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_vector.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_vector.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_vector.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_vector.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_vector.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_vector.o: /usr/include/libkern/i386/_OSByteOrder.h
c_vector.o: /usr/include/sys/_types/_u_char.h
c_vector.o: /usr/include/sys/_types/_u_short.h
c_vector.o: /usr/include/sys/_types/_u_int.h
c_vector.o: /usr/include/sys/_types/_caddr_t.h
c_vector.o: /usr/include/sys/_types/_dev_t.h
c_vector.o: /usr/include/sys/_types/_blkcnt_t.h
c_vector.o: /usr/include/sys/_types/_blksize_t.h
c_vector.o: /usr/include/sys/_types/_gid_t.h
c_vector.o: /usr/include/sys/_types/_in_addr_t.h
c_vector.o: /usr/include/sys/_types/_in_port_t.h
c_vector.o: /usr/include/sys/_types/_ino_t.h
c_vector.o: /usr/include/sys/_types/_ino64_t.h
c_vector.o: /usr/include/sys/_types/_key_t.h
c_vector.o: /usr/include/sys/_types/_mode_t.h
c_vector.o: /usr/include/sys/_types/_nlink_t.h
c_vector.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_vector.o: /usr/include/sys/_types/_uid_t.h
c_vector.o: /usr/include/sys/_types/_clock_t.h
c_vector.o: /usr/include/sys/_types/_time_t.h
c_vector.o: /usr/include/sys/_types/_useconds_t.h
c_vector.o: /usr/include/sys/_types/_suseconds_t.h
c_vector.o: /usr/include/sys/_types/_rsize_t.h
c_vector.o: /usr/include/sys/_types/_errno_t.h
c_vector.o: /usr/include/sys/_types/_fd_def.h
c_vector.o: /usr/include/sys/_types/_fd_setsize.h
c_vector.o: /usr/include/sys/_types/_fd_set.h
c_vector.o: /usr/include/sys/_types/_fd_clr.h
c_vector.o: /usr/include/sys/_types/_fd_zero.h
c_vector.o: /usr/include/sys/_types/_fd_isset.h
c_vector.o: /usr/include/sys/_types/_fd_copy.h
c_vector.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_vector.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_vector.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_vector.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_vector.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_vector.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_vector.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_vector.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_vector.o: /usr/include/sys/_pthread/_pthread_t.h
c_vector.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_vector.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_vector.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_vector.o: ../../../.././include/ncarg/c.h
c_wtstr.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_wtstr.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_wtstr.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_wtstr.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_wtstr.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_wtstr.o: /usr/include/i386/_types.h
c_wtstr.o: /usr/include/sys/_pthread/_pthread_types.h
c_wtstr.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_wtstr.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_wtstr.o: /usr/include/sys/_types/_int16_t.h
c_wtstr.o: /usr/include/sys/_types/_int32_t.h
c_wtstr.o: /usr/include/sys/_types/_int64_t.h
c_wtstr.o: /usr/include/sys/_types/_u_int8_t.h
c_wtstr.o: /usr/include/sys/_types/_u_int16_t.h
c_wtstr.o: /usr/include/sys/_types/_u_int32_t.h
c_wtstr.o: /usr/include/sys/_types/_u_int64_t.h
c_wtstr.o: /usr/include/sys/_types/_intptr_t.h
c_wtstr.o: /usr/include/sys/_types/_uintptr_t.h
c_wtstr.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_wtstr.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_wtstr.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_wtstr.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_wtstr.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_wtstr.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_wtstr.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_wtstr.o: /usr/include/libkern/i386/_OSByteOrder.h
c_wtstr.o: /usr/include/sys/_types/_u_char.h
c_wtstr.o: /usr/include/sys/_types/_u_short.h
c_wtstr.o: /usr/include/sys/_types/_u_int.h
c_wtstr.o: /usr/include/sys/_types/_caddr_t.h
c_wtstr.o: /usr/include/sys/_types/_dev_t.h
c_wtstr.o: /usr/include/sys/_types/_blkcnt_t.h
c_wtstr.o: /usr/include/sys/_types/_blksize_t.h
c_wtstr.o: /usr/include/sys/_types/_gid_t.h
c_wtstr.o: /usr/include/sys/_types/_in_addr_t.h
c_wtstr.o: /usr/include/sys/_types/_in_port_t.h
c_wtstr.o: /usr/include/sys/_types/_ino_t.h
c_wtstr.o: /usr/include/sys/_types/_ino64_t.h
c_wtstr.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_wtstr.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_wtstr.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_wtstr.o: /usr/include/sys/_types/_clock_t.h
c_wtstr.o: /usr/include/sys/_types/_time_t.h
c_wtstr.o: /usr/include/sys/_types/_useconds_t.h
c_wtstr.o: /usr/include/sys/_types/_suseconds_t.h
c_wtstr.o: /usr/include/sys/_types/_rsize_t.h
c_wtstr.o: /usr/include/sys/_types/_errno_t.h
c_wtstr.o: /usr/include/sys/_types/_fd_def.h
c_wtstr.o: /usr/include/sys/_types/_fd_setsize.h
c_wtstr.o: /usr/include/sys/_types/_fd_set.h
c_wtstr.o: /usr/include/sys/_types/_fd_clr.h
c_wtstr.o: /usr/include/sys/_types/_fd_zero.h
c_wtstr.o: /usr/include/sys/_types/_fd_isset.h
c_wtstr.o: /usr/include/sys/_types/_fd_copy.h
c_wtstr.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_wtstr.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_wtstr.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_wtstr.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_wtstr.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_wtstr.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_wtstr.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_wtstr.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_wtstr.o: /usr/include/sys/_pthread/_pthread_t.h
c_wtstr.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_wtstr.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_wtstr.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_wtstr.o: ../../../.././include/ncarg/c.h
c_plotit.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_plotit.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_plotit.o: /usr/include/sys/_posix_availability.h
c_plotit.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_plotit.o: /usr/include/_types.h /usr/include/sys/_types.h
c_plotit.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_plotit.o: /usr/include/sys/_pthread/_pthread_types.h
c_plotit.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_plotit.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_plotit.o: /usr/include/sys/_types/_int16_t.h
c_plotit.o: /usr/include/sys/_types/_int32_t.h
c_plotit.o: /usr/include/sys/_types/_int64_t.h
c_plotit.o: /usr/include/sys/_types/_u_int8_t.h
c_plotit.o: /usr/include/sys/_types/_u_int16_t.h
c_plotit.o: /usr/include/sys/_types/_u_int32_t.h
c_plotit.o: /usr/include/sys/_types/_u_int64_t.h
c_plotit.o: /usr/include/sys/_types/_intptr_t.h
c_plotit.o: /usr/include/sys/_types/_uintptr_t.h
c_plotit.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_plotit.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_plotit.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_plotit.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_plotit.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_plotit.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_plotit.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_plotit.o: /usr/include/libkern/i386/_OSByteOrder.h
c_plotit.o: /usr/include/sys/_types/_u_char.h
c_plotit.o: /usr/include/sys/_types/_u_short.h
c_plotit.o: /usr/include/sys/_types/_u_int.h
c_plotit.o: /usr/include/sys/_types/_caddr_t.h
c_plotit.o: /usr/include/sys/_types/_dev_t.h
c_plotit.o: /usr/include/sys/_types/_blkcnt_t.h
c_plotit.o: /usr/include/sys/_types/_blksize_t.h
c_plotit.o: /usr/include/sys/_types/_gid_t.h
c_plotit.o: /usr/include/sys/_types/_in_addr_t.h
c_plotit.o: /usr/include/sys/_types/_in_port_t.h
c_plotit.o: /usr/include/sys/_types/_ino_t.h
c_plotit.o: /usr/include/sys/_types/_ino64_t.h
c_plotit.o: /usr/include/sys/_types/_key_t.h
c_plotit.o: /usr/include/sys/_types/_mode_t.h
c_plotit.o: /usr/include/sys/_types/_nlink_t.h
c_plotit.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_plotit.o: /usr/include/sys/_types/_uid_t.h
c_plotit.o: /usr/include/sys/_types/_clock_t.h
c_plotit.o: /usr/include/sys/_types/_time_t.h
c_plotit.o: /usr/include/sys/_types/_useconds_t.h
c_plotit.o: /usr/include/sys/_types/_suseconds_t.h
c_plotit.o: /usr/include/sys/_types/_rsize_t.h
c_plotit.o: /usr/include/sys/_types/_errno_t.h
c_plotit.o: /usr/include/sys/_types/_fd_def.h
c_plotit.o: /usr/include/sys/_types/_fd_setsize.h
c_plotit.o: /usr/include/sys/_types/_fd_set.h
c_plotit.o: /usr/include/sys/_types/_fd_clr.h
c_plotit.o: /usr/include/sys/_types/_fd_zero.h
c_plotit.o: /usr/include/sys/_types/_fd_isset.h
c_plotit.o: /usr/include/sys/_types/_fd_copy.h
c_plotit.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_plotit.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_plotit.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_plotit.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_plotit.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_plotit.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_plotit.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_plotit.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_plotit.o: /usr/include/sys/_pthread/_pthread_t.h
c_plotit.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_plotit.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_plotit.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_plotit.o: ../../../.././include/ncarg/c.h
c_pwrit.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_pwrit.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_pwrit.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_pwrit.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_pwrit.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_pwrit.o: /usr/include/i386/_types.h
c_pwrit.o: /usr/include/sys/_pthread/_pthread_types.h
c_pwrit.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_pwrit.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_pwrit.o: /usr/include/sys/_types/_int16_t.h
c_pwrit.o: /usr/include/sys/_types/_int32_t.h
c_pwrit.o: /usr/include/sys/_types/_int64_t.h
c_pwrit.o: /usr/include/sys/_types/_u_int8_t.h
c_pwrit.o: /usr/include/sys/_types/_u_int16_t.h
c_pwrit.o: /usr/include/sys/_types/_u_int32_t.h
c_pwrit.o: /usr/include/sys/_types/_u_int64_t.h
c_pwrit.o: /usr/include/sys/_types/_intptr_t.h
c_pwrit.o: /usr/include/sys/_types/_uintptr_t.h
c_pwrit.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_pwrit.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_pwrit.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_pwrit.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_pwrit.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_pwrit.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_pwrit.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_pwrit.o: /usr/include/libkern/i386/_OSByteOrder.h
c_pwrit.o: /usr/include/sys/_types/_u_char.h
c_pwrit.o: /usr/include/sys/_types/_u_short.h
c_pwrit.o: /usr/include/sys/_types/_u_int.h
c_pwrit.o: /usr/include/sys/_types/_caddr_t.h
c_pwrit.o: /usr/include/sys/_types/_dev_t.h
c_pwrit.o: /usr/include/sys/_types/_blkcnt_t.h
c_pwrit.o: /usr/include/sys/_types/_blksize_t.h
c_pwrit.o: /usr/include/sys/_types/_gid_t.h
c_pwrit.o: /usr/include/sys/_types/_in_addr_t.h
c_pwrit.o: /usr/include/sys/_types/_in_port_t.h
c_pwrit.o: /usr/include/sys/_types/_ino_t.h
c_pwrit.o: /usr/include/sys/_types/_ino64_t.h
c_pwrit.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_pwrit.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_pwrit.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_pwrit.o: /usr/include/sys/_types/_clock_t.h
c_pwrit.o: /usr/include/sys/_types/_time_t.h
c_pwrit.o: /usr/include/sys/_types/_useconds_t.h
c_pwrit.o: /usr/include/sys/_types/_suseconds_t.h
c_pwrit.o: /usr/include/sys/_types/_rsize_t.h
c_pwrit.o: /usr/include/sys/_types/_errno_t.h
c_pwrit.o: /usr/include/sys/_types/_fd_def.h
c_pwrit.o: /usr/include/sys/_types/_fd_setsize.h
c_pwrit.o: /usr/include/sys/_types/_fd_set.h
c_pwrit.o: /usr/include/sys/_types/_fd_clr.h
c_pwrit.o: /usr/include/sys/_types/_fd_zero.h
c_pwrit.o: /usr/include/sys/_types/_fd_isset.h
c_pwrit.o: /usr/include/sys/_types/_fd_copy.h
c_pwrit.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_pwrit.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_pwrit.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_pwrit.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_pwrit.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_pwrit.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_pwrit.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_pwrit.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_pwrit.o: /usr/include/sys/_pthread/_pthread_t.h
c_pwrit.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_pwrit.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_pwrit.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_pwrit.o: ../../../.././include/ncarg/c.h
c_cfux.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_cfux.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_cfux.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_cfux.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_cfux.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_cfux.o: /usr/include/i386/_types.h
c_cfux.o: /usr/include/sys/_pthread/_pthread_types.h
c_cfux.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_cfux.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_cfux.o: /usr/include/sys/_types/_int16_t.h
c_cfux.o: /usr/include/sys/_types/_int32_t.h
c_cfux.o: /usr/include/sys/_types/_int64_t.h
c_cfux.o: /usr/include/sys/_types/_u_int8_t.h
c_cfux.o: /usr/include/sys/_types/_u_int16_t.h
c_cfux.o: /usr/include/sys/_types/_u_int32_t.h
c_cfux.o: /usr/include/sys/_types/_u_int64_t.h
c_cfux.o: /usr/include/sys/_types/_intptr_t.h
c_cfux.o: /usr/include/sys/_types/_uintptr_t.h
c_cfux.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_cfux.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_cfux.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_cfux.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_cfux.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_cfux.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_cfux.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_cfux.o: /usr/include/libkern/i386/_OSByteOrder.h
c_cfux.o: /usr/include/sys/_types/_u_char.h
c_cfux.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
c_cfux.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
c_cfux.o: /usr/include/sys/_types/_blkcnt_t.h
c_cfux.o: /usr/include/sys/_types/_blksize_t.h
c_cfux.o: /usr/include/sys/_types/_gid_t.h
c_cfux.o: /usr/include/sys/_types/_in_addr_t.h
c_cfux.o: /usr/include/sys/_types/_in_port_t.h
c_cfux.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
c_cfux.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_cfux.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_cfux.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_cfux.o: /usr/include/sys/_types/_clock_t.h
c_cfux.o: /usr/include/sys/_types/_time_t.h
c_cfux.o: /usr/include/sys/_types/_useconds_t.h
c_cfux.o: /usr/include/sys/_types/_suseconds_t.h
c_cfux.o: /usr/include/sys/_types/_rsize_t.h
c_cfux.o: /usr/include/sys/_types/_errno_t.h
c_cfux.o: /usr/include/sys/_types/_fd_def.h
c_cfux.o: /usr/include/sys/_types/_fd_setsize.h
c_cfux.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
c_cfux.o: /usr/include/sys/_types/_fd_zero.h
c_cfux.o: /usr/include/sys/_types/_fd_isset.h
c_cfux.o: /usr/include/sys/_types/_fd_copy.h
c_cfux.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_cfux.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_cfux.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_cfux.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_cfux.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_cfux.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_cfux.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_cfux.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_cfux.o: /usr/include/sys/_pthread/_pthread_t.h
c_cfux.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_cfux.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_cfux.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_cfux.o: ../../../.././include/ncarg/c.h
c_cfuy.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_cfuy.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_cfuy.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_cfuy.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_cfuy.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_cfuy.o: /usr/include/i386/_types.h
c_cfuy.o: /usr/include/sys/_pthread/_pthread_types.h
c_cfuy.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_cfuy.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_cfuy.o: /usr/include/sys/_types/_int16_t.h
c_cfuy.o: /usr/include/sys/_types/_int32_t.h
c_cfuy.o: /usr/include/sys/_types/_int64_t.h
c_cfuy.o: /usr/include/sys/_types/_u_int8_t.h
c_cfuy.o: /usr/include/sys/_types/_u_int16_t.h
c_cfuy.o: /usr/include/sys/_types/_u_int32_t.h
c_cfuy.o: /usr/include/sys/_types/_u_int64_t.h
c_cfuy.o: /usr/include/sys/_types/_intptr_t.h
c_cfuy.o: /usr/include/sys/_types/_uintptr_t.h
c_cfuy.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_cfuy.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_cfuy.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_cfuy.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_cfuy.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_cfuy.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_cfuy.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_cfuy.o: /usr/include/libkern/i386/_OSByteOrder.h
c_cfuy.o: /usr/include/sys/_types/_u_char.h
c_cfuy.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
c_cfuy.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
c_cfuy.o: /usr/include/sys/_types/_blkcnt_t.h
c_cfuy.o: /usr/include/sys/_types/_blksize_t.h
c_cfuy.o: /usr/include/sys/_types/_gid_t.h
c_cfuy.o: /usr/include/sys/_types/_in_addr_t.h
c_cfuy.o: /usr/include/sys/_types/_in_port_t.h
c_cfuy.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
c_cfuy.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_cfuy.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_cfuy.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_cfuy.o: /usr/include/sys/_types/_clock_t.h
c_cfuy.o: /usr/include/sys/_types/_time_t.h
c_cfuy.o: /usr/include/sys/_types/_useconds_t.h
c_cfuy.o: /usr/include/sys/_types/_suseconds_t.h
c_cfuy.o: /usr/include/sys/_types/_rsize_t.h
c_cfuy.o: /usr/include/sys/_types/_errno_t.h
c_cfuy.o: /usr/include/sys/_types/_fd_def.h
c_cfuy.o: /usr/include/sys/_types/_fd_setsize.h
c_cfuy.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
c_cfuy.o: /usr/include/sys/_types/_fd_zero.h
c_cfuy.o: /usr/include/sys/_types/_fd_isset.h
c_cfuy.o: /usr/include/sys/_types/_fd_copy.h
c_cfuy.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_cfuy.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_cfuy.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_cfuy.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_cfuy.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_cfuy.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_cfuy.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_cfuy.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_cfuy.o: /usr/include/sys/_pthread/_pthread_t.h
c_cfuy.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_cfuy.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_cfuy.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_cfuy.o: ../../../.././include/ncarg/c.h
c_cmfx.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_cmfx.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_cmfx.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_cmfx.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_cmfx.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_cmfx.o: /usr/include/i386/_types.h
c_cmfx.o: /usr/include/sys/_pthread/_pthread_types.h
c_cmfx.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_cmfx.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_cmfx.o: /usr/include/sys/_types/_int16_t.h
c_cmfx.o: /usr/include/sys/_types/_int32_t.h
c_cmfx.o: /usr/include/sys/_types/_int64_t.h
c_cmfx.o: /usr/include/sys/_types/_u_int8_t.h
c_cmfx.o: /usr/include/sys/_types/_u_int16_t.h
c_cmfx.o: /usr/include/sys/_types/_u_int32_t.h
c_cmfx.o: /usr/include/sys/_types/_u_int64_t.h
c_cmfx.o: /usr/include/sys/_types/_intptr_t.h
c_cmfx.o: /usr/include/sys/_types/_uintptr_t.h
c_cmfx.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_cmfx.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_cmfx.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_cmfx.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_cmfx.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_cmfx.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_cmfx.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_cmfx.o: /usr/include/libkern/i386/_OSByteOrder.h
c_cmfx.o: /usr/include/sys/_types/_u_char.h
c_cmfx.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
c_cmfx.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
c_cmfx.o: /usr/include/sys/_types/_blkcnt_t.h
c_cmfx.o: /usr/include/sys/_types/_blksize_t.h
c_cmfx.o: /usr/include/sys/_types/_gid_t.h
c_cmfx.o: /usr/include/sys/_types/_in_addr_t.h
c_cmfx.o: /usr/include/sys/_types/_in_port_t.h
c_cmfx.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
c_cmfx.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_cmfx.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_cmfx.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_cmfx.o: /usr/include/sys/_types/_clock_t.h
c_cmfx.o: /usr/include/sys/_types/_time_t.h
c_cmfx.o: /usr/include/sys/_types/_useconds_t.h
c_cmfx.o: /usr/include/sys/_types/_suseconds_t.h
c_cmfx.o: /usr/include/sys/_types/_rsize_t.h
c_cmfx.o: /usr/include/sys/_types/_errno_t.h
c_cmfx.o: /usr/include/sys/_types/_fd_def.h
c_cmfx.o: /usr/include/sys/_types/_fd_setsize.h
c_cmfx.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
c_cmfx.o: /usr/include/sys/_types/_fd_zero.h
c_cmfx.o: /usr/include/sys/_types/_fd_isset.h
c_cmfx.o: /usr/include/sys/_types/_fd_copy.h
c_cmfx.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_cmfx.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_cmfx.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_cmfx.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_cmfx.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_cmfx.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_cmfx.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_cmfx.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_cmfx.o: /usr/include/sys/_pthread/_pthread_t.h
c_cmfx.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_cmfx.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_cmfx.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_cmfx.o: ../../../.././include/ncarg/c.h
c_cmfy.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_cmfy.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_cmfy.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_cmfy.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_cmfy.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_cmfy.o: /usr/include/i386/_types.h
c_cmfy.o: /usr/include/sys/_pthread/_pthread_types.h
c_cmfy.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_cmfy.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_cmfy.o: /usr/include/sys/_types/_int16_t.h
c_cmfy.o: /usr/include/sys/_types/_int32_t.h
c_cmfy.o: /usr/include/sys/_types/_int64_t.h
c_cmfy.o: /usr/include/sys/_types/_u_int8_t.h
c_cmfy.o: /usr/include/sys/_types/_u_int16_t.h
c_cmfy.o: /usr/include/sys/_types/_u_int32_t.h
c_cmfy.o: /usr/include/sys/_types/_u_int64_t.h
c_cmfy.o: /usr/include/sys/_types/_intptr_t.h
c_cmfy.o: /usr/include/sys/_types/_uintptr_t.h
c_cmfy.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_cmfy.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_cmfy.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_cmfy.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_cmfy.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_cmfy.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_cmfy.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_cmfy.o: /usr/include/libkern/i386/_OSByteOrder.h
c_cmfy.o: /usr/include/sys/_types/_u_char.h
c_cmfy.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
c_cmfy.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
c_cmfy.o: /usr/include/sys/_types/_blkcnt_t.h
c_cmfy.o: /usr/include/sys/_types/_blksize_t.h
c_cmfy.o: /usr/include/sys/_types/_gid_t.h
c_cmfy.o: /usr/include/sys/_types/_in_addr_t.h
c_cmfy.o: /usr/include/sys/_types/_in_port_t.h
c_cmfy.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
c_cmfy.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_cmfy.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_cmfy.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_cmfy.o: /usr/include/sys/_types/_clock_t.h
c_cmfy.o: /usr/include/sys/_types/_time_t.h
c_cmfy.o: /usr/include/sys/_types/_useconds_t.h
c_cmfy.o: /usr/include/sys/_types/_suseconds_t.h
c_cmfy.o: /usr/include/sys/_types/_rsize_t.h
c_cmfy.o: /usr/include/sys/_types/_errno_t.h
c_cmfy.o: /usr/include/sys/_types/_fd_def.h
c_cmfy.o: /usr/include/sys/_types/_fd_setsize.h
c_cmfy.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
c_cmfy.o: /usr/include/sys/_types/_fd_zero.h
c_cmfy.o: /usr/include/sys/_types/_fd_isset.h
c_cmfy.o: /usr/include/sys/_types/_fd_copy.h
c_cmfy.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_cmfy.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_cmfy.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_cmfy.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_cmfy.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_cmfy.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_cmfy.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_cmfy.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_cmfy.o: /usr/include/sys/_pthread/_pthread_t.h
c_cmfy.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_cmfy.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_cmfy.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_cmfy.o: ../../../.././include/ncarg/c.h
c_cufx.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_cufx.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_cufx.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_cufx.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_cufx.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_cufx.o: /usr/include/i386/_types.h
c_cufx.o: /usr/include/sys/_pthread/_pthread_types.h
c_cufx.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_cufx.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_cufx.o: /usr/include/sys/_types/_int16_t.h
c_cufx.o: /usr/include/sys/_types/_int32_t.h
c_cufx.o: /usr/include/sys/_types/_int64_t.h
c_cufx.o: /usr/include/sys/_types/_u_int8_t.h
c_cufx.o: /usr/include/sys/_types/_u_int16_t.h
c_cufx.o: /usr/include/sys/_types/_u_int32_t.h
c_cufx.o: /usr/include/sys/_types/_u_int64_t.h
c_cufx.o: /usr/include/sys/_types/_intptr_t.h
c_cufx.o: /usr/include/sys/_types/_uintptr_t.h
c_cufx.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_cufx.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_cufx.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_cufx.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_cufx.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_cufx.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_cufx.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_cufx.o: /usr/include/libkern/i386/_OSByteOrder.h
c_cufx.o: /usr/include/sys/_types/_u_char.h
c_cufx.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
c_cufx.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
c_cufx.o: /usr/include/sys/_types/_blkcnt_t.h
c_cufx.o: /usr/include/sys/_types/_blksize_t.h
c_cufx.o: /usr/include/sys/_types/_gid_t.h
c_cufx.o: /usr/include/sys/_types/_in_addr_t.h
c_cufx.o: /usr/include/sys/_types/_in_port_t.h
c_cufx.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
c_cufx.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_cufx.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_cufx.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_cufx.o: /usr/include/sys/_types/_clock_t.h
c_cufx.o: /usr/include/sys/_types/_time_t.h
c_cufx.o: /usr/include/sys/_types/_useconds_t.h
c_cufx.o: /usr/include/sys/_types/_suseconds_t.h
c_cufx.o: /usr/include/sys/_types/_rsize_t.h
c_cufx.o: /usr/include/sys/_types/_errno_t.h
c_cufx.o: /usr/include/sys/_types/_fd_def.h
c_cufx.o: /usr/include/sys/_types/_fd_setsize.h
c_cufx.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
c_cufx.o: /usr/include/sys/_types/_fd_zero.h
c_cufx.o: /usr/include/sys/_types/_fd_isset.h
c_cufx.o: /usr/include/sys/_types/_fd_copy.h
c_cufx.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_cufx.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_cufx.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_cufx.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_cufx.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_cufx.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_cufx.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_cufx.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_cufx.o: /usr/include/sys/_pthread/_pthread_t.h
c_cufx.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_cufx.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_cufx.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_cufx.o: ../../../.././include/ncarg/c.h
c_cufy.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_cufy.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_cufy.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_cufy.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_cufy.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_cufy.o: /usr/include/i386/_types.h
c_cufy.o: /usr/include/sys/_pthread/_pthread_types.h
c_cufy.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_cufy.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_cufy.o: /usr/include/sys/_types/_int16_t.h
c_cufy.o: /usr/include/sys/_types/_int32_t.h
c_cufy.o: /usr/include/sys/_types/_int64_t.h
c_cufy.o: /usr/include/sys/_types/_u_int8_t.h
c_cufy.o: /usr/include/sys/_types/_u_int16_t.h
c_cufy.o: /usr/include/sys/_types/_u_int32_t.h
c_cufy.o: /usr/include/sys/_types/_u_int64_t.h
c_cufy.o: /usr/include/sys/_types/_intptr_t.h
c_cufy.o: /usr/include/sys/_types/_uintptr_t.h
c_cufy.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_cufy.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_cufy.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_cufy.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_cufy.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_cufy.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_cufy.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_cufy.o: /usr/include/libkern/i386/_OSByteOrder.h
c_cufy.o: /usr/include/sys/_types/_u_char.h
c_cufy.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
c_cufy.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
c_cufy.o: /usr/include/sys/_types/_blkcnt_t.h
c_cufy.o: /usr/include/sys/_types/_blksize_t.h
c_cufy.o: /usr/include/sys/_types/_gid_t.h
c_cufy.o: /usr/include/sys/_types/_in_addr_t.h
c_cufy.o: /usr/include/sys/_types/_in_port_t.h
c_cufy.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
c_cufy.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_cufy.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_cufy.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_cufy.o: /usr/include/sys/_types/_clock_t.h
c_cufy.o: /usr/include/sys/_types/_time_t.h
c_cufy.o: /usr/include/sys/_types/_useconds_t.h
c_cufy.o: /usr/include/sys/_types/_suseconds_t.h
c_cufy.o: /usr/include/sys/_types/_rsize_t.h
c_cufy.o: /usr/include/sys/_types/_errno_t.h
c_cufy.o: /usr/include/sys/_types/_fd_def.h
c_cufy.o: /usr/include/sys/_types/_fd_setsize.h
c_cufy.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
c_cufy.o: /usr/include/sys/_types/_fd_zero.h
c_cufy.o: /usr/include/sys/_types/_fd_isset.h
c_cufy.o: /usr/include/sys/_types/_fd_copy.h
c_cufy.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_cufy.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_cufy.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_cufy.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_cufy.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_cufy.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_cufy.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_cufy.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_cufy.o: /usr/include/sys/_pthread/_pthread_t.h
c_cufy.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_cufy.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_cufy.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_cufy.o: ../../../.././include/ncarg/c.h
c_cpux.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_cpux.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_cpux.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_cpux.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_cpux.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_cpux.o: /usr/include/i386/_types.h
c_cpux.o: /usr/include/sys/_pthread/_pthread_types.h
c_cpux.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_cpux.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_cpux.o: /usr/include/sys/_types/_int16_t.h
c_cpux.o: /usr/include/sys/_types/_int32_t.h
c_cpux.o: /usr/include/sys/_types/_int64_t.h
c_cpux.o: /usr/include/sys/_types/_u_int8_t.h
c_cpux.o: /usr/include/sys/_types/_u_int16_t.h
c_cpux.o: /usr/include/sys/_types/_u_int32_t.h
c_cpux.o: /usr/include/sys/_types/_u_int64_t.h
c_cpux.o: /usr/include/sys/_types/_intptr_t.h
c_cpux.o: /usr/include/sys/_types/_uintptr_t.h
c_cpux.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_cpux.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_cpux.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_cpux.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_cpux.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_cpux.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_cpux.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_cpux.o: /usr/include/libkern/i386/_OSByteOrder.h
c_cpux.o: /usr/include/sys/_types/_u_char.h
c_cpux.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
c_cpux.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
c_cpux.o: /usr/include/sys/_types/_blkcnt_t.h
c_cpux.o: /usr/include/sys/_types/_blksize_t.h
c_cpux.o: /usr/include/sys/_types/_gid_t.h
c_cpux.o: /usr/include/sys/_types/_in_addr_t.h
c_cpux.o: /usr/include/sys/_types/_in_port_t.h
c_cpux.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
c_cpux.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_cpux.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_cpux.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_cpux.o: /usr/include/sys/_types/_clock_t.h
c_cpux.o: /usr/include/sys/_types/_time_t.h
c_cpux.o: /usr/include/sys/_types/_useconds_t.h
c_cpux.o: /usr/include/sys/_types/_suseconds_t.h
c_cpux.o: /usr/include/sys/_types/_rsize_t.h
c_cpux.o: /usr/include/sys/_types/_errno_t.h
c_cpux.o: /usr/include/sys/_types/_fd_def.h
c_cpux.o: /usr/include/sys/_types/_fd_setsize.h
c_cpux.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
c_cpux.o: /usr/include/sys/_types/_fd_zero.h
c_cpux.o: /usr/include/sys/_types/_fd_isset.h
c_cpux.o: /usr/include/sys/_types/_fd_copy.h
c_cpux.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_cpux.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_cpux.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_cpux.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_cpux.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_cpux.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_cpux.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_cpux.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_cpux.o: /usr/include/sys/_pthread/_pthread_t.h
c_cpux.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_cpux.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_cpux.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_cpux.o: ../../../.././include/ncarg/c.h
c_cpuy.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_cpuy.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_cpuy.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_cpuy.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_cpuy.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_cpuy.o: /usr/include/i386/_types.h
c_cpuy.o: /usr/include/sys/_pthread/_pthread_types.h
c_cpuy.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_cpuy.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_cpuy.o: /usr/include/sys/_types/_int16_t.h
c_cpuy.o: /usr/include/sys/_types/_int32_t.h
c_cpuy.o: /usr/include/sys/_types/_int64_t.h
c_cpuy.o: /usr/include/sys/_types/_u_int8_t.h
c_cpuy.o: /usr/include/sys/_types/_u_int16_t.h
c_cpuy.o: /usr/include/sys/_types/_u_int32_t.h
c_cpuy.o: /usr/include/sys/_types/_u_int64_t.h
c_cpuy.o: /usr/include/sys/_types/_intptr_t.h
c_cpuy.o: /usr/include/sys/_types/_uintptr_t.h
c_cpuy.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_cpuy.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_cpuy.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_cpuy.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_cpuy.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_cpuy.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_cpuy.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_cpuy.o: /usr/include/libkern/i386/_OSByteOrder.h
c_cpuy.o: /usr/include/sys/_types/_u_char.h
c_cpuy.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
c_cpuy.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
c_cpuy.o: /usr/include/sys/_types/_blkcnt_t.h
c_cpuy.o: /usr/include/sys/_types/_blksize_t.h
c_cpuy.o: /usr/include/sys/_types/_gid_t.h
c_cpuy.o: /usr/include/sys/_types/_in_addr_t.h
c_cpuy.o: /usr/include/sys/_types/_in_port_t.h
c_cpuy.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
c_cpuy.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_cpuy.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_cpuy.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_cpuy.o: /usr/include/sys/_types/_clock_t.h
c_cpuy.o: /usr/include/sys/_types/_time_t.h
c_cpuy.o: /usr/include/sys/_types/_useconds_t.h
c_cpuy.o: /usr/include/sys/_types/_suseconds_t.h
c_cpuy.o: /usr/include/sys/_types/_rsize_t.h
c_cpuy.o: /usr/include/sys/_types/_errno_t.h
c_cpuy.o: /usr/include/sys/_types/_fd_def.h
c_cpuy.o: /usr/include/sys/_types/_fd_setsize.h
c_cpuy.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
c_cpuy.o: /usr/include/sys/_types/_fd_zero.h
c_cpuy.o: /usr/include/sys/_types/_fd_isset.h
c_cpuy.o: /usr/include/sys/_types/_fd_copy.h
c_cpuy.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_cpuy.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_cpuy.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_cpuy.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_cpuy.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_cpuy.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_cpuy.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_cpuy.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_cpuy.o: /usr/include/sys/_pthread/_pthread_t.h
c_cpuy.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_cpuy.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_cpuy.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_cpuy.o: ../../../.././include/ncarg/c.h
c_cpfx.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_cpfx.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_cpfx.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_cpfx.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_cpfx.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_cpfx.o: /usr/include/i386/_types.h
c_cpfx.o: /usr/include/sys/_pthread/_pthread_types.h
c_cpfx.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_cpfx.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_cpfx.o: /usr/include/sys/_types/_int16_t.h
c_cpfx.o: /usr/include/sys/_types/_int32_t.h
c_cpfx.o: /usr/include/sys/_types/_int64_t.h
c_cpfx.o: /usr/include/sys/_types/_u_int8_t.h
c_cpfx.o: /usr/include/sys/_types/_u_int16_t.h
c_cpfx.o: /usr/include/sys/_types/_u_int32_t.h
c_cpfx.o: /usr/include/sys/_types/_u_int64_t.h
c_cpfx.o: /usr/include/sys/_types/_intptr_t.h
c_cpfx.o: /usr/include/sys/_types/_uintptr_t.h
c_cpfx.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_cpfx.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_cpfx.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_cpfx.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_cpfx.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_cpfx.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_cpfx.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_cpfx.o: /usr/include/libkern/i386/_OSByteOrder.h
c_cpfx.o: /usr/include/sys/_types/_u_char.h
c_cpfx.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
c_cpfx.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
c_cpfx.o: /usr/include/sys/_types/_blkcnt_t.h
c_cpfx.o: /usr/include/sys/_types/_blksize_t.h
c_cpfx.o: /usr/include/sys/_types/_gid_t.h
c_cpfx.o: /usr/include/sys/_types/_in_addr_t.h
c_cpfx.o: /usr/include/sys/_types/_in_port_t.h
c_cpfx.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
c_cpfx.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_cpfx.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_cpfx.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_cpfx.o: /usr/include/sys/_types/_clock_t.h
c_cpfx.o: /usr/include/sys/_types/_time_t.h
c_cpfx.o: /usr/include/sys/_types/_useconds_t.h
c_cpfx.o: /usr/include/sys/_types/_suseconds_t.h
c_cpfx.o: /usr/include/sys/_types/_rsize_t.h
c_cpfx.o: /usr/include/sys/_types/_errno_t.h
c_cpfx.o: /usr/include/sys/_types/_fd_def.h
c_cpfx.o: /usr/include/sys/_types/_fd_setsize.h
c_cpfx.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
c_cpfx.o: /usr/include/sys/_types/_fd_zero.h
c_cpfx.o: /usr/include/sys/_types/_fd_isset.h
c_cpfx.o: /usr/include/sys/_types/_fd_copy.h
c_cpfx.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_cpfx.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_cpfx.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_cpfx.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_cpfx.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_cpfx.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_cpfx.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_cpfx.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_cpfx.o: /usr/include/sys/_pthread/_pthread_t.h
c_cpfx.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_cpfx.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_cpfx.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_cpfx.o: ../../../.././include/ncarg/c.h
c_cpfy.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_cpfy.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_cpfy.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_cpfy.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_cpfy.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_cpfy.o: /usr/include/i386/_types.h
c_cpfy.o: /usr/include/sys/_pthread/_pthread_types.h
c_cpfy.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_cpfy.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_cpfy.o: /usr/include/sys/_types/_int16_t.h
c_cpfy.o: /usr/include/sys/_types/_int32_t.h
c_cpfy.o: /usr/include/sys/_types/_int64_t.h
c_cpfy.o: /usr/include/sys/_types/_u_int8_t.h
c_cpfy.o: /usr/include/sys/_types/_u_int16_t.h
c_cpfy.o: /usr/include/sys/_types/_u_int32_t.h
c_cpfy.o: /usr/include/sys/_types/_u_int64_t.h
c_cpfy.o: /usr/include/sys/_types/_intptr_t.h
c_cpfy.o: /usr/include/sys/_types/_uintptr_t.h
c_cpfy.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_cpfy.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_cpfy.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_cpfy.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_cpfy.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_cpfy.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_cpfy.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_cpfy.o: /usr/include/libkern/i386/_OSByteOrder.h
c_cpfy.o: /usr/include/sys/_types/_u_char.h
c_cpfy.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
c_cpfy.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
c_cpfy.o: /usr/include/sys/_types/_blkcnt_t.h
c_cpfy.o: /usr/include/sys/_types/_blksize_t.h
c_cpfy.o: /usr/include/sys/_types/_gid_t.h
c_cpfy.o: /usr/include/sys/_types/_in_addr_t.h
c_cpfy.o: /usr/include/sys/_types/_in_port_t.h
c_cpfy.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
c_cpfy.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_cpfy.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_cpfy.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_cpfy.o: /usr/include/sys/_types/_clock_t.h
c_cpfy.o: /usr/include/sys/_types/_time_t.h
c_cpfy.o: /usr/include/sys/_types/_useconds_t.h
c_cpfy.o: /usr/include/sys/_types/_suseconds_t.h
c_cpfy.o: /usr/include/sys/_types/_rsize_t.h
c_cpfy.o: /usr/include/sys/_types/_errno_t.h
c_cpfy.o: /usr/include/sys/_types/_fd_def.h
c_cpfy.o: /usr/include/sys/_types/_fd_setsize.h
c_cpfy.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
c_cpfy.o: /usr/include/sys/_types/_fd_zero.h
c_cpfy.o: /usr/include/sys/_types/_fd_isset.h
c_cpfy.o: /usr/include/sys/_types/_fd_copy.h
c_cpfy.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_cpfy.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_cpfy.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_cpfy.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_cpfy.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_cpfy.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_cpfy.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_cpfy.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_cpfy.o: /usr/include/sys/_pthread/_pthread_t.h
c_cpfy.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_cpfy.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_cpfy.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_cpfy.o: ../../../.././include/ncarg/c.h
c_cmux.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_cmux.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_cmux.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_cmux.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_cmux.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_cmux.o: /usr/include/i386/_types.h
c_cmux.o: /usr/include/sys/_pthread/_pthread_types.h
c_cmux.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_cmux.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_cmux.o: /usr/include/sys/_types/_int16_t.h
c_cmux.o: /usr/include/sys/_types/_int32_t.h
c_cmux.o: /usr/include/sys/_types/_int64_t.h
c_cmux.o: /usr/include/sys/_types/_u_int8_t.h
c_cmux.o: /usr/include/sys/_types/_u_int16_t.h
c_cmux.o: /usr/include/sys/_types/_u_int32_t.h
c_cmux.o: /usr/include/sys/_types/_u_int64_t.h
c_cmux.o: /usr/include/sys/_types/_intptr_t.h
c_cmux.o: /usr/include/sys/_types/_uintptr_t.h
c_cmux.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_cmux.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_cmux.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_cmux.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_cmux.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_cmux.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_cmux.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_cmux.o: /usr/include/libkern/i386/_OSByteOrder.h
c_cmux.o: /usr/include/sys/_types/_u_char.h
c_cmux.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
c_cmux.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
c_cmux.o: /usr/include/sys/_types/_blkcnt_t.h
c_cmux.o: /usr/include/sys/_types/_blksize_t.h
c_cmux.o: /usr/include/sys/_types/_gid_t.h
c_cmux.o: /usr/include/sys/_types/_in_addr_t.h
c_cmux.o: /usr/include/sys/_types/_in_port_t.h
c_cmux.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
c_cmux.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_cmux.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_cmux.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_cmux.o: /usr/include/sys/_types/_clock_t.h
c_cmux.o: /usr/include/sys/_types/_time_t.h
c_cmux.o: /usr/include/sys/_types/_useconds_t.h
c_cmux.o: /usr/include/sys/_types/_suseconds_t.h
c_cmux.o: /usr/include/sys/_types/_rsize_t.h
c_cmux.o: /usr/include/sys/_types/_errno_t.h
c_cmux.o: /usr/include/sys/_types/_fd_def.h
c_cmux.o: /usr/include/sys/_types/_fd_setsize.h
c_cmux.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
c_cmux.o: /usr/include/sys/_types/_fd_zero.h
c_cmux.o: /usr/include/sys/_types/_fd_isset.h
c_cmux.o: /usr/include/sys/_types/_fd_copy.h
c_cmux.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_cmux.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_cmux.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_cmux.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_cmux.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_cmux.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_cmux.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_cmux.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_cmux.o: /usr/include/sys/_pthread/_pthread_t.h
c_cmux.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_cmux.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_cmux.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_cmux.o: ../../../.././include/ncarg/c.h
c_cmuy.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_cmuy.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_cmuy.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_cmuy.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_cmuy.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_cmuy.o: /usr/include/i386/_types.h
c_cmuy.o: /usr/include/sys/_pthread/_pthread_types.h
c_cmuy.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_cmuy.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_cmuy.o: /usr/include/sys/_types/_int16_t.h
c_cmuy.o: /usr/include/sys/_types/_int32_t.h
c_cmuy.o: /usr/include/sys/_types/_int64_t.h
c_cmuy.o: /usr/include/sys/_types/_u_int8_t.h
c_cmuy.o: /usr/include/sys/_types/_u_int16_t.h
c_cmuy.o: /usr/include/sys/_types/_u_int32_t.h
c_cmuy.o: /usr/include/sys/_types/_u_int64_t.h
c_cmuy.o: /usr/include/sys/_types/_intptr_t.h
c_cmuy.o: /usr/include/sys/_types/_uintptr_t.h
c_cmuy.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_cmuy.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_cmuy.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_cmuy.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_cmuy.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_cmuy.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_cmuy.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_cmuy.o: /usr/include/libkern/i386/_OSByteOrder.h
c_cmuy.o: /usr/include/sys/_types/_u_char.h
c_cmuy.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
c_cmuy.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
c_cmuy.o: /usr/include/sys/_types/_blkcnt_t.h
c_cmuy.o: /usr/include/sys/_types/_blksize_t.h
c_cmuy.o: /usr/include/sys/_types/_gid_t.h
c_cmuy.o: /usr/include/sys/_types/_in_addr_t.h
c_cmuy.o: /usr/include/sys/_types/_in_port_t.h
c_cmuy.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
c_cmuy.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_cmuy.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_cmuy.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_cmuy.o: /usr/include/sys/_types/_clock_t.h
c_cmuy.o: /usr/include/sys/_types/_time_t.h
c_cmuy.o: /usr/include/sys/_types/_useconds_t.h
c_cmuy.o: /usr/include/sys/_types/_suseconds_t.h
c_cmuy.o: /usr/include/sys/_types/_rsize_t.h
c_cmuy.o: /usr/include/sys/_types/_errno_t.h
c_cmuy.o: /usr/include/sys/_types/_fd_def.h
c_cmuy.o: /usr/include/sys/_types/_fd_setsize.h
c_cmuy.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
c_cmuy.o: /usr/include/sys/_types/_fd_zero.h
c_cmuy.o: /usr/include/sys/_types/_fd_isset.h
c_cmuy.o: /usr/include/sys/_types/_fd_copy.h
c_cmuy.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_cmuy.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_cmuy.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_cmuy.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_cmuy.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_cmuy.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_cmuy.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_cmuy.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_cmuy.o: /usr/include/sys/_pthread/_pthread_t.h
c_cmuy.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_cmuy.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_cmuy.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_cmuy.o: ../../../.././include/ncarg/c.h
c_kfpx.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_kfpx.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_kfpx.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_kfpx.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_kfpx.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_kfpx.o: /usr/include/i386/_types.h
c_kfpx.o: /usr/include/sys/_pthread/_pthread_types.h
c_kfpx.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_kfpx.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_kfpx.o: /usr/include/sys/_types/_int16_t.h
c_kfpx.o: /usr/include/sys/_types/_int32_t.h
c_kfpx.o: /usr/include/sys/_types/_int64_t.h
c_kfpx.o: /usr/include/sys/_types/_u_int8_t.h
c_kfpx.o: /usr/include/sys/_types/_u_int16_t.h
c_kfpx.o: /usr/include/sys/_types/_u_int32_t.h
c_kfpx.o: /usr/include/sys/_types/_u_int64_t.h
c_kfpx.o: /usr/include/sys/_types/_intptr_t.h
c_kfpx.o: /usr/include/sys/_types/_uintptr_t.h
c_kfpx.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_kfpx.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_kfpx.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_kfpx.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_kfpx.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_kfpx.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_kfpx.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_kfpx.o: /usr/include/libkern/i386/_OSByteOrder.h
c_kfpx.o: /usr/include/sys/_types/_u_char.h
c_kfpx.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
c_kfpx.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
c_kfpx.o: /usr/include/sys/_types/_blkcnt_t.h
c_kfpx.o: /usr/include/sys/_types/_blksize_t.h
c_kfpx.o: /usr/include/sys/_types/_gid_t.h
c_kfpx.o: /usr/include/sys/_types/_in_addr_t.h
c_kfpx.o: /usr/include/sys/_types/_in_port_t.h
c_kfpx.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
c_kfpx.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_kfpx.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_kfpx.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_kfpx.o: /usr/include/sys/_types/_clock_t.h
c_kfpx.o: /usr/include/sys/_types/_time_t.h
c_kfpx.o: /usr/include/sys/_types/_useconds_t.h
c_kfpx.o: /usr/include/sys/_types/_suseconds_t.h
c_kfpx.o: /usr/include/sys/_types/_rsize_t.h
c_kfpx.o: /usr/include/sys/_types/_errno_t.h
c_kfpx.o: /usr/include/sys/_types/_fd_def.h
c_kfpx.o: /usr/include/sys/_types/_fd_setsize.h
c_kfpx.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
c_kfpx.o: /usr/include/sys/_types/_fd_zero.h
c_kfpx.o: /usr/include/sys/_types/_fd_isset.h
c_kfpx.o: /usr/include/sys/_types/_fd_copy.h
c_kfpx.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_kfpx.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_kfpx.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_kfpx.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_kfpx.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_kfpx.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_kfpx.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_kfpx.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_kfpx.o: /usr/include/sys/_pthread/_pthread_t.h
c_kfpx.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_kfpx.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_kfpx.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_kfpx.o: ../../../.././include/ncarg/c.h
c_kfpy.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_kfpy.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_kfpy.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_kfpy.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_kfpy.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_kfpy.o: /usr/include/i386/_types.h
c_kfpy.o: /usr/include/sys/_pthread/_pthread_types.h
c_kfpy.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_kfpy.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_kfpy.o: /usr/include/sys/_types/_int16_t.h
c_kfpy.o: /usr/include/sys/_types/_int32_t.h
c_kfpy.o: /usr/include/sys/_types/_int64_t.h
c_kfpy.o: /usr/include/sys/_types/_u_int8_t.h
c_kfpy.o: /usr/include/sys/_types/_u_int16_t.h
c_kfpy.o: /usr/include/sys/_types/_u_int32_t.h
c_kfpy.o: /usr/include/sys/_types/_u_int64_t.h
c_kfpy.o: /usr/include/sys/_types/_intptr_t.h
c_kfpy.o: /usr/include/sys/_types/_uintptr_t.h
c_kfpy.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_kfpy.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_kfpy.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_kfpy.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_kfpy.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_kfpy.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_kfpy.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_kfpy.o: /usr/include/libkern/i386/_OSByteOrder.h
c_kfpy.o: /usr/include/sys/_types/_u_char.h
c_kfpy.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
c_kfpy.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
c_kfpy.o: /usr/include/sys/_types/_blkcnt_t.h
c_kfpy.o: /usr/include/sys/_types/_blksize_t.h
c_kfpy.o: /usr/include/sys/_types/_gid_t.h
c_kfpy.o: /usr/include/sys/_types/_in_addr_t.h
c_kfpy.o: /usr/include/sys/_types/_in_port_t.h
c_kfpy.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
c_kfpy.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_kfpy.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_kfpy.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_kfpy.o: /usr/include/sys/_types/_clock_t.h
c_kfpy.o: /usr/include/sys/_types/_time_t.h
c_kfpy.o: /usr/include/sys/_types/_useconds_t.h
c_kfpy.o: /usr/include/sys/_types/_suseconds_t.h
c_kfpy.o: /usr/include/sys/_types/_rsize_t.h
c_kfpy.o: /usr/include/sys/_types/_errno_t.h
c_kfpy.o: /usr/include/sys/_types/_fd_def.h
c_kfpy.o: /usr/include/sys/_types/_fd_setsize.h
c_kfpy.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
c_kfpy.o: /usr/include/sys/_types/_fd_zero.h
c_kfpy.o: /usr/include/sys/_types/_fd_isset.h
c_kfpy.o: /usr/include/sys/_types/_fd_copy.h
c_kfpy.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_kfpy.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_kfpy.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_kfpy.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_kfpy.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_kfpy.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_kfpy.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_kfpy.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_kfpy.o: /usr/include/sys/_pthread/_pthread_t.h
c_kfpy.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_kfpy.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_kfpy.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_kfpy.o: ../../../.././include/ncarg/c.h
c_kfmx.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_kfmx.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_kfmx.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_kfmx.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_kfmx.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_kfmx.o: /usr/include/i386/_types.h
c_kfmx.o: /usr/include/sys/_pthread/_pthread_types.h
c_kfmx.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_kfmx.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_kfmx.o: /usr/include/sys/_types/_int16_t.h
c_kfmx.o: /usr/include/sys/_types/_int32_t.h
c_kfmx.o: /usr/include/sys/_types/_int64_t.h
c_kfmx.o: /usr/include/sys/_types/_u_int8_t.h
c_kfmx.o: /usr/include/sys/_types/_u_int16_t.h
c_kfmx.o: /usr/include/sys/_types/_u_int32_t.h
c_kfmx.o: /usr/include/sys/_types/_u_int64_t.h
c_kfmx.o: /usr/include/sys/_types/_intptr_t.h
c_kfmx.o: /usr/include/sys/_types/_uintptr_t.h
c_kfmx.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_kfmx.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_kfmx.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_kfmx.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_kfmx.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_kfmx.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_kfmx.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_kfmx.o: /usr/include/libkern/i386/_OSByteOrder.h
c_kfmx.o: /usr/include/sys/_types/_u_char.h
c_kfmx.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
c_kfmx.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
c_kfmx.o: /usr/include/sys/_types/_blkcnt_t.h
c_kfmx.o: /usr/include/sys/_types/_blksize_t.h
c_kfmx.o: /usr/include/sys/_types/_gid_t.h
c_kfmx.o: /usr/include/sys/_types/_in_addr_t.h
c_kfmx.o: /usr/include/sys/_types/_in_port_t.h
c_kfmx.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
c_kfmx.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_kfmx.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_kfmx.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_kfmx.o: /usr/include/sys/_types/_clock_t.h
c_kfmx.o: /usr/include/sys/_types/_time_t.h
c_kfmx.o: /usr/include/sys/_types/_useconds_t.h
c_kfmx.o: /usr/include/sys/_types/_suseconds_t.h
c_kfmx.o: /usr/include/sys/_types/_rsize_t.h
c_kfmx.o: /usr/include/sys/_types/_errno_t.h
c_kfmx.o: /usr/include/sys/_types/_fd_def.h
c_kfmx.o: /usr/include/sys/_types/_fd_setsize.h
c_kfmx.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
c_kfmx.o: /usr/include/sys/_types/_fd_zero.h
c_kfmx.o: /usr/include/sys/_types/_fd_isset.h
c_kfmx.o: /usr/include/sys/_types/_fd_copy.h
c_kfmx.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_kfmx.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_kfmx.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_kfmx.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_kfmx.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_kfmx.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_kfmx.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_kfmx.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_kfmx.o: /usr/include/sys/_pthread/_pthread_t.h
c_kfmx.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_kfmx.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_kfmx.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_kfmx.o: ../../../.././include/ncarg/c.h
c_kfmy.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_kfmy.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_kfmy.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_kfmy.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_kfmy.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_kfmy.o: /usr/include/i386/_types.h
c_kfmy.o: /usr/include/sys/_pthread/_pthread_types.h
c_kfmy.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_kfmy.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_kfmy.o: /usr/include/sys/_types/_int16_t.h
c_kfmy.o: /usr/include/sys/_types/_int32_t.h
c_kfmy.o: /usr/include/sys/_types/_int64_t.h
c_kfmy.o: /usr/include/sys/_types/_u_int8_t.h
c_kfmy.o: /usr/include/sys/_types/_u_int16_t.h
c_kfmy.o: /usr/include/sys/_types/_u_int32_t.h
c_kfmy.o: /usr/include/sys/_types/_u_int64_t.h
c_kfmy.o: /usr/include/sys/_types/_intptr_t.h
c_kfmy.o: /usr/include/sys/_types/_uintptr_t.h
c_kfmy.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_kfmy.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_kfmy.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_kfmy.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_kfmy.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_kfmy.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_kfmy.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_kfmy.o: /usr/include/libkern/i386/_OSByteOrder.h
c_kfmy.o: /usr/include/sys/_types/_u_char.h
c_kfmy.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
c_kfmy.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
c_kfmy.o: /usr/include/sys/_types/_blkcnt_t.h
c_kfmy.o: /usr/include/sys/_types/_blksize_t.h
c_kfmy.o: /usr/include/sys/_types/_gid_t.h
c_kfmy.o: /usr/include/sys/_types/_in_addr_t.h
c_kfmy.o: /usr/include/sys/_types/_in_port_t.h
c_kfmy.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
c_kfmy.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_kfmy.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_kfmy.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_kfmy.o: /usr/include/sys/_types/_clock_t.h
c_kfmy.o: /usr/include/sys/_types/_time_t.h
c_kfmy.o: /usr/include/sys/_types/_useconds_t.h
c_kfmy.o: /usr/include/sys/_types/_suseconds_t.h
c_kfmy.o: /usr/include/sys/_types/_rsize_t.h
c_kfmy.o: /usr/include/sys/_types/_errno_t.h
c_kfmy.o: /usr/include/sys/_types/_fd_def.h
c_kfmy.o: /usr/include/sys/_types/_fd_setsize.h
c_kfmy.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
c_kfmy.o: /usr/include/sys/_types/_fd_zero.h
c_kfmy.o: /usr/include/sys/_types/_fd_isset.h
c_kfmy.o: /usr/include/sys/_types/_fd_copy.h
c_kfmy.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_kfmy.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_kfmy.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_kfmy.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_kfmy.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_kfmy.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_kfmy.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_kfmy.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_kfmy.o: /usr/include/sys/_pthread/_pthread_t.h
c_kfmy.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_kfmy.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_kfmy.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_kfmy.o: ../../../.././include/ncarg/c.h
c_kupx.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_kupx.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_kupx.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_kupx.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_kupx.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_kupx.o: /usr/include/i386/_types.h
c_kupx.o: /usr/include/sys/_pthread/_pthread_types.h
c_kupx.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_kupx.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_kupx.o: /usr/include/sys/_types/_int16_t.h
c_kupx.o: /usr/include/sys/_types/_int32_t.h
c_kupx.o: /usr/include/sys/_types/_int64_t.h
c_kupx.o: /usr/include/sys/_types/_u_int8_t.h
c_kupx.o: /usr/include/sys/_types/_u_int16_t.h
c_kupx.o: /usr/include/sys/_types/_u_int32_t.h
c_kupx.o: /usr/include/sys/_types/_u_int64_t.h
c_kupx.o: /usr/include/sys/_types/_intptr_t.h
c_kupx.o: /usr/include/sys/_types/_uintptr_t.h
c_kupx.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_kupx.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_kupx.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_kupx.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_kupx.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_kupx.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_kupx.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_kupx.o: /usr/include/libkern/i386/_OSByteOrder.h
c_kupx.o: /usr/include/sys/_types/_u_char.h
c_kupx.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
c_kupx.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
c_kupx.o: /usr/include/sys/_types/_blkcnt_t.h
c_kupx.o: /usr/include/sys/_types/_blksize_t.h
c_kupx.o: /usr/include/sys/_types/_gid_t.h
c_kupx.o: /usr/include/sys/_types/_in_addr_t.h
c_kupx.o: /usr/include/sys/_types/_in_port_t.h
c_kupx.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
c_kupx.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_kupx.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_kupx.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_kupx.o: /usr/include/sys/_types/_clock_t.h
c_kupx.o: /usr/include/sys/_types/_time_t.h
c_kupx.o: /usr/include/sys/_types/_useconds_t.h
c_kupx.o: /usr/include/sys/_types/_suseconds_t.h
c_kupx.o: /usr/include/sys/_types/_rsize_t.h
c_kupx.o: /usr/include/sys/_types/_errno_t.h
c_kupx.o: /usr/include/sys/_types/_fd_def.h
c_kupx.o: /usr/include/sys/_types/_fd_setsize.h
c_kupx.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
c_kupx.o: /usr/include/sys/_types/_fd_zero.h
c_kupx.o: /usr/include/sys/_types/_fd_isset.h
c_kupx.o: /usr/include/sys/_types/_fd_copy.h
c_kupx.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_kupx.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_kupx.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_kupx.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_kupx.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_kupx.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_kupx.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_kupx.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_kupx.o: /usr/include/sys/_pthread/_pthread_t.h
c_kupx.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_kupx.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_kupx.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_kupx.o: ../../../.././include/ncarg/c.h
c_kupy.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_kupy.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_kupy.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_kupy.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_kupy.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_kupy.o: /usr/include/i386/_types.h
c_kupy.o: /usr/include/sys/_pthread/_pthread_types.h
c_kupy.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_kupy.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_kupy.o: /usr/include/sys/_types/_int16_t.h
c_kupy.o: /usr/include/sys/_types/_int32_t.h
c_kupy.o: /usr/include/sys/_types/_int64_t.h
c_kupy.o: /usr/include/sys/_types/_u_int8_t.h
c_kupy.o: /usr/include/sys/_types/_u_int16_t.h
c_kupy.o: /usr/include/sys/_types/_u_int32_t.h
c_kupy.o: /usr/include/sys/_types/_u_int64_t.h
c_kupy.o: /usr/include/sys/_types/_intptr_t.h
c_kupy.o: /usr/include/sys/_types/_uintptr_t.h
c_kupy.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_kupy.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_kupy.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_kupy.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_kupy.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_kupy.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_kupy.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_kupy.o: /usr/include/libkern/i386/_OSByteOrder.h
c_kupy.o: /usr/include/sys/_types/_u_char.h
c_kupy.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
c_kupy.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
c_kupy.o: /usr/include/sys/_types/_blkcnt_t.h
c_kupy.o: /usr/include/sys/_types/_blksize_t.h
c_kupy.o: /usr/include/sys/_types/_gid_t.h
c_kupy.o: /usr/include/sys/_types/_in_addr_t.h
c_kupy.o: /usr/include/sys/_types/_in_port_t.h
c_kupy.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
c_kupy.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_kupy.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_kupy.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_kupy.o: /usr/include/sys/_types/_clock_t.h
c_kupy.o: /usr/include/sys/_types/_time_t.h
c_kupy.o: /usr/include/sys/_types/_useconds_t.h
c_kupy.o: /usr/include/sys/_types/_suseconds_t.h
c_kupy.o: /usr/include/sys/_types/_rsize_t.h
c_kupy.o: /usr/include/sys/_types/_errno_t.h
c_kupy.o: /usr/include/sys/_types/_fd_def.h
c_kupy.o: /usr/include/sys/_types/_fd_setsize.h
c_kupy.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
c_kupy.o: /usr/include/sys/_types/_fd_zero.h
c_kupy.o: /usr/include/sys/_types/_fd_isset.h
c_kupy.o: /usr/include/sys/_types/_fd_copy.h
c_kupy.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_kupy.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_kupy.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_kupy.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_kupy.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_kupy.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_kupy.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_kupy.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_kupy.o: /usr/include/sys/_pthread/_pthread_t.h
c_kupy.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_kupy.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_kupy.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_kupy.o: ../../../.././include/ncarg/c.h
c_kumx.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_kumx.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_kumx.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_kumx.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_kumx.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_kumx.o: /usr/include/i386/_types.h
c_kumx.o: /usr/include/sys/_pthread/_pthread_types.h
c_kumx.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_kumx.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_kumx.o: /usr/include/sys/_types/_int16_t.h
c_kumx.o: /usr/include/sys/_types/_int32_t.h
c_kumx.o: /usr/include/sys/_types/_int64_t.h
c_kumx.o: /usr/include/sys/_types/_u_int8_t.h
c_kumx.o: /usr/include/sys/_types/_u_int16_t.h
c_kumx.o: /usr/include/sys/_types/_u_int32_t.h
c_kumx.o: /usr/include/sys/_types/_u_int64_t.h
c_kumx.o: /usr/include/sys/_types/_intptr_t.h
c_kumx.o: /usr/include/sys/_types/_uintptr_t.h
c_kumx.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_kumx.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_kumx.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_kumx.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_kumx.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_kumx.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_kumx.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_kumx.o: /usr/include/libkern/i386/_OSByteOrder.h
c_kumx.o: /usr/include/sys/_types/_u_char.h
c_kumx.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
c_kumx.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
c_kumx.o: /usr/include/sys/_types/_blkcnt_t.h
c_kumx.o: /usr/include/sys/_types/_blksize_t.h
c_kumx.o: /usr/include/sys/_types/_gid_t.h
c_kumx.o: /usr/include/sys/_types/_in_addr_t.h
c_kumx.o: /usr/include/sys/_types/_in_port_t.h
c_kumx.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
c_kumx.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_kumx.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_kumx.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_kumx.o: /usr/include/sys/_types/_clock_t.h
c_kumx.o: /usr/include/sys/_types/_time_t.h
c_kumx.o: /usr/include/sys/_types/_useconds_t.h
c_kumx.o: /usr/include/sys/_types/_suseconds_t.h
c_kumx.o: /usr/include/sys/_types/_rsize_t.h
c_kumx.o: /usr/include/sys/_types/_errno_t.h
c_kumx.o: /usr/include/sys/_types/_fd_def.h
c_kumx.o: /usr/include/sys/_types/_fd_setsize.h
c_kumx.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
c_kumx.o: /usr/include/sys/_types/_fd_zero.h
c_kumx.o: /usr/include/sys/_types/_fd_isset.h
c_kumx.o: /usr/include/sys/_types/_fd_copy.h
c_kumx.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_kumx.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_kumx.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_kumx.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_kumx.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_kumx.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_kumx.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_kumx.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_kumx.o: /usr/include/sys/_pthread/_pthread_t.h
c_kumx.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_kumx.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_kumx.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_kumx.o: ../../../.././include/ncarg/c.h
c_kumy.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_kumy.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_kumy.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_kumy.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_kumy.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_kumy.o: /usr/include/i386/_types.h
c_kumy.o: /usr/include/sys/_pthread/_pthread_types.h
c_kumy.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_kumy.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_kumy.o: /usr/include/sys/_types/_int16_t.h
c_kumy.o: /usr/include/sys/_types/_int32_t.h
c_kumy.o: /usr/include/sys/_types/_int64_t.h
c_kumy.o: /usr/include/sys/_types/_u_int8_t.h
c_kumy.o: /usr/include/sys/_types/_u_int16_t.h
c_kumy.o: /usr/include/sys/_types/_u_int32_t.h
c_kumy.o: /usr/include/sys/_types/_u_int64_t.h
c_kumy.o: /usr/include/sys/_types/_intptr_t.h
c_kumy.o: /usr/include/sys/_types/_uintptr_t.h
c_kumy.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_kumy.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_kumy.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_kumy.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_kumy.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_kumy.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_kumy.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_kumy.o: /usr/include/libkern/i386/_OSByteOrder.h
c_kumy.o: /usr/include/sys/_types/_u_char.h
c_kumy.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
c_kumy.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
c_kumy.o: /usr/include/sys/_types/_blkcnt_t.h
c_kumy.o: /usr/include/sys/_types/_blksize_t.h
c_kumy.o: /usr/include/sys/_types/_gid_t.h
c_kumy.o: /usr/include/sys/_types/_in_addr_t.h
c_kumy.o: /usr/include/sys/_types/_in_port_t.h
c_kumy.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
c_kumy.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_kumy.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_kumy.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_kumy.o: /usr/include/sys/_types/_clock_t.h
c_kumy.o: /usr/include/sys/_types/_time_t.h
c_kumy.o: /usr/include/sys/_types/_useconds_t.h
c_kumy.o: /usr/include/sys/_types/_suseconds_t.h
c_kumy.o: /usr/include/sys/_types/_rsize_t.h
c_kumy.o: /usr/include/sys/_types/_errno_t.h
c_kumy.o: /usr/include/sys/_types/_fd_def.h
c_kumy.o: /usr/include/sys/_types/_fd_setsize.h
c_kumy.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
c_kumy.o: /usr/include/sys/_types/_fd_zero.h
c_kumy.o: /usr/include/sys/_types/_fd_isset.h
c_kumy.o: /usr/include/sys/_types/_fd_copy.h
c_kumy.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_kumy.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_kumy.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_kumy.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_kumy.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_kumy.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_kumy.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_kumy.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_kumy.o: /usr/include/sys/_pthread/_pthread_t.h
c_kumy.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_kumy.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_kumy.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_kumy.o: ../../../.././include/ncarg/c.h
c_kpmx.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_kpmx.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_kpmx.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_kpmx.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_kpmx.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_kpmx.o: /usr/include/i386/_types.h
c_kpmx.o: /usr/include/sys/_pthread/_pthread_types.h
c_kpmx.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_kpmx.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_kpmx.o: /usr/include/sys/_types/_int16_t.h
c_kpmx.o: /usr/include/sys/_types/_int32_t.h
c_kpmx.o: /usr/include/sys/_types/_int64_t.h
c_kpmx.o: /usr/include/sys/_types/_u_int8_t.h
c_kpmx.o: /usr/include/sys/_types/_u_int16_t.h
c_kpmx.o: /usr/include/sys/_types/_u_int32_t.h
c_kpmx.o: /usr/include/sys/_types/_u_int64_t.h
c_kpmx.o: /usr/include/sys/_types/_intptr_t.h
c_kpmx.o: /usr/include/sys/_types/_uintptr_t.h
c_kpmx.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_kpmx.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_kpmx.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_kpmx.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_kpmx.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_kpmx.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_kpmx.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_kpmx.o: /usr/include/libkern/i386/_OSByteOrder.h
c_kpmx.o: /usr/include/sys/_types/_u_char.h
c_kpmx.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
c_kpmx.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
c_kpmx.o: /usr/include/sys/_types/_blkcnt_t.h
c_kpmx.o: /usr/include/sys/_types/_blksize_t.h
c_kpmx.o: /usr/include/sys/_types/_gid_t.h
c_kpmx.o: /usr/include/sys/_types/_in_addr_t.h
c_kpmx.o: /usr/include/sys/_types/_in_port_t.h
c_kpmx.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
c_kpmx.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_kpmx.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_kpmx.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_kpmx.o: /usr/include/sys/_types/_clock_t.h
c_kpmx.o: /usr/include/sys/_types/_time_t.h
c_kpmx.o: /usr/include/sys/_types/_useconds_t.h
c_kpmx.o: /usr/include/sys/_types/_suseconds_t.h
c_kpmx.o: /usr/include/sys/_types/_rsize_t.h
c_kpmx.o: /usr/include/sys/_types/_errno_t.h
c_kpmx.o: /usr/include/sys/_types/_fd_def.h
c_kpmx.o: /usr/include/sys/_types/_fd_setsize.h
c_kpmx.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
c_kpmx.o: /usr/include/sys/_types/_fd_zero.h
c_kpmx.o: /usr/include/sys/_types/_fd_isset.h
c_kpmx.o: /usr/include/sys/_types/_fd_copy.h
c_kpmx.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_kpmx.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_kpmx.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_kpmx.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_kpmx.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_kpmx.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_kpmx.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_kpmx.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_kpmx.o: /usr/include/sys/_pthread/_pthread_t.h
c_kpmx.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_kpmx.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_kpmx.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_kpmx.o: ../../../.././include/ncarg/c.h
c_kpmy.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_kpmy.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_kpmy.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_kpmy.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_kpmy.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_kpmy.o: /usr/include/i386/_types.h
c_kpmy.o: /usr/include/sys/_pthread/_pthread_types.h
c_kpmy.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_kpmy.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_kpmy.o: /usr/include/sys/_types/_int16_t.h
c_kpmy.o: /usr/include/sys/_types/_int32_t.h
c_kpmy.o: /usr/include/sys/_types/_int64_t.h
c_kpmy.o: /usr/include/sys/_types/_u_int8_t.h
c_kpmy.o: /usr/include/sys/_types/_u_int16_t.h
c_kpmy.o: /usr/include/sys/_types/_u_int32_t.h
c_kpmy.o: /usr/include/sys/_types/_u_int64_t.h
c_kpmy.o: /usr/include/sys/_types/_intptr_t.h
c_kpmy.o: /usr/include/sys/_types/_uintptr_t.h
c_kpmy.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_kpmy.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_kpmy.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_kpmy.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_kpmy.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_kpmy.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_kpmy.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_kpmy.o: /usr/include/libkern/i386/_OSByteOrder.h
c_kpmy.o: /usr/include/sys/_types/_u_char.h
c_kpmy.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
c_kpmy.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
c_kpmy.o: /usr/include/sys/_types/_blkcnt_t.h
c_kpmy.o: /usr/include/sys/_types/_blksize_t.h
c_kpmy.o: /usr/include/sys/_types/_gid_t.h
c_kpmy.o: /usr/include/sys/_types/_in_addr_t.h
c_kpmy.o: /usr/include/sys/_types/_in_port_t.h
c_kpmy.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
c_kpmy.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_kpmy.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_kpmy.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_kpmy.o: /usr/include/sys/_types/_clock_t.h
c_kpmy.o: /usr/include/sys/_types/_time_t.h
c_kpmy.o: /usr/include/sys/_types/_useconds_t.h
c_kpmy.o: /usr/include/sys/_types/_suseconds_t.h
c_kpmy.o: /usr/include/sys/_types/_rsize_t.h
c_kpmy.o: /usr/include/sys/_types/_errno_t.h
c_kpmy.o: /usr/include/sys/_types/_fd_def.h
c_kpmy.o: /usr/include/sys/_types/_fd_setsize.h
c_kpmy.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
c_kpmy.o: /usr/include/sys/_types/_fd_zero.h
c_kpmy.o: /usr/include/sys/_types/_fd_isset.h
c_kpmy.o: /usr/include/sys/_types/_fd_copy.h
c_kpmy.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_kpmy.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_kpmy.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_kpmy.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_kpmy.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_kpmy.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_kpmy.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_kpmy.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_kpmy.o: /usr/include/sys/_pthread/_pthread_t.h
c_kpmy.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_kpmy.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_kpmy.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_kpmy.o: ../../../.././include/ncarg/c.h
c_kmpx.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_kmpx.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_kmpx.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_kmpx.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_kmpx.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_kmpx.o: /usr/include/i386/_types.h
c_kmpx.o: /usr/include/sys/_pthread/_pthread_types.h
c_kmpx.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_kmpx.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_kmpx.o: /usr/include/sys/_types/_int16_t.h
c_kmpx.o: /usr/include/sys/_types/_int32_t.h
c_kmpx.o: /usr/include/sys/_types/_int64_t.h
c_kmpx.o: /usr/include/sys/_types/_u_int8_t.h
c_kmpx.o: /usr/include/sys/_types/_u_int16_t.h
c_kmpx.o: /usr/include/sys/_types/_u_int32_t.h
c_kmpx.o: /usr/include/sys/_types/_u_int64_t.h
c_kmpx.o: /usr/include/sys/_types/_intptr_t.h
c_kmpx.o: /usr/include/sys/_types/_uintptr_t.h
c_kmpx.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_kmpx.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_kmpx.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_kmpx.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_kmpx.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_kmpx.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_kmpx.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_kmpx.o: /usr/include/libkern/i386/_OSByteOrder.h
c_kmpx.o: /usr/include/sys/_types/_u_char.h
c_kmpx.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
c_kmpx.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
c_kmpx.o: /usr/include/sys/_types/_blkcnt_t.h
c_kmpx.o: /usr/include/sys/_types/_blksize_t.h
c_kmpx.o: /usr/include/sys/_types/_gid_t.h
c_kmpx.o: /usr/include/sys/_types/_in_addr_t.h
c_kmpx.o: /usr/include/sys/_types/_in_port_t.h
c_kmpx.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
c_kmpx.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_kmpx.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_kmpx.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_kmpx.o: /usr/include/sys/_types/_clock_t.h
c_kmpx.o: /usr/include/sys/_types/_time_t.h
c_kmpx.o: /usr/include/sys/_types/_useconds_t.h
c_kmpx.o: /usr/include/sys/_types/_suseconds_t.h
c_kmpx.o: /usr/include/sys/_types/_rsize_t.h
c_kmpx.o: /usr/include/sys/_types/_errno_t.h
c_kmpx.o: /usr/include/sys/_types/_fd_def.h
c_kmpx.o: /usr/include/sys/_types/_fd_setsize.h
c_kmpx.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
c_kmpx.o: /usr/include/sys/_types/_fd_zero.h
c_kmpx.o: /usr/include/sys/_types/_fd_isset.h
c_kmpx.o: /usr/include/sys/_types/_fd_copy.h
c_kmpx.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_kmpx.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_kmpx.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_kmpx.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_kmpx.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_kmpx.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_kmpx.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_kmpx.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_kmpx.o: /usr/include/sys/_pthread/_pthread_t.h
c_kmpx.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_kmpx.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_kmpx.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_kmpx.o: ../../../.././include/ncarg/c.h
c_kmpy.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_kmpy.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_kmpy.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_kmpy.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_kmpy.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_kmpy.o: /usr/include/i386/_types.h
c_kmpy.o: /usr/include/sys/_pthread/_pthread_types.h
c_kmpy.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_kmpy.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_kmpy.o: /usr/include/sys/_types/_int16_t.h
c_kmpy.o: /usr/include/sys/_types/_int32_t.h
c_kmpy.o: /usr/include/sys/_types/_int64_t.h
c_kmpy.o: /usr/include/sys/_types/_u_int8_t.h
c_kmpy.o: /usr/include/sys/_types/_u_int16_t.h
c_kmpy.o: /usr/include/sys/_types/_u_int32_t.h
c_kmpy.o: /usr/include/sys/_types/_u_int64_t.h
c_kmpy.o: /usr/include/sys/_types/_intptr_t.h
c_kmpy.o: /usr/include/sys/_types/_uintptr_t.h
c_kmpy.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_kmpy.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_kmpy.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_kmpy.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_kmpy.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_kmpy.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_kmpy.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_kmpy.o: /usr/include/libkern/i386/_OSByteOrder.h
c_kmpy.o: /usr/include/sys/_types/_u_char.h
c_kmpy.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
c_kmpy.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
c_kmpy.o: /usr/include/sys/_types/_blkcnt_t.h
c_kmpy.o: /usr/include/sys/_types/_blksize_t.h
c_kmpy.o: /usr/include/sys/_types/_gid_t.h
c_kmpy.o: /usr/include/sys/_types/_in_addr_t.h
c_kmpy.o: /usr/include/sys/_types/_in_port_t.h
c_kmpy.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
c_kmpy.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_kmpy.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_kmpy.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_kmpy.o: /usr/include/sys/_types/_clock_t.h
c_kmpy.o: /usr/include/sys/_types/_time_t.h
c_kmpy.o: /usr/include/sys/_types/_useconds_t.h
c_kmpy.o: /usr/include/sys/_types/_suseconds_t.h
c_kmpy.o: /usr/include/sys/_types/_rsize_t.h
c_kmpy.o: /usr/include/sys/_types/_errno_t.h
c_kmpy.o: /usr/include/sys/_types/_fd_def.h
c_kmpy.o: /usr/include/sys/_types/_fd_setsize.h
c_kmpy.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
c_kmpy.o: /usr/include/sys/_types/_fd_zero.h
c_kmpy.o: /usr/include/sys/_types/_fd_isset.h
c_kmpy.o: /usr/include/sys/_types/_fd_copy.h
c_kmpy.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_kmpy.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_kmpy.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_kmpy.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_kmpy.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_kmpy.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_kmpy.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_kmpy.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_kmpy.o: /usr/include/sys/_pthread/_pthread_t.h
c_kmpy.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_kmpy.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_kmpy.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_kmpy.o: ../../../.././include/ncarg/c.h
