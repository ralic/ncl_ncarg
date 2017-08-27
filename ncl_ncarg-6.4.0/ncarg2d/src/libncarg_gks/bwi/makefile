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
CURRENT_DIR		= ./ncarg2d/src/libncarg_gks/bwi

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
#	$Id: yMakefile,v 1.13 2010-03-16 01:59:21 haley Exp $
#

MYNAME	= bwi

GENERIC_SRCS = g01bkd.f  g01ca.f   g01clp.f  g01clw.f  g01ctb.f  g01d2r.f  g01d2s.f  g01diq.f g01dmp.f  g01esc.f  g01fa.f   g01flb.f  g01iac.f  g01iws.f  g01mio.f  g01msg.f g01pat.f  g01pbl.f  g01pl.f   g01pm.f   g01sas.f  g01seg.f  g01siq.f  g01snp.f g01tx.f   g01wdr.f  g01wkt.f  gflcnv.f  giargs.f  ginlod.f  gkasci.f gmflod.f  gmpart.f  gopnic.f  gputmd.f  gputni.f  gputpr.f  gputps.f  gtnlen.f gupasf.f  gupdva.f  gupdvi.f  gupdvr.f  argb2ci.f

GENERIC_OBJS = g01bkd.o  g01ca.o   g01clp.o  g01clw.o  g01ctb.o  g01d2r.o  g01d2s.o  g01diq.o g01dmp.o  g01esc.o  g01fa.o   g01flb.o  g01iac.o  g01iws.o  g01mio.o  g01msg.o g01pat.o  g01pbl.o  g01pl.o   g01pm.o   g01sas.o  g01seg.o  g01siq.o  g01snp.o g01tx.o   g01wdr.o  g01wkt.o  gflcnv.o  giargs.o  ginlod.o  gkasci.o gmflod.o  gmpart.o  gopnic.o  gputmd.o  gputni.o  gputpr.o  gputps.o  gtnlen.o gupasf.o  gupdva.o  gupdvi.o  gupdvr.o  argb2ci.o

EXTRA_CDEFINES   = -DNGTMPDIR='"$(NGTMPDIR)"' -DDEFAULT_GKS_BUFSIZE='$(GKS_BUFFER_SIZE)'

OTHER_SRCS = wks.c wks.h
OTHER_OBJS = wks.o

SOURCES = $(GENERIC_SRCS) $(OTHER_SRCS)
OBJECTS = $(GENERIC_OBJS) $(OTHER_OBJS)

all::

all-local::../libncarg_gks.a

../libncarg_gks.a::  $(OBJECTS)
	$(AR_REG) ../libncarg_gks.a  $(OBJECTS)

all::

clean-local::
	-@$(RM)  $(OBJECTS)

all::

all-local::../libbasegks.a

../libbasegks.a::  $(OBJECTS)
	$(AR_REG) ../libbasegks.a  $(OBJECTS)

all::

clean-local::
	-@$(RM)  $(OBJECTS)

all::

depend-local:: wks.c
	$(DEPEND) -s "# DO NOT DELETE" -- $(ALLDEFINES) -- wks.c

g01bkd.o : g01bkd.f     g01prm.h     g01wsl.doc     g01wsl.h     g01wdt.doc     g01wdt.h     g01ins.doc     g01ins.h     g01dfa.doc     g01dfa.h     g01adc.doc     g01adc.h     g01io.doc     g01io.h     gksenu.doc     gksenu.h     g01opc.doc     g01opc.h

g01ca.o : g01ca.f     g01prm.h     gksin.h     g01io.h     g01ins.h     g01wsl.h     gksenu.h     g01opc.h

g01clp.o : g01clp.f     g01prm.h     gksin.h     g01wsl.h     g01ins.h     g01opc.h

g01clw.o : g01clw.f     g01prm.h     g01rqa.h     gksin.h     g01wsl.h     g01io.h     g01ins.h     g01ast.h     g01dfa.h     g01opc.h     gksenu.h

g01ctb.o : g01ctb.f     g01prm.h     gksin.h     g01wsl.h     g01ins.h     g01opc.h     gksenu.h

g01d2r.o : g01d2r.f     g01prm.h     g01rqa.h     g01dfa.h

g01d2s.o : g01d2s.f     g01prm.h     g01ast.h     g01dfa.h

g01diq.o : g01diq.f     g01prm.h     gksin.h     g01wdt.h     g01ins.h

g01dmp.o : g01dmp.f     g01prm.h     g01wdt.h     g01wsl.h     g01adc.h     g01rqa.h     g01ast.h     g01dfa.h     g01io.h     g01ins.h     g01opc.h     gksenu.h

g01esc.o : g01esc.f     g01prm.h     gksin.h     g01ins.h

g01fa.o : g01fa.f     g01prm.h     gksin.h     g01io.h     g01ins.h     g01wsl.h     g01rqa.h     g01ast.h     g01adc.h     g01opc.h     gksenu.h

g01flb.o : g01flb.f     g01prm.h     g01io.h

g01iac.o : g01iac.f     g01prm.h     g01rqa.h     g01ast.h     g01dfa.h     g01adc.h

g01iws.o : g01iws.f     g01prm.h     g01wsl.h     g01ins.h     gksenu.h

g01msg.o : g01msg.f     g01prm.h     gksin.h     g01ins.h

g01pat.o : g01pat.f     gksin.h     g01ins.h     g01adc.h

g01pbl.o : g01pbl.f

g01pl.o : g01pl.f     g01prm.h     gksin.h     g01io.h     g01ins.h     g01wsl.h     g01rqa.h     g01ast.h     g01adc.h     g01opc.h     gksenu.h

g01pm.o : g01pm.f     gksin.h     g01io.h     g01ins.h     g01wsl.h     g01rqa.h     g01ast.h     g01adc.h     g01opc.h     gksenu.h

g01sas.o : g01sas.f     g01prm.h     g01ins.h     g01adc.h     g01rqa.h     g01ast.h     g01opc.h

g01seg.o : g01seg.f     g01prm.h     gksin.h     g01io.h

g01siq.o : g01siq.f     gksin.h     g01wsl.h     g01ins.h

g01snp.o : g01snp.f     g01prm.h     g01io.h     g01opc.h     g01ins.h     g01wsl.h     gksenu.h

g01tx.o : g01tx.f     g01prm.h     gksin.h     g01io.h     g01ins.h     g01wsl.h     g01rqa.h     g01ast.h     g01adc.h     g01opc.h     gksenu.h

g01wdr.o : g01wdr.f     g01prm.h     gksin.h     g01wdt.h     g01wsl.h     g01adc.h     g01rqa.h     g01ast.h     g01dfa.h     g01io.h     g01ins.h     g01opc.h     gksenu.h

g01wkt.o : g01wkt.f     g01prm.h     gksin.h     g01wsl.h     g01ins.h

gflcnv.o : gflcnv.f

giargs.o : giargs.f

ginlod.o : ginlod.f     g01ins.h     g01io.h

gkasci.o : gkasci.f

gmflod.o : gmflod.f     g01prm.h     g01io.h

gmpart.o : gmpart.f     g01prm.h     g01ins.h     g01io.h

gopnic.o : gopnic.f

gputmd.o : gputmd.f     g01prm.h     g01wdt.h     g01opc.h     g01ins.h

gputni.o : gputni.f     g01prm.h     g01ins.h

gputpr.o : gputpr.f     g01prm.h     g01ins.h

gputps.o : gputps.f     g01prm.h     g01ins.h

gtnlen.o : gtnlen.f

gupasf.o : gupasf.f     gksin.h     g01rqa.h     g01ast.h     g01adc.h

gupdva.o : gupdva.f     g01prm.h     gksin.h     g01rqa.h     g01ast.h     g01adc.h

gupdvi.o : gupdvi.f     g01prm.h     g01rqa.h     g01ast.h     g01adc.h

gupdvr.o : gupdvr.f     g01prm.h     g01rqa.h     g01ast.h     g01adc.h

argb2ci.o : argb2ci.f     g01wsl.h

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

wks.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
wks.o: /usr/include/sys/_symbol_aliasing.h
wks.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
wks.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
wks.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
wks.o: /usr/include/i386/_types.h /usr/include/sys/_pthread/_pthread_types.h
wks.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
wks.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
wks.o: /usr/include/sys/_types/_int16_t.h /usr/include/sys/_types/_int32_t.h
wks.o: /usr/include/sys/_types/_int64_t.h /usr/include/sys/_types/_u_int8_t.h
wks.o: /usr/include/sys/_types/_u_int16_t.h
wks.o: /usr/include/sys/_types/_u_int32_t.h
wks.o: /usr/include/sys/_types/_u_int64_t.h
wks.o: /usr/include/sys/_types/_intptr_t.h
wks.o: /usr/include/sys/_types/_uintptr_t.h /usr/include/sys/_types/_size_t.h
wks.o: /usr/include/sys/_types/_null.h /usr/include/sys/stdio.h
wks.o: /usr/include/sys/_types/_off_t.h /usr/include/sys/_types/_ssize_t.h
wks.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
wks.o: /usr/include/stdlib.h /usr/include/sys/wait.h
wks.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_id_t.h
wks.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
wks.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
wks.o: /usr/include/machine/_mcontext.h /usr/include/i386/_mcontext.h
wks.o: /usr/include/mach/machine/_structs.h /usr/include/mach/i386/_structs.h
wks.o: /usr/include/sys/_pthread/_pthread_attr_t.h
wks.o: /usr/include/sys/_types/_sigaltstack.h
wks.o: /usr/include/sys/_types/_ucontext.h
wks.o: /usr/include/sys/_types/_sigset_t.h /usr/include/sys/_types/_uid_t.h
wks.o: /usr/include/sys/resource.h /usr/include/stdint.h
wks.o: /usr/include/_types/_uint8_t.h /usr/include/_types/_uint16_t.h
wks.o: /usr/include/_types/_uint32_t.h /usr/include/_types/_uint64_t.h
wks.o: /usr/include/_types/_intmax_t.h /usr/include/_types/_uintmax_t.h
wks.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
wks.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
wks.o: /usr/include/libkern/_OSByteOrder.h
wks.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
wks.o: /usr/include/sys/_types/_ct_rune_t.h /usr/include/sys/_types/_rune_t.h
wks.o: /usr/include/sys/_types/_wchar_t.h /usr/include/sys/_types/_dev_t.h
wks.o: /usr/include/sys/_types/_mode_t.h /usr/include/string.h
wks.o: /usr/include/sys/_types/_rsize_t.h /usr/include/sys/_types/_errno_t.h
wks.o: /usr/include/strings.h /usr/include/secure/_strings.h
wks.o: /usr/include/secure/_string.h ../../../.././include/ncarg/c.h
wks.o: /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
wks.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
wks.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_blkcnt_t.h
wks.o: /usr/include/sys/_types/_blksize_t.h /usr/include/sys/_types/_gid_t.h
wks.o: /usr/include/sys/_types/_in_addr_t.h
wks.o: /usr/include/sys/_types/_in_port_t.h /usr/include/sys/_types/_ino_t.h
wks.o: /usr/include/sys/_types/_ino64_t.h /usr/include/sys/_types/_key_t.h
wks.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_clock_t.h
wks.o: /usr/include/sys/_types/_time_t.h
wks.o: /usr/include/sys/_types/_useconds_t.h
wks.o: /usr/include/sys/_types/_suseconds_t.h
wks.o: /usr/include/sys/_types/_fd_def.h
wks.o: /usr/include/sys/_types/_fd_setsize.h
wks.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
wks.o: /usr/include/sys/_types/_fd_zero.h /usr/include/sys/_types/_fd_isset.h
wks.o: /usr/include/sys/_types/_fd_copy.h
wks.o: /usr/include/sys/_pthread/_pthread_cond_t.h
wks.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
wks.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
wks.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
wks.o: /usr/include/sys/_pthread/_pthread_once_t.h
wks.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
wks.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
wks.o: /usr/include/sys/_pthread/_pthread_t.h
wks.o: /usr/include/sys/_pthread/_pthread_key_t.h
wks.o: /usr/include/sys/_types/_fsblkcnt_t.h
wks.o: /usr/include/sys/_types/_fsfilcnt_t.h /usr/include/fcntl.h
wks.o: /usr/include/sys/fcntl.h /usr/include/sys/_types/_o_sync.h
wks.o: /usr/include/sys/_types/_o_dsync.h /usr/include/sys/_types/_seek_set.h
wks.o: /usr/include/sys/_types/_s_ifmt.h /usr/include/sys/_types/_timespec.h
wks.o: /usr/include/sys/_types/_filesec_t.h /usr/include/unistd.h
wks.o: /usr/include/sys/unistd.h /usr/include/sys/_types/_posix_vdisable.h
wks.o: /usr/include/sys/select.h /usr/include/sys/_select.h
wks.o: /usr/include/sys/_types/_uuid_t.h /usr/include/gethostuuid.h
wks.o: /usr/include/sys/file.h /usr/include/sys/queue.h wks.h
