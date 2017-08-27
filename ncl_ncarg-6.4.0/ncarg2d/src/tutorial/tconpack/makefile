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
CURRENT_DIR		= ./ncarg2d/src/tutorial/tconpack

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
#	$Id: yMakefile,v 1.8 1995-06-14 14:06:54 haley Exp $
#
MYNAME	= tconpack

SRCS =  	ccpback.f ccpcff.f ccpcfx.f ccpcica.f ccpcir.f ccpcis.f ccpcit.f  	ccpclc.f ccpcld.f ccpcldm.f ccpcldr.f ccpcll.f ccpclu.f ccpcnrc.f  	ccpdflt.f ccpezct.f ccpfil.f ccpga.f ccphand.f ccphcf.f ccphl.f  	ccphlt.f ccpila.f ccpils.f ccpilt.f ccpklb.f ccplbam.f ccplbdr.f  	ccpline.f ccpllb.f ccpllc.f ccplll.f ccpllo.f ccpllp.f ccpllt.f  	ccpllw.f ccpmap.f ccpmovi.f ccpmpxy.f ccpncls.f ccpnet.f ccpnof.f  	ccpnsd.f ccppc.f ccppc1.f ccppc2.f ccppc3.f ccppc4.f ccppkcl.f  	ccppole.f ccprc.f ccprect.f ccprwc.f ccprwu.f ccpscam.f ccpset.f  	ccpsps1.f ccpsps2.f ccpspv.f ccpt2d.f ccptitle.f ccpvp.f ccpvs.f  	cidsfft.f colcon.f ggdini.f ccpex.dat ccpila.dat cpmpxy1.dat

OBJS =  	ccpback.o ccpcff.o ccpcfx.o ccpcica.o ccpcir.o ccpcis.o ccpcit.o  	ccpclc.o ccpcld.o ccpcldm.o ccpcldr.o ccpcll.o ccpclu.o ccpcnrc.o  	ccpdflt.o ccpezct.o ccpfil.o ccpga.o ccphand.o ccphcf.o ccphl.o  	ccphlt.o ccpila.o ccpils.o ccpilt.o ccpklb.o ccplbam.o ccplbdr.o  	ccpline.o ccpllb.o ccpllc.o ccplll.o ccpllo.o ccpllp.o ccpllt.o  	ccpllw.o ccpmap.o ccpmovi.o ccpmpxy.o ccpncls.o ccpnet.o ccpnof.o  	ccpnsd.o ccppc.o ccppc1.o ccppc2.o ccppc3.o ccppc4.o ccppkcl.o  	ccppole.o ccprc.o ccprect.o ccprwc.o ccprwu.o ccpscam.o ccpset.o  	ccpsps1.o ccpsps2.o ccpspv.o ccpt2d.o ccptitle.o ccpvp.o ccpvs.o  	cidsfft.o colcon.o ggdini.o

PROG_LIBS		= $(NCARGLIBS)
SYS_LIBS		= -lm
EXTRA_LIBS		= $(XLIB)

all::

all-local:: $(SRCS)

install-local:: $(SRCS)
	@if (test ! -d $(TUTORIALPATH)); then ($(MKDIRHIER) $(TUTORIALPATH)); fi
	@for name in $(SRCS); do \
	if (test -f $$name.$(EXE_SUFFIX)) then \
	(echo "Installing $$name.$(EXE_SUFFIX)";\
	$(INSTALL) -m $(INSTALL_LIB) $$name.$(EXE_SUFFIX) $(TUTORIALPATH));\
	else \
	(echo "Installing $$name";\
	$(INSTALL) -m $(INSTALL_LIB) $$name $(TUTORIALPATH)); fi \
	done

examples:: ccpback

ccpback: ccpback.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpback ccpback.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpback ccpback.o

all::

clean-local::
	-@$(RM) ccpback ccpback.o

examples:: ccpcff

ccpcff: ccpcff.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpcff ccpcff.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpcff ccpcff.o

all::

clean-local::
	-@$(RM) ccpcff ccpcff.o

examples:: ccpcfx

ccpcfx: ccpcfx.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpcfx ccpcfx.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpcfx ccpcfx.o

all::

clean-local::
	-@$(RM) ccpcfx ccpcfx.o

examples:: ccpcica

ccpcica: ccpcica.o ggdini.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpcica ccpcica.o ggdini.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpcica ccpcica.o ggdini.o

all::

clean-local::
	-@$(RM) ccpcica ccpcica.o ggdini.o

examples:: ccpcir

ccpcir: ccpcir.o ggdini.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpcir ccpcir.o ggdini.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpcir ccpcir.o ggdini.o

all::

clean-local::
	-@$(RM) ccpcir ccpcir.o ggdini.o

examples:: ccpcis

ccpcis: ccpcis.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpcis ccpcis.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpcis ccpcis.o

all::

clean-local::
	-@$(RM) ccpcis ccpcis.o

examples:: ccpcit

ccpcit: ccpcit.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpcit ccpcit.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpcit ccpcit.o

all::

clean-local::
	-@$(RM) ccpcit ccpcit.o

examples:: ccpclc

ccpclc: ccpclc.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpclc ccpclc.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpclc ccpclc.o

all::

clean-local::
	-@$(RM) ccpclc ccpclc.o

examples:: ccpcld

ccpcld: ccpcld.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpcld ccpcld.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpcld ccpcld.o

all::

clean-local::
	-@$(RM) ccpcld ccpcld.o

examples:: ccpcldm

ccpcldm: ccpcldm.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpcldm ccpcldm.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpcldm ccpcldm.o

all::

clean-local::
	-@$(RM) ccpcldm ccpcldm.o

examples:: ccpcldr

ccpcldr: ccpcldr.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpcldr ccpcldr.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpcldr ccpcldr.o

all::

clean-local::
	-@$(RM) ccpcldr ccpcldr.o

examples:: ccpcll

ccpcll: ccpcll.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpcll ccpcll.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpcll ccpcll.o

all::

clean-local::
	-@$(RM) ccpcll ccpcll.o

examples:: ccpclu

ccpclu: ccpclu.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpclu ccpclu.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpclu ccpclu.o

all::

clean-local::
	-@$(RM) ccpclu ccpclu.o

examples:: ccpcnrc

ccpcnrc: ccpcnrc.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpcnrc ccpcnrc.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpcnrc ccpcnrc.o

all::

clean-local::
	-@$(RM) ccpcnrc ccpcnrc.o

examples:: ccpdflt

ccpdflt: ccpdflt.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpdflt ccpdflt.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpdflt ccpdflt.o

all::

clean-local::
	-@$(RM) ccpdflt ccpdflt.o

examples:: ccpezct

ccpezct: ccpezct.o ggdini.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpezct ccpezct.o ggdini.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpezct ccpezct.o ggdini.o

all::

clean-local::
	-@$(RM) ccpezct ccpezct.o ggdini.o

examples:: ccpfil

ccpfil: ccpfil.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpfil ccpfil.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpfil ccpfil.o

all::

clean-local::
	-@$(RM) ccpfil ccpfil.o

examples:: ccpga

ccpga: ccpga.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpga ccpga.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpga ccpga.o

all::

clean-local::
	-@$(RM) ccpga ccpga.o

examples:: ccphand

ccphand: ccphand.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccphand ccphand.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccphand ccphand.o

all::

clean-local::
	-@$(RM) ccphand ccphand.o

examples:: ccphcf

ccphcf: ccphcf.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccphcf ccphcf.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccphcf ccphcf.o

all::

clean-local::
	-@$(RM) ccphcf ccphcf.o

examples:: ccphl

ccphl: ccphl.o ggdini.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccphl ccphl.o ggdini.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccphl ccphl.o ggdini.o

all::

clean-local::
	-@$(RM) ccphl ccphl.o ggdini.o

examples:: ccphlt

ccphlt: ccphlt.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccphlt ccphlt.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccphlt ccphlt.o

all::

clean-local::
	-@$(RM) ccphlt ccphlt.o

examples:: ccpila

ccpila: ccpila.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpila ccpila.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpila ccpila.o

all::

clean-local::
	-@$(RM) ccpila ccpila.o

examples:: ccpils

ccpils: ccpils.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpils ccpils.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpils ccpils.o

all::

clean-local::
	-@$(RM) ccpils ccpils.o

examples:: ccpilt

ccpilt: ccpilt.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpilt ccpilt.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpilt ccpilt.o

all::

clean-local::
	-@$(RM) ccpilt ccpilt.o

examples:: ccpklb

ccpklb: ccpklb.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpklb ccpklb.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpklb ccpklb.o

all::

clean-local::
	-@$(RM) ccpklb ccpklb.o

examples:: ccplbam

ccplbam: ccplbam.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccplbam ccplbam.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccplbam ccplbam.o

all::

clean-local::
	-@$(RM) ccplbam ccplbam.o

examples:: ccplbdr

ccplbdr: ccplbdr.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccplbdr ccplbdr.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccplbdr ccplbdr.o

all::

clean-local::
	-@$(RM) ccplbdr ccplbdr.o

examples:: ccpline

ccpline: ccpline.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpline ccpline.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpline ccpline.o

all::

clean-local::
	-@$(RM) ccpline ccpline.o

examples:: ccpllb

ccpllb: ccpllb.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpllb ccpllb.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpllb ccpllb.o

all::

clean-local::
	-@$(RM) ccpllb ccpllb.o

examples:: ccpllc

ccpllc: ccpllc.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpllc ccpllc.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpllc ccpllc.o

all::

clean-local::
	-@$(RM) ccpllc ccpllc.o

examples:: ccplll

ccplll: ccplll.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccplll ccplll.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccplll ccplll.o

all::

clean-local::
	-@$(RM) ccplll ccplll.o

examples:: ccpllo

ccpllo: ccpllo.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpllo ccpllo.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpllo ccpllo.o

all::

clean-local::
	-@$(RM) ccpllo ccpllo.o

examples:: ccpllp

ccpllp: ccpllp.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpllp ccpllp.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpllp ccpllp.o

all::

clean-local::
	-@$(RM) ccpllp ccpllp.o

examples:: ccpllt

ccpllt: ccpllt.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpllt ccpllt.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpllt ccpllt.o

all::

clean-local::
	-@$(RM) ccpllt ccpllt.o

examples:: ccpllw

ccpllw: ccpllw.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpllw ccpllw.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpllw ccpllw.o

all::

clean-local::
	-@$(RM) ccpllw ccpllw.o

examples:: ccpmap

ccpmap: ccpmap.o ggdini.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpmap ccpmap.o ggdini.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpmap ccpmap.o ggdini.o

all::

clean-local::
	-@$(RM) ccpmap ccpmap.o ggdini.o

examples:: ccpmovi

ccpmovi: ccpmovi.o ggdini.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpmovi ccpmovi.o ggdini.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpmovi ccpmovi.o ggdini.o

all::

clean-local::
	-@$(RM) ccpmovi ccpmovi.o ggdini.o

examples:: ccpmpxy

ccpmpxy: ccpmpxy.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpmpxy ccpmpxy.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpmpxy ccpmpxy.o

all::

clean-local::
	-@$(RM) ccpmpxy ccpmpxy.o

examples:: ccpncls

ccpncls: ccpncls.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpncls ccpncls.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpncls ccpncls.o

all::

clean-local::
	-@$(RM) ccpncls ccpncls.o

examples:: ccpnet

ccpnet: ccpnet.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpnet ccpnet.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpnet ccpnet.o

all::

clean-local::
	-@$(RM) ccpnet ccpnet.o

examples:: ccpnof

ccpnof: ccpnof.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpnof ccpnof.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpnof ccpnof.o

all::

clean-local::
	-@$(RM) ccpnof ccpnof.o

examples:: ccpnsd

ccpnsd: ccpnsd.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpnsd ccpnsd.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpnsd ccpnsd.o

all::

clean-local::
	-@$(RM) ccpnsd ccpnsd.o

examples:: ccppc

ccppc: ccppc.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccppc ccppc.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccppc ccppc.o

all::

clean-local::
	-@$(RM) ccppc ccppc.o

examples:: ccppc1

ccppc1: ccppc1.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccppc1 ccppc1.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccppc1 ccppc1.o

all::

clean-local::
	-@$(RM) ccppc1 ccppc1.o

examples:: ccppc2

ccppc2: ccppc2.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccppc2 ccppc2.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccppc2 ccppc2.o

all::

clean-local::
	-@$(RM) ccppc2 ccppc2.o

examples:: ccppc3

ccppc3: ccppc3.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccppc3 ccppc3.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccppc3 ccppc3.o

all::

clean-local::
	-@$(RM) ccppc3 ccppc3.o

examples:: ccppc4

ccppc4: ccppc4.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccppc4 ccppc4.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccppc4 ccppc4.o

all::

clean-local::
	-@$(RM) ccppc4 ccppc4.o

examples:: ccppkcl

ccppkcl: ccppkcl.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccppkcl ccppkcl.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccppkcl ccppkcl.o

all::

clean-local::
	-@$(RM) ccppkcl ccppkcl.o

examples:: ccppole

ccppole: ccppole.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccppole ccppole.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccppole ccppole.o

all::

clean-local::
	-@$(RM) ccppole ccppole.o

examples:: ccprc

ccprc: ccprc.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccprc ccprc.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccprc ccprc.o

all::

clean-local::
	-@$(RM) ccprc ccprc.o

examples:: ccprect

ccprect: ccprect.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccprect ccprect.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccprect ccprect.o

all::

clean-local::
	-@$(RM) ccprect ccprect.o

examples:: ccprwc

ccprwc: ccprwc.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccprwc ccprwc.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccprwc ccprwc.o

all::

clean-local::
	-@$(RM) ccprwc ccprwc.o

examples:: ccprwu

ccprwu: ccprwu.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccprwu ccprwu.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccprwu ccprwu.o

all::

clean-local::
	-@$(RM) ccprwu ccprwu.o

examples:: ccpscam

ccpscam: ccpscam.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpscam ccpscam.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpscam ccpscam.o

all::

clean-local::
	-@$(RM) ccpscam ccpscam.o

examples:: ccpset

ccpset: ccpset.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpset ccpset.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpset ccpset.o

all::

clean-local::
	-@$(RM) ccpset ccpset.o

examples:: ccpsps1

ccpsps1: ccpsps1.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpsps1 ccpsps1.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpsps1 ccpsps1.o

all::

clean-local::
	-@$(RM) ccpsps1 ccpsps1.o

examples:: ccpsps2

ccpsps2: ccpsps2.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpsps2 ccpsps2.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpsps2 ccpsps2.o

all::

clean-local::
	-@$(RM) ccpsps2 ccpsps2.o

examples:: ccpspv

ccpspv: ccpspv.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpspv ccpspv.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpspv ccpspv.o

all::

clean-local::
	-@$(RM) ccpspv ccpspv.o

examples:: ccpt2d

ccpt2d: ccpt2d.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpt2d ccpt2d.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpt2d ccpt2d.o

all::

clean-local::
	-@$(RM) ccpt2d ccpt2d.o

examples:: ccptitle

ccptitle: ccptitle.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccptitle ccptitle.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccptitle ccptitle.o

all::

clean-local::
	-@$(RM) ccptitle ccptitle.o

examples:: ccpvp

ccpvp: ccpvp.o ggdini.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpvp ccpvp.o ggdini.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpvp ccpvp.o ggdini.o

all::

clean-local::
	-@$(RM) ccpvp ccpvp.o ggdini.o

examples:: ccpvs

ccpvs: ccpvs.o
	$(F77_LD) $(FC_LDOPTIONS) -o ccpvs ccpvs.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) ccpvs ccpvs.o

all::

clean-local::
	-@$(RM) ccpvs ccpvs.o

examples:: cidsfft

cidsfft: cidsfft.o
	$(F77_LD) $(FC_LDOPTIONS) -o cidsfft cidsfft.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) cidsfft cidsfft.o

all::

clean-local::
	-@$(RM) cidsfft cidsfft.o

examples:: colcon

colcon: colcon.o
	$(F77_LD) $(FC_LDOPTIONS) -o colcon colcon.o $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) colcon colcon.o

all::

clean-local::
	-@$(RM) colcon colcon.o

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

