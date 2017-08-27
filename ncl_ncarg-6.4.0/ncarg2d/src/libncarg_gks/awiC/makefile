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
CURRENT_DIR		= ./ncarg2d/src/libncarg_gks/awiC

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
#	$Id: yMakefile,v 1.2 2010-03-16 01:59:21 haley Exp $
#

MYNAME	= awiC

SRCS =  	s_gactm.c s_gacwk.c s_gca.c s_gclks.c s_gclrwk.c s_gclwk.c  	s_gdawk.c s_geclks.c s_gerhnd.c s_gerlog.c s_gesc.c s_gevtm.c  	s_gfa.c s_ggdp.c s_ggtitm.c s_giitm.c s_gopks.c s_gopwk.c s_gpl.c s_gpm.c  	s_gqacwk.c s_gqasf.c s_gqcf.c s_gqchb.c s_gqchh.c s_gqchsp.c s_gqchup.c  	s_gqchw.c s_gqchxp.c s_gqclip.c s_gqcntn.c s_gqcr.c s_gqdsp.c s_gqeci.c  	s_gqegdp.c s_gqentn.c s_gqewk.c s_gqfaci.c s_gqfaf.c s_gqfai.c s_gqfais.c  	s_gqfasi.c s_gqgdp.c s_gqina.c s_gqln.c s_gqlvks.c s_gqlwk.c s_gqlwsc.c  	s_gqmk.c s_gqmksc.c s_gqmntn.c s_gqnt.c s_gqops.c s_gqopwk.c  	s_gqpaf.c s_gqpahv.c s_gqparf.c s_gqpawv.c s_gqpcr.c s_gqpfar.c  	s_gqplci.c s_gqplf.c s_gqpli.c s_gqpmci.c s_gqpmf.c s_gqpmi.c s_gqppar.c  	s_gqpplr.c s_gqppmr.c s_gqpra.c s_gqptxr.c s_gqpx.c s_gqpxa.c s_gqpxad.c  	s_gqtxal.c s_gqtxci.c s_gqtxf.c s_gqtxfp.c s_gqtxi.c s_gqtxx.c s_gqtxp.c  	s_gqwkc.c s_gqwkca.c s_gqwkcl.c s_gqwkdu.c s_gqwks.c s_gqwkt.c  	s_grditm.c s_gsasf.c s_gschh.c s_gschsp.c s_gschup.c s_gschxp.c  	s_gsclip.c s_gscr.c s_gselnt.c s_gsfaci.c s_gsfai.c s_gsfais.c  	s_gsfasi.c s_gsln.c s_gslwsc.c s_gsmk.c s_gsmksc.c s_gspa.c s_gsparf.c  	s_gsplci.c s_gspli.c s_gspmci.c s_gspmi.c s_gstxal.c s_gstxci.c  	s_gstxfp.c s_gstxi.c s_gstxp.c s_gsvp.c s_gswkvp.c s_gswkwn.c s_gswn.c  	s_gtx.c s_guwk.c s_gwitm.c s_gcrsg.c s_gclsg.c s_gdsg.c s_gcsgwk.c  	s_gssgt.c s_ginst.c s_gqopsg.c s_gqsgus.c s_grqst.c s_gsstm.c  	getcid.c rmcid.c addcid.c setcid.c gstore.c

OBJS =  	s_gactm.o s_gacwk.o s_gca.o s_gclks.o s_gclrwk.o s_gclwk.o  	s_gdawk.o s_geclks.o s_gerhnd.o s_gerlog.o s_gesc.o s_gevtm.o  	s_gfa.o s_ggdp.o s_ggtitm.o s_giitm.o s_gopks.o s_gopwk.o s_gpl.o s_gpm.o  	s_gqacwk.o s_gqasf.o s_gqcf.o s_gqchb.o s_gqchh.o s_gqchsp.o s_gqchup.o  	s_gqchw.o s_gqchxp.o s_gqclip.o s_gqcntn.o s_gqcr.o s_gqdsp.o s_gqeci.o  	s_gqegdp.o s_gqentn.o s_gqewk.o s_gqfaci.o s_gqfaf.o s_gqfai.o s_gqfais.o  	s_gqfasi.o s_gqgdp.o s_gqina.o s_gqln.o s_gqlvks.o s_gqlwk.o s_gqlwsc.o  	s_gqmk.o s_gqmksc.o s_gqmntn.o s_gqnt.o s_gqops.o s_gqopwk.o  	s_gqpaf.o s_gqpahv.o s_gqparf.o s_gqpawv.o s_gqpcr.o s_gqpfar.o  	s_gqplci.o s_gqplf.o s_gqpli.o s_gqpmci.o s_gqpmf.o s_gqpmi.o s_gqppar.o  	s_gqpplr.o s_gqppmr.o s_gqpra.o s_gqptxr.o s_gqpx.o s_gqpxa.o s_gqpxad.o  	s_gqtxal.o s_gqtxci.o s_gqtxf.o s_gqtxfp.o s_gqtxi.o s_gqtxx.o s_gqtxp.o  	s_gqwkc.o s_gqwkca.o s_gqwkcl.o s_gqwkdu.o s_gqwks.o s_gqwkt.o  	s_grditm.o s_gsasf.o s_gschh.o s_gschsp.o s_gschup.o s_gschxp.o  	s_gsclip.o s_gscr.o s_gselnt.o s_gsfaci.o s_gsfai.o s_gsfais.o  	s_gsfasi.o s_gsln.o s_gslwsc.o s_gsmk.o s_gsmksc.o s_gspa.o s_gsparf.o  	s_gsplci.o s_gspli.o s_gspmci.o s_gspmi.o s_gstxal.o s_gstxci.o  	s_gstxfp.o s_gstxi.o s_gstxp.o s_gsvp.o s_gswkvp.o s_gswkwn.o s_gswn.o  	s_gtx.o s_guwk.o s_gwitm.o s_gcrsg.o s_gclsg.o s_gdsg.o s_gcsgwk.o  	s_gssgt.o s_ginst.o s_gqopsg.o s_gqsgus.o s_grqst.o s_gsstm.o  	getcid.o rmcid.o addcid.o setcid.o gstore.o

EXTRA_CDEFINES = $(OS_DEF) $(MAJOR_DEF)

all::

all-local::../libncarg_gks.a

../libncarg_gks.a:: $(OBJS)
	$(AR_REG) ../libncarg_gks.a $(OBJS)

all::

clean-local::
	-@$(RM) $(OBJS)

all::

all-local::../libbasegks.a

../libbasegks.a:: $(OBJS)
	$(AR_REG) ../libbasegks.a $(OBJS)

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

s_gactm.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gactm.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gactm.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gactm.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gactm.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gactm.o: /usr/include/i386/_types.h
s_gactm.o: /usr/include/sys/_pthread/_pthread_types.h
s_gactm.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gactm.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gactm.o: /usr/include/sys/_types/_int16_t.h
s_gactm.o: /usr/include/sys/_types/_int32_t.h
s_gactm.o: /usr/include/sys/_types/_int64_t.h
s_gactm.o: /usr/include/sys/_types/_u_int8_t.h
s_gactm.o: /usr/include/sys/_types/_u_int16_t.h
s_gactm.o: /usr/include/sys/_types/_u_int32_t.h
s_gactm.o: /usr/include/sys/_types/_u_int64_t.h
s_gactm.o: /usr/include/sys/_types/_intptr_t.h
s_gactm.o: /usr/include/sys/_types/_uintptr_t.h
s_gactm.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gactm.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gactm.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gactm.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gactm.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gactm.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gactm.o: /usr/include/libkern/_OSByteOrder.h
s_gactm.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gactm.o: /usr/include/sys/_types/_u_char.h
s_gactm.o: /usr/include/sys/_types/_u_short.h
s_gactm.o: /usr/include/sys/_types/_u_int.h
s_gactm.o: /usr/include/sys/_types/_caddr_t.h
s_gactm.o: /usr/include/sys/_types/_dev_t.h
s_gactm.o: /usr/include/sys/_types/_blkcnt_t.h
s_gactm.o: /usr/include/sys/_types/_blksize_t.h
s_gactm.o: /usr/include/sys/_types/_gid_t.h
s_gactm.o: /usr/include/sys/_types/_in_addr_t.h
s_gactm.o: /usr/include/sys/_types/_in_port_t.h
s_gactm.o: /usr/include/sys/_types/_ino_t.h
s_gactm.o: /usr/include/sys/_types/_ino64_t.h
s_gactm.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gactm.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gactm.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gactm.o: /usr/include/sys/_types/_clock_t.h
s_gactm.o: /usr/include/sys/_types/_time_t.h
s_gactm.o: /usr/include/sys/_types/_useconds_t.h
s_gactm.o: /usr/include/sys/_types/_suseconds_t.h
s_gactm.o: /usr/include/sys/_types/_rsize_t.h
s_gactm.o: /usr/include/sys/_types/_errno_t.h
s_gactm.o: /usr/include/sys/_types/_fd_def.h
s_gactm.o: /usr/include/sys/_types/_fd_setsize.h
s_gactm.o: /usr/include/sys/_types/_fd_set.h
s_gactm.o: /usr/include/sys/_types/_fd_clr.h
s_gactm.o: /usr/include/sys/_types/_fd_zero.h
s_gactm.o: /usr/include/sys/_types/_fd_isset.h
s_gactm.o: /usr/include/sys/_types/_fd_copy.h
s_gactm.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gactm.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gactm.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gactm.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gactm.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gactm.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gactm.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gactm.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gactm.o: /usr/include/sys/_pthread/_pthread_t.h
s_gactm.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gactm.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gactm.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gactm.o: ../../../.././include/ncarg/c.h
s_gacwk.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gacwk.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gacwk.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gacwk.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gacwk.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gacwk.o: /usr/include/i386/_types.h
s_gacwk.o: /usr/include/sys/_pthread/_pthread_types.h
s_gacwk.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gacwk.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gacwk.o: /usr/include/sys/_types/_int16_t.h
s_gacwk.o: /usr/include/sys/_types/_int32_t.h
s_gacwk.o: /usr/include/sys/_types/_int64_t.h
s_gacwk.o: /usr/include/sys/_types/_u_int8_t.h
s_gacwk.o: /usr/include/sys/_types/_u_int16_t.h
s_gacwk.o: /usr/include/sys/_types/_u_int32_t.h
s_gacwk.o: /usr/include/sys/_types/_u_int64_t.h
s_gacwk.o: /usr/include/sys/_types/_intptr_t.h
s_gacwk.o: /usr/include/sys/_types/_uintptr_t.h
s_gacwk.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gacwk.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gacwk.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gacwk.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gacwk.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gacwk.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gacwk.o: /usr/include/libkern/_OSByteOrder.h
s_gacwk.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gacwk.o: /usr/include/sys/_types/_u_char.h
s_gacwk.o: /usr/include/sys/_types/_u_short.h
s_gacwk.o: /usr/include/sys/_types/_u_int.h
s_gacwk.o: /usr/include/sys/_types/_caddr_t.h
s_gacwk.o: /usr/include/sys/_types/_dev_t.h
s_gacwk.o: /usr/include/sys/_types/_blkcnt_t.h
s_gacwk.o: /usr/include/sys/_types/_blksize_t.h
s_gacwk.o: /usr/include/sys/_types/_gid_t.h
s_gacwk.o: /usr/include/sys/_types/_in_addr_t.h
s_gacwk.o: /usr/include/sys/_types/_in_port_t.h
s_gacwk.o: /usr/include/sys/_types/_ino_t.h
s_gacwk.o: /usr/include/sys/_types/_ino64_t.h
s_gacwk.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gacwk.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gacwk.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gacwk.o: /usr/include/sys/_types/_clock_t.h
s_gacwk.o: /usr/include/sys/_types/_time_t.h
s_gacwk.o: /usr/include/sys/_types/_useconds_t.h
s_gacwk.o: /usr/include/sys/_types/_suseconds_t.h
s_gacwk.o: /usr/include/sys/_types/_rsize_t.h
s_gacwk.o: /usr/include/sys/_types/_errno_t.h
s_gacwk.o: /usr/include/sys/_types/_fd_def.h
s_gacwk.o: /usr/include/sys/_types/_fd_setsize.h
s_gacwk.o: /usr/include/sys/_types/_fd_set.h
s_gacwk.o: /usr/include/sys/_types/_fd_clr.h
s_gacwk.o: /usr/include/sys/_types/_fd_zero.h
s_gacwk.o: /usr/include/sys/_types/_fd_isset.h
s_gacwk.o: /usr/include/sys/_types/_fd_copy.h
s_gacwk.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gacwk.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gacwk.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gacwk.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gacwk.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gacwk.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gacwk.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gacwk.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gacwk.o: /usr/include/sys/_pthread/_pthread_t.h
s_gacwk.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gacwk.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gacwk.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gacwk.o: ../../../.././include/ncarg/c.h
s_gca.o: /usr/include/stdlib.h /usr/include/Availability.h
s_gca.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gca.o: /usr/include/sys/_types.h /usr/include/sys/cdefs.h
s_gca.o: /usr/include/sys/_symbol_aliasing.h
s_gca.o: /usr/include/sys/_posix_availability.h /usr/include/machine/_types.h
s_gca.o: /usr/include/i386/_types.h
s_gca.o: /usr/include/sys/_pthread/_pthread_types.h /usr/include/sys/wait.h
s_gca.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_id_t.h
s_gca.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
s_gca.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
s_gca.o: /usr/include/machine/_mcontext.h /usr/include/i386/_mcontext.h
s_gca.o: /usr/include/mach/machine/_structs.h
s_gca.o: /usr/include/mach/i386/_structs.h /usr/include/machine/types.h
s_gca.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gca.o: /usr/include/sys/_types/_int16_t.h
s_gca.o: /usr/include/sys/_types/_int32_t.h
s_gca.o: /usr/include/sys/_types/_int64_t.h
s_gca.o: /usr/include/sys/_types/_u_int8_t.h
s_gca.o: /usr/include/sys/_types/_u_int16_t.h
s_gca.o: /usr/include/sys/_types/_u_int32_t.h
s_gca.o: /usr/include/sys/_types/_u_int64_t.h
s_gca.o: /usr/include/sys/_types/_intptr_t.h
s_gca.o: /usr/include/sys/_types/_uintptr_t.h
s_gca.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gca.o: /usr/include/sys/_types/_sigaltstack.h
s_gca.o: /usr/include/sys/_types/_ucontext.h
s_gca.o: /usr/include/sys/_types/_sigset_t.h
s_gca.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_uid_t.h
s_gca.o: /usr/include/sys/resource.h /usr/include/stdint.h
s_gca.o: /usr/include/_types/_uint8_t.h /usr/include/_types/_uint16_t.h
s_gca.o: /usr/include/_types/_uint32_t.h /usr/include/_types/_uint64_t.h
s_gca.o: /usr/include/_types/_intmax_t.h /usr/include/_types/_uintmax_t.h
s_gca.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
s_gca.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gca.o: /usr/include/libkern/_OSByteOrder.h
s_gca.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
s_gca.o: /usr/include/sys/_types/_ct_rune_t.h
s_gca.o: /usr/include/sys/_types/_rune_t.h /usr/include/sys/_types/_wchar_t.h
s_gca.o: /usr/include/sys/_types/_null.h /usr/include/sys/_types/_dev_t.h
s_gca.o: /usr/include/sys/_types/_mode_t.h ../../../.././include/ncarg/gks.h
s_gca.o: /usr/include/stdio.h /usr/include/sys/_types/_va_list.h
s_gca.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gca.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gca.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gca.o: /usr/include/sys/_types/_u_char.h /usr/include/sys/_types/_u_short.h
s_gca.o: /usr/include/sys/_types/_u_int.h /usr/include/sys/_types/_caddr_t.h
s_gca.o: /usr/include/sys/_types/_blkcnt_t.h
s_gca.o: /usr/include/sys/_types/_blksize_t.h
s_gca.o: /usr/include/sys/_types/_gid_t.h
s_gca.o: /usr/include/sys/_types/_in_addr_t.h
s_gca.o: /usr/include/sys/_types/_in_port_t.h
s_gca.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
s_gca.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_nlink_t.h
s_gca.o: /usr/include/sys/_types/_clock_t.h /usr/include/sys/_types/_time_t.h
s_gca.o: /usr/include/sys/_types/_useconds_t.h
s_gca.o: /usr/include/sys/_types/_suseconds_t.h
s_gca.o: /usr/include/sys/_types/_rsize_t.h
s_gca.o: /usr/include/sys/_types/_errno_t.h /usr/include/sys/_types/_fd_def.h
s_gca.o: /usr/include/sys/_types/_fd_setsize.h
s_gca.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
s_gca.o: /usr/include/sys/_types/_fd_zero.h
s_gca.o: /usr/include/sys/_types/_fd_isset.h
s_gca.o: /usr/include/sys/_types/_fd_copy.h
s_gca.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gca.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gca.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gca.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gca.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gca.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gca.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gca.o: /usr/include/sys/_pthread/_pthread_t.h
s_gca.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gca.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gca.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gca.o: ../../../.././include/ncarg/c.h
s_gclks.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gclks.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gclks.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gclks.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gclks.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gclks.o: /usr/include/i386/_types.h
s_gclks.o: /usr/include/sys/_pthread/_pthread_types.h
s_gclks.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gclks.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gclks.o: /usr/include/sys/_types/_int16_t.h
s_gclks.o: /usr/include/sys/_types/_int32_t.h
s_gclks.o: /usr/include/sys/_types/_int64_t.h
s_gclks.o: /usr/include/sys/_types/_u_int8_t.h
s_gclks.o: /usr/include/sys/_types/_u_int16_t.h
s_gclks.o: /usr/include/sys/_types/_u_int32_t.h
s_gclks.o: /usr/include/sys/_types/_u_int64_t.h
s_gclks.o: /usr/include/sys/_types/_intptr_t.h
s_gclks.o: /usr/include/sys/_types/_uintptr_t.h
s_gclks.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gclks.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gclks.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gclks.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gclks.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gclks.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gclks.o: /usr/include/libkern/_OSByteOrder.h
s_gclks.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gclks.o: /usr/include/sys/_types/_u_char.h
s_gclks.o: /usr/include/sys/_types/_u_short.h
s_gclks.o: /usr/include/sys/_types/_u_int.h
s_gclks.o: /usr/include/sys/_types/_caddr_t.h
s_gclks.o: /usr/include/sys/_types/_dev_t.h
s_gclks.o: /usr/include/sys/_types/_blkcnt_t.h
s_gclks.o: /usr/include/sys/_types/_blksize_t.h
s_gclks.o: /usr/include/sys/_types/_gid_t.h
s_gclks.o: /usr/include/sys/_types/_in_addr_t.h
s_gclks.o: /usr/include/sys/_types/_in_port_t.h
s_gclks.o: /usr/include/sys/_types/_ino_t.h
s_gclks.o: /usr/include/sys/_types/_ino64_t.h
s_gclks.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gclks.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gclks.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gclks.o: /usr/include/sys/_types/_clock_t.h
s_gclks.o: /usr/include/sys/_types/_time_t.h
s_gclks.o: /usr/include/sys/_types/_useconds_t.h
s_gclks.o: /usr/include/sys/_types/_suseconds_t.h
s_gclks.o: /usr/include/sys/_types/_rsize_t.h
s_gclks.o: /usr/include/sys/_types/_errno_t.h
s_gclks.o: /usr/include/sys/_types/_fd_def.h
s_gclks.o: /usr/include/sys/_types/_fd_setsize.h
s_gclks.o: /usr/include/sys/_types/_fd_set.h
s_gclks.o: /usr/include/sys/_types/_fd_clr.h
s_gclks.o: /usr/include/sys/_types/_fd_zero.h
s_gclks.o: /usr/include/sys/_types/_fd_isset.h
s_gclks.o: /usr/include/sys/_types/_fd_copy.h
s_gclks.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gclks.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gclks.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gclks.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gclks.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gclks.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gclks.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gclks.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gclks.o: /usr/include/sys/_pthread/_pthread_t.h
s_gclks.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gclks.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gclks.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gclks.o: ../../../.././include/ncarg/c.h
s_gclrwk.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gclrwk.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gclrwk.o: /usr/include/sys/_posix_availability.h
s_gclrwk.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gclrwk.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gclrwk.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gclrwk.o: /usr/include/sys/_pthread/_pthread_types.h
s_gclrwk.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gclrwk.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gclrwk.o: /usr/include/sys/_types/_int16_t.h
s_gclrwk.o: /usr/include/sys/_types/_int32_t.h
s_gclrwk.o: /usr/include/sys/_types/_int64_t.h
s_gclrwk.o: /usr/include/sys/_types/_u_int8_t.h
s_gclrwk.o: /usr/include/sys/_types/_u_int16_t.h
s_gclrwk.o: /usr/include/sys/_types/_u_int32_t.h
s_gclrwk.o: /usr/include/sys/_types/_u_int64_t.h
s_gclrwk.o: /usr/include/sys/_types/_intptr_t.h
s_gclrwk.o: /usr/include/sys/_types/_uintptr_t.h
s_gclrwk.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gclrwk.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gclrwk.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gclrwk.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gclrwk.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gclrwk.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gclrwk.o: /usr/include/libkern/_OSByteOrder.h
s_gclrwk.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gclrwk.o: /usr/include/sys/_types/_u_char.h
s_gclrwk.o: /usr/include/sys/_types/_u_short.h
s_gclrwk.o: /usr/include/sys/_types/_u_int.h
s_gclrwk.o: /usr/include/sys/_types/_caddr_t.h
s_gclrwk.o: /usr/include/sys/_types/_dev_t.h
s_gclrwk.o: /usr/include/sys/_types/_blkcnt_t.h
s_gclrwk.o: /usr/include/sys/_types/_blksize_t.h
s_gclrwk.o: /usr/include/sys/_types/_gid_t.h
s_gclrwk.o: /usr/include/sys/_types/_in_addr_t.h
s_gclrwk.o: /usr/include/sys/_types/_in_port_t.h
s_gclrwk.o: /usr/include/sys/_types/_ino_t.h
s_gclrwk.o: /usr/include/sys/_types/_ino64_t.h
s_gclrwk.o: /usr/include/sys/_types/_key_t.h
s_gclrwk.o: /usr/include/sys/_types/_mode_t.h
s_gclrwk.o: /usr/include/sys/_types/_nlink_t.h
s_gclrwk.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gclrwk.o: /usr/include/sys/_types/_uid_t.h
s_gclrwk.o: /usr/include/sys/_types/_clock_t.h
s_gclrwk.o: /usr/include/sys/_types/_time_t.h
s_gclrwk.o: /usr/include/sys/_types/_useconds_t.h
s_gclrwk.o: /usr/include/sys/_types/_suseconds_t.h
s_gclrwk.o: /usr/include/sys/_types/_rsize_t.h
s_gclrwk.o: /usr/include/sys/_types/_errno_t.h
s_gclrwk.o: /usr/include/sys/_types/_fd_def.h
s_gclrwk.o: /usr/include/sys/_types/_fd_setsize.h
s_gclrwk.o: /usr/include/sys/_types/_fd_set.h
s_gclrwk.o: /usr/include/sys/_types/_fd_clr.h
s_gclrwk.o: /usr/include/sys/_types/_fd_zero.h
s_gclrwk.o: /usr/include/sys/_types/_fd_isset.h
s_gclrwk.o: /usr/include/sys/_types/_fd_copy.h
s_gclrwk.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gclrwk.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gclrwk.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gclrwk.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gclrwk.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gclrwk.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gclrwk.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gclrwk.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gclrwk.o: /usr/include/sys/_pthread/_pthread_t.h
s_gclrwk.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gclrwk.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gclrwk.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gclrwk.o: ../../../.././include/ncarg/c.h
s_gclwk.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gclwk.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gclwk.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gclwk.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gclwk.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gclwk.o: /usr/include/i386/_types.h
s_gclwk.o: /usr/include/sys/_pthread/_pthread_types.h
s_gclwk.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gclwk.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gclwk.o: /usr/include/sys/_types/_int16_t.h
s_gclwk.o: /usr/include/sys/_types/_int32_t.h
s_gclwk.o: /usr/include/sys/_types/_int64_t.h
s_gclwk.o: /usr/include/sys/_types/_u_int8_t.h
s_gclwk.o: /usr/include/sys/_types/_u_int16_t.h
s_gclwk.o: /usr/include/sys/_types/_u_int32_t.h
s_gclwk.o: /usr/include/sys/_types/_u_int64_t.h
s_gclwk.o: /usr/include/sys/_types/_intptr_t.h
s_gclwk.o: /usr/include/sys/_types/_uintptr_t.h
s_gclwk.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gclwk.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gclwk.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gclwk.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gclwk.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gclwk.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gclwk.o: /usr/include/libkern/_OSByteOrder.h
s_gclwk.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gclwk.o: /usr/include/sys/_types/_u_char.h
s_gclwk.o: /usr/include/sys/_types/_u_short.h
s_gclwk.o: /usr/include/sys/_types/_u_int.h
s_gclwk.o: /usr/include/sys/_types/_caddr_t.h
s_gclwk.o: /usr/include/sys/_types/_dev_t.h
s_gclwk.o: /usr/include/sys/_types/_blkcnt_t.h
s_gclwk.o: /usr/include/sys/_types/_blksize_t.h
s_gclwk.o: /usr/include/sys/_types/_gid_t.h
s_gclwk.o: /usr/include/sys/_types/_in_addr_t.h
s_gclwk.o: /usr/include/sys/_types/_in_port_t.h
s_gclwk.o: /usr/include/sys/_types/_ino_t.h
s_gclwk.o: /usr/include/sys/_types/_ino64_t.h
s_gclwk.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gclwk.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gclwk.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gclwk.o: /usr/include/sys/_types/_clock_t.h
s_gclwk.o: /usr/include/sys/_types/_time_t.h
s_gclwk.o: /usr/include/sys/_types/_useconds_t.h
s_gclwk.o: /usr/include/sys/_types/_suseconds_t.h
s_gclwk.o: /usr/include/sys/_types/_rsize_t.h
s_gclwk.o: /usr/include/sys/_types/_errno_t.h
s_gclwk.o: /usr/include/sys/_types/_fd_def.h
s_gclwk.o: /usr/include/sys/_types/_fd_setsize.h
s_gclwk.o: /usr/include/sys/_types/_fd_set.h
s_gclwk.o: /usr/include/sys/_types/_fd_clr.h
s_gclwk.o: /usr/include/sys/_types/_fd_zero.h
s_gclwk.o: /usr/include/sys/_types/_fd_isset.h
s_gclwk.o: /usr/include/sys/_types/_fd_copy.h
s_gclwk.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gclwk.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gclwk.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gclwk.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gclwk.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gclwk.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gclwk.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gclwk.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gclwk.o: /usr/include/sys/_pthread/_pthread_t.h
s_gclwk.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gclwk.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gclwk.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gclwk.o: ../../../.././include/ncarg/c.h
s_gdawk.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gdawk.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gdawk.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gdawk.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gdawk.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gdawk.o: /usr/include/i386/_types.h
s_gdawk.o: /usr/include/sys/_pthread/_pthread_types.h
s_gdawk.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gdawk.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gdawk.o: /usr/include/sys/_types/_int16_t.h
s_gdawk.o: /usr/include/sys/_types/_int32_t.h
s_gdawk.o: /usr/include/sys/_types/_int64_t.h
s_gdawk.o: /usr/include/sys/_types/_u_int8_t.h
s_gdawk.o: /usr/include/sys/_types/_u_int16_t.h
s_gdawk.o: /usr/include/sys/_types/_u_int32_t.h
s_gdawk.o: /usr/include/sys/_types/_u_int64_t.h
s_gdawk.o: /usr/include/sys/_types/_intptr_t.h
s_gdawk.o: /usr/include/sys/_types/_uintptr_t.h
s_gdawk.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gdawk.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gdawk.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gdawk.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gdawk.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gdawk.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gdawk.o: /usr/include/libkern/_OSByteOrder.h
s_gdawk.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gdawk.o: /usr/include/sys/_types/_u_char.h
s_gdawk.o: /usr/include/sys/_types/_u_short.h
s_gdawk.o: /usr/include/sys/_types/_u_int.h
s_gdawk.o: /usr/include/sys/_types/_caddr_t.h
s_gdawk.o: /usr/include/sys/_types/_dev_t.h
s_gdawk.o: /usr/include/sys/_types/_blkcnt_t.h
s_gdawk.o: /usr/include/sys/_types/_blksize_t.h
s_gdawk.o: /usr/include/sys/_types/_gid_t.h
s_gdawk.o: /usr/include/sys/_types/_in_addr_t.h
s_gdawk.o: /usr/include/sys/_types/_in_port_t.h
s_gdawk.o: /usr/include/sys/_types/_ino_t.h
s_gdawk.o: /usr/include/sys/_types/_ino64_t.h
s_gdawk.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gdawk.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gdawk.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gdawk.o: /usr/include/sys/_types/_clock_t.h
s_gdawk.o: /usr/include/sys/_types/_time_t.h
s_gdawk.o: /usr/include/sys/_types/_useconds_t.h
s_gdawk.o: /usr/include/sys/_types/_suseconds_t.h
s_gdawk.o: /usr/include/sys/_types/_rsize_t.h
s_gdawk.o: /usr/include/sys/_types/_errno_t.h
s_gdawk.o: /usr/include/sys/_types/_fd_def.h
s_gdawk.o: /usr/include/sys/_types/_fd_setsize.h
s_gdawk.o: /usr/include/sys/_types/_fd_set.h
s_gdawk.o: /usr/include/sys/_types/_fd_clr.h
s_gdawk.o: /usr/include/sys/_types/_fd_zero.h
s_gdawk.o: /usr/include/sys/_types/_fd_isset.h
s_gdawk.o: /usr/include/sys/_types/_fd_copy.h
s_gdawk.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gdawk.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gdawk.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gdawk.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gdawk.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gdawk.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gdawk.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gdawk.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gdawk.o: /usr/include/sys/_pthread/_pthread_t.h
s_gdawk.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gdawk.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gdawk.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gdawk.o: ../../../.././include/ncarg/c.h
s_geclks.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_geclks.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_geclks.o: /usr/include/sys/_posix_availability.h
s_geclks.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_geclks.o: /usr/include/_types.h /usr/include/sys/_types.h
s_geclks.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_geclks.o: /usr/include/sys/_pthread/_pthread_types.h
s_geclks.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_geclks.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_geclks.o: /usr/include/sys/_types/_int16_t.h
s_geclks.o: /usr/include/sys/_types/_int32_t.h
s_geclks.o: /usr/include/sys/_types/_int64_t.h
s_geclks.o: /usr/include/sys/_types/_u_int8_t.h
s_geclks.o: /usr/include/sys/_types/_u_int16_t.h
s_geclks.o: /usr/include/sys/_types/_u_int32_t.h
s_geclks.o: /usr/include/sys/_types/_u_int64_t.h
s_geclks.o: /usr/include/sys/_types/_intptr_t.h
s_geclks.o: /usr/include/sys/_types/_uintptr_t.h
s_geclks.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_geclks.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_geclks.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_geclks.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_geclks.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_geclks.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_geclks.o: /usr/include/libkern/_OSByteOrder.h
s_geclks.o: /usr/include/libkern/i386/_OSByteOrder.h
s_geclks.o: /usr/include/sys/_types/_u_char.h
s_geclks.o: /usr/include/sys/_types/_u_short.h
s_geclks.o: /usr/include/sys/_types/_u_int.h
s_geclks.o: /usr/include/sys/_types/_caddr_t.h
s_geclks.o: /usr/include/sys/_types/_dev_t.h
s_geclks.o: /usr/include/sys/_types/_blkcnt_t.h
s_geclks.o: /usr/include/sys/_types/_blksize_t.h
s_geclks.o: /usr/include/sys/_types/_gid_t.h
s_geclks.o: /usr/include/sys/_types/_in_addr_t.h
s_geclks.o: /usr/include/sys/_types/_in_port_t.h
s_geclks.o: /usr/include/sys/_types/_ino_t.h
s_geclks.o: /usr/include/sys/_types/_ino64_t.h
s_geclks.o: /usr/include/sys/_types/_key_t.h
s_geclks.o: /usr/include/sys/_types/_mode_t.h
s_geclks.o: /usr/include/sys/_types/_nlink_t.h
s_geclks.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_geclks.o: /usr/include/sys/_types/_uid_t.h
s_geclks.o: /usr/include/sys/_types/_clock_t.h
s_geclks.o: /usr/include/sys/_types/_time_t.h
s_geclks.o: /usr/include/sys/_types/_useconds_t.h
s_geclks.o: /usr/include/sys/_types/_suseconds_t.h
s_geclks.o: /usr/include/sys/_types/_rsize_t.h
s_geclks.o: /usr/include/sys/_types/_errno_t.h
s_geclks.o: /usr/include/sys/_types/_fd_def.h
s_geclks.o: /usr/include/sys/_types/_fd_setsize.h
s_geclks.o: /usr/include/sys/_types/_fd_set.h
s_geclks.o: /usr/include/sys/_types/_fd_clr.h
s_geclks.o: /usr/include/sys/_types/_fd_zero.h
s_geclks.o: /usr/include/sys/_types/_fd_isset.h
s_geclks.o: /usr/include/sys/_types/_fd_copy.h
s_geclks.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_geclks.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_geclks.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_geclks.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_geclks.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_geclks.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_geclks.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_geclks.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_geclks.o: /usr/include/sys/_pthread/_pthread_t.h
s_geclks.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_geclks.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_geclks.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_geclks.o: ../../../.././include/ncarg/c.h
s_gerhnd.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gerhnd.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gerhnd.o: /usr/include/sys/_posix_availability.h
s_gerhnd.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gerhnd.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gerhnd.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gerhnd.o: /usr/include/sys/_pthread/_pthread_types.h
s_gerhnd.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gerhnd.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gerhnd.o: /usr/include/sys/_types/_int16_t.h
s_gerhnd.o: /usr/include/sys/_types/_int32_t.h
s_gerhnd.o: /usr/include/sys/_types/_int64_t.h
s_gerhnd.o: /usr/include/sys/_types/_u_int8_t.h
s_gerhnd.o: /usr/include/sys/_types/_u_int16_t.h
s_gerhnd.o: /usr/include/sys/_types/_u_int32_t.h
s_gerhnd.o: /usr/include/sys/_types/_u_int64_t.h
s_gerhnd.o: /usr/include/sys/_types/_intptr_t.h
s_gerhnd.o: /usr/include/sys/_types/_uintptr_t.h
s_gerhnd.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gerhnd.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gerhnd.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gerhnd.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gerhnd.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gerhnd.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gerhnd.o: /usr/include/libkern/_OSByteOrder.h
s_gerhnd.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gerhnd.o: /usr/include/sys/_types/_u_char.h
s_gerhnd.o: /usr/include/sys/_types/_u_short.h
s_gerhnd.o: /usr/include/sys/_types/_u_int.h
s_gerhnd.o: /usr/include/sys/_types/_caddr_t.h
s_gerhnd.o: /usr/include/sys/_types/_dev_t.h
s_gerhnd.o: /usr/include/sys/_types/_blkcnt_t.h
s_gerhnd.o: /usr/include/sys/_types/_blksize_t.h
s_gerhnd.o: /usr/include/sys/_types/_gid_t.h
s_gerhnd.o: /usr/include/sys/_types/_in_addr_t.h
s_gerhnd.o: /usr/include/sys/_types/_in_port_t.h
s_gerhnd.o: /usr/include/sys/_types/_ino_t.h
s_gerhnd.o: /usr/include/sys/_types/_ino64_t.h
s_gerhnd.o: /usr/include/sys/_types/_key_t.h
s_gerhnd.o: /usr/include/sys/_types/_mode_t.h
s_gerhnd.o: /usr/include/sys/_types/_nlink_t.h
s_gerhnd.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gerhnd.o: /usr/include/sys/_types/_uid_t.h
s_gerhnd.o: /usr/include/sys/_types/_clock_t.h
s_gerhnd.o: /usr/include/sys/_types/_time_t.h
s_gerhnd.o: /usr/include/sys/_types/_useconds_t.h
s_gerhnd.o: /usr/include/sys/_types/_suseconds_t.h
s_gerhnd.o: /usr/include/sys/_types/_rsize_t.h
s_gerhnd.o: /usr/include/sys/_types/_errno_t.h
s_gerhnd.o: /usr/include/sys/_types/_fd_def.h
s_gerhnd.o: /usr/include/sys/_types/_fd_setsize.h
s_gerhnd.o: /usr/include/sys/_types/_fd_set.h
s_gerhnd.o: /usr/include/sys/_types/_fd_clr.h
s_gerhnd.o: /usr/include/sys/_types/_fd_zero.h
s_gerhnd.o: /usr/include/sys/_types/_fd_isset.h
s_gerhnd.o: /usr/include/sys/_types/_fd_copy.h
s_gerhnd.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gerhnd.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gerhnd.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gerhnd.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gerhnd.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gerhnd.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gerhnd.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gerhnd.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gerhnd.o: /usr/include/sys/_pthread/_pthread_t.h
s_gerhnd.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gerhnd.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gerhnd.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gerhnd.o: ../../../.././include/ncarg/c.h
s_gerlog.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gerlog.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gerlog.o: /usr/include/sys/_posix_availability.h
s_gerlog.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gerlog.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gerlog.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gerlog.o: /usr/include/sys/_pthread/_pthread_types.h
s_gerlog.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gerlog.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gerlog.o: /usr/include/sys/_types/_int16_t.h
s_gerlog.o: /usr/include/sys/_types/_int32_t.h
s_gerlog.o: /usr/include/sys/_types/_int64_t.h
s_gerlog.o: /usr/include/sys/_types/_u_int8_t.h
s_gerlog.o: /usr/include/sys/_types/_u_int16_t.h
s_gerlog.o: /usr/include/sys/_types/_u_int32_t.h
s_gerlog.o: /usr/include/sys/_types/_u_int64_t.h
s_gerlog.o: /usr/include/sys/_types/_intptr_t.h
s_gerlog.o: /usr/include/sys/_types/_uintptr_t.h
s_gerlog.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gerlog.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gerlog.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gerlog.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gerlog.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gerlog.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gerlog.o: /usr/include/libkern/_OSByteOrder.h
s_gerlog.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gerlog.o: /usr/include/sys/_types/_u_char.h
s_gerlog.o: /usr/include/sys/_types/_u_short.h
s_gerlog.o: /usr/include/sys/_types/_u_int.h
s_gerlog.o: /usr/include/sys/_types/_caddr_t.h
s_gerlog.o: /usr/include/sys/_types/_dev_t.h
s_gerlog.o: /usr/include/sys/_types/_blkcnt_t.h
s_gerlog.o: /usr/include/sys/_types/_blksize_t.h
s_gerlog.o: /usr/include/sys/_types/_gid_t.h
s_gerlog.o: /usr/include/sys/_types/_in_addr_t.h
s_gerlog.o: /usr/include/sys/_types/_in_port_t.h
s_gerlog.o: /usr/include/sys/_types/_ino_t.h
s_gerlog.o: /usr/include/sys/_types/_ino64_t.h
s_gerlog.o: /usr/include/sys/_types/_key_t.h
s_gerlog.o: /usr/include/sys/_types/_mode_t.h
s_gerlog.o: /usr/include/sys/_types/_nlink_t.h
s_gerlog.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gerlog.o: /usr/include/sys/_types/_uid_t.h
s_gerlog.o: /usr/include/sys/_types/_clock_t.h
s_gerlog.o: /usr/include/sys/_types/_time_t.h
s_gerlog.o: /usr/include/sys/_types/_useconds_t.h
s_gerlog.o: /usr/include/sys/_types/_suseconds_t.h
s_gerlog.o: /usr/include/sys/_types/_rsize_t.h
s_gerlog.o: /usr/include/sys/_types/_errno_t.h
s_gerlog.o: /usr/include/sys/_types/_fd_def.h
s_gerlog.o: /usr/include/sys/_types/_fd_setsize.h
s_gerlog.o: /usr/include/sys/_types/_fd_set.h
s_gerlog.o: /usr/include/sys/_types/_fd_clr.h
s_gerlog.o: /usr/include/sys/_types/_fd_zero.h
s_gerlog.o: /usr/include/sys/_types/_fd_isset.h
s_gerlog.o: /usr/include/sys/_types/_fd_copy.h
s_gerlog.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gerlog.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gerlog.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gerlog.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gerlog.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gerlog.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gerlog.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gerlog.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gerlog.o: /usr/include/sys/_pthread/_pthread_t.h
s_gerlog.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gerlog.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gerlog.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gerlog.o: ../../../.././include/ncarg/c.h
s_gesc.o: /usr/include/stdlib.h /usr/include/Availability.h
s_gesc.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gesc.o: /usr/include/sys/_types.h /usr/include/sys/cdefs.h
s_gesc.o: /usr/include/sys/_symbol_aliasing.h
s_gesc.o: /usr/include/sys/_posix_availability.h
s_gesc.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gesc.o: /usr/include/sys/_pthread/_pthread_types.h /usr/include/sys/wait.h
s_gesc.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_id_t.h
s_gesc.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
s_gesc.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
s_gesc.o: /usr/include/machine/_mcontext.h /usr/include/i386/_mcontext.h
s_gesc.o: /usr/include/mach/machine/_structs.h
s_gesc.o: /usr/include/mach/i386/_structs.h /usr/include/machine/types.h
s_gesc.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gesc.o: /usr/include/sys/_types/_int16_t.h
s_gesc.o: /usr/include/sys/_types/_int32_t.h
s_gesc.o: /usr/include/sys/_types/_int64_t.h
s_gesc.o: /usr/include/sys/_types/_u_int8_t.h
s_gesc.o: /usr/include/sys/_types/_u_int16_t.h
s_gesc.o: /usr/include/sys/_types/_u_int32_t.h
s_gesc.o: /usr/include/sys/_types/_u_int64_t.h
s_gesc.o: /usr/include/sys/_types/_intptr_t.h
s_gesc.o: /usr/include/sys/_types/_uintptr_t.h
s_gesc.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gesc.o: /usr/include/sys/_types/_sigaltstack.h
s_gesc.o: /usr/include/sys/_types/_ucontext.h
s_gesc.o: /usr/include/sys/_types/_sigset_t.h
s_gesc.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_uid_t.h
s_gesc.o: /usr/include/sys/resource.h /usr/include/stdint.h
s_gesc.o: /usr/include/_types/_uint8_t.h /usr/include/_types/_uint16_t.h
s_gesc.o: /usr/include/_types/_uint32_t.h /usr/include/_types/_uint64_t.h
s_gesc.o: /usr/include/_types/_intmax_t.h /usr/include/_types/_uintmax_t.h
s_gesc.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
s_gesc.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gesc.o: /usr/include/libkern/_OSByteOrder.h
s_gesc.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
s_gesc.o: /usr/include/sys/_types/_ct_rune_t.h
s_gesc.o: /usr/include/sys/_types/_rune_t.h
s_gesc.o: /usr/include/sys/_types/_wchar_t.h /usr/include/sys/_types/_null.h
s_gesc.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_mode_t.h
s_gesc.o: /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
s_gesc.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
s_gesc.o: /usr/include/sys/_types/_caddr_t.h
s_gesc.o: /usr/include/sys/_types/_blkcnt_t.h
s_gesc.o: /usr/include/sys/_types/_blksize_t.h
s_gesc.o: /usr/include/sys/_types/_gid_t.h
s_gesc.o: /usr/include/sys/_types/_in_addr_t.h
s_gesc.o: /usr/include/sys/_types/_in_port_t.h
s_gesc.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
s_gesc.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_nlink_t.h
s_gesc.o: /usr/include/sys/_types/_off_t.h /usr/include/sys/_types/_clock_t.h
s_gesc.o: /usr/include/sys/_types/_ssize_t.h
s_gesc.o: /usr/include/sys/_types/_time_t.h
s_gesc.o: /usr/include/sys/_types/_useconds_t.h
s_gesc.o: /usr/include/sys/_types/_suseconds_t.h
s_gesc.o: /usr/include/sys/_types/_rsize_t.h
s_gesc.o: /usr/include/sys/_types/_errno_t.h
s_gesc.o: /usr/include/sys/_types/_fd_def.h
s_gesc.o: /usr/include/sys/_types/_fd_setsize.h
s_gesc.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
s_gesc.o: /usr/include/sys/_types/_fd_zero.h
s_gesc.o: /usr/include/sys/_types/_fd_isset.h
s_gesc.o: /usr/include/sys/_types/_fd_copy.h
s_gesc.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gesc.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gesc.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gesc.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gesc.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gesc.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gesc.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gesc.o: /usr/include/sys/_pthread/_pthread_t.h
s_gesc.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gesc.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gesc.o: /usr/include/sys/_types/_fsfilcnt_t.h /usr/include/string.h
s_gesc.o: /usr/include/strings.h /usr/include/secure/_strings.h
s_gesc.o: /usr/include/secure/_common.h /usr/include/secure/_string.h
s_gesc.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gesc.o: /usr/include/sys/_types/_va_list.h /usr/include/sys/stdio.h
s_gesc.o: /usr/include/secure/_stdio.h ../../../.././include/ncarg/c.h
s_gesc.o: ../../../.././include/ncarg/gksP.h gstore.h
s_gevtm.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gevtm.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gevtm.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gevtm.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gevtm.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gevtm.o: /usr/include/i386/_types.h
s_gevtm.o: /usr/include/sys/_pthread/_pthread_types.h
s_gevtm.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gevtm.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gevtm.o: /usr/include/sys/_types/_int16_t.h
s_gevtm.o: /usr/include/sys/_types/_int32_t.h
s_gevtm.o: /usr/include/sys/_types/_int64_t.h
s_gevtm.o: /usr/include/sys/_types/_u_int8_t.h
s_gevtm.o: /usr/include/sys/_types/_u_int16_t.h
s_gevtm.o: /usr/include/sys/_types/_u_int32_t.h
s_gevtm.o: /usr/include/sys/_types/_u_int64_t.h
s_gevtm.o: /usr/include/sys/_types/_intptr_t.h
s_gevtm.o: /usr/include/sys/_types/_uintptr_t.h
s_gevtm.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gevtm.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gevtm.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gevtm.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gevtm.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gevtm.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gevtm.o: /usr/include/libkern/_OSByteOrder.h
s_gevtm.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gevtm.o: /usr/include/sys/_types/_u_char.h
s_gevtm.o: /usr/include/sys/_types/_u_short.h
s_gevtm.o: /usr/include/sys/_types/_u_int.h
s_gevtm.o: /usr/include/sys/_types/_caddr_t.h
s_gevtm.o: /usr/include/sys/_types/_dev_t.h
s_gevtm.o: /usr/include/sys/_types/_blkcnt_t.h
s_gevtm.o: /usr/include/sys/_types/_blksize_t.h
s_gevtm.o: /usr/include/sys/_types/_gid_t.h
s_gevtm.o: /usr/include/sys/_types/_in_addr_t.h
s_gevtm.o: /usr/include/sys/_types/_in_port_t.h
s_gevtm.o: /usr/include/sys/_types/_ino_t.h
s_gevtm.o: /usr/include/sys/_types/_ino64_t.h
s_gevtm.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gevtm.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gevtm.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gevtm.o: /usr/include/sys/_types/_clock_t.h
s_gevtm.o: /usr/include/sys/_types/_time_t.h
s_gevtm.o: /usr/include/sys/_types/_useconds_t.h
s_gevtm.o: /usr/include/sys/_types/_suseconds_t.h
s_gevtm.o: /usr/include/sys/_types/_rsize_t.h
s_gevtm.o: /usr/include/sys/_types/_errno_t.h
s_gevtm.o: /usr/include/sys/_types/_fd_def.h
s_gevtm.o: /usr/include/sys/_types/_fd_setsize.h
s_gevtm.o: /usr/include/sys/_types/_fd_set.h
s_gevtm.o: /usr/include/sys/_types/_fd_clr.h
s_gevtm.o: /usr/include/sys/_types/_fd_zero.h
s_gevtm.o: /usr/include/sys/_types/_fd_isset.h
s_gevtm.o: /usr/include/sys/_types/_fd_copy.h
s_gevtm.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gevtm.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gevtm.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gevtm.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gevtm.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gevtm.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gevtm.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gevtm.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gevtm.o: /usr/include/sys/_pthread/_pthread_t.h
s_gevtm.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gevtm.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gevtm.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gevtm.o: ../../../.././include/ncarg/c.h
s_gfa.o: /usr/include/stdlib.h /usr/include/Availability.h
s_gfa.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gfa.o: /usr/include/sys/_types.h /usr/include/sys/cdefs.h
s_gfa.o: /usr/include/sys/_symbol_aliasing.h
s_gfa.o: /usr/include/sys/_posix_availability.h /usr/include/machine/_types.h
s_gfa.o: /usr/include/i386/_types.h
s_gfa.o: /usr/include/sys/_pthread/_pthread_types.h /usr/include/sys/wait.h
s_gfa.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_id_t.h
s_gfa.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
s_gfa.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
s_gfa.o: /usr/include/machine/_mcontext.h /usr/include/i386/_mcontext.h
s_gfa.o: /usr/include/mach/machine/_structs.h
s_gfa.o: /usr/include/mach/i386/_structs.h /usr/include/machine/types.h
s_gfa.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gfa.o: /usr/include/sys/_types/_int16_t.h
s_gfa.o: /usr/include/sys/_types/_int32_t.h
s_gfa.o: /usr/include/sys/_types/_int64_t.h
s_gfa.o: /usr/include/sys/_types/_u_int8_t.h
s_gfa.o: /usr/include/sys/_types/_u_int16_t.h
s_gfa.o: /usr/include/sys/_types/_u_int32_t.h
s_gfa.o: /usr/include/sys/_types/_u_int64_t.h
s_gfa.o: /usr/include/sys/_types/_intptr_t.h
s_gfa.o: /usr/include/sys/_types/_uintptr_t.h
s_gfa.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gfa.o: /usr/include/sys/_types/_sigaltstack.h
s_gfa.o: /usr/include/sys/_types/_ucontext.h
s_gfa.o: /usr/include/sys/_types/_sigset_t.h
s_gfa.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_uid_t.h
s_gfa.o: /usr/include/sys/resource.h /usr/include/stdint.h
s_gfa.o: /usr/include/_types/_uint8_t.h /usr/include/_types/_uint16_t.h
s_gfa.o: /usr/include/_types/_uint32_t.h /usr/include/_types/_uint64_t.h
s_gfa.o: /usr/include/_types/_intmax_t.h /usr/include/_types/_uintmax_t.h
s_gfa.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
s_gfa.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gfa.o: /usr/include/libkern/_OSByteOrder.h
s_gfa.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
s_gfa.o: /usr/include/sys/_types/_ct_rune_t.h
s_gfa.o: /usr/include/sys/_types/_rune_t.h /usr/include/sys/_types/_wchar_t.h
s_gfa.o: /usr/include/sys/_types/_null.h /usr/include/sys/_types/_dev_t.h
s_gfa.o: /usr/include/sys/_types/_mode_t.h ../../../.././include/ncarg/gks.h
s_gfa.o: /usr/include/stdio.h /usr/include/sys/_types/_va_list.h
s_gfa.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gfa.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gfa.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gfa.o: /usr/include/sys/_types/_u_char.h /usr/include/sys/_types/_u_short.h
s_gfa.o: /usr/include/sys/_types/_u_int.h /usr/include/sys/_types/_caddr_t.h
s_gfa.o: /usr/include/sys/_types/_blkcnt_t.h
s_gfa.o: /usr/include/sys/_types/_blksize_t.h
s_gfa.o: /usr/include/sys/_types/_gid_t.h
s_gfa.o: /usr/include/sys/_types/_in_addr_t.h
s_gfa.o: /usr/include/sys/_types/_in_port_t.h
s_gfa.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
s_gfa.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_nlink_t.h
s_gfa.o: /usr/include/sys/_types/_clock_t.h /usr/include/sys/_types/_time_t.h
s_gfa.o: /usr/include/sys/_types/_useconds_t.h
s_gfa.o: /usr/include/sys/_types/_suseconds_t.h
s_gfa.o: /usr/include/sys/_types/_rsize_t.h
s_gfa.o: /usr/include/sys/_types/_errno_t.h /usr/include/sys/_types/_fd_def.h
s_gfa.o: /usr/include/sys/_types/_fd_setsize.h
s_gfa.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
s_gfa.o: /usr/include/sys/_types/_fd_zero.h
s_gfa.o: /usr/include/sys/_types/_fd_isset.h
s_gfa.o: /usr/include/sys/_types/_fd_copy.h
s_gfa.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gfa.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gfa.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gfa.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gfa.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gfa.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gfa.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gfa.o: /usr/include/sys/_pthread/_pthread_t.h
s_gfa.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gfa.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gfa.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gfa.o: ../../../.././include/ncarg/c.h
s_ggdp.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_ggdp.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_ggdp.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_ggdp.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_ggdp.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_ggdp.o: /usr/include/i386/_types.h
s_ggdp.o: /usr/include/sys/_pthread/_pthread_types.h
s_ggdp.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_ggdp.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_ggdp.o: /usr/include/sys/_types/_int16_t.h
s_ggdp.o: /usr/include/sys/_types/_int32_t.h
s_ggdp.o: /usr/include/sys/_types/_int64_t.h
s_ggdp.o: /usr/include/sys/_types/_u_int8_t.h
s_ggdp.o: /usr/include/sys/_types/_u_int16_t.h
s_ggdp.o: /usr/include/sys/_types/_u_int32_t.h
s_ggdp.o: /usr/include/sys/_types/_u_int64_t.h
s_ggdp.o: /usr/include/sys/_types/_intptr_t.h
s_ggdp.o: /usr/include/sys/_types/_uintptr_t.h
s_ggdp.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_ggdp.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_ggdp.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_ggdp.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_ggdp.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_ggdp.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_ggdp.o: /usr/include/libkern/_OSByteOrder.h
s_ggdp.o: /usr/include/libkern/i386/_OSByteOrder.h
s_ggdp.o: /usr/include/sys/_types/_u_char.h
s_ggdp.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
s_ggdp.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
s_ggdp.o: /usr/include/sys/_types/_blkcnt_t.h
s_ggdp.o: /usr/include/sys/_types/_blksize_t.h
s_ggdp.o: /usr/include/sys/_types/_gid_t.h
s_ggdp.o: /usr/include/sys/_types/_in_addr_t.h
s_ggdp.o: /usr/include/sys/_types/_in_port_t.h
s_ggdp.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
s_ggdp.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_ggdp.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_ggdp.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_ggdp.o: /usr/include/sys/_types/_clock_t.h
s_ggdp.o: /usr/include/sys/_types/_time_t.h
s_ggdp.o: /usr/include/sys/_types/_useconds_t.h
s_ggdp.o: /usr/include/sys/_types/_suseconds_t.h
s_ggdp.o: /usr/include/sys/_types/_rsize_t.h
s_ggdp.o: /usr/include/sys/_types/_errno_t.h
s_ggdp.o: /usr/include/sys/_types/_fd_def.h
s_ggdp.o: /usr/include/sys/_types/_fd_setsize.h
s_ggdp.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
s_ggdp.o: /usr/include/sys/_types/_fd_zero.h
s_ggdp.o: /usr/include/sys/_types/_fd_isset.h
s_ggdp.o: /usr/include/sys/_types/_fd_copy.h
s_ggdp.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_ggdp.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_ggdp.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_ggdp.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_ggdp.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_ggdp.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_ggdp.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_ggdp.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_ggdp.o: /usr/include/sys/_pthread/_pthread_t.h
s_ggdp.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_ggdp.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_ggdp.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_ggdp.o: ../../../.././include/ncarg/c.h
s_ggtitm.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_ggtitm.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_ggtitm.o: /usr/include/sys/_posix_availability.h
s_ggtitm.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_ggtitm.o: /usr/include/_types.h /usr/include/sys/_types.h
s_ggtitm.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_ggtitm.o: /usr/include/sys/_pthread/_pthread_types.h
s_ggtitm.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_ggtitm.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_ggtitm.o: /usr/include/sys/_types/_int16_t.h
s_ggtitm.o: /usr/include/sys/_types/_int32_t.h
s_ggtitm.o: /usr/include/sys/_types/_int64_t.h
s_ggtitm.o: /usr/include/sys/_types/_u_int8_t.h
s_ggtitm.o: /usr/include/sys/_types/_u_int16_t.h
s_ggtitm.o: /usr/include/sys/_types/_u_int32_t.h
s_ggtitm.o: /usr/include/sys/_types/_u_int64_t.h
s_ggtitm.o: /usr/include/sys/_types/_intptr_t.h
s_ggtitm.o: /usr/include/sys/_types/_uintptr_t.h
s_ggtitm.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_ggtitm.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_ggtitm.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_ggtitm.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_ggtitm.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_ggtitm.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_ggtitm.o: /usr/include/libkern/_OSByteOrder.h
s_ggtitm.o: /usr/include/libkern/i386/_OSByteOrder.h
s_ggtitm.o: /usr/include/sys/_types/_u_char.h
s_ggtitm.o: /usr/include/sys/_types/_u_short.h
s_ggtitm.o: /usr/include/sys/_types/_u_int.h
s_ggtitm.o: /usr/include/sys/_types/_caddr_t.h
s_ggtitm.o: /usr/include/sys/_types/_dev_t.h
s_ggtitm.o: /usr/include/sys/_types/_blkcnt_t.h
s_ggtitm.o: /usr/include/sys/_types/_blksize_t.h
s_ggtitm.o: /usr/include/sys/_types/_gid_t.h
s_ggtitm.o: /usr/include/sys/_types/_in_addr_t.h
s_ggtitm.o: /usr/include/sys/_types/_in_port_t.h
s_ggtitm.o: /usr/include/sys/_types/_ino_t.h
s_ggtitm.o: /usr/include/sys/_types/_ino64_t.h
s_ggtitm.o: /usr/include/sys/_types/_key_t.h
s_ggtitm.o: /usr/include/sys/_types/_mode_t.h
s_ggtitm.o: /usr/include/sys/_types/_nlink_t.h
s_ggtitm.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_ggtitm.o: /usr/include/sys/_types/_uid_t.h
s_ggtitm.o: /usr/include/sys/_types/_clock_t.h
s_ggtitm.o: /usr/include/sys/_types/_time_t.h
s_ggtitm.o: /usr/include/sys/_types/_useconds_t.h
s_ggtitm.o: /usr/include/sys/_types/_suseconds_t.h
s_ggtitm.o: /usr/include/sys/_types/_rsize_t.h
s_ggtitm.o: /usr/include/sys/_types/_errno_t.h
s_ggtitm.o: /usr/include/sys/_types/_fd_def.h
s_ggtitm.o: /usr/include/sys/_types/_fd_setsize.h
s_ggtitm.o: /usr/include/sys/_types/_fd_set.h
s_ggtitm.o: /usr/include/sys/_types/_fd_clr.h
s_ggtitm.o: /usr/include/sys/_types/_fd_zero.h
s_ggtitm.o: /usr/include/sys/_types/_fd_isset.h
s_ggtitm.o: /usr/include/sys/_types/_fd_copy.h
s_ggtitm.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_ggtitm.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_ggtitm.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_ggtitm.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_ggtitm.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_ggtitm.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_ggtitm.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_ggtitm.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_ggtitm.o: /usr/include/sys/_pthread/_pthread_t.h
s_ggtitm.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_ggtitm.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_ggtitm.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_ggtitm.o: ../../../.././include/ncarg/c.h
s_giitm.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_giitm.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_giitm.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_giitm.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_giitm.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_giitm.o: /usr/include/i386/_types.h
s_giitm.o: /usr/include/sys/_pthread/_pthread_types.h
s_giitm.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_giitm.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_giitm.o: /usr/include/sys/_types/_int16_t.h
s_giitm.o: /usr/include/sys/_types/_int32_t.h
s_giitm.o: /usr/include/sys/_types/_int64_t.h
s_giitm.o: /usr/include/sys/_types/_u_int8_t.h
s_giitm.o: /usr/include/sys/_types/_u_int16_t.h
s_giitm.o: /usr/include/sys/_types/_u_int32_t.h
s_giitm.o: /usr/include/sys/_types/_u_int64_t.h
s_giitm.o: /usr/include/sys/_types/_intptr_t.h
s_giitm.o: /usr/include/sys/_types/_uintptr_t.h
s_giitm.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_giitm.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_giitm.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_giitm.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_giitm.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_giitm.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_giitm.o: /usr/include/libkern/_OSByteOrder.h
s_giitm.o: /usr/include/libkern/i386/_OSByteOrder.h
s_giitm.o: /usr/include/sys/_types/_u_char.h
s_giitm.o: /usr/include/sys/_types/_u_short.h
s_giitm.o: /usr/include/sys/_types/_u_int.h
s_giitm.o: /usr/include/sys/_types/_caddr_t.h
s_giitm.o: /usr/include/sys/_types/_dev_t.h
s_giitm.o: /usr/include/sys/_types/_blkcnt_t.h
s_giitm.o: /usr/include/sys/_types/_blksize_t.h
s_giitm.o: /usr/include/sys/_types/_gid_t.h
s_giitm.o: /usr/include/sys/_types/_in_addr_t.h
s_giitm.o: /usr/include/sys/_types/_in_port_t.h
s_giitm.o: /usr/include/sys/_types/_ino_t.h
s_giitm.o: /usr/include/sys/_types/_ino64_t.h
s_giitm.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_giitm.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_giitm.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_giitm.o: /usr/include/sys/_types/_clock_t.h
s_giitm.o: /usr/include/sys/_types/_time_t.h
s_giitm.o: /usr/include/sys/_types/_useconds_t.h
s_giitm.o: /usr/include/sys/_types/_suseconds_t.h
s_giitm.o: /usr/include/sys/_types/_rsize_t.h
s_giitm.o: /usr/include/sys/_types/_errno_t.h
s_giitm.o: /usr/include/sys/_types/_fd_def.h
s_giitm.o: /usr/include/sys/_types/_fd_setsize.h
s_giitm.o: /usr/include/sys/_types/_fd_set.h
s_giitm.o: /usr/include/sys/_types/_fd_clr.h
s_giitm.o: /usr/include/sys/_types/_fd_zero.h
s_giitm.o: /usr/include/sys/_types/_fd_isset.h
s_giitm.o: /usr/include/sys/_types/_fd_copy.h
s_giitm.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_giitm.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_giitm.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_giitm.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_giitm.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_giitm.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_giitm.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_giitm.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_giitm.o: /usr/include/sys/_pthread/_pthread_t.h
s_giitm.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_giitm.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_giitm.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_giitm.o: ../../../.././include/ncarg/c.h
s_gopks.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gopks.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gopks.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gopks.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gopks.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gopks.o: /usr/include/i386/_types.h
s_gopks.o: /usr/include/sys/_pthread/_pthread_types.h
s_gopks.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gopks.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gopks.o: /usr/include/sys/_types/_int16_t.h
s_gopks.o: /usr/include/sys/_types/_int32_t.h
s_gopks.o: /usr/include/sys/_types/_int64_t.h
s_gopks.o: /usr/include/sys/_types/_u_int8_t.h
s_gopks.o: /usr/include/sys/_types/_u_int16_t.h
s_gopks.o: /usr/include/sys/_types/_u_int32_t.h
s_gopks.o: /usr/include/sys/_types/_u_int64_t.h
s_gopks.o: /usr/include/sys/_types/_intptr_t.h
s_gopks.o: /usr/include/sys/_types/_uintptr_t.h
s_gopks.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gopks.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gopks.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gopks.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gopks.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gopks.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gopks.o: /usr/include/libkern/_OSByteOrder.h
s_gopks.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gopks.o: /usr/include/sys/_types/_u_char.h
s_gopks.o: /usr/include/sys/_types/_u_short.h
s_gopks.o: /usr/include/sys/_types/_u_int.h
s_gopks.o: /usr/include/sys/_types/_caddr_t.h
s_gopks.o: /usr/include/sys/_types/_dev_t.h
s_gopks.o: /usr/include/sys/_types/_blkcnt_t.h
s_gopks.o: /usr/include/sys/_types/_blksize_t.h
s_gopks.o: /usr/include/sys/_types/_gid_t.h
s_gopks.o: /usr/include/sys/_types/_in_addr_t.h
s_gopks.o: /usr/include/sys/_types/_in_port_t.h
s_gopks.o: /usr/include/sys/_types/_ino_t.h
s_gopks.o: /usr/include/sys/_types/_ino64_t.h
s_gopks.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gopks.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gopks.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gopks.o: /usr/include/sys/_types/_clock_t.h
s_gopks.o: /usr/include/sys/_types/_time_t.h
s_gopks.o: /usr/include/sys/_types/_useconds_t.h
s_gopks.o: /usr/include/sys/_types/_suseconds_t.h
s_gopks.o: /usr/include/sys/_types/_rsize_t.h
s_gopks.o: /usr/include/sys/_types/_errno_t.h
s_gopks.o: /usr/include/sys/_types/_fd_def.h
s_gopks.o: /usr/include/sys/_types/_fd_setsize.h
s_gopks.o: /usr/include/sys/_types/_fd_set.h
s_gopks.o: /usr/include/sys/_types/_fd_clr.h
s_gopks.o: /usr/include/sys/_types/_fd_zero.h
s_gopks.o: /usr/include/sys/_types/_fd_isset.h
s_gopks.o: /usr/include/sys/_types/_fd_copy.h
s_gopks.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gopks.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gopks.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gopks.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gopks.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gopks.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gopks.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gopks.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gopks.o: /usr/include/sys/_pthread/_pthread_t.h
s_gopks.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gopks.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gopks.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gopks.o: ../../../.././include/ncarg/c.h /usr/include/string.h
s_gopks.o: /usr/include/strings.h /usr/include/secure/_strings.h
s_gopks.o: /usr/include/secure/_string.h
s_gopwk.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gopwk.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gopwk.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gopwk.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gopwk.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gopwk.o: /usr/include/i386/_types.h
s_gopwk.o: /usr/include/sys/_pthread/_pthread_types.h
s_gopwk.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gopwk.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gopwk.o: /usr/include/sys/_types/_int16_t.h
s_gopwk.o: /usr/include/sys/_types/_int32_t.h
s_gopwk.o: /usr/include/sys/_types/_int64_t.h
s_gopwk.o: /usr/include/sys/_types/_u_int8_t.h
s_gopwk.o: /usr/include/sys/_types/_u_int16_t.h
s_gopwk.o: /usr/include/sys/_types/_u_int32_t.h
s_gopwk.o: /usr/include/sys/_types/_u_int64_t.h
s_gopwk.o: /usr/include/sys/_types/_intptr_t.h
s_gopwk.o: /usr/include/sys/_types/_uintptr_t.h
s_gopwk.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gopwk.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gopwk.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gopwk.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gopwk.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gopwk.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gopwk.o: /usr/include/libkern/_OSByteOrder.h
s_gopwk.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gopwk.o: /usr/include/sys/_types/_u_char.h
s_gopwk.o: /usr/include/sys/_types/_u_short.h
s_gopwk.o: /usr/include/sys/_types/_u_int.h
s_gopwk.o: /usr/include/sys/_types/_caddr_t.h
s_gopwk.o: /usr/include/sys/_types/_dev_t.h
s_gopwk.o: /usr/include/sys/_types/_blkcnt_t.h
s_gopwk.o: /usr/include/sys/_types/_blksize_t.h
s_gopwk.o: /usr/include/sys/_types/_gid_t.h
s_gopwk.o: /usr/include/sys/_types/_in_addr_t.h
s_gopwk.o: /usr/include/sys/_types/_in_port_t.h
s_gopwk.o: /usr/include/sys/_types/_ino_t.h
s_gopwk.o: /usr/include/sys/_types/_ino64_t.h
s_gopwk.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gopwk.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gopwk.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gopwk.o: /usr/include/sys/_types/_clock_t.h
s_gopwk.o: /usr/include/sys/_types/_time_t.h
s_gopwk.o: /usr/include/sys/_types/_useconds_t.h
s_gopwk.o: /usr/include/sys/_types/_suseconds_t.h
s_gopwk.o: /usr/include/sys/_types/_rsize_t.h
s_gopwk.o: /usr/include/sys/_types/_errno_t.h
s_gopwk.o: /usr/include/sys/_types/_fd_def.h
s_gopwk.o: /usr/include/sys/_types/_fd_setsize.h
s_gopwk.o: /usr/include/sys/_types/_fd_set.h
s_gopwk.o: /usr/include/sys/_types/_fd_clr.h
s_gopwk.o: /usr/include/sys/_types/_fd_zero.h
s_gopwk.o: /usr/include/sys/_types/_fd_isset.h
s_gopwk.o: /usr/include/sys/_types/_fd_copy.h
s_gopwk.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gopwk.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gopwk.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gopwk.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gopwk.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gopwk.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gopwk.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gopwk.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gopwk.o: /usr/include/sys/_pthread/_pthread_t.h
s_gopwk.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gopwk.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gopwk.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gopwk.o: ../../../.././include/ncarg/c.h /usr/include/stdlib.h
s_gopwk.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
s_gopwk.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
s_gopwk.o: /usr/include/machine/_mcontext.h /usr/include/i386/_mcontext.h
s_gopwk.o: /usr/include/mach/machine/_structs.h
s_gopwk.o: /usr/include/mach/i386/_structs.h
s_gopwk.o: /usr/include/sys/_types/_sigaltstack.h
s_gopwk.o: /usr/include/sys/_types/_ucontext.h
s_gopwk.o: /usr/include/sys/_types/_sigset_t.h /usr/include/sys/resource.h
s_gopwk.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
s_gopwk.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
s_gopwk.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
s_gopwk.o: /usr/include/_types/_uintmax_t.h
s_gopwk.o: /usr/include/sys/_types/_timeval.h /usr/include/alloca.h
s_gopwk.o: /usr/include/sys/_types/_ct_rune_t.h
s_gopwk.o: /usr/include/sys/_types/_rune_t.h
s_gopwk.o: /usr/include/sys/_types/_wchar_t.h /usr/include/string.h
s_gopwk.o: /usr/include/strings.h /usr/include/secure/_strings.h
s_gopwk.o: /usr/include/secure/_string.h /usr/include/ctype.h
s_gopwk.o: /usr/include/runetype.h /usr/include/sys/_types/_wint_t.h
s_gpl.o: /usr/include/stdlib.h /usr/include/Availability.h
s_gpl.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gpl.o: /usr/include/sys/_types.h /usr/include/sys/cdefs.h
s_gpl.o: /usr/include/sys/_symbol_aliasing.h
s_gpl.o: /usr/include/sys/_posix_availability.h /usr/include/machine/_types.h
s_gpl.o: /usr/include/i386/_types.h
s_gpl.o: /usr/include/sys/_pthread/_pthread_types.h /usr/include/sys/wait.h
s_gpl.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_id_t.h
s_gpl.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
s_gpl.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
s_gpl.o: /usr/include/machine/_mcontext.h /usr/include/i386/_mcontext.h
s_gpl.o: /usr/include/mach/machine/_structs.h
s_gpl.o: /usr/include/mach/i386/_structs.h /usr/include/machine/types.h
s_gpl.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gpl.o: /usr/include/sys/_types/_int16_t.h
s_gpl.o: /usr/include/sys/_types/_int32_t.h
s_gpl.o: /usr/include/sys/_types/_int64_t.h
s_gpl.o: /usr/include/sys/_types/_u_int8_t.h
s_gpl.o: /usr/include/sys/_types/_u_int16_t.h
s_gpl.o: /usr/include/sys/_types/_u_int32_t.h
s_gpl.o: /usr/include/sys/_types/_u_int64_t.h
s_gpl.o: /usr/include/sys/_types/_intptr_t.h
s_gpl.o: /usr/include/sys/_types/_uintptr_t.h
s_gpl.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gpl.o: /usr/include/sys/_types/_sigaltstack.h
s_gpl.o: /usr/include/sys/_types/_ucontext.h
s_gpl.o: /usr/include/sys/_types/_sigset_t.h
s_gpl.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_uid_t.h
s_gpl.o: /usr/include/sys/resource.h /usr/include/stdint.h
s_gpl.o: /usr/include/_types/_uint8_t.h /usr/include/_types/_uint16_t.h
s_gpl.o: /usr/include/_types/_uint32_t.h /usr/include/_types/_uint64_t.h
s_gpl.o: /usr/include/_types/_intmax_t.h /usr/include/_types/_uintmax_t.h
s_gpl.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
s_gpl.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gpl.o: /usr/include/libkern/_OSByteOrder.h
s_gpl.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
s_gpl.o: /usr/include/sys/_types/_ct_rune_t.h
s_gpl.o: /usr/include/sys/_types/_rune_t.h /usr/include/sys/_types/_wchar_t.h
s_gpl.o: /usr/include/sys/_types/_null.h /usr/include/sys/_types/_dev_t.h
s_gpl.o: /usr/include/sys/_types/_mode_t.h ../../../.././include/ncarg/gks.h
s_gpl.o: /usr/include/stdio.h /usr/include/sys/_types/_va_list.h
s_gpl.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gpl.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gpl.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gpl.o: /usr/include/sys/_types/_u_char.h /usr/include/sys/_types/_u_short.h
s_gpl.o: /usr/include/sys/_types/_u_int.h /usr/include/sys/_types/_caddr_t.h
s_gpl.o: /usr/include/sys/_types/_blkcnt_t.h
s_gpl.o: /usr/include/sys/_types/_blksize_t.h
s_gpl.o: /usr/include/sys/_types/_gid_t.h
s_gpl.o: /usr/include/sys/_types/_in_addr_t.h
s_gpl.o: /usr/include/sys/_types/_in_port_t.h
s_gpl.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
s_gpl.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_nlink_t.h
s_gpl.o: /usr/include/sys/_types/_clock_t.h /usr/include/sys/_types/_time_t.h
s_gpl.o: /usr/include/sys/_types/_useconds_t.h
s_gpl.o: /usr/include/sys/_types/_suseconds_t.h
s_gpl.o: /usr/include/sys/_types/_rsize_t.h
s_gpl.o: /usr/include/sys/_types/_errno_t.h /usr/include/sys/_types/_fd_def.h
s_gpl.o: /usr/include/sys/_types/_fd_setsize.h
s_gpl.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
s_gpl.o: /usr/include/sys/_types/_fd_zero.h
s_gpl.o: /usr/include/sys/_types/_fd_isset.h
s_gpl.o: /usr/include/sys/_types/_fd_copy.h
s_gpl.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gpl.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gpl.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gpl.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gpl.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gpl.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gpl.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gpl.o: /usr/include/sys/_pthread/_pthread_t.h
s_gpl.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gpl.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gpl.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gpl.o: ../../../.././include/ncarg/c.h
s_gpm.o: /usr/include/stdlib.h /usr/include/Availability.h
s_gpm.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gpm.o: /usr/include/sys/_types.h /usr/include/sys/cdefs.h
s_gpm.o: /usr/include/sys/_symbol_aliasing.h
s_gpm.o: /usr/include/sys/_posix_availability.h /usr/include/machine/_types.h
s_gpm.o: /usr/include/i386/_types.h
s_gpm.o: /usr/include/sys/_pthread/_pthread_types.h /usr/include/sys/wait.h
s_gpm.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_id_t.h
s_gpm.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
s_gpm.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
s_gpm.o: /usr/include/machine/_mcontext.h /usr/include/i386/_mcontext.h
s_gpm.o: /usr/include/mach/machine/_structs.h
s_gpm.o: /usr/include/mach/i386/_structs.h /usr/include/machine/types.h
s_gpm.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gpm.o: /usr/include/sys/_types/_int16_t.h
s_gpm.o: /usr/include/sys/_types/_int32_t.h
s_gpm.o: /usr/include/sys/_types/_int64_t.h
s_gpm.o: /usr/include/sys/_types/_u_int8_t.h
s_gpm.o: /usr/include/sys/_types/_u_int16_t.h
s_gpm.o: /usr/include/sys/_types/_u_int32_t.h
s_gpm.o: /usr/include/sys/_types/_u_int64_t.h
s_gpm.o: /usr/include/sys/_types/_intptr_t.h
s_gpm.o: /usr/include/sys/_types/_uintptr_t.h
s_gpm.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gpm.o: /usr/include/sys/_types/_sigaltstack.h
s_gpm.o: /usr/include/sys/_types/_ucontext.h
s_gpm.o: /usr/include/sys/_types/_sigset_t.h
s_gpm.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_uid_t.h
s_gpm.o: /usr/include/sys/resource.h /usr/include/stdint.h
s_gpm.o: /usr/include/_types/_uint8_t.h /usr/include/_types/_uint16_t.h
s_gpm.o: /usr/include/_types/_uint32_t.h /usr/include/_types/_uint64_t.h
s_gpm.o: /usr/include/_types/_intmax_t.h /usr/include/_types/_uintmax_t.h
s_gpm.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
s_gpm.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gpm.o: /usr/include/libkern/_OSByteOrder.h
s_gpm.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
s_gpm.o: /usr/include/sys/_types/_ct_rune_t.h
s_gpm.o: /usr/include/sys/_types/_rune_t.h /usr/include/sys/_types/_wchar_t.h
s_gpm.o: /usr/include/sys/_types/_null.h /usr/include/sys/_types/_dev_t.h
s_gpm.o: /usr/include/sys/_types/_mode_t.h ../../../.././include/ncarg/gks.h
s_gpm.o: /usr/include/stdio.h /usr/include/sys/_types/_va_list.h
s_gpm.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gpm.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gpm.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gpm.o: /usr/include/sys/_types/_u_char.h /usr/include/sys/_types/_u_short.h
s_gpm.o: /usr/include/sys/_types/_u_int.h /usr/include/sys/_types/_caddr_t.h
s_gpm.o: /usr/include/sys/_types/_blkcnt_t.h
s_gpm.o: /usr/include/sys/_types/_blksize_t.h
s_gpm.o: /usr/include/sys/_types/_gid_t.h
s_gpm.o: /usr/include/sys/_types/_in_addr_t.h
s_gpm.o: /usr/include/sys/_types/_in_port_t.h
s_gpm.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
s_gpm.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_nlink_t.h
s_gpm.o: /usr/include/sys/_types/_clock_t.h /usr/include/sys/_types/_time_t.h
s_gpm.o: /usr/include/sys/_types/_useconds_t.h
s_gpm.o: /usr/include/sys/_types/_suseconds_t.h
s_gpm.o: /usr/include/sys/_types/_rsize_t.h
s_gpm.o: /usr/include/sys/_types/_errno_t.h /usr/include/sys/_types/_fd_def.h
s_gpm.o: /usr/include/sys/_types/_fd_setsize.h
s_gpm.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
s_gpm.o: /usr/include/sys/_types/_fd_zero.h
s_gpm.o: /usr/include/sys/_types/_fd_isset.h
s_gpm.o: /usr/include/sys/_types/_fd_copy.h
s_gpm.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gpm.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gpm.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gpm.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gpm.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gpm.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gpm.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gpm.o: /usr/include/sys/_pthread/_pthread_t.h
s_gpm.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gpm.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gpm.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gpm.o: ../../../.././include/ncarg/c.h
s_gqacwk.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqacwk.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqacwk.o: /usr/include/sys/_posix_availability.h
s_gqacwk.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gqacwk.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gqacwk.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gqacwk.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqacwk.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqacwk.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqacwk.o: /usr/include/sys/_types/_int16_t.h
s_gqacwk.o: /usr/include/sys/_types/_int32_t.h
s_gqacwk.o: /usr/include/sys/_types/_int64_t.h
s_gqacwk.o: /usr/include/sys/_types/_u_int8_t.h
s_gqacwk.o: /usr/include/sys/_types/_u_int16_t.h
s_gqacwk.o: /usr/include/sys/_types/_u_int32_t.h
s_gqacwk.o: /usr/include/sys/_types/_u_int64_t.h
s_gqacwk.o: /usr/include/sys/_types/_intptr_t.h
s_gqacwk.o: /usr/include/sys/_types/_uintptr_t.h
s_gqacwk.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqacwk.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqacwk.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqacwk.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqacwk.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqacwk.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqacwk.o: /usr/include/libkern/_OSByteOrder.h
s_gqacwk.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqacwk.o: /usr/include/sys/_types/_u_char.h
s_gqacwk.o: /usr/include/sys/_types/_u_short.h
s_gqacwk.o: /usr/include/sys/_types/_u_int.h
s_gqacwk.o: /usr/include/sys/_types/_caddr_t.h
s_gqacwk.o: /usr/include/sys/_types/_dev_t.h
s_gqacwk.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqacwk.o: /usr/include/sys/_types/_blksize_t.h
s_gqacwk.o: /usr/include/sys/_types/_gid_t.h
s_gqacwk.o: /usr/include/sys/_types/_in_addr_t.h
s_gqacwk.o: /usr/include/sys/_types/_in_port_t.h
s_gqacwk.o: /usr/include/sys/_types/_ino_t.h
s_gqacwk.o: /usr/include/sys/_types/_ino64_t.h
s_gqacwk.o: /usr/include/sys/_types/_key_t.h
s_gqacwk.o: /usr/include/sys/_types/_mode_t.h
s_gqacwk.o: /usr/include/sys/_types/_nlink_t.h
s_gqacwk.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gqacwk.o: /usr/include/sys/_types/_uid_t.h
s_gqacwk.o: /usr/include/sys/_types/_clock_t.h
s_gqacwk.o: /usr/include/sys/_types/_time_t.h
s_gqacwk.o: /usr/include/sys/_types/_useconds_t.h
s_gqacwk.o: /usr/include/sys/_types/_suseconds_t.h
s_gqacwk.o: /usr/include/sys/_types/_rsize_t.h
s_gqacwk.o: /usr/include/sys/_types/_errno_t.h
s_gqacwk.o: /usr/include/sys/_types/_fd_def.h
s_gqacwk.o: /usr/include/sys/_types/_fd_setsize.h
s_gqacwk.o: /usr/include/sys/_types/_fd_set.h
s_gqacwk.o: /usr/include/sys/_types/_fd_clr.h
s_gqacwk.o: /usr/include/sys/_types/_fd_zero.h
s_gqacwk.o: /usr/include/sys/_types/_fd_isset.h
s_gqacwk.o: /usr/include/sys/_types/_fd_copy.h
s_gqacwk.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqacwk.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqacwk.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqacwk.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqacwk.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqacwk.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqacwk.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqacwk.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqacwk.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqacwk.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqacwk.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqacwk.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqacwk.o: ../../../.././include/ncarg/c.h
s_gqasf.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqasf.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqasf.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gqasf.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gqasf.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gqasf.o: /usr/include/i386/_types.h
s_gqasf.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqasf.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqasf.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqasf.o: /usr/include/sys/_types/_int16_t.h
s_gqasf.o: /usr/include/sys/_types/_int32_t.h
s_gqasf.o: /usr/include/sys/_types/_int64_t.h
s_gqasf.o: /usr/include/sys/_types/_u_int8_t.h
s_gqasf.o: /usr/include/sys/_types/_u_int16_t.h
s_gqasf.o: /usr/include/sys/_types/_u_int32_t.h
s_gqasf.o: /usr/include/sys/_types/_u_int64_t.h
s_gqasf.o: /usr/include/sys/_types/_intptr_t.h
s_gqasf.o: /usr/include/sys/_types/_uintptr_t.h
s_gqasf.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqasf.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqasf.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqasf.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqasf.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqasf.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqasf.o: /usr/include/libkern/_OSByteOrder.h
s_gqasf.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqasf.o: /usr/include/sys/_types/_u_char.h
s_gqasf.o: /usr/include/sys/_types/_u_short.h
s_gqasf.o: /usr/include/sys/_types/_u_int.h
s_gqasf.o: /usr/include/sys/_types/_caddr_t.h
s_gqasf.o: /usr/include/sys/_types/_dev_t.h
s_gqasf.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqasf.o: /usr/include/sys/_types/_blksize_t.h
s_gqasf.o: /usr/include/sys/_types/_gid_t.h
s_gqasf.o: /usr/include/sys/_types/_in_addr_t.h
s_gqasf.o: /usr/include/sys/_types/_in_port_t.h
s_gqasf.o: /usr/include/sys/_types/_ino_t.h
s_gqasf.o: /usr/include/sys/_types/_ino64_t.h
s_gqasf.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gqasf.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gqasf.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gqasf.o: /usr/include/sys/_types/_clock_t.h
s_gqasf.o: /usr/include/sys/_types/_time_t.h
s_gqasf.o: /usr/include/sys/_types/_useconds_t.h
s_gqasf.o: /usr/include/sys/_types/_suseconds_t.h
s_gqasf.o: /usr/include/sys/_types/_rsize_t.h
s_gqasf.o: /usr/include/sys/_types/_errno_t.h
s_gqasf.o: /usr/include/sys/_types/_fd_def.h
s_gqasf.o: /usr/include/sys/_types/_fd_setsize.h
s_gqasf.o: /usr/include/sys/_types/_fd_set.h
s_gqasf.o: /usr/include/sys/_types/_fd_clr.h
s_gqasf.o: /usr/include/sys/_types/_fd_zero.h
s_gqasf.o: /usr/include/sys/_types/_fd_isset.h
s_gqasf.o: /usr/include/sys/_types/_fd_copy.h
s_gqasf.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqasf.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqasf.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqasf.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqasf.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqasf.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqasf.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqasf.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqasf.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqasf.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqasf.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqasf.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqasf.o: ../../../.././include/ncarg/c.h
s_gqcf.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqcf.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqcf.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gqcf.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gqcf.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gqcf.o: /usr/include/i386/_types.h
s_gqcf.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqcf.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqcf.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqcf.o: /usr/include/sys/_types/_int16_t.h
s_gqcf.o: /usr/include/sys/_types/_int32_t.h
s_gqcf.o: /usr/include/sys/_types/_int64_t.h
s_gqcf.o: /usr/include/sys/_types/_u_int8_t.h
s_gqcf.o: /usr/include/sys/_types/_u_int16_t.h
s_gqcf.o: /usr/include/sys/_types/_u_int32_t.h
s_gqcf.o: /usr/include/sys/_types/_u_int64_t.h
s_gqcf.o: /usr/include/sys/_types/_intptr_t.h
s_gqcf.o: /usr/include/sys/_types/_uintptr_t.h
s_gqcf.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqcf.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqcf.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqcf.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqcf.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqcf.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqcf.o: /usr/include/libkern/_OSByteOrder.h
s_gqcf.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqcf.o: /usr/include/sys/_types/_u_char.h
s_gqcf.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
s_gqcf.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
s_gqcf.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqcf.o: /usr/include/sys/_types/_blksize_t.h
s_gqcf.o: /usr/include/sys/_types/_gid_t.h
s_gqcf.o: /usr/include/sys/_types/_in_addr_t.h
s_gqcf.o: /usr/include/sys/_types/_in_port_t.h
s_gqcf.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
s_gqcf.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gqcf.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gqcf.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gqcf.o: /usr/include/sys/_types/_clock_t.h
s_gqcf.o: /usr/include/sys/_types/_time_t.h
s_gqcf.o: /usr/include/sys/_types/_useconds_t.h
s_gqcf.o: /usr/include/sys/_types/_suseconds_t.h
s_gqcf.o: /usr/include/sys/_types/_rsize_t.h
s_gqcf.o: /usr/include/sys/_types/_errno_t.h
s_gqcf.o: /usr/include/sys/_types/_fd_def.h
s_gqcf.o: /usr/include/sys/_types/_fd_setsize.h
s_gqcf.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
s_gqcf.o: /usr/include/sys/_types/_fd_zero.h
s_gqcf.o: /usr/include/sys/_types/_fd_isset.h
s_gqcf.o: /usr/include/sys/_types/_fd_copy.h
s_gqcf.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqcf.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqcf.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqcf.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqcf.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqcf.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqcf.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqcf.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqcf.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqcf.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqcf.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqcf.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqcf.o: ../../../.././include/ncarg/c.h
s_gqchb.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqchb.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqchb.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gqchb.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gqchb.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gqchb.o: /usr/include/i386/_types.h
s_gqchb.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqchb.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqchb.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqchb.o: /usr/include/sys/_types/_int16_t.h
s_gqchb.o: /usr/include/sys/_types/_int32_t.h
s_gqchb.o: /usr/include/sys/_types/_int64_t.h
s_gqchb.o: /usr/include/sys/_types/_u_int8_t.h
s_gqchb.o: /usr/include/sys/_types/_u_int16_t.h
s_gqchb.o: /usr/include/sys/_types/_u_int32_t.h
s_gqchb.o: /usr/include/sys/_types/_u_int64_t.h
s_gqchb.o: /usr/include/sys/_types/_intptr_t.h
s_gqchb.o: /usr/include/sys/_types/_uintptr_t.h
s_gqchb.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqchb.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqchb.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqchb.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqchb.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqchb.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqchb.o: /usr/include/libkern/_OSByteOrder.h
s_gqchb.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqchb.o: /usr/include/sys/_types/_u_char.h
s_gqchb.o: /usr/include/sys/_types/_u_short.h
s_gqchb.o: /usr/include/sys/_types/_u_int.h
s_gqchb.o: /usr/include/sys/_types/_caddr_t.h
s_gqchb.o: /usr/include/sys/_types/_dev_t.h
s_gqchb.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqchb.o: /usr/include/sys/_types/_blksize_t.h
s_gqchb.o: /usr/include/sys/_types/_gid_t.h
s_gqchb.o: /usr/include/sys/_types/_in_addr_t.h
s_gqchb.o: /usr/include/sys/_types/_in_port_t.h
s_gqchb.o: /usr/include/sys/_types/_ino_t.h
s_gqchb.o: /usr/include/sys/_types/_ino64_t.h
s_gqchb.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gqchb.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gqchb.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gqchb.o: /usr/include/sys/_types/_clock_t.h
s_gqchb.o: /usr/include/sys/_types/_time_t.h
s_gqchb.o: /usr/include/sys/_types/_useconds_t.h
s_gqchb.o: /usr/include/sys/_types/_suseconds_t.h
s_gqchb.o: /usr/include/sys/_types/_rsize_t.h
s_gqchb.o: /usr/include/sys/_types/_errno_t.h
s_gqchb.o: /usr/include/sys/_types/_fd_def.h
s_gqchb.o: /usr/include/sys/_types/_fd_setsize.h
s_gqchb.o: /usr/include/sys/_types/_fd_set.h
s_gqchb.o: /usr/include/sys/_types/_fd_clr.h
s_gqchb.o: /usr/include/sys/_types/_fd_zero.h
s_gqchb.o: /usr/include/sys/_types/_fd_isset.h
s_gqchb.o: /usr/include/sys/_types/_fd_copy.h
s_gqchb.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqchb.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqchb.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqchb.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqchb.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqchb.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqchb.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqchb.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqchb.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqchb.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqchb.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqchb.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqchb.o: ../../../.././include/ncarg/c.h
s_gqchh.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqchh.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqchh.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gqchh.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gqchh.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gqchh.o: /usr/include/i386/_types.h
s_gqchh.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqchh.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqchh.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqchh.o: /usr/include/sys/_types/_int16_t.h
s_gqchh.o: /usr/include/sys/_types/_int32_t.h
s_gqchh.o: /usr/include/sys/_types/_int64_t.h
s_gqchh.o: /usr/include/sys/_types/_u_int8_t.h
s_gqchh.o: /usr/include/sys/_types/_u_int16_t.h
s_gqchh.o: /usr/include/sys/_types/_u_int32_t.h
s_gqchh.o: /usr/include/sys/_types/_u_int64_t.h
s_gqchh.o: /usr/include/sys/_types/_intptr_t.h
s_gqchh.o: /usr/include/sys/_types/_uintptr_t.h
s_gqchh.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqchh.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqchh.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqchh.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqchh.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqchh.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqchh.o: /usr/include/libkern/_OSByteOrder.h
s_gqchh.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqchh.o: /usr/include/sys/_types/_u_char.h
s_gqchh.o: /usr/include/sys/_types/_u_short.h
s_gqchh.o: /usr/include/sys/_types/_u_int.h
s_gqchh.o: /usr/include/sys/_types/_caddr_t.h
s_gqchh.o: /usr/include/sys/_types/_dev_t.h
s_gqchh.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqchh.o: /usr/include/sys/_types/_blksize_t.h
s_gqchh.o: /usr/include/sys/_types/_gid_t.h
s_gqchh.o: /usr/include/sys/_types/_in_addr_t.h
s_gqchh.o: /usr/include/sys/_types/_in_port_t.h
s_gqchh.o: /usr/include/sys/_types/_ino_t.h
s_gqchh.o: /usr/include/sys/_types/_ino64_t.h
s_gqchh.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gqchh.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gqchh.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gqchh.o: /usr/include/sys/_types/_clock_t.h
s_gqchh.o: /usr/include/sys/_types/_time_t.h
s_gqchh.o: /usr/include/sys/_types/_useconds_t.h
s_gqchh.o: /usr/include/sys/_types/_suseconds_t.h
s_gqchh.o: /usr/include/sys/_types/_rsize_t.h
s_gqchh.o: /usr/include/sys/_types/_errno_t.h
s_gqchh.o: /usr/include/sys/_types/_fd_def.h
s_gqchh.o: /usr/include/sys/_types/_fd_setsize.h
s_gqchh.o: /usr/include/sys/_types/_fd_set.h
s_gqchh.o: /usr/include/sys/_types/_fd_clr.h
s_gqchh.o: /usr/include/sys/_types/_fd_zero.h
s_gqchh.o: /usr/include/sys/_types/_fd_isset.h
s_gqchh.o: /usr/include/sys/_types/_fd_copy.h
s_gqchh.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqchh.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqchh.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqchh.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqchh.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqchh.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqchh.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqchh.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqchh.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqchh.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqchh.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqchh.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqchh.o: ../../../.././include/ncarg/c.h
s_gqchsp.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqchsp.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqchsp.o: /usr/include/sys/_posix_availability.h
s_gqchsp.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gqchsp.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gqchsp.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gqchsp.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqchsp.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqchsp.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqchsp.o: /usr/include/sys/_types/_int16_t.h
s_gqchsp.o: /usr/include/sys/_types/_int32_t.h
s_gqchsp.o: /usr/include/sys/_types/_int64_t.h
s_gqchsp.o: /usr/include/sys/_types/_u_int8_t.h
s_gqchsp.o: /usr/include/sys/_types/_u_int16_t.h
s_gqchsp.o: /usr/include/sys/_types/_u_int32_t.h
s_gqchsp.o: /usr/include/sys/_types/_u_int64_t.h
s_gqchsp.o: /usr/include/sys/_types/_intptr_t.h
s_gqchsp.o: /usr/include/sys/_types/_uintptr_t.h
s_gqchsp.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqchsp.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqchsp.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqchsp.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqchsp.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqchsp.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqchsp.o: /usr/include/libkern/_OSByteOrder.h
s_gqchsp.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqchsp.o: /usr/include/sys/_types/_u_char.h
s_gqchsp.o: /usr/include/sys/_types/_u_short.h
s_gqchsp.o: /usr/include/sys/_types/_u_int.h
s_gqchsp.o: /usr/include/sys/_types/_caddr_t.h
s_gqchsp.o: /usr/include/sys/_types/_dev_t.h
s_gqchsp.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqchsp.o: /usr/include/sys/_types/_blksize_t.h
s_gqchsp.o: /usr/include/sys/_types/_gid_t.h
s_gqchsp.o: /usr/include/sys/_types/_in_addr_t.h
s_gqchsp.o: /usr/include/sys/_types/_in_port_t.h
s_gqchsp.o: /usr/include/sys/_types/_ino_t.h
s_gqchsp.o: /usr/include/sys/_types/_ino64_t.h
s_gqchsp.o: /usr/include/sys/_types/_key_t.h
s_gqchsp.o: /usr/include/sys/_types/_mode_t.h
s_gqchsp.o: /usr/include/sys/_types/_nlink_t.h
s_gqchsp.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gqchsp.o: /usr/include/sys/_types/_uid_t.h
s_gqchsp.o: /usr/include/sys/_types/_clock_t.h
s_gqchsp.o: /usr/include/sys/_types/_time_t.h
s_gqchsp.o: /usr/include/sys/_types/_useconds_t.h
s_gqchsp.o: /usr/include/sys/_types/_suseconds_t.h
s_gqchsp.o: /usr/include/sys/_types/_rsize_t.h
s_gqchsp.o: /usr/include/sys/_types/_errno_t.h
s_gqchsp.o: /usr/include/sys/_types/_fd_def.h
s_gqchsp.o: /usr/include/sys/_types/_fd_setsize.h
s_gqchsp.o: /usr/include/sys/_types/_fd_set.h
s_gqchsp.o: /usr/include/sys/_types/_fd_clr.h
s_gqchsp.o: /usr/include/sys/_types/_fd_zero.h
s_gqchsp.o: /usr/include/sys/_types/_fd_isset.h
s_gqchsp.o: /usr/include/sys/_types/_fd_copy.h
s_gqchsp.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqchsp.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqchsp.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqchsp.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqchsp.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqchsp.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqchsp.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqchsp.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqchsp.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqchsp.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqchsp.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqchsp.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqchsp.o: ../../../.././include/ncarg/c.h
s_gqchup.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqchup.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqchup.o: /usr/include/sys/_posix_availability.h
s_gqchup.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gqchup.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gqchup.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gqchup.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqchup.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqchup.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqchup.o: /usr/include/sys/_types/_int16_t.h
s_gqchup.o: /usr/include/sys/_types/_int32_t.h
s_gqchup.o: /usr/include/sys/_types/_int64_t.h
s_gqchup.o: /usr/include/sys/_types/_u_int8_t.h
s_gqchup.o: /usr/include/sys/_types/_u_int16_t.h
s_gqchup.o: /usr/include/sys/_types/_u_int32_t.h
s_gqchup.o: /usr/include/sys/_types/_u_int64_t.h
s_gqchup.o: /usr/include/sys/_types/_intptr_t.h
s_gqchup.o: /usr/include/sys/_types/_uintptr_t.h
s_gqchup.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqchup.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqchup.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqchup.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqchup.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqchup.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqchup.o: /usr/include/libkern/_OSByteOrder.h
s_gqchup.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqchup.o: /usr/include/sys/_types/_u_char.h
s_gqchup.o: /usr/include/sys/_types/_u_short.h
s_gqchup.o: /usr/include/sys/_types/_u_int.h
s_gqchup.o: /usr/include/sys/_types/_caddr_t.h
s_gqchup.o: /usr/include/sys/_types/_dev_t.h
s_gqchup.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqchup.o: /usr/include/sys/_types/_blksize_t.h
s_gqchup.o: /usr/include/sys/_types/_gid_t.h
s_gqchup.o: /usr/include/sys/_types/_in_addr_t.h
s_gqchup.o: /usr/include/sys/_types/_in_port_t.h
s_gqchup.o: /usr/include/sys/_types/_ino_t.h
s_gqchup.o: /usr/include/sys/_types/_ino64_t.h
s_gqchup.o: /usr/include/sys/_types/_key_t.h
s_gqchup.o: /usr/include/sys/_types/_mode_t.h
s_gqchup.o: /usr/include/sys/_types/_nlink_t.h
s_gqchup.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gqchup.o: /usr/include/sys/_types/_uid_t.h
s_gqchup.o: /usr/include/sys/_types/_clock_t.h
s_gqchup.o: /usr/include/sys/_types/_time_t.h
s_gqchup.o: /usr/include/sys/_types/_useconds_t.h
s_gqchup.o: /usr/include/sys/_types/_suseconds_t.h
s_gqchup.o: /usr/include/sys/_types/_rsize_t.h
s_gqchup.o: /usr/include/sys/_types/_errno_t.h
s_gqchup.o: /usr/include/sys/_types/_fd_def.h
s_gqchup.o: /usr/include/sys/_types/_fd_setsize.h
s_gqchup.o: /usr/include/sys/_types/_fd_set.h
s_gqchup.o: /usr/include/sys/_types/_fd_clr.h
s_gqchup.o: /usr/include/sys/_types/_fd_zero.h
s_gqchup.o: /usr/include/sys/_types/_fd_isset.h
s_gqchup.o: /usr/include/sys/_types/_fd_copy.h
s_gqchup.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqchup.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqchup.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqchup.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqchup.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqchup.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqchup.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqchup.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqchup.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqchup.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqchup.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqchup.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqchup.o: ../../../.././include/ncarg/c.h
s_gqchw.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqchw.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqchw.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gqchw.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gqchw.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gqchw.o: /usr/include/i386/_types.h
s_gqchw.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqchw.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqchw.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqchw.o: /usr/include/sys/_types/_int16_t.h
s_gqchw.o: /usr/include/sys/_types/_int32_t.h
s_gqchw.o: /usr/include/sys/_types/_int64_t.h
s_gqchw.o: /usr/include/sys/_types/_u_int8_t.h
s_gqchw.o: /usr/include/sys/_types/_u_int16_t.h
s_gqchw.o: /usr/include/sys/_types/_u_int32_t.h
s_gqchw.o: /usr/include/sys/_types/_u_int64_t.h
s_gqchw.o: /usr/include/sys/_types/_intptr_t.h
s_gqchw.o: /usr/include/sys/_types/_uintptr_t.h
s_gqchw.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqchw.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqchw.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqchw.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqchw.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqchw.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqchw.o: /usr/include/libkern/_OSByteOrder.h
s_gqchw.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqchw.o: /usr/include/sys/_types/_u_char.h
s_gqchw.o: /usr/include/sys/_types/_u_short.h
s_gqchw.o: /usr/include/sys/_types/_u_int.h
s_gqchw.o: /usr/include/sys/_types/_caddr_t.h
s_gqchw.o: /usr/include/sys/_types/_dev_t.h
s_gqchw.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqchw.o: /usr/include/sys/_types/_blksize_t.h
s_gqchw.o: /usr/include/sys/_types/_gid_t.h
s_gqchw.o: /usr/include/sys/_types/_in_addr_t.h
s_gqchw.o: /usr/include/sys/_types/_in_port_t.h
s_gqchw.o: /usr/include/sys/_types/_ino_t.h
s_gqchw.o: /usr/include/sys/_types/_ino64_t.h
s_gqchw.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gqchw.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gqchw.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gqchw.o: /usr/include/sys/_types/_clock_t.h
s_gqchw.o: /usr/include/sys/_types/_time_t.h
s_gqchw.o: /usr/include/sys/_types/_useconds_t.h
s_gqchw.o: /usr/include/sys/_types/_suseconds_t.h
s_gqchw.o: /usr/include/sys/_types/_rsize_t.h
s_gqchw.o: /usr/include/sys/_types/_errno_t.h
s_gqchw.o: /usr/include/sys/_types/_fd_def.h
s_gqchw.o: /usr/include/sys/_types/_fd_setsize.h
s_gqchw.o: /usr/include/sys/_types/_fd_set.h
s_gqchw.o: /usr/include/sys/_types/_fd_clr.h
s_gqchw.o: /usr/include/sys/_types/_fd_zero.h
s_gqchw.o: /usr/include/sys/_types/_fd_isset.h
s_gqchw.o: /usr/include/sys/_types/_fd_copy.h
s_gqchw.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqchw.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqchw.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqchw.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqchw.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqchw.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqchw.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqchw.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqchw.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqchw.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqchw.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqchw.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqchw.o: ../../../.././include/ncarg/c.h
s_gqchxp.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqchxp.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqchxp.o: /usr/include/sys/_posix_availability.h
s_gqchxp.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gqchxp.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gqchxp.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gqchxp.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqchxp.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqchxp.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqchxp.o: /usr/include/sys/_types/_int16_t.h
s_gqchxp.o: /usr/include/sys/_types/_int32_t.h
s_gqchxp.o: /usr/include/sys/_types/_int64_t.h
s_gqchxp.o: /usr/include/sys/_types/_u_int8_t.h
s_gqchxp.o: /usr/include/sys/_types/_u_int16_t.h
s_gqchxp.o: /usr/include/sys/_types/_u_int32_t.h
s_gqchxp.o: /usr/include/sys/_types/_u_int64_t.h
s_gqchxp.o: /usr/include/sys/_types/_intptr_t.h
s_gqchxp.o: /usr/include/sys/_types/_uintptr_t.h
s_gqchxp.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqchxp.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqchxp.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqchxp.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqchxp.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqchxp.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqchxp.o: /usr/include/libkern/_OSByteOrder.h
s_gqchxp.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqchxp.o: /usr/include/sys/_types/_u_char.h
s_gqchxp.o: /usr/include/sys/_types/_u_short.h
s_gqchxp.o: /usr/include/sys/_types/_u_int.h
s_gqchxp.o: /usr/include/sys/_types/_caddr_t.h
s_gqchxp.o: /usr/include/sys/_types/_dev_t.h
s_gqchxp.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqchxp.o: /usr/include/sys/_types/_blksize_t.h
s_gqchxp.o: /usr/include/sys/_types/_gid_t.h
s_gqchxp.o: /usr/include/sys/_types/_in_addr_t.h
s_gqchxp.o: /usr/include/sys/_types/_in_port_t.h
s_gqchxp.o: /usr/include/sys/_types/_ino_t.h
s_gqchxp.o: /usr/include/sys/_types/_ino64_t.h
s_gqchxp.o: /usr/include/sys/_types/_key_t.h
s_gqchxp.o: /usr/include/sys/_types/_mode_t.h
s_gqchxp.o: /usr/include/sys/_types/_nlink_t.h
s_gqchxp.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gqchxp.o: /usr/include/sys/_types/_uid_t.h
s_gqchxp.o: /usr/include/sys/_types/_clock_t.h
s_gqchxp.o: /usr/include/sys/_types/_time_t.h
s_gqchxp.o: /usr/include/sys/_types/_useconds_t.h
s_gqchxp.o: /usr/include/sys/_types/_suseconds_t.h
s_gqchxp.o: /usr/include/sys/_types/_rsize_t.h
s_gqchxp.o: /usr/include/sys/_types/_errno_t.h
s_gqchxp.o: /usr/include/sys/_types/_fd_def.h
s_gqchxp.o: /usr/include/sys/_types/_fd_setsize.h
s_gqchxp.o: /usr/include/sys/_types/_fd_set.h
s_gqchxp.o: /usr/include/sys/_types/_fd_clr.h
s_gqchxp.o: /usr/include/sys/_types/_fd_zero.h
s_gqchxp.o: /usr/include/sys/_types/_fd_isset.h
s_gqchxp.o: /usr/include/sys/_types/_fd_copy.h
s_gqchxp.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqchxp.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqchxp.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqchxp.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqchxp.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqchxp.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqchxp.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqchxp.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqchxp.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqchxp.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqchxp.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqchxp.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqchxp.o: ../../../.././include/ncarg/c.h
s_gqclip.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqclip.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqclip.o: /usr/include/sys/_posix_availability.h
s_gqclip.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gqclip.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gqclip.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gqclip.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqclip.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqclip.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqclip.o: /usr/include/sys/_types/_int16_t.h
s_gqclip.o: /usr/include/sys/_types/_int32_t.h
s_gqclip.o: /usr/include/sys/_types/_int64_t.h
s_gqclip.o: /usr/include/sys/_types/_u_int8_t.h
s_gqclip.o: /usr/include/sys/_types/_u_int16_t.h
s_gqclip.o: /usr/include/sys/_types/_u_int32_t.h
s_gqclip.o: /usr/include/sys/_types/_u_int64_t.h
s_gqclip.o: /usr/include/sys/_types/_intptr_t.h
s_gqclip.o: /usr/include/sys/_types/_uintptr_t.h
s_gqclip.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqclip.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqclip.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqclip.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqclip.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqclip.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqclip.o: /usr/include/libkern/_OSByteOrder.h
s_gqclip.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqclip.o: /usr/include/sys/_types/_u_char.h
s_gqclip.o: /usr/include/sys/_types/_u_short.h
s_gqclip.o: /usr/include/sys/_types/_u_int.h
s_gqclip.o: /usr/include/sys/_types/_caddr_t.h
s_gqclip.o: /usr/include/sys/_types/_dev_t.h
s_gqclip.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqclip.o: /usr/include/sys/_types/_blksize_t.h
s_gqclip.o: /usr/include/sys/_types/_gid_t.h
s_gqclip.o: /usr/include/sys/_types/_in_addr_t.h
s_gqclip.o: /usr/include/sys/_types/_in_port_t.h
s_gqclip.o: /usr/include/sys/_types/_ino_t.h
s_gqclip.o: /usr/include/sys/_types/_ino64_t.h
s_gqclip.o: /usr/include/sys/_types/_key_t.h
s_gqclip.o: /usr/include/sys/_types/_mode_t.h
s_gqclip.o: /usr/include/sys/_types/_nlink_t.h
s_gqclip.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gqclip.o: /usr/include/sys/_types/_uid_t.h
s_gqclip.o: /usr/include/sys/_types/_clock_t.h
s_gqclip.o: /usr/include/sys/_types/_time_t.h
s_gqclip.o: /usr/include/sys/_types/_useconds_t.h
s_gqclip.o: /usr/include/sys/_types/_suseconds_t.h
s_gqclip.o: /usr/include/sys/_types/_rsize_t.h
s_gqclip.o: /usr/include/sys/_types/_errno_t.h
s_gqclip.o: /usr/include/sys/_types/_fd_def.h
s_gqclip.o: /usr/include/sys/_types/_fd_setsize.h
s_gqclip.o: /usr/include/sys/_types/_fd_set.h
s_gqclip.o: /usr/include/sys/_types/_fd_clr.h
s_gqclip.o: /usr/include/sys/_types/_fd_zero.h
s_gqclip.o: /usr/include/sys/_types/_fd_isset.h
s_gqclip.o: /usr/include/sys/_types/_fd_copy.h
s_gqclip.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqclip.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqclip.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqclip.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqclip.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqclip.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqclip.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqclip.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqclip.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqclip.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqclip.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqclip.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqclip.o: ../../../.././include/ncarg/c.h
s_gqcntn.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqcntn.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqcntn.o: /usr/include/sys/_posix_availability.h
s_gqcntn.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gqcntn.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gqcntn.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gqcntn.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqcntn.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqcntn.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqcntn.o: /usr/include/sys/_types/_int16_t.h
s_gqcntn.o: /usr/include/sys/_types/_int32_t.h
s_gqcntn.o: /usr/include/sys/_types/_int64_t.h
s_gqcntn.o: /usr/include/sys/_types/_u_int8_t.h
s_gqcntn.o: /usr/include/sys/_types/_u_int16_t.h
s_gqcntn.o: /usr/include/sys/_types/_u_int32_t.h
s_gqcntn.o: /usr/include/sys/_types/_u_int64_t.h
s_gqcntn.o: /usr/include/sys/_types/_intptr_t.h
s_gqcntn.o: /usr/include/sys/_types/_uintptr_t.h
s_gqcntn.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqcntn.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqcntn.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqcntn.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqcntn.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqcntn.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqcntn.o: /usr/include/libkern/_OSByteOrder.h
s_gqcntn.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqcntn.o: /usr/include/sys/_types/_u_char.h
s_gqcntn.o: /usr/include/sys/_types/_u_short.h
s_gqcntn.o: /usr/include/sys/_types/_u_int.h
s_gqcntn.o: /usr/include/sys/_types/_caddr_t.h
s_gqcntn.o: /usr/include/sys/_types/_dev_t.h
s_gqcntn.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqcntn.o: /usr/include/sys/_types/_blksize_t.h
s_gqcntn.o: /usr/include/sys/_types/_gid_t.h
s_gqcntn.o: /usr/include/sys/_types/_in_addr_t.h
s_gqcntn.o: /usr/include/sys/_types/_in_port_t.h
s_gqcntn.o: /usr/include/sys/_types/_ino_t.h
s_gqcntn.o: /usr/include/sys/_types/_ino64_t.h
s_gqcntn.o: /usr/include/sys/_types/_key_t.h
s_gqcntn.o: /usr/include/sys/_types/_mode_t.h
s_gqcntn.o: /usr/include/sys/_types/_nlink_t.h
s_gqcntn.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gqcntn.o: /usr/include/sys/_types/_uid_t.h
s_gqcntn.o: /usr/include/sys/_types/_clock_t.h
s_gqcntn.o: /usr/include/sys/_types/_time_t.h
s_gqcntn.o: /usr/include/sys/_types/_useconds_t.h
s_gqcntn.o: /usr/include/sys/_types/_suseconds_t.h
s_gqcntn.o: /usr/include/sys/_types/_rsize_t.h
s_gqcntn.o: /usr/include/sys/_types/_errno_t.h
s_gqcntn.o: /usr/include/sys/_types/_fd_def.h
s_gqcntn.o: /usr/include/sys/_types/_fd_setsize.h
s_gqcntn.o: /usr/include/sys/_types/_fd_set.h
s_gqcntn.o: /usr/include/sys/_types/_fd_clr.h
s_gqcntn.o: /usr/include/sys/_types/_fd_zero.h
s_gqcntn.o: /usr/include/sys/_types/_fd_isset.h
s_gqcntn.o: /usr/include/sys/_types/_fd_copy.h
s_gqcntn.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqcntn.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqcntn.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqcntn.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqcntn.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqcntn.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqcntn.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqcntn.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqcntn.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqcntn.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqcntn.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqcntn.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqcntn.o: ../../../.././include/ncarg/c.h
s_gqcr.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqcr.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqcr.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gqcr.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gqcr.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gqcr.o: /usr/include/i386/_types.h
s_gqcr.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqcr.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqcr.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqcr.o: /usr/include/sys/_types/_int16_t.h
s_gqcr.o: /usr/include/sys/_types/_int32_t.h
s_gqcr.o: /usr/include/sys/_types/_int64_t.h
s_gqcr.o: /usr/include/sys/_types/_u_int8_t.h
s_gqcr.o: /usr/include/sys/_types/_u_int16_t.h
s_gqcr.o: /usr/include/sys/_types/_u_int32_t.h
s_gqcr.o: /usr/include/sys/_types/_u_int64_t.h
s_gqcr.o: /usr/include/sys/_types/_intptr_t.h
s_gqcr.o: /usr/include/sys/_types/_uintptr_t.h
s_gqcr.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqcr.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqcr.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqcr.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqcr.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqcr.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqcr.o: /usr/include/libkern/_OSByteOrder.h
s_gqcr.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqcr.o: /usr/include/sys/_types/_u_char.h
s_gqcr.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
s_gqcr.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
s_gqcr.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqcr.o: /usr/include/sys/_types/_blksize_t.h
s_gqcr.o: /usr/include/sys/_types/_gid_t.h
s_gqcr.o: /usr/include/sys/_types/_in_addr_t.h
s_gqcr.o: /usr/include/sys/_types/_in_port_t.h
s_gqcr.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
s_gqcr.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gqcr.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gqcr.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gqcr.o: /usr/include/sys/_types/_clock_t.h
s_gqcr.o: /usr/include/sys/_types/_time_t.h
s_gqcr.o: /usr/include/sys/_types/_useconds_t.h
s_gqcr.o: /usr/include/sys/_types/_suseconds_t.h
s_gqcr.o: /usr/include/sys/_types/_rsize_t.h
s_gqcr.o: /usr/include/sys/_types/_errno_t.h
s_gqcr.o: /usr/include/sys/_types/_fd_def.h
s_gqcr.o: /usr/include/sys/_types/_fd_setsize.h
s_gqcr.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
s_gqcr.o: /usr/include/sys/_types/_fd_zero.h
s_gqcr.o: /usr/include/sys/_types/_fd_isset.h
s_gqcr.o: /usr/include/sys/_types/_fd_copy.h
s_gqcr.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqcr.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqcr.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqcr.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqcr.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqcr.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqcr.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqcr.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqcr.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqcr.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqcr.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqcr.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqcr.o: ../../../.././include/ncarg/c.h
s_gqdsp.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqdsp.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqdsp.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gqdsp.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gqdsp.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gqdsp.o: /usr/include/i386/_types.h
s_gqdsp.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqdsp.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqdsp.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqdsp.o: /usr/include/sys/_types/_int16_t.h
s_gqdsp.o: /usr/include/sys/_types/_int32_t.h
s_gqdsp.o: /usr/include/sys/_types/_int64_t.h
s_gqdsp.o: /usr/include/sys/_types/_u_int8_t.h
s_gqdsp.o: /usr/include/sys/_types/_u_int16_t.h
s_gqdsp.o: /usr/include/sys/_types/_u_int32_t.h
s_gqdsp.o: /usr/include/sys/_types/_u_int64_t.h
s_gqdsp.o: /usr/include/sys/_types/_intptr_t.h
s_gqdsp.o: /usr/include/sys/_types/_uintptr_t.h
s_gqdsp.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqdsp.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqdsp.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqdsp.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqdsp.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqdsp.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqdsp.o: /usr/include/libkern/_OSByteOrder.h
s_gqdsp.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqdsp.o: /usr/include/sys/_types/_u_char.h
s_gqdsp.o: /usr/include/sys/_types/_u_short.h
s_gqdsp.o: /usr/include/sys/_types/_u_int.h
s_gqdsp.o: /usr/include/sys/_types/_caddr_t.h
s_gqdsp.o: /usr/include/sys/_types/_dev_t.h
s_gqdsp.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqdsp.o: /usr/include/sys/_types/_blksize_t.h
s_gqdsp.o: /usr/include/sys/_types/_gid_t.h
s_gqdsp.o: /usr/include/sys/_types/_in_addr_t.h
s_gqdsp.o: /usr/include/sys/_types/_in_port_t.h
s_gqdsp.o: /usr/include/sys/_types/_ino_t.h
s_gqdsp.o: /usr/include/sys/_types/_ino64_t.h
s_gqdsp.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gqdsp.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gqdsp.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gqdsp.o: /usr/include/sys/_types/_clock_t.h
s_gqdsp.o: /usr/include/sys/_types/_time_t.h
s_gqdsp.o: /usr/include/sys/_types/_useconds_t.h
s_gqdsp.o: /usr/include/sys/_types/_suseconds_t.h
s_gqdsp.o: /usr/include/sys/_types/_rsize_t.h
s_gqdsp.o: /usr/include/sys/_types/_errno_t.h
s_gqdsp.o: /usr/include/sys/_types/_fd_def.h
s_gqdsp.o: /usr/include/sys/_types/_fd_setsize.h
s_gqdsp.o: /usr/include/sys/_types/_fd_set.h
s_gqdsp.o: /usr/include/sys/_types/_fd_clr.h
s_gqdsp.o: /usr/include/sys/_types/_fd_zero.h
s_gqdsp.o: /usr/include/sys/_types/_fd_isset.h
s_gqdsp.o: /usr/include/sys/_types/_fd_copy.h
s_gqdsp.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqdsp.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqdsp.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqdsp.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqdsp.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqdsp.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqdsp.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqdsp.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqdsp.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqdsp.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqdsp.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqdsp.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqdsp.o: ../../../.././include/ncarg/c.h
s_gqeci.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqeci.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqeci.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gqeci.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gqeci.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gqeci.o: /usr/include/i386/_types.h
s_gqeci.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqeci.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqeci.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqeci.o: /usr/include/sys/_types/_int16_t.h
s_gqeci.o: /usr/include/sys/_types/_int32_t.h
s_gqeci.o: /usr/include/sys/_types/_int64_t.h
s_gqeci.o: /usr/include/sys/_types/_u_int8_t.h
s_gqeci.o: /usr/include/sys/_types/_u_int16_t.h
s_gqeci.o: /usr/include/sys/_types/_u_int32_t.h
s_gqeci.o: /usr/include/sys/_types/_u_int64_t.h
s_gqeci.o: /usr/include/sys/_types/_intptr_t.h
s_gqeci.o: /usr/include/sys/_types/_uintptr_t.h
s_gqeci.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqeci.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqeci.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqeci.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqeci.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqeci.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqeci.o: /usr/include/libkern/_OSByteOrder.h
s_gqeci.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqeci.o: /usr/include/sys/_types/_u_char.h
s_gqeci.o: /usr/include/sys/_types/_u_short.h
s_gqeci.o: /usr/include/sys/_types/_u_int.h
s_gqeci.o: /usr/include/sys/_types/_caddr_t.h
s_gqeci.o: /usr/include/sys/_types/_dev_t.h
s_gqeci.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqeci.o: /usr/include/sys/_types/_blksize_t.h
s_gqeci.o: /usr/include/sys/_types/_gid_t.h
s_gqeci.o: /usr/include/sys/_types/_in_addr_t.h
s_gqeci.o: /usr/include/sys/_types/_in_port_t.h
s_gqeci.o: /usr/include/sys/_types/_ino_t.h
s_gqeci.o: /usr/include/sys/_types/_ino64_t.h
s_gqeci.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gqeci.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gqeci.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gqeci.o: /usr/include/sys/_types/_clock_t.h
s_gqeci.o: /usr/include/sys/_types/_time_t.h
s_gqeci.o: /usr/include/sys/_types/_useconds_t.h
s_gqeci.o: /usr/include/sys/_types/_suseconds_t.h
s_gqeci.o: /usr/include/sys/_types/_rsize_t.h
s_gqeci.o: /usr/include/sys/_types/_errno_t.h
s_gqeci.o: /usr/include/sys/_types/_fd_def.h
s_gqeci.o: /usr/include/sys/_types/_fd_setsize.h
s_gqeci.o: /usr/include/sys/_types/_fd_set.h
s_gqeci.o: /usr/include/sys/_types/_fd_clr.h
s_gqeci.o: /usr/include/sys/_types/_fd_zero.h
s_gqeci.o: /usr/include/sys/_types/_fd_isset.h
s_gqeci.o: /usr/include/sys/_types/_fd_copy.h
s_gqeci.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqeci.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqeci.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqeci.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqeci.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqeci.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqeci.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqeci.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqeci.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqeci.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqeci.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqeci.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqeci.o: ../../../.././include/ncarg/c.h
s_gqegdp.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqegdp.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqegdp.o: /usr/include/sys/_posix_availability.h
s_gqegdp.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gqegdp.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gqegdp.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gqegdp.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqegdp.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqegdp.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqegdp.o: /usr/include/sys/_types/_int16_t.h
s_gqegdp.o: /usr/include/sys/_types/_int32_t.h
s_gqegdp.o: /usr/include/sys/_types/_int64_t.h
s_gqegdp.o: /usr/include/sys/_types/_u_int8_t.h
s_gqegdp.o: /usr/include/sys/_types/_u_int16_t.h
s_gqegdp.o: /usr/include/sys/_types/_u_int32_t.h
s_gqegdp.o: /usr/include/sys/_types/_u_int64_t.h
s_gqegdp.o: /usr/include/sys/_types/_intptr_t.h
s_gqegdp.o: /usr/include/sys/_types/_uintptr_t.h
s_gqegdp.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqegdp.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqegdp.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqegdp.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqegdp.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqegdp.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqegdp.o: /usr/include/libkern/_OSByteOrder.h
s_gqegdp.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqegdp.o: /usr/include/sys/_types/_u_char.h
s_gqegdp.o: /usr/include/sys/_types/_u_short.h
s_gqegdp.o: /usr/include/sys/_types/_u_int.h
s_gqegdp.o: /usr/include/sys/_types/_caddr_t.h
s_gqegdp.o: /usr/include/sys/_types/_dev_t.h
s_gqegdp.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqegdp.o: /usr/include/sys/_types/_blksize_t.h
s_gqegdp.o: /usr/include/sys/_types/_gid_t.h
s_gqegdp.o: /usr/include/sys/_types/_in_addr_t.h
s_gqegdp.o: /usr/include/sys/_types/_in_port_t.h
s_gqegdp.o: /usr/include/sys/_types/_ino_t.h
s_gqegdp.o: /usr/include/sys/_types/_ino64_t.h
s_gqegdp.o: /usr/include/sys/_types/_key_t.h
s_gqegdp.o: /usr/include/sys/_types/_mode_t.h
s_gqegdp.o: /usr/include/sys/_types/_nlink_t.h
s_gqegdp.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gqegdp.o: /usr/include/sys/_types/_uid_t.h
s_gqegdp.o: /usr/include/sys/_types/_clock_t.h
s_gqegdp.o: /usr/include/sys/_types/_time_t.h
s_gqegdp.o: /usr/include/sys/_types/_useconds_t.h
s_gqegdp.o: /usr/include/sys/_types/_suseconds_t.h
s_gqegdp.o: /usr/include/sys/_types/_rsize_t.h
s_gqegdp.o: /usr/include/sys/_types/_errno_t.h
s_gqegdp.o: /usr/include/sys/_types/_fd_def.h
s_gqegdp.o: /usr/include/sys/_types/_fd_setsize.h
s_gqegdp.o: /usr/include/sys/_types/_fd_set.h
s_gqegdp.o: /usr/include/sys/_types/_fd_clr.h
s_gqegdp.o: /usr/include/sys/_types/_fd_zero.h
s_gqegdp.o: /usr/include/sys/_types/_fd_isset.h
s_gqegdp.o: /usr/include/sys/_types/_fd_copy.h
s_gqegdp.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqegdp.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqegdp.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqegdp.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqegdp.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqegdp.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqegdp.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqegdp.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqegdp.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqegdp.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqegdp.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqegdp.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqegdp.o: ../../../.././include/ncarg/c.h
s_gqentn.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqentn.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqentn.o: /usr/include/sys/_posix_availability.h
s_gqentn.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gqentn.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gqentn.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gqentn.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqentn.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqentn.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqentn.o: /usr/include/sys/_types/_int16_t.h
s_gqentn.o: /usr/include/sys/_types/_int32_t.h
s_gqentn.o: /usr/include/sys/_types/_int64_t.h
s_gqentn.o: /usr/include/sys/_types/_u_int8_t.h
s_gqentn.o: /usr/include/sys/_types/_u_int16_t.h
s_gqentn.o: /usr/include/sys/_types/_u_int32_t.h
s_gqentn.o: /usr/include/sys/_types/_u_int64_t.h
s_gqentn.o: /usr/include/sys/_types/_intptr_t.h
s_gqentn.o: /usr/include/sys/_types/_uintptr_t.h
s_gqentn.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqentn.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqentn.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqentn.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqentn.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqentn.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqentn.o: /usr/include/libkern/_OSByteOrder.h
s_gqentn.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqentn.o: /usr/include/sys/_types/_u_char.h
s_gqentn.o: /usr/include/sys/_types/_u_short.h
s_gqentn.o: /usr/include/sys/_types/_u_int.h
s_gqentn.o: /usr/include/sys/_types/_caddr_t.h
s_gqentn.o: /usr/include/sys/_types/_dev_t.h
s_gqentn.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqentn.o: /usr/include/sys/_types/_blksize_t.h
s_gqentn.o: /usr/include/sys/_types/_gid_t.h
s_gqentn.o: /usr/include/sys/_types/_in_addr_t.h
s_gqentn.o: /usr/include/sys/_types/_in_port_t.h
s_gqentn.o: /usr/include/sys/_types/_ino_t.h
s_gqentn.o: /usr/include/sys/_types/_ino64_t.h
s_gqentn.o: /usr/include/sys/_types/_key_t.h
s_gqentn.o: /usr/include/sys/_types/_mode_t.h
s_gqentn.o: /usr/include/sys/_types/_nlink_t.h
s_gqentn.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gqentn.o: /usr/include/sys/_types/_uid_t.h
s_gqentn.o: /usr/include/sys/_types/_clock_t.h
s_gqentn.o: /usr/include/sys/_types/_time_t.h
s_gqentn.o: /usr/include/sys/_types/_useconds_t.h
s_gqentn.o: /usr/include/sys/_types/_suseconds_t.h
s_gqentn.o: /usr/include/sys/_types/_rsize_t.h
s_gqentn.o: /usr/include/sys/_types/_errno_t.h
s_gqentn.o: /usr/include/sys/_types/_fd_def.h
s_gqentn.o: /usr/include/sys/_types/_fd_setsize.h
s_gqentn.o: /usr/include/sys/_types/_fd_set.h
s_gqentn.o: /usr/include/sys/_types/_fd_clr.h
s_gqentn.o: /usr/include/sys/_types/_fd_zero.h
s_gqentn.o: /usr/include/sys/_types/_fd_isset.h
s_gqentn.o: /usr/include/sys/_types/_fd_copy.h
s_gqentn.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqentn.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqentn.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqentn.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqentn.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqentn.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqentn.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqentn.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqentn.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqentn.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqentn.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqentn.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqentn.o: ../../../.././include/ncarg/c.h
s_gqewk.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqewk.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqewk.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gqewk.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gqewk.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gqewk.o: /usr/include/i386/_types.h
s_gqewk.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqewk.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqewk.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqewk.o: /usr/include/sys/_types/_int16_t.h
s_gqewk.o: /usr/include/sys/_types/_int32_t.h
s_gqewk.o: /usr/include/sys/_types/_int64_t.h
s_gqewk.o: /usr/include/sys/_types/_u_int8_t.h
s_gqewk.o: /usr/include/sys/_types/_u_int16_t.h
s_gqewk.o: /usr/include/sys/_types/_u_int32_t.h
s_gqewk.o: /usr/include/sys/_types/_u_int64_t.h
s_gqewk.o: /usr/include/sys/_types/_intptr_t.h
s_gqewk.o: /usr/include/sys/_types/_uintptr_t.h
s_gqewk.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqewk.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqewk.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqewk.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqewk.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqewk.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqewk.o: /usr/include/libkern/_OSByteOrder.h
s_gqewk.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqewk.o: /usr/include/sys/_types/_u_char.h
s_gqewk.o: /usr/include/sys/_types/_u_short.h
s_gqewk.o: /usr/include/sys/_types/_u_int.h
s_gqewk.o: /usr/include/sys/_types/_caddr_t.h
s_gqewk.o: /usr/include/sys/_types/_dev_t.h
s_gqewk.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqewk.o: /usr/include/sys/_types/_blksize_t.h
s_gqewk.o: /usr/include/sys/_types/_gid_t.h
s_gqewk.o: /usr/include/sys/_types/_in_addr_t.h
s_gqewk.o: /usr/include/sys/_types/_in_port_t.h
s_gqewk.o: /usr/include/sys/_types/_ino_t.h
s_gqewk.o: /usr/include/sys/_types/_ino64_t.h
s_gqewk.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gqewk.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gqewk.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gqewk.o: /usr/include/sys/_types/_clock_t.h
s_gqewk.o: /usr/include/sys/_types/_time_t.h
s_gqewk.o: /usr/include/sys/_types/_useconds_t.h
s_gqewk.o: /usr/include/sys/_types/_suseconds_t.h
s_gqewk.o: /usr/include/sys/_types/_rsize_t.h
s_gqewk.o: /usr/include/sys/_types/_errno_t.h
s_gqewk.o: /usr/include/sys/_types/_fd_def.h
s_gqewk.o: /usr/include/sys/_types/_fd_setsize.h
s_gqewk.o: /usr/include/sys/_types/_fd_set.h
s_gqewk.o: /usr/include/sys/_types/_fd_clr.h
s_gqewk.o: /usr/include/sys/_types/_fd_zero.h
s_gqewk.o: /usr/include/sys/_types/_fd_isset.h
s_gqewk.o: /usr/include/sys/_types/_fd_copy.h
s_gqewk.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqewk.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqewk.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqewk.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqewk.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqewk.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqewk.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqewk.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqewk.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqewk.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqewk.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqewk.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqewk.o: ../../../.././include/ncarg/c.h
s_gqfaci.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqfaci.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqfaci.o: /usr/include/sys/_posix_availability.h
s_gqfaci.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gqfaci.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gqfaci.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gqfaci.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqfaci.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqfaci.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqfaci.o: /usr/include/sys/_types/_int16_t.h
s_gqfaci.o: /usr/include/sys/_types/_int32_t.h
s_gqfaci.o: /usr/include/sys/_types/_int64_t.h
s_gqfaci.o: /usr/include/sys/_types/_u_int8_t.h
s_gqfaci.o: /usr/include/sys/_types/_u_int16_t.h
s_gqfaci.o: /usr/include/sys/_types/_u_int32_t.h
s_gqfaci.o: /usr/include/sys/_types/_u_int64_t.h
s_gqfaci.o: /usr/include/sys/_types/_intptr_t.h
s_gqfaci.o: /usr/include/sys/_types/_uintptr_t.h
s_gqfaci.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqfaci.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqfaci.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqfaci.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqfaci.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqfaci.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqfaci.o: /usr/include/libkern/_OSByteOrder.h
s_gqfaci.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqfaci.o: /usr/include/sys/_types/_u_char.h
s_gqfaci.o: /usr/include/sys/_types/_u_short.h
s_gqfaci.o: /usr/include/sys/_types/_u_int.h
s_gqfaci.o: /usr/include/sys/_types/_caddr_t.h
s_gqfaci.o: /usr/include/sys/_types/_dev_t.h
s_gqfaci.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqfaci.o: /usr/include/sys/_types/_blksize_t.h
s_gqfaci.o: /usr/include/sys/_types/_gid_t.h
s_gqfaci.o: /usr/include/sys/_types/_in_addr_t.h
s_gqfaci.o: /usr/include/sys/_types/_in_port_t.h
s_gqfaci.o: /usr/include/sys/_types/_ino_t.h
s_gqfaci.o: /usr/include/sys/_types/_ino64_t.h
s_gqfaci.o: /usr/include/sys/_types/_key_t.h
s_gqfaci.o: /usr/include/sys/_types/_mode_t.h
s_gqfaci.o: /usr/include/sys/_types/_nlink_t.h
s_gqfaci.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gqfaci.o: /usr/include/sys/_types/_uid_t.h
s_gqfaci.o: /usr/include/sys/_types/_clock_t.h
s_gqfaci.o: /usr/include/sys/_types/_time_t.h
s_gqfaci.o: /usr/include/sys/_types/_useconds_t.h
s_gqfaci.o: /usr/include/sys/_types/_suseconds_t.h
s_gqfaci.o: /usr/include/sys/_types/_rsize_t.h
s_gqfaci.o: /usr/include/sys/_types/_errno_t.h
s_gqfaci.o: /usr/include/sys/_types/_fd_def.h
s_gqfaci.o: /usr/include/sys/_types/_fd_setsize.h
s_gqfaci.o: /usr/include/sys/_types/_fd_set.h
s_gqfaci.o: /usr/include/sys/_types/_fd_clr.h
s_gqfaci.o: /usr/include/sys/_types/_fd_zero.h
s_gqfaci.o: /usr/include/sys/_types/_fd_isset.h
s_gqfaci.o: /usr/include/sys/_types/_fd_copy.h
s_gqfaci.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqfaci.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqfaci.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqfaci.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqfaci.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqfaci.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqfaci.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqfaci.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqfaci.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqfaci.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqfaci.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqfaci.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqfaci.o: ../../../.././include/ncarg/c.h
s_gqfaf.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqfaf.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqfaf.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gqfaf.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gqfaf.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gqfaf.o: /usr/include/i386/_types.h
s_gqfaf.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqfaf.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqfaf.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqfaf.o: /usr/include/sys/_types/_int16_t.h
s_gqfaf.o: /usr/include/sys/_types/_int32_t.h
s_gqfaf.o: /usr/include/sys/_types/_int64_t.h
s_gqfaf.o: /usr/include/sys/_types/_u_int8_t.h
s_gqfaf.o: /usr/include/sys/_types/_u_int16_t.h
s_gqfaf.o: /usr/include/sys/_types/_u_int32_t.h
s_gqfaf.o: /usr/include/sys/_types/_u_int64_t.h
s_gqfaf.o: /usr/include/sys/_types/_intptr_t.h
s_gqfaf.o: /usr/include/sys/_types/_uintptr_t.h
s_gqfaf.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqfaf.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqfaf.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqfaf.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqfaf.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqfaf.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqfaf.o: /usr/include/libkern/_OSByteOrder.h
s_gqfaf.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqfaf.o: /usr/include/sys/_types/_u_char.h
s_gqfaf.o: /usr/include/sys/_types/_u_short.h
s_gqfaf.o: /usr/include/sys/_types/_u_int.h
s_gqfaf.o: /usr/include/sys/_types/_caddr_t.h
s_gqfaf.o: /usr/include/sys/_types/_dev_t.h
s_gqfaf.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqfaf.o: /usr/include/sys/_types/_blksize_t.h
s_gqfaf.o: /usr/include/sys/_types/_gid_t.h
s_gqfaf.o: /usr/include/sys/_types/_in_addr_t.h
s_gqfaf.o: /usr/include/sys/_types/_in_port_t.h
s_gqfaf.o: /usr/include/sys/_types/_ino_t.h
s_gqfaf.o: /usr/include/sys/_types/_ino64_t.h
s_gqfaf.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gqfaf.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gqfaf.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gqfaf.o: /usr/include/sys/_types/_clock_t.h
s_gqfaf.o: /usr/include/sys/_types/_time_t.h
s_gqfaf.o: /usr/include/sys/_types/_useconds_t.h
s_gqfaf.o: /usr/include/sys/_types/_suseconds_t.h
s_gqfaf.o: /usr/include/sys/_types/_rsize_t.h
s_gqfaf.o: /usr/include/sys/_types/_errno_t.h
s_gqfaf.o: /usr/include/sys/_types/_fd_def.h
s_gqfaf.o: /usr/include/sys/_types/_fd_setsize.h
s_gqfaf.o: /usr/include/sys/_types/_fd_set.h
s_gqfaf.o: /usr/include/sys/_types/_fd_clr.h
s_gqfaf.o: /usr/include/sys/_types/_fd_zero.h
s_gqfaf.o: /usr/include/sys/_types/_fd_isset.h
s_gqfaf.o: /usr/include/sys/_types/_fd_copy.h
s_gqfaf.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqfaf.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqfaf.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqfaf.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqfaf.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqfaf.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqfaf.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqfaf.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqfaf.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqfaf.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqfaf.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqfaf.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqfaf.o: ../../../.././include/ncarg/c.h
s_gqfai.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqfai.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqfai.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gqfai.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gqfai.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gqfai.o: /usr/include/i386/_types.h
s_gqfai.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqfai.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqfai.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqfai.o: /usr/include/sys/_types/_int16_t.h
s_gqfai.o: /usr/include/sys/_types/_int32_t.h
s_gqfai.o: /usr/include/sys/_types/_int64_t.h
s_gqfai.o: /usr/include/sys/_types/_u_int8_t.h
s_gqfai.o: /usr/include/sys/_types/_u_int16_t.h
s_gqfai.o: /usr/include/sys/_types/_u_int32_t.h
s_gqfai.o: /usr/include/sys/_types/_u_int64_t.h
s_gqfai.o: /usr/include/sys/_types/_intptr_t.h
s_gqfai.o: /usr/include/sys/_types/_uintptr_t.h
s_gqfai.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqfai.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqfai.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqfai.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqfai.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqfai.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqfai.o: /usr/include/libkern/_OSByteOrder.h
s_gqfai.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqfai.o: /usr/include/sys/_types/_u_char.h
s_gqfai.o: /usr/include/sys/_types/_u_short.h
s_gqfai.o: /usr/include/sys/_types/_u_int.h
s_gqfai.o: /usr/include/sys/_types/_caddr_t.h
s_gqfai.o: /usr/include/sys/_types/_dev_t.h
s_gqfai.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqfai.o: /usr/include/sys/_types/_blksize_t.h
s_gqfai.o: /usr/include/sys/_types/_gid_t.h
s_gqfai.o: /usr/include/sys/_types/_in_addr_t.h
s_gqfai.o: /usr/include/sys/_types/_in_port_t.h
s_gqfai.o: /usr/include/sys/_types/_ino_t.h
s_gqfai.o: /usr/include/sys/_types/_ino64_t.h
s_gqfai.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gqfai.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gqfai.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gqfai.o: /usr/include/sys/_types/_clock_t.h
s_gqfai.o: /usr/include/sys/_types/_time_t.h
s_gqfai.o: /usr/include/sys/_types/_useconds_t.h
s_gqfai.o: /usr/include/sys/_types/_suseconds_t.h
s_gqfai.o: /usr/include/sys/_types/_rsize_t.h
s_gqfai.o: /usr/include/sys/_types/_errno_t.h
s_gqfai.o: /usr/include/sys/_types/_fd_def.h
s_gqfai.o: /usr/include/sys/_types/_fd_setsize.h
s_gqfai.o: /usr/include/sys/_types/_fd_set.h
s_gqfai.o: /usr/include/sys/_types/_fd_clr.h
s_gqfai.o: /usr/include/sys/_types/_fd_zero.h
s_gqfai.o: /usr/include/sys/_types/_fd_isset.h
s_gqfai.o: /usr/include/sys/_types/_fd_copy.h
s_gqfai.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqfai.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqfai.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqfai.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqfai.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqfai.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqfai.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqfai.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqfai.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqfai.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqfai.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqfai.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqfai.o: ../../../.././include/ncarg/c.h
s_gqfais.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqfais.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqfais.o: /usr/include/sys/_posix_availability.h
s_gqfais.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gqfais.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gqfais.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gqfais.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqfais.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqfais.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqfais.o: /usr/include/sys/_types/_int16_t.h
s_gqfais.o: /usr/include/sys/_types/_int32_t.h
s_gqfais.o: /usr/include/sys/_types/_int64_t.h
s_gqfais.o: /usr/include/sys/_types/_u_int8_t.h
s_gqfais.o: /usr/include/sys/_types/_u_int16_t.h
s_gqfais.o: /usr/include/sys/_types/_u_int32_t.h
s_gqfais.o: /usr/include/sys/_types/_u_int64_t.h
s_gqfais.o: /usr/include/sys/_types/_intptr_t.h
s_gqfais.o: /usr/include/sys/_types/_uintptr_t.h
s_gqfais.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqfais.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqfais.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqfais.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqfais.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqfais.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqfais.o: /usr/include/libkern/_OSByteOrder.h
s_gqfais.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqfais.o: /usr/include/sys/_types/_u_char.h
s_gqfais.o: /usr/include/sys/_types/_u_short.h
s_gqfais.o: /usr/include/sys/_types/_u_int.h
s_gqfais.o: /usr/include/sys/_types/_caddr_t.h
s_gqfais.o: /usr/include/sys/_types/_dev_t.h
s_gqfais.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqfais.o: /usr/include/sys/_types/_blksize_t.h
s_gqfais.o: /usr/include/sys/_types/_gid_t.h
s_gqfais.o: /usr/include/sys/_types/_in_addr_t.h
s_gqfais.o: /usr/include/sys/_types/_in_port_t.h
s_gqfais.o: /usr/include/sys/_types/_ino_t.h
s_gqfais.o: /usr/include/sys/_types/_ino64_t.h
s_gqfais.o: /usr/include/sys/_types/_key_t.h
s_gqfais.o: /usr/include/sys/_types/_mode_t.h
s_gqfais.o: /usr/include/sys/_types/_nlink_t.h
s_gqfais.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gqfais.o: /usr/include/sys/_types/_uid_t.h
s_gqfais.o: /usr/include/sys/_types/_clock_t.h
s_gqfais.o: /usr/include/sys/_types/_time_t.h
s_gqfais.o: /usr/include/sys/_types/_useconds_t.h
s_gqfais.o: /usr/include/sys/_types/_suseconds_t.h
s_gqfais.o: /usr/include/sys/_types/_rsize_t.h
s_gqfais.o: /usr/include/sys/_types/_errno_t.h
s_gqfais.o: /usr/include/sys/_types/_fd_def.h
s_gqfais.o: /usr/include/sys/_types/_fd_setsize.h
s_gqfais.o: /usr/include/sys/_types/_fd_set.h
s_gqfais.o: /usr/include/sys/_types/_fd_clr.h
s_gqfais.o: /usr/include/sys/_types/_fd_zero.h
s_gqfais.o: /usr/include/sys/_types/_fd_isset.h
s_gqfais.o: /usr/include/sys/_types/_fd_copy.h
s_gqfais.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqfais.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqfais.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqfais.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqfais.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqfais.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqfais.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqfais.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqfais.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqfais.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqfais.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqfais.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqfais.o: ../../../.././include/ncarg/c.h
s_gqfasi.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqfasi.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqfasi.o: /usr/include/sys/_posix_availability.h
s_gqfasi.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gqfasi.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gqfasi.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gqfasi.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqfasi.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqfasi.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqfasi.o: /usr/include/sys/_types/_int16_t.h
s_gqfasi.o: /usr/include/sys/_types/_int32_t.h
s_gqfasi.o: /usr/include/sys/_types/_int64_t.h
s_gqfasi.o: /usr/include/sys/_types/_u_int8_t.h
s_gqfasi.o: /usr/include/sys/_types/_u_int16_t.h
s_gqfasi.o: /usr/include/sys/_types/_u_int32_t.h
s_gqfasi.o: /usr/include/sys/_types/_u_int64_t.h
s_gqfasi.o: /usr/include/sys/_types/_intptr_t.h
s_gqfasi.o: /usr/include/sys/_types/_uintptr_t.h
s_gqfasi.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqfasi.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqfasi.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqfasi.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqfasi.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqfasi.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqfasi.o: /usr/include/libkern/_OSByteOrder.h
s_gqfasi.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqfasi.o: /usr/include/sys/_types/_u_char.h
s_gqfasi.o: /usr/include/sys/_types/_u_short.h
s_gqfasi.o: /usr/include/sys/_types/_u_int.h
s_gqfasi.o: /usr/include/sys/_types/_caddr_t.h
s_gqfasi.o: /usr/include/sys/_types/_dev_t.h
s_gqfasi.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqfasi.o: /usr/include/sys/_types/_blksize_t.h
s_gqfasi.o: /usr/include/sys/_types/_gid_t.h
s_gqfasi.o: /usr/include/sys/_types/_in_addr_t.h
s_gqfasi.o: /usr/include/sys/_types/_in_port_t.h
s_gqfasi.o: /usr/include/sys/_types/_ino_t.h
s_gqfasi.o: /usr/include/sys/_types/_ino64_t.h
s_gqfasi.o: /usr/include/sys/_types/_key_t.h
s_gqfasi.o: /usr/include/sys/_types/_mode_t.h
s_gqfasi.o: /usr/include/sys/_types/_nlink_t.h
s_gqfasi.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gqfasi.o: /usr/include/sys/_types/_uid_t.h
s_gqfasi.o: /usr/include/sys/_types/_clock_t.h
s_gqfasi.o: /usr/include/sys/_types/_time_t.h
s_gqfasi.o: /usr/include/sys/_types/_useconds_t.h
s_gqfasi.o: /usr/include/sys/_types/_suseconds_t.h
s_gqfasi.o: /usr/include/sys/_types/_rsize_t.h
s_gqfasi.o: /usr/include/sys/_types/_errno_t.h
s_gqfasi.o: /usr/include/sys/_types/_fd_def.h
s_gqfasi.o: /usr/include/sys/_types/_fd_setsize.h
s_gqfasi.o: /usr/include/sys/_types/_fd_set.h
s_gqfasi.o: /usr/include/sys/_types/_fd_clr.h
s_gqfasi.o: /usr/include/sys/_types/_fd_zero.h
s_gqfasi.o: /usr/include/sys/_types/_fd_isset.h
s_gqfasi.o: /usr/include/sys/_types/_fd_copy.h
s_gqfasi.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqfasi.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqfasi.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqfasi.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqfasi.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqfasi.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqfasi.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqfasi.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqfasi.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqfasi.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqfasi.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqfasi.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqfasi.o: ../../../.././include/ncarg/c.h
s_gqgdp.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqgdp.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqgdp.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gqgdp.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gqgdp.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gqgdp.o: /usr/include/i386/_types.h
s_gqgdp.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqgdp.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqgdp.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqgdp.o: /usr/include/sys/_types/_int16_t.h
s_gqgdp.o: /usr/include/sys/_types/_int32_t.h
s_gqgdp.o: /usr/include/sys/_types/_int64_t.h
s_gqgdp.o: /usr/include/sys/_types/_u_int8_t.h
s_gqgdp.o: /usr/include/sys/_types/_u_int16_t.h
s_gqgdp.o: /usr/include/sys/_types/_u_int32_t.h
s_gqgdp.o: /usr/include/sys/_types/_u_int64_t.h
s_gqgdp.o: /usr/include/sys/_types/_intptr_t.h
s_gqgdp.o: /usr/include/sys/_types/_uintptr_t.h
s_gqgdp.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqgdp.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqgdp.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqgdp.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqgdp.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqgdp.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqgdp.o: /usr/include/libkern/_OSByteOrder.h
s_gqgdp.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqgdp.o: /usr/include/sys/_types/_u_char.h
s_gqgdp.o: /usr/include/sys/_types/_u_short.h
s_gqgdp.o: /usr/include/sys/_types/_u_int.h
s_gqgdp.o: /usr/include/sys/_types/_caddr_t.h
s_gqgdp.o: /usr/include/sys/_types/_dev_t.h
s_gqgdp.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqgdp.o: /usr/include/sys/_types/_blksize_t.h
s_gqgdp.o: /usr/include/sys/_types/_gid_t.h
s_gqgdp.o: /usr/include/sys/_types/_in_addr_t.h
s_gqgdp.o: /usr/include/sys/_types/_in_port_t.h
s_gqgdp.o: /usr/include/sys/_types/_ino_t.h
s_gqgdp.o: /usr/include/sys/_types/_ino64_t.h
s_gqgdp.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gqgdp.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gqgdp.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gqgdp.o: /usr/include/sys/_types/_clock_t.h
s_gqgdp.o: /usr/include/sys/_types/_time_t.h
s_gqgdp.o: /usr/include/sys/_types/_useconds_t.h
s_gqgdp.o: /usr/include/sys/_types/_suseconds_t.h
s_gqgdp.o: /usr/include/sys/_types/_rsize_t.h
s_gqgdp.o: /usr/include/sys/_types/_errno_t.h
s_gqgdp.o: /usr/include/sys/_types/_fd_def.h
s_gqgdp.o: /usr/include/sys/_types/_fd_setsize.h
s_gqgdp.o: /usr/include/sys/_types/_fd_set.h
s_gqgdp.o: /usr/include/sys/_types/_fd_clr.h
s_gqgdp.o: /usr/include/sys/_types/_fd_zero.h
s_gqgdp.o: /usr/include/sys/_types/_fd_isset.h
s_gqgdp.o: /usr/include/sys/_types/_fd_copy.h
s_gqgdp.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqgdp.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqgdp.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqgdp.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqgdp.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqgdp.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqgdp.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqgdp.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqgdp.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqgdp.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqgdp.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqgdp.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqgdp.o: ../../../.././include/ncarg/c.h
s_gqina.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqina.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqina.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gqina.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gqina.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gqina.o: /usr/include/i386/_types.h
s_gqina.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqina.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqina.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqina.o: /usr/include/sys/_types/_int16_t.h
s_gqina.o: /usr/include/sys/_types/_int32_t.h
s_gqina.o: /usr/include/sys/_types/_int64_t.h
s_gqina.o: /usr/include/sys/_types/_u_int8_t.h
s_gqina.o: /usr/include/sys/_types/_u_int16_t.h
s_gqina.o: /usr/include/sys/_types/_u_int32_t.h
s_gqina.o: /usr/include/sys/_types/_u_int64_t.h
s_gqina.o: /usr/include/sys/_types/_intptr_t.h
s_gqina.o: /usr/include/sys/_types/_uintptr_t.h
s_gqina.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqina.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqina.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqina.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqina.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqina.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqina.o: /usr/include/libkern/_OSByteOrder.h
s_gqina.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqina.o: /usr/include/sys/_types/_u_char.h
s_gqina.o: /usr/include/sys/_types/_u_short.h
s_gqina.o: /usr/include/sys/_types/_u_int.h
s_gqina.o: /usr/include/sys/_types/_caddr_t.h
s_gqina.o: /usr/include/sys/_types/_dev_t.h
s_gqina.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqina.o: /usr/include/sys/_types/_blksize_t.h
s_gqina.o: /usr/include/sys/_types/_gid_t.h
s_gqina.o: /usr/include/sys/_types/_in_addr_t.h
s_gqina.o: /usr/include/sys/_types/_in_port_t.h
s_gqina.o: /usr/include/sys/_types/_ino_t.h
s_gqina.o: /usr/include/sys/_types/_ino64_t.h
s_gqina.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gqina.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gqina.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gqina.o: /usr/include/sys/_types/_clock_t.h
s_gqina.o: /usr/include/sys/_types/_time_t.h
s_gqina.o: /usr/include/sys/_types/_useconds_t.h
s_gqina.o: /usr/include/sys/_types/_suseconds_t.h
s_gqina.o: /usr/include/sys/_types/_rsize_t.h
s_gqina.o: /usr/include/sys/_types/_errno_t.h
s_gqina.o: /usr/include/sys/_types/_fd_def.h
s_gqina.o: /usr/include/sys/_types/_fd_setsize.h
s_gqina.o: /usr/include/sys/_types/_fd_set.h
s_gqina.o: /usr/include/sys/_types/_fd_clr.h
s_gqina.o: /usr/include/sys/_types/_fd_zero.h
s_gqina.o: /usr/include/sys/_types/_fd_isset.h
s_gqina.o: /usr/include/sys/_types/_fd_copy.h
s_gqina.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqina.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqina.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqina.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqina.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqina.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqina.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqina.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqina.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqina.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqina.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqina.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqina.o: ../../../.././include/ncarg/c.h
s_gqln.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqln.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqln.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gqln.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gqln.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gqln.o: /usr/include/i386/_types.h
s_gqln.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqln.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqln.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqln.o: /usr/include/sys/_types/_int16_t.h
s_gqln.o: /usr/include/sys/_types/_int32_t.h
s_gqln.o: /usr/include/sys/_types/_int64_t.h
s_gqln.o: /usr/include/sys/_types/_u_int8_t.h
s_gqln.o: /usr/include/sys/_types/_u_int16_t.h
s_gqln.o: /usr/include/sys/_types/_u_int32_t.h
s_gqln.o: /usr/include/sys/_types/_u_int64_t.h
s_gqln.o: /usr/include/sys/_types/_intptr_t.h
s_gqln.o: /usr/include/sys/_types/_uintptr_t.h
s_gqln.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqln.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqln.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqln.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqln.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqln.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqln.o: /usr/include/libkern/_OSByteOrder.h
s_gqln.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqln.o: /usr/include/sys/_types/_u_char.h
s_gqln.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
s_gqln.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
s_gqln.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqln.o: /usr/include/sys/_types/_blksize_t.h
s_gqln.o: /usr/include/sys/_types/_gid_t.h
s_gqln.o: /usr/include/sys/_types/_in_addr_t.h
s_gqln.o: /usr/include/sys/_types/_in_port_t.h
s_gqln.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
s_gqln.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gqln.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gqln.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gqln.o: /usr/include/sys/_types/_clock_t.h
s_gqln.o: /usr/include/sys/_types/_time_t.h
s_gqln.o: /usr/include/sys/_types/_useconds_t.h
s_gqln.o: /usr/include/sys/_types/_suseconds_t.h
s_gqln.o: /usr/include/sys/_types/_rsize_t.h
s_gqln.o: /usr/include/sys/_types/_errno_t.h
s_gqln.o: /usr/include/sys/_types/_fd_def.h
s_gqln.o: /usr/include/sys/_types/_fd_setsize.h
s_gqln.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
s_gqln.o: /usr/include/sys/_types/_fd_zero.h
s_gqln.o: /usr/include/sys/_types/_fd_isset.h
s_gqln.o: /usr/include/sys/_types/_fd_copy.h
s_gqln.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqln.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqln.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqln.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqln.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqln.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqln.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqln.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqln.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqln.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqln.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqln.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqln.o: ../../../.././include/ncarg/c.h
s_gqlvks.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqlvks.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqlvks.o: /usr/include/sys/_posix_availability.h
s_gqlvks.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gqlvks.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gqlvks.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gqlvks.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqlvks.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqlvks.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqlvks.o: /usr/include/sys/_types/_int16_t.h
s_gqlvks.o: /usr/include/sys/_types/_int32_t.h
s_gqlvks.o: /usr/include/sys/_types/_int64_t.h
s_gqlvks.o: /usr/include/sys/_types/_u_int8_t.h
s_gqlvks.o: /usr/include/sys/_types/_u_int16_t.h
s_gqlvks.o: /usr/include/sys/_types/_u_int32_t.h
s_gqlvks.o: /usr/include/sys/_types/_u_int64_t.h
s_gqlvks.o: /usr/include/sys/_types/_intptr_t.h
s_gqlvks.o: /usr/include/sys/_types/_uintptr_t.h
s_gqlvks.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqlvks.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqlvks.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqlvks.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqlvks.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqlvks.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqlvks.o: /usr/include/libkern/_OSByteOrder.h
s_gqlvks.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqlvks.o: /usr/include/sys/_types/_u_char.h
s_gqlvks.o: /usr/include/sys/_types/_u_short.h
s_gqlvks.o: /usr/include/sys/_types/_u_int.h
s_gqlvks.o: /usr/include/sys/_types/_caddr_t.h
s_gqlvks.o: /usr/include/sys/_types/_dev_t.h
s_gqlvks.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqlvks.o: /usr/include/sys/_types/_blksize_t.h
s_gqlvks.o: /usr/include/sys/_types/_gid_t.h
s_gqlvks.o: /usr/include/sys/_types/_in_addr_t.h
s_gqlvks.o: /usr/include/sys/_types/_in_port_t.h
s_gqlvks.o: /usr/include/sys/_types/_ino_t.h
s_gqlvks.o: /usr/include/sys/_types/_ino64_t.h
s_gqlvks.o: /usr/include/sys/_types/_key_t.h
s_gqlvks.o: /usr/include/sys/_types/_mode_t.h
s_gqlvks.o: /usr/include/sys/_types/_nlink_t.h
s_gqlvks.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gqlvks.o: /usr/include/sys/_types/_uid_t.h
s_gqlvks.o: /usr/include/sys/_types/_clock_t.h
s_gqlvks.o: /usr/include/sys/_types/_time_t.h
s_gqlvks.o: /usr/include/sys/_types/_useconds_t.h
s_gqlvks.o: /usr/include/sys/_types/_suseconds_t.h
s_gqlvks.o: /usr/include/sys/_types/_rsize_t.h
s_gqlvks.o: /usr/include/sys/_types/_errno_t.h
s_gqlvks.o: /usr/include/sys/_types/_fd_def.h
s_gqlvks.o: /usr/include/sys/_types/_fd_setsize.h
s_gqlvks.o: /usr/include/sys/_types/_fd_set.h
s_gqlvks.o: /usr/include/sys/_types/_fd_clr.h
s_gqlvks.o: /usr/include/sys/_types/_fd_zero.h
s_gqlvks.o: /usr/include/sys/_types/_fd_isset.h
s_gqlvks.o: /usr/include/sys/_types/_fd_copy.h
s_gqlvks.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqlvks.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqlvks.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqlvks.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqlvks.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqlvks.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqlvks.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqlvks.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqlvks.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqlvks.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqlvks.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqlvks.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqlvks.o: ../../../.././include/ncarg/c.h
s_gqlwk.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqlwk.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqlwk.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gqlwk.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gqlwk.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gqlwk.o: /usr/include/i386/_types.h
s_gqlwk.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqlwk.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqlwk.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqlwk.o: /usr/include/sys/_types/_int16_t.h
s_gqlwk.o: /usr/include/sys/_types/_int32_t.h
s_gqlwk.o: /usr/include/sys/_types/_int64_t.h
s_gqlwk.o: /usr/include/sys/_types/_u_int8_t.h
s_gqlwk.o: /usr/include/sys/_types/_u_int16_t.h
s_gqlwk.o: /usr/include/sys/_types/_u_int32_t.h
s_gqlwk.o: /usr/include/sys/_types/_u_int64_t.h
s_gqlwk.o: /usr/include/sys/_types/_intptr_t.h
s_gqlwk.o: /usr/include/sys/_types/_uintptr_t.h
s_gqlwk.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqlwk.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqlwk.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqlwk.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqlwk.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqlwk.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqlwk.o: /usr/include/libkern/_OSByteOrder.h
s_gqlwk.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqlwk.o: /usr/include/sys/_types/_u_char.h
s_gqlwk.o: /usr/include/sys/_types/_u_short.h
s_gqlwk.o: /usr/include/sys/_types/_u_int.h
s_gqlwk.o: /usr/include/sys/_types/_caddr_t.h
s_gqlwk.o: /usr/include/sys/_types/_dev_t.h
s_gqlwk.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqlwk.o: /usr/include/sys/_types/_blksize_t.h
s_gqlwk.o: /usr/include/sys/_types/_gid_t.h
s_gqlwk.o: /usr/include/sys/_types/_in_addr_t.h
s_gqlwk.o: /usr/include/sys/_types/_in_port_t.h
s_gqlwk.o: /usr/include/sys/_types/_ino_t.h
s_gqlwk.o: /usr/include/sys/_types/_ino64_t.h
s_gqlwk.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gqlwk.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gqlwk.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gqlwk.o: /usr/include/sys/_types/_clock_t.h
s_gqlwk.o: /usr/include/sys/_types/_time_t.h
s_gqlwk.o: /usr/include/sys/_types/_useconds_t.h
s_gqlwk.o: /usr/include/sys/_types/_suseconds_t.h
s_gqlwk.o: /usr/include/sys/_types/_rsize_t.h
s_gqlwk.o: /usr/include/sys/_types/_errno_t.h
s_gqlwk.o: /usr/include/sys/_types/_fd_def.h
s_gqlwk.o: /usr/include/sys/_types/_fd_setsize.h
s_gqlwk.o: /usr/include/sys/_types/_fd_set.h
s_gqlwk.o: /usr/include/sys/_types/_fd_clr.h
s_gqlwk.o: /usr/include/sys/_types/_fd_zero.h
s_gqlwk.o: /usr/include/sys/_types/_fd_isset.h
s_gqlwk.o: /usr/include/sys/_types/_fd_copy.h
s_gqlwk.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqlwk.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqlwk.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqlwk.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqlwk.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqlwk.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqlwk.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqlwk.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqlwk.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqlwk.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqlwk.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqlwk.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqlwk.o: ../../../.././include/ncarg/c.h
s_gqlwsc.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqlwsc.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqlwsc.o: /usr/include/sys/_posix_availability.h
s_gqlwsc.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gqlwsc.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gqlwsc.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gqlwsc.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqlwsc.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqlwsc.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqlwsc.o: /usr/include/sys/_types/_int16_t.h
s_gqlwsc.o: /usr/include/sys/_types/_int32_t.h
s_gqlwsc.o: /usr/include/sys/_types/_int64_t.h
s_gqlwsc.o: /usr/include/sys/_types/_u_int8_t.h
s_gqlwsc.o: /usr/include/sys/_types/_u_int16_t.h
s_gqlwsc.o: /usr/include/sys/_types/_u_int32_t.h
s_gqlwsc.o: /usr/include/sys/_types/_u_int64_t.h
s_gqlwsc.o: /usr/include/sys/_types/_intptr_t.h
s_gqlwsc.o: /usr/include/sys/_types/_uintptr_t.h
s_gqlwsc.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqlwsc.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqlwsc.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqlwsc.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqlwsc.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqlwsc.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqlwsc.o: /usr/include/libkern/_OSByteOrder.h
s_gqlwsc.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqlwsc.o: /usr/include/sys/_types/_u_char.h
s_gqlwsc.o: /usr/include/sys/_types/_u_short.h
s_gqlwsc.o: /usr/include/sys/_types/_u_int.h
s_gqlwsc.o: /usr/include/sys/_types/_caddr_t.h
s_gqlwsc.o: /usr/include/sys/_types/_dev_t.h
s_gqlwsc.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqlwsc.o: /usr/include/sys/_types/_blksize_t.h
s_gqlwsc.o: /usr/include/sys/_types/_gid_t.h
s_gqlwsc.o: /usr/include/sys/_types/_in_addr_t.h
s_gqlwsc.o: /usr/include/sys/_types/_in_port_t.h
s_gqlwsc.o: /usr/include/sys/_types/_ino_t.h
s_gqlwsc.o: /usr/include/sys/_types/_ino64_t.h
s_gqlwsc.o: /usr/include/sys/_types/_key_t.h
s_gqlwsc.o: /usr/include/sys/_types/_mode_t.h
s_gqlwsc.o: /usr/include/sys/_types/_nlink_t.h
s_gqlwsc.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gqlwsc.o: /usr/include/sys/_types/_uid_t.h
s_gqlwsc.o: /usr/include/sys/_types/_clock_t.h
s_gqlwsc.o: /usr/include/sys/_types/_time_t.h
s_gqlwsc.o: /usr/include/sys/_types/_useconds_t.h
s_gqlwsc.o: /usr/include/sys/_types/_suseconds_t.h
s_gqlwsc.o: /usr/include/sys/_types/_rsize_t.h
s_gqlwsc.o: /usr/include/sys/_types/_errno_t.h
s_gqlwsc.o: /usr/include/sys/_types/_fd_def.h
s_gqlwsc.o: /usr/include/sys/_types/_fd_setsize.h
s_gqlwsc.o: /usr/include/sys/_types/_fd_set.h
s_gqlwsc.o: /usr/include/sys/_types/_fd_clr.h
s_gqlwsc.o: /usr/include/sys/_types/_fd_zero.h
s_gqlwsc.o: /usr/include/sys/_types/_fd_isset.h
s_gqlwsc.o: /usr/include/sys/_types/_fd_copy.h
s_gqlwsc.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqlwsc.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqlwsc.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqlwsc.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqlwsc.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqlwsc.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqlwsc.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqlwsc.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqlwsc.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqlwsc.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqlwsc.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqlwsc.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqlwsc.o: ../../../.././include/ncarg/c.h
s_gqmk.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqmk.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqmk.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gqmk.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gqmk.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gqmk.o: /usr/include/i386/_types.h
s_gqmk.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqmk.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqmk.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqmk.o: /usr/include/sys/_types/_int16_t.h
s_gqmk.o: /usr/include/sys/_types/_int32_t.h
s_gqmk.o: /usr/include/sys/_types/_int64_t.h
s_gqmk.o: /usr/include/sys/_types/_u_int8_t.h
s_gqmk.o: /usr/include/sys/_types/_u_int16_t.h
s_gqmk.o: /usr/include/sys/_types/_u_int32_t.h
s_gqmk.o: /usr/include/sys/_types/_u_int64_t.h
s_gqmk.o: /usr/include/sys/_types/_intptr_t.h
s_gqmk.o: /usr/include/sys/_types/_uintptr_t.h
s_gqmk.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqmk.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqmk.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqmk.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqmk.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqmk.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqmk.o: /usr/include/libkern/_OSByteOrder.h
s_gqmk.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqmk.o: /usr/include/sys/_types/_u_char.h
s_gqmk.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
s_gqmk.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
s_gqmk.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqmk.o: /usr/include/sys/_types/_blksize_t.h
s_gqmk.o: /usr/include/sys/_types/_gid_t.h
s_gqmk.o: /usr/include/sys/_types/_in_addr_t.h
s_gqmk.o: /usr/include/sys/_types/_in_port_t.h
s_gqmk.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
s_gqmk.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gqmk.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gqmk.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gqmk.o: /usr/include/sys/_types/_clock_t.h
s_gqmk.o: /usr/include/sys/_types/_time_t.h
s_gqmk.o: /usr/include/sys/_types/_useconds_t.h
s_gqmk.o: /usr/include/sys/_types/_suseconds_t.h
s_gqmk.o: /usr/include/sys/_types/_rsize_t.h
s_gqmk.o: /usr/include/sys/_types/_errno_t.h
s_gqmk.o: /usr/include/sys/_types/_fd_def.h
s_gqmk.o: /usr/include/sys/_types/_fd_setsize.h
s_gqmk.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
s_gqmk.o: /usr/include/sys/_types/_fd_zero.h
s_gqmk.o: /usr/include/sys/_types/_fd_isset.h
s_gqmk.o: /usr/include/sys/_types/_fd_copy.h
s_gqmk.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqmk.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqmk.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqmk.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqmk.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqmk.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqmk.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqmk.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqmk.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqmk.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqmk.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqmk.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqmk.o: ../../../.././include/ncarg/c.h
s_gqmksc.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqmksc.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqmksc.o: /usr/include/sys/_posix_availability.h
s_gqmksc.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gqmksc.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gqmksc.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gqmksc.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqmksc.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqmksc.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqmksc.o: /usr/include/sys/_types/_int16_t.h
s_gqmksc.o: /usr/include/sys/_types/_int32_t.h
s_gqmksc.o: /usr/include/sys/_types/_int64_t.h
s_gqmksc.o: /usr/include/sys/_types/_u_int8_t.h
s_gqmksc.o: /usr/include/sys/_types/_u_int16_t.h
s_gqmksc.o: /usr/include/sys/_types/_u_int32_t.h
s_gqmksc.o: /usr/include/sys/_types/_u_int64_t.h
s_gqmksc.o: /usr/include/sys/_types/_intptr_t.h
s_gqmksc.o: /usr/include/sys/_types/_uintptr_t.h
s_gqmksc.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqmksc.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqmksc.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqmksc.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqmksc.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqmksc.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqmksc.o: /usr/include/libkern/_OSByteOrder.h
s_gqmksc.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqmksc.o: /usr/include/sys/_types/_u_char.h
s_gqmksc.o: /usr/include/sys/_types/_u_short.h
s_gqmksc.o: /usr/include/sys/_types/_u_int.h
s_gqmksc.o: /usr/include/sys/_types/_caddr_t.h
s_gqmksc.o: /usr/include/sys/_types/_dev_t.h
s_gqmksc.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqmksc.o: /usr/include/sys/_types/_blksize_t.h
s_gqmksc.o: /usr/include/sys/_types/_gid_t.h
s_gqmksc.o: /usr/include/sys/_types/_in_addr_t.h
s_gqmksc.o: /usr/include/sys/_types/_in_port_t.h
s_gqmksc.o: /usr/include/sys/_types/_ino_t.h
s_gqmksc.o: /usr/include/sys/_types/_ino64_t.h
s_gqmksc.o: /usr/include/sys/_types/_key_t.h
s_gqmksc.o: /usr/include/sys/_types/_mode_t.h
s_gqmksc.o: /usr/include/sys/_types/_nlink_t.h
s_gqmksc.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gqmksc.o: /usr/include/sys/_types/_uid_t.h
s_gqmksc.o: /usr/include/sys/_types/_clock_t.h
s_gqmksc.o: /usr/include/sys/_types/_time_t.h
s_gqmksc.o: /usr/include/sys/_types/_useconds_t.h
s_gqmksc.o: /usr/include/sys/_types/_suseconds_t.h
s_gqmksc.o: /usr/include/sys/_types/_rsize_t.h
s_gqmksc.o: /usr/include/sys/_types/_errno_t.h
s_gqmksc.o: /usr/include/sys/_types/_fd_def.h
s_gqmksc.o: /usr/include/sys/_types/_fd_setsize.h
s_gqmksc.o: /usr/include/sys/_types/_fd_set.h
s_gqmksc.o: /usr/include/sys/_types/_fd_clr.h
s_gqmksc.o: /usr/include/sys/_types/_fd_zero.h
s_gqmksc.o: /usr/include/sys/_types/_fd_isset.h
s_gqmksc.o: /usr/include/sys/_types/_fd_copy.h
s_gqmksc.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqmksc.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqmksc.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqmksc.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqmksc.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqmksc.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqmksc.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqmksc.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqmksc.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqmksc.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqmksc.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqmksc.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqmksc.o: ../../../.././include/ncarg/c.h
s_gqmntn.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqmntn.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqmntn.o: /usr/include/sys/_posix_availability.h
s_gqmntn.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gqmntn.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gqmntn.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gqmntn.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqmntn.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqmntn.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqmntn.o: /usr/include/sys/_types/_int16_t.h
s_gqmntn.o: /usr/include/sys/_types/_int32_t.h
s_gqmntn.o: /usr/include/sys/_types/_int64_t.h
s_gqmntn.o: /usr/include/sys/_types/_u_int8_t.h
s_gqmntn.o: /usr/include/sys/_types/_u_int16_t.h
s_gqmntn.o: /usr/include/sys/_types/_u_int32_t.h
s_gqmntn.o: /usr/include/sys/_types/_u_int64_t.h
s_gqmntn.o: /usr/include/sys/_types/_intptr_t.h
s_gqmntn.o: /usr/include/sys/_types/_uintptr_t.h
s_gqmntn.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqmntn.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqmntn.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqmntn.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqmntn.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqmntn.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqmntn.o: /usr/include/libkern/_OSByteOrder.h
s_gqmntn.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqmntn.o: /usr/include/sys/_types/_u_char.h
s_gqmntn.o: /usr/include/sys/_types/_u_short.h
s_gqmntn.o: /usr/include/sys/_types/_u_int.h
s_gqmntn.o: /usr/include/sys/_types/_caddr_t.h
s_gqmntn.o: /usr/include/sys/_types/_dev_t.h
s_gqmntn.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqmntn.o: /usr/include/sys/_types/_blksize_t.h
s_gqmntn.o: /usr/include/sys/_types/_gid_t.h
s_gqmntn.o: /usr/include/sys/_types/_in_addr_t.h
s_gqmntn.o: /usr/include/sys/_types/_in_port_t.h
s_gqmntn.o: /usr/include/sys/_types/_ino_t.h
s_gqmntn.o: /usr/include/sys/_types/_ino64_t.h
s_gqmntn.o: /usr/include/sys/_types/_key_t.h
s_gqmntn.o: /usr/include/sys/_types/_mode_t.h
s_gqmntn.o: /usr/include/sys/_types/_nlink_t.h
s_gqmntn.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gqmntn.o: /usr/include/sys/_types/_uid_t.h
s_gqmntn.o: /usr/include/sys/_types/_clock_t.h
s_gqmntn.o: /usr/include/sys/_types/_time_t.h
s_gqmntn.o: /usr/include/sys/_types/_useconds_t.h
s_gqmntn.o: /usr/include/sys/_types/_suseconds_t.h
s_gqmntn.o: /usr/include/sys/_types/_rsize_t.h
s_gqmntn.o: /usr/include/sys/_types/_errno_t.h
s_gqmntn.o: /usr/include/sys/_types/_fd_def.h
s_gqmntn.o: /usr/include/sys/_types/_fd_setsize.h
s_gqmntn.o: /usr/include/sys/_types/_fd_set.h
s_gqmntn.o: /usr/include/sys/_types/_fd_clr.h
s_gqmntn.o: /usr/include/sys/_types/_fd_zero.h
s_gqmntn.o: /usr/include/sys/_types/_fd_isset.h
s_gqmntn.o: /usr/include/sys/_types/_fd_copy.h
s_gqmntn.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqmntn.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqmntn.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqmntn.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqmntn.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqmntn.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqmntn.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqmntn.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqmntn.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqmntn.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqmntn.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqmntn.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqmntn.o: ../../../.././include/ncarg/c.h
s_gqnt.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqnt.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqnt.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gqnt.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gqnt.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gqnt.o: /usr/include/i386/_types.h
s_gqnt.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqnt.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqnt.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqnt.o: /usr/include/sys/_types/_int16_t.h
s_gqnt.o: /usr/include/sys/_types/_int32_t.h
s_gqnt.o: /usr/include/sys/_types/_int64_t.h
s_gqnt.o: /usr/include/sys/_types/_u_int8_t.h
s_gqnt.o: /usr/include/sys/_types/_u_int16_t.h
s_gqnt.o: /usr/include/sys/_types/_u_int32_t.h
s_gqnt.o: /usr/include/sys/_types/_u_int64_t.h
s_gqnt.o: /usr/include/sys/_types/_intptr_t.h
s_gqnt.o: /usr/include/sys/_types/_uintptr_t.h
s_gqnt.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqnt.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqnt.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqnt.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqnt.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqnt.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqnt.o: /usr/include/libkern/_OSByteOrder.h
s_gqnt.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqnt.o: /usr/include/sys/_types/_u_char.h
s_gqnt.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
s_gqnt.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
s_gqnt.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqnt.o: /usr/include/sys/_types/_blksize_t.h
s_gqnt.o: /usr/include/sys/_types/_gid_t.h
s_gqnt.o: /usr/include/sys/_types/_in_addr_t.h
s_gqnt.o: /usr/include/sys/_types/_in_port_t.h
s_gqnt.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
s_gqnt.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gqnt.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gqnt.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gqnt.o: /usr/include/sys/_types/_clock_t.h
s_gqnt.o: /usr/include/sys/_types/_time_t.h
s_gqnt.o: /usr/include/sys/_types/_useconds_t.h
s_gqnt.o: /usr/include/sys/_types/_suseconds_t.h
s_gqnt.o: /usr/include/sys/_types/_rsize_t.h
s_gqnt.o: /usr/include/sys/_types/_errno_t.h
s_gqnt.o: /usr/include/sys/_types/_fd_def.h
s_gqnt.o: /usr/include/sys/_types/_fd_setsize.h
s_gqnt.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
s_gqnt.o: /usr/include/sys/_types/_fd_zero.h
s_gqnt.o: /usr/include/sys/_types/_fd_isset.h
s_gqnt.o: /usr/include/sys/_types/_fd_copy.h
s_gqnt.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqnt.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqnt.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqnt.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqnt.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqnt.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqnt.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqnt.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqnt.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqnt.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqnt.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqnt.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqnt.o: ../../../.././include/ncarg/c.h
s_gqops.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqops.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqops.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gqops.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gqops.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gqops.o: /usr/include/i386/_types.h
s_gqops.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqops.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqops.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqops.o: /usr/include/sys/_types/_int16_t.h
s_gqops.o: /usr/include/sys/_types/_int32_t.h
s_gqops.o: /usr/include/sys/_types/_int64_t.h
s_gqops.o: /usr/include/sys/_types/_u_int8_t.h
s_gqops.o: /usr/include/sys/_types/_u_int16_t.h
s_gqops.o: /usr/include/sys/_types/_u_int32_t.h
s_gqops.o: /usr/include/sys/_types/_u_int64_t.h
s_gqops.o: /usr/include/sys/_types/_intptr_t.h
s_gqops.o: /usr/include/sys/_types/_uintptr_t.h
s_gqops.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqops.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqops.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqops.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqops.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqops.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqops.o: /usr/include/libkern/_OSByteOrder.h
s_gqops.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqops.o: /usr/include/sys/_types/_u_char.h
s_gqops.o: /usr/include/sys/_types/_u_short.h
s_gqops.o: /usr/include/sys/_types/_u_int.h
s_gqops.o: /usr/include/sys/_types/_caddr_t.h
s_gqops.o: /usr/include/sys/_types/_dev_t.h
s_gqops.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqops.o: /usr/include/sys/_types/_blksize_t.h
s_gqops.o: /usr/include/sys/_types/_gid_t.h
s_gqops.o: /usr/include/sys/_types/_in_addr_t.h
s_gqops.o: /usr/include/sys/_types/_in_port_t.h
s_gqops.o: /usr/include/sys/_types/_ino_t.h
s_gqops.o: /usr/include/sys/_types/_ino64_t.h
s_gqops.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gqops.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gqops.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gqops.o: /usr/include/sys/_types/_clock_t.h
s_gqops.o: /usr/include/sys/_types/_time_t.h
s_gqops.o: /usr/include/sys/_types/_useconds_t.h
s_gqops.o: /usr/include/sys/_types/_suseconds_t.h
s_gqops.o: /usr/include/sys/_types/_rsize_t.h
s_gqops.o: /usr/include/sys/_types/_errno_t.h
s_gqops.o: /usr/include/sys/_types/_fd_def.h
s_gqops.o: /usr/include/sys/_types/_fd_setsize.h
s_gqops.o: /usr/include/sys/_types/_fd_set.h
s_gqops.o: /usr/include/sys/_types/_fd_clr.h
s_gqops.o: /usr/include/sys/_types/_fd_zero.h
s_gqops.o: /usr/include/sys/_types/_fd_isset.h
s_gqops.o: /usr/include/sys/_types/_fd_copy.h
s_gqops.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqops.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqops.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqops.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqops.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqops.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqops.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqops.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqops.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqops.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqops.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqops.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqops.o: ../../../.././include/ncarg/c.h
s_gqopwk.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqopwk.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqopwk.o: /usr/include/sys/_posix_availability.h
s_gqopwk.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gqopwk.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gqopwk.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gqopwk.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqopwk.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqopwk.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqopwk.o: /usr/include/sys/_types/_int16_t.h
s_gqopwk.o: /usr/include/sys/_types/_int32_t.h
s_gqopwk.o: /usr/include/sys/_types/_int64_t.h
s_gqopwk.o: /usr/include/sys/_types/_u_int8_t.h
s_gqopwk.o: /usr/include/sys/_types/_u_int16_t.h
s_gqopwk.o: /usr/include/sys/_types/_u_int32_t.h
s_gqopwk.o: /usr/include/sys/_types/_u_int64_t.h
s_gqopwk.o: /usr/include/sys/_types/_intptr_t.h
s_gqopwk.o: /usr/include/sys/_types/_uintptr_t.h
s_gqopwk.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqopwk.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqopwk.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqopwk.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqopwk.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqopwk.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqopwk.o: /usr/include/libkern/_OSByteOrder.h
s_gqopwk.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqopwk.o: /usr/include/sys/_types/_u_char.h
s_gqopwk.o: /usr/include/sys/_types/_u_short.h
s_gqopwk.o: /usr/include/sys/_types/_u_int.h
s_gqopwk.o: /usr/include/sys/_types/_caddr_t.h
s_gqopwk.o: /usr/include/sys/_types/_dev_t.h
s_gqopwk.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqopwk.o: /usr/include/sys/_types/_blksize_t.h
s_gqopwk.o: /usr/include/sys/_types/_gid_t.h
s_gqopwk.o: /usr/include/sys/_types/_in_addr_t.h
s_gqopwk.o: /usr/include/sys/_types/_in_port_t.h
s_gqopwk.o: /usr/include/sys/_types/_ino_t.h
s_gqopwk.o: /usr/include/sys/_types/_ino64_t.h
s_gqopwk.o: /usr/include/sys/_types/_key_t.h
s_gqopwk.o: /usr/include/sys/_types/_mode_t.h
s_gqopwk.o: /usr/include/sys/_types/_nlink_t.h
s_gqopwk.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gqopwk.o: /usr/include/sys/_types/_uid_t.h
s_gqopwk.o: /usr/include/sys/_types/_clock_t.h
s_gqopwk.o: /usr/include/sys/_types/_time_t.h
s_gqopwk.o: /usr/include/sys/_types/_useconds_t.h
s_gqopwk.o: /usr/include/sys/_types/_suseconds_t.h
s_gqopwk.o: /usr/include/sys/_types/_rsize_t.h
s_gqopwk.o: /usr/include/sys/_types/_errno_t.h
s_gqopwk.o: /usr/include/sys/_types/_fd_def.h
s_gqopwk.o: /usr/include/sys/_types/_fd_setsize.h
s_gqopwk.o: /usr/include/sys/_types/_fd_set.h
s_gqopwk.o: /usr/include/sys/_types/_fd_clr.h
s_gqopwk.o: /usr/include/sys/_types/_fd_zero.h
s_gqopwk.o: /usr/include/sys/_types/_fd_isset.h
s_gqopwk.o: /usr/include/sys/_types/_fd_copy.h
s_gqopwk.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqopwk.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqopwk.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqopwk.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqopwk.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqopwk.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqopwk.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqopwk.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqopwk.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqopwk.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqopwk.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqopwk.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqopwk.o: ../../../.././include/ncarg/c.h
s_gqpaf.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqpaf.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqpaf.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gqpaf.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gqpaf.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gqpaf.o: /usr/include/i386/_types.h
s_gqpaf.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqpaf.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqpaf.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqpaf.o: /usr/include/sys/_types/_int16_t.h
s_gqpaf.o: /usr/include/sys/_types/_int32_t.h
s_gqpaf.o: /usr/include/sys/_types/_int64_t.h
s_gqpaf.o: /usr/include/sys/_types/_u_int8_t.h
s_gqpaf.o: /usr/include/sys/_types/_u_int16_t.h
s_gqpaf.o: /usr/include/sys/_types/_u_int32_t.h
s_gqpaf.o: /usr/include/sys/_types/_u_int64_t.h
s_gqpaf.o: /usr/include/sys/_types/_intptr_t.h
s_gqpaf.o: /usr/include/sys/_types/_uintptr_t.h
s_gqpaf.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqpaf.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqpaf.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqpaf.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqpaf.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqpaf.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqpaf.o: /usr/include/libkern/_OSByteOrder.h
s_gqpaf.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqpaf.o: /usr/include/sys/_types/_u_char.h
s_gqpaf.o: /usr/include/sys/_types/_u_short.h
s_gqpaf.o: /usr/include/sys/_types/_u_int.h
s_gqpaf.o: /usr/include/sys/_types/_caddr_t.h
s_gqpaf.o: /usr/include/sys/_types/_dev_t.h
s_gqpaf.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqpaf.o: /usr/include/sys/_types/_blksize_t.h
s_gqpaf.o: /usr/include/sys/_types/_gid_t.h
s_gqpaf.o: /usr/include/sys/_types/_in_addr_t.h
s_gqpaf.o: /usr/include/sys/_types/_in_port_t.h
s_gqpaf.o: /usr/include/sys/_types/_ino_t.h
s_gqpaf.o: /usr/include/sys/_types/_ino64_t.h
s_gqpaf.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gqpaf.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gqpaf.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gqpaf.o: /usr/include/sys/_types/_clock_t.h
s_gqpaf.o: /usr/include/sys/_types/_time_t.h
s_gqpaf.o: /usr/include/sys/_types/_useconds_t.h
s_gqpaf.o: /usr/include/sys/_types/_suseconds_t.h
s_gqpaf.o: /usr/include/sys/_types/_rsize_t.h
s_gqpaf.o: /usr/include/sys/_types/_errno_t.h
s_gqpaf.o: /usr/include/sys/_types/_fd_def.h
s_gqpaf.o: /usr/include/sys/_types/_fd_setsize.h
s_gqpaf.o: /usr/include/sys/_types/_fd_set.h
s_gqpaf.o: /usr/include/sys/_types/_fd_clr.h
s_gqpaf.o: /usr/include/sys/_types/_fd_zero.h
s_gqpaf.o: /usr/include/sys/_types/_fd_isset.h
s_gqpaf.o: /usr/include/sys/_types/_fd_copy.h
s_gqpaf.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqpaf.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqpaf.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqpaf.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqpaf.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqpaf.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqpaf.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqpaf.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqpaf.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqpaf.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqpaf.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqpaf.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqpaf.o: ../../../.././include/ncarg/c.h
s_gqpahv.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqpahv.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqpahv.o: /usr/include/sys/_posix_availability.h
s_gqpahv.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gqpahv.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gqpahv.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gqpahv.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqpahv.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqpahv.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqpahv.o: /usr/include/sys/_types/_int16_t.h
s_gqpahv.o: /usr/include/sys/_types/_int32_t.h
s_gqpahv.o: /usr/include/sys/_types/_int64_t.h
s_gqpahv.o: /usr/include/sys/_types/_u_int8_t.h
s_gqpahv.o: /usr/include/sys/_types/_u_int16_t.h
s_gqpahv.o: /usr/include/sys/_types/_u_int32_t.h
s_gqpahv.o: /usr/include/sys/_types/_u_int64_t.h
s_gqpahv.o: /usr/include/sys/_types/_intptr_t.h
s_gqpahv.o: /usr/include/sys/_types/_uintptr_t.h
s_gqpahv.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqpahv.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqpahv.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqpahv.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqpahv.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqpahv.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqpahv.o: /usr/include/libkern/_OSByteOrder.h
s_gqpahv.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqpahv.o: /usr/include/sys/_types/_u_char.h
s_gqpahv.o: /usr/include/sys/_types/_u_short.h
s_gqpahv.o: /usr/include/sys/_types/_u_int.h
s_gqpahv.o: /usr/include/sys/_types/_caddr_t.h
s_gqpahv.o: /usr/include/sys/_types/_dev_t.h
s_gqpahv.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqpahv.o: /usr/include/sys/_types/_blksize_t.h
s_gqpahv.o: /usr/include/sys/_types/_gid_t.h
s_gqpahv.o: /usr/include/sys/_types/_in_addr_t.h
s_gqpahv.o: /usr/include/sys/_types/_in_port_t.h
s_gqpahv.o: /usr/include/sys/_types/_ino_t.h
s_gqpahv.o: /usr/include/sys/_types/_ino64_t.h
s_gqpahv.o: /usr/include/sys/_types/_key_t.h
s_gqpahv.o: /usr/include/sys/_types/_mode_t.h
s_gqpahv.o: /usr/include/sys/_types/_nlink_t.h
s_gqpahv.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gqpahv.o: /usr/include/sys/_types/_uid_t.h
s_gqpahv.o: /usr/include/sys/_types/_clock_t.h
s_gqpahv.o: /usr/include/sys/_types/_time_t.h
s_gqpahv.o: /usr/include/sys/_types/_useconds_t.h
s_gqpahv.o: /usr/include/sys/_types/_suseconds_t.h
s_gqpahv.o: /usr/include/sys/_types/_rsize_t.h
s_gqpahv.o: /usr/include/sys/_types/_errno_t.h
s_gqpahv.o: /usr/include/sys/_types/_fd_def.h
s_gqpahv.o: /usr/include/sys/_types/_fd_setsize.h
s_gqpahv.o: /usr/include/sys/_types/_fd_set.h
s_gqpahv.o: /usr/include/sys/_types/_fd_clr.h
s_gqpahv.o: /usr/include/sys/_types/_fd_zero.h
s_gqpahv.o: /usr/include/sys/_types/_fd_isset.h
s_gqpahv.o: /usr/include/sys/_types/_fd_copy.h
s_gqpahv.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqpahv.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqpahv.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqpahv.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqpahv.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqpahv.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqpahv.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqpahv.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqpahv.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqpahv.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqpahv.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqpahv.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqpahv.o: ../../../.././include/ncarg/c.h
s_gqparf.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqparf.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqparf.o: /usr/include/sys/_posix_availability.h
s_gqparf.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gqparf.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gqparf.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gqparf.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqparf.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqparf.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqparf.o: /usr/include/sys/_types/_int16_t.h
s_gqparf.o: /usr/include/sys/_types/_int32_t.h
s_gqparf.o: /usr/include/sys/_types/_int64_t.h
s_gqparf.o: /usr/include/sys/_types/_u_int8_t.h
s_gqparf.o: /usr/include/sys/_types/_u_int16_t.h
s_gqparf.o: /usr/include/sys/_types/_u_int32_t.h
s_gqparf.o: /usr/include/sys/_types/_u_int64_t.h
s_gqparf.o: /usr/include/sys/_types/_intptr_t.h
s_gqparf.o: /usr/include/sys/_types/_uintptr_t.h
s_gqparf.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqparf.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqparf.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqparf.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqparf.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqparf.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqparf.o: /usr/include/libkern/_OSByteOrder.h
s_gqparf.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqparf.o: /usr/include/sys/_types/_u_char.h
s_gqparf.o: /usr/include/sys/_types/_u_short.h
s_gqparf.o: /usr/include/sys/_types/_u_int.h
s_gqparf.o: /usr/include/sys/_types/_caddr_t.h
s_gqparf.o: /usr/include/sys/_types/_dev_t.h
s_gqparf.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqparf.o: /usr/include/sys/_types/_blksize_t.h
s_gqparf.o: /usr/include/sys/_types/_gid_t.h
s_gqparf.o: /usr/include/sys/_types/_in_addr_t.h
s_gqparf.o: /usr/include/sys/_types/_in_port_t.h
s_gqparf.o: /usr/include/sys/_types/_ino_t.h
s_gqparf.o: /usr/include/sys/_types/_ino64_t.h
s_gqparf.o: /usr/include/sys/_types/_key_t.h
s_gqparf.o: /usr/include/sys/_types/_mode_t.h
s_gqparf.o: /usr/include/sys/_types/_nlink_t.h
s_gqparf.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gqparf.o: /usr/include/sys/_types/_uid_t.h
s_gqparf.o: /usr/include/sys/_types/_clock_t.h
s_gqparf.o: /usr/include/sys/_types/_time_t.h
s_gqparf.o: /usr/include/sys/_types/_useconds_t.h
s_gqparf.o: /usr/include/sys/_types/_suseconds_t.h
s_gqparf.o: /usr/include/sys/_types/_rsize_t.h
s_gqparf.o: /usr/include/sys/_types/_errno_t.h
s_gqparf.o: /usr/include/sys/_types/_fd_def.h
s_gqparf.o: /usr/include/sys/_types/_fd_setsize.h
s_gqparf.o: /usr/include/sys/_types/_fd_set.h
s_gqparf.o: /usr/include/sys/_types/_fd_clr.h
s_gqparf.o: /usr/include/sys/_types/_fd_zero.h
s_gqparf.o: /usr/include/sys/_types/_fd_isset.h
s_gqparf.o: /usr/include/sys/_types/_fd_copy.h
s_gqparf.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqparf.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqparf.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqparf.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqparf.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqparf.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqparf.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqparf.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqparf.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqparf.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqparf.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqparf.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqparf.o: ../../../.././include/ncarg/c.h
s_gqpawv.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqpawv.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqpawv.o: /usr/include/sys/_posix_availability.h
s_gqpawv.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gqpawv.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gqpawv.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gqpawv.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqpawv.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqpawv.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqpawv.o: /usr/include/sys/_types/_int16_t.h
s_gqpawv.o: /usr/include/sys/_types/_int32_t.h
s_gqpawv.o: /usr/include/sys/_types/_int64_t.h
s_gqpawv.o: /usr/include/sys/_types/_u_int8_t.h
s_gqpawv.o: /usr/include/sys/_types/_u_int16_t.h
s_gqpawv.o: /usr/include/sys/_types/_u_int32_t.h
s_gqpawv.o: /usr/include/sys/_types/_u_int64_t.h
s_gqpawv.o: /usr/include/sys/_types/_intptr_t.h
s_gqpawv.o: /usr/include/sys/_types/_uintptr_t.h
s_gqpawv.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqpawv.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqpawv.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqpawv.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqpawv.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqpawv.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqpawv.o: /usr/include/libkern/_OSByteOrder.h
s_gqpawv.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqpawv.o: /usr/include/sys/_types/_u_char.h
s_gqpawv.o: /usr/include/sys/_types/_u_short.h
s_gqpawv.o: /usr/include/sys/_types/_u_int.h
s_gqpawv.o: /usr/include/sys/_types/_caddr_t.h
s_gqpawv.o: /usr/include/sys/_types/_dev_t.h
s_gqpawv.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqpawv.o: /usr/include/sys/_types/_blksize_t.h
s_gqpawv.o: /usr/include/sys/_types/_gid_t.h
s_gqpawv.o: /usr/include/sys/_types/_in_addr_t.h
s_gqpawv.o: /usr/include/sys/_types/_in_port_t.h
s_gqpawv.o: /usr/include/sys/_types/_ino_t.h
s_gqpawv.o: /usr/include/sys/_types/_ino64_t.h
s_gqpawv.o: /usr/include/sys/_types/_key_t.h
s_gqpawv.o: /usr/include/sys/_types/_mode_t.h
s_gqpawv.o: /usr/include/sys/_types/_nlink_t.h
s_gqpawv.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gqpawv.o: /usr/include/sys/_types/_uid_t.h
s_gqpawv.o: /usr/include/sys/_types/_clock_t.h
s_gqpawv.o: /usr/include/sys/_types/_time_t.h
s_gqpawv.o: /usr/include/sys/_types/_useconds_t.h
s_gqpawv.o: /usr/include/sys/_types/_suseconds_t.h
s_gqpawv.o: /usr/include/sys/_types/_rsize_t.h
s_gqpawv.o: /usr/include/sys/_types/_errno_t.h
s_gqpawv.o: /usr/include/sys/_types/_fd_def.h
s_gqpawv.o: /usr/include/sys/_types/_fd_setsize.h
s_gqpawv.o: /usr/include/sys/_types/_fd_set.h
s_gqpawv.o: /usr/include/sys/_types/_fd_clr.h
s_gqpawv.o: /usr/include/sys/_types/_fd_zero.h
s_gqpawv.o: /usr/include/sys/_types/_fd_isset.h
s_gqpawv.o: /usr/include/sys/_types/_fd_copy.h
s_gqpawv.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqpawv.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqpawv.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqpawv.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqpawv.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqpawv.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqpawv.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqpawv.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqpawv.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqpawv.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqpawv.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqpawv.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqpawv.o: ../../../.././include/ncarg/c.h
s_gqpcr.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqpcr.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqpcr.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gqpcr.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gqpcr.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gqpcr.o: /usr/include/i386/_types.h
s_gqpcr.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqpcr.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqpcr.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqpcr.o: /usr/include/sys/_types/_int16_t.h
s_gqpcr.o: /usr/include/sys/_types/_int32_t.h
s_gqpcr.o: /usr/include/sys/_types/_int64_t.h
s_gqpcr.o: /usr/include/sys/_types/_u_int8_t.h
s_gqpcr.o: /usr/include/sys/_types/_u_int16_t.h
s_gqpcr.o: /usr/include/sys/_types/_u_int32_t.h
s_gqpcr.o: /usr/include/sys/_types/_u_int64_t.h
s_gqpcr.o: /usr/include/sys/_types/_intptr_t.h
s_gqpcr.o: /usr/include/sys/_types/_uintptr_t.h
s_gqpcr.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqpcr.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqpcr.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqpcr.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqpcr.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqpcr.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqpcr.o: /usr/include/libkern/_OSByteOrder.h
s_gqpcr.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqpcr.o: /usr/include/sys/_types/_u_char.h
s_gqpcr.o: /usr/include/sys/_types/_u_short.h
s_gqpcr.o: /usr/include/sys/_types/_u_int.h
s_gqpcr.o: /usr/include/sys/_types/_caddr_t.h
s_gqpcr.o: /usr/include/sys/_types/_dev_t.h
s_gqpcr.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqpcr.o: /usr/include/sys/_types/_blksize_t.h
s_gqpcr.o: /usr/include/sys/_types/_gid_t.h
s_gqpcr.o: /usr/include/sys/_types/_in_addr_t.h
s_gqpcr.o: /usr/include/sys/_types/_in_port_t.h
s_gqpcr.o: /usr/include/sys/_types/_ino_t.h
s_gqpcr.o: /usr/include/sys/_types/_ino64_t.h
s_gqpcr.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gqpcr.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gqpcr.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gqpcr.o: /usr/include/sys/_types/_clock_t.h
s_gqpcr.o: /usr/include/sys/_types/_time_t.h
s_gqpcr.o: /usr/include/sys/_types/_useconds_t.h
s_gqpcr.o: /usr/include/sys/_types/_suseconds_t.h
s_gqpcr.o: /usr/include/sys/_types/_rsize_t.h
s_gqpcr.o: /usr/include/sys/_types/_errno_t.h
s_gqpcr.o: /usr/include/sys/_types/_fd_def.h
s_gqpcr.o: /usr/include/sys/_types/_fd_setsize.h
s_gqpcr.o: /usr/include/sys/_types/_fd_set.h
s_gqpcr.o: /usr/include/sys/_types/_fd_clr.h
s_gqpcr.o: /usr/include/sys/_types/_fd_zero.h
s_gqpcr.o: /usr/include/sys/_types/_fd_isset.h
s_gqpcr.o: /usr/include/sys/_types/_fd_copy.h
s_gqpcr.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqpcr.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqpcr.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqpcr.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqpcr.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqpcr.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqpcr.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqpcr.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqpcr.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqpcr.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqpcr.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqpcr.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqpcr.o: ../../../.././include/ncarg/c.h
s_gqpfar.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqpfar.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqpfar.o: /usr/include/sys/_posix_availability.h
s_gqpfar.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gqpfar.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gqpfar.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gqpfar.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqpfar.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqpfar.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqpfar.o: /usr/include/sys/_types/_int16_t.h
s_gqpfar.o: /usr/include/sys/_types/_int32_t.h
s_gqpfar.o: /usr/include/sys/_types/_int64_t.h
s_gqpfar.o: /usr/include/sys/_types/_u_int8_t.h
s_gqpfar.o: /usr/include/sys/_types/_u_int16_t.h
s_gqpfar.o: /usr/include/sys/_types/_u_int32_t.h
s_gqpfar.o: /usr/include/sys/_types/_u_int64_t.h
s_gqpfar.o: /usr/include/sys/_types/_intptr_t.h
s_gqpfar.o: /usr/include/sys/_types/_uintptr_t.h
s_gqpfar.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqpfar.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqpfar.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqpfar.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqpfar.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqpfar.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqpfar.o: /usr/include/libkern/_OSByteOrder.h
s_gqpfar.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqpfar.o: /usr/include/sys/_types/_u_char.h
s_gqpfar.o: /usr/include/sys/_types/_u_short.h
s_gqpfar.o: /usr/include/sys/_types/_u_int.h
s_gqpfar.o: /usr/include/sys/_types/_caddr_t.h
s_gqpfar.o: /usr/include/sys/_types/_dev_t.h
s_gqpfar.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqpfar.o: /usr/include/sys/_types/_blksize_t.h
s_gqpfar.o: /usr/include/sys/_types/_gid_t.h
s_gqpfar.o: /usr/include/sys/_types/_in_addr_t.h
s_gqpfar.o: /usr/include/sys/_types/_in_port_t.h
s_gqpfar.o: /usr/include/sys/_types/_ino_t.h
s_gqpfar.o: /usr/include/sys/_types/_ino64_t.h
s_gqpfar.o: /usr/include/sys/_types/_key_t.h
s_gqpfar.o: /usr/include/sys/_types/_mode_t.h
s_gqpfar.o: /usr/include/sys/_types/_nlink_t.h
s_gqpfar.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gqpfar.o: /usr/include/sys/_types/_uid_t.h
s_gqpfar.o: /usr/include/sys/_types/_clock_t.h
s_gqpfar.o: /usr/include/sys/_types/_time_t.h
s_gqpfar.o: /usr/include/sys/_types/_useconds_t.h
s_gqpfar.o: /usr/include/sys/_types/_suseconds_t.h
s_gqpfar.o: /usr/include/sys/_types/_rsize_t.h
s_gqpfar.o: /usr/include/sys/_types/_errno_t.h
s_gqpfar.o: /usr/include/sys/_types/_fd_def.h
s_gqpfar.o: /usr/include/sys/_types/_fd_setsize.h
s_gqpfar.o: /usr/include/sys/_types/_fd_set.h
s_gqpfar.o: /usr/include/sys/_types/_fd_clr.h
s_gqpfar.o: /usr/include/sys/_types/_fd_zero.h
s_gqpfar.o: /usr/include/sys/_types/_fd_isset.h
s_gqpfar.o: /usr/include/sys/_types/_fd_copy.h
s_gqpfar.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqpfar.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqpfar.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqpfar.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqpfar.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqpfar.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqpfar.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqpfar.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqpfar.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqpfar.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqpfar.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqpfar.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqpfar.o: ../../../.././include/ncarg/c.h
s_gqplci.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqplci.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqplci.o: /usr/include/sys/_posix_availability.h
s_gqplci.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gqplci.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gqplci.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gqplci.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqplci.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqplci.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqplci.o: /usr/include/sys/_types/_int16_t.h
s_gqplci.o: /usr/include/sys/_types/_int32_t.h
s_gqplci.o: /usr/include/sys/_types/_int64_t.h
s_gqplci.o: /usr/include/sys/_types/_u_int8_t.h
s_gqplci.o: /usr/include/sys/_types/_u_int16_t.h
s_gqplci.o: /usr/include/sys/_types/_u_int32_t.h
s_gqplci.o: /usr/include/sys/_types/_u_int64_t.h
s_gqplci.o: /usr/include/sys/_types/_intptr_t.h
s_gqplci.o: /usr/include/sys/_types/_uintptr_t.h
s_gqplci.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqplci.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqplci.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqplci.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqplci.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqplci.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqplci.o: /usr/include/libkern/_OSByteOrder.h
s_gqplci.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqplci.o: /usr/include/sys/_types/_u_char.h
s_gqplci.o: /usr/include/sys/_types/_u_short.h
s_gqplci.o: /usr/include/sys/_types/_u_int.h
s_gqplci.o: /usr/include/sys/_types/_caddr_t.h
s_gqplci.o: /usr/include/sys/_types/_dev_t.h
s_gqplci.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqplci.o: /usr/include/sys/_types/_blksize_t.h
s_gqplci.o: /usr/include/sys/_types/_gid_t.h
s_gqplci.o: /usr/include/sys/_types/_in_addr_t.h
s_gqplci.o: /usr/include/sys/_types/_in_port_t.h
s_gqplci.o: /usr/include/sys/_types/_ino_t.h
s_gqplci.o: /usr/include/sys/_types/_ino64_t.h
s_gqplci.o: /usr/include/sys/_types/_key_t.h
s_gqplci.o: /usr/include/sys/_types/_mode_t.h
s_gqplci.o: /usr/include/sys/_types/_nlink_t.h
s_gqplci.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gqplci.o: /usr/include/sys/_types/_uid_t.h
s_gqplci.o: /usr/include/sys/_types/_clock_t.h
s_gqplci.o: /usr/include/sys/_types/_time_t.h
s_gqplci.o: /usr/include/sys/_types/_useconds_t.h
s_gqplci.o: /usr/include/sys/_types/_suseconds_t.h
s_gqplci.o: /usr/include/sys/_types/_rsize_t.h
s_gqplci.o: /usr/include/sys/_types/_errno_t.h
s_gqplci.o: /usr/include/sys/_types/_fd_def.h
s_gqplci.o: /usr/include/sys/_types/_fd_setsize.h
s_gqplci.o: /usr/include/sys/_types/_fd_set.h
s_gqplci.o: /usr/include/sys/_types/_fd_clr.h
s_gqplci.o: /usr/include/sys/_types/_fd_zero.h
s_gqplci.o: /usr/include/sys/_types/_fd_isset.h
s_gqplci.o: /usr/include/sys/_types/_fd_copy.h
s_gqplci.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqplci.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqplci.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqplci.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqplci.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqplci.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqplci.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqplci.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqplci.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqplci.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqplci.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqplci.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqplci.o: ../../../.././include/ncarg/c.h
s_gqplf.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqplf.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqplf.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gqplf.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gqplf.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gqplf.o: /usr/include/i386/_types.h
s_gqplf.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqplf.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqplf.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqplf.o: /usr/include/sys/_types/_int16_t.h
s_gqplf.o: /usr/include/sys/_types/_int32_t.h
s_gqplf.o: /usr/include/sys/_types/_int64_t.h
s_gqplf.o: /usr/include/sys/_types/_u_int8_t.h
s_gqplf.o: /usr/include/sys/_types/_u_int16_t.h
s_gqplf.o: /usr/include/sys/_types/_u_int32_t.h
s_gqplf.o: /usr/include/sys/_types/_u_int64_t.h
s_gqplf.o: /usr/include/sys/_types/_intptr_t.h
s_gqplf.o: /usr/include/sys/_types/_uintptr_t.h
s_gqplf.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqplf.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqplf.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqplf.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqplf.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqplf.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqplf.o: /usr/include/libkern/_OSByteOrder.h
s_gqplf.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqplf.o: /usr/include/sys/_types/_u_char.h
s_gqplf.o: /usr/include/sys/_types/_u_short.h
s_gqplf.o: /usr/include/sys/_types/_u_int.h
s_gqplf.o: /usr/include/sys/_types/_caddr_t.h
s_gqplf.o: /usr/include/sys/_types/_dev_t.h
s_gqplf.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqplf.o: /usr/include/sys/_types/_blksize_t.h
s_gqplf.o: /usr/include/sys/_types/_gid_t.h
s_gqplf.o: /usr/include/sys/_types/_in_addr_t.h
s_gqplf.o: /usr/include/sys/_types/_in_port_t.h
s_gqplf.o: /usr/include/sys/_types/_ino_t.h
s_gqplf.o: /usr/include/sys/_types/_ino64_t.h
s_gqplf.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gqplf.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gqplf.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gqplf.o: /usr/include/sys/_types/_clock_t.h
s_gqplf.o: /usr/include/sys/_types/_time_t.h
s_gqplf.o: /usr/include/sys/_types/_useconds_t.h
s_gqplf.o: /usr/include/sys/_types/_suseconds_t.h
s_gqplf.o: /usr/include/sys/_types/_rsize_t.h
s_gqplf.o: /usr/include/sys/_types/_errno_t.h
s_gqplf.o: /usr/include/sys/_types/_fd_def.h
s_gqplf.o: /usr/include/sys/_types/_fd_setsize.h
s_gqplf.o: /usr/include/sys/_types/_fd_set.h
s_gqplf.o: /usr/include/sys/_types/_fd_clr.h
s_gqplf.o: /usr/include/sys/_types/_fd_zero.h
s_gqplf.o: /usr/include/sys/_types/_fd_isset.h
s_gqplf.o: /usr/include/sys/_types/_fd_copy.h
s_gqplf.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqplf.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqplf.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqplf.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqplf.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqplf.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqplf.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqplf.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqplf.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqplf.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqplf.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqplf.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqplf.o: ../../../.././include/ncarg/c.h
s_gqpli.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqpli.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqpli.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gqpli.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gqpli.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gqpli.o: /usr/include/i386/_types.h
s_gqpli.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqpli.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqpli.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqpli.o: /usr/include/sys/_types/_int16_t.h
s_gqpli.o: /usr/include/sys/_types/_int32_t.h
s_gqpli.o: /usr/include/sys/_types/_int64_t.h
s_gqpli.o: /usr/include/sys/_types/_u_int8_t.h
s_gqpli.o: /usr/include/sys/_types/_u_int16_t.h
s_gqpli.o: /usr/include/sys/_types/_u_int32_t.h
s_gqpli.o: /usr/include/sys/_types/_u_int64_t.h
s_gqpli.o: /usr/include/sys/_types/_intptr_t.h
s_gqpli.o: /usr/include/sys/_types/_uintptr_t.h
s_gqpli.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqpli.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqpli.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqpli.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqpli.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqpli.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqpli.o: /usr/include/libkern/_OSByteOrder.h
s_gqpli.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqpli.o: /usr/include/sys/_types/_u_char.h
s_gqpli.o: /usr/include/sys/_types/_u_short.h
s_gqpli.o: /usr/include/sys/_types/_u_int.h
s_gqpli.o: /usr/include/sys/_types/_caddr_t.h
s_gqpli.o: /usr/include/sys/_types/_dev_t.h
s_gqpli.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqpli.o: /usr/include/sys/_types/_blksize_t.h
s_gqpli.o: /usr/include/sys/_types/_gid_t.h
s_gqpli.o: /usr/include/sys/_types/_in_addr_t.h
s_gqpli.o: /usr/include/sys/_types/_in_port_t.h
s_gqpli.o: /usr/include/sys/_types/_ino_t.h
s_gqpli.o: /usr/include/sys/_types/_ino64_t.h
s_gqpli.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gqpli.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gqpli.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gqpli.o: /usr/include/sys/_types/_clock_t.h
s_gqpli.o: /usr/include/sys/_types/_time_t.h
s_gqpli.o: /usr/include/sys/_types/_useconds_t.h
s_gqpli.o: /usr/include/sys/_types/_suseconds_t.h
s_gqpli.o: /usr/include/sys/_types/_rsize_t.h
s_gqpli.o: /usr/include/sys/_types/_errno_t.h
s_gqpli.o: /usr/include/sys/_types/_fd_def.h
s_gqpli.o: /usr/include/sys/_types/_fd_setsize.h
s_gqpli.o: /usr/include/sys/_types/_fd_set.h
s_gqpli.o: /usr/include/sys/_types/_fd_clr.h
s_gqpli.o: /usr/include/sys/_types/_fd_zero.h
s_gqpli.o: /usr/include/sys/_types/_fd_isset.h
s_gqpli.o: /usr/include/sys/_types/_fd_copy.h
s_gqpli.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqpli.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqpli.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqpli.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqpli.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqpli.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqpli.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqpli.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqpli.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqpli.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqpli.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqpli.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqpli.o: ../../../.././include/ncarg/c.h
s_gqpmci.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqpmci.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqpmci.o: /usr/include/sys/_posix_availability.h
s_gqpmci.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gqpmci.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gqpmci.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gqpmci.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqpmci.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqpmci.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqpmci.o: /usr/include/sys/_types/_int16_t.h
s_gqpmci.o: /usr/include/sys/_types/_int32_t.h
s_gqpmci.o: /usr/include/sys/_types/_int64_t.h
s_gqpmci.o: /usr/include/sys/_types/_u_int8_t.h
s_gqpmci.o: /usr/include/sys/_types/_u_int16_t.h
s_gqpmci.o: /usr/include/sys/_types/_u_int32_t.h
s_gqpmci.o: /usr/include/sys/_types/_u_int64_t.h
s_gqpmci.o: /usr/include/sys/_types/_intptr_t.h
s_gqpmci.o: /usr/include/sys/_types/_uintptr_t.h
s_gqpmci.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqpmci.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqpmci.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqpmci.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqpmci.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqpmci.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqpmci.o: /usr/include/libkern/_OSByteOrder.h
s_gqpmci.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqpmci.o: /usr/include/sys/_types/_u_char.h
s_gqpmci.o: /usr/include/sys/_types/_u_short.h
s_gqpmci.o: /usr/include/sys/_types/_u_int.h
s_gqpmci.o: /usr/include/sys/_types/_caddr_t.h
s_gqpmci.o: /usr/include/sys/_types/_dev_t.h
s_gqpmci.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqpmci.o: /usr/include/sys/_types/_blksize_t.h
s_gqpmci.o: /usr/include/sys/_types/_gid_t.h
s_gqpmci.o: /usr/include/sys/_types/_in_addr_t.h
s_gqpmci.o: /usr/include/sys/_types/_in_port_t.h
s_gqpmci.o: /usr/include/sys/_types/_ino_t.h
s_gqpmci.o: /usr/include/sys/_types/_ino64_t.h
s_gqpmci.o: /usr/include/sys/_types/_key_t.h
s_gqpmci.o: /usr/include/sys/_types/_mode_t.h
s_gqpmci.o: /usr/include/sys/_types/_nlink_t.h
s_gqpmci.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gqpmci.o: /usr/include/sys/_types/_uid_t.h
s_gqpmci.o: /usr/include/sys/_types/_clock_t.h
s_gqpmci.o: /usr/include/sys/_types/_time_t.h
s_gqpmci.o: /usr/include/sys/_types/_useconds_t.h
s_gqpmci.o: /usr/include/sys/_types/_suseconds_t.h
s_gqpmci.o: /usr/include/sys/_types/_rsize_t.h
s_gqpmci.o: /usr/include/sys/_types/_errno_t.h
s_gqpmci.o: /usr/include/sys/_types/_fd_def.h
s_gqpmci.o: /usr/include/sys/_types/_fd_setsize.h
s_gqpmci.o: /usr/include/sys/_types/_fd_set.h
s_gqpmci.o: /usr/include/sys/_types/_fd_clr.h
s_gqpmci.o: /usr/include/sys/_types/_fd_zero.h
s_gqpmci.o: /usr/include/sys/_types/_fd_isset.h
s_gqpmci.o: /usr/include/sys/_types/_fd_copy.h
s_gqpmci.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqpmci.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqpmci.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqpmci.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqpmci.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqpmci.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqpmci.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqpmci.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqpmci.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqpmci.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqpmci.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqpmci.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqpmci.o: ../../../.././include/ncarg/c.h
s_gqpmf.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqpmf.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqpmf.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gqpmf.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gqpmf.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gqpmf.o: /usr/include/i386/_types.h
s_gqpmf.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqpmf.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqpmf.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqpmf.o: /usr/include/sys/_types/_int16_t.h
s_gqpmf.o: /usr/include/sys/_types/_int32_t.h
s_gqpmf.o: /usr/include/sys/_types/_int64_t.h
s_gqpmf.o: /usr/include/sys/_types/_u_int8_t.h
s_gqpmf.o: /usr/include/sys/_types/_u_int16_t.h
s_gqpmf.o: /usr/include/sys/_types/_u_int32_t.h
s_gqpmf.o: /usr/include/sys/_types/_u_int64_t.h
s_gqpmf.o: /usr/include/sys/_types/_intptr_t.h
s_gqpmf.o: /usr/include/sys/_types/_uintptr_t.h
s_gqpmf.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqpmf.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqpmf.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqpmf.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqpmf.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqpmf.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqpmf.o: /usr/include/libkern/_OSByteOrder.h
s_gqpmf.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqpmf.o: /usr/include/sys/_types/_u_char.h
s_gqpmf.o: /usr/include/sys/_types/_u_short.h
s_gqpmf.o: /usr/include/sys/_types/_u_int.h
s_gqpmf.o: /usr/include/sys/_types/_caddr_t.h
s_gqpmf.o: /usr/include/sys/_types/_dev_t.h
s_gqpmf.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqpmf.o: /usr/include/sys/_types/_blksize_t.h
s_gqpmf.o: /usr/include/sys/_types/_gid_t.h
s_gqpmf.o: /usr/include/sys/_types/_in_addr_t.h
s_gqpmf.o: /usr/include/sys/_types/_in_port_t.h
s_gqpmf.o: /usr/include/sys/_types/_ino_t.h
s_gqpmf.o: /usr/include/sys/_types/_ino64_t.h
s_gqpmf.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gqpmf.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gqpmf.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gqpmf.o: /usr/include/sys/_types/_clock_t.h
s_gqpmf.o: /usr/include/sys/_types/_time_t.h
s_gqpmf.o: /usr/include/sys/_types/_useconds_t.h
s_gqpmf.o: /usr/include/sys/_types/_suseconds_t.h
s_gqpmf.o: /usr/include/sys/_types/_rsize_t.h
s_gqpmf.o: /usr/include/sys/_types/_errno_t.h
s_gqpmf.o: /usr/include/sys/_types/_fd_def.h
s_gqpmf.o: /usr/include/sys/_types/_fd_setsize.h
s_gqpmf.o: /usr/include/sys/_types/_fd_set.h
s_gqpmf.o: /usr/include/sys/_types/_fd_clr.h
s_gqpmf.o: /usr/include/sys/_types/_fd_zero.h
s_gqpmf.o: /usr/include/sys/_types/_fd_isset.h
s_gqpmf.o: /usr/include/sys/_types/_fd_copy.h
s_gqpmf.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqpmf.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqpmf.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqpmf.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqpmf.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqpmf.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqpmf.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqpmf.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqpmf.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqpmf.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqpmf.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqpmf.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqpmf.o: ../../../.././include/ncarg/c.h
s_gqpmi.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqpmi.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqpmi.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gqpmi.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gqpmi.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gqpmi.o: /usr/include/i386/_types.h
s_gqpmi.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqpmi.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqpmi.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqpmi.o: /usr/include/sys/_types/_int16_t.h
s_gqpmi.o: /usr/include/sys/_types/_int32_t.h
s_gqpmi.o: /usr/include/sys/_types/_int64_t.h
s_gqpmi.o: /usr/include/sys/_types/_u_int8_t.h
s_gqpmi.o: /usr/include/sys/_types/_u_int16_t.h
s_gqpmi.o: /usr/include/sys/_types/_u_int32_t.h
s_gqpmi.o: /usr/include/sys/_types/_u_int64_t.h
s_gqpmi.o: /usr/include/sys/_types/_intptr_t.h
s_gqpmi.o: /usr/include/sys/_types/_uintptr_t.h
s_gqpmi.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqpmi.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqpmi.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqpmi.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqpmi.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqpmi.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqpmi.o: /usr/include/libkern/_OSByteOrder.h
s_gqpmi.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqpmi.o: /usr/include/sys/_types/_u_char.h
s_gqpmi.o: /usr/include/sys/_types/_u_short.h
s_gqpmi.o: /usr/include/sys/_types/_u_int.h
s_gqpmi.o: /usr/include/sys/_types/_caddr_t.h
s_gqpmi.o: /usr/include/sys/_types/_dev_t.h
s_gqpmi.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqpmi.o: /usr/include/sys/_types/_blksize_t.h
s_gqpmi.o: /usr/include/sys/_types/_gid_t.h
s_gqpmi.o: /usr/include/sys/_types/_in_addr_t.h
s_gqpmi.o: /usr/include/sys/_types/_in_port_t.h
s_gqpmi.o: /usr/include/sys/_types/_ino_t.h
s_gqpmi.o: /usr/include/sys/_types/_ino64_t.h
s_gqpmi.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gqpmi.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gqpmi.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gqpmi.o: /usr/include/sys/_types/_clock_t.h
s_gqpmi.o: /usr/include/sys/_types/_time_t.h
s_gqpmi.o: /usr/include/sys/_types/_useconds_t.h
s_gqpmi.o: /usr/include/sys/_types/_suseconds_t.h
s_gqpmi.o: /usr/include/sys/_types/_rsize_t.h
s_gqpmi.o: /usr/include/sys/_types/_errno_t.h
s_gqpmi.o: /usr/include/sys/_types/_fd_def.h
s_gqpmi.o: /usr/include/sys/_types/_fd_setsize.h
s_gqpmi.o: /usr/include/sys/_types/_fd_set.h
s_gqpmi.o: /usr/include/sys/_types/_fd_clr.h
s_gqpmi.o: /usr/include/sys/_types/_fd_zero.h
s_gqpmi.o: /usr/include/sys/_types/_fd_isset.h
s_gqpmi.o: /usr/include/sys/_types/_fd_copy.h
s_gqpmi.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqpmi.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqpmi.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqpmi.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqpmi.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqpmi.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqpmi.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqpmi.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqpmi.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqpmi.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqpmi.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqpmi.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqpmi.o: ../../../.././include/ncarg/c.h
s_gqppar.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqppar.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqppar.o: /usr/include/sys/_posix_availability.h
s_gqppar.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gqppar.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gqppar.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gqppar.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqppar.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqppar.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqppar.o: /usr/include/sys/_types/_int16_t.h
s_gqppar.o: /usr/include/sys/_types/_int32_t.h
s_gqppar.o: /usr/include/sys/_types/_int64_t.h
s_gqppar.o: /usr/include/sys/_types/_u_int8_t.h
s_gqppar.o: /usr/include/sys/_types/_u_int16_t.h
s_gqppar.o: /usr/include/sys/_types/_u_int32_t.h
s_gqppar.o: /usr/include/sys/_types/_u_int64_t.h
s_gqppar.o: /usr/include/sys/_types/_intptr_t.h
s_gqppar.o: /usr/include/sys/_types/_uintptr_t.h
s_gqppar.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqppar.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqppar.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqppar.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqppar.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqppar.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqppar.o: /usr/include/libkern/_OSByteOrder.h
s_gqppar.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqppar.o: /usr/include/sys/_types/_u_char.h
s_gqppar.o: /usr/include/sys/_types/_u_short.h
s_gqppar.o: /usr/include/sys/_types/_u_int.h
s_gqppar.o: /usr/include/sys/_types/_caddr_t.h
s_gqppar.o: /usr/include/sys/_types/_dev_t.h
s_gqppar.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqppar.o: /usr/include/sys/_types/_blksize_t.h
s_gqppar.o: /usr/include/sys/_types/_gid_t.h
s_gqppar.o: /usr/include/sys/_types/_in_addr_t.h
s_gqppar.o: /usr/include/sys/_types/_in_port_t.h
s_gqppar.o: /usr/include/sys/_types/_ino_t.h
s_gqppar.o: /usr/include/sys/_types/_ino64_t.h
s_gqppar.o: /usr/include/sys/_types/_key_t.h
s_gqppar.o: /usr/include/sys/_types/_mode_t.h
s_gqppar.o: /usr/include/sys/_types/_nlink_t.h
s_gqppar.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gqppar.o: /usr/include/sys/_types/_uid_t.h
s_gqppar.o: /usr/include/sys/_types/_clock_t.h
s_gqppar.o: /usr/include/sys/_types/_time_t.h
s_gqppar.o: /usr/include/sys/_types/_useconds_t.h
s_gqppar.o: /usr/include/sys/_types/_suseconds_t.h
s_gqppar.o: /usr/include/sys/_types/_rsize_t.h
s_gqppar.o: /usr/include/sys/_types/_errno_t.h
s_gqppar.o: /usr/include/sys/_types/_fd_def.h
s_gqppar.o: /usr/include/sys/_types/_fd_setsize.h
s_gqppar.o: /usr/include/sys/_types/_fd_set.h
s_gqppar.o: /usr/include/sys/_types/_fd_clr.h
s_gqppar.o: /usr/include/sys/_types/_fd_zero.h
s_gqppar.o: /usr/include/sys/_types/_fd_isset.h
s_gqppar.o: /usr/include/sys/_types/_fd_copy.h
s_gqppar.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqppar.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqppar.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqppar.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqppar.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqppar.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqppar.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqppar.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqppar.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqppar.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqppar.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqppar.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqppar.o: ../../../.././include/ncarg/c.h /usr/include/stdlib.h
s_gqppar.o: /usr/include/sys/wait.h /usr/include/sys/signal.h
s_gqppar.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
s_gqppar.o: /usr/include/machine/_mcontext.h /usr/include/i386/_mcontext.h
s_gqppar.o: /usr/include/mach/machine/_structs.h
s_gqppar.o: /usr/include/mach/i386/_structs.h
s_gqppar.o: /usr/include/sys/_types/_sigaltstack.h
s_gqppar.o: /usr/include/sys/_types/_ucontext.h
s_gqppar.o: /usr/include/sys/_types/_sigset_t.h /usr/include/sys/resource.h
s_gqppar.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
s_gqppar.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
s_gqppar.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
s_gqppar.o: /usr/include/_types/_uintmax_t.h
s_gqppar.o: /usr/include/sys/_types/_timeval.h /usr/include/alloca.h
s_gqppar.o: /usr/include/sys/_types/_ct_rune_t.h
s_gqppar.o: /usr/include/sys/_types/_rune_t.h
s_gqppar.o: /usr/include/sys/_types/_wchar_t.h
s_gqpplr.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqpplr.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqpplr.o: /usr/include/sys/_posix_availability.h
s_gqpplr.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gqpplr.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gqpplr.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gqpplr.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqpplr.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqpplr.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqpplr.o: /usr/include/sys/_types/_int16_t.h
s_gqpplr.o: /usr/include/sys/_types/_int32_t.h
s_gqpplr.o: /usr/include/sys/_types/_int64_t.h
s_gqpplr.o: /usr/include/sys/_types/_u_int8_t.h
s_gqpplr.o: /usr/include/sys/_types/_u_int16_t.h
s_gqpplr.o: /usr/include/sys/_types/_u_int32_t.h
s_gqpplr.o: /usr/include/sys/_types/_u_int64_t.h
s_gqpplr.o: /usr/include/sys/_types/_intptr_t.h
s_gqpplr.o: /usr/include/sys/_types/_uintptr_t.h
s_gqpplr.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqpplr.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqpplr.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqpplr.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqpplr.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqpplr.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqpplr.o: /usr/include/libkern/_OSByteOrder.h
s_gqpplr.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqpplr.o: /usr/include/sys/_types/_u_char.h
s_gqpplr.o: /usr/include/sys/_types/_u_short.h
s_gqpplr.o: /usr/include/sys/_types/_u_int.h
s_gqpplr.o: /usr/include/sys/_types/_caddr_t.h
s_gqpplr.o: /usr/include/sys/_types/_dev_t.h
s_gqpplr.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqpplr.o: /usr/include/sys/_types/_blksize_t.h
s_gqpplr.o: /usr/include/sys/_types/_gid_t.h
s_gqpplr.o: /usr/include/sys/_types/_in_addr_t.h
s_gqpplr.o: /usr/include/sys/_types/_in_port_t.h
s_gqpplr.o: /usr/include/sys/_types/_ino_t.h
s_gqpplr.o: /usr/include/sys/_types/_ino64_t.h
s_gqpplr.o: /usr/include/sys/_types/_key_t.h
s_gqpplr.o: /usr/include/sys/_types/_mode_t.h
s_gqpplr.o: /usr/include/sys/_types/_nlink_t.h
s_gqpplr.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gqpplr.o: /usr/include/sys/_types/_uid_t.h
s_gqpplr.o: /usr/include/sys/_types/_clock_t.h
s_gqpplr.o: /usr/include/sys/_types/_time_t.h
s_gqpplr.o: /usr/include/sys/_types/_useconds_t.h
s_gqpplr.o: /usr/include/sys/_types/_suseconds_t.h
s_gqpplr.o: /usr/include/sys/_types/_rsize_t.h
s_gqpplr.o: /usr/include/sys/_types/_errno_t.h
s_gqpplr.o: /usr/include/sys/_types/_fd_def.h
s_gqpplr.o: /usr/include/sys/_types/_fd_setsize.h
s_gqpplr.o: /usr/include/sys/_types/_fd_set.h
s_gqpplr.o: /usr/include/sys/_types/_fd_clr.h
s_gqpplr.o: /usr/include/sys/_types/_fd_zero.h
s_gqpplr.o: /usr/include/sys/_types/_fd_isset.h
s_gqpplr.o: /usr/include/sys/_types/_fd_copy.h
s_gqpplr.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqpplr.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqpplr.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqpplr.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqpplr.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqpplr.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqpplr.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqpplr.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqpplr.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqpplr.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqpplr.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqpplr.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqpplr.o: ../../../.././include/ncarg/c.h
s_gqppmr.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqppmr.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqppmr.o: /usr/include/sys/_posix_availability.h
s_gqppmr.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gqppmr.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gqppmr.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gqppmr.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqppmr.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqppmr.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqppmr.o: /usr/include/sys/_types/_int16_t.h
s_gqppmr.o: /usr/include/sys/_types/_int32_t.h
s_gqppmr.o: /usr/include/sys/_types/_int64_t.h
s_gqppmr.o: /usr/include/sys/_types/_u_int8_t.h
s_gqppmr.o: /usr/include/sys/_types/_u_int16_t.h
s_gqppmr.o: /usr/include/sys/_types/_u_int32_t.h
s_gqppmr.o: /usr/include/sys/_types/_u_int64_t.h
s_gqppmr.o: /usr/include/sys/_types/_intptr_t.h
s_gqppmr.o: /usr/include/sys/_types/_uintptr_t.h
s_gqppmr.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqppmr.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqppmr.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqppmr.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqppmr.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqppmr.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqppmr.o: /usr/include/libkern/_OSByteOrder.h
s_gqppmr.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqppmr.o: /usr/include/sys/_types/_u_char.h
s_gqppmr.o: /usr/include/sys/_types/_u_short.h
s_gqppmr.o: /usr/include/sys/_types/_u_int.h
s_gqppmr.o: /usr/include/sys/_types/_caddr_t.h
s_gqppmr.o: /usr/include/sys/_types/_dev_t.h
s_gqppmr.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqppmr.o: /usr/include/sys/_types/_blksize_t.h
s_gqppmr.o: /usr/include/sys/_types/_gid_t.h
s_gqppmr.o: /usr/include/sys/_types/_in_addr_t.h
s_gqppmr.o: /usr/include/sys/_types/_in_port_t.h
s_gqppmr.o: /usr/include/sys/_types/_ino_t.h
s_gqppmr.o: /usr/include/sys/_types/_ino64_t.h
s_gqppmr.o: /usr/include/sys/_types/_key_t.h
s_gqppmr.o: /usr/include/sys/_types/_mode_t.h
s_gqppmr.o: /usr/include/sys/_types/_nlink_t.h
s_gqppmr.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gqppmr.o: /usr/include/sys/_types/_uid_t.h
s_gqppmr.o: /usr/include/sys/_types/_clock_t.h
s_gqppmr.o: /usr/include/sys/_types/_time_t.h
s_gqppmr.o: /usr/include/sys/_types/_useconds_t.h
s_gqppmr.o: /usr/include/sys/_types/_suseconds_t.h
s_gqppmr.o: /usr/include/sys/_types/_rsize_t.h
s_gqppmr.o: /usr/include/sys/_types/_errno_t.h
s_gqppmr.o: /usr/include/sys/_types/_fd_def.h
s_gqppmr.o: /usr/include/sys/_types/_fd_setsize.h
s_gqppmr.o: /usr/include/sys/_types/_fd_set.h
s_gqppmr.o: /usr/include/sys/_types/_fd_clr.h
s_gqppmr.o: /usr/include/sys/_types/_fd_zero.h
s_gqppmr.o: /usr/include/sys/_types/_fd_isset.h
s_gqppmr.o: /usr/include/sys/_types/_fd_copy.h
s_gqppmr.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqppmr.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqppmr.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqppmr.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqppmr.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqppmr.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqppmr.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqppmr.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqppmr.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqppmr.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqppmr.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqppmr.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqppmr.o: ../../../.././include/ncarg/c.h
s_gqpra.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqpra.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqpra.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gqpra.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gqpra.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gqpra.o: /usr/include/i386/_types.h
s_gqpra.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqpra.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqpra.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqpra.o: /usr/include/sys/_types/_int16_t.h
s_gqpra.o: /usr/include/sys/_types/_int32_t.h
s_gqpra.o: /usr/include/sys/_types/_int64_t.h
s_gqpra.o: /usr/include/sys/_types/_u_int8_t.h
s_gqpra.o: /usr/include/sys/_types/_u_int16_t.h
s_gqpra.o: /usr/include/sys/_types/_u_int32_t.h
s_gqpra.o: /usr/include/sys/_types/_u_int64_t.h
s_gqpra.o: /usr/include/sys/_types/_intptr_t.h
s_gqpra.o: /usr/include/sys/_types/_uintptr_t.h
s_gqpra.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqpra.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqpra.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqpra.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqpra.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqpra.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqpra.o: /usr/include/libkern/_OSByteOrder.h
s_gqpra.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqpra.o: /usr/include/sys/_types/_u_char.h
s_gqpra.o: /usr/include/sys/_types/_u_short.h
s_gqpra.o: /usr/include/sys/_types/_u_int.h
s_gqpra.o: /usr/include/sys/_types/_caddr_t.h
s_gqpra.o: /usr/include/sys/_types/_dev_t.h
s_gqpra.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqpra.o: /usr/include/sys/_types/_blksize_t.h
s_gqpra.o: /usr/include/sys/_types/_gid_t.h
s_gqpra.o: /usr/include/sys/_types/_in_addr_t.h
s_gqpra.o: /usr/include/sys/_types/_in_port_t.h
s_gqpra.o: /usr/include/sys/_types/_ino_t.h
s_gqpra.o: /usr/include/sys/_types/_ino64_t.h
s_gqpra.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gqpra.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gqpra.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gqpra.o: /usr/include/sys/_types/_clock_t.h
s_gqpra.o: /usr/include/sys/_types/_time_t.h
s_gqpra.o: /usr/include/sys/_types/_useconds_t.h
s_gqpra.o: /usr/include/sys/_types/_suseconds_t.h
s_gqpra.o: /usr/include/sys/_types/_rsize_t.h
s_gqpra.o: /usr/include/sys/_types/_errno_t.h
s_gqpra.o: /usr/include/sys/_types/_fd_def.h
s_gqpra.o: /usr/include/sys/_types/_fd_setsize.h
s_gqpra.o: /usr/include/sys/_types/_fd_set.h
s_gqpra.o: /usr/include/sys/_types/_fd_clr.h
s_gqpra.o: /usr/include/sys/_types/_fd_zero.h
s_gqpra.o: /usr/include/sys/_types/_fd_isset.h
s_gqpra.o: /usr/include/sys/_types/_fd_copy.h
s_gqpra.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqpra.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqpra.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqpra.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqpra.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqpra.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqpra.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqpra.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqpra.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqpra.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqpra.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqpra.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqpra.o: ../../../.././include/ncarg/c.h
s_gqptxr.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqptxr.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqptxr.o: /usr/include/sys/_posix_availability.h
s_gqptxr.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gqptxr.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gqptxr.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gqptxr.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqptxr.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqptxr.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqptxr.o: /usr/include/sys/_types/_int16_t.h
s_gqptxr.o: /usr/include/sys/_types/_int32_t.h
s_gqptxr.o: /usr/include/sys/_types/_int64_t.h
s_gqptxr.o: /usr/include/sys/_types/_u_int8_t.h
s_gqptxr.o: /usr/include/sys/_types/_u_int16_t.h
s_gqptxr.o: /usr/include/sys/_types/_u_int32_t.h
s_gqptxr.o: /usr/include/sys/_types/_u_int64_t.h
s_gqptxr.o: /usr/include/sys/_types/_intptr_t.h
s_gqptxr.o: /usr/include/sys/_types/_uintptr_t.h
s_gqptxr.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqptxr.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqptxr.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqptxr.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqptxr.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqptxr.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqptxr.o: /usr/include/libkern/_OSByteOrder.h
s_gqptxr.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqptxr.o: /usr/include/sys/_types/_u_char.h
s_gqptxr.o: /usr/include/sys/_types/_u_short.h
s_gqptxr.o: /usr/include/sys/_types/_u_int.h
s_gqptxr.o: /usr/include/sys/_types/_caddr_t.h
s_gqptxr.o: /usr/include/sys/_types/_dev_t.h
s_gqptxr.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqptxr.o: /usr/include/sys/_types/_blksize_t.h
s_gqptxr.o: /usr/include/sys/_types/_gid_t.h
s_gqptxr.o: /usr/include/sys/_types/_in_addr_t.h
s_gqptxr.o: /usr/include/sys/_types/_in_port_t.h
s_gqptxr.o: /usr/include/sys/_types/_ino_t.h
s_gqptxr.o: /usr/include/sys/_types/_ino64_t.h
s_gqptxr.o: /usr/include/sys/_types/_key_t.h
s_gqptxr.o: /usr/include/sys/_types/_mode_t.h
s_gqptxr.o: /usr/include/sys/_types/_nlink_t.h
s_gqptxr.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gqptxr.o: /usr/include/sys/_types/_uid_t.h
s_gqptxr.o: /usr/include/sys/_types/_clock_t.h
s_gqptxr.o: /usr/include/sys/_types/_time_t.h
s_gqptxr.o: /usr/include/sys/_types/_useconds_t.h
s_gqptxr.o: /usr/include/sys/_types/_suseconds_t.h
s_gqptxr.o: /usr/include/sys/_types/_rsize_t.h
s_gqptxr.o: /usr/include/sys/_types/_errno_t.h
s_gqptxr.o: /usr/include/sys/_types/_fd_def.h
s_gqptxr.o: /usr/include/sys/_types/_fd_setsize.h
s_gqptxr.o: /usr/include/sys/_types/_fd_set.h
s_gqptxr.o: /usr/include/sys/_types/_fd_clr.h
s_gqptxr.o: /usr/include/sys/_types/_fd_zero.h
s_gqptxr.o: /usr/include/sys/_types/_fd_isset.h
s_gqptxr.o: /usr/include/sys/_types/_fd_copy.h
s_gqptxr.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqptxr.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqptxr.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqptxr.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqptxr.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqptxr.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqptxr.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqptxr.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqptxr.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqptxr.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqptxr.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqptxr.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqptxr.o: ../../../.././include/ncarg/c.h
s_gqpx.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqpx.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqpx.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gqpx.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gqpx.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gqpx.o: /usr/include/i386/_types.h
s_gqpx.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqpx.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqpx.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqpx.o: /usr/include/sys/_types/_int16_t.h
s_gqpx.o: /usr/include/sys/_types/_int32_t.h
s_gqpx.o: /usr/include/sys/_types/_int64_t.h
s_gqpx.o: /usr/include/sys/_types/_u_int8_t.h
s_gqpx.o: /usr/include/sys/_types/_u_int16_t.h
s_gqpx.o: /usr/include/sys/_types/_u_int32_t.h
s_gqpx.o: /usr/include/sys/_types/_u_int64_t.h
s_gqpx.o: /usr/include/sys/_types/_intptr_t.h
s_gqpx.o: /usr/include/sys/_types/_uintptr_t.h
s_gqpx.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqpx.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqpx.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqpx.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqpx.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqpx.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqpx.o: /usr/include/libkern/_OSByteOrder.h
s_gqpx.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqpx.o: /usr/include/sys/_types/_u_char.h
s_gqpx.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
s_gqpx.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
s_gqpx.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqpx.o: /usr/include/sys/_types/_blksize_t.h
s_gqpx.o: /usr/include/sys/_types/_gid_t.h
s_gqpx.o: /usr/include/sys/_types/_in_addr_t.h
s_gqpx.o: /usr/include/sys/_types/_in_port_t.h
s_gqpx.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
s_gqpx.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gqpx.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gqpx.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gqpx.o: /usr/include/sys/_types/_clock_t.h
s_gqpx.o: /usr/include/sys/_types/_time_t.h
s_gqpx.o: /usr/include/sys/_types/_useconds_t.h
s_gqpx.o: /usr/include/sys/_types/_suseconds_t.h
s_gqpx.o: /usr/include/sys/_types/_rsize_t.h
s_gqpx.o: /usr/include/sys/_types/_errno_t.h
s_gqpx.o: /usr/include/sys/_types/_fd_def.h
s_gqpx.o: /usr/include/sys/_types/_fd_setsize.h
s_gqpx.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
s_gqpx.o: /usr/include/sys/_types/_fd_zero.h
s_gqpx.o: /usr/include/sys/_types/_fd_isset.h
s_gqpx.o: /usr/include/sys/_types/_fd_copy.h
s_gqpx.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqpx.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqpx.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqpx.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqpx.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqpx.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqpx.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqpx.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqpx.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqpx.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqpx.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqpx.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqpx.o: ../../../.././include/ncarg/c.h
s_gqpxa.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqpxa.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqpxa.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gqpxa.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gqpxa.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gqpxa.o: /usr/include/i386/_types.h
s_gqpxa.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqpxa.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqpxa.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqpxa.o: /usr/include/sys/_types/_int16_t.h
s_gqpxa.o: /usr/include/sys/_types/_int32_t.h
s_gqpxa.o: /usr/include/sys/_types/_int64_t.h
s_gqpxa.o: /usr/include/sys/_types/_u_int8_t.h
s_gqpxa.o: /usr/include/sys/_types/_u_int16_t.h
s_gqpxa.o: /usr/include/sys/_types/_u_int32_t.h
s_gqpxa.o: /usr/include/sys/_types/_u_int64_t.h
s_gqpxa.o: /usr/include/sys/_types/_intptr_t.h
s_gqpxa.o: /usr/include/sys/_types/_uintptr_t.h
s_gqpxa.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqpxa.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqpxa.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqpxa.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqpxa.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqpxa.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqpxa.o: /usr/include/libkern/_OSByteOrder.h
s_gqpxa.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqpxa.o: /usr/include/sys/_types/_u_char.h
s_gqpxa.o: /usr/include/sys/_types/_u_short.h
s_gqpxa.o: /usr/include/sys/_types/_u_int.h
s_gqpxa.o: /usr/include/sys/_types/_caddr_t.h
s_gqpxa.o: /usr/include/sys/_types/_dev_t.h
s_gqpxa.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqpxa.o: /usr/include/sys/_types/_blksize_t.h
s_gqpxa.o: /usr/include/sys/_types/_gid_t.h
s_gqpxa.o: /usr/include/sys/_types/_in_addr_t.h
s_gqpxa.o: /usr/include/sys/_types/_in_port_t.h
s_gqpxa.o: /usr/include/sys/_types/_ino_t.h
s_gqpxa.o: /usr/include/sys/_types/_ino64_t.h
s_gqpxa.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gqpxa.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gqpxa.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gqpxa.o: /usr/include/sys/_types/_clock_t.h
s_gqpxa.o: /usr/include/sys/_types/_time_t.h
s_gqpxa.o: /usr/include/sys/_types/_useconds_t.h
s_gqpxa.o: /usr/include/sys/_types/_suseconds_t.h
s_gqpxa.o: /usr/include/sys/_types/_rsize_t.h
s_gqpxa.o: /usr/include/sys/_types/_errno_t.h
s_gqpxa.o: /usr/include/sys/_types/_fd_def.h
s_gqpxa.o: /usr/include/sys/_types/_fd_setsize.h
s_gqpxa.o: /usr/include/sys/_types/_fd_set.h
s_gqpxa.o: /usr/include/sys/_types/_fd_clr.h
s_gqpxa.o: /usr/include/sys/_types/_fd_zero.h
s_gqpxa.o: /usr/include/sys/_types/_fd_isset.h
s_gqpxa.o: /usr/include/sys/_types/_fd_copy.h
s_gqpxa.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqpxa.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqpxa.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqpxa.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqpxa.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqpxa.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqpxa.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqpxa.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqpxa.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqpxa.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqpxa.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqpxa.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqpxa.o: ../../../.././include/ncarg/c.h
s_gqpxad.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqpxad.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqpxad.o: /usr/include/sys/_posix_availability.h
s_gqpxad.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gqpxad.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gqpxad.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gqpxad.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqpxad.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqpxad.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqpxad.o: /usr/include/sys/_types/_int16_t.h
s_gqpxad.o: /usr/include/sys/_types/_int32_t.h
s_gqpxad.o: /usr/include/sys/_types/_int64_t.h
s_gqpxad.o: /usr/include/sys/_types/_u_int8_t.h
s_gqpxad.o: /usr/include/sys/_types/_u_int16_t.h
s_gqpxad.o: /usr/include/sys/_types/_u_int32_t.h
s_gqpxad.o: /usr/include/sys/_types/_u_int64_t.h
s_gqpxad.o: /usr/include/sys/_types/_intptr_t.h
s_gqpxad.o: /usr/include/sys/_types/_uintptr_t.h
s_gqpxad.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqpxad.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqpxad.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqpxad.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqpxad.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqpxad.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqpxad.o: /usr/include/libkern/_OSByteOrder.h
s_gqpxad.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqpxad.o: /usr/include/sys/_types/_u_char.h
s_gqpxad.o: /usr/include/sys/_types/_u_short.h
s_gqpxad.o: /usr/include/sys/_types/_u_int.h
s_gqpxad.o: /usr/include/sys/_types/_caddr_t.h
s_gqpxad.o: /usr/include/sys/_types/_dev_t.h
s_gqpxad.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqpxad.o: /usr/include/sys/_types/_blksize_t.h
s_gqpxad.o: /usr/include/sys/_types/_gid_t.h
s_gqpxad.o: /usr/include/sys/_types/_in_addr_t.h
s_gqpxad.o: /usr/include/sys/_types/_in_port_t.h
s_gqpxad.o: /usr/include/sys/_types/_ino_t.h
s_gqpxad.o: /usr/include/sys/_types/_ino64_t.h
s_gqpxad.o: /usr/include/sys/_types/_key_t.h
s_gqpxad.o: /usr/include/sys/_types/_mode_t.h
s_gqpxad.o: /usr/include/sys/_types/_nlink_t.h
s_gqpxad.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gqpxad.o: /usr/include/sys/_types/_uid_t.h
s_gqpxad.o: /usr/include/sys/_types/_clock_t.h
s_gqpxad.o: /usr/include/sys/_types/_time_t.h
s_gqpxad.o: /usr/include/sys/_types/_useconds_t.h
s_gqpxad.o: /usr/include/sys/_types/_suseconds_t.h
s_gqpxad.o: /usr/include/sys/_types/_rsize_t.h
s_gqpxad.o: /usr/include/sys/_types/_errno_t.h
s_gqpxad.o: /usr/include/sys/_types/_fd_def.h
s_gqpxad.o: /usr/include/sys/_types/_fd_setsize.h
s_gqpxad.o: /usr/include/sys/_types/_fd_set.h
s_gqpxad.o: /usr/include/sys/_types/_fd_clr.h
s_gqpxad.o: /usr/include/sys/_types/_fd_zero.h
s_gqpxad.o: /usr/include/sys/_types/_fd_isset.h
s_gqpxad.o: /usr/include/sys/_types/_fd_copy.h
s_gqpxad.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqpxad.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqpxad.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqpxad.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqpxad.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqpxad.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqpxad.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqpxad.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqpxad.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqpxad.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqpxad.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqpxad.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqpxad.o: ../../../.././include/ncarg/c.h
s_gqtxal.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqtxal.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqtxal.o: /usr/include/sys/_posix_availability.h
s_gqtxal.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gqtxal.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gqtxal.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gqtxal.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqtxal.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqtxal.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqtxal.o: /usr/include/sys/_types/_int16_t.h
s_gqtxal.o: /usr/include/sys/_types/_int32_t.h
s_gqtxal.o: /usr/include/sys/_types/_int64_t.h
s_gqtxal.o: /usr/include/sys/_types/_u_int8_t.h
s_gqtxal.o: /usr/include/sys/_types/_u_int16_t.h
s_gqtxal.o: /usr/include/sys/_types/_u_int32_t.h
s_gqtxal.o: /usr/include/sys/_types/_u_int64_t.h
s_gqtxal.o: /usr/include/sys/_types/_intptr_t.h
s_gqtxal.o: /usr/include/sys/_types/_uintptr_t.h
s_gqtxal.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqtxal.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqtxal.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqtxal.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqtxal.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqtxal.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqtxal.o: /usr/include/libkern/_OSByteOrder.h
s_gqtxal.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqtxal.o: /usr/include/sys/_types/_u_char.h
s_gqtxal.o: /usr/include/sys/_types/_u_short.h
s_gqtxal.o: /usr/include/sys/_types/_u_int.h
s_gqtxal.o: /usr/include/sys/_types/_caddr_t.h
s_gqtxal.o: /usr/include/sys/_types/_dev_t.h
s_gqtxal.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqtxal.o: /usr/include/sys/_types/_blksize_t.h
s_gqtxal.o: /usr/include/sys/_types/_gid_t.h
s_gqtxal.o: /usr/include/sys/_types/_in_addr_t.h
s_gqtxal.o: /usr/include/sys/_types/_in_port_t.h
s_gqtxal.o: /usr/include/sys/_types/_ino_t.h
s_gqtxal.o: /usr/include/sys/_types/_ino64_t.h
s_gqtxal.o: /usr/include/sys/_types/_key_t.h
s_gqtxal.o: /usr/include/sys/_types/_mode_t.h
s_gqtxal.o: /usr/include/sys/_types/_nlink_t.h
s_gqtxal.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gqtxal.o: /usr/include/sys/_types/_uid_t.h
s_gqtxal.o: /usr/include/sys/_types/_clock_t.h
s_gqtxal.o: /usr/include/sys/_types/_time_t.h
s_gqtxal.o: /usr/include/sys/_types/_useconds_t.h
s_gqtxal.o: /usr/include/sys/_types/_suseconds_t.h
s_gqtxal.o: /usr/include/sys/_types/_rsize_t.h
s_gqtxal.o: /usr/include/sys/_types/_errno_t.h
s_gqtxal.o: /usr/include/sys/_types/_fd_def.h
s_gqtxal.o: /usr/include/sys/_types/_fd_setsize.h
s_gqtxal.o: /usr/include/sys/_types/_fd_set.h
s_gqtxal.o: /usr/include/sys/_types/_fd_clr.h
s_gqtxal.o: /usr/include/sys/_types/_fd_zero.h
s_gqtxal.o: /usr/include/sys/_types/_fd_isset.h
s_gqtxal.o: /usr/include/sys/_types/_fd_copy.h
s_gqtxal.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqtxal.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqtxal.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqtxal.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqtxal.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqtxal.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqtxal.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqtxal.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqtxal.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqtxal.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqtxal.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqtxal.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqtxal.o: ../../../.././include/ncarg/c.h
s_gqtxci.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqtxci.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqtxci.o: /usr/include/sys/_posix_availability.h
s_gqtxci.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gqtxci.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gqtxci.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gqtxci.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqtxci.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqtxci.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqtxci.o: /usr/include/sys/_types/_int16_t.h
s_gqtxci.o: /usr/include/sys/_types/_int32_t.h
s_gqtxci.o: /usr/include/sys/_types/_int64_t.h
s_gqtxci.o: /usr/include/sys/_types/_u_int8_t.h
s_gqtxci.o: /usr/include/sys/_types/_u_int16_t.h
s_gqtxci.o: /usr/include/sys/_types/_u_int32_t.h
s_gqtxci.o: /usr/include/sys/_types/_u_int64_t.h
s_gqtxci.o: /usr/include/sys/_types/_intptr_t.h
s_gqtxci.o: /usr/include/sys/_types/_uintptr_t.h
s_gqtxci.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqtxci.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqtxci.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqtxci.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqtxci.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqtxci.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqtxci.o: /usr/include/libkern/_OSByteOrder.h
s_gqtxci.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqtxci.o: /usr/include/sys/_types/_u_char.h
s_gqtxci.o: /usr/include/sys/_types/_u_short.h
s_gqtxci.o: /usr/include/sys/_types/_u_int.h
s_gqtxci.o: /usr/include/sys/_types/_caddr_t.h
s_gqtxci.o: /usr/include/sys/_types/_dev_t.h
s_gqtxci.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqtxci.o: /usr/include/sys/_types/_blksize_t.h
s_gqtxci.o: /usr/include/sys/_types/_gid_t.h
s_gqtxci.o: /usr/include/sys/_types/_in_addr_t.h
s_gqtxci.o: /usr/include/sys/_types/_in_port_t.h
s_gqtxci.o: /usr/include/sys/_types/_ino_t.h
s_gqtxci.o: /usr/include/sys/_types/_ino64_t.h
s_gqtxci.o: /usr/include/sys/_types/_key_t.h
s_gqtxci.o: /usr/include/sys/_types/_mode_t.h
s_gqtxci.o: /usr/include/sys/_types/_nlink_t.h
s_gqtxci.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gqtxci.o: /usr/include/sys/_types/_uid_t.h
s_gqtxci.o: /usr/include/sys/_types/_clock_t.h
s_gqtxci.o: /usr/include/sys/_types/_time_t.h
s_gqtxci.o: /usr/include/sys/_types/_useconds_t.h
s_gqtxci.o: /usr/include/sys/_types/_suseconds_t.h
s_gqtxci.o: /usr/include/sys/_types/_rsize_t.h
s_gqtxci.o: /usr/include/sys/_types/_errno_t.h
s_gqtxci.o: /usr/include/sys/_types/_fd_def.h
s_gqtxci.o: /usr/include/sys/_types/_fd_setsize.h
s_gqtxci.o: /usr/include/sys/_types/_fd_set.h
s_gqtxci.o: /usr/include/sys/_types/_fd_clr.h
s_gqtxci.o: /usr/include/sys/_types/_fd_zero.h
s_gqtxci.o: /usr/include/sys/_types/_fd_isset.h
s_gqtxci.o: /usr/include/sys/_types/_fd_copy.h
s_gqtxci.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqtxci.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqtxci.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqtxci.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqtxci.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqtxci.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqtxci.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqtxci.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqtxci.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqtxci.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqtxci.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqtxci.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqtxci.o: ../../../.././include/ncarg/c.h
s_gqtxf.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqtxf.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqtxf.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gqtxf.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gqtxf.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gqtxf.o: /usr/include/i386/_types.h
s_gqtxf.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqtxf.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqtxf.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqtxf.o: /usr/include/sys/_types/_int16_t.h
s_gqtxf.o: /usr/include/sys/_types/_int32_t.h
s_gqtxf.o: /usr/include/sys/_types/_int64_t.h
s_gqtxf.o: /usr/include/sys/_types/_u_int8_t.h
s_gqtxf.o: /usr/include/sys/_types/_u_int16_t.h
s_gqtxf.o: /usr/include/sys/_types/_u_int32_t.h
s_gqtxf.o: /usr/include/sys/_types/_u_int64_t.h
s_gqtxf.o: /usr/include/sys/_types/_intptr_t.h
s_gqtxf.o: /usr/include/sys/_types/_uintptr_t.h
s_gqtxf.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqtxf.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqtxf.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqtxf.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqtxf.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqtxf.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqtxf.o: /usr/include/libkern/_OSByteOrder.h
s_gqtxf.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqtxf.o: /usr/include/sys/_types/_u_char.h
s_gqtxf.o: /usr/include/sys/_types/_u_short.h
s_gqtxf.o: /usr/include/sys/_types/_u_int.h
s_gqtxf.o: /usr/include/sys/_types/_caddr_t.h
s_gqtxf.o: /usr/include/sys/_types/_dev_t.h
s_gqtxf.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqtxf.o: /usr/include/sys/_types/_blksize_t.h
s_gqtxf.o: /usr/include/sys/_types/_gid_t.h
s_gqtxf.o: /usr/include/sys/_types/_in_addr_t.h
s_gqtxf.o: /usr/include/sys/_types/_in_port_t.h
s_gqtxf.o: /usr/include/sys/_types/_ino_t.h
s_gqtxf.o: /usr/include/sys/_types/_ino64_t.h
s_gqtxf.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gqtxf.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gqtxf.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gqtxf.o: /usr/include/sys/_types/_clock_t.h
s_gqtxf.o: /usr/include/sys/_types/_time_t.h
s_gqtxf.o: /usr/include/sys/_types/_useconds_t.h
s_gqtxf.o: /usr/include/sys/_types/_suseconds_t.h
s_gqtxf.o: /usr/include/sys/_types/_rsize_t.h
s_gqtxf.o: /usr/include/sys/_types/_errno_t.h
s_gqtxf.o: /usr/include/sys/_types/_fd_def.h
s_gqtxf.o: /usr/include/sys/_types/_fd_setsize.h
s_gqtxf.o: /usr/include/sys/_types/_fd_set.h
s_gqtxf.o: /usr/include/sys/_types/_fd_clr.h
s_gqtxf.o: /usr/include/sys/_types/_fd_zero.h
s_gqtxf.o: /usr/include/sys/_types/_fd_isset.h
s_gqtxf.o: /usr/include/sys/_types/_fd_copy.h
s_gqtxf.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqtxf.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqtxf.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqtxf.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqtxf.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqtxf.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqtxf.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqtxf.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqtxf.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqtxf.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqtxf.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqtxf.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqtxf.o: ../../../.././include/ncarg/c.h
s_gqtxfp.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqtxfp.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqtxfp.o: /usr/include/sys/_posix_availability.h
s_gqtxfp.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gqtxfp.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gqtxfp.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gqtxfp.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqtxfp.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqtxfp.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqtxfp.o: /usr/include/sys/_types/_int16_t.h
s_gqtxfp.o: /usr/include/sys/_types/_int32_t.h
s_gqtxfp.o: /usr/include/sys/_types/_int64_t.h
s_gqtxfp.o: /usr/include/sys/_types/_u_int8_t.h
s_gqtxfp.o: /usr/include/sys/_types/_u_int16_t.h
s_gqtxfp.o: /usr/include/sys/_types/_u_int32_t.h
s_gqtxfp.o: /usr/include/sys/_types/_u_int64_t.h
s_gqtxfp.o: /usr/include/sys/_types/_intptr_t.h
s_gqtxfp.o: /usr/include/sys/_types/_uintptr_t.h
s_gqtxfp.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqtxfp.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqtxfp.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqtxfp.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqtxfp.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqtxfp.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqtxfp.o: /usr/include/libkern/_OSByteOrder.h
s_gqtxfp.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqtxfp.o: /usr/include/sys/_types/_u_char.h
s_gqtxfp.o: /usr/include/sys/_types/_u_short.h
s_gqtxfp.o: /usr/include/sys/_types/_u_int.h
s_gqtxfp.o: /usr/include/sys/_types/_caddr_t.h
s_gqtxfp.o: /usr/include/sys/_types/_dev_t.h
s_gqtxfp.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqtxfp.o: /usr/include/sys/_types/_blksize_t.h
s_gqtxfp.o: /usr/include/sys/_types/_gid_t.h
s_gqtxfp.o: /usr/include/sys/_types/_in_addr_t.h
s_gqtxfp.o: /usr/include/sys/_types/_in_port_t.h
s_gqtxfp.o: /usr/include/sys/_types/_ino_t.h
s_gqtxfp.o: /usr/include/sys/_types/_ino64_t.h
s_gqtxfp.o: /usr/include/sys/_types/_key_t.h
s_gqtxfp.o: /usr/include/sys/_types/_mode_t.h
s_gqtxfp.o: /usr/include/sys/_types/_nlink_t.h
s_gqtxfp.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gqtxfp.o: /usr/include/sys/_types/_uid_t.h
s_gqtxfp.o: /usr/include/sys/_types/_clock_t.h
s_gqtxfp.o: /usr/include/sys/_types/_time_t.h
s_gqtxfp.o: /usr/include/sys/_types/_useconds_t.h
s_gqtxfp.o: /usr/include/sys/_types/_suseconds_t.h
s_gqtxfp.o: /usr/include/sys/_types/_rsize_t.h
s_gqtxfp.o: /usr/include/sys/_types/_errno_t.h
s_gqtxfp.o: /usr/include/sys/_types/_fd_def.h
s_gqtxfp.o: /usr/include/sys/_types/_fd_setsize.h
s_gqtxfp.o: /usr/include/sys/_types/_fd_set.h
s_gqtxfp.o: /usr/include/sys/_types/_fd_clr.h
s_gqtxfp.o: /usr/include/sys/_types/_fd_zero.h
s_gqtxfp.o: /usr/include/sys/_types/_fd_isset.h
s_gqtxfp.o: /usr/include/sys/_types/_fd_copy.h
s_gqtxfp.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqtxfp.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqtxfp.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqtxfp.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqtxfp.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqtxfp.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqtxfp.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqtxfp.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqtxfp.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqtxfp.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqtxfp.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqtxfp.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqtxfp.o: ../../../.././include/ncarg/c.h
s_gqtxi.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqtxi.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqtxi.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gqtxi.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gqtxi.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gqtxi.o: /usr/include/i386/_types.h
s_gqtxi.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqtxi.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqtxi.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqtxi.o: /usr/include/sys/_types/_int16_t.h
s_gqtxi.o: /usr/include/sys/_types/_int32_t.h
s_gqtxi.o: /usr/include/sys/_types/_int64_t.h
s_gqtxi.o: /usr/include/sys/_types/_u_int8_t.h
s_gqtxi.o: /usr/include/sys/_types/_u_int16_t.h
s_gqtxi.o: /usr/include/sys/_types/_u_int32_t.h
s_gqtxi.o: /usr/include/sys/_types/_u_int64_t.h
s_gqtxi.o: /usr/include/sys/_types/_intptr_t.h
s_gqtxi.o: /usr/include/sys/_types/_uintptr_t.h
s_gqtxi.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqtxi.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqtxi.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqtxi.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqtxi.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqtxi.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqtxi.o: /usr/include/libkern/_OSByteOrder.h
s_gqtxi.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqtxi.o: /usr/include/sys/_types/_u_char.h
s_gqtxi.o: /usr/include/sys/_types/_u_short.h
s_gqtxi.o: /usr/include/sys/_types/_u_int.h
s_gqtxi.o: /usr/include/sys/_types/_caddr_t.h
s_gqtxi.o: /usr/include/sys/_types/_dev_t.h
s_gqtxi.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqtxi.o: /usr/include/sys/_types/_blksize_t.h
s_gqtxi.o: /usr/include/sys/_types/_gid_t.h
s_gqtxi.o: /usr/include/sys/_types/_in_addr_t.h
s_gqtxi.o: /usr/include/sys/_types/_in_port_t.h
s_gqtxi.o: /usr/include/sys/_types/_ino_t.h
s_gqtxi.o: /usr/include/sys/_types/_ino64_t.h
s_gqtxi.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gqtxi.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gqtxi.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gqtxi.o: /usr/include/sys/_types/_clock_t.h
s_gqtxi.o: /usr/include/sys/_types/_time_t.h
s_gqtxi.o: /usr/include/sys/_types/_useconds_t.h
s_gqtxi.o: /usr/include/sys/_types/_suseconds_t.h
s_gqtxi.o: /usr/include/sys/_types/_rsize_t.h
s_gqtxi.o: /usr/include/sys/_types/_errno_t.h
s_gqtxi.o: /usr/include/sys/_types/_fd_def.h
s_gqtxi.o: /usr/include/sys/_types/_fd_setsize.h
s_gqtxi.o: /usr/include/sys/_types/_fd_set.h
s_gqtxi.o: /usr/include/sys/_types/_fd_clr.h
s_gqtxi.o: /usr/include/sys/_types/_fd_zero.h
s_gqtxi.o: /usr/include/sys/_types/_fd_isset.h
s_gqtxi.o: /usr/include/sys/_types/_fd_copy.h
s_gqtxi.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqtxi.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqtxi.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqtxi.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqtxi.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqtxi.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqtxi.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqtxi.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqtxi.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqtxi.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqtxi.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqtxi.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqtxi.o: ../../../.././include/ncarg/c.h
s_gqtxx.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqtxx.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqtxx.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gqtxx.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gqtxx.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gqtxx.o: /usr/include/i386/_types.h
s_gqtxx.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqtxx.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqtxx.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqtxx.o: /usr/include/sys/_types/_int16_t.h
s_gqtxx.o: /usr/include/sys/_types/_int32_t.h
s_gqtxx.o: /usr/include/sys/_types/_int64_t.h
s_gqtxx.o: /usr/include/sys/_types/_u_int8_t.h
s_gqtxx.o: /usr/include/sys/_types/_u_int16_t.h
s_gqtxx.o: /usr/include/sys/_types/_u_int32_t.h
s_gqtxx.o: /usr/include/sys/_types/_u_int64_t.h
s_gqtxx.o: /usr/include/sys/_types/_intptr_t.h
s_gqtxx.o: /usr/include/sys/_types/_uintptr_t.h
s_gqtxx.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqtxx.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqtxx.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqtxx.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqtxx.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqtxx.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqtxx.o: /usr/include/libkern/_OSByteOrder.h
s_gqtxx.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqtxx.o: /usr/include/sys/_types/_u_char.h
s_gqtxx.o: /usr/include/sys/_types/_u_short.h
s_gqtxx.o: /usr/include/sys/_types/_u_int.h
s_gqtxx.o: /usr/include/sys/_types/_caddr_t.h
s_gqtxx.o: /usr/include/sys/_types/_dev_t.h
s_gqtxx.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqtxx.o: /usr/include/sys/_types/_blksize_t.h
s_gqtxx.o: /usr/include/sys/_types/_gid_t.h
s_gqtxx.o: /usr/include/sys/_types/_in_addr_t.h
s_gqtxx.o: /usr/include/sys/_types/_in_port_t.h
s_gqtxx.o: /usr/include/sys/_types/_ino_t.h
s_gqtxx.o: /usr/include/sys/_types/_ino64_t.h
s_gqtxx.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gqtxx.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gqtxx.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gqtxx.o: /usr/include/sys/_types/_clock_t.h
s_gqtxx.o: /usr/include/sys/_types/_time_t.h
s_gqtxx.o: /usr/include/sys/_types/_useconds_t.h
s_gqtxx.o: /usr/include/sys/_types/_suseconds_t.h
s_gqtxx.o: /usr/include/sys/_types/_rsize_t.h
s_gqtxx.o: /usr/include/sys/_types/_errno_t.h
s_gqtxx.o: /usr/include/sys/_types/_fd_def.h
s_gqtxx.o: /usr/include/sys/_types/_fd_setsize.h
s_gqtxx.o: /usr/include/sys/_types/_fd_set.h
s_gqtxx.o: /usr/include/sys/_types/_fd_clr.h
s_gqtxx.o: /usr/include/sys/_types/_fd_zero.h
s_gqtxx.o: /usr/include/sys/_types/_fd_isset.h
s_gqtxx.o: /usr/include/sys/_types/_fd_copy.h
s_gqtxx.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqtxx.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqtxx.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqtxx.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqtxx.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqtxx.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqtxx.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqtxx.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqtxx.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqtxx.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqtxx.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqtxx.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqtxx.o: ../../../.././include/ncarg/c.h /usr/include/string.h
s_gqtxx.o: /usr/include/strings.h /usr/include/secure/_strings.h
s_gqtxx.o: /usr/include/secure/_string.h
s_gqtxp.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqtxp.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqtxp.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gqtxp.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gqtxp.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gqtxp.o: /usr/include/i386/_types.h
s_gqtxp.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqtxp.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqtxp.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqtxp.o: /usr/include/sys/_types/_int16_t.h
s_gqtxp.o: /usr/include/sys/_types/_int32_t.h
s_gqtxp.o: /usr/include/sys/_types/_int64_t.h
s_gqtxp.o: /usr/include/sys/_types/_u_int8_t.h
s_gqtxp.o: /usr/include/sys/_types/_u_int16_t.h
s_gqtxp.o: /usr/include/sys/_types/_u_int32_t.h
s_gqtxp.o: /usr/include/sys/_types/_u_int64_t.h
s_gqtxp.o: /usr/include/sys/_types/_intptr_t.h
s_gqtxp.o: /usr/include/sys/_types/_uintptr_t.h
s_gqtxp.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqtxp.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqtxp.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqtxp.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqtxp.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqtxp.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqtxp.o: /usr/include/libkern/_OSByteOrder.h
s_gqtxp.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqtxp.o: /usr/include/sys/_types/_u_char.h
s_gqtxp.o: /usr/include/sys/_types/_u_short.h
s_gqtxp.o: /usr/include/sys/_types/_u_int.h
s_gqtxp.o: /usr/include/sys/_types/_caddr_t.h
s_gqtxp.o: /usr/include/sys/_types/_dev_t.h
s_gqtxp.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqtxp.o: /usr/include/sys/_types/_blksize_t.h
s_gqtxp.o: /usr/include/sys/_types/_gid_t.h
s_gqtxp.o: /usr/include/sys/_types/_in_addr_t.h
s_gqtxp.o: /usr/include/sys/_types/_in_port_t.h
s_gqtxp.o: /usr/include/sys/_types/_ino_t.h
s_gqtxp.o: /usr/include/sys/_types/_ino64_t.h
s_gqtxp.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gqtxp.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gqtxp.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gqtxp.o: /usr/include/sys/_types/_clock_t.h
s_gqtxp.o: /usr/include/sys/_types/_time_t.h
s_gqtxp.o: /usr/include/sys/_types/_useconds_t.h
s_gqtxp.o: /usr/include/sys/_types/_suseconds_t.h
s_gqtxp.o: /usr/include/sys/_types/_rsize_t.h
s_gqtxp.o: /usr/include/sys/_types/_errno_t.h
s_gqtxp.o: /usr/include/sys/_types/_fd_def.h
s_gqtxp.o: /usr/include/sys/_types/_fd_setsize.h
s_gqtxp.o: /usr/include/sys/_types/_fd_set.h
s_gqtxp.o: /usr/include/sys/_types/_fd_clr.h
s_gqtxp.o: /usr/include/sys/_types/_fd_zero.h
s_gqtxp.o: /usr/include/sys/_types/_fd_isset.h
s_gqtxp.o: /usr/include/sys/_types/_fd_copy.h
s_gqtxp.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqtxp.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqtxp.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqtxp.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqtxp.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqtxp.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqtxp.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqtxp.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqtxp.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqtxp.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqtxp.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqtxp.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqtxp.o: ../../../.././include/ncarg/c.h
s_gqwkc.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqwkc.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqwkc.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gqwkc.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gqwkc.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gqwkc.o: /usr/include/i386/_types.h
s_gqwkc.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqwkc.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqwkc.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqwkc.o: /usr/include/sys/_types/_int16_t.h
s_gqwkc.o: /usr/include/sys/_types/_int32_t.h
s_gqwkc.o: /usr/include/sys/_types/_int64_t.h
s_gqwkc.o: /usr/include/sys/_types/_u_int8_t.h
s_gqwkc.o: /usr/include/sys/_types/_u_int16_t.h
s_gqwkc.o: /usr/include/sys/_types/_u_int32_t.h
s_gqwkc.o: /usr/include/sys/_types/_u_int64_t.h
s_gqwkc.o: /usr/include/sys/_types/_intptr_t.h
s_gqwkc.o: /usr/include/sys/_types/_uintptr_t.h
s_gqwkc.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqwkc.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqwkc.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqwkc.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqwkc.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqwkc.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqwkc.o: /usr/include/libkern/_OSByteOrder.h
s_gqwkc.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqwkc.o: /usr/include/sys/_types/_u_char.h
s_gqwkc.o: /usr/include/sys/_types/_u_short.h
s_gqwkc.o: /usr/include/sys/_types/_u_int.h
s_gqwkc.o: /usr/include/sys/_types/_caddr_t.h
s_gqwkc.o: /usr/include/sys/_types/_dev_t.h
s_gqwkc.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqwkc.o: /usr/include/sys/_types/_blksize_t.h
s_gqwkc.o: /usr/include/sys/_types/_gid_t.h
s_gqwkc.o: /usr/include/sys/_types/_in_addr_t.h
s_gqwkc.o: /usr/include/sys/_types/_in_port_t.h
s_gqwkc.o: /usr/include/sys/_types/_ino_t.h
s_gqwkc.o: /usr/include/sys/_types/_ino64_t.h
s_gqwkc.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gqwkc.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gqwkc.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gqwkc.o: /usr/include/sys/_types/_clock_t.h
s_gqwkc.o: /usr/include/sys/_types/_time_t.h
s_gqwkc.o: /usr/include/sys/_types/_useconds_t.h
s_gqwkc.o: /usr/include/sys/_types/_suseconds_t.h
s_gqwkc.o: /usr/include/sys/_types/_rsize_t.h
s_gqwkc.o: /usr/include/sys/_types/_errno_t.h
s_gqwkc.o: /usr/include/sys/_types/_fd_def.h
s_gqwkc.o: /usr/include/sys/_types/_fd_setsize.h
s_gqwkc.o: /usr/include/sys/_types/_fd_set.h
s_gqwkc.o: /usr/include/sys/_types/_fd_clr.h
s_gqwkc.o: /usr/include/sys/_types/_fd_zero.h
s_gqwkc.o: /usr/include/sys/_types/_fd_isset.h
s_gqwkc.o: /usr/include/sys/_types/_fd_copy.h
s_gqwkc.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqwkc.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqwkc.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqwkc.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqwkc.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqwkc.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqwkc.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqwkc.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqwkc.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqwkc.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqwkc.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqwkc.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqwkc.o: ../../../.././include/ncarg/c.h
s_gqwkca.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqwkca.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqwkca.o: /usr/include/sys/_posix_availability.h
s_gqwkca.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gqwkca.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gqwkca.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gqwkca.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqwkca.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqwkca.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqwkca.o: /usr/include/sys/_types/_int16_t.h
s_gqwkca.o: /usr/include/sys/_types/_int32_t.h
s_gqwkca.o: /usr/include/sys/_types/_int64_t.h
s_gqwkca.o: /usr/include/sys/_types/_u_int8_t.h
s_gqwkca.o: /usr/include/sys/_types/_u_int16_t.h
s_gqwkca.o: /usr/include/sys/_types/_u_int32_t.h
s_gqwkca.o: /usr/include/sys/_types/_u_int64_t.h
s_gqwkca.o: /usr/include/sys/_types/_intptr_t.h
s_gqwkca.o: /usr/include/sys/_types/_uintptr_t.h
s_gqwkca.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqwkca.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqwkca.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqwkca.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqwkca.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqwkca.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqwkca.o: /usr/include/libkern/_OSByteOrder.h
s_gqwkca.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqwkca.o: /usr/include/sys/_types/_u_char.h
s_gqwkca.o: /usr/include/sys/_types/_u_short.h
s_gqwkca.o: /usr/include/sys/_types/_u_int.h
s_gqwkca.o: /usr/include/sys/_types/_caddr_t.h
s_gqwkca.o: /usr/include/sys/_types/_dev_t.h
s_gqwkca.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqwkca.o: /usr/include/sys/_types/_blksize_t.h
s_gqwkca.o: /usr/include/sys/_types/_gid_t.h
s_gqwkca.o: /usr/include/sys/_types/_in_addr_t.h
s_gqwkca.o: /usr/include/sys/_types/_in_port_t.h
s_gqwkca.o: /usr/include/sys/_types/_ino_t.h
s_gqwkca.o: /usr/include/sys/_types/_ino64_t.h
s_gqwkca.o: /usr/include/sys/_types/_key_t.h
s_gqwkca.o: /usr/include/sys/_types/_mode_t.h
s_gqwkca.o: /usr/include/sys/_types/_nlink_t.h
s_gqwkca.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gqwkca.o: /usr/include/sys/_types/_uid_t.h
s_gqwkca.o: /usr/include/sys/_types/_clock_t.h
s_gqwkca.o: /usr/include/sys/_types/_time_t.h
s_gqwkca.o: /usr/include/sys/_types/_useconds_t.h
s_gqwkca.o: /usr/include/sys/_types/_suseconds_t.h
s_gqwkca.o: /usr/include/sys/_types/_rsize_t.h
s_gqwkca.o: /usr/include/sys/_types/_errno_t.h
s_gqwkca.o: /usr/include/sys/_types/_fd_def.h
s_gqwkca.o: /usr/include/sys/_types/_fd_setsize.h
s_gqwkca.o: /usr/include/sys/_types/_fd_set.h
s_gqwkca.o: /usr/include/sys/_types/_fd_clr.h
s_gqwkca.o: /usr/include/sys/_types/_fd_zero.h
s_gqwkca.o: /usr/include/sys/_types/_fd_isset.h
s_gqwkca.o: /usr/include/sys/_types/_fd_copy.h
s_gqwkca.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqwkca.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqwkca.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqwkca.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqwkca.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqwkca.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqwkca.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqwkca.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqwkca.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqwkca.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqwkca.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqwkca.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqwkca.o: ../../../.././include/ncarg/c.h
s_gqwkcl.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqwkcl.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqwkcl.o: /usr/include/sys/_posix_availability.h
s_gqwkcl.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gqwkcl.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gqwkcl.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gqwkcl.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqwkcl.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqwkcl.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqwkcl.o: /usr/include/sys/_types/_int16_t.h
s_gqwkcl.o: /usr/include/sys/_types/_int32_t.h
s_gqwkcl.o: /usr/include/sys/_types/_int64_t.h
s_gqwkcl.o: /usr/include/sys/_types/_u_int8_t.h
s_gqwkcl.o: /usr/include/sys/_types/_u_int16_t.h
s_gqwkcl.o: /usr/include/sys/_types/_u_int32_t.h
s_gqwkcl.o: /usr/include/sys/_types/_u_int64_t.h
s_gqwkcl.o: /usr/include/sys/_types/_intptr_t.h
s_gqwkcl.o: /usr/include/sys/_types/_uintptr_t.h
s_gqwkcl.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqwkcl.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqwkcl.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqwkcl.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqwkcl.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqwkcl.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqwkcl.o: /usr/include/libkern/_OSByteOrder.h
s_gqwkcl.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqwkcl.o: /usr/include/sys/_types/_u_char.h
s_gqwkcl.o: /usr/include/sys/_types/_u_short.h
s_gqwkcl.o: /usr/include/sys/_types/_u_int.h
s_gqwkcl.o: /usr/include/sys/_types/_caddr_t.h
s_gqwkcl.o: /usr/include/sys/_types/_dev_t.h
s_gqwkcl.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqwkcl.o: /usr/include/sys/_types/_blksize_t.h
s_gqwkcl.o: /usr/include/sys/_types/_gid_t.h
s_gqwkcl.o: /usr/include/sys/_types/_in_addr_t.h
s_gqwkcl.o: /usr/include/sys/_types/_in_port_t.h
s_gqwkcl.o: /usr/include/sys/_types/_ino_t.h
s_gqwkcl.o: /usr/include/sys/_types/_ino64_t.h
s_gqwkcl.o: /usr/include/sys/_types/_key_t.h
s_gqwkcl.o: /usr/include/sys/_types/_mode_t.h
s_gqwkcl.o: /usr/include/sys/_types/_nlink_t.h
s_gqwkcl.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gqwkcl.o: /usr/include/sys/_types/_uid_t.h
s_gqwkcl.o: /usr/include/sys/_types/_clock_t.h
s_gqwkcl.o: /usr/include/sys/_types/_time_t.h
s_gqwkcl.o: /usr/include/sys/_types/_useconds_t.h
s_gqwkcl.o: /usr/include/sys/_types/_suseconds_t.h
s_gqwkcl.o: /usr/include/sys/_types/_rsize_t.h
s_gqwkcl.o: /usr/include/sys/_types/_errno_t.h
s_gqwkcl.o: /usr/include/sys/_types/_fd_def.h
s_gqwkcl.o: /usr/include/sys/_types/_fd_setsize.h
s_gqwkcl.o: /usr/include/sys/_types/_fd_set.h
s_gqwkcl.o: /usr/include/sys/_types/_fd_clr.h
s_gqwkcl.o: /usr/include/sys/_types/_fd_zero.h
s_gqwkcl.o: /usr/include/sys/_types/_fd_isset.h
s_gqwkcl.o: /usr/include/sys/_types/_fd_copy.h
s_gqwkcl.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqwkcl.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqwkcl.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqwkcl.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqwkcl.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqwkcl.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqwkcl.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqwkcl.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqwkcl.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqwkcl.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqwkcl.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqwkcl.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqwkcl.o: ../../../.././include/ncarg/c.h
s_gqwkdu.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqwkdu.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqwkdu.o: /usr/include/sys/_posix_availability.h
s_gqwkdu.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gqwkdu.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gqwkdu.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gqwkdu.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqwkdu.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqwkdu.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqwkdu.o: /usr/include/sys/_types/_int16_t.h
s_gqwkdu.o: /usr/include/sys/_types/_int32_t.h
s_gqwkdu.o: /usr/include/sys/_types/_int64_t.h
s_gqwkdu.o: /usr/include/sys/_types/_u_int8_t.h
s_gqwkdu.o: /usr/include/sys/_types/_u_int16_t.h
s_gqwkdu.o: /usr/include/sys/_types/_u_int32_t.h
s_gqwkdu.o: /usr/include/sys/_types/_u_int64_t.h
s_gqwkdu.o: /usr/include/sys/_types/_intptr_t.h
s_gqwkdu.o: /usr/include/sys/_types/_uintptr_t.h
s_gqwkdu.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqwkdu.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqwkdu.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqwkdu.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqwkdu.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqwkdu.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqwkdu.o: /usr/include/libkern/_OSByteOrder.h
s_gqwkdu.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqwkdu.o: /usr/include/sys/_types/_u_char.h
s_gqwkdu.o: /usr/include/sys/_types/_u_short.h
s_gqwkdu.o: /usr/include/sys/_types/_u_int.h
s_gqwkdu.o: /usr/include/sys/_types/_caddr_t.h
s_gqwkdu.o: /usr/include/sys/_types/_dev_t.h
s_gqwkdu.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqwkdu.o: /usr/include/sys/_types/_blksize_t.h
s_gqwkdu.o: /usr/include/sys/_types/_gid_t.h
s_gqwkdu.o: /usr/include/sys/_types/_in_addr_t.h
s_gqwkdu.o: /usr/include/sys/_types/_in_port_t.h
s_gqwkdu.o: /usr/include/sys/_types/_ino_t.h
s_gqwkdu.o: /usr/include/sys/_types/_ino64_t.h
s_gqwkdu.o: /usr/include/sys/_types/_key_t.h
s_gqwkdu.o: /usr/include/sys/_types/_mode_t.h
s_gqwkdu.o: /usr/include/sys/_types/_nlink_t.h
s_gqwkdu.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gqwkdu.o: /usr/include/sys/_types/_uid_t.h
s_gqwkdu.o: /usr/include/sys/_types/_clock_t.h
s_gqwkdu.o: /usr/include/sys/_types/_time_t.h
s_gqwkdu.o: /usr/include/sys/_types/_useconds_t.h
s_gqwkdu.o: /usr/include/sys/_types/_suseconds_t.h
s_gqwkdu.o: /usr/include/sys/_types/_rsize_t.h
s_gqwkdu.o: /usr/include/sys/_types/_errno_t.h
s_gqwkdu.o: /usr/include/sys/_types/_fd_def.h
s_gqwkdu.o: /usr/include/sys/_types/_fd_setsize.h
s_gqwkdu.o: /usr/include/sys/_types/_fd_set.h
s_gqwkdu.o: /usr/include/sys/_types/_fd_clr.h
s_gqwkdu.o: /usr/include/sys/_types/_fd_zero.h
s_gqwkdu.o: /usr/include/sys/_types/_fd_isset.h
s_gqwkdu.o: /usr/include/sys/_types/_fd_copy.h
s_gqwkdu.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqwkdu.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqwkdu.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqwkdu.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqwkdu.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqwkdu.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqwkdu.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqwkdu.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqwkdu.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqwkdu.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqwkdu.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqwkdu.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqwkdu.o: ../../../.././include/ncarg/c.h
s_gqwks.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqwks.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqwks.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gqwks.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gqwks.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gqwks.o: /usr/include/i386/_types.h
s_gqwks.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqwks.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqwks.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqwks.o: /usr/include/sys/_types/_int16_t.h
s_gqwks.o: /usr/include/sys/_types/_int32_t.h
s_gqwks.o: /usr/include/sys/_types/_int64_t.h
s_gqwks.o: /usr/include/sys/_types/_u_int8_t.h
s_gqwks.o: /usr/include/sys/_types/_u_int16_t.h
s_gqwks.o: /usr/include/sys/_types/_u_int32_t.h
s_gqwks.o: /usr/include/sys/_types/_u_int64_t.h
s_gqwks.o: /usr/include/sys/_types/_intptr_t.h
s_gqwks.o: /usr/include/sys/_types/_uintptr_t.h
s_gqwks.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqwks.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqwks.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqwks.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqwks.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqwks.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqwks.o: /usr/include/libkern/_OSByteOrder.h
s_gqwks.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqwks.o: /usr/include/sys/_types/_u_char.h
s_gqwks.o: /usr/include/sys/_types/_u_short.h
s_gqwks.o: /usr/include/sys/_types/_u_int.h
s_gqwks.o: /usr/include/sys/_types/_caddr_t.h
s_gqwks.o: /usr/include/sys/_types/_dev_t.h
s_gqwks.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqwks.o: /usr/include/sys/_types/_blksize_t.h
s_gqwks.o: /usr/include/sys/_types/_gid_t.h
s_gqwks.o: /usr/include/sys/_types/_in_addr_t.h
s_gqwks.o: /usr/include/sys/_types/_in_port_t.h
s_gqwks.o: /usr/include/sys/_types/_ino_t.h
s_gqwks.o: /usr/include/sys/_types/_ino64_t.h
s_gqwks.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gqwks.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gqwks.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gqwks.o: /usr/include/sys/_types/_clock_t.h
s_gqwks.o: /usr/include/sys/_types/_time_t.h
s_gqwks.o: /usr/include/sys/_types/_useconds_t.h
s_gqwks.o: /usr/include/sys/_types/_suseconds_t.h
s_gqwks.o: /usr/include/sys/_types/_rsize_t.h
s_gqwks.o: /usr/include/sys/_types/_errno_t.h
s_gqwks.o: /usr/include/sys/_types/_fd_def.h
s_gqwks.o: /usr/include/sys/_types/_fd_setsize.h
s_gqwks.o: /usr/include/sys/_types/_fd_set.h
s_gqwks.o: /usr/include/sys/_types/_fd_clr.h
s_gqwks.o: /usr/include/sys/_types/_fd_zero.h
s_gqwks.o: /usr/include/sys/_types/_fd_isset.h
s_gqwks.o: /usr/include/sys/_types/_fd_copy.h
s_gqwks.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqwks.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqwks.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqwks.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqwks.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqwks.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqwks.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqwks.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqwks.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqwks.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqwks.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqwks.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqwks.o: ../../../.././include/ncarg/c.h
s_gqwkt.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqwkt.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqwkt.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gqwkt.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gqwkt.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gqwkt.o: /usr/include/i386/_types.h
s_gqwkt.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqwkt.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqwkt.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqwkt.o: /usr/include/sys/_types/_int16_t.h
s_gqwkt.o: /usr/include/sys/_types/_int32_t.h
s_gqwkt.o: /usr/include/sys/_types/_int64_t.h
s_gqwkt.o: /usr/include/sys/_types/_u_int8_t.h
s_gqwkt.o: /usr/include/sys/_types/_u_int16_t.h
s_gqwkt.o: /usr/include/sys/_types/_u_int32_t.h
s_gqwkt.o: /usr/include/sys/_types/_u_int64_t.h
s_gqwkt.o: /usr/include/sys/_types/_intptr_t.h
s_gqwkt.o: /usr/include/sys/_types/_uintptr_t.h
s_gqwkt.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqwkt.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqwkt.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqwkt.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqwkt.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqwkt.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqwkt.o: /usr/include/libkern/_OSByteOrder.h
s_gqwkt.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqwkt.o: /usr/include/sys/_types/_u_char.h
s_gqwkt.o: /usr/include/sys/_types/_u_short.h
s_gqwkt.o: /usr/include/sys/_types/_u_int.h
s_gqwkt.o: /usr/include/sys/_types/_caddr_t.h
s_gqwkt.o: /usr/include/sys/_types/_dev_t.h
s_gqwkt.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqwkt.o: /usr/include/sys/_types/_blksize_t.h
s_gqwkt.o: /usr/include/sys/_types/_gid_t.h
s_gqwkt.o: /usr/include/sys/_types/_in_addr_t.h
s_gqwkt.o: /usr/include/sys/_types/_in_port_t.h
s_gqwkt.o: /usr/include/sys/_types/_ino_t.h
s_gqwkt.o: /usr/include/sys/_types/_ino64_t.h
s_gqwkt.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gqwkt.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gqwkt.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gqwkt.o: /usr/include/sys/_types/_clock_t.h
s_gqwkt.o: /usr/include/sys/_types/_time_t.h
s_gqwkt.o: /usr/include/sys/_types/_useconds_t.h
s_gqwkt.o: /usr/include/sys/_types/_suseconds_t.h
s_gqwkt.o: /usr/include/sys/_types/_rsize_t.h
s_gqwkt.o: /usr/include/sys/_types/_errno_t.h
s_gqwkt.o: /usr/include/sys/_types/_fd_def.h
s_gqwkt.o: /usr/include/sys/_types/_fd_setsize.h
s_gqwkt.o: /usr/include/sys/_types/_fd_set.h
s_gqwkt.o: /usr/include/sys/_types/_fd_clr.h
s_gqwkt.o: /usr/include/sys/_types/_fd_zero.h
s_gqwkt.o: /usr/include/sys/_types/_fd_isset.h
s_gqwkt.o: /usr/include/sys/_types/_fd_copy.h
s_gqwkt.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqwkt.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqwkt.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqwkt.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqwkt.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqwkt.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqwkt.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqwkt.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqwkt.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqwkt.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqwkt.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqwkt.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqwkt.o: ../../../.././include/ncarg/c.h
s_grditm.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_grditm.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_grditm.o: /usr/include/sys/_posix_availability.h
s_grditm.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_grditm.o: /usr/include/_types.h /usr/include/sys/_types.h
s_grditm.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_grditm.o: /usr/include/sys/_pthread/_pthread_types.h
s_grditm.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_grditm.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_grditm.o: /usr/include/sys/_types/_int16_t.h
s_grditm.o: /usr/include/sys/_types/_int32_t.h
s_grditm.o: /usr/include/sys/_types/_int64_t.h
s_grditm.o: /usr/include/sys/_types/_u_int8_t.h
s_grditm.o: /usr/include/sys/_types/_u_int16_t.h
s_grditm.o: /usr/include/sys/_types/_u_int32_t.h
s_grditm.o: /usr/include/sys/_types/_u_int64_t.h
s_grditm.o: /usr/include/sys/_types/_intptr_t.h
s_grditm.o: /usr/include/sys/_types/_uintptr_t.h
s_grditm.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_grditm.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_grditm.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_grditm.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_grditm.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_grditm.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_grditm.o: /usr/include/libkern/_OSByteOrder.h
s_grditm.o: /usr/include/libkern/i386/_OSByteOrder.h
s_grditm.o: /usr/include/sys/_types/_u_char.h
s_grditm.o: /usr/include/sys/_types/_u_short.h
s_grditm.o: /usr/include/sys/_types/_u_int.h
s_grditm.o: /usr/include/sys/_types/_caddr_t.h
s_grditm.o: /usr/include/sys/_types/_dev_t.h
s_grditm.o: /usr/include/sys/_types/_blkcnt_t.h
s_grditm.o: /usr/include/sys/_types/_blksize_t.h
s_grditm.o: /usr/include/sys/_types/_gid_t.h
s_grditm.o: /usr/include/sys/_types/_in_addr_t.h
s_grditm.o: /usr/include/sys/_types/_in_port_t.h
s_grditm.o: /usr/include/sys/_types/_ino_t.h
s_grditm.o: /usr/include/sys/_types/_ino64_t.h
s_grditm.o: /usr/include/sys/_types/_key_t.h
s_grditm.o: /usr/include/sys/_types/_mode_t.h
s_grditm.o: /usr/include/sys/_types/_nlink_t.h
s_grditm.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_grditm.o: /usr/include/sys/_types/_uid_t.h
s_grditm.o: /usr/include/sys/_types/_clock_t.h
s_grditm.o: /usr/include/sys/_types/_time_t.h
s_grditm.o: /usr/include/sys/_types/_useconds_t.h
s_grditm.o: /usr/include/sys/_types/_suseconds_t.h
s_grditm.o: /usr/include/sys/_types/_rsize_t.h
s_grditm.o: /usr/include/sys/_types/_errno_t.h
s_grditm.o: /usr/include/sys/_types/_fd_def.h
s_grditm.o: /usr/include/sys/_types/_fd_setsize.h
s_grditm.o: /usr/include/sys/_types/_fd_set.h
s_grditm.o: /usr/include/sys/_types/_fd_clr.h
s_grditm.o: /usr/include/sys/_types/_fd_zero.h
s_grditm.o: /usr/include/sys/_types/_fd_isset.h
s_grditm.o: /usr/include/sys/_types/_fd_copy.h
s_grditm.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_grditm.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_grditm.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_grditm.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_grditm.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_grditm.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_grditm.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_grditm.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_grditm.o: /usr/include/sys/_pthread/_pthread_t.h
s_grditm.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_grditm.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_grditm.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_grditm.o: ../../../.././include/ncarg/c.h
s_gsasf.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gsasf.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gsasf.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gsasf.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gsasf.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gsasf.o: /usr/include/i386/_types.h
s_gsasf.o: /usr/include/sys/_pthread/_pthread_types.h
s_gsasf.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gsasf.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gsasf.o: /usr/include/sys/_types/_int16_t.h
s_gsasf.o: /usr/include/sys/_types/_int32_t.h
s_gsasf.o: /usr/include/sys/_types/_int64_t.h
s_gsasf.o: /usr/include/sys/_types/_u_int8_t.h
s_gsasf.o: /usr/include/sys/_types/_u_int16_t.h
s_gsasf.o: /usr/include/sys/_types/_u_int32_t.h
s_gsasf.o: /usr/include/sys/_types/_u_int64_t.h
s_gsasf.o: /usr/include/sys/_types/_intptr_t.h
s_gsasf.o: /usr/include/sys/_types/_uintptr_t.h
s_gsasf.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gsasf.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gsasf.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gsasf.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gsasf.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gsasf.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gsasf.o: /usr/include/libkern/_OSByteOrder.h
s_gsasf.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gsasf.o: /usr/include/sys/_types/_u_char.h
s_gsasf.o: /usr/include/sys/_types/_u_short.h
s_gsasf.o: /usr/include/sys/_types/_u_int.h
s_gsasf.o: /usr/include/sys/_types/_caddr_t.h
s_gsasf.o: /usr/include/sys/_types/_dev_t.h
s_gsasf.o: /usr/include/sys/_types/_blkcnt_t.h
s_gsasf.o: /usr/include/sys/_types/_blksize_t.h
s_gsasf.o: /usr/include/sys/_types/_gid_t.h
s_gsasf.o: /usr/include/sys/_types/_in_addr_t.h
s_gsasf.o: /usr/include/sys/_types/_in_port_t.h
s_gsasf.o: /usr/include/sys/_types/_ino_t.h
s_gsasf.o: /usr/include/sys/_types/_ino64_t.h
s_gsasf.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gsasf.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gsasf.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gsasf.o: /usr/include/sys/_types/_clock_t.h
s_gsasf.o: /usr/include/sys/_types/_time_t.h
s_gsasf.o: /usr/include/sys/_types/_useconds_t.h
s_gsasf.o: /usr/include/sys/_types/_suseconds_t.h
s_gsasf.o: /usr/include/sys/_types/_rsize_t.h
s_gsasf.o: /usr/include/sys/_types/_errno_t.h
s_gsasf.o: /usr/include/sys/_types/_fd_def.h
s_gsasf.o: /usr/include/sys/_types/_fd_setsize.h
s_gsasf.o: /usr/include/sys/_types/_fd_set.h
s_gsasf.o: /usr/include/sys/_types/_fd_clr.h
s_gsasf.o: /usr/include/sys/_types/_fd_zero.h
s_gsasf.o: /usr/include/sys/_types/_fd_isset.h
s_gsasf.o: /usr/include/sys/_types/_fd_copy.h
s_gsasf.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gsasf.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gsasf.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gsasf.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gsasf.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gsasf.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gsasf.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gsasf.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gsasf.o: /usr/include/sys/_pthread/_pthread_t.h
s_gsasf.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gsasf.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gsasf.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gsasf.o: ../../../.././include/ncarg/c.h
s_gschh.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gschh.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gschh.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gschh.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gschh.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gschh.o: /usr/include/i386/_types.h
s_gschh.o: /usr/include/sys/_pthread/_pthread_types.h
s_gschh.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gschh.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gschh.o: /usr/include/sys/_types/_int16_t.h
s_gschh.o: /usr/include/sys/_types/_int32_t.h
s_gschh.o: /usr/include/sys/_types/_int64_t.h
s_gschh.o: /usr/include/sys/_types/_u_int8_t.h
s_gschh.o: /usr/include/sys/_types/_u_int16_t.h
s_gschh.o: /usr/include/sys/_types/_u_int32_t.h
s_gschh.o: /usr/include/sys/_types/_u_int64_t.h
s_gschh.o: /usr/include/sys/_types/_intptr_t.h
s_gschh.o: /usr/include/sys/_types/_uintptr_t.h
s_gschh.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gschh.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gschh.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gschh.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gschh.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gschh.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gschh.o: /usr/include/libkern/_OSByteOrder.h
s_gschh.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gschh.o: /usr/include/sys/_types/_u_char.h
s_gschh.o: /usr/include/sys/_types/_u_short.h
s_gschh.o: /usr/include/sys/_types/_u_int.h
s_gschh.o: /usr/include/sys/_types/_caddr_t.h
s_gschh.o: /usr/include/sys/_types/_dev_t.h
s_gschh.o: /usr/include/sys/_types/_blkcnt_t.h
s_gschh.o: /usr/include/sys/_types/_blksize_t.h
s_gschh.o: /usr/include/sys/_types/_gid_t.h
s_gschh.o: /usr/include/sys/_types/_in_addr_t.h
s_gschh.o: /usr/include/sys/_types/_in_port_t.h
s_gschh.o: /usr/include/sys/_types/_ino_t.h
s_gschh.o: /usr/include/sys/_types/_ino64_t.h
s_gschh.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gschh.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gschh.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gschh.o: /usr/include/sys/_types/_clock_t.h
s_gschh.o: /usr/include/sys/_types/_time_t.h
s_gschh.o: /usr/include/sys/_types/_useconds_t.h
s_gschh.o: /usr/include/sys/_types/_suseconds_t.h
s_gschh.o: /usr/include/sys/_types/_rsize_t.h
s_gschh.o: /usr/include/sys/_types/_errno_t.h
s_gschh.o: /usr/include/sys/_types/_fd_def.h
s_gschh.o: /usr/include/sys/_types/_fd_setsize.h
s_gschh.o: /usr/include/sys/_types/_fd_set.h
s_gschh.o: /usr/include/sys/_types/_fd_clr.h
s_gschh.o: /usr/include/sys/_types/_fd_zero.h
s_gschh.o: /usr/include/sys/_types/_fd_isset.h
s_gschh.o: /usr/include/sys/_types/_fd_copy.h
s_gschh.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gschh.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gschh.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gschh.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gschh.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gschh.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gschh.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gschh.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gschh.o: /usr/include/sys/_pthread/_pthread_t.h
s_gschh.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gschh.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gschh.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gschh.o: ../../../.././include/ncarg/c.h
s_gschsp.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gschsp.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gschsp.o: /usr/include/sys/_posix_availability.h
s_gschsp.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gschsp.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gschsp.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gschsp.o: /usr/include/sys/_pthread/_pthread_types.h
s_gschsp.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gschsp.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gschsp.o: /usr/include/sys/_types/_int16_t.h
s_gschsp.o: /usr/include/sys/_types/_int32_t.h
s_gschsp.o: /usr/include/sys/_types/_int64_t.h
s_gschsp.o: /usr/include/sys/_types/_u_int8_t.h
s_gschsp.o: /usr/include/sys/_types/_u_int16_t.h
s_gschsp.o: /usr/include/sys/_types/_u_int32_t.h
s_gschsp.o: /usr/include/sys/_types/_u_int64_t.h
s_gschsp.o: /usr/include/sys/_types/_intptr_t.h
s_gschsp.o: /usr/include/sys/_types/_uintptr_t.h
s_gschsp.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gschsp.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gschsp.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gschsp.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gschsp.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gschsp.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gschsp.o: /usr/include/libkern/_OSByteOrder.h
s_gschsp.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gschsp.o: /usr/include/sys/_types/_u_char.h
s_gschsp.o: /usr/include/sys/_types/_u_short.h
s_gschsp.o: /usr/include/sys/_types/_u_int.h
s_gschsp.o: /usr/include/sys/_types/_caddr_t.h
s_gschsp.o: /usr/include/sys/_types/_dev_t.h
s_gschsp.o: /usr/include/sys/_types/_blkcnt_t.h
s_gschsp.o: /usr/include/sys/_types/_blksize_t.h
s_gschsp.o: /usr/include/sys/_types/_gid_t.h
s_gschsp.o: /usr/include/sys/_types/_in_addr_t.h
s_gschsp.o: /usr/include/sys/_types/_in_port_t.h
s_gschsp.o: /usr/include/sys/_types/_ino_t.h
s_gschsp.o: /usr/include/sys/_types/_ino64_t.h
s_gschsp.o: /usr/include/sys/_types/_key_t.h
s_gschsp.o: /usr/include/sys/_types/_mode_t.h
s_gschsp.o: /usr/include/sys/_types/_nlink_t.h
s_gschsp.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gschsp.o: /usr/include/sys/_types/_uid_t.h
s_gschsp.o: /usr/include/sys/_types/_clock_t.h
s_gschsp.o: /usr/include/sys/_types/_time_t.h
s_gschsp.o: /usr/include/sys/_types/_useconds_t.h
s_gschsp.o: /usr/include/sys/_types/_suseconds_t.h
s_gschsp.o: /usr/include/sys/_types/_rsize_t.h
s_gschsp.o: /usr/include/sys/_types/_errno_t.h
s_gschsp.o: /usr/include/sys/_types/_fd_def.h
s_gschsp.o: /usr/include/sys/_types/_fd_setsize.h
s_gschsp.o: /usr/include/sys/_types/_fd_set.h
s_gschsp.o: /usr/include/sys/_types/_fd_clr.h
s_gschsp.o: /usr/include/sys/_types/_fd_zero.h
s_gschsp.o: /usr/include/sys/_types/_fd_isset.h
s_gschsp.o: /usr/include/sys/_types/_fd_copy.h
s_gschsp.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gschsp.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gschsp.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gschsp.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gschsp.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gschsp.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gschsp.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gschsp.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gschsp.o: /usr/include/sys/_pthread/_pthread_t.h
s_gschsp.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gschsp.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gschsp.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gschsp.o: ../../../.././include/ncarg/c.h
s_gschup.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gschup.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gschup.o: /usr/include/sys/_posix_availability.h
s_gschup.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gschup.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gschup.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gschup.o: /usr/include/sys/_pthread/_pthread_types.h
s_gschup.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gschup.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gschup.o: /usr/include/sys/_types/_int16_t.h
s_gschup.o: /usr/include/sys/_types/_int32_t.h
s_gschup.o: /usr/include/sys/_types/_int64_t.h
s_gschup.o: /usr/include/sys/_types/_u_int8_t.h
s_gschup.o: /usr/include/sys/_types/_u_int16_t.h
s_gschup.o: /usr/include/sys/_types/_u_int32_t.h
s_gschup.o: /usr/include/sys/_types/_u_int64_t.h
s_gschup.o: /usr/include/sys/_types/_intptr_t.h
s_gschup.o: /usr/include/sys/_types/_uintptr_t.h
s_gschup.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gschup.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gschup.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gschup.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gschup.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gschup.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gschup.o: /usr/include/libkern/_OSByteOrder.h
s_gschup.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gschup.o: /usr/include/sys/_types/_u_char.h
s_gschup.o: /usr/include/sys/_types/_u_short.h
s_gschup.o: /usr/include/sys/_types/_u_int.h
s_gschup.o: /usr/include/sys/_types/_caddr_t.h
s_gschup.o: /usr/include/sys/_types/_dev_t.h
s_gschup.o: /usr/include/sys/_types/_blkcnt_t.h
s_gschup.o: /usr/include/sys/_types/_blksize_t.h
s_gschup.o: /usr/include/sys/_types/_gid_t.h
s_gschup.o: /usr/include/sys/_types/_in_addr_t.h
s_gschup.o: /usr/include/sys/_types/_in_port_t.h
s_gschup.o: /usr/include/sys/_types/_ino_t.h
s_gschup.o: /usr/include/sys/_types/_ino64_t.h
s_gschup.o: /usr/include/sys/_types/_key_t.h
s_gschup.o: /usr/include/sys/_types/_mode_t.h
s_gschup.o: /usr/include/sys/_types/_nlink_t.h
s_gschup.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gschup.o: /usr/include/sys/_types/_uid_t.h
s_gschup.o: /usr/include/sys/_types/_clock_t.h
s_gschup.o: /usr/include/sys/_types/_time_t.h
s_gschup.o: /usr/include/sys/_types/_useconds_t.h
s_gschup.o: /usr/include/sys/_types/_suseconds_t.h
s_gschup.o: /usr/include/sys/_types/_rsize_t.h
s_gschup.o: /usr/include/sys/_types/_errno_t.h
s_gschup.o: /usr/include/sys/_types/_fd_def.h
s_gschup.o: /usr/include/sys/_types/_fd_setsize.h
s_gschup.o: /usr/include/sys/_types/_fd_set.h
s_gschup.o: /usr/include/sys/_types/_fd_clr.h
s_gschup.o: /usr/include/sys/_types/_fd_zero.h
s_gschup.o: /usr/include/sys/_types/_fd_isset.h
s_gschup.o: /usr/include/sys/_types/_fd_copy.h
s_gschup.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gschup.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gschup.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gschup.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gschup.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gschup.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gschup.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gschup.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gschup.o: /usr/include/sys/_pthread/_pthread_t.h
s_gschup.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gschup.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gschup.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gschup.o: ../../../.././include/ncarg/c.h
s_gschxp.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gschxp.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gschxp.o: /usr/include/sys/_posix_availability.h
s_gschxp.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gschxp.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gschxp.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gschxp.o: /usr/include/sys/_pthread/_pthread_types.h
s_gschxp.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gschxp.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gschxp.o: /usr/include/sys/_types/_int16_t.h
s_gschxp.o: /usr/include/sys/_types/_int32_t.h
s_gschxp.o: /usr/include/sys/_types/_int64_t.h
s_gschxp.o: /usr/include/sys/_types/_u_int8_t.h
s_gschxp.o: /usr/include/sys/_types/_u_int16_t.h
s_gschxp.o: /usr/include/sys/_types/_u_int32_t.h
s_gschxp.o: /usr/include/sys/_types/_u_int64_t.h
s_gschxp.o: /usr/include/sys/_types/_intptr_t.h
s_gschxp.o: /usr/include/sys/_types/_uintptr_t.h
s_gschxp.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gschxp.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gschxp.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gschxp.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gschxp.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gschxp.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gschxp.o: /usr/include/libkern/_OSByteOrder.h
s_gschxp.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gschxp.o: /usr/include/sys/_types/_u_char.h
s_gschxp.o: /usr/include/sys/_types/_u_short.h
s_gschxp.o: /usr/include/sys/_types/_u_int.h
s_gschxp.o: /usr/include/sys/_types/_caddr_t.h
s_gschxp.o: /usr/include/sys/_types/_dev_t.h
s_gschxp.o: /usr/include/sys/_types/_blkcnt_t.h
s_gschxp.o: /usr/include/sys/_types/_blksize_t.h
s_gschxp.o: /usr/include/sys/_types/_gid_t.h
s_gschxp.o: /usr/include/sys/_types/_in_addr_t.h
s_gschxp.o: /usr/include/sys/_types/_in_port_t.h
s_gschxp.o: /usr/include/sys/_types/_ino_t.h
s_gschxp.o: /usr/include/sys/_types/_ino64_t.h
s_gschxp.o: /usr/include/sys/_types/_key_t.h
s_gschxp.o: /usr/include/sys/_types/_mode_t.h
s_gschxp.o: /usr/include/sys/_types/_nlink_t.h
s_gschxp.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gschxp.o: /usr/include/sys/_types/_uid_t.h
s_gschxp.o: /usr/include/sys/_types/_clock_t.h
s_gschxp.o: /usr/include/sys/_types/_time_t.h
s_gschxp.o: /usr/include/sys/_types/_useconds_t.h
s_gschxp.o: /usr/include/sys/_types/_suseconds_t.h
s_gschxp.o: /usr/include/sys/_types/_rsize_t.h
s_gschxp.o: /usr/include/sys/_types/_errno_t.h
s_gschxp.o: /usr/include/sys/_types/_fd_def.h
s_gschxp.o: /usr/include/sys/_types/_fd_setsize.h
s_gschxp.o: /usr/include/sys/_types/_fd_set.h
s_gschxp.o: /usr/include/sys/_types/_fd_clr.h
s_gschxp.o: /usr/include/sys/_types/_fd_zero.h
s_gschxp.o: /usr/include/sys/_types/_fd_isset.h
s_gschxp.o: /usr/include/sys/_types/_fd_copy.h
s_gschxp.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gschxp.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gschxp.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gschxp.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gschxp.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gschxp.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gschxp.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gschxp.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gschxp.o: /usr/include/sys/_pthread/_pthread_t.h
s_gschxp.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gschxp.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gschxp.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gschxp.o: ../../../.././include/ncarg/c.h
s_gsclip.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gsclip.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gsclip.o: /usr/include/sys/_posix_availability.h
s_gsclip.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gsclip.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gsclip.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gsclip.o: /usr/include/sys/_pthread/_pthread_types.h
s_gsclip.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gsclip.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gsclip.o: /usr/include/sys/_types/_int16_t.h
s_gsclip.o: /usr/include/sys/_types/_int32_t.h
s_gsclip.o: /usr/include/sys/_types/_int64_t.h
s_gsclip.o: /usr/include/sys/_types/_u_int8_t.h
s_gsclip.o: /usr/include/sys/_types/_u_int16_t.h
s_gsclip.o: /usr/include/sys/_types/_u_int32_t.h
s_gsclip.o: /usr/include/sys/_types/_u_int64_t.h
s_gsclip.o: /usr/include/sys/_types/_intptr_t.h
s_gsclip.o: /usr/include/sys/_types/_uintptr_t.h
s_gsclip.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gsclip.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gsclip.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gsclip.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gsclip.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gsclip.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gsclip.o: /usr/include/libkern/_OSByteOrder.h
s_gsclip.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gsclip.o: /usr/include/sys/_types/_u_char.h
s_gsclip.o: /usr/include/sys/_types/_u_short.h
s_gsclip.o: /usr/include/sys/_types/_u_int.h
s_gsclip.o: /usr/include/sys/_types/_caddr_t.h
s_gsclip.o: /usr/include/sys/_types/_dev_t.h
s_gsclip.o: /usr/include/sys/_types/_blkcnt_t.h
s_gsclip.o: /usr/include/sys/_types/_blksize_t.h
s_gsclip.o: /usr/include/sys/_types/_gid_t.h
s_gsclip.o: /usr/include/sys/_types/_in_addr_t.h
s_gsclip.o: /usr/include/sys/_types/_in_port_t.h
s_gsclip.o: /usr/include/sys/_types/_ino_t.h
s_gsclip.o: /usr/include/sys/_types/_ino64_t.h
s_gsclip.o: /usr/include/sys/_types/_key_t.h
s_gsclip.o: /usr/include/sys/_types/_mode_t.h
s_gsclip.o: /usr/include/sys/_types/_nlink_t.h
s_gsclip.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gsclip.o: /usr/include/sys/_types/_uid_t.h
s_gsclip.o: /usr/include/sys/_types/_clock_t.h
s_gsclip.o: /usr/include/sys/_types/_time_t.h
s_gsclip.o: /usr/include/sys/_types/_useconds_t.h
s_gsclip.o: /usr/include/sys/_types/_suseconds_t.h
s_gsclip.o: /usr/include/sys/_types/_rsize_t.h
s_gsclip.o: /usr/include/sys/_types/_errno_t.h
s_gsclip.o: /usr/include/sys/_types/_fd_def.h
s_gsclip.o: /usr/include/sys/_types/_fd_setsize.h
s_gsclip.o: /usr/include/sys/_types/_fd_set.h
s_gsclip.o: /usr/include/sys/_types/_fd_clr.h
s_gsclip.o: /usr/include/sys/_types/_fd_zero.h
s_gsclip.o: /usr/include/sys/_types/_fd_isset.h
s_gsclip.o: /usr/include/sys/_types/_fd_copy.h
s_gsclip.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gsclip.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gsclip.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gsclip.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gsclip.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gsclip.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gsclip.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gsclip.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gsclip.o: /usr/include/sys/_pthread/_pthread_t.h
s_gsclip.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gsclip.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gsclip.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gsclip.o: ../../../.././include/ncarg/c.h
s_gscr.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gscr.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gscr.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gscr.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gscr.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gscr.o: /usr/include/i386/_types.h
s_gscr.o: /usr/include/sys/_pthread/_pthread_types.h
s_gscr.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gscr.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gscr.o: /usr/include/sys/_types/_int16_t.h
s_gscr.o: /usr/include/sys/_types/_int32_t.h
s_gscr.o: /usr/include/sys/_types/_int64_t.h
s_gscr.o: /usr/include/sys/_types/_u_int8_t.h
s_gscr.o: /usr/include/sys/_types/_u_int16_t.h
s_gscr.o: /usr/include/sys/_types/_u_int32_t.h
s_gscr.o: /usr/include/sys/_types/_u_int64_t.h
s_gscr.o: /usr/include/sys/_types/_intptr_t.h
s_gscr.o: /usr/include/sys/_types/_uintptr_t.h
s_gscr.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gscr.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gscr.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gscr.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gscr.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gscr.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gscr.o: /usr/include/libkern/_OSByteOrder.h
s_gscr.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gscr.o: /usr/include/sys/_types/_u_char.h
s_gscr.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
s_gscr.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
s_gscr.o: /usr/include/sys/_types/_blkcnt_t.h
s_gscr.o: /usr/include/sys/_types/_blksize_t.h
s_gscr.o: /usr/include/sys/_types/_gid_t.h
s_gscr.o: /usr/include/sys/_types/_in_addr_t.h
s_gscr.o: /usr/include/sys/_types/_in_port_t.h
s_gscr.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
s_gscr.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gscr.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gscr.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gscr.o: /usr/include/sys/_types/_clock_t.h
s_gscr.o: /usr/include/sys/_types/_time_t.h
s_gscr.o: /usr/include/sys/_types/_useconds_t.h
s_gscr.o: /usr/include/sys/_types/_suseconds_t.h
s_gscr.o: /usr/include/sys/_types/_rsize_t.h
s_gscr.o: /usr/include/sys/_types/_errno_t.h
s_gscr.o: /usr/include/sys/_types/_fd_def.h
s_gscr.o: /usr/include/sys/_types/_fd_setsize.h
s_gscr.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
s_gscr.o: /usr/include/sys/_types/_fd_zero.h
s_gscr.o: /usr/include/sys/_types/_fd_isset.h
s_gscr.o: /usr/include/sys/_types/_fd_copy.h
s_gscr.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gscr.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gscr.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gscr.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gscr.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gscr.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gscr.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gscr.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gscr.o: /usr/include/sys/_pthread/_pthread_t.h
s_gscr.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gscr.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gscr.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gscr.o: ../../../.././include/ncarg/c.h
s_gselnt.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gselnt.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gselnt.o: /usr/include/sys/_posix_availability.h
s_gselnt.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gselnt.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gselnt.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gselnt.o: /usr/include/sys/_pthread/_pthread_types.h
s_gselnt.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gselnt.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gselnt.o: /usr/include/sys/_types/_int16_t.h
s_gselnt.o: /usr/include/sys/_types/_int32_t.h
s_gselnt.o: /usr/include/sys/_types/_int64_t.h
s_gselnt.o: /usr/include/sys/_types/_u_int8_t.h
s_gselnt.o: /usr/include/sys/_types/_u_int16_t.h
s_gselnt.o: /usr/include/sys/_types/_u_int32_t.h
s_gselnt.o: /usr/include/sys/_types/_u_int64_t.h
s_gselnt.o: /usr/include/sys/_types/_intptr_t.h
s_gselnt.o: /usr/include/sys/_types/_uintptr_t.h
s_gselnt.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gselnt.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gselnt.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gselnt.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gselnt.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gselnt.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gselnt.o: /usr/include/libkern/_OSByteOrder.h
s_gselnt.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gselnt.o: /usr/include/sys/_types/_u_char.h
s_gselnt.o: /usr/include/sys/_types/_u_short.h
s_gselnt.o: /usr/include/sys/_types/_u_int.h
s_gselnt.o: /usr/include/sys/_types/_caddr_t.h
s_gselnt.o: /usr/include/sys/_types/_dev_t.h
s_gselnt.o: /usr/include/sys/_types/_blkcnt_t.h
s_gselnt.o: /usr/include/sys/_types/_blksize_t.h
s_gselnt.o: /usr/include/sys/_types/_gid_t.h
s_gselnt.o: /usr/include/sys/_types/_in_addr_t.h
s_gselnt.o: /usr/include/sys/_types/_in_port_t.h
s_gselnt.o: /usr/include/sys/_types/_ino_t.h
s_gselnt.o: /usr/include/sys/_types/_ino64_t.h
s_gselnt.o: /usr/include/sys/_types/_key_t.h
s_gselnt.o: /usr/include/sys/_types/_mode_t.h
s_gselnt.o: /usr/include/sys/_types/_nlink_t.h
s_gselnt.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gselnt.o: /usr/include/sys/_types/_uid_t.h
s_gselnt.o: /usr/include/sys/_types/_clock_t.h
s_gselnt.o: /usr/include/sys/_types/_time_t.h
s_gselnt.o: /usr/include/sys/_types/_useconds_t.h
s_gselnt.o: /usr/include/sys/_types/_suseconds_t.h
s_gselnt.o: /usr/include/sys/_types/_rsize_t.h
s_gselnt.o: /usr/include/sys/_types/_errno_t.h
s_gselnt.o: /usr/include/sys/_types/_fd_def.h
s_gselnt.o: /usr/include/sys/_types/_fd_setsize.h
s_gselnt.o: /usr/include/sys/_types/_fd_set.h
s_gselnt.o: /usr/include/sys/_types/_fd_clr.h
s_gselnt.o: /usr/include/sys/_types/_fd_zero.h
s_gselnt.o: /usr/include/sys/_types/_fd_isset.h
s_gselnt.o: /usr/include/sys/_types/_fd_copy.h
s_gselnt.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gselnt.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gselnt.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gselnt.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gselnt.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gselnt.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gselnt.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gselnt.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gselnt.o: /usr/include/sys/_pthread/_pthread_t.h
s_gselnt.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gselnt.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gselnt.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gselnt.o: ../../../.././include/ncarg/c.h
s_gsfaci.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gsfaci.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gsfaci.o: /usr/include/sys/_posix_availability.h
s_gsfaci.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gsfaci.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gsfaci.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gsfaci.o: /usr/include/sys/_pthread/_pthread_types.h
s_gsfaci.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gsfaci.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gsfaci.o: /usr/include/sys/_types/_int16_t.h
s_gsfaci.o: /usr/include/sys/_types/_int32_t.h
s_gsfaci.o: /usr/include/sys/_types/_int64_t.h
s_gsfaci.o: /usr/include/sys/_types/_u_int8_t.h
s_gsfaci.o: /usr/include/sys/_types/_u_int16_t.h
s_gsfaci.o: /usr/include/sys/_types/_u_int32_t.h
s_gsfaci.o: /usr/include/sys/_types/_u_int64_t.h
s_gsfaci.o: /usr/include/sys/_types/_intptr_t.h
s_gsfaci.o: /usr/include/sys/_types/_uintptr_t.h
s_gsfaci.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gsfaci.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gsfaci.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gsfaci.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gsfaci.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gsfaci.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gsfaci.o: /usr/include/libkern/_OSByteOrder.h
s_gsfaci.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gsfaci.o: /usr/include/sys/_types/_u_char.h
s_gsfaci.o: /usr/include/sys/_types/_u_short.h
s_gsfaci.o: /usr/include/sys/_types/_u_int.h
s_gsfaci.o: /usr/include/sys/_types/_caddr_t.h
s_gsfaci.o: /usr/include/sys/_types/_dev_t.h
s_gsfaci.o: /usr/include/sys/_types/_blkcnt_t.h
s_gsfaci.o: /usr/include/sys/_types/_blksize_t.h
s_gsfaci.o: /usr/include/sys/_types/_gid_t.h
s_gsfaci.o: /usr/include/sys/_types/_in_addr_t.h
s_gsfaci.o: /usr/include/sys/_types/_in_port_t.h
s_gsfaci.o: /usr/include/sys/_types/_ino_t.h
s_gsfaci.o: /usr/include/sys/_types/_ino64_t.h
s_gsfaci.o: /usr/include/sys/_types/_key_t.h
s_gsfaci.o: /usr/include/sys/_types/_mode_t.h
s_gsfaci.o: /usr/include/sys/_types/_nlink_t.h
s_gsfaci.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gsfaci.o: /usr/include/sys/_types/_uid_t.h
s_gsfaci.o: /usr/include/sys/_types/_clock_t.h
s_gsfaci.o: /usr/include/sys/_types/_time_t.h
s_gsfaci.o: /usr/include/sys/_types/_useconds_t.h
s_gsfaci.o: /usr/include/sys/_types/_suseconds_t.h
s_gsfaci.o: /usr/include/sys/_types/_rsize_t.h
s_gsfaci.o: /usr/include/sys/_types/_errno_t.h
s_gsfaci.o: /usr/include/sys/_types/_fd_def.h
s_gsfaci.o: /usr/include/sys/_types/_fd_setsize.h
s_gsfaci.o: /usr/include/sys/_types/_fd_set.h
s_gsfaci.o: /usr/include/sys/_types/_fd_clr.h
s_gsfaci.o: /usr/include/sys/_types/_fd_zero.h
s_gsfaci.o: /usr/include/sys/_types/_fd_isset.h
s_gsfaci.o: /usr/include/sys/_types/_fd_copy.h
s_gsfaci.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gsfaci.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gsfaci.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gsfaci.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gsfaci.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gsfaci.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gsfaci.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gsfaci.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gsfaci.o: /usr/include/sys/_pthread/_pthread_t.h
s_gsfaci.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gsfaci.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gsfaci.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gsfaci.o: ../../../.././include/ncarg/c.h
s_gsfai.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gsfai.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gsfai.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gsfai.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gsfai.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gsfai.o: /usr/include/i386/_types.h
s_gsfai.o: /usr/include/sys/_pthread/_pthread_types.h
s_gsfai.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gsfai.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gsfai.o: /usr/include/sys/_types/_int16_t.h
s_gsfai.o: /usr/include/sys/_types/_int32_t.h
s_gsfai.o: /usr/include/sys/_types/_int64_t.h
s_gsfai.o: /usr/include/sys/_types/_u_int8_t.h
s_gsfai.o: /usr/include/sys/_types/_u_int16_t.h
s_gsfai.o: /usr/include/sys/_types/_u_int32_t.h
s_gsfai.o: /usr/include/sys/_types/_u_int64_t.h
s_gsfai.o: /usr/include/sys/_types/_intptr_t.h
s_gsfai.o: /usr/include/sys/_types/_uintptr_t.h
s_gsfai.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gsfai.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gsfai.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gsfai.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gsfai.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gsfai.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gsfai.o: /usr/include/libkern/_OSByteOrder.h
s_gsfai.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gsfai.o: /usr/include/sys/_types/_u_char.h
s_gsfai.o: /usr/include/sys/_types/_u_short.h
s_gsfai.o: /usr/include/sys/_types/_u_int.h
s_gsfai.o: /usr/include/sys/_types/_caddr_t.h
s_gsfai.o: /usr/include/sys/_types/_dev_t.h
s_gsfai.o: /usr/include/sys/_types/_blkcnt_t.h
s_gsfai.o: /usr/include/sys/_types/_blksize_t.h
s_gsfai.o: /usr/include/sys/_types/_gid_t.h
s_gsfai.o: /usr/include/sys/_types/_in_addr_t.h
s_gsfai.o: /usr/include/sys/_types/_in_port_t.h
s_gsfai.o: /usr/include/sys/_types/_ino_t.h
s_gsfai.o: /usr/include/sys/_types/_ino64_t.h
s_gsfai.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gsfai.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gsfai.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gsfai.o: /usr/include/sys/_types/_clock_t.h
s_gsfai.o: /usr/include/sys/_types/_time_t.h
s_gsfai.o: /usr/include/sys/_types/_useconds_t.h
s_gsfai.o: /usr/include/sys/_types/_suseconds_t.h
s_gsfai.o: /usr/include/sys/_types/_rsize_t.h
s_gsfai.o: /usr/include/sys/_types/_errno_t.h
s_gsfai.o: /usr/include/sys/_types/_fd_def.h
s_gsfai.o: /usr/include/sys/_types/_fd_setsize.h
s_gsfai.o: /usr/include/sys/_types/_fd_set.h
s_gsfai.o: /usr/include/sys/_types/_fd_clr.h
s_gsfai.o: /usr/include/sys/_types/_fd_zero.h
s_gsfai.o: /usr/include/sys/_types/_fd_isset.h
s_gsfai.o: /usr/include/sys/_types/_fd_copy.h
s_gsfai.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gsfai.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gsfai.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gsfai.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gsfai.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gsfai.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gsfai.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gsfai.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gsfai.o: /usr/include/sys/_pthread/_pthread_t.h
s_gsfai.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gsfai.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gsfai.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gsfai.o: ../../../.././include/ncarg/c.h
s_gsfais.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gsfais.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gsfais.o: /usr/include/sys/_posix_availability.h
s_gsfais.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gsfais.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gsfais.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gsfais.o: /usr/include/sys/_pthread/_pthread_types.h
s_gsfais.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gsfais.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gsfais.o: /usr/include/sys/_types/_int16_t.h
s_gsfais.o: /usr/include/sys/_types/_int32_t.h
s_gsfais.o: /usr/include/sys/_types/_int64_t.h
s_gsfais.o: /usr/include/sys/_types/_u_int8_t.h
s_gsfais.o: /usr/include/sys/_types/_u_int16_t.h
s_gsfais.o: /usr/include/sys/_types/_u_int32_t.h
s_gsfais.o: /usr/include/sys/_types/_u_int64_t.h
s_gsfais.o: /usr/include/sys/_types/_intptr_t.h
s_gsfais.o: /usr/include/sys/_types/_uintptr_t.h
s_gsfais.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gsfais.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gsfais.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gsfais.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gsfais.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gsfais.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gsfais.o: /usr/include/libkern/_OSByteOrder.h
s_gsfais.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gsfais.o: /usr/include/sys/_types/_u_char.h
s_gsfais.o: /usr/include/sys/_types/_u_short.h
s_gsfais.o: /usr/include/sys/_types/_u_int.h
s_gsfais.o: /usr/include/sys/_types/_caddr_t.h
s_gsfais.o: /usr/include/sys/_types/_dev_t.h
s_gsfais.o: /usr/include/sys/_types/_blkcnt_t.h
s_gsfais.o: /usr/include/sys/_types/_blksize_t.h
s_gsfais.o: /usr/include/sys/_types/_gid_t.h
s_gsfais.o: /usr/include/sys/_types/_in_addr_t.h
s_gsfais.o: /usr/include/sys/_types/_in_port_t.h
s_gsfais.o: /usr/include/sys/_types/_ino_t.h
s_gsfais.o: /usr/include/sys/_types/_ino64_t.h
s_gsfais.o: /usr/include/sys/_types/_key_t.h
s_gsfais.o: /usr/include/sys/_types/_mode_t.h
s_gsfais.o: /usr/include/sys/_types/_nlink_t.h
s_gsfais.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gsfais.o: /usr/include/sys/_types/_uid_t.h
s_gsfais.o: /usr/include/sys/_types/_clock_t.h
s_gsfais.o: /usr/include/sys/_types/_time_t.h
s_gsfais.o: /usr/include/sys/_types/_useconds_t.h
s_gsfais.o: /usr/include/sys/_types/_suseconds_t.h
s_gsfais.o: /usr/include/sys/_types/_rsize_t.h
s_gsfais.o: /usr/include/sys/_types/_errno_t.h
s_gsfais.o: /usr/include/sys/_types/_fd_def.h
s_gsfais.o: /usr/include/sys/_types/_fd_setsize.h
s_gsfais.o: /usr/include/sys/_types/_fd_set.h
s_gsfais.o: /usr/include/sys/_types/_fd_clr.h
s_gsfais.o: /usr/include/sys/_types/_fd_zero.h
s_gsfais.o: /usr/include/sys/_types/_fd_isset.h
s_gsfais.o: /usr/include/sys/_types/_fd_copy.h
s_gsfais.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gsfais.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gsfais.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gsfais.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gsfais.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gsfais.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gsfais.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gsfais.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gsfais.o: /usr/include/sys/_pthread/_pthread_t.h
s_gsfais.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gsfais.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gsfais.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gsfais.o: ../../../.././include/ncarg/c.h
s_gsfasi.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gsfasi.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gsfasi.o: /usr/include/sys/_posix_availability.h
s_gsfasi.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gsfasi.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gsfasi.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gsfasi.o: /usr/include/sys/_pthread/_pthread_types.h
s_gsfasi.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gsfasi.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gsfasi.o: /usr/include/sys/_types/_int16_t.h
s_gsfasi.o: /usr/include/sys/_types/_int32_t.h
s_gsfasi.o: /usr/include/sys/_types/_int64_t.h
s_gsfasi.o: /usr/include/sys/_types/_u_int8_t.h
s_gsfasi.o: /usr/include/sys/_types/_u_int16_t.h
s_gsfasi.o: /usr/include/sys/_types/_u_int32_t.h
s_gsfasi.o: /usr/include/sys/_types/_u_int64_t.h
s_gsfasi.o: /usr/include/sys/_types/_intptr_t.h
s_gsfasi.o: /usr/include/sys/_types/_uintptr_t.h
s_gsfasi.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gsfasi.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gsfasi.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gsfasi.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gsfasi.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gsfasi.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gsfasi.o: /usr/include/libkern/_OSByteOrder.h
s_gsfasi.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gsfasi.o: /usr/include/sys/_types/_u_char.h
s_gsfasi.o: /usr/include/sys/_types/_u_short.h
s_gsfasi.o: /usr/include/sys/_types/_u_int.h
s_gsfasi.o: /usr/include/sys/_types/_caddr_t.h
s_gsfasi.o: /usr/include/sys/_types/_dev_t.h
s_gsfasi.o: /usr/include/sys/_types/_blkcnt_t.h
s_gsfasi.o: /usr/include/sys/_types/_blksize_t.h
s_gsfasi.o: /usr/include/sys/_types/_gid_t.h
s_gsfasi.o: /usr/include/sys/_types/_in_addr_t.h
s_gsfasi.o: /usr/include/sys/_types/_in_port_t.h
s_gsfasi.o: /usr/include/sys/_types/_ino_t.h
s_gsfasi.o: /usr/include/sys/_types/_ino64_t.h
s_gsfasi.o: /usr/include/sys/_types/_key_t.h
s_gsfasi.o: /usr/include/sys/_types/_mode_t.h
s_gsfasi.o: /usr/include/sys/_types/_nlink_t.h
s_gsfasi.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gsfasi.o: /usr/include/sys/_types/_uid_t.h
s_gsfasi.o: /usr/include/sys/_types/_clock_t.h
s_gsfasi.o: /usr/include/sys/_types/_time_t.h
s_gsfasi.o: /usr/include/sys/_types/_useconds_t.h
s_gsfasi.o: /usr/include/sys/_types/_suseconds_t.h
s_gsfasi.o: /usr/include/sys/_types/_rsize_t.h
s_gsfasi.o: /usr/include/sys/_types/_errno_t.h
s_gsfasi.o: /usr/include/sys/_types/_fd_def.h
s_gsfasi.o: /usr/include/sys/_types/_fd_setsize.h
s_gsfasi.o: /usr/include/sys/_types/_fd_set.h
s_gsfasi.o: /usr/include/sys/_types/_fd_clr.h
s_gsfasi.o: /usr/include/sys/_types/_fd_zero.h
s_gsfasi.o: /usr/include/sys/_types/_fd_isset.h
s_gsfasi.o: /usr/include/sys/_types/_fd_copy.h
s_gsfasi.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gsfasi.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gsfasi.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gsfasi.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gsfasi.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gsfasi.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gsfasi.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gsfasi.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gsfasi.o: /usr/include/sys/_pthread/_pthread_t.h
s_gsfasi.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gsfasi.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gsfasi.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gsfasi.o: ../../../.././include/ncarg/c.h
s_gsln.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gsln.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gsln.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gsln.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gsln.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gsln.o: /usr/include/i386/_types.h
s_gsln.o: /usr/include/sys/_pthread/_pthread_types.h
s_gsln.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gsln.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gsln.o: /usr/include/sys/_types/_int16_t.h
s_gsln.o: /usr/include/sys/_types/_int32_t.h
s_gsln.o: /usr/include/sys/_types/_int64_t.h
s_gsln.o: /usr/include/sys/_types/_u_int8_t.h
s_gsln.o: /usr/include/sys/_types/_u_int16_t.h
s_gsln.o: /usr/include/sys/_types/_u_int32_t.h
s_gsln.o: /usr/include/sys/_types/_u_int64_t.h
s_gsln.o: /usr/include/sys/_types/_intptr_t.h
s_gsln.o: /usr/include/sys/_types/_uintptr_t.h
s_gsln.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gsln.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gsln.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gsln.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gsln.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gsln.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gsln.o: /usr/include/libkern/_OSByteOrder.h
s_gsln.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gsln.o: /usr/include/sys/_types/_u_char.h
s_gsln.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
s_gsln.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
s_gsln.o: /usr/include/sys/_types/_blkcnt_t.h
s_gsln.o: /usr/include/sys/_types/_blksize_t.h
s_gsln.o: /usr/include/sys/_types/_gid_t.h
s_gsln.o: /usr/include/sys/_types/_in_addr_t.h
s_gsln.o: /usr/include/sys/_types/_in_port_t.h
s_gsln.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
s_gsln.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gsln.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gsln.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gsln.o: /usr/include/sys/_types/_clock_t.h
s_gsln.o: /usr/include/sys/_types/_time_t.h
s_gsln.o: /usr/include/sys/_types/_useconds_t.h
s_gsln.o: /usr/include/sys/_types/_suseconds_t.h
s_gsln.o: /usr/include/sys/_types/_rsize_t.h
s_gsln.o: /usr/include/sys/_types/_errno_t.h
s_gsln.o: /usr/include/sys/_types/_fd_def.h
s_gsln.o: /usr/include/sys/_types/_fd_setsize.h
s_gsln.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
s_gsln.o: /usr/include/sys/_types/_fd_zero.h
s_gsln.o: /usr/include/sys/_types/_fd_isset.h
s_gsln.o: /usr/include/sys/_types/_fd_copy.h
s_gsln.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gsln.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gsln.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gsln.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gsln.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gsln.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gsln.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gsln.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gsln.o: /usr/include/sys/_pthread/_pthread_t.h
s_gsln.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gsln.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gsln.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gsln.o: ../../../.././include/ncarg/c.h
s_gslwsc.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gslwsc.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gslwsc.o: /usr/include/sys/_posix_availability.h
s_gslwsc.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gslwsc.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gslwsc.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gslwsc.o: /usr/include/sys/_pthread/_pthread_types.h
s_gslwsc.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gslwsc.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gslwsc.o: /usr/include/sys/_types/_int16_t.h
s_gslwsc.o: /usr/include/sys/_types/_int32_t.h
s_gslwsc.o: /usr/include/sys/_types/_int64_t.h
s_gslwsc.o: /usr/include/sys/_types/_u_int8_t.h
s_gslwsc.o: /usr/include/sys/_types/_u_int16_t.h
s_gslwsc.o: /usr/include/sys/_types/_u_int32_t.h
s_gslwsc.o: /usr/include/sys/_types/_u_int64_t.h
s_gslwsc.o: /usr/include/sys/_types/_intptr_t.h
s_gslwsc.o: /usr/include/sys/_types/_uintptr_t.h
s_gslwsc.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gslwsc.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gslwsc.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gslwsc.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gslwsc.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gslwsc.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gslwsc.o: /usr/include/libkern/_OSByteOrder.h
s_gslwsc.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gslwsc.o: /usr/include/sys/_types/_u_char.h
s_gslwsc.o: /usr/include/sys/_types/_u_short.h
s_gslwsc.o: /usr/include/sys/_types/_u_int.h
s_gslwsc.o: /usr/include/sys/_types/_caddr_t.h
s_gslwsc.o: /usr/include/sys/_types/_dev_t.h
s_gslwsc.o: /usr/include/sys/_types/_blkcnt_t.h
s_gslwsc.o: /usr/include/sys/_types/_blksize_t.h
s_gslwsc.o: /usr/include/sys/_types/_gid_t.h
s_gslwsc.o: /usr/include/sys/_types/_in_addr_t.h
s_gslwsc.o: /usr/include/sys/_types/_in_port_t.h
s_gslwsc.o: /usr/include/sys/_types/_ino_t.h
s_gslwsc.o: /usr/include/sys/_types/_ino64_t.h
s_gslwsc.o: /usr/include/sys/_types/_key_t.h
s_gslwsc.o: /usr/include/sys/_types/_mode_t.h
s_gslwsc.o: /usr/include/sys/_types/_nlink_t.h
s_gslwsc.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gslwsc.o: /usr/include/sys/_types/_uid_t.h
s_gslwsc.o: /usr/include/sys/_types/_clock_t.h
s_gslwsc.o: /usr/include/sys/_types/_time_t.h
s_gslwsc.o: /usr/include/sys/_types/_useconds_t.h
s_gslwsc.o: /usr/include/sys/_types/_suseconds_t.h
s_gslwsc.o: /usr/include/sys/_types/_rsize_t.h
s_gslwsc.o: /usr/include/sys/_types/_errno_t.h
s_gslwsc.o: /usr/include/sys/_types/_fd_def.h
s_gslwsc.o: /usr/include/sys/_types/_fd_setsize.h
s_gslwsc.o: /usr/include/sys/_types/_fd_set.h
s_gslwsc.o: /usr/include/sys/_types/_fd_clr.h
s_gslwsc.o: /usr/include/sys/_types/_fd_zero.h
s_gslwsc.o: /usr/include/sys/_types/_fd_isset.h
s_gslwsc.o: /usr/include/sys/_types/_fd_copy.h
s_gslwsc.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gslwsc.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gslwsc.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gslwsc.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gslwsc.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gslwsc.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gslwsc.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gslwsc.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gslwsc.o: /usr/include/sys/_pthread/_pthread_t.h
s_gslwsc.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gslwsc.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gslwsc.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gslwsc.o: ../../../.././include/ncarg/c.h
s_gsmk.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gsmk.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gsmk.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gsmk.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gsmk.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gsmk.o: /usr/include/i386/_types.h
s_gsmk.o: /usr/include/sys/_pthread/_pthread_types.h
s_gsmk.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gsmk.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gsmk.o: /usr/include/sys/_types/_int16_t.h
s_gsmk.o: /usr/include/sys/_types/_int32_t.h
s_gsmk.o: /usr/include/sys/_types/_int64_t.h
s_gsmk.o: /usr/include/sys/_types/_u_int8_t.h
s_gsmk.o: /usr/include/sys/_types/_u_int16_t.h
s_gsmk.o: /usr/include/sys/_types/_u_int32_t.h
s_gsmk.o: /usr/include/sys/_types/_u_int64_t.h
s_gsmk.o: /usr/include/sys/_types/_intptr_t.h
s_gsmk.o: /usr/include/sys/_types/_uintptr_t.h
s_gsmk.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gsmk.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gsmk.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gsmk.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gsmk.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gsmk.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gsmk.o: /usr/include/libkern/_OSByteOrder.h
s_gsmk.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gsmk.o: /usr/include/sys/_types/_u_char.h
s_gsmk.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
s_gsmk.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
s_gsmk.o: /usr/include/sys/_types/_blkcnt_t.h
s_gsmk.o: /usr/include/sys/_types/_blksize_t.h
s_gsmk.o: /usr/include/sys/_types/_gid_t.h
s_gsmk.o: /usr/include/sys/_types/_in_addr_t.h
s_gsmk.o: /usr/include/sys/_types/_in_port_t.h
s_gsmk.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
s_gsmk.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gsmk.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gsmk.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gsmk.o: /usr/include/sys/_types/_clock_t.h
s_gsmk.o: /usr/include/sys/_types/_time_t.h
s_gsmk.o: /usr/include/sys/_types/_useconds_t.h
s_gsmk.o: /usr/include/sys/_types/_suseconds_t.h
s_gsmk.o: /usr/include/sys/_types/_rsize_t.h
s_gsmk.o: /usr/include/sys/_types/_errno_t.h
s_gsmk.o: /usr/include/sys/_types/_fd_def.h
s_gsmk.o: /usr/include/sys/_types/_fd_setsize.h
s_gsmk.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
s_gsmk.o: /usr/include/sys/_types/_fd_zero.h
s_gsmk.o: /usr/include/sys/_types/_fd_isset.h
s_gsmk.o: /usr/include/sys/_types/_fd_copy.h
s_gsmk.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gsmk.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gsmk.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gsmk.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gsmk.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gsmk.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gsmk.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gsmk.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gsmk.o: /usr/include/sys/_pthread/_pthread_t.h
s_gsmk.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gsmk.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gsmk.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gsmk.o: ../../../.././include/ncarg/c.h
s_gsmksc.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gsmksc.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gsmksc.o: /usr/include/sys/_posix_availability.h
s_gsmksc.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gsmksc.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gsmksc.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gsmksc.o: /usr/include/sys/_pthread/_pthread_types.h
s_gsmksc.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gsmksc.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gsmksc.o: /usr/include/sys/_types/_int16_t.h
s_gsmksc.o: /usr/include/sys/_types/_int32_t.h
s_gsmksc.o: /usr/include/sys/_types/_int64_t.h
s_gsmksc.o: /usr/include/sys/_types/_u_int8_t.h
s_gsmksc.o: /usr/include/sys/_types/_u_int16_t.h
s_gsmksc.o: /usr/include/sys/_types/_u_int32_t.h
s_gsmksc.o: /usr/include/sys/_types/_u_int64_t.h
s_gsmksc.o: /usr/include/sys/_types/_intptr_t.h
s_gsmksc.o: /usr/include/sys/_types/_uintptr_t.h
s_gsmksc.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gsmksc.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gsmksc.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gsmksc.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gsmksc.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gsmksc.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gsmksc.o: /usr/include/libkern/_OSByteOrder.h
s_gsmksc.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gsmksc.o: /usr/include/sys/_types/_u_char.h
s_gsmksc.o: /usr/include/sys/_types/_u_short.h
s_gsmksc.o: /usr/include/sys/_types/_u_int.h
s_gsmksc.o: /usr/include/sys/_types/_caddr_t.h
s_gsmksc.o: /usr/include/sys/_types/_dev_t.h
s_gsmksc.o: /usr/include/sys/_types/_blkcnt_t.h
s_gsmksc.o: /usr/include/sys/_types/_blksize_t.h
s_gsmksc.o: /usr/include/sys/_types/_gid_t.h
s_gsmksc.o: /usr/include/sys/_types/_in_addr_t.h
s_gsmksc.o: /usr/include/sys/_types/_in_port_t.h
s_gsmksc.o: /usr/include/sys/_types/_ino_t.h
s_gsmksc.o: /usr/include/sys/_types/_ino64_t.h
s_gsmksc.o: /usr/include/sys/_types/_key_t.h
s_gsmksc.o: /usr/include/sys/_types/_mode_t.h
s_gsmksc.o: /usr/include/sys/_types/_nlink_t.h
s_gsmksc.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gsmksc.o: /usr/include/sys/_types/_uid_t.h
s_gsmksc.o: /usr/include/sys/_types/_clock_t.h
s_gsmksc.o: /usr/include/sys/_types/_time_t.h
s_gsmksc.o: /usr/include/sys/_types/_useconds_t.h
s_gsmksc.o: /usr/include/sys/_types/_suseconds_t.h
s_gsmksc.o: /usr/include/sys/_types/_rsize_t.h
s_gsmksc.o: /usr/include/sys/_types/_errno_t.h
s_gsmksc.o: /usr/include/sys/_types/_fd_def.h
s_gsmksc.o: /usr/include/sys/_types/_fd_setsize.h
s_gsmksc.o: /usr/include/sys/_types/_fd_set.h
s_gsmksc.o: /usr/include/sys/_types/_fd_clr.h
s_gsmksc.o: /usr/include/sys/_types/_fd_zero.h
s_gsmksc.o: /usr/include/sys/_types/_fd_isset.h
s_gsmksc.o: /usr/include/sys/_types/_fd_copy.h
s_gsmksc.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gsmksc.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gsmksc.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gsmksc.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gsmksc.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gsmksc.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gsmksc.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gsmksc.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gsmksc.o: /usr/include/sys/_pthread/_pthread_t.h
s_gsmksc.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gsmksc.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gsmksc.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gsmksc.o: ../../../.././include/ncarg/c.h
s_gspa.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gspa.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gspa.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gspa.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gspa.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gspa.o: /usr/include/i386/_types.h
s_gspa.o: /usr/include/sys/_pthread/_pthread_types.h
s_gspa.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gspa.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gspa.o: /usr/include/sys/_types/_int16_t.h
s_gspa.o: /usr/include/sys/_types/_int32_t.h
s_gspa.o: /usr/include/sys/_types/_int64_t.h
s_gspa.o: /usr/include/sys/_types/_u_int8_t.h
s_gspa.o: /usr/include/sys/_types/_u_int16_t.h
s_gspa.o: /usr/include/sys/_types/_u_int32_t.h
s_gspa.o: /usr/include/sys/_types/_u_int64_t.h
s_gspa.o: /usr/include/sys/_types/_intptr_t.h
s_gspa.o: /usr/include/sys/_types/_uintptr_t.h
s_gspa.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gspa.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gspa.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gspa.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gspa.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gspa.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gspa.o: /usr/include/libkern/_OSByteOrder.h
s_gspa.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gspa.o: /usr/include/sys/_types/_u_char.h
s_gspa.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
s_gspa.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
s_gspa.o: /usr/include/sys/_types/_blkcnt_t.h
s_gspa.o: /usr/include/sys/_types/_blksize_t.h
s_gspa.o: /usr/include/sys/_types/_gid_t.h
s_gspa.o: /usr/include/sys/_types/_in_addr_t.h
s_gspa.o: /usr/include/sys/_types/_in_port_t.h
s_gspa.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
s_gspa.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gspa.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gspa.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gspa.o: /usr/include/sys/_types/_clock_t.h
s_gspa.o: /usr/include/sys/_types/_time_t.h
s_gspa.o: /usr/include/sys/_types/_useconds_t.h
s_gspa.o: /usr/include/sys/_types/_suseconds_t.h
s_gspa.o: /usr/include/sys/_types/_rsize_t.h
s_gspa.o: /usr/include/sys/_types/_errno_t.h
s_gspa.o: /usr/include/sys/_types/_fd_def.h
s_gspa.o: /usr/include/sys/_types/_fd_setsize.h
s_gspa.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
s_gspa.o: /usr/include/sys/_types/_fd_zero.h
s_gspa.o: /usr/include/sys/_types/_fd_isset.h
s_gspa.o: /usr/include/sys/_types/_fd_copy.h
s_gspa.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gspa.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gspa.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gspa.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gspa.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gspa.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gspa.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gspa.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gspa.o: /usr/include/sys/_pthread/_pthread_t.h
s_gspa.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gspa.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gspa.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gspa.o: ../../../.././include/ncarg/c.h
s_gsparf.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gsparf.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gsparf.o: /usr/include/sys/_posix_availability.h
s_gsparf.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gsparf.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gsparf.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gsparf.o: /usr/include/sys/_pthread/_pthread_types.h
s_gsparf.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gsparf.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gsparf.o: /usr/include/sys/_types/_int16_t.h
s_gsparf.o: /usr/include/sys/_types/_int32_t.h
s_gsparf.o: /usr/include/sys/_types/_int64_t.h
s_gsparf.o: /usr/include/sys/_types/_u_int8_t.h
s_gsparf.o: /usr/include/sys/_types/_u_int16_t.h
s_gsparf.o: /usr/include/sys/_types/_u_int32_t.h
s_gsparf.o: /usr/include/sys/_types/_u_int64_t.h
s_gsparf.o: /usr/include/sys/_types/_intptr_t.h
s_gsparf.o: /usr/include/sys/_types/_uintptr_t.h
s_gsparf.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gsparf.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gsparf.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gsparf.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gsparf.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gsparf.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gsparf.o: /usr/include/libkern/_OSByteOrder.h
s_gsparf.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gsparf.o: /usr/include/sys/_types/_u_char.h
s_gsparf.o: /usr/include/sys/_types/_u_short.h
s_gsparf.o: /usr/include/sys/_types/_u_int.h
s_gsparf.o: /usr/include/sys/_types/_caddr_t.h
s_gsparf.o: /usr/include/sys/_types/_dev_t.h
s_gsparf.o: /usr/include/sys/_types/_blkcnt_t.h
s_gsparf.o: /usr/include/sys/_types/_blksize_t.h
s_gsparf.o: /usr/include/sys/_types/_gid_t.h
s_gsparf.o: /usr/include/sys/_types/_in_addr_t.h
s_gsparf.o: /usr/include/sys/_types/_in_port_t.h
s_gsparf.o: /usr/include/sys/_types/_ino_t.h
s_gsparf.o: /usr/include/sys/_types/_ino64_t.h
s_gsparf.o: /usr/include/sys/_types/_key_t.h
s_gsparf.o: /usr/include/sys/_types/_mode_t.h
s_gsparf.o: /usr/include/sys/_types/_nlink_t.h
s_gsparf.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gsparf.o: /usr/include/sys/_types/_uid_t.h
s_gsparf.o: /usr/include/sys/_types/_clock_t.h
s_gsparf.o: /usr/include/sys/_types/_time_t.h
s_gsparf.o: /usr/include/sys/_types/_useconds_t.h
s_gsparf.o: /usr/include/sys/_types/_suseconds_t.h
s_gsparf.o: /usr/include/sys/_types/_rsize_t.h
s_gsparf.o: /usr/include/sys/_types/_errno_t.h
s_gsparf.o: /usr/include/sys/_types/_fd_def.h
s_gsparf.o: /usr/include/sys/_types/_fd_setsize.h
s_gsparf.o: /usr/include/sys/_types/_fd_set.h
s_gsparf.o: /usr/include/sys/_types/_fd_clr.h
s_gsparf.o: /usr/include/sys/_types/_fd_zero.h
s_gsparf.o: /usr/include/sys/_types/_fd_isset.h
s_gsparf.o: /usr/include/sys/_types/_fd_copy.h
s_gsparf.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gsparf.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gsparf.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gsparf.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gsparf.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gsparf.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gsparf.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gsparf.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gsparf.o: /usr/include/sys/_pthread/_pthread_t.h
s_gsparf.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gsparf.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gsparf.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gsparf.o: ../../../.././include/ncarg/c.h
s_gsplci.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gsplci.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gsplci.o: /usr/include/sys/_posix_availability.h
s_gsplci.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gsplci.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gsplci.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gsplci.o: /usr/include/sys/_pthread/_pthread_types.h
s_gsplci.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gsplci.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gsplci.o: /usr/include/sys/_types/_int16_t.h
s_gsplci.o: /usr/include/sys/_types/_int32_t.h
s_gsplci.o: /usr/include/sys/_types/_int64_t.h
s_gsplci.o: /usr/include/sys/_types/_u_int8_t.h
s_gsplci.o: /usr/include/sys/_types/_u_int16_t.h
s_gsplci.o: /usr/include/sys/_types/_u_int32_t.h
s_gsplci.o: /usr/include/sys/_types/_u_int64_t.h
s_gsplci.o: /usr/include/sys/_types/_intptr_t.h
s_gsplci.o: /usr/include/sys/_types/_uintptr_t.h
s_gsplci.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gsplci.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gsplci.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gsplci.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gsplci.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gsplci.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gsplci.o: /usr/include/libkern/_OSByteOrder.h
s_gsplci.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gsplci.o: /usr/include/sys/_types/_u_char.h
s_gsplci.o: /usr/include/sys/_types/_u_short.h
s_gsplci.o: /usr/include/sys/_types/_u_int.h
s_gsplci.o: /usr/include/sys/_types/_caddr_t.h
s_gsplci.o: /usr/include/sys/_types/_dev_t.h
s_gsplci.o: /usr/include/sys/_types/_blkcnt_t.h
s_gsplci.o: /usr/include/sys/_types/_blksize_t.h
s_gsplci.o: /usr/include/sys/_types/_gid_t.h
s_gsplci.o: /usr/include/sys/_types/_in_addr_t.h
s_gsplci.o: /usr/include/sys/_types/_in_port_t.h
s_gsplci.o: /usr/include/sys/_types/_ino_t.h
s_gsplci.o: /usr/include/sys/_types/_ino64_t.h
s_gsplci.o: /usr/include/sys/_types/_key_t.h
s_gsplci.o: /usr/include/sys/_types/_mode_t.h
s_gsplci.o: /usr/include/sys/_types/_nlink_t.h
s_gsplci.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gsplci.o: /usr/include/sys/_types/_uid_t.h
s_gsplci.o: /usr/include/sys/_types/_clock_t.h
s_gsplci.o: /usr/include/sys/_types/_time_t.h
s_gsplci.o: /usr/include/sys/_types/_useconds_t.h
s_gsplci.o: /usr/include/sys/_types/_suseconds_t.h
s_gsplci.o: /usr/include/sys/_types/_rsize_t.h
s_gsplci.o: /usr/include/sys/_types/_errno_t.h
s_gsplci.o: /usr/include/sys/_types/_fd_def.h
s_gsplci.o: /usr/include/sys/_types/_fd_setsize.h
s_gsplci.o: /usr/include/sys/_types/_fd_set.h
s_gsplci.o: /usr/include/sys/_types/_fd_clr.h
s_gsplci.o: /usr/include/sys/_types/_fd_zero.h
s_gsplci.o: /usr/include/sys/_types/_fd_isset.h
s_gsplci.o: /usr/include/sys/_types/_fd_copy.h
s_gsplci.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gsplci.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gsplci.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gsplci.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gsplci.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gsplci.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gsplci.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gsplci.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gsplci.o: /usr/include/sys/_pthread/_pthread_t.h
s_gsplci.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gsplci.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gsplci.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gsplci.o: ../../../.././include/ncarg/c.h
s_gspli.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gspli.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gspli.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gspli.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gspli.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gspli.o: /usr/include/i386/_types.h
s_gspli.o: /usr/include/sys/_pthread/_pthread_types.h
s_gspli.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gspli.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gspli.o: /usr/include/sys/_types/_int16_t.h
s_gspli.o: /usr/include/sys/_types/_int32_t.h
s_gspli.o: /usr/include/sys/_types/_int64_t.h
s_gspli.o: /usr/include/sys/_types/_u_int8_t.h
s_gspli.o: /usr/include/sys/_types/_u_int16_t.h
s_gspli.o: /usr/include/sys/_types/_u_int32_t.h
s_gspli.o: /usr/include/sys/_types/_u_int64_t.h
s_gspli.o: /usr/include/sys/_types/_intptr_t.h
s_gspli.o: /usr/include/sys/_types/_uintptr_t.h
s_gspli.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gspli.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gspli.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gspli.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gspli.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gspli.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gspli.o: /usr/include/libkern/_OSByteOrder.h
s_gspli.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gspli.o: /usr/include/sys/_types/_u_char.h
s_gspli.o: /usr/include/sys/_types/_u_short.h
s_gspli.o: /usr/include/sys/_types/_u_int.h
s_gspli.o: /usr/include/sys/_types/_caddr_t.h
s_gspli.o: /usr/include/sys/_types/_dev_t.h
s_gspli.o: /usr/include/sys/_types/_blkcnt_t.h
s_gspli.o: /usr/include/sys/_types/_blksize_t.h
s_gspli.o: /usr/include/sys/_types/_gid_t.h
s_gspli.o: /usr/include/sys/_types/_in_addr_t.h
s_gspli.o: /usr/include/sys/_types/_in_port_t.h
s_gspli.o: /usr/include/sys/_types/_ino_t.h
s_gspli.o: /usr/include/sys/_types/_ino64_t.h
s_gspli.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gspli.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gspli.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gspli.o: /usr/include/sys/_types/_clock_t.h
s_gspli.o: /usr/include/sys/_types/_time_t.h
s_gspli.o: /usr/include/sys/_types/_useconds_t.h
s_gspli.o: /usr/include/sys/_types/_suseconds_t.h
s_gspli.o: /usr/include/sys/_types/_rsize_t.h
s_gspli.o: /usr/include/sys/_types/_errno_t.h
s_gspli.o: /usr/include/sys/_types/_fd_def.h
s_gspli.o: /usr/include/sys/_types/_fd_setsize.h
s_gspli.o: /usr/include/sys/_types/_fd_set.h
s_gspli.o: /usr/include/sys/_types/_fd_clr.h
s_gspli.o: /usr/include/sys/_types/_fd_zero.h
s_gspli.o: /usr/include/sys/_types/_fd_isset.h
s_gspli.o: /usr/include/sys/_types/_fd_copy.h
s_gspli.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gspli.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gspli.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gspli.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gspli.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gspli.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gspli.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gspli.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gspli.o: /usr/include/sys/_pthread/_pthread_t.h
s_gspli.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gspli.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gspli.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gspli.o: ../../../.././include/ncarg/c.h
s_gspmci.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gspmci.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gspmci.o: /usr/include/sys/_posix_availability.h
s_gspmci.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gspmci.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gspmci.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gspmci.o: /usr/include/sys/_pthread/_pthread_types.h
s_gspmci.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gspmci.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gspmci.o: /usr/include/sys/_types/_int16_t.h
s_gspmci.o: /usr/include/sys/_types/_int32_t.h
s_gspmci.o: /usr/include/sys/_types/_int64_t.h
s_gspmci.o: /usr/include/sys/_types/_u_int8_t.h
s_gspmci.o: /usr/include/sys/_types/_u_int16_t.h
s_gspmci.o: /usr/include/sys/_types/_u_int32_t.h
s_gspmci.o: /usr/include/sys/_types/_u_int64_t.h
s_gspmci.o: /usr/include/sys/_types/_intptr_t.h
s_gspmci.o: /usr/include/sys/_types/_uintptr_t.h
s_gspmci.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gspmci.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gspmci.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gspmci.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gspmci.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gspmci.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gspmci.o: /usr/include/libkern/_OSByteOrder.h
s_gspmci.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gspmci.o: /usr/include/sys/_types/_u_char.h
s_gspmci.o: /usr/include/sys/_types/_u_short.h
s_gspmci.o: /usr/include/sys/_types/_u_int.h
s_gspmci.o: /usr/include/sys/_types/_caddr_t.h
s_gspmci.o: /usr/include/sys/_types/_dev_t.h
s_gspmci.o: /usr/include/sys/_types/_blkcnt_t.h
s_gspmci.o: /usr/include/sys/_types/_blksize_t.h
s_gspmci.o: /usr/include/sys/_types/_gid_t.h
s_gspmci.o: /usr/include/sys/_types/_in_addr_t.h
s_gspmci.o: /usr/include/sys/_types/_in_port_t.h
s_gspmci.o: /usr/include/sys/_types/_ino_t.h
s_gspmci.o: /usr/include/sys/_types/_ino64_t.h
s_gspmci.o: /usr/include/sys/_types/_key_t.h
s_gspmci.o: /usr/include/sys/_types/_mode_t.h
s_gspmci.o: /usr/include/sys/_types/_nlink_t.h
s_gspmci.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gspmci.o: /usr/include/sys/_types/_uid_t.h
s_gspmci.o: /usr/include/sys/_types/_clock_t.h
s_gspmci.o: /usr/include/sys/_types/_time_t.h
s_gspmci.o: /usr/include/sys/_types/_useconds_t.h
s_gspmci.o: /usr/include/sys/_types/_suseconds_t.h
s_gspmci.o: /usr/include/sys/_types/_rsize_t.h
s_gspmci.o: /usr/include/sys/_types/_errno_t.h
s_gspmci.o: /usr/include/sys/_types/_fd_def.h
s_gspmci.o: /usr/include/sys/_types/_fd_setsize.h
s_gspmci.o: /usr/include/sys/_types/_fd_set.h
s_gspmci.o: /usr/include/sys/_types/_fd_clr.h
s_gspmci.o: /usr/include/sys/_types/_fd_zero.h
s_gspmci.o: /usr/include/sys/_types/_fd_isset.h
s_gspmci.o: /usr/include/sys/_types/_fd_copy.h
s_gspmci.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gspmci.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gspmci.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gspmci.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gspmci.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gspmci.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gspmci.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gspmci.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gspmci.o: /usr/include/sys/_pthread/_pthread_t.h
s_gspmci.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gspmci.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gspmci.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gspmci.o: ../../../.././include/ncarg/c.h
s_gspmi.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gspmi.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gspmi.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gspmi.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gspmi.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gspmi.o: /usr/include/i386/_types.h
s_gspmi.o: /usr/include/sys/_pthread/_pthread_types.h
s_gspmi.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gspmi.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gspmi.o: /usr/include/sys/_types/_int16_t.h
s_gspmi.o: /usr/include/sys/_types/_int32_t.h
s_gspmi.o: /usr/include/sys/_types/_int64_t.h
s_gspmi.o: /usr/include/sys/_types/_u_int8_t.h
s_gspmi.o: /usr/include/sys/_types/_u_int16_t.h
s_gspmi.o: /usr/include/sys/_types/_u_int32_t.h
s_gspmi.o: /usr/include/sys/_types/_u_int64_t.h
s_gspmi.o: /usr/include/sys/_types/_intptr_t.h
s_gspmi.o: /usr/include/sys/_types/_uintptr_t.h
s_gspmi.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gspmi.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gspmi.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gspmi.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gspmi.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gspmi.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gspmi.o: /usr/include/libkern/_OSByteOrder.h
s_gspmi.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gspmi.o: /usr/include/sys/_types/_u_char.h
s_gspmi.o: /usr/include/sys/_types/_u_short.h
s_gspmi.o: /usr/include/sys/_types/_u_int.h
s_gspmi.o: /usr/include/sys/_types/_caddr_t.h
s_gspmi.o: /usr/include/sys/_types/_dev_t.h
s_gspmi.o: /usr/include/sys/_types/_blkcnt_t.h
s_gspmi.o: /usr/include/sys/_types/_blksize_t.h
s_gspmi.o: /usr/include/sys/_types/_gid_t.h
s_gspmi.o: /usr/include/sys/_types/_in_addr_t.h
s_gspmi.o: /usr/include/sys/_types/_in_port_t.h
s_gspmi.o: /usr/include/sys/_types/_ino_t.h
s_gspmi.o: /usr/include/sys/_types/_ino64_t.h
s_gspmi.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gspmi.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gspmi.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gspmi.o: /usr/include/sys/_types/_clock_t.h
s_gspmi.o: /usr/include/sys/_types/_time_t.h
s_gspmi.o: /usr/include/sys/_types/_useconds_t.h
s_gspmi.o: /usr/include/sys/_types/_suseconds_t.h
s_gspmi.o: /usr/include/sys/_types/_rsize_t.h
s_gspmi.o: /usr/include/sys/_types/_errno_t.h
s_gspmi.o: /usr/include/sys/_types/_fd_def.h
s_gspmi.o: /usr/include/sys/_types/_fd_setsize.h
s_gspmi.o: /usr/include/sys/_types/_fd_set.h
s_gspmi.o: /usr/include/sys/_types/_fd_clr.h
s_gspmi.o: /usr/include/sys/_types/_fd_zero.h
s_gspmi.o: /usr/include/sys/_types/_fd_isset.h
s_gspmi.o: /usr/include/sys/_types/_fd_copy.h
s_gspmi.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gspmi.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gspmi.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gspmi.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gspmi.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gspmi.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gspmi.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gspmi.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gspmi.o: /usr/include/sys/_pthread/_pthread_t.h
s_gspmi.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gspmi.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gspmi.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gspmi.o: ../../../.././include/ncarg/c.h
s_gstxal.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gstxal.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gstxal.o: /usr/include/sys/_posix_availability.h
s_gstxal.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gstxal.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gstxal.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gstxal.o: /usr/include/sys/_pthread/_pthread_types.h
s_gstxal.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gstxal.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gstxal.o: /usr/include/sys/_types/_int16_t.h
s_gstxal.o: /usr/include/sys/_types/_int32_t.h
s_gstxal.o: /usr/include/sys/_types/_int64_t.h
s_gstxal.o: /usr/include/sys/_types/_u_int8_t.h
s_gstxal.o: /usr/include/sys/_types/_u_int16_t.h
s_gstxal.o: /usr/include/sys/_types/_u_int32_t.h
s_gstxal.o: /usr/include/sys/_types/_u_int64_t.h
s_gstxal.o: /usr/include/sys/_types/_intptr_t.h
s_gstxal.o: /usr/include/sys/_types/_uintptr_t.h
s_gstxal.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gstxal.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gstxal.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gstxal.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gstxal.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gstxal.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gstxal.o: /usr/include/libkern/_OSByteOrder.h
s_gstxal.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gstxal.o: /usr/include/sys/_types/_u_char.h
s_gstxal.o: /usr/include/sys/_types/_u_short.h
s_gstxal.o: /usr/include/sys/_types/_u_int.h
s_gstxal.o: /usr/include/sys/_types/_caddr_t.h
s_gstxal.o: /usr/include/sys/_types/_dev_t.h
s_gstxal.o: /usr/include/sys/_types/_blkcnt_t.h
s_gstxal.o: /usr/include/sys/_types/_blksize_t.h
s_gstxal.o: /usr/include/sys/_types/_gid_t.h
s_gstxal.o: /usr/include/sys/_types/_in_addr_t.h
s_gstxal.o: /usr/include/sys/_types/_in_port_t.h
s_gstxal.o: /usr/include/sys/_types/_ino_t.h
s_gstxal.o: /usr/include/sys/_types/_ino64_t.h
s_gstxal.o: /usr/include/sys/_types/_key_t.h
s_gstxal.o: /usr/include/sys/_types/_mode_t.h
s_gstxal.o: /usr/include/sys/_types/_nlink_t.h
s_gstxal.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gstxal.o: /usr/include/sys/_types/_uid_t.h
s_gstxal.o: /usr/include/sys/_types/_clock_t.h
s_gstxal.o: /usr/include/sys/_types/_time_t.h
s_gstxal.o: /usr/include/sys/_types/_useconds_t.h
s_gstxal.o: /usr/include/sys/_types/_suseconds_t.h
s_gstxal.o: /usr/include/sys/_types/_rsize_t.h
s_gstxal.o: /usr/include/sys/_types/_errno_t.h
s_gstxal.o: /usr/include/sys/_types/_fd_def.h
s_gstxal.o: /usr/include/sys/_types/_fd_setsize.h
s_gstxal.o: /usr/include/sys/_types/_fd_set.h
s_gstxal.o: /usr/include/sys/_types/_fd_clr.h
s_gstxal.o: /usr/include/sys/_types/_fd_zero.h
s_gstxal.o: /usr/include/sys/_types/_fd_isset.h
s_gstxal.o: /usr/include/sys/_types/_fd_copy.h
s_gstxal.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gstxal.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gstxal.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gstxal.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gstxal.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gstxal.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gstxal.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gstxal.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gstxal.o: /usr/include/sys/_pthread/_pthread_t.h
s_gstxal.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gstxal.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gstxal.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gstxal.o: ../../../.././include/ncarg/c.h
s_gstxci.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gstxci.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gstxci.o: /usr/include/sys/_posix_availability.h
s_gstxci.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gstxci.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gstxci.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gstxci.o: /usr/include/sys/_pthread/_pthread_types.h
s_gstxci.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gstxci.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gstxci.o: /usr/include/sys/_types/_int16_t.h
s_gstxci.o: /usr/include/sys/_types/_int32_t.h
s_gstxci.o: /usr/include/sys/_types/_int64_t.h
s_gstxci.o: /usr/include/sys/_types/_u_int8_t.h
s_gstxci.o: /usr/include/sys/_types/_u_int16_t.h
s_gstxci.o: /usr/include/sys/_types/_u_int32_t.h
s_gstxci.o: /usr/include/sys/_types/_u_int64_t.h
s_gstxci.o: /usr/include/sys/_types/_intptr_t.h
s_gstxci.o: /usr/include/sys/_types/_uintptr_t.h
s_gstxci.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gstxci.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gstxci.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gstxci.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gstxci.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gstxci.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gstxci.o: /usr/include/libkern/_OSByteOrder.h
s_gstxci.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gstxci.o: /usr/include/sys/_types/_u_char.h
s_gstxci.o: /usr/include/sys/_types/_u_short.h
s_gstxci.o: /usr/include/sys/_types/_u_int.h
s_gstxci.o: /usr/include/sys/_types/_caddr_t.h
s_gstxci.o: /usr/include/sys/_types/_dev_t.h
s_gstxci.o: /usr/include/sys/_types/_blkcnt_t.h
s_gstxci.o: /usr/include/sys/_types/_blksize_t.h
s_gstxci.o: /usr/include/sys/_types/_gid_t.h
s_gstxci.o: /usr/include/sys/_types/_in_addr_t.h
s_gstxci.o: /usr/include/sys/_types/_in_port_t.h
s_gstxci.o: /usr/include/sys/_types/_ino_t.h
s_gstxci.o: /usr/include/sys/_types/_ino64_t.h
s_gstxci.o: /usr/include/sys/_types/_key_t.h
s_gstxci.o: /usr/include/sys/_types/_mode_t.h
s_gstxci.o: /usr/include/sys/_types/_nlink_t.h
s_gstxci.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gstxci.o: /usr/include/sys/_types/_uid_t.h
s_gstxci.o: /usr/include/sys/_types/_clock_t.h
s_gstxci.o: /usr/include/sys/_types/_time_t.h
s_gstxci.o: /usr/include/sys/_types/_useconds_t.h
s_gstxci.o: /usr/include/sys/_types/_suseconds_t.h
s_gstxci.o: /usr/include/sys/_types/_rsize_t.h
s_gstxci.o: /usr/include/sys/_types/_errno_t.h
s_gstxci.o: /usr/include/sys/_types/_fd_def.h
s_gstxci.o: /usr/include/sys/_types/_fd_setsize.h
s_gstxci.o: /usr/include/sys/_types/_fd_set.h
s_gstxci.o: /usr/include/sys/_types/_fd_clr.h
s_gstxci.o: /usr/include/sys/_types/_fd_zero.h
s_gstxci.o: /usr/include/sys/_types/_fd_isset.h
s_gstxci.o: /usr/include/sys/_types/_fd_copy.h
s_gstxci.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gstxci.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gstxci.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gstxci.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gstxci.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gstxci.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gstxci.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gstxci.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gstxci.o: /usr/include/sys/_pthread/_pthread_t.h
s_gstxci.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gstxci.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gstxci.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gstxci.o: ../../../.././include/ncarg/c.h
s_gstxfp.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gstxfp.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gstxfp.o: /usr/include/sys/_posix_availability.h
s_gstxfp.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gstxfp.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gstxfp.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gstxfp.o: /usr/include/sys/_pthread/_pthread_types.h
s_gstxfp.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gstxfp.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gstxfp.o: /usr/include/sys/_types/_int16_t.h
s_gstxfp.o: /usr/include/sys/_types/_int32_t.h
s_gstxfp.o: /usr/include/sys/_types/_int64_t.h
s_gstxfp.o: /usr/include/sys/_types/_u_int8_t.h
s_gstxfp.o: /usr/include/sys/_types/_u_int16_t.h
s_gstxfp.o: /usr/include/sys/_types/_u_int32_t.h
s_gstxfp.o: /usr/include/sys/_types/_u_int64_t.h
s_gstxfp.o: /usr/include/sys/_types/_intptr_t.h
s_gstxfp.o: /usr/include/sys/_types/_uintptr_t.h
s_gstxfp.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gstxfp.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gstxfp.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gstxfp.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gstxfp.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gstxfp.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gstxfp.o: /usr/include/libkern/_OSByteOrder.h
s_gstxfp.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gstxfp.o: /usr/include/sys/_types/_u_char.h
s_gstxfp.o: /usr/include/sys/_types/_u_short.h
s_gstxfp.o: /usr/include/sys/_types/_u_int.h
s_gstxfp.o: /usr/include/sys/_types/_caddr_t.h
s_gstxfp.o: /usr/include/sys/_types/_dev_t.h
s_gstxfp.o: /usr/include/sys/_types/_blkcnt_t.h
s_gstxfp.o: /usr/include/sys/_types/_blksize_t.h
s_gstxfp.o: /usr/include/sys/_types/_gid_t.h
s_gstxfp.o: /usr/include/sys/_types/_in_addr_t.h
s_gstxfp.o: /usr/include/sys/_types/_in_port_t.h
s_gstxfp.o: /usr/include/sys/_types/_ino_t.h
s_gstxfp.o: /usr/include/sys/_types/_ino64_t.h
s_gstxfp.o: /usr/include/sys/_types/_key_t.h
s_gstxfp.o: /usr/include/sys/_types/_mode_t.h
s_gstxfp.o: /usr/include/sys/_types/_nlink_t.h
s_gstxfp.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gstxfp.o: /usr/include/sys/_types/_uid_t.h
s_gstxfp.o: /usr/include/sys/_types/_clock_t.h
s_gstxfp.o: /usr/include/sys/_types/_time_t.h
s_gstxfp.o: /usr/include/sys/_types/_useconds_t.h
s_gstxfp.o: /usr/include/sys/_types/_suseconds_t.h
s_gstxfp.o: /usr/include/sys/_types/_rsize_t.h
s_gstxfp.o: /usr/include/sys/_types/_errno_t.h
s_gstxfp.o: /usr/include/sys/_types/_fd_def.h
s_gstxfp.o: /usr/include/sys/_types/_fd_setsize.h
s_gstxfp.o: /usr/include/sys/_types/_fd_set.h
s_gstxfp.o: /usr/include/sys/_types/_fd_clr.h
s_gstxfp.o: /usr/include/sys/_types/_fd_zero.h
s_gstxfp.o: /usr/include/sys/_types/_fd_isset.h
s_gstxfp.o: /usr/include/sys/_types/_fd_copy.h
s_gstxfp.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gstxfp.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gstxfp.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gstxfp.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gstxfp.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gstxfp.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gstxfp.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gstxfp.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gstxfp.o: /usr/include/sys/_pthread/_pthread_t.h
s_gstxfp.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gstxfp.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gstxfp.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gstxfp.o: ../../../.././include/ncarg/c.h
s_gstxi.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gstxi.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gstxi.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gstxi.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gstxi.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gstxi.o: /usr/include/i386/_types.h
s_gstxi.o: /usr/include/sys/_pthread/_pthread_types.h
s_gstxi.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gstxi.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gstxi.o: /usr/include/sys/_types/_int16_t.h
s_gstxi.o: /usr/include/sys/_types/_int32_t.h
s_gstxi.o: /usr/include/sys/_types/_int64_t.h
s_gstxi.o: /usr/include/sys/_types/_u_int8_t.h
s_gstxi.o: /usr/include/sys/_types/_u_int16_t.h
s_gstxi.o: /usr/include/sys/_types/_u_int32_t.h
s_gstxi.o: /usr/include/sys/_types/_u_int64_t.h
s_gstxi.o: /usr/include/sys/_types/_intptr_t.h
s_gstxi.o: /usr/include/sys/_types/_uintptr_t.h
s_gstxi.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gstxi.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gstxi.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gstxi.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gstxi.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gstxi.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gstxi.o: /usr/include/libkern/_OSByteOrder.h
s_gstxi.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gstxi.o: /usr/include/sys/_types/_u_char.h
s_gstxi.o: /usr/include/sys/_types/_u_short.h
s_gstxi.o: /usr/include/sys/_types/_u_int.h
s_gstxi.o: /usr/include/sys/_types/_caddr_t.h
s_gstxi.o: /usr/include/sys/_types/_dev_t.h
s_gstxi.o: /usr/include/sys/_types/_blkcnt_t.h
s_gstxi.o: /usr/include/sys/_types/_blksize_t.h
s_gstxi.o: /usr/include/sys/_types/_gid_t.h
s_gstxi.o: /usr/include/sys/_types/_in_addr_t.h
s_gstxi.o: /usr/include/sys/_types/_in_port_t.h
s_gstxi.o: /usr/include/sys/_types/_ino_t.h
s_gstxi.o: /usr/include/sys/_types/_ino64_t.h
s_gstxi.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gstxi.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gstxi.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gstxi.o: /usr/include/sys/_types/_clock_t.h
s_gstxi.o: /usr/include/sys/_types/_time_t.h
s_gstxi.o: /usr/include/sys/_types/_useconds_t.h
s_gstxi.o: /usr/include/sys/_types/_suseconds_t.h
s_gstxi.o: /usr/include/sys/_types/_rsize_t.h
s_gstxi.o: /usr/include/sys/_types/_errno_t.h
s_gstxi.o: /usr/include/sys/_types/_fd_def.h
s_gstxi.o: /usr/include/sys/_types/_fd_setsize.h
s_gstxi.o: /usr/include/sys/_types/_fd_set.h
s_gstxi.o: /usr/include/sys/_types/_fd_clr.h
s_gstxi.o: /usr/include/sys/_types/_fd_zero.h
s_gstxi.o: /usr/include/sys/_types/_fd_isset.h
s_gstxi.o: /usr/include/sys/_types/_fd_copy.h
s_gstxi.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gstxi.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gstxi.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gstxi.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gstxi.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gstxi.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gstxi.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gstxi.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gstxi.o: /usr/include/sys/_pthread/_pthread_t.h
s_gstxi.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gstxi.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gstxi.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gstxi.o: ../../../.././include/ncarg/c.h
s_gstxp.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gstxp.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gstxp.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gstxp.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gstxp.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gstxp.o: /usr/include/i386/_types.h
s_gstxp.o: /usr/include/sys/_pthread/_pthread_types.h
s_gstxp.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gstxp.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gstxp.o: /usr/include/sys/_types/_int16_t.h
s_gstxp.o: /usr/include/sys/_types/_int32_t.h
s_gstxp.o: /usr/include/sys/_types/_int64_t.h
s_gstxp.o: /usr/include/sys/_types/_u_int8_t.h
s_gstxp.o: /usr/include/sys/_types/_u_int16_t.h
s_gstxp.o: /usr/include/sys/_types/_u_int32_t.h
s_gstxp.o: /usr/include/sys/_types/_u_int64_t.h
s_gstxp.o: /usr/include/sys/_types/_intptr_t.h
s_gstxp.o: /usr/include/sys/_types/_uintptr_t.h
s_gstxp.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gstxp.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gstxp.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gstxp.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gstxp.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gstxp.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gstxp.o: /usr/include/libkern/_OSByteOrder.h
s_gstxp.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gstxp.o: /usr/include/sys/_types/_u_char.h
s_gstxp.o: /usr/include/sys/_types/_u_short.h
s_gstxp.o: /usr/include/sys/_types/_u_int.h
s_gstxp.o: /usr/include/sys/_types/_caddr_t.h
s_gstxp.o: /usr/include/sys/_types/_dev_t.h
s_gstxp.o: /usr/include/sys/_types/_blkcnt_t.h
s_gstxp.o: /usr/include/sys/_types/_blksize_t.h
s_gstxp.o: /usr/include/sys/_types/_gid_t.h
s_gstxp.o: /usr/include/sys/_types/_in_addr_t.h
s_gstxp.o: /usr/include/sys/_types/_in_port_t.h
s_gstxp.o: /usr/include/sys/_types/_ino_t.h
s_gstxp.o: /usr/include/sys/_types/_ino64_t.h
s_gstxp.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gstxp.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gstxp.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gstxp.o: /usr/include/sys/_types/_clock_t.h
s_gstxp.o: /usr/include/sys/_types/_time_t.h
s_gstxp.o: /usr/include/sys/_types/_useconds_t.h
s_gstxp.o: /usr/include/sys/_types/_suseconds_t.h
s_gstxp.o: /usr/include/sys/_types/_rsize_t.h
s_gstxp.o: /usr/include/sys/_types/_errno_t.h
s_gstxp.o: /usr/include/sys/_types/_fd_def.h
s_gstxp.o: /usr/include/sys/_types/_fd_setsize.h
s_gstxp.o: /usr/include/sys/_types/_fd_set.h
s_gstxp.o: /usr/include/sys/_types/_fd_clr.h
s_gstxp.o: /usr/include/sys/_types/_fd_zero.h
s_gstxp.o: /usr/include/sys/_types/_fd_isset.h
s_gstxp.o: /usr/include/sys/_types/_fd_copy.h
s_gstxp.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gstxp.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gstxp.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gstxp.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gstxp.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gstxp.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gstxp.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gstxp.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gstxp.o: /usr/include/sys/_pthread/_pthread_t.h
s_gstxp.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gstxp.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gstxp.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gstxp.o: ../../../.././include/ncarg/c.h
s_gsvp.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gsvp.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gsvp.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gsvp.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gsvp.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gsvp.o: /usr/include/i386/_types.h
s_gsvp.o: /usr/include/sys/_pthread/_pthread_types.h
s_gsvp.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gsvp.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gsvp.o: /usr/include/sys/_types/_int16_t.h
s_gsvp.o: /usr/include/sys/_types/_int32_t.h
s_gsvp.o: /usr/include/sys/_types/_int64_t.h
s_gsvp.o: /usr/include/sys/_types/_u_int8_t.h
s_gsvp.o: /usr/include/sys/_types/_u_int16_t.h
s_gsvp.o: /usr/include/sys/_types/_u_int32_t.h
s_gsvp.o: /usr/include/sys/_types/_u_int64_t.h
s_gsvp.o: /usr/include/sys/_types/_intptr_t.h
s_gsvp.o: /usr/include/sys/_types/_uintptr_t.h
s_gsvp.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gsvp.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gsvp.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gsvp.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gsvp.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gsvp.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gsvp.o: /usr/include/libkern/_OSByteOrder.h
s_gsvp.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gsvp.o: /usr/include/sys/_types/_u_char.h
s_gsvp.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
s_gsvp.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
s_gsvp.o: /usr/include/sys/_types/_blkcnt_t.h
s_gsvp.o: /usr/include/sys/_types/_blksize_t.h
s_gsvp.o: /usr/include/sys/_types/_gid_t.h
s_gsvp.o: /usr/include/sys/_types/_in_addr_t.h
s_gsvp.o: /usr/include/sys/_types/_in_port_t.h
s_gsvp.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
s_gsvp.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gsvp.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gsvp.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gsvp.o: /usr/include/sys/_types/_clock_t.h
s_gsvp.o: /usr/include/sys/_types/_time_t.h
s_gsvp.o: /usr/include/sys/_types/_useconds_t.h
s_gsvp.o: /usr/include/sys/_types/_suseconds_t.h
s_gsvp.o: /usr/include/sys/_types/_rsize_t.h
s_gsvp.o: /usr/include/sys/_types/_errno_t.h
s_gsvp.o: /usr/include/sys/_types/_fd_def.h
s_gsvp.o: /usr/include/sys/_types/_fd_setsize.h
s_gsvp.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
s_gsvp.o: /usr/include/sys/_types/_fd_zero.h
s_gsvp.o: /usr/include/sys/_types/_fd_isset.h
s_gsvp.o: /usr/include/sys/_types/_fd_copy.h
s_gsvp.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gsvp.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gsvp.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gsvp.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gsvp.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gsvp.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gsvp.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gsvp.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gsvp.o: /usr/include/sys/_pthread/_pthread_t.h
s_gsvp.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gsvp.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gsvp.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gsvp.o: ../../../.././include/ncarg/c.h
s_gswkvp.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gswkvp.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gswkvp.o: /usr/include/sys/_posix_availability.h
s_gswkvp.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gswkvp.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gswkvp.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gswkvp.o: /usr/include/sys/_pthread/_pthread_types.h
s_gswkvp.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gswkvp.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gswkvp.o: /usr/include/sys/_types/_int16_t.h
s_gswkvp.o: /usr/include/sys/_types/_int32_t.h
s_gswkvp.o: /usr/include/sys/_types/_int64_t.h
s_gswkvp.o: /usr/include/sys/_types/_u_int8_t.h
s_gswkvp.o: /usr/include/sys/_types/_u_int16_t.h
s_gswkvp.o: /usr/include/sys/_types/_u_int32_t.h
s_gswkvp.o: /usr/include/sys/_types/_u_int64_t.h
s_gswkvp.o: /usr/include/sys/_types/_intptr_t.h
s_gswkvp.o: /usr/include/sys/_types/_uintptr_t.h
s_gswkvp.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gswkvp.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gswkvp.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gswkvp.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gswkvp.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gswkvp.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gswkvp.o: /usr/include/libkern/_OSByteOrder.h
s_gswkvp.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gswkvp.o: /usr/include/sys/_types/_u_char.h
s_gswkvp.o: /usr/include/sys/_types/_u_short.h
s_gswkvp.o: /usr/include/sys/_types/_u_int.h
s_gswkvp.o: /usr/include/sys/_types/_caddr_t.h
s_gswkvp.o: /usr/include/sys/_types/_dev_t.h
s_gswkvp.o: /usr/include/sys/_types/_blkcnt_t.h
s_gswkvp.o: /usr/include/sys/_types/_blksize_t.h
s_gswkvp.o: /usr/include/sys/_types/_gid_t.h
s_gswkvp.o: /usr/include/sys/_types/_in_addr_t.h
s_gswkvp.o: /usr/include/sys/_types/_in_port_t.h
s_gswkvp.o: /usr/include/sys/_types/_ino_t.h
s_gswkvp.o: /usr/include/sys/_types/_ino64_t.h
s_gswkvp.o: /usr/include/sys/_types/_key_t.h
s_gswkvp.o: /usr/include/sys/_types/_mode_t.h
s_gswkvp.o: /usr/include/sys/_types/_nlink_t.h
s_gswkvp.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gswkvp.o: /usr/include/sys/_types/_uid_t.h
s_gswkvp.o: /usr/include/sys/_types/_clock_t.h
s_gswkvp.o: /usr/include/sys/_types/_time_t.h
s_gswkvp.o: /usr/include/sys/_types/_useconds_t.h
s_gswkvp.o: /usr/include/sys/_types/_suseconds_t.h
s_gswkvp.o: /usr/include/sys/_types/_rsize_t.h
s_gswkvp.o: /usr/include/sys/_types/_errno_t.h
s_gswkvp.o: /usr/include/sys/_types/_fd_def.h
s_gswkvp.o: /usr/include/sys/_types/_fd_setsize.h
s_gswkvp.o: /usr/include/sys/_types/_fd_set.h
s_gswkvp.o: /usr/include/sys/_types/_fd_clr.h
s_gswkvp.o: /usr/include/sys/_types/_fd_zero.h
s_gswkvp.o: /usr/include/sys/_types/_fd_isset.h
s_gswkvp.o: /usr/include/sys/_types/_fd_copy.h
s_gswkvp.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gswkvp.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gswkvp.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gswkvp.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gswkvp.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gswkvp.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gswkvp.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gswkvp.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gswkvp.o: /usr/include/sys/_pthread/_pthread_t.h
s_gswkvp.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gswkvp.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gswkvp.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gswkvp.o: ../../../.././include/ncarg/c.h
s_gswkwn.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gswkwn.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gswkwn.o: /usr/include/sys/_posix_availability.h
s_gswkwn.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gswkwn.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gswkwn.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gswkwn.o: /usr/include/sys/_pthread/_pthread_types.h
s_gswkwn.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gswkwn.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gswkwn.o: /usr/include/sys/_types/_int16_t.h
s_gswkwn.o: /usr/include/sys/_types/_int32_t.h
s_gswkwn.o: /usr/include/sys/_types/_int64_t.h
s_gswkwn.o: /usr/include/sys/_types/_u_int8_t.h
s_gswkwn.o: /usr/include/sys/_types/_u_int16_t.h
s_gswkwn.o: /usr/include/sys/_types/_u_int32_t.h
s_gswkwn.o: /usr/include/sys/_types/_u_int64_t.h
s_gswkwn.o: /usr/include/sys/_types/_intptr_t.h
s_gswkwn.o: /usr/include/sys/_types/_uintptr_t.h
s_gswkwn.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gswkwn.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gswkwn.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gswkwn.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gswkwn.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gswkwn.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gswkwn.o: /usr/include/libkern/_OSByteOrder.h
s_gswkwn.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gswkwn.o: /usr/include/sys/_types/_u_char.h
s_gswkwn.o: /usr/include/sys/_types/_u_short.h
s_gswkwn.o: /usr/include/sys/_types/_u_int.h
s_gswkwn.o: /usr/include/sys/_types/_caddr_t.h
s_gswkwn.o: /usr/include/sys/_types/_dev_t.h
s_gswkwn.o: /usr/include/sys/_types/_blkcnt_t.h
s_gswkwn.o: /usr/include/sys/_types/_blksize_t.h
s_gswkwn.o: /usr/include/sys/_types/_gid_t.h
s_gswkwn.o: /usr/include/sys/_types/_in_addr_t.h
s_gswkwn.o: /usr/include/sys/_types/_in_port_t.h
s_gswkwn.o: /usr/include/sys/_types/_ino_t.h
s_gswkwn.o: /usr/include/sys/_types/_ino64_t.h
s_gswkwn.o: /usr/include/sys/_types/_key_t.h
s_gswkwn.o: /usr/include/sys/_types/_mode_t.h
s_gswkwn.o: /usr/include/sys/_types/_nlink_t.h
s_gswkwn.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gswkwn.o: /usr/include/sys/_types/_uid_t.h
s_gswkwn.o: /usr/include/sys/_types/_clock_t.h
s_gswkwn.o: /usr/include/sys/_types/_time_t.h
s_gswkwn.o: /usr/include/sys/_types/_useconds_t.h
s_gswkwn.o: /usr/include/sys/_types/_suseconds_t.h
s_gswkwn.o: /usr/include/sys/_types/_rsize_t.h
s_gswkwn.o: /usr/include/sys/_types/_errno_t.h
s_gswkwn.o: /usr/include/sys/_types/_fd_def.h
s_gswkwn.o: /usr/include/sys/_types/_fd_setsize.h
s_gswkwn.o: /usr/include/sys/_types/_fd_set.h
s_gswkwn.o: /usr/include/sys/_types/_fd_clr.h
s_gswkwn.o: /usr/include/sys/_types/_fd_zero.h
s_gswkwn.o: /usr/include/sys/_types/_fd_isset.h
s_gswkwn.o: /usr/include/sys/_types/_fd_copy.h
s_gswkwn.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gswkwn.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gswkwn.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gswkwn.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gswkwn.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gswkwn.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gswkwn.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gswkwn.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gswkwn.o: /usr/include/sys/_pthread/_pthread_t.h
s_gswkwn.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gswkwn.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gswkwn.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gswkwn.o: ../../../.././include/ncarg/c.h
s_gswn.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gswn.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gswn.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gswn.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gswn.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gswn.o: /usr/include/i386/_types.h
s_gswn.o: /usr/include/sys/_pthread/_pthread_types.h
s_gswn.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gswn.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gswn.o: /usr/include/sys/_types/_int16_t.h
s_gswn.o: /usr/include/sys/_types/_int32_t.h
s_gswn.o: /usr/include/sys/_types/_int64_t.h
s_gswn.o: /usr/include/sys/_types/_u_int8_t.h
s_gswn.o: /usr/include/sys/_types/_u_int16_t.h
s_gswn.o: /usr/include/sys/_types/_u_int32_t.h
s_gswn.o: /usr/include/sys/_types/_u_int64_t.h
s_gswn.o: /usr/include/sys/_types/_intptr_t.h
s_gswn.o: /usr/include/sys/_types/_uintptr_t.h
s_gswn.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gswn.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gswn.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gswn.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gswn.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gswn.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gswn.o: /usr/include/libkern/_OSByteOrder.h
s_gswn.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gswn.o: /usr/include/sys/_types/_u_char.h
s_gswn.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
s_gswn.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
s_gswn.o: /usr/include/sys/_types/_blkcnt_t.h
s_gswn.o: /usr/include/sys/_types/_blksize_t.h
s_gswn.o: /usr/include/sys/_types/_gid_t.h
s_gswn.o: /usr/include/sys/_types/_in_addr_t.h
s_gswn.o: /usr/include/sys/_types/_in_port_t.h
s_gswn.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
s_gswn.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gswn.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gswn.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gswn.o: /usr/include/sys/_types/_clock_t.h
s_gswn.o: /usr/include/sys/_types/_time_t.h
s_gswn.o: /usr/include/sys/_types/_useconds_t.h
s_gswn.o: /usr/include/sys/_types/_suseconds_t.h
s_gswn.o: /usr/include/sys/_types/_rsize_t.h
s_gswn.o: /usr/include/sys/_types/_errno_t.h
s_gswn.o: /usr/include/sys/_types/_fd_def.h
s_gswn.o: /usr/include/sys/_types/_fd_setsize.h
s_gswn.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
s_gswn.o: /usr/include/sys/_types/_fd_zero.h
s_gswn.o: /usr/include/sys/_types/_fd_isset.h
s_gswn.o: /usr/include/sys/_types/_fd_copy.h
s_gswn.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gswn.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gswn.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gswn.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gswn.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gswn.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gswn.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gswn.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gswn.o: /usr/include/sys/_pthread/_pthread_t.h
s_gswn.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gswn.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gswn.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gswn.o: ../../../.././include/ncarg/c.h
s_gtx.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gtx.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gtx.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gtx.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gtx.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gtx.o: /usr/include/i386/_types.h
s_gtx.o: /usr/include/sys/_pthread/_pthread_types.h
s_gtx.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gtx.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gtx.o: /usr/include/sys/_types/_int16_t.h
s_gtx.o: /usr/include/sys/_types/_int32_t.h
s_gtx.o: /usr/include/sys/_types/_int64_t.h
s_gtx.o: /usr/include/sys/_types/_u_int8_t.h
s_gtx.o: /usr/include/sys/_types/_u_int16_t.h
s_gtx.o: /usr/include/sys/_types/_u_int32_t.h
s_gtx.o: /usr/include/sys/_types/_u_int64_t.h
s_gtx.o: /usr/include/sys/_types/_intptr_t.h
s_gtx.o: /usr/include/sys/_types/_uintptr_t.h
s_gtx.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gtx.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gtx.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gtx.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gtx.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gtx.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gtx.o: /usr/include/libkern/_OSByteOrder.h
s_gtx.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gtx.o: /usr/include/sys/_types/_u_char.h /usr/include/sys/_types/_u_short.h
s_gtx.o: /usr/include/sys/_types/_u_int.h /usr/include/sys/_types/_caddr_t.h
s_gtx.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_blkcnt_t.h
s_gtx.o: /usr/include/sys/_types/_blksize_t.h
s_gtx.o: /usr/include/sys/_types/_gid_t.h
s_gtx.o: /usr/include/sys/_types/_in_addr_t.h
s_gtx.o: /usr/include/sys/_types/_in_port_t.h
s_gtx.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
s_gtx.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gtx.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gtx.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gtx.o: /usr/include/sys/_types/_clock_t.h /usr/include/sys/_types/_time_t.h
s_gtx.o: /usr/include/sys/_types/_useconds_t.h
s_gtx.o: /usr/include/sys/_types/_suseconds_t.h
s_gtx.o: /usr/include/sys/_types/_rsize_t.h
s_gtx.o: /usr/include/sys/_types/_errno_t.h /usr/include/sys/_types/_fd_def.h
s_gtx.o: /usr/include/sys/_types/_fd_setsize.h
s_gtx.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
s_gtx.o: /usr/include/sys/_types/_fd_zero.h
s_gtx.o: /usr/include/sys/_types/_fd_isset.h
s_gtx.o: /usr/include/sys/_types/_fd_copy.h
s_gtx.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gtx.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gtx.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gtx.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gtx.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gtx.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gtx.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gtx.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gtx.o: /usr/include/sys/_pthread/_pthread_t.h
s_gtx.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gtx.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gtx.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gtx.o: ../../../.././include/ncarg/c.h /usr/include/string.h
s_gtx.o: /usr/include/strings.h /usr/include/secure/_strings.h
s_gtx.o: /usr/include/secure/_string.h
s_guwk.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_guwk.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_guwk.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_guwk.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_guwk.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_guwk.o: /usr/include/i386/_types.h
s_guwk.o: /usr/include/sys/_pthread/_pthread_types.h
s_guwk.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_guwk.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_guwk.o: /usr/include/sys/_types/_int16_t.h
s_guwk.o: /usr/include/sys/_types/_int32_t.h
s_guwk.o: /usr/include/sys/_types/_int64_t.h
s_guwk.o: /usr/include/sys/_types/_u_int8_t.h
s_guwk.o: /usr/include/sys/_types/_u_int16_t.h
s_guwk.o: /usr/include/sys/_types/_u_int32_t.h
s_guwk.o: /usr/include/sys/_types/_u_int64_t.h
s_guwk.o: /usr/include/sys/_types/_intptr_t.h
s_guwk.o: /usr/include/sys/_types/_uintptr_t.h
s_guwk.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_guwk.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_guwk.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_guwk.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_guwk.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_guwk.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_guwk.o: /usr/include/libkern/_OSByteOrder.h
s_guwk.o: /usr/include/libkern/i386/_OSByteOrder.h
s_guwk.o: /usr/include/sys/_types/_u_char.h
s_guwk.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
s_guwk.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
s_guwk.o: /usr/include/sys/_types/_blkcnt_t.h
s_guwk.o: /usr/include/sys/_types/_blksize_t.h
s_guwk.o: /usr/include/sys/_types/_gid_t.h
s_guwk.o: /usr/include/sys/_types/_in_addr_t.h
s_guwk.o: /usr/include/sys/_types/_in_port_t.h
s_guwk.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
s_guwk.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_guwk.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_guwk.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_guwk.o: /usr/include/sys/_types/_clock_t.h
s_guwk.o: /usr/include/sys/_types/_time_t.h
s_guwk.o: /usr/include/sys/_types/_useconds_t.h
s_guwk.o: /usr/include/sys/_types/_suseconds_t.h
s_guwk.o: /usr/include/sys/_types/_rsize_t.h
s_guwk.o: /usr/include/sys/_types/_errno_t.h
s_guwk.o: /usr/include/sys/_types/_fd_def.h
s_guwk.o: /usr/include/sys/_types/_fd_setsize.h
s_guwk.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
s_guwk.o: /usr/include/sys/_types/_fd_zero.h
s_guwk.o: /usr/include/sys/_types/_fd_isset.h
s_guwk.o: /usr/include/sys/_types/_fd_copy.h
s_guwk.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_guwk.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_guwk.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_guwk.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_guwk.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_guwk.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_guwk.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_guwk.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_guwk.o: /usr/include/sys/_pthread/_pthread_t.h
s_guwk.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_guwk.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_guwk.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_guwk.o: ../../../.././include/ncarg/c.h
s_gwitm.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gwitm.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gwitm.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gwitm.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gwitm.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gwitm.o: /usr/include/i386/_types.h
s_gwitm.o: /usr/include/sys/_pthread/_pthread_types.h
s_gwitm.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gwitm.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gwitm.o: /usr/include/sys/_types/_int16_t.h
s_gwitm.o: /usr/include/sys/_types/_int32_t.h
s_gwitm.o: /usr/include/sys/_types/_int64_t.h
s_gwitm.o: /usr/include/sys/_types/_u_int8_t.h
s_gwitm.o: /usr/include/sys/_types/_u_int16_t.h
s_gwitm.o: /usr/include/sys/_types/_u_int32_t.h
s_gwitm.o: /usr/include/sys/_types/_u_int64_t.h
s_gwitm.o: /usr/include/sys/_types/_intptr_t.h
s_gwitm.o: /usr/include/sys/_types/_uintptr_t.h
s_gwitm.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gwitm.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gwitm.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gwitm.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gwitm.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gwitm.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gwitm.o: /usr/include/libkern/_OSByteOrder.h
s_gwitm.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gwitm.o: /usr/include/sys/_types/_u_char.h
s_gwitm.o: /usr/include/sys/_types/_u_short.h
s_gwitm.o: /usr/include/sys/_types/_u_int.h
s_gwitm.o: /usr/include/sys/_types/_caddr_t.h
s_gwitm.o: /usr/include/sys/_types/_dev_t.h
s_gwitm.o: /usr/include/sys/_types/_blkcnt_t.h
s_gwitm.o: /usr/include/sys/_types/_blksize_t.h
s_gwitm.o: /usr/include/sys/_types/_gid_t.h
s_gwitm.o: /usr/include/sys/_types/_in_addr_t.h
s_gwitm.o: /usr/include/sys/_types/_in_port_t.h
s_gwitm.o: /usr/include/sys/_types/_ino_t.h
s_gwitm.o: /usr/include/sys/_types/_ino64_t.h
s_gwitm.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gwitm.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gwitm.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gwitm.o: /usr/include/sys/_types/_clock_t.h
s_gwitm.o: /usr/include/sys/_types/_time_t.h
s_gwitm.o: /usr/include/sys/_types/_useconds_t.h
s_gwitm.o: /usr/include/sys/_types/_suseconds_t.h
s_gwitm.o: /usr/include/sys/_types/_rsize_t.h
s_gwitm.o: /usr/include/sys/_types/_errno_t.h
s_gwitm.o: /usr/include/sys/_types/_fd_def.h
s_gwitm.o: /usr/include/sys/_types/_fd_setsize.h
s_gwitm.o: /usr/include/sys/_types/_fd_set.h
s_gwitm.o: /usr/include/sys/_types/_fd_clr.h
s_gwitm.o: /usr/include/sys/_types/_fd_zero.h
s_gwitm.o: /usr/include/sys/_types/_fd_isset.h
s_gwitm.o: /usr/include/sys/_types/_fd_copy.h
s_gwitm.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gwitm.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gwitm.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gwitm.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gwitm.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gwitm.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gwitm.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gwitm.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gwitm.o: /usr/include/sys/_pthread/_pthread_t.h
s_gwitm.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gwitm.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gwitm.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gwitm.o: ../../../.././include/ncarg/c.h
s_gcrsg.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gcrsg.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gcrsg.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gcrsg.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gcrsg.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gcrsg.o: /usr/include/i386/_types.h
s_gcrsg.o: /usr/include/sys/_pthread/_pthread_types.h
s_gcrsg.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gcrsg.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gcrsg.o: /usr/include/sys/_types/_int16_t.h
s_gcrsg.o: /usr/include/sys/_types/_int32_t.h
s_gcrsg.o: /usr/include/sys/_types/_int64_t.h
s_gcrsg.o: /usr/include/sys/_types/_u_int8_t.h
s_gcrsg.o: /usr/include/sys/_types/_u_int16_t.h
s_gcrsg.o: /usr/include/sys/_types/_u_int32_t.h
s_gcrsg.o: /usr/include/sys/_types/_u_int64_t.h
s_gcrsg.o: /usr/include/sys/_types/_intptr_t.h
s_gcrsg.o: /usr/include/sys/_types/_uintptr_t.h
s_gcrsg.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gcrsg.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gcrsg.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gcrsg.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gcrsg.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gcrsg.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gcrsg.o: /usr/include/libkern/_OSByteOrder.h
s_gcrsg.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gcrsg.o: /usr/include/sys/_types/_u_char.h
s_gcrsg.o: /usr/include/sys/_types/_u_short.h
s_gcrsg.o: /usr/include/sys/_types/_u_int.h
s_gcrsg.o: /usr/include/sys/_types/_caddr_t.h
s_gcrsg.o: /usr/include/sys/_types/_dev_t.h
s_gcrsg.o: /usr/include/sys/_types/_blkcnt_t.h
s_gcrsg.o: /usr/include/sys/_types/_blksize_t.h
s_gcrsg.o: /usr/include/sys/_types/_gid_t.h
s_gcrsg.o: /usr/include/sys/_types/_in_addr_t.h
s_gcrsg.o: /usr/include/sys/_types/_in_port_t.h
s_gcrsg.o: /usr/include/sys/_types/_ino_t.h
s_gcrsg.o: /usr/include/sys/_types/_ino64_t.h
s_gcrsg.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gcrsg.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gcrsg.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gcrsg.o: /usr/include/sys/_types/_clock_t.h
s_gcrsg.o: /usr/include/sys/_types/_time_t.h
s_gcrsg.o: /usr/include/sys/_types/_useconds_t.h
s_gcrsg.o: /usr/include/sys/_types/_suseconds_t.h
s_gcrsg.o: /usr/include/sys/_types/_rsize_t.h
s_gcrsg.o: /usr/include/sys/_types/_errno_t.h
s_gcrsg.o: /usr/include/sys/_types/_fd_def.h
s_gcrsg.o: /usr/include/sys/_types/_fd_setsize.h
s_gcrsg.o: /usr/include/sys/_types/_fd_set.h
s_gcrsg.o: /usr/include/sys/_types/_fd_clr.h
s_gcrsg.o: /usr/include/sys/_types/_fd_zero.h
s_gcrsg.o: /usr/include/sys/_types/_fd_isset.h
s_gcrsg.o: /usr/include/sys/_types/_fd_copy.h
s_gcrsg.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gcrsg.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gcrsg.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gcrsg.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gcrsg.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gcrsg.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gcrsg.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gcrsg.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gcrsg.o: /usr/include/sys/_pthread/_pthread_t.h
s_gcrsg.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gcrsg.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gcrsg.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gcrsg.o: ../../../.././include/ncarg/c.h
s_gclsg.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gclsg.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gclsg.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gclsg.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gclsg.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gclsg.o: /usr/include/i386/_types.h
s_gclsg.o: /usr/include/sys/_pthread/_pthread_types.h
s_gclsg.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gclsg.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gclsg.o: /usr/include/sys/_types/_int16_t.h
s_gclsg.o: /usr/include/sys/_types/_int32_t.h
s_gclsg.o: /usr/include/sys/_types/_int64_t.h
s_gclsg.o: /usr/include/sys/_types/_u_int8_t.h
s_gclsg.o: /usr/include/sys/_types/_u_int16_t.h
s_gclsg.o: /usr/include/sys/_types/_u_int32_t.h
s_gclsg.o: /usr/include/sys/_types/_u_int64_t.h
s_gclsg.o: /usr/include/sys/_types/_intptr_t.h
s_gclsg.o: /usr/include/sys/_types/_uintptr_t.h
s_gclsg.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gclsg.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gclsg.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gclsg.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gclsg.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gclsg.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gclsg.o: /usr/include/libkern/_OSByteOrder.h
s_gclsg.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gclsg.o: /usr/include/sys/_types/_u_char.h
s_gclsg.o: /usr/include/sys/_types/_u_short.h
s_gclsg.o: /usr/include/sys/_types/_u_int.h
s_gclsg.o: /usr/include/sys/_types/_caddr_t.h
s_gclsg.o: /usr/include/sys/_types/_dev_t.h
s_gclsg.o: /usr/include/sys/_types/_blkcnt_t.h
s_gclsg.o: /usr/include/sys/_types/_blksize_t.h
s_gclsg.o: /usr/include/sys/_types/_gid_t.h
s_gclsg.o: /usr/include/sys/_types/_in_addr_t.h
s_gclsg.o: /usr/include/sys/_types/_in_port_t.h
s_gclsg.o: /usr/include/sys/_types/_ino_t.h
s_gclsg.o: /usr/include/sys/_types/_ino64_t.h
s_gclsg.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gclsg.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gclsg.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gclsg.o: /usr/include/sys/_types/_clock_t.h
s_gclsg.o: /usr/include/sys/_types/_time_t.h
s_gclsg.o: /usr/include/sys/_types/_useconds_t.h
s_gclsg.o: /usr/include/sys/_types/_suseconds_t.h
s_gclsg.o: /usr/include/sys/_types/_rsize_t.h
s_gclsg.o: /usr/include/sys/_types/_errno_t.h
s_gclsg.o: /usr/include/sys/_types/_fd_def.h
s_gclsg.o: /usr/include/sys/_types/_fd_setsize.h
s_gclsg.o: /usr/include/sys/_types/_fd_set.h
s_gclsg.o: /usr/include/sys/_types/_fd_clr.h
s_gclsg.o: /usr/include/sys/_types/_fd_zero.h
s_gclsg.o: /usr/include/sys/_types/_fd_isset.h
s_gclsg.o: /usr/include/sys/_types/_fd_copy.h
s_gclsg.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gclsg.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gclsg.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gclsg.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gclsg.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gclsg.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gclsg.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gclsg.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gclsg.o: /usr/include/sys/_pthread/_pthread_t.h
s_gclsg.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gclsg.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gclsg.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gclsg.o: ../../../.././include/ncarg/c.h
s_gdsg.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gdsg.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gdsg.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gdsg.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gdsg.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gdsg.o: /usr/include/i386/_types.h
s_gdsg.o: /usr/include/sys/_pthread/_pthread_types.h
s_gdsg.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gdsg.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gdsg.o: /usr/include/sys/_types/_int16_t.h
s_gdsg.o: /usr/include/sys/_types/_int32_t.h
s_gdsg.o: /usr/include/sys/_types/_int64_t.h
s_gdsg.o: /usr/include/sys/_types/_u_int8_t.h
s_gdsg.o: /usr/include/sys/_types/_u_int16_t.h
s_gdsg.o: /usr/include/sys/_types/_u_int32_t.h
s_gdsg.o: /usr/include/sys/_types/_u_int64_t.h
s_gdsg.o: /usr/include/sys/_types/_intptr_t.h
s_gdsg.o: /usr/include/sys/_types/_uintptr_t.h
s_gdsg.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gdsg.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gdsg.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gdsg.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gdsg.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gdsg.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gdsg.o: /usr/include/libkern/_OSByteOrder.h
s_gdsg.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gdsg.o: /usr/include/sys/_types/_u_char.h
s_gdsg.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
s_gdsg.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
s_gdsg.o: /usr/include/sys/_types/_blkcnt_t.h
s_gdsg.o: /usr/include/sys/_types/_blksize_t.h
s_gdsg.o: /usr/include/sys/_types/_gid_t.h
s_gdsg.o: /usr/include/sys/_types/_in_addr_t.h
s_gdsg.o: /usr/include/sys/_types/_in_port_t.h
s_gdsg.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
s_gdsg.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gdsg.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gdsg.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gdsg.o: /usr/include/sys/_types/_clock_t.h
s_gdsg.o: /usr/include/sys/_types/_time_t.h
s_gdsg.o: /usr/include/sys/_types/_useconds_t.h
s_gdsg.o: /usr/include/sys/_types/_suseconds_t.h
s_gdsg.o: /usr/include/sys/_types/_rsize_t.h
s_gdsg.o: /usr/include/sys/_types/_errno_t.h
s_gdsg.o: /usr/include/sys/_types/_fd_def.h
s_gdsg.o: /usr/include/sys/_types/_fd_setsize.h
s_gdsg.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
s_gdsg.o: /usr/include/sys/_types/_fd_zero.h
s_gdsg.o: /usr/include/sys/_types/_fd_isset.h
s_gdsg.o: /usr/include/sys/_types/_fd_copy.h
s_gdsg.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gdsg.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gdsg.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gdsg.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gdsg.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gdsg.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gdsg.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gdsg.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gdsg.o: /usr/include/sys/_pthread/_pthread_t.h
s_gdsg.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gdsg.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gdsg.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gdsg.o: ../../../.././include/ncarg/c.h
s_gcsgwk.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gcsgwk.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gcsgwk.o: /usr/include/sys/_posix_availability.h
s_gcsgwk.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gcsgwk.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gcsgwk.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gcsgwk.o: /usr/include/sys/_pthread/_pthread_types.h
s_gcsgwk.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gcsgwk.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gcsgwk.o: /usr/include/sys/_types/_int16_t.h
s_gcsgwk.o: /usr/include/sys/_types/_int32_t.h
s_gcsgwk.o: /usr/include/sys/_types/_int64_t.h
s_gcsgwk.o: /usr/include/sys/_types/_u_int8_t.h
s_gcsgwk.o: /usr/include/sys/_types/_u_int16_t.h
s_gcsgwk.o: /usr/include/sys/_types/_u_int32_t.h
s_gcsgwk.o: /usr/include/sys/_types/_u_int64_t.h
s_gcsgwk.o: /usr/include/sys/_types/_intptr_t.h
s_gcsgwk.o: /usr/include/sys/_types/_uintptr_t.h
s_gcsgwk.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gcsgwk.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gcsgwk.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gcsgwk.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gcsgwk.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gcsgwk.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gcsgwk.o: /usr/include/libkern/_OSByteOrder.h
s_gcsgwk.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gcsgwk.o: /usr/include/sys/_types/_u_char.h
s_gcsgwk.o: /usr/include/sys/_types/_u_short.h
s_gcsgwk.o: /usr/include/sys/_types/_u_int.h
s_gcsgwk.o: /usr/include/sys/_types/_caddr_t.h
s_gcsgwk.o: /usr/include/sys/_types/_dev_t.h
s_gcsgwk.o: /usr/include/sys/_types/_blkcnt_t.h
s_gcsgwk.o: /usr/include/sys/_types/_blksize_t.h
s_gcsgwk.o: /usr/include/sys/_types/_gid_t.h
s_gcsgwk.o: /usr/include/sys/_types/_in_addr_t.h
s_gcsgwk.o: /usr/include/sys/_types/_in_port_t.h
s_gcsgwk.o: /usr/include/sys/_types/_ino_t.h
s_gcsgwk.o: /usr/include/sys/_types/_ino64_t.h
s_gcsgwk.o: /usr/include/sys/_types/_key_t.h
s_gcsgwk.o: /usr/include/sys/_types/_mode_t.h
s_gcsgwk.o: /usr/include/sys/_types/_nlink_t.h
s_gcsgwk.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gcsgwk.o: /usr/include/sys/_types/_uid_t.h
s_gcsgwk.o: /usr/include/sys/_types/_clock_t.h
s_gcsgwk.o: /usr/include/sys/_types/_time_t.h
s_gcsgwk.o: /usr/include/sys/_types/_useconds_t.h
s_gcsgwk.o: /usr/include/sys/_types/_suseconds_t.h
s_gcsgwk.o: /usr/include/sys/_types/_rsize_t.h
s_gcsgwk.o: /usr/include/sys/_types/_errno_t.h
s_gcsgwk.o: /usr/include/sys/_types/_fd_def.h
s_gcsgwk.o: /usr/include/sys/_types/_fd_setsize.h
s_gcsgwk.o: /usr/include/sys/_types/_fd_set.h
s_gcsgwk.o: /usr/include/sys/_types/_fd_clr.h
s_gcsgwk.o: /usr/include/sys/_types/_fd_zero.h
s_gcsgwk.o: /usr/include/sys/_types/_fd_isset.h
s_gcsgwk.o: /usr/include/sys/_types/_fd_copy.h
s_gcsgwk.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gcsgwk.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gcsgwk.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gcsgwk.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gcsgwk.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gcsgwk.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gcsgwk.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gcsgwk.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gcsgwk.o: /usr/include/sys/_pthread/_pthread_t.h
s_gcsgwk.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gcsgwk.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gcsgwk.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gcsgwk.o: ../../../.././include/ncarg/c.h
s_gssgt.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gssgt.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gssgt.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gssgt.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gssgt.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gssgt.o: /usr/include/i386/_types.h
s_gssgt.o: /usr/include/sys/_pthread/_pthread_types.h
s_gssgt.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gssgt.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gssgt.o: /usr/include/sys/_types/_int16_t.h
s_gssgt.o: /usr/include/sys/_types/_int32_t.h
s_gssgt.o: /usr/include/sys/_types/_int64_t.h
s_gssgt.o: /usr/include/sys/_types/_u_int8_t.h
s_gssgt.o: /usr/include/sys/_types/_u_int16_t.h
s_gssgt.o: /usr/include/sys/_types/_u_int32_t.h
s_gssgt.o: /usr/include/sys/_types/_u_int64_t.h
s_gssgt.o: /usr/include/sys/_types/_intptr_t.h
s_gssgt.o: /usr/include/sys/_types/_uintptr_t.h
s_gssgt.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gssgt.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gssgt.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gssgt.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gssgt.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gssgt.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gssgt.o: /usr/include/libkern/_OSByteOrder.h
s_gssgt.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gssgt.o: /usr/include/sys/_types/_u_char.h
s_gssgt.o: /usr/include/sys/_types/_u_short.h
s_gssgt.o: /usr/include/sys/_types/_u_int.h
s_gssgt.o: /usr/include/sys/_types/_caddr_t.h
s_gssgt.o: /usr/include/sys/_types/_dev_t.h
s_gssgt.o: /usr/include/sys/_types/_blkcnt_t.h
s_gssgt.o: /usr/include/sys/_types/_blksize_t.h
s_gssgt.o: /usr/include/sys/_types/_gid_t.h
s_gssgt.o: /usr/include/sys/_types/_in_addr_t.h
s_gssgt.o: /usr/include/sys/_types/_in_port_t.h
s_gssgt.o: /usr/include/sys/_types/_ino_t.h
s_gssgt.o: /usr/include/sys/_types/_ino64_t.h
s_gssgt.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gssgt.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gssgt.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gssgt.o: /usr/include/sys/_types/_clock_t.h
s_gssgt.o: /usr/include/sys/_types/_time_t.h
s_gssgt.o: /usr/include/sys/_types/_useconds_t.h
s_gssgt.o: /usr/include/sys/_types/_suseconds_t.h
s_gssgt.o: /usr/include/sys/_types/_rsize_t.h
s_gssgt.o: /usr/include/sys/_types/_errno_t.h
s_gssgt.o: /usr/include/sys/_types/_fd_def.h
s_gssgt.o: /usr/include/sys/_types/_fd_setsize.h
s_gssgt.o: /usr/include/sys/_types/_fd_set.h
s_gssgt.o: /usr/include/sys/_types/_fd_clr.h
s_gssgt.o: /usr/include/sys/_types/_fd_zero.h
s_gssgt.o: /usr/include/sys/_types/_fd_isset.h
s_gssgt.o: /usr/include/sys/_types/_fd_copy.h
s_gssgt.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gssgt.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gssgt.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gssgt.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gssgt.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gssgt.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gssgt.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gssgt.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gssgt.o: /usr/include/sys/_pthread/_pthread_t.h
s_gssgt.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gssgt.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gssgt.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gssgt.o: ../../../.././include/ncarg/c.h
s_ginst.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_ginst.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_ginst.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_ginst.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_ginst.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_ginst.o: /usr/include/i386/_types.h
s_ginst.o: /usr/include/sys/_pthread/_pthread_types.h
s_ginst.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_ginst.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_ginst.o: /usr/include/sys/_types/_int16_t.h
s_ginst.o: /usr/include/sys/_types/_int32_t.h
s_ginst.o: /usr/include/sys/_types/_int64_t.h
s_ginst.o: /usr/include/sys/_types/_u_int8_t.h
s_ginst.o: /usr/include/sys/_types/_u_int16_t.h
s_ginst.o: /usr/include/sys/_types/_u_int32_t.h
s_ginst.o: /usr/include/sys/_types/_u_int64_t.h
s_ginst.o: /usr/include/sys/_types/_intptr_t.h
s_ginst.o: /usr/include/sys/_types/_uintptr_t.h
s_ginst.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_ginst.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_ginst.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_ginst.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_ginst.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_ginst.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_ginst.o: /usr/include/libkern/_OSByteOrder.h
s_ginst.o: /usr/include/libkern/i386/_OSByteOrder.h
s_ginst.o: /usr/include/sys/_types/_u_char.h
s_ginst.o: /usr/include/sys/_types/_u_short.h
s_ginst.o: /usr/include/sys/_types/_u_int.h
s_ginst.o: /usr/include/sys/_types/_caddr_t.h
s_ginst.o: /usr/include/sys/_types/_dev_t.h
s_ginst.o: /usr/include/sys/_types/_blkcnt_t.h
s_ginst.o: /usr/include/sys/_types/_blksize_t.h
s_ginst.o: /usr/include/sys/_types/_gid_t.h
s_ginst.o: /usr/include/sys/_types/_in_addr_t.h
s_ginst.o: /usr/include/sys/_types/_in_port_t.h
s_ginst.o: /usr/include/sys/_types/_ino_t.h
s_ginst.o: /usr/include/sys/_types/_ino64_t.h
s_ginst.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_ginst.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_ginst.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_ginst.o: /usr/include/sys/_types/_clock_t.h
s_ginst.o: /usr/include/sys/_types/_time_t.h
s_ginst.o: /usr/include/sys/_types/_useconds_t.h
s_ginst.o: /usr/include/sys/_types/_suseconds_t.h
s_ginst.o: /usr/include/sys/_types/_rsize_t.h
s_ginst.o: /usr/include/sys/_types/_errno_t.h
s_ginst.o: /usr/include/sys/_types/_fd_def.h
s_ginst.o: /usr/include/sys/_types/_fd_setsize.h
s_ginst.o: /usr/include/sys/_types/_fd_set.h
s_ginst.o: /usr/include/sys/_types/_fd_clr.h
s_ginst.o: /usr/include/sys/_types/_fd_zero.h
s_ginst.o: /usr/include/sys/_types/_fd_isset.h
s_ginst.o: /usr/include/sys/_types/_fd_copy.h
s_ginst.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_ginst.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_ginst.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_ginst.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_ginst.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_ginst.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_ginst.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_ginst.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_ginst.o: /usr/include/sys/_pthread/_pthread_t.h
s_ginst.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_ginst.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_ginst.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_ginst.o: ../../../.././include/ncarg/c.h /usr/include/string.h
s_ginst.o: /usr/include/strings.h /usr/include/secure/_strings.h
s_ginst.o: /usr/include/secure/_string.h
s_gqopsg.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqopsg.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqopsg.o: /usr/include/sys/_posix_availability.h
s_gqopsg.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gqopsg.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gqopsg.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gqopsg.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqopsg.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqopsg.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqopsg.o: /usr/include/sys/_types/_int16_t.h
s_gqopsg.o: /usr/include/sys/_types/_int32_t.h
s_gqopsg.o: /usr/include/sys/_types/_int64_t.h
s_gqopsg.o: /usr/include/sys/_types/_u_int8_t.h
s_gqopsg.o: /usr/include/sys/_types/_u_int16_t.h
s_gqopsg.o: /usr/include/sys/_types/_u_int32_t.h
s_gqopsg.o: /usr/include/sys/_types/_u_int64_t.h
s_gqopsg.o: /usr/include/sys/_types/_intptr_t.h
s_gqopsg.o: /usr/include/sys/_types/_uintptr_t.h
s_gqopsg.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqopsg.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqopsg.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqopsg.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqopsg.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqopsg.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqopsg.o: /usr/include/libkern/_OSByteOrder.h
s_gqopsg.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqopsg.o: /usr/include/sys/_types/_u_char.h
s_gqopsg.o: /usr/include/sys/_types/_u_short.h
s_gqopsg.o: /usr/include/sys/_types/_u_int.h
s_gqopsg.o: /usr/include/sys/_types/_caddr_t.h
s_gqopsg.o: /usr/include/sys/_types/_dev_t.h
s_gqopsg.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqopsg.o: /usr/include/sys/_types/_blksize_t.h
s_gqopsg.o: /usr/include/sys/_types/_gid_t.h
s_gqopsg.o: /usr/include/sys/_types/_in_addr_t.h
s_gqopsg.o: /usr/include/sys/_types/_in_port_t.h
s_gqopsg.o: /usr/include/sys/_types/_ino_t.h
s_gqopsg.o: /usr/include/sys/_types/_ino64_t.h
s_gqopsg.o: /usr/include/sys/_types/_key_t.h
s_gqopsg.o: /usr/include/sys/_types/_mode_t.h
s_gqopsg.o: /usr/include/sys/_types/_nlink_t.h
s_gqopsg.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gqopsg.o: /usr/include/sys/_types/_uid_t.h
s_gqopsg.o: /usr/include/sys/_types/_clock_t.h
s_gqopsg.o: /usr/include/sys/_types/_time_t.h
s_gqopsg.o: /usr/include/sys/_types/_useconds_t.h
s_gqopsg.o: /usr/include/sys/_types/_suseconds_t.h
s_gqopsg.o: /usr/include/sys/_types/_rsize_t.h
s_gqopsg.o: /usr/include/sys/_types/_errno_t.h
s_gqopsg.o: /usr/include/sys/_types/_fd_def.h
s_gqopsg.o: /usr/include/sys/_types/_fd_setsize.h
s_gqopsg.o: /usr/include/sys/_types/_fd_set.h
s_gqopsg.o: /usr/include/sys/_types/_fd_clr.h
s_gqopsg.o: /usr/include/sys/_types/_fd_zero.h
s_gqopsg.o: /usr/include/sys/_types/_fd_isset.h
s_gqopsg.o: /usr/include/sys/_types/_fd_copy.h
s_gqopsg.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqopsg.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqopsg.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqopsg.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqopsg.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqopsg.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqopsg.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqopsg.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqopsg.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqopsg.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqopsg.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqopsg.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqopsg.o: ../../../.././include/ncarg/c.h
s_gqsgus.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gqsgus.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gqsgus.o: /usr/include/sys/_posix_availability.h
s_gqsgus.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
s_gqsgus.o: /usr/include/_types.h /usr/include/sys/_types.h
s_gqsgus.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
s_gqsgus.o: /usr/include/sys/_pthread/_pthread_types.h
s_gqsgus.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gqsgus.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gqsgus.o: /usr/include/sys/_types/_int16_t.h
s_gqsgus.o: /usr/include/sys/_types/_int32_t.h
s_gqsgus.o: /usr/include/sys/_types/_int64_t.h
s_gqsgus.o: /usr/include/sys/_types/_u_int8_t.h
s_gqsgus.o: /usr/include/sys/_types/_u_int16_t.h
s_gqsgus.o: /usr/include/sys/_types/_u_int32_t.h
s_gqsgus.o: /usr/include/sys/_types/_u_int64_t.h
s_gqsgus.o: /usr/include/sys/_types/_intptr_t.h
s_gqsgus.o: /usr/include/sys/_types/_uintptr_t.h
s_gqsgus.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gqsgus.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gqsgus.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gqsgus.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gqsgus.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gqsgus.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gqsgus.o: /usr/include/libkern/_OSByteOrder.h
s_gqsgus.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gqsgus.o: /usr/include/sys/_types/_u_char.h
s_gqsgus.o: /usr/include/sys/_types/_u_short.h
s_gqsgus.o: /usr/include/sys/_types/_u_int.h
s_gqsgus.o: /usr/include/sys/_types/_caddr_t.h
s_gqsgus.o: /usr/include/sys/_types/_dev_t.h
s_gqsgus.o: /usr/include/sys/_types/_blkcnt_t.h
s_gqsgus.o: /usr/include/sys/_types/_blksize_t.h
s_gqsgus.o: /usr/include/sys/_types/_gid_t.h
s_gqsgus.o: /usr/include/sys/_types/_in_addr_t.h
s_gqsgus.o: /usr/include/sys/_types/_in_port_t.h
s_gqsgus.o: /usr/include/sys/_types/_ino_t.h
s_gqsgus.o: /usr/include/sys/_types/_ino64_t.h
s_gqsgus.o: /usr/include/sys/_types/_key_t.h
s_gqsgus.o: /usr/include/sys/_types/_mode_t.h
s_gqsgus.o: /usr/include/sys/_types/_nlink_t.h
s_gqsgus.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
s_gqsgus.o: /usr/include/sys/_types/_uid_t.h
s_gqsgus.o: /usr/include/sys/_types/_clock_t.h
s_gqsgus.o: /usr/include/sys/_types/_time_t.h
s_gqsgus.o: /usr/include/sys/_types/_useconds_t.h
s_gqsgus.o: /usr/include/sys/_types/_suseconds_t.h
s_gqsgus.o: /usr/include/sys/_types/_rsize_t.h
s_gqsgus.o: /usr/include/sys/_types/_errno_t.h
s_gqsgus.o: /usr/include/sys/_types/_fd_def.h
s_gqsgus.o: /usr/include/sys/_types/_fd_setsize.h
s_gqsgus.o: /usr/include/sys/_types/_fd_set.h
s_gqsgus.o: /usr/include/sys/_types/_fd_clr.h
s_gqsgus.o: /usr/include/sys/_types/_fd_zero.h
s_gqsgus.o: /usr/include/sys/_types/_fd_isset.h
s_gqsgus.o: /usr/include/sys/_types/_fd_copy.h
s_gqsgus.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gqsgus.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gqsgus.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gqsgus.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gqsgus.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gqsgus.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gqsgus.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gqsgus.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gqsgus.o: /usr/include/sys/_pthread/_pthread_t.h
s_gqsgus.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gqsgus.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gqsgus.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gqsgus.o: ../../../.././include/ncarg/c.h
s_grqst.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_grqst.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_grqst.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_grqst.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_grqst.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_grqst.o: /usr/include/i386/_types.h
s_grqst.o: /usr/include/sys/_pthread/_pthread_types.h
s_grqst.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_grqst.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_grqst.o: /usr/include/sys/_types/_int16_t.h
s_grqst.o: /usr/include/sys/_types/_int32_t.h
s_grqst.o: /usr/include/sys/_types/_int64_t.h
s_grqst.o: /usr/include/sys/_types/_u_int8_t.h
s_grqst.o: /usr/include/sys/_types/_u_int16_t.h
s_grqst.o: /usr/include/sys/_types/_u_int32_t.h
s_grqst.o: /usr/include/sys/_types/_u_int64_t.h
s_grqst.o: /usr/include/sys/_types/_intptr_t.h
s_grqst.o: /usr/include/sys/_types/_uintptr_t.h
s_grqst.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_grqst.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_grqst.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_grqst.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_grqst.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_grqst.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_grqst.o: /usr/include/libkern/_OSByteOrder.h
s_grqst.o: /usr/include/libkern/i386/_OSByteOrder.h
s_grqst.o: /usr/include/sys/_types/_u_char.h
s_grqst.o: /usr/include/sys/_types/_u_short.h
s_grqst.o: /usr/include/sys/_types/_u_int.h
s_grqst.o: /usr/include/sys/_types/_caddr_t.h
s_grqst.o: /usr/include/sys/_types/_dev_t.h
s_grqst.o: /usr/include/sys/_types/_blkcnt_t.h
s_grqst.o: /usr/include/sys/_types/_blksize_t.h
s_grqst.o: /usr/include/sys/_types/_gid_t.h
s_grqst.o: /usr/include/sys/_types/_in_addr_t.h
s_grqst.o: /usr/include/sys/_types/_in_port_t.h
s_grqst.o: /usr/include/sys/_types/_ino_t.h
s_grqst.o: /usr/include/sys/_types/_ino64_t.h
s_grqst.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_grqst.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_grqst.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_grqst.o: /usr/include/sys/_types/_clock_t.h
s_grqst.o: /usr/include/sys/_types/_time_t.h
s_grqst.o: /usr/include/sys/_types/_useconds_t.h
s_grqst.o: /usr/include/sys/_types/_suseconds_t.h
s_grqst.o: /usr/include/sys/_types/_rsize_t.h
s_grqst.o: /usr/include/sys/_types/_errno_t.h
s_grqst.o: /usr/include/sys/_types/_fd_def.h
s_grqst.o: /usr/include/sys/_types/_fd_setsize.h
s_grqst.o: /usr/include/sys/_types/_fd_set.h
s_grqst.o: /usr/include/sys/_types/_fd_clr.h
s_grqst.o: /usr/include/sys/_types/_fd_zero.h
s_grqst.o: /usr/include/sys/_types/_fd_isset.h
s_grqst.o: /usr/include/sys/_types/_fd_copy.h
s_grqst.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_grqst.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_grqst.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_grqst.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_grqst.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_grqst.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_grqst.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_grqst.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_grqst.o: /usr/include/sys/_pthread/_pthread_t.h
s_grqst.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_grqst.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_grqst.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_grqst.o: ../../../.././include/ncarg/c.h /usr/include/string.h
s_grqst.o: /usr/include/strings.h /usr/include/secure/_strings.h
s_grqst.o: /usr/include/secure/_string.h
s_gsstm.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
s_gsstm.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
s_gsstm.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
s_gsstm.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
s_gsstm.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
s_gsstm.o: /usr/include/i386/_types.h
s_gsstm.o: /usr/include/sys/_pthread/_pthread_types.h
s_gsstm.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
s_gsstm.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
s_gsstm.o: /usr/include/sys/_types/_int16_t.h
s_gsstm.o: /usr/include/sys/_types/_int32_t.h
s_gsstm.o: /usr/include/sys/_types/_int64_t.h
s_gsstm.o: /usr/include/sys/_types/_u_int8_t.h
s_gsstm.o: /usr/include/sys/_types/_u_int16_t.h
s_gsstm.o: /usr/include/sys/_types/_u_int32_t.h
s_gsstm.o: /usr/include/sys/_types/_u_int64_t.h
s_gsstm.o: /usr/include/sys/_types/_intptr_t.h
s_gsstm.o: /usr/include/sys/_types/_uintptr_t.h
s_gsstm.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
s_gsstm.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
s_gsstm.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
s_gsstm.o: /usr/include/secure/_common.h /usr/include/sys/types.h
s_gsstm.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
s_gsstm.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
s_gsstm.o: /usr/include/libkern/_OSByteOrder.h
s_gsstm.o: /usr/include/libkern/i386/_OSByteOrder.h
s_gsstm.o: /usr/include/sys/_types/_u_char.h
s_gsstm.o: /usr/include/sys/_types/_u_short.h
s_gsstm.o: /usr/include/sys/_types/_u_int.h
s_gsstm.o: /usr/include/sys/_types/_caddr_t.h
s_gsstm.o: /usr/include/sys/_types/_dev_t.h
s_gsstm.o: /usr/include/sys/_types/_blkcnt_t.h
s_gsstm.o: /usr/include/sys/_types/_blksize_t.h
s_gsstm.o: /usr/include/sys/_types/_gid_t.h
s_gsstm.o: /usr/include/sys/_types/_in_addr_t.h
s_gsstm.o: /usr/include/sys/_types/_in_port_t.h
s_gsstm.o: /usr/include/sys/_types/_ino_t.h
s_gsstm.o: /usr/include/sys/_types/_ino64_t.h
s_gsstm.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
s_gsstm.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
s_gsstm.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
s_gsstm.o: /usr/include/sys/_types/_clock_t.h
s_gsstm.o: /usr/include/sys/_types/_time_t.h
s_gsstm.o: /usr/include/sys/_types/_useconds_t.h
s_gsstm.o: /usr/include/sys/_types/_suseconds_t.h
s_gsstm.o: /usr/include/sys/_types/_rsize_t.h
s_gsstm.o: /usr/include/sys/_types/_errno_t.h
s_gsstm.o: /usr/include/sys/_types/_fd_def.h
s_gsstm.o: /usr/include/sys/_types/_fd_setsize.h
s_gsstm.o: /usr/include/sys/_types/_fd_set.h
s_gsstm.o: /usr/include/sys/_types/_fd_clr.h
s_gsstm.o: /usr/include/sys/_types/_fd_zero.h
s_gsstm.o: /usr/include/sys/_types/_fd_isset.h
s_gsstm.o: /usr/include/sys/_types/_fd_copy.h
s_gsstm.o: /usr/include/sys/_pthread/_pthread_attr_t.h
s_gsstm.o: /usr/include/sys/_pthread/_pthread_cond_t.h
s_gsstm.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
s_gsstm.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
s_gsstm.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
s_gsstm.o: /usr/include/sys/_pthread/_pthread_once_t.h
s_gsstm.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
s_gsstm.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
s_gsstm.o: /usr/include/sys/_pthread/_pthread_t.h
s_gsstm.o: /usr/include/sys/_pthread/_pthread_key_t.h
s_gsstm.o: /usr/include/sys/_types/_fsblkcnt_t.h
s_gsstm.o: /usr/include/sys/_types/_fsfilcnt_t.h
s_gsstm.o: ../../../.././include/ncarg/c.h
getcid.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
getcid.o: /usr/include/sys/_symbol_aliasing.h
getcid.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
getcid.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
getcid.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
getcid.o: /usr/include/i386/_types.h
getcid.o: /usr/include/sys/_pthread/_pthread_types.h
getcid.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
getcid.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
getcid.o: /usr/include/sys/_types/_int16_t.h
getcid.o: /usr/include/sys/_types/_int32_t.h
getcid.o: /usr/include/sys/_types/_int64_t.h
getcid.o: /usr/include/sys/_types/_u_int8_t.h
getcid.o: /usr/include/sys/_types/_u_int16_t.h
getcid.o: /usr/include/sys/_types/_u_int32_t.h
getcid.o: /usr/include/sys/_types/_u_int64_t.h
getcid.o: /usr/include/sys/_types/_intptr_t.h
getcid.o: /usr/include/sys/_types/_uintptr_t.h
getcid.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
getcid.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
getcid.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
getcid.o: /usr/include/secure/_common.h
rmcid.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
rmcid.o: /usr/include/sys/_symbol_aliasing.h
rmcid.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
rmcid.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
rmcid.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
rmcid.o: /usr/include/i386/_types.h
rmcid.o: /usr/include/sys/_pthread/_pthread_types.h
rmcid.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
rmcid.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
rmcid.o: /usr/include/sys/_types/_int16_t.h
rmcid.o: /usr/include/sys/_types/_int32_t.h
rmcid.o: /usr/include/sys/_types/_int64_t.h
rmcid.o: /usr/include/sys/_types/_u_int8_t.h
rmcid.o: /usr/include/sys/_types/_u_int16_t.h
rmcid.o: /usr/include/sys/_types/_u_int32_t.h
rmcid.o: /usr/include/sys/_types/_u_int64_t.h
rmcid.o: /usr/include/sys/_types/_intptr_t.h
rmcid.o: /usr/include/sys/_types/_uintptr_t.h
rmcid.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
rmcid.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
rmcid.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
rmcid.o: /usr/include/secure/_common.h
addcid.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
addcid.o: /usr/include/sys/_symbol_aliasing.h
addcid.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
addcid.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
addcid.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
addcid.o: /usr/include/i386/_types.h
addcid.o: /usr/include/sys/_pthread/_pthread_types.h
addcid.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
addcid.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
addcid.o: /usr/include/sys/_types/_int16_t.h
addcid.o: /usr/include/sys/_types/_int32_t.h
addcid.o: /usr/include/sys/_types/_int64_t.h
addcid.o: /usr/include/sys/_types/_u_int8_t.h
addcid.o: /usr/include/sys/_types/_u_int16_t.h
addcid.o: /usr/include/sys/_types/_u_int32_t.h
addcid.o: /usr/include/sys/_types/_u_int64_t.h
addcid.o: /usr/include/sys/_types/_intptr_t.h
addcid.o: /usr/include/sys/_types/_uintptr_t.h
addcid.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
addcid.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
addcid.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
addcid.o: /usr/include/secure/_common.h
setcid.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
setcid.o: /usr/include/sys/_symbol_aliasing.h
setcid.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
setcid.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
setcid.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
setcid.o: /usr/include/i386/_types.h
setcid.o: /usr/include/sys/_pthread/_pthread_types.h
setcid.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
setcid.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
setcid.o: /usr/include/sys/_types/_int16_t.h
setcid.o: /usr/include/sys/_types/_int32_t.h
setcid.o: /usr/include/sys/_types/_int64_t.h
setcid.o: /usr/include/sys/_types/_u_int8_t.h
setcid.o: /usr/include/sys/_types/_u_int16_t.h
setcid.o: /usr/include/sys/_types/_u_int32_t.h
setcid.o: /usr/include/sys/_types/_u_int64_t.h
setcid.o: /usr/include/sys/_types/_intptr_t.h
setcid.o: /usr/include/sys/_types/_uintptr_t.h
setcid.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
setcid.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
setcid.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
setcid.o: /usr/include/secure/_common.h
gstore.o: /usr/include/stdlib.h /usr/include/Availability.h
gstore.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
gstore.o: /usr/include/sys/_types.h /usr/include/sys/cdefs.h
gstore.o: /usr/include/sys/_symbol_aliasing.h
gstore.o: /usr/include/sys/_posix_availability.h
gstore.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
gstore.o: /usr/include/sys/_pthread/_pthread_types.h /usr/include/sys/wait.h
gstore.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_id_t.h
gstore.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
gstore.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
gstore.o: /usr/include/machine/_mcontext.h /usr/include/i386/_mcontext.h
gstore.o: /usr/include/mach/machine/_structs.h
gstore.o: /usr/include/mach/i386/_structs.h /usr/include/machine/types.h
gstore.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
gstore.o: /usr/include/sys/_types/_int16_t.h
gstore.o: /usr/include/sys/_types/_int32_t.h
gstore.o: /usr/include/sys/_types/_int64_t.h
gstore.o: /usr/include/sys/_types/_u_int8_t.h
gstore.o: /usr/include/sys/_types/_u_int16_t.h
gstore.o: /usr/include/sys/_types/_u_int32_t.h
gstore.o: /usr/include/sys/_types/_u_int64_t.h
gstore.o: /usr/include/sys/_types/_intptr_t.h
gstore.o: /usr/include/sys/_types/_uintptr_t.h
gstore.o: /usr/include/sys/_pthread/_pthread_attr_t.h
gstore.o: /usr/include/sys/_types/_sigaltstack.h
gstore.o: /usr/include/sys/_types/_ucontext.h
gstore.o: /usr/include/sys/_types/_sigset_t.h
gstore.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_uid_t.h
gstore.o: /usr/include/sys/resource.h /usr/include/stdint.h
gstore.o: /usr/include/_types/_uint8_t.h /usr/include/_types/_uint16_t.h
gstore.o: /usr/include/_types/_uint32_t.h /usr/include/_types/_uint64_t.h
gstore.o: /usr/include/_types/_intmax_t.h /usr/include/_types/_uintmax_t.h
gstore.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
gstore.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
gstore.o: /usr/include/libkern/_OSByteOrder.h
gstore.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
gstore.o: /usr/include/sys/_types/_ct_rune_t.h
gstore.o: /usr/include/sys/_types/_rune_t.h
gstore.o: /usr/include/sys/_types/_wchar_t.h /usr/include/sys/_types/_null.h
gstore.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_mode_t.h
gstore.o: /usr/include/string.h /usr/include/sys/_types/_rsize_t.h
gstore.o: /usr/include/sys/_types/_errno_t.h
gstore.o: /usr/include/sys/_types/_ssize_t.h /usr/include/strings.h
gstore.o: /usr/include/secure/_strings.h /usr/include/secure/_common.h
gstore.o: /usr/include/secure/_string.h gstore.h
gstore.o: ../../../.././include/ncarg/gks.h /usr/include/stdio.h
gstore.o: /usr/include/sys/_types/_va_list.h /usr/include/sys/stdio.h
gstore.o: /usr/include/sys/_types/_off_t.h /usr/include/secure/_stdio.h
gstore.o: /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
gstore.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
gstore.o: /usr/include/sys/_types/_caddr_t.h
gstore.o: /usr/include/sys/_types/_blkcnt_t.h
gstore.o: /usr/include/sys/_types/_blksize_t.h
gstore.o: /usr/include/sys/_types/_gid_t.h
gstore.o: /usr/include/sys/_types/_in_addr_t.h
gstore.o: /usr/include/sys/_types/_in_port_t.h
gstore.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
gstore.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_nlink_t.h
gstore.o: /usr/include/sys/_types/_clock_t.h
gstore.o: /usr/include/sys/_types/_time_t.h
gstore.o: /usr/include/sys/_types/_useconds_t.h
gstore.o: /usr/include/sys/_types/_suseconds_t.h
gstore.o: /usr/include/sys/_types/_fd_def.h
gstore.o: /usr/include/sys/_types/_fd_setsize.h
gstore.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
gstore.o: /usr/include/sys/_types/_fd_zero.h
gstore.o: /usr/include/sys/_types/_fd_isset.h
gstore.o: /usr/include/sys/_types/_fd_copy.h
gstore.o: /usr/include/sys/_pthread/_pthread_cond_t.h
gstore.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
gstore.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
gstore.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
gstore.o: /usr/include/sys/_pthread/_pthread_once_t.h
gstore.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
gstore.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
gstore.o: /usr/include/sys/_pthread/_pthread_t.h
gstore.o: /usr/include/sys/_pthread/_pthread_key_t.h
gstore.o: /usr/include/sys/_types/_fsblkcnt_t.h
gstore.o: /usr/include/sys/_types/_fsfilcnt_t.h
gstore.o: ../../../.././include/ncarg/c.h
