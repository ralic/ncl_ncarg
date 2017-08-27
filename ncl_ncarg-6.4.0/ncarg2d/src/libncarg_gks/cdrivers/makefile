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
CURRENT_DIR		= ./ncarg2d/src/libncarg_gks/cdrivers

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
#	$Id: yMakefile,v 1.14 2010-04-02 16:36:16 brownrig Exp $
#

MYNAME		= cdrivers

INCLUDES	= gksP.h
BASICSRC	= fort_c.c gksc.c transform.c gksP.c argb.c
DEVSRC		= gks_device.c
CTXTSRC		= ctxt_device.c ctxt.c ctxt_conv.c
XSRC		= xdevice.c font.c text.c x_conv.c xattribute.c xcontrol.c  		xinquiry.c xoutput.c

PSSRC		= psdevice.c ps.c ps_conv.c psfill.c
PDFSRC          = pdf.c pdfdevice.c pdf_conv.c pdffill.c

PIXSRC 		= pixcontrol.c     pixoutput.c pixdevice.c  		pix_conv.c       pixattribute.c  pixinquiry.c  		pixwrite_xwd.c

BASICOBJ	= fort_c.o gksc.o transform.o gksP.o argb.o
DEVOBJ		= gks_device.o
CTXTOBJ		= ctxt_device.o ctxt.o ctxt_conv.o
XOBJ		= xdevice.o font.o text.o x_conv.o xattribute.o xcontrol.o  		xinquiry.o xoutput.o

PSOBJ		= psdevice.o ps.o ps_conv.o psfill.o
PDFOBJ          = pdf.o pdfdevice.o pdf_conv.o pdffill.o

PIXOBJ 		= pixcontrol.o     pixoutput.o pixdevice.o   		pix_conv.o       pixattribute.o  pixinquiry.o  		pixwrite_xwd.o $()

SOURCES		= $(BASICSRC) $(CTXTSRC) $(XSRC) $(PSSRC) $(PDFSRC) $(PIXSRC) $()
OBJECTS		= $(BASICOBJ) $(DEVOBJ) $(CTXTOBJ) $(XOBJ) $(PSOBJ) $(PDFOBJ) $(PIXOBJ) $()

EXTRA_CDEFINES	= $(OS_DEF) $(PNG_DEFINES) $() -DCTXT -DX11 -DPS -DPDF -DPIX

all::

all-local::../libbasegks.a

../libbasegks.a:: $(BASICOBJ)
	$(AR_REG) ../libbasegks.a $(BASICOBJ)

all::

clean-local::
	-@$(RM) $(BASICOBJ)

CAIROOBJ	=  cro_device.o cro.o cro_conv.o gks_device_cairo.o crotiff.o crodisplay.o

OBJECTS_CAIRO	= $(BASICOBJ) $(CTXTOBJ) $(XOBJ) $(PSOBJ) $(PDFOBJ) $(PIXOBJ) $(CAIROOBJ)

gks_device_cairo.c: gks_device.c
	$(LN) gks_device.c gks_device_cairo.c

gks_device_cairo.o: gks_device_cairo.c
	$(CC) $(CCOPTIONS) $(ALLDEFINES) -c gks_device_cairo.c

all::

all-local::../libncarg_gks.a

../libncarg_gks.a:: $(OBJECTS_CAIRO)
	$(AR_REG) ../libncarg_gks.a $(OBJECTS_CAIRO)

all::

clean-local::
	-@$(RM) $(OBJECTS_CAIRO)

all::

clean-local::
	-@$(RM) gks_device_cairo.c gks_device_cairo.o

all::

includes-local::
	@if (test ! -d $(BUILDINCDIR)/ncarg); then ($(MKDIRHIER) $(BUILDINCDIR)/ncarg); fi
	@cd $(BUILDINCDIR)/ncarg;\
	$(RM) $(INCLUDES); \
	for i in $(INCLUDES); do \
	(echo "$(LN) dstupdir/$(BUILDINCTOP)/$(CURRENT_DIR)/$$i .";\
	$(LN) ../$(BUILDINCTOP)/$(CURRENT_DIR)/$$i .;)\
	done

all::

all-local:: $(INCLUDES)

install-local:: $(INCLUDES)
	@if (test ! -d $(NCARGINCPATH)); then ($(MKDIRHIER) $(NCARGINCPATH)); fi
	@for name in $(INCLUDES); do \
	if (test -f $$name.$(EXE_SUFFIX)) then \
	(echo "Installing $$name.$(EXE_SUFFIX)";\
	$(INSTALL) -m $(INSTALL_INC) $$name.$(EXE_SUFFIX) $(NCARGINCPATH));\
	else \
	(echo "Installing $$name";\
	$(INSTALL) -m $(INSTALL_INC) $$name $(NCARGINCPATH)); fi \
	done

all::

depend-local:: $(SOURCES)
	$(DEPEND) -s "# DO NOT DELETE" -- $(ALLDEFINES) -- $(SOURCES)

all::

clean-local::
	-@$(RM) gks_device.o

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

fort_c.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
fort_c.o: /usr/include/sys/_symbol_aliasing.h
fort_c.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
fort_c.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
fort_c.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
fort_c.o: /usr/include/i386/_types.h
fort_c.o: /usr/include/sys/_pthread/_pthread_types.h
fort_c.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
fort_c.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
fort_c.o: /usr/include/sys/_types/_int16_t.h
fort_c.o: /usr/include/sys/_types/_int32_t.h
fort_c.o: /usr/include/sys/_types/_int64_t.h
fort_c.o: /usr/include/sys/_types/_u_int8_t.h
fort_c.o: /usr/include/sys/_types/_u_int16_t.h
fort_c.o: /usr/include/sys/_types/_u_int32_t.h
fort_c.o: /usr/include/sys/_types/_u_int64_t.h
fort_c.o: /usr/include/sys/_types/_intptr_t.h
fort_c.o: /usr/include/sys/_types/_uintptr_t.h
fort_c.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
fort_c.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
fort_c.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
fort_c.o: /usr/include/secure/_common.h /usr/include/stdlib.h
fort_c.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
fort_c.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
fort_c.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
fort_c.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
fort_c.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
fort_c.o: /usr/include/mach/i386/_structs.h
fort_c.o: /usr/include/sys/_pthread/_pthread_attr_t.h
fort_c.o: /usr/include/sys/_types/_sigaltstack.h
fort_c.o: /usr/include/sys/_types/_ucontext.h
fort_c.o: /usr/include/sys/_types/_sigset_t.h
fort_c.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
fort_c.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
fort_c.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
fort_c.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
fort_c.o: /usr/include/_types/_uintmax_t.h /usr/include/sys/_types/_timeval.h
fort_c.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
fort_c.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
fort_c.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
fort_c.o: /usr/include/sys/_types/_ct_rune_t.h
fort_c.o: /usr/include/sys/_types/_rune_t.h
fort_c.o: /usr/include/sys/_types/_wchar_t.h /usr/include/sys/_types/_dev_t.h
fort_c.o: /usr/include/sys/_types/_mode_t.h ../../../.././include/ncarg/c.h
fort_c.o: /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
fort_c.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
fort_c.o: /usr/include/sys/_types/_caddr_t.h
fort_c.o: /usr/include/sys/_types/_blkcnt_t.h
fort_c.o: /usr/include/sys/_types/_blksize_t.h
fort_c.o: /usr/include/sys/_types/_gid_t.h
fort_c.o: /usr/include/sys/_types/_in_addr_t.h
fort_c.o: /usr/include/sys/_types/_in_port_t.h
fort_c.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
fort_c.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_nlink_t.h
fort_c.o: /usr/include/sys/_types/_clock_t.h
fort_c.o: /usr/include/sys/_types/_time_t.h
fort_c.o: /usr/include/sys/_types/_useconds_t.h
fort_c.o: /usr/include/sys/_types/_suseconds_t.h
fort_c.o: /usr/include/sys/_types/_rsize_t.h
fort_c.o: /usr/include/sys/_types/_errno_t.h
fort_c.o: /usr/include/sys/_types/_fd_def.h
fort_c.o: /usr/include/sys/_types/_fd_setsize.h
fort_c.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
fort_c.o: /usr/include/sys/_types/_fd_zero.h
fort_c.o: /usr/include/sys/_types/_fd_isset.h
fort_c.o: /usr/include/sys/_types/_fd_copy.h
fort_c.o: /usr/include/sys/_pthread/_pthread_cond_t.h
fort_c.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
fort_c.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
fort_c.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
fort_c.o: /usr/include/sys/_pthread/_pthread_once_t.h
fort_c.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
fort_c.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
fort_c.o: /usr/include/sys/_pthread/_pthread_t.h
fort_c.o: /usr/include/sys/_pthread/_pthread_key_t.h
fort_c.o: /usr/include/sys/_types/_fsblkcnt_t.h
fort_c.o: /usr/include/sys/_types/_fsfilcnt_t.h gks.h gksc.h fort_c.h
gksc.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
gksc.o: /usr/include/sys/_symbol_aliasing.h
gksc.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
gksc.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
gksc.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
gksc.o: /usr/include/i386/_types.h /usr/include/sys/_pthread/_pthread_types.h
gksc.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
gksc.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
gksc.o: /usr/include/sys/_types/_int16_t.h /usr/include/sys/_types/_int32_t.h
gksc.o: /usr/include/sys/_types/_int64_t.h
gksc.o: /usr/include/sys/_types/_u_int8_t.h
gksc.o: /usr/include/sys/_types/_u_int16_t.h
gksc.o: /usr/include/sys/_types/_u_int32_t.h
gksc.o: /usr/include/sys/_types/_u_int64_t.h
gksc.o: /usr/include/sys/_types/_intptr_t.h
gksc.o: /usr/include/sys/_types/_uintptr_t.h
gksc.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
gksc.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
gksc.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
gksc.o: /usr/include/secure/_common.h /usr/include/stdlib.h
gksc.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
gksc.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
gksc.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
gksc.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
gksc.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
gksc.o: /usr/include/mach/i386/_structs.h
gksc.o: /usr/include/sys/_pthread/_pthread_attr_t.h
gksc.o: /usr/include/sys/_types/_sigaltstack.h
gksc.o: /usr/include/sys/_types/_ucontext.h
gksc.o: /usr/include/sys/_types/_sigset_t.h /usr/include/sys/_types/_uid_t.h
gksc.o: /usr/include/sys/resource.h /usr/include/stdint.h
gksc.o: /usr/include/_types/_uint8_t.h /usr/include/_types/_uint16_t.h
gksc.o: /usr/include/_types/_uint32_t.h /usr/include/_types/_uint64_t.h
gksc.o: /usr/include/_types/_intmax_t.h /usr/include/_types/_uintmax_t.h
gksc.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
gksc.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
gksc.o: /usr/include/libkern/_OSByteOrder.h
gksc.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
gksc.o: /usr/include/sys/_types/_ct_rune_t.h
gksc.o: /usr/include/sys/_types/_rune_t.h /usr/include/sys/_types/_wchar_t.h
gksc.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_mode_t.h
gksc.o: /usr/include/errno.h /usr/include/sys/errno.h
gksc.o: /usr/include/sys/_types/_errno_t.h /usr/include/limits.h
gksc.o: /usr/include/machine/limits.h /usr/include/i386/limits.h
gksc.o: /usr/include/i386/_limits.h /usr/include/sys/syslimits.h
gksc.o: ../../../.././include/ncarg/c.h /usr/include/sys/types.h
gksc.o: /usr/include/sys/_types/_u_char.h /usr/include/sys/_types/_u_short.h
gksc.o: /usr/include/sys/_types/_u_int.h /usr/include/sys/_types/_caddr_t.h
gksc.o: /usr/include/sys/_types/_blkcnt_t.h
gksc.o: /usr/include/sys/_types/_blksize_t.h /usr/include/sys/_types/_gid_t.h
gksc.o: /usr/include/sys/_types/_in_addr_t.h
gksc.o: /usr/include/sys/_types/_in_port_t.h /usr/include/sys/_types/_ino_t.h
gksc.o: /usr/include/sys/_types/_ino64_t.h /usr/include/sys/_types/_key_t.h
gksc.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_clock_t.h
gksc.o: /usr/include/sys/_types/_time_t.h
gksc.o: /usr/include/sys/_types/_useconds_t.h
gksc.o: /usr/include/sys/_types/_suseconds_t.h
gksc.o: /usr/include/sys/_types/_rsize_t.h /usr/include/sys/_types/_fd_def.h
gksc.o: /usr/include/sys/_types/_fd_setsize.h
gksc.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
gksc.o: /usr/include/sys/_types/_fd_zero.h
gksc.o: /usr/include/sys/_types/_fd_isset.h
gksc.o: /usr/include/sys/_types/_fd_copy.h
gksc.o: /usr/include/sys/_pthread/_pthread_cond_t.h
gksc.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
gksc.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
gksc.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
gksc.o: /usr/include/sys/_pthread/_pthread_once_t.h
gksc.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
gksc.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
gksc.o: /usr/include/sys/_pthread/_pthread_t.h
gksc.o: /usr/include/sys/_pthread/_pthread_key_t.h
gksc.o: /usr/include/sys/_types/_fsblkcnt_t.h
gksc.o: /usr/include/sys/_types/_fsfilcnt_t.h gks_device.h
gksc.o: /usr/include/stddef.h /usr/include/sys/_types/_offsetof.h
gksc.o: /usr/include/sys/_types/_ptrdiff_t.h
gksc.o: /usr/include/sys/_types/_wint_t.h gksc.h gks.h
transform.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
transform.o: /usr/include/sys/_symbol_aliasing.h
transform.o: /usr/include/sys/_posix_availability.h
transform.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
transform.o: /usr/include/_types.h /usr/include/sys/_types.h
transform.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
transform.o: /usr/include/sys/_pthread/_pthread_types.h
transform.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
transform.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
transform.o: /usr/include/sys/_types/_int16_t.h
transform.o: /usr/include/sys/_types/_int32_t.h
transform.o: /usr/include/sys/_types/_int64_t.h
transform.o: /usr/include/sys/_types/_u_int8_t.h
transform.o: /usr/include/sys/_types/_u_int16_t.h
transform.o: /usr/include/sys/_types/_u_int32_t.h
transform.o: /usr/include/sys/_types/_u_int64_t.h
transform.o: /usr/include/sys/_types/_intptr_t.h
transform.o: /usr/include/sys/_types/_uintptr_t.h
transform.o: /usr/include/sys/_types/_size_t.h
transform.o: /usr/include/sys/_types/_null.h /usr/include/sys/stdio.h
transform.o: /usr/include/sys/_types/_off_t.h
transform.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
transform.o: /usr/include/secure/_common.h transform.h common.h
transform.o: /usr/include/math.h
gksP.o: ../../../.././include/ncarg/gksP.h ../../../.././include/ncarg/c.h
gksP.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
gksP.o: /usr/include/sys/_symbol_aliasing.h
gksP.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
gksP.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
gksP.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
gksP.o: /usr/include/i386/_types.h /usr/include/sys/_pthread/_pthread_types.h
gksP.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
gksP.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
gksP.o: /usr/include/sys/_types/_int16_t.h /usr/include/sys/_types/_int32_t.h
gksP.o: /usr/include/sys/_types/_int64_t.h
gksP.o: /usr/include/sys/_types/_u_int8_t.h
gksP.o: /usr/include/sys/_types/_u_int16_t.h
gksP.o: /usr/include/sys/_types/_u_int32_t.h
gksP.o: /usr/include/sys/_types/_u_int64_t.h
gksP.o: /usr/include/sys/_types/_intptr_t.h
gksP.o: /usr/include/sys/_types/_uintptr_t.h
gksP.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
gksP.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
gksP.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
gksP.o: /usr/include/secure/_common.h /usr/include/sys/types.h
gksP.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
gksP.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
gksP.o: /usr/include/libkern/_OSByteOrder.h
gksP.o: /usr/include/libkern/i386/_OSByteOrder.h
gksP.o: /usr/include/sys/_types/_u_char.h /usr/include/sys/_types/_u_short.h
gksP.o: /usr/include/sys/_types/_u_int.h /usr/include/sys/_types/_caddr_t.h
gksP.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_blkcnt_t.h
gksP.o: /usr/include/sys/_types/_blksize_t.h /usr/include/sys/_types/_gid_t.h
gksP.o: /usr/include/sys/_types/_in_addr_t.h
gksP.o: /usr/include/sys/_types/_in_port_t.h /usr/include/sys/_types/_ino_t.h
gksP.o: /usr/include/sys/_types/_ino64_t.h /usr/include/sys/_types/_key_t.h
gksP.o: /usr/include/sys/_types/_mode_t.h /usr/include/sys/_types/_nlink_t.h
gksP.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
gksP.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/_types/_clock_t.h
gksP.o: /usr/include/sys/_types/_time_t.h
gksP.o: /usr/include/sys/_types/_useconds_t.h
gksP.o: /usr/include/sys/_types/_suseconds_t.h
gksP.o: /usr/include/sys/_types/_rsize_t.h /usr/include/sys/_types/_errno_t.h
gksP.o: /usr/include/sys/_types/_fd_def.h
gksP.o: /usr/include/sys/_types/_fd_setsize.h
gksP.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
gksP.o: /usr/include/sys/_types/_fd_zero.h
gksP.o: /usr/include/sys/_types/_fd_isset.h
gksP.o: /usr/include/sys/_types/_fd_copy.h
gksP.o: /usr/include/sys/_pthread/_pthread_attr_t.h
gksP.o: /usr/include/sys/_pthread/_pthread_cond_t.h
gksP.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
gksP.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
gksP.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
gksP.o: /usr/include/sys/_pthread/_pthread_once_t.h
gksP.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
gksP.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
gksP.o: /usr/include/sys/_pthread/_pthread_t.h
gksP.o: /usr/include/sys/_pthread/_pthread_key_t.h
gksP.o: /usr/include/sys/_types/_fsblkcnt_t.h
gksP.o: /usr/include/sys/_types/_fsfilcnt_t.h gks.h gksc.h gks_device.h
gksP.o: /usr/include/stddef.h /usr/include/sys/_types/_offsetof.h
gksP.o: /usr/include/sys/_types/_ptrdiff_t.h
gksP.o: /usr/include/sys/_types/_wchar_t.h /usr/include/sys/_types/_wint_t.h
argb.o: argb.h
ctxt_device.o: gks_device.h /usr/include/stddef.h /usr/include/_types.h
ctxt_device.o: /usr/include/sys/_types.h /usr/include/sys/cdefs.h
ctxt_device.o: /usr/include/sys/_symbol_aliasing.h
ctxt_device.o: /usr/include/sys/_posix_availability.h
ctxt_device.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
ctxt_device.o: /usr/include/sys/_pthread/_pthread_types.h
ctxt_device.o: /usr/include/sys/_types/_null.h
ctxt_device.o: /usr/include/sys/_types/_offsetof.h
ctxt_device.o: /usr/include/sys/_types/_ptrdiff_t.h
ctxt_device.o: /usr/include/machine/types.h /usr/include/i386/types.h
ctxt_device.o: /usr/include/sys/_types/_int8_t.h
ctxt_device.o: /usr/include/sys/_types/_int16_t.h
ctxt_device.o: /usr/include/sys/_types/_int32_t.h
ctxt_device.o: /usr/include/sys/_types/_int64_t.h
ctxt_device.o: /usr/include/sys/_types/_u_int8_t.h
ctxt_device.o: /usr/include/sys/_types/_u_int16_t.h
ctxt_device.o: /usr/include/sys/_types/_u_int32_t.h
ctxt_device.o: /usr/include/sys/_types/_u_int64_t.h
ctxt_device.o: /usr/include/sys/_types/_intptr_t.h
ctxt_device.o: /usr/include/sys/_types/_uintptr_t.h
ctxt_device.o: /usr/include/sys/_types/_rsize_t.h
ctxt_device.o: /usr/include/sys/_types/_size_t.h
ctxt_device.o: /usr/include/sys/_types/_wchar_t.h
ctxt_device.o: /usr/include/sys/_types/_wint_t.h ctxt_device.h gksc.h
ctxt_device.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
ctxt_device.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
ctxt_device.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
ctxt_device.o: /usr/include/libkern/i386/_OSByteOrder.h
ctxt_device.o: /usr/include/sys/_types/_u_char.h
ctxt_device.o: /usr/include/sys/_types/_u_short.h
ctxt_device.o: /usr/include/sys/_types/_u_int.h
ctxt_device.o: /usr/include/sys/_types/_caddr_t.h
ctxt_device.o: /usr/include/sys/_types/_dev_t.h
ctxt_device.o: /usr/include/sys/_types/_blkcnt_t.h
ctxt_device.o: /usr/include/sys/_types/_blksize_t.h
ctxt_device.o: /usr/include/sys/_types/_gid_t.h
ctxt_device.o: /usr/include/sys/_types/_in_addr_t.h
ctxt_device.o: /usr/include/sys/_types/_in_port_t.h
ctxt_device.o: /usr/include/sys/_types/_ino_t.h
ctxt_device.o: /usr/include/sys/_types/_ino64_t.h
ctxt_device.o: /usr/include/sys/_types/_key_t.h
ctxt_device.o: /usr/include/sys/_types/_mode_t.h
ctxt_device.o: /usr/include/sys/_types/_nlink_t.h
ctxt_device.o: /usr/include/sys/_types/_id_t.h
ctxt_device.o: /usr/include/sys/_types/_pid_t.h
ctxt_device.o: /usr/include/sys/_types/_off_t.h
ctxt_device.o: /usr/include/sys/_types/_uid_t.h
ctxt_device.o: /usr/include/sys/_types/_clock_t.h
ctxt_device.o: /usr/include/sys/_types/_ssize_t.h
ctxt_device.o: /usr/include/sys/_types/_time_t.h
ctxt_device.o: /usr/include/sys/_types/_useconds_t.h
ctxt_device.o: /usr/include/sys/_types/_suseconds_t.h
ctxt_device.o: /usr/include/sys/_types/_errno_t.h
ctxt_device.o: /usr/include/sys/_types/_fd_def.h
ctxt_device.o: /usr/include/sys/_types/_fd_setsize.h
ctxt_device.o: /usr/include/sys/_types/_fd_set.h
ctxt_device.o: /usr/include/sys/_types/_fd_clr.h
ctxt_device.o: /usr/include/sys/_types/_fd_zero.h
ctxt_device.o: /usr/include/sys/_types/_fd_isset.h
ctxt_device.o: /usr/include/sys/_types/_fd_copy.h
ctxt_device.o: /usr/include/sys/_pthread/_pthread_attr_t.h
ctxt_device.o: /usr/include/sys/_pthread/_pthread_cond_t.h
ctxt_device.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
ctxt_device.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
ctxt_device.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
ctxt_device.o: /usr/include/sys/_pthread/_pthread_once_t.h
ctxt_device.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
ctxt_device.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
ctxt_device.o: /usr/include/sys/_pthread/_pthread_t.h
ctxt_device.o: /usr/include/sys/_pthread/_pthread_key_t.h
ctxt_device.o: /usr/include/sys/_types/_fsblkcnt_t.h
ctxt_device.o: /usr/include/sys/_types/_fsfilcnt_t.h ctxt.h
ctxt.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
ctxt.o: /usr/include/sys/_symbol_aliasing.h
ctxt.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
ctxt.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
ctxt.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
ctxt.o: /usr/include/i386/_types.h /usr/include/sys/_pthread/_pthread_types.h
ctxt.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
ctxt.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
ctxt.o: /usr/include/sys/_types/_int16_t.h /usr/include/sys/_types/_int32_t.h
ctxt.o: /usr/include/sys/_types/_int64_t.h
ctxt.o: /usr/include/sys/_types/_u_int8_t.h
ctxt.o: /usr/include/sys/_types/_u_int16_t.h
ctxt.o: /usr/include/sys/_types/_u_int32_t.h
ctxt.o: /usr/include/sys/_types/_u_int64_t.h
ctxt.o: /usr/include/sys/_types/_intptr_t.h
ctxt.o: /usr/include/sys/_types/_uintptr_t.h
ctxt.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
ctxt.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
ctxt.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
ctxt.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gksP.h
ctxt.o: ../../../.././include/ncarg/c.h /usr/include/sys/types.h
ctxt.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
ctxt.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
ctxt.o: /usr/include/libkern/_OSByteOrder.h
ctxt.o: /usr/include/libkern/i386/_OSByteOrder.h
ctxt.o: /usr/include/sys/_types/_u_char.h /usr/include/sys/_types/_u_short.h
ctxt.o: /usr/include/sys/_types/_u_int.h /usr/include/sys/_types/_caddr_t.h
ctxt.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_blkcnt_t.h
ctxt.o: /usr/include/sys/_types/_blksize_t.h /usr/include/sys/_types/_gid_t.h
ctxt.o: /usr/include/sys/_types/_in_addr_t.h
ctxt.o: /usr/include/sys/_types/_in_port_t.h /usr/include/sys/_types/_ino_t.h
ctxt.o: /usr/include/sys/_types/_ino64_t.h /usr/include/sys/_types/_key_t.h
ctxt.o: /usr/include/sys/_types/_mode_t.h /usr/include/sys/_types/_nlink_t.h
ctxt.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
ctxt.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/_types/_clock_t.h
ctxt.o: /usr/include/sys/_types/_time_t.h
ctxt.o: /usr/include/sys/_types/_useconds_t.h
ctxt.o: /usr/include/sys/_types/_suseconds_t.h
ctxt.o: /usr/include/sys/_types/_rsize_t.h /usr/include/sys/_types/_errno_t.h
ctxt.o: /usr/include/sys/_types/_fd_def.h
ctxt.o: /usr/include/sys/_types/_fd_setsize.h
ctxt.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
ctxt.o: /usr/include/sys/_types/_fd_zero.h
ctxt.o: /usr/include/sys/_types/_fd_isset.h
ctxt.o: /usr/include/sys/_types/_fd_copy.h
ctxt.o: /usr/include/sys/_pthread/_pthread_attr_t.h
ctxt.o: /usr/include/sys/_pthread/_pthread_cond_t.h
ctxt.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
ctxt.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
ctxt.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
ctxt.o: /usr/include/sys/_pthread/_pthread_once_t.h
ctxt.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
ctxt.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
ctxt.o: /usr/include/sys/_pthread/_pthread_t.h
ctxt.o: /usr/include/sys/_pthread/_pthread_key_t.h
ctxt.o: /usr/include/sys/_types/_fsblkcnt_t.h
ctxt.o: /usr/include/sys/_types/_fsfilcnt_t.h
ctxt.o: ../../../.././include/ncarg/gks.h gksc.h ctxt.h gks.h
ctxt_conv.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
ctxt_conv.o: /usr/include/sys/_symbol_aliasing.h
ctxt_conv.o: /usr/include/sys/_posix_availability.h
ctxt_conv.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
ctxt_conv.o: /usr/include/_types.h /usr/include/sys/_types.h
ctxt_conv.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
ctxt_conv.o: /usr/include/sys/_pthread/_pthread_types.h
ctxt_conv.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
ctxt_conv.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
ctxt_conv.o: /usr/include/sys/_types/_int16_t.h
ctxt_conv.o: /usr/include/sys/_types/_int32_t.h
ctxt_conv.o: /usr/include/sys/_types/_int64_t.h
ctxt_conv.o: /usr/include/sys/_types/_u_int8_t.h
ctxt_conv.o: /usr/include/sys/_types/_u_int16_t.h
ctxt_conv.o: /usr/include/sys/_types/_u_int32_t.h
ctxt_conv.o: /usr/include/sys/_types/_u_int64_t.h
ctxt_conv.o: /usr/include/sys/_types/_intptr_t.h
ctxt_conv.o: /usr/include/sys/_types/_uintptr_t.h
ctxt_conv.o: /usr/include/sys/_types/_size_t.h
ctxt_conv.o: /usr/include/sys/_types/_null.h /usr/include/sys/stdio.h
ctxt_conv.o: /usr/include/sys/_types/_off_t.h
ctxt_conv.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
ctxt_conv.o: /usr/include/secure/_common.h gksc.h /usr/include/sys/types.h
ctxt_conv.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
ctxt_conv.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
ctxt_conv.o: /usr/include/libkern/_OSByteOrder.h
ctxt_conv.o: /usr/include/libkern/i386/_OSByteOrder.h
ctxt_conv.o: /usr/include/sys/_types/_u_char.h
ctxt_conv.o: /usr/include/sys/_types/_u_short.h
ctxt_conv.o: /usr/include/sys/_types/_u_int.h
ctxt_conv.o: /usr/include/sys/_types/_caddr_t.h
ctxt_conv.o: /usr/include/sys/_types/_dev_t.h
ctxt_conv.o: /usr/include/sys/_types/_blkcnt_t.h
ctxt_conv.o: /usr/include/sys/_types/_blksize_t.h
ctxt_conv.o: /usr/include/sys/_types/_gid_t.h
ctxt_conv.o: /usr/include/sys/_types/_in_addr_t.h
ctxt_conv.o: /usr/include/sys/_types/_in_port_t.h
ctxt_conv.o: /usr/include/sys/_types/_ino_t.h
ctxt_conv.o: /usr/include/sys/_types/_ino64_t.h
ctxt_conv.o: /usr/include/sys/_types/_key_t.h
ctxt_conv.o: /usr/include/sys/_types/_mode_t.h
ctxt_conv.o: /usr/include/sys/_types/_nlink_t.h
ctxt_conv.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
ctxt_conv.o: /usr/include/sys/_types/_uid_t.h
ctxt_conv.o: /usr/include/sys/_types/_clock_t.h
ctxt_conv.o: /usr/include/sys/_types/_time_t.h
ctxt_conv.o: /usr/include/sys/_types/_useconds_t.h
ctxt_conv.o: /usr/include/sys/_types/_suseconds_t.h
ctxt_conv.o: /usr/include/sys/_types/_rsize_t.h
ctxt_conv.o: /usr/include/sys/_types/_errno_t.h
ctxt_conv.o: /usr/include/sys/_types/_fd_def.h
ctxt_conv.o: /usr/include/sys/_types/_fd_setsize.h
ctxt_conv.o: /usr/include/sys/_types/_fd_set.h
ctxt_conv.o: /usr/include/sys/_types/_fd_clr.h
ctxt_conv.o: /usr/include/sys/_types/_fd_zero.h
ctxt_conv.o: /usr/include/sys/_types/_fd_isset.h
ctxt_conv.o: /usr/include/sys/_types/_fd_copy.h
ctxt_conv.o: /usr/include/sys/_pthread/_pthread_attr_t.h
ctxt_conv.o: /usr/include/sys/_pthread/_pthread_cond_t.h
ctxt_conv.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
ctxt_conv.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
ctxt_conv.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
ctxt_conv.o: /usr/include/sys/_pthread/_pthread_once_t.h
ctxt_conv.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
ctxt_conv.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
ctxt_conv.o: /usr/include/sys/_pthread/_pthread_t.h
ctxt_conv.o: /usr/include/sys/_pthread/_pthread_key_t.h
ctxt_conv.o: /usr/include/sys/_types/_fsblkcnt_t.h
ctxt_conv.o: /usr/include/sys/_types/_fsfilcnt_t.h ctxt.h ctxt_device.h
xdevice.o: gks_device.h /usr/include/stddef.h /usr/include/_types.h
xdevice.o: /usr/include/sys/_types.h /usr/include/sys/cdefs.h
xdevice.o: /usr/include/sys/_symbol_aliasing.h
xdevice.o: /usr/include/sys/_posix_availability.h
xdevice.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
xdevice.o: /usr/include/sys/_pthread/_pthread_types.h
xdevice.o: /usr/include/sys/_types/_null.h
xdevice.o: /usr/include/sys/_types/_offsetof.h
xdevice.o: /usr/include/sys/_types/_ptrdiff_t.h /usr/include/machine/types.h
xdevice.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
xdevice.o: /usr/include/sys/_types/_int16_t.h
xdevice.o: /usr/include/sys/_types/_int32_t.h
xdevice.o: /usr/include/sys/_types/_int64_t.h
xdevice.o: /usr/include/sys/_types/_u_int8_t.h
xdevice.o: /usr/include/sys/_types/_u_int16_t.h
xdevice.o: /usr/include/sys/_types/_u_int32_t.h
xdevice.o: /usr/include/sys/_types/_u_int64_t.h
xdevice.o: /usr/include/sys/_types/_intptr_t.h
xdevice.o: /usr/include/sys/_types/_uintptr_t.h
xdevice.o: /usr/include/sys/_types/_rsize_t.h
xdevice.o: /usr/include/sys/_types/_size_t.h
xdevice.o: /usr/include/sys/_types/_wchar_t.h
xdevice.o: /usr/include/sys/_types/_wint_t.h /opt/X11/include/X11/Xlib.h
xdevice.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
xdevice.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
xdevice.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
xdevice.o: /usr/include/libkern/i386/_OSByteOrder.h
xdevice.o: /usr/include/sys/_types/_u_char.h
xdevice.o: /usr/include/sys/_types/_u_short.h
xdevice.o: /usr/include/sys/_types/_u_int.h
xdevice.o: /usr/include/sys/_types/_caddr_t.h
xdevice.o: /usr/include/sys/_types/_dev_t.h
xdevice.o: /usr/include/sys/_types/_blkcnt_t.h
xdevice.o: /usr/include/sys/_types/_blksize_t.h
xdevice.o: /usr/include/sys/_types/_gid_t.h
xdevice.o: /usr/include/sys/_types/_in_addr_t.h
xdevice.o: /usr/include/sys/_types/_in_port_t.h
xdevice.o: /usr/include/sys/_types/_ino_t.h
xdevice.o: /usr/include/sys/_types/_ino64_t.h
xdevice.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
xdevice.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
xdevice.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_off_t.h
xdevice.o: /usr/include/sys/_types/_uid_t.h
xdevice.o: /usr/include/sys/_types/_clock_t.h
xdevice.o: /usr/include/sys/_types/_ssize_t.h
xdevice.o: /usr/include/sys/_types/_time_t.h
xdevice.o: /usr/include/sys/_types/_useconds_t.h
xdevice.o: /usr/include/sys/_types/_suseconds_t.h
xdevice.o: /usr/include/sys/_types/_errno_t.h
xdevice.o: /usr/include/sys/_types/_fd_def.h
xdevice.o: /usr/include/sys/_types/_fd_setsize.h
xdevice.o: /usr/include/sys/_types/_fd_set.h
xdevice.o: /usr/include/sys/_types/_fd_clr.h
xdevice.o: /usr/include/sys/_types/_fd_zero.h
xdevice.o: /usr/include/sys/_types/_fd_isset.h
xdevice.o: /usr/include/sys/_types/_fd_copy.h
xdevice.o: /usr/include/sys/_pthread/_pthread_attr_t.h
xdevice.o: /usr/include/sys/_pthread/_pthread_cond_t.h
xdevice.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
xdevice.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
xdevice.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
xdevice.o: /usr/include/sys/_pthread/_pthread_once_t.h
xdevice.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
xdevice.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
xdevice.o: /usr/include/sys/_pthread/_pthread_t.h
xdevice.o: /usr/include/sys/_pthread/_pthread_key_t.h
xdevice.o: /usr/include/sys/_types/_fsblkcnt_t.h
xdevice.o: /usr/include/sys/_types/_fsfilcnt_t.h /opt/X11/include/X11/X.h
xdevice.o: /opt/X11/include/X11/Xfuncproto.h /opt/X11/include/X11/Xosdefs.h
xdevice.o: x_device.h gksc.h
font.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
font.o: /usr/include/sys/_symbol_aliasing.h
font.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
font.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
font.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
font.o: /usr/include/i386/_types.h /usr/include/sys/_pthread/_pthread_types.h
font.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
font.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
font.o: /usr/include/sys/_types/_int16_t.h /usr/include/sys/_types/_int32_t.h
font.o: /usr/include/sys/_types/_int64_t.h
font.o: /usr/include/sys/_types/_u_int8_t.h
font.o: /usr/include/sys/_types/_u_int16_t.h
font.o: /usr/include/sys/_types/_u_int32_t.h
font.o: /usr/include/sys/_types/_u_int64_t.h
font.o: /usr/include/sys/_types/_intptr_t.h
font.o: /usr/include/sys/_types/_uintptr_t.h
font.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
font.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
font.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
font.o: /usr/include/secure/_common.h /usr/include/stdlib.h
font.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
font.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
font.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
font.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
font.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
font.o: /usr/include/mach/i386/_structs.h
font.o: /usr/include/sys/_pthread/_pthread_attr_t.h
font.o: /usr/include/sys/_types/_sigaltstack.h
font.o: /usr/include/sys/_types/_ucontext.h
font.o: /usr/include/sys/_types/_sigset_t.h /usr/include/sys/_types/_uid_t.h
font.o: /usr/include/sys/resource.h /usr/include/stdint.h
font.o: /usr/include/_types/_uint8_t.h /usr/include/_types/_uint16_t.h
font.o: /usr/include/_types/_uint32_t.h /usr/include/_types/_uint64_t.h
font.o: /usr/include/_types/_intmax_t.h /usr/include/_types/_uintmax_t.h
font.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
font.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
font.o: /usr/include/libkern/_OSByteOrder.h
font.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
font.o: /usr/include/sys/_types/_ct_rune_t.h
font.o: /usr/include/sys/_types/_rune_t.h /usr/include/sys/_types/_wchar_t.h
font.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_mode_t.h
font.o: /usr/include/fcntl.h /usr/include/sys/fcntl.h
font.o: /usr/include/sys/_types/_o_sync.h /usr/include/sys/_types/_o_dsync.h
font.o: /usr/include/sys/_types/_seek_set.h /usr/include/sys/_types/_s_ifmt.h
font.o: /usr/include/sys/_types/_timespec.h
font.o: /usr/include/sys/_types/_filesec_t.h /usr/include/unistd.h
font.o: /usr/include/sys/unistd.h /usr/include/sys/_types/_posix_vdisable.h
font.o: /usr/include/sys/_types/_gid_t.h
font.o: /usr/include/sys/_types/_useconds_t.h /usr/include/sys/select.h
font.o: /usr/include/sys/_types/_fd_def.h /usr/include/sys/_types/_time_t.h
font.o: /usr/include/sys/_types/_suseconds_t.h
font.o: /usr/include/sys/_types/_fd_setsize.h
font.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
font.o: /usr/include/sys/_types/_fd_isset.h
font.o: /usr/include/sys/_types/_fd_zero.h /usr/include/sys/_types/_fd_copy.h
font.o: /usr/include/sys/_select.h /usr/include/sys/_types/_uuid_t.h
font.o: /usr/include/gethostuuid.h /usr/include/errno.h
font.o: /usr/include/sys/errno.h /usr/include/sys/_types/_errno_t.h
font.o: ../../../.././include/ncarg/c.h /usr/include/sys/types.h
font.o: /usr/include/sys/_types/_u_char.h /usr/include/sys/_types/_u_short.h
font.o: /usr/include/sys/_types/_u_int.h /usr/include/sys/_types/_caddr_t.h
font.o: /usr/include/sys/_types/_blkcnt_t.h
font.o: /usr/include/sys/_types/_blksize_t.h
font.o: /usr/include/sys/_types/_in_addr_t.h
font.o: /usr/include/sys/_types/_in_port_t.h /usr/include/sys/_types/_ino_t.h
font.o: /usr/include/sys/_types/_ino64_t.h /usr/include/sys/_types/_key_t.h
font.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_clock_t.h
font.o: /usr/include/sys/_types/_rsize_t.h
font.o: /usr/include/sys/_pthread/_pthread_cond_t.h
font.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
font.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
font.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
font.o: /usr/include/sys/_pthread/_pthread_once_t.h
font.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
font.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
font.o: /usr/include/sys/_pthread/_pthread_t.h
font.o: /usr/include/sys/_pthread/_pthread_key_t.h
font.o: /usr/include/sys/_types/_fsblkcnt_t.h
font.o: /usr/include/sys/_types/_fsfilcnt_t.h font.h common.h
font.o: /usr/include/math.h gksc.h
text.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
text.o: /usr/include/sys/_symbol_aliasing.h
text.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
text.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
text.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
text.o: /usr/include/i386/_types.h /usr/include/sys/_pthread/_pthread_types.h
text.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
text.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
text.o: /usr/include/sys/_types/_int16_t.h /usr/include/sys/_types/_int32_t.h
text.o: /usr/include/sys/_types/_int64_t.h
text.o: /usr/include/sys/_types/_u_int8_t.h
text.o: /usr/include/sys/_types/_u_int16_t.h
text.o: /usr/include/sys/_types/_u_int32_t.h
text.o: /usr/include/sys/_types/_u_int64_t.h
text.o: /usr/include/sys/_types/_intptr_t.h
text.o: /usr/include/sys/_types/_uintptr_t.h
text.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
text.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
text.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
text.o: /usr/include/secure/_common.h /usr/include/stdlib.h
text.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
text.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
text.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
text.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
text.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
text.o: /usr/include/mach/i386/_structs.h
text.o: /usr/include/sys/_pthread/_pthread_attr_t.h
text.o: /usr/include/sys/_types/_sigaltstack.h
text.o: /usr/include/sys/_types/_ucontext.h
text.o: /usr/include/sys/_types/_sigset_t.h /usr/include/sys/_types/_uid_t.h
text.o: /usr/include/sys/resource.h /usr/include/stdint.h
text.o: /usr/include/_types/_uint8_t.h /usr/include/_types/_uint16_t.h
text.o: /usr/include/_types/_uint32_t.h /usr/include/_types/_uint64_t.h
text.o: /usr/include/_types/_intmax_t.h /usr/include/_types/_uintmax_t.h
text.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
text.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
text.o: /usr/include/libkern/_OSByteOrder.h
text.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
text.o: /usr/include/sys/_types/_ct_rune_t.h
text.o: /usr/include/sys/_types/_rune_t.h /usr/include/sys/_types/_wchar_t.h
text.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_mode_t.h
text.o: /usr/include/string.h /usr/include/sys/_types/_rsize_t.h
text.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
text.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
text.o: /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
text.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
text.o: /usr/include/sys/_types/_caddr_t.h
text.o: /usr/include/sys/_types/_blkcnt_t.h
text.o: /usr/include/sys/_types/_blksize_t.h /usr/include/sys/_types/_gid_t.h
text.o: /usr/include/sys/_types/_in_addr_t.h
text.o: /usr/include/sys/_types/_in_port_t.h /usr/include/sys/_types/_ino_t.h
text.o: /usr/include/sys/_types/_ino64_t.h /usr/include/sys/_types/_key_t.h
text.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_clock_t.h
text.o: /usr/include/sys/_types/_time_t.h
text.o: /usr/include/sys/_types/_useconds_t.h
text.o: /usr/include/sys/_types/_suseconds_t.h
text.o: /usr/include/sys/_types/_fd_def.h
text.o: /usr/include/sys/_types/_fd_setsize.h
text.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
text.o: /usr/include/sys/_types/_fd_zero.h
text.o: /usr/include/sys/_types/_fd_isset.h
text.o: /usr/include/sys/_types/_fd_copy.h
text.o: /usr/include/sys/_pthread/_pthread_cond_t.h
text.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
text.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
text.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
text.o: /usr/include/sys/_pthread/_pthread_once_t.h
text.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
text.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
text.o: /usr/include/sys/_pthread/_pthread_t.h
text.o: /usr/include/sys/_pthread/_pthread_key_t.h
text.o: /usr/include/sys/_types/_fsblkcnt_t.h
text.o: /usr/include/sys/_types/_fsfilcnt_t.h font.h common.h
text.o: /usr/include/math.h gks.h gksc.h text.h
text.o: ../../../.././include/ncarg/c.h
x_conv.o: /opt/X11/include/X11/Xlib.h /usr/include/sys/types.h
x_conv.o: /usr/include/sys/appleapiopts.h /usr/include/sys/cdefs.h
x_conv.o: /usr/include/sys/_symbol_aliasing.h
x_conv.o: /usr/include/sys/_posix_availability.h /usr/include/machine/types.h
x_conv.o: /usr/include/i386/types.h /usr/include/i386/_types.h
x_conv.o: /usr/include/sys/_types/_int8_t.h
x_conv.o: /usr/include/sys/_types/_int16_t.h
x_conv.o: /usr/include/sys/_types/_int32_t.h
x_conv.o: /usr/include/sys/_types/_int64_t.h
x_conv.o: /usr/include/sys/_types/_u_int8_t.h
x_conv.o: /usr/include/sys/_types/_u_int16_t.h
x_conv.o: /usr/include/sys/_types/_u_int32_t.h
x_conv.o: /usr/include/sys/_types/_u_int64_t.h
x_conv.o: /usr/include/sys/_types/_intptr_t.h
x_conv.o: /usr/include/sys/_types/_uintptr_t.h /usr/include/sys/_types.h
x_conv.o: /usr/include/machine/_types.h
x_conv.o: /usr/include/sys/_pthread/_pthread_types.h
x_conv.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
x_conv.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
x_conv.o: /usr/include/libkern/i386/_OSByteOrder.h
x_conv.o: /usr/include/sys/_types/_u_char.h
x_conv.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
x_conv.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
x_conv.o: /usr/include/sys/_types/_blkcnt_t.h
x_conv.o: /usr/include/sys/_types/_blksize_t.h
x_conv.o: /usr/include/sys/_types/_gid_t.h
x_conv.o: /usr/include/sys/_types/_in_addr_t.h
x_conv.o: /usr/include/sys/_types/_in_port_t.h
x_conv.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
x_conv.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
x_conv.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
x_conv.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_off_t.h
x_conv.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/_types/_clock_t.h
x_conv.o: /usr/include/sys/_types/_size_t.h
x_conv.o: /usr/include/sys/_types/_ssize_t.h
x_conv.o: /usr/include/sys/_types/_time_t.h
x_conv.o: /usr/include/sys/_types/_useconds_t.h
x_conv.o: /usr/include/sys/_types/_suseconds_t.h
x_conv.o: /usr/include/sys/_types/_rsize_t.h
x_conv.o: /usr/include/sys/_types/_errno_t.h
x_conv.o: /usr/include/sys/_types/_fd_def.h
x_conv.o: /usr/include/sys/_types/_fd_setsize.h
x_conv.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
x_conv.o: /usr/include/sys/_types/_fd_zero.h
x_conv.o: /usr/include/sys/_types/_fd_isset.h
x_conv.o: /usr/include/sys/_types/_fd_copy.h
x_conv.o: /usr/include/sys/_pthread/_pthread_attr_t.h
x_conv.o: /usr/include/sys/_pthread/_pthread_cond_t.h
x_conv.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
x_conv.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
x_conv.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
x_conv.o: /usr/include/sys/_pthread/_pthread_once_t.h
x_conv.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
x_conv.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
x_conv.o: /usr/include/sys/_pthread/_pthread_t.h
x_conv.o: /usr/include/sys/_pthread/_pthread_key_t.h
x_conv.o: /usr/include/sys/_types/_fsblkcnt_t.h
x_conv.o: /usr/include/sys/_types/_fsfilcnt_t.h /opt/X11/include/X11/X.h
x_conv.o: /opt/X11/include/X11/Xfuncproto.h /opt/X11/include/X11/Xosdefs.h
x_conv.o: /usr/include/stddef.h /usr/include/_types.h
x_conv.o: /usr/include/sys/_types/_null.h /usr/include/sys/_types/_offsetof.h
x_conv.o: /usr/include/sys/_types/_ptrdiff_t.h
x_conv.o: /usr/include/sys/_types/_wchar_t.h
x_conv.o: /usr/include/sys/_types/_wint_t.h gksc.h xddi.h
x_conv.o: ../../../.././include/ncarg/gksP.h x.h gks.h common.h
x_conv.o: /usr/include/math.h /usr/include/Availability.h
x_conv.o: /usr/include/AvailabilityInternal.h transform.h x_device.h
xattribute.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
xattribute.o: /usr/include/sys/_symbol_aliasing.h
xattribute.o: /usr/include/sys/_posix_availability.h
xattribute.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
xattribute.o: /usr/include/_types.h /usr/include/sys/_types.h
xattribute.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
xattribute.o: /usr/include/sys/_pthread/_pthread_types.h
xattribute.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
xattribute.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
xattribute.o: /usr/include/sys/_types/_int16_t.h
xattribute.o: /usr/include/sys/_types/_int32_t.h
xattribute.o: /usr/include/sys/_types/_int64_t.h
xattribute.o: /usr/include/sys/_types/_u_int8_t.h
xattribute.o: /usr/include/sys/_types/_u_int16_t.h
xattribute.o: /usr/include/sys/_types/_u_int32_t.h
xattribute.o: /usr/include/sys/_types/_u_int64_t.h
xattribute.o: /usr/include/sys/_types/_intptr_t.h
xattribute.o: /usr/include/sys/_types/_uintptr_t.h
xattribute.o: /usr/include/sys/_types/_size_t.h
xattribute.o: /usr/include/sys/_types/_null.h /usr/include/sys/stdio.h
xattribute.o: /usr/include/sys/_types/_off_t.h
xattribute.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
xattribute.o: /usr/include/secure/_common.h /usr/include/stdlib.h
xattribute.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
xattribute.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
xattribute.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
xattribute.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
xattribute.o: /usr/include/i386/_mcontext.h
xattribute.o: /usr/include/mach/machine/_structs.h
xattribute.o: /usr/include/mach/i386/_structs.h
xattribute.o: /usr/include/sys/_pthread/_pthread_attr_t.h
xattribute.o: /usr/include/sys/_types/_sigaltstack.h
xattribute.o: /usr/include/sys/_types/_ucontext.h
xattribute.o: /usr/include/sys/_types/_sigset_t.h
xattribute.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
xattribute.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
xattribute.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
xattribute.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
xattribute.o: /usr/include/_types/_uintmax_t.h
xattribute.o: /usr/include/sys/_types/_timeval.h
xattribute.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
xattribute.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
xattribute.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
xattribute.o: /usr/include/sys/_types/_ct_rune_t.h
xattribute.o: /usr/include/sys/_types/_rune_t.h
xattribute.o: /usr/include/sys/_types/_wchar_t.h
xattribute.o: /usr/include/sys/_types/_dev_t.h
xattribute.o: /usr/include/sys/_types/_mode_t.h /opt/X11/include/X11/Xlib.h
xattribute.o: /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
xattribute.o: /usr/include/sys/_types/_u_short.h
xattribute.o: /usr/include/sys/_types/_u_int.h
xattribute.o: /usr/include/sys/_types/_caddr_t.h
xattribute.o: /usr/include/sys/_types/_blkcnt_t.h
xattribute.o: /usr/include/sys/_types/_blksize_t.h
xattribute.o: /usr/include/sys/_types/_gid_t.h
xattribute.o: /usr/include/sys/_types/_in_addr_t.h
xattribute.o: /usr/include/sys/_types/_in_port_t.h
xattribute.o: /usr/include/sys/_types/_ino_t.h
xattribute.o: /usr/include/sys/_types/_ino64_t.h
xattribute.o: /usr/include/sys/_types/_key_t.h
xattribute.o: /usr/include/sys/_types/_nlink_t.h
xattribute.o: /usr/include/sys/_types/_clock_t.h
xattribute.o: /usr/include/sys/_types/_time_t.h
xattribute.o: /usr/include/sys/_types/_useconds_t.h
xattribute.o: /usr/include/sys/_types/_suseconds_t.h
xattribute.o: /usr/include/sys/_types/_rsize_t.h
xattribute.o: /usr/include/sys/_types/_errno_t.h
xattribute.o: /usr/include/sys/_types/_fd_def.h
xattribute.o: /usr/include/sys/_types/_fd_setsize.h
xattribute.o: /usr/include/sys/_types/_fd_set.h
xattribute.o: /usr/include/sys/_types/_fd_clr.h
xattribute.o: /usr/include/sys/_types/_fd_zero.h
xattribute.o: /usr/include/sys/_types/_fd_isset.h
xattribute.o: /usr/include/sys/_types/_fd_copy.h
xattribute.o: /usr/include/sys/_pthread/_pthread_cond_t.h
xattribute.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
xattribute.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
xattribute.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
xattribute.o: /usr/include/sys/_pthread/_pthread_once_t.h
xattribute.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
xattribute.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
xattribute.o: /usr/include/sys/_pthread/_pthread_t.h
xattribute.o: /usr/include/sys/_pthread/_pthread_key_t.h
xattribute.o: /usr/include/sys/_types/_fsblkcnt_t.h
xattribute.o: /usr/include/sys/_types/_fsfilcnt_t.h /opt/X11/include/X11/X.h
xattribute.o: /opt/X11/include/X11/Xfuncproto.h
xattribute.o: /opt/X11/include/X11/Xosdefs.h /usr/include/stddef.h
xattribute.o: /usr/include/sys/_types/_offsetof.h
xattribute.o: /usr/include/sys/_types/_ptrdiff_t.h
xattribute.o: /usr/include/sys/_types/_wint_t.h /opt/X11/include/X11/Xutil.h
xattribute.o: /opt/X11/include/X11/keysym.h /opt/X11/include/X11/keysymdef.h
xattribute.o: common.h /usr/include/math.h gksc.h gks.h x.h x_device.h xddi.h
xattribute.o: ../../../.././include/ncarg/gksP.h transform.h xattribute.h
xattribute.o: text.h ../../../.././include/ncarg/c.h
xcontrol.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
xcontrol.o: /usr/include/sys/_symbol_aliasing.h
xcontrol.o: /usr/include/sys/_posix_availability.h
xcontrol.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
xcontrol.o: /usr/include/_types.h /usr/include/sys/_types.h
xcontrol.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
xcontrol.o: /usr/include/sys/_pthread/_pthread_types.h
xcontrol.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
xcontrol.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
xcontrol.o: /usr/include/sys/_types/_int16_t.h
xcontrol.o: /usr/include/sys/_types/_int32_t.h
xcontrol.o: /usr/include/sys/_types/_int64_t.h
xcontrol.o: /usr/include/sys/_types/_u_int8_t.h
xcontrol.o: /usr/include/sys/_types/_u_int16_t.h
xcontrol.o: /usr/include/sys/_types/_u_int32_t.h
xcontrol.o: /usr/include/sys/_types/_u_int64_t.h
xcontrol.o: /usr/include/sys/_types/_intptr_t.h
xcontrol.o: /usr/include/sys/_types/_uintptr_t.h
xcontrol.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
xcontrol.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
xcontrol.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
xcontrol.o: /usr/include/secure/_common.h /usr/include/stdlib.h
xcontrol.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
xcontrol.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
xcontrol.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
xcontrol.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
xcontrol.o: /usr/include/i386/_mcontext.h
xcontrol.o: /usr/include/mach/machine/_structs.h
xcontrol.o: /usr/include/mach/i386/_structs.h
xcontrol.o: /usr/include/sys/_pthread/_pthread_attr_t.h
xcontrol.o: /usr/include/sys/_types/_sigaltstack.h
xcontrol.o: /usr/include/sys/_types/_ucontext.h
xcontrol.o: /usr/include/sys/_types/_sigset_t.h
xcontrol.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
xcontrol.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
xcontrol.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
xcontrol.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
xcontrol.o: /usr/include/_types/_uintmax_t.h
xcontrol.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
xcontrol.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
xcontrol.o: /usr/include/libkern/_OSByteOrder.h
xcontrol.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
xcontrol.o: /usr/include/sys/_types/_ct_rune_t.h
xcontrol.o: /usr/include/sys/_types/_rune_t.h
xcontrol.o: /usr/include/sys/_types/_wchar_t.h
xcontrol.o: /usr/include/sys/_types/_dev_t.h
xcontrol.o: /usr/include/sys/_types/_mode_t.h /usr/include/string.h
xcontrol.o: /usr/include/sys/_types/_rsize_t.h
xcontrol.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
xcontrol.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
xcontrol.o: /usr/include/errno.h /usr/include/sys/errno.h
xcontrol.o: /opt/X11/include/X11/Xlib.h /usr/include/sys/types.h
xcontrol.o: /usr/include/sys/_types/_u_char.h
xcontrol.o: /usr/include/sys/_types/_u_short.h
xcontrol.o: /usr/include/sys/_types/_u_int.h
xcontrol.o: /usr/include/sys/_types/_caddr_t.h
xcontrol.o: /usr/include/sys/_types/_blkcnt_t.h
xcontrol.o: /usr/include/sys/_types/_blksize_t.h
xcontrol.o: /usr/include/sys/_types/_gid_t.h
xcontrol.o: /usr/include/sys/_types/_in_addr_t.h
xcontrol.o: /usr/include/sys/_types/_in_port_t.h
xcontrol.o: /usr/include/sys/_types/_ino_t.h
xcontrol.o: /usr/include/sys/_types/_ino64_t.h
xcontrol.o: /usr/include/sys/_types/_key_t.h
xcontrol.o: /usr/include/sys/_types/_nlink_t.h
xcontrol.o: /usr/include/sys/_types/_clock_t.h
xcontrol.o: /usr/include/sys/_types/_time_t.h
xcontrol.o: /usr/include/sys/_types/_useconds_t.h
xcontrol.o: /usr/include/sys/_types/_suseconds_t.h
xcontrol.o: /usr/include/sys/_types/_fd_def.h
xcontrol.o: /usr/include/sys/_types/_fd_setsize.h
xcontrol.o: /usr/include/sys/_types/_fd_set.h
xcontrol.o: /usr/include/sys/_types/_fd_clr.h
xcontrol.o: /usr/include/sys/_types/_fd_zero.h
xcontrol.o: /usr/include/sys/_types/_fd_isset.h
xcontrol.o: /usr/include/sys/_types/_fd_copy.h
xcontrol.o: /usr/include/sys/_pthread/_pthread_cond_t.h
xcontrol.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
xcontrol.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
xcontrol.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
xcontrol.o: /usr/include/sys/_pthread/_pthread_once_t.h
xcontrol.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
xcontrol.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
xcontrol.o: /usr/include/sys/_pthread/_pthread_t.h
xcontrol.o: /usr/include/sys/_pthread/_pthread_key_t.h
xcontrol.o: /usr/include/sys/_types/_fsblkcnt_t.h
xcontrol.o: /usr/include/sys/_types/_fsfilcnt_t.h /opt/X11/include/X11/X.h
xcontrol.o: /opt/X11/include/X11/Xfuncproto.h /opt/X11/include/X11/Xosdefs.h
xcontrol.o: /usr/include/stddef.h /usr/include/sys/_types/_offsetof.h
xcontrol.o: /usr/include/sys/_types/_ptrdiff_t.h
xcontrol.o: /usr/include/sys/_types/_wint_t.h /opt/X11/include/X11/Xutil.h
xcontrol.o: /opt/X11/include/X11/keysym.h /opt/X11/include/X11/keysymdef.h
xcontrol.o: /opt/X11/include/X11/Xatom.h ../../../.././include/ncarg/c.h
xcontrol.o: ../../../.././include/ncarg/gks.h gks_device.h common.h
xcontrol.o: /usr/include/math.h gksc.h gks.h
xcontrol.o: ../../../.././include/ncarg/gksP.h x.h x_device.h xddi.h
xcontrol.o: transform.h
xinquiry.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
xinquiry.o: /usr/include/sys/_symbol_aliasing.h
xinquiry.o: /usr/include/sys/_posix_availability.h
xinquiry.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
xinquiry.o: /usr/include/_types.h /usr/include/sys/_types.h
xinquiry.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
xinquiry.o: /usr/include/sys/_pthread/_pthread_types.h
xinquiry.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
xinquiry.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
xinquiry.o: /usr/include/sys/_types/_int16_t.h
xinquiry.o: /usr/include/sys/_types/_int32_t.h
xinquiry.o: /usr/include/sys/_types/_int64_t.h
xinquiry.o: /usr/include/sys/_types/_u_int8_t.h
xinquiry.o: /usr/include/sys/_types/_u_int16_t.h
xinquiry.o: /usr/include/sys/_types/_u_int32_t.h
xinquiry.o: /usr/include/sys/_types/_u_int64_t.h
xinquiry.o: /usr/include/sys/_types/_intptr_t.h
xinquiry.o: /usr/include/sys/_types/_uintptr_t.h
xinquiry.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
xinquiry.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
xinquiry.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
xinquiry.o: /usr/include/secure/_common.h /opt/X11/include/X11/Xlib.h
xinquiry.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
xinquiry.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
xinquiry.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
xinquiry.o: /usr/include/libkern/i386/_OSByteOrder.h
xinquiry.o: /usr/include/sys/_types/_u_char.h
xinquiry.o: /usr/include/sys/_types/_u_short.h
xinquiry.o: /usr/include/sys/_types/_u_int.h
xinquiry.o: /usr/include/sys/_types/_caddr_t.h
xinquiry.o: /usr/include/sys/_types/_dev_t.h
xinquiry.o: /usr/include/sys/_types/_blkcnt_t.h
xinquiry.o: /usr/include/sys/_types/_blksize_t.h
xinquiry.o: /usr/include/sys/_types/_gid_t.h
xinquiry.o: /usr/include/sys/_types/_in_addr_t.h
xinquiry.o: /usr/include/sys/_types/_in_port_t.h
xinquiry.o: /usr/include/sys/_types/_ino_t.h
xinquiry.o: /usr/include/sys/_types/_ino64_t.h
xinquiry.o: /usr/include/sys/_types/_key_t.h
xinquiry.o: /usr/include/sys/_types/_mode_t.h
xinquiry.o: /usr/include/sys/_types/_nlink_t.h
xinquiry.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
xinquiry.o: /usr/include/sys/_types/_uid_t.h
xinquiry.o: /usr/include/sys/_types/_clock_t.h
xinquiry.o: /usr/include/sys/_types/_time_t.h
xinquiry.o: /usr/include/sys/_types/_useconds_t.h
xinquiry.o: /usr/include/sys/_types/_suseconds_t.h
xinquiry.o: /usr/include/sys/_types/_rsize_t.h
xinquiry.o: /usr/include/sys/_types/_errno_t.h
xinquiry.o: /usr/include/sys/_types/_fd_def.h
xinquiry.o: /usr/include/sys/_types/_fd_setsize.h
xinquiry.o: /usr/include/sys/_types/_fd_set.h
xinquiry.o: /usr/include/sys/_types/_fd_clr.h
xinquiry.o: /usr/include/sys/_types/_fd_zero.h
xinquiry.o: /usr/include/sys/_types/_fd_isset.h
xinquiry.o: /usr/include/sys/_types/_fd_copy.h
xinquiry.o: /usr/include/sys/_pthread/_pthread_attr_t.h
xinquiry.o: /usr/include/sys/_pthread/_pthread_cond_t.h
xinquiry.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
xinquiry.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
xinquiry.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
xinquiry.o: /usr/include/sys/_pthread/_pthread_once_t.h
xinquiry.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
xinquiry.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
xinquiry.o: /usr/include/sys/_pthread/_pthread_t.h
xinquiry.o: /usr/include/sys/_pthread/_pthread_key_t.h
xinquiry.o: /usr/include/sys/_types/_fsblkcnt_t.h
xinquiry.o: /usr/include/sys/_types/_fsfilcnt_t.h /opt/X11/include/X11/X.h
xinquiry.o: /opt/X11/include/X11/Xfuncproto.h /opt/X11/include/X11/Xosdefs.h
xinquiry.o: /usr/include/stddef.h /usr/include/sys/_types/_offsetof.h
xinquiry.o: /usr/include/sys/_types/_ptrdiff_t.h
xinquiry.o: /usr/include/sys/_types/_wchar_t.h
xinquiry.o: /usr/include/sys/_types/_wint_t.h /opt/X11/include/X11/Xutil.h
xinquiry.o: /opt/X11/include/X11/keysym.h /opt/X11/include/X11/keysymdef.h
xinquiry.o: common.h /usr/include/math.h gksc.h gks.h x.h x_device.h xddi.h
xinquiry.o: ../../../.././include/ncarg/gksP.h transform.h
xoutput.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
xoutput.o: /usr/include/sys/_symbol_aliasing.h
xoutput.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
xoutput.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
xoutput.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
xoutput.o: /usr/include/i386/_types.h
xoutput.o: /usr/include/sys/_pthread/_pthread_types.h
xoutput.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
xoutput.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
xoutput.o: /usr/include/sys/_types/_int16_t.h
xoutput.o: /usr/include/sys/_types/_int32_t.h
xoutput.o: /usr/include/sys/_types/_int64_t.h
xoutput.o: /usr/include/sys/_types/_u_int8_t.h
xoutput.o: /usr/include/sys/_types/_u_int16_t.h
xoutput.o: /usr/include/sys/_types/_u_int32_t.h
xoutput.o: /usr/include/sys/_types/_u_int64_t.h
xoutput.o: /usr/include/sys/_types/_intptr_t.h
xoutput.o: /usr/include/sys/_types/_uintptr_t.h
xoutput.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
xoutput.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
xoutput.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
xoutput.o: /usr/include/secure/_common.h /usr/include/stdlib.h
xoutput.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
xoutput.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
xoutput.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
xoutput.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
xoutput.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
xoutput.o: /usr/include/mach/i386/_structs.h
xoutput.o: /usr/include/sys/_pthread/_pthread_attr_t.h
xoutput.o: /usr/include/sys/_types/_sigaltstack.h
xoutput.o: /usr/include/sys/_types/_ucontext.h
xoutput.o: /usr/include/sys/_types/_sigset_t.h
xoutput.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
xoutput.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
xoutput.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
xoutput.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
xoutput.o: /usr/include/_types/_uintmax_t.h
xoutput.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
xoutput.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
xoutput.o: /usr/include/libkern/_OSByteOrder.h
xoutput.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
xoutput.o: /usr/include/sys/_types/_ct_rune_t.h
xoutput.o: /usr/include/sys/_types/_rune_t.h
xoutput.o: /usr/include/sys/_types/_wchar_t.h
xoutput.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_mode_t.h
xoutput.o: /usr/include/errno.h /usr/include/sys/errno.h
xoutput.o: /usr/include/sys/_types/_errno_t.h /opt/X11/include/X11/Xlib.h
xoutput.o: /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
xoutput.o: /usr/include/sys/_types/_u_short.h
xoutput.o: /usr/include/sys/_types/_u_int.h
xoutput.o: /usr/include/sys/_types/_caddr_t.h
xoutput.o: /usr/include/sys/_types/_blkcnt_t.h
xoutput.o: /usr/include/sys/_types/_blksize_t.h
xoutput.o: /usr/include/sys/_types/_gid_t.h
xoutput.o: /usr/include/sys/_types/_in_addr_t.h
xoutput.o: /usr/include/sys/_types/_in_port_t.h
xoutput.o: /usr/include/sys/_types/_ino_t.h
xoutput.o: /usr/include/sys/_types/_ino64_t.h
xoutput.o: /usr/include/sys/_types/_key_t.h
xoutput.o: /usr/include/sys/_types/_nlink_t.h
xoutput.o: /usr/include/sys/_types/_clock_t.h
xoutput.o: /usr/include/sys/_types/_time_t.h
xoutput.o: /usr/include/sys/_types/_useconds_t.h
xoutput.o: /usr/include/sys/_types/_suseconds_t.h
xoutput.o: /usr/include/sys/_types/_rsize_t.h
xoutput.o: /usr/include/sys/_types/_fd_def.h
xoutput.o: /usr/include/sys/_types/_fd_setsize.h
xoutput.o: /usr/include/sys/_types/_fd_set.h
xoutput.o: /usr/include/sys/_types/_fd_clr.h
xoutput.o: /usr/include/sys/_types/_fd_zero.h
xoutput.o: /usr/include/sys/_types/_fd_isset.h
xoutput.o: /usr/include/sys/_types/_fd_copy.h
xoutput.o: /usr/include/sys/_pthread/_pthread_cond_t.h
xoutput.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
xoutput.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
xoutput.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
xoutput.o: /usr/include/sys/_pthread/_pthread_once_t.h
xoutput.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
xoutput.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
xoutput.o: /usr/include/sys/_pthread/_pthread_t.h
xoutput.o: /usr/include/sys/_pthread/_pthread_key_t.h
xoutput.o: /usr/include/sys/_types/_fsblkcnt_t.h
xoutput.o: /usr/include/sys/_types/_fsfilcnt_t.h /opt/X11/include/X11/X.h
xoutput.o: /opt/X11/include/X11/Xfuncproto.h /opt/X11/include/X11/Xosdefs.h
xoutput.o: /usr/include/stddef.h /usr/include/sys/_types/_offsetof.h
xoutput.o: /usr/include/sys/_types/_ptrdiff_t.h
xoutput.o: /usr/include/sys/_types/_wint_t.h /opt/X11/include/X11/Xutil.h
xoutput.o: /opt/X11/include/X11/keysym.h /opt/X11/include/X11/keysymdef.h
xoutput.o: ../../../.././include/ncarg/c.h common.h /usr/include/math.h
xoutput.o: gksc.h gks.h x.h x_device.h xddi.h
xoutput.o: ../../../.././include/ncarg/gksP.h transform.h text.h
psdevice.o: gks_device.h /usr/include/stddef.h /usr/include/_types.h
psdevice.o: /usr/include/sys/_types.h /usr/include/sys/cdefs.h
psdevice.o: /usr/include/sys/_symbol_aliasing.h
psdevice.o: /usr/include/sys/_posix_availability.h
psdevice.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
psdevice.o: /usr/include/sys/_pthread/_pthread_types.h
psdevice.o: /usr/include/sys/_types/_null.h
psdevice.o: /usr/include/sys/_types/_offsetof.h
psdevice.o: /usr/include/sys/_types/_ptrdiff_t.h /usr/include/machine/types.h
psdevice.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
psdevice.o: /usr/include/sys/_types/_int16_t.h
psdevice.o: /usr/include/sys/_types/_int32_t.h
psdevice.o: /usr/include/sys/_types/_int64_t.h
psdevice.o: /usr/include/sys/_types/_u_int8_t.h
psdevice.o: /usr/include/sys/_types/_u_int16_t.h
psdevice.o: /usr/include/sys/_types/_u_int32_t.h
psdevice.o: /usr/include/sys/_types/_u_int64_t.h
psdevice.o: /usr/include/sys/_types/_intptr_t.h
psdevice.o: /usr/include/sys/_types/_uintptr_t.h
psdevice.o: /usr/include/sys/_types/_rsize_t.h
psdevice.o: /usr/include/sys/_types/_size_t.h
psdevice.o: /usr/include/sys/_types/_wchar_t.h
psdevice.o: /usr/include/sys/_types/_wint_t.h ps.h ps_device.h gksc.h
psdevice.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
psdevice.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
psdevice.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
psdevice.o: /usr/include/libkern/i386/_OSByteOrder.h
psdevice.o: /usr/include/sys/_types/_u_char.h
psdevice.o: /usr/include/sys/_types/_u_short.h
psdevice.o: /usr/include/sys/_types/_u_int.h
psdevice.o: /usr/include/sys/_types/_caddr_t.h
psdevice.o: /usr/include/sys/_types/_dev_t.h
psdevice.o: /usr/include/sys/_types/_blkcnt_t.h
psdevice.o: /usr/include/sys/_types/_blksize_t.h
psdevice.o: /usr/include/sys/_types/_gid_t.h
psdevice.o: /usr/include/sys/_types/_in_addr_t.h
psdevice.o: /usr/include/sys/_types/_in_port_t.h
psdevice.o: /usr/include/sys/_types/_ino_t.h
psdevice.o: /usr/include/sys/_types/_ino64_t.h
psdevice.o: /usr/include/sys/_types/_key_t.h
psdevice.o: /usr/include/sys/_types/_mode_t.h
psdevice.o: /usr/include/sys/_types/_nlink_t.h
psdevice.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
psdevice.o: /usr/include/sys/_types/_off_t.h /usr/include/sys/_types/_uid_t.h
psdevice.o: /usr/include/sys/_types/_clock_t.h
psdevice.o: /usr/include/sys/_types/_ssize_t.h
psdevice.o: /usr/include/sys/_types/_time_t.h
psdevice.o: /usr/include/sys/_types/_useconds_t.h
psdevice.o: /usr/include/sys/_types/_suseconds_t.h
psdevice.o: /usr/include/sys/_types/_errno_t.h
psdevice.o: /usr/include/sys/_types/_fd_def.h
psdevice.o: /usr/include/sys/_types/_fd_setsize.h
psdevice.o: /usr/include/sys/_types/_fd_set.h
psdevice.o: /usr/include/sys/_types/_fd_clr.h
psdevice.o: /usr/include/sys/_types/_fd_zero.h
psdevice.o: /usr/include/sys/_types/_fd_isset.h
psdevice.o: /usr/include/sys/_types/_fd_copy.h
psdevice.o: /usr/include/sys/_pthread/_pthread_attr_t.h
psdevice.o: /usr/include/sys/_pthread/_pthread_cond_t.h
psdevice.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
psdevice.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
psdevice.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
psdevice.o: /usr/include/sys/_pthread/_pthread_once_t.h
psdevice.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
psdevice.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
psdevice.o: /usr/include/sys/_pthread/_pthread_t.h
psdevice.o: /usr/include/sys/_pthread/_pthread_key_t.h
psdevice.o: /usr/include/sys/_types/_fsblkcnt_t.h
psdevice.o: /usr/include/sys/_types/_fsfilcnt_t.h
ps.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
ps.o: /usr/include/sys/_symbol_aliasing.h
ps.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
ps.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
ps.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
ps.o: /usr/include/i386/_types.h /usr/include/sys/_pthread/_pthread_types.h
ps.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
ps.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
ps.o: /usr/include/sys/_types/_int16_t.h /usr/include/sys/_types/_int32_t.h
ps.o: /usr/include/sys/_types/_int64_t.h /usr/include/sys/_types/_u_int8_t.h
ps.o: /usr/include/sys/_types/_u_int16_t.h
ps.o: /usr/include/sys/_types/_u_int32_t.h
ps.o: /usr/include/sys/_types/_u_int64_t.h
ps.o: /usr/include/sys/_types/_intptr_t.h
ps.o: /usr/include/sys/_types/_uintptr_t.h /usr/include/sys/_types/_size_t.h
ps.o: /usr/include/sys/_types/_null.h /usr/include/sys/stdio.h
ps.o: /usr/include/sys/_types/_off_t.h /usr/include/sys/_types/_ssize_t.h
ps.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
ps.o: /usr/include/stddef.h /usr/include/sys/_types/_offsetof.h
ps.o: /usr/include/sys/_types/_ptrdiff_t.h /usr/include/sys/_types/_rsize_t.h
ps.o: /usr/include/sys/_types/_wchar_t.h /usr/include/sys/_types/_wint_t.h
ps.o: /usr/include/string.h /usr/include/sys/_types/_errno_t.h
ps.o: /usr/include/strings.h /usr/include/secure/_strings.h
ps.o: /usr/include/secure/_string.h /usr/include/math.h /usr/include/stdlib.h
ps.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
ps.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
ps.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
ps.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
ps.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
ps.o: /usr/include/mach/i386/_structs.h
ps.o: /usr/include/sys/_pthread/_pthread_attr_t.h
ps.o: /usr/include/sys/_types/_sigaltstack.h
ps.o: /usr/include/sys/_types/_ucontext.h /usr/include/sys/_types/_sigset_t.h
ps.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
ps.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
ps.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
ps.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
ps.o: /usr/include/_types/_uintmax_t.h /usr/include/sys/_types/_timeval.h
ps.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
ps.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
ps.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
ps.o: /usr/include/sys/_types/_ct_rune_t.h /usr/include/sys/_types/_rune_t.h
ps.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_mode_t.h
ps.o: /usr/include/errno.h /usr/include/sys/errno.h
ps.o: ../../../.././include/ncarg/gksP.h ../../../.././include/ncarg/c.h
ps.o: /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
ps.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
ps.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_blkcnt_t.h
ps.o: /usr/include/sys/_types/_blksize_t.h /usr/include/sys/_types/_gid_t.h
ps.o: /usr/include/sys/_types/_in_addr_t.h
ps.o: /usr/include/sys/_types/_in_port_t.h /usr/include/sys/_types/_ino_t.h
ps.o: /usr/include/sys/_types/_ino64_t.h /usr/include/sys/_types/_key_t.h
ps.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_clock_t.h
ps.o: /usr/include/sys/_types/_time_t.h /usr/include/sys/_types/_useconds_t.h
ps.o: /usr/include/sys/_types/_suseconds_t.h
ps.o: /usr/include/sys/_types/_fd_def.h /usr/include/sys/_types/_fd_setsize.h
ps.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
ps.o: /usr/include/sys/_types/_fd_zero.h /usr/include/sys/_types/_fd_isset.h
ps.o: /usr/include/sys/_types/_fd_copy.h
ps.o: /usr/include/sys/_pthread/_pthread_cond_t.h
ps.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
ps.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
ps.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
ps.o: /usr/include/sys/_pthread/_pthread_once_t.h
ps.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
ps.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
ps.o: /usr/include/sys/_pthread/_pthread_t.h
ps.o: /usr/include/sys/_pthread/_pthread_key_t.h
ps.o: /usr/include/sys/_types/_fsblkcnt_t.h
ps.o: /usr/include/sys/_types/_fsfilcnt_t.h ../../../.././include/ncarg/gks.h
ps.o: gksc.h gks.h common.h ps_device.h psddi.h ps.h transform.h argb.h
ps_conv.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
ps_conv.o: /usr/include/sys/_symbol_aliasing.h
ps_conv.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
ps_conv.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
ps_conv.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
ps_conv.o: /usr/include/i386/_types.h
ps_conv.o: /usr/include/sys/_pthread/_pthread_types.h
ps_conv.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
ps_conv.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
ps_conv.o: /usr/include/sys/_types/_int16_t.h
ps_conv.o: /usr/include/sys/_types/_int32_t.h
ps_conv.o: /usr/include/sys/_types/_int64_t.h
ps_conv.o: /usr/include/sys/_types/_u_int8_t.h
ps_conv.o: /usr/include/sys/_types/_u_int16_t.h
ps_conv.o: /usr/include/sys/_types/_u_int32_t.h
ps_conv.o: /usr/include/sys/_types/_u_int64_t.h
ps_conv.o: /usr/include/sys/_types/_intptr_t.h
ps_conv.o: /usr/include/sys/_types/_uintptr_t.h
ps_conv.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
ps_conv.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
ps_conv.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
ps_conv.o: /usr/include/secure/_common.h gksc.h /usr/include/sys/types.h
ps_conv.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
ps_conv.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
ps_conv.o: /usr/include/libkern/_OSByteOrder.h
ps_conv.o: /usr/include/libkern/i386/_OSByteOrder.h
ps_conv.o: /usr/include/sys/_types/_u_char.h
ps_conv.o: /usr/include/sys/_types/_u_short.h
ps_conv.o: /usr/include/sys/_types/_u_int.h
ps_conv.o: /usr/include/sys/_types/_caddr_t.h
ps_conv.o: /usr/include/sys/_types/_dev_t.h
ps_conv.o: /usr/include/sys/_types/_blkcnt_t.h
ps_conv.o: /usr/include/sys/_types/_blksize_t.h
ps_conv.o: /usr/include/sys/_types/_gid_t.h
ps_conv.o: /usr/include/sys/_types/_in_addr_t.h
ps_conv.o: /usr/include/sys/_types/_in_port_t.h
ps_conv.o: /usr/include/sys/_types/_ino_t.h
ps_conv.o: /usr/include/sys/_types/_ino64_t.h
ps_conv.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
ps_conv.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
ps_conv.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
ps_conv.o: /usr/include/sys/_types/_clock_t.h
ps_conv.o: /usr/include/sys/_types/_time_t.h
ps_conv.o: /usr/include/sys/_types/_useconds_t.h
ps_conv.o: /usr/include/sys/_types/_suseconds_t.h
ps_conv.o: /usr/include/sys/_types/_rsize_t.h
ps_conv.o: /usr/include/sys/_types/_errno_t.h
ps_conv.o: /usr/include/sys/_types/_fd_def.h
ps_conv.o: /usr/include/sys/_types/_fd_setsize.h
ps_conv.o: /usr/include/sys/_types/_fd_set.h
ps_conv.o: /usr/include/sys/_types/_fd_clr.h
ps_conv.o: /usr/include/sys/_types/_fd_zero.h
ps_conv.o: /usr/include/sys/_types/_fd_isset.h
ps_conv.o: /usr/include/sys/_types/_fd_copy.h
ps_conv.o: /usr/include/sys/_pthread/_pthread_attr_t.h
ps_conv.o: /usr/include/sys/_pthread/_pthread_cond_t.h
ps_conv.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
ps_conv.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
ps_conv.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
ps_conv.o: /usr/include/sys/_pthread/_pthread_once_t.h
ps_conv.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
ps_conv.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
ps_conv.o: /usr/include/sys/_pthread/_pthread_t.h
ps_conv.o: /usr/include/sys/_pthread/_pthread_key_t.h
ps_conv.o: /usr/include/sys/_types/_fsblkcnt_t.h
ps_conv.o: /usr/include/sys/_types/_fsfilcnt_t.h ps.h psddi.h gks.h common.h
ps_conv.o: /usr/include/math.h transform.h ps_device.h
psfill.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
psfill.o: /usr/include/sys/_symbol_aliasing.h
psfill.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
psfill.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
psfill.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
psfill.o: /usr/include/i386/_types.h
psfill.o: /usr/include/sys/_pthread/_pthread_types.h
psfill.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
psfill.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
psfill.o: /usr/include/sys/_types/_int16_t.h
psfill.o: /usr/include/sys/_types/_int32_t.h
psfill.o: /usr/include/sys/_types/_int64_t.h
psfill.o: /usr/include/sys/_types/_u_int8_t.h
psfill.o: /usr/include/sys/_types/_u_int16_t.h
psfill.o: /usr/include/sys/_types/_u_int32_t.h
psfill.o: /usr/include/sys/_types/_u_int64_t.h
psfill.o: /usr/include/sys/_types/_intptr_t.h
psfill.o: /usr/include/sys/_types/_uintptr_t.h
psfill.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
psfill.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
psfill.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
psfill.o: /usr/include/secure/_common.h /usr/include/stdlib.h
psfill.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
psfill.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
psfill.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
psfill.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
psfill.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
psfill.o: /usr/include/mach/i386/_structs.h
psfill.o: /usr/include/sys/_pthread/_pthread_attr_t.h
psfill.o: /usr/include/sys/_types/_sigaltstack.h
psfill.o: /usr/include/sys/_types/_ucontext.h
psfill.o: /usr/include/sys/_types/_sigset_t.h
psfill.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
psfill.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
psfill.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
psfill.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
psfill.o: /usr/include/_types/_uintmax_t.h /usr/include/sys/_types/_timeval.h
psfill.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
psfill.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
psfill.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
psfill.o: /usr/include/sys/_types/_ct_rune_t.h
psfill.o: /usr/include/sys/_types/_rune_t.h
psfill.o: /usr/include/sys/_types/_wchar_t.h /usr/include/sys/_types/_dev_t.h
psfill.o: /usr/include/sys/_types/_mode_t.h /usr/include/string.h
psfill.o: /usr/include/sys/_types/_rsize_t.h
psfill.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
psfill.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
psfill.o: /usr/include/errno.h /usr/include/sys/errno.h /usr/include/math.h
psfill.o: gksc.h /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
psfill.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
psfill.o: /usr/include/sys/_types/_caddr_t.h
psfill.o: /usr/include/sys/_types/_blkcnt_t.h
psfill.o: /usr/include/sys/_types/_blksize_t.h
psfill.o: /usr/include/sys/_types/_gid_t.h
psfill.o: /usr/include/sys/_types/_in_addr_t.h
psfill.o: /usr/include/sys/_types/_in_port_t.h
psfill.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
psfill.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_nlink_t.h
psfill.o: /usr/include/sys/_types/_clock_t.h
psfill.o: /usr/include/sys/_types/_time_t.h
psfill.o: /usr/include/sys/_types/_useconds_t.h
psfill.o: /usr/include/sys/_types/_suseconds_t.h
psfill.o: /usr/include/sys/_types/_fd_def.h
psfill.o: /usr/include/sys/_types/_fd_setsize.h
psfill.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
psfill.o: /usr/include/sys/_types/_fd_zero.h
psfill.o: /usr/include/sys/_types/_fd_isset.h
psfill.o: /usr/include/sys/_types/_fd_copy.h
psfill.o: /usr/include/sys/_pthread/_pthread_cond_t.h
psfill.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
psfill.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
psfill.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
psfill.o: /usr/include/sys/_pthread/_pthread_once_t.h
psfill.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
psfill.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
psfill.o: /usr/include/sys/_pthread/_pthread_t.h
psfill.o: /usr/include/sys/_pthread/_pthread_key_t.h
psfill.o: /usr/include/sys/_types/_fsblkcnt_t.h
psfill.o: /usr/include/sys/_types/_fsfilcnt_t.h gks.h common.h ps.h psddi.h
psfill.o: transform.h ps_device.h
pdf.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
pdf.o: /usr/include/sys/_symbol_aliasing.h
pdf.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
pdf.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
pdf.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
pdf.o: /usr/include/i386/_types.h /usr/include/sys/_pthread/_pthread_types.h
pdf.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
pdf.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
pdf.o: /usr/include/sys/_types/_int16_t.h /usr/include/sys/_types/_int32_t.h
pdf.o: /usr/include/sys/_types/_int64_t.h /usr/include/sys/_types/_u_int8_t.h
pdf.o: /usr/include/sys/_types/_u_int16_t.h
pdf.o: /usr/include/sys/_types/_u_int32_t.h
pdf.o: /usr/include/sys/_types/_u_int64_t.h
pdf.o: /usr/include/sys/_types/_intptr_t.h
pdf.o: /usr/include/sys/_types/_uintptr_t.h /usr/include/sys/_types/_size_t.h
pdf.o: /usr/include/sys/_types/_null.h /usr/include/sys/stdio.h
pdf.o: /usr/include/sys/_types/_off_t.h /usr/include/sys/_types/_ssize_t.h
pdf.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
pdf.o: /usr/include/stddef.h /usr/include/sys/_types/_offsetof.h
pdf.o: /usr/include/sys/_types/_ptrdiff_t.h
pdf.o: /usr/include/sys/_types/_rsize_t.h /usr/include/sys/_types/_wchar_t.h
pdf.o: /usr/include/sys/_types/_wint_t.h /usr/include/string.h
pdf.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
pdf.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
pdf.o: /usr/include/math.h /usr/include/stdlib.h /usr/include/sys/wait.h
pdf.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_id_t.h
pdf.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
pdf.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
pdf.o: /usr/include/machine/_mcontext.h /usr/include/i386/_mcontext.h
pdf.o: /usr/include/mach/machine/_structs.h /usr/include/mach/i386/_structs.h
pdf.o: /usr/include/sys/_pthread/_pthread_attr_t.h
pdf.o: /usr/include/sys/_types/_sigaltstack.h
pdf.o: /usr/include/sys/_types/_ucontext.h
pdf.o: /usr/include/sys/_types/_sigset_t.h /usr/include/sys/_types/_uid_t.h
pdf.o: /usr/include/sys/resource.h /usr/include/stdint.h
pdf.o: /usr/include/_types/_uint8_t.h /usr/include/_types/_uint16_t.h
pdf.o: /usr/include/_types/_uint32_t.h /usr/include/_types/_uint64_t.h
pdf.o: /usr/include/_types/_intmax_t.h /usr/include/_types/_uintmax_t.h
pdf.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
pdf.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
pdf.o: /usr/include/libkern/_OSByteOrder.h
pdf.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
pdf.o: /usr/include/sys/_types/_ct_rune_t.h /usr/include/sys/_types/_rune_t.h
pdf.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_mode_t.h
pdf.o: /usr/include/errno.h /usr/include/sys/errno.h
pdf.o: ../../../.././include/ncarg/gksP.h ../../../.././include/ncarg/c.h
pdf.o: /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
pdf.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
pdf.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_blkcnt_t.h
pdf.o: /usr/include/sys/_types/_blksize_t.h /usr/include/sys/_types/_gid_t.h
pdf.o: /usr/include/sys/_types/_in_addr_t.h
pdf.o: /usr/include/sys/_types/_in_port_t.h /usr/include/sys/_types/_ino_t.h
pdf.o: /usr/include/sys/_types/_ino64_t.h /usr/include/sys/_types/_key_t.h
pdf.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_clock_t.h
pdf.o: /usr/include/sys/_types/_time_t.h
pdf.o: /usr/include/sys/_types/_useconds_t.h
pdf.o: /usr/include/sys/_types/_suseconds_t.h
pdf.o: /usr/include/sys/_types/_fd_def.h
pdf.o: /usr/include/sys/_types/_fd_setsize.h
pdf.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
pdf.o: /usr/include/sys/_types/_fd_zero.h /usr/include/sys/_types/_fd_isset.h
pdf.o: /usr/include/sys/_types/_fd_copy.h
pdf.o: /usr/include/sys/_pthread/_pthread_cond_t.h
pdf.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
pdf.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
pdf.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
pdf.o: /usr/include/sys/_pthread/_pthread_once_t.h
pdf.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
pdf.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
pdf.o: /usr/include/sys/_pthread/_pthread_t.h
pdf.o: /usr/include/sys/_pthread/_pthread_key_t.h
pdf.o: /usr/include/sys/_types/_fsblkcnt_t.h
pdf.o: /usr/include/sys/_types/_fsfilcnt_t.h
pdf.o: ../../../.././include/ncarg/gks.h gksc.h gks.h common.h pdf_device.h
pdf.o: pdfddi.h pdf.h transform.h argb.h
pdfdevice.o: gks_device.h /usr/include/stddef.h /usr/include/_types.h
pdfdevice.o: /usr/include/sys/_types.h /usr/include/sys/cdefs.h
pdfdevice.o: /usr/include/sys/_symbol_aliasing.h
pdfdevice.o: /usr/include/sys/_posix_availability.h
pdfdevice.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
pdfdevice.o: /usr/include/sys/_pthread/_pthread_types.h
pdfdevice.o: /usr/include/sys/_types/_null.h
pdfdevice.o: /usr/include/sys/_types/_offsetof.h
pdfdevice.o: /usr/include/sys/_types/_ptrdiff_t.h
pdfdevice.o: /usr/include/machine/types.h /usr/include/i386/types.h
pdfdevice.o: /usr/include/sys/_types/_int8_t.h
pdfdevice.o: /usr/include/sys/_types/_int16_t.h
pdfdevice.o: /usr/include/sys/_types/_int32_t.h
pdfdevice.o: /usr/include/sys/_types/_int64_t.h
pdfdevice.o: /usr/include/sys/_types/_u_int8_t.h
pdfdevice.o: /usr/include/sys/_types/_u_int16_t.h
pdfdevice.o: /usr/include/sys/_types/_u_int32_t.h
pdfdevice.o: /usr/include/sys/_types/_u_int64_t.h
pdfdevice.o: /usr/include/sys/_types/_intptr_t.h
pdfdevice.o: /usr/include/sys/_types/_uintptr_t.h
pdfdevice.o: /usr/include/sys/_types/_rsize_t.h
pdfdevice.o: /usr/include/sys/_types/_size_t.h
pdfdevice.o: /usr/include/sys/_types/_wchar_t.h
pdfdevice.o: /usr/include/sys/_types/_wint_t.h pdf.h /usr/include/stdio.h
pdfdevice.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
pdfdevice.o: /usr/include/sys/_types/_va_list.h /usr/include/sys/stdio.h
pdfdevice.o: /usr/include/sys/_types/_off_t.h
pdfdevice.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
pdfdevice.o: /usr/include/secure/_common.h pdf_device.h gksc.h
pdfdevice.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
pdfdevice.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
pdfdevice.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
pdfdevice.o: /usr/include/libkern/i386/_OSByteOrder.h
pdfdevice.o: /usr/include/sys/_types/_u_char.h
pdfdevice.o: /usr/include/sys/_types/_u_short.h
pdfdevice.o: /usr/include/sys/_types/_u_int.h
pdfdevice.o: /usr/include/sys/_types/_caddr_t.h
pdfdevice.o: /usr/include/sys/_types/_dev_t.h
pdfdevice.o: /usr/include/sys/_types/_blkcnt_t.h
pdfdevice.o: /usr/include/sys/_types/_blksize_t.h
pdfdevice.o: /usr/include/sys/_types/_gid_t.h
pdfdevice.o: /usr/include/sys/_types/_in_addr_t.h
pdfdevice.o: /usr/include/sys/_types/_in_port_t.h
pdfdevice.o: /usr/include/sys/_types/_ino_t.h
pdfdevice.o: /usr/include/sys/_types/_ino64_t.h
pdfdevice.o: /usr/include/sys/_types/_key_t.h
pdfdevice.o: /usr/include/sys/_types/_mode_t.h
pdfdevice.o: /usr/include/sys/_types/_nlink_t.h
pdfdevice.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
pdfdevice.o: /usr/include/sys/_types/_uid_t.h
pdfdevice.o: /usr/include/sys/_types/_clock_t.h
pdfdevice.o: /usr/include/sys/_types/_time_t.h
pdfdevice.o: /usr/include/sys/_types/_useconds_t.h
pdfdevice.o: /usr/include/sys/_types/_suseconds_t.h
pdfdevice.o: /usr/include/sys/_types/_errno_t.h
pdfdevice.o: /usr/include/sys/_types/_fd_def.h
pdfdevice.o: /usr/include/sys/_types/_fd_setsize.h
pdfdevice.o: /usr/include/sys/_types/_fd_set.h
pdfdevice.o: /usr/include/sys/_types/_fd_clr.h
pdfdevice.o: /usr/include/sys/_types/_fd_zero.h
pdfdevice.o: /usr/include/sys/_types/_fd_isset.h
pdfdevice.o: /usr/include/sys/_types/_fd_copy.h
pdfdevice.o: /usr/include/sys/_pthread/_pthread_attr_t.h
pdfdevice.o: /usr/include/sys/_pthread/_pthread_cond_t.h
pdfdevice.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
pdfdevice.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
pdfdevice.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
pdfdevice.o: /usr/include/sys/_pthread/_pthread_once_t.h
pdfdevice.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
pdfdevice.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
pdfdevice.o: /usr/include/sys/_pthread/_pthread_t.h
pdfdevice.o: /usr/include/sys/_pthread/_pthread_key_t.h
pdfdevice.o: /usr/include/sys/_types/_fsblkcnt_t.h
pdfdevice.o: /usr/include/sys/_types/_fsfilcnt_t.h
pdf_conv.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
pdf_conv.o: /usr/include/sys/_symbol_aliasing.h
pdf_conv.o: /usr/include/sys/_posix_availability.h
pdf_conv.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
pdf_conv.o: /usr/include/_types.h /usr/include/sys/_types.h
pdf_conv.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
pdf_conv.o: /usr/include/sys/_pthread/_pthread_types.h
pdf_conv.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
pdf_conv.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
pdf_conv.o: /usr/include/sys/_types/_int16_t.h
pdf_conv.o: /usr/include/sys/_types/_int32_t.h
pdf_conv.o: /usr/include/sys/_types/_int64_t.h
pdf_conv.o: /usr/include/sys/_types/_u_int8_t.h
pdf_conv.o: /usr/include/sys/_types/_u_int16_t.h
pdf_conv.o: /usr/include/sys/_types/_u_int32_t.h
pdf_conv.o: /usr/include/sys/_types/_u_int64_t.h
pdf_conv.o: /usr/include/sys/_types/_intptr_t.h
pdf_conv.o: /usr/include/sys/_types/_uintptr_t.h
pdf_conv.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
pdf_conv.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
pdf_conv.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
pdf_conv.o: /usr/include/secure/_common.h gksc.h /usr/include/sys/types.h
pdf_conv.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
pdf_conv.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
pdf_conv.o: /usr/include/libkern/_OSByteOrder.h
pdf_conv.o: /usr/include/libkern/i386/_OSByteOrder.h
pdf_conv.o: /usr/include/sys/_types/_u_char.h
pdf_conv.o: /usr/include/sys/_types/_u_short.h
pdf_conv.o: /usr/include/sys/_types/_u_int.h
pdf_conv.o: /usr/include/sys/_types/_caddr_t.h
pdf_conv.o: /usr/include/sys/_types/_dev_t.h
pdf_conv.o: /usr/include/sys/_types/_blkcnt_t.h
pdf_conv.o: /usr/include/sys/_types/_blksize_t.h
pdf_conv.o: /usr/include/sys/_types/_gid_t.h
pdf_conv.o: /usr/include/sys/_types/_in_addr_t.h
pdf_conv.o: /usr/include/sys/_types/_in_port_t.h
pdf_conv.o: /usr/include/sys/_types/_ino_t.h
pdf_conv.o: /usr/include/sys/_types/_ino64_t.h
pdf_conv.o: /usr/include/sys/_types/_key_t.h
pdf_conv.o: /usr/include/sys/_types/_mode_t.h
pdf_conv.o: /usr/include/sys/_types/_nlink_t.h
pdf_conv.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
pdf_conv.o: /usr/include/sys/_types/_uid_t.h
pdf_conv.o: /usr/include/sys/_types/_clock_t.h
pdf_conv.o: /usr/include/sys/_types/_time_t.h
pdf_conv.o: /usr/include/sys/_types/_useconds_t.h
pdf_conv.o: /usr/include/sys/_types/_suseconds_t.h
pdf_conv.o: /usr/include/sys/_types/_rsize_t.h
pdf_conv.o: /usr/include/sys/_types/_errno_t.h
pdf_conv.o: /usr/include/sys/_types/_fd_def.h
pdf_conv.o: /usr/include/sys/_types/_fd_setsize.h
pdf_conv.o: /usr/include/sys/_types/_fd_set.h
pdf_conv.o: /usr/include/sys/_types/_fd_clr.h
pdf_conv.o: /usr/include/sys/_types/_fd_zero.h
pdf_conv.o: /usr/include/sys/_types/_fd_isset.h
pdf_conv.o: /usr/include/sys/_types/_fd_copy.h
pdf_conv.o: /usr/include/sys/_pthread/_pthread_attr_t.h
pdf_conv.o: /usr/include/sys/_pthread/_pthread_cond_t.h
pdf_conv.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
pdf_conv.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
pdf_conv.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
pdf_conv.o: /usr/include/sys/_pthread/_pthread_once_t.h
pdf_conv.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
pdf_conv.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
pdf_conv.o: /usr/include/sys/_pthread/_pthread_t.h
pdf_conv.o: /usr/include/sys/_pthread/_pthread_key_t.h
pdf_conv.o: /usr/include/sys/_types/_fsblkcnt_t.h
pdf_conv.o: /usr/include/sys/_types/_fsfilcnt_t.h pdf.h pdfddi.h gks.h
pdf_conv.o: common.h /usr/include/math.h transform.h pdf_device.h
pdffill.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
pdffill.o: /usr/include/sys/_symbol_aliasing.h
pdffill.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
pdffill.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
pdffill.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
pdffill.o: /usr/include/i386/_types.h
pdffill.o: /usr/include/sys/_pthread/_pthread_types.h
pdffill.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
pdffill.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
pdffill.o: /usr/include/sys/_types/_int16_t.h
pdffill.o: /usr/include/sys/_types/_int32_t.h
pdffill.o: /usr/include/sys/_types/_int64_t.h
pdffill.o: /usr/include/sys/_types/_u_int8_t.h
pdffill.o: /usr/include/sys/_types/_u_int16_t.h
pdffill.o: /usr/include/sys/_types/_u_int32_t.h
pdffill.o: /usr/include/sys/_types/_u_int64_t.h
pdffill.o: /usr/include/sys/_types/_intptr_t.h
pdffill.o: /usr/include/sys/_types/_uintptr_t.h
pdffill.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
pdffill.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
pdffill.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
pdffill.o: /usr/include/secure/_common.h /usr/include/stdlib.h
pdffill.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
pdffill.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
pdffill.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
pdffill.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
pdffill.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
pdffill.o: /usr/include/mach/i386/_structs.h
pdffill.o: /usr/include/sys/_pthread/_pthread_attr_t.h
pdffill.o: /usr/include/sys/_types/_sigaltstack.h
pdffill.o: /usr/include/sys/_types/_ucontext.h
pdffill.o: /usr/include/sys/_types/_sigset_t.h
pdffill.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
pdffill.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
pdffill.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
pdffill.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
pdffill.o: /usr/include/_types/_uintmax_t.h
pdffill.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
pdffill.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
pdffill.o: /usr/include/libkern/_OSByteOrder.h
pdffill.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
pdffill.o: /usr/include/sys/_types/_ct_rune_t.h
pdffill.o: /usr/include/sys/_types/_rune_t.h
pdffill.o: /usr/include/sys/_types/_wchar_t.h
pdffill.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_mode_t.h
pdffill.o: /usr/include/string.h /usr/include/sys/_types/_rsize_t.h
pdffill.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
pdffill.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
pdffill.o: /usr/include/errno.h /usr/include/sys/errno.h /usr/include/math.h
pdffill.o: gksc.h /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
pdffill.o: /usr/include/sys/_types/_u_short.h
pdffill.o: /usr/include/sys/_types/_u_int.h
pdffill.o: /usr/include/sys/_types/_caddr_t.h
pdffill.o: /usr/include/sys/_types/_blkcnt_t.h
pdffill.o: /usr/include/sys/_types/_blksize_t.h
pdffill.o: /usr/include/sys/_types/_gid_t.h
pdffill.o: /usr/include/sys/_types/_in_addr_t.h
pdffill.o: /usr/include/sys/_types/_in_port_t.h
pdffill.o: /usr/include/sys/_types/_ino_t.h
pdffill.o: /usr/include/sys/_types/_ino64_t.h
pdffill.o: /usr/include/sys/_types/_key_t.h
pdffill.o: /usr/include/sys/_types/_nlink_t.h
pdffill.o: /usr/include/sys/_types/_clock_t.h
pdffill.o: /usr/include/sys/_types/_time_t.h
pdffill.o: /usr/include/sys/_types/_useconds_t.h
pdffill.o: /usr/include/sys/_types/_suseconds_t.h
pdffill.o: /usr/include/sys/_types/_fd_def.h
pdffill.o: /usr/include/sys/_types/_fd_setsize.h
pdffill.o: /usr/include/sys/_types/_fd_set.h
pdffill.o: /usr/include/sys/_types/_fd_clr.h
pdffill.o: /usr/include/sys/_types/_fd_zero.h
pdffill.o: /usr/include/sys/_types/_fd_isset.h
pdffill.o: /usr/include/sys/_types/_fd_copy.h
pdffill.o: /usr/include/sys/_pthread/_pthread_cond_t.h
pdffill.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
pdffill.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
pdffill.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
pdffill.o: /usr/include/sys/_pthread/_pthread_once_t.h
pdffill.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
pdffill.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
pdffill.o: /usr/include/sys/_pthread/_pthread_t.h
pdffill.o: /usr/include/sys/_pthread/_pthread_key_t.h
pdffill.o: /usr/include/sys/_types/_fsblkcnt_t.h
pdffill.o: /usr/include/sys/_types/_fsfilcnt_t.h gks.h common.h pdf.h
pdffill.o: pdfddi.h transform.h pdf_device.h
pixcontrol.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
pixcontrol.o: /usr/include/sys/_symbol_aliasing.h
pixcontrol.o: /usr/include/sys/_posix_availability.h
pixcontrol.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
pixcontrol.o: /usr/include/_types.h /usr/include/sys/_types.h
pixcontrol.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
pixcontrol.o: /usr/include/sys/_pthread/_pthread_types.h
pixcontrol.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
pixcontrol.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
pixcontrol.o: /usr/include/sys/_types/_int16_t.h
pixcontrol.o: /usr/include/sys/_types/_int32_t.h
pixcontrol.o: /usr/include/sys/_types/_int64_t.h
pixcontrol.o: /usr/include/sys/_types/_u_int8_t.h
pixcontrol.o: /usr/include/sys/_types/_u_int16_t.h
pixcontrol.o: /usr/include/sys/_types/_u_int32_t.h
pixcontrol.o: /usr/include/sys/_types/_u_int64_t.h
pixcontrol.o: /usr/include/sys/_types/_intptr_t.h
pixcontrol.o: /usr/include/sys/_types/_uintptr_t.h
pixcontrol.o: /usr/include/sys/_types/_size_t.h
pixcontrol.o: /usr/include/sys/_types/_null.h /usr/include/sys/stdio.h
pixcontrol.o: /usr/include/sys/_types/_off_t.h
pixcontrol.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
pixcontrol.o: /usr/include/secure/_common.h /usr/include/stdlib.h
pixcontrol.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
pixcontrol.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
pixcontrol.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
pixcontrol.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
pixcontrol.o: /usr/include/i386/_mcontext.h
pixcontrol.o: /usr/include/mach/machine/_structs.h
pixcontrol.o: /usr/include/mach/i386/_structs.h
pixcontrol.o: /usr/include/sys/_pthread/_pthread_attr_t.h
pixcontrol.o: /usr/include/sys/_types/_sigaltstack.h
pixcontrol.o: /usr/include/sys/_types/_ucontext.h
pixcontrol.o: /usr/include/sys/_types/_sigset_t.h
pixcontrol.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
pixcontrol.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
pixcontrol.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
pixcontrol.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
pixcontrol.o: /usr/include/_types/_uintmax_t.h
pixcontrol.o: /usr/include/sys/_types/_timeval.h
pixcontrol.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
pixcontrol.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
pixcontrol.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
pixcontrol.o: /usr/include/sys/_types/_ct_rune_t.h
pixcontrol.o: /usr/include/sys/_types/_rune_t.h
pixcontrol.o: /usr/include/sys/_types/_wchar_t.h
pixcontrol.o: /usr/include/sys/_types/_dev_t.h
pixcontrol.o: /usr/include/sys/_types/_mode_t.h /usr/include/string.h
pixcontrol.o: /usr/include/sys/_types/_rsize_t.h
pixcontrol.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
pixcontrol.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
pixcontrol.o: /usr/include/errno.h /usr/include/sys/errno.h
pixcontrol.o: /opt/X11/include/X11/Xlib.h /usr/include/sys/types.h
pixcontrol.o: /usr/include/sys/_types/_u_char.h
pixcontrol.o: /usr/include/sys/_types/_u_short.h
pixcontrol.o: /usr/include/sys/_types/_u_int.h
pixcontrol.o: /usr/include/sys/_types/_caddr_t.h
pixcontrol.o: /usr/include/sys/_types/_blkcnt_t.h
pixcontrol.o: /usr/include/sys/_types/_blksize_t.h
pixcontrol.o: /usr/include/sys/_types/_gid_t.h
pixcontrol.o: /usr/include/sys/_types/_in_addr_t.h
pixcontrol.o: /usr/include/sys/_types/_in_port_t.h
pixcontrol.o: /usr/include/sys/_types/_ino_t.h
pixcontrol.o: /usr/include/sys/_types/_ino64_t.h
pixcontrol.o: /usr/include/sys/_types/_key_t.h
pixcontrol.o: /usr/include/sys/_types/_nlink_t.h
pixcontrol.o: /usr/include/sys/_types/_clock_t.h
pixcontrol.o: /usr/include/sys/_types/_time_t.h
pixcontrol.o: /usr/include/sys/_types/_useconds_t.h
pixcontrol.o: /usr/include/sys/_types/_suseconds_t.h
pixcontrol.o: /usr/include/sys/_types/_fd_def.h
pixcontrol.o: /usr/include/sys/_types/_fd_setsize.h
pixcontrol.o: /usr/include/sys/_types/_fd_set.h
pixcontrol.o: /usr/include/sys/_types/_fd_clr.h
pixcontrol.o: /usr/include/sys/_types/_fd_zero.h
pixcontrol.o: /usr/include/sys/_types/_fd_isset.h
pixcontrol.o: /usr/include/sys/_types/_fd_copy.h
pixcontrol.o: /usr/include/sys/_pthread/_pthread_cond_t.h
pixcontrol.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
pixcontrol.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
pixcontrol.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
pixcontrol.o: /usr/include/sys/_pthread/_pthread_once_t.h
pixcontrol.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
pixcontrol.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
pixcontrol.o: /usr/include/sys/_pthread/_pthread_t.h
pixcontrol.o: /usr/include/sys/_pthread/_pthread_key_t.h
pixcontrol.o: /usr/include/sys/_types/_fsblkcnt_t.h
pixcontrol.o: /usr/include/sys/_types/_fsfilcnt_t.h /opt/X11/include/X11/X.h
pixcontrol.o: /opt/X11/include/X11/Xfuncproto.h
pixcontrol.o: /opt/X11/include/X11/Xosdefs.h /usr/include/stddef.h
pixcontrol.o: /usr/include/sys/_types/_offsetof.h
pixcontrol.o: /usr/include/sys/_types/_ptrdiff_t.h
pixcontrol.o: /usr/include/sys/_types/_wint_t.h /opt/X11/include/X11/Xutil.h
pixcontrol.o: /opt/X11/include/X11/keysym.h /opt/X11/include/X11/keysymdef.h
pixcontrol.o: /opt/X11/include/X11/Xatom.h ../../../.././include/ncarg/c.h
pixcontrol.o: ../../../.././include/ncarg/gks.h gks_device.h common.h
pixcontrol.o: /usr/include/math.h gksc.h gks.h
pixcontrol.o: ../../../.././include/ncarg/gksP.h x.h pix_device.h pixddi.h
pixcontrol.o: transform.h
pixoutput.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
pixoutput.o: /usr/include/sys/_symbol_aliasing.h
pixoutput.o: /usr/include/sys/_posix_availability.h
pixoutput.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
pixoutput.o: /usr/include/_types.h /usr/include/sys/_types.h
pixoutput.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
pixoutput.o: /usr/include/sys/_pthread/_pthread_types.h
pixoutput.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
pixoutput.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
pixoutput.o: /usr/include/sys/_types/_int16_t.h
pixoutput.o: /usr/include/sys/_types/_int32_t.h
pixoutput.o: /usr/include/sys/_types/_int64_t.h
pixoutput.o: /usr/include/sys/_types/_u_int8_t.h
pixoutput.o: /usr/include/sys/_types/_u_int16_t.h
pixoutput.o: /usr/include/sys/_types/_u_int32_t.h
pixoutput.o: /usr/include/sys/_types/_u_int64_t.h
pixoutput.o: /usr/include/sys/_types/_intptr_t.h
pixoutput.o: /usr/include/sys/_types/_uintptr_t.h
pixoutput.o: /usr/include/sys/_types/_size_t.h
pixoutput.o: /usr/include/sys/_types/_null.h /usr/include/sys/stdio.h
pixoutput.o: /usr/include/sys/_types/_off_t.h
pixoutput.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
pixoutput.o: /usr/include/secure/_common.h /usr/include/stdlib.h
pixoutput.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
pixoutput.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
pixoutput.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
pixoutput.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
pixoutput.o: /usr/include/i386/_mcontext.h
pixoutput.o: /usr/include/mach/machine/_structs.h
pixoutput.o: /usr/include/mach/i386/_structs.h
pixoutput.o: /usr/include/sys/_pthread/_pthread_attr_t.h
pixoutput.o: /usr/include/sys/_types/_sigaltstack.h
pixoutput.o: /usr/include/sys/_types/_ucontext.h
pixoutput.o: /usr/include/sys/_types/_sigset_t.h
pixoutput.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
pixoutput.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
pixoutput.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
pixoutput.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
pixoutput.o: /usr/include/_types/_uintmax_t.h
pixoutput.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
pixoutput.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
pixoutput.o: /usr/include/libkern/_OSByteOrder.h
pixoutput.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
pixoutput.o: /usr/include/sys/_types/_ct_rune_t.h
pixoutput.o: /usr/include/sys/_types/_rune_t.h
pixoutput.o: /usr/include/sys/_types/_wchar_t.h
pixoutput.o: /usr/include/sys/_types/_dev_t.h
pixoutput.o: /usr/include/sys/_types/_mode_t.h /usr/include/errno.h
pixoutput.o: /usr/include/sys/errno.h /usr/include/sys/_types/_errno_t.h
pixoutput.o: /opt/X11/include/X11/Xlib.h /usr/include/sys/types.h
pixoutput.o: /usr/include/sys/_types/_u_char.h
pixoutput.o: /usr/include/sys/_types/_u_short.h
pixoutput.o: /usr/include/sys/_types/_u_int.h
pixoutput.o: /usr/include/sys/_types/_caddr_t.h
pixoutput.o: /usr/include/sys/_types/_blkcnt_t.h
pixoutput.o: /usr/include/sys/_types/_blksize_t.h
pixoutput.o: /usr/include/sys/_types/_gid_t.h
pixoutput.o: /usr/include/sys/_types/_in_addr_t.h
pixoutput.o: /usr/include/sys/_types/_in_port_t.h
pixoutput.o: /usr/include/sys/_types/_ino_t.h
pixoutput.o: /usr/include/sys/_types/_ino64_t.h
pixoutput.o: /usr/include/sys/_types/_key_t.h
pixoutput.o: /usr/include/sys/_types/_nlink_t.h
pixoutput.o: /usr/include/sys/_types/_clock_t.h
pixoutput.o: /usr/include/sys/_types/_time_t.h
pixoutput.o: /usr/include/sys/_types/_useconds_t.h
pixoutput.o: /usr/include/sys/_types/_suseconds_t.h
pixoutput.o: /usr/include/sys/_types/_rsize_t.h
pixoutput.o: /usr/include/sys/_types/_fd_def.h
pixoutput.o: /usr/include/sys/_types/_fd_setsize.h
pixoutput.o: /usr/include/sys/_types/_fd_set.h
pixoutput.o: /usr/include/sys/_types/_fd_clr.h
pixoutput.o: /usr/include/sys/_types/_fd_zero.h
pixoutput.o: /usr/include/sys/_types/_fd_isset.h
pixoutput.o: /usr/include/sys/_types/_fd_copy.h
pixoutput.o: /usr/include/sys/_pthread/_pthread_cond_t.h
pixoutput.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
pixoutput.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
pixoutput.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
pixoutput.o: /usr/include/sys/_pthread/_pthread_once_t.h
pixoutput.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
pixoutput.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
pixoutput.o: /usr/include/sys/_pthread/_pthread_t.h
pixoutput.o: /usr/include/sys/_pthread/_pthread_key_t.h
pixoutput.o: /usr/include/sys/_types/_fsblkcnt_t.h
pixoutput.o: /usr/include/sys/_types/_fsfilcnt_t.h /opt/X11/include/X11/X.h
pixoutput.o: /opt/X11/include/X11/Xfuncproto.h /opt/X11/include/X11/Xosdefs.h
pixoutput.o: /usr/include/stddef.h /usr/include/sys/_types/_offsetof.h
pixoutput.o: /usr/include/sys/_types/_ptrdiff_t.h
pixoutput.o: /usr/include/sys/_types/_wint_t.h /opt/X11/include/X11/Xutil.h
pixoutput.o: /opt/X11/include/X11/keysym.h /opt/X11/include/X11/keysymdef.h
pixoutput.o: ../../../.././include/ncarg/c.h common.h /usr/include/math.h
pixoutput.o: gksc.h gks.h x.h pix_device.h pixddi.h
pixoutput.o: ../../../.././include/ncarg/gksP.h transform.h text.h
pixdevice.o: gks_device.h /usr/include/stddef.h /usr/include/_types.h
pixdevice.o: /usr/include/sys/_types.h /usr/include/sys/cdefs.h
pixdevice.o: /usr/include/sys/_symbol_aliasing.h
pixdevice.o: /usr/include/sys/_posix_availability.h
pixdevice.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
pixdevice.o: /usr/include/sys/_pthread/_pthread_types.h
pixdevice.o: /usr/include/sys/_types/_null.h
pixdevice.o: /usr/include/sys/_types/_offsetof.h
pixdevice.o: /usr/include/sys/_types/_ptrdiff_t.h
pixdevice.o: /usr/include/machine/types.h /usr/include/i386/types.h
pixdevice.o: /usr/include/sys/_types/_int8_t.h
pixdevice.o: /usr/include/sys/_types/_int16_t.h
pixdevice.o: /usr/include/sys/_types/_int32_t.h
pixdevice.o: /usr/include/sys/_types/_int64_t.h
pixdevice.o: /usr/include/sys/_types/_u_int8_t.h
pixdevice.o: /usr/include/sys/_types/_u_int16_t.h
pixdevice.o: /usr/include/sys/_types/_u_int32_t.h
pixdevice.o: /usr/include/sys/_types/_u_int64_t.h
pixdevice.o: /usr/include/sys/_types/_intptr_t.h
pixdevice.o: /usr/include/sys/_types/_uintptr_t.h
pixdevice.o: /usr/include/sys/_types/_rsize_t.h
pixdevice.o: /usr/include/sys/_types/_size_t.h
pixdevice.o: /usr/include/sys/_types/_wchar_t.h
pixdevice.o: /usr/include/sys/_types/_wint_t.h /opt/X11/include/X11/Xlib.h
pixdevice.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
pixdevice.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
pixdevice.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
pixdevice.o: /usr/include/libkern/i386/_OSByteOrder.h
pixdevice.o: /usr/include/sys/_types/_u_char.h
pixdevice.o: /usr/include/sys/_types/_u_short.h
pixdevice.o: /usr/include/sys/_types/_u_int.h
pixdevice.o: /usr/include/sys/_types/_caddr_t.h
pixdevice.o: /usr/include/sys/_types/_dev_t.h
pixdevice.o: /usr/include/sys/_types/_blkcnt_t.h
pixdevice.o: /usr/include/sys/_types/_blksize_t.h
pixdevice.o: /usr/include/sys/_types/_gid_t.h
pixdevice.o: /usr/include/sys/_types/_in_addr_t.h
pixdevice.o: /usr/include/sys/_types/_in_port_t.h
pixdevice.o: /usr/include/sys/_types/_ino_t.h
pixdevice.o: /usr/include/sys/_types/_ino64_t.h
pixdevice.o: /usr/include/sys/_types/_key_t.h
pixdevice.o: /usr/include/sys/_types/_mode_t.h
pixdevice.o: /usr/include/sys/_types/_nlink_t.h
pixdevice.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
pixdevice.o: /usr/include/sys/_types/_off_t.h
pixdevice.o: /usr/include/sys/_types/_uid_t.h
pixdevice.o: /usr/include/sys/_types/_clock_t.h
pixdevice.o: /usr/include/sys/_types/_ssize_t.h
pixdevice.o: /usr/include/sys/_types/_time_t.h
pixdevice.o: /usr/include/sys/_types/_useconds_t.h
pixdevice.o: /usr/include/sys/_types/_suseconds_t.h
pixdevice.o: /usr/include/sys/_types/_errno_t.h
pixdevice.o: /usr/include/sys/_types/_fd_def.h
pixdevice.o: /usr/include/sys/_types/_fd_setsize.h
pixdevice.o: /usr/include/sys/_types/_fd_set.h
pixdevice.o: /usr/include/sys/_types/_fd_clr.h
pixdevice.o: /usr/include/sys/_types/_fd_zero.h
pixdevice.o: /usr/include/sys/_types/_fd_isset.h
pixdevice.o: /usr/include/sys/_types/_fd_copy.h
pixdevice.o: /usr/include/sys/_pthread/_pthread_attr_t.h
pixdevice.o: /usr/include/sys/_pthread/_pthread_cond_t.h
pixdevice.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
pixdevice.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
pixdevice.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
pixdevice.o: /usr/include/sys/_pthread/_pthread_once_t.h
pixdevice.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
pixdevice.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
pixdevice.o: /usr/include/sys/_pthread/_pthread_t.h
pixdevice.o: /usr/include/sys/_pthread/_pthread_key_t.h
pixdevice.o: /usr/include/sys/_types/_fsblkcnt_t.h
pixdevice.o: /usr/include/sys/_types/_fsfilcnt_t.h /opt/X11/include/X11/X.h
pixdevice.o: /opt/X11/include/X11/Xfuncproto.h /opt/X11/include/X11/Xosdefs.h
pixdevice.o: pix_device.h gksc.h
pix_conv.o: /opt/X11/include/X11/Xlib.h /usr/include/sys/types.h
pix_conv.o: /usr/include/sys/appleapiopts.h /usr/include/sys/cdefs.h
pix_conv.o: /usr/include/sys/_symbol_aliasing.h
pix_conv.o: /usr/include/sys/_posix_availability.h
pix_conv.o: /usr/include/machine/types.h /usr/include/i386/types.h
pix_conv.o: /usr/include/i386/_types.h /usr/include/sys/_types/_int8_t.h
pix_conv.o: /usr/include/sys/_types/_int16_t.h
pix_conv.o: /usr/include/sys/_types/_int32_t.h
pix_conv.o: /usr/include/sys/_types/_int64_t.h
pix_conv.o: /usr/include/sys/_types/_u_int8_t.h
pix_conv.o: /usr/include/sys/_types/_u_int16_t.h
pix_conv.o: /usr/include/sys/_types/_u_int32_t.h
pix_conv.o: /usr/include/sys/_types/_u_int64_t.h
pix_conv.o: /usr/include/sys/_types/_intptr_t.h
pix_conv.o: /usr/include/sys/_types/_uintptr_t.h /usr/include/sys/_types.h
pix_conv.o: /usr/include/machine/_types.h
pix_conv.o: /usr/include/sys/_pthread/_pthread_types.h
pix_conv.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
pix_conv.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
pix_conv.o: /usr/include/libkern/i386/_OSByteOrder.h
pix_conv.o: /usr/include/sys/_types/_u_char.h
pix_conv.o: /usr/include/sys/_types/_u_short.h
pix_conv.o: /usr/include/sys/_types/_u_int.h
pix_conv.o: /usr/include/sys/_types/_caddr_t.h
pix_conv.o: /usr/include/sys/_types/_dev_t.h
pix_conv.o: /usr/include/sys/_types/_blkcnt_t.h
pix_conv.o: /usr/include/sys/_types/_blksize_t.h
pix_conv.o: /usr/include/sys/_types/_gid_t.h
pix_conv.o: /usr/include/sys/_types/_in_addr_t.h
pix_conv.o: /usr/include/sys/_types/_in_port_t.h
pix_conv.o: /usr/include/sys/_types/_ino_t.h
pix_conv.o: /usr/include/sys/_types/_ino64_t.h
pix_conv.o: /usr/include/sys/_types/_key_t.h
pix_conv.o: /usr/include/sys/_types/_mode_t.h
pix_conv.o: /usr/include/sys/_types/_nlink_t.h
pix_conv.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
pix_conv.o: /usr/include/sys/_types/_off_t.h /usr/include/sys/_types/_uid_t.h
pix_conv.o: /usr/include/sys/_types/_clock_t.h
pix_conv.o: /usr/include/sys/_types/_size_t.h
pix_conv.o: /usr/include/sys/_types/_ssize_t.h
pix_conv.o: /usr/include/sys/_types/_time_t.h
pix_conv.o: /usr/include/sys/_types/_useconds_t.h
pix_conv.o: /usr/include/sys/_types/_suseconds_t.h
pix_conv.o: /usr/include/sys/_types/_rsize_t.h
pix_conv.o: /usr/include/sys/_types/_errno_t.h
pix_conv.o: /usr/include/sys/_types/_fd_def.h
pix_conv.o: /usr/include/sys/_types/_fd_setsize.h
pix_conv.o: /usr/include/sys/_types/_fd_set.h
pix_conv.o: /usr/include/sys/_types/_fd_clr.h
pix_conv.o: /usr/include/sys/_types/_fd_zero.h
pix_conv.o: /usr/include/sys/_types/_fd_isset.h
pix_conv.o: /usr/include/sys/_types/_fd_copy.h
pix_conv.o: /usr/include/sys/_pthread/_pthread_attr_t.h
pix_conv.o: /usr/include/sys/_pthread/_pthread_cond_t.h
pix_conv.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
pix_conv.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
pix_conv.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
pix_conv.o: /usr/include/sys/_pthread/_pthread_once_t.h
pix_conv.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
pix_conv.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
pix_conv.o: /usr/include/sys/_pthread/_pthread_t.h
pix_conv.o: /usr/include/sys/_pthread/_pthread_key_t.h
pix_conv.o: /usr/include/sys/_types/_fsblkcnt_t.h
pix_conv.o: /usr/include/sys/_types/_fsfilcnt_t.h /opt/X11/include/X11/X.h
pix_conv.o: /opt/X11/include/X11/Xfuncproto.h /opt/X11/include/X11/Xosdefs.h
pix_conv.o: /usr/include/stddef.h /usr/include/_types.h
pix_conv.o: /usr/include/sys/_types/_null.h
pix_conv.o: /usr/include/sys/_types/_offsetof.h
pix_conv.o: /usr/include/sys/_types/_ptrdiff_t.h
pix_conv.o: /usr/include/sys/_types/_wchar_t.h
pix_conv.o: /usr/include/sys/_types/_wint_t.h gksc.h pixddi.h
pix_conv.o: ../../../.././include/ncarg/gksP.h x.h gks.h common.h
pix_conv.o: /usr/include/math.h /usr/include/Availability.h
pix_conv.o: /usr/include/AvailabilityInternal.h transform.h pix_device.h
pixattribute.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
pixattribute.o: /usr/include/sys/_symbol_aliasing.h
pixattribute.o: /usr/include/sys/_posix_availability.h
pixattribute.o: /usr/include/Availability.h
pixattribute.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
pixattribute.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
pixattribute.o: /usr/include/i386/_types.h
pixattribute.o: /usr/include/sys/_pthread/_pthread_types.h
pixattribute.o: /usr/include/sys/_types/_va_list.h
pixattribute.o: /usr/include/machine/types.h /usr/include/i386/types.h
pixattribute.o: /usr/include/sys/_types/_int8_t.h
pixattribute.o: /usr/include/sys/_types/_int16_t.h
pixattribute.o: /usr/include/sys/_types/_int32_t.h
pixattribute.o: /usr/include/sys/_types/_int64_t.h
pixattribute.o: /usr/include/sys/_types/_u_int8_t.h
pixattribute.o: /usr/include/sys/_types/_u_int16_t.h
pixattribute.o: /usr/include/sys/_types/_u_int32_t.h
pixattribute.o: /usr/include/sys/_types/_u_int64_t.h
pixattribute.o: /usr/include/sys/_types/_intptr_t.h
pixattribute.o: /usr/include/sys/_types/_uintptr_t.h
pixattribute.o: /usr/include/sys/_types/_size_t.h
pixattribute.o: /usr/include/sys/_types/_null.h /usr/include/sys/stdio.h
pixattribute.o: /usr/include/sys/_types/_off_t.h
pixattribute.o: /usr/include/sys/_types/_ssize_t.h
pixattribute.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
pixattribute.o: /usr/include/stdlib.h /usr/include/sys/wait.h
pixattribute.o: /usr/include/sys/_types/_pid_t.h
pixattribute.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
pixattribute.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
pixattribute.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
pixattribute.o: /usr/include/i386/_mcontext.h
pixattribute.o: /usr/include/mach/machine/_structs.h
pixattribute.o: /usr/include/mach/i386/_structs.h
pixattribute.o: /usr/include/sys/_pthread/_pthread_attr_t.h
pixattribute.o: /usr/include/sys/_types/_sigaltstack.h
pixattribute.o: /usr/include/sys/_types/_ucontext.h
pixattribute.o: /usr/include/sys/_types/_sigset_t.h
pixattribute.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
pixattribute.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
pixattribute.o: /usr/include/_types/_uint16_t.h
pixattribute.o: /usr/include/_types/_uint32_t.h
pixattribute.o: /usr/include/_types/_uint64_t.h
pixattribute.o: /usr/include/_types/_intmax_t.h
pixattribute.o: /usr/include/_types/_uintmax_t.h
pixattribute.o: /usr/include/sys/_types/_timeval.h
pixattribute.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
pixattribute.o: /usr/include/sys/_endian.h
pixattribute.o: /usr/include/libkern/_OSByteOrder.h
pixattribute.o: /usr/include/libkern/i386/_OSByteOrder.h
pixattribute.o: /usr/include/alloca.h /usr/include/sys/_types/_ct_rune_t.h
pixattribute.o: /usr/include/sys/_types/_rune_t.h
pixattribute.o: /usr/include/sys/_types/_wchar_t.h
pixattribute.o: /usr/include/sys/_types/_dev_t.h
pixattribute.o: /usr/include/sys/_types/_mode_t.h /opt/X11/include/X11/Xlib.h
pixattribute.o: /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
pixattribute.o: /usr/include/sys/_types/_u_short.h
pixattribute.o: /usr/include/sys/_types/_u_int.h
pixattribute.o: /usr/include/sys/_types/_caddr_t.h
pixattribute.o: /usr/include/sys/_types/_blkcnt_t.h
pixattribute.o: /usr/include/sys/_types/_blksize_t.h
pixattribute.o: /usr/include/sys/_types/_gid_t.h
pixattribute.o: /usr/include/sys/_types/_in_addr_t.h
pixattribute.o: /usr/include/sys/_types/_in_port_t.h
pixattribute.o: /usr/include/sys/_types/_ino_t.h
pixattribute.o: /usr/include/sys/_types/_ino64_t.h
pixattribute.o: /usr/include/sys/_types/_key_t.h
pixattribute.o: /usr/include/sys/_types/_nlink_t.h
pixattribute.o: /usr/include/sys/_types/_clock_t.h
pixattribute.o: /usr/include/sys/_types/_time_t.h
pixattribute.o: /usr/include/sys/_types/_useconds_t.h
pixattribute.o: /usr/include/sys/_types/_suseconds_t.h
pixattribute.o: /usr/include/sys/_types/_rsize_t.h
pixattribute.o: /usr/include/sys/_types/_errno_t.h
pixattribute.o: /usr/include/sys/_types/_fd_def.h
pixattribute.o: /usr/include/sys/_types/_fd_setsize.h
pixattribute.o: /usr/include/sys/_types/_fd_set.h
pixattribute.o: /usr/include/sys/_types/_fd_clr.h
pixattribute.o: /usr/include/sys/_types/_fd_zero.h
pixattribute.o: /usr/include/sys/_types/_fd_isset.h
pixattribute.o: /usr/include/sys/_types/_fd_copy.h
pixattribute.o: /usr/include/sys/_pthread/_pthread_cond_t.h
pixattribute.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
pixattribute.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
pixattribute.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
pixattribute.o: /usr/include/sys/_pthread/_pthread_once_t.h
pixattribute.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
pixattribute.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
pixattribute.o: /usr/include/sys/_pthread/_pthread_t.h
pixattribute.o: /usr/include/sys/_pthread/_pthread_key_t.h
pixattribute.o: /usr/include/sys/_types/_fsblkcnt_t.h
pixattribute.o: /usr/include/sys/_types/_fsfilcnt_t.h
pixattribute.o: /opt/X11/include/X11/X.h /opt/X11/include/X11/Xfuncproto.h
pixattribute.o: /opt/X11/include/X11/Xosdefs.h /usr/include/stddef.h
pixattribute.o: /usr/include/sys/_types/_offsetof.h
pixattribute.o: /usr/include/sys/_types/_ptrdiff_t.h
pixattribute.o: /usr/include/sys/_types/_wint_t.h
pixattribute.o: /opt/X11/include/X11/Xutil.h /opt/X11/include/X11/keysym.h
pixattribute.o: /opt/X11/include/X11/keysymdef.h common.h /usr/include/math.h
pixattribute.o: gksc.h gks.h x.h pix_device.h pixddi.h
pixattribute.o: ../../../.././include/ncarg/gksP.h transform.h xattribute.h
pixattribute.o: text.h ../../../.././include/ncarg/c.h
pixinquiry.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
pixinquiry.o: /usr/include/sys/_symbol_aliasing.h
pixinquiry.o: /usr/include/sys/_posix_availability.h
pixinquiry.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
pixinquiry.o: /usr/include/_types.h /usr/include/sys/_types.h
pixinquiry.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
pixinquiry.o: /usr/include/sys/_pthread/_pthread_types.h
pixinquiry.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
pixinquiry.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
pixinquiry.o: /usr/include/sys/_types/_int16_t.h
pixinquiry.o: /usr/include/sys/_types/_int32_t.h
pixinquiry.o: /usr/include/sys/_types/_int64_t.h
pixinquiry.o: /usr/include/sys/_types/_u_int8_t.h
pixinquiry.o: /usr/include/sys/_types/_u_int16_t.h
pixinquiry.o: /usr/include/sys/_types/_u_int32_t.h
pixinquiry.o: /usr/include/sys/_types/_u_int64_t.h
pixinquiry.o: /usr/include/sys/_types/_intptr_t.h
pixinquiry.o: /usr/include/sys/_types/_uintptr_t.h
pixinquiry.o: /usr/include/sys/_types/_size_t.h
pixinquiry.o: /usr/include/sys/_types/_null.h /usr/include/sys/stdio.h
pixinquiry.o: /usr/include/sys/_types/_off_t.h
pixinquiry.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
pixinquiry.o: /usr/include/secure/_common.h /opt/X11/include/X11/Xlib.h
pixinquiry.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
pixinquiry.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
pixinquiry.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
pixinquiry.o: /usr/include/libkern/i386/_OSByteOrder.h
pixinquiry.o: /usr/include/sys/_types/_u_char.h
pixinquiry.o: /usr/include/sys/_types/_u_short.h
pixinquiry.o: /usr/include/sys/_types/_u_int.h
pixinquiry.o: /usr/include/sys/_types/_caddr_t.h
pixinquiry.o: /usr/include/sys/_types/_dev_t.h
pixinquiry.o: /usr/include/sys/_types/_blkcnt_t.h
pixinquiry.o: /usr/include/sys/_types/_blksize_t.h
pixinquiry.o: /usr/include/sys/_types/_gid_t.h
pixinquiry.o: /usr/include/sys/_types/_in_addr_t.h
pixinquiry.o: /usr/include/sys/_types/_in_port_t.h
pixinquiry.o: /usr/include/sys/_types/_ino_t.h
pixinquiry.o: /usr/include/sys/_types/_ino64_t.h
pixinquiry.o: /usr/include/sys/_types/_key_t.h
pixinquiry.o: /usr/include/sys/_types/_mode_t.h
pixinquiry.o: /usr/include/sys/_types/_nlink_t.h
pixinquiry.o: /usr/include/sys/_types/_id_t.h
pixinquiry.o: /usr/include/sys/_types/_pid_t.h
pixinquiry.o: /usr/include/sys/_types/_uid_t.h
pixinquiry.o: /usr/include/sys/_types/_clock_t.h
pixinquiry.o: /usr/include/sys/_types/_time_t.h
pixinquiry.o: /usr/include/sys/_types/_useconds_t.h
pixinquiry.o: /usr/include/sys/_types/_suseconds_t.h
pixinquiry.o: /usr/include/sys/_types/_rsize_t.h
pixinquiry.o: /usr/include/sys/_types/_errno_t.h
pixinquiry.o: /usr/include/sys/_types/_fd_def.h
pixinquiry.o: /usr/include/sys/_types/_fd_setsize.h
pixinquiry.o: /usr/include/sys/_types/_fd_set.h
pixinquiry.o: /usr/include/sys/_types/_fd_clr.h
pixinquiry.o: /usr/include/sys/_types/_fd_zero.h
pixinquiry.o: /usr/include/sys/_types/_fd_isset.h
pixinquiry.o: /usr/include/sys/_types/_fd_copy.h
pixinquiry.o: /usr/include/sys/_pthread/_pthread_attr_t.h
pixinquiry.o: /usr/include/sys/_pthread/_pthread_cond_t.h
pixinquiry.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
pixinquiry.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
pixinquiry.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
pixinquiry.o: /usr/include/sys/_pthread/_pthread_once_t.h
pixinquiry.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
pixinquiry.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
pixinquiry.o: /usr/include/sys/_pthread/_pthread_t.h
pixinquiry.o: /usr/include/sys/_pthread/_pthread_key_t.h
pixinquiry.o: /usr/include/sys/_types/_fsblkcnt_t.h
pixinquiry.o: /usr/include/sys/_types/_fsfilcnt_t.h /opt/X11/include/X11/X.h
pixinquiry.o: /opt/X11/include/X11/Xfuncproto.h
pixinquiry.o: /opt/X11/include/X11/Xosdefs.h /usr/include/stddef.h
pixinquiry.o: /usr/include/sys/_types/_offsetof.h
pixinquiry.o: /usr/include/sys/_types/_ptrdiff_t.h
pixinquiry.o: /usr/include/sys/_types/_wchar_t.h
pixinquiry.o: /usr/include/sys/_types/_wint_t.h /opt/X11/include/X11/Xutil.h
pixinquiry.o: /opt/X11/include/X11/keysym.h /opt/X11/include/X11/keysymdef.h
pixinquiry.o: common.h /usr/include/math.h gksc.h gks.h x.h pix_device.h
pixinquiry.o: pixddi.h ../../../.././include/ncarg/gksP.h transform.h
pixwrite_xwd.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
pixwrite_xwd.o: /usr/include/sys/_symbol_aliasing.h
pixwrite_xwd.o: /usr/include/sys/_posix_availability.h
pixwrite_xwd.o: /usr/include/Availability.h
pixwrite_xwd.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
pixwrite_xwd.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
pixwrite_xwd.o: /usr/include/i386/_types.h
pixwrite_xwd.o: /usr/include/sys/_pthread/_pthread_types.h
pixwrite_xwd.o: /usr/include/sys/_types/_va_list.h
pixwrite_xwd.o: /usr/include/machine/types.h /usr/include/i386/types.h
pixwrite_xwd.o: /usr/include/sys/_types/_int8_t.h
pixwrite_xwd.o: /usr/include/sys/_types/_int16_t.h
pixwrite_xwd.o: /usr/include/sys/_types/_int32_t.h
pixwrite_xwd.o: /usr/include/sys/_types/_int64_t.h
pixwrite_xwd.o: /usr/include/sys/_types/_u_int8_t.h
pixwrite_xwd.o: /usr/include/sys/_types/_u_int16_t.h
pixwrite_xwd.o: /usr/include/sys/_types/_u_int32_t.h
pixwrite_xwd.o: /usr/include/sys/_types/_u_int64_t.h
pixwrite_xwd.o: /usr/include/sys/_types/_intptr_t.h
pixwrite_xwd.o: /usr/include/sys/_types/_uintptr_t.h
pixwrite_xwd.o: /usr/include/sys/_types/_size_t.h
pixwrite_xwd.o: /usr/include/sys/_types/_null.h /usr/include/sys/stdio.h
pixwrite_xwd.o: /usr/include/sys/_types/_off_t.h
pixwrite_xwd.o: /usr/include/sys/_types/_ssize_t.h
pixwrite_xwd.o: /usr/include/secure/_stdio.h /usr/include/secure/_common.h
pixwrite_xwd.o: /usr/include/stdlib.h /usr/include/sys/wait.h
pixwrite_xwd.o: /usr/include/sys/_types/_pid_t.h
pixwrite_xwd.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
pixwrite_xwd.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
pixwrite_xwd.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
pixwrite_xwd.o: /usr/include/i386/_mcontext.h
pixwrite_xwd.o: /usr/include/mach/machine/_structs.h
pixwrite_xwd.o: /usr/include/mach/i386/_structs.h
pixwrite_xwd.o: /usr/include/sys/_pthread/_pthread_attr_t.h
pixwrite_xwd.o: /usr/include/sys/_types/_sigaltstack.h
pixwrite_xwd.o: /usr/include/sys/_types/_ucontext.h
pixwrite_xwd.o: /usr/include/sys/_types/_sigset_t.h
pixwrite_xwd.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
pixwrite_xwd.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
pixwrite_xwd.o: /usr/include/_types/_uint16_t.h
pixwrite_xwd.o: /usr/include/_types/_uint32_t.h
pixwrite_xwd.o: /usr/include/_types/_uint64_t.h
pixwrite_xwd.o: /usr/include/_types/_intmax_t.h
pixwrite_xwd.o: /usr/include/_types/_uintmax_t.h
pixwrite_xwd.o: /usr/include/sys/_types/_timeval.h
pixwrite_xwd.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
pixwrite_xwd.o: /usr/include/sys/_endian.h
pixwrite_xwd.o: /usr/include/libkern/_OSByteOrder.h
pixwrite_xwd.o: /usr/include/libkern/i386/_OSByteOrder.h
pixwrite_xwd.o: /usr/include/alloca.h /usr/include/sys/_types/_ct_rune_t.h
pixwrite_xwd.o: /usr/include/sys/_types/_rune_t.h
pixwrite_xwd.o: /usr/include/sys/_types/_wchar_t.h
pixwrite_xwd.o: /usr/include/sys/_types/_dev_t.h
pixwrite_xwd.o: /usr/include/sys/_types/_mode_t.h /usr/include/string.h
pixwrite_xwd.o: /usr/include/sys/_types/_rsize_t.h
pixwrite_xwd.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
pixwrite_xwd.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
pixwrite_xwd.o: /usr/include/errno.h /usr/include/sys/errno.h
pixwrite_xwd.o: /opt/X11/include/X11/Xlib.h /usr/include/sys/types.h
pixwrite_xwd.o: /usr/include/sys/_types/_u_char.h
pixwrite_xwd.o: /usr/include/sys/_types/_u_short.h
pixwrite_xwd.o: /usr/include/sys/_types/_u_int.h
pixwrite_xwd.o: /usr/include/sys/_types/_caddr_t.h
pixwrite_xwd.o: /usr/include/sys/_types/_blkcnt_t.h
pixwrite_xwd.o: /usr/include/sys/_types/_blksize_t.h
pixwrite_xwd.o: /usr/include/sys/_types/_gid_t.h
pixwrite_xwd.o: /usr/include/sys/_types/_in_addr_t.h
pixwrite_xwd.o: /usr/include/sys/_types/_in_port_t.h
pixwrite_xwd.o: /usr/include/sys/_types/_ino_t.h
pixwrite_xwd.o: /usr/include/sys/_types/_ino64_t.h
pixwrite_xwd.o: /usr/include/sys/_types/_key_t.h
pixwrite_xwd.o: /usr/include/sys/_types/_nlink_t.h
pixwrite_xwd.o: /usr/include/sys/_types/_clock_t.h
pixwrite_xwd.o: /usr/include/sys/_types/_time_t.h
pixwrite_xwd.o: /usr/include/sys/_types/_useconds_t.h
pixwrite_xwd.o: /usr/include/sys/_types/_suseconds_t.h
pixwrite_xwd.o: /usr/include/sys/_types/_fd_def.h
pixwrite_xwd.o: /usr/include/sys/_types/_fd_setsize.h
pixwrite_xwd.o: /usr/include/sys/_types/_fd_set.h
pixwrite_xwd.o: /usr/include/sys/_types/_fd_clr.h
pixwrite_xwd.o: /usr/include/sys/_types/_fd_zero.h
pixwrite_xwd.o: /usr/include/sys/_types/_fd_isset.h
pixwrite_xwd.o: /usr/include/sys/_types/_fd_copy.h
pixwrite_xwd.o: /usr/include/sys/_pthread/_pthread_cond_t.h
pixwrite_xwd.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
pixwrite_xwd.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
pixwrite_xwd.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
pixwrite_xwd.o: /usr/include/sys/_pthread/_pthread_once_t.h
pixwrite_xwd.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
pixwrite_xwd.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
pixwrite_xwd.o: /usr/include/sys/_pthread/_pthread_t.h
pixwrite_xwd.o: /usr/include/sys/_pthread/_pthread_key_t.h
pixwrite_xwd.o: /usr/include/sys/_types/_fsblkcnt_t.h
pixwrite_xwd.o: /usr/include/sys/_types/_fsfilcnt_t.h
pixwrite_xwd.o: /opt/X11/include/X11/X.h /opt/X11/include/X11/Xfuncproto.h
pixwrite_xwd.o: /opt/X11/include/X11/Xosdefs.h /usr/include/stddef.h
pixwrite_xwd.o: /usr/include/sys/_types/_offsetof.h
pixwrite_xwd.o: /usr/include/sys/_types/_ptrdiff_t.h
pixwrite_xwd.o: /usr/include/sys/_types/_wint_t.h
pixwrite_xwd.o: /opt/X11/include/X11/Xutil.h /opt/X11/include/X11/keysym.h
pixwrite_xwd.o: /opt/X11/include/X11/keysymdef.h /opt/X11/include/X11/Xatom.h
pixwrite_xwd.o: /opt/X11/include/X11/Xmu/WinUtil.h
pixwrite_xwd.o: /opt/X11/include/X11/XWDFile.h /opt/X11/include/X11/Xmd.h
pixwrite_xwd.o: ../../../.././include/ncarg/c.h gks_device.h common.h
pixwrite_xwd.o: /usr/include/math.h gksc.h gks.h
pixwrite_xwd.o: ../../../.././include/ncarg/gksP.h x.h pix_device.h pixddi.h
pixwrite_xwd.o: transform.h
