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
CURRENT_DIR		= ./ncarview/src/bin/idt

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
#
#	$Id: yMakefile,v 1.20 2002-02-23 01:14:18 haley Exp $
#
#
#	Makefile for idt -- Interactive Image Display Tool
#
#

MYNAME	= idt

TRANSLATOR	= ictrans

PROG_LIBS	=	$(CGMLIB) $(NCARGCLIB)
DEPLIBS		=	$(DEPCGMLIB) $(DEPNCARGCLIB)

EXTRA_LIBS	=	$(XMUATHENACLIENTLIBS)
SYS_LIBS	=	-lm

MY_DEFINES =

EXTRA_CDEFINES =	-DXAPPDIR='"$(XAPPDIR)"'  			-DTRANSLATOR='"$(TRANSLATOR)"' -DBINDIR='"$(BINDIR)"'  			$(VDEFINE) $(OS_DEF) $(MAJOR_DEF) $(MY_DEFINES)

OBJS	=  	w_idt.o w_file.o file.o glob.o w_display.o display.o w_dialog.o  	commands.o talkto.o xrubber.o scroll.o w_text.o spooler.o w_icon.o  	xt_env.o animate.o animate_cb.o

SRCS	=  	w_idt.c w_file.c file.c glob.c w_display.c display.c w_dialog.c  	commands.c talkto.c xrubber.c scroll.c w_text.c spooler.c w_icon.c  	xt_env.c animate.c animate_cb.c

all::

all-local:: $(MYNAME)

$(MYNAME): $(OBJS) $(DEPLIBS)
	$(CC_LD) $(CC_LDOPTIONS) -o $(MYNAME) $(OBJS) $(PROG_LIBS) $(LDLIBS) $(EXTRA_LDLIBS)

all::

clean-local::
	-@$(RM) $(MYNAME) $(OBJS)

all::

all-local:: $(MYNAME)

install-local:: $(MYNAME)
	@if (test ! -d $(BINPATH)); then ($(MKDIRHIER) $(BINPATH)); fi
	@for name in $(MYNAME); do \
	if (test -f $$name.$(EXE_SUFFIX)) then \
	(echo "Installing $$name.$(EXE_SUFFIX)";\
	$(INSTALL) -m $(INSTALL_BIN) $$name.$(EXE_SUFFIX) $(BINPATH));\
	else \
	(echo "Installing $$name";\
	$(INSTALL) -m $(INSTALL_BIN) $$name $(BINPATH)); fi \
	done

all::

depend-local:: $(SRCS)
	$(DEPEND) -s "# DO NOT DELETE" -- $(ALLDEFINES) -- $(SRCS)

talkto.o: talkto.c
	$(CC) $(ALLINCLUDES) $(CCOPTIONS) $(COPT) -c talkto.c

#
#	w_idt.c needs to know the name of the translator
#
w_idt.o:     w_idt.c Makefile

#
#	xt_env.c needs to know the default location of the idt resoure file
#
xt_env.o:     xt_env.c Makefile

lint:	$(HDR) $(SRCS)
	lint  $(CFLAGS)$(LINTLIBS) $(SRCS)  1> lint.out 2>&1

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

w_idt.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
w_idt.o: /usr/include/sys/_symbol_aliasing.h
w_idt.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
w_idt.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
w_idt.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
w_idt.o: /usr/include/i386/_types.h
w_idt.o: /usr/include/sys/_pthread/_pthread_types.h
w_idt.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
w_idt.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
w_idt.o: /usr/include/sys/_types/_int16_t.h
w_idt.o: /usr/include/sys/_types/_int32_t.h
w_idt.o: /usr/include/sys/_types/_int64_t.h
w_idt.o: /usr/include/sys/_types/_u_int8_t.h
w_idt.o: /usr/include/sys/_types/_u_int16_t.h
w_idt.o: /usr/include/sys/_types/_u_int32_t.h
w_idt.o: /usr/include/sys/_types/_u_int64_t.h
w_idt.o: /usr/include/sys/_types/_intptr_t.h
w_idt.o: /usr/include/sys/_types/_uintptr_t.h
w_idt.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
w_idt.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
w_idt.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
w_idt.o: /usr/include/secure/_common.h /usr/include/stdlib.h
w_idt.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
w_idt.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
w_idt.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
w_idt.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
w_idt.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
w_idt.o: /usr/include/mach/i386/_structs.h
w_idt.o: /usr/include/sys/_pthread/_pthread_attr_t.h
w_idt.o: /usr/include/sys/_types/_sigaltstack.h
w_idt.o: /usr/include/sys/_types/_ucontext.h
w_idt.o: /usr/include/sys/_types/_sigset_t.h /usr/include/sys/_types/_uid_t.h
w_idt.o: /usr/include/sys/resource.h /usr/include/stdint.h
w_idt.o: /usr/include/_types/_uint8_t.h /usr/include/_types/_uint16_t.h
w_idt.o: /usr/include/_types/_uint32_t.h /usr/include/_types/_uint64_t.h
w_idt.o: /usr/include/_types/_intmax_t.h /usr/include/_types/_uintmax_t.h
w_idt.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
w_idt.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
w_idt.o: /usr/include/libkern/_OSByteOrder.h
w_idt.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
w_idt.o: /usr/include/sys/_types/_ct_rune_t.h
w_idt.o: /usr/include/sys/_types/_rune_t.h /usr/include/sys/_types/_wchar_t.h
w_idt.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_mode_t.h
w_idt.o: /opt/X11/include/X11/Intrinsic.h /opt/X11/include/X11/Xlib.h
w_idt.o: /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
w_idt.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
w_idt.o: /usr/include/sys/_types/_caddr_t.h
w_idt.o: /usr/include/sys/_types/_blkcnt_t.h
w_idt.o: /usr/include/sys/_types/_blksize_t.h
w_idt.o: /usr/include/sys/_types/_gid_t.h
w_idt.o: /usr/include/sys/_types/_in_addr_t.h
w_idt.o: /usr/include/sys/_types/_in_port_t.h
w_idt.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
w_idt.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_nlink_t.h
w_idt.o: /usr/include/sys/_types/_clock_t.h /usr/include/sys/_types/_time_t.h
w_idt.o: /usr/include/sys/_types/_useconds_t.h
w_idt.o: /usr/include/sys/_types/_suseconds_t.h
w_idt.o: /usr/include/sys/_types/_rsize_t.h
w_idt.o: /usr/include/sys/_types/_errno_t.h /usr/include/sys/_types/_fd_def.h
w_idt.o: /usr/include/sys/_types/_fd_setsize.h
w_idt.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
w_idt.o: /usr/include/sys/_types/_fd_zero.h
w_idt.o: /usr/include/sys/_types/_fd_isset.h
w_idt.o: /usr/include/sys/_types/_fd_copy.h
w_idt.o: /usr/include/sys/_pthread/_pthread_cond_t.h
w_idt.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
w_idt.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
w_idt.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
w_idt.o: /usr/include/sys/_pthread/_pthread_once_t.h
w_idt.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
w_idt.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
w_idt.o: /usr/include/sys/_pthread/_pthread_t.h
w_idt.o: /usr/include/sys/_pthread/_pthread_key_t.h
w_idt.o: /usr/include/sys/_types/_fsblkcnt_t.h
w_idt.o: /usr/include/sys/_types/_fsfilcnt_t.h /opt/X11/include/X11/X.h
w_idt.o: /opt/X11/include/X11/Xfuncproto.h /opt/X11/include/X11/Xosdefs.h
w_idt.o: /usr/include/stddef.h /usr/include/sys/_types/_offsetof.h
w_idt.o: /usr/include/sys/_types/_ptrdiff_t.h
w_idt.o: /usr/include/sys/_types/_wint_t.h /opt/X11/include/X11/Xutil.h
w_idt.o: /opt/X11/include/X11/keysym.h /opt/X11/include/X11/keysymdef.h
w_idt.o: /opt/X11/include/X11/Xresource.h /usr/include/string.h
w_idt.o: /usr/include/strings.h /usr/include/secure/_strings.h
w_idt.o: /usr/include/secure/_string.h /opt/X11/include/X11/Core.h
w_idt.o: /opt/X11/include/X11/Composite.h /opt/X11/include/X11/Constraint.h
w_idt.o: /opt/X11/include/X11/Object.h /opt/X11/include/X11/RectObj.h
w_idt.o: /opt/X11/include/X11/StringDefs.h
w_idt.o: /opt/X11/include/X11/Xaw/AsciiText.h /opt/X11/include/X11/Xaw/Text.h
w_idt.o: /opt/X11/include/X11/Xaw/Simple.h
w_idt.o: /opt/X11/include/X11/Xmu/Converters.h
w_idt.o: /opt/X11/include/X11/Xaw/TextSink.h
w_idt.o: /opt/X11/include/X11/Xaw/TextSrc.h
w_idt.o: /opt/X11/include/X11/Xaw/AsciiSrc.h
w_idt.o: /opt/X11/include/X11/Xaw/AsciiSink.h
w_idt.o: /opt/X11/include/X11/Xaw/MultiSrc.h
w_idt.o: /opt/X11/include/X11/Xaw/Command.h /opt/X11/include/X11/Xaw/Label.h
w_idt.o: /opt/X11/include/X11/Xaw/Dialog.h /opt/X11/include/X11/Xaw/Form.h
w_idt.o: /opt/X11/include/X11/Xaw/List.h
w_idt.o: /opt/X11/include/X11/Xaw/MenuButton.h
w_idt.o: /opt/X11/include/X11/Xaw/Scrollbar.h
w_idt.o: /opt/X11/include/X11/Xaw/StripChart.h
w_idt.o: /opt/X11/include/X11/Xaw/Paned.h
w_idt.o: /opt/X11/include/X11/Xaw/Cardinals.h ../../../.././include/ncarg/c.h
w_idt.o: idt.h text.h file.h display.h animate.h w_file.h w_display.h
w_idt.o: w_dialog.h xt_env.h
w_file.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
w_file.o: /usr/include/sys/_symbol_aliasing.h
w_file.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
w_file.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
w_file.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
w_file.o: /usr/include/i386/_types.h
w_file.o: /usr/include/sys/_pthread/_pthread_types.h
w_file.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
w_file.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
w_file.o: /usr/include/sys/_types/_int16_t.h
w_file.o: /usr/include/sys/_types/_int32_t.h
w_file.o: /usr/include/sys/_types/_int64_t.h
w_file.o: /usr/include/sys/_types/_u_int8_t.h
w_file.o: /usr/include/sys/_types/_u_int16_t.h
w_file.o: /usr/include/sys/_types/_u_int32_t.h
w_file.o: /usr/include/sys/_types/_u_int64_t.h
w_file.o: /usr/include/sys/_types/_intptr_t.h
w_file.o: /usr/include/sys/_types/_uintptr_t.h
w_file.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
w_file.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
w_file.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
w_file.o: /usr/include/secure/_common.h /usr/include/stdlib.h
w_file.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
w_file.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
w_file.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
w_file.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
w_file.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
w_file.o: /usr/include/mach/i386/_structs.h
w_file.o: /usr/include/sys/_pthread/_pthread_attr_t.h
w_file.o: /usr/include/sys/_types/_sigaltstack.h
w_file.o: /usr/include/sys/_types/_ucontext.h
w_file.o: /usr/include/sys/_types/_sigset_t.h
w_file.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
w_file.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
w_file.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
w_file.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
w_file.o: /usr/include/_types/_uintmax_t.h /usr/include/sys/_types/_timeval.h
w_file.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
w_file.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
w_file.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
w_file.o: /usr/include/sys/_types/_ct_rune_t.h
w_file.o: /usr/include/sys/_types/_rune_t.h
w_file.o: /usr/include/sys/_types/_wchar_t.h /usr/include/sys/_types/_dev_t.h
w_file.o: /usr/include/sys/_types/_mode_t.h /opt/X11/include/X11/Intrinsic.h
w_file.o: /opt/X11/include/X11/Xlib.h /usr/include/sys/types.h
w_file.o: /usr/include/sys/_types/_u_char.h
w_file.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
w_file.o: /usr/include/sys/_types/_caddr_t.h
w_file.o: /usr/include/sys/_types/_blkcnt_t.h
w_file.o: /usr/include/sys/_types/_blksize_t.h
w_file.o: /usr/include/sys/_types/_gid_t.h
w_file.o: /usr/include/sys/_types/_in_addr_t.h
w_file.o: /usr/include/sys/_types/_in_port_t.h
w_file.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
w_file.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_nlink_t.h
w_file.o: /usr/include/sys/_types/_clock_t.h
w_file.o: /usr/include/sys/_types/_time_t.h
w_file.o: /usr/include/sys/_types/_useconds_t.h
w_file.o: /usr/include/sys/_types/_suseconds_t.h
w_file.o: /usr/include/sys/_types/_rsize_t.h
w_file.o: /usr/include/sys/_types/_errno_t.h
w_file.o: /usr/include/sys/_types/_fd_def.h
w_file.o: /usr/include/sys/_types/_fd_setsize.h
w_file.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
w_file.o: /usr/include/sys/_types/_fd_zero.h
w_file.o: /usr/include/sys/_types/_fd_isset.h
w_file.o: /usr/include/sys/_types/_fd_copy.h
w_file.o: /usr/include/sys/_pthread/_pthread_cond_t.h
w_file.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
w_file.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
w_file.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
w_file.o: /usr/include/sys/_pthread/_pthread_once_t.h
w_file.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
w_file.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
w_file.o: /usr/include/sys/_pthread/_pthread_t.h
w_file.o: /usr/include/sys/_pthread/_pthread_key_t.h
w_file.o: /usr/include/sys/_types/_fsblkcnt_t.h
w_file.o: /usr/include/sys/_types/_fsfilcnt_t.h /opt/X11/include/X11/X.h
w_file.o: /opt/X11/include/X11/Xfuncproto.h /opt/X11/include/X11/Xosdefs.h
w_file.o: /usr/include/stddef.h /usr/include/sys/_types/_offsetof.h
w_file.o: /usr/include/sys/_types/_ptrdiff_t.h
w_file.o: /usr/include/sys/_types/_wint_t.h /opt/X11/include/X11/Xutil.h
w_file.o: /opt/X11/include/X11/keysym.h /opt/X11/include/X11/keysymdef.h
w_file.o: /opt/X11/include/X11/Xresource.h /usr/include/string.h
w_file.o: /usr/include/strings.h /usr/include/secure/_strings.h
w_file.o: /usr/include/secure/_string.h /opt/X11/include/X11/Core.h
w_file.o: /opt/X11/include/X11/Composite.h /opt/X11/include/X11/Constraint.h
w_file.o: /opt/X11/include/X11/Object.h /opt/X11/include/X11/RectObj.h
w_file.o: /opt/X11/include/X11/StringDefs.h /opt/X11/include/X11/Shell.h
w_file.o: /opt/X11/include/X11/SM/SMlib.h /opt/X11/include/X11/SM/SM.h
w_file.o: /opt/X11/include/X11/ICE/ICElib.h /opt/X11/include/X11/ICE/ICE.h
w_file.o: /opt/X11/include/X11/Xaw/Dialog.h /opt/X11/include/X11/Xaw/Form.h
w_file.o: /opt/X11/include/X11/Xaw/Paned.h
w_file.o: /opt/X11/include/X11/Xmu/Converters.h
w_file.o: /opt/X11/include/X11/Xaw/Scrollbar.h
w_file.o: /opt/X11/include/X11/Xaw/AsciiText.h
w_file.o: /opt/X11/include/X11/Xaw/Text.h /opt/X11/include/X11/Xaw/Simple.h
w_file.o: /opt/X11/include/X11/Xaw/TextSink.h
w_file.o: /opt/X11/include/X11/Xaw/TextSrc.h
w_file.o: /opt/X11/include/X11/Xaw/AsciiSrc.h
w_file.o: /opt/X11/include/X11/Xaw/AsciiSink.h
w_file.o: /opt/X11/include/X11/Xaw/MultiSrc.h
w_file.o: /opt/X11/include/X11/Xaw/Command.h /opt/X11/include/X11/Xaw/Label.h
w_file.o: ../../../.././include/ncarg/c.h idt.h file.h w_file.h
file.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
file.o: /usr/include/sys/_symbol_aliasing.h
file.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
file.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
file.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
file.o: /usr/include/i386/_types.h /usr/include/sys/_pthread/_pthread_types.h
file.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
file.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
file.o: /usr/include/sys/_types/_int16_t.h /usr/include/sys/_types/_int32_t.h
file.o: /usr/include/sys/_types/_int64_t.h
file.o: /usr/include/sys/_types/_u_int8_t.h
file.o: /usr/include/sys/_types/_u_int16_t.h
file.o: /usr/include/sys/_types/_u_int32_t.h
file.o: /usr/include/sys/_types/_u_int64_t.h
file.o: /usr/include/sys/_types/_intptr_t.h
file.o: /usr/include/sys/_types/_uintptr_t.h
file.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
file.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
file.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
file.o: /usr/include/secure/_common.h /usr/include/stdlib.h
file.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
file.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
file.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
file.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
file.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
file.o: /usr/include/mach/i386/_structs.h
file.o: /usr/include/sys/_pthread/_pthread_attr_t.h
file.o: /usr/include/sys/_types/_sigaltstack.h
file.o: /usr/include/sys/_types/_ucontext.h
file.o: /usr/include/sys/_types/_sigset_t.h /usr/include/sys/_types/_uid_t.h
file.o: /usr/include/sys/resource.h /usr/include/stdint.h
file.o: /usr/include/_types/_uint8_t.h /usr/include/_types/_uint16_t.h
file.o: /usr/include/_types/_uint32_t.h /usr/include/_types/_uint64_t.h
file.o: /usr/include/_types/_intmax_t.h /usr/include/_types/_uintmax_t.h
file.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
file.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
file.o: /usr/include/libkern/_OSByteOrder.h
file.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
file.o: /usr/include/sys/_types/_ct_rune_t.h
file.o: /usr/include/sys/_types/_rune_t.h /usr/include/sys/_types/_wchar_t.h
file.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_mode_t.h
file.o: /usr/include/string.h /usr/include/sys/_types/_rsize_t.h
file.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
file.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
file.o: ../../../.././include/ncarg/c.h /usr/include/sys/types.h
file.o: /usr/include/sys/_types/_u_char.h /usr/include/sys/_types/_u_short.h
file.o: /usr/include/sys/_types/_u_int.h /usr/include/sys/_types/_caddr_t.h
file.o: /usr/include/sys/_types/_blkcnt_t.h
file.o: /usr/include/sys/_types/_blksize_t.h /usr/include/sys/_types/_gid_t.h
file.o: /usr/include/sys/_types/_in_addr_t.h
file.o: /usr/include/sys/_types/_in_port_t.h /usr/include/sys/_types/_ino_t.h
file.o: /usr/include/sys/_types/_ino64_t.h /usr/include/sys/_types/_key_t.h
file.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_clock_t.h
file.o: /usr/include/sys/_types/_time_t.h
file.o: /usr/include/sys/_types/_useconds_t.h
file.o: /usr/include/sys/_types/_suseconds_t.h
file.o: /usr/include/sys/_types/_fd_def.h
file.o: /usr/include/sys/_types/_fd_setsize.h
file.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
file.o: /usr/include/sys/_types/_fd_zero.h
file.o: /usr/include/sys/_types/_fd_isset.h
file.o: /usr/include/sys/_types/_fd_copy.h
file.o: /usr/include/sys/_pthread/_pthread_cond_t.h
file.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
file.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
file.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
file.o: /usr/include/sys/_pthread/_pthread_once_t.h
file.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
file.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
file.o: /usr/include/sys/_pthread/_pthread_t.h
file.o: /usr/include/sys/_pthread/_pthread_key_t.h
file.o: /usr/include/sys/_types/_fsblkcnt_t.h
file.o: /usr/include/sys/_types/_fsfilcnt_t.h glob.h idt.h
file.o: /opt/X11/include/X11/Xlib.h /opt/X11/include/X11/X.h
file.o: /opt/X11/include/X11/Xfuncproto.h /opt/X11/include/X11/Xosdefs.h
file.o: /usr/include/stddef.h /usr/include/sys/_types/_offsetof.h
file.o: /usr/include/sys/_types/_ptrdiff_t.h
file.o: /usr/include/sys/_types/_wint_t.h /opt/X11/include/X11/Intrinsic.h
file.o: /opt/X11/include/X11/Xutil.h /opt/X11/include/X11/keysym.h
file.o: /opt/X11/include/X11/keysymdef.h /opt/X11/include/X11/Xresource.h
file.o: /opt/X11/include/X11/Core.h /opt/X11/include/X11/Composite.h
file.o: /opt/X11/include/X11/Constraint.h /opt/X11/include/X11/Object.h
file.o: /opt/X11/include/X11/RectObj.h
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
glob.o: /usr/include/sys/_types/_fsfilcnt_t.h glob.h
w_display.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
w_display.o: /usr/include/sys/_symbol_aliasing.h
w_display.o: /usr/include/sys/_posix_availability.h
w_display.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
w_display.o: /usr/include/_types.h /usr/include/sys/_types.h
w_display.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
w_display.o: /usr/include/sys/_pthread/_pthread_types.h
w_display.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
w_display.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
w_display.o: /usr/include/sys/_types/_int16_t.h
w_display.o: /usr/include/sys/_types/_int32_t.h
w_display.o: /usr/include/sys/_types/_int64_t.h
w_display.o: /usr/include/sys/_types/_u_int8_t.h
w_display.o: /usr/include/sys/_types/_u_int16_t.h
w_display.o: /usr/include/sys/_types/_u_int32_t.h
w_display.o: /usr/include/sys/_types/_u_int64_t.h
w_display.o: /usr/include/sys/_types/_intptr_t.h
w_display.o: /usr/include/sys/_types/_uintptr_t.h
w_display.o: /usr/include/sys/_types/_size_t.h
w_display.o: /usr/include/sys/_types/_null.h /usr/include/sys/stdio.h
w_display.o: /usr/include/sys/_types/_off_t.h
w_display.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
w_display.o: /usr/include/secure/_common.h /usr/include/stdlib.h
w_display.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
w_display.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
w_display.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
w_display.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
w_display.o: /usr/include/i386/_mcontext.h
w_display.o: /usr/include/mach/machine/_structs.h
w_display.o: /usr/include/mach/i386/_structs.h
w_display.o: /usr/include/sys/_pthread/_pthread_attr_t.h
w_display.o: /usr/include/sys/_types/_sigaltstack.h
w_display.o: /usr/include/sys/_types/_ucontext.h
w_display.o: /usr/include/sys/_types/_sigset_t.h
w_display.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
w_display.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
w_display.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
w_display.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
w_display.o: /usr/include/_types/_uintmax_t.h
w_display.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
w_display.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
w_display.o: /usr/include/libkern/_OSByteOrder.h
w_display.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
w_display.o: /usr/include/sys/_types/_ct_rune_t.h
w_display.o: /usr/include/sys/_types/_rune_t.h
w_display.o: /usr/include/sys/_types/_wchar_t.h
w_display.o: /usr/include/sys/_types/_dev_t.h
w_display.o: /usr/include/sys/_types/_mode_t.h
w_display.o: /opt/X11/include/X11/Intrinsic.h /opt/X11/include/X11/Xlib.h
w_display.o: /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
w_display.o: /usr/include/sys/_types/_u_short.h
w_display.o: /usr/include/sys/_types/_u_int.h
w_display.o: /usr/include/sys/_types/_caddr_t.h
w_display.o: /usr/include/sys/_types/_blkcnt_t.h
w_display.o: /usr/include/sys/_types/_blksize_t.h
w_display.o: /usr/include/sys/_types/_gid_t.h
w_display.o: /usr/include/sys/_types/_in_addr_t.h
w_display.o: /usr/include/sys/_types/_in_port_t.h
w_display.o: /usr/include/sys/_types/_ino_t.h
w_display.o: /usr/include/sys/_types/_ino64_t.h
w_display.o: /usr/include/sys/_types/_key_t.h
w_display.o: /usr/include/sys/_types/_nlink_t.h
w_display.o: /usr/include/sys/_types/_clock_t.h
w_display.o: /usr/include/sys/_types/_time_t.h
w_display.o: /usr/include/sys/_types/_useconds_t.h
w_display.o: /usr/include/sys/_types/_suseconds_t.h
w_display.o: /usr/include/sys/_types/_rsize_t.h
w_display.o: /usr/include/sys/_types/_errno_t.h
w_display.o: /usr/include/sys/_types/_fd_def.h
w_display.o: /usr/include/sys/_types/_fd_setsize.h
w_display.o: /usr/include/sys/_types/_fd_set.h
w_display.o: /usr/include/sys/_types/_fd_clr.h
w_display.o: /usr/include/sys/_types/_fd_zero.h
w_display.o: /usr/include/sys/_types/_fd_isset.h
w_display.o: /usr/include/sys/_types/_fd_copy.h
w_display.o: /usr/include/sys/_pthread/_pthread_cond_t.h
w_display.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
w_display.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
w_display.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
w_display.o: /usr/include/sys/_pthread/_pthread_once_t.h
w_display.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
w_display.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
w_display.o: /usr/include/sys/_pthread/_pthread_t.h
w_display.o: /usr/include/sys/_pthread/_pthread_key_t.h
w_display.o: /usr/include/sys/_types/_fsblkcnt_t.h
w_display.o: /usr/include/sys/_types/_fsfilcnt_t.h /opt/X11/include/X11/X.h
w_display.o: /opt/X11/include/X11/Xfuncproto.h /opt/X11/include/X11/Xosdefs.h
w_display.o: /usr/include/stddef.h /usr/include/sys/_types/_offsetof.h
w_display.o: /usr/include/sys/_types/_ptrdiff_t.h
w_display.o: /usr/include/sys/_types/_wint_t.h /opt/X11/include/X11/Xutil.h
w_display.o: /opt/X11/include/X11/keysym.h /opt/X11/include/X11/keysymdef.h
w_display.o: /opt/X11/include/X11/Xresource.h /usr/include/string.h
w_display.o: /usr/include/strings.h /usr/include/secure/_strings.h
w_display.o: /usr/include/secure/_string.h /opt/X11/include/X11/Core.h
w_display.o: /opt/X11/include/X11/Composite.h
w_display.o: /opt/X11/include/X11/Constraint.h /opt/X11/include/X11/Object.h
w_display.o: /opt/X11/include/X11/RectObj.h /opt/X11/include/X11/StringDefs.h
w_display.o: /opt/X11/include/X11/Shell.h /opt/X11/include/X11/SM/SMlib.h
w_display.o: /opt/X11/include/X11/SM/SM.h /opt/X11/include/X11/ICE/ICElib.h
w_display.o: /opt/X11/include/X11/ICE/ICE.h /opt/X11/include/X11/Xaw/Dialog.h
w_display.o: /opt/X11/include/X11/Xaw/Form.h /opt/X11/include/X11/Xaw/Paned.h
w_display.o: /opt/X11/include/X11/Xmu/Converters.h
w_display.o: /opt/X11/include/X11/Xaw/AsciiText.h
w_display.o: /opt/X11/include/X11/Xaw/Text.h
w_display.o: /opt/X11/include/X11/Xaw/Simple.h
w_display.o: /opt/X11/include/X11/Xaw/TextSink.h
w_display.o: /opt/X11/include/X11/Xaw/TextSrc.h
w_display.o: /opt/X11/include/X11/Xaw/AsciiSrc.h
w_display.o: /opt/X11/include/X11/Xaw/AsciiSink.h
w_display.o: /opt/X11/include/X11/Xaw/MultiSrc.h
w_display.o: /opt/X11/include/X11/Xaw/Command.h
w_display.o: /opt/X11/include/X11/Xaw/Label.h
w_display.o: /opt/X11/include/X11/Xaw/Toggle.h
w_display.o: /opt/X11/include/X11/Xaw/Scrollbar.h
w_display.o: /opt/X11/include/X11/Xaw/MenuButton.h
w_display.o: /opt/X11/include/X11/Xaw/SimpleMenu.h
w_display.o: /opt/X11/include/X11/Xaw/Sme.h /opt/X11/include/X11/Xaw/SmeBSB.h
w_display.o: ../../../.././include/ncarg/c.h idt.h w_dialog.h display.h
w_display.o: animate.h talkto.h bits.h commands.h /usr/include/signal.h
w_display.o: scroll.h xrubber.h
display.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
display.o: /usr/include/sys/_symbol_aliasing.h
display.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
display.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
display.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
display.o: /usr/include/i386/_types.h
display.o: /usr/include/sys/_pthread/_pthread_types.h
display.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
display.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
display.o: /usr/include/sys/_types/_int16_t.h
display.o: /usr/include/sys/_types/_int32_t.h
display.o: /usr/include/sys/_types/_int64_t.h
display.o: /usr/include/sys/_types/_u_int8_t.h
display.o: /usr/include/sys/_types/_u_int16_t.h
display.o: /usr/include/sys/_types/_u_int32_t.h
display.o: /usr/include/sys/_types/_u_int64_t.h
display.o: /usr/include/sys/_types/_intptr_t.h
display.o: /usr/include/sys/_types/_uintptr_t.h
display.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
display.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
display.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
display.o: /usr/include/secure/_common.h /usr/include/fcntl.h
display.o: /usr/include/sys/fcntl.h /usr/include/sys/_types/_mode_t.h
display.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_o_sync.h
display.o: /usr/include/sys/_types/_o_dsync.h
display.o: /usr/include/sys/_types/_seek_set.h
display.o: /usr/include/sys/_types/_s_ifmt.h
display.o: /usr/include/sys/_types/_timespec.h
display.o: /usr/include/sys/_types/_filesec_t.h /usr/include/string.h
display.o: /usr/include/sys/_types/_rsize_t.h
display.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
display.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
display.o: ../../../.././include/ncarg/c.h /usr/include/sys/types.h
display.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
display.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
display.o: /usr/include/libkern/_OSByteOrder.h
display.o: /usr/include/libkern/i386/_OSByteOrder.h
display.o: /usr/include/sys/_types/_u_char.h
display.o: /usr/include/sys/_types/_u_short.h
display.o: /usr/include/sys/_types/_u_int.h
display.o: /usr/include/sys/_types/_caddr_t.h
display.o: /usr/include/sys/_types/_dev_t.h
display.o: /usr/include/sys/_types/_blkcnt_t.h
display.o: /usr/include/sys/_types/_blksize_t.h
display.o: /usr/include/sys/_types/_gid_t.h
display.o: /usr/include/sys/_types/_in_addr_t.h
display.o: /usr/include/sys/_types/_in_port_t.h
display.o: /usr/include/sys/_types/_ino_t.h
display.o: /usr/include/sys/_types/_ino64_t.h
display.o: /usr/include/sys/_types/_key_t.h
display.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
display.o: /usr/include/sys/_types/_uid_t.h
display.o: /usr/include/sys/_types/_clock_t.h
display.o: /usr/include/sys/_types/_time_t.h
display.o: /usr/include/sys/_types/_useconds_t.h
display.o: /usr/include/sys/_types/_suseconds_t.h
display.o: /usr/include/sys/_types/_fd_def.h
display.o: /usr/include/sys/_types/_fd_setsize.h
display.o: /usr/include/sys/_types/_fd_set.h
display.o: /usr/include/sys/_types/_fd_clr.h
display.o: /usr/include/sys/_types/_fd_zero.h
display.o: /usr/include/sys/_types/_fd_isset.h
display.o: /usr/include/sys/_types/_fd_copy.h
display.o: /usr/include/sys/_pthread/_pthread_attr_t.h
display.o: /usr/include/sys/_pthread/_pthread_cond_t.h
display.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
display.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
display.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
display.o: /usr/include/sys/_pthread/_pthread_once_t.h
display.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
display.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
display.o: /usr/include/sys/_pthread/_pthread_t.h
display.o: /usr/include/sys/_pthread/_pthread_key_t.h
display.o: /usr/include/sys/_types/_fsblkcnt_t.h
display.o: /usr/include/sys/_types/_fsfilcnt_t.h display.h
display.o: /opt/X11/include/X11/Intrinsic.h /opt/X11/include/X11/Xlib.h
display.o: /opt/X11/include/X11/X.h /opt/X11/include/X11/Xfuncproto.h
display.o: /opt/X11/include/X11/Xosdefs.h /usr/include/stddef.h
display.o: /usr/include/sys/_types/_offsetof.h
display.o: /usr/include/sys/_types/_ptrdiff_t.h
display.o: /usr/include/sys/_types/_wchar_t.h
display.o: /usr/include/sys/_types/_wint_t.h /opt/X11/include/X11/Xutil.h
display.o: /opt/X11/include/X11/keysym.h /opt/X11/include/X11/keysymdef.h
display.o: /opt/X11/include/X11/Xresource.h /opt/X11/include/X11/Core.h
display.o: /opt/X11/include/X11/Composite.h /opt/X11/include/X11/Constraint.h
display.o: /opt/X11/include/X11/Object.h /opt/X11/include/X11/RectObj.h
display.o: animate.h talkto.h
w_dialog.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
w_dialog.o: /usr/include/sys/_symbol_aliasing.h
w_dialog.o: /usr/include/sys/_posix_availability.h
w_dialog.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
w_dialog.o: /usr/include/_types.h /usr/include/sys/_types.h
w_dialog.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
w_dialog.o: /usr/include/sys/_pthread/_pthread_types.h
w_dialog.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
w_dialog.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
w_dialog.o: /usr/include/sys/_types/_int16_t.h
w_dialog.o: /usr/include/sys/_types/_int32_t.h
w_dialog.o: /usr/include/sys/_types/_int64_t.h
w_dialog.o: /usr/include/sys/_types/_u_int8_t.h
w_dialog.o: /usr/include/sys/_types/_u_int16_t.h
w_dialog.o: /usr/include/sys/_types/_u_int32_t.h
w_dialog.o: /usr/include/sys/_types/_u_int64_t.h
w_dialog.o: /usr/include/sys/_types/_intptr_t.h
w_dialog.o: /usr/include/sys/_types/_uintptr_t.h
w_dialog.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
w_dialog.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
w_dialog.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
w_dialog.o: /usr/include/secure/_common.h /opt/X11/include/X11/Intrinsic.h
w_dialog.o: /opt/X11/include/X11/Xlib.h /usr/include/sys/types.h
w_dialog.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
w_dialog.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
w_dialog.o: /usr/include/libkern/_OSByteOrder.h
w_dialog.o: /usr/include/libkern/i386/_OSByteOrder.h
w_dialog.o: /usr/include/sys/_types/_u_char.h
w_dialog.o: /usr/include/sys/_types/_u_short.h
w_dialog.o: /usr/include/sys/_types/_u_int.h
w_dialog.o: /usr/include/sys/_types/_caddr_t.h
w_dialog.o: /usr/include/sys/_types/_dev_t.h
w_dialog.o: /usr/include/sys/_types/_blkcnt_t.h
w_dialog.o: /usr/include/sys/_types/_blksize_t.h
w_dialog.o: /usr/include/sys/_types/_gid_t.h
w_dialog.o: /usr/include/sys/_types/_in_addr_t.h
w_dialog.o: /usr/include/sys/_types/_in_port_t.h
w_dialog.o: /usr/include/sys/_types/_ino_t.h
w_dialog.o: /usr/include/sys/_types/_ino64_t.h
w_dialog.o: /usr/include/sys/_types/_key_t.h
w_dialog.o: /usr/include/sys/_types/_mode_t.h
w_dialog.o: /usr/include/sys/_types/_nlink_t.h
w_dialog.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
w_dialog.o: /usr/include/sys/_types/_uid_t.h
w_dialog.o: /usr/include/sys/_types/_clock_t.h
w_dialog.o: /usr/include/sys/_types/_time_t.h
w_dialog.o: /usr/include/sys/_types/_useconds_t.h
w_dialog.o: /usr/include/sys/_types/_suseconds_t.h
w_dialog.o: /usr/include/sys/_types/_rsize_t.h
w_dialog.o: /usr/include/sys/_types/_errno_t.h
w_dialog.o: /usr/include/sys/_types/_fd_def.h
w_dialog.o: /usr/include/sys/_types/_fd_setsize.h
w_dialog.o: /usr/include/sys/_types/_fd_set.h
w_dialog.o: /usr/include/sys/_types/_fd_clr.h
w_dialog.o: /usr/include/sys/_types/_fd_zero.h
w_dialog.o: /usr/include/sys/_types/_fd_isset.h
w_dialog.o: /usr/include/sys/_types/_fd_copy.h
w_dialog.o: /usr/include/sys/_pthread/_pthread_attr_t.h
w_dialog.o: /usr/include/sys/_pthread/_pthread_cond_t.h
w_dialog.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
w_dialog.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
w_dialog.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
w_dialog.o: /usr/include/sys/_pthread/_pthread_once_t.h
w_dialog.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
w_dialog.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
w_dialog.o: /usr/include/sys/_pthread/_pthread_t.h
w_dialog.o: /usr/include/sys/_pthread/_pthread_key_t.h
w_dialog.o: /usr/include/sys/_types/_fsblkcnt_t.h
w_dialog.o: /usr/include/sys/_types/_fsfilcnt_t.h /opt/X11/include/X11/X.h
w_dialog.o: /opt/X11/include/X11/Xfuncproto.h /opt/X11/include/X11/Xosdefs.h
w_dialog.o: /usr/include/stddef.h /usr/include/sys/_types/_offsetof.h
w_dialog.o: /usr/include/sys/_types/_ptrdiff_t.h
w_dialog.o: /usr/include/sys/_types/_wchar_t.h
w_dialog.o: /usr/include/sys/_types/_wint_t.h /opt/X11/include/X11/Xutil.h
w_dialog.o: /opt/X11/include/X11/keysym.h /opt/X11/include/X11/keysymdef.h
w_dialog.o: /opt/X11/include/X11/Xresource.h /usr/include/string.h
w_dialog.o: /usr/include/strings.h /usr/include/secure/_strings.h
w_dialog.o: /usr/include/secure/_string.h /opt/X11/include/X11/Core.h
w_dialog.o: /opt/X11/include/X11/Composite.h
w_dialog.o: /opt/X11/include/X11/Constraint.h /opt/X11/include/X11/Object.h
w_dialog.o: /opt/X11/include/X11/RectObj.h /opt/X11/include/X11/StringDefs.h
w_dialog.o: /opt/X11/include/X11/Shell.h /opt/X11/include/X11/SM/SMlib.h
w_dialog.o: /opt/X11/include/X11/SM/SM.h /opt/X11/include/X11/ICE/ICElib.h
w_dialog.o: /opt/X11/include/X11/ICE/ICE.h /opt/X11/include/X11/Xaw/Dialog.h
w_dialog.o: /opt/X11/include/X11/Xaw/Form.h
w_dialog.o: /opt/X11/include/X11/Xaw/Command.h
w_dialog.o: /opt/X11/include/X11/Xaw/Label.h
w_dialog.o: /opt/X11/include/X11/Xaw/Simple.h
w_dialog.o: /opt/X11/include/X11/Xmu/Converters.h
w_dialog.o: ../../../.././include/ncarg/c.h idt.h w_dialog.h
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
commands.o: /usr/include/secure/_common.h /usr/include/sys/types.h
commands.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
commands.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
commands.o: /usr/include/libkern/_OSByteOrder.h
commands.o: /usr/include/libkern/i386/_OSByteOrder.h
commands.o: /usr/include/sys/_types/_u_char.h
commands.o: /usr/include/sys/_types/_u_short.h
commands.o: /usr/include/sys/_types/_u_int.h
commands.o: /usr/include/sys/_types/_caddr_t.h
commands.o: /usr/include/sys/_types/_dev_t.h
commands.o: /usr/include/sys/_types/_blkcnt_t.h
commands.o: /usr/include/sys/_types/_blksize_t.h
commands.o: /usr/include/sys/_types/_gid_t.h
commands.o: /usr/include/sys/_types/_in_addr_t.h
commands.o: /usr/include/sys/_types/_in_port_t.h
commands.o: /usr/include/sys/_types/_ino_t.h
commands.o: /usr/include/sys/_types/_ino64_t.h
commands.o: /usr/include/sys/_types/_key_t.h
commands.o: /usr/include/sys/_types/_mode_t.h
commands.o: /usr/include/sys/_types/_nlink_t.h
commands.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
commands.o: /usr/include/sys/_types/_uid_t.h
commands.o: /usr/include/sys/_types/_clock_t.h
commands.o: /usr/include/sys/_types/_time_t.h
commands.o: /usr/include/sys/_types/_useconds_t.h
commands.o: /usr/include/sys/_types/_suseconds_t.h
commands.o: /usr/include/sys/_types/_rsize_t.h
commands.o: /usr/include/sys/_types/_errno_t.h
commands.o: /usr/include/sys/_types/_fd_def.h
commands.o: /usr/include/sys/_types/_fd_setsize.h
commands.o: /usr/include/sys/_types/_fd_set.h
commands.o: /usr/include/sys/_types/_fd_clr.h
commands.o: /usr/include/sys/_types/_fd_zero.h
commands.o: /usr/include/sys/_types/_fd_isset.h
commands.o: /usr/include/sys/_types/_fd_copy.h
commands.o: /usr/include/sys/_pthread/_pthread_attr_t.h
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
commands.o: /usr/include/sys/signal.h /usr/include/machine/signal.h
commands.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
commands.o: /usr/include/i386/_mcontext.h
commands.o: /usr/include/mach/machine/_structs.h
commands.o: /usr/include/mach/i386/_structs.h
commands.o: /usr/include/sys/_types/_sigaltstack.h
commands.o: /usr/include/sys/_types/_ucontext.h
commands.o: /usr/include/sys/_types/_sigset_t.h /usr/include/string.h
commands.o: /usr/include/strings.h /usr/include/secure/_strings.h
commands.o: /usr/include/secure/_string.h display.h
commands.o: /opt/X11/include/X11/Intrinsic.h /opt/X11/include/X11/Xlib.h
commands.o: /opt/X11/include/X11/X.h /opt/X11/include/X11/Xfuncproto.h
commands.o: /opt/X11/include/X11/Xosdefs.h /usr/include/stddef.h
commands.o: /usr/include/sys/_types/_offsetof.h
commands.o: /usr/include/sys/_types/_ptrdiff_t.h
commands.o: /usr/include/sys/_types/_wchar_t.h
commands.o: /usr/include/sys/_types/_wint_t.h /opt/X11/include/X11/Xutil.h
commands.o: /opt/X11/include/X11/keysym.h /opt/X11/include/X11/keysymdef.h
commands.o: /opt/X11/include/X11/Xresource.h /opt/X11/include/X11/Core.h
commands.o: /opt/X11/include/X11/Composite.h
commands.o: /opt/X11/include/X11/Constraint.h /opt/X11/include/X11/Object.h
commands.o: /opt/X11/include/X11/RectObj.h animate.h
commands.o: ../../../.././include/ncarg/c.h commands.h talkto.h
talkto.o: /usr/include/stdlib.h /usr/include/Availability.h
talkto.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
talkto.o: /usr/include/sys/_types.h /usr/include/sys/cdefs.h
talkto.o: /usr/include/sys/_symbol_aliasing.h
talkto.o: /usr/include/sys/_posix_availability.h
talkto.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
talkto.o: /usr/include/sys/_pthread/_pthread_types.h /usr/include/sys/wait.h
talkto.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_id_t.h
talkto.o: /usr/include/sys/signal.h /usr/include/sys/appleapiopts.h
talkto.o: /usr/include/machine/signal.h /usr/include/i386/signal.h
talkto.o: /usr/include/machine/_mcontext.h /usr/include/i386/_mcontext.h
talkto.o: /usr/include/mach/machine/_structs.h
talkto.o: /usr/include/mach/i386/_structs.h /usr/include/machine/types.h
talkto.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
talkto.o: /usr/include/sys/_types/_int16_t.h
talkto.o: /usr/include/sys/_types/_int32_t.h
talkto.o: /usr/include/sys/_types/_int64_t.h
talkto.o: /usr/include/sys/_types/_u_int8_t.h
talkto.o: /usr/include/sys/_types/_u_int16_t.h
talkto.o: /usr/include/sys/_types/_u_int32_t.h
talkto.o: /usr/include/sys/_types/_u_int64_t.h
talkto.o: /usr/include/sys/_types/_intptr_t.h
talkto.o: /usr/include/sys/_types/_uintptr_t.h
talkto.o: /usr/include/sys/_pthread/_pthread_attr_t.h
talkto.o: /usr/include/sys/_types/_sigaltstack.h
talkto.o: /usr/include/sys/_types/_ucontext.h
talkto.o: /usr/include/sys/_types/_sigset_t.h
talkto.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_uid_t.h
talkto.o: /usr/include/sys/resource.h /usr/include/stdint.h
talkto.o: /usr/include/_types/_uint8_t.h /usr/include/_types/_uint16_t.h
talkto.o: /usr/include/_types/_uint32_t.h /usr/include/_types/_uint64_t.h
talkto.o: /usr/include/_types/_intmax_t.h /usr/include/_types/_uintmax_t.h
talkto.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
talkto.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
talkto.o: /usr/include/libkern/_OSByteOrder.h
talkto.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
talkto.o: /usr/include/sys/_types/_ct_rune_t.h
talkto.o: /usr/include/sys/_types/_rune_t.h
talkto.o: /usr/include/sys/_types/_wchar_t.h /usr/include/sys/_types/_null.h
talkto.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_mode_t.h
talkto.o: /usr/include/stdio.h /usr/include/sys/_types/_va_list.h
talkto.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
talkto.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
talkto.o: /usr/include/secure/_common.h /usr/include/sys/types.h
talkto.o: /usr/include/sys/_types/_u_char.h
talkto.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
talkto.o: /usr/include/sys/_types/_caddr_t.h
talkto.o: /usr/include/sys/_types/_blkcnt_t.h
talkto.o: /usr/include/sys/_types/_blksize_t.h
talkto.o: /usr/include/sys/_types/_gid_t.h
talkto.o: /usr/include/sys/_types/_in_addr_t.h
talkto.o: /usr/include/sys/_types/_in_port_t.h
talkto.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
talkto.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_nlink_t.h
talkto.o: /usr/include/sys/_types/_clock_t.h
talkto.o: /usr/include/sys/_types/_time_t.h
talkto.o: /usr/include/sys/_types/_useconds_t.h
talkto.o: /usr/include/sys/_types/_suseconds_t.h
talkto.o: /usr/include/sys/_types/_rsize_t.h
talkto.o: /usr/include/sys/_types/_errno_t.h
talkto.o: /usr/include/sys/_types/_fd_def.h
talkto.o: /usr/include/sys/_types/_fd_setsize.h
talkto.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
talkto.o: /usr/include/sys/_types/_fd_zero.h
talkto.o: /usr/include/sys/_types/_fd_isset.h
talkto.o: /usr/include/sys/_types/_fd_copy.h
talkto.o: /usr/include/sys/_pthread/_pthread_cond_t.h
talkto.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
talkto.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
talkto.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
talkto.o: /usr/include/sys/_pthread/_pthread_once_t.h
talkto.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
talkto.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
talkto.o: /usr/include/sys/_pthread/_pthread_t.h
talkto.o: /usr/include/sys/_pthread/_pthread_key_t.h
talkto.o: /usr/include/sys/_types/_fsblkcnt_t.h
talkto.o: /usr/include/sys/_types/_fsfilcnt_t.h /usr/include/signal.h
talkto.o: /usr/include/errno.h /usr/include/sys/errno.h /usr/include/fcntl.h
talkto.o: /usr/include/sys/fcntl.h /usr/include/sys/_types/_o_sync.h
talkto.o: /usr/include/sys/_types/_o_dsync.h
talkto.o: /usr/include/sys/_types/_seek_set.h
talkto.o: /usr/include/sys/_types/_s_ifmt.h
talkto.o: /usr/include/sys/_types/_timespec.h
talkto.o: /usr/include/sys/_types/_filesec_t.h /usr/include/string.h
talkto.o: /usr/include/strings.h /usr/include/secure/_strings.h
talkto.o: /usr/include/secure/_string.h /usr/include/sys/time.h
talkto.o: /usr/include/sys/_types/_timeval64.h /usr/include/time.h
talkto.o: /usr/include/sys/_select.h text.h /opt/X11/include/X11/Intrinsic.h
talkto.o: /opt/X11/include/X11/Xlib.h /opt/X11/include/X11/X.h
talkto.o: /opt/X11/include/X11/Xfuncproto.h /opt/X11/include/X11/Xosdefs.h
talkto.o: /usr/include/stddef.h /usr/include/sys/_types/_offsetof.h
talkto.o: /usr/include/sys/_types/_ptrdiff_t.h
talkto.o: /usr/include/sys/_types/_wint_t.h /opt/X11/include/X11/Xutil.h
talkto.o: /opt/X11/include/X11/keysym.h /opt/X11/include/X11/keysymdef.h
talkto.o: /opt/X11/include/X11/Xresource.h /opt/X11/include/X11/Core.h
talkto.o: /opt/X11/include/X11/Composite.h /opt/X11/include/X11/Constraint.h
talkto.o: /opt/X11/include/X11/Object.h /opt/X11/include/X11/RectObj.h
talkto.o: /usr/include/sys/param.h /usr/include/sys/syslimits.h
talkto.o: /usr/include/machine/param.h /usr/include/i386/param.h
talkto.o: /usr/include/i386/_param.h /usr/include/limits.h
talkto.o: /usr/include/machine/limits.h /usr/include/i386/limits.h
talkto.o: /usr/include/i386/_limits.h /usr/include/sys/select.h
talkto.o: ../../../.././include/ncarg/c.h display.h animate.h talkto.h
xrubber.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
xrubber.o: /usr/include/sys/_symbol_aliasing.h
xrubber.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
xrubber.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
xrubber.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
xrubber.o: /usr/include/i386/_types.h
xrubber.o: /usr/include/sys/_pthread/_pthread_types.h
xrubber.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
xrubber.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
xrubber.o: /usr/include/sys/_types/_int16_t.h
xrubber.o: /usr/include/sys/_types/_int32_t.h
xrubber.o: /usr/include/sys/_types/_int64_t.h
xrubber.o: /usr/include/sys/_types/_u_int8_t.h
xrubber.o: /usr/include/sys/_types/_u_int16_t.h
xrubber.o: /usr/include/sys/_types/_u_int32_t.h
xrubber.o: /usr/include/sys/_types/_u_int64_t.h
xrubber.o: /usr/include/sys/_types/_intptr_t.h
xrubber.o: /usr/include/sys/_types/_uintptr_t.h
xrubber.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
xrubber.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
xrubber.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
xrubber.o: /usr/include/secure/_common.h /usr/include/assert.h
xrubber.o: /opt/X11/include/X11/Xlib.h /usr/include/sys/types.h
xrubber.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
xrubber.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
xrubber.o: /usr/include/libkern/_OSByteOrder.h
xrubber.o: /usr/include/libkern/i386/_OSByteOrder.h
xrubber.o: /usr/include/sys/_types/_u_char.h
xrubber.o: /usr/include/sys/_types/_u_short.h
xrubber.o: /usr/include/sys/_types/_u_int.h
xrubber.o: /usr/include/sys/_types/_caddr_t.h
xrubber.o: /usr/include/sys/_types/_dev_t.h
xrubber.o: /usr/include/sys/_types/_blkcnt_t.h
xrubber.o: /usr/include/sys/_types/_blksize_t.h
xrubber.o: /usr/include/sys/_types/_gid_t.h
xrubber.o: /usr/include/sys/_types/_in_addr_t.h
xrubber.o: /usr/include/sys/_types/_in_port_t.h
xrubber.o: /usr/include/sys/_types/_ino_t.h
xrubber.o: /usr/include/sys/_types/_ino64_t.h
xrubber.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
xrubber.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
xrubber.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
xrubber.o: /usr/include/sys/_types/_clock_t.h
xrubber.o: /usr/include/sys/_types/_time_t.h
xrubber.o: /usr/include/sys/_types/_useconds_t.h
xrubber.o: /usr/include/sys/_types/_suseconds_t.h
xrubber.o: /usr/include/sys/_types/_rsize_t.h
xrubber.o: /usr/include/sys/_types/_errno_t.h
xrubber.o: /usr/include/sys/_types/_fd_def.h
xrubber.o: /usr/include/sys/_types/_fd_setsize.h
xrubber.o: /usr/include/sys/_types/_fd_set.h
xrubber.o: /usr/include/sys/_types/_fd_clr.h
xrubber.o: /usr/include/sys/_types/_fd_zero.h
xrubber.o: /usr/include/sys/_types/_fd_isset.h
xrubber.o: /usr/include/sys/_types/_fd_copy.h
xrubber.o: /usr/include/sys/_pthread/_pthread_attr_t.h
xrubber.o: /usr/include/sys/_pthread/_pthread_cond_t.h
xrubber.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
xrubber.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
xrubber.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
xrubber.o: /usr/include/sys/_pthread/_pthread_once_t.h
xrubber.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
xrubber.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
xrubber.o: /usr/include/sys/_pthread/_pthread_t.h
xrubber.o: /usr/include/sys/_pthread/_pthread_key_t.h
xrubber.o: /usr/include/sys/_types/_fsblkcnt_t.h
xrubber.o: /usr/include/sys/_types/_fsfilcnt_t.h /opt/X11/include/X11/X.h
xrubber.o: /opt/X11/include/X11/Xfuncproto.h /opt/X11/include/X11/Xosdefs.h
xrubber.o: /usr/include/stddef.h /usr/include/sys/_types/_offsetof.h
xrubber.o: /usr/include/sys/_types/_ptrdiff_t.h
xrubber.o: /usr/include/sys/_types/_wchar_t.h
xrubber.o: /usr/include/sys/_types/_wint_t.h /opt/X11/include/X11/Xutil.h
xrubber.o: /opt/X11/include/X11/keysym.h /opt/X11/include/X11/keysymdef.h
xrubber.o: /opt/X11/include/X11/cursorfont.h
xrubber.o: /opt/X11/include/X11/Xmu/WinUtil.h xrubber.h
scroll.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
scroll.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
scroll.o: /usr/include/sys/_posix_availability.h /usr/include/machine/types.h
scroll.o: /usr/include/i386/types.h /usr/include/i386/_types.h
scroll.o: /usr/include/sys/_types/_int8_t.h
scroll.o: /usr/include/sys/_types/_int16_t.h
scroll.o: /usr/include/sys/_types/_int32_t.h
scroll.o: /usr/include/sys/_types/_int64_t.h
scroll.o: /usr/include/sys/_types/_u_int8_t.h
scroll.o: /usr/include/sys/_types/_u_int16_t.h
scroll.o: /usr/include/sys/_types/_u_int32_t.h
scroll.o: /usr/include/sys/_types/_u_int64_t.h
scroll.o: /usr/include/sys/_types/_intptr_t.h
scroll.o: /usr/include/sys/_types/_uintptr_t.h /usr/include/sys/_types.h
scroll.o: /usr/include/machine/_types.h
scroll.o: /usr/include/sys/_pthread/_pthread_types.h
scroll.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
scroll.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
scroll.o: /usr/include/libkern/i386/_OSByteOrder.h
scroll.o: /usr/include/sys/_types/_u_char.h
scroll.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
scroll.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
scroll.o: /usr/include/sys/_types/_blkcnt_t.h
scroll.o: /usr/include/sys/_types/_blksize_t.h
scroll.o: /usr/include/sys/_types/_gid_t.h
scroll.o: /usr/include/sys/_types/_in_addr_t.h
scroll.o: /usr/include/sys/_types/_in_port_t.h
scroll.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
scroll.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
scroll.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
scroll.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_off_t.h
scroll.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/_types/_clock_t.h
scroll.o: /usr/include/sys/_types/_size_t.h
scroll.o: /usr/include/sys/_types/_ssize_t.h
scroll.o: /usr/include/sys/_types/_time_t.h
scroll.o: /usr/include/sys/_types/_useconds_t.h
scroll.o: /usr/include/sys/_types/_suseconds_t.h
scroll.o: /usr/include/sys/_types/_rsize_t.h
scroll.o: /usr/include/sys/_types/_errno_t.h
scroll.o: /usr/include/sys/_types/_fd_def.h
scroll.o: /usr/include/sys/_types/_fd_setsize.h
scroll.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
scroll.o: /usr/include/sys/_types/_fd_zero.h
scroll.o: /usr/include/sys/_types/_fd_isset.h
scroll.o: /usr/include/sys/_types/_fd_copy.h
scroll.o: /usr/include/sys/_pthread/_pthread_attr_t.h
scroll.o: /usr/include/sys/_pthread/_pthread_cond_t.h
scroll.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
scroll.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
scroll.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
scroll.o: /usr/include/sys/_pthread/_pthread_once_t.h
scroll.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
scroll.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
scroll.o: /usr/include/sys/_pthread/_pthread_t.h
scroll.o: /usr/include/sys/_pthread/_pthread_key_t.h
scroll.o: /usr/include/sys/_types/_fsblkcnt_t.h
scroll.o: /usr/include/sys/_types/_fsfilcnt_t.h commands.h
scroll.o: /usr/include/signal.h /usr/include/_types.h
scroll.o: /usr/include/sys/signal.h /usr/include/Availability.h
scroll.o: /usr/include/AvailabilityInternal.h /usr/include/machine/signal.h
scroll.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
scroll.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
scroll.o: /usr/include/mach/i386/_structs.h
scroll.o: /usr/include/sys/_types/_sigaltstack.h
scroll.o: /usr/include/sys/_types/_ucontext.h
scroll.o: /usr/include/sys/_types/_sigset_t.h display.h
scroll.o: /opt/X11/include/X11/Intrinsic.h /opt/X11/include/X11/Xlib.h
scroll.o: /opt/X11/include/X11/X.h /opt/X11/include/X11/Xfuncproto.h
scroll.o: /opt/X11/include/X11/Xosdefs.h /usr/include/stddef.h
scroll.o: /usr/include/sys/_types/_null.h /usr/include/sys/_types/_offsetof.h
scroll.o: /usr/include/sys/_types/_ptrdiff_t.h
scroll.o: /usr/include/sys/_types/_wchar_t.h
scroll.o: /usr/include/sys/_types/_wint_t.h /opt/X11/include/X11/Xutil.h
scroll.o: /opt/X11/include/X11/keysym.h /opt/X11/include/X11/keysymdef.h
scroll.o: /opt/X11/include/X11/Xresource.h /usr/include/string.h
scroll.o: /usr/include/strings.h /usr/include/secure/_strings.h
scroll.o: /usr/include/secure/_common.h /usr/include/secure/_string.h
scroll.o: /opt/X11/include/X11/Core.h /opt/X11/include/X11/Composite.h
scroll.o: /opt/X11/include/X11/Constraint.h /opt/X11/include/X11/Object.h
scroll.o: /opt/X11/include/X11/RectObj.h animate.h
scroll.o: ../../../.././include/ncarg/c.h /usr/include/stdio.h
scroll.o: /usr/include/sys/_types/_va_list.h /usr/include/sys/stdio.h
scroll.o: /usr/include/secure/_stdio.h scroll.h
w_text.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
w_text.o: /usr/include/sys/_symbol_aliasing.h
w_text.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
w_text.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
w_text.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
w_text.o: /usr/include/i386/_types.h
w_text.o: /usr/include/sys/_pthread/_pthread_types.h
w_text.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
w_text.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
w_text.o: /usr/include/sys/_types/_int16_t.h
w_text.o: /usr/include/sys/_types/_int32_t.h
w_text.o: /usr/include/sys/_types/_int64_t.h
w_text.o: /usr/include/sys/_types/_u_int8_t.h
w_text.o: /usr/include/sys/_types/_u_int16_t.h
w_text.o: /usr/include/sys/_types/_u_int32_t.h
w_text.o: /usr/include/sys/_types/_u_int64_t.h
w_text.o: /usr/include/sys/_types/_intptr_t.h
w_text.o: /usr/include/sys/_types/_uintptr_t.h
w_text.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
w_text.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
w_text.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
w_text.o: /usr/include/secure/_common.h /usr/include/stdlib.h
w_text.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
w_text.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
w_text.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
w_text.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
w_text.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
w_text.o: /usr/include/mach/i386/_structs.h
w_text.o: /usr/include/sys/_pthread/_pthread_attr_t.h
w_text.o: /usr/include/sys/_types/_sigaltstack.h
w_text.o: /usr/include/sys/_types/_ucontext.h
w_text.o: /usr/include/sys/_types/_sigset_t.h
w_text.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
w_text.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
w_text.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
w_text.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
w_text.o: /usr/include/_types/_uintmax_t.h /usr/include/sys/_types/_timeval.h
w_text.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
w_text.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
w_text.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
w_text.o: /usr/include/sys/_types/_ct_rune_t.h
w_text.o: /usr/include/sys/_types/_rune_t.h
w_text.o: /usr/include/sys/_types/_wchar_t.h /usr/include/sys/_types/_dev_t.h
w_text.o: /usr/include/sys/_types/_mode_t.h /usr/include/string.h
w_text.o: /usr/include/sys/_types/_rsize_t.h
w_text.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
w_text.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
w_text.o: /opt/X11/include/X11/Intrinsic.h /opt/X11/include/X11/Xlib.h
w_text.o: /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
w_text.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
w_text.o: /usr/include/sys/_types/_caddr_t.h
w_text.o: /usr/include/sys/_types/_blkcnt_t.h
w_text.o: /usr/include/sys/_types/_blksize_t.h
w_text.o: /usr/include/sys/_types/_gid_t.h
w_text.o: /usr/include/sys/_types/_in_addr_t.h
w_text.o: /usr/include/sys/_types/_in_port_t.h
w_text.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
w_text.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_nlink_t.h
w_text.o: /usr/include/sys/_types/_clock_t.h
w_text.o: /usr/include/sys/_types/_time_t.h
w_text.o: /usr/include/sys/_types/_useconds_t.h
w_text.o: /usr/include/sys/_types/_suseconds_t.h
w_text.o: /usr/include/sys/_types/_fd_def.h
w_text.o: /usr/include/sys/_types/_fd_setsize.h
w_text.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
w_text.o: /usr/include/sys/_types/_fd_zero.h
w_text.o: /usr/include/sys/_types/_fd_isset.h
w_text.o: /usr/include/sys/_types/_fd_copy.h
w_text.o: /usr/include/sys/_pthread/_pthread_cond_t.h
w_text.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
w_text.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
w_text.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
w_text.o: /usr/include/sys/_pthread/_pthread_once_t.h
w_text.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
w_text.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
w_text.o: /usr/include/sys/_pthread/_pthread_t.h
w_text.o: /usr/include/sys/_pthread/_pthread_key_t.h
w_text.o: /usr/include/sys/_types/_fsblkcnt_t.h
w_text.o: /usr/include/sys/_types/_fsfilcnt_t.h /opt/X11/include/X11/X.h
w_text.o: /opt/X11/include/X11/Xfuncproto.h /opt/X11/include/X11/Xosdefs.h
w_text.o: /usr/include/stddef.h /usr/include/sys/_types/_offsetof.h
w_text.o: /usr/include/sys/_types/_ptrdiff_t.h
w_text.o: /usr/include/sys/_types/_wint_t.h /opt/X11/include/X11/Xutil.h
w_text.o: /opt/X11/include/X11/keysym.h /opt/X11/include/X11/keysymdef.h
w_text.o: /opt/X11/include/X11/Xresource.h /opt/X11/include/X11/Core.h
w_text.o: /opt/X11/include/X11/Composite.h /opt/X11/include/X11/Constraint.h
w_text.o: /opt/X11/include/X11/Object.h /opt/X11/include/X11/RectObj.h
w_text.o: /opt/X11/include/X11/StringDefs.h
w_text.o: /opt/X11/include/X11/Xaw/AsciiText.h
w_text.o: /opt/X11/include/X11/Xaw/Text.h /opt/X11/include/X11/Xaw/Simple.h
w_text.o: /opt/X11/include/X11/Xmu/Converters.h
w_text.o: /opt/X11/include/X11/Xaw/TextSink.h
w_text.o: /opt/X11/include/X11/Xaw/TextSrc.h
w_text.o: /opt/X11/include/X11/Xaw/AsciiSrc.h
w_text.o: /opt/X11/include/X11/Xaw/AsciiSink.h
w_text.o: /opt/X11/include/X11/Xaw/MultiSrc.h ../../../.././include/ncarg/c.h
w_text.o: idt.h text.h
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
spooler.o: /usr/include/ctype.h /usr/include/runetype.h
spooler.o: /usr/include/sys/_types/_wint_t.h /usr/include/string.h
spooler.o: /usr/include/sys/_types/_rsize_t.h
spooler.o: /usr/include/sys/_types/_errno_t.h /usr/include/strings.h
spooler.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
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
spooler.o: /usr/include/sys/_pthread/_pthread_t.h
spooler.o: /usr/include/sys/_pthread/_pthread_key_t.h
spooler.o: /usr/include/sys/_types/_fsblkcnt_t.h
spooler.o: /usr/include/sys/_types/_fsfilcnt_t.h
w_icon.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
w_icon.o: /usr/include/sys/_symbol_aliasing.h
w_icon.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
w_icon.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
w_icon.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
w_icon.o: /usr/include/i386/_types.h
w_icon.o: /usr/include/sys/_pthread/_pthread_types.h
w_icon.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
w_icon.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
w_icon.o: /usr/include/sys/_types/_int16_t.h
w_icon.o: /usr/include/sys/_types/_int32_t.h
w_icon.o: /usr/include/sys/_types/_int64_t.h
w_icon.o: /usr/include/sys/_types/_u_int8_t.h
w_icon.o: /usr/include/sys/_types/_u_int16_t.h
w_icon.o: /usr/include/sys/_types/_u_int32_t.h
w_icon.o: /usr/include/sys/_types/_u_int64_t.h
w_icon.o: /usr/include/sys/_types/_intptr_t.h
w_icon.o: /usr/include/sys/_types/_uintptr_t.h
w_icon.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
w_icon.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
w_icon.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
w_icon.o: /usr/include/secure/_common.h /opt/X11/include/X11/Intrinsic.h
w_icon.o: /opt/X11/include/X11/Xlib.h /usr/include/sys/types.h
w_icon.o: /usr/include/sys/appleapiopts.h /usr/include/machine/endian.h
w_icon.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
w_icon.o: /usr/include/libkern/_OSByteOrder.h
w_icon.o: /usr/include/libkern/i386/_OSByteOrder.h
w_icon.o: /usr/include/sys/_types/_u_char.h
w_icon.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
w_icon.o: /usr/include/sys/_types/_caddr_t.h /usr/include/sys/_types/_dev_t.h
w_icon.o: /usr/include/sys/_types/_blkcnt_t.h
w_icon.o: /usr/include/sys/_types/_blksize_t.h
w_icon.o: /usr/include/sys/_types/_gid_t.h
w_icon.o: /usr/include/sys/_types/_in_addr_t.h
w_icon.o: /usr/include/sys/_types/_in_port_t.h
w_icon.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
w_icon.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
w_icon.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
w_icon.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
w_icon.o: /usr/include/sys/_types/_clock_t.h
w_icon.o: /usr/include/sys/_types/_time_t.h
w_icon.o: /usr/include/sys/_types/_useconds_t.h
w_icon.o: /usr/include/sys/_types/_suseconds_t.h
w_icon.o: /usr/include/sys/_types/_rsize_t.h
w_icon.o: /usr/include/sys/_types/_errno_t.h
w_icon.o: /usr/include/sys/_types/_fd_def.h
w_icon.o: /usr/include/sys/_types/_fd_setsize.h
w_icon.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
w_icon.o: /usr/include/sys/_types/_fd_zero.h
w_icon.o: /usr/include/sys/_types/_fd_isset.h
w_icon.o: /usr/include/sys/_types/_fd_copy.h
w_icon.o: /usr/include/sys/_pthread/_pthread_attr_t.h
w_icon.o: /usr/include/sys/_pthread/_pthread_cond_t.h
w_icon.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
w_icon.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
w_icon.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
w_icon.o: /usr/include/sys/_pthread/_pthread_once_t.h
w_icon.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
w_icon.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
w_icon.o: /usr/include/sys/_pthread/_pthread_t.h
w_icon.o: /usr/include/sys/_pthread/_pthread_key_t.h
w_icon.o: /usr/include/sys/_types/_fsblkcnt_t.h
w_icon.o: /usr/include/sys/_types/_fsfilcnt_t.h /opt/X11/include/X11/X.h
w_icon.o: /opt/X11/include/X11/Xfuncproto.h /opt/X11/include/X11/Xosdefs.h
w_icon.o: /usr/include/stddef.h /usr/include/sys/_types/_offsetof.h
w_icon.o: /usr/include/sys/_types/_ptrdiff_t.h
w_icon.o: /usr/include/sys/_types/_wchar_t.h
w_icon.o: /usr/include/sys/_types/_wint_t.h /opt/X11/include/X11/Xutil.h
w_icon.o: /opt/X11/include/X11/keysym.h /opt/X11/include/X11/keysymdef.h
w_icon.o: /opt/X11/include/X11/Xresource.h /usr/include/string.h
w_icon.o: /usr/include/strings.h /usr/include/secure/_strings.h
w_icon.o: /usr/include/secure/_string.h /opt/X11/include/X11/Core.h
w_icon.o: /opt/X11/include/X11/Composite.h /opt/X11/include/X11/Constraint.h
w_icon.o: /opt/X11/include/X11/Object.h /opt/X11/include/X11/RectObj.h
w_icon.o: /opt/X11/include/X11/StringDefs.h /opt/X11/include/X11/Shell.h
w_icon.o: /opt/X11/include/X11/SM/SMlib.h /opt/X11/include/X11/SM/SM.h
w_icon.o: /opt/X11/include/X11/ICE/ICElib.h /opt/X11/include/X11/ICE/ICE.h
w_icon.o: earth.xbm
xt_env.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
xt_env.o: /usr/include/sys/_symbol_aliasing.h
xt_env.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
xt_env.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
xt_env.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
xt_env.o: /usr/include/i386/_types.h
xt_env.o: /usr/include/sys/_pthread/_pthread_types.h
xt_env.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
xt_env.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
xt_env.o: /usr/include/sys/_types/_int16_t.h
xt_env.o: /usr/include/sys/_types/_int32_t.h
xt_env.o: /usr/include/sys/_types/_int64_t.h
xt_env.o: /usr/include/sys/_types/_u_int8_t.h
xt_env.o: /usr/include/sys/_types/_u_int16_t.h
xt_env.o: /usr/include/sys/_types/_u_int32_t.h
xt_env.o: /usr/include/sys/_types/_u_int64_t.h
xt_env.o: /usr/include/sys/_types/_intptr_t.h
xt_env.o: /usr/include/sys/_types/_uintptr_t.h
xt_env.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
xt_env.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
xt_env.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
xt_env.o: /usr/include/secure/_common.h /usr/include/stdlib.h
xt_env.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
xt_env.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
xt_env.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
xt_env.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
xt_env.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
xt_env.o: /usr/include/mach/i386/_structs.h
xt_env.o: /usr/include/sys/_pthread/_pthread_attr_t.h
xt_env.o: /usr/include/sys/_types/_sigaltstack.h
xt_env.o: /usr/include/sys/_types/_ucontext.h
xt_env.o: /usr/include/sys/_types/_sigset_t.h
xt_env.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
xt_env.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
xt_env.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
xt_env.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
xt_env.o: /usr/include/_types/_uintmax_t.h /usr/include/sys/_types/_timeval.h
xt_env.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
xt_env.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
xt_env.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
xt_env.o: /usr/include/sys/_types/_ct_rune_t.h
xt_env.o: /usr/include/sys/_types/_rune_t.h
xt_env.o: /usr/include/sys/_types/_wchar_t.h /usr/include/sys/_types/_dev_t.h
xt_env.o: /usr/include/sys/_types/_mode_t.h ../../../.././include/ncarg/c.h
xt_env.o: /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
xt_env.o: /usr/include/sys/_types/_u_short.h /usr/include/sys/_types/_u_int.h
xt_env.o: /usr/include/sys/_types/_caddr_t.h
xt_env.o: /usr/include/sys/_types/_blkcnt_t.h
xt_env.o: /usr/include/sys/_types/_blksize_t.h
xt_env.o: /usr/include/sys/_types/_gid_t.h
xt_env.o: /usr/include/sys/_types/_in_addr_t.h
xt_env.o: /usr/include/sys/_types/_in_port_t.h
xt_env.o: /usr/include/sys/_types/_ino_t.h /usr/include/sys/_types/_ino64_t.h
xt_env.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_nlink_t.h
xt_env.o: /usr/include/sys/_types/_clock_t.h
xt_env.o: /usr/include/sys/_types/_time_t.h
xt_env.o: /usr/include/sys/_types/_useconds_t.h
xt_env.o: /usr/include/sys/_types/_suseconds_t.h
xt_env.o: /usr/include/sys/_types/_rsize_t.h
xt_env.o: /usr/include/sys/_types/_errno_t.h
xt_env.o: /usr/include/sys/_types/_fd_def.h
xt_env.o: /usr/include/sys/_types/_fd_setsize.h
xt_env.o: /usr/include/sys/_types/_fd_set.h /usr/include/sys/_types/_fd_clr.h
xt_env.o: /usr/include/sys/_types/_fd_zero.h
xt_env.o: /usr/include/sys/_types/_fd_isset.h
xt_env.o: /usr/include/sys/_types/_fd_copy.h
xt_env.o: /usr/include/sys/_pthread/_pthread_cond_t.h
xt_env.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
xt_env.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
xt_env.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
xt_env.o: /usr/include/sys/_pthread/_pthread_once_t.h
xt_env.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
xt_env.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
xt_env.o: /usr/include/sys/_pthread/_pthread_t.h
xt_env.o: /usr/include/sys/_pthread/_pthread_key_t.h
xt_env.o: /usr/include/sys/_types/_fsblkcnt_t.h
xt_env.o: /usr/include/sys/_types/_fsfilcnt_t.h /usr/include/string.h
xt_env.o: /usr/include/strings.h /usr/include/secure/_strings.h
xt_env.o: /usr/include/secure/_string.h xt_env.h
animate.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
animate.o: /usr/include/sys/_symbol_aliasing.h
animate.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
animate.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
animate.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
animate.o: /usr/include/i386/_types.h
animate.o: /usr/include/sys/_pthread/_pthread_types.h
animate.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
animate.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
animate.o: /usr/include/sys/_types/_int16_t.h
animate.o: /usr/include/sys/_types/_int32_t.h
animate.o: /usr/include/sys/_types/_int64_t.h
animate.o: /usr/include/sys/_types/_u_int8_t.h
animate.o: /usr/include/sys/_types/_u_int16_t.h
animate.o: /usr/include/sys/_types/_u_int32_t.h
animate.o: /usr/include/sys/_types/_u_int64_t.h
animate.o: /usr/include/sys/_types/_intptr_t.h
animate.o: /usr/include/sys/_types/_uintptr_t.h
animate.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
animate.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
animate.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
animate.o: /usr/include/secure/_common.h /usr/include/stdlib.h
animate.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
animate.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
animate.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
animate.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
animate.o: /usr/include/i386/_mcontext.h /usr/include/mach/machine/_structs.h
animate.o: /usr/include/mach/i386/_structs.h
animate.o: /usr/include/sys/_pthread/_pthread_attr_t.h
animate.o: /usr/include/sys/_types/_sigaltstack.h
animate.o: /usr/include/sys/_types/_ucontext.h
animate.o: /usr/include/sys/_types/_sigset_t.h
animate.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
animate.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
animate.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
animate.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
animate.o: /usr/include/_types/_uintmax_t.h
animate.o: /usr/include/sys/_types/_timeval.h /usr/include/machine/endian.h
animate.o: /usr/include/i386/endian.h /usr/include/sys/_endian.h
animate.o: /usr/include/libkern/_OSByteOrder.h
animate.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
animate.o: /usr/include/sys/_types/_ct_rune_t.h
animate.o: /usr/include/sys/_types/_rune_t.h
animate.o: /usr/include/sys/_types/_wchar_t.h
animate.o: /usr/include/sys/_types/_dev_t.h /usr/include/sys/_types/_mode_t.h
animate.o: /usr/include/errno.h /usr/include/sys/errno.h
animate.o: /usr/include/sys/_types/_errno_t.h /opt/X11/include/X11/Xlib.h
animate.o: /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
animate.o: /usr/include/sys/_types/_u_short.h
animate.o: /usr/include/sys/_types/_u_int.h
animate.o: /usr/include/sys/_types/_caddr_t.h
animate.o: /usr/include/sys/_types/_blkcnt_t.h
animate.o: /usr/include/sys/_types/_blksize_t.h
animate.o: /usr/include/sys/_types/_gid_t.h
animate.o: /usr/include/sys/_types/_in_addr_t.h
animate.o: /usr/include/sys/_types/_in_port_t.h
animate.o: /usr/include/sys/_types/_ino_t.h
animate.o: /usr/include/sys/_types/_ino64_t.h
animate.o: /usr/include/sys/_types/_key_t.h
animate.o: /usr/include/sys/_types/_nlink_t.h
animate.o: /usr/include/sys/_types/_clock_t.h
animate.o: /usr/include/sys/_types/_time_t.h
animate.o: /usr/include/sys/_types/_useconds_t.h
animate.o: /usr/include/sys/_types/_suseconds_t.h
animate.o: /usr/include/sys/_types/_rsize_t.h
animate.o: /usr/include/sys/_types/_fd_def.h
animate.o: /usr/include/sys/_types/_fd_setsize.h
animate.o: /usr/include/sys/_types/_fd_set.h
animate.o: /usr/include/sys/_types/_fd_clr.h
animate.o: /usr/include/sys/_types/_fd_zero.h
animate.o: /usr/include/sys/_types/_fd_isset.h
animate.o: /usr/include/sys/_types/_fd_copy.h
animate.o: /usr/include/sys/_pthread/_pthread_cond_t.h
animate.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
animate.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
animate.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
animate.o: /usr/include/sys/_pthread/_pthread_once_t.h
animate.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
animate.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
animate.o: /usr/include/sys/_pthread/_pthread_t.h
animate.o: /usr/include/sys/_pthread/_pthread_key_t.h
animate.o: /usr/include/sys/_types/_fsblkcnt_t.h
animate.o: /usr/include/sys/_types/_fsfilcnt_t.h /opt/X11/include/X11/X.h
animate.o: /opt/X11/include/X11/Xfuncproto.h /opt/X11/include/X11/Xosdefs.h
animate.o: /usr/include/stddef.h /usr/include/sys/_types/_offsetof.h
animate.o: /usr/include/sys/_types/_ptrdiff_t.h
animate.o: /usr/include/sys/_types/_wint_t.h ../../../.././include/ncarg/c.h
animate.o: animate.h /opt/X11/include/X11/Intrinsic.h
animate.o: /opt/X11/include/X11/Xutil.h /opt/X11/include/X11/keysym.h
animate.o: /opt/X11/include/X11/keysymdef.h /opt/X11/include/X11/Xresource.h
animate.o: /usr/include/string.h /usr/include/strings.h
animate.o: /usr/include/secure/_strings.h /usr/include/secure/_string.h
animate.o: /opt/X11/include/X11/Core.h /opt/X11/include/X11/Composite.h
animate.o: /opt/X11/include/X11/Constraint.h /opt/X11/include/X11/Object.h
animate.o: /opt/X11/include/X11/RectObj.h
animate_cb.o: /usr/include/stdio.h /usr/include/sys/cdefs.h
animate_cb.o: /usr/include/sys/_symbol_aliasing.h
animate_cb.o: /usr/include/sys/_posix_availability.h
animate_cb.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
animate_cb.o: /usr/include/_types.h /usr/include/sys/_types.h
animate_cb.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
animate_cb.o: /usr/include/sys/_pthread/_pthread_types.h
animate_cb.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
animate_cb.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
animate_cb.o: /usr/include/sys/_types/_int16_t.h
animate_cb.o: /usr/include/sys/_types/_int32_t.h
animate_cb.o: /usr/include/sys/_types/_int64_t.h
animate_cb.o: /usr/include/sys/_types/_u_int8_t.h
animate_cb.o: /usr/include/sys/_types/_u_int16_t.h
animate_cb.o: /usr/include/sys/_types/_u_int32_t.h
animate_cb.o: /usr/include/sys/_types/_u_int64_t.h
animate_cb.o: /usr/include/sys/_types/_intptr_t.h
animate_cb.o: /usr/include/sys/_types/_uintptr_t.h
animate_cb.o: /usr/include/sys/_types/_size_t.h
animate_cb.o: /usr/include/sys/_types/_null.h /usr/include/sys/stdio.h
animate_cb.o: /usr/include/sys/_types/_off_t.h
animate_cb.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
animate_cb.o: /usr/include/secure/_common.h /usr/include/stdlib.h
animate_cb.o: /usr/include/sys/wait.h /usr/include/sys/_types/_pid_t.h
animate_cb.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/signal.h
animate_cb.o: /usr/include/sys/appleapiopts.h /usr/include/machine/signal.h
animate_cb.o: /usr/include/i386/signal.h /usr/include/machine/_mcontext.h
animate_cb.o: /usr/include/i386/_mcontext.h
animate_cb.o: /usr/include/mach/machine/_structs.h
animate_cb.o: /usr/include/mach/i386/_structs.h
animate_cb.o: /usr/include/sys/_pthread/_pthread_attr_t.h
animate_cb.o: /usr/include/sys/_types/_sigaltstack.h
animate_cb.o: /usr/include/sys/_types/_ucontext.h
animate_cb.o: /usr/include/sys/_types/_sigset_t.h
animate_cb.o: /usr/include/sys/_types/_uid_t.h /usr/include/sys/resource.h
animate_cb.o: /usr/include/stdint.h /usr/include/_types/_uint8_t.h
animate_cb.o: /usr/include/_types/_uint16_t.h /usr/include/_types/_uint32_t.h
animate_cb.o: /usr/include/_types/_uint64_t.h /usr/include/_types/_intmax_t.h
animate_cb.o: /usr/include/_types/_uintmax_t.h
animate_cb.o: /usr/include/sys/_types/_timeval.h
animate_cb.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
animate_cb.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
animate_cb.o: /usr/include/libkern/i386/_OSByteOrder.h /usr/include/alloca.h
animate_cb.o: /usr/include/sys/_types/_ct_rune_t.h
animate_cb.o: /usr/include/sys/_types/_rune_t.h
animate_cb.o: /usr/include/sys/_types/_wchar_t.h
animate_cb.o: /usr/include/sys/_types/_dev_t.h
animate_cb.o: /usr/include/sys/_types/_mode_t.h
animate_cb.o: /opt/X11/include/X11/Intrinsic.h /opt/X11/include/X11/Xlib.h
animate_cb.o: /usr/include/sys/types.h /usr/include/sys/_types/_u_char.h
animate_cb.o: /usr/include/sys/_types/_u_short.h
animate_cb.o: /usr/include/sys/_types/_u_int.h
animate_cb.o: /usr/include/sys/_types/_caddr_t.h
animate_cb.o: /usr/include/sys/_types/_blkcnt_t.h
animate_cb.o: /usr/include/sys/_types/_blksize_t.h
animate_cb.o: /usr/include/sys/_types/_gid_t.h
animate_cb.o: /usr/include/sys/_types/_in_addr_t.h
animate_cb.o: /usr/include/sys/_types/_in_port_t.h
animate_cb.o: /usr/include/sys/_types/_ino_t.h
animate_cb.o: /usr/include/sys/_types/_ino64_t.h
animate_cb.o: /usr/include/sys/_types/_key_t.h
animate_cb.o: /usr/include/sys/_types/_nlink_t.h
animate_cb.o: /usr/include/sys/_types/_clock_t.h
animate_cb.o: /usr/include/sys/_types/_time_t.h
animate_cb.o: /usr/include/sys/_types/_useconds_t.h
animate_cb.o: /usr/include/sys/_types/_suseconds_t.h
animate_cb.o: /usr/include/sys/_types/_rsize_t.h
animate_cb.o: /usr/include/sys/_types/_errno_t.h
animate_cb.o: /usr/include/sys/_types/_fd_def.h
animate_cb.o: /usr/include/sys/_types/_fd_setsize.h
animate_cb.o: /usr/include/sys/_types/_fd_set.h
animate_cb.o: /usr/include/sys/_types/_fd_clr.h
animate_cb.o: /usr/include/sys/_types/_fd_zero.h
animate_cb.o: /usr/include/sys/_types/_fd_isset.h
animate_cb.o: /usr/include/sys/_types/_fd_copy.h
animate_cb.o: /usr/include/sys/_pthread/_pthread_cond_t.h
animate_cb.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
animate_cb.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
animate_cb.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
animate_cb.o: /usr/include/sys/_pthread/_pthread_once_t.h
animate_cb.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
animate_cb.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
animate_cb.o: /usr/include/sys/_pthread/_pthread_t.h
animate_cb.o: /usr/include/sys/_pthread/_pthread_key_t.h
animate_cb.o: /usr/include/sys/_types/_fsblkcnt_t.h
animate_cb.o: /usr/include/sys/_types/_fsfilcnt_t.h /opt/X11/include/X11/X.h
animate_cb.o: /opt/X11/include/X11/Xfuncproto.h
animate_cb.o: /opt/X11/include/X11/Xosdefs.h /usr/include/stddef.h
animate_cb.o: /usr/include/sys/_types/_offsetof.h
animate_cb.o: /usr/include/sys/_types/_ptrdiff_t.h
animate_cb.o: /usr/include/sys/_types/_wint_t.h /opt/X11/include/X11/Xutil.h
animate_cb.o: /opt/X11/include/X11/keysym.h /opt/X11/include/X11/keysymdef.h
animate_cb.o: /opt/X11/include/X11/Xresource.h /usr/include/string.h
animate_cb.o: /usr/include/strings.h /usr/include/secure/_strings.h
animate_cb.o: /usr/include/secure/_string.h /opt/X11/include/X11/Core.h
animate_cb.o: /opt/X11/include/X11/Composite.h
animate_cb.o: /opt/X11/include/X11/Constraint.h /opt/X11/include/X11/Object.h
animate_cb.o: /opt/X11/include/X11/RectObj.h
animate_cb.o: /opt/X11/include/X11/StringDefs.h
animate_cb.o: ../../../.././include/ncarg/c.h animate.h talkto.h display.h
