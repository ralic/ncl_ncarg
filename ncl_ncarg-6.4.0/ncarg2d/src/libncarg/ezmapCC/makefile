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
CURRENT_DIR		= ./ncarg2d/src/libncarg/ezmapCC

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
#	$Id: yMakefile,v 1.3 2008-04-18 04:09:21 kennison Exp $
#

MYNAME	= ezmapC

SRCS =  c_mapblm.c c_mapdrw.c c_mapfst.c c_mapgrd.c c_mapgtc.c c_mapgti.c  	c_mapgtl.c c_mapgtr.c c_mapint.c c_mapiq.c  c_mapit.c  c_maplbl.c  	c_maplmb.c c_maplot.c c_mappos.c c_maproj.c c_maprs.c  c_maprst.c  	c_mapsav.c c_mapset.c c_mapstc.c c_mapsti.c c_mapstl.c c_mapstr.c  	c_maptri.c c_maptrn.c c_mapvec.c c_supcon.c c_supmap.c c_mapgci.c  	c_maptra.c c_mapiqa.c c_mapiqm.c c_mapita.c c_mapitm.c c_mapaci.c  	c_mapbla.c c_mapgrm.c c_mpgetc.c c_mpgeti.c c_mpgetl.c c_mpgetr.c  	c_mpsetc.c c_mpseti.c c_mpsetl.c c_mpsetr.c c_mapiqd.c c_mapitd.c  	c_mprset.c c_mpfnme.c c_mpglty.c c_mpiaty.c c_mpifnb.c c_mpilnb.c  	c_mpiola.c c_mpiosa.c c_mpipai.c c_mpipan.c c_mpipar.c c_mpisci.c  	c_mplnam.c c_mplndm.c c_mplndr.c c_mplnri.c c_mpname.c  	c_mapgtd.c c_mapstd.c c_mpsetd.c c_mdgetc.c c_mdgetd.c c_mdgeti.c  	c_mdgetl.c c_mdgetr.c c_mdsetc.c c_mdsetd.c c_mdseti.c c_mdsetl.c  	c_mdsetr.c c_mdfnme.c c_mdglty.c c_mdiaty.c c_mdifnb.c c_mdilnb.c  	c_mdiola.c c_mdiosa.c c_mdipai.c c_mdipan.c c_mdipar.c c_mdisci.c  	c_mdlnam.c c_mdlndm.c c_mdlndr.c c_mdlnri.c c_mdname.c c_mdpaci.c  	c_mdpbla.c c_mdpblm.c c_mdpdrw.c c_mdpgrd.c c_mdpgrm.c c_mdpint.c  	c_mdplbl.c c_mdplmb.c c_mdplot.c c_mdprs.c  c_mdprst.c c_mdpsav.c  	c_mdrset.c c_mdpiq.c  c_mdpiqd.c c_mdrgol.c c_mdrgsf.c c_mdpfst.c  	c_mdpvec.c c_mdpiqa.c c_mdpiqm.c c_mdpit.c  c_mdpita.c c_mdpitd.c  	c_mdpitm.c c_mdppos.c c_mdproj.c c_mdpset.c c_mdptra.c c_mdptri.c  	c_mdptrn.c c_mpgetd.c c_mdpgci.c c_mdgcog.c c_mdlbln.c c_mdlblt.c  	c_mdrggc.c c_mdrgsc.c c_mdritd.c c_mdutfd.c c_mdutfs.c c_mdutid.c  	c_mdutis.c c_mputfd.c c_mputfs.c c_mputid.c c_mputis.c c_mdutin.c  	c_mputin.c c_mdlach.c c_mdloch.c c_mdrgdl.c c_mdscal.c c_maplmm.c  	c_mdplmm.c c_maqini.c c_maqtra.c c_maqtri.c c_maqtrn.c c_mdqini.c  	c_mdqtra.c c_mdqtri.c c_mdqtrn.c

OBJS =  c_mapblm.o c_mapdrw.o c_mapfst.o c_mapgrd.o c_mapgtc.o c_mapgti.o  	c_mapgtl.o c_mapgtr.o c_mapint.o c_mapiq.o  c_mapit.o  c_maplbl.o  	c_maplmb.o c_maplot.o c_mappos.o c_maproj.o c_maprs.o  c_maprst.o  	c_mapsav.o c_mapset.o c_mapstc.o c_mapsti.o c_mapstl.o c_mapstr.o  	c_maptri.o c_maptrn.o c_mapvec.o c_supcon.o c_supmap.o c_mapgci.o  	c_maptra.o c_mapiqa.o c_mapiqm.o c_mapita.o c_mapitm.o c_mapaci.o  	c_mapbla.o c_mapgrm.o c_mpgetc.o c_mpgeti.o c_mpgetl.o c_mpgetr.o  	c_mpsetc.o c_mpseti.o c_mpsetl.o c_mpsetr.o c_mapiqd.o c_mapitd.o  	c_mprset.o c_mpfnme.o c_mpglty.o c_mpiaty.o c_mpifnb.o c_mpilnb.o  	c_mpiola.o c_mpiosa.o c_mpipai.o c_mpipan.o c_mpipar.o c_mpisci.o  	c_mplnam.o c_mplndm.o c_mplndr.o c_mplnri.o c_mpname.o  	c_mapgtd.o c_mapstd.o c_mpsetd.o c_mdgetc.o c_mdgetd.o c_mdgeti.o  	c_mdgetl.o c_mdgetr.o c_mdsetc.o c_mdsetd.o c_mdseti.o c_mdsetl.o  	c_mdsetr.o c_mdfnme.o c_mdglty.o c_mdiaty.o c_mdifnb.o c_mdilnb.o  	c_mdiola.o c_mdiosa.o c_mdipai.o c_mdipan.o c_mdipar.o c_mdisci.o  	c_mdlnam.o c_mdlndm.o c_mdlndr.o c_mdlnri.o c_mdname.o c_mdpaci.o  	c_mdpbla.o c_mdpblm.o c_mdpdrw.o c_mdpgrd.o c_mdpgrm.o c_mdpint.o  	c_mdplbl.o c_mdplmb.o c_mdplot.o c_mdprs.o  c_mdprst.o c_mdpsav.o  	c_mdrset.o c_mdpiq.o  c_mdpiqd.o c_mdrgol.o c_mdrgsf.o c_mdpfst.o  	c_mdpvec.o c_mdpiqa.o c_mdpiqm.o c_mdpit.o  c_mdpita.o c_mdpitd.o  	c_mdpitm.o c_mdppos.o c_mdproj.o c_mdpset.o c_mdptra.o c_mdptri.o  	c_mdptrn.o c_mpgetd.o c_mdpgci.o c_mdgcog.o c_mdlbln.o c_mdlblt.o  	c_mdrggc.o c_mdrgsc.o c_mdritd.o c_mdutfd.o c_mdutfs.o c_mdutid.o  	c_mdutis.o c_mputfd.o c_mputfs.o c_mputid.o c_mputis.o c_mdutin.o  	c_mputin.o c_mdlach.o c_mdloch.o c_mdrgdl.o c_mdscal.o c_maplmm.o  	c_mdplmm.o c_maqini.o c_maqtra.o c_maqtri.o c_maqtrn.o c_mdqini.o  	c_mdqtra.o c_mdqtri.o c_mdqtrn.o

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

c_mapblm.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mapblm.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mapblm.o: /usr/include/sys/_posix_availability.h
c_mapblm.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mapblm.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mapblm.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mapblm.o: /usr/include/sys/_pthread/_pthread_types.h
c_mapblm.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mapblm.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mapblm.o: /usr/include/sys/_types/_int16_t.h
c_mapblm.o: /usr/include/sys/_types/_int32_t.h
c_mapblm.o: /usr/include/sys/_types/_int64_t.h
c_mapblm.o: /usr/include/sys/_types/_u_int8_t.h
c_mapblm.o: /usr/include/sys/_types/_u_int16_t.h
c_mapblm.o: /usr/include/sys/_types/_u_int32_t.h
c_mapblm.o: /usr/include/sys/_types/_u_int64_t.h
c_mapblm.o: /usr/include/sys/_types/_intptr_t.h
c_mapblm.o: /usr/include/sys/_types/_uintptr_t.h
c_mapblm.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mapblm.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mapblm.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mapblm.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mapblm.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mapblm.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mapblm.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mapblm.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mapblm.o: /usr/include/sys/_types/_u_char.h
c_mapblm.o: /usr/include/sys/_types/_u_short.h
c_mapblm.o: /usr/include/sys/_types/_u_int.h
c_mapblm.o: /usr/include/sys/_types/_caddr_t.h
c_mapblm.o: /usr/include/sys/_types/_dev_t.h
c_mapblm.o: /usr/include/sys/_types/_blkcnt_t.h
c_mapblm.o: /usr/include/sys/_types/_blksize_t.h
c_mapblm.o: /usr/include/sys/_types/_gid_t.h
c_mapblm.o: /usr/include/sys/_types/_in_addr_t.h
c_mapblm.o: /usr/include/sys/_types/_in_port_t.h
c_mapblm.o: /usr/include/sys/_types/_ino_t.h
c_mapblm.o: /usr/include/sys/_types/_ino64_t.h
c_mapblm.o: /usr/include/sys/_types/_key_t.h
c_mapblm.o: /usr/include/sys/_types/_mode_t.h
c_mapblm.o: /usr/include/sys/_types/_nlink_t.h
c_mapblm.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mapblm.o: /usr/include/sys/_types/_uid_t.h
c_mapblm.o: /usr/include/sys/_types/_clock_t.h
c_mapblm.o: /usr/include/sys/_types/_time_t.h
c_mapblm.o: /usr/include/sys/_types/_useconds_t.h
c_mapblm.o: /usr/include/sys/_types/_suseconds_t.h
c_mapblm.o: /usr/include/sys/_types/_rsize_t.h
c_mapblm.o: /usr/include/sys/_types/_errno_t.h
c_mapblm.o: /usr/include/sys/_types/_fd_def.h
c_mapblm.o: /usr/include/sys/_types/_fd_setsize.h
c_mapblm.o: /usr/include/sys/_types/_fd_set.h
c_mapblm.o: /usr/include/sys/_types/_fd_clr.h
c_mapblm.o: /usr/include/sys/_types/_fd_zero.h
c_mapblm.o: /usr/include/sys/_types/_fd_isset.h
c_mapblm.o: /usr/include/sys/_types/_fd_copy.h
c_mapblm.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mapblm.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mapblm.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mapblm.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mapblm.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mapblm.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mapblm.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mapblm.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mapblm.o: /usr/include/sys/_pthread/_pthread_t.h
c_mapblm.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mapblm.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mapblm.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mapblm.o: ../../../.././include/ncarg/c.h
c_mapdrw.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mapdrw.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mapdrw.o: /usr/include/sys/_posix_availability.h
c_mapdrw.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mapdrw.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mapdrw.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mapdrw.o: /usr/include/sys/_pthread/_pthread_types.h
c_mapdrw.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mapdrw.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mapdrw.o: /usr/include/sys/_types/_int16_t.h
c_mapdrw.o: /usr/include/sys/_types/_int32_t.h
c_mapdrw.o: /usr/include/sys/_types/_int64_t.h
c_mapdrw.o: /usr/include/sys/_types/_u_int8_t.h
c_mapdrw.o: /usr/include/sys/_types/_u_int16_t.h
c_mapdrw.o: /usr/include/sys/_types/_u_int32_t.h
c_mapdrw.o: /usr/include/sys/_types/_u_int64_t.h
c_mapdrw.o: /usr/include/sys/_types/_intptr_t.h
c_mapdrw.o: /usr/include/sys/_types/_uintptr_t.h
c_mapdrw.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mapdrw.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mapdrw.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mapdrw.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mapdrw.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mapdrw.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mapdrw.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mapdrw.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mapdrw.o: /usr/include/sys/_types/_u_char.h
c_mapdrw.o: /usr/include/sys/_types/_u_short.h
c_mapdrw.o: /usr/include/sys/_types/_u_int.h
c_mapdrw.o: /usr/include/sys/_types/_caddr_t.h
c_mapdrw.o: /usr/include/sys/_types/_dev_t.h
c_mapdrw.o: /usr/include/sys/_types/_blkcnt_t.h
c_mapdrw.o: /usr/include/sys/_types/_blksize_t.h
c_mapdrw.o: /usr/include/sys/_types/_gid_t.h
c_mapdrw.o: /usr/include/sys/_types/_in_addr_t.h
c_mapdrw.o: /usr/include/sys/_types/_in_port_t.h
c_mapdrw.o: /usr/include/sys/_types/_ino_t.h
c_mapdrw.o: /usr/include/sys/_types/_ino64_t.h
c_mapdrw.o: /usr/include/sys/_types/_key_t.h
c_mapdrw.o: /usr/include/sys/_types/_mode_t.h
c_mapdrw.o: /usr/include/sys/_types/_nlink_t.h
c_mapdrw.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mapdrw.o: /usr/include/sys/_types/_uid_t.h
c_mapdrw.o: /usr/include/sys/_types/_clock_t.h
c_mapdrw.o: /usr/include/sys/_types/_time_t.h
c_mapdrw.o: /usr/include/sys/_types/_useconds_t.h
c_mapdrw.o: /usr/include/sys/_types/_suseconds_t.h
c_mapdrw.o: /usr/include/sys/_types/_rsize_t.h
c_mapdrw.o: /usr/include/sys/_types/_errno_t.h
c_mapdrw.o: /usr/include/sys/_types/_fd_def.h
c_mapdrw.o: /usr/include/sys/_types/_fd_setsize.h
c_mapdrw.o: /usr/include/sys/_types/_fd_set.h
c_mapdrw.o: /usr/include/sys/_types/_fd_clr.h
c_mapdrw.o: /usr/include/sys/_types/_fd_zero.h
c_mapdrw.o: /usr/include/sys/_types/_fd_isset.h
c_mapdrw.o: /usr/include/sys/_types/_fd_copy.h
c_mapdrw.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mapdrw.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mapdrw.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mapdrw.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mapdrw.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mapdrw.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mapdrw.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mapdrw.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mapdrw.o: /usr/include/sys/_pthread/_pthread_t.h
c_mapdrw.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mapdrw.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mapdrw.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mapdrw.o: ../../../.././include/ncarg/c.h
c_mapfst.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mapfst.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mapfst.o: /usr/include/sys/_posix_availability.h
c_mapfst.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mapfst.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mapfst.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mapfst.o: /usr/include/sys/_pthread/_pthread_types.h
c_mapfst.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mapfst.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mapfst.o: /usr/include/sys/_types/_int16_t.h
c_mapfst.o: /usr/include/sys/_types/_int32_t.h
c_mapfst.o: /usr/include/sys/_types/_int64_t.h
c_mapfst.o: /usr/include/sys/_types/_u_int8_t.h
c_mapfst.o: /usr/include/sys/_types/_u_int16_t.h
c_mapfst.o: /usr/include/sys/_types/_u_int32_t.h
c_mapfst.o: /usr/include/sys/_types/_u_int64_t.h
c_mapfst.o: /usr/include/sys/_types/_intptr_t.h
c_mapfst.o: /usr/include/sys/_types/_uintptr_t.h
c_mapfst.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mapfst.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mapfst.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mapfst.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mapfst.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mapfst.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mapfst.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mapfst.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mapfst.o: /usr/include/sys/_types/_u_char.h
c_mapfst.o: /usr/include/sys/_types/_u_short.h
c_mapfst.o: /usr/include/sys/_types/_u_int.h
c_mapfst.o: /usr/include/sys/_types/_caddr_t.h
c_mapfst.o: /usr/include/sys/_types/_dev_t.h
c_mapfst.o: /usr/include/sys/_types/_blkcnt_t.h
c_mapfst.o: /usr/include/sys/_types/_blksize_t.h
c_mapfst.o: /usr/include/sys/_types/_gid_t.h
c_mapfst.o: /usr/include/sys/_types/_in_addr_t.h
c_mapfst.o: /usr/include/sys/_types/_in_port_t.h
c_mapfst.o: /usr/include/sys/_types/_ino_t.h
c_mapfst.o: /usr/include/sys/_types/_ino64_t.h
c_mapfst.o: /usr/include/sys/_types/_key_t.h
c_mapfst.o: /usr/include/sys/_types/_mode_t.h
c_mapfst.o: /usr/include/sys/_types/_nlink_t.h
c_mapfst.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mapfst.o: /usr/include/sys/_types/_uid_t.h
c_mapfst.o: /usr/include/sys/_types/_clock_t.h
c_mapfst.o: /usr/include/sys/_types/_time_t.h
c_mapfst.o: /usr/include/sys/_types/_useconds_t.h
c_mapfst.o: /usr/include/sys/_types/_suseconds_t.h
c_mapfst.o: /usr/include/sys/_types/_rsize_t.h
c_mapfst.o: /usr/include/sys/_types/_errno_t.h
c_mapfst.o: /usr/include/sys/_types/_fd_def.h
c_mapfst.o: /usr/include/sys/_types/_fd_setsize.h
c_mapfst.o: /usr/include/sys/_types/_fd_set.h
c_mapfst.o: /usr/include/sys/_types/_fd_clr.h
c_mapfst.o: /usr/include/sys/_types/_fd_zero.h
c_mapfst.o: /usr/include/sys/_types/_fd_isset.h
c_mapfst.o: /usr/include/sys/_types/_fd_copy.h
c_mapfst.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mapfst.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mapfst.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mapfst.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mapfst.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mapfst.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mapfst.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mapfst.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mapfst.o: /usr/include/sys/_pthread/_pthread_t.h
c_mapfst.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mapfst.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mapfst.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mapfst.o: ../../../.././include/ncarg/c.h
c_mapgrd.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mapgrd.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mapgrd.o: /usr/include/sys/_posix_availability.h
c_mapgrd.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mapgrd.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mapgrd.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mapgrd.o: /usr/include/sys/_pthread/_pthread_types.h
c_mapgrd.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mapgrd.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mapgrd.o: /usr/include/sys/_types/_int16_t.h
c_mapgrd.o: /usr/include/sys/_types/_int32_t.h
c_mapgrd.o: /usr/include/sys/_types/_int64_t.h
c_mapgrd.o: /usr/include/sys/_types/_u_int8_t.h
c_mapgrd.o: /usr/include/sys/_types/_u_int16_t.h
c_mapgrd.o: /usr/include/sys/_types/_u_int32_t.h
c_mapgrd.o: /usr/include/sys/_types/_u_int64_t.h
c_mapgrd.o: /usr/include/sys/_types/_intptr_t.h
c_mapgrd.o: /usr/include/sys/_types/_uintptr_t.h
c_mapgrd.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mapgrd.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mapgrd.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mapgrd.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mapgrd.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mapgrd.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mapgrd.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mapgrd.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mapgrd.o: /usr/include/sys/_types/_u_char.h
c_mapgrd.o: /usr/include/sys/_types/_u_short.h
c_mapgrd.o: /usr/include/sys/_types/_u_int.h
c_mapgrd.o: /usr/include/sys/_types/_caddr_t.h
c_mapgrd.o: /usr/include/sys/_types/_dev_t.h
c_mapgrd.o: /usr/include/sys/_types/_blkcnt_t.h
c_mapgrd.o: /usr/include/sys/_types/_blksize_t.h
c_mapgrd.o: /usr/include/sys/_types/_gid_t.h
c_mapgrd.o: /usr/include/sys/_types/_in_addr_t.h
c_mapgrd.o: /usr/include/sys/_types/_in_port_t.h
c_mapgrd.o: /usr/include/sys/_types/_ino_t.h
c_mapgrd.o: /usr/include/sys/_types/_ino64_t.h
c_mapgrd.o: /usr/include/sys/_types/_key_t.h
c_mapgrd.o: /usr/include/sys/_types/_mode_t.h
c_mapgrd.o: /usr/include/sys/_types/_nlink_t.h
c_mapgrd.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mapgrd.o: /usr/include/sys/_types/_uid_t.h
c_mapgrd.o: /usr/include/sys/_types/_clock_t.h
c_mapgrd.o: /usr/include/sys/_types/_time_t.h
c_mapgrd.o: /usr/include/sys/_types/_useconds_t.h
c_mapgrd.o: /usr/include/sys/_types/_suseconds_t.h
c_mapgrd.o: /usr/include/sys/_types/_rsize_t.h
c_mapgrd.o: /usr/include/sys/_types/_errno_t.h
c_mapgrd.o: /usr/include/sys/_types/_fd_def.h
c_mapgrd.o: /usr/include/sys/_types/_fd_setsize.h
c_mapgrd.o: /usr/include/sys/_types/_fd_set.h
c_mapgrd.o: /usr/include/sys/_types/_fd_clr.h
c_mapgrd.o: /usr/include/sys/_types/_fd_zero.h
c_mapgrd.o: /usr/include/sys/_types/_fd_isset.h
c_mapgrd.o: /usr/include/sys/_types/_fd_copy.h
c_mapgrd.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mapgrd.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mapgrd.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mapgrd.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mapgrd.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mapgrd.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mapgrd.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mapgrd.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mapgrd.o: /usr/include/sys/_pthread/_pthread_t.h
c_mapgrd.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mapgrd.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mapgrd.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mapgrd.o: ../../../.././include/ncarg/c.h
c_mapgtc.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mapgtc.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mapgtc.o: /usr/include/sys/_posix_availability.h
c_mapgtc.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mapgtc.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mapgtc.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mapgtc.o: /usr/include/sys/_pthread/_pthread_types.h
c_mapgtc.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mapgtc.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mapgtc.o: /usr/include/sys/_types/_int16_t.h
c_mapgtc.o: /usr/include/sys/_types/_int32_t.h
c_mapgtc.o: /usr/include/sys/_types/_int64_t.h
c_mapgtc.o: /usr/include/sys/_types/_u_int8_t.h
c_mapgtc.o: /usr/include/sys/_types/_u_int16_t.h
c_mapgtc.o: /usr/include/sys/_types/_u_int32_t.h
c_mapgtc.o: /usr/include/sys/_types/_u_int64_t.h
c_mapgtc.o: /usr/include/sys/_types/_intptr_t.h
c_mapgtc.o: /usr/include/sys/_types/_uintptr_t.h
c_mapgtc.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mapgtc.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mapgtc.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mapgtc.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mapgtc.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mapgtc.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mapgtc.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mapgtc.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mapgtc.o: /usr/include/sys/_types/_u_char.h
c_mapgtc.o: /usr/include/sys/_types/_u_short.h
c_mapgtc.o: /usr/include/sys/_types/_u_int.h
c_mapgtc.o: /usr/include/sys/_types/_caddr_t.h
c_mapgtc.o: /usr/include/sys/_types/_dev_t.h
c_mapgtc.o: /usr/include/sys/_types/_blkcnt_t.h
c_mapgtc.o: /usr/include/sys/_types/_blksize_t.h
c_mapgtc.o: /usr/include/sys/_types/_gid_t.h
c_mapgtc.o: /usr/include/sys/_types/_in_addr_t.h
c_mapgtc.o: /usr/include/sys/_types/_in_port_t.h
c_mapgtc.o: /usr/include/sys/_types/_ino_t.h
c_mapgtc.o: /usr/include/sys/_types/_ino64_t.h
c_mapgtc.o: /usr/include/sys/_types/_key_t.h
c_mapgtc.o: /usr/include/sys/_types/_mode_t.h
c_mapgtc.o: /usr/include/sys/_types/_nlink_t.h
c_mapgtc.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mapgtc.o: /usr/include/sys/_types/_uid_t.h
c_mapgtc.o: /usr/include/sys/_types/_clock_t.h
c_mapgtc.o: /usr/include/sys/_types/_time_t.h
c_mapgtc.o: /usr/include/sys/_types/_useconds_t.h
c_mapgtc.o: /usr/include/sys/_types/_suseconds_t.h
c_mapgtc.o: /usr/include/sys/_types/_rsize_t.h
c_mapgtc.o: /usr/include/sys/_types/_errno_t.h
c_mapgtc.o: /usr/include/sys/_types/_fd_def.h
c_mapgtc.o: /usr/include/sys/_types/_fd_setsize.h
c_mapgtc.o: /usr/include/sys/_types/_fd_set.h
c_mapgtc.o: /usr/include/sys/_types/_fd_clr.h
c_mapgtc.o: /usr/include/sys/_types/_fd_zero.h
c_mapgtc.o: /usr/include/sys/_types/_fd_isset.h
c_mapgtc.o: /usr/include/sys/_types/_fd_copy.h
c_mapgtc.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mapgtc.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mapgtc.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mapgtc.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mapgtc.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mapgtc.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mapgtc.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mapgtc.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mapgtc.o: /usr/include/sys/_pthread/_pthread_t.h
c_mapgtc.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mapgtc.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mapgtc.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mapgtc.o: ../../../.././include/ncarg/c.h
c_mapgti.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mapgti.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mapgti.o: /usr/include/sys/_posix_availability.h
c_mapgti.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mapgti.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mapgti.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mapgti.o: /usr/include/sys/_pthread/_pthread_types.h
c_mapgti.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mapgti.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mapgti.o: /usr/include/sys/_types/_int16_t.h
c_mapgti.o: /usr/include/sys/_types/_int32_t.h
c_mapgti.o: /usr/include/sys/_types/_int64_t.h
c_mapgti.o: /usr/include/sys/_types/_u_int8_t.h
c_mapgti.o: /usr/include/sys/_types/_u_int16_t.h
c_mapgti.o: /usr/include/sys/_types/_u_int32_t.h
c_mapgti.o: /usr/include/sys/_types/_u_int64_t.h
c_mapgti.o: /usr/include/sys/_types/_intptr_t.h
c_mapgti.o: /usr/include/sys/_types/_uintptr_t.h
c_mapgti.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mapgti.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mapgti.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mapgti.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mapgti.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mapgti.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mapgti.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mapgti.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mapgti.o: /usr/include/sys/_types/_u_char.h
c_mapgti.o: /usr/include/sys/_types/_u_short.h
c_mapgti.o: /usr/include/sys/_types/_u_int.h
c_mapgti.o: /usr/include/sys/_types/_caddr_t.h
c_mapgti.o: /usr/include/sys/_types/_dev_t.h
c_mapgti.o: /usr/include/sys/_types/_blkcnt_t.h
c_mapgti.o: /usr/include/sys/_types/_blksize_t.h
c_mapgti.o: /usr/include/sys/_types/_gid_t.h
c_mapgti.o: /usr/include/sys/_types/_in_addr_t.h
c_mapgti.o: /usr/include/sys/_types/_in_port_t.h
c_mapgti.o: /usr/include/sys/_types/_ino_t.h
c_mapgti.o: /usr/include/sys/_types/_ino64_t.h
c_mapgti.o: /usr/include/sys/_types/_key_t.h
c_mapgti.o: /usr/include/sys/_types/_mode_t.h
c_mapgti.o: /usr/include/sys/_types/_nlink_t.h
c_mapgti.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mapgti.o: /usr/include/sys/_types/_uid_t.h
c_mapgti.o: /usr/include/sys/_types/_clock_t.h
c_mapgti.o: /usr/include/sys/_types/_time_t.h
c_mapgti.o: /usr/include/sys/_types/_useconds_t.h
c_mapgti.o: /usr/include/sys/_types/_suseconds_t.h
c_mapgti.o: /usr/include/sys/_types/_rsize_t.h
c_mapgti.o: /usr/include/sys/_types/_errno_t.h
c_mapgti.o: /usr/include/sys/_types/_fd_def.h
c_mapgti.o: /usr/include/sys/_types/_fd_setsize.h
c_mapgti.o: /usr/include/sys/_types/_fd_set.h
c_mapgti.o: /usr/include/sys/_types/_fd_clr.h
c_mapgti.o: /usr/include/sys/_types/_fd_zero.h
c_mapgti.o: /usr/include/sys/_types/_fd_isset.h
c_mapgti.o: /usr/include/sys/_types/_fd_copy.h
c_mapgti.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mapgti.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mapgti.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mapgti.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mapgti.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mapgti.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mapgti.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mapgti.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mapgti.o: /usr/include/sys/_pthread/_pthread_t.h
c_mapgti.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mapgti.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mapgti.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mapgti.o: ../../../.././include/ncarg/c.h
c_mapgtl.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mapgtl.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mapgtl.o: /usr/include/sys/_posix_availability.h
c_mapgtl.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mapgtl.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mapgtl.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mapgtl.o: /usr/include/sys/_pthread/_pthread_types.h
c_mapgtl.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mapgtl.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mapgtl.o: /usr/include/sys/_types/_int16_t.h
c_mapgtl.o: /usr/include/sys/_types/_int32_t.h
c_mapgtl.o: /usr/include/sys/_types/_int64_t.h
c_mapgtl.o: /usr/include/sys/_types/_u_int8_t.h
c_mapgtl.o: /usr/include/sys/_types/_u_int16_t.h
c_mapgtl.o: /usr/include/sys/_types/_u_int32_t.h
c_mapgtl.o: /usr/include/sys/_types/_u_int64_t.h
c_mapgtl.o: /usr/include/sys/_types/_intptr_t.h
c_mapgtl.o: /usr/include/sys/_types/_uintptr_t.h
c_mapgtl.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mapgtl.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mapgtl.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mapgtl.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mapgtl.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mapgtl.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mapgtl.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mapgtl.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mapgtl.o: /usr/include/sys/_types/_u_char.h
c_mapgtl.o: /usr/include/sys/_types/_u_short.h
c_mapgtl.o: /usr/include/sys/_types/_u_int.h
c_mapgtl.o: /usr/include/sys/_types/_caddr_t.h
c_mapgtl.o: /usr/include/sys/_types/_dev_t.h
c_mapgtl.o: /usr/include/sys/_types/_blkcnt_t.h
c_mapgtl.o: /usr/include/sys/_types/_blksize_t.h
c_mapgtl.o: /usr/include/sys/_types/_gid_t.h
c_mapgtl.o: /usr/include/sys/_types/_in_addr_t.h
c_mapgtl.o: /usr/include/sys/_types/_in_port_t.h
c_mapgtl.o: /usr/include/sys/_types/_ino_t.h
c_mapgtl.o: /usr/include/sys/_types/_ino64_t.h
c_mapgtl.o: /usr/include/sys/_types/_key_t.h
c_mapgtl.o: /usr/include/sys/_types/_mode_t.h
c_mapgtl.o: /usr/include/sys/_types/_nlink_t.h
c_mapgtl.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mapgtl.o: /usr/include/sys/_types/_uid_t.h
c_mapgtl.o: /usr/include/sys/_types/_clock_t.h
c_mapgtl.o: /usr/include/sys/_types/_time_t.h
c_mapgtl.o: /usr/include/sys/_types/_useconds_t.h
c_mapgtl.o: /usr/include/sys/_types/_suseconds_t.h
c_mapgtl.o: /usr/include/sys/_types/_rsize_t.h
c_mapgtl.o: /usr/include/sys/_types/_errno_t.h
c_mapgtl.o: /usr/include/sys/_types/_fd_def.h
c_mapgtl.o: /usr/include/sys/_types/_fd_setsize.h
c_mapgtl.o: /usr/include/sys/_types/_fd_set.h
c_mapgtl.o: /usr/include/sys/_types/_fd_clr.h
c_mapgtl.o: /usr/include/sys/_types/_fd_zero.h
c_mapgtl.o: /usr/include/sys/_types/_fd_isset.h
c_mapgtl.o: /usr/include/sys/_types/_fd_copy.h
c_mapgtl.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mapgtl.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mapgtl.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mapgtl.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mapgtl.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mapgtl.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mapgtl.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mapgtl.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mapgtl.o: /usr/include/sys/_pthread/_pthread_t.h
c_mapgtl.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mapgtl.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mapgtl.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mapgtl.o: ../../../.././include/ncarg/c.h
c_mapgtr.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mapgtr.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mapgtr.o: /usr/include/sys/_posix_availability.h
c_mapgtr.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mapgtr.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mapgtr.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mapgtr.o: /usr/include/sys/_pthread/_pthread_types.h
c_mapgtr.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mapgtr.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mapgtr.o: /usr/include/sys/_types/_int16_t.h
c_mapgtr.o: /usr/include/sys/_types/_int32_t.h
c_mapgtr.o: /usr/include/sys/_types/_int64_t.h
c_mapgtr.o: /usr/include/sys/_types/_u_int8_t.h
c_mapgtr.o: /usr/include/sys/_types/_u_int16_t.h
c_mapgtr.o: /usr/include/sys/_types/_u_int32_t.h
c_mapgtr.o: /usr/include/sys/_types/_u_int64_t.h
c_mapgtr.o: /usr/include/sys/_types/_intptr_t.h
c_mapgtr.o: /usr/include/sys/_types/_uintptr_t.h
c_mapgtr.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mapgtr.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mapgtr.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mapgtr.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mapgtr.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mapgtr.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mapgtr.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mapgtr.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mapgtr.o: /usr/include/sys/_types/_u_char.h
c_mapgtr.o: /usr/include/sys/_types/_u_short.h
c_mapgtr.o: /usr/include/sys/_types/_u_int.h
c_mapgtr.o: /usr/include/sys/_types/_caddr_t.h
c_mapgtr.o: /usr/include/sys/_types/_dev_t.h
c_mapgtr.o: /usr/include/sys/_types/_blkcnt_t.h
c_mapgtr.o: /usr/include/sys/_types/_blksize_t.h
c_mapgtr.o: /usr/include/sys/_types/_gid_t.h
c_mapgtr.o: /usr/include/sys/_types/_in_addr_t.h
c_mapgtr.o: /usr/include/sys/_types/_in_port_t.h
c_mapgtr.o: /usr/include/sys/_types/_ino_t.h
c_mapgtr.o: /usr/include/sys/_types/_ino64_t.h
c_mapgtr.o: /usr/include/sys/_types/_key_t.h
c_mapgtr.o: /usr/include/sys/_types/_mode_t.h
c_mapgtr.o: /usr/include/sys/_types/_nlink_t.h
c_mapgtr.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mapgtr.o: /usr/include/sys/_types/_uid_t.h
c_mapgtr.o: /usr/include/sys/_types/_clock_t.h
c_mapgtr.o: /usr/include/sys/_types/_time_t.h
c_mapgtr.o: /usr/include/sys/_types/_useconds_t.h
c_mapgtr.o: /usr/include/sys/_types/_suseconds_t.h
c_mapgtr.o: /usr/include/sys/_types/_rsize_t.h
c_mapgtr.o: /usr/include/sys/_types/_errno_t.h
c_mapgtr.o: /usr/include/sys/_types/_fd_def.h
c_mapgtr.o: /usr/include/sys/_types/_fd_setsize.h
c_mapgtr.o: /usr/include/sys/_types/_fd_set.h
c_mapgtr.o: /usr/include/sys/_types/_fd_clr.h
c_mapgtr.o: /usr/include/sys/_types/_fd_zero.h
c_mapgtr.o: /usr/include/sys/_types/_fd_isset.h
c_mapgtr.o: /usr/include/sys/_types/_fd_copy.h
c_mapgtr.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mapgtr.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mapgtr.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mapgtr.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mapgtr.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mapgtr.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mapgtr.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mapgtr.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mapgtr.o: /usr/include/sys/_pthread/_pthread_t.h
c_mapgtr.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mapgtr.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mapgtr.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mapgtr.o: ../../../.././include/ncarg/c.h
c_mapint.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mapint.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mapint.o: /usr/include/sys/_posix_availability.h
c_mapint.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mapint.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mapint.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mapint.o: /usr/include/sys/_pthread/_pthread_types.h
c_mapint.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mapint.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mapint.o: /usr/include/sys/_types/_int16_t.h
c_mapint.o: /usr/include/sys/_types/_int32_t.h
c_mapint.o: /usr/include/sys/_types/_int64_t.h
c_mapint.o: /usr/include/sys/_types/_u_int8_t.h
c_mapint.o: /usr/include/sys/_types/_u_int16_t.h
c_mapint.o: /usr/include/sys/_types/_u_int32_t.h
c_mapint.o: /usr/include/sys/_types/_u_int64_t.h
c_mapint.o: /usr/include/sys/_types/_intptr_t.h
c_mapint.o: /usr/include/sys/_types/_uintptr_t.h
c_mapint.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mapint.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mapint.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mapint.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mapint.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mapint.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mapint.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mapint.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mapint.o: /usr/include/sys/_types/_u_char.h
c_mapint.o: /usr/include/sys/_types/_u_short.h
c_mapint.o: /usr/include/sys/_types/_u_int.h
c_mapint.o: /usr/include/sys/_types/_caddr_t.h
c_mapint.o: /usr/include/sys/_types/_dev_t.h
c_mapint.o: /usr/include/sys/_types/_blkcnt_t.h
c_mapint.o: /usr/include/sys/_types/_blksize_t.h
c_mapint.o: /usr/include/sys/_types/_gid_t.h
c_mapint.o: /usr/include/sys/_types/_in_addr_t.h
c_mapint.o: /usr/include/sys/_types/_in_port_t.h
c_mapint.o: /usr/include/sys/_types/_ino_t.h
c_mapint.o: /usr/include/sys/_types/_ino64_t.h
c_mapint.o: /usr/include/sys/_types/_key_t.h
c_mapint.o: /usr/include/sys/_types/_mode_t.h
c_mapint.o: /usr/include/sys/_types/_nlink_t.h
c_mapint.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mapint.o: /usr/include/sys/_types/_uid_t.h
c_mapint.o: /usr/include/sys/_types/_clock_t.h
c_mapint.o: /usr/include/sys/_types/_time_t.h
c_mapint.o: /usr/include/sys/_types/_useconds_t.h
c_mapint.o: /usr/include/sys/_types/_suseconds_t.h
c_mapint.o: /usr/include/sys/_types/_rsize_t.h
c_mapint.o: /usr/include/sys/_types/_errno_t.h
c_mapint.o: /usr/include/sys/_types/_fd_def.h
c_mapint.o: /usr/include/sys/_types/_fd_setsize.h
c_mapint.o: /usr/include/sys/_types/_fd_set.h
c_mapint.o: /usr/include/sys/_types/_fd_clr.h
c_mapint.o: /usr/include/sys/_types/_fd_zero.h
c_mapint.o: /usr/include/sys/_types/_fd_isset.h
c_mapint.o: /usr/include/sys/_types/_fd_copy.h
c_mapint.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mapint.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mapint.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mapint.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mapint.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mapint.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mapint.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mapint.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mapint.o: /usr/include/sys/_pthread/_pthread_t.h
c_mapint.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mapint.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mapint.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mapint.o: ../../../.././include/ncarg/c.h
c_mapiq.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mapiq.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mapiq.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_mapiq.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_mapiq.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_mapiq.o: /usr/include/i386/_types.h
c_mapiq.o: /usr/include/sys/_pthread/_pthread_types.h
c_mapiq.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mapiq.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mapiq.o: /usr/include/sys/_types/_int16_t.h
c_mapiq.o: /usr/include/sys/_types/_int32_t.h
c_mapiq.o: /usr/include/sys/_types/_int64_t.h
c_mapiq.o: /usr/include/sys/_types/_u_int8_t.h
c_mapiq.o: /usr/include/sys/_types/_u_int16_t.h
c_mapiq.o: /usr/include/sys/_types/_u_int32_t.h
c_mapiq.o: /usr/include/sys/_types/_u_int64_t.h
c_mapiq.o: /usr/include/sys/_types/_intptr_t.h
c_mapiq.o: /usr/include/sys/_types/_uintptr_t.h
c_mapiq.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mapiq.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mapiq.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mapiq.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mapiq.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mapiq.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mapiq.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mapiq.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mapiq.o: /usr/include/sys/_types/_u_char.h
c_mapiq.o: /usr/include/sys/_types/_u_short.h
c_mapiq.o: /usr/include/sys/_types/_u_int.h
c_mapiq.o: /usr/include/sys/_types/_caddr_t.h
c_mapiq.o: /usr/include/sys/_types/_dev_t.h
c_mapiq.o: /usr/include/sys/_types/_blkcnt_t.h
c_mapiq.o: /usr/include/sys/_types/_blksize_t.h
c_mapiq.o: /usr/include/sys/_types/_gid_t.h
c_mapiq.o: /usr/include/sys/_types/_in_addr_t.h
c_mapiq.o: /usr/include/sys/_types/_in_port_t.h
c_mapiq.o: /usr/include/sys/_types/_ino_t.h
c_mapiq.o: /usr/include/sys/_types/_ino64_t.h
c_mapiq.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_mapiq.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_mapiq.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_mapiq.o: /usr/include/sys/_types/_clock_t.h
c_mapiq.o: /usr/include/sys/_types/_time_t.h
c_mapiq.o: /usr/include/sys/_types/_useconds_t.h
c_mapiq.o: /usr/include/sys/_types/_suseconds_t.h
c_mapiq.o: /usr/include/sys/_types/_rsize_t.h
c_mapiq.o: /usr/include/sys/_types/_errno_t.h
c_mapiq.o: /usr/include/sys/_types/_fd_def.h
c_mapiq.o: /usr/include/sys/_types/_fd_setsize.h
c_mapiq.o: /usr/include/sys/_types/_fd_set.h
c_mapiq.o: /usr/include/sys/_types/_fd_clr.h
c_mapiq.o: /usr/include/sys/_types/_fd_zero.h
c_mapiq.o: /usr/include/sys/_types/_fd_isset.h
c_mapiq.o: /usr/include/sys/_types/_fd_copy.h
c_mapiq.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mapiq.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mapiq.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mapiq.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mapiq.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mapiq.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mapiq.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mapiq.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mapiq.o: /usr/include/sys/_pthread/_pthread_t.h
c_mapiq.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mapiq.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mapiq.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mapiq.o: ../../../.././include/ncarg/c.h
c_mapit.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mapit.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mapit.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_mapit.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_mapit.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_mapit.o: /usr/include/i386/_types.h
c_mapit.o: /usr/include/sys/_pthread/_pthread_types.h
c_mapit.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mapit.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mapit.o: /usr/include/sys/_types/_int16_t.h
c_mapit.o: /usr/include/sys/_types/_int32_t.h
c_mapit.o: /usr/include/sys/_types/_int64_t.h
c_mapit.o: /usr/include/sys/_types/_u_int8_t.h
c_mapit.o: /usr/include/sys/_types/_u_int16_t.h
c_mapit.o: /usr/include/sys/_types/_u_int32_t.h
c_mapit.o: /usr/include/sys/_types/_u_int64_t.h
c_mapit.o: /usr/include/sys/_types/_intptr_t.h
c_mapit.o: /usr/include/sys/_types/_uintptr_t.h
c_mapit.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mapit.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mapit.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mapit.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mapit.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mapit.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mapit.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mapit.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mapit.o: /usr/include/sys/_types/_u_char.h
c_mapit.o: /usr/include/sys/_types/_u_short.h
c_mapit.o: /usr/include/sys/_types/_u_int.h
c_mapit.o: /usr/include/sys/_types/_caddr_t.h
c_mapit.o: /usr/include/sys/_types/_dev_t.h
c_mapit.o: /usr/include/sys/_types/_blkcnt_t.h
c_mapit.o: /usr/include/sys/_types/_blksize_t.h
c_mapit.o: /usr/include/sys/_types/_gid_t.h
c_mapit.o: /usr/include/sys/_types/_in_addr_t.h
c_mapit.o: /usr/include/sys/_types/_in_port_t.h
c_mapit.o: /usr/include/sys/_types/_ino_t.h
c_mapit.o: /usr/include/sys/_types/_ino64_t.h
c_mapit.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_mapit.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_mapit.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_mapit.o: /usr/include/sys/_types/_clock_t.h
c_mapit.o: /usr/include/sys/_types/_time_t.h
c_mapit.o: /usr/include/sys/_types/_useconds_t.h
c_mapit.o: /usr/include/sys/_types/_suseconds_t.h
c_mapit.o: /usr/include/sys/_types/_rsize_t.h
c_mapit.o: /usr/include/sys/_types/_errno_t.h
c_mapit.o: /usr/include/sys/_types/_fd_def.h
c_mapit.o: /usr/include/sys/_types/_fd_setsize.h
c_mapit.o: /usr/include/sys/_types/_fd_set.h
c_mapit.o: /usr/include/sys/_types/_fd_clr.h
c_mapit.o: /usr/include/sys/_types/_fd_zero.h
c_mapit.o: /usr/include/sys/_types/_fd_isset.h
c_mapit.o: /usr/include/sys/_types/_fd_copy.h
c_mapit.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mapit.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mapit.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mapit.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mapit.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mapit.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mapit.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mapit.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mapit.o: /usr/include/sys/_pthread/_pthread_t.h
c_mapit.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mapit.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mapit.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mapit.o: ../../../.././include/ncarg/c.h
c_maplbl.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_maplbl.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_maplbl.o: /usr/include/sys/_posix_availability.h
c_maplbl.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_maplbl.o: /usr/include/_types.h /usr/include/sys/_types.h
c_maplbl.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_maplbl.o: /usr/include/sys/_pthread/_pthread_types.h
c_maplbl.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_maplbl.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_maplbl.o: /usr/include/sys/_types/_int16_t.h
c_maplbl.o: /usr/include/sys/_types/_int32_t.h
c_maplbl.o: /usr/include/sys/_types/_int64_t.h
c_maplbl.o: /usr/include/sys/_types/_u_int8_t.h
c_maplbl.o: /usr/include/sys/_types/_u_int16_t.h
c_maplbl.o: /usr/include/sys/_types/_u_int32_t.h
c_maplbl.o: /usr/include/sys/_types/_u_int64_t.h
c_maplbl.o: /usr/include/sys/_types/_intptr_t.h
c_maplbl.o: /usr/include/sys/_types/_uintptr_t.h
c_maplbl.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_maplbl.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_maplbl.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_maplbl.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_maplbl.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_maplbl.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_maplbl.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_maplbl.o: /usr/include/libkern/i386/_OSByteOrder.h
c_maplbl.o: /usr/include/sys/_types/_u_char.h
c_maplbl.o: /usr/include/sys/_types/_u_short.h
c_maplbl.o: /usr/include/sys/_types/_u_int.h
c_maplbl.o: /usr/include/sys/_types/_caddr_t.h
c_maplbl.o: /usr/include/sys/_types/_dev_t.h
c_maplbl.o: /usr/include/sys/_types/_blkcnt_t.h
c_maplbl.o: /usr/include/sys/_types/_blksize_t.h
c_maplbl.o: /usr/include/sys/_types/_gid_t.h
c_maplbl.o: /usr/include/sys/_types/_in_addr_t.h
c_maplbl.o: /usr/include/sys/_types/_in_port_t.h
c_maplbl.o: /usr/include/sys/_types/_ino_t.h
c_maplbl.o: /usr/include/sys/_types/_ino64_t.h
c_maplbl.o: /usr/include/sys/_types/_key_t.h
c_maplbl.o: /usr/include/sys/_types/_mode_t.h
c_maplbl.o: /usr/include/sys/_types/_nlink_t.h
c_maplbl.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_maplbl.o: /usr/include/sys/_types/_uid_t.h
c_maplbl.o: /usr/include/sys/_types/_clock_t.h
c_maplbl.o: /usr/include/sys/_types/_time_t.h
c_maplbl.o: /usr/include/sys/_types/_useconds_t.h
c_maplbl.o: /usr/include/sys/_types/_suseconds_t.h
c_maplbl.o: /usr/include/sys/_types/_rsize_t.h
c_maplbl.o: /usr/include/sys/_types/_errno_t.h
c_maplbl.o: /usr/include/sys/_types/_fd_def.h
c_maplbl.o: /usr/include/sys/_types/_fd_setsize.h
c_maplbl.o: /usr/include/sys/_types/_fd_set.h
c_maplbl.o: /usr/include/sys/_types/_fd_clr.h
c_maplbl.o: /usr/include/sys/_types/_fd_zero.h
c_maplbl.o: /usr/include/sys/_types/_fd_isset.h
c_maplbl.o: /usr/include/sys/_types/_fd_copy.h
c_maplbl.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_maplbl.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_maplbl.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_maplbl.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_maplbl.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_maplbl.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_maplbl.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_maplbl.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_maplbl.o: /usr/include/sys/_pthread/_pthread_t.h
c_maplbl.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_maplbl.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_maplbl.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_maplbl.o: ../../../.././include/ncarg/c.h
c_maplmb.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_maplmb.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_maplmb.o: /usr/include/sys/_posix_availability.h
c_maplmb.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_maplmb.o: /usr/include/_types.h /usr/include/sys/_types.h
c_maplmb.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_maplmb.o: /usr/include/sys/_pthread/_pthread_types.h
c_maplmb.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_maplmb.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_maplmb.o: /usr/include/sys/_types/_int16_t.h
c_maplmb.o: /usr/include/sys/_types/_int32_t.h
c_maplmb.o: /usr/include/sys/_types/_int64_t.h
c_maplmb.o: /usr/include/sys/_types/_u_int8_t.h
c_maplmb.o: /usr/include/sys/_types/_u_int16_t.h
c_maplmb.o: /usr/include/sys/_types/_u_int32_t.h
c_maplmb.o: /usr/include/sys/_types/_u_int64_t.h
c_maplmb.o: /usr/include/sys/_types/_intptr_t.h
c_maplmb.o: /usr/include/sys/_types/_uintptr_t.h
c_maplmb.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_maplmb.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_maplmb.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_maplmb.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_maplmb.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_maplmb.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_maplmb.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_maplmb.o: /usr/include/libkern/i386/_OSByteOrder.h
c_maplmb.o: /usr/include/sys/_types/_u_char.h
c_maplmb.o: /usr/include/sys/_types/_u_short.h
c_maplmb.o: /usr/include/sys/_types/_u_int.h
c_maplmb.o: /usr/include/sys/_types/_caddr_t.h
c_maplmb.o: /usr/include/sys/_types/_dev_t.h
c_maplmb.o: /usr/include/sys/_types/_blkcnt_t.h
c_maplmb.o: /usr/include/sys/_types/_blksize_t.h
c_maplmb.o: /usr/include/sys/_types/_gid_t.h
c_maplmb.o: /usr/include/sys/_types/_in_addr_t.h
c_maplmb.o: /usr/include/sys/_types/_in_port_t.h
c_maplmb.o: /usr/include/sys/_types/_ino_t.h
c_maplmb.o: /usr/include/sys/_types/_ino64_t.h
c_maplmb.o: /usr/include/sys/_types/_key_t.h
c_maplmb.o: /usr/include/sys/_types/_mode_t.h
c_maplmb.o: /usr/include/sys/_types/_nlink_t.h
c_maplmb.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_maplmb.o: /usr/include/sys/_types/_uid_t.h
c_maplmb.o: /usr/include/sys/_types/_clock_t.h
c_maplmb.o: /usr/include/sys/_types/_time_t.h
c_maplmb.o: /usr/include/sys/_types/_useconds_t.h
c_maplmb.o: /usr/include/sys/_types/_suseconds_t.h
c_maplmb.o: /usr/include/sys/_types/_rsize_t.h
c_maplmb.o: /usr/include/sys/_types/_errno_t.h
c_maplmb.o: /usr/include/sys/_types/_fd_def.h
c_maplmb.o: /usr/include/sys/_types/_fd_setsize.h
c_maplmb.o: /usr/include/sys/_types/_fd_set.h
c_maplmb.o: /usr/include/sys/_types/_fd_clr.h
c_maplmb.o: /usr/include/sys/_types/_fd_zero.h
c_maplmb.o: /usr/include/sys/_types/_fd_isset.h
c_maplmb.o: /usr/include/sys/_types/_fd_copy.h
c_maplmb.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_maplmb.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_maplmb.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_maplmb.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_maplmb.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_maplmb.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_maplmb.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_maplmb.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_maplmb.o: /usr/include/sys/_pthread/_pthread_t.h
c_maplmb.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_maplmb.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_maplmb.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_maplmb.o: ../../../.././include/ncarg/c.h
c_maplot.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_maplot.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_maplot.o: /usr/include/sys/_posix_availability.h
c_maplot.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_maplot.o: /usr/include/_types.h /usr/include/sys/_types.h
c_maplot.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_maplot.o: /usr/include/sys/_pthread/_pthread_types.h
c_maplot.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_maplot.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_maplot.o: /usr/include/sys/_types/_int16_t.h
c_maplot.o: /usr/include/sys/_types/_int32_t.h
c_maplot.o: /usr/include/sys/_types/_int64_t.h
c_maplot.o: /usr/include/sys/_types/_u_int8_t.h
c_maplot.o: /usr/include/sys/_types/_u_int16_t.h
c_maplot.o: /usr/include/sys/_types/_u_int32_t.h
c_maplot.o: /usr/include/sys/_types/_u_int64_t.h
c_maplot.o: /usr/include/sys/_types/_intptr_t.h
c_maplot.o: /usr/include/sys/_types/_uintptr_t.h
c_maplot.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_maplot.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_maplot.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_maplot.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_maplot.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_maplot.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_maplot.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_maplot.o: /usr/include/libkern/i386/_OSByteOrder.h
c_maplot.o: /usr/include/sys/_types/_u_char.h
c_maplot.o: /usr/include/sys/_types/_u_short.h
c_maplot.o: /usr/include/sys/_types/_u_int.h
c_maplot.o: /usr/include/sys/_types/_caddr_t.h
c_maplot.o: /usr/include/sys/_types/_dev_t.h
c_maplot.o: /usr/include/sys/_types/_blkcnt_t.h
c_maplot.o: /usr/include/sys/_types/_blksize_t.h
c_maplot.o: /usr/include/sys/_types/_gid_t.h
c_maplot.o: /usr/include/sys/_types/_in_addr_t.h
c_maplot.o: /usr/include/sys/_types/_in_port_t.h
c_maplot.o: /usr/include/sys/_types/_ino_t.h
c_maplot.o: /usr/include/sys/_types/_ino64_t.h
c_maplot.o: /usr/include/sys/_types/_key_t.h
c_maplot.o: /usr/include/sys/_types/_mode_t.h
c_maplot.o: /usr/include/sys/_types/_nlink_t.h
c_maplot.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_maplot.o: /usr/include/sys/_types/_uid_t.h
c_maplot.o: /usr/include/sys/_types/_clock_t.h
c_maplot.o: /usr/include/sys/_types/_time_t.h
c_maplot.o: /usr/include/sys/_types/_useconds_t.h
c_maplot.o: /usr/include/sys/_types/_suseconds_t.h
c_maplot.o: /usr/include/sys/_types/_rsize_t.h
c_maplot.o: /usr/include/sys/_types/_errno_t.h
c_maplot.o: /usr/include/sys/_types/_fd_def.h
c_maplot.o: /usr/include/sys/_types/_fd_setsize.h
c_maplot.o: /usr/include/sys/_types/_fd_set.h
c_maplot.o: /usr/include/sys/_types/_fd_clr.h
c_maplot.o: /usr/include/sys/_types/_fd_zero.h
c_maplot.o: /usr/include/sys/_types/_fd_isset.h
c_maplot.o: /usr/include/sys/_types/_fd_copy.h
c_maplot.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_maplot.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_maplot.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_maplot.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_maplot.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_maplot.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_maplot.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_maplot.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_maplot.o: /usr/include/sys/_pthread/_pthread_t.h
c_maplot.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_maplot.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_maplot.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_maplot.o: ../../../.././include/ncarg/c.h
c_mappos.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mappos.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mappos.o: /usr/include/sys/_posix_availability.h
c_mappos.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mappos.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mappos.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mappos.o: /usr/include/sys/_pthread/_pthread_types.h
c_mappos.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mappos.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mappos.o: /usr/include/sys/_types/_int16_t.h
c_mappos.o: /usr/include/sys/_types/_int32_t.h
c_mappos.o: /usr/include/sys/_types/_int64_t.h
c_mappos.o: /usr/include/sys/_types/_u_int8_t.h
c_mappos.o: /usr/include/sys/_types/_u_int16_t.h
c_mappos.o: /usr/include/sys/_types/_u_int32_t.h
c_mappos.o: /usr/include/sys/_types/_u_int64_t.h
c_mappos.o: /usr/include/sys/_types/_intptr_t.h
c_mappos.o: /usr/include/sys/_types/_uintptr_t.h
c_mappos.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mappos.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mappos.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mappos.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mappos.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mappos.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mappos.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mappos.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mappos.o: /usr/include/sys/_types/_u_char.h
c_mappos.o: /usr/include/sys/_types/_u_short.h
c_mappos.o: /usr/include/sys/_types/_u_int.h
c_mappos.o: /usr/include/sys/_types/_caddr_t.h
c_mappos.o: /usr/include/sys/_types/_dev_t.h
c_mappos.o: /usr/include/sys/_types/_blkcnt_t.h
c_mappos.o: /usr/include/sys/_types/_blksize_t.h
c_mappos.o: /usr/include/sys/_types/_gid_t.h
c_mappos.o: /usr/include/sys/_types/_in_addr_t.h
c_mappos.o: /usr/include/sys/_types/_in_port_t.h
c_mappos.o: /usr/include/sys/_types/_ino_t.h
c_mappos.o: /usr/include/sys/_types/_ino64_t.h
c_mappos.o: /usr/include/sys/_types/_key_t.h
c_mappos.o: /usr/include/sys/_types/_mode_t.h
c_mappos.o: /usr/include/sys/_types/_nlink_t.h
c_mappos.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mappos.o: /usr/include/sys/_types/_uid_t.h
c_mappos.o: /usr/include/sys/_types/_clock_t.h
c_mappos.o: /usr/include/sys/_types/_time_t.h
c_mappos.o: /usr/include/sys/_types/_useconds_t.h
c_mappos.o: /usr/include/sys/_types/_suseconds_t.h
c_mappos.o: /usr/include/sys/_types/_rsize_t.h
c_mappos.o: /usr/include/sys/_types/_errno_t.h
c_mappos.o: /usr/include/sys/_types/_fd_def.h
c_mappos.o: /usr/include/sys/_types/_fd_setsize.h
c_mappos.o: /usr/include/sys/_types/_fd_set.h
c_mappos.o: /usr/include/sys/_types/_fd_clr.h
c_mappos.o: /usr/include/sys/_types/_fd_zero.h
c_mappos.o: /usr/include/sys/_types/_fd_isset.h
c_mappos.o: /usr/include/sys/_types/_fd_copy.h
c_mappos.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mappos.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mappos.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mappos.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mappos.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mappos.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mappos.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mappos.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mappos.o: /usr/include/sys/_pthread/_pthread_t.h
c_mappos.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mappos.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mappos.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mappos.o: ../../../.././include/ncarg/c.h
c_maproj.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_maproj.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_maproj.o: /usr/include/sys/_posix_availability.h
c_maproj.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_maproj.o: /usr/include/_types.h /usr/include/sys/_types.h
c_maproj.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_maproj.o: /usr/include/sys/_pthread/_pthread_types.h
c_maproj.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_maproj.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_maproj.o: /usr/include/sys/_types/_int16_t.h
c_maproj.o: /usr/include/sys/_types/_int32_t.h
c_maproj.o: /usr/include/sys/_types/_int64_t.h
c_maproj.o: /usr/include/sys/_types/_u_int8_t.h
c_maproj.o: /usr/include/sys/_types/_u_int16_t.h
c_maproj.o: /usr/include/sys/_types/_u_int32_t.h
c_maproj.o: /usr/include/sys/_types/_u_int64_t.h
c_maproj.o: /usr/include/sys/_types/_intptr_t.h
c_maproj.o: /usr/include/sys/_types/_uintptr_t.h
c_maproj.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_maproj.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_maproj.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_maproj.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_maproj.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_maproj.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_maproj.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_maproj.o: /usr/include/libkern/i386/_OSByteOrder.h
c_maproj.o: /usr/include/sys/_types/_u_char.h
c_maproj.o: /usr/include/sys/_types/_u_short.h
c_maproj.o: /usr/include/sys/_types/_u_int.h
c_maproj.o: /usr/include/sys/_types/_caddr_t.h
c_maproj.o: /usr/include/sys/_types/_dev_t.h
c_maproj.o: /usr/include/sys/_types/_blkcnt_t.h
c_maproj.o: /usr/include/sys/_types/_blksize_t.h
c_maproj.o: /usr/include/sys/_types/_gid_t.h
c_maproj.o: /usr/include/sys/_types/_in_addr_t.h
c_maproj.o: /usr/include/sys/_types/_in_port_t.h
c_maproj.o: /usr/include/sys/_types/_ino_t.h
c_maproj.o: /usr/include/sys/_types/_ino64_t.h
c_maproj.o: /usr/include/sys/_types/_key_t.h
c_maproj.o: /usr/include/sys/_types/_mode_t.h
c_maproj.o: /usr/include/sys/_types/_nlink_t.h
c_maproj.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_maproj.o: /usr/include/sys/_types/_uid_t.h
c_maproj.o: /usr/include/sys/_types/_clock_t.h
c_maproj.o: /usr/include/sys/_types/_time_t.h
c_maproj.o: /usr/include/sys/_types/_useconds_t.h
c_maproj.o: /usr/include/sys/_types/_suseconds_t.h
c_maproj.o: /usr/include/sys/_types/_rsize_t.h
c_maproj.o: /usr/include/sys/_types/_errno_t.h
c_maproj.o: /usr/include/sys/_types/_fd_def.h
c_maproj.o: /usr/include/sys/_types/_fd_setsize.h
c_maproj.o: /usr/include/sys/_types/_fd_set.h
c_maproj.o: /usr/include/sys/_types/_fd_clr.h
c_maproj.o: /usr/include/sys/_types/_fd_zero.h
c_maproj.o: /usr/include/sys/_types/_fd_isset.h
c_maproj.o: /usr/include/sys/_types/_fd_copy.h
c_maproj.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_maproj.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_maproj.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_maproj.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_maproj.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_maproj.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_maproj.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_maproj.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_maproj.o: /usr/include/sys/_pthread/_pthread_t.h
c_maproj.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_maproj.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_maproj.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_maproj.o: ../../../.././include/ncarg/c.h
c_maprs.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_maprs.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_maprs.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_maprs.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_maprs.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_maprs.o: /usr/include/i386/_types.h
c_maprs.o: /usr/include/sys/_pthread/_pthread_types.h
c_maprs.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_maprs.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_maprs.o: /usr/include/sys/_types/_int16_t.h
c_maprs.o: /usr/include/sys/_types/_int32_t.h
c_maprs.o: /usr/include/sys/_types/_int64_t.h
c_maprs.o: /usr/include/sys/_types/_u_int8_t.h
c_maprs.o: /usr/include/sys/_types/_u_int16_t.h
c_maprs.o: /usr/include/sys/_types/_u_int32_t.h
c_maprs.o: /usr/include/sys/_types/_u_int64_t.h
c_maprs.o: /usr/include/sys/_types/_intptr_t.h
c_maprs.o: /usr/include/sys/_types/_uintptr_t.h
c_maprs.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_maprs.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_maprs.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_maprs.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_maprs.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_maprs.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_maprs.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_maprs.o: /usr/include/libkern/i386/_OSByteOrder.h
c_maprs.o: /usr/include/sys/_types/_u_char.h
c_maprs.o: /usr/include/sys/_types/_u_short.h
c_maprs.o: /usr/include/sys/_types/_u_int.h
c_maprs.o: /usr/include/sys/_types/_caddr_t.h
c_maprs.o: /usr/include/sys/_types/_dev_t.h
c_maprs.o: /usr/include/sys/_types/_blkcnt_t.h
c_maprs.o: /usr/include/sys/_types/_blksize_t.h
c_maprs.o: /usr/include/sys/_types/_gid_t.h
c_maprs.o: /usr/include/sys/_types/_in_addr_t.h
c_maprs.o: /usr/include/sys/_types/_in_port_t.h
c_maprs.o: /usr/include/sys/_types/_ino_t.h
c_maprs.o: /usr/include/sys/_types/_ino64_t.h
c_maprs.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_maprs.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_maprs.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_maprs.o: /usr/include/sys/_types/_clock_t.h
c_maprs.o: /usr/include/sys/_types/_time_t.h
c_maprs.o: /usr/include/sys/_types/_useconds_t.h
c_maprs.o: /usr/include/sys/_types/_suseconds_t.h
c_maprs.o: /usr/include/sys/_types/_rsize_t.h
c_maprs.o: /usr/include/sys/_types/_errno_t.h
c_maprs.o: /usr/include/sys/_types/_fd_def.h
c_maprs.o: /usr/include/sys/_types/_fd_setsize.h
c_maprs.o: /usr/include/sys/_types/_fd_set.h
c_maprs.o: /usr/include/sys/_types/_fd_clr.h
c_maprs.o: /usr/include/sys/_types/_fd_zero.h
c_maprs.o: /usr/include/sys/_types/_fd_isset.h
c_maprs.o: /usr/include/sys/_types/_fd_copy.h
c_maprs.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_maprs.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_maprs.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_maprs.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_maprs.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_maprs.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_maprs.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_maprs.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_maprs.o: /usr/include/sys/_pthread/_pthread_t.h
c_maprs.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_maprs.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_maprs.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_maprs.o: ../../../.././include/ncarg/c.h
c_maprst.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_maprst.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_maprst.o: /usr/include/sys/_posix_availability.h
c_maprst.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_maprst.o: /usr/include/_types.h /usr/include/sys/_types.h
c_maprst.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_maprst.o: /usr/include/sys/_pthread/_pthread_types.h
c_maprst.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_maprst.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_maprst.o: /usr/include/sys/_types/_int16_t.h
c_maprst.o: /usr/include/sys/_types/_int32_t.h
c_maprst.o: /usr/include/sys/_types/_int64_t.h
c_maprst.o: /usr/include/sys/_types/_u_int8_t.h
c_maprst.o: /usr/include/sys/_types/_u_int16_t.h
c_maprst.o: /usr/include/sys/_types/_u_int32_t.h
c_maprst.o: /usr/include/sys/_types/_u_int64_t.h
c_maprst.o: /usr/include/sys/_types/_intptr_t.h
c_maprst.o: /usr/include/sys/_types/_uintptr_t.h
c_maprst.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_maprst.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_maprst.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_maprst.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_maprst.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_maprst.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_maprst.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_maprst.o: /usr/include/libkern/i386/_OSByteOrder.h
c_maprst.o: /usr/include/sys/_types/_u_char.h
c_maprst.o: /usr/include/sys/_types/_u_short.h
c_maprst.o: /usr/include/sys/_types/_u_int.h
c_maprst.o: /usr/include/sys/_types/_caddr_t.h
c_maprst.o: /usr/include/sys/_types/_dev_t.h
c_maprst.o: /usr/include/sys/_types/_blkcnt_t.h
c_maprst.o: /usr/include/sys/_types/_blksize_t.h
c_maprst.o: /usr/include/sys/_types/_gid_t.h
c_maprst.o: /usr/include/sys/_types/_in_addr_t.h
c_maprst.o: /usr/include/sys/_types/_in_port_t.h
c_maprst.o: /usr/include/sys/_types/_ino_t.h
c_maprst.o: /usr/include/sys/_types/_ino64_t.h
c_maprst.o: /usr/include/sys/_types/_key_t.h
c_maprst.o: /usr/include/sys/_types/_mode_t.h
c_maprst.o: /usr/include/sys/_types/_nlink_t.h
c_maprst.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_maprst.o: /usr/include/sys/_types/_uid_t.h
c_maprst.o: /usr/include/sys/_types/_clock_t.h
c_maprst.o: /usr/include/sys/_types/_time_t.h
c_maprst.o: /usr/include/sys/_types/_useconds_t.h
c_maprst.o: /usr/include/sys/_types/_suseconds_t.h
c_maprst.o: /usr/include/sys/_types/_rsize_t.h
c_maprst.o: /usr/include/sys/_types/_errno_t.h
c_maprst.o: /usr/include/sys/_types/_fd_def.h
c_maprst.o: /usr/include/sys/_types/_fd_setsize.h
c_maprst.o: /usr/include/sys/_types/_fd_set.h
c_maprst.o: /usr/include/sys/_types/_fd_clr.h
c_maprst.o: /usr/include/sys/_types/_fd_zero.h
c_maprst.o: /usr/include/sys/_types/_fd_isset.h
c_maprst.o: /usr/include/sys/_types/_fd_copy.h
c_maprst.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_maprst.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_maprst.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_maprst.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_maprst.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_maprst.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_maprst.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_maprst.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_maprst.o: /usr/include/sys/_pthread/_pthread_t.h
c_maprst.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_maprst.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_maprst.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_maprst.o: ../../../.././include/ncarg/c.h
c_mapsav.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mapsav.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mapsav.o: /usr/include/sys/_posix_availability.h
c_mapsav.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mapsav.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mapsav.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mapsav.o: /usr/include/sys/_pthread/_pthread_types.h
c_mapsav.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mapsav.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mapsav.o: /usr/include/sys/_types/_int16_t.h
c_mapsav.o: /usr/include/sys/_types/_int32_t.h
c_mapsav.o: /usr/include/sys/_types/_int64_t.h
c_mapsav.o: /usr/include/sys/_types/_u_int8_t.h
c_mapsav.o: /usr/include/sys/_types/_u_int16_t.h
c_mapsav.o: /usr/include/sys/_types/_u_int32_t.h
c_mapsav.o: /usr/include/sys/_types/_u_int64_t.h
c_mapsav.o: /usr/include/sys/_types/_intptr_t.h
c_mapsav.o: /usr/include/sys/_types/_uintptr_t.h
c_mapsav.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mapsav.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mapsav.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mapsav.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mapsav.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mapsav.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mapsav.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mapsav.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mapsav.o: /usr/include/sys/_types/_u_char.h
c_mapsav.o: /usr/include/sys/_types/_u_short.h
c_mapsav.o: /usr/include/sys/_types/_u_int.h
c_mapsav.o: /usr/include/sys/_types/_caddr_t.h
c_mapsav.o: /usr/include/sys/_types/_dev_t.h
c_mapsav.o: /usr/include/sys/_types/_blkcnt_t.h
c_mapsav.o: /usr/include/sys/_types/_blksize_t.h
c_mapsav.o: /usr/include/sys/_types/_gid_t.h
c_mapsav.o: /usr/include/sys/_types/_in_addr_t.h
c_mapsav.o: /usr/include/sys/_types/_in_port_t.h
c_mapsav.o: /usr/include/sys/_types/_ino_t.h
c_mapsav.o: /usr/include/sys/_types/_ino64_t.h
c_mapsav.o: /usr/include/sys/_types/_key_t.h
c_mapsav.o: /usr/include/sys/_types/_mode_t.h
c_mapsav.o: /usr/include/sys/_types/_nlink_t.h
c_mapsav.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mapsav.o: /usr/include/sys/_types/_uid_t.h
c_mapsav.o: /usr/include/sys/_types/_clock_t.h
c_mapsav.o: /usr/include/sys/_types/_time_t.h
c_mapsav.o: /usr/include/sys/_types/_useconds_t.h
c_mapsav.o: /usr/include/sys/_types/_suseconds_t.h
c_mapsav.o: /usr/include/sys/_types/_rsize_t.h
c_mapsav.o: /usr/include/sys/_types/_errno_t.h
c_mapsav.o: /usr/include/sys/_types/_fd_def.h
c_mapsav.o: /usr/include/sys/_types/_fd_setsize.h
c_mapsav.o: /usr/include/sys/_types/_fd_set.h
c_mapsav.o: /usr/include/sys/_types/_fd_clr.h
c_mapsav.o: /usr/include/sys/_types/_fd_zero.h
c_mapsav.o: /usr/include/sys/_types/_fd_isset.h
c_mapsav.o: /usr/include/sys/_types/_fd_copy.h
c_mapsav.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mapsav.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mapsav.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mapsav.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mapsav.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mapsav.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mapsav.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mapsav.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mapsav.o: /usr/include/sys/_pthread/_pthread_t.h
c_mapsav.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mapsav.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mapsav.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mapsav.o: ../../../.././include/ncarg/c.h
c_mapset.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mapset.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mapset.o: /usr/include/sys/_posix_availability.h
c_mapset.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mapset.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mapset.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mapset.o: /usr/include/sys/_pthread/_pthread_types.h
c_mapset.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mapset.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mapset.o: /usr/include/sys/_types/_int16_t.h
c_mapset.o: /usr/include/sys/_types/_int32_t.h
c_mapset.o: /usr/include/sys/_types/_int64_t.h
c_mapset.o: /usr/include/sys/_types/_u_int8_t.h
c_mapset.o: /usr/include/sys/_types/_u_int16_t.h
c_mapset.o: /usr/include/sys/_types/_u_int32_t.h
c_mapset.o: /usr/include/sys/_types/_u_int64_t.h
c_mapset.o: /usr/include/sys/_types/_intptr_t.h
c_mapset.o: /usr/include/sys/_types/_uintptr_t.h
c_mapset.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mapset.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mapset.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mapset.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mapset.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mapset.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mapset.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mapset.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mapset.o: /usr/include/sys/_types/_u_char.h
c_mapset.o: /usr/include/sys/_types/_u_short.h
c_mapset.o: /usr/include/sys/_types/_u_int.h
c_mapset.o: /usr/include/sys/_types/_caddr_t.h
c_mapset.o: /usr/include/sys/_types/_dev_t.h
c_mapset.o: /usr/include/sys/_types/_blkcnt_t.h
c_mapset.o: /usr/include/sys/_types/_blksize_t.h
c_mapset.o: /usr/include/sys/_types/_gid_t.h
c_mapset.o: /usr/include/sys/_types/_in_addr_t.h
c_mapset.o: /usr/include/sys/_types/_in_port_t.h
c_mapset.o: /usr/include/sys/_types/_ino_t.h
c_mapset.o: /usr/include/sys/_types/_ino64_t.h
c_mapset.o: /usr/include/sys/_types/_key_t.h
c_mapset.o: /usr/include/sys/_types/_mode_t.h
c_mapset.o: /usr/include/sys/_types/_nlink_t.h
c_mapset.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mapset.o: /usr/include/sys/_types/_uid_t.h
c_mapset.o: /usr/include/sys/_types/_clock_t.h
c_mapset.o: /usr/include/sys/_types/_time_t.h
c_mapset.o: /usr/include/sys/_types/_useconds_t.h
c_mapset.o: /usr/include/sys/_types/_suseconds_t.h
c_mapset.o: /usr/include/sys/_types/_rsize_t.h
c_mapset.o: /usr/include/sys/_types/_errno_t.h
c_mapset.o: /usr/include/sys/_types/_fd_def.h
c_mapset.o: /usr/include/sys/_types/_fd_setsize.h
c_mapset.o: /usr/include/sys/_types/_fd_set.h
c_mapset.o: /usr/include/sys/_types/_fd_clr.h
c_mapset.o: /usr/include/sys/_types/_fd_zero.h
c_mapset.o: /usr/include/sys/_types/_fd_isset.h
c_mapset.o: /usr/include/sys/_types/_fd_copy.h
c_mapset.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mapset.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mapset.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mapset.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mapset.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mapset.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mapset.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mapset.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mapset.o: /usr/include/sys/_pthread/_pthread_t.h
c_mapset.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mapset.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mapset.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mapset.o: ../../../.././include/ncarg/c.h
c_mapstc.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mapstc.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mapstc.o: /usr/include/sys/_posix_availability.h
c_mapstc.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mapstc.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mapstc.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mapstc.o: /usr/include/sys/_pthread/_pthread_types.h
c_mapstc.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mapstc.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mapstc.o: /usr/include/sys/_types/_int16_t.h
c_mapstc.o: /usr/include/sys/_types/_int32_t.h
c_mapstc.o: /usr/include/sys/_types/_int64_t.h
c_mapstc.o: /usr/include/sys/_types/_u_int8_t.h
c_mapstc.o: /usr/include/sys/_types/_u_int16_t.h
c_mapstc.o: /usr/include/sys/_types/_u_int32_t.h
c_mapstc.o: /usr/include/sys/_types/_u_int64_t.h
c_mapstc.o: /usr/include/sys/_types/_intptr_t.h
c_mapstc.o: /usr/include/sys/_types/_uintptr_t.h
c_mapstc.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mapstc.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mapstc.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mapstc.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mapstc.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mapstc.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mapstc.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mapstc.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mapstc.o: /usr/include/sys/_types/_u_char.h
c_mapstc.o: /usr/include/sys/_types/_u_short.h
c_mapstc.o: /usr/include/sys/_types/_u_int.h
c_mapstc.o: /usr/include/sys/_types/_caddr_t.h
c_mapstc.o: /usr/include/sys/_types/_dev_t.h
c_mapstc.o: /usr/include/sys/_types/_blkcnt_t.h
c_mapstc.o: /usr/include/sys/_types/_blksize_t.h
c_mapstc.o: /usr/include/sys/_types/_gid_t.h
c_mapstc.o: /usr/include/sys/_types/_in_addr_t.h
c_mapstc.o: /usr/include/sys/_types/_in_port_t.h
c_mapstc.o: /usr/include/sys/_types/_ino_t.h
c_mapstc.o: /usr/include/sys/_types/_ino64_t.h
c_mapstc.o: /usr/include/sys/_types/_key_t.h
c_mapstc.o: /usr/include/sys/_types/_mode_t.h
c_mapstc.o: /usr/include/sys/_types/_nlink_t.h
c_mapstc.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mapstc.o: /usr/include/sys/_types/_uid_t.h
c_mapstc.o: /usr/include/sys/_types/_clock_t.h
c_mapstc.o: /usr/include/sys/_types/_time_t.h
c_mapstc.o: /usr/include/sys/_types/_useconds_t.h
c_mapstc.o: /usr/include/sys/_types/_suseconds_t.h
c_mapstc.o: /usr/include/sys/_types/_rsize_t.h
c_mapstc.o: /usr/include/sys/_types/_errno_t.h
c_mapstc.o: /usr/include/sys/_types/_fd_def.h
c_mapstc.o: /usr/include/sys/_types/_fd_setsize.h
c_mapstc.o: /usr/include/sys/_types/_fd_set.h
c_mapstc.o: /usr/include/sys/_types/_fd_clr.h
c_mapstc.o: /usr/include/sys/_types/_fd_zero.h
c_mapstc.o: /usr/include/sys/_types/_fd_isset.h
c_mapstc.o: /usr/include/sys/_types/_fd_copy.h
c_mapstc.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mapstc.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mapstc.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mapstc.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mapstc.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mapstc.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mapstc.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mapstc.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mapstc.o: /usr/include/sys/_pthread/_pthread_t.h
c_mapstc.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mapstc.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mapstc.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mapstc.o: ../../../.././include/ncarg/c.h
c_mapsti.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mapsti.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mapsti.o: /usr/include/sys/_posix_availability.h
c_mapsti.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mapsti.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mapsti.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mapsti.o: /usr/include/sys/_pthread/_pthread_types.h
c_mapsti.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mapsti.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mapsti.o: /usr/include/sys/_types/_int16_t.h
c_mapsti.o: /usr/include/sys/_types/_int32_t.h
c_mapsti.o: /usr/include/sys/_types/_int64_t.h
c_mapsti.o: /usr/include/sys/_types/_u_int8_t.h
c_mapsti.o: /usr/include/sys/_types/_u_int16_t.h
c_mapsti.o: /usr/include/sys/_types/_u_int32_t.h
c_mapsti.o: /usr/include/sys/_types/_u_int64_t.h
c_mapsti.o: /usr/include/sys/_types/_intptr_t.h
c_mapsti.o: /usr/include/sys/_types/_uintptr_t.h
c_mapsti.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mapsti.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mapsti.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mapsti.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mapsti.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mapsti.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mapsti.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mapsti.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mapsti.o: /usr/include/sys/_types/_u_char.h
c_mapsti.o: /usr/include/sys/_types/_u_short.h
c_mapsti.o: /usr/include/sys/_types/_u_int.h
c_mapsti.o: /usr/include/sys/_types/_caddr_t.h
c_mapsti.o: /usr/include/sys/_types/_dev_t.h
c_mapsti.o: /usr/include/sys/_types/_blkcnt_t.h
c_mapsti.o: /usr/include/sys/_types/_blksize_t.h
c_mapsti.o: /usr/include/sys/_types/_gid_t.h
c_mapsti.o: /usr/include/sys/_types/_in_addr_t.h
c_mapsti.o: /usr/include/sys/_types/_in_port_t.h
c_mapsti.o: /usr/include/sys/_types/_ino_t.h
c_mapsti.o: /usr/include/sys/_types/_ino64_t.h
c_mapsti.o: /usr/include/sys/_types/_key_t.h
c_mapsti.o: /usr/include/sys/_types/_mode_t.h
c_mapsti.o: /usr/include/sys/_types/_nlink_t.h
c_mapsti.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mapsti.o: /usr/include/sys/_types/_uid_t.h
c_mapsti.o: /usr/include/sys/_types/_clock_t.h
c_mapsti.o: /usr/include/sys/_types/_time_t.h
c_mapsti.o: /usr/include/sys/_types/_useconds_t.h
c_mapsti.o: /usr/include/sys/_types/_suseconds_t.h
c_mapsti.o: /usr/include/sys/_types/_rsize_t.h
c_mapsti.o: /usr/include/sys/_types/_errno_t.h
c_mapsti.o: /usr/include/sys/_types/_fd_def.h
c_mapsti.o: /usr/include/sys/_types/_fd_setsize.h
c_mapsti.o: /usr/include/sys/_types/_fd_set.h
c_mapsti.o: /usr/include/sys/_types/_fd_clr.h
c_mapsti.o: /usr/include/sys/_types/_fd_zero.h
c_mapsti.o: /usr/include/sys/_types/_fd_isset.h
c_mapsti.o: /usr/include/sys/_types/_fd_copy.h
c_mapsti.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mapsti.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mapsti.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mapsti.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mapsti.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mapsti.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mapsti.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mapsti.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mapsti.o: /usr/include/sys/_pthread/_pthread_t.h
c_mapsti.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mapsti.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mapsti.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mapsti.o: ../../../.././include/ncarg/c.h
c_mapstl.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mapstl.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mapstl.o: /usr/include/sys/_posix_availability.h
c_mapstl.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mapstl.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mapstl.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mapstl.o: /usr/include/sys/_pthread/_pthread_types.h
c_mapstl.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mapstl.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mapstl.o: /usr/include/sys/_types/_int16_t.h
c_mapstl.o: /usr/include/sys/_types/_int32_t.h
c_mapstl.o: /usr/include/sys/_types/_int64_t.h
c_mapstl.o: /usr/include/sys/_types/_u_int8_t.h
c_mapstl.o: /usr/include/sys/_types/_u_int16_t.h
c_mapstl.o: /usr/include/sys/_types/_u_int32_t.h
c_mapstl.o: /usr/include/sys/_types/_u_int64_t.h
c_mapstl.o: /usr/include/sys/_types/_intptr_t.h
c_mapstl.o: /usr/include/sys/_types/_uintptr_t.h
c_mapstl.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mapstl.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mapstl.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mapstl.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mapstl.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mapstl.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mapstl.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mapstl.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mapstl.o: /usr/include/sys/_types/_u_char.h
c_mapstl.o: /usr/include/sys/_types/_u_short.h
c_mapstl.o: /usr/include/sys/_types/_u_int.h
c_mapstl.o: /usr/include/sys/_types/_caddr_t.h
c_mapstl.o: /usr/include/sys/_types/_dev_t.h
c_mapstl.o: /usr/include/sys/_types/_blkcnt_t.h
c_mapstl.o: /usr/include/sys/_types/_blksize_t.h
c_mapstl.o: /usr/include/sys/_types/_gid_t.h
c_mapstl.o: /usr/include/sys/_types/_in_addr_t.h
c_mapstl.o: /usr/include/sys/_types/_in_port_t.h
c_mapstl.o: /usr/include/sys/_types/_ino_t.h
c_mapstl.o: /usr/include/sys/_types/_ino64_t.h
c_mapstl.o: /usr/include/sys/_types/_key_t.h
c_mapstl.o: /usr/include/sys/_types/_mode_t.h
c_mapstl.o: /usr/include/sys/_types/_nlink_t.h
c_mapstl.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mapstl.o: /usr/include/sys/_types/_uid_t.h
c_mapstl.o: /usr/include/sys/_types/_clock_t.h
c_mapstl.o: /usr/include/sys/_types/_time_t.h
c_mapstl.o: /usr/include/sys/_types/_useconds_t.h
c_mapstl.o: /usr/include/sys/_types/_suseconds_t.h
c_mapstl.o: /usr/include/sys/_types/_rsize_t.h
c_mapstl.o: /usr/include/sys/_types/_errno_t.h
c_mapstl.o: /usr/include/sys/_types/_fd_def.h
c_mapstl.o: /usr/include/sys/_types/_fd_setsize.h
c_mapstl.o: /usr/include/sys/_types/_fd_set.h
c_mapstl.o: /usr/include/sys/_types/_fd_clr.h
c_mapstl.o: /usr/include/sys/_types/_fd_zero.h
c_mapstl.o: /usr/include/sys/_types/_fd_isset.h
c_mapstl.o: /usr/include/sys/_types/_fd_copy.h
c_mapstl.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mapstl.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mapstl.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mapstl.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mapstl.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mapstl.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mapstl.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mapstl.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mapstl.o: /usr/include/sys/_pthread/_pthread_t.h
c_mapstl.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mapstl.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mapstl.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mapstl.o: ../../../.././include/ncarg/c.h
c_mapstr.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mapstr.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mapstr.o: /usr/include/sys/_posix_availability.h
c_mapstr.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mapstr.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mapstr.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mapstr.o: /usr/include/sys/_pthread/_pthread_types.h
c_mapstr.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mapstr.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mapstr.o: /usr/include/sys/_types/_int16_t.h
c_mapstr.o: /usr/include/sys/_types/_int32_t.h
c_mapstr.o: /usr/include/sys/_types/_int64_t.h
c_mapstr.o: /usr/include/sys/_types/_u_int8_t.h
c_mapstr.o: /usr/include/sys/_types/_u_int16_t.h
c_mapstr.o: /usr/include/sys/_types/_u_int32_t.h
c_mapstr.o: /usr/include/sys/_types/_u_int64_t.h
c_mapstr.o: /usr/include/sys/_types/_intptr_t.h
c_mapstr.o: /usr/include/sys/_types/_uintptr_t.h
c_mapstr.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mapstr.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mapstr.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mapstr.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mapstr.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mapstr.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mapstr.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mapstr.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mapstr.o: /usr/include/sys/_types/_u_char.h
c_mapstr.o: /usr/include/sys/_types/_u_short.h
c_mapstr.o: /usr/include/sys/_types/_u_int.h
c_mapstr.o: /usr/include/sys/_types/_caddr_t.h
c_mapstr.o: /usr/include/sys/_types/_dev_t.h
c_mapstr.o: /usr/include/sys/_types/_blkcnt_t.h
c_mapstr.o: /usr/include/sys/_types/_blksize_t.h
c_mapstr.o: /usr/include/sys/_types/_gid_t.h
c_mapstr.o: /usr/include/sys/_types/_in_addr_t.h
c_mapstr.o: /usr/include/sys/_types/_in_port_t.h
c_mapstr.o: /usr/include/sys/_types/_ino_t.h
c_mapstr.o: /usr/include/sys/_types/_ino64_t.h
c_mapstr.o: /usr/include/sys/_types/_key_t.h
c_mapstr.o: /usr/include/sys/_types/_mode_t.h
c_mapstr.o: /usr/include/sys/_types/_nlink_t.h
c_mapstr.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mapstr.o: /usr/include/sys/_types/_uid_t.h
c_mapstr.o: /usr/include/sys/_types/_clock_t.h
c_mapstr.o: /usr/include/sys/_types/_time_t.h
c_mapstr.o: /usr/include/sys/_types/_useconds_t.h
c_mapstr.o: /usr/include/sys/_types/_suseconds_t.h
c_mapstr.o: /usr/include/sys/_types/_rsize_t.h
c_mapstr.o: /usr/include/sys/_types/_errno_t.h
c_mapstr.o: /usr/include/sys/_types/_fd_def.h
c_mapstr.o: /usr/include/sys/_types/_fd_setsize.h
c_mapstr.o: /usr/include/sys/_types/_fd_set.h
c_mapstr.o: /usr/include/sys/_types/_fd_clr.h
c_mapstr.o: /usr/include/sys/_types/_fd_zero.h
c_mapstr.o: /usr/include/sys/_types/_fd_isset.h
c_mapstr.o: /usr/include/sys/_types/_fd_copy.h
c_mapstr.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mapstr.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mapstr.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mapstr.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mapstr.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mapstr.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mapstr.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mapstr.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mapstr.o: /usr/include/sys/_pthread/_pthread_t.h
c_mapstr.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mapstr.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mapstr.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mapstr.o: ../../../.././include/ncarg/c.h
c_maptri.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_maptri.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_maptri.o: /usr/include/sys/_posix_availability.h
c_maptri.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_maptri.o: /usr/include/_types.h /usr/include/sys/_types.h
c_maptri.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_maptri.o: /usr/include/sys/_pthread/_pthread_types.h
c_maptri.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_maptri.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_maptri.o: /usr/include/sys/_types/_int16_t.h
c_maptri.o: /usr/include/sys/_types/_int32_t.h
c_maptri.o: /usr/include/sys/_types/_int64_t.h
c_maptri.o: /usr/include/sys/_types/_u_int8_t.h
c_maptri.o: /usr/include/sys/_types/_u_int16_t.h
c_maptri.o: /usr/include/sys/_types/_u_int32_t.h
c_maptri.o: /usr/include/sys/_types/_u_int64_t.h
c_maptri.o: /usr/include/sys/_types/_intptr_t.h
c_maptri.o: /usr/include/sys/_types/_uintptr_t.h
c_maptri.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_maptri.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_maptri.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_maptri.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_maptri.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_maptri.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_maptri.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_maptri.o: /usr/include/libkern/i386/_OSByteOrder.h
c_maptri.o: /usr/include/sys/_types/_u_char.h
c_maptri.o: /usr/include/sys/_types/_u_short.h
c_maptri.o: /usr/include/sys/_types/_u_int.h
c_maptri.o: /usr/include/sys/_types/_caddr_t.h
c_maptri.o: /usr/include/sys/_types/_dev_t.h
c_maptri.o: /usr/include/sys/_types/_blkcnt_t.h
c_maptri.o: /usr/include/sys/_types/_blksize_t.h
c_maptri.o: /usr/include/sys/_types/_gid_t.h
c_maptri.o: /usr/include/sys/_types/_in_addr_t.h
c_maptri.o: /usr/include/sys/_types/_in_port_t.h
c_maptri.o: /usr/include/sys/_types/_ino_t.h
c_maptri.o: /usr/include/sys/_types/_ino64_t.h
c_maptri.o: /usr/include/sys/_types/_key_t.h
c_maptri.o: /usr/include/sys/_types/_mode_t.h
c_maptri.o: /usr/include/sys/_types/_nlink_t.h
c_maptri.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_maptri.o: /usr/include/sys/_types/_uid_t.h
c_maptri.o: /usr/include/sys/_types/_clock_t.h
c_maptri.o: /usr/include/sys/_types/_time_t.h
c_maptri.o: /usr/include/sys/_types/_useconds_t.h
c_maptri.o: /usr/include/sys/_types/_suseconds_t.h
c_maptri.o: /usr/include/sys/_types/_rsize_t.h
c_maptri.o: /usr/include/sys/_types/_errno_t.h
c_maptri.o: /usr/include/sys/_types/_fd_def.h
c_maptri.o: /usr/include/sys/_types/_fd_setsize.h
c_maptri.o: /usr/include/sys/_types/_fd_set.h
c_maptri.o: /usr/include/sys/_types/_fd_clr.h
c_maptri.o: /usr/include/sys/_types/_fd_zero.h
c_maptri.o: /usr/include/sys/_types/_fd_isset.h
c_maptri.o: /usr/include/sys/_types/_fd_copy.h
c_maptri.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_maptri.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_maptri.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_maptri.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_maptri.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_maptri.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_maptri.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_maptri.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_maptri.o: /usr/include/sys/_pthread/_pthread_t.h
c_maptri.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_maptri.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_maptri.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_maptri.o: ../../../.././include/ncarg/c.h
c_maptrn.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_maptrn.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_maptrn.o: /usr/include/sys/_posix_availability.h
c_maptrn.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_maptrn.o: /usr/include/_types.h /usr/include/sys/_types.h
c_maptrn.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_maptrn.o: /usr/include/sys/_pthread/_pthread_types.h
c_maptrn.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_maptrn.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_maptrn.o: /usr/include/sys/_types/_int16_t.h
c_maptrn.o: /usr/include/sys/_types/_int32_t.h
c_maptrn.o: /usr/include/sys/_types/_int64_t.h
c_maptrn.o: /usr/include/sys/_types/_u_int8_t.h
c_maptrn.o: /usr/include/sys/_types/_u_int16_t.h
c_maptrn.o: /usr/include/sys/_types/_u_int32_t.h
c_maptrn.o: /usr/include/sys/_types/_u_int64_t.h
c_maptrn.o: /usr/include/sys/_types/_intptr_t.h
c_maptrn.o: /usr/include/sys/_types/_uintptr_t.h
c_maptrn.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_maptrn.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_maptrn.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_maptrn.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_maptrn.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_maptrn.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_maptrn.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_maptrn.o: /usr/include/libkern/i386/_OSByteOrder.h
c_maptrn.o: /usr/include/sys/_types/_u_char.h
c_maptrn.o: /usr/include/sys/_types/_u_short.h
c_maptrn.o: /usr/include/sys/_types/_u_int.h
c_maptrn.o: /usr/include/sys/_types/_caddr_t.h
c_maptrn.o: /usr/include/sys/_types/_dev_t.h
c_maptrn.o: /usr/include/sys/_types/_blkcnt_t.h
c_maptrn.o: /usr/include/sys/_types/_blksize_t.h
c_maptrn.o: /usr/include/sys/_types/_gid_t.h
c_maptrn.o: /usr/include/sys/_types/_in_addr_t.h
c_maptrn.o: /usr/include/sys/_types/_in_port_t.h
c_maptrn.o: /usr/include/sys/_types/_ino_t.h
c_maptrn.o: /usr/include/sys/_types/_ino64_t.h
c_maptrn.o: /usr/include/sys/_types/_key_t.h
c_maptrn.o: /usr/include/sys/_types/_mode_t.h
c_maptrn.o: /usr/include/sys/_types/_nlink_t.h
c_maptrn.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_maptrn.o: /usr/include/sys/_types/_uid_t.h
c_maptrn.o: /usr/include/sys/_types/_clock_t.h
c_maptrn.o: /usr/include/sys/_types/_time_t.h
c_maptrn.o: /usr/include/sys/_types/_useconds_t.h
c_maptrn.o: /usr/include/sys/_types/_suseconds_t.h
c_maptrn.o: /usr/include/sys/_types/_rsize_t.h
c_maptrn.o: /usr/include/sys/_types/_errno_t.h
c_maptrn.o: /usr/include/sys/_types/_fd_def.h
c_maptrn.o: /usr/include/sys/_types/_fd_setsize.h
c_maptrn.o: /usr/include/sys/_types/_fd_set.h
c_maptrn.o: /usr/include/sys/_types/_fd_clr.h
c_maptrn.o: /usr/include/sys/_types/_fd_zero.h
c_maptrn.o: /usr/include/sys/_types/_fd_isset.h
c_maptrn.o: /usr/include/sys/_types/_fd_copy.h
c_maptrn.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_maptrn.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_maptrn.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_maptrn.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_maptrn.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_maptrn.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_maptrn.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_maptrn.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_maptrn.o: /usr/include/sys/_pthread/_pthread_t.h
c_maptrn.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_maptrn.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_maptrn.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_maptrn.o: ../../../.././include/ncarg/c.h
c_mapvec.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mapvec.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mapvec.o: /usr/include/sys/_posix_availability.h
c_mapvec.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mapvec.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mapvec.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mapvec.o: /usr/include/sys/_pthread/_pthread_types.h
c_mapvec.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mapvec.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mapvec.o: /usr/include/sys/_types/_int16_t.h
c_mapvec.o: /usr/include/sys/_types/_int32_t.h
c_mapvec.o: /usr/include/sys/_types/_int64_t.h
c_mapvec.o: /usr/include/sys/_types/_u_int8_t.h
c_mapvec.o: /usr/include/sys/_types/_u_int16_t.h
c_mapvec.o: /usr/include/sys/_types/_u_int32_t.h
c_mapvec.o: /usr/include/sys/_types/_u_int64_t.h
c_mapvec.o: /usr/include/sys/_types/_intptr_t.h
c_mapvec.o: /usr/include/sys/_types/_uintptr_t.h
c_mapvec.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mapvec.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mapvec.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mapvec.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mapvec.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mapvec.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mapvec.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mapvec.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mapvec.o: /usr/include/sys/_types/_u_char.h
c_mapvec.o: /usr/include/sys/_types/_u_short.h
c_mapvec.o: /usr/include/sys/_types/_u_int.h
c_mapvec.o: /usr/include/sys/_types/_caddr_t.h
c_mapvec.o: /usr/include/sys/_types/_dev_t.h
c_mapvec.o: /usr/include/sys/_types/_blkcnt_t.h
c_mapvec.o: /usr/include/sys/_types/_blksize_t.h
c_mapvec.o: /usr/include/sys/_types/_gid_t.h
c_mapvec.o: /usr/include/sys/_types/_in_addr_t.h
c_mapvec.o: /usr/include/sys/_types/_in_port_t.h
c_mapvec.o: /usr/include/sys/_types/_ino_t.h
c_mapvec.o: /usr/include/sys/_types/_ino64_t.h
c_mapvec.o: /usr/include/sys/_types/_key_t.h
c_mapvec.o: /usr/include/sys/_types/_mode_t.h
c_mapvec.o: /usr/include/sys/_types/_nlink_t.h
c_mapvec.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mapvec.o: /usr/include/sys/_types/_uid_t.h
c_mapvec.o: /usr/include/sys/_types/_clock_t.h
c_mapvec.o: /usr/include/sys/_types/_time_t.h
c_mapvec.o: /usr/include/sys/_types/_useconds_t.h
c_mapvec.o: /usr/include/sys/_types/_suseconds_t.h
c_mapvec.o: /usr/include/sys/_types/_rsize_t.h
c_mapvec.o: /usr/include/sys/_types/_errno_t.h
c_mapvec.o: /usr/include/sys/_types/_fd_def.h
c_mapvec.o: /usr/include/sys/_types/_fd_setsize.h
c_mapvec.o: /usr/include/sys/_types/_fd_set.h
c_mapvec.o: /usr/include/sys/_types/_fd_clr.h
c_mapvec.o: /usr/include/sys/_types/_fd_zero.h
c_mapvec.o: /usr/include/sys/_types/_fd_isset.h
c_mapvec.o: /usr/include/sys/_types/_fd_copy.h
c_mapvec.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mapvec.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mapvec.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mapvec.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mapvec.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mapvec.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mapvec.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mapvec.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mapvec.o: /usr/include/sys/_pthread/_pthread_t.h
c_mapvec.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mapvec.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mapvec.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mapvec.o: ../../../.././include/ncarg/c.h
c_supcon.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_supcon.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_supcon.o: /usr/include/sys/_posix_availability.h
c_supcon.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_supcon.o: /usr/include/_types.h /usr/include/sys/_types.h
c_supcon.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_supcon.o: /usr/include/sys/_pthread/_pthread_types.h
c_supcon.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_supcon.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_supcon.o: /usr/include/sys/_types/_int16_t.h
c_supcon.o: /usr/include/sys/_types/_int32_t.h
c_supcon.o: /usr/include/sys/_types/_int64_t.h
c_supcon.o: /usr/include/sys/_types/_u_int8_t.h
c_supcon.o: /usr/include/sys/_types/_u_int16_t.h
c_supcon.o: /usr/include/sys/_types/_u_int32_t.h
c_supcon.o: /usr/include/sys/_types/_u_int64_t.h
c_supcon.o: /usr/include/sys/_types/_intptr_t.h
c_supcon.o: /usr/include/sys/_types/_uintptr_t.h
c_supcon.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_supcon.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_supcon.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_supcon.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_supcon.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_supcon.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_supcon.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_supcon.o: /usr/include/libkern/i386/_OSByteOrder.h
c_supcon.o: /usr/include/sys/_types/_u_char.h
c_supcon.o: /usr/include/sys/_types/_u_short.h
c_supcon.o: /usr/include/sys/_types/_u_int.h
c_supcon.o: /usr/include/sys/_types/_caddr_t.h
c_supcon.o: /usr/include/sys/_types/_dev_t.h
c_supcon.o: /usr/include/sys/_types/_blkcnt_t.h
c_supcon.o: /usr/include/sys/_types/_blksize_t.h
c_supcon.o: /usr/include/sys/_types/_gid_t.h
c_supcon.o: /usr/include/sys/_types/_in_addr_t.h
c_supcon.o: /usr/include/sys/_types/_in_port_t.h
c_supcon.o: /usr/include/sys/_types/_ino_t.h
c_supcon.o: /usr/include/sys/_types/_ino64_t.h
c_supcon.o: /usr/include/sys/_types/_key_t.h
c_supcon.o: /usr/include/sys/_types/_mode_t.h
c_supcon.o: /usr/include/sys/_types/_nlink_t.h
c_supcon.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_supcon.o: /usr/include/sys/_types/_uid_t.h
c_supcon.o: /usr/include/sys/_types/_clock_t.h
c_supcon.o: /usr/include/sys/_types/_time_t.h
c_supcon.o: /usr/include/sys/_types/_useconds_t.h
c_supcon.o: /usr/include/sys/_types/_suseconds_t.h
c_supcon.o: /usr/include/sys/_types/_rsize_t.h
c_supcon.o: /usr/include/sys/_types/_errno_t.h
c_supcon.o: /usr/include/sys/_types/_fd_def.h
c_supcon.o: /usr/include/sys/_types/_fd_setsize.h
c_supcon.o: /usr/include/sys/_types/_fd_set.h
c_supcon.o: /usr/include/sys/_types/_fd_clr.h
c_supcon.o: /usr/include/sys/_types/_fd_zero.h
c_supcon.o: /usr/include/sys/_types/_fd_isset.h
c_supcon.o: /usr/include/sys/_types/_fd_copy.h
c_supcon.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_supcon.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_supcon.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_supcon.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_supcon.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_supcon.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_supcon.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_supcon.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_supcon.o: /usr/include/sys/_pthread/_pthread_t.h
c_supcon.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_supcon.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_supcon.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_supcon.o: ../../../.././include/ncarg/c.h
c_supmap.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_supmap.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_supmap.o: /usr/include/sys/_posix_availability.h
c_supmap.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_supmap.o: /usr/include/_types.h /usr/include/sys/_types.h
c_supmap.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_supmap.o: /usr/include/sys/_pthread/_pthread_types.h
c_supmap.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_supmap.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_supmap.o: /usr/include/sys/_types/_int16_t.h
c_supmap.o: /usr/include/sys/_types/_int32_t.h
c_supmap.o: /usr/include/sys/_types/_int64_t.h
c_supmap.o: /usr/include/sys/_types/_u_int8_t.h
c_supmap.o: /usr/include/sys/_types/_u_int16_t.h
c_supmap.o: /usr/include/sys/_types/_u_int32_t.h
c_supmap.o: /usr/include/sys/_types/_u_int64_t.h
c_supmap.o: /usr/include/sys/_types/_intptr_t.h
c_supmap.o: /usr/include/sys/_types/_uintptr_t.h
c_supmap.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_supmap.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_supmap.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_supmap.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_supmap.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_supmap.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_supmap.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_supmap.o: /usr/include/libkern/i386/_OSByteOrder.h
c_supmap.o: /usr/include/sys/_types/_u_char.h
c_supmap.o: /usr/include/sys/_types/_u_short.h
c_supmap.o: /usr/include/sys/_types/_u_int.h
c_supmap.o: /usr/include/sys/_types/_caddr_t.h
c_supmap.o: /usr/include/sys/_types/_dev_t.h
c_supmap.o: /usr/include/sys/_types/_blkcnt_t.h
c_supmap.o: /usr/include/sys/_types/_blksize_t.h
c_supmap.o: /usr/include/sys/_types/_gid_t.h
c_supmap.o: /usr/include/sys/_types/_in_addr_t.h
c_supmap.o: /usr/include/sys/_types/_in_port_t.h
c_supmap.o: /usr/include/sys/_types/_ino_t.h
c_supmap.o: /usr/include/sys/_types/_ino64_t.h
c_supmap.o: /usr/include/sys/_types/_key_t.h
c_supmap.o: /usr/include/sys/_types/_mode_t.h
c_supmap.o: /usr/include/sys/_types/_nlink_t.h
c_supmap.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_supmap.o: /usr/include/sys/_types/_uid_t.h
c_supmap.o: /usr/include/sys/_types/_clock_t.h
c_supmap.o: /usr/include/sys/_types/_time_t.h
c_supmap.o: /usr/include/sys/_types/_useconds_t.h
c_supmap.o: /usr/include/sys/_types/_suseconds_t.h
c_supmap.o: /usr/include/sys/_types/_rsize_t.h
c_supmap.o: /usr/include/sys/_types/_errno_t.h
c_supmap.o: /usr/include/sys/_types/_fd_def.h
c_supmap.o: /usr/include/sys/_types/_fd_setsize.h
c_supmap.o: /usr/include/sys/_types/_fd_set.h
c_supmap.o: /usr/include/sys/_types/_fd_clr.h
c_supmap.o: /usr/include/sys/_types/_fd_zero.h
c_supmap.o: /usr/include/sys/_types/_fd_isset.h
c_supmap.o: /usr/include/sys/_types/_fd_copy.h
c_supmap.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_supmap.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_supmap.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_supmap.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_supmap.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_supmap.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_supmap.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_supmap.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_supmap.o: /usr/include/sys/_pthread/_pthread_t.h
c_supmap.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_supmap.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_supmap.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_supmap.o: ../../../.././include/ncarg/c.h
c_mapgci.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mapgci.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mapgci.o: /usr/include/sys/_posix_availability.h
c_mapgci.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mapgci.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mapgci.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mapgci.o: /usr/include/sys/_pthread/_pthread_types.h
c_mapgci.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mapgci.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mapgci.o: /usr/include/sys/_types/_int16_t.h
c_mapgci.o: /usr/include/sys/_types/_int32_t.h
c_mapgci.o: /usr/include/sys/_types/_int64_t.h
c_mapgci.o: /usr/include/sys/_types/_u_int8_t.h
c_mapgci.o: /usr/include/sys/_types/_u_int16_t.h
c_mapgci.o: /usr/include/sys/_types/_u_int32_t.h
c_mapgci.o: /usr/include/sys/_types/_u_int64_t.h
c_mapgci.o: /usr/include/sys/_types/_intptr_t.h
c_mapgci.o: /usr/include/sys/_types/_uintptr_t.h
c_mapgci.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mapgci.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mapgci.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mapgci.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mapgci.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mapgci.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mapgci.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mapgci.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mapgci.o: /usr/include/sys/_types/_u_char.h
c_mapgci.o: /usr/include/sys/_types/_u_short.h
c_mapgci.o: /usr/include/sys/_types/_u_int.h
c_mapgci.o: /usr/include/sys/_types/_caddr_t.h
c_mapgci.o: /usr/include/sys/_types/_dev_t.h
c_mapgci.o: /usr/include/sys/_types/_blkcnt_t.h
c_mapgci.o: /usr/include/sys/_types/_blksize_t.h
c_mapgci.o: /usr/include/sys/_types/_gid_t.h
c_mapgci.o: /usr/include/sys/_types/_in_addr_t.h
c_mapgci.o: /usr/include/sys/_types/_in_port_t.h
c_mapgci.o: /usr/include/sys/_types/_ino_t.h
c_mapgci.o: /usr/include/sys/_types/_ino64_t.h
c_mapgci.o: /usr/include/sys/_types/_key_t.h
c_mapgci.o: /usr/include/sys/_types/_mode_t.h
c_mapgci.o: /usr/include/sys/_types/_nlink_t.h
c_mapgci.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mapgci.o: /usr/include/sys/_types/_uid_t.h
c_mapgci.o: /usr/include/sys/_types/_clock_t.h
c_mapgci.o: /usr/include/sys/_types/_time_t.h
c_mapgci.o: /usr/include/sys/_types/_useconds_t.h
c_mapgci.o: /usr/include/sys/_types/_suseconds_t.h
c_mapgci.o: /usr/include/sys/_types/_rsize_t.h
c_mapgci.o: /usr/include/sys/_types/_errno_t.h
c_mapgci.o: /usr/include/sys/_types/_fd_def.h
c_mapgci.o: /usr/include/sys/_types/_fd_setsize.h
c_mapgci.o: /usr/include/sys/_types/_fd_set.h
c_mapgci.o: /usr/include/sys/_types/_fd_clr.h
c_mapgci.o: /usr/include/sys/_types/_fd_zero.h
c_mapgci.o: /usr/include/sys/_types/_fd_isset.h
c_mapgci.o: /usr/include/sys/_types/_fd_copy.h
c_mapgci.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mapgci.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mapgci.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mapgci.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mapgci.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mapgci.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mapgci.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mapgci.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mapgci.o: /usr/include/sys/_pthread/_pthread_t.h
c_mapgci.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mapgci.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mapgci.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mapgci.o: ../../../.././include/ncarg/c.h
c_maptra.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_maptra.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_maptra.o: /usr/include/sys/_posix_availability.h
c_maptra.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_maptra.o: /usr/include/_types.h /usr/include/sys/_types.h
c_maptra.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_maptra.o: /usr/include/sys/_pthread/_pthread_types.h
c_maptra.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_maptra.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_maptra.o: /usr/include/sys/_types/_int16_t.h
c_maptra.o: /usr/include/sys/_types/_int32_t.h
c_maptra.o: /usr/include/sys/_types/_int64_t.h
c_maptra.o: /usr/include/sys/_types/_u_int8_t.h
c_maptra.o: /usr/include/sys/_types/_u_int16_t.h
c_maptra.o: /usr/include/sys/_types/_u_int32_t.h
c_maptra.o: /usr/include/sys/_types/_u_int64_t.h
c_maptra.o: /usr/include/sys/_types/_intptr_t.h
c_maptra.o: /usr/include/sys/_types/_uintptr_t.h
c_maptra.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_maptra.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_maptra.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_maptra.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_maptra.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_maptra.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_maptra.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_maptra.o: /usr/include/libkern/i386/_OSByteOrder.h
c_maptra.o: /usr/include/sys/_types/_u_char.h
c_maptra.o: /usr/include/sys/_types/_u_short.h
c_maptra.o: /usr/include/sys/_types/_u_int.h
c_maptra.o: /usr/include/sys/_types/_caddr_t.h
c_maptra.o: /usr/include/sys/_types/_dev_t.h
c_maptra.o: /usr/include/sys/_types/_blkcnt_t.h
c_maptra.o: /usr/include/sys/_types/_blksize_t.h
c_maptra.o: /usr/include/sys/_types/_gid_t.h
c_maptra.o: /usr/include/sys/_types/_in_addr_t.h
c_maptra.o: /usr/include/sys/_types/_in_port_t.h
c_maptra.o: /usr/include/sys/_types/_ino_t.h
c_maptra.o: /usr/include/sys/_types/_ino64_t.h
c_maptra.o: /usr/include/sys/_types/_key_t.h
c_maptra.o: /usr/include/sys/_types/_mode_t.h
c_maptra.o: /usr/include/sys/_types/_nlink_t.h
c_maptra.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_maptra.o: /usr/include/sys/_types/_uid_t.h
c_maptra.o: /usr/include/sys/_types/_clock_t.h
c_maptra.o: /usr/include/sys/_types/_time_t.h
c_maptra.o: /usr/include/sys/_types/_useconds_t.h
c_maptra.o: /usr/include/sys/_types/_suseconds_t.h
c_maptra.o: /usr/include/sys/_types/_rsize_t.h
c_maptra.o: /usr/include/sys/_types/_errno_t.h
c_maptra.o: /usr/include/sys/_types/_fd_def.h
c_maptra.o: /usr/include/sys/_types/_fd_setsize.h
c_maptra.o: /usr/include/sys/_types/_fd_set.h
c_maptra.o: /usr/include/sys/_types/_fd_clr.h
c_maptra.o: /usr/include/sys/_types/_fd_zero.h
c_maptra.o: /usr/include/sys/_types/_fd_isset.h
c_maptra.o: /usr/include/sys/_types/_fd_copy.h
c_maptra.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_maptra.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_maptra.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_maptra.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_maptra.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_maptra.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_maptra.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_maptra.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_maptra.o: /usr/include/sys/_pthread/_pthread_t.h
c_maptra.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_maptra.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_maptra.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_maptra.o: ../../../.././include/ncarg/c.h
c_mapiqa.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mapiqa.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mapiqa.o: /usr/include/sys/_posix_availability.h
c_mapiqa.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mapiqa.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mapiqa.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mapiqa.o: /usr/include/sys/_pthread/_pthread_types.h
c_mapiqa.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mapiqa.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mapiqa.o: /usr/include/sys/_types/_int16_t.h
c_mapiqa.o: /usr/include/sys/_types/_int32_t.h
c_mapiqa.o: /usr/include/sys/_types/_int64_t.h
c_mapiqa.o: /usr/include/sys/_types/_u_int8_t.h
c_mapiqa.o: /usr/include/sys/_types/_u_int16_t.h
c_mapiqa.o: /usr/include/sys/_types/_u_int32_t.h
c_mapiqa.o: /usr/include/sys/_types/_u_int64_t.h
c_mapiqa.o: /usr/include/sys/_types/_intptr_t.h
c_mapiqa.o: /usr/include/sys/_types/_uintptr_t.h
c_mapiqa.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mapiqa.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mapiqa.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mapiqa.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mapiqa.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mapiqa.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mapiqa.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mapiqa.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mapiqa.o: /usr/include/sys/_types/_u_char.h
c_mapiqa.o: /usr/include/sys/_types/_u_short.h
c_mapiqa.o: /usr/include/sys/_types/_u_int.h
c_mapiqa.o: /usr/include/sys/_types/_caddr_t.h
c_mapiqa.o: /usr/include/sys/_types/_dev_t.h
c_mapiqa.o: /usr/include/sys/_types/_blkcnt_t.h
c_mapiqa.o: /usr/include/sys/_types/_blksize_t.h
c_mapiqa.o: /usr/include/sys/_types/_gid_t.h
c_mapiqa.o: /usr/include/sys/_types/_in_addr_t.h
c_mapiqa.o: /usr/include/sys/_types/_in_port_t.h
c_mapiqa.o: /usr/include/sys/_types/_ino_t.h
c_mapiqa.o: /usr/include/sys/_types/_ino64_t.h
c_mapiqa.o: /usr/include/sys/_types/_key_t.h
c_mapiqa.o: /usr/include/sys/_types/_mode_t.h
c_mapiqa.o: /usr/include/sys/_types/_nlink_t.h
c_mapiqa.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mapiqa.o: /usr/include/sys/_types/_uid_t.h
c_mapiqa.o: /usr/include/sys/_types/_clock_t.h
c_mapiqa.o: /usr/include/sys/_types/_time_t.h
c_mapiqa.o: /usr/include/sys/_types/_useconds_t.h
c_mapiqa.o: /usr/include/sys/_types/_suseconds_t.h
c_mapiqa.o: /usr/include/sys/_types/_rsize_t.h
c_mapiqa.o: /usr/include/sys/_types/_errno_t.h
c_mapiqa.o: /usr/include/sys/_types/_fd_def.h
c_mapiqa.o: /usr/include/sys/_types/_fd_setsize.h
c_mapiqa.o: /usr/include/sys/_types/_fd_set.h
c_mapiqa.o: /usr/include/sys/_types/_fd_clr.h
c_mapiqa.o: /usr/include/sys/_types/_fd_zero.h
c_mapiqa.o: /usr/include/sys/_types/_fd_isset.h
c_mapiqa.o: /usr/include/sys/_types/_fd_copy.h
c_mapiqa.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mapiqa.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mapiqa.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mapiqa.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mapiqa.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mapiqa.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mapiqa.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mapiqa.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mapiqa.o: /usr/include/sys/_pthread/_pthread_t.h
c_mapiqa.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mapiqa.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mapiqa.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mapiqa.o: ../../../.././include/ncarg/c.h
c_mapiqm.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mapiqm.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mapiqm.o: /usr/include/sys/_posix_availability.h
c_mapiqm.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mapiqm.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mapiqm.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mapiqm.o: /usr/include/sys/_pthread/_pthread_types.h
c_mapiqm.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mapiqm.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mapiqm.o: /usr/include/sys/_types/_int16_t.h
c_mapiqm.o: /usr/include/sys/_types/_int32_t.h
c_mapiqm.o: /usr/include/sys/_types/_int64_t.h
c_mapiqm.o: /usr/include/sys/_types/_u_int8_t.h
c_mapiqm.o: /usr/include/sys/_types/_u_int16_t.h
c_mapiqm.o: /usr/include/sys/_types/_u_int32_t.h
c_mapiqm.o: /usr/include/sys/_types/_u_int64_t.h
c_mapiqm.o: /usr/include/sys/_types/_intptr_t.h
c_mapiqm.o: /usr/include/sys/_types/_uintptr_t.h
c_mapiqm.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mapiqm.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mapiqm.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mapiqm.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mapiqm.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mapiqm.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mapiqm.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mapiqm.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mapiqm.o: /usr/include/sys/_types/_u_char.h
c_mapiqm.o: /usr/include/sys/_types/_u_short.h
c_mapiqm.o: /usr/include/sys/_types/_u_int.h
c_mapiqm.o: /usr/include/sys/_types/_caddr_t.h
c_mapiqm.o: /usr/include/sys/_types/_dev_t.h
c_mapiqm.o: /usr/include/sys/_types/_blkcnt_t.h
c_mapiqm.o: /usr/include/sys/_types/_blksize_t.h
c_mapiqm.o: /usr/include/sys/_types/_gid_t.h
c_mapiqm.o: /usr/include/sys/_types/_in_addr_t.h
c_mapiqm.o: /usr/include/sys/_types/_in_port_t.h
c_mapiqm.o: /usr/include/sys/_types/_ino_t.h
c_mapiqm.o: /usr/include/sys/_types/_ino64_t.h
c_mapiqm.o: /usr/include/sys/_types/_key_t.h
c_mapiqm.o: /usr/include/sys/_types/_mode_t.h
c_mapiqm.o: /usr/include/sys/_types/_nlink_t.h
c_mapiqm.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mapiqm.o: /usr/include/sys/_types/_uid_t.h
c_mapiqm.o: /usr/include/sys/_types/_clock_t.h
c_mapiqm.o: /usr/include/sys/_types/_time_t.h
c_mapiqm.o: /usr/include/sys/_types/_useconds_t.h
c_mapiqm.o: /usr/include/sys/_types/_suseconds_t.h
c_mapiqm.o: /usr/include/sys/_types/_rsize_t.h
c_mapiqm.o: /usr/include/sys/_types/_errno_t.h
c_mapiqm.o: /usr/include/sys/_types/_fd_def.h
c_mapiqm.o: /usr/include/sys/_types/_fd_setsize.h
c_mapiqm.o: /usr/include/sys/_types/_fd_set.h
c_mapiqm.o: /usr/include/sys/_types/_fd_clr.h
c_mapiqm.o: /usr/include/sys/_types/_fd_zero.h
c_mapiqm.o: /usr/include/sys/_types/_fd_isset.h
c_mapiqm.o: /usr/include/sys/_types/_fd_copy.h
c_mapiqm.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mapiqm.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mapiqm.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mapiqm.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mapiqm.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mapiqm.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mapiqm.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mapiqm.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mapiqm.o: /usr/include/sys/_pthread/_pthread_t.h
c_mapiqm.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mapiqm.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mapiqm.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mapiqm.o: ../../../.././include/ncarg/c.h
c_mapita.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mapita.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mapita.o: /usr/include/sys/_posix_availability.h
c_mapita.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mapita.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mapita.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mapita.o: /usr/include/sys/_pthread/_pthread_types.h
c_mapita.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mapita.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mapita.o: /usr/include/sys/_types/_int16_t.h
c_mapita.o: /usr/include/sys/_types/_int32_t.h
c_mapita.o: /usr/include/sys/_types/_int64_t.h
c_mapita.o: /usr/include/sys/_types/_u_int8_t.h
c_mapita.o: /usr/include/sys/_types/_u_int16_t.h
c_mapita.o: /usr/include/sys/_types/_u_int32_t.h
c_mapita.o: /usr/include/sys/_types/_u_int64_t.h
c_mapita.o: /usr/include/sys/_types/_intptr_t.h
c_mapita.o: /usr/include/sys/_types/_uintptr_t.h
c_mapita.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mapita.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mapita.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mapita.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mapita.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mapita.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mapita.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mapita.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mapita.o: /usr/include/sys/_types/_u_char.h
c_mapita.o: /usr/include/sys/_types/_u_short.h
c_mapita.o: /usr/include/sys/_types/_u_int.h
c_mapita.o: /usr/include/sys/_types/_caddr_t.h
c_mapita.o: /usr/include/sys/_types/_dev_t.h
c_mapita.o: /usr/include/sys/_types/_blkcnt_t.h
c_mapita.o: /usr/include/sys/_types/_blksize_t.h
c_mapita.o: /usr/include/sys/_types/_gid_t.h
c_mapita.o: /usr/include/sys/_types/_in_addr_t.h
c_mapita.o: /usr/include/sys/_types/_in_port_t.h
c_mapita.o: /usr/include/sys/_types/_ino_t.h
c_mapita.o: /usr/include/sys/_types/_ino64_t.h
c_mapita.o: /usr/include/sys/_types/_key_t.h
c_mapita.o: /usr/include/sys/_types/_mode_t.h
c_mapita.o: /usr/include/sys/_types/_nlink_t.h
c_mapita.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mapita.o: /usr/include/sys/_types/_uid_t.h
c_mapita.o: /usr/include/sys/_types/_clock_t.h
c_mapita.o: /usr/include/sys/_types/_time_t.h
c_mapita.o: /usr/include/sys/_types/_useconds_t.h
c_mapita.o: /usr/include/sys/_types/_suseconds_t.h
c_mapita.o: /usr/include/sys/_types/_rsize_t.h
c_mapita.o: /usr/include/sys/_types/_errno_t.h
c_mapita.o: /usr/include/sys/_types/_fd_def.h
c_mapita.o: /usr/include/sys/_types/_fd_setsize.h
c_mapita.o: /usr/include/sys/_types/_fd_set.h
c_mapita.o: /usr/include/sys/_types/_fd_clr.h
c_mapita.o: /usr/include/sys/_types/_fd_zero.h
c_mapita.o: /usr/include/sys/_types/_fd_isset.h
c_mapita.o: /usr/include/sys/_types/_fd_copy.h
c_mapita.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mapita.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mapita.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mapita.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mapita.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mapita.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mapita.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mapita.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mapita.o: /usr/include/sys/_pthread/_pthread_t.h
c_mapita.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mapita.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mapita.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mapita.o: ../../../.././include/ncarg/c.h
c_mapitm.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mapitm.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mapitm.o: /usr/include/sys/_posix_availability.h
c_mapitm.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mapitm.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mapitm.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mapitm.o: /usr/include/sys/_pthread/_pthread_types.h
c_mapitm.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mapitm.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mapitm.o: /usr/include/sys/_types/_int16_t.h
c_mapitm.o: /usr/include/sys/_types/_int32_t.h
c_mapitm.o: /usr/include/sys/_types/_int64_t.h
c_mapitm.o: /usr/include/sys/_types/_u_int8_t.h
c_mapitm.o: /usr/include/sys/_types/_u_int16_t.h
c_mapitm.o: /usr/include/sys/_types/_u_int32_t.h
c_mapitm.o: /usr/include/sys/_types/_u_int64_t.h
c_mapitm.o: /usr/include/sys/_types/_intptr_t.h
c_mapitm.o: /usr/include/sys/_types/_uintptr_t.h
c_mapitm.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mapitm.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mapitm.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mapitm.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mapitm.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mapitm.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mapitm.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mapitm.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mapitm.o: /usr/include/sys/_types/_u_char.h
c_mapitm.o: /usr/include/sys/_types/_u_short.h
c_mapitm.o: /usr/include/sys/_types/_u_int.h
c_mapitm.o: /usr/include/sys/_types/_caddr_t.h
c_mapitm.o: /usr/include/sys/_types/_dev_t.h
c_mapitm.o: /usr/include/sys/_types/_blkcnt_t.h
c_mapitm.o: /usr/include/sys/_types/_blksize_t.h
c_mapitm.o: /usr/include/sys/_types/_gid_t.h
c_mapitm.o: /usr/include/sys/_types/_in_addr_t.h
c_mapitm.o: /usr/include/sys/_types/_in_port_t.h
c_mapitm.o: /usr/include/sys/_types/_ino_t.h
c_mapitm.o: /usr/include/sys/_types/_ino64_t.h
c_mapitm.o: /usr/include/sys/_types/_key_t.h
c_mapitm.o: /usr/include/sys/_types/_mode_t.h
c_mapitm.o: /usr/include/sys/_types/_nlink_t.h
c_mapitm.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mapitm.o: /usr/include/sys/_types/_uid_t.h
c_mapitm.o: /usr/include/sys/_types/_clock_t.h
c_mapitm.o: /usr/include/sys/_types/_time_t.h
c_mapitm.o: /usr/include/sys/_types/_useconds_t.h
c_mapitm.o: /usr/include/sys/_types/_suseconds_t.h
c_mapitm.o: /usr/include/sys/_types/_rsize_t.h
c_mapitm.o: /usr/include/sys/_types/_errno_t.h
c_mapitm.o: /usr/include/sys/_types/_fd_def.h
c_mapitm.o: /usr/include/sys/_types/_fd_setsize.h
c_mapitm.o: /usr/include/sys/_types/_fd_set.h
c_mapitm.o: /usr/include/sys/_types/_fd_clr.h
c_mapitm.o: /usr/include/sys/_types/_fd_zero.h
c_mapitm.o: /usr/include/sys/_types/_fd_isset.h
c_mapitm.o: /usr/include/sys/_types/_fd_copy.h
c_mapitm.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mapitm.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mapitm.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mapitm.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mapitm.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mapitm.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mapitm.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mapitm.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mapitm.o: /usr/include/sys/_pthread/_pthread_t.h
c_mapitm.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mapitm.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mapitm.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mapitm.o: ../../../.././include/ncarg/c.h
c_mapaci.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mapaci.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mapaci.o: /usr/include/sys/_posix_availability.h
c_mapaci.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mapaci.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mapaci.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mapaci.o: /usr/include/sys/_pthread/_pthread_types.h
c_mapaci.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mapaci.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mapaci.o: /usr/include/sys/_types/_int16_t.h
c_mapaci.o: /usr/include/sys/_types/_int32_t.h
c_mapaci.o: /usr/include/sys/_types/_int64_t.h
c_mapaci.o: /usr/include/sys/_types/_u_int8_t.h
c_mapaci.o: /usr/include/sys/_types/_u_int16_t.h
c_mapaci.o: /usr/include/sys/_types/_u_int32_t.h
c_mapaci.o: /usr/include/sys/_types/_u_int64_t.h
c_mapaci.o: /usr/include/sys/_types/_intptr_t.h
c_mapaci.o: /usr/include/sys/_types/_uintptr_t.h
c_mapaci.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mapaci.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mapaci.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mapaci.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mapaci.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mapaci.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mapaci.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mapaci.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mapaci.o: /usr/include/sys/_types/_u_char.h
c_mapaci.o: /usr/include/sys/_types/_u_short.h
c_mapaci.o: /usr/include/sys/_types/_u_int.h
c_mapaci.o: /usr/include/sys/_types/_caddr_t.h
c_mapaci.o: /usr/include/sys/_types/_dev_t.h
c_mapaci.o: /usr/include/sys/_types/_blkcnt_t.h
c_mapaci.o: /usr/include/sys/_types/_blksize_t.h
c_mapaci.o: /usr/include/sys/_types/_gid_t.h
c_mapaci.o: /usr/include/sys/_types/_in_addr_t.h
c_mapaci.o: /usr/include/sys/_types/_in_port_t.h
c_mapaci.o: /usr/include/sys/_types/_ino_t.h
c_mapaci.o: /usr/include/sys/_types/_ino64_t.h
c_mapaci.o: /usr/include/sys/_types/_key_t.h
c_mapaci.o: /usr/include/sys/_types/_mode_t.h
c_mapaci.o: /usr/include/sys/_types/_nlink_t.h
c_mapaci.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mapaci.o: /usr/include/sys/_types/_uid_t.h
c_mapaci.o: /usr/include/sys/_types/_clock_t.h
c_mapaci.o: /usr/include/sys/_types/_time_t.h
c_mapaci.o: /usr/include/sys/_types/_useconds_t.h
c_mapaci.o: /usr/include/sys/_types/_suseconds_t.h
c_mapaci.o: /usr/include/sys/_types/_rsize_t.h
c_mapaci.o: /usr/include/sys/_types/_errno_t.h
c_mapaci.o: /usr/include/sys/_types/_fd_def.h
c_mapaci.o: /usr/include/sys/_types/_fd_setsize.h
c_mapaci.o: /usr/include/sys/_types/_fd_set.h
c_mapaci.o: /usr/include/sys/_types/_fd_clr.h
c_mapaci.o: /usr/include/sys/_types/_fd_zero.h
c_mapaci.o: /usr/include/sys/_types/_fd_isset.h
c_mapaci.o: /usr/include/sys/_types/_fd_copy.h
c_mapaci.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mapaci.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mapaci.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mapaci.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mapaci.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mapaci.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mapaci.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mapaci.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mapaci.o: /usr/include/sys/_pthread/_pthread_t.h
c_mapaci.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mapaci.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mapaci.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mapaci.o: ../../../.././include/ncarg/c.h
c_mapbla.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mapbla.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mapbla.o: /usr/include/sys/_posix_availability.h
c_mapbla.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mapbla.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mapbla.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mapbla.o: /usr/include/sys/_pthread/_pthread_types.h
c_mapbla.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mapbla.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mapbla.o: /usr/include/sys/_types/_int16_t.h
c_mapbla.o: /usr/include/sys/_types/_int32_t.h
c_mapbla.o: /usr/include/sys/_types/_int64_t.h
c_mapbla.o: /usr/include/sys/_types/_u_int8_t.h
c_mapbla.o: /usr/include/sys/_types/_u_int16_t.h
c_mapbla.o: /usr/include/sys/_types/_u_int32_t.h
c_mapbla.o: /usr/include/sys/_types/_u_int64_t.h
c_mapbla.o: /usr/include/sys/_types/_intptr_t.h
c_mapbla.o: /usr/include/sys/_types/_uintptr_t.h
c_mapbla.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mapbla.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mapbla.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mapbla.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mapbla.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mapbla.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mapbla.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mapbla.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mapbla.o: /usr/include/sys/_types/_u_char.h
c_mapbla.o: /usr/include/sys/_types/_u_short.h
c_mapbla.o: /usr/include/sys/_types/_u_int.h
c_mapbla.o: /usr/include/sys/_types/_caddr_t.h
c_mapbla.o: /usr/include/sys/_types/_dev_t.h
c_mapbla.o: /usr/include/sys/_types/_blkcnt_t.h
c_mapbla.o: /usr/include/sys/_types/_blksize_t.h
c_mapbla.o: /usr/include/sys/_types/_gid_t.h
c_mapbla.o: /usr/include/sys/_types/_in_addr_t.h
c_mapbla.o: /usr/include/sys/_types/_in_port_t.h
c_mapbla.o: /usr/include/sys/_types/_ino_t.h
c_mapbla.o: /usr/include/sys/_types/_ino64_t.h
c_mapbla.o: /usr/include/sys/_types/_key_t.h
c_mapbla.o: /usr/include/sys/_types/_mode_t.h
c_mapbla.o: /usr/include/sys/_types/_nlink_t.h
c_mapbla.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mapbla.o: /usr/include/sys/_types/_uid_t.h
c_mapbla.o: /usr/include/sys/_types/_clock_t.h
c_mapbla.o: /usr/include/sys/_types/_time_t.h
c_mapbla.o: /usr/include/sys/_types/_useconds_t.h
c_mapbla.o: /usr/include/sys/_types/_suseconds_t.h
c_mapbla.o: /usr/include/sys/_types/_rsize_t.h
c_mapbla.o: /usr/include/sys/_types/_errno_t.h
c_mapbla.o: /usr/include/sys/_types/_fd_def.h
c_mapbla.o: /usr/include/sys/_types/_fd_setsize.h
c_mapbla.o: /usr/include/sys/_types/_fd_set.h
c_mapbla.o: /usr/include/sys/_types/_fd_clr.h
c_mapbla.o: /usr/include/sys/_types/_fd_zero.h
c_mapbla.o: /usr/include/sys/_types/_fd_isset.h
c_mapbla.o: /usr/include/sys/_types/_fd_copy.h
c_mapbla.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mapbla.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mapbla.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mapbla.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mapbla.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mapbla.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mapbla.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mapbla.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mapbla.o: /usr/include/sys/_pthread/_pthread_t.h
c_mapbla.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mapbla.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mapbla.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mapbla.o: ../../../.././include/ncarg/c.h
c_mapgrm.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mapgrm.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mapgrm.o: /usr/include/sys/_posix_availability.h
c_mapgrm.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mapgrm.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mapgrm.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mapgrm.o: /usr/include/sys/_pthread/_pthread_types.h
c_mapgrm.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mapgrm.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mapgrm.o: /usr/include/sys/_types/_int16_t.h
c_mapgrm.o: /usr/include/sys/_types/_int32_t.h
c_mapgrm.o: /usr/include/sys/_types/_int64_t.h
c_mapgrm.o: /usr/include/sys/_types/_u_int8_t.h
c_mapgrm.o: /usr/include/sys/_types/_u_int16_t.h
c_mapgrm.o: /usr/include/sys/_types/_u_int32_t.h
c_mapgrm.o: /usr/include/sys/_types/_u_int64_t.h
c_mapgrm.o: /usr/include/sys/_types/_intptr_t.h
c_mapgrm.o: /usr/include/sys/_types/_uintptr_t.h
c_mapgrm.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mapgrm.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mapgrm.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mapgrm.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mapgrm.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mapgrm.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mapgrm.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mapgrm.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mapgrm.o: /usr/include/sys/_types/_u_char.h
c_mapgrm.o: /usr/include/sys/_types/_u_short.h
c_mapgrm.o: /usr/include/sys/_types/_u_int.h
c_mapgrm.o: /usr/include/sys/_types/_caddr_t.h
c_mapgrm.o: /usr/include/sys/_types/_dev_t.h
c_mapgrm.o: /usr/include/sys/_types/_blkcnt_t.h
c_mapgrm.o: /usr/include/sys/_types/_blksize_t.h
c_mapgrm.o: /usr/include/sys/_types/_gid_t.h
c_mapgrm.o: /usr/include/sys/_types/_in_addr_t.h
c_mapgrm.o: /usr/include/sys/_types/_in_port_t.h
c_mapgrm.o: /usr/include/sys/_types/_ino_t.h
c_mapgrm.o: /usr/include/sys/_types/_ino64_t.h
c_mapgrm.o: /usr/include/sys/_types/_key_t.h
c_mapgrm.o: /usr/include/sys/_types/_mode_t.h
c_mapgrm.o: /usr/include/sys/_types/_nlink_t.h
c_mapgrm.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mapgrm.o: /usr/include/sys/_types/_uid_t.h
c_mapgrm.o: /usr/include/sys/_types/_clock_t.h
c_mapgrm.o: /usr/include/sys/_types/_time_t.h
c_mapgrm.o: /usr/include/sys/_types/_useconds_t.h
c_mapgrm.o: /usr/include/sys/_types/_suseconds_t.h
c_mapgrm.o: /usr/include/sys/_types/_rsize_t.h
c_mapgrm.o: /usr/include/sys/_types/_errno_t.h
c_mapgrm.o: /usr/include/sys/_types/_fd_def.h
c_mapgrm.o: /usr/include/sys/_types/_fd_setsize.h
c_mapgrm.o: /usr/include/sys/_types/_fd_set.h
c_mapgrm.o: /usr/include/sys/_types/_fd_clr.h
c_mapgrm.o: /usr/include/sys/_types/_fd_zero.h
c_mapgrm.o: /usr/include/sys/_types/_fd_isset.h
c_mapgrm.o: /usr/include/sys/_types/_fd_copy.h
c_mapgrm.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mapgrm.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mapgrm.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mapgrm.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mapgrm.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mapgrm.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mapgrm.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mapgrm.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mapgrm.o: /usr/include/sys/_pthread/_pthread_t.h
c_mapgrm.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mapgrm.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mapgrm.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mapgrm.o: ../../../.././include/ncarg/c.h
c_mpgetc.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mpgetc.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mpgetc.o: /usr/include/sys/_posix_availability.h
c_mpgetc.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mpgetc.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mpgetc.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mpgetc.o: /usr/include/sys/_pthread/_pthread_types.h
c_mpgetc.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mpgetc.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mpgetc.o: /usr/include/sys/_types/_int16_t.h
c_mpgetc.o: /usr/include/sys/_types/_int32_t.h
c_mpgetc.o: /usr/include/sys/_types/_int64_t.h
c_mpgetc.o: /usr/include/sys/_types/_u_int8_t.h
c_mpgetc.o: /usr/include/sys/_types/_u_int16_t.h
c_mpgetc.o: /usr/include/sys/_types/_u_int32_t.h
c_mpgetc.o: /usr/include/sys/_types/_u_int64_t.h
c_mpgetc.o: /usr/include/sys/_types/_intptr_t.h
c_mpgetc.o: /usr/include/sys/_types/_uintptr_t.h
c_mpgetc.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mpgetc.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mpgetc.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mpgetc.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mpgetc.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mpgetc.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mpgetc.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mpgetc.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mpgetc.o: /usr/include/sys/_types/_u_char.h
c_mpgetc.o: /usr/include/sys/_types/_u_short.h
c_mpgetc.o: /usr/include/sys/_types/_u_int.h
c_mpgetc.o: /usr/include/sys/_types/_caddr_t.h
c_mpgetc.o: /usr/include/sys/_types/_dev_t.h
c_mpgetc.o: /usr/include/sys/_types/_blkcnt_t.h
c_mpgetc.o: /usr/include/sys/_types/_blksize_t.h
c_mpgetc.o: /usr/include/sys/_types/_gid_t.h
c_mpgetc.o: /usr/include/sys/_types/_in_addr_t.h
c_mpgetc.o: /usr/include/sys/_types/_in_port_t.h
c_mpgetc.o: /usr/include/sys/_types/_ino_t.h
c_mpgetc.o: /usr/include/sys/_types/_ino64_t.h
c_mpgetc.o: /usr/include/sys/_types/_key_t.h
c_mpgetc.o: /usr/include/sys/_types/_mode_t.h
c_mpgetc.o: /usr/include/sys/_types/_nlink_t.h
c_mpgetc.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mpgetc.o: /usr/include/sys/_types/_uid_t.h
c_mpgetc.o: /usr/include/sys/_types/_clock_t.h
c_mpgetc.o: /usr/include/sys/_types/_time_t.h
c_mpgetc.o: /usr/include/sys/_types/_useconds_t.h
c_mpgetc.o: /usr/include/sys/_types/_suseconds_t.h
c_mpgetc.o: /usr/include/sys/_types/_rsize_t.h
c_mpgetc.o: /usr/include/sys/_types/_errno_t.h
c_mpgetc.o: /usr/include/sys/_types/_fd_def.h
c_mpgetc.o: /usr/include/sys/_types/_fd_setsize.h
c_mpgetc.o: /usr/include/sys/_types/_fd_set.h
c_mpgetc.o: /usr/include/sys/_types/_fd_clr.h
c_mpgetc.o: /usr/include/sys/_types/_fd_zero.h
c_mpgetc.o: /usr/include/sys/_types/_fd_isset.h
c_mpgetc.o: /usr/include/sys/_types/_fd_copy.h
c_mpgetc.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mpgetc.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mpgetc.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mpgetc.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mpgetc.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mpgetc.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mpgetc.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mpgetc.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mpgetc.o: /usr/include/sys/_pthread/_pthread_t.h
c_mpgetc.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mpgetc.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mpgetc.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mpgetc.o: ../../../.././include/ncarg/c.h
c_mpgeti.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mpgeti.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mpgeti.o: /usr/include/sys/_posix_availability.h
c_mpgeti.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mpgeti.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mpgeti.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mpgeti.o: /usr/include/sys/_pthread/_pthread_types.h
c_mpgeti.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mpgeti.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mpgeti.o: /usr/include/sys/_types/_int16_t.h
c_mpgeti.o: /usr/include/sys/_types/_int32_t.h
c_mpgeti.o: /usr/include/sys/_types/_int64_t.h
c_mpgeti.o: /usr/include/sys/_types/_u_int8_t.h
c_mpgeti.o: /usr/include/sys/_types/_u_int16_t.h
c_mpgeti.o: /usr/include/sys/_types/_u_int32_t.h
c_mpgeti.o: /usr/include/sys/_types/_u_int64_t.h
c_mpgeti.o: /usr/include/sys/_types/_intptr_t.h
c_mpgeti.o: /usr/include/sys/_types/_uintptr_t.h
c_mpgeti.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mpgeti.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mpgeti.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mpgeti.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mpgeti.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mpgeti.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mpgeti.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mpgeti.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mpgeti.o: /usr/include/sys/_types/_u_char.h
c_mpgeti.o: /usr/include/sys/_types/_u_short.h
c_mpgeti.o: /usr/include/sys/_types/_u_int.h
c_mpgeti.o: /usr/include/sys/_types/_caddr_t.h
c_mpgeti.o: /usr/include/sys/_types/_dev_t.h
c_mpgeti.o: /usr/include/sys/_types/_blkcnt_t.h
c_mpgeti.o: /usr/include/sys/_types/_blksize_t.h
c_mpgeti.o: /usr/include/sys/_types/_gid_t.h
c_mpgeti.o: /usr/include/sys/_types/_in_addr_t.h
c_mpgeti.o: /usr/include/sys/_types/_in_port_t.h
c_mpgeti.o: /usr/include/sys/_types/_ino_t.h
c_mpgeti.o: /usr/include/sys/_types/_ino64_t.h
c_mpgeti.o: /usr/include/sys/_types/_key_t.h
c_mpgeti.o: /usr/include/sys/_types/_mode_t.h
c_mpgeti.o: /usr/include/sys/_types/_nlink_t.h
c_mpgeti.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mpgeti.o: /usr/include/sys/_types/_uid_t.h
c_mpgeti.o: /usr/include/sys/_types/_clock_t.h
c_mpgeti.o: /usr/include/sys/_types/_time_t.h
c_mpgeti.o: /usr/include/sys/_types/_useconds_t.h
c_mpgeti.o: /usr/include/sys/_types/_suseconds_t.h
c_mpgeti.o: /usr/include/sys/_types/_rsize_t.h
c_mpgeti.o: /usr/include/sys/_types/_errno_t.h
c_mpgeti.o: /usr/include/sys/_types/_fd_def.h
c_mpgeti.o: /usr/include/sys/_types/_fd_setsize.h
c_mpgeti.o: /usr/include/sys/_types/_fd_set.h
c_mpgeti.o: /usr/include/sys/_types/_fd_clr.h
c_mpgeti.o: /usr/include/sys/_types/_fd_zero.h
c_mpgeti.o: /usr/include/sys/_types/_fd_isset.h
c_mpgeti.o: /usr/include/sys/_types/_fd_copy.h
c_mpgeti.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mpgeti.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mpgeti.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mpgeti.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mpgeti.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mpgeti.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mpgeti.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mpgeti.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mpgeti.o: /usr/include/sys/_pthread/_pthread_t.h
c_mpgeti.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mpgeti.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mpgeti.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mpgeti.o: ../../../.././include/ncarg/c.h
c_mpgetl.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mpgetl.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mpgetl.o: /usr/include/sys/_posix_availability.h
c_mpgetl.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mpgetl.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mpgetl.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mpgetl.o: /usr/include/sys/_pthread/_pthread_types.h
c_mpgetl.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mpgetl.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mpgetl.o: /usr/include/sys/_types/_int16_t.h
c_mpgetl.o: /usr/include/sys/_types/_int32_t.h
c_mpgetl.o: /usr/include/sys/_types/_int64_t.h
c_mpgetl.o: /usr/include/sys/_types/_u_int8_t.h
c_mpgetl.o: /usr/include/sys/_types/_u_int16_t.h
c_mpgetl.o: /usr/include/sys/_types/_u_int32_t.h
c_mpgetl.o: /usr/include/sys/_types/_u_int64_t.h
c_mpgetl.o: /usr/include/sys/_types/_intptr_t.h
c_mpgetl.o: /usr/include/sys/_types/_uintptr_t.h
c_mpgetl.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mpgetl.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mpgetl.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mpgetl.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mpgetl.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mpgetl.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mpgetl.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mpgetl.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mpgetl.o: /usr/include/sys/_types/_u_char.h
c_mpgetl.o: /usr/include/sys/_types/_u_short.h
c_mpgetl.o: /usr/include/sys/_types/_u_int.h
c_mpgetl.o: /usr/include/sys/_types/_caddr_t.h
c_mpgetl.o: /usr/include/sys/_types/_dev_t.h
c_mpgetl.o: /usr/include/sys/_types/_blkcnt_t.h
c_mpgetl.o: /usr/include/sys/_types/_blksize_t.h
c_mpgetl.o: /usr/include/sys/_types/_gid_t.h
c_mpgetl.o: /usr/include/sys/_types/_in_addr_t.h
c_mpgetl.o: /usr/include/sys/_types/_in_port_t.h
c_mpgetl.o: /usr/include/sys/_types/_ino_t.h
c_mpgetl.o: /usr/include/sys/_types/_ino64_t.h
c_mpgetl.o: /usr/include/sys/_types/_key_t.h
c_mpgetl.o: /usr/include/sys/_types/_mode_t.h
c_mpgetl.o: /usr/include/sys/_types/_nlink_t.h
c_mpgetl.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mpgetl.o: /usr/include/sys/_types/_uid_t.h
c_mpgetl.o: /usr/include/sys/_types/_clock_t.h
c_mpgetl.o: /usr/include/sys/_types/_time_t.h
c_mpgetl.o: /usr/include/sys/_types/_useconds_t.h
c_mpgetl.o: /usr/include/sys/_types/_suseconds_t.h
c_mpgetl.o: /usr/include/sys/_types/_rsize_t.h
c_mpgetl.o: /usr/include/sys/_types/_errno_t.h
c_mpgetl.o: /usr/include/sys/_types/_fd_def.h
c_mpgetl.o: /usr/include/sys/_types/_fd_setsize.h
c_mpgetl.o: /usr/include/sys/_types/_fd_set.h
c_mpgetl.o: /usr/include/sys/_types/_fd_clr.h
c_mpgetl.o: /usr/include/sys/_types/_fd_zero.h
c_mpgetl.o: /usr/include/sys/_types/_fd_isset.h
c_mpgetl.o: /usr/include/sys/_types/_fd_copy.h
c_mpgetl.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mpgetl.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mpgetl.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mpgetl.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mpgetl.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mpgetl.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mpgetl.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mpgetl.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mpgetl.o: /usr/include/sys/_pthread/_pthread_t.h
c_mpgetl.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mpgetl.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mpgetl.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mpgetl.o: ../../../.././include/ncarg/c.h
c_mpgetr.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mpgetr.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mpgetr.o: /usr/include/sys/_posix_availability.h
c_mpgetr.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mpgetr.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mpgetr.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mpgetr.o: /usr/include/sys/_pthread/_pthread_types.h
c_mpgetr.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mpgetr.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mpgetr.o: /usr/include/sys/_types/_int16_t.h
c_mpgetr.o: /usr/include/sys/_types/_int32_t.h
c_mpgetr.o: /usr/include/sys/_types/_int64_t.h
c_mpgetr.o: /usr/include/sys/_types/_u_int8_t.h
c_mpgetr.o: /usr/include/sys/_types/_u_int16_t.h
c_mpgetr.o: /usr/include/sys/_types/_u_int32_t.h
c_mpgetr.o: /usr/include/sys/_types/_u_int64_t.h
c_mpgetr.o: /usr/include/sys/_types/_intptr_t.h
c_mpgetr.o: /usr/include/sys/_types/_uintptr_t.h
c_mpgetr.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mpgetr.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mpgetr.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mpgetr.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mpgetr.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mpgetr.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mpgetr.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mpgetr.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mpgetr.o: /usr/include/sys/_types/_u_char.h
c_mpgetr.o: /usr/include/sys/_types/_u_short.h
c_mpgetr.o: /usr/include/sys/_types/_u_int.h
c_mpgetr.o: /usr/include/sys/_types/_caddr_t.h
c_mpgetr.o: /usr/include/sys/_types/_dev_t.h
c_mpgetr.o: /usr/include/sys/_types/_blkcnt_t.h
c_mpgetr.o: /usr/include/sys/_types/_blksize_t.h
c_mpgetr.o: /usr/include/sys/_types/_gid_t.h
c_mpgetr.o: /usr/include/sys/_types/_in_addr_t.h
c_mpgetr.o: /usr/include/sys/_types/_in_port_t.h
c_mpgetr.o: /usr/include/sys/_types/_ino_t.h
c_mpgetr.o: /usr/include/sys/_types/_ino64_t.h
c_mpgetr.o: /usr/include/sys/_types/_key_t.h
c_mpgetr.o: /usr/include/sys/_types/_mode_t.h
c_mpgetr.o: /usr/include/sys/_types/_nlink_t.h
c_mpgetr.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mpgetr.o: /usr/include/sys/_types/_uid_t.h
c_mpgetr.o: /usr/include/sys/_types/_clock_t.h
c_mpgetr.o: /usr/include/sys/_types/_time_t.h
c_mpgetr.o: /usr/include/sys/_types/_useconds_t.h
c_mpgetr.o: /usr/include/sys/_types/_suseconds_t.h
c_mpgetr.o: /usr/include/sys/_types/_rsize_t.h
c_mpgetr.o: /usr/include/sys/_types/_errno_t.h
c_mpgetr.o: /usr/include/sys/_types/_fd_def.h
c_mpgetr.o: /usr/include/sys/_types/_fd_setsize.h
c_mpgetr.o: /usr/include/sys/_types/_fd_set.h
c_mpgetr.o: /usr/include/sys/_types/_fd_clr.h
c_mpgetr.o: /usr/include/sys/_types/_fd_zero.h
c_mpgetr.o: /usr/include/sys/_types/_fd_isset.h
c_mpgetr.o: /usr/include/sys/_types/_fd_copy.h
c_mpgetr.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mpgetr.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mpgetr.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mpgetr.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mpgetr.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mpgetr.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mpgetr.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mpgetr.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mpgetr.o: /usr/include/sys/_pthread/_pthread_t.h
c_mpgetr.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mpgetr.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mpgetr.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mpgetr.o: ../../../.././include/ncarg/c.h
c_mpsetc.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mpsetc.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mpsetc.o: /usr/include/sys/_posix_availability.h
c_mpsetc.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mpsetc.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mpsetc.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mpsetc.o: /usr/include/sys/_pthread/_pthread_types.h
c_mpsetc.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mpsetc.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mpsetc.o: /usr/include/sys/_types/_int16_t.h
c_mpsetc.o: /usr/include/sys/_types/_int32_t.h
c_mpsetc.o: /usr/include/sys/_types/_int64_t.h
c_mpsetc.o: /usr/include/sys/_types/_u_int8_t.h
c_mpsetc.o: /usr/include/sys/_types/_u_int16_t.h
c_mpsetc.o: /usr/include/sys/_types/_u_int32_t.h
c_mpsetc.o: /usr/include/sys/_types/_u_int64_t.h
c_mpsetc.o: /usr/include/sys/_types/_intptr_t.h
c_mpsetc.o: /usr/include/sys/_types/_uintptr_t.h
c_mpsetc.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mpsetc.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mpsetc.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mpsetc.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mpsetc.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mpsetc.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mpsetc.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mpsetc.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mpsetc.o: /usr/include/sys/_types/_u_char.h
c_mpsetc.o: /usr/include/sys/_types/_u_short.h
c_mpsetc.o: /usr/include/sys/_types/_u_int.h
c_mpsetc.o: /usr/include/sys/_types/_caddr_t.h
c_mpsetc.o: /usr/include/sys/_types/_dev_t.h
c_mpsetc.o: /usr/include/sys/_types/_blkcnt_t.h
c_mpsetc.o: /usr/include/sys/_types/_blksize_t.h
c_mpsetc.o: /usr/include/sys/_types/_gid_t.h
c_mpsetc.o: /usr/include/sys/_types/_in_addr_t.h
c_mpsetc.o: /usr/include/sys/_types/_in_port_t.h
c_mpsetc.o: /usr/include/sys/_types/_ino_t.h
c_mpsetc.o: /usr/include/sys/_types/_ino64_t.h
c_mpsetc.o: /usr/include/sys/_types/_key_t.h
c_mpsetc.o: /usr/include/sys/_types/_mode_t.h
c_mpsetc.o: /usr/include/sys/_types/_nlink_t.h
c_mpsetc.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mpsetc.o: /usr/include/sys/_types/_uid_t.h
c_mpsetc.o: /usr/include/sys/_types/_clock_t.h
c_mpsetc.o: /usr/include/sys/_types/_time_t.h
c_mpsetc.o: /usr/include/sys/_types/_useconds_t.h
c_mpsetc.o: /usr/include/sys/_types/_suseconds_t.h
c_mpsetc.o: /usr/include/sys/_types/_rsize_t.h
c_mpsetc.o: /usr/include/sys/_types/_errno_t.h
c_mpsetc.o: /usr/include/sys/_types/_fd_def.h
c_mpsetc.o: /usr/include/sys/_types/_fd_setsize.h
c_mpsetc.o: /usr/include/sys/_types/_fd_set.h
c_mpsetc.o: /usr/include/sys/_types/_fd_clr.h
c_mpsetc.o: /usr/include/sys/_types/_fd_zero.h
c_mpsetc.o: /usr/include/sys/_types/_fd_isset.h
c_mpsetc.o: /usr/include/sys/_types/_fd_copy.h
c_mpsetc.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mpsetc.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mpsetc.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mpsetc.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mpsetc.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mpsetc.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mpsetc.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mpsetc.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mpsetc.o: /usr/include/sys/_pthread/_pthread_t.h
c_mpsetc.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mpsetc.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mpsetc.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mpsetc.o: ../../../.././include/ncarg/c.h
c_mpseti.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mpseti.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mpseti.o: /usr/include/sys/_posix_availability.h
c_mpseti.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mpseti.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mpseti.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mpseti.o: /usr/include/sys/_pthread/_pthread_types.h
c_mpseti.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mpseti.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mpseti.o: /usr/include/sys/_types/_int16_t.h
c_mpseti.o: /usr/include/sys/_types/_int32_t.h
c_mpseti.o: /usr/include/sys/_types/_int64_t.h
c_mpseti.o: /usr/include/sys/_types/_u_int8_t.h
c_mpseti.o: /usr/include/sys/_types/_u_int16_t.h
c_mpseti.o: /usr/include/sys/_types/_u_int32_t.h
c_mpseti.o: /usr/include/sys/_types/_u_int64_t.h
c_mpseti.o: /usr/include/sys/_types/_intptr_t.h
c_mpseti.o: /usr/include/sys/_types/_uintptr_t.h
c_mpseti.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mpseti.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mpseti.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mpseti.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mpseti.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mpseti.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mpseti.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mpseti.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mpseti.o: /usr/include/sys/_types/_u_char.h
c_mpseti.o: /usr/include/sys/_types/_u_short.h
c_mpseti.o: /usr/include/sys/_types/_u_int.h
c_mpseti.o: /usr/include/sys/_types/_caddr_t.h
c_mpseti.o: /usr/include/sys/_types/_dev_t.h
c_mpseti.o: /usr/include/sys/_types/_blkcnt_t.h
c_mpseti.o: /usr/include/sys/_types/_blksize_t.h
c_mpseti.o: /usr/include/sys/_types/_gid_t.h
c_mpseti.o: /usr/include/sys/_types/_in_addr_t.h
c_mpseti.o: /usr/include/sys/_types/_in_port_t.h
c_mpseti.o: /usr/include/sys/_types/_ino_t.h
c_mpseti.o: /usr/include/sys/_types/_ino64_t.h
c_mpseti.o: /usr/include/sys/_types/_key_t.h
c_mpseti.o: /usr/include/sys/_types/_mode_t.h
c_mpseti.o: /usr/include/sys/_types/_nlink_t.h
c_mpseti.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mpseti.o: /usr/include/sys/_types/_uid_t.h
c_mpseti.o: /usr/include/sys/_types/_clock_t.h
c_mpseti.o: /usr/include/sys/_types/_time_t.h
c_mpseti.o: /usr/include/sys/_types/_useconds_t.h
c_mpseti.o: /usr/include/sys/_types/_suseconds_t.h
c_mpseti.o: /usr/include/sys/_types/_rsize_t.h
c_mpseti.o: /usr/include/sys/_types/_errno_t.h
c_mpseti.o: /usr/include/sys/_types/_fd_def.h
c_mpseti.o: /usr/include/sys/_types/_fd_setsize.h
c_mpseti.o: /usr/include/sys/_types/_fd_set.h
c_mpseti.o: /usr/include/sys/_types/_fd_clr.h
c_mpseti.o: /usr/include/sys/_types/_fd_zero.h
c_mpseti.o: /usr/include/sys/_types/_fd_isset.h
c_mpseti.o: /usr/include/sys/_types/_fd_copy.h
c_mpseti.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mpseti.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mpseti.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mpseti.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mpseti.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mpseti.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mpseti.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mpseti.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mpseti.o: /usr/include/sys/_pthread/_pthread_t.h
c_mpseti.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mpseti.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mpseti.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mpseti.o: ../../../.././include/ncarg/c.h
c_mpsetl.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mpsetl.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mpsetl.o: /usr/include/sys/_posix_availability.h
c_mpsetl.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mpsetl.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mpsetl.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mpsetl.o: /usr/include/sys/_pthread/_pthread_types.h
c_mpsetl.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mpsetl.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mpsetl.o: /usr/include/sys/_types/_int16_t.h
c_mpsetl.o: /usr/include/sys/_types/_int32_t.h
c_mpsetl.o: /usr/include/sys/_types/_int64_t.h
c_mpsetl.o: /usr/include/sys/_types/_u_int8_t.h
c_mpsetl.o: /usr/include/sys/_types/_u_int16_t.h
c_mpsetl.o: /usr/include/sys/_types/_u_int32_t.h
c_mpsetl.o: /usr/include/sys/_types/_u_int64_t.h
c_mpsetl.o: /usr/include/sys/_types/_intptr_t.h
c_mpsetl.o: /usr/include/sys/_types/_uintptr_t.h
c_mpsetl.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mpsetl.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mpsetl.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mpsetl.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mpsetl.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mpsetl.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mpsetl.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mpsetl.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mpsetl.o: /usr/include/sys/_types/_u_char.h
c_mpsetl.o: /usr/include/sys/_types/_u_short.h
c_mpsetl.o: /usr/include/sys/_types/_u_int.h
c_mpsetl.o: /usr/include/sys/_types/_caddr_t.h
c_mpsetl.o: /usr/include/sys/_types/_dev_t.h
c_mpsetl.o: /usr/include/sys/_types/_blkcnt_t.h
c_mpsetl.o: /usr/include/sys/_types/_blksize_t.h
c_mpsetl.o: /usr/include/sys/_types/_gid_t.h
c_mpsetl.o: /usr/include/sys/_types/_in_addr_t.h
c_mpsetl.o: /usr/include/sys/_types/_in_port_t.h
c_mpsetl.o: /usr/include/sys/_types/_ino_t.h
c_mpsetl.o: /usr/include/sys/_types/_ino64_t.h
c_mpsetl.o: /usr/include/sys/_types/_key_t.h
c_mpsetl.o: /usr/include/sys/_types/_mode_t.h
c_mpsetl.o: /usr/include/sys/_types/_nlink_t.h
c_mpsetl.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mpsetl.o: /usr/include/sys/_types/_uid_t.h
c_mpsetl.o: /usr/include/sys/_types/_clock_t.h
c_mpsetl.o: /usr/include/sys/_types/_time_t.h
c_mpsetl.o: /usr/include/sys/_types/_useconds_t.h
c_mpsetl.o: /usr/include/sys/_types/_suseconds_t.h
c_mpsetl.o: /usr/include/sys/_types/_rsize_t.h
c_mpsetl.o: /usr/include/sys/_types/_errno_t.h
c_mpsetl.o: /usr/include/sys/_types/_fd_def.h
c_mpsetl.o: /usr/include/sys/_types/_fd_setsize.h
c_mpsetl.o: /usr/include/sys/_types/_fd_set.h
c_mpsetl.o: /usr/include/sys/_types/_fd_clr.h
c_mpsetl.o: /usr/include/sys/_types/_fd_zero.h
c_mpsetl.o: /usr/include/sys/_types/_fd_isset.h
c_mpsetl.o: /usr/include/sys/_types/_fd_copy.h
c_mpsetl.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mpsetl.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mpsetl.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mpsetl.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mpsetl.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mpsetl.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mpsetl.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mpsetl.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mpsetl.o: /usr/include/sys/_pthread/_pthread_t.h
c_mpsetl.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mpsetl.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mpsetl.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mpsetl.o: ../../../.././include/ncarg/c.h
c_mpsetr.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mpsetr.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mpsetr.o: /usr/include/sys/_posix_availability.h
c_mpsetr.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mpsetr.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mpsetr.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mpsetr.o: /usr/include/sys/_pthread/_pthread_types.h
c_mpsetr.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mpsetr.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mpsetr.o: /usr/include/sys/_types/_int16_t.h
c_mpsetr.o: /usr/include/sys/_types/_int32_t.h
c_mpsetr.o: /usr/include/sys/_types/_int64_t.h
c_mpsetr.o: /usr/include/sys/_types/_u_int8_t.h
c_mpsetr.o: /usr/include/sys/_types/_u_int16_t.h
c_mpsetr.o: /usr/include/sys/_types/_u_int32_t.h
c_mpsetr.o: /usr/include/sys/_types/_u_int64_t.h
c_mpsetr.o: /usr/include/sys/_types/_intptr_t.h
c_mpsetr.o: /usr/include/sys/_types/_uintptr_t.h
c_mpsetr.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mpsetr.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mpsetr.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mpsetr.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mpsetr.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mpsetr.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mpsetr.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mpsetr.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mpsetr.o: /usr/include/sys/_types/_u_char.h
c_mpsetr.o: /usr/include/sys/_types/_u_short.h
c_mpsetr.o: /usr/include/sys/_types/_u_int.h
c_mpsetr.o: /usr/include/sys/_types/_caddr_t.h
c_mpsetr.o: /usr/include/sys/_types/_dev_t.h
c_mpsetr.o: /usr/include/sys/_types/_blkcnt_t.h
c_mpsetr.o: /usr/include/sys/_types/_blksize_t.h
c_mpsetr.o: /usr/include/sys/_types/_gid_t.h
c_mpsetr.o: /usr/include/sys/_types/_in_addr_t.h
c_mpsetr.o: /usr/include/sys/_types/_in_port_t.h
c_mpsetr.o: /usr/include/sys/_types/_ino_t.h
c_mpsetr.o: /usr/include/sys/_types/_ino64_t.h
c_mpsetr.o: /usr/include/sys/_types/_key_t.h
c_mpsetr.o: /usr/include/sys/_types/_mode_t.h
c_mpsetr.o: /usr/include/sys/_types/_nlink_t.h
c_mpsetr.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mpsetr.o: /usr/include/sys/_types/_uid_t.h
c_mpsetr.o: /usr/include/sys/_types/_clock_t.h
c_mpsetr.o: /usr/include/sys/_types/_time_t.h
c_mpsetr.o: /usr/include/sys/_types/_useconds_t.h
c_mpsetr.o: /usr/include/sys/_types/_suseconds_t.h
c_mpsetr.o: /usr/include/sys/_types/_rsize_t.h
c_mpsetr.o: /usr/include/sys/_types/_errno_t.h
c_mpsetr.o: /usr/include/sys/_types/_fd_def.h
c_mpsetr.o: /usr/include/sys/_types/_fd_setsize.h
c_mpsetr.o: /usr/include/sys/_types/_fd_set.h
c_mpsetr.o: /usr/include/sys/_types/_fd_clr.h
c_mpsetr.o: /usr/include/sys/_types/_fd_zero.h
c_mpsetr.o: /usr/include/sys/_types/_fd_isset.h
c_mpsetr.o: /usr/include/sys/_types/_fd_copy.h
c_mpsetr.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mpsetr.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mpsetr.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mpsetr.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mpsetr.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mpsetr.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mpsetr.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mpsetr.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mpsetr.o: /usr/include/sys/_pthread/_pthread_t.h
c_mpsetr.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mpsetr.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mpsetr.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mpsetr.o: ../../../.././include/ncarg/c.h
c_mapiqd.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mapiqd.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mapiqd.o: /usr/include/sys/_posix_availability.h
c_mapiqd.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mapiqd.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mapiqd.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mapiqd.o: /usr/include/sys/_pthread/_pthread_types.h
c_mapiqd.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mapiqd.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mapiqd.o: /usr/include/sys/_types/_int16_t.h
c_mapiqd.o: /usr/include/sys/_types/_int32_t.h
c_mapiqd.o: /usr/include/sys/_types/_int64_t.h
c_mapiqd.o: /usr/include/sys/_types/_u_int8_t.h
c_mapiqd.o: /usr/include/sys/_types/_u_int16_t.h
c_mapiqd.o: /usr/include/sys/_types/_u_int32_t.h
c_mapiqd.o: /usr/include/sys/_types/_u_int64_t.h
c_mapiqd.o: /usr/include/sys/_types/_intptr_t.h
c_mapiqd.o: /usr/include/sys/_types/_uintptr_t.h
c_mapiqd.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mapiqd.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mapiqd.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mapiqd.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mapiqd.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mapiqd.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mapiqd.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mapiqd.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mapiqd.o: /usr/include/sys/_types/_u_char.h
c_mapiqd.o: /usr/include/sys/_types/_u_short.h
c_mapiqd.o: /usr/include/sys/_types/_u_int.h
c_mapiqd.o: /usr/include/sys/_types/_caddr_t.h
c_mapiqd.o: /usr/include/sys/_types/_dev_t.h
c_mapiqd.o: /usr/include/sys/_types/_blkcnt_t.h
c_mapiqd.o: /usr/include/sys/_types/_blksize_t.h
c_mapiqd.o: /usr/include/sys/_types/_gid_t.h
c_mapiqd.o: /usr/include/sys/_types/_in_addr_t.h
c_mapiqd.o: /usr/include/sys/_types/_in_port_t.h
c_mapiqd.o: /usr/include/sys/_types/_ino_t.h
c_mapiqd.o: /usr/include/sys/_types/_ino64_t.h
c_mapiqd.o: /usr/include/sys/_types/_key_t.h
c_mapiqd.o: /usr/include/sys/_types/_mode_t.h
c_mapiqd.o: /usr/include/sys/_types/_nlink_t.h
c_mapiqd.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mapiqd.o: /usr/include/sys/_types/_uid_t.h
c_mapiqd.o: /usr/include/sys/_types/_clock_t.h
c_mapiqd.o: /usr/include/sys/_types/_time_t.h
c_mapiqd.o: /usr/include/sys/_types/_useconds_t.h
c_mapiqd.o: /usr/include/sys/_types/_suseconds_t.h
c_mapiqd.o: /usr/include/sys/_types/_rsize_t.h
c_mapiqd.o: /usr/include/sys/_types/_errno_t.h
c_mapiqd.o: /usr/include/sys/_types/_fd_def.h
c_mapiqd.o: /usr/include/sys/_types/_fd_setsize.h
c_mapiqd.o: /usr/include/sys/_types/_fd_set.h
c_mapiqd.o: /usr/include/sys/_types/_fd_clr.h
c_mapiqd.o: /usr/include/sys/_types/_fd_zero.h
c_mapiqd.o: /usr/include/sys/_types/_fd_isset.h
c_mapiqd.o: /usr/include/sys/_types/_fd_copy.h
c_mapiqd.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mapiqd.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mapiqd.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mapiqd.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mapiqd.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mapiqd.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mapiqd.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mapiqd.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mapiqd.o: /usr/include/sys/_pthread/_pthread_t.h
c_mapiqd.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mapiqd.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mapiqd.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mapiqd.o: ../../../.././include/ncarg/c.h
c_mapitd.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mapitd.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mapitd.o: /usr/include/sys/_posix_availability.h
c_mapitd.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mapitd.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mapitd.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mapitd.o: /usr/include/sys/_pthread/_pthread_types.h
c_mapitd.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mapitd.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mapitd.o: /usr/include/sys/_types/_int16_t.h
c_mapitd.o: /usr/include/sys/_types/_int32_t.h
c_mapitd.o: /usr/include/sys/_types/_int64_t.h
c_mapitd.o: /usr/include/sys/_types/_u_int8_t.h
c_mapitd.o: /usr/include/sys/_types/_u_int16_t.h
c_mapitd.o: /usr/include/sys/_types/_u_int32_t.h
c_mapitd.o: /usr/include/sys/_types/_u_int64_t.h
c_mapitd.o: /usr/include/sys/_types/_intptr_t.h
c_mapitd.o: /usr/include/sys/_types/_uintptr_t.h
c_mapitd.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mapitd.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mapitd.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mapitd.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mapitd.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mapitd.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mapitd.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mapitd.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mapitd.o: /usr/include/sys/_types/_u_char.h
c_mapitd.o: /usr/include/sys/_types/_u_short.h
c_mapitd.o: /usr/include/sys/_types/_u_int.h
c_mapitd.o: /usr/include/sys/_types/_caddr_t.h
c_mapitd.o: /usr/include/sys/_types/_dev_t.h
c_mapitd.o: /usr/include/sys/_types/_blkcnt_t.h
c_mapitd.o: /usr/include/sys/_types/_blksize_t.h
c_mapitd.o: /usr/include/sys/_types/_gid_t.h
c_mapitd.o: /usr/include/sys/_types/_in_addr_t.h
c_mapitd.o: /usr/include/sys/_types/_in_port_t.h
c_mapitd.o: /usr/include/sys/_types/_ino_t.h
c_mapitd.o: /usr/include/sys/_types/_ino64_t.h
c_mapitd.o: /usr/include/sys/_types/_key_t.h
c_mapitd.o: /usr/include/sys/_types/_mode_t.h
c_mapitd.o: /usr/include/sys/_types/_nlink_t.h
c_mapitd.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mapitd.o: /usr/include/sys/_types/_uid_t.h
c_mapitd.o: /usr/include/sys/_types/_clock_t.h
c_mapitd.o: /usr/include/sys/_types/_time_t.h
c_mapitd.o: /usr/include/sys/_types/_useconds_t.h
c_mapitd.o: /usr/include/sys/_types/_suseconds_t.h
c_mapitd.o: /usr/include/sys/_types/_rsize_t.h
c_mapitd.o: /usr/include/sys/_types/_errno_t.h
c_mapitd.o: /usr/include/sys/_types/_fd_def.h
c_mapitd.o: /usr/include/sys/_types/_fd_setsize.h
c_mapitd.o: /usr/include/sys/_types/_fd_set.h
c_mapitd.o: /usr/include/sys/_types/_fd_clr.h
c_mapitd.o: /usr/include/sys/_types/_fd_zero.h
c_mapitd.o: /usr/include/sys/_types/_fd_isset.h
c_mapitd.o: /usr/include/sys/_types/_fd_copy.h
c_mapitd.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mapitd.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mapitd.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mapitd.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mapitd.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mapitd.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mapitd.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mapitd.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mapitd.o: /usr/include/sys/_pthread/_pthread_t.h
c_mapitd.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mapitd.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mapitd.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mapitd.o: ../../../.././include/ncarg/c.h
c_mprset.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mprset.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mprset.o: /usr/include/sys/_posix_availability.h
c_mprset.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mprset.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mprset.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mprset.o: /usr/include/sys/_pthread/_pthread_types.h
c_mprset.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mprset.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mprset.o: /usr/include/sys/_types/_int16_t.h
c_mprset.o: /usr/include/sys/_types/_int32_t.h
c_mprset.o: /usr/include/sys/_types/_int64_t.h
c_mprset.o: /usr/include/sys/_types/_u_int8_t.h
c_mprset.o: /usr/include/sys/_types/_u_int16_t.h
c_mprset.o: /usr/include/sys/_types/_u_int32_t.h
c_mprset.o: /usr/include/sys/_types/_u_int64_t.h
c_mprset.o: /usr/include/sys/_types/_intptr_t.h
c_mprset.o: /usr/include/sys/_types/_uintptr_t.h
c_mprset.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mprset.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mprset.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mprset.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mprset.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mprset.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mprset.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mprset.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mprset.o: /usr/include/sys/_types/_u_char.h
c_mprset.o: /usr/include/sys/_types/_u_short.h
c_mprset.o: /usr/include/sys/_types/_u_int.h
c_mprset.o: /usr/include/sys/_types/_caddr_t.h
c_mprset.o: /usr/include/sys/_types/_dev_t.h
c_mprset.o: /usr/include/sys/_types/_blkcnt_t.h
c_mprset.o: /usr/include/sys/_types/_blksize_t.h
c_mprset.o: /usr/include/sys/_types/_gid_t.h
c_mprset.o: /usr/include/sys/_types/_in_addr_t.h
c_mprset.o: /usr/include/sys/_types/_in_port_t.h
c_mprset.o: /usr/include/sys/_types/_ino_t.h
c_mprset.o: /usr/include/sys/_types/_ino64_t.h
c_mprset.o: /usr/include/sys/_types/_key_t.h
c_mprset.o: /usr/include/sys/_types/_mode_t.h
c_mprset.o: /usr/include/sys/_types/_nlink_t.h
c_mprset.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mprset.o: /usr/include/sys/_types/_uid_t.h
c_mprset.o: /usr/include/sys/_types/_clock_t.h
c_mprset.o: /usr/include/sys/_types/_time_t.h
c_mprset.o: /usr/include/sys/_types/_useconds_t.h
c_mprset.o: /usr/include/sys/_types/_suseconds_t.h
c_mprset.o: /usr/include/sys/_types/_rsize_t.h
c_mprset.o: /usr/include/sys/_types/_errno_t.h
c_mprset.o: /usr/include/sys/_types/_fd_def.h
c_mprset.o: /usr/include/sys/_types/_fd_setsize.h
c_mprset.o: /usr/include/sys/_types/_fd_set.h
c_mprset.o: /usr/include/sys/_types/_fd_clr.h
c_mprset.o: /usr/include/sys/_types/_fd_zero.h
c_mprset.o: /usr/include/sys/_types/_fd_isset.h
c_mprset.o: /usr/include/sys/_types/_fd_copy.h
c_mprset.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mprset.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mprset.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mprset.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mprset.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mprset.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mprset.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mprset.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mprset.o: /usr/include/sys/_pthread/_pthread_t.h
c_mprset.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mprset.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mprset.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mprset.o: ../../../.././include/ncarg/c.h
c_mpfnme.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mpfnme.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mpfnme.o: /usr/include/sys/_posix_availability.h
c_mpfnme.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mpfnme.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mpfnme.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mpfnme.o: /usr/include/sys/_pthread/_pthread_types.h
c_mpfnme.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mpfnme.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mpfnme.o: /usr/include/sys/_types/_int16_t.h
c_mpfnme.o: /usr/include/sys/_types/_int32_t.h
c_mpfnme.o: /usr/include/sys/_types/_int64_t.h
c_mpfnme.o: /usr/include/sys/_types/_u_int8_t.h
c_mpfnme.o: /usr/include/sys/_types/_u_int16_t.h
c_mpfnme.o: /usr/include/sys/_types/_u_int32_t.h
c_mpfnme.o: /usr/include/sys/_types/_u_int64_t.h
c_mpfnme.o: /usr/include/sys/_types/_intptr_t.h
c_mpfnme.o: /usr/include/sys/_types/_uintptr_t.h
c_mpfnme.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mpfnme.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mpfnme.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mpfnme.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mpfnme.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mpfnme.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mpfnme.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mpfnme.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mpfnme.o: /usr/include/sys/_types/_u_char.h
c_mpfnme.o: /usr/include/sys/_types/_u_short.h
c_mpfnme.o: /usr/include/sys/_types/_u_int.h
c_mpfnme.o: /usr/include/sys/_types/_caddr_t.h
c_mpfnme.o: /usr/include/sys/_types/_dev_t.h
c_mpfnme.o: /usr/include/sys/_types/_blkcnt_t.h
c_mpfnme.o: /usr/include/sys/_types/_blksize_t.h
c_mpfnme.o: /usr/include/sys/_types/_gid_t.h
c_mpfnme.o: /usr/include/sys/_types/_in_addr_t.h
c_mpfnme.o: /usr/include/sys/_types/_in_port_t.h
c_mpfnme.o: /usr/include/sys/_types/_ino_t.h
c_mpfnme.o: /usr/include/sys/_types/_ino64_t.h
c_mpfnme.o: /usr/include/sys/_types/_key_t.h
c_mpfnme.o: /usr/include/sys/_types/_mode_t.h
c_mpfnme.o: /usr/include/sys/_types/_nlink_t.h
c_mpfnme.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mpfnme.o: /usr/include/sys/_types/_uid_t.h
c_mpfnme.o: /usr/include/sys/_types/_clock_t.h
c_mpfnme.o: /usr/include/sys/_types/_time_t.h
c_mpfnme.o: /usr/include/sys/_types/_useconds_t.h
c_mpfnme.o: /usr/include/sys/_types/_suseconds_t.h
c_mpfnme.o: /usr/include/sys/_types/_rsize_t.h
c_mpfnme.o: /usr/include/sys/_types/_errno_t.h
c_mpfnme.o: /usr/include/sys/_types/_fd_def.h
c_mpfnme.o: /usr/include/sys/_types/_fd_setsize.h
c_mpfnme.o: /usr/include/sys/_types/_fd_set.h
c_mpfnme.o: /usr/include/sys/_types/_fd_clr.h
c_mpfnme.o: /usr/include/sys/_types/_fd_zero.h
c_mpfnme.o: /usr/include/sys/_types/_fd_isset.h
c_mpfnme.o: /usr/include/sys/_types/_fd_copy.h
c_mpfnme.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mpfnme.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mpfnme.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mpfnme.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mpfnme.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mpfnme.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mpfnme.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mpfnme.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mpfnme.o: /usr/include/sys/_pthread/_pthread_t.h
c_mpfnme.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mpfnme.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mpfnme.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mpfnme.o: ../../../.././include/ncarg/c.h
c_mpglty.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mpglty.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mpglty.o: /usr/include/sys/_posix_availability.h
c_mpglty.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mpglty.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mpglty.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mpglty.o: /usr/include/sys/_pthread/_pthread_types.h
c_mpglty.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mpglty.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mpglty.o: /usr/include/sys/_types/_int16_t.h
c_mpglty.o: /usr/include/sys/_types/_int32_t.h
c_mpglty.o: /usr/include/sys/_types/_int64_t.h
c_mpglty.o: /usr/include/sys/_types/_u_int8_t.h
c_mpglty.o: /usr/include/sys/_types/_u_int16_t.h
c_mpglty.o: /usr/include/sys/_types/_u_int32_t.h
c_mpglty.o: /usr/include/sys/_types/_u_int64_t.h
c_mpglty.o: /usr/include/sys/_types/_intptr_t.h
c_mpglty.o: /usr/include/sys/_types/_uintptr_t.h
c_mpglty.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mpglty.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mpglty.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mpglty.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mpglty.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mpglty.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mpglty.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mpglty.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mpglty.o: /usr/include/sys/_types/_u_char.h
c_mpglty.o: /usr/include/sys/_types/_u_short.h
c_mpglty.o: /usr/include/sys/_types/_u_int.h
c_mpglty.o: /usr/include/sys/_types/_caddr_t.h
c_mpglty.o: /usr/include/sys/_types/_dev_t.h
c_mpglty.o: /usr/include/sys/_types/_blkcnt_t.h
c_mpglty.o: /usr/include/sys/_types/_blksize_t.h
c_mpglty.o: /usr/include/sys/_types/_gid_t.h
c_mpglty.o: /usr/include/sys/_types/_in_addr_t.h
c_mpglty.o: /usr/include/sys/_types/_in_port_t.h
c_mpglty.o: /usr/include/sys/_types/_ino_t.h
c_mpglty.o: /usr/include/sys/_types/_ino64_t.h
c_mpglty.o: /usr/include/sys/_types/_key_t.h
c_mpglty.o: /usr/include/sys/_types/_mode_t.h
c_mpglty.o: /usr/include/sys/_types/_nlink_t.h
c_mpglty.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mpglty.o: /usr/include/sys/_types/_uid_t.h
c_mpglty.o: /usr/include/sys/_types/_clock_t.h
c_mpglty.o: /usr/include/sys/_types/_time_t.h
c_mpglty.o: /usr/include/sys/_types/_useconds_t.h
c_mpglty.o: /usr/include/sys/_types/_suseconds_t.h
c_mpglty.o: /usr/include/sys/_types/_rsize_t.h
c_mpglty.o: /usr/include/sys/_types/_errno_t.h
c_mpglty.o: /usr/include/sys/_types/_fd_def.h
c_mpglty.o: /usr/include/sys/_types/_fd_setsize.h
c_mpglty.o: /usr/include/sys/_types/_fd_set.h
c_mpglty.o: /usr/include/sys/_types/_fd_clr.h
c_mpglty.o: /usr/include/sys/_types/_fd_zero.h
c_mpglty.o: /usr/include/sys/_types/_fd_isset.h
c_mpglty.o: /usr/include/sys/_types/_fd_copy.h
c_mpglty.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mpglty.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mpglty.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mpglty.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mpglty.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mpglty.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mpglty.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mpglty.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mpglty.o: /usr/include/sys/_pthread/_pthread_t.h
c_mpglty.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mpglty.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mpglty.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mpglty.o: ../../../.././include/ncarg/c.h
c_mpiaty.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mpiaty.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mpiaty.o: /usr/include/sys/_posix_availability.h
c_mpiaty.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mpiaty.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mpiaty.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mpiaty.o: /usr/include/sys/_pthread/_pthread_types.h
c_mpiaty.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mpiaty.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mpiaty.o: /usr/include/sys/_types/_int16_t.h
c_mpiaty.o: /usr/include/sys/_types/_int32_t.h
c_mpiaty.o: /usr/include/sys/_types/_int64_t.h
c_mpiaty.o: /usr/include/sys/_types/_u_int8_t.h
c_mpiaty.o: /usr/include/sys/_types/_u_int16_t.h
c_mpiaty.o: /usr/include/sys/_types/_u_int32_t.h
c_mpiaty.o: /usr/include/sys/_types/_u_int64_t.h
c_mpiaty.o: /usr/include/sys/_types/_intptr_t.h
c_mpiaty.o: /usr/include/sys/_types/_uintptr_t.h
c_mpiaty.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mpiaty.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mpiaty.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mpiaty.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mpiaty.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mpiaty.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mpiaty.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mpiaty.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mpiaty.o: /usr/include/sys/_types/_u_char.h
c_mpiaty.o: /usr/include/sys/_types/_u_short.h
c_mpiaty.o: /usr/include/sys/_types/_u_int.h
c_mpiaty.o: /usr/include/sys/_types/_caddr_t.h
c_mpiaty.o: /usr/include/sys/_types/_dev_t.h
c_mpiaty.o: /usr/include/sys/_types/_blkcnt_t.h
c_mpiaty.o: /usr/include/sys/_types/_blksize_t.h
c_mpiaty.o: /usr/include/sys/_types/_gid_t.h
c_mpiaty.o: /usr/include/sys/_types/_in_addr_t.h
c_mpiaty.o: /usr/include/sys/_types/_in_port_t.h
c_mpiaty.o: /usr/include/sys/_types/_ino_t.h
c_mpiaty.o: /usr/include/sys/_types/_ino64_t.h
c_mpiaty.o: /usr/include/sys/_types/_key_t.h
c_mpiaty.o: /usr/include/sys/_types/_mode_t.h
c_mpiaty.o: /usr/include/sys/_types/_nlink_t.h
c_mpiaty.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mpiaty.o: /usr/include/sys/_types/_uid_t.h
c_mpiaty.o: /usr/include/sys/_types/_clock_t.h
c_mpiaty.o: /usr/include/sys/_types/_time_t.h
c_mpiaty.o: /usr/include/sys/_types/_useconds_t.h
c_mpiaty.o: /usr/include/sys/_types/_suseconds_t.h
c_mpiaty.o: /usr/include/sys/_types/_rsize_t.h
c_mpiaty.o: /usr/include/sys/_types/_errno_t.h
c_mpiaty.o: /usr/include/sys/_types/_fd_def.h
c_mpiaty.o: /usr/include/sys/_types/_fd_setsize.h
c_mpiaty.o: /usr/include/sys/_types/_fd_set.h
c_mpiaty.o: /usr/include/sys/_types/_fd_clr.h
c_mpiaty.o: /usr/include/sys/_types/_fd_zero.h
c_mpiaty.o: /usr/include/sys/_types/_fd_isset.h
c_mpiaty.o: /usr/include/sys/_types/_fd_copy.h
c_mpiaty.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mpiaty.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mpiaty.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mpiaty.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mpiaty.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mpiaty.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mpiaty.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mpiaty.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mpiaty.o: /usr/include/sys/_pthread/_pthread_t.h
c_mpiaty.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mpiaty.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mpiaty.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mpiaty.o: ../../../.././include/ncarg/c.h
c_mpifnb.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mpifnb.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mpifnb.o: /usr/include/sys/_posix_availability.h
c_mpifnb.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mpifnb.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mpifnb.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mpifnb.o: /usr/include/sys/_pthread/_pthread_types.h
c_mpifnb.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mpifnb.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mpifnb.o: /usr/include/sys/_types/_int16_t.h
c_mpifnb.o: /usr/include/sys/_types/_int32_t.h
c_mpifnb.o: /usr/include/sys/_types/_int64_t.h
c_mpifnb.o: /usr/include/sys/_types/_u_int8_t.h
c_mpifnb.o: /usr/include/sys/_types/_u_int16_t.h
c_mpifnb.o: /usr/include/sys/_types/_u_int32_t.h
c_mpifnb.o: /usr/include/sys/_types/_u_int64_t.h
c_mpifnb.o: /usr/include/sys/_types/_intptr_t.h
c_mpifnb.o: /usr/include/sys/_types/_uintptr_t.h
c_mpifnb.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mpifnb.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mpifnb.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mpifnb.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mpifnb.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mpifnb.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mpifnb.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mpifnb.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mpifnb.o: /usr/include/sys/_types/_u_char.h
c_mpifnb.o: /usr/include/sys/_types/_u_short.h
c_mpifnb.o: /usr/include/sys/_types/_u_int.h
c_mpifnb.o: /usr/include/sys/_types/_caddr_t.h
c_mpifnb.o: /usr/include/sys/_types/_dev_t.h
c_mpifnb.o: /usr/include/sys/_types/_blkcnt_t.h
c_mpifnb.o: /usr/include/sys/_types/_blksize_t.h
c_mpifnb.o: /usr/include/sys/_types/_gid_t.h
c_mpifnb.o: /usr/include/sys/_types/_in_addr_t.h
c_mpifnb.o: /usr/include/sys/_types/_in_port_t.h
c_mpifnb.o: /usr/include/sys/_types/_ino_t.h
c_mpifnb.o: /usr/include/sys/_types/_ino64_t.h
c_mpifnb.o: /usr/include/sys/_types/_key_t.h
c_mpifnb.o: /usr/include/sys/_types/_mode_t.h
c_mpifnb.o: /usr/include/sys/_types/_nlink_t.h
c_mpifnb.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mpifnb.o: /usr/include/sys/_types/_uid_t.h
c_mpifnb.o: /usr/include/sys/_types/_clock_t.h
c_mpifnb.o: /usr/include/sys/_types/_time_t.h
c_mpifnb.o: /usr/include/sys/_types/_useconds_t.h
c_mpifnb.o: /usr/include/sys/_types/_suseconds_t.h
c_mpifnb.o: /usr/include/sys/_types/_rsize_t.h
c_mpifnb.o: /usr/include/sys/_types/_errno_t.h
c_mpifnb.o: /usr/include/sys/_types/_fd_def.h
c_mpifnb.o: /usr/include/sys/_types/_fd_setsize.h
c_mpifnb.o: /usr/include/sys/_types/_fd_set.h
c_mpifnb.o: /usr/include/sys/_types/_fd_clr.h
c_mpifnb.o: /usr/include/sys/_types/_fd_zero.h
c_mpifnb.o: /usr/include/sys/_types/_fd_isset.h
c_mpifnb.o: /usr/include/sys/_types/_fd_copy.h
c_mpifnb.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mpifnb.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mpifnb.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mpifnb.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mpifnb.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mpifnb.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mpifnb.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mpifnb.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mpifnb.o: /usr/include/sys/_pthread/_pthread_t.h
c_mpifnb.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mpifnb.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mpifnb.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mpifnb.o: ../../../.././include/ncarg/c.h
c_mpilnb.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mpilnb.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mpilnb.o: /usr/include/sys/_posix_availability.h
c_mpilnb.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mpilnb.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mpilnb.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mpilnb.o: /usr/include/sys/_pthread/_pthread_types.h
c_mpilnb.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mpilnb.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mpilnb.o: /usr/include/sys/_types/_int16_t.h
c_mpilnb.o: /usr/include/sys/_types/_int32_t.h
c_mpilnb.o: /usr/include/sys/_types/_int64_t.h
c_mpilnb.o: /usr/include/sys/_types/_u_int8_t.h
c_mpilnb.o: /usr/include/sys/_types/_u_int16_t.h
c_mpilnb.o: /usr/include/sys/_types/_u_int32_t.h
c_mpilnb.o: /usr/include/sys/_types/_u_int64_t.h
c_mpilnb.o: /usr/include/sys/_types/_intptr_t.h
c_mpilnb.o: /usr/include/sys/_types/_uintptr_t.h
c_mpilnb.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mpilnb.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mpilnb.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mpilnb.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mpilnb.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mpilnb.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mpilnb.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mpilnb.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mpilnb.o: /usr/include/sys/_types/_u_char.h
c_mpilnb.o: /usr/include/sys/_types/_u_short.h
c_mpilnb.o: /usr/include/sys/_types/_u_int.h
c_mpilnb.o: /usr/include/sys/_types/_caddr_t.h
c_mpilnb.o: /usr/include/sys/_types/_dev_t.h
c_mpilnb.o: /usr/include/sys/_types/_blkcnt_t.h
c_mpilnb.o: /usr/include/sys/_types/_blksize_t.h
c_mpilnb.o: /usr/include/sys/_types/_gid_t.h
c_mpilnb.o: /usr/include/sys/_types/_in_addr_t.h
c_mpilnb.o: /usr/include/sys/_types/_in_port_t.h
c_mpilnb.o: /usr/include/sys/_types/_ino_t.h
c_mpilnb.o: /usr/include/sys/_types/_ino64_t.h
c_mpilnb.o: /usr/include/sys/_types/_key_t.h
c_mpilnb.o: /usr/include/sys/_types/_mode_t.h
c_mpilnb.o: /usr/include/sys/_types/_nlink_t.h
c_mpilnb.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mpilnb.o: /usr/include/sys/_types/_uid_t.h
c_mpilnb.o: /usr/include/sys/_types/_clock_t.h
c_mpilnb.o: /usr/include/sys/_types/_time_t.h
c_mpilnb.o: /usr/include/sys/_types/_useconds_t.h
c_mpilnb.o: /usr/include/sys/_types/_suseconds_t.h
c_mpilnb.o: /usr/include/sys/_types/_rsize_t.h
c_mpilnb.o: /usr/include/sys/_types/_errno_t.h
c_mpilnb.o: /usr/include/sys/_types/_fd_def.h
c_mpilnb.o: /usr/include/sys/_types/_fd_setsize.h
c_mpilnb.o: /usr/include/sys/_types/_fd_set.h
c_mpilnb.o: /usr/include/sys/_types/_fd_clr.h
c_mpilnb.o: /usr/include/sys/_types/_fd_zero.h
c_mpilnb.o: /usr/include/sys/_types/_fd_isset.h
c_mpilnb.o: /usr/include/sys/_types/_fd_copy.h
c_mpilnb.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mpilnb.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mpilnb.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mpilnb.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mpilnb.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mpilnb.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mpilnb.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mpilnb.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mpilnb.o: /usr/include/sys/_pthread/_pthread_t.h
c_mpilnb.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mpilnb.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mpilnb.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mpilnb.o: ../../../.././include/ncarg/c.h
c_mpiola.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mpiola.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mpiola.o: /usr/include/sys/_posix_availability.h
c_mpiola.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mpiola.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mpiola.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mpiola.o: /usr/include/sys/_pthread/_pthread_types.h
c_mpiola.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mpiola.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mpiola.o: /usr/include/sys/_types/_int16_t.h
c_mpiola.o: /usr/include/sys/_types/_int32_t.h
c_mpiola.o: /usr/include/sys/_types/_int64_t.h
c_mpiola.o: /usr/include/sys/_types/_u_int8_t.h
c_mpiola.o: /usr/include/sys/_types/_u_int16_t.h
c_mpiola.o: /usr/include/sys/_types/_u_int32_t.h
c_mpiola.o: /usr/include/sys/_types/_u_int64_t.h
c_mpiola.o: /usr/include/sys/_types/_intptr_t.h
c_mpiola.o: /usr/include/sys/_types/_uintptr_t.h
c_mpiola.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mpiola.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mpiola.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mpiola.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mpiola.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mpiola.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mpiola.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mpiola.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mpiola.o: /usr/include/sys/_types/_u_char.h
c_mpiola.o: /usr/include/sys/_types/_u_short.h
c_mpiola.o: /usr/include/sys/_types/_u_int.h
c_mpiola.o: /usr/include/sys/_types/_caddr_t.h
c_mpiola.o: /usr/include/sys/_types/_dev_t.h
c_mpiola.o: /usr/include/sys/_types/_blkcnt_t.h
c_mpiola.o: /usr/include/sys/_types/_blksize_t.h
c_mpiola.o: /usr/include/sys/_types/_gid_t.h
c_mpiola.o: /usr/include/sys/_types/_in_addr_t.h
c_mpiola.o: /usr/include/sys/_types/_in_port_t.h
c_mpiola.o: /usr/include/sys/_types/_ino_t.h
c_mpiola.o: /usr/include/sys/_types/_ino64_t.h
c_mpiola.o: /usr/include/sys/_types/_key_t.h
c_mpiola.o: /usr/include/sys/_types/_mode_t.h
c_mpiola.o: /usr/include/sys/_types/_nlink_t.h
c_mpiola.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mpiola.o: /usr/include/sys/_types/_uid_t.h
c_mpiola.o: /usr/include/sys/_types/_clock_t.h
c_mpiola.o: /usr/include/sys/_types/_time_t.h
c_mpiola.o: /usr/include/sys/_types/_useconds_t.h
c_mpiola.o: /usr/include/sys/_types/_suseconds_t.h
c_mpiola.o: /usr/include/sys/_types/_rsize_t.h
c_mpiola.o: /usr/include/sys/_types/_errno_t.h
c_mpiola.o: /usr/include/sys/_types/_fd_def.h
c_mpiola.o: /usr/include/sys/_types/_fd_setsize.h
c_mpiola.o: /usr/include/sys/_types/_fd_set.h
c_mpiola.o: /usr/include/sys/_types/_fd_clr.h
c_mpiola.o: /usr/include/sys/_types/_fd_zero.h
c_mpiola.o: /usr/include/sys/_types/_fd_isset.h
c_mpiola.o: /usr/include/sys/_types/_fd_copy.h
c_mpiola.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mpiola.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mpiola.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mpiola.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mpiola.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mpiola.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mpiola.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mpiola.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mpiola.o: /usr/include/sys/_pthread/_pthread_t.h
c_mpiola.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mpiola.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mpiola.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mpiola.o: ../../../.././include/ncarg/c.h
c_mpiosa.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mpiosa.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mpiosa.o: /usr/include/sys/_posix_availability.h
c_mpiosa.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mpiosa.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mpiosa.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mpiosa.o: /usr/include/sys/_pthread/_pthread_types.h
c_mpiosa.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mpiosa.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mpiosa.o: /usr/include/sys/_types/_int16_t.h
c_mpiosa.o: /usr/include/sys/_types/_int32_t.h
c_mpiosa.o: /usr/include/sys/_types/_int64_t.h
c_mpiosa.o: /usr/include/sys/_types/_u_int8_t.h
c_mpiosa.o: /usr/include/sys/_types/_u_int16_t.h
c_mpiosa.o: /usr/include/sys/_types/_u_int32_t.h
c_mpiosa.o: /usr/include/sys/_types/_u_int64_t.h
c_mpiosa.o: /usr/include/sys/_types/_intptr_t.h
c_mpiosa.o: /usr/include/sys/_types/_uintptr_t.h
c_mpiosa.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mpiosa.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mpiosa.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mpiosa.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mpiosa.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mpiosa.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mpiosa.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mpiosa.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mpiosa.o: /usr/include/sys/_types/_u_char.h
c_mpiosa.o: /usr/include/sys/_types/_u_short.h
c_mpiosa.o: /usr/include/sys/_types/_u_int.h
c_mpiosa.o: /usr/include/sys/_types/_caddr_t.h
c_mpiosa.o: /usr/include/sys/_types/_dev_t.h
c_mpiosa.o: /usr/include/sys/_types/_blkcnt_t.h
c_mpiosa.o: /usr/include/sys/_types/_blksize_t.h
c_mpiosa.o: /usr/include/sys/_types/_gid_t.h
c_mpiosa.o: /usr/include/sys/_types/_in_addr_t.h
c_mpiosa.o: /usr/include/sys/_types/_in_port_t.h
c_mpiosa.o: /usr/include/sys/_types/_ino_t.h
c_mpiosa.o: /usr/include/sys/_types/_ino64_t.h
c_mpiosa.o: /usr/include/sys/_types/_key_t.h
c_mpiosa.o: /usr/include/sys/_types/_mode_t.h
c_mpiosa.o: /usr/include/sys/_types/_nlink_t.h
c_mpiosa.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mpiosa.o: /usr/include/sys/_types/_uid_t.h
c_mpiosa.o: /usr/include/sys/_types/_clock_t.h
c_mpiosa.o: /usr/include/sys/_types/_time_t.h
c_mpiosa.o: /usr/include/sys/_types/_useconds_t.h
c_mpiosa.o: /usr/include/sys/_types/_suseconds_t.h
c_mpiosa.o: /usr/include/sys/_types/_rsize_t.h
c_mpiosa.o: /usr/include/sys/_types/_errno_t.h
c_mpiosa.o: /usr/include/sys/_types/_fd_def.h
c_mpiosa.o: /usr/include/sys/_types/_fd_setsize.h
c_mpiosa.o: /usr/include/sys/_types/_fd_set.h
c_mpiosa.o: /usr/include/sys/_types/_fd_clr.h
c_mpiosa.o: /usr/include/sys/_types/_fd_zero.h
c_mpiosa.o: /usr/include/sys/_types/_fd_isset.h
c_mpiosa.o: /usr/include/sys/_types/_fd_copy.h
c_mpiosa.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mpiosa.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mpiosa.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mpiosa.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mpiosa.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mpiosa.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mpiosa.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mpiosa.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mpiosa.o: /usr/include/sys/_pthread/_pthread_t.h
c_mpiosa.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mpiosa.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mpiosa.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mpiosa.o: ../../../.././include/ncarg/c.h
c_mpipai.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mpipai.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mpipai.o: /usr/include/sys/_posix_availability.h
c_mpipai.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mpipai.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mpipai.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mpipai.o: /usr/include/sys/_pthread/_pthread_types.h
c_mpipai.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mpipai.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mpipai.o: /usr/include/sys/_types/_int16_t.h
c_mpipai.o: /usr/include/sys/_types/_int32_t.h
c_mpipai.o: /usr/include/sys/_types/_int64_t.h
c_mpipai.o: /usr/include/sys/_types/_u_int8_t.h
c_mpipai.o: /usr/include/sys/_types/_u_int16_t.h
c_mpipai.o: /usr/include/sys/_types/_u_int32_t.h
c_mpipai.o: /usr/include/sys/_types/_u_int64_t.h
c_mpipai.o: /usr/include/sys/_types/_intptr_t.h
c_mpipai.o: /usr/include/sys/_types/_uintptr_t.h
c_mpipai.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mpipai.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mpipai.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mpipai.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mpipai.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mpipai.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mpipai.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mpipai.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mpipai.o: /usr/include/sys/_types/_u_char.h
c_mpipai.o: /usr/include/sys/_types/_u_short.h
c_mpipai.o: /usr/include/sys/_types/_u_int.h
c_mpipai.o: /usr/include/sys/_types/_caddr_t.h
c_mpipai.o: /usr/include/sys/_types/_dev_t.h
c_mpipai.o: /usr/include/sys/_types/_blkcnt_t.h
c_mpipai.o: /usr/include/sys/_types/_blksize_t.h
c_mpipai.o: /usr/include/sys/_types/_gid_t.h
c_mpipai.o: /usr/include/sys/_types/_in_addr_t.h
c_mpipai.o: /usr/include/sys/_types/_in_port_t.h
c_mpipai.o: /usr/include/sys/_types/_ino_t.h
c_mpipai.o: /usr/include/sys/_types/_ino64_t.h
c_mpipai.o: /usr/include/sys/_types/_key_t.h
c_mpipai.o: /usr/include/sys/_types/_mode_t.h
c_mpipai.o: /usr/include/sys/_types/_nlink_t.h
c_mpipai.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mpipai.o: /usr/include/sys/_types/_uid_t.h
c_mpipai.o: /usr/include/sys/_types/_clock_t.h
c_mpipai.o: /usr/include/sys/_types/_time_t.h
c_mpipai.o: /usr/include/sys/_types/_useconds_t.h
c_mpipai.o: /usr/include/sys/_types/_suseconds_t.h
c_mpipai.o: /usr/include/sys/_types/_rsize_t.h
c_mpipai.o: /usr/include/sys/_types/_errno_t.h
c_mpipai.o: /usr/include/sys/_types/_fd_def.h
c_mpipai.o: /usr/include/sys/_types/_fd_setsize.h
c_mpipai.o: /usr/include/sys/_types/_fd_set.h
c_mpipai.o: /usr/include/sys/_types/_fd_clr.h
c_mpipai.o: /usr/include/sys/_types/_fd_zero.h
c_mpipai.o: /usr/include/sys/_types/_fd_isset.h
c_mpipai.o: /usr/include/sys/_types/_fd_copy.h
c_mpipai.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mpipai.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mpipai.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mpipai.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mpipai.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mpipai.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mpipai.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mpipai.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mpipai.o: /usr/include/sys/_pthread/_pthread_t.h
c_mpipai.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mpipai.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mpipai.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mpipai.o: ../../../.././include/ncarg/c.h
c_mpipan.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mpipan.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mpipan.o: /usr/include/sys/_posix_availability.h
c_mpipan.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mpipan.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mpipan.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mpipan.o: /usr/include/sys/_pthread/_pthread_types.h
c_mpipan.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mpipan.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mpipan.o: /usr/include/sys/_types/_int16_t.h
c_mpipan.o: /usr/include/sys/_types/_int32_t.h
c_mpipan.o: /usr/include/sys/_types/_int64_t.h
c_mpipan.o: /usr/include/sys/_types/_u_int8_t.h
c_mpipan.o: /usr/include/sys/_types/_u_int16_t.h
c_mpipan.o: /usr/include/sys/_types/_u_int32_t.h
c_mpipan.o: /usr/include/sys/_types/_u_int64_t.h
c_mpipan.o: /usr/include/sys/_types/_intptr_t.h
c_mpipan.o: /usr/include/sys/_types/_uintptr_t.h
c_mpipan.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mpipan.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mpipan.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mpipan.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mpipan.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mpipan.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mpipan.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mpipan.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mpipan.o: /usr/include/sys/_types/_u_char.h
c_mpipan.o: /usr/include/sys/_types/_u_short.h
c_mpipan.o: /usr/include/sys/_types/_u_int.h
c_mpipan.o: /usr/include/sys/_types/_caddr_t.h
c_mpipan.o: /usr/include/sys/_types/_dev_t.h
c_mpipan.o: /usr/include/sys/_types/_blkcnt_t.h
c_mpipan.o: /usr/include/sys/_types/_blksize_t.h
c_mpipan.o: /usr/include/sys/_types/_gid_t.h
c_mpipan.o: /usr/include/sys/_types/_in_addr_t.h
c_mpipan.o: /usr/include/sys/_types/_in_port_t.h
c_mpipan.o: /usr/include/sys/_types/_ino_t.h
c_mpipan.o: /usr/include/sys/_types/_ino64_t.h
c_mpipan.o: /usr/include/sys/_types/_key_t.h
c_mpipan.o: /usr/include/sys/_types/_mode_t.h
c_mpipan.o: /usr/include/sys/_types/_nlink_t.h
c_mpipan.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mpipan.o: /usr/include/sys/_types/_uid_t.h
c_mpipan.o: /usr/include/sys/_types/_clock_t.h
c_mpipan.o: /usr/include/sys/_types/_time_t.h
c_mpipan.o: /usr/include/sys/_types/_useconds_t.h
c_mpipan.o: /usr/include/sys/_types/_suseconds_t.h
c_mpipan.o: /usr/include/sys/_types/_rsize_t.h
c_mpipan.o: /usr/include/sys/_types/_errno_t.h
c_mpipan.o: /usr/include/sys/_types/_fd_def.h
c_mpipan.o: /usr/include/sys/_types/_fd_setsize.h
c_mpipan.o: /usr/include/sys/_types/_fd_set.h
c_mpipan.o: /usr/include/sys/_types/_fd_clr.h
c_mpipan.o: /usr/include/sys/_types/_fd_zero.h
c_mpipan.o: /usr/include/sys/_types/_fd_isset.h
c_mpipan.o: /usr/include/sys/_types/_fd_copy.h
c_mpipan.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mpipan.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mpipan.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mpipan.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mpipan.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mpipan.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mpipan.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mpipan.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mpipan.o: /usr/include/sys/_pthread/_pthread_t.h
c_mpipan.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mpipan.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mpipan.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mpipan.o: ../../../.././include/ncarg/c.h
c_mpipar.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mpipar.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mpipar.o: /usr/include/sys/_posix_availability.h
c_mpipar.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mpipar.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mpipar.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mpipar.o: /usr/include/sys/_pthread/_pthread_types.h
c_mpipar.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mpipar.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mpipar.o: /usr/include/sys/_types/_int16_t.h
c_mpipar.o: /usr/include/sys/_types/_int32_t.h
c_mpipar.o: /usr/include/sys/_types/_int64_t.h
c_mpipar.o: /usr/include/sys/_types/_u_int8_t.h
c_mpipar.o: /usr/include/sys/_types/_u_int16_t.h
c_mpipar.o: /usr/include/sys/_types/_u_int32_t.h
c_mpipar.o: /usr/include/sys/_types/_u_int64_t.h
c_mpipar.o: /usr/include/sys/_types/_intptr_t.h
c_mpipar.o: /usr/include/sys/_types/_uintptr_t.h
c_mpipar.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mpipar.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mpipar.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mpipar.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mpipar.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mpipar.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mpipar.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mpipar.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mpipar.o: /usr/include/sys/_types/_u_char.h
c_mpipar.o: /usr/include/sys/_types/_u_short.h
c_mpipar.o: /usr/include/sys/_types/_u_int.h
c_mpipar.o: /usr/include/sys/_types/_caddr_t.h
c_mpipar.o: /usr/include/sys/_types/_dev_t.h
c_mpipar.o: /usr/include/sys/_types/_blkcnt_t.h
c_mpipar.o: /usr/include/sys/_types/_blksize_t.h
c_mpipar.o: /usr/include/sys/_types/_gid_t.h
c_mpipar.o: /usr/include/sys/_types/_in_addr_t.h
c_mpipar.o: /usr/include/sys/_types/_in_port_t.h
c_mpipar.o: /usr/include/sys/_types/_ino_t.h
c_mpipar.o: /usr/include/sys/_types/_ino64_t.h
c_mpipar.o: /usr/include/sys/_types/_key_t.h
c_mpipar.o: /usr/include/sys/_types/_mode_t.h
c_mpipar.o: /usr/include/sys/_types/_nlink_t.h
c_mpipar.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mpipar.o: /usr/include/sys/_types/_uid_t.h
c_mpipar.o: /usr/include/sys/_types/_clock_t.h
c_mpipar.o: /usr/include/sys/_types/_time_t.h
c_mpipar.o: /usr/include/sys/_types/_useconds_t.h
c_mpipar.o: /usr/include/sys/_types/_suseconds_t.h
c_mpipar.o: /usr/include/sys/_types/_rsize_t.h
c_mpipar.o: /usr/include/sys/_types/_errno_t.h
c_mpipar.o: /usr/include/sys/_types/_fd_def.h
c_mpipar.o: /usr/include/sys/_types/_fd_setsize.h
c_mpipar.o: /usr/include/sys/_types/_fd_set.h
c_mpipar.o: /usr/include/sys/_types/_fd_clr.h
c_mpipar.o: /usr/include/sys/_types/_fd_zero.h
c_mpipar.o: /usr/include/sys/_types/_fd_isset.h
c_mpipar.o: /usr/include/sys/_types/_fd_copy.h
c_mpipar.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mpipar.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mpipar.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mpipar.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mpipar.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mpipar.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mpipar.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mpipar.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mpipar.o: /usr/include/sys/_pthread/_pthread_t.h
c_mpipar.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mpipar.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mpipar.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mpipar.o: ../../../.././include/ncarg/c.h
c_mpisci.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mpisci.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mpisci.o: /usr/include/sys/_posix_availability.h
c_mpisci.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mpisci.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mpisci.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mpisci.o: /usr/include/sys/_pthread/_pthread_types.h
c_mpisci.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mpisci.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mpisci.o: /usr/include/sys/_types/_int16_t.h
c_mpisci.o: /usr/include/sys/_types/_int32_t.h
c_mpisci.o: /usr/include/sys/_types/_int64_t.h
c_mpisci.o: /usr/include/sys/_types/_u_int8_t.h
c_mpisci.o: /usr/include/sys/_types/_u_int16_t.h
c_mpisci.o: /usr/include/sys/_types/_u_int32_t.h
c_mpisci.o: /usr/include/sys/_types/_u_int64_t.h
c_mpisci.o: /usr/include/sys/_types/_intptr_t.h
c_mpisci.o: /usr/include/sys/_types/_uintptr_t.h
c_mpisci.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mpisci.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mpisci.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mpisci.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mpisci.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mpisci.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mpisci.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mpisci.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mpisci.o: /usr/include/sys/_types/_u_char.h
c_mpisci.o: /usr/include/sys/_types/_u_short.h
c_mpisci.o: /usr/include/sys/_types/_u_int.h
c_mpisci.o: /usr/include/sys/_types/_caddr_t.h
c_mpisci.o: /usr/include/sys/_types/_dev_t.h
c_mpisci.o: /usr/include/sys/_types/_blkcnt_t.h
c_mpisci.o: /usr/include/sys/_types/_blksize_t.h
c_mpisci.o: /usr/include/sys/_types/_gid_t.h
c_mpisci.o: /usr/include/sys/_types/_in_addr_t.h
c_mpisci.o: /usr/include/sys/_types/_in_port_t.h
c_mpisci.o: /usr/include/sys/_types/_ino_t.h
c_mpisci.o: /usr/include/sys/_types/_ino64_t.h
c_mpisci.o: /usr/include/sys/_types/_key_t.h
c_mpisci.o: /usr/include/sys/_types/_mode_t.h
c_mpisci.o: /usr/include/sys/_types/_nlink_t.h
c_mpisci.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mpisci.o: /usr/include/sys/_types/_uid_t.h
c_mpisci.o: /usr/include/sys/_types/_clock_t.h
c_mpisci.o: /usr/include/sys/_types/_time_t.h
c_mpisci.o: /usr/include/sys/_types/_useconds_t.h
c_mpisci.o: /usr/include/sys/_types/_suseconds_t.h
c_mpisci.o: /usr/include/sys/_types/_rsize_t.h
c_mpisci.o: /usr/include/sys/_types/_errno_t.h
c_mpisci.o: /usr/include/sys/_types/_fd_def.h
c_mpisci.o: /usr/include/sys/_types/_fd_setsize.h
c_mpisci.o: /usr/include/sys/_types/_fd_set.h
c_mpisci.o: /usr/include/sys/_types/_fd_clr.h
c_mpisci.o: /usr/include/sys/_types/_fd_zero.h
c_mpisci.o: /usr/include/sys/_types/_fd_isset.h
c_mpisci.o: /usr/include/sys/_types/_fd_copy.h
c_mpisci.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mpisci.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mpisci.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mpisci.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mpisci.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mpisci.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mpisci.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mpisci.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mpisci.o: /usr/include/sys/_pthread/_pthread_t.h
c_mpisci.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mpisci.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mpisci.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mpisci.o: ../../../.././include/ncarg/c.h
c_mplnam.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mplnam.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mplnam.o: /usr/include/sys/_posix_availability.h
c_mplnam.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mplnam.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mplnam.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mplnam.o: /usr/include/sys/_pthread/_pthread_types.h
c_mplnam.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mplnam.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mplnam.o: /usr/include/sys/_types/_int16_t.h
c_mplnam.o: /usr/include/sys/_types/_int32_t.h
c_mplnam.o: /usr/include/sys/_types/_int64_t.h
c_mplnam.o: /usr/include/sys/_types/_u_int8_t.h
c_mplnam.o: /usr/include/sys/_types/_u_int16_t.h
c_mplnam.o: /usr/include/sys/_types/_u_int32_t.h
c_mplnam.o: /usr/include/sys/_types/_u_int64_t.h
c_mplnam.o: /usr/include/sys/_types/_intptr_t.h
c_mplnam.o: /usr/include/sys/_types/_uintptr_t.h
c_mplnam.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mplnam.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mplnam.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mplnam.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mplnam.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mplnam.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mplnam.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mplnam.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mplnam.o: /usr/include/sys/_types/_u_char.h
c_mplnam.o: /usr/include/sys/_types/_u_short.h
c_mplnam.o: /usr/include/sys/_types/_u_int.h
c_mplnam.o: /usr/include/sys/_types/_caddr_t.h
c_mplnam.o: /usr/include/sys/_types/_dev_t.h
c_mplnam.o: /usr/include/sys/_types/_blkcnt_t.h
c_mplnam.o: /usr/include/sys/_types/_blksize_t.h
c_mplnam.o: /usr/include/sys/_types/_gid_t.h
c_mplnam.o: /usr/include/sys/_types/_in_addr_t.h
c_mplnam.o: /usr/include/sys/_types/_in_port_t.h
c_mplnam.o: /usr/include/sys/_types/_ino_t.h
c_mplnam.o: /usr/include/sys/_types/_ino64_t.h
c_mplnam.o: /usr/include/sys/_types/_key_t.h
c_mplnam.o: /usr/include/sys/_types/_mode_t.h
c_mplnam.o: /usr/include/sys/_types/_nlink_t.h
c_mplnam.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mplnam.o: /usr/include/sys/_types/_uid_t.h
c_mplnam.o: /usr/include/sys/_types/_clock_t.h
c_mplnam.o: /usr/include/sys/_types/_time_t.h
c_mplnam.o: /usr/include/sys/_types/_useconds_t.h
c_mplnam.o: /usr/include/sys/_types/_suseconds_t.h
c_mplnam.o: /usr/include/sys/_types/_rsize_t.h
c_mplnam.o: /usr/include/sys/_types/_errno_t.h
c_mplnam.o: /usr/include/sys/_types/_fd_def.h
c_mplnam.o: /usr/include/sys/_types/_fd_setsize.h
c_mplnam.o: /usr/include/sys/_types/_fd_set.h
c_mplnam.o: /usr/include/sys/_types/_fd_clr.h
c_mplnam.o: /usr/include/sys/_types/_fd_zero.h
c_mplnam.o: /usr/include/sys/_types/_fd_isset.h
c_mplnam.o: /usr/include/sys/_types/_fd_copy.h
c_mplnam.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mplnam.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mplnam.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mplnam.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mplnam.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mplnam.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mplnam.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mplnam.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mplnam.o: /usr/include/sys/_pthread/_pthread_t.h
c_mplnam.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mplnam.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mplnam.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mplnam.o: ../../../.././include/ncarg/c.h
c_mplndm.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mplndm.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mplndm.o: /usr/include/sys/_posix_availability.h
c_mplndm.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mplndm.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mplndm.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mplndm.o: /usr/include/sys/_pthread/_pthread_types.h
c_mplndm.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mplndm.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mplndm.o: /usr/include/sys/_types/_int16_t.h
c_mplndm.o: /usr/include/sys/_types/_int32_t.h
c_mplndm.o: /usr/include/sys/_types/_int64_t.h
c_mplndm.o: /usr/include/sys/_types/_u_int8_t.h
c_mplndm.o: /usr/include/sys/_types/_u_int16_t.h
c_mplndm.o: /usr/include/sys/_types/_u_int32_t.h
c_mplndm.o: /usr/include/sys/_types/_u_int64_t.h
c_mplndm.o: /usr/include/sys/_types/_intptr_t.h
c_mplndm.o: /usr/include/sys/_types/_uintptr_t.h
c_mplndm.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mplndm.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mplndm.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mplndm.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mplndm.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mplndm.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mplndm.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mplndm.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mplndm.o: /usr/include/sys/_types/_u_char.h
c_mplndm.o: /usr/include/sys/_types/_u_short.h
c_mplndm.o: /usr/include/sys/_types/_u_int.h
c_mplndm.o: /usr/include/sys/_types/_caddr_t.h
c_mplndm.o: /usr/include/sys/_types/_dev_t.h
c_mplndm.o: /usr/include/sys/_types/_blkcnt_t.h
c_mplndm.o: /usr/include/sys/_types/_blksize_t.h
c_mplndm.o: /usr/include/sys/_types/_gid_t.h
c_mplndm.o: /usr/include/sys/_types/_in_addr_t.h
c_mplndm.o: /usr/include/sys/_types/_in_port_t.h
c_mplndm.o: /usr/include/sys/_types/_ino_t.h
c_mplndm.o: /usr/include/sys/_types/_ino64_t.h
c_mplndm.o: /usr/include/sys/_types/_key_t.h
c_mplndm.o: /usr/include/sys/_types/_mode_t.h
c_mplndm.o: /usr/include/sys/_types/_nlink_t.h
c_mplndm.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mplndm.o: /usr/include/sys/_types/_uid_t.h
c_mplndm.o: /usr/include/sys/_types/_clock_t.h
c_mplndm.o: /usr/include/sys/_types/_time_t.h
c_mplndm.o: /usr/include/sys/_types/_useconds_t.h
c_mplndm.o: /usr/include/sys/_types/_suseconds_t.h
c_mplndm.o: /usr/include/sys/_types/_rsize_t.h
c_mplndm.o: /usr/include/sys/_types/_errno_t.h
c_mplndm.o: /usr/include/sys/_types/_fd_def.h
c_mplndm.o: /usr/include/sys/_types/_fd_setsize.h
c_mplndm.o: /usr/include/sys/_types/_fd_set.h
c_mplndm.o: /usr/include/sys/_types/_fd_clr.h
c_mplndm.o: /usr/include/sys/_types/_fd_zero.h
c_mplndm.o: /usr/include/sys/_types/_fd_isset.h
c_mplndm.o: /usr/include/sys/_types/_fd_copy.h
c_mplndm.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mplndm.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mplndm.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mplndm.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mplndm.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mplndm.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mplndm.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mplndm.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mplndm.o: /usr/include/sys/_pthread/_pthread_t.h
c_mplndm.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mplndm.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mplndm.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mplndm.o: ../../../.././include/ncarg/c.h
c_mplndr.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mplndr.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mplndr.o: /usr/include/sys/_posix_availability.h
c_mplndr.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mplndr.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mplndr.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mplndr.o: /usr/include/sys/_pthread/_pthread_types.h
c_mplndr.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mplndr.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mplndr.o: /usr/include/sys/_types/_int16_t.h
c_mplndr.o: /usr/include/sys/_types/_int32_t.h
c_mplndr.o: /usr/include/sys/_types/_int64_t.h
c_mplndr.o: /usr/include/sys/_types/_u_int8_t.h
c_mplndr.o: /usr/include/sys/_types/_u_int16_t.h
c_mplndr.o: /usr/include/sys/_types/_u_int32_t.h
c_mplndr.o: /usr/include/sys/_types/_u_int64_t.h
c_mplndr.o: /usr/include/sys/_types/_intptr_t.h
c_mplndr.o: /usr/include/sys/_types/_uintptr_t.h
c_mplndr.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mplndr.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mplndr.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mplndr.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mplndr.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mplndr.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mplndr.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mplndr.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mplndr.o: /usr/include/sys/_types/_u_char.h
c_mplndr.o: /usr/include/sys/_types/_u_short.h
c_mplndr.o: /usr/include/sys/_types/_u_int.h
c_mplndr.o: /usr/include/sys/_types/_caddr_t.h
c_mplndr.o: /usr/include/sys/_types/_dev_t.h
c_mplndr.o: /usr/include/sys/_types/_blkcnt_t.h
c_mplndr.o: /usr/include/sys/_types/_blksize_t.h
c_mplndr.o: /usr/include/sys/_types/_gid_t.h
c_mplndr.o: /usr/include/sys/_types/_in_addr_t.h
c_mplndr.o: /usr/include/sys/_types/_in_port_t.h
c_mplndr.o: /usr/include/sys/_types/_ino_t.h
c_mplndr.o: /usr/include/sys/_types/_ino64_t.h
c_mplndr.o: /usr/include/sys/_types/_key_t.h
c_mplndr.o: /usr/include/sys/_types/_mode_t.h
c_mplndr.o: /usr/include/sys/_types/_nlink_t.h
c_mplndr.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mplndr.o: /usr/include/sys/_types/_uid_t.h
c_mplndr.o: /usr/include/sys/_types/_clock_t.h
c_mplndr.o: /usr/include/sys/_types/_time_t.h
c_mplndr.o: /usr/include/sys/_types/_useconds_t.h
c_mplndr.o: /usr/include/sys/_types/_suseconds_t.h
c_mplndr.o: /usr/include/sys/_types/_rsize_t.h
c_mplndr.o: /usr/include/sys/_types/_errno_t.h
c_mplndr.o: /usr/include/sys/_types/_fd_def.h
c_mplndr.o: /usr/include/sys/_types/_fd_setsize.h
c_mplndr.o: /usr/include/sys/_types/_fd_set.h
c_mplndr.o: /usr/include/sys/_types/_fd_clr.h
c_mplndr.o: /usr/include/sys/_types/_fd_zero.h
c_mplndr.o: /usr/include/sys/_types/_fd_isset.h
c_mplndr.o: /usr/include/sys/_types/_fd_copy.h
c_mplndr.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mplndr.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mplndr.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mplndr.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mplndr.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mplndr.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mplndr.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mplndr.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mplndr.o: /usr/include/sys/_pthread/_pthread_t.h
c_mplndr.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mplndr.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mplndr.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mplndr.o: ../../../.././include/ncarg/c.h
c_mplnri.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mplnri.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mplnri.o: /usr/include/sys/_posix_availability.h
c_mplnri.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mplnri.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mplnri.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mplnri.o: /usr/include/sys/_pthread/_pthread_types.h
c_mplnri.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mplnri.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mplnri.o: /usr/include/sys/_types/_int16_t.h
c_mplnri.o: /usr/include/sys/_types/_int32_t.h
c_mplnri.o: /usr/include/sys/_types/_int64_t.h
c_mplnri.o: /usr/include/sys/_types/_u_int8_t.h
c_mplnri.o: /usr/include/sys/_types/_u_int16_t.h
c_mplnri.o: /usr/include/sys/_types/_u_int32_t.h
c_mplnri.o: /usr/include/sys/_types/_u_int64_t.h
c_mplnri.o: /usr/include/sys/_types/_intptr_t.h
c_mplnri.o: /usr/include/sys/_types/_uintptr_t.h
c_mplnri.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mplnri.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mplnri.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mplnri.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mplnri.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mplnri.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mplnri.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mplnri.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mplnri.o: /usr/include/sys/_types/_u_char.h
c_mplnri.o: /usr/include/sys/_types/_u_short.h
c_mplnri.o: /usr/include/sys/_types/_u_int.h
c_mplnri.o: /usr/include/sys/_types/_caddr_t.h
c_mplnri.o: /usr/include/sys/_types/_dev_t.h
c_mplnri.o: /usr/include/sys/_types/_blkcnt_t.h
c_mplnri.o: /usr/include/sys/_types/_blksize_t.h
c_mplnri.o: /usr/include/sys/_types/_gid_t.h
c_mplnri.o: /usr/include/sys/_types/_in_addr_t.h
c_mplnri.o: /usr/include/sys/_types/_in_port_t.h
c_mplnri.o: /usr/include/sys/_types/_ino_t.h
c_mplnri.o: /usr/include/sys/_types/_ino64_t.h
c_mplnri.o: /usr/include/sys/_types/_key_t.h
c_mplnri.o: /usr/include/sys/_types/_mode_t.h
c_mplnri.o: /usr/include/sys/_types/_nlink_t.h
c_mplnri.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mplnri.o: /usr/include/sys/_types/_uid_t.h
c_mplnri.o: /usr/include/sys/_types/_clock_t.h
c_mplnri.o: /usr/include/sys/_types/_time_t.h
c_mplnri.o: /usr/include/sys/_types/_useconds_t.h
c_mplnri.o: /usr/include/sys/_types/_suseconds_t.h
c_mplnri.o: /usr/include/sys/_types/_rsize_t.h
c_mplnri.o: /usr/include/sys/_types/_errno_t.h
c_mplnri.o: /usr/include/sys/_types/_fd_def.h
c_mplnri.o: /usr/include/sys/_types/_fd_setsize.h
c_mplnri.o: /usr/include/sys/_types/_fd_set.h
c_mplnri.o: /usr/include/sys/_types/_fd_clr.h
c_mplnri.o: /usr/include/sys/_types/_fd_zero.h
c_mplnri.o: /usr/include/sys/_types/_fd_isset.h
c_mplnri.o: /usr/include/sys/_types/_fd_copy.h
c_mplnri.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mplnri.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mplnri.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mplnri.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mplnri.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mplnri.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mplnri.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mplnri.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mplnri.o: /usr/include/sys/_pthread/_pthread_t.h
c_mplnri.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mplnri.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mplnri.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mplnri.o: ../../../.././include/ncarg/c.h
c_mpname.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mpname.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mpname.o: /usr/include/sys/_posix_availability.h
c_mpname.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mpname.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mpname.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mpname.o: /usr/include/sys/_pthread/_pthread_types.h
c_mpname.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mpname.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mpname.o: /usr/include/sys/_types/_int16_t.h
c_mpname.o: /usr/include/sys/_types/_int32_t.h
c_mpname.o: /usr/include/sys/_types/_int64_t.h
c_mpname.o: /usr/include/sys/_types/_u_int8_t.h
c_mpname.o: /usr/include/sys/_types/_u_int16_t.h
c_mpname.o: /usr/include/sys/_types/_u_int32_t.h
c_mpname.o: /usr/include/sys/_types/_u_int64_t.h
c_mpname.o: /usr/include/sys/_types/_intptr_t.h
c_mpname.o: /usr/include/sys/_types/_uintptr_t.h
c_mpname.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mpname.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mpname.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mpname.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mpname.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mpname.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mpname.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mpname.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mpname.o: /usr/include/sys/_types/_u_char.h
c_mpname.o: /usr/include/sys/_types/_u_short.h
c_mpname.o: /usr/include/sys/_types/_u_int.h
c_mpname.o: /usr/include/sys/_types/_caddr_t.h
c_mpname.o: /usr/include/sys/_types/_dev_t.h
c_mpname.o: /usr/include/sys/_types/_blkcnt_t.h
c_mpname.o: /usr/include/sys/_types/_blksize_t.h
c_mpname.o: /usr/include/sys/_types/_gid_t.h
c_mpname.o: /usr/include/sys/_types/_in_addr_t.h
c_mpname.o: /usr/include/sys/_types/_in_port_t.h
c_mpname.o: /usr/include/sys/_types/_ino_t.h
c_mpname.o: /usr/include/sys/_types/_ino64_t.h
c_mpname.o: /usr/include/sys/_types/_key_t.h
c_mpname.o: /usr/include/sys/_types/_mode_t.h
c_mpname.o: /usr/include/sys/_types/_nlink_t.h
c_mpname.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mpname.o: /usr/include/sys/_types/_uid_t.h
c_mpname.o: /usr/include/sys/_types/_clock_t.h
c_mpname.o: /usr/include/sys/_types/_time_t.h
c_mpname.o: /usr/include/sys/_types/_useconds_t.h
c_mpname.o: /usr/include/sys/_types/_suseconds_t.h
c_mpname.o: /usr/include/sys/_types/_rsize_t.h
c_mpname.o: /usr/include/sys/_types/_errno_t.h
c_mpname.o: /usr/include/sys/_types/_fd_def.h
c_mpname.o: /usr/include/sys/_types/_fd_setsize.h
c_mpname.o: /usr/include/sys/_types/_fd_set.h
c_mpname.o: /usr/include/sys/_types/_fd_clr.h
c_mpname.o: /usr/include/sys/_types/_fd_zero.h
c_mpname.o: /usr/include/sys/_types/_fd_isset.h
c_mpname.o: /usr/include/sys/_types/_fd_copy.h
c_mpname.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mpname.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mpname.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mpname.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mpname.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mpname.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mpname.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mpname.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mpname.o: /usr/include/sys/_pthread/_pthread_t.h
c_mpname.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mpname.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mpname.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mpname.o: ../../../.././include/ncarg/c.h
c_mapgtd.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mapgtd.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mapgtd.o: /usr/include/sys/_posix_availability.h
c_mapgtd.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mapgtd.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mapgtd.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mapgtd.o: /usr/include/sys/_pthread/_pthread_types.h
c_mapgtd.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mapgtd.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mapgtd.o: /usr/include/sys/_types/_int16_t.h
c_mapgtd.o: /usr/include/sys/_types/_int32_t.h
c_mapgtd.o: /usr/include/sys/_types/_int64_t.h
c_mapgtd.o: /usr/include/sys/_types/_u_int8_t.h
c_mapgtd.o: /usr/include/sys/_types/_u_int16_t.h
c_mapgtd.o: /usr/include/sys/_types/_u_int32_t.h
c_mapgtd.o: /usr/include/sys/_types/_u_int64_t.h
c_mapgtd.o: /usr/include/sys/_types/_intptr_t.h
c_mapgtd.o: /usr/include/sys/_types/_uintptr_t.h
c_mapgtd.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mapgtd.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mapgtd.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mapgtd.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mapgtd.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mapgtd.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mapgtd.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mapgtd.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mapgtd.o: /usr/include/sys/_types/_u_char.h
c_mapgtd.o: /usr/include/sys/_types/_u_short.h
c_mapgtd.o: /usr/include/sys/_types/_u_int.h
c_mapgtd.o: /usr/include/sys/_types/_caddr_t.h
c_mapgtd.o: /usr/include/sys/_types/_dev_t.h
c_mapgtd.o: /usr/include/sys/_types/_blkcnt_t.h
c_mapgtd.o: /usr/include/sys/_types/_blksize_t.h
c_mapgtd.o: /usr/include/sys/_types/_gid_t.h
c_mapgtd.o: /usr/include/sys/_types/_in_addr_t.h
c_mapgtd.o: /usr/include/sys/_types/_in_port_t.h
c_mapgtd.o: /usr/include/sys/_types/_ino_t.h
c_mapgtd.o: /usr/include/sys/_types/_ino64_t.h
c_mapgtd.o: /usr/include/sys/_types/_key_t.h
c_mapgtd.o: /usr/include/sys/_types/_mode_t.h
c_mapgtd.o: /usr/include/sys/_types/_nlink_t.h
c_mapgtd.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mapgtd.o: /usr/include/sys/_types/_uid_t.h
c_mapgtd.o: /usr/include/sys/_types/_clock_t.h
c_mapgtd.o: /usr/include/sys/_types/_time_t.h
c_mapgtd.o: /usr/include/sys/_types/_useconds_t.h
c_mapgtd.o: /usr/include/sys/_types/_suseconds_t.h
c_mapgtd.o: /usr/include/sys/_types/_rsize_t.h
c_mapgtd.o: /usr/include/sys/_types/_errno_t.h
c_mapgtd.o: /usr/include/sys/_types/_fd_def.h
c_mapgtd.o: /usr/include/sys/_types/_fd_setsize.h
c_mapgtd.o: /usr/include/sys/_types/_fd_set.h
c_mapgtd.o: /usr/include/sys/_types/_fd_clr.h
c_mapgtd.o: /usr/include/sys/_types/_fd_zero.h
c_mapgtd.o: /usr/include/sys/_types/_fd_isset.h
c_mapgtd.o: /usr/include/sys/_types/_fd_copy.h
c_mapgtd.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mapgtd.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mapgtd.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mapgtd.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mapgtd.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mapgtd.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mapgtd.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mapgtd.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mapgtd.o: /usr/include/sys/_pthread/_pthread_t.h
c_mapgtd.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mapgtd.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mapgtd.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mapgtd.o: ../../../.././include/ncarg/c.h
c_mapstd.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mapstd.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mapstd.o: /usr/include/sys/_posix_availability.h
c_mapstd.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mapstd.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mapstd.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mapstd.o: /usr/include/sys/_pthread/_pthread_types.h
c_mapstd.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mapstd.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mapstd.o: /usr/include/sys/_types/_int16_t.h
c_mapstd.o: /usr/include/sys/_types/_int32_t.h
c_mapstd.o: /usr/include/sys/_types/_int64_t.h
c_mapstd.o: /usr/include/sys/_types/_u_int8_t.h
c_mapstd.o: /usr/include/sys/_types/_u_int16_t.h
c_mapstd.o: /usr/include/sys/_types/_u_int32_t.h
c_mapstd.o: /usr/include/sys/_types/_u_int64_t.h
c_mapstd.o: /usr/include/sys/_types/_intptr_t.h
c_mapstd.o: /usr/include/sys/_types/_uintptr_t.h
c_mapstd.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mapstd.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mapstd.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mapstd.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mapstd.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mapstd.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mapstd.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mapstd.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mapstd.o: /usr/include/sys/_types/_u_char.h
c_mapstd.o: /usr/include/sys/_types/_u_short.h
c_mapstd.o: /usr/include/sys/_types/_u_int.h
c_mapstd.o: /usr/include/sys/_types/_caddr_t.h
c_mapstd.o: /usr/include/sys/_types/_dev_t.h
c_mapstd.o: /usr/include/sys/_types/_blkcnt_t.h
c_mapstd.o: /usr/include/sys/_types/_blksize_t.h
c_mapstd.o: /usr/include/sys/_types/_gid_t.h
c_mapstd.o: /usr/include/sys/_types/_in_addr_t.h
c_mapstd.o: /usr/include/sys/_types/_in_port_t.h
c_mapstd.o: /usr/include/sys/_types/_ino_t.h
c_mapstd.o: /usr/include/sys/_types/_ino64_t.h
c_mapstd.o: /usr/include/sys/_types/_key_t.h
c_mapstd.o: /usr/include/sys/_types/_mode_t.h
c_mapstd.o: /usr/include/sys/_types/_nlink_t.h
c_mapstd.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mapstd.o: /usr/include/sys/_types/_uid_t.h
c_mapstd.o: /usr/include/sys/_types/_clock_t.h
c_mapstd.o: /usr/include/sys/_types/_time_t.h
c_mapstd.o: /usr/include/sys/_types/_useconds_t.h
c_mapstd.o: /usr/include/sys/_types/_suseconds_t.h
c_mapstd.o: /usr/include/sys/_types/_rsize_t.h
c_mapstd.o: /usr/include/sys/_types/_errno_t.h
c_mapstd.o: /usr/include/sys/_types/_fd_def.h
c_mapstd.o: /usr/include/sys/_types/_fd_setsize.h
c_mapstd.o: /usr/include/sys/_types/_fd_set.h
c_mapstd.o: /usr/include/sys/_types/_fd_clr.h
c_mapstd.o: /usr/include/sys/_types/_fd_zero.h
c_mapstd.o: /usr/include/sys/_types/_fd_isset.h
c_mapstd.o: /usr/include/sys/_types/_fd_copy.h
c_mapstd.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mapstd.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mapstd.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mapstd.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mapstd.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mapstd.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mapstd.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mapstd.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mapstd.o: /usr/include/sys/_pthread/_pthread_t.h
c_mapstd.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mapstd.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mapstd.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mapstd.o: ../../../.././include/ncarg/c.h
c_mpsetd.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mpsetd.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mpsetd.o: /usr/include/sys/_posix_availability.h
c_mpsetd.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mpsetd.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mpsetd.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mpsetd.o: /usr/include/sys/_pthread/_pthread_types.h
c_mpsetd.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mpsetd.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mpsetd.o: /usr/include/sys/_types/_int16_t.h
c_mpsetd.o: /usr/include/sys/_types/_int32_t.h
c_mpsetd.o: /usr/include/sys/_types/_int64_t.h
c_mpsetd.o: /usr/include/sys/_types/_u_int8_t.h
c_mpsetd.o: /usr/include/sys/_types/_u_int16_t.h
c_mpsetd.o: /usr/include/sys/_types/_u_int32_t.h
c_mpsetd.o: /usr/include/sys/_types/_u_int64_t.h
c_mpsetd.o: /usr/include/sys/_types/_intptr_t.h
c_mpsetd.o: /usr/include/sys/_types/_uintptr_t.h
c_mpsetd.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mpsetd.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mpsetd.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mpsetd.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mpsetd.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mpsetd.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mpsetd.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mpsetd.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mpsetd.o: /usr/include/sys/_types/_u_char.h
c_mpsetd.o: /usr/include/sys/_types/_u_short.h
c_mpsetd.o: /usr/include/sys/_types/_u_int.h
c_mpsetd.o: /usr/include/sys/_types/_caddr_t.h
c_mpsetd.o: /usr/include/sys/_types/_dev_t.h
c_mpsetd.o: /usr/include/sys/_types/_blkcnt_t.h
c_mpsetd.o: /usr/include/sys/_types/_blksize_t.h
c_mpsetd.o: /usr/include/sys/_types/_gid_t.h
c_mpsetd.o: /usr/include/sys/_types/_in_addr_t.h
c_mpsetd.o: /usr/include/sys/_types/_in_port_t.h
c_mpsetd.o: /usr/include/sys/_types/_ino_t.h
c_mpsetd.o: /usr/include/sys/_types/_ino64_t.h
c_mpsetd.o: /usr/include/sys/_types/_key_t.h
c_mpsetd.o: /usr/include/sys/_types/_mode_t.h
c_mpsetd.o: /usr/include/sys/_types/_nlink_t.h
c_mpsetd.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mpsetd.o: /usr/include/sys/_types/_uid_t.h
c_mpsetd.o: /usr/include/sys/_types/_clock_t.h
c_mpsetd.o: /usr/include/sys/_types/_time_t.h
c_mpsetd.o: /usr/include/sys/_types/_useconds_t.h
c_mpsetd.o: /usr/include/sys/_types/_suseconds_t.h
c_mpsetd.o: /usr/include/sys/_types/_rsize_t.h
c_mpsetd.o: /usr/include/sys/_types/_errno_t.h
c_mpsetd.o: /usr/include/sys/_types/_fd_def.h
c_mpsetd.o: /usr/include/sys/_types/_fd_setsize.h
c_mpsetd.o: /usr/include/sys/_types/_fd_set.h
c_mpsetd.o: /usr/include/sys/_types/_fd_clr.h
c_mpsetd.o: /usr/include/sys/_types/_fd_zero.h
c_mpsetd.o: /usr/include/sys/_types/_fd_isset.h
c_mpsetd.o: /usr/include/sys/_types/_fd_copy.h
c_mpsetd.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mpsetd.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mpsetd.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mpsetd.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mpsetd.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mpsetd.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mpsetd.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mpsetd.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mpsetd.o: /usr/include/sys/_pthread/_pthread_t.h
c_mpsetd.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mpsetd.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mpsetd.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mpsetd.o: ../../../.././include/ncarg/c.h
c_mdgetc.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdgetc.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdgetc.o: /usr/include/sys/_posix_availability.h
c_mdgetc.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdgetc.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdgetc.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdgetc.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdgetc.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdgetc.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdgetc.o: /usr/include/sys/_types/_int16_t.h
c_mdgetc.o: /usr/include/sys/_types/_int32_t.h
c_mdgetc.o: /usr/include/sys/_types/_int64_t.h
c_mdgetc.o: /usr/include/sys/_types/_u_int8_t.h
c_mdgetc.o: /usr/include/sys/_types/_u_int16_t.h
c_mdgetc.o: /usr/include/sys/_types/_u_int32_t.h
c_mdgetc.o: /usr/include/sys/_types/_u_int64_t.h
c_mdgetc.o: /usr/include/sys/_types/_intptr_t.h
c_mdgetc.o: /usr/include/sys/_types/_uintptr_t.h
c_mdgetc.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdgetc.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdgetc.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdgetc.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdgetc.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdgetc.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdgetc.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdgetc.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdgetc.o: /usr/include/sys/_types/_u_char.h
c_mdgetc.o: /usr/include/sys/_types/_u_short.h
c_mdgetc.o: /usr/include/sys/_types/_u_int.h
c_mdgetc.o: /usr/include/sys/_types/_caddr_t.h
c_mdgetc.o: /usr/include/sys/_types/_dev_t.h
c_mdgetc.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdgetc.o: /usr/include/sys/_types/_blksize_t.h
c_mdgetc.o: /usr/include/sys/_types/_gid_t.h
c_mdgetc.o: /usr/include/sys/_types/_in_addr_t.h
c_mdgetc.o: /usr/include/sys/_types/_in_port_t.h
c_mdgetc.o: /usr/include/sys/_types/_ino_t.h
c_mdgetc.o: /usr/include/sys/_types/_ino64_t.h
c_mdgetc.o: /usr/include/sys/_types/_key_t.h
c_mdgetc.o: /usr/include/sys/_types/_mode_t.h
c_mdgetc.o: /usr/include/sys/_types/_nlink_t.h
c_mdgetc.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdgetc.o: /usr/include/sys/_types/_uid_t.h
c_mdgetc.o: /usr/include/sys/_types/_clock_t.h
c_mdgetc.o: /usr/include/sys/_types/_time_t.h
c_mdgetc.o: /usr/include/sys/_types/_useconds_t.h
c_mdgetc.o: /usr/include/sys/_types/_suseconds_t.h
c_mdgetc.o: /usr/include/sys/_types/_rsize_t.h
c_mdgetc.o: /usr/include/sys/_types/_errno_t.h
c_mdgetc.o: /usr/include/sys/_types/_fd_def.h
c_mdgetc.o: /usr/include/sys/_types/_fd_setsize.h
c_mdgetc.o: /usr/include/sys/_types/_fd_set.h
c_mdgetc.o: /usr/include/sys/_types/_fd_clr.h
c_mdgetc.o: /usr/include/sys/_types/_fd_zero.h
c_mdgetc.o: /usr/include/sys/_types/_fd_isset.h
c_mdgetc.o: /usr/include/sys/_types/_fd_copy.h
c_mdgetc.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdgetc.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdgetc.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdgetc.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdgetc.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdgetc.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdgetc.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdgetc.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdgetc.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdgetc.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdgetc.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdgetc.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdgetc.o: ../../../.././include/ncarg/c.h
c_mdgetd.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdgetd.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdgetd.o: /usr/include/sys/_posix_availability.h
c_mdgetd.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdgetd.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdgetd.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdgetd.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdgetd.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdgetd.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdgetd.o: /usr/include/sys/_types/_int16_t.h
c_mdgetd.o: /usr/include/sys/_types/_int32_t.h
c_mdgetd.o: /usr/include/sys/_types/_int64_t.h
c_mdgetd.o: /usr/include/sys/_types/_u_int8_t.h
c_mdgetd.o: /usr/include/sys/_types/_u_int16_t.h
c_mdgetd.o: /usr/include/sys/_types/_u_int32_t.h
c_mdgetd.o: /usr/include/sys/_types/_u_int64_t.h
c_mdgetd.o: /usr/include/sys/_types/_intptr_t.h
c_mdgetd.o: /usr/include/sys/_types/_uintptr_t.h
c_mdgetd.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdgetd.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdgetd.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdgetd.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdgetd.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdgetd.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdgetd.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdgetd.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdgetd.o: /usr/include/sys/_types/_u_char.h
c_mdgetd.o: /usr/include/sys/_types/_u_short.h
c_mdgetd.o: /usr/include/sys/_types/_u_int.h
c_mdgetd.o: /usr/include/sys/_types/_caddr_t.h
c_mdgetd.o: /usr/include/sys/_types/_dev_t.h
c_mdgetd.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdgetd.o: /usr/include/sys/_types/_blksize_t.h
c_mdgetd.o: /usr/include/sys/_types/_gid_t.h
c_mdgetd.o: /usr/include/sys/_types/_in_addr_t.h
c_mdgetd.o: /usr/include/sys/_types/_in_port_t.h
c_mdgetd.o: /usr/include/sys/_types/_ino_t.h
c_mdgetd.o: /usr/include/sys/_types/_ino64_t.h
c_mdgetd.o: /usr/include/sys/_types/_key_t.h
c_mdgetd.o: /usr/include/sys/_types/_mode_t.h
c_mdgetd.o: /usr/include/sys/_types/_nlink_t.h
c_mdgetd.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdgetd.o: /usr/include/sys/_types/_uid_t.h
c_mdgetd.o: /usr/include/sys/_types/_clock_t.h
c_mdgetd.o: /usr/include/sys/_types/_time_t.h
c_mdgetd.o: /usr/include/sys/_types/_useconds_t.h
c_mdgetd.o: /usr/include/sys/_types/_suseconds_t.h
c_mdgetd.o: /usr/include/sys/_types/_rsize_t.h
c_mdgetd.o: /usr/include/sys/_types/_errno_t.h
c_mdgetd.o: /usr/include/sys/_types/_fd_def.h
c_mdgetd.o: /usr/include/sys/_types/_fd_setsize.h
c_mdgetd.o: /usr/include/sys/_types/_fd_set.h
c_mdgetd.o: /usr/include/sys/_types/_fd_clr.h
c_mdgetd.o: /usr/include/sys/_types/_fd_zero.h
c_mdgetd.o: /usr/include/sys/_types/_fd_isset.h
c_mdgetd.o: /usr/include/sys/_types/_fd_copy.h
c_mdgetd.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdgetd.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdgetd.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdgetd.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdgetd.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdgetd.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdgetd.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdgetd.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdgetd.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdgetd.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdgetd.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdgetd.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdgetd.o: ../../../.././include/ncarg/c.h
c_mdgeti.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdgeti.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdgeti.o: /usr/include/sys/_posix_availability.h
c_mdgeti.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdgeti.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdgeti.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdgeti.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdgeti.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdgeti.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdgeti.o: /usr/include/sys/_types/_int16_t.h
c_mdgeti.o: /usr/include/sys/_types/_int32_t.h
c_mdgeti.o: /usr/include/sys/_types/_int64_t.h
c_mdgeti.o: /usr/include/sys/_types/_u_int8_t.h
c_mdgeti.o: /usr/include/sys/_types/_u_int16_t.h
c_mdgeti.o: /usr/include/sys/_types/_u_int32_t.h
c_mdgeti.o: /usr/include/sys/_types/_u_int64_t.h
c_mdgeti.o: /usr/include/sys/_types/_intptr_t.h
c_mdgeti.o: /usr/include/sys/_types/_uintptr_t.h
c_mdgeti.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdgeti.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdgeti.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdgeti.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdgeti.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdgeti.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdgeti.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdgeti.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdgeti.o: /usr/include/sys/_types/_u_char.h
c_mdgeti.o: /usr/include/sys/_types/_u_short.h
c_mdgeti.o: /usr/include/sys/_types/_u_int.h
c_mdgeti.o: /usr/include/sys/_types/_caddr_t.h
c_mdgeti.o: /usr/include/sys/_types/_dev_t.h
c_mdgeti.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdgeti.o: /usr/include/sys/_types/_blksize_t.h
c_mdgeti.o: /usr/include/sys/_types/_gid_t.h
c_mdgeti.o: /usr/include/sys/_types/_in_addr_t.h
c_mdgeti.o: /usr/include/sys/_types/_in_port_t.h
c_mdgeti.o: /usr/include/sys/_types/_ino_t.h
c_mdgeti.o: /usr/include/sys/_types/_ino64_t.h
c_mdgeti.o: /usr/include/sys/_types/_key_t.h
c_mdgeti.o: /usr/include/sys/_types/_mode_t.h
c_mdgeti.o: /usr/include/sys/_types/_nlink_t.h
c_mdgeti.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdgeti.o: /usr/include/sys/_types/_uid_t.h
c_mdgeti.o: /usr/include/sys/_types/_clock_t.h
c_mdgeti.o: /usr/include/sys/_types/_time_t.h
c_mdgeti.o: /usr/include/sys/_types/_useconds_t.h
c_mdgeti.o: /usr/include/sys/_types/_suseconds_t.h
c_mdgeti.o: /usr/include/sys/_types/_rsize_t.h
c_mdgeti.o: /usr/include/sys/_types/_errno_t.h
c_mdgeti.o: /usr/include/sys/_types/_fd_def.h
c_mdgeti.o: /usr/include/sys/_types/_fd_setsize.h
c_mdgeti.o: /usr/include/sys/_types/_fd_set.h
c_mdgeti.o: /usr/include/sys/_types/_fd_clr.h
c_mdgeti.o: /usr/include/sys/_types/_fd_zero.h
c_mdgeti.o: /usr/include/sys/_types/_fd_isset.h
c_mdgeti.o: /usr/include/sys/_types/_fd_copy.h
c_mdgeti.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdgeti.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdgeti.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdgeti.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdgeti.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdgeti.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdgeti.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdgeti.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdgeti.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdgeti.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdgeti.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdgeti.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdgeti.o: ../../../.././include/ncarg/c.h
c_mdgetl.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdgetl.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdgetl.o: /usr/include/sys/_posix_availability.h
c_mdgetl.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdgetl.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdgetl.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdgetl.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdgetl.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdgetl.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdgetl.o: /usr/include/sys/_types/_int16_t.h
c_mdgetl.o: /usr/include/sys/_types/_int32_t.h
c_mdgetl.o: /usr/include/sys/_types/_int64_t.h
c_mdgetl.o: /usr/include/sys/_types/_u_int8_t.h
c_mdgetl.o: /usr/include/sys/_types/_u_int16_t.h
c_mdgetl.o: /usr/include/sys/_types/_u_int32_t.h
c_mdgetl.o: /usr/include/sys/_types/_u_int64_t.h
c_mdgetl.o: /usr/include/sys/_types/_intptr_t.h
c_mdgetl.o: /usr/include/sys/_types/_uintptr_t.h
c_mdgetl.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdgetl.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdgetl.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdgetl.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdgetl.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdgetl.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdgetl.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdgetl.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdgetl.o: /usr/include/sys/_types/_u_char.h
c_mdgetl.o: /usr/include/sys/_types/_u_short.h
c_mdgetl.o: /usr/include/sys/_types/_u_int.h
c_mdgetl.o: /usr/include/sys/_types/_caddr_t.h
c_mdgetl.o: /usr/include/sys/_types/_dev_t.h
c_mdgetl.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdgetl.o: /usr/include/sys/_types/_blksize_t.h
c_mdgetl.o: /usr/include/sys/_types/_gid_t.h
c_mdgetl.o: /usr/include/sys/_types/_in_addr_t.h
c_mdgetl.o: /usr/include/sys/_types/_in_port_t.h
c_mdgetl.o: /usr/include/sys/_types/_ino_t.h
c_mdgetl.o: /usr/include/sys/_types/_ino64_t.h
c_mdgetl.o: /usr/include/sys/_types/_key_t.h
c_mdgetl.o: /usr/include/sys/_types/_mode_t.h
c_mdgetl.o: /usr/include/sys/_types/_nlink_t.h
c_mdgetl.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdgetl.o: /usr/include/sys/_types/_uid_t.h
c_mdgetl.o: /usr/include/sys/_types/_clock_t.h
c_mdgetl.o: /usr/include/sys/_types/_time_t.h
c_mdgetl.o: /usr/include/sys/_types/_useconds_t.h
c_mdgetl.o: /usr/include/sys/_types/_suseconds_t.h
c_mdgetl.o: /usr/include/sys/_types/_rsize_t.h
c_mdgetl.o: /usr/include/sys/_types/_errno_t.h
c_mdgetl.o: /usr/include/sys/_types/_fd_def.h
c_mdgetl.o: /usr/include/sys/_types/_fd_setsize.h
c_mdgetl.o: /usr/include/sys/_types/_fd_set.h
c_mdgetl.o: /usr/include/sys/_types/_fd_clr.h
c_mdgetl.o: /usr/include/sys/_types/_fd_zero.h
c_mdgetl.o: /usr/include/sys/_types/_fd_isset.h
c_mdgetl.o: /usr/include/sys/_types/_fd_copy.h
c_mdgetl.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdgetl.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdgetl.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdgetl.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdgetl.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdgetl.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdgetl.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdgetl.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdgetl.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdgetl.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdgetl.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdgetl.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdgetl.o: ../../../.././include/ncarg/c.h
c_mdgetr.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdgetr.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdgetr.o: /usr/include/sys/_posix_availability.h
c_mdgetr.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdgetr.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdgetr.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdgetr.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdgetr.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdgetr.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdgetr.o: /usr/include/sys/_types/_int16_t.h
c_mdgetr.o: /usr/include/sys/_types/_int32_t.h
c_mdgetr.o: /usr/include/sys/_types/_int64_t.h
c_mdgetr.o: /usr/include/sys/_types/_u_int8_t.h
c_mdgetr.o: /usr/include/sys/_types/_u_int16_t.h
c_mdgetr.o: /usr/include/sys/_types/_u_int32_t.h
c_mdgetr.o: /usr/include/sys/_types/_u_int64_t.h
c_mdgetr.o: /usr/include/sys/_types/_intptr_t.h
c_mdgetr.o: /usr/include/sys/_types/_uintptr_t.h
c_mdgetr.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdgetr.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdgetr.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdgetr.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdgetr.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdgetr.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdgetr.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdgetr.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdgetr.o: /usr/include/sys/_types/_u_char.h
c_mdgetr.o: /usr/include/sys/_types/_u_short.h
c_mdgetr.o: /usr/include/sys/_types/_u_int.h
c_mdgetr.o: /usr/include/sys/_types/_caddr_t.h
c_mdgetr.o: /usr/include/sys/_types/_dev_t.h
c_mdgetr.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdgetr.o: /usr/include/sys/_types/_blksize_t.h
c_mdgetr.o: /usr/include/sys/_types/_gid_t.h
c_mdgetr.o: /usr/include/sys/_types/_in_addr_t.h
c_mdgetr.o: /usr/include/sys/_types/_in_port_t.h
c_mdgetr.o: /usr/include/sys/_types/_ino_t.h
c_mdgetr.o: /usr/include/sys/_types/_ino64_t.h
c_mdgetr.o: /usr/include/sys/_types/_key_t.h
c_mdgetr.o: /usr/include/sys/_types/_mode_t.h
c_mdgetr.o: /usr/include/sys/_types/_nlink_t.h
c_mdgetr.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdgetr.o: /usr/include/sys/_types/_uid_t.h
c_mdgetr.o: /usr/include/sys/_types/_clock_t.h
c_mdgetr.o: /usr/include/sys/_types/_time_t.h
c_mdgetr.o: /usr/include/sys/_types/_useconds_t.h
c_mdgetr.o: /usr/include/sys/_types/_suseconds_t.h
c_mdgetr.o: /usr/include/sys/_types/_rsize_t.h
c_mdgetr.o: /usr/include/sys/_types/_errno_t.h
c_mdgetr.o: /usr/include/sys/_types/_fd_def.h
c_mdgetr.o: /usr/include/sys/_types/_fd_setsize.h
c_mdgetr.o: /usr/include/sys/_types/_fd_set.h
c_mdgetr.o: /usr/include/sys/_types/_fd_clr.h
c_mdgetr.o: /usr/include/sys/_types/_fd_zero.h
c_mdgetr.o: /usr/include/sys/_types/_fd_isset.h
c_mdgetr.o: /usr/include/sys/_types/_fd_copy.h
c_mdgetr.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdgetr.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdgetr.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdgetr.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdgetr.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdgetr.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdgetr.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdgetr.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdgetr.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdgetr.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdgetr.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdgetr.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdgetr.o: ../../../.././include/ncarg/c.h
c_mdsetc.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdsetc.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdsetc.o: /usr/include/sys/_posix_availability.h
c_mdsetc.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdsetc.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdsetc.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdsetc.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdsetc.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdsetc.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdsetc.o: /usr/include/sys/_types/_int16_t.h
c_mdsetc.o: /usr/include/sys/_types/_int32_t.h
c_mdsetc.o: /usr/include/sys/_types/_int64_t.h
c_mdsetc.o: /usr/include/sys/_types/_u_int8_t.h
c_mdsetc.o: /usr/include/sys/_types/_u_int16_t.h
c_mdsetc.o: /usr/include/sys/_types/_u_int32_t.h
c_mdsetc.o: /usr/include/sys/_types/_u_int64_t.h
c_mdsetc.o: /usr/include/sys/_types/_intptr_t.h
c_mdsetc.o: /usr/include/sys/_types/_uintptr_t.h
c_mdsetc.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdsetc.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdsetc.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdsetc.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdsetc.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdsetc.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdsetc.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdsetc.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdsetc.o: /usr/include/sys/_types/_u_char.h
c_mdsetc.o: /usr/include/sys/_types/_u_short.h
c_mdsetc.o: /usr/include/sys/_types/_u_int.h
c_mdsetc.o: /usr/include/sys/_types/_caddr_t.h
c_mdsetc.o: /usr/include/sys/_types/_dev_t.h
c_mdsetc.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdsetc.o: /usr/include/sys/_types/_blksize_t.h
c_mdsetc.o: /usr/include/sys/_types/_gid_t.h
c_mdsetc.o: /usr/include/sys/_types/_in_addr_t.h
c_mdsetc.o: /usr/include/sys/_types/_in_port_t.h
c_mdsetc.o: /usr/include/sys/_types/_ino_t.h
c_mdsetc.o: /usr/include/sys/_types/_ino64_t.h
c_mdsetc.o: /usr/include/sys/_types/_key_t.h
c_mdsetc.o: /usr/include/sys/_types/_mode_t.h
c_mdsetc.o: /usr/include/sys/_types/_nlink_t.h
c_mdsetc.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdsetc.o: /usr/include/sys/_types/_uid_t.h
c_mdsetc.o: /usr/include/sys/_types/_clock_t.h
c_mdsetc.o: /usr/include/sys/_types/_time_t.h
c_mdsetc.o: /usr/include/sys/_types/_useconds_t.h
c_mdsetc.o: /usr/include/sys/_types/_suseconds_t.h
c_mdsetc.o: /usr/include/sys/_types/_rsize_t.h
c_mdsetc.o: /usr/include/sys/_types/_errno_t.h
c_mdsetc.o: /usr/include/sys/_types/_fd_def.h
c_mdsetc.o: /usr/include/sys/_types/_fd_setsize.h
c_mdsetc.o: /usr/include/sys/_types/_fd_set.h
c_mdsetc.o: /usr/include/sys/_types/_fd_clr.h
c_mdsetc.o: /usr/include/sys/_types/_fd_zero.h
c_mdsetc.o: /usr/include/sys/_types/_fd_isset.h
c_mdsetc.o: /usr/include/sys/_types/_fd_copy.h
c_mdsetc.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdsetc.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdsetc.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdsetc.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdsetc.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdsetc.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdsetc.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdsetc.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdsetc.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdsetc.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdsetc.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdsetc.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdsetc.o: ../../../.././include/ncarg/c.h
c_mdsetd.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdsetd.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdsetd.o: /usr/include/sys/_posix_availability.h
c_mdsetd.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdsetd.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdsetd.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdsetd.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdsetd.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdsetd.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdsetd.o: /usr/include/sys/_types/_int16_t.h
c_mdsetd.o: /usr/include/sys/_types/_int32_t.h
c_mdsetd.o: /usr/include/sys/_types/_int64_t.h
c_mdsetd.o: /usr/include/sys/_types/_u_int8_t.h
c_mdsetd.o: /usr/include/sys/_types/_u_int16_t.h
c_mdsetd.o: /usr/include/sys/_types/_u_int32_t.h
c_mdsetd.o: /usr/include/sys/_types/_u_int64_t.h
c_mdsetd.o: /usr/include/sys/_types/_intptr_t.h
c_mdsetd.o: /usr/include/sys/_types/_uintptr_t.h
c_mdsetd.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdsetd.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdsetd.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdsetd.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdsetd.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdsetd.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdsetd.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdsetd.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdsetd.o: /usr/include/sys/_types/_u_char.h
c_mdsetd.o: /usr/include/sys/_types/_u_short.h
c_mdsetd.o: /usr/include/sys/_types/_u_int.h
c_mdsetd.o: /usr/include/sys/_types/_caddr_t.h
c_mdsetd.o: /usr/include/sys/_types/_dev_t.h
c_mdsetd.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdsetd.o: /usr/include/sys/_types/_blksize_t.h
c_mdsetd.o: /usr/include/sys/_types/_gid_t.h
c_mdsetd.o: /usr/include/sys/_types/_in_addr_t.h
c_mdsetd.o: /usr/include/sys/_types/_in_port_t.h
c_mdsetd.o: /usr/include/sys/_types/_ino_t.h
c_mdsetd.o: /usr/include/sys/_types/_ino64_t.h
c_mdsetd.o: /usr/include/sys/_types/_key_t.h
c_mdsetd.o: /usr/include/sys/_types/_mode_t.h
c_mdsetd.o: /usr/include/sys/_types/_nlink_t.h
c_mdsetd.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdsetd.o: /usr/include/sys/_types/_uid_t.h
c_mdsetd.o: /usr/include/sys/_types/_clock_t.h
c_mdsetd.o: /usr/include/sys/_types/_time_t.h
c_mdsetd.o: /usr/include/sys/_types/_useconds_t.h
c_mdsetd.o: /usr/include/sys/_types/_suseconds_t.h
c_mdsetd.o: /usr/include/sys/_types/_rsize_t.h
c_mdsetd.o: /usr/include/sys/_types/_errno_t.h
c_mdsetd.o: /usr/include/sys/_types/_fd_def.h
c_mdsetd.o: /usr/include/sys/_types/_fd_setsize.h
c_mdsetd.o: /usr/include/sys/_types/_fd_set.h
c_mdsetd.o: /usr/include/sys/_types/_fd_clr.h
c_mdsetd.o: /usr/include/sys/_types/_fd_zero.h
c_mdsetd.o: /usr/include/sys/_types/_fd_isset.h
c_mdsetd.o: /usr/include/sys/_types/_fd_copy.h
c_mdsetd.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdsetd.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdsetd.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdsetd.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdsetd.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdsetd.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdsetd.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdsetd.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdsetd.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdsetd.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdsetd.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdsetd.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdsetd.o: ../../../.././include/ncarg/c.h
c_mdseti.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdseti.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdseti.o: /usr/include/sys/_posix_availability.h
c_mdseti.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdseti.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdseti.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdseti.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdseti.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdseti.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdseti.o: /usr/include/sys/_types/_int16_t.h
c_mdseti.o: /usr/include/sys/_types/_int32_t.h
c_mdseti.o: /usr/include/sys/_types/_int64_t.h
c_mdseti.o: /usr/include/sys/_types/_u_int8_t.h
c_mdseti.o: /usr/include/sys/_types/_u_int16_t.h
c_mdseti.o: /usr/include/sys/_types/_u_int32_t.h
c_mdseti.o: /usr/include/sys/_types/_u_int64_t.h
c_mdseti.o: /usr/include/sys/_types/_intptr_t.h
c_mdseti.o: /usr/include/sys/_types/_uintptr_t.h
c_mdseti.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdseti.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdseti.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdseti.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdseti.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdseti.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdseti.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdseti.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdseti.o: /usr/include/sys/_types/_u_char.h
c_mdseti.o: /usr/include/sys/_types/_u_short.h
c_mdseti.o: /usr/include/sys/_types/_u_int.h
c_mdseti.o: /usr/include/sys/_types/_caddr_t.h
c_mdseti.o: /usr/include/sys/_types/_dev_t.h
c_mdseti.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdseti.o: /usr/include/sys/_types/_blksize_t.h
c_mdseti.o: /usr/include/sys/_types/_gid_t.h
c_mdseti.o: /usr/include/sys/_types/_in_addr_t.h
c_mdseti.o: /usr/include/sys/_types/_in_port_t.h
c_mdseti.o: /usr/include/sys/_types/_ino_t.h
c_mdseti.o: /usr/include/sys/_types/_ino64_t.h
c_mdseti.o: /usr/include/sys/_types/_key_t.h
c_mdseti.o: /usr/include/sys/_types/_mode_t.h
c_mdseti.o: /usr/include/sys/_types/_nlink_t.h
c_mdseti.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdseti.o: /usr/include/sys/_types/_uid_t.h
c_mdseti.o: /usr/include/sys/_types/_clock_t.h
c_mdseti.o: /usr/include/sys/_types/_time_t.h
c_mdseti.o: /usr/include/sys/_types/_useconds_t.h
c_mdseti.o: /usr/include/sys/_types/_suseconds_t.h
c_mdseti.o: /usr/include/sys/_types/_rsize_t.h
c_mdseti.o: /usr/include/sys/_types/_errno_t.h
c_mdseti.o: /usr/include/sys/_types/_fd_def.h
c_mdseti.o: /usr/include/sys/_types/_fd_setsize.h
c_mdseti.o: /usr/include/sys/_types/_fd_set.h
c_mdseti.o: /usr/include/sys/_types/_fd_clr.h
c_mdseti.o: /usr/include/sys/_types/_fd_zero.h
c_mdseti.o: /usr/include/sys/_types/_fd_isset.h
c_mdseti.o: /usr/include/sys/_types/_fd_copy.h
c_mdseti.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdseti.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdseti.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdseti.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdseti.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdseti.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdseti.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdseti.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdseti.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdseti.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdseti.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdseti.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdseti.o: ../../../.././include/ncarg/c.h
c_mdsetl.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdsetl.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdsetl.o: /usr/include/sys/_posix_availability.h
c_mdsetl.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdsetl.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdsetl.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdsetl.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdsetl.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdsetl.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdsetl.o: /usr/include/sys/_types/_int16_t.h
c_mdsetl.o: /usr/include/sys/_types/_int32_t.h
c_mdsetl.o: /usr/include/sys/_types/_int64_t.h
c_mdsetl.o: /usr/include/sys/_types/_u_int8_t.h
c_mdsetl.o: /usr/include/sys/_types/_u_int16_t.h
c_mdsetl.o: /usr/include/sys/_types/_u_int32_t.h
c_mdsetl.o: /usr/include/sys/_types/_u_int64_t.h
c_mdsetl.o: /usr/include/sys/_types/_intptr_t.h
c_mdsetl.o: /usr/include/sys/_types/_uintptr_t.h
c_mdsetl.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdsetl.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdsetl.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdsetl.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdsetl.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdsetl.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdsetl.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdsetl.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdsetl.o: /usr/include/sys/_types/_u_char.h
c_mdsetl.o: /usr/include/sys/_types/_u_short.h
c_mdsetl.o: /usr/include/sys/_types/_u_int.h
c_mdsetl.o: /usr/include/sys/_types/_caddr_t.h
c_mdsetl.o: /usr/include/sys/_types/_dev_t.h
c_mdsetl.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdsetl.o: /usr/include/sys/_types/_blksize_t.h
c_mdsetl.o: /usr/include/sys/_types/_gid_t.h
c_mdsetl.o: /usr/include/sys/_types/_in_addr_t.h
c_mdsetl.o: /usr/include/sys/_types/_in_port_t.h
c_mdsetl.o: /usr/include/sys/_types/_ino_t.h
c_mdsetl.o: /usr/include/sys/_types/_ino64_t.h
c_mdsetl.o: /usr/include/sys/_types/_key_t.h
c_mdsetl.o: /usr/include/sys/_types/_mode_t.h
c_mdsetl.o: /usr/include/sys/_types/_nlink_t.h
c_mdsetl.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdsetl.o: /usr/include/sys/_types/_uid_t.h
c_mdsetl.o: /usr/include/sys/_types/_clock_t.h
c_mdsetl.o: /usr/include/sys/_types/_time_t.h
c_mdsetl.o: /usr/include/sys/_types/_useconds_t.h
c_mdsetl.o: /usr/include/sys/_types/_suseconds_t.h
c_mdsetl.o: /usr/include/sys/_types/_rsize_t.h
c_mdsetl.o: /usr/include/sys/_types/_errno_t.h
c_mdsetl.o: /usr/include/sys/_types/_fd_def.h
c_mdsetl.o: /usr/include/sys/_types/_fd_setsize.h
c_mdsetl.o: /usr/include/sys/_types/_fd_set.h
c_mdsetl.o: /usr/include/sys/_types/_fd_clr.h
c_mdsetl.o: /usr/include/sys/_types/_fd_zero.h
c_mdsetl.o: /usr/include/sys/_types/_fd_isset.h
c_mdsetl.o: /usr/include/sys/_types/_fd_copy.h
c_mdsetl.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdsetl.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdsetl.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdsetl.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdsetl.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdsetl.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdsetl.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdsetl.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdsetl.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdsetl.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdsetl.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdsetl.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdsetl.o: ../../../.././include/ncarg/c.h
c_mdsetr.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdsetr.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdsetr.o: /usr/include/sys/_posix_availability.h
c_mdsetr.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdsetr.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdsetr.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdsetr.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdsetr.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdsetr.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdsetr.o: /usr/include/sys/_types/_int16_t.h
c_mdsetr.o: /usr/include/sys/_types/_int32_t.h
c_mdsetr.o: /usr/include/sys/_types/_int64_t.h
c_mdsetr.o: /usr/include/sys/_types/_u_int8_t.h
c_mdsetr.o: /usr/include/sys/_types/_u_int16_t.h
c_mdsetr.o: /usr/include/sys/_types/_u_int32_t.h
c_mdsetr.o: /usr/include/sys/_types/_u_int64_t.h
c_mdsetr.o: /usr/include/sys/_types/_intptr_t.h
c_mdsetr.o: /usr/include/sys/_types/_uintptr_t.h
c_mdsetr.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdsetr.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdsetr.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdsetr.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdsetr.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdsetr.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdsetr.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdsetr.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdsetr.o: /usr/include/sys/_types/_u_char.h
c_mdsetr.o: /usr/include/sys/_types/_u_short.h
c_mdsetr.o: /usr/include/sys/_types/_u_int.h
c_mdsetr.o: /usr/include/sys/_types/_caddr_t.h
c_mdsetr.o: /usr/include/sys/_types/_dev_t.h
c_mdsetr.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdsetr.o: /usr/include/sys/_types/_blksize_t.h
c_mdsetr.o: /usr/include/sys/_types/_gid_t.h
c_mdsetr.o: /usr/include/sys/_types/_in_addr_t.h
c_mdsetr.o: /usr/include/sys/_types/_in_port_t.h
c_mdsetr.o: /usr/include/sys/_types/_ino_t.h
c_mdsetr.o: /usr/include/sys/_types/_ino64_t.h
c_mdsetr.o: /usr/include/sys/_types/_key_t.h
c_mdsetr.o: /usr/include/sys/_types/_mode_t.h
c_mdsetr.o: /usr/include/sys/_types/_nlink_t.h
c_mdsetr.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdsetr.o: /usr/include/sys/_types/_uid_t.h
c_mdsetr.o: /usr/include/sys/_types/_clock_t.h
c_mdsetr.o: /usr/include/sys/_types/_time_t.h
c_mdsetr.o: /usr/include/sys/_types/_useconds_t.h
c_mdsetr.o: /usr/include/sys/_types/_suseconds_t.h
c_mdsetr.o: /usr/include/sys/_types/_rsize_t.h
c_mdsetr.o: /usr/include/sys/_types/_errno_t.h
c_mdsetr.o: /usr/include/sys/_types/_fd_def.h
c_mdsetr.o: /usr/include/sys/_types/_fd_setsize.h
c_mdsetr.o: /usr/include/sys/_types/_fd_set.h
c_mdsetr.o: /usr/include/sys/_types/_fd_clr.h
c_mdsetr.o: /usr/include/sys/_types/_fd_zero.h
c_mdsetr.o: /usr/include/sys/_types/_fd_isset.h
c_mdsetr.o: /usr/include/sys/_types/_fd_copy.h
c_mdsetr.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdsetr.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdsetr.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdsetr.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdsetr.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdsetr.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdsetr.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdsetr.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdsetr.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdsetr.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdsetr.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdsetr.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdsetr.o: ../../../.././include/ncarg/c.h
c_mdfnme.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdfnme.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdfnme.o: /usr/include/sys/_posix_availability.h
c_mdfnme.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdfnme.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdfnme.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdfnme.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdfnme.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdfnme.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdfnme.o: /usr/include/sys/_types/_int16_t.h
c_mdfnme.o: /usr/include/sys/_types/_int32_t.h
c_mdfnme.o: /usr/include/sys/_types/_int64_t.h
c_mdfnme.o: /usr/include/sys/_types/_u_int8_t.h
c_mdfnme.o: /usr/include/sys/_types/_u_int16_t.h
c_mdfnme.o: /usr/include/sys/_types/_u_int32_t.h
c_mdfnme.o: /usr/include/sys/_types/_u_int64_t.h
c_mdfnme.o: /usr/include/sys/_types/_intptr_t.h
c_mdfnme.o: /usr/include/sys/_types/_uintptr_t.h
c_mdfnme.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdfnme.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdfnme.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdfnme.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdfnme.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdfnme.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdfnme.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdfnme.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdfnme.o: /usr/include/sys/_types/_u_char.h
c_mdfnme.o: /usr/include/sys/_types/_u_short.h
c_mdfnme.o: /usr/include/sys/_types/_u_int.h
c_mdfnme.o: /usr/include/sys/_types/_caddr_t.h
c_mdfnme.o: /usr/include/sys/_types/_dev_t.h
c_mdfnme.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdfnme.o: /usr/include/sys/_types/_blksize_t.h
c_mdfnme.o: /usr/include/sys/_types/_gid_t.h
c_mdfnme.o: /usr/include/sys/_types/_in_addr_t.h
c_mdfnme.o: /usr/include/sys/_types/_in_port_t.h
c_mdfnme.o: /usr/include/sys/_types/_ino_t.h
c_mdfnme.o: /usr/include/sys/_types/_ino64_t.h
c_mdfnme.o: /usr/include/sys/_types/_key_t.h
c_mdfnme.o: /usr/include/sys/_types/_mode_t.h
c_mdfnme.o: /usr/include/sys/_types/_nlink_t.h
c_mdfnme.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdfnme.o: /usr/include/sys/_types/_uid_t.h
c_mdfnme.o: /usr/include/sys/_types/_clock_t.h
c_mdfnme.o: /usr/include/sys/_types/_time_t.h
c_mdfnme.o: /usr/include/sys/_types/_useconds_t.h
c_mdfnme.o: /usr/include/sys/_types/_suseconds_t.h
c_mdfnme.o: /usr/include/sys/_types/_rsize_t.h
c_mdfnme.o: /usr/include/sys/_types/_errno_t.h
c_mdfnme.o: /usr/include/sys/_types/_fd_def.h
c_mdfnme.o: /usr/include/sys/_types/_fd_setsize.h
c_mdfnme.o: /usr/include/sys/_types/_fd_set.h
c_mdfnme.o: /usr/include/sys/_types/_fd_clr.h
c_mdfnme.o: /usr/include/sys/_types/_fd_zero.h
c_mdfnme.o: /usr/include/sys/_types/_fd_isset.h
c_mdfnme.o: /usr/include/sys/_types/_fd_copy.h
c_mdfnme.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdfnme.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdfnme.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdfnme.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdfnme.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdfnme.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdfnme.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdfnme.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdfnme.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdfnme.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdfnme.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdfnme.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdfnme.o: ../../../.././include/ncarg/c.h
c_mdglty.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdglty.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdglty.o: /usr/include/sys/_posix_availability.h
c_mdglty.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdglty.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdglty.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdglty.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdglty.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdglty.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdglty.o: /usr/include/sys/_types/_int16_t.h
c_mdglty.o: /usr/include/sys/_types/_int32_t.h
c_mdglty.o: /usr/include/sys/_types/_int64_t.h
c_mdglty.o: /usr/include/sys/_types/_u_int8_t.h
c_mdglty.o: /usr/include/sys/_types/_u_int16_t.h
c_mdglty.o: /usr/include/sys/_types/_u_int32_t.h
c_mdglty.o: /usr/include/sys/_types/_u_int64_t.h
c_mdglty.o: /usr/include/sys/_types/_intptr_t.h
c_mdglty.o: /usr/include/sys/_types/_uintptr_t.h
c_mdglty.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdglty.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdglty.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdglty.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdglty.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdglty.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdglty.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdglty.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdglty.o: /usr/include/sys/_types/_u_char.h
c_mdglty.o: /usr/include/sys/_types/_u_short.h
c_mdglty.o: /usr/include/sys/_types/_u_int.h
c_mdglty.o: /usr/include/sys/_types/_caddr_t.h
c_mdglty.o: /usr/include/sys/_types/_dev_t.h
c_mdglty.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdglty.o: /usr/include/sys/_types/_blksize_t.h
c_mdglty.o: /usr/include/sys/_types/_gid_t.h
c_mdglty.o: /usr/include/sys/_types/_in_addr_t.h
c_mdglty.o: /usr/include/sys/_types/_in_port_t.h
c_mdglty.o: /usr/include/sys/_types/_ino_t.h
c_mdglty.o: /usr/include/sys/_types/_ino64_t.h
c_mdglty.o: /usr/include/sys/_types/_key_t.h
c_mdglty.o: /usr/include/sys/_types/_mode_t.h
c_mdglty.o: /usr/include/sys/_types/_nlink_t.h
c_mdglty.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdglty.o: /usr/include/sys/_types/_uid_t.h
c_mdglty.o: /usr/include/sys/_types/_clock_t.h
c_mdglty.o: /usr/include/sys/_types/_time_t.h
c_mdglty.o: /usr/include/sys/_types/_useconds_t.h
c_mdglty.o: /usr/include/sys/_types/_suseconds_t.h
c_mdglty.o: /usr/include/sys/_types/_rsize_t.h
c_mdglty.o: /usr/include/sys/_types/_errno_t.h
c_mdglty.o: /usr/include/sys/_types/_fd_def.h
c_mdglty.o: /usr/include/sys/_types/_fd_setsize.h
c_mdglty.o: /usr/include/sys/_types/_fd_set.h
c_mdglty.o: /usr/include/sys/_types/_fd_clr.h
c_mdglty.o: /usr/include/sys/_types/_fd_zero.h
c_mdglty.o: /usr/include/sys/_types/_fd_isset.h
c_mdglty.o: /usr/include/sys/_types/_fd_copy.h
c_mdglty.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdglty.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdglty.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdglty.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdglty.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdglty.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdglty.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdglty.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdglty.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdglty.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdglty.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdglty.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdglty.o: ../../../.././include/ncarg/c.h
c_mdiaty.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdiaty.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdiaty.o: /usr/include/sys/_posix_availability.h
c_mdiaty.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdiaty.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdiaty.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdiaty.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdiaty.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdiaty.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdiaty.o: /usr/include/sys/_types/_int16_t.h
c_mdiaty.o: /usr/include/sys/_types/_int32_t.h
c_mdiaty.o: /usr/include/sys/_types/_int64_t.h
c_mdiaty.o: /usr/include/sys/_types/_u_int8_t.h
c_mdiaty.o: /usr/include/sys/_types/_u_int16_t.h
c_mdiaty.o: /usr/include/sys/_types/_u_int32_t.h
c_mdiaty.o: /usr/include/sys/_types/_u_int64_t.h
c_mdiaty.o: /usr/include/sys/_types/_intptr_t.h
c_mdiaty.o: /usr/include/sys/_types/_uintptr_t.h
c_mdiaty.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdiaty.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdiaty.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdiaty.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdiaty.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdiaty.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdiaty.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdiaty.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdiaty.o: /usr/include/sys/_types/_u_char.h
c_mdiaty.o: /usr/include/sys/_types/_u_short.h
c_mdiaty.o: /usr/include/sys/_types/_u_int.h
c_mdiaty.o: /usr/include/sys/_types/_caddr_t.h
c_mdiaty.o: /usr/include/sys/_types/_dev_t.h
c_mdiaty.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdiaty.o: /usr/include/sys/_types/_blksize_t.h
c_mdiaty.o: /usr/include/sys/_types/_gid_t.h
c_mdiaty.o: /usr/include/sys/_types/_in_addr_t.h
c_mdiaty.o: /usr/include/sys/_types/_in_port_t.h
c_mdiaty.o: /usr/include/sys/_types/_ino_t.h
c_mdiaty.o: /usr/include/sys/_types/_ino64_t.h
c_mdiaty.o: /usr/include/sys/_types/_key_t.h
c_mdiaty.o: /usr/include/sys/_types/_mode_t.h
c_mdiaty.o: /usr/include/sys/_types/_nlink_t.h
c_mdiaty.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdiaty.o: /usr/include/sys/_types/_uid_t.h
c_mdiaty.o: /usr/include/sys/_types/_clock_t.h
c_mdiaty.o: /usr/include/sys/_types/_time_t.h
c_mdiaty.o: /usr/include/sys/_types/_useconds_t.h
c_mdiaty.o: /usr/include/sys/_types/_suseconds_t.h
c_mdiaty.o: /usr/include/sys/_types/_rsize_t.h
c_mdiaty.o: /usr/include/sys/_types/_errno_t.h
c_mdiaty.o: /usr/include/sys/_types/_fd_def.h
c_mdiaty.o: /usr/include/sys/_types/_fd_setsize.h
c_mdiaty.o: /usr/include/sys/_types/_fd_set.h
c_mdiaty.o: /usr/include/sys/_types/_fd_clr.h
c_mdiaty.o: /usr/include/sys/_types/_fd_zero.h
c_mdiaty.o: /usr/include/sys/_types/_fd_isset.h
c_mdiaty.o: /usr/include/sys/_types/_fd_copy.h
c_mdiaty.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdiaty.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdiaty.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdiaty.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdiaty.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdiaty.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdiaty.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdiaty.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdiaty.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdiaty.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdiaty.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdiaty.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdiaty.o: ../../../.././include/ncarg/c.h
c_mdifnb.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdifnb.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdifnb.o: /usr/include/sys/_posix_availability.h
c_mdifnb.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdifnb.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdifnb.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdifnb.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdifnb.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdifnb.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdifnb.o: /usr/include/sys/_types/_int16_t.h
c_mdifnb.o: /usr/include/sys/_types/_int32_t.h
c_mdifnb.o: /usr/include/sys/_types/_int64_t.h
c_mdifnb.o: /usr/include/sys/_types/_u_int8_t.h
c_mdifnb.o: /usr/include/sys/_types/_u_int16_t.h
c_mdifnb.o: /usr/include/sys/_types/_u_int32_t.h
c_mdifnb.o: /usr/include/sys/_types/_u_int64_t.h
c_mdifnb.o: /usr/include/sys/_types/_intptr_t.h
c_mdifnb.o: /usr/include/sys/_types/_uintptr_t.h
c_mdifnb.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdifnb.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdifnb.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdifnb.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdifnb.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdifnb.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdifnb.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdifnb.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdifnb.o: /usr/include/sys/_types/_u_char.h
c_mdifnb.o: /usr/include/sys/_types/_u_short.h
c_mdifnb.o: /usr/include/sys/_types/_u_int.h
c_mdifnb.o: /usr/include/sys/_types/_caddr_t.h
c_mdifnb.o: /usr/include/sys/_types/_dev_t.h
c_mdifnb.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdifnb.o: /usr/include/sys/_types/_blksize_t.h
c_mdifnb.o: /usr/include/sys/_types/_gid_t.h
c_mdifnb.o: /usr/include/sys/_types/_in_addr_t.h
c_mdifnb.o: /usr/include/sys/_types/_in_port_t.h
c_mdifnb.o: /usr/include/sys/_types/_ino_t.h
c_mdifnb.o: /usr/include/sys/_types/_ino64_t.h
c_mdifnb.o: /usr/include/sys/_types/_key_t.h
c_mdifnb.o: /usr/include/sys/_types/_mode_t.h
c_mdifnb.o: /usr/include/sys/_types/_nlink_t.h
c_mdifnb.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdifnb.o: /usr/include/sys/_types/_uid_t.h
c_mdifnb.o: /usr/include/sys/_types/_clock_t.h
c_mdifnb.o: /usr/include/sys/_types/_time_t.h
c_mdifnb.o: /usr/include/sys/_types/_useconds_t.h
c_mdifnb.o: /usr/include/sys/_types/_suseconds_t.h
c_mdifnb.o: /usr/include/sys/_types/_rsize_t.h
c_mdifnb.o: /usr/include/sys/_types/_errno_t.h
c_mdifnb.o: /usr/include/sys/_types/_fd_def.h
c_mdifnb.o: /usr/include/sys/_types/_fd_setsize.h
c_mdifnb.o: /usr/include/sys/_types/_fd_set.h
c_mdifnb.o: /usr/include/sys/_types/_fd_clr.h
c_mdifnb.o: /usr/include/sys/_types/_fd_zero.h
c_mdifnb.o: /usr/include/sys/_types/_fd_isset.h
c_mdifnb.o: /usr/include/sys/_types/_fd_copy.h
c_mdifnb.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdifnb.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdifnb.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdifnb.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdifnb.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdifnb.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdifnb.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdifnb.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdifnb.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdifnb.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdifnb.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdifnb.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdifnb.o: ../../../.././include/ncarg/c.h
c_mdilnb.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdilnb.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdilnb.o: /usr/include/sys/_posix_availability.h
c_mdilnb.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdilnb.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdilnb.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdilnb.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdilnb.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdilnb.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdilnb.o: /usr/include/sys/_types/_int16_t.h
c_mdilnb.o: /usr/include/sys/_types/_int32_t.h
c_mdilnb.o: /usr/include/sys/_types/_int64_t.h
c_mdilnb.o: /usr/include/sys/_types/_u_int8_t.h
c_mdilnb.o: /usr/include/sys/_types/_u_int16_t.h
c_mdilnb.o: /usr/include/sys/_types/_u_int32_t.h
c_mdilnb.o: /usr/include/sys/_types/_u_int64_t.h
c_mdilnb.o: /usr/include/sys/_types/_intptr_t.h
c_mdilnb.o: /usr/include/sys/_types/_uintptr_t.h
c_mdilnb.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdilnb.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdilnb.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdilnb.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdilnb.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdilnb.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdilnb.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdilnb.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdilnb.o: /usr/include/sys/_types/_u_char.h
c_mdilnb.o: /usr/include/sys/_types/_u_short.h
c_mdilnb.o: /usr/include/sys/_types/_u_int.h
c_mdilnb.o: /usr/include/sys/_types/_caddr_t.h
c_mdilnb.o: /usr/include/sys/_types/_dev_t.h
c_mdilnb.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdilnb.o: /usr/include/sys/_types/_blksize_t.h
c_mdilnb.o: /usr/include/sys/_types/_gid_t.h
c_mdilnb.o: /usr/include/sys/_types/_in_addr_t.h
c_mdilnb.o: /usr/include/sys/_types/_in_port_t.h
c_mdilnb.o: /usr/include/sys/_types/_ino_t.h
c_mdilnb.o: /usr/include/sys/_types/_ino64_t.h
c_mdilnb.o: /usr/include/sys/_types/_key_t.h
c_mdilnb.o: /usr/include/sys/_types/_mode_t.h
c_mdilnb.o: /usr/include/sys/_types/_nlink_t.h
c_mdilnb.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdilnb.o: /usr/include/sys/_types/_uid_t.h
c_mdilnb.o: /usr/include/sys/_types/_clock_t.h
c_mdilnb.o: /usr/include/sys/_types/_time_t.h
c_mdilnb.o: /usr/include/sys/_types/_useconds_t.h
c_mdilnb.o: /usr/include/sys/_types/_suseconds_t.h
c_mdilnb.o: /usr/include/sys/_types/_rsize_t.h
c_mdilnb.o: /usr/include/sys/_types/_errno_t.h
c_mdilnb.o: /usr/include/sys/_types/_fd_def.h
c_mdilnb.o: /usr/include/sys/_types/_fd_setsize.h
c_mdilnb.o: /usr/include/sys/_types/_fd_set.h
c_mdilnb.o: /usr/include/sys/_types/_fd_clr.h
c_mdilnb.o: /usr/include/sys/_types/_fd_zero.h
c_mdilnb.o: /usr/include/sys/_types/_fd_isset.h
c_mdilnb.o: /usr/include/sys/_types/_fd_copy.h
c_mdilnb.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdilnb.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdilnb.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdilnb.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdilnb.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdilnb.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdilnb.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdilnb.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdilnb.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdilnb.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdilnb.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdilnb.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdilnb.o: ../../../.././include/ncarg/c.h
c_mdiola.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdiola.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdiola.o: /usr/include/sys/_posix_availability.h
c_mdiola.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdiola.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdiola.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdiola.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdiola.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdiola.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdiola.o: /usr/include/sys/_types/_int16_t.h
c_mdiola.o: /usr/include/sys/_types/_int32_t.h
c_mdiola.o: /usr/include/sys/_types/_int64_t.h
c_mdiola.o: /usr/include/sys/_types/_u_int8_t.h
c_mdiola.o: /usr/include/sys/_types/_u_int16_t.h
c_mdiola.o: /usr/include/sys/_types/_u_int32_t.h
c_mdiola.o: /usr/include/sys/_types/_u_int64_t.h
c_mdiola.o: /usr/include/sys/_types/_intptr_t.h
c_mdiola.o: /usr/include/sys/_types/_uintptr_t.h
c_mdiola.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdiola.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdiola.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdiola.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdiola.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdiola.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdiola.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdiola.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdiola.o: /usr/include/sys/_types/_u_char.h
c_mdiola.o: /usr/include/sys/_types/_u_short.h
c_mdiola.o: /usr/include/sys/_types/_u_int.h
c_mdiola.o: /usr/include/sys/_types/_caddr_t.h
c_mdiola.o: /usr/include/sys/_types/_dev_t.h
c_mdiola.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdiola.o: /usr/include/sys/_types/_blksize_t.h
c_mdiola.o: /usr/include/sys/_types/_gid_t.h
c_mdiola.o: /usr/include/sys/_types/_in_addr_t.h
c_mdiola.o: /usr/include/sys/_types/_in_port_t.h
c_mdiola.o: /usr/include/sys/_types/_ino_t.h
c_mdiola.o: /usr/include/sys/_types/_ino64_t.h
c_mdiola.o: /usr/include/sys/_types/_key_t.h
c_mdiola.o: /usr/include/sys/_types/_mode_t.h
c_mdiola.o: /usr/include/sys/_types/_nlink_t.h
c_mdiola.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdiola.o: /usr/include/sys/_types/_uid_t.h
c_mdiola.o: /usr/include/sys/_types/_clock_t.h
c_mdiola.o: /usr/include/sys/_types/_time_t.h
c_mdiola.o: /usr/include/sys/_types/_useconds_t.h
c_mdiola.o: /usr/include/sys/_types/_suseconds_t.h
c_mdiola.o: /usr/include/sys/_types/_rsize_t.h
c_mdiola.o: /usr/include/sys/_types/_errno_t.h
c_mdiola.o: /usr/include/sys/_types/_fd_def.h
c_mdiola.o: /usr/include/sys/_types/_fd_setsize.h
c_mdiola.o: /usr/include/sys/_types/_fd_set.h
c_mdiola.o: /usr/include/sys/_types/_fd_clr.h
c_mdiola.o: /usr/include/sys/_types/_fd_zero.h
c_mdiola.o: /usr/include/sys/_types/_fd_isset.h
c_mdiola.o: /usr/include/sys/_types/_fd_copy.h
c_mdiola.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdiola.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdiola.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdiola.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdiola.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdiola.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdiola.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdiola.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdiola.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdiola.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdiola.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdiola.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdiola.o: ../../../.././include/ncarg/c.h
c_mdiosa.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdiosa.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdiosa.o: /usr/include/sys/_posix_availability.h
c_mdiosa.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdiosa.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdiosa.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdiosa.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdiosa.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdiosa.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdiosa.o: /usr/include/sys/_types/_int16_t.h
c_mdiosa.o: /usr/include/sys/_types/_int32_t.h
c_mdiosa.o: /usr/include/sys/_types/_int64_t.h
c_mdiosa.o: /usr/include/sys/_types/_u_int8_t.h
c_mdiosa.o: /usr/include/sys/_types/_u_int16_t.h
c_mdiosa.o: /usr/include/sys/_types/_u_int32_t.h
c_mdiosa.o: /usr/include/sys/_types/_u_int64_t.h
c_mdiosa.o: /usr/include/sys/_types/_intptr_t.h
c_mdiosa.o: /usr/include/sys/_types/_uintptr_t.h
c_mdiosa.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdiosa.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdiosa.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdiosa.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdiosa.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdiosa.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdiosa.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdiosa.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdiosa.o: /usr/include/sys/_types/_u_char.h
c_mdiosa.o: /usr/include/sys/_types/_u_short.h
c_mdiosa.o: /usr/include/sys/_types/_u_int.h
c_mdiosa.o: /usr/include/sys/_types/_caddr_t.h
c_mdiosa.o: /usr/include/sys/_types/_dev_t.h
c_mdiosa.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdiosa.o: /usr/include/sys/_types/_blksize_t.h
c_mdiosa.o: /usr/include/sys/_types/_gid_t.h
c_mdiosa.o: /usr/include/sys/_types/_in_addr_t.h
c_mdiosa.o: /usr/include/sys/_types/_in_port_t.h
c_mdiosa.o: /usr/include/sys/_types/_ino_t.h
c_mdiosa.o: /usr/include/sys/_types/_ino64_t.h
c_mdiosa.o: /usr/include/sys/_types/_key_t.h
c_mdiosa.o: /usr/include/sys/_types/_mode_t.h
c_mdiosa.o: /usr/include/sys/_types/_nlink_t.h
c_mdiosa.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdiosa.o: /usr/include/sys/_types/_uid_t.h
c_mdiosa.o: /usr/include/sys/_types/_clock_t.h
c_mdiosa.o: /usr/include/sys/_types/_time_t.h
c_mdiosa.o: /usr/include/sys/_types/_useconds_t.h
c_mdiosa.o: /usr/include/sys/_types/_suseconds_t.h
c_mdiosa.o: /usr/include/sys/_types/_rsize_t.h
c_mdiosa.o: /usr/include/sys/_types/_errno_t.h
c_mdiosa.o: /usr/include/sys/_types/_fd_def.h
c_mdiosa.o: /usr/include/sys/_types/_fd_setsize.h
c_mdiosa.o: /usr/include/sys/_types/_fd_set.h
c_mdiosa.o: /usr/include/sys/_types/_fd_clr.h
c_mdiosa.o: /usr/include/sys/_types/_fd_zero.h
c_mdiosa.o: /usr/include/sys/_types/_fd_isset.h
c_mdiosa.o: /usr/include/sys/_types/_fd_copy.h
c_mdiosa.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdiosa.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdiosa.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdiosa.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdiosa.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdiosa.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdiosa.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdiosa.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdiosa.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdiosa.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdiosa.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdiosa.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdiosa.o: ../../../.././include/ncarg/c.h
c_mdipai.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdipai.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdipai.o: /usr/include/sys/_posix_availability.h
c_mdipai.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdipai.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdipai.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdipai.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdipai.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdipai.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdipai.o: /usr/include/sys/_types/_int16_t.h
c_mdipai.o: /usr/include/sys/_types/_int32_t.h
c_mdipai.o: /usr/include/sys/_types/_int64_t.h
c_mdipai.o: /usr/include/sys/_types/_u_int8_t.h
c_mdipai.o: /usr/include/sys/_types/_u_int16_t.h
c_mdipai.o: /usr/include/sys/_types/_u_int32_t.h
c_mdipai.o: /usr/include/sys/_types/_u_int64_t.h
c_mdipai.o: /usr/include/sys/_types/_intptr_t.h
c_mdipai.o: /usr/include/sys/_types/_uintptr_t.h
c_mdipai.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdipai.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdipai.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdipai.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdipai.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdipai.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdipai.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdipai.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdipai.o: /usr/include/sys/_types/_u_char.h
c_mdipai.o: /usr/include/sys/_types/_u_short.h
c_mdipai.o: /usr/include/sys/_types/_u_int.h
c_mdipai.o: /usr/include/sys/_types/_caddr_t.h
c_mdipai.o: /usr/include/sys/_types/_dev_t.h
c_mdipai.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdipai.o: /usr/include/sys/_types/_blksize_t.h
c_mdipai.o: /usr/include/sys/_types/_gid_t.h
c_mdipai.o: /usr/include/sys/_types/_in_addr_t.h
c_mdipai.o: /usr/include/sys/_types/_in_port_t.h
c_mdipai.o: /usr/include/sys/_types/_ino_t.h
c_mdipai.o: /usr/include/sys/_types/_ino64_t.h
c_mdipai.o: /usr/include/sys/_types/_key_t.h
c_mdipai.o: /usr/include/sys/_types/_mode_t.h
c_mdipai.o: /usr/include/sys/_types/_nlink_t.h
c_mdipai.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdipai.o: /usr/include/sys/_types/_uid_t.h
c_mdipai.o: /usr/include/sys/_types/_clock_t.h
c_mdipai.o: /usr/include/sys/_types/_time_t.h
c_mdipai.o: /usr/include/sys/_types/_useconds_t.h
c_mdipai.o: /usr/include/sys/_types/_suseconds_t.h
c_mdipai.o: /usr/include/sys/_types/_rsize_t.h
c_mdipai.o: /usr/include/sys/_types/_errno_t.h
c_mdipai.o: /usr/include/sys/_types/_fd_def.h
c_mdipai.o: /usr/include/sys/_types/_fd_setsize.h
c_mdipai.o: /usr/include/sys/_types/_fd_set.h
c_mdipai.o: /usr/include/sys/_types/_fd_clr.h
c_mdipai.o: /usr/include/sys/_types/_fd_zero.h
c_mdipai.o: /usr/include/sys/_types/_fd_isset.h
c_mdipai.o: /usr/include/sys/_types/_fd_copy.h
c_mdipai.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdipai.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdipai.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdipai.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdipai.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdipai.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdipai.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdipai.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdipai.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdipai.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdipai.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdipai.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdipai.o: ../../../.././include/ncarg/c.h
c_mdipan.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdipan.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdipan.o: /usr/include/sys/_posix_availability.h
c_mdipan.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdipan.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdipan.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdipan.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdipan.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdipan.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdipan.o: /usr/include/sys/_types/_int16_t.h
c_mdipan.o: /usr/include/sys/_types/_int32_t.h
c_mdipan.o: /usr/include/sys/_types/_int64_t.h
c_mdipan.o: /usr/include/sys/_types/_u_int8_t.h
c_mdipan.o: /usr/include/sys/_types/_u_int16_t.h
c_mdipan.o: /usr/include/sys/_types/_u_int32_t.h
c_mdipan.o: /usr/include/sys/_types/_u_int64_t.h
c_mdipan.o: /usr/include/sys/_types/_intptr_t.h
c_mdipan.o: /usr/include/sys/_types/_uintptr_t.h
c_mdipan.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdipan.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdipan.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdipan.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdipan.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdipan.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdipan.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdipan.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdipan.o: /usr/include/sys/_types/_u_char.h
c_mdipan.o: /usr/include/sys/_types/_u_short.h
c_mdipan.o: /usr/include/sys/_types/_u_int.h
c_mdipan.o: /usr/include/sys/_types/_caddr_t.h
c_mdipan.o: /usr/include/sys/_types/_dev_t.h
c_mdipan.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdipan.o: /usr/include/sys/_types/_blksize_t.h
c_mdipan.o: /usr/include/sys/_types/_gid_t.h
c_mdipan.o: /usr/include/sys/_types/_in_addr_t.h
c_mdipan.o: /usr/include/sys/_types/_in_port_t.h
c_mdipan.o: /usr/include/sys/_types/_ino_t.h
c_mdipan.o: /usr/include/sys/_types/_ino64_t.h
c_mdipan.o: /usr/include/sys/_types/_key_t.h
c_mdipan.o: /usr/include/sys/_types/_mode_t.h
c_mdipan.o: /usr/include/sys/_types/_nlink_t.h
c_mdipan.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdipan.o: /usr/include/sys/_types/_uid_t.h
c_mdipan.o: /usr/include/sys/_types/_clock_t.h
c_mdipan.o: /usr/include/sys/_types/_time_t.h
c_mdipan.o: /usr/include/sys/_types/_useconds_t.h
c_mdipan.o: /usr/include/sys/_types/_suseconds_t.h
c_mdipan.o: /usr/include/sys/_types/_rsize_t.h
c_mdipan.o: /usr/include/sys/_types/_errno_t.h
c_mdipan.o: /usr/include/sys/_types/_fd_def.h
c_mdipan.o: /usr/include/sys/_types/_fd_setsize.h
c_mdipan.o: /usr/include/sys/_types/_fd_set.h
c_mdipan.o: /usr/include/sys/_types/_fd_clr.h
c_mdipan.o: /usr/include/sys/_types/_fd_zero.h
c_mdipan.o: /usr/include/sys/_types/_fd_isset.h
c_mdipan.o: /usr/include/sys/_types/_fd_copy.h
c_mdipan.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdipan.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdipan.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdipan.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdipan.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdipan.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdipan.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdipan.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdipan.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdipan.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdipan.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdipan.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdipan.o: ../../../.././include/ncarg/c.h
c_mdipar.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdipar.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdipar.o: /usr/include/sys/_posix_availability.h
c_mdipar.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdipar.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdipar.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdipar.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdipar.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdipar.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdipar.o: /usr/include/sys/_types/_int16_t.h
c_mdipar.o: /usr/include/sys/_types/_int32_t.h
c_mdipar.o: /usr/include/sys/_types/_int64_t.h
c_mdipar.o: /usr/include/sys/_types/_u_int8_t.h
c_mdipar.o: /usr/include/sys/_types/_u_int16_t.h
c_mdipar.o: /usr/include/sys/_types/_u_int32_t.h
c_mdipar.o: /usr/include/sys/_types/_u_int64_t.h
c_mdipar.o: /usr/include/sys/_types/_intptr_t.h
c_mdipar.o: /usr/include/sys/_types/_uintptr_t.h
c_mdipar.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdipar.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdipar.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdipar.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdipar.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdipar.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdipar.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdipar.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdipar.o: /usr/include/sys/_types/_u_char.h
c_mdipar.o: /usr/include/sys/_types/_u_short.h
c_mdipar.o: /usr/include/sys/_types/_u_int.h
c_mdipar.o: /usr/include/sys/_types/_caddr_t.h
c_mdipar.o: /usr/include/sys/_types/_dev_t.h
c_mdipar.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdipar.o: /usr/include/sys/_types/_blksize_t.h
c_mdipar.o: /usr/include/sys/_types/_gid_t.h
c_mdipar.o: /usr/include/sys/_types/_in_addr_t.h
c_mdipar.o: /usr/include/sys/_types/_in_port_t.h
c_mdipar.o: /usr/include/sys/_types/_ino_t.h
c_mdipar.o: /usr/include/sys/_types/_ino64_t.h
c_mdipar.o: /usr/include/sys/_types/_key_t.h
c_mdipar.o: /usr/include/sys/_types/_mode_t.h
c_mdipar.o: /usr/include/sys/_types/_nlink_t.h
c_mdipar.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdipar.o: /usr/include/sys/_types/_uid_t.h
c_mdipar.o: /usr/include/sys/_types/_clock_t.h
c_mdipar.o: /usr/include/sys/_types/_time_t.h
c_mdipar.o: /usr/include/sys/_types/_useconds_t.h
c_mdipar.o: /usr/include/sys/_types/_suseconds_t.h
c_mdipar.o: /usr/include/sys/_types/_rsize_t.h
c_mdipar.o: /usr/include/sys/_types/_errno_t.h
c_mdipar.o: /usr/include/sys/_types/_fd_def.h
c_mdipar.o: /usr/include/sys/_types/_fd_setsize.h
c_mdipar.o: /usr/include/sys/_types/_fd_set.h
c_mdipar.o: /usr/include/sys/_types/_fd_clr.h
c_mdipar.o: /usr/include/sys/_types/_fd_zero.h
c_mdipar.o: /usr/include/sys/_types/_fd_isset.h
c_mdipar.o: /usr/include/sys/_types/_fd_copy.h
c_mdipar.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdipar.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdipar.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdipar.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdipar.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdipar.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdipar.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdipar.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdipar.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdipar.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdipar.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdipar.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdipar.o: ../../../.././include/ncarg/c.h
c_mdisci.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdisci.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdisci.o: /usr/include/sys/_posix_availability.h
c_mdisci.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdisci.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdisci.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdisci.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdisci.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdisci.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdisci.o: /usr/include/sys/_types/_int16_t.h
c_mdisci.o: /usr/include/sys/_types/_int32_t.h
c_mdisci.o: /usr/include/sys/_types/_int64_t.h
c_mdisci.o: /usr/include/sys/_types/_u_int8_t.h
c_mdisci.o: /usr/include/sys/_types/_u_int16_t.h
c_mdisci.o: /usr/include/sys/_types/_u_int32_t.h
c_mdisci.o: /usr/include/sys/_types/_u_int64_t.h
c_mdisci.o: /usr/include/sys/_types/_intptr_t.h
c_mdisci.o: /usr/include/sys/_types/_uintptr_t.h
c_mdisci.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdisci.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdisci.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdisci.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdisci.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdisci.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdisci.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdisci.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdisci.o: /usr/include/sys/_types/_u_char.h
c_mdisci.o: /usr/include/sys/_types/_u_short.h
c_mdisci.o: /usr/include/sys/_types/_u_int.h
c_mdisci.o: /usr/include/sys/_types/_caddr_t.h
c_mdisci.o: /usr/include/sys/_types/_dev_t.h
c_mdisci.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdisci.o: /usr/include/sys/_types/_blksize_t.h
c_mdisci.o: /usr/include/sys/_types/_gid_t.h
c_mdisci.o: /usr/include/sys/_types/_in_addr_t.h
c_mdisci.o: /usr/include/sys/_types/_in_port_t.h
c_mdisci.o: /usr/include/sys/_types/_ino_t.h
c_mdisci.o: /usr/include/sys/_types/_ino64_t.h
c_mdisci.o: /usr/include/sys/_types/_key_t.h
c_mdisci.o: /usr/include/sys/_types/_mode_t.h
c_mdisci.o: /usr/include/sys/_types/_nlink_t.h
c_mdisci.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdisci.o: /usr/include/sys/_types/_uid_t.h
c_mdisci.o: /usr/include/sys/_types/_clock_t.h
c_mdisci.o: /usr/include/sys/_types/_time_t.h
c_mdisci.o: /usr/include/sys/_types/_useconds_t.h
c_mdisci.o: /usr/include/sys/_types/_suseconds_t.h
c_mdisci.o: /usr/include/sys/_types/_rsize_t.h
c_mdisci.o: /usr/include/sys/_types/_errno_t.h
c_mdisci.o: /usr/include/sys/_types/_fd_def.h
c_mdisci.o: /usr/include/sys/_types/_fd_setsize.h
c_mdisci.o: /usr/include/sys/_types/_fd_set.h
c_mdisci.o: /usr/include/sys/_types/_fd_clr.h
c_mdisci.o: /usr/include/sys/_types/_fd_zero.h
c_mdisci.o: /usr/include/sys/_types/_fd_isset.h
c_mdisci.o: /usr/include/sys/_types/_fd_copy.h
c_mdisci.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdisci.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdisci.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdisci.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdisci.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdisci.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdisci.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdisci.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdisci.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdisci.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdisci.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdisci.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdisci.o: ../../../.././include/ncarg/c.h
c_mdlnam.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdlnam.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdlnam.o: /usr/include/sys/_posix_availability.h
c_mdlnam.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdlnam.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdlnam.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdlnam.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdlnam.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdlnam.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdlnam.o: /usr/include/sys/_types/_int16_t.h
c_mdlnam.o: /usr/include/sys/_types/_int32_t.h
c_mdlnam.o: /usr/include/sys/_types/_int64_t.h
c_mdlnam.o: /usr/include/sys/_types/_u_int8_t.h
c_mdlnam.o: /usr/include/sys/_types/_u_int16_t.h
c_mdlnam.o: /usr/include/sys/_types/_u_int32_t.h
c_mdlnam.o: /usr/include/sys/_types/_u_int64_t.h
c_mdlnam.o: /usr/include/sys/_types/_intptr_t.h
c_mdlnam.o: /usr/include/sys/_types/_uintptr_t.h
c_mdlnam.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdlnam.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdlnam.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdlnam.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdlnam.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdlnam.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdlnam.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdlnam.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdlnam.o: /usr/include/sys/_types/_u_char.h
c_mdlnam.o: /usr/include/sys/_types/_u_short.h
c_mdlnam.o: /usr/include/sys/_types/_u_int.h
c_mdlnam.o: /usr/include/sys/_types/_caddr_t.h
c_mdlnam.o: /usr/include/sys/_types/_dev_t.h
c_mdlnam.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdlnam.o: /usr/include/sys/_types/_blksize_t.h
c_mdlnam.o: /usr/include/sys/_types/_gid_t.h
c_mdlnam.o: /usr/include/sys/_types/_in_addr_t.h
c_mdlnam.o: /usr/include/sys/_types/_in_port_t.h
c_mdlnam.o: /usr/include/sys/_types/_ino_t.h
c_mdlnam.o: /usr/include/sys/_types/_ino64_t.h
c_mdlnam.o: /usr/include/sys/_types/_key_t.h
c_mdlnam.o: /usr/include/sys/_types/_mode_t.h
c_mdlnam.o: /usr/include/sys/_types/_nlink_t.h
c_mdlnam.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdlnam.o: /usr/include/sys/_types/_uid_t.h
c_mdlnam.o: /usr/include/sys/_types/_clock_t.h
c_mdlnam.o: /usr/include/sys/_types/_time_t.h
c_mdlnam.o: /usr/include/sys/_types/_useconds_t.h
c_mdlnam.o: /usr/include/sys/_types/_suseconds_t.h
c_mdlnam.o: /usr/include/sys/_types/_rsize_t.h
c_mdlnam.o: /usr/include/sys/_types/_errno_t.h
c_mdlnam.o: /usr/include/sys/_types/_fd_def.h
c_mdlnam.o: /usr/include/sys/_types/_fd_setsize.h
c_mdlnam.o: /usr/include/sys/_types/_fd_set.h
c_mdlnam.o: /usr/include/sys/_types/_fd_clr.h
c_mdlnam.o: /usr/include/sys/_types/_fd_zero.h
c_mdlnam.o: /usr/include/sys/_types/_fd_isset.h
c_mdlnam.o: /usr/include/sys/_types/_fd_copy.h
c_mdlnam.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdlnam.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdlnam.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdlnam.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdlnam.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdlnam.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdlnam.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdlnam.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdlnam.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdlnam.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdlnam.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdlnam.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdlnam.o: ../../../.././include/ncarg/c.h
c_mdlndm.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdlndm.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdlndm.o: /usr/include/sys/_posix_availability.h
c_mdlndm.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdlndm.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdlndm.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdlndm.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdlndm.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdlndm.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdlndm.o: /usr/include/sys/_types/_int16_t.h
c_mdlndm.o: /usr/include/sys/_types/_int32_t.h
c_mdlndm.o: /usr/include/sys/_types/_int64_t.h
c_mdlndm.o: /usr/include/sys/_types/_u_int8_t.h
c_mdlndm.o: /usr/include/sys/_types/_u_int16_t.h
c_mdlndm.o: /usr/include/sys/_types/_u_int32_t.h
c_mdlndm.o: /usr/include/sys/_types/_u_int64_t.h
c_mdlndm.o: /usr/include/sys/_types/_intptr_t.h
c_mdlndm.o: /usr/include/sys/_types/_uintptr_t.h
c_mdlndm.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdlndm.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdlndm.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdlndm.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdlndm.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdlndm.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdlndm.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdlndm.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdlndm.o: /usr/include/sys/_types/_u_char.h
c_mdlndm.o: /usr/include/sys/_types/_u_short.h
c_mdlndm.o: /usr/include/sys/_types/_u_int.h
c_mdlndm.o: /usr/include/sys/_types/_caddr_t.h
c_mdlndm.o: /usr/include/sys/_types/_dev_t.h
c_mdlndm.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdlndm.o: /usr/include/sys/_types/_blksize_t.h
c_mdlndm.o: /usr/include/sys/_types/_gid_t.h
c_mdlndm.o: /usr/include/sys/_types/_in_addr_t.h
c_mdlndm.o: /usr/include/sys/_types/_in_port_t.h
c_mdlndm.o: /usr/include/sys/_types/_ino_t.h
c_mdlndm.o: /usr/include/sys/_types/_ino64_t.h
c_mdlndm.o: /usr/include/sys/_types/_key_t.h
c_mdlndm.o: /usr/include/sys/_types/_mode_t.h
c_mdlndm.o: /usr/include/sys/_types/_nlink_t.h
c_mdlndm.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdlndm.o: /usr/include/sys/_types/_uid_t.h
c_mdlndm.o: /usr/include/sys/_types/_clock_t.h
c_mdlndm.o: /usr/include/sys/_types/_time_t.h
c_mdlndm.o: /usr/include/sys/_types/_useconds_t.h
c_mdlndm.o: /usr/include/sys/_types/_suseconds_t.h
c_mdlndm.o: /usr/include/sys/_types/_rsize_t.h
c_mdlndm.o: /usr/include/sys/_types/_errno_t.h
c_mdlndm.o: /usr/include/sys/_types/_fd_def.h
c_mdlndm.o: /usr/include/sys/_types/_fd_setsize.h
c_mdlndm.o: /usr/include/sys/_types/_fd_set.h
c_mdlndm.o: /usr/include/sys/_types/_fd_clr.h
c_mdlndm.o: /usr/include/sys/_types/_fd_zero.h
c_mdlndm.o: /usr/include/sys/_types/_fd_isset.h
c_mdlndm.o: /usr/include/sys/_types/_fd_copy.h
c_mdlndm.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdlndm.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdlndm.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdlndm.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdlndm.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdlndm.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdlndm.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdlndm.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdlndm.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdlndm.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdlndm.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdlndm.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdlndm.o: ../../../.././include/ncarg/c.h
c_mdlndr.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdlndr.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdlndr.o: /usr/include/sys/_posix_availability.h
c_mdlndr.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdlndr.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdlndr.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdlndr.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdlndr.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdlndr.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdlndr.o: /usr/include/sys/_types/_int16_t.h
c_mdlndr.o: /usr/include/sys/_types/_int32_t.h
c_mdlndr.o: /usr/include/sys/_types/_int64_t.h
c_mdlndr.o: /usr/include/sys/_types/_u_int8_t.h
c_mdlndr.o: /usr/include/sys/_types/_u_int16_t.h
c_mdlndr.o: /usr/include/sys/_types/_u_int32_t.h
c_mdlndr.o: /usr/include/sys/_types/_u_int64_t.h
c_mdlndr.o: /usr/include/sys/_types/_intptr_t.h
c_mdlndr.o: /usr/include/sys/_types/_uintptr_t.h
c_mdlndr.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdlndr.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdlndr.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdlndr.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdlndr.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdlndr.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdlndr.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdlndr.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdlndr.o: /usr/include/sys/_types/_u_char.h
c_mdlndr.o: /usr/include/sys/_types/_u_short.h
c_mdlndr.o: /usr/include/sys/_types/_u_int.h
c_mdlndr.o: /usr/include/sys/_types/_caddr_t.h
c_mdlndr.o: /usr/include/sys/_types/_dev_t.h
c_mdlndr.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdlndr.o: /usr/include/sys/_types/_blksize_t.h
c_mdlndr.o: /usr/include/sys/_types/_gid_t.h
c_mdlndr.o: /usr/include/sys/_types/_in_addr_t.h
c_mdlndr.o: /usr/include/sys/_types/_in_port_t.h
c_mdlndr.o: /usr/include/sys/_types/_ino_t.h
c_mdlndr.o: /usr/include/sys/_types/_ino64_t.h
c_mdlndr.o: /usr/include/sys/_types/_key_t.h
c_mdlndr.o: /usr/include/sys/_types/_mode_t.h
c_mdlndr.o: /usr/include/sys/_types/_nlink_t.h
c_mdlndr.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdlndr.o: /usr/include/sys/_types/_uid_t.h
c_mdlndr.o: /usr/include/sys/_types/_clock_t.h
c_mdlndr.o: /usr/include/sys/_types/_time_t.h
c_mdlndr.o: /usr/include/sys/_types/_useconds_t.h
c_mdlndr.o: /usr/include/sys/_types/_suseconds_t.h
c_mdlndr.o: /usr/include/sys/_types/_rsize_t.h
c_mdlndr.o: /usr/include/sys/_types/_errno_t.h
c_mdlndr.o: /usr/include/sys/_types/_fd_def.h
c_mdlndr.o: /usr/include/sys/_types/_fd_setsize.h
c_mdlndr.o: /usr/include/sys/_types/_fd_set.h
c_mdlndr.o: /usr/include/sys/_types/_fd_clr.h
c_mdlndr.o: /usr/include/sys/_types/_fd_zero.h
c_mdlndr.o: /usr/include/sys/_types/_fd_isset.h
c_mdlndr.o: /usr/include/sys/_types/_fd_copy.h
c_mdlndr.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdlndr.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdlndr.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdlndr.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdlndr.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdlndr.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdlndr.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdlndr.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdlndr.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdlndr.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdlndr.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdlndr.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdlndr.o: ../../../.././include/ncarg/c.h
c_mdlnri.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdlnri.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdlnri.o: /usr/include/sys/_posix_availability.h
c_mdlnri.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdlnri.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdlnri.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdlnri.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdlnri.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdlnri.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdlnri.o: /usr/include/sys/_types/_int16_t.h
c_mdlnri.o: /usr/include/sys/_types/_int32_t.h
c_mdlnri.o: /usr/include/sys/_types/_int64_t.h
c_mdlnri.o: /usr/include/sys/_types/_u_int8_t.h
c_mdlnri.o: /usr/include/sys/_types/_u_int16_t.h
c_mdlnri.o: /usr/include/sys/_types/_u_int32_t.h
c_mdlnri.o: /usr/include/sys/_types/_u_int64_t.h
c_mdlnri.o: /usr/include/sys/_types/_intptr_t.h
c_mdlnri.o: /usr/include/sys/_types/_uintptr_t.h
c_mdlnri.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdlnri.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdlnri.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdlnri.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdlnri.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdlnri.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdlnri.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdlnri.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdlnri.o: /usr/include/sys/_types/_u_char.h
c_mdlnri.o: /usr/include/sys/_types/_u_short.h
c_mdlnri.o: /usr/include/sys/_types/_u_int.h
c_mdlnri.o: /usr/include/sys/_types/_caddr_t.h
c_mdlnri.o: /usr/include/sys/_types/_dev_t.h
c_mdlnri.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdlnri.o: /usr/include/sys/_types/_blksize_t.h
c_mdlnri.o: /usr/include/sys/_types/_gid_t.h
c_mdlnri.o: /usr/include/sys/_types/_in_addr_t.h
c_mdlnri.o: /usr/include/sys/_types/_in_port_t.h
c_mdlnri.o: /usr/include/sys/_types/_ino_t.h
c_mdlnri.o: /usr/include/sys/_types/_ino64_t.h
c_mdlnri.o: /usr/include/sys/_types/_key_t.h
c_mdlnri.o: /usr/include/sys/_types/_mode_t.h
c_mdlnri.o: /usr/include/sys/_types/_nlink_t.h
c_mdlnri.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdlnri.o: /usr/include/sys/_types/_uid_t.h
c_mdlnri.o: /usr/include/sys/_types/_clock_t.h
c_mdlnri.o: /usr/include/sys/_types/_time_t.h
c_mdlnri.o: /usr/include/sys/_types/_useconds_t.h
c_mdlnri.o: /usr/include/sys/_types/_suseconds_t.h
c_mdlnri.o: /usr/include/sys/_types/_rsize_t.h
c_mdlnri.o: /usr/include/sys/_types/_errno_t.h
c_mdlnri.o: /usr/include/sys/_types/_fd_def.h
c_mdlnri.o: /usr/include/sys/_types/_fd_setsize.h
c_mdlnri.o: /usr/include/sys/_types/_fd_set.h
c_mdlnri.o: /usr/include/sys/_types/_fd_clr.h
c_mdlnri.o: /usr/include/sys/_types/_fd_zero.h
c_mdlnri.o: /usr/include/sys/_types/_fd_isset.h
c_mdlnri.o: /usr/include/sys/_types/_fd_copy.h
c_mdlnri.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdlnri.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdlnri.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdlnri.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdlnri.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdlnri.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdlnri.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdlnri.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdlnri.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdlnri.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdlnri.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdlnri.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdlnri.o: ../../../.././include/ncarg/c.h
c_mdname.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdname.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdname.o: /usr/include/sys/_posix_availability.h
c_mdname.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdname.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdname.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdname.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdname.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdname.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdname.o: /usr/include/sys/_types/_int16_t.h
c_mdname.o: /usr/include/sys/_types/_int32_t.h
c_mdname.o: /usr/include/sys/_types/_int64_t.h
c_mdname.o: /usr/include/sys/_types/_u_int8_t.h
c_mdname.o: /usr/include/sys/_types/_u_int16_t.h
c_mdname.o: /usr/include/sys/_types/_u_int32_t.h
c_mdname.o: /usr/include/sys/_types/_u_int64_t.h
c_mdname.o: /usr/include/sys/_types/_intptr_t.h
c_mdname.o: /usr/include/sys/_types/_uintptr_t.h
c_mdname.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdname.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdname.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdname.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdname.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdname.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdname.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdname.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdname.o: /usr/include/sys/_types/_u_char.h
c_mdname.o: /usr/include/sys/_types/_u_short.h
c_mdname.o: /usr/include/sys/_types/_u_int.h
c_mdname.o: /usr/include/sys/_types/_caddr_t.h
c_mdname.o: /usr/include/sys/_types/_dev_t.h
c_mdname.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdname.o: /usr/include/sys/_types/_blksize_t.h
c_mdname.o: /usr/include/sys/_types/_gid_t.h
c_mdname.o: /usr/include/sys/_types/_in_addr_t.h
c_mdname.o: /usr/include/sys/_types/_in_port_t.h
c_mdname.o: /usr/include/sys/_types/_ino_t.h
c_mdname.o: /usr/include/sys/_types/_ino64_t.h
c_mdname.o: /usr/include/sys/_types/_key_t.h
c_mdname.o: /usr/include/sys/_types/_mode_t.h
c_mdname.o: /usr/include/sys/_types/_nlink_t.h
c_mdname.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdname.o: /usr/include/sys/_types/_uid_t.h
c_mdname.o: /usr/include/sys/_types/_clock_t.h
c_mdname.o: /usr/include/sys/_types/_time_t.h
c_mdname.o: /usr/include/sys/_types/_useconds_t.h
c_mdname.o: /usr/include/sys/_types/_suseconds_t.h
c_mdname.o: /usr/include/sys/_types/_rsize_t.h
c_mdname.o: /usr/include/sys/_types/_errno_t.h
c_mdname.o: /usr/include/sys/_types/_fd_def.h
c_mdname.o: /usr/include/sys/_types/_fd_setsize.h
c_mdname.o: /usr/include/sys/_types/_fd_set.h
c_mdname.o: /usr/include/sys/_types/_fd_clr.h
c_mdname.o: /usr/include/sys/_types/_fd_zero.h
c_mdname.o: /usr/include/sys/_types/_fd_isset.h
c_mdname.o: /usr/include/sys/_types/_fd_copy.h
c_mdname.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdname.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdname.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdname.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdname.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdname.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdname.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdname.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdname.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdname.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdname.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdname.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdname.o: ../../../.././include/ncarg/c.h
c_mdpaci.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdpaci.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdpaci.o: /usr/include/sys/_posix_availability.h
c_mdpaci.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdpaci.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdpaci.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdpaci.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdpaci.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdpaci.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdpaci.o: /usr/include/sys/_types/_int16_t.h
c_mdpaci.o: /usr/include/sys/_types/_int32_t.h
c_mdpaci.o: /usr/include/sys/_types/_int64_t.h
c_mdpaci.o: /usr/include/sys/_types/_u_int8_t.h
c_mdpaci.o: /usr/include/sys/_types/_u_int16_t.h
c_mdpaci.o: /usr/include/sys/_types/_u_int32_t.h
c_mdpaci.o: /usr/include/sys/_types/_u_int64_t.h
c_mdpaci.o: /usr/include/sys/_types/_intptr_t.h
c_mdpaci.o: /usr/include/sys/_types/_uintptr_t.h
c_mdpaci.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdpaci.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdpaci.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdpaci.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdpaci.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdpaci.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdpaci.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdpaci.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdpaci.o: /usr/include/sys/_types/_u_char.h
c_mdpaci.o: /usr/include/sys/_types/_u_short.h
c_mdpaci.o: /usr/include/sys/_types/_u_int.h
c_mdpaci.o: /usr/include/sys/_types/_caddr_t.h
c_mdpaci.o: /usr/include/sys/_types/_dev_t.h
c_mdpaci.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdpaci.o: /usr/include/sys/_types/_blksize_t.h
c_mdpaci.o: /usr/include/sys/_types/_gid_t.h
c_mdpaci.o: /usr/include/sys/_types/_in_addr_t.h
c_mdpaci.o: /usr/include/sys/_types/_in_port_t.h
c_mdpaci.o: /usr/include/sys/_types/_ino_t.h
c_mdpaci.o: /usr/include/sys/_types/_ino64_t.h
c_mdpaci.o: /usr/include/sys/_types/_key_t.h
c_mdpaci.o: /usr/include/sys/_types/_mode_t.h
c_mdpaci.o: /usr/include/sys/_types/_nlink_t.h
c_mdpaci.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdpaci.o: /usr/include/sys/_types/_uid_t.h
c_mdpaci.o: /usr/include/sys/_types/_clock_t.h
c_mdpaci.o: /usr/include/sys/_types/_time_t.h
c_mdpaci.o: /usr/include/sys/_types/_useconds_t.h
c_mdpaci.o: /usr/include/sys/_types/_suseconds_t.h
c_mdpaci.o: /usr/include/sys/_types/_rsize_t.h
c_mdpaci.o: /usr/include/sys/_types/_errno_t.h
c_mdpaci.o: /usr/include/sys/_types/_fd_def.h
c_mdpaci.o: /usr/include/sys/_types/_fd_setsize.h
c_mdpaci.o: /usr/include/sys/_types/_fd_set.h
c_mdpaci.o: /usr/include/sys/_types/_fd_clr.h
c_mdpaci.o: /usr/include/sys/_types/_fd_zero.h
c_mdpaci.o: /usr/include/sys/_types/_fd_isset.h
c_mdpaci.o: /usr/include/sys/_types/_fd_copy.h
c_mdpaci.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdpaci.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdpaci.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdpaci.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdpaci.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdpaci.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdpaci.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdpaci.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdpaci.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdpaci.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdpaci.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdpaci.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdpaci.o: ../../../.././include/ncarg/c.h
c_mdpbla.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdpbla.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdpbla.o: /usr/include/sys/_posix_availability.h
c_mdpbla.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdpbla.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdpbla.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdpbla.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdpbla.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdpbla.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdpbla.o: /usr/include/sys/_types/_int16_t.h
c_mdpbla.o: /usr/include/sys/_types/_int32_t.h
c_mdpbla.o: /usr/include/sys/_types/_int64_t.h
c_mdpbla.o: /usr/include/sys/_types/_u_int8_t.h
c_mdpbla.o: /usr/include/sys/_types/_u_int16_t.h
c_mdpbla.o: /usr/include/sys/_types/_u_int32_t.h
c_mdpbla.o: /usr/include/sys/_types/_u_int64_t.h
c_mdpbla.o: /usr/include/sys/_types/_intptr_t.h
c_mdpbla.o: /usr/include/sys/_types/_uintptr_t.h
c_mdpbla.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdpbla.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdpbla.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdpbla.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdpbla.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdpbla.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdpbla.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdpbla.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdpbla.o: /usr/include/sys/_types/_u_char.h
c_mdpbla.o: /usr/include/sys/_types/_u_short.h
c_mdpbla.o: /usr/include/sys/_types/_u_int.h
c_mdpbla.o: /usr/include/sys/_types/_caddr_t.h
c_mdpbla.o: /usr/include/sys/_types/_dev_t.h
c_mdpbla.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdpbla.o: /usr/include/sys/_types/_blksize_t.h
c_mdpbla.o: /usr/include/sys/_types/_gid_t.h
c_mdpbla.o: /usr/include/sys/_types/_in_addr_t.h
c_mdpbla.o: /usr/include/sys/_types/_in_port_t.h
c_mdpbla.o: /usr/include/sys/_types/_ino_t.h
c_mdpbla.o: /usr/include/sys/_types/_ino64_t.h
c_mdpbla.o: /usr/include/sys/_types/_key_t.h
c_mdpbla.o: /usr/include/sys/_types/_mode_t.h
c_mdpbla.o: /usr/include/sys/_types/_nlink_t.h
c_mdpbla.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdpbla.o: /usr/include/sys/_types/_uid_t.h
c_mdpbla.o: /usr/include/sys/_types/_clock_t.h
c_mdpbla.o: /usr/include/sys/_types/_time_t.h
c_mdpbla.o: /usr/include/sys/_types/_useconds_t.h
c_mdpbla.o: /usr/include/sys/_types/_suseconds_t.h
c_mdpbla.o: /usr/include/sys/_types/_rsize_t.h
c_mdpbla.o: /usr/include/sys/_types/_errno_t.h
c_mdpbla.o: /usr/include/sys/_types/_fd_def.h
c_mdpbla.o: /usr/include/sys/_types/_fd_setsize.h
c_mdpbla.o: /usr/include/sys/_types/_fd_set.h
c_mdpbla.o: /usr/include/sys/_types/_fd_clr.h
c_mdpbla.o: /usr/include/sys/_types/_fd_zero.h
c_mdpbla.o: /usr/include/sys/_types/_fd_isset.h
c_mdpbla.o: /usr/include/sys/_types/_fd_copy.h
c_mdpbla.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdpbla.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdpbla.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdpbla.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdpbla.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdpbla.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdpbla.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdpbla.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdpbla.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdpbla.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdpbla.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdpbla.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdpbla.o: ../../../.././include/ncarg/c.h
c_mdpblm.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdpblm.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdpblm.o: /usr/include/sys/_posix_availability.h
c_mdpblm.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdpblm.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdpblm.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdpblm.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdpblm.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdpblm.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdpblm.o: /usr/include/sys/_types/_int16_t.h
c_mdpblm.o: /usr/include/sys/_types/_int32_t.h
c_mdpblm.o: /usr/include/sys/_types/_int64_t.h
c_mdpblm.o: /usr/include/sys/_types/_u_int8_t.h
c_mdpblm.o: /usr/include/sys/_types/_u_int16_t.h
c_mdpblm.o: /usr/include/sys/_types/_u_int32_t.h
c_mdpblm.o: /usr/include/sys/_types/_u_int64_t.h
c_mdpblm.o: /usr/include/sys/_types/_intptr_t.h
c_mdpblm.o: /usr/include/sys/_types/_uintptr_t.h
c_mdpblm.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdpblm.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdpblm.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdpblm.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdpblm.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdpblm.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdpblm.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdpblm.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdpblm.o: /usr/include/sys/_types/_u_char.h
c_mdpblm.o: /usr/include/sys/_types/_u_short.h
c_mdpblm.o: /usr/include/sys/_types/_u_int.h
c_mdpblm.o: /usr/include/sys/_types/_caddr_t.h
c_mdpblm.o: /usr/include/sys/_types/_dev_t.h
c_mdpblm.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdpblm.o: /usr/include/sys/_types/_blksize_t.h
c_mdpblm.o: /usr/include/sys/_types/_gid_t.h
c_mdpblm.o: /usr/include/sys/_types/_in_addr_t.h
c_mdpblm.o: /usr/include/sys/_types/_in_port_t.h
c_mdpblm.o: /usr/include/sys/_types/_ino_t.h
c_mdpblm.o: /usr/include/sys/_types/_ino64_t.h
c_mdpblm.o: /usr/include/sys/_types/_key_t.h
c_mdpblm.o: /usr/include/sys/_types/_mode_t.h
c_mdpblm.o: /usr/include/sys/_types/_nlink_t.h
c_mdpblm.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdpblm.o: /usr/include/sys/_types/_uid_t.h
c_mdpblm.o: /usr/include/sys/_types/_clock_t.h
c_mdpblm.o: /usr/include/sys/_types/_time_t.h
c_mdpblm.o: /usr/include/sys/_types/_useconds_t.h
c_mdpblm.o: /usr/include/sys/_types/_suseconds_t.h
c_mdpblm.o: /usr/include/sys/_types/_rsize_t.h
c_mdpblm.o: /usr/include/sys/_types/_errno_t.h
c_mdpblm.o: /usr/include/sys/_types/_fd_def.h
c_mdpblm.o: /usr/include/sys/_types/_fd_setsize.h
c_mdpblm.o: /usr/include/sys/_types/_fd_set.h
c_mdpblm.o: /usr/include/sys/_types/_fd_clr.h
c_mdpblm.o: /usr/include/sys/_types/_fd_zero.h
c_mdpblm.o: /usr/include/sys/_types/_fd_isset.h
c_mdpblm.o: /usr/include/sys/_types/_fd_copy.h
c_mdpblm.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdpblm.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdpblm.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdpblm.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdpblm.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdpblm.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdpblm.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdpblm.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdpblm.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdpblm.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdpblm.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdpblm.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdpblm.o: ../../../.././include/ncarg/c.h
c_mdpdrw.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdpdrw.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdpdrw.o: /usr/include/sys/_posix_availability.h
c_mdpdrw.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdpdrw.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdpdrw.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdpdrw.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdpdrw.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdpdrw.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdpdrw.o: /usr/include/sys/_types/_int16_t.h
c_mdpdrw.o: /usr/include/sys/_types/_int32_t.h
c_mdpdrw.o: /usr/include/sys/_types/_int64_t.h
c_mdpdrw.o: /usr/include/sys/_types/_u_int8_t.h
c_mdpdrw.o: /usr/include/sys/_types/_u_int16_t.h
c_mdpdrw.o: /usr/include/sys/_types/_u_int32_t.h
c_mdpdrw.o: /usr/include/sys/_types/_u_int64_t.h
c_mdpdrw.o: /usr/include/sys/_types/_intptr_t.h
c_mdpdrw.o: /usr/include/sys/_types/_uintptr_t.h
c_mdpdrw.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdpdrw.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdpdrw.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdpdrw.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdpdrw.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdpdrw.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdpdrw.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdpdrw.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdpdrw.o: /usr/include/sys/_types/_u_char.h
c_mdpdrw.o: /usr/include/sys/_types/_u_short.h
c_mdpdrw.o: /usr/include/sys/_types/_u_int.h
c_mdpdrw.o: /usr/include/sys/_types/_caddr_t.h
c_mdpdrw.o: /usr/include/sys/_types/_dev_t.h
c_mdpdrw.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdpdrw.o: /usr/include/sys/_types/_blksize_t.h
c_mdpdrw.o: /usr/include/sys/_types/_gid_t.h
c_mdpdrw.o: /usr/include/sys/_types/_in_addr_t.h
c_mdpdrw.o: /usr/include/sys/_types/_in_port_t.h
c_mdpdrw.o: /usr/include/sys/_types/_ino_t.h
c_mdpdrw.o: /usr/include/sys/_types/_ino64_t.h
c_mdpdrw.o: /usr/include/sys/_types/_key_t.h
c_mdpdrw.o: /usr/include/sys/_types/_mode_t.h
c_mdpdrw.o: /usr/include/sys/_types/_nlink_t.h
c_mdpdrw.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdpdrw.o: /usr/include/sys/_types/_uid_t.h
c_mdpdrw.o: /usr/include/sys/_types/_clock_t.h
c_mdpdrw.o: /usr/include/sys/_types/_time_t.h
c_mdpdrw.o: /usr/include/sys/_types/_useconds_t.h
c_mdpdrw.o: /usr/include/sys/_types/_suseconds_t.h
c_mdpdrw.o: /usr/include/sys/_types/_rsize_t.h
c_mdpdrw.o: /usr/include/sys/_types/_errno_t.h
c_mdpdrw.o: /usr/include/sys/_types/_fd_def.h
c_mdpdrw.o: /usr/include/sys/_types/_fd_setsize.h
c_mdpdrw.o: /usr/include/sys/_types/_fd_set.h
c_mdpdrw.o: /usr/include/sys/_types/_fd_clr.h
c_mdpdrw.o: /usr/include/sys/_types/_fd_zero.h
c_mdpdrw.o: /usr/include/sys/_types/_fd_isset.h
c_mdpdrw.o: /usr/include/sys/_types/_fd_copy.h
c_mdpdrw.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdpdrw.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdpdrw.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdpdrw.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdpdrw.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdpdrw.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdpdrw.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdpdrw.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdpdrw.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdpdrw.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdpdrw.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdpdrw.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdpdrw.o: ../../../.././include/ncarg/c.h
c_mdpgrd.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdpgrd.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdpgrd.o: /usr/include/sys/_posix_availability.h
c_mdpgrd.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdpgrd.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdpgrd.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdpgrd.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdpgrd.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdpgrd.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdpgrd.o: /usr/include/sys/_types/_int16_t.h
c_mdpgrd.o: /usr/include/sys/_types/_int32_t.h
c_mdpgrd.o: /usr/include/sys/_types/_int64_t.h
c_mdpgrd.o: /usr/include/sys/_types/_u_int8_t.h
c_mdpgrd.o: /usr/include/sys/_types/_u_int16_t.h
c_mdpgrd.o: /usr/include/sys/_types/_u_int32_t.h
c_mdpgrd.o: /usr/include/sys/_types/_u_int64_t.h
c_mdpgrd.o: /usr/include/sys/_types/_intptr_t.h
c_mdpgrd.o: /usr/include/sys/_types/_uintptr_t.h
c_mdpgrd.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdpgrd.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdpgrd.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdpgrd.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdpgrd.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdpgrd.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdpgrd.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdpgrd.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdpgrd.o: /usr/include/sys/_types/_u_char.h
c_mdpgrd.o: /usr/include/sys/_types/_u_short.h
c_mdpgrd.o: /usr/include/sys/_types/_u_int.h
c_mdpgrd.o: /usr/include/sys/_types/_caddr_t.h
c_mdpgrd.o: /usr/include/sys/_types/_dev_t.h
c_mdpgrd.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdpgrd.o: /usr/include/sys/_types/_blksize_t.h
c_mdpgrd.o: /usr/include/sys/_types/_gid_t.h
c_mdpgrd.o: /usr/include/sys/_types/_in_addr_t.h
c_mdpgrd.o: /usr/include/sys/_types/_in_port_t.h
c_mdpgrd.o: /usr/include/sys/_types/_ino_t.h
c_mdpgrd.o: /usr/include/sys/_types/_ino64_t.h
c_mdpgrd.o: /usr/include/sys/_types/_key_t.h
c_mdpgrd.o: /usr/include/sys/_types/_mode_t.h
c_mdpgrd.o: /usr/include/sys/_types/_nlink_t.h
c_mdpgrd.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdpgrd.o: /usr/include/sys/_types/_uid_t.h
c_mdpgrd.o: /usr/include/sys/_types/_clock_t.h
c_mdpgrd.o: /usr/include/sys/_types/_time_t.h
c_mdpgrd.o: /usr/include/sys/_types/_useconds_t.h
c_mdpgrd.o: /usr/include/sys/_types/_suseconds_t.h
c_mdpgrd.o: /usr/include/sys/_types/_rsize_t.h
c_mdpgrd.o: /usr/include/sys/_types/_errno_t.h
c_mdpgrd.o: /usr/include/sys/_types/_fd_def.h
c_mdpgrd.o: /usr/include/sys/_types/_fd_setsize.h
c_mdpgrd.o: /usr/include/sys/_types/_fd_set.h
c_mdpgrd.o: /usr/include/sys/_types/_fd_clr.h
c_mdpgrd.o: /usr/include/sys/_types/_fd_zero.h
c_mdpgrd.o: /usr/include/sys/_types/_fd_isset.h
c_mdpgrd.o: /usr/include/sys/_types/_fd_copy.h
c_mdpgrd.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdpgrd.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdpgrd.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdpgrd.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdpgrd.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdpgrd.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdpgrd.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdpgrd.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdpgrd.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdpgrd.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdpgrd.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdpgrd.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdpgrd.o: ../../../.././include/ncarg/c.h
c_mdpgrm.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdpgrm.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdpgrm.o: /usr/include/sys/_posix_availability.h
c_mdpgrm.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdpgrm.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdpgrm.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdpgrm.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdpgrm.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdpgrm.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdpgrm.o: /usr/include/sys/_types/_int16_t.h
c_mdpgrm.o: /usr/include/sys/_types/_int32_t.h
c_mdpgrm.o: /usr/include/sys/_types/_int64_t.h
c_mdpgrm.o: /usr/include/sys/_types/_u_int8_t.h
c_mdpgrm.o: /usr/include/sys/_types/_u_int16_t.h
c_mdpgrm.o: /usr/include/sys/_types/_u_int32_t.h
c_mdpgrm.o: /usr/include/sys/_types/_u_int64_t.h
c_mdpgrm.o: /usr/include/sys/_types/_intptr_t.h
c_mdpgrm.o: /usr/include/sys/_types/_uintptr_t.h
c_mdpgrm.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdpgrm.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdpgrm.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdpgrm.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdpgrm.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdpgrm.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdpgrm.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdpgrm.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdpgrm.o: /usr/include/sys/_types/_u_char.h
c_mdpgrm.o: /usr/include/sys/_types/_u_short.h
c_mdpgrm.o: /usr/include/sys/_types/_u_int.h
c_mdpgrm.o: /usr/include/sys/_types/_caddr_t.h
c_mdpgrm.o: /usr/include/sys/_types/_dev_t.h
c_mdpgrm.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdpgrm.o: /usr/include/sys/_types/_blksize_t.h
c_mdpgrm.o: /usr/include/sys/_types/_gid_t.h
c_mdpgrm.o: /usr/include/sys/_types/_in_addr_t.h
c_mdpgrm.o: /usr/include/sys/_types/_in_port_t.h
c_mdpgrm.o: /usr/include/sys/_types/_ino_t.h
c_mdpgrm.o: /usr/include/sys/_types/_ino64_t.h
c_mdpgrm.o: /usr/include/sys/_types/_key_t.h
c_mdpgrm.o: /usr/include/sys/_types/_mode_t.h
c_mdpgrm.o: /usr/include/sys/_types/_nlink_t.h
c_mdpgrm.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdpgrm.o: /usr/include/sys/_types/_uid_t.h
c_mdpgrm.o: /usr/include/sys/_types/_clock_t.h
c_mdpgrm.o: /usr/include/sys/_types/_time_t.h
c_mdpgrm.o: /usr/include/sys/_types/_useconds_t.h
c_mdpgrm.o: /usr/include/sys/_types/_suseconds_t.h
c_mdpgrm.o: /usr/include/sys/_types/_rsize_t.h
c_mdpgrm.o: /usr/include/sys/_types/_errno_t.h
c_mdpgrm.o: /usr/include/sys/_types/_fd_def.h
c_mdpgrm.o: /usr/include/sys/_types/_fd_setsize.h
c_mdpgrm.o: /usr/include/sys/_types/_fd_set.h
c_mdpgrm.o: /usr/include/sys/_types/_fd_clr.h
c_mdpgrm.o: /usr/include/sys/_types/_fd_zero.h
c_mdpgrm.o: /usr/include/sys/_types/_fd_isset.h
c_mdpgrm.o: /usr/include/sys/_types/_fd_copy.h
c_mdpgrm.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdpgrm.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdpgrm.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdpgrm.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdpgrm.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdpgrm.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdpgrm.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdpgrm.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdpgrm.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdpgrm.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdpgrm.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdpgrm.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdpgrm.o: ../../../.././include/ncarg/c.h
c_mdpint.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdpint.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdpint.o: /usr/include/sys/_posix_availability.h
c_mdpint.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdpint.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdpint.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdpint.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdpint.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdpint.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdpint.o: /usr/include/sys/_types/_int16_t.h
c_mdpint.o: /usr/include/sys/_types/_int32_t.h
c_mdpint.o: /usr/include/sys/_types/_int64_t.h
c_mdpint.o: /usr/include/sys/_types/_u_int8_t.h
c_mdpint.o: /usr/include/sys/_types/_u_int16_t.h
c_mdpint.o: /usr/include/sys/_types/_u_int32_t.h
c_mdpint.o: /usr/include/sys/_types/_u_int64_t.h
c_mdpint.o: /usr/include/sys/_types/_intptr_t.h
c_mdpint.o: /usr/include/sys/_types/_uintptr_t.h
c_mdpint.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdpint.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdpint.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdpint.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdpint.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdpint.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdpint.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdpint.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdpint.o: /usr/include/sys/_types/_u_char.h
c_mdpint.o: /usr/include/sys/_types/_u_short.h
c_mdpint.o: /usr/include/sys/_types/_u_int.h
c_mdpint.o: /usr/include/sys/_types/_caddr_t.h
c_mdpint.o: /usr/include/sys/_types/_dev_t.h
c_mdpint.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdpint.o: /usr/include/sys/_types/_blksize_t.h
c_mdpint.o: /usr/include/sys/_types/_gid_t.h
c_mdpint.o: /usr/include/sys/_types/_in_addr_t.h
c_mdpint.o: /usr/include/sys/_types/_in_port_t.h
c_mdpint.o: /usr/include/sys/_types/_ino_t.h
c_mdpint.o: /usr/include/sys/_types/_ino64_t.h
c_mdpint.o: /usr/include/sys/_types/_key_t.h
c_mdpint.o: /usr/include/sys/_types/_mode_t.h
c_mdpint.o: /usr/include/sys/_types/_nlink_t.h
c_mdpint.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdpint.o: /usr/include/sys/_types/_uid_t.h
c_mdpint.o: /usr/include/sys/_types/_clock_t.h
c_mdpint.o: /usr/include/sys/_types/_time_t.h
c_mdpint.o: /usr/include/sys/_types/_useconds_t.h
c_mdpint.o: /usr/include/sys/_types/_suseconds_t.h
c_mdpint.o: /usr/include/sys/_types/_rsize_t.h
c_mdpint.o: /usr/include/sys/_types/_errno_t.h
c_mdpint.o: /usr/include/sys/_types/_fd_def.h
c_mdpint.o: /usr/include/sys/_types/_fd_setsize.h
c_mdpint.o: /usr/include/sys/_types/_fd_set.h
c_mdpint.o: /usr/include/sys/_types/_fd_clr.h
c_mdpint.o: /usr/include/sys/_types/_fd_zero.h
c_mdpint.o: /usr/include/sys/_types/_fd_isset.h
c_mdpint.o: /usr/include/sys/_types/_fd_copy.h
c_mdpint.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdpint.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdpint.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdpint.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdpint.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdpint.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdpint.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdpint.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdpint.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdpint.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdpint.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdpint.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdpint.o: ../../../.././include/ncarg/c.h
c_mdplbl.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdplbl.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdplbl.o: /usr/include/sys/_posix_availability.h
c_mdplbl.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdplbl.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdplbl.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdplbl.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdplbl.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdplbl.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdplbl.o: /usr/include/sys/_types/_int16_t.h
c_mdplbl.o: /usr/include/sys/_types/_int32_t.h
c_mdplbl.o: /usr/include/sys/_types/_int64_t.h
c_mdplbl.o: /usr/include/sys/_types/_u_int8_t.h
c_mdplbl.o: /usr/include/sys/_types/_u_int16_t.h
c_mdplbl.o: /usr/include/sys/_types/_u_int32_t.h
c_mdplbl.o: /usr/include/sys/_types/_u_int64_t.h
c_mdplbl.o: /usr/include/sys/_types/_intptr_t.h
c_mdplbl.o: /usr/include/sys/_types/_uintptr_t.h
c_mdplbl.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdplbl.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdplbl.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdplbl.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdplbl.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdplbl.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdplbl.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdplbl.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdplbl.o: /usr/include/sys/_types/_u_char.h
c_mdplbl.o: /usr/include/sys/_types/_u_short.h
c_mdplbl.o: /usr/include/sys/_types/_u_int.h
c_mdplbl.o: /usr/include/sys/_types/_caddr_t.h
c_mdplbl.o: /usr/include/sys/_types/_dev_t.h
c_mdplbl.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdplbl.o: /usr/include/sys/_types/_blksize_t.h
c_mdplbl.o: /usr/include/sys/_types/_gid_t.h
c_mdplbl.o: /usr/include/sys/_types/_in_addr_t.h
c_mdplbl.o: /usr/include/sys/_types/_in_port_t.h
c_mdplbl.o: /usr/include/sys/_types/_ino_t.h
c_mdplbl.o: /usr/include/sys/_types/_ino64_t.h
c_mdplbl.o: /usr/include/sys/_types/_key_t.h
c_mdplbl.o: /usr/include/sys/_types/_mode_t.h
c_mdplbl.o: /usr/include/sys/_types/_nlink_t.h
c_mdplbl.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdplbl.o: /usr/include/sys/_types/_uid_t.h
c_mdplbl.o: /usr/include/sys/_types/_clock_t.h
c_mdplbl.o: /usr/include/sys/_types/_time_t.h
c_mdplbl.o: /usr/include/sys/_types/_useconds_t.h
c_mdplbl.o: /usr/include/sys/_types/_suseconds_t.h
c_mdplbl.o: /usr/include/sys/_types/_rsize_t.h
c_mdplbl.o: /usr/include/sys/_types/_errno_t.h
c_mdplbl.o: /usr/include/sys/_types/_fd_def.h
c_mdplbl.o: /usr/include/sys/_types/_fd_setsize.h
c_mdplbl.o: /usr/include/sys/_types/_fd_set.h
c_mdplbl.o: /usr/include/sys/_types/_fd_clr.h
c_mdplbl.o: /usr/include/sys/_types/_fd_zero.h
c_mdplbl.o: /usr/include/sys/_types/_fd_isset.h
c_mdplbl.o: /usr/include/sys/_types/_fd_copy.h
c_mdplbl.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdplbl.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdplbl.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdplbl.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdplbl.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdplbl.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdplbl.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdplbl.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdplbl.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdplbl.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdplbl.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdplbl.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdplbl.o: ../../../.././include/ncarg/c.h
c_mdplmb.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdplmb.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdplmb.o: /usr/include/sys/_posix_availability.h
c_mdplmb.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdplmb.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdplmb.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdplmb.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdplmb.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdplmb.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdplmb.o: /usr/include/sys/_types/_int16_t.h
c_mdplmb.o: /usr/include/sys/_types/_int32_t.h
c_mdplmb.o: /usr/include/sys/_types/_int64_t.h
c_mdplmb.o: /usr/include/sys/_types/_u_int8_t.h
c_mdplmb.o: /usr/include/sys/_types/_u_int16_t.h
c_mdplmb.o: /usr/include/sys/_types/_u_int32_t.h
c_mdplmb.o: /usr/include/sys/_types/_u_int64_t.h
c_mdplmb.o: /usr/include/sys/_types/_intptr_t.h
c_mdplmb.o: /usr/include/sys/_types/_uintptr_t.h
c_mdplmb.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdplmb.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdplmb.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdplmb.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdplmb.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdplmb.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdplmb.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdplmb.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdplmb.o: /usr/include/sys/_types/_u_char.h
c_mdplmb.o: /usr/include/sys/_types/_u_short.h
c_mdplmb.o: /usr/include/sys/_types/_u_int.h
c_mdplmb.o: /usr/include/sys/_types/_caddr_t.h
c_mdplmb.o: /usr/include/sys/_types/_dev_t.h
c_mdplmb.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdplmb.o: /usr/include/sys/_types/_blksize_t.h
c_mdplmb.o: /usr/include/sys/_types/_gid_t.h
c_mdplmb.o: /usr/include/sys/_types/_in_addr_t.h
c_mdplmb.o: /usr/include/sys/_types/_in_port_t.h
c_mdplmb.o: /usr/include/sys/_types/_ino_t.h
c_mdplmb.o: /usr/include/sys/_types/_ino64_t.h
c_mdplmb.o: /usr/include/sys/_types/_key_t.h
c_mdplmb.o: /usr/include/sys/_types/_mode_t.h
c_mdplmb.o: /usr/include/sys/_types/_nlink_t.h
c_mdplmb.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdplmb.o: /usr/include/sys/_types/_uid_t.h
c_mdplmb.o: /usr/include/sys/_types/_clock_t.h
c_mdplmb.o: /usr/include/sys/_types/_time_t.h
c_mdplmb.o: /usr/include/sys/_types/_useconds_t.h
c_mdplmb.o: /usr/include/sys/_types/_suseconds_t.h
c_mdplmb.o: /usr/include/sys/_types/_rsize_t.h
c_mdplmb.o: /usr/include/sys/_types/_errno_t.h
c_mdplmb.o: /usr/include/sys/_types/_fd_def.h
c_mdplmb.o: /usr/include/sys/_types/_fd_setsize.h
c_mdplmb.o: /usr/include/sys/_types/_fd_set.h
c_mdplmb.o: /usr/include/sys/_types/_fd_clr.h
c_mdplmb.o: /usr/include/sys/_types/_fd_zero.h
c_mdplmb.o: /usr/include/sys/_types/_fd_isset.h
c_mdplmb.o: /usr/include/sys/_types/_fd_copy.h
c_mdplmb.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdplmb.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdplmb.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdplmb.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdplmb.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdplmb.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdplmb.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdplmb.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdplmb.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdplmb.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdplmb.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdplmb.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdplmb.o: ../../../.././include/ncarg/c.h
c_mdplot.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdplot.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdplot.o: /usr/include/sys/_posix_availability.h
c_mdplot.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdplot.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdplot.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdplot.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdplot.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdplot.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdplot.o: /usr/include/sys/_types/_int16_t.h
c_mdplot.o: /usr/include/sys/_types/_int32_t.h
c_mdplot.o: /usr/include/sys/_types/_int64_t.h
c_mdplot.o: /usr/include/sys/_types/_u_int8_t.h
c_mdplot.o: /usr/include/sys/_types/_u_int16_t.h
c_mdplot.o: /usr/include/sys/_types/_u_int32_t.h
c_mdplot.o: /usr/include/sys/_types/_u_int64_t.h
c_mdplot.o: /usr/include/sys/_types/_intptr_t.h
c_mdplot.o: /usr/include/sys/_types/_uintptr_t.h
c_mdplot.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdplot.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdplot.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdplot.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdplot.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdplot.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdplot.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdplot.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdplot.o: /usr/include/sys/_types/_u_char.h
c_mdplot.o: /usr/include/sys/_types/_u_short.h
c_mdplot.o: /usr/include/sys/_types/_u_int.h
c_mdplot.o: /usr/include/sys/_types/_caddr_t.h
c_mdplot.o: /usr/include/sys/_types/_dev_t.h
c_mdplot.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdplot.o: /usr/include/sys/_types/_blksize_t.h
c_mdplot.o: /usr/include/sys/_types/_gid_t.h
c_mdplot.o: /usr/include/sys/_types/_in_addr_t.h
c_mdplot.o: /usr/include/sys/_types/_in_port_t.h
c_mdplot.o: /usr/include/sys/_types/_ino_t.h
c_mdplot.o: /usr/include/sys/_types/_ino64_t.h
c_mdplot.o: /usr/include/sys/_types/_key_t.h
c_mdplot.o: /usr/include/sys/_types/_mode_t.h
c_mdplot.o: /usr/include/sys/_types/_nlink_t.h
c_mdplot.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdplot.o: /usr/include/sys/_types/_uid_t.h
c_mdplot.o: /usr/include/sys/_types/_clock_t.h
c_mdplot.o: /usr/include/sys/_types/_time_t.h
c_mdplot.o: /usr/include/sys/_types/_useconds_t.h
c_mdplot.o: /usr/include/sys/_types/_suseconds_t.h
c_mdplot.o: /usr/include/sys/_types/_rsize_t.h
c_mdplot.o: /usr/include/sys/_types/_errno_t.h
c_mdplot.o: /usr/include/sys/_types/_fd_def.h
c_mdplot.o: /usr/include/sys/_types/_fd_setsize.h
c_mdplot.o: /usr/include/sys/_types/_fd_set.h
c_mdplot.o: /usr/include/sys/_types/_fd_clr.h
c_mdplot.o: /usr/include/sys/_types/_fd_zero.h
c_mdplot.o: /usr/include/sys/_types/_fd_isset.h
c_mdplot.o: /usr/include/sys/_types/_fd_copy.h
c_mdplot.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdplot.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdplot.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdplot.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdplot.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdplot.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdplot.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdplot.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdplot.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdplot.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdplot.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdplot.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdplot.o: ../../../.././include/ncarg/c.h
c_mdprs.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdprs.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdprs.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_mdprs.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_mdprs.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_mdprs.o: /usr/include/i386/_types.h
c_mdprs.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdprs.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdprs.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdprs.o: /usr/include/sys/_types/_int16_t.h
c_mdprs.o: /usr/include/sys/_types/_int32_t.h
c_mdprs.o: /usr/include/sys/_types/_int64_t.h
c_mdprs.o: /usr/include/sys/_types/_u_int8_t.h
c_mdprs.o: /usr/include/sys/_types/_u_int16_t.h
c_mdprs.o: /usr/include/sys/_types/_u_int32_t.h
c_mdprs.o: /usr/include/sys/_types/_u_int64_t.h
c_mdprs.o: /usr/include/sys/_types/_intptr_t.h
c_mdprs.o: /usr/include/sys/_types/_uintptr_t.h
c_mdprs.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdprs.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdprs.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdprs.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdprs.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdprs.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdprs.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdprs.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdprs.o: /usr/include/sys/_types/_u_char.h
c_mdprs.o: /usr/include/sys/_types/_u_short.h
c_mdprs.o: /usr/include/sys/_types/_u_int.h
c_mdprs.o: /usr/include/sys/_types/_caddr_t.h
c_mdprs.o: /usr/include/sys/_types/_dev_t.h
c_mdprs.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdprs.o: /usr/include/sys/_types/_blksize_t.h
c_mdprs.o: /usr/include/sys/_types/_gid_t.h
c_mdprs.o: /usr/include/sys/_types/_in_addr_t.h
c_mdprs.o: /usr/include/sys/_types/_in_port_t.h
c_mdprs.o: /usr/include/sys/_types/_ino_t.h
c_mdprs.o: /usr/include/sys/_types/_ino64_t.h
c_mdprs.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_mdprs.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_mdprs.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_mdprs.o: /usr/include/sys/_types/_clock_t.h
c_mdprs.o: /usr/include/sys/_types/_time_t.h
c_mdprs.o: /usr/include/sys/_types/_useconds_t.h
c_mdprs.o: /usr/include/sys/_types/_suseconds_t.h
c_mdprs.o: /usr/include/sys/_types/_rsize_t.h
c_mdprs.o: /usr/include/sys/_types/_errno_t.h
c_mdprs.o: /usr/include/sys/_types/_fd_def.h
c_mdprs.o: /usr/include/sys/_types/_fd_setsize.h
c_mdprs.o: /usr/include/sys/_types/_fd_set.h
c_mdprs.o: /usr/include/sys/_types/_fd_clr.h
c_mdprs.o: /usr/include/sys/_types/_fd_zero.h
c_mdprs.o: /usr/include/sys/_types/_fd_isset.h
c_mdprs.o: /usr/include/sys/_types/_fd_copy.h
c_mdprs.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdprs.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdprs.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdprs.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdprs.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdprs.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdprs.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdprs.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdprs.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdprs.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdprs.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdprs.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdprs.o: ../../../.././include/ncarg/c.h
c_mdprst.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdprst.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdprst.o: /usr/include/sys/_posix_availability.h
c_mdprst.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdprst.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdprst.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdprst.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdprst.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdprst.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdprst.o: /usr/include/sys/_types/_int16_t.h
c_mdprst.o: /usr/include/sys/_types/_int32_t.h
c_mdprst.o: /usr/include/sys/_types/_int64_t.h
c_mdprst.o: /usr/include/sys/_types/_u_int8_t.h
c_mdprst.o: /usr/include/sys/_types/_u_int16_t.h
c_mdprst.o: /usr/include/sys/_types/_u_int32_t.h
c_mdprst.o: /usr/include/sys/_types/_u_int64_t.h
c_mdprst.o: /usr/include/sys/_types/_intptr_t.h
c_mdprst.o: /usr/include/sys/_types/_uintptr_t.h
c_mdprst.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdprst.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdprst.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdprst.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdprst.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdprst.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdprst.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdprst.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdprst.o: /usr/include/sys/_types/_u_char.h
c_mdprst.o: /usr/include/sys/_types/_u_short.h
c_mdprst.o: /usr/include/sys/_types/_u_int.h
c_mdprst.o: /usr/include/sys/_types/_caddr_t.h
c_mdprst.o: /usr/include/sys/_types/_dev_t.h
c_mdprst.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdprst.o: /usr/include/sys/_types/_blksize_t.h
c_mdprst.o: /usr/include/sys/_types/_gid_t.h
c_mdprst.o: /usr/include/sys/_types/_in_addr_t.h
c_mdprst.o: /usr/include/sys/_types/_in_port_t.h
c_mdprst.o: /usr/include/sys/_types/_ino_t.h
c_mdprst.o: /usr/include/sys/_types/_ino64_t.h
c_mdprst.o: /usr/include/sys/_types/_key_t.h
c_mdprst.o: /usr/include/sys/_types/_mode_t.h
c_mdprst.o: /usr/include/sys/_types/_nlink_t.h
c_mdprst.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdprst.o: /usr/include/sys/_types/_uid_t.h
c_mdprst.o: /usr/include/sys/_types/_clock_t.h
c_mdprst.o: /usr/include/sys/_types/_time_t.h
c_mdprst.o: /usr/include/sys/_types/_useconds_t.h
c_mdprst.o: /usr/include/sys/_types/_suseconds_t.h
c_mdprst.o: /usr/include/sys/_types/_rsize_t.h
c_mdprst.o: /usr/include/sys/_types/_errno_t.h
c_mdprst.o: /usr/include/sys/_types/_fd_def.h
c_mdprst.o: /usr/include/sys/_types/_fd_setsize.h
c_mdprst.o: /usr/include/sys/_types/_fd_set.h
c_mdprst.o: /usr/include/sys/_types/_fd_clr.h
c_mdprst.o: /usr/include/sys/_types/_fd_zero.h
c_mdprst.o: /usr/include/sys/_types/_fd_isset.h
c_mdprst.o: /usr/include/sys/_types/_fd_copy.h
c_mdprst.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdprst.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdprst.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdprst.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdprst.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdprst.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdprst.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdprst.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdprst.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdprst.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdprst.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdprst.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdprst.o: ../../../.././include/ncarg/c.h
c_mdpsav.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdpsav.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdpsav.o: /usr/include/sys/_posix_availability.h
c_mdpsav.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdpsav.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdpsav.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdpsav.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdpsav.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdpsav.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdpsav.o: /usr/include/sys/_types/_int16_t.h
c_mdpsav.o: /usr/include/sys/_types/_int32_t.h
c_mdpsav.o: /usr/include/sys/_types/_int64_t.h
c_mdpsav.o: /usr/include/sys/_types/_u_int8_t.h
c_mdpsav.o: /usr/include/sys/_types/_u_int16_t.h
c_mdpsav.o: /usr/include/sys/_types/_u_int32_t.h
c_mdpsav.o: /usr/include/sys/_types/_u_int64_t.h
c_mdpsav.o: /usr/include/sys/_types/_intptr_t.h
c_mdpsav.o: /usr/include/sys/_types/_uintptr_t.h
c_mdpsav.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdpsav.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdpsav.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdpsav.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdpsav.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdpsav.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdpsav.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdpsav.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdpsav.o: /usr/include/sys/_types/_u_char.h
c_mdpsav.o: /usr/include/sys/_types/_u_short.h
c_mdpsav.o: /usr/include/sys/_types/_u_int.h
c_mdpsav.o: /usr/include/sys/_types/_caddr_t.h
c_mdpsav.o: /usr/include/sys/_types/_dev_t.h
c_mdpsav.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdpsav.o: /usr/include/sys/_types/_blksize_t.h
c_mdpsav.o: /usr/include/sys/_types/_gid_t.h
c_mdpsav.o: /usr/include/sys/_types/_in_addr_t.h
c_mdpsav.o: /usr/include/sys/_types/_in_port_t.h
c_mdpsav.o: /usr/include/sys/_types/_ino_t.h
c_mdpsav.o: /usr/include/sys/_types/_ino64_t.h
c_mdpsav.o: /usr/include/sys/_types/_key_t.h
c_mdpsav.o: /usr/include/sys/_types/_mode_t.h
c_mdpsav.o: /usr/include/sys/_types/_nlink_t.h
c_mdpsav.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdpsav.o: /usr/include/sys/_types/_uid_t.h
c_mdpsav.o: /usr/include/sys/_types/_clock_t.h
c_mdpsav.o: /usr/include/sys/_types/_time_t.h
c_mdpsav.o: /usr/include/sys/_types/_useconds_t.h
c_mdpsav.o: /usr/include/sys/_types/_suseconds_t.h
c_mdpsav.o: /usr/include/sys/_types/_rsize_t.h
c_mdpsav.o: /usr/include/sys/_types/_errno_t.h
c_mdpsav.o: /usr/include/sys/_types/_fd_def.h
c_mdpsav.o: /usr/include/sys/_types/_fd_setsize.h
c_mdpsav.o: /usr/include/sys/_types/_fd_set.h
c_mdpsav.o: /usr/include/sys/_types/_fd_clr.h
c_mdpsav.o: /usr/include/sys/_types/_fd_zero.h
c_mdpsav.o: /usr/include/sys/_types/_fd_isset.h
c_mdpsav.o: /usr/include/sys/_types/_fd_copy.h
c_mdpsav.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdpsav.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdpsav.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdpsav.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdpsav.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdpsav.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdpsav.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdpsav.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdpsav.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdpsav.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdpsav.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdpsav.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdpsav.o: ../../../.././include/ncarg/c.h
c_mdrset.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdrset.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdrset.o: /usr/include/sys/_posix_availability.h
c_mdrset.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdrset.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdrset.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdrset.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdrset.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdrset.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdrset.o: /usr/include/sys/_types/_int16_t.h
c_mdrset.o: /usr/include/sys/_types/_int32_t.h
c_mdrset.o: /usr/include/sys/_types/_int64_t.h
c_mdrset.o: /usr/include/sys/_types/_u_int8_t.h
c_mdrset.o: /usr/include/sys/_types/_u_int16_t.h
c_mdrset.o: /usr/include/sys/_types/_u_int32_t.h
c_mdrset.o: /usr/include/sys/_types/_u_int64_t.h
c_mdrset.o: /usr/include/sys/_types/_intptr_t.h
c_mdrset.o: /usr/include/sys/_types/_uintptr_t.h
c_mdrset.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdrset.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdrset.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdrset.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdrset.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdrset.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdrset.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdrset.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdrset.o: /usr/include/sys/_types/_u_char.h
c_mdrset.o: /usr/include/sys/_types/_u_short.h
c_mdrset.o: /usr/include/sys/_types/_u_int.h
c_mdrset.o: /usr/include/sys/_types/_caddr_t.h
c_mdrset.o: /usr/include/sys/_types/_dev_t.h
c_mdrset.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdrset.o: /usr/include/sys/_types/_blksize_t.h
c_mdrset.o: /usr/include/sys/_types/_gid_t.h
c_mdrset.o: /usr/include/sys/_types/_in_addr_t.h
c_mdrset.o: /usr/include/sys/_types/_in_port_t.h
c_mdrset.o: /usr/include/sys/_types/_ino_t.h
c_mdrset.o: /usr/include/sys/_types/_ino64_t.h
c_mdrset.o: /usr/include/sys/_types/_key_t.h
c_mdrset.o: /usr/include/sys/_types/_mode_t.h
c_mdrset.o: /usr/include/sys/_types/_nlink_t.h
c_mdrset.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdrset.o: /usr/include/sys/_types/_uid_t.h
c_mdrset.o: /usr/include/sys/_types/_clock_t.h
c_mdrset.o: /usr/include/sys/_types/_time_t.h
c_mdrset.o: /usr/include/sys/_types/_useconds_t.h
c_mdrset.o: /usr/include/sys/_types/_suseconds_t.h
c_mdrset.o: /usr/include/sys/_types/_rsize_t.h
c_mdrset.o: /usr/include/sys/_types/_errno_t.h
c_mdrset.o: /usr/include/sys/_types/_fd_def.h
c_mdrset.o: /usr/include/sys/_types/_fd_setsize.h
c_mdrset.o: /usr/include/sys/_types/_fd_set.h
c_mdrset.o: /usr/include/sys/_types/_fd_clr.h
c_mdrset.o: /usr/include/sys/_types/_fd_zero.h
c_mdrset.o: /usr/include/sys/_types/_fd_isset.h
c_mdrset.o: /usr/include/sys/_types/_fd_copy.h
c_mdrset.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdrset.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdrset.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdrset.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdrset.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdrset.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdrset.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdrset.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdrset.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdrset.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdrset.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdrset.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdrset.o: ../../../.././include/ncarg/c.h
c_mdpiq.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdpiq.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdpiq.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_mdpiq.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_mdpiq.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_mdpiq.o: /usr/include/i386/_types.h
c_mdpiq.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdpiq.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdpiq.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdpiq.o: /usr/include/sys/_types/_int16_t.h
c_mdpiq.o: /usr/include/sys/_types/_int32_t.h
c_mdpiq.o: /usr/include/sys/_types/_int64_t.h
c_mdpiq.o: /usr/include/sys/_types/_u_int8_t.h
c_mdpiq.o: /usr/include/sys/_types/_u_int16_t.h
c_mdpiq.o: /usr/include/sys/_types/_u_int32_t.h
c_mdpiq.o: /usr/include/sys/_types/_u_int64_t.h
c_mdpiq.o: /usr/include/sys/_types/_intptr_t.h
c_mdpiq.o: /usr/include/sys/_types/_uintptr_t.h
c_mdpiq.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdpiq.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdpiq.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdpiq.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdpiq.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdpiq.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdpiq.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdpiq.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdpiq.o: /usr/include/sys/_types/_u_char.h
c_mdpiq.o: /usr/include/sys/_types/_u_short.h
c_mdpiq.o: /usr/include/sys/_types/_u_int.h
c_mdpiq.o: /usr/include/sys/_types/_caddr_t.h
c_mdpiq.o: /usr/include/sys/_types/_dev_t.h
c_mdpiq.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdpiq.o: /usr/include/sys/_types/_blksize_t.h
c_mdpiq.o: /usr/include/sys/_types/_gid_t.h
c_mdpiq.o: /usr/include/sys/_types/_in_addr_t.h
c_mdpiq.o: /usr/include/sys/_types/_in_port_t.h
c_mdpiq.o: /usr/include/sys/_types/_ino_t.h
c_mdpiq.o: /usr/include/sys/_types/_ino64_t.h
c_mdpiq.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_mdpiq.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_mdpiq.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_mdpiq.o: /usr/include/sys/_types/_clock_t.h
c_mdpiq.o: /usr/include/sys/_types/_time_t.h
c_mdpiq.o: /usr/include/sys/_types/_useconds_t.h
c_mdpiq.o: /usr/include/sys/_types/_suseconds_t.h
c_mdpiq.o: /usr/include/sys/_types/_rsize_t.h
c_mdpiq.o: /usr/include/sys/_types/_errno_t.h
c_mdpiq.o: /usr/include/sys/_types/_fd_def.h
c_mdpiq.o: /usr/include/sys/_types/_fd_setsize.h
c_mdpiq.o: /usr/include/sys/_types/_fd_set.h
c_mdpiq.o: /usr/include/sys/_types/_fd_clr.h
c_mdpiq.o: /usr/include/sys/_types/_fd_zero.h
c_mdpiq.o: /usr/include/sys/_types/_fd_isset.h
c_mdpiq.o: /usr/include/sys/_types/_fd_copy.h
c_mdpiq.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdpiq.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdpiq.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdpiq.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdpiq.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdpiq.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdpiq.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdpiq.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdpiq.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdpiq.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdpiq.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdpiq.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdpiq.o: ../../../.././include/ncarg/c.h
c_mdpiqd.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdpiqd.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdpiqd.o: /usr/include/sys/_posix_availability.h
c_mdpiqd.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdpiqd.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdpiqd.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdpiqd.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdpiqd.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdpiqd.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdpiqd.o: /usr/include/sys/_types/_int16_t.h
c_mdpiqd.o: /usr/include/sys/_types/_int32_t.h
c_mdpiqd.o: /usr/include/sys/_types/_int64_t.h
c_mdpiqd.o: /usr/include/sys/_types/_u_int8_t.h
c_mdpiqd.o: /usr/include/sys/_types/_u_int16_t.h
c_mdpiqd.o: /usr/include/sys/_types/_u_int32_t.h
c_mdpiqd.o: /usr/include/sys/_types/_u_int64_t.h
c_mdpiqd.o: /usr/include/sys/_types/_intptr_t.h
c_mdpiqd.o: /usr/include/sys/_types/_uintptr_t.h
c_mdpiqd.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdpiqd.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdpiqd.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdpiqd.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdpiqd.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdpiqd.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdpiqd.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdpiqd.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdpiqd.o: /usr/include/sys/_types/_u_char.h
c_mdpiqd.o: /usr/include/sys/_types/_u_short.h
c_mdpiqd.o: /usr/include/sys/_types/_u_int.h
c_mdpiqd.o: /usr/include/sys/_types/_caddr_t.h
c_mdpiqd.o: /usr/include/sys/_types/_dev_t.h
c_mdpiqd.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdpiqd.o: /usr/include/sys/_types/_blksize_t.h
c_mdpiqd.o: /usr/include/sys/_types/_gid_t.h
c_mdpiqd.o: /usr/include/sys/_types/_in_addr_t.h
c_mdpiqd.o: /usr/include/sys/_types/_in_port_t.h
c_mdpiqd.o: /usr/include/sys/_types/_ino_t.h
c_mdpiqd.o: /usr/include/sys/_types/_ino64_t.h
c_mdpiqd.o: /usr/include/sys/_types/_key_t.h
c_mdpiqd.o: /usr/include/sys/_types/_mode_t.h
c_mdpiqd.o: /usr/include/sys/_types/_nlink_t.h
c_mdpiqd.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdpiqd.o: /usr/include/sys/_types/_uid_t.h
c_mdpiqd.o: /usr/include/sys/_types/_clock_t.h
c_mdpiqd.o: /usr/include/sys/_types/_time_t.h
c_mdpiqd.o: /usr/include/sys/_types/_useconds_t.h
c_mdpiqd.o: /usr/include/sys/_types/_suseconds_t.h
c_mdpiqd.o: /usr/include/sys/_types/_rsize_t.h
c_mdpiqd.o: /usr/include/sys/_types/_errno_t.h
c_mdpiqd.o: /usr/include/sys/_types/_fd_def.h
c_mdpiqd.o: /usr/include/sys/_types/_fd_setsize.h
c_mdpiqd.o: /usr/include/sys/_types/_fd_set.h
c_mdpiqd.o: /usr/include/sys/_types/_fd_clr.h
c_mdpiqd.o: /usr/include/sys/_types/_fd_zero.h
c_mdpiqd.o: /usr/include/sys/_types/_fd_isset.h
c_mdpiqd.o: /usr/include/sys/_types/_fd_copy.h
c_mdpiqd.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdpiqd.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdpiqd.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdpiqd.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdpiqd.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdpiqd.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdpiqd.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdpiqd.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdpiqd.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdpiqd.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdpiqd.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdpiqd.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdpiqd.o: ../../../.././include/ncarg/c.h
c_mdrgol.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdrgol.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdrgol.o: /usr/include/sys/_posix_availability.h
c_mdrgol.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdrgol.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdrgol.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdrgol.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdrgol.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdrgol.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdrgol.o: /usr/include/sys/_types/_int16_t.h
c_mdrgol.o: /usr/include/sys/_types/_int32_t.h
c_mdrgol.o: /usr/include/sys/_types/_int64_t.h
c_mdrgol.o: /usr/include/sys/_types/_u_int8_t.h
c_mdrgol.o: /usr/include/sys/_types/_u_int16_t.h
c_mdrgol.o: /usr/include/sys/_types/_u_int32_t.h
c_mdrgol.o: /usr/include/sys/_types/_u_int64_t.h
c_mdrgol.o: /usr/include/sys/_types/_intptr_t.h
c_mdrgol.o: /usr/include/sys/_types/_uintptr_t.h
c_mdrgol.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdrgol.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdrgol.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdrgol.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdrgol.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdrgol.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdrgol.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdrgol.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdrgol.o: /usr/include/sys/_types/_u_char.h
c_mdrgol.o: /usr/include/sys/_types/_u_short.h
c_mdrgol.o: /usr/include/sys/_types/_u_int.h
c_mdrgol.o: /usr/include/sys/_types/_caddr_t.h
c_mdrgol.o: /usr/include/sys/_types/_dev_t.h
c_mdrgol.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdrgol.o: /usr/include/sys/_types/_blksize_t.h
c_mdrgol.o: /usr/include/sys/_types/_gid_t.h
c_mdrgol.o: /usr/include/sys/_types/_in_addr_t.h
c_mdrgol.o: /usr/include/sys/_types/_in_port_t.h
c_mdrgol.o: /usr/include/sys/_types/_ino_t.h
c_mdrgol.o: /usr/include/sys/_types/_ino64_t.h
c_mdrgol.o: /usr/include/sys/_types/_key_t.h
c_mdrgol.o: /usr/include/sys/_types/_mode_t.h
c_mdrgol.o: /usr/include/sys/_types/_nlink_t.h
c_mdrgol.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdrgol.o: /usr/include/sys/_types/_uid_t.h
c_mdrgol.o: /usr/include/sys/_types/_clock_t.h
c_mdrgol.o: /usr/include/sys/_types/_time_t.h
c_mdrgol.o: /usr/include/sys/_types/_useconds_t.h
c_mdrgol.o: /usr/include/sys/_types/_suseconds_t.h
c_mdrgol.o: /usr/include/sys/_types/_rsize_t.h
c_mdrgol.o: /usr/include/sys/_types/_errno_t.h
c_mdrgol.o: /usr/include/sys/_types/_fd_def.h
c_mdrgol.o: /usr/include/sys/_types/_fd_setsize.h
c_mdrgol.o: /usr/include/sys/_types/_fd_set.h
c_mdrgol.o: /usr/include/sys/_types/_fd_clr.h
c_mdrgol.o: /usr/include/sys/_types/_fd_zero.h
c_mdrgol.o: /usr/include/sys/_types/_fd_isset.h
c_mdrgol.o: /usr/include/sys/_types/_fd_copy.h
c_mdrgol.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdrgol.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdrgol.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdrgol.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdrgol.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdrgol.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdrgol.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdrgol.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdrgol.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdrgol.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdrgol.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdrgol.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdrgol.o: ../../../.././include/ncarg/c.h
c_mdrgsf.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdrgsf.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdrgsf.o: /usr/include/sys/_posix_availability.h
c_mdrgsf.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdrgsf.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdrgsf.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdrgsf.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdrgsf.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdrgsf.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdrgsf.o: /usr/include/sys/_types/_int16_t.h
c_mdrgsf.o: /usr/include/sys/_types/_int32_t.h
c_mdrgsf.o: /usr/include/sys/_types/_int64_t.h
c_mdrgsf.o: /usr/include/sys/_types/_u_int8_t.h
c_mdrgsf.o: /usr/include/sys/_types/_u_int16_t.h
c_mdrgsf.o: /usr/include/sys/_types/_u_int32_t.h
c_mdrgsf.o: /usr/include/sys/_types/_u_int64_t.h
c_mdrgsf.o: /usr/include/sys/_types/_intptr_t.h
c_mdrgsf.o: /usr/include/sys/_types/_uintptr_t.h
c_mdrgsf.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdrgsf.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdrgsf.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdrgsf.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdrgsf.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdrgsf.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdrgsf.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdrgsf.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdrgsf.o: /usr/include/sys/_types/_u_char.h
c_mdrgsf.o: /usr/include/sys/_types/_u_short.h
c_mdrgsf.o: /usr/include/sys/_types/_u_int.h
c_mdrgsf.o: /usr/include/sys/_types/_caddr_t.h
c_mdrgsf.o: /usr/include/sys/_types/_dev_t.h
c_mdrgsf.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdrgsf.o: /usr/include/sys/_types/_blksize_t.h
c_mdrgsf.o: /usr/include/sys/_types/_gid_t.h
c_mdrgsf.o: /usr/include/sys/_types/_in_addr_t.h
c_mdrgsf.o: /usr/include/sys/_types/_in_port_t.h
c_mdrgsf.o: /usr/include/sys/_types/_ino_t.h
c_mdrgsf.o: /usr/include/sys/_types/_ino64_t.h
c_mdrgsf.o: /usr/include/sys/_types/_key_t.h
c_mdrgsf.o: /usr/include/sys/_types/_mode_t.h
c_mdrgsf.o: /usr/include/sys/_types/_nlink_t.h
c_mdrgsf.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdrgsf.o: /usr/include/sys/_types/_uid_t.h
c_mdrgsf.o: /usr/include/sys/_types/_clock_t.h
c_mdrgsf.o: /usr/include/sys/_types/_time_t.h
c_mdrgsf.o: /usr/include/sys/_types/_useconds_t.h
c_mdrgsf.o: /usr/include/sys/_types/_suseconds_t.h
c_mdrgsf.o: /usr/include/sys/_types/_rsize_t.h
c_mdrgsf.o: /usr/include/sys/_types/_errno_t.h
c_mdrgsf.o: /usr/include/sys/_types/_fd_def.h
c_mdrgsf.o: /usr/include/sys/_types/_fd_setsize.h
c_mdrgsf.o: /usr/include/sys/_types/_fd_set.h
c_mdrgsf.o: /usr/include/sys/_types/_fd_clr.h
c_mdrgsf.o: /usr/include/sys/_types/_fd_zero.h
c_mdrgsf.o: /usr/include/sys/_types/_fd_isset.h
c_mdrgsf.o: /usr/include/sys/_types/_fd_copy.h
c_mdrgsf.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdrgsf.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdrgsf.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdrgsf.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdrgsf.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdrgsf.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdrgsf.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdrgsf.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdrgsf.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdrgsf.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdrgsf.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdrgsf.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdrgsf.o: ../../../.././include/ncarg/c.h
c_mdpfst.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdpfst.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdpfst.o: /usr/include/sys/_posix_availability.h
c_mdpfst.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdpfst.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdpfst.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdpfst.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdpfst.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdpfst.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdpfst.o: /usr/include/sys/_types/_int16_t.h
c_mdpfst.o: /usr/include/sys/_types/_int32_t.h
c_mdpfst.o: /usr/include/sys/_types/_int64_t.h
c_mdpfst.o: /usr/include/sys/_types/_u_int8_t.h
c_mdpfst.o: /usr/include/sys/_types/_u_int16_t.h
c_mdpfst.o: /usr/include/sys/_types/_u_int32_t.h
c_mdpfst.o: /usr/include/sys/_types/_u_int64_t.h
c_mdpfst.o: /usr/include/sys/_types/_intptr_t.h
c_mdpfst.o: /usr/include/sys/_types/_uintptr_t.h
c_mdpfst.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdpfst.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdpfst.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdpfst.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdpfst.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdpfst.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdpfst.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdpfst.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdpfst.o: /usr/include/sys/_types/_u_char.h
c_mdpfst.o: /usr/include/sys/_types/_u_short.h
c_mdpfst.o: /usr/include/sys/_types/_u_int.h
c_mdpfst.o: /usr/include/sys/_types/_caddr_t.h
c_mdpfst.o: /usr/include/sys/_types/_dev_t.h
c_mdpfst.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdpfst.o: /usr/include/sys/_types/_blksize_t.h
c_mdpfst.o: /usr/include/sys/_types/_gid_t.h
c_mdpfst.o: /usr/include/sys/_types/_in_addr_t.h
c_mdpfst.o: /usr/include/sys/_types/_in_port_t.h
c_mdpfst.o: /usr/include/sys/_types/_ino_t.h
c_mdpfst.o: /usr/include/sys/_types/_ino64_t.h
c_mdpfst.o: /usr/include/sys/_types/_key_t.h
c_mdpfst.o: /usr/include/sys/_types/_mode_t.h
c_mdpfst.o: /usr/include/sys/_types/_nlink_t.h
c_mdpfst.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdpfst.o: /usr/include/sys/_types/_uid_t.h
c_mdpfst.o: /usr/include/sys/_types/_clock_t.h
c_mdpfst.o: /usr/include/sys/_types/_time_t.h
c_mdpfst.o: /usr/include/sys/_types/_useconds_t.h
c_mdpfst.o: /usr/include/sys/_types/_suseconds_t.h
c_mdpfst.o: /usr/include/sys/_types/_rsize_t.h
c_mdpfst.o: /usr/include/sys/_types/_errno_t.h
c_mdpfst.o: /usr/include/sys/_types/_fd_def.h
c_mdpfst.o: /usr/include/sys/_types/_fd_setsize.h
c_mdpfst.o: /usr/include/sys/_types/_fd_set.h
c_mdpfst.o: /usr/include/sys/_types/_fd_clr.h
c_mdpfst.o: /usr/include/sys/_types/_fd_zero.h
c_mdpfst.o: /usr/include/sys/_types/_fd_isset.h
c_mdpfst.o: /usr/include/sys/_types/_fd_copy.h
c_mdpfst.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdpfst.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdpfst.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdpfst.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdpfst.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdpfst.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdpfst.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdpfst.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdpfst.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdpfst.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdpfst.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdpfst.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdpfst.o: ../../../.././include/ncarg/c.h
c_mdpvec.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdpvec.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdpvec.o: /usr/include/sys/_posix_availability.h
c_mdpvec.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdpvec.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdpvec.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdpvec.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdpvec.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdpvec.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdpvec.o: /usr/include/sys/_types/_int16_t.h
c_mdpvec.o: /usr/include/sys/_types/_int32_t.h
c_mdpvec.o: /usr/include/sys/_types/_int64_t.h
c_mdpvec.o: /usr/include/sys/_types/_u_int8_t.h
c_mdpvec.o: /usr/include/sys/_types/_u_int16_t.h
c_mdpvec.o: /usr/include/sys/_types/_u_int32_t.h
c_mdpvec.o: /usr/include/sys/_types/_u_int64_t.h
c_mdpvec.o: /usr/include/sys/_types/_intptr_t.h
c_mdpvec.o: /usr/include/sys/_types/_uintptr_t.h
c_mdpvec.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdpvec.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdpvec.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdpvec.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdpvec.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdpvec.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdpvec.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdpvec.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdpvec.o: /usr/include/sys/_types/_u_char.h
c_mdpvec.o: /usr/include/sys/_types/_u_short.h
c_mdpvec.o: /usr/include/sys/_types/_u_int.h
c_mdpvec.o: /usr/include/sys/_types/_caddr_t.h
c_mdpvec.o: /usr/include/sys/_types/_dev_t.h
c_mdpvec.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdpvec.o: /usr/include/sys/_types/_blksize_t.h
c_mdpvec.o: /usr/include/sys/_types/_gid_t.h
c_mdpvec.o: /usr/include/sys/_types/_in_addr_t.h
c_mdpvec.o: /usr/include/sys/_types/_in_port_t.h
c_mdpvec.o: /usr/include/sys/_types/_ino_t.h
c_mdpvec.o: /usr/include/sys/_types/_ino64_t.h
c_mdpvec.o: /usr/include/sys/_types/_key_t.h
c_mdpvec.o: /usr/include/sys/_types/_mode_t.h
c_mdpvec.o: /usr/include/sys/_types/_nlink_t.h
c_mdpvec.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdpvec.o: /usr/include/sys/_types/_uid_t.h
c_mdpvec.o: /usr/include/sys/_types/_clock_t.h
c_mdpvec.o: /usr/include/sys/_types/_time_t.h
c_mdpvec.o: /usr/include/sys/_types/_useconds_t.h
c_mdpvec.o: /usr/include/sys/_types/_suseconds_t.h
c_mdpvec.o: /usr/include/sys/_types/_rsize_t.h
c_mdpvec.o: /usr/include/sys/_types/_errno_t.h
c_mdpvec.o: /usr/include/sys/_types/_fd_def.h
c_mdpvec.o: /usr/include/sys/_types/_fd_setsize.h
c_mdpvec.o: /usr/include/sys/_types/_fd_set.h
c_mdpvec.o: /usr/include/sys/_types/_fd_clr.h
c_mdpvec.o: /usr/include/sys/_types/_fd_zero.h
c_mdpvec.o: /usr/include/sys/_types/_fd_isset.h
c_mdpvec.o: /usr/include/sys/_types/_fd_copy.h
c_mdpvec.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdpvec.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdpvec.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdpvec.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdpvec.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdpvec.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdpvec.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdpvec.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdpvec.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdpvec.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdpvec.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdpvec.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdpvec.o: ../../../.././include/ncarg/c.h
c_mdpiqa.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdpiqa.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdpiqa.o: /usr/include/sys/_posix_availability.h
c_mdpiqa.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdpiqa.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdpiqa.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdpiqa.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdpiqa.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdpiqa.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdpiqa.o: /usr/include/sys/_types/_int16_t.h
c_mdpiqa.o: /usr/include/sys/_types/_int32_t.h
c_mdpiqa.o: /usr/include/sys/_types/_int64_t.h
c_mdpiqa.o: /usr/include/sys/_types/_u_int8_t.h
c_mdpiqa.o: /usr/include/sys/_types/_u_int16_t.h
c_mdpiqa.o: /usr/include/sys/_types/_u_int32_t.h
c_mdpiqa.o: /usr/include/sys/_types/_u_int64_t.h
c_mdpiqa.o: /usr/include/sys/_types/_intptr_t.h
c_mdpiqa.o: /usr/include/sys/_types/_uintptr_t.h
c_mdpiqa.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdpiqa.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdpiqa.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdpiqa.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdpiqa.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdpiqa.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdpiqa.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdpiqa.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdpiqa.o: /usr/include/sys/_types/_u_char.h
c_mdpiqa.o: /usr/include/sys/_types/_u_short.h
c_mdpiqa.o: /usr/include/sys/_types/_u_int.h
c_mdpiqa.o: /usr/include/sys/_types/_caddr_t.h
c_mdpiqa.o: /usr/include/sys/_types/_dev_t.h
c_mdpiqa.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdpiqa.o: /usr/include/sys/_types/_blksize_t.h
c_mdpiqa.o: /usr/include/sys/_types/_gid_t.h
c_mdpiqa.o: /usr/include/sys/_types/_in_addr_t.h
c_mdpiqa.o: /usr/include/sys/_types/_in_port_t.h
c_mdpiqa.o: /usr/include/sys/_types/_ino_t.h
c_mdpiqa.o: /usr/include/sys/_types/_ino64_t.h
c_mdpiqa.o: /usr/include/sys/_types/_key_t.h
c_mdpiqa.o: /usr/include/sys/_types/_mode_t.h
c_mdpiqa.o: /usr/include/sys/_types/_nlink_t.h
c_mdpiqa.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdpiqa.o: /usr/include/sys/_types/_uid_t.h
c_mdpiqa.o: /usr/include/sys/_types/_clock_t.h
c_mdpiqa.o: /usr/include/sys/_types/_time_t.h
c_mdpiqa.o: /usr/include/sys/_types/_useconds_t.h
c_mdpiqa.o: /usr/include/sys/_types/_suseconds_t.h
c_mdpiqa.o: /usr/include/sys/_types/_rsize_t.h
c_mdpiqa.o: /usr/include/sys/_types/_errno_t.h
c_mdpiqa.o: /usr/include/sys/_types/_fd_def.h
c_mdpiqa.o: /usr/include/sys/_types/_fd_setsize.h
c_mdpiqa.o: /usr/include/sys/_types/_fd_set.h
c_mdpiqa.o: /usr/include/sys/_types/_fd_clr.h
c_mdpiqa.o: /usr/include/sys/_types/_fd_zero.h
c_mdpiqa.o: /usr/include/sys/_types/_fd_isset.h
c_mdpiqa.o: /usr/include/sys/_types/_fd_copy.h
c_mdpiqa.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdpiqa.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdpiqa.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdpiqa.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdpiqa.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdpiqa.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdpiqa.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdpiqa.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdpiqa.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdpiqa.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdpiqa.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdpiqa.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdpiqa.o: ../../../.././include/ncarg/c.h
c_mdpiqm.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdpiqm.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdpiqm.o: /usr/include/sys/_posix_availability.h
c_mdpiqm.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdpiqm.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdpiqm.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdpiqm.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdpiqm.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdpiqm.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdpiqm.o: /usr/include/sys/_types/_int16_t.h
c_mdpiqm.o: /usr/include/sys/_types/_int32_t.h
c_mdpiqm.o: /usr/include/sys/_types/_int64_t.h
c_mdpiqm.o: /usr/include/sys/_types/_u_int8_t.h
c_mdpiqm.o: /usr/include/sys/_types/_u_int16_t.h
c_mdpiqm.o: /usr/include/sys/_types/_u_int32_t.h
c_mdpiqm.o: /usr/include/sys/_types/_u_int64_t.h
c_mdpiqm.o: /usr/include/sys/_types/_intptr_t.h
c_mdpiqm.o: /usr/include/sys/_types/_uintptr_t.h
c_mdpiqm.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdpiqm.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdpiqm.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdpiqm.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdpiqm.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdpiqm.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdpiqm.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdpiqm.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdpiqm.o: /usr/include/sys/_types/_u_char.h
c_mdpiqm.o: /usr/include/sys/_types/_u_short.h
c_mdpiqm.o: /usr/include/sys/_types/_u_int.h
c_mdpiqm.o: /usr/include/sys/_types/_caddr_t.h
c_mdpiqm.o: /usr/include/sys/_types/_dev_t.h
c_mdpiqm.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdpiqm.o: /usr/include/sys/_types/_blksize_t.h
c_mdpiqm.o: /usr/include/sys/_types/_gid_t.h
c_mdpiqm.o: /usr/include/sys/_types/_in_addr_t.h
c_mdpiqm.o: /usr/include/sys/_types/_in_port_t.h
c_mdpiqm.o: /usr/include/sys/_types/_ino_t.h
c_mdpiqm.o: /usr/include/sys/_types/_ino64_t.h
c_mdpiqm.o: /usr/include/sys/_types/_key_t.h
c_mdpiqm.o: /usr/include/sys/_types/_mode_t.h
c_mdpiqm.o: /usr/include/sys/_types/_nlink_t.h
c_mdpiqm.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdpiqm.o: /usr/include/sys/_types/_uid_t.h
c_mdpiqm.o: /usr/include/sys/_types/_clock_t.h
c_mdpiqm.o: /usr/include/sys/_types/_time_t.h
c_mdpiqm.o: /usr/include/sys/_types/_useconds_t.h
c_mdpiqm.o: /usr/include/sys/_types/_suseconds_t.h
c_mdpiqm.o: /usr/include/sys/_types/_rsize_t.h
c_mdpiqm.o: /usr/include/sys/_types/_errno_t.h
c_mdpiqm.o: /usr/include/sys/_types/_fd_def.h
c_mdpiqm.o: /usr/include/sys/_types/_fd_setsize.h
c_mdpiqm.o: /usr/include/sys/_types/_fd_set.h
c_mdpiqm.o: /usr/include/sys/_types/_fd_clr.h
c_mdpiqm.o: /usr/include/sys/_types/_fd_zero.h
c_mdpiqm.o: /usr/include/sys/_types/_fd_isset.h
c_mdpiqm.o: /usr/include/sys/_types/_fd_copy.h
c_mdpiqm.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdpiqm.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdpiqm.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdpiqm.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdpiqm.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdpiqm.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdpiqm.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdpiqm.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdpiqm.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdpiqm.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdpiqm.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdpiqm.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdpiqm.o: ../../../.././include/ncarg/c.h
c_mdpit.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdpit.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdpit.o: /usr/include/sys/_posix_availability.h /usr/include/Availability.h
c_mdpit.o: /usr/include/AvailabilityInternal.h /usr/include/_types.h
c_mdpit.o: /usr/include/sys/_types.h /usr/include/machine/_types.h
c_mdpit.o: /usr/include/i386/_types.h
c_mdpit.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdpit.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdpit.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdpit.o: /usr/include/sys/_types/_int16_t.h
c_mdpit.o: /usr/include/sys/_types/_int32_t.h
c_mdpit.o: /usr/include/sys/_types/_int64_t.h
c_mdpit.o: /usr/include/sys/_types/_u_int8_t.h
c_mdpit.o: /usr/include/sys/_types/_u_int16_t.h
c_mdpit.o: /usr/include/sys/_types/_u_int32_t.h
c_mdpit.o: /usr/include/sys/_types/_u_int64_t.h
c_mdpit.o: /usr/include/sys/_types/_intptr_t.h
c_mdpit.o: /usr/include/sys/_types/_uintptr_t.h
c_mdpit.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdpit.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdpit.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdpit.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdpit.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdpit.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdpit.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdpit.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdpit.o: /usr/include/sys/_types/_u_char.h
c_mdpit.o: /usr/include/sys/_types/_u_short.h
c_mdpit.o: /usr/include/sys/_types/_u_int.h
c_mdpit.o: /usr/include/sys/_types/_caddr_t.h
c_mdpit.o: /usr/include/sys/_types/_dev_t.h
c_mdpit.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdpit.o: /usr/include/sys/_types/_blksize_t.h
c_mdpit.o: /usr/include/sys/_types/_gid_t.h
c_mdpit.o: /usr/include/sys/_types/_in_addr_t.h
c_mdpit.o: /usr/include/sys/_types/_in_port_t.h
c_mdpit.o: /usr/include/sys/_types/_ino_t.h
c_mdpit.o: /usr/include/sys/_types/_ino64_t.h
c_mdpit.o: /usr/include/sys/_types/_key_t.h /usr/include/sys/_types/_mode_t.h
c_mdpit.o: /usr/include/sys/_types/_nlink_t.h /usr/include/sys/_types/_id_t.h
c_mdpit.o: /usr/include/sys/_types/_pid_t.h /usr/include/sys/_types/_uid_t.h
c_mdpit.o: /usr/include/sys/_types/_clock_t.h
c_mdpit.o: /usr/include/sys/_types/_time_t.h
c_mdpit.o: /usr/include/sys/_types/_useconds_t.h
c_mdpit.o: /usr/include/sys/_types/_suseconds_t.h
c_mdpit.o: /usr/include/sys/_types/_rsize_t.h
c_mdpit.o: /usr/include/sys/_types/_errno_t.h
c_mdpit.o: /usr/include/sys/_types/_fd_def.h
c_mdpit.o: /usr/include/sys/_types/_fd_setsize.h
c_mdpit.o: /usr/include/sys/_types/_fd_set.h
c_mdpit.o: /usr/include/sys/_types/_fd_clr.h
c_mdpit.o: /usr/include/sys/_types/_fd_zero.h
c_mdpit.o: /usr/include/sys/_types/_fd_isset.h
c_mdpit.o: /usr/include/sys/_types/_fd_copy.h
c_mdpit.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdpit.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdpit.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdpit.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdpit.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdpit.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdpit.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdpit.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdpit.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdpit.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdpit.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdpit.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdpit.o: ../../../.././include/ncarg/c.h
c_mdpita.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdpita.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdpita.o: /usr/include/sys/_posix_availability.h
c_mdpita.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdpita.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdpita.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdpita.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdpita.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdpita.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdpita.o: /usr/include/sys/_types/_int16_t.h
c_mdpita.o: /usr/include/sys/_types/_int32_t.h
c_mdpita.o: /usr/include/sys/_types/_int64_t.h
c_mdpita.o: /usr/include/sys/_types/_u_int8_t.h
c_mdpita.o: /usr/include/sys/_types/_u_int16_t.h
c_mdpita.o: /usr/include/sys/_types/_u_int32_t.h
c_mdpita.o: /usr/include/sys/_types/_u_int64_t.h
c_mdpita.o: /usr/include/sys/_types/_intptr_t.h
c_mdpita.o: /usr/include/sys/_types/_uintptr_t.h
c_mdpita.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdpita.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdpita.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdpita.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdpita.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdpita.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdpita.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdpita.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdpita.o: /usr/include/sys/_types/_u_char.h
c_mdpita.o: /usr/include/sys/_types/_u_short.h
c_mdpita.o: /usr/include/sys/_types/_u_int.h
c_mdpita.o: /usr/include/sys/_types/_caddr_t.h
c_mdpita.o: /usr/include/sys/_types/_dev_t.h
c_mdpita.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdpita.o: /usr/include/sys/_types/_blksize_t.h
c_mdpita.o: /usr/include/sys/_types/_gid_t.h
c_mdpita.o: /usr/include/sys/_types/_in_addr_t.h
c_mdpita.o: /usr/include/sys/_types/_in_port_t.h
c_mdpita.o: /usr/include/sys/_types/_ino_t.h
c_mdpita.o: /usr/include/sys/_types/_ino64_t.h
c_mdpita.o: /usr/include/sys/_types/_key_t.h
c_mdpita.o: /usr/include/sys/_types/_mode_t.h
c_mdpita.o: /usr/include/sys/_types/_nlink_t.h
c_mdpita.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdpita.o: /usr/include/sys/_types/_uid_t.h
c_mdpita.o: /usr/include/sys/_types/_clock_t.h
c_mdpita.o: /usr/include/sys/_types/_time_t.h
c_mdpita.o: /usr/include/sys/_types/_useconds_t.h
c_mdpita.o: /usr/include/sys/_types/_suseconds_t.h
c_mdpita.o: /usr/include/sys/_types/_rsize_t.h
c_mdpita.o: /usr/include/sys/_types/_errno_t.h
c_mdpita.o: /usr/include/sys/_types/_fd_def.h
c_mdpita.o: /usr/include/sys/_types/_fd_setsize.h
c_mdpita.o: /usr/include/sys/_types/_fd_set.h
c_mdpita.o: /usr/include/sys/_types/_fd_clr.h
c_mdpita.o: /usr/include/sys/_types/_fd_zero.h
c_mdpita.o: /usr/include/sys/_types/_fd_isset.h
c_mdpita.o: /usr/include/sys/_types/_fd_copy.h
c_mdpita.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdpita.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdpita.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdpita.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdpita.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdpita.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdpita.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdpita.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdpita.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdpita.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdpita.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdpita.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdpita.o: ../../../.././include/ncarg/c.h
c_mdpitd.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdpitd.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdpitd.o: /usr/include/sys/_posix_availability.h
c_mdpitd.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdpitd.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdpitd.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdpitd.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdpitd.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdpitd.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdpitd.o: /usr/include/sys/_types/_int16_t.h
c_mdpitd.o: /usr/include/sys/_types/_int32_t.h
c_mdpitd.o: /usr/include/sys/_types/_int64_t.h
c_mdpitd.o: /usr/include/sys/_types/_u_int8_t.h
c_mdpitd.o: /usr/include/sys/_types/_u_int16_t.h
c_mdpitd.o: /usr/include/sys/_types/_u_int32_t.h
c_mdpitd.o: /usr/include/sys/_types/_u_int64_t.h
c_mdpitd.o: /usr/include/sys/_types/_intptr_t.h
c_mdpitd.o: /usr/include/sys/_types/_uintptr_t.h
c_mdpitd.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdpitd.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdpitd.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdpitd.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdpitd.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdpitd.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdpitd.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdpitd.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdpitd.o: /usr/include/sys/_types/_u_char.h
c_mdpitd.o: /usr/include/sys/_types/_u_short.h
c_mdpitd.o: /usr/include/sys/_types/_u_int.h
c_mdpitd.o: /usr/include/sys/_types/_caddr_t.h
c_mdpitd.o: /usr/include/sys/_types/_dev_t.h
c_mdpitd.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdpitd.o: /usr/include/sys/_types/_blksize_t.h
c_mdpitd.o: /usr/include/sys/_types/_gid_t.h
c_mdpitd.o: /usr/include/sys/_types/_in_addr_t.h
c_mdpitd.o: /usr/include/sys/_types/_in_port_t.h
c_mdpitd.o: /usr/include/sys/_types/_ino_t.h
c_mdpitd.o: /usr/include/sys/_types/_ino64_t.h
c_mdpitd.o: /usr/include/sys/_types/_key_t.h
c_mdpitd.o: /usr/include/sys/_types/_mode_t.h
c_mdpitd.o: /usr/include/sys/_types/_nlink_t.h
c_mdpitd.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdpitd.o: /usr/include/sys/_types/_uid_t.h
c_mdpitd.o: /usr/include/sys/_types/_clock_t.h
c_mdpitd.o: /usr/include/sys/_types/_time_t.h
c_mdpitd.o: /usr/include/sys/_types/_useconds_t.h
c_mdpitd.o: /usr/include/sys/_types/_suseconds_t.h
c_mdpitd.o: /usr/include/sys/_types/_rsize_t.h
c_mdpitd.o: /usr/include/sys/_types/_errno_t.h
c_mdpitd.o: /usr/include/sys/_types/_fd_def.h
c_mdpitd.o: /usr/include/sys/_types/_fd_setsize.h
c_mdpitd.o: /usr/include/sys/_types/_fd_set.h
c_mdpitd.o: /usr/include/sys/_types/_fd_clr.h
c_mdpitd.o: /usr/include/sys/_types/_fd_zero.h
c_mdpitd.o: /usr/include/sys/_types/_fd_isset.h
c_mdpitd.o: /usr/include/sys/_types/_fd_copy.h
c_mdpitd.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdpitd.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdpitd.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdpitd.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdpitd.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdpitd.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdpitd.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdpitd.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdpitd.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdpitd.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdpitd.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdpitd.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdpitd.o: ../../../.././include/ncarg/c.h
c_mdpitm.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdpitm.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdpitm.o: /usr/include/sys/_posix_availability.h
c_mdpitm.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdpitm.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdpitm.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdpitm.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdpitm.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdpitm.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdpitm.o: /usr/include/sys/_types/_int16_t.h
c_mdpitm.o: /usr/include/sys/_types/_int32_t.h
c_mdpitm.o: /usr/include/sys/_types/_int64_t.h
c_mdpitm.o: /usr/include/sys/_types/_u_int8_t.h
c_mdpitm.o: /usr/include/sys/_types/_u_int16_t.h
c_mdpitm.o: /usr/include/sys/_types/_u_int32_t.h
c_mdpitm.o: /usr/include/sys/_types/_u_int64_t.h
c_mdpitm.o: /usr/include/sys/_types/_intptr_t.h
c_mdpitm.o: /usr/include/sys/_types/_uintptr_t.h
c_mdpitm.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdpitm.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdpitm.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdpitm.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdpitm.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdpitm.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdpitm.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdpitm.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdpitm.o: /usr/include/sys/_types/_u_char.h
c_mdpitm.o: /usr/include/sys/_types/_u_short.h
c_mdpitm.o: /usr/include/sys/_types/_u_int.h
c_mdpitm.o: /usr/include/sys/_types/_caddr_t.h
c_mdpitm.o: /usr/include/sys/_types/_dev_t.h
c_mdpitm.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdpitm.o: /usr/include/sys/_types/_blksize_t.h
c_mdpitm.o: /usr/include/sys/_types/_gid_t.h
c_mdpitm.o: /usr/include/sys/_types/_in_addr_t.h
c_mdpitm.o: /usr/include/sys/_types/_in_port_t.h
c_mdpitm.o: /usr/include/sys/_types/_ino_t.h
c_mdpitm.o: /usr/include/sys/_types/_ino64_t.h
c_mdpitm.o: /usr/include/sys/_types/_key_t.h
c_mdpitm.o: /usr/include/sys/_types/_mode_t.h
c_mdpitm.o: /usr/include/sys/_types/_nlink_t.h
c_mdpitm.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdpitm.o: /usr/include/sys/_types/_uid_t.h
c_mdpitm.o: /usr/include/sys/_types/_clock_t.h
c_mdpitm.o: /usr/include/sys/_types/_time_t.h
c_mdpitm.o: /usr/include/sys/_types/_useconds_t.h
c_mdpitm.o: /usr/include/sys/_types/_suseconds_t.h
c_mdpitm.o: /usr/include/sys/_types/_rsize_t.h
c_mdpitm.o: /usr/include/sys/_types/_errno_t.h
c_mdpitm.o: /usr/include/sys/_types/_fd_def.h
c_mdpitm.o: /usr/include/sys/_types/_fd_setsize.h
c_mdpitm.o: /usr/include/sys/_types/_fd_set.h
c_mdpitm.o: /usr/include/sys/_types/_fd_clr.h
c_mdpitm.o: /usr/include/sys/_types/_fd_zero.h
c_mdpitm.o: /usr/include/sys/_types/_fd_isset.h
c_mdpitm.o: /usr/include/sys/_types/_fd_copy.h
c_mdpitm.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdpitm.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdpitm.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdpitm.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdpitm.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdpitm.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdpitm.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdpitm.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdpitm.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdpitm.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdpitm.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdpitm.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdpitm.o: ../../../.././include/ncarg/c.h
c_mdppos.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdppos.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdppos.o: /usr/include/sys/_posix_availability.h
c_mdppos.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdppos.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdppos.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdppos.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdppos.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdppos.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdppos.o: /usr/include/sys/_types/_int16_t.h
c_mdppos.o: /usr/include/sys/_types/_int32_t.h
c_mdppos.o: /usr/include/sys/_types/_int64_t.h
c_mdppos.o: /usr/include/sys/_types/_u_int8_t.h
c_mdppos.o: /usr/include/sys/_types/_u_int16_t.h
c_mdppos.o: /usr/include/sys/_types/_u_int32_t.h
c_mdppos.o: /usr/include/sys/_types/_u_int64_t.h
c_mdppos.o: /usr/include/sys/_types/_intptr_t.h
c_mdppos.o: /usr/include/sys/_types/_uintptr_t.h
c_mdppos.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdppos.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdppos.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdppos.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdppos.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdppos.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdppos.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdppos.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdppos.o: /usr/include/sys/_types/_u_char.h
c_mdppos.o: /usr/include/sys/_types/_u_short.h
c_mdppos.o: /usr/include/sys/_types/_u_int.h
c_mdppos.o: /usr/include/sys/_types/_caddr_t.h
c_mdppos.o: /usr/include/sys/_types/_dev_t.h
c_mdppos.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdppos.o: /usr/include/sys/_types/_blksize_t.h
c_mdppos.o: /usr/include/sys/_types/_gid_t.h
c_mdppos.o: /usr/include/sys/_types/_in_addr_t.h
c_mdppos.o: /usr/include/sys/_types/_in_port_t.h
c_mdppos.o: /usr/include/sys/_types/_ino_t.h
c_mdppos.o: /usr/include/sys/_types/_ino64_t.h
c_mdppos.o: /usr/include/sys/_types/_key_t.h
c_mdppos.o: /usr/include/sys/_types/_mode_t.h
c_mdppos.o: /usr/include/sys/_types/_nlink_t.h
c_mdppos.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdppos.o: /usr/include/sys/_types/_uid_t.h
c_mdppos.o: /usr/include/sys/_types/_clock_t.h
c_mdppos.o: /usr/include/sys/_types/_time_t.h
c_mdppos.o: /usr/include/sys/_types/_useconds_t.h
c_mdppos.o: /usr/include/sys/_types/_suseconds_t.h
c_mdppos.o: /usr/include/sys/_types/_rsize_t.h
c_mdppos.o: /usr/include/sys/_types/_errno_t.h
c_mdppos.o: /usr/include/sys/_types/_fd_def.h
c_mdppos.o: /usr/include/sys/_types/_fd_setsize.h
c_mdppos.o: /usr/include/sys/_types/_fd_set.h
c_mdppos.o: /usr/include/sys/_types/_fd_clr.h
c_mdppos.o: /usr/include/sys/_types/_fd_zero.h
c_mdppos.o: /usr/include/sys/_types/_fd_isset.h
c_mdppos.o: /usr/include/sys/_types/_fd_copy.h
c_mdppos.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdppos.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdppos.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdppos.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdppos.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdppos.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdppos.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdppos.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdppos.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdppos.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdppos.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdppos.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdppos.o: ../../../.././include/ncarg/c.h
c_mdproj.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdproj.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdproj.o: /usr/include/sys/_posix_availability.h
c_mdproj.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdproj.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdproj.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdproj.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdproj.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdproj.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdproj.o: /usr/include/sys/_types/_int16_t.h
c_mdproj.o: /usr/include/sys/_types/_int32_t.h
c_mdproj.o: /usr/include/sys/_types/_int64_t.h
c_mdproj.o: /usr/include/sys/_types/_u_int8_t.h
c_mdproj.o: /usr/include/sys/_types/_u_int16_t.h
c_mdproj.o: /usr/include/sys/_types/_u_int32_t.h
c_mdproj.o: /usr/include/sys/_types/_u_int64_t.h
c_mdproj.o: /usr/include/sys/_types/_intptr_t.h
c_mdproj.o: /usr/include/sys/_types/_uintptr_t.h
c_mdproj.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdproj.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdproj.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdproj.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdproj.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdproj.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdproj.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdproj.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdproj.o: /usr/include/sys/_types/_u_char.h
c_mdproj.o: /usr/include/sys/_types/_u_short.h
c_mdproj.o: /usr/include/sys/_types/_u_int.h
c_mdproj.o: /usr/include/sys/_types/_caddr_t.h
c_mdproj.o: /usr/include/sys/_types/_dev_t.h
c_mdproj.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdproj.o: /usr/include/sys/_types/_blksize_t.h
c_mdproj.o: /usr/include/sys/_types/_gid_t.h
c_mdproj.o: /usr/include/sys/_types/_in_addr_t.h
c_mdproj.o: /usr/include/sys/_types/_in_port_t.h
c_mdproj.o: /usr/include/sys/_types/_ino_t.h
c_mdproj.o: /usr/include/sys/_types/_ino64_t.h
c_mdproj.o: /usr/include/sys/_types/_key_t.h
c_mdproj.o: /usr/include/sys/_types/_mode_t.h
c_mdproj.o: /usr/include/sys/_types/_nlink_t.h
c_mdproj.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdproj.o: /usr/include/sys/_types/_uid_t.h
c_mdproj.o: /usr/include/sys/_types/_clock_t.h
c_mdproj.o: /usr/include/sys/_types/_time_t.h
c_mdproj.o: /usr/include/sys/_types/_useconds_t.h
c_mdproj.o: /usr/include/sys/_types/_suseconds_t.h
c_mdproj.o: /usr/include/sys/_types/_rsize_t.h
c_mdproj.o: /usr/include/sys/_types/_errno_t.h
c_mdproj.o: /usr/include/sys/_types/_fd_def.h
c_mdproj.o: /usr/include/sys/_types/_fd_setsize.h
c_mdproj.o: /usr/include/sys/_types/_fd_set.h
c_mdproj.o: /usr/include/sys/_types/_fd_clr.h
c_mdproj.o: /usr/include/sys/_types/_fd_zero.h
c_mdproj.o: /usr/include/sys/_types/_fd_isset.h
c_mdproj.o: /usr/include/sys/_types/_fd_copy.h
c_mdproj.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdproj.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdproj.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdproj.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdproj.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdproj.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdproj.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdproj.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdproj.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdproj.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdproj.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdproj.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdproj.o: ../../../.././include/ncarg/c.h
c_mdpset.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdpset.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdpset.o: /usr/include/sys/_posix_availability.h
c_mdpset.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdpset.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdpset.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdpset.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdpset.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdpset.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdpset.o: /usr/include/sys/_types/_int16_t.h
c_mdpset.o: /usr/include/sys/_types/_int32_t.h
c_mdpset.o: /usr/include/sys/_types/_int64_t.h
c_mdpset.o: /usr/include/sys/_types/_u_int8_t.h
c_mdpset.o: /usr/include/sys/_types/_u_int16_t.h
c_mdpset.o: /usr/include/sys/_types/_u_int32_t.h
c_mdpset.o: /usr/include/sys/_types/_u_int64_t.h
c_mdpset.o: /usr/include/sys/_types/_intptr_t.h
c_mdpset.o: /usr/include/sys/_types/_uintptr_t.h
c_mdpset.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdpset.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdpset.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdpset.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdpset.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdpset.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdpset.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdpset.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdpset.o: /usr/include/sys/_types/_u_char.h
c_mdpset.o: /usr/include/sys/_types/_u_short.h
c_mdpset.o: /usr/include/sys/_types/_u_int.h
c_mdpset.o: /usr/include/sys/_types/_caddr_t.h
c_mdpset.o: /usr/include/sys/_types/_dev_t.h
c_mdpset.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdpset.o: /usr/include/sys/_types/_blksize_t.h
c_mdpset.o: /usr/include/sys/_types/_gid_t.h
c_mdpset.o: /usr/include/sys/_types/_in_addr_t.h
c_mdpset.o: /usr/include/sys/_types/_in_port_t.h
c_mdpset.o: /usr/include/sys/_types/_ino_t.h
c_mdpset.o: /usr/include/sys/_types/_ino64_t.h
c_mdpset.o: /usr/include/sys/_types/_key_t.h
c_mdpset.o: /usr/include/sys/_types/_mode_t.h
c_mdpset.o: /usr/include/sys/_types/_nlink_t.h
c_mdpset.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdpset.o: /usr/include/sys/_types/_uid_t.h
c_mdpset.o: /usr/include/sys/_types/_clock_t.h
c_mdpset.o: /usr/include/sys/_types/_time_t.h
c_mdpset.o: /usr/include/sys/_types/_useconds_t.h
c_mdpset.o: /usr/include/sys/_types/_suseconds_t.h
c_mdpset.o: /usr/include/sys/_types/_rsize_t.h
c_mdpset.o: /usr/include/sys/_types/_errno_t.h
c_mdpset.o: /usr/include/sys/_types/_fd_def.h
c_mdpset.o: /usr/include/sys/_types/_fd_setsize.h
c_mdpset.o: /usr/include/sys/_types/_fd_set.h
c_mdpset.o: /usr/include/sys/_types/_fd_clr.h
c_mdpset.o: /usr/include/sys/_types/_fd_zero.h
c_mdpset.o: /usr/include/sys/_types/_fd_isset.h
c_mdpset.o: /usr/include/sys/_types/_fd_copy.h
c_mdpset.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdpset.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdpset.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdpset.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdpset.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdpset.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdpset.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdpset.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdpset.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdpset.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdpset.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdpset.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdpset.o: ../../../.././include/ncarg/c.h
c_mdptra.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdptra.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdptra.o: /usr/include/sys/_posix_availability.h
c_mdptra.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdptra.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdptra.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdptra.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdptra.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdptra.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdptra.o: /usr/include/sys/_types/_int16_t.h
c_mdptra.o: /usr/include/sys/_types/_int32_t.h
c_mdptra.o: /usr/include/sys/_types/_int64_t.h
c_mdptra.o: /usr/include/sys/_types/_u_int8_t.h
c_mdptra.o: /usr/include/sys/_types/_u_int16_t.h
c_mdptra.o: /usr/include/sys/_types/_u_int32_t.h
c_mdptra.o: /usr/include/sys/_types/_u_int64_t.h
c_mdptra.o: /usr/include/sys/_types/_intptr_t.h
c_mdptra.o: /usr/include/sys/_types/_uintptr_t.h
c_mdptra.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdptra.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdptra.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdptra.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdptra.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdptra.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdptra.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdptra.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdptra.o: /usr/include/sys/_types/_u_char.h
c_mdptra.o: /usr/include/sys/_types/_u_short.h
c_mdptra.o: /usr/include/sys/_types/_u_int.h
c_mdptra.o: /usr/include/sys/_types/_caddr_t.h
c_mdptra.o: /usr/include/sys/_types/_dev_t.h
c_mdptra.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdptra.o: /usr/include/sys/_types/_blksize_t.h
c_mdptra.o: /usr/include/sys/_types/_gid_t.h
c_mdptra.o: /usr/include/sys/_types/_in_addr_t.h
c_mdptra.o: /usr/include/sys/_types/_in_port_t.h
c_mdptra.o: /usr/include/sys/_types/_ino_t.h
c_mdptra.o: /usr/include/sys/_types/_ino64_t.h
c_mdptra.o: /usr/include/sys/_types/_key_t.h
c_mdptra.o: /usr/include/sys/_types/_mode_t.h
c_mdptra.o: /usr/include/sys/_types/_nlink_t.h
c_mdptra.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdptra.o: /usr/include/sys/_types/_uid_t.h
c_mdptra.o: /usr/include/sys/_types/_clock_t.h
c_mdptra.o: /usr/include/sys/_types/_time_t.h
c_mdptra.o: /usr/include/sys/_types/_useconds_t.h
c_mdptra.o: /usr/include/sys/_types/_suseconds_t.h
c_mdptra.o: /usr/include/sys/_types/_rsize_t.h
c_mdptra.o: /usr/include/sys/_types/_errno_t.h
c_mdptra.o: /usr/include/sys/_types/_fd_def.h
c_mdptra.o: /usr/include/sys/_types/_fd_setsize.h
c_mdptra.o: /usr/include/sys/_types/_fd_set.h
c_mdptra.o: /usr/include/sys/_types/_fd_clr.h
c_mdptra.o: /usr/include/sys/_types/_fd_zero.h
c_mdptra.o: /usr/include/sys/_types/_fd_isset.h
c_mdptra.o: /usr/include/sys/_types/_fd_copy.h
c_mdptra.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdptra.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdptra.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdptra.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdptra.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdptra.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdptra.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdptra.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdptra.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdptra.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdptra.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdptra.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdptra.o: ../../../.././include/ncarg/c.h
c_mdptri.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdptri.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdptri.o: /usr/include/sys/_posix_availability.h
c_mdptri.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdptri.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdptri.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdptri.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdptri.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdptri.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdptri.o: /usr/include/sys/_types/_int16_t.h
c_mdptri.o: /usr/include/sys/_types/_int32_t.h
c_mdptri.o: /usr/include/sys/_types/_int64_t.h
c_mdptri.o: /usr/include/sys/_types/_u_int8_t.h
c_mdptri.o: /usr/include/sys/_types/_u_int16_t.h
c_mdptri.o: /usr/include/sys/_types/_u_int32_t.h
c_mdptri.o: /usr/include/sys/_types/_u_int64_t.h
c_mdptri.o: /usr/include/sys/_types/_intptr_t.h
c_mdptri.o: /usr/include/sys/_types/_uintptr_t.h
c_mdptri.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdptri.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdptri.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdptri.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdptri.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdptri.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdptri.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdptri.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdptri.o: /usr/include/sys/_types/_u_char.h
c_mdptri.o: /usr/include/sys/_types/_u_short.h
c_mdptri.o: /usr/include/sys/_types/_u_int.h
c_mdptri.o: /usr/include/sys/_types/_caddr_t.h
c_mdptri.o: /usr/include/sys/_types/_dev_t.h
c_mdptri.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdptri.o: /usr/include/sys/_types/_blksize_t.h
c_mdptri.o: /usr/include/sys/_types/_gid_t.h
c_mdptri.o: /usr/include/sys/_types/_in_addr_t.h
c_mdptri.o: /usr/include/sys/_types/_in_port_t.h
c_mdptri.o: /usr/include/sys/_types/_ino_t.h
c_mdptri.o: /usr/include/sys/_types/_ino64_t.h
c_mdptri.o: /usr/include/sys/_types/_key_t.h
c_mdptri.o: /usr/include/sys/_types/_mode_t.h
c_mdptri.o: /usr/include/sys/_types/_nlink_t.h
c_mdptri.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdptri.o: /usr/include/sys/_types/_uid_t.h
c_mdptri.o: /usr/include/sys/_types/_clock_t.h
c_mdptri.o: /usr/include/sys/_types/_time_t.h
c_mdptri.o: /usr/include/sys/_types/_useconds_t.h
c_mdptri.o: /usr/include/sys/_types/_suseconds_t.h
c_mdptri.o: /usr/include/sys/_types/_rsize_t.h
c_mdptri.o: /usr/include/sys/_types/_errno_t.h
c_mdptri.o: /usr/include/sys/_types/_fd_def.h
c_mdptri.o: /usr/include/sys/_types/_fd_setsize.h
c_mdptri.o: /usr/include/sys/_types/_fd_set.h
c_mdptri.o: /usr/include/sys/_types/_fd_clr.h
c_mdptri.o: /usr/include/sys/_types/_fd_zero.h
c_mdptri.o: /usr/include/sys/_types/_fd_isset.h
c_mdptri.o: /usr/include/sys/_types/_fd_copy.h
c_mdptri.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdptri.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdptri.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdptri.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdptri.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdptri.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdptri.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdptri.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdptri.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdptri.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdptri.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdptri.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdptri.o: ../../../.././include/ncarg/c.h
c_mdptrn.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdptrn.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdptrn.o: /usr/include/sys/_posix_availability.h
c_mdptrn.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdptrn.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdptrn.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdptrn.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdptrn.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdptrn.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdptrn.o: /usr/include/sys/_types/_int16_t.h
c_mdptrn.o: /usr/include/sys/_types/_int32_t.h
c_mdptrn.o: /usr/include/sys/_types/_int64_t.h
c_mdptrn.o: /usr/include/sys/_types/_u_int8_t.h
c_mdptrn.o: /usr/include/sys/_types/_u_int16_t.h
c_mdptrn.o: /usr/include/sys/_types/_u_int32_t.h
c_mdptrn.o: /usr/include/sys/_types/_u_int64_t.h
c_mdptrn.o: /usr/include/sys/_types/_intptr_t.h
c_mdptrn.o: /usr/include/sys/_types/_uintptr_t.h
c_mdptrn.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdptrn.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdptrn.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdptrn.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdptrn.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdptrn.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdptrn.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdptrn.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdptrn.o: /usr/include/sys/_types/_u_char.h
c_mdptrn.o: /usr/include/sys/_types/_u_short.h
c_mdptrn.o: /usr/include/sys/_types/_u_int.h
c_mdptrn.o: /usr/include/sys/_types/_caddr_t.h
c_mdptrn.o: /usr/include/sys/_types/_dev_t.h
c_mdptrn.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdptrn.o: /usr/include/sys/_types/_blksize_t.h
c_mdptrn.o: /usr/include/sys/_types/_gid_t.h
c_mdptrn.o: /usr/include/sys/_types/_in_addr_t.h
c_mdptrn.o: /usr/include/sys/_types/_in_port_t.h
c_mdptrn.o: /usr/include/sys/_types/_ino_t.h
c_mdptrn.o: /usr/include/sys/_types/_ino64_t.h
c_mdptrn.o: /usr/include/sys/_types/_key_t.h
c_mdptrn.o: /usr/include/sys/_types/_mode_t.h
c_mdptrn.o: /usr/include/sys/_types/_nlink_t.h
c_mdptrn.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdptrn.o: /usr/include/sys/_types/_uid_t.h
c_mdptrn.o: /usr/include/sys/_types/_clock_t.h
c_mdptrn.o: /usr/include/sys/_types/_time_t.h
c_mdptrn.o: /usr/include/sys/_types/_useconds_t.h
c_mdptrn.o: /usr/include/sys/_types/_suseconds_t.h
c_mdptrn.o: /usr/include/sys/_types/_rsize_t.h
c_mdptrn.o: /usr/include/sys/_types/_errno_t.h
c_mdptrn.o: /usr/include/sys/_types/_fd_def.h
c_mdptrn.o: /usr/include/sys/_types/_fd_setsize.h
c_mdptrn.o: /usr/include/sys/_types/_fd_set.h
c_mdptrn.o: /usr/include/sys/_types/_fd_clr.h
c_mdptrn.o: /usr/include/sys/_types/_fd_zero.h
c_mdptrn.o: /usr/include/sys/_types/_fd_isset.h
c_mdptrn.o: /usr/include/sys/_types/_fd_copy.h
c_mdptrn.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdptrn.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdptrn.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdptrn.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdptrn.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdptrn.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdptrn.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdptrn.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdptrn.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdptrn.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdptrn.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdptrn.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdptrn.o: ../../../.././include/ncarg/c.h
c_mpgetd.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mpgetd.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mpgetd.o: /usr/include/sys/_posix_availability.h
c_mpgetd.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mpgetd.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mpgetd.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mpgetd.o: /usr/include/sys/_pthread/_pthread_types.h
c_mpgetd.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mpgetd.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mpgetd.o: /usr/include/sys/_types/_int16_t.h
c_mpgetd.o: /usr/include/sys/_types/_int32_t.h
c_mpgetd.o: /usr/include/sys/_types/_int64_t.h
c_mpgetd.o: /usr/include/sys/_types/_u_int8_t.h
c_mpgetd.o: /usr/include/sys/_types/_u_int16_t.h
c_mpgetd.o: /usr/include/sys/_types/_u_int32_t.h
c_mpgetd.o: /usr/include/sys/_types/_u_int64_t.h
c_mpgetd.o: /usr/include/sys/_types/_intptr_t.h
c_mpgetd.o: /usr/include/sys/_types/_uintptr_t.h
c_mpgetd.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mpgetd.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mpgetd.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mpgetd.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mpgetd.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mpgetd.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mpgetd.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mpgetd.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mpgetd.o: /usr/include/sys/_types/_u_char.h
c_mpgetd.o: /usr/include/sys/_types/_u_short.h
c_mpgetd.o: /usr/include/sys/_types/_u_int.h
c_mpgetd.o: /usr/include/sys/_types/_caddr_t.h
c_mpgetd.o: /usr/include/sys/_types/_dev_t.h
c_mpgetd.o: /usr/include/sys/_types/_blkcnt_t.h
c_mpgetd.o: /usr/include/sys/_types/_blksize_t.h
c_mpgetd.o: /usr/include/sys/_types/_gid_t.h
c_mpgetd.o: /usr/include/sys/_types/_in_addr_t.h
c_mpgetd.o: /usr/include/sys/_types/_in_port_t.h
c_mpgetd.o: /usr/include/sys/_types/_ino_t.h
c_mpgetd.o: /usr/include/sys/_types/_ino64_t.h
c_mpgetd.o: /usr/include/sys/_types/_key_t.h
c_mpgetd.o: /usr/include/sys/_types/_mode_t.h
c_mpgetd.o: /usr/include/sys/_types/_nlink_t.h
c_mpgetd.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mpgetd.o: /usr/include/sys/_types/_uid_t.h
c_mpgetd.o: /usr/include/sys/_types/_clock_t.h
c_mpgetd.o: /usr/include/sys/_types/_time_t.h
c_mpgetd.o: /usr/include/sys/_types/_useconds_t.h
c_mpgetd.o: /usr/include/sys/_types/_suseconds_t.h
c_mpgetd.o: /usr/include/sys/_types/_rsize_t.h
c_mpgetd.o: /usr/include/sys/_types/_errno_t.h
c_mpgetd.o: /usr/include/sys/_types/_fd_def.h
c_mpgetd.o: /usr/include/sys/_types/_fd_setsize.h
c_mpgetd.o: /usr/include/sys/_types/_fd_set.h
c_mpgetd.o: /usr/include/sys/_types/_fd_clr.h
c_mpgetd.o: /usr/include/sys/_types/_fd_zero.h
c_mpgetd.o: /usr/include/sys/_types/_fd_isset.h
c_mpgetd.o: /usr/include/sys/_types/_fd_copy.h
c_mpgetd.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mpgetd.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mpgetd.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mpgetd.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mpgetd.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mpgetd.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mpgetd.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mpgetd.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mpgetd.o: /usr/include/sys/_pthread/_pthread_t.h
c_mpgetd.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mpgetd.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mpgetd.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mpgetd.o: ../../../.././include/ncarg/c.h
c_mdpgci.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdpgci.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdpgci.o: /usr/include/sys/_posix_availability.h
c_mdpgci.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdpgci.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdpgci.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdpgci.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdpgci.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdpgci.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdpgci.o: /usr/include/sys/_types/_int16_t.h
c_mdpgci.o: /usr/include/sys/_types/_int32_t.h
c_mdpgci.o: /usr/include/sys/_types/_int64_t.h
c_mdpgci.o: /usr/include/sys/_types/_u_int8_t.h
c_mdpgci.o: /usr/include/sys/_types/_u_int16_t.h
c_mdpgci.o: /usr/include/sys/_types/_u_int32_t.h
c_mdpgci.o: /usr/include/sys/_types/_u_int64_t.h
c_mdpgci.o: /usr/include/sys/_types/_intptr_t.h
c_mdpgci.o: /usr/include/sys/_types/_uintptr_t.h
c_mdpgci.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdpgci.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdpgci.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdpgci.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdpgci.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdpgci.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdpgci.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdpgci.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdpgci.o: /usr/include/sys/_types/_u_char.h
c_mdpgci.o: /usr/include/sys/_types/_u_short.h
c_mdpgci.o: /usr/include/sys/_types/_u_int.h
c_mdpgci.o: /usr/include/sys/_types/_caddr_t.h
c_mdpgci.o: /usr/include/sys/_types/_dev_t.h
c_mdpgci.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdpgci.o: /usr/include/sys/_types/_blksize_t.h
c_mdpgci.o: /usr/include/sys/_types/_gid_t.h
c_mdpgci.o: /usr/include/sys/_types/_in_addr_t.h
c_mdpgci.o: /usr/include/sys/_types/_in_port_t.h
c_mdpgci.o: /usr/include/sys/_types/_ino_t.h
c_mdpgci.o: /usr/include/sys/_types/_ino64_t.h
c_mdpgci.o: /usr/include/sys/_types/_key_t.h
c_mdpgci.o: /usr/include/sys/_types/_mode_t.h
c_mdpgci.o: /usr/include/sys/_types/_nlink_t.h
c_mdpgci.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdpgci.o: /usr/include/sys/_types/_uid_t.h
c_mdpgci.o: /usr/include/sys/_types/_clock_t.h
c_mdpgci.o: /usr/include/sys/_types/_time_t.h
c_mdpgci.o: /usr/include/sys/_types/_useconds_t.h
c_mdpgci.o: /usr/include/sys/_types/_suseconds_t.h
c_mdpgci.o: /usr/include/sys/_types/_rsize_t.h
c_mdpgci.o: /usr/include/sys/_types/_errno_t.h
c_mdpgci.o: /usr/include/sys/_types/_fd_def.h
c_mdpgci.o: /usr/include/sys/_types/_fd_setsize.h
c_mdpgci.o: /usr/include/sys/_types/_fd_set.h
c_mdpgci.o: /usr/include/sys/_types/_fd_clr.h
c_mdpgci.o: /usr/include/sys/_types/_fd_zero.h
c_mdpgci.o: /usr/include/sys/_types/_fd_isset.h
c_mdpgci.o: /usr/include/sys/_types/_fd_copy.h
c_mdpgci.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdpgci.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdpgci.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdpgci.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdpgci.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdpgci.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdpgci.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdpgci.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdpgci.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdpgci.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdpgci.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdpgci.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdpgci.o: ../../../.././include/ncarg/c.h
c_mdgcog.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdgcog.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdgcog.o: /usr/include/sys/_posix_availability.h
c_mdgcog.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdgcog.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdgcog.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdgcog.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdgcog.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdgcog.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdgcog.o: /usr/include/sys/_types/_int16_t.h
c_mdgcog.o: /usr/include/sys/_types/_int32_t.h
c_mdgcog.o: /usr/include/sys/_types/_int64_t.h
c_mdgcog.o: /usr/include/sys/_types/_u_int8_t.h
c_mdgcog.o: /usr/include/sys/_types/_u_int16_t.h
c_mdgcog.o: /usr/include/sys/_types/_u_int32_t.h
c_mdgcog.o: /usr/include/sys/_types/_u_int64_t.h
c_mdgcog.o: /usr/include/sys/_types/_intptr_t.h
c_mdgcog.o: /usr/include/sys/_types/_uintptr_t.h
c_mdgcog.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdgcog.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdgcog.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdgcog.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdgcog.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdgcog.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdgcog.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdgcog.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdgcog.o: /usr/include/sys/_types/_u_char.h
c_mdgcog.o: /usr/include/sys/_types/_u_short.h
c_mdgcog.o: /usr/include/sys/_types/_u_int.h
c_mdgcog.o: /usr/include/sys/_types/_caddr_t.h
c_mdgcog.o: /usr/include/sys/_types/_dev_t.h
c_mdgcog.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdgcog.o: /usr/include/sys/_types/_blksize_t.h
c_mdgcog.o: /usr/include/sys/_types/_gid_t.h
c_mdgcog.o: /usr/include/sys/_types/_in_addr_t.h
c_mdgcog.o: /usr/include/sys/_types/_in_port_t.h
c_mdgcog.o: /usr/include/sys/_types/_ino_t.h
c_mdgcog.o: /usr/include/sys/_types/_ino64_t.h
c_mdgcog.o: /usr/include/sys/_types/_key_t.h
c_mdgcog.o: /usr/include/sys/_types/_mode_t.h
c_mdgcog.o: /usr/include/sys/_types/_nlink_t.h
c_mdgcog.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdgcog.o: /usr/include/sys/_types/_uid_t.h
c_mdgcog.o: /usr/include/sys/_types/_clock_t.h
c_mdgcog.o: /usr/include/sys/_types/_time_t.h
c_mdgcog.o: /usr/include/sys/_types/_useconds_t.h
c_mdgcog.o: /usr/include/sys/_types/_suseconds_t.h
c_mdgcog.o: /usr/include/sys/_types/_rsize_t.h
c_mdgcog.o: /usr/include/sys/_types/_errno_t.h
c_mdgcog.o: /usr/include/sys/_types/_fd_def.h
c_mdgcog.o: /usr/include/sys/_types/_fd_setsize.h
c_mdgcog.o: /usr/include/sys/_types/_fd_set.h
c_mdgcog.o: /usr/include/sys/_types/_fd_clr.h
c_mdgcog.o: /usr/include/sys/_types/_fd_zero.h
c_mdgcog.o: /usr/include/sys/_types/_fd_isset.h
c_mdgcog.o: /usr/include/sys/_types/_fd_copy.h
c_mdgcog.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdgcog.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdgcog.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdgcog.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdgcog.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdgcog.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdgcog.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdgcog.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdgcog.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdgcog.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdgcog.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdgcog.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdgcog.o: ../../../.././include/ncarg/c.h
c_mdlbln.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdlbln.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdlbln.o: /usr/include/sys/_posix_availability.h
c_mdlbln.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdlbln.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdlbln.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdlbln.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdlbln.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdlbln.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdlbln.o: /usr/include/sys/_types/_int16_t.h
c_mdlbln.o: /usr/include/sys/_types/_int32_t.h
c_mdlbln.o: /usr/include/sys/_types/_int64_t.h
c_mdlbln.o: /usr/include/sys/_types/_u_int8_t.h
c_mdlbln.o: /usr/include/sys/_types/_u_int16_t.h
c_mdlbln.o: /usr/include/sys/_types/_u_int32_t.h
c_mdlbln.o: /usr/include/sys/_types/_u_int64_t.h
c_mdlbln.o: /usr/include/sys/_types/_intptr_t.h
c_mdlbln.o: /usr/include/sys/_types/_uintptr_t.h
c_mdlbln.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdlbln.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdlbln.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdlbln.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdlbln.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdlbln.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdlbln.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdlbln.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdlbln.o: /usr/include/sys/_types/_u_char.h
c_mdlbln.o: /usr/include/sys/_types/_u_short.h
c_mdlbln.o: /usr/include/sys/_types/_u_int.h
c_mdlbln.o: /usr/include/sys/_types/_caddr_t.h
c_mdlbln.o: /usr/include/sys/_types/_dev_t.h
c_mdlbln.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdlbln.o: /usr/include/sys/_types/_blksize_t.h
c_mdlbln.o: /usr/include/sys/_types/_gid_t.h
c_mdlbln.o: /usr/include/sys/_types/_in_addr_t.h
c_mdlbln.o: /usr/include/sys/_types/_in_port_t.h
c_mdlbln.o: /usr/include/sys/_types/_ino_t.h
c_mdlbln.o: /usr/include/sys/_types/_ino64_t.h
c_mdlbln.o: /usr/include/sys/_types/_key_t.h
c_mdlbln.o: /usr/include/sys/_types/_mode_t.h
c_mdlbln.o: /usr/include/sys/_types/_nlink_t.h
c_mdlbln.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdlbln.o: /usr/include/sys/_types/_uid_t.h
c_mdlbln.o: /usr/include/sys/_types/_clock_t.h
c_mdlbln.o: /usr/include/sys/_types/_time_t.h
c_mdlbln.o: /usr/include/sys/_types/_useconds_t.h
c_mdlbln.o: /usr/include/sys/_types/_suseconds_t.h
c_mdlbln.o: /usr/include/sys/_types/_rsize_t.h
c_mdlbln.o: /usr/include/sys/_types/_errno_t.h
c_mdlbln.o: /usr/include/sys/_types/_fd_def.h
c_mdlbln.o: /usr/include/sys/_types/_fd_setsize.h
c_mdlbln.o: /usr/include/sys/_types/_fd_set.h
c_mdlbln.o: /usr/include/sys/_types/_fd_clr.h
c_mdlbln.o: /usr/include/sys/_types/_fd_zero.h
c_mdlbln.o: /usr/include/sys/_types/_fd_isset.h
c_mdlbln.o: /usr/include/sys/_types/_fd_copy.h
c_mdlbln.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdlbln.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdlbln.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdlbln.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdlbln.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdlbln.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdlbln.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdlbln.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdlbln.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdlbln.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdlbln.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdlbln.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdlbln.o: ../../../.././include/ncarg/c.h
c_mdlblt.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdlblt.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdlblt.o: /usr/include/sys/_posix_availability.h
c_mdlblt.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdlblt.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdlblt.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdlblt.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdlblt.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdlblt.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdlblt.o: /usr/include/sys/_types/_int16_t.h
c_mdlblt.o: /usr/include/sys/_types/_int32_t.h
c_mdlblt.o: /usr/include/sys/_types/_int64_t.h
c_mdlblt.o: /usr/include/sys/_types/_u_int8_t.h
c_mdlblt.o: /usr/include/sys/_types/_u_int16_t.h
c_mdlblt.o: /usr/include/sys/_types/_u_int32_t.h
c_mdlblt.o: /usr/include/sys/_types/_u_int64_t.h
c_mdlblt.o: /usr/include/sys/_types/_intptr_t.h
c_mdlblt.o: /usr/include/sys/_types/_uintptr_t.h
c_mdlblt.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdlblt.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdlblt.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdlblt.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdlblt.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdlblt.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdlblt.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdlblt.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdlblt.o: /usr/include/sys/_types/_u_char.h
c_mdlblt.o: /usr/include/sys/_types/_u_short.h
c_mdlblt.o: /usr/include/sys/_types/_u_int.h
c_mdlblt.o: /usr/include/sys/_types/_caddr_t.h
c_mdlblt.o: /usr/include/sys/_types/_dev_t.h
c_mdlblt.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdlblt.o: /usr/include/sys/_types/_blksize_t.h
c_mdlblt.o: /usr/include/sys/_types/_gid_t.h
c_mdlblt.o: /usr/include/sys/_types/_in_addr_t.h
c_mdlblt.o: /usr/include/sys/_types/_in_port_t.h
c_mdlblt.o: /usr/include/sys/_types/_ino_t.h
c_mdlblt.o: /usr/include/sys/_types/_ino64_t.h
c_mdlblt.o: /usr/include/sys/_types/_key_t.h
c_mdlblt.o: /usr/include/sys/_types/_mode_t.h
c_mdlblt.o: /usr/include/sys/_types/_nlink_t.h
c_mdlblt.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdlblt.o: /usr/include/sys/_types/_uid_t.h
c_mdlblt.o: /usr/include/sys/_types/_clock_t.h
c_mdlblt.o: /usr/include/sys/_types/_time_t.h
c_mdlblt.o: /usr/include/sys/_types/_useconds_t.h
c_mdlblt.o: /usr/include/sys/_types/_suseconds_t.h
c_mdlblt.o: /usr/include/sys/_types/_rsize_t.h
c_mdlblt.o: /usr/include/sys/_types/_errno_t.h
c_mdlblt.o: /usr/include/sys/_types/_fd_def.h
c_mdlblt.o: /usr/include/sys/_types/_fd_setsize.h
c_mdlblt.o: /usr/include/sys/_types/_fd_set.h
c_mdlblt.o: /usr/include/sys/_types/_fd_clr.h
c_mdlblt.o: /usr/include/sys/_types/_fd_zero.h
c_mdlblt.o: /usr/include/sys/_types/_fd_isset.h
c_mdlblt.o: /usr/include/sys/_types/_fd_copy.h
c_mdlblt.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdlblt.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdlblt.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdlblt.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdlblt.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdlblt.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdlblt.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdlblt.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdlblt.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdlblt.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdlblt.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdlblt.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdlblt.o: ../../../.././include/ncarg/c.h
c_mdrggc.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdrggc.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdrggc.o: /usr/include/sys/_posix_availability.h
c_mdrggc.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdrggc.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdrggc.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdrggc.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdrggc.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdrggc.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdrggc.o: /usr/include/sys/_types/_int16_t.h
c_mdrggc.o: /usr/include/sys/_types/_int32_t.h
c_mdrggc.o: /usr/include/sys/_types/_int64_t.h
c_mdrggc.o: /usr/include/sys/_types/_u_int8_t.h
c_mdrggc.o: /usr/include/sys/_types/_u_int16_t.h
c_mdrggc.o: /usr/include/sys/_types/_u_int32_t.h
c_mdrggc.o: /usr/include/sys/_types/_u_int64_t.h
c_mdrggc.o: /usr/include/sys/_types/_intptr_t.h
c_mdrggc.o: /usr/include/sys/_types/_uintptr_t.h
c_mdrggc.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdrggc.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdrggc.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdrggc.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdrggc.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdrggc.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdrggc.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdrggc.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdrggc.o: /usr/include/sys/_types/_u_char.h
c_mdrggc.o: /usr/include/sys/_types/_u_short.h
c_mdrggc.o: /usr/include/sys/_types/_u_int.h
c_mdrggc.o: /usr/include/sys/_types/_caddr_t.h
c_mdrggc.o: /usr/include/sys/_types/_dev_t.h
c_mdrggc.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdrggc.o: /usr/include/sys/_types/_blksize_t.h
c_mdrggc.o: /usr/include/sys/_types/_gid_t.h
c_mdrggc.o: /usr/include/sys/_types/_in_addr_t.h
c_mdrggc.o: /usr/include/sys/_types/_in_port_t.h
c_mdrggc.o: /usr/include/sys/_types/_ino_t.h
c_mdrggc.o: /usr/include/sys/_types/_ino64_t.h
c_mdrggc.o: /usr/include/sys/_types/_key_t.h
c_mdrggc.o: /usr/include/sys/_types/_mode_t.h
c_mdrggc.o: /usr/include/sys/_types/_nlink_t.h
c_mdrggc.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdrggc.o: /usr/include/sys/_types/_uid_t.h
c_mdrggc.o: /usr/include/sys/_types/_clock_t.h
c_mdrggc.o: /usr/include/sys/_types/_time_t.h
c_mdrggc.o: /usr/include/sys/_types/_useconds_t.h
c_mdrggc.o: /usr/include/sys/_types/_suseconds_t.h
c_mdrggc.o: /usr/include/sys/_types/_rsize_t.h
c_mdrggc.o: /usr/include/sys/_types/_errno_t.h
c_mdrggc.o: /usr/include/sys/_types/_fd_def.h
c_mdrggc.o: /usr/include/sys/_types/_fd_setsize.h
c_mdrggc.o: /usr/include/sys/_types/_fd_set.h
c_mdrggc.o: /usr/include/sys/_types/_fd_clr.h
c_mdrggc.o: /usr/include/sys/_types/_fd_zero.h
c_mdrggc.o: /usr/include/sys/_types/_fd_isset.h
c_mdrggc.o: /usr/include/sys/_types/_fd_copy.h
c_mdrggc.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdrggc.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdrggc.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdrggc.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdrggc.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdrggc.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdrggc.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdrggc.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdrggc.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdrggc.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdrggc.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdrggc.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdrggc.o: ../../../.././include/ncarg/c.h
c_mdrgsc.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdrgsc.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdrgsc.o: /usr/include/sys/_posix_availability.h
c_mdrgsc.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdrgsc.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdrgsc.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdrgsc.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdrgsc.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdrgsc.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdrgsc.o: /usr/include/sys/_types/_int16_t.h
c_mdrgsc.o: /usr/include/sys/_types/_int32_t.h
c_mdrgsc.o: /usr/include/sys/_types/_int64_t.h
c_mdrgsc.o: /usr/include/sys/_types/_u_int8_t.h
c_mdrgsc.o: /usr/include/sys/_types/_u_int16_t.h
c_mdrgsc.o: /usr/include/sys/_types/_u_int32_t.h
c_mdrgsc.o: /usr/include/sys/_types/_u_int64_t.h
c_mdrgsc.o: /usr/include/sys/_types/_intptr_t.h
c_mdrgsc.o: /usr/include/sys/_types/_uintptr_t.h
c_mdrgsc.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdrgsc.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdrgsc.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdrgsc.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdrgsc.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdrgsc.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdrgsc.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdrgsc.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdrgsc.o: /usr/include/sys/_types/_u_char.h
c_mdrgsc.o: /usr/include/sys/_types/_u_short.h
c_mdrgsc.o: /usr/include/sys/_types/_u_int.h
c_mdrgsc.o: /usr/include/sys/_types/_caddr_t.h
c_mdrgsc.o: /usr/include/sys/_types/_dev_t.h
c_mdrgsc.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdrgsc.o: /usr/include/sys/_types/_blksize_t.h
c_mdrgsc.o: /usr/include/sys/_types/_gid_t.h
c_mdrgsc.o: /usr/include/sys/_types/_in_addr_t.h
c_mdrgsc.o: /usr/include/sys/_types/_in_port_t.h
c_mdrgsc.o: /usr/include/sys/_types/_ino_t.h
c_mdrgsc.o: /usr/include/sys/_types/_ino64_t.h
c_mdrgsc.o: /usr/include/sys/_types/_key_t.h
c_mdrgsc.o: /usr/include/sys/_types/_mode_t.h
c_mdrgsc.o: /usr/include/sys/_types/_nlink_t.h
c_mdrgsc.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdrgsc.o: /usr/include/sys/_types/_uid_t.h
c_mdrgsc.o: /usr/include/sys/_types/_clock_t.h
c_mdrgsc.o: /usr/include/sys/_types/_time_t.h
c_mdrgsc.o: /usr/include/sys/_types/_useconds_t.h
c_mdrgsc.o: /usr/include/sys/_types/_suseconds_t.h
c_mdrgsc.o: /usr/include/sys/_types/_rsize_t.h
c_mdrgsc.o: /usr/include/sys/_types/_errno_t.h
c_mdrgsc.o: /usr/include/sys/_types/_fd_def.h
c_mdrgsc.o: /usr/include/sys/_types/_fd_setsize.h
c_mdrgsc.o: /usr/include/sys/_types/_fd_set.h
c_mdrgsc.o: /usr/include/sys/_types/_fd_clr.h
c_mdrgsc.o: /usr/include/sys/_types/_fd_zero.h
c_mdrgsc.o: /usr/include/sys/_types/_fd_isset.h
c_mdrgsc.o: /usr/include/sys/_types/_fd_copy.h
c_mdrgsc.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdrgsc.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdrgsc.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdrgsc.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdrgsc.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdrgsc.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdrgsc.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdrgsc.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdrgsc.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdrgsc.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdrgsc.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdrgsc.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdrgsc.o: ../../../.././include/ncarg/c.h
c_mdritd.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdritd.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdritd.o: /usr/include/sys/_posix_availability.h
c_mdritd.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdritd.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdritd.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdritd.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdritd.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdritd.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdritd.o: /usr/include/sys/_types/_int16_t.h
c_mdritd.o: /usr/include/sys/_types/_int32_t.h
c_mdritd.o: /usr/include/sys/_types/_int64_t.h
c_mdritd.o: /usr/include/sys/_types/_u_int8_t.h
c_mdritd.o: /usr/include/sys/_types/_u_int16_t.h
c_mdritd.o: /usr/include/sys/_types/_u_int32_t.h
c_mdritd.o: /usr/include/sys/_types/_u_int64_t.h
c_mdritd.o: /usr/include/sys/_types/_intptr_t.h
c_mdritd.o: /usr/include/sys/_types/_uintptr_t.h
c_mdritd.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdritd.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdritd.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdritd.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdritd.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdritd.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdritd.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdritd.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdritd.o: /usr/include/sys/_types/_u_char.h
c_mdritd.o: /usr/include/sys/_types/_u_short.h
c_mdritd.o: /usr/include/sys/_types/_u_int.h
c_mdritd.o: /usr/include/sys/_types/_caddr_t.h
c_mdritd.o: /usr/include/sys/_types/_dev_t.h
c_mdritd.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdritd.o: /usr/include/sys/_types/_blksize_t.h
c_mdritd.o: /usr/include/sys/_types/_gid_t.h
c_mdritd.o: /usr/include/sys/_types/_in_addr_t.h
c_mdritd.o: /usr/include/sys/_types/_in_port_t.h
c_mdritd.o: /usr/include/sys/_types/_ino_t.h
c_mdritd.o: /usr/include/sys/_types/_ino64_t.h
c_mdritd.o: /usr/include/sys/_types/_key_t.h
c_mdritd.o: /usr/include/sys/_types/_mode_t.h
c_mdritd.o: /usr/include/sys/_types/_nlink_t.h
c_mdritd.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdritd.o: /usr/include/sys/_types/_uid_t.h
c_mdritd.o: /usr/include/sys/_types/_clock_t.h
c_mdritd.o: /usr/include/sys/_types/_time_t.h
c_mdritd.o: /usr/include/sys/_types/_useconds_t.h
c_mdritd.o: /usr/include/sys/_types/_suseconds_t.h
c_mdritd.o: /usr/include/sys/_types/_rsize_t.h
c_mdritd.o: /usr/include/sys/_types/_errno_t.h
c_mdritd.o: /usr/include/sys/_types/_fd_def.h
c_mdritd.o: /usr/include/sys/_types/_fd_setsize.h
c_mdritd.o: /usr/include/sys/_types/_fd_set.h
c_mdritd.o: /usr/include/sys/_types/_fd_clr.h
c_mdritd.o: /usr/include/sys/_types/_fd_zero.h
c_mdritd.o: /usr/include/sys/_types/_fd_isset.h
c_mdritd.o: /usr/include/sys/_types/_fd_copy.h
c_mdritd.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdritd.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdritd.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdritd.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdritd.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdritd.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdritd.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdritd.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdritd.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdritd.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdritd.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdritd.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdritd.o: ../../../.././include/ncarg/c.h
c_mdutfd.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdutfd.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdutfd.o: /usr/include/sys/_posix_availability.h
c_mdutfd.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdutfd.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdutfd.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdutfd.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdutfd.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdutfd.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdutfd.o: /usr/include/sys/_types/_int16_t.h
c_mdutfd.o: /usr/include/sys/_types/_int32_t.h
c_mdutfd.o: /usr/include/sys/_types/_int64_t.h
c_mdutfd.o: /usr/include/sys/_types/_u_int8_t.h
c_mdutfd.o: /usr/include/sys/_types/_u_int16_t.h
c_mdutfd.o: /usr/include/sys/_types/_u_int32_t.h
c_mdutfd.o: /usr/include/sys/_types/_u_int64_t.h
c_mdutfd.o: /usr/include/sys/_types/_intptr_t.h
c_mdutfd.o: /usr/include/sys/_types/_uintptr_t.h
c_mdutfd.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdutfd.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdutfd.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdutfd.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdutfd.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdutfd.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdutfd.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdutfd.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdutfd.o: /usr/include/sys/_types/_u_char.h
c_mdutfd.o: /usr/include/sys/_types/_u_short.h
c_mdutfd.o: /usr/include/sys/_types/_u_int.h
c_mdutfd.o: /usr/include/sys/_types/_caddr_t.h
c_mdutfd.o: /usr/include/sys/_types/_dev_t.h
c_mdutfd.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdutfd.o: /usr/include/sys/_types/_blksize_t.h
c_mdutfd.o: /usr/include/sys/_types/_gid_t.h
c_mdutfd.o: /usr/include/sys/_types/_in_addr_t.h
c_mdutfd.o: /usr/include/sys/_types/_in_port_t.h
c_mdutfd.o: /usr/include/sys/_types/_ino_t.h
c_mdutfd.o: /usr/include/sys/_types/_ino64_t.h
c_mdutfd.o: /usr/include/sys/_types/_key_t.h
c_mdutfd.o: /usr/include/sys/_types/_mode_t.h
c_mdutfd.o: /usr/include/sys/_types/_nlink_t.h
c_mdutfd.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdutfd.o: /usr/include/sys/_types/_uid_t.h
c_mdutfd.o: /usr/include/sys/_types/_clock_t.h
c_mdutfd.o: /usr/include/sys/_types/_time_t.h
c_mdutfd.o: /usr/include/sys/_types/_useconds_t.h
c_mdutfd.o: /usr/include/sys/_types/_suseconds_t.h
c_mdutfd.o: /usr/include/sys/_types/_rsize_t.h
c_mdutfd.o: /usr/include/sys/_types/_errno_t.h
c_mdutfd.o: /usr/include/sys/_types/_fd_def.h
c_mdutfd.o: /usr/include/sys/_types/_fd_setsize.h
c_mdutfd.o: /usr/include/sys/_types/_fd_set.h
c_mdutfd.o: /usr/include/sys/_types/_fd_clr.h
c_mdutfd.o: /usr/include/sys/_types/_fd_zero.h
c_mdutfd.o: /usr/include/sys/_types/_fd_isset.h
c_mdutfd.o: /usr/include/sys/_types/_fd_copy.h
c_mdutfd.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdutfd.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdutfd.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdutfd.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdutfd.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdutfd.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdutfd.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdutfd.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdutfd.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdutfd.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdutfd.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdutfd.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdutfd.o: ../../../.././include/ncarg/c.h
c_mdutfs.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdutfs.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdutfs.o: /usr/include/sys/_posix_availability.h
c_mdutfs.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdutfs.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdutfs.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdutfs.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdutfs.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdutfs.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdutfs.o: /usr/include/sys/_types/_int16_t.h
c_mdutfs.o: /usr/include/sys/_types/_int32_t.h
c_mdutfs.o: /usr/include/sys/_types/_int64_t.h
c_mdutfs.o: /usr/include/sys/_types/_u_int8_t.h
c_mdutfs.o: /usr/include/sys/_types/_u_int16_t.h
c_mdutfs.o: /usr/include/sys/_types/_u_int32_t.h
c_mdutfs.o: /usr/include/sys/_types/_u_int64_t.h
c_mdutfs.o: /usr/include/sys/_types/_intptr_t.h
c_mdutfs.o: /usr/include/sys/_types/_uintptr_t.h
c_mdutfs.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdutfs.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdutfs.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdutfs.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdutfs.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdutfs.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdutfs.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdutfs.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdutfs.o: /usr/include/sys/_types/_u_char.h
c_mdutfs.o: /usr/include/sys/_types/_u_short.h
c_mdutfs.o: /usr/include/sys/_types/_u_int.h
c_mdutfs.o: /usr/include/sys/_types/_caddr_t.h
c_mdutfs.o: /usr/include/sys/_types/_dev_t.h
c_mdutfs.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdutfs.o: /usr/include/sys/_types/_blksize_t.h
c_mdutfs.o: /usr/include/sys/_types/_gid_t.h
c_mdutfs.o: /usr/include/sys/_types/_in_addr_t.h
c_mdutfs.o: /usr/include/sys/_types/_in_port_t.h
c_mdutfs.o: /usr/include/sys/_types/_ino_t.h
c_mdutfs.o: /usr/include/sys/_types/_ino64_t.h
c_mdutfs.o: /usr/include/sys/_types/_key_t.h
c_mdutfs.o: /usr/include/sys/_types/_mode_t.h
c_mdutfs.o: /usr/include/sys/_types/_nlink_t.h
c_mdutfs.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdutfs.o: /usr/include/sys/_types/_uid_t.h
c_mdutfs.o: /usr/include/sys/_types/_clock_t.h
c_mdutfs.o: /usr/include/sys/_types/_time_t.h
c_mdutfs.o: /usr/include/sys/_types/_useconds_t.h
c_mdutfs.o: /usr/include/sys/_types/_suseconds_t.h
c_mdutfs.o: /usr/include/sys/_types/_rsize_t.h
c_mdutfs.o: /usr/include/sys/_types/_errno_t.h
c_mdutfs.o: /usr/include/sys/_types/_fd_def.h
c_mdutfs.o: /usr/include/sys/_types/_fd_setsize.h
c_mdutfs.o: /usr/include/sys/_types/_fd_set.h
c_mdutfs.o: /usr/include/sys/_types/_fd_clr.h
c_mdutfs.o: /usr/include/sys/_types/_fd_zero.h
c_mdutfs.o: /usr/include/sys/_types/_fd_isset.h
c_mdutfs.o: /usr/include/sys/_types/_fd_copy.h
c_mdutfs.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdutfs.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdutfs.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdutfs.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdutfs.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdutfs.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdutfs.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdutfs.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdutfs.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdutfs.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdutfs.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdutfs.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdutfs.o: ../../../.././include/ncarg/c.h
c_mdutid.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdutid.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdutid.o: /usr/include/sys/_posix_availability.h
c_mdutid.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdutid.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdutid.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdutid.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdutid.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdutid.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdutid.o: /usr/include/sys/_types/_int16_t.h
c_mdutid.o: /usr/include/sys/_types/_int32_t.h
c_mdutid.o: /usr/include/sys/_types/_int64_t.h
c_mdutid.o: /usr/include/sys/_types/_u_int8_t.h
c_mdutid.o: /usr/include/sys/_types/_u_int16_t.h
c_mdutid.o: /usr/include/sys/_types/_u_int32_t.h
c_mdutid.o: /usr/include/sys/_types/_u_int64_t.h
c_mdutid.o: /usr/include/sys/_types/_intptr_t.h
c_mdutid.o: /usr/include/sys/_types/_uintptr_t.h
c_mdutid.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdutid.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdutid.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdutid.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdutid.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdutid.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdutid.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdutid.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdutid.o: /usr/include/sys/_types/_u_char.h
c_mdutid.o: /usr/include/sys/_types/_u_short.h
c_mdutid.o: /usr/include/sys/_types/_u_int.h
c_mdutid.o: /usr/include/sys/_types/_caddr_t.h
c_mdutid.o: /usr/include/sys/_types/_dev_t.h
c_mdutid.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdutid.o: /usr/include/sys/_types/_blksize_t.h
c_mdutid.o: /usr/include/sys/_types/_gid_t.h
c_mdutid.o: /usr/include/sys/_types/_in_addr_t.h
c_mdutid.o: /usr/include/sys/_types/_in_port_t.h
c_mdutid.o: /usr/include/sys/_types/_ino_t.h
c_mdutid.o: /usr/include/sys/_types/_ino64_t.h
c_mdutid.o: /usr/include/sys/_types/_key_t.h
c_mdutid.o: /usr/include/sys/_types/_mode_t.h
c_mdutid.o: /usr/include/sys/_types/_nlink_t.h
c_mdutid.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdutid.o: /usr/include/sys/_types/_uid_t.h
c_mdutid.o: /usr/include/sys/_types/_clock_t.h
c_mdutid.o: /usr/include/sys/_types/_time_t.h
c_mdutid.o: /usr/include/sys/_types/_useconds_t.h
c_mdutid.o: /usr/include/sys/_types/_suseconds_t.h
c_mdutid.o: /usr/include/sys/_types/_rsize_t.h
c_mdutid.o: /usr/include/sys/_types/_errno_t.h
c_mdutid.o: /usr/include/sys/_types/_fd_def.h
c_mdutid.o: /usr/include/sys/_types/_fd_setsize.h
c_mdutid.o: /usr/include/sys/_types/_fd_set.h
c_mdutid.o: /usr/include/sys/_types/_fd_clr.h
c_mdutid.o: /usr/include/sys/_types/_fd_zero.h
c_mdutid.o: /usr/include/sys/_types/_fd_isset.h
c_mdutid.o: /usr/include/sys/_types/_fd_copy.h
c_mdutid.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdutid.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdutid.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdutid.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdutid.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdutid.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdutid.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdutid.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdutid.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdutid.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdutid.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdutid.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdutid.o: ../../../.././include/ncarg/c.h
c_mdutis.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdutis.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdutis.o: /usr/include/sys/_posix_availability.h
c_mdutis.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdutis.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdutis.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdutis.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdutis.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdutis.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdutis.o: /usr/include/sys/_types/_int16_t.h
c_mdutis.o: /usr/include/sys/_types/_int32_t.h
c_mdutis.o: /usr/include/sys/_types/_int64_t.h
c_mdutis.o: /usr/include/sys/_types/_u_int8_t.h
c_mdutis.o: /usr/include/sys/_types/_u_int16_t.h
c_mdutis.o: /usr/include/sys/_types/_u_int32_t.h
c_mdutis.o: /usr/include/sys/_types/_u_int64_t.h
c_mdutis.o: /usr/include/sys/_types/_intptr_t.h
c_mdutis.o: /usr/include/sys/_types/_uintptr_t.h
c_mdutis.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdutis.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdutis.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdutis.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdutis.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdutis.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdutis.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdutis.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdutis.o: /usr/include/sys/_types/_u_char.h
c_mdutis.o: /usr/include/sys/_types/_u_short.h
c_mdutis.o: /usr/include/sys/_types/_u_int.h
c_mdutis.o: /usr/include/sys/_types/_caddr_t.h
c_mdutis.o: /usr/include/sys/_types/_dev_t.h
c_mdutis.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdutis.o: /usr/include/sys/_types/_blksize_t.h
c_mdutis.o: /usr/include/sys/_types/_gid_t.h
c_mdutis.o: /usr/include/sys/_types/_in_addr_t.h
c_mdutis.o: /usr/include/sys/_types/_in_port_t.h
c_mdutis.o: /usr/include/sys/_types/_ino_t.h
c_mdutis.o: /usr/include/sys/_types/_ino64_t.h
c_mdutis.o: /usr/include/sys/_types/_key_t.h
c_mdutis.o: /usr/include/sys/_types/_mode_t.h
c_mdutis.o: /usr/include/sys/_types/_nlink_t.h
c_mdutis.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdutis.o: /usr/include/sys/_types/_uid_t.h
c_mdutis.o: /usr/include/sys/_types/_clock_t.h
c_mdutis.o: /usr/include/sys/_types/_time_t.h
c_mdutis.o: /usr/include/sys/_types/_useconds_t.h
c_mdutis.o: /usr/include/sys/_types/_suseconds_t.h
c_mdutis.o: /usr/include/sys/_types/_rsize_t.h
c_mdutis.o: /usr/include/sys/_types/_errno_t.h
c_mdutis.o: /usr/include/sys/_types/_fd_def.h
c_mdutis.o: /usr/include/sys/_types/_fd_setsize.h
c_mdutis.o: /usr/include/sys/_types/_fd_set.h
c_mdutis.o: /usr/include/sys/_types/_fd_clr.h
c_mdutis.o: /usr/include/sys/_types/_fd_zero.h
c_mdutis.o: /usr/include/sys/_types/_fd_isset.h
c_mdutis.o: /usr/include/sys/_types/_fd_copy.h
c_mdutis.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdutis.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdutis.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdutis.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdutis.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdutis.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdutis.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdutis.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdutis.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdutis.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdutis.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdutis.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdutis.o: ../../../.././include/ncarg/c.h
c_mputfd.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mputfd.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mputfd.o: /usr/include/sys/_posix_availability.h
c_mputfd.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mputfd.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mputfd.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mputfd.o: /usr/include/sys/_pthread/_pthread_types.h
c_mputfd.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mputfd.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mputfd.o: /usr/include/sys/_types/_int16_t.h
c_mputfd.o: /usr/include/sys/_types/_int32_t.h
c_mputfd.o: /usr/include/sys/_types/_int64_t.h
c_mputfd.o: /usr/include/sys/_types/_u_int8_t.h
c_mputfd.o: /usr/include/sys/_types/_u_int16_t.h
c_mputfd.o: /usr/include/sys/_types/_u_int32_t.h
c_mputfd.o: /usr/include/sys/_types/_u_int64_t.h
c_mputfd.o: /usr/include/sys/_types/_intptr_t.h
c_mputfd.o: /usr/include/sys/_types/_uintptr_t.h
c_mputfd.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mputfd.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mputfd.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mputfd.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mputfd.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mputfd.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mputfd.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mputfd.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mputfd.o: /usr/include/sys/_types/_u_char.h
c_mputfd.o: /usr/include/sys/_types/_u_short.h
c_mputfd.o: /usr/include/sys/_types/_u_int.h
c_mputfd.o: /usr/include/sys/_types/_caddr_t.h
c_mputfd.o: /usr/include/sys/_types/_dev_t.h
c_mputfd.o: /usr/include/sys/_types/_blkcnt_t.h
c_mputfd.o: /usr/include/sys/_types/_blksize_t.h
c_mputfd.o: /usr/include/sys/_types/_gid_t.h
c_mputfd.o: /usr/include/sys/_types/_in_addr_t.h
c_mputfd.o: /usr/include/sys/_types/_in_port_t.h
c_mputfd.o: /usr/include/sys/_types/_ino_t.h
c_mputfd.o: /usr/include/sys/_types/_ino64_t.h
c_mputfd.o: /usr/include/sys/_types/_key_t.h
c_mputfd.o: /usr/include/sys/_types/_mode_t.h
c_mputfd.o: /usr/include/sys/_types/_nlink_t.h
c_mputfd.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mputfd.o: /usr/include/sys/_types/_uid_t.h
c_mputfd.o: /usr/include/sys/_types/_clock_t.h
c_mputfd.o: /usr/include/sys/_types/_time_t.h
c_mputfd.o: /usr/include/sys/_types/_useconds_t.h
c_mputfd.o: /usr/include/sys/_types/_suseconds_t.h
c_mputfd.o: /usr/include/sys/_types/_rsize_t.h
c_mputfd.o: /usr/include/sys/_types/_errno_t.h
c_mputfd.o: /usr/include/sys/_types/_fd_def.h
c_mputfd.o: /usr/include/sys/_types/_fd_setsize.h
c_mputfd.o: /usr/include/sys/_types/_fd_set.h
c_mputfd.o: /usr/include/sys/_types/_fd_clr.h
c_mputfd.o: /usr/include/sys/_types/_fd_zero.h
c_mputfd.o: /usr/include/sys/_types/_fd_isset.h
c_mputfd.o: /usr/include/sys/_types/_fd_copy.h
c_mputfd.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mputfd.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mputfd.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mputfd.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mputfd.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mputfd.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mputfd.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mputfd.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mputfd.o: /usr/include/sys/_pthread/_pthread_t.h
c_mputfd.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mputfd.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mputfd.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mputfd.o: ../../../.././include/ncarg/c.h
c_mputfs.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mputfs.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mputfs.o: /usr/include/sys/_posix_availability.h
c_mputfs.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mputfs.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mputfs.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mputfs.o: /usr/include/sys/_pthread/_pthread_types.h
c_mputfs.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mputfs.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mputfs.o: /usr/include/sys/_types/_int16_t.h
c_mputfs.o: /usr/include/sys/_types/_int32_t.h
c_mputfs.o: /usr/include/sys/_types/_int64_t.h
c_mputfs.o: /usr/include/sys/_types/_u_int8_t.h
c_mputfs.o: /usr/include/sys/_types/_u_int16_t.h
c_mputfs.o: /usr/include/sys/_types/_u_int32_t.h
c_mputfs.o: /usr/include/sys/_types/_u_int64_t.h
c_mputfs.o: /usr/include/sys/_types/_intptr_t.h
c_mputfs.o: /usr/include/sys/_types/_uintptr_t.h
c_mputfs.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mputfs.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mputfs.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mputfs.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mputfs.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mputfs.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mputfs.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mputfs.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mputfs.o: /usr/include/sys/_types/_u_char.h
c_mputfs.o: /usr/include/sys/_types/_u_short.h
c_mputfs.o: /usr/include/sys/_types/_u_int.h
c_mputfs.o: /usr/include/sys/_types/_caddr_t.h
c_mputfs.o: /usr/include/sys/_types/_dev_t.h
c_mputfs.o: /usr/include/sys/_types/_blkcnt_t.h
c_mputfs.o: /usr/include/sys/_types/_blksize_t.h
c_mputfs.o: /usr/include/sys/_types/_gid_t.h
c_mputfs.o: /usr/include/sys/_types/_in_addr_t.h
c_mputfs.o: /usr/include/sys/_types/_in_port_t.h
c_mputfs.o: /usr/include/sys/_types/_ino_t.h
c_mputfs.o: /usr/include/sys/_types/_ino64_t.h
c_mputfs.o: /usr/include/sys/_types/_key_t.h
c_mputfs.o: /usr/include/sys/_types/_mode_t.h
c_mputfs.o: /usr/include/sys/_types/_nlink_t.h
c_mputfs.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mputfs.o: /usr/include/sys/_types/_uid_t.h
c_mputfs.o: /usr/include/sys/_types/_clock_t.h
c_mputfs.o: /usr/include/sys/_types/_time_t.h
c_mputfs.o: /usr/include/sys/_types/_useconds_t.h
c_mputfs.o: /usr/include/sys/_types/_suseconds_t.h
c_mputfs.o: /usr/include/sys/_types/_rsize_t.h
c_mputfs.o: /usr/include/sys/_types/_errno_t.h
c_mputfs.o: /usr/include/sys/_types/_fd_def.h
c_mputfs.o: /usr/include/sys/_types/_fd_setsize.h
c_mputfs.o: /usr/include/sys/_types/_fd_set.h
c_mputfs.o: /usr/include/sys/_types/_fd_clr.h
c_mputfs.o: /usr/include/sys/_types/_fd_zero.h
c_mputfs.o: /usr/include/sys/_types/_fd_isset.h
c_mputfs.o: /usr/include/sys/_types/_fd_copy.h
c_mputfs.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mputfs.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mputfs.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mputfs.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mputfs.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mputfs.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mputfs.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mputfs.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mputfs.o: /usr/include/sys/_pthread/_pthread_t.h
c_mputfs.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mputfs.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mputfs.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mputfs.o: ../../../.././include/ncarg/c.h
c_mputid.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mputid.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mputid.o: /usr/include/sys/_posix_availability.h
c_mputid.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mputid.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mputid.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mputid.o: /usr/include/sys/_pthread/_pthread_types.h
c_mputid.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mputid.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mputid.o: /usr/include/sys/_types/_int16_t.h
c_mputid.o: /usr/include/sys/_types/_int32_t.h
c_mputid.o: /usr/include/sys/_types/_int64_t.h
c_mputid.o: /usr/include/sys/_types/_u_int8_t.h
c_mputid.o: /usr/include/sys/_types/_u_int16_t.h
c_mputid.o: /usr/include/sys/_types/_u_int32_t.h
c_mputid.o: /usr/include/sys/_types/_u_int64_t.h
c_mputid.o: /usr/include/sys/_types/_intptr_t.h
c_mputid.o: /usr/include/sys/_types/_uintptr_t.h
c_mputid.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mputid.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mputid.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mputid.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mputid.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mputid.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mputid.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mputid.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mputid.o: /usr/include/sys/_types/_u_char.h
c_mputid.o: /usr/include/sys/_types/_u_short.h
c_mputid.o: /usr/include/sys/_types/_u_int.h
c_mputid.o: /usr/include/sys/_types/_caddr_t.h
c_mputid.o: /usr/include/sys/_types/_dev_t.h
c_mputid.o: /usr/include/sys/_types/_blkcnt_t.h
c_mputid.o: /usr/include/sys/_types/_blksize_t.h
c_mputid.o: /usr/include/sys/_types/_gid_t.h
c_mputid.o: /usr/include/sys/_types/_in_addr_t.h
c_mputid.o: /usr/include/sys/_types/_in_port_t.h
c_mputid.o: /usr/include/sys/_types/_ino_t.h
c_mputid.o: /usr/include/sys/_types/_ino64_t.h
c_mputid.o: /usr/include/sys/_types/_key_t.h
c_mputid.o: /usr/include/sys/_types/_mode_t.h
c_mputid.o: /usr/include/sys/_types/_nlink_t.h
c_mputid.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mputid.o: /usr/include/sys/_types/_uid_t.h
c_mputid.o: /usr/include/sys/_types/_clock_t.h
c_mputid.o: /usr/include/sys/_types/_time_t.h
c_mputid.o: /usr/include/sys/_types/_useconds_t.h
c_mputid.o: /usr/include/sys/_types/_suseconds_t.h
c_mputid.o: /usr/include/sys/_types/_rsize_t.h
c_mputid.o: /usr/include/sys/_types/_errno_t.h
c_mputid.o: /usr/include/sys/_types/_fd_def.h
c_mputid.o: /usr/include/sys/_types/_fd_setsize.h
c_mputid.o: /usr/include/sys/_types/_fd_set.h
c_mputid.o: /usr/include/sys/_types/_fd_clr.h
c_mputid.o: /usr/include/sys/_types/_fd_zero.h
c_mputid.o: /usr/include/sys/_types/_fd_isset.h
c_mputid.o: /usr/include/sys/_types/_fd_copy.h
c_mputid.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mputid.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mputid.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mputid.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mputid.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mputid.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mputid.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mputid.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mputid.o: /usr/include/sys/_pthread/_pthread_t.h
c_mputid.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mputid.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mputid.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mputid.o: ../../../.././include/ncarg/c.h
c_mputis.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mputis.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mputis.o: /usr/include/sys/_posix_availability.h
c_mputis.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mputis.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mputis.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mputis.o: /usr/include/sys/_pthread/_pthread_types.h
c_mputis.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mputis.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mputis.o: /usr/include/sys/_types/_int16_t.h
c_mputis.o: /usr/include/sys/_types/_int32_t.h
c_mputis.o: /usr/include/sys/_types/_int64_t.h
c_mputis.o: /usr/include/sys/_types/_u_int8_t.h
c_mputis.o: /usr/include/sys/_types/_u_int16_t.h
c_mputis.o: /usr/include/sys/_types/_u_int32_t.h
c_mputis.o: /usr/include/sys/_types/_u_int64_t.h
c_mputis.o: /usr/include/sys/_types/_intptr_t.h
c_mputis.o: /usr/include/sys/_types/_uintptr_t.h
c_mputis.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mputis.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mputis.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mputis.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mputis.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mputis.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mputis.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mputis.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mputis.o: /usr/include/sys/_types/_u_char.h
c_mputis.o: /usr/include/sys/_types/_u_short.h
c_mputis.o: /usr/include/sys/_types/_u_int.h
c_mputis.o: /usr/include/sys/_types/_caddr_t.h
c_mputis.o: /usr/include/sys/_types/_dev_t.h
c_mputis.o: /usr/include/sys/_types/_blkcnt_t.h
c_mputis.o: /usr/include/sys/_types/_blksize_t.h
c_mputis.o: /usr/include/sys/_types/_gid_t.h
c_mputis.o: /usr/include/sys/_types/_in_addr_t.h
c_mputis.o: /usr/include/sys/_types/_in_port_t.h
c_mputis.o: /usr/include/sys/_types/_ino_t.h
c_mputis.o: /usr/include/sys/_types/_ino64_t.h
c_mputis.o: /usr/include/sys/_types/_key_t.h
c_mputis.o: /usr/include/sys/_types/_mode_t.h
c_mputis.o: /usr/include/sys/_types/_nlink_t.h
c_mputis.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mputis.o: /usr/include/sys/_types/_uid_t.h
c_mputis.o: /usr/include/sys/_types/_clock_t.h
c_mputis.o: /usr/include/sys/_types/_time_t.h
c_mputis.o: /usr/include/sys/_types/_useconds_t.h
c_mputis.o: /usr/include/sys/_types/_suseconds_t.h
c_mputis.o: /usr/include/sys/_types/_rsize_t.h
c_mputis.o: /usr/include/sys/_types/_errno_t.h
c_mputis.o: /usr/include/sys/_types/_fd_def.h
c_mputis.o: /usr/include/sys/_types/_fd_setsize.h
c_mputis.o: /usr/include/sys/_types/_fd_set.h
c_mputis.o: /usr/include/sys/_types/_fd_clr.h
c_mputis.o: /usr/include/sys/_types/_fd_zero.h
c_mputis.o: /usr/include/sys/_types/_fd_isset.h
c_mputis.o: /usr/include/sys/_types/_fd_copy.h
c_mputis.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mputis.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mputis.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mputis.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mputis.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mputis.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mputis.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mputis.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mputis.o: /usr/include/sys/_pthread/_pthread_t.h
c_mputis.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mputis.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mputis.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mputis.o: ../../../.././include/ncarg/c.h
c_mdutin.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdutin.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdutin.o: /usr/include/sys/_posix_availability.h
c_mdutin.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdutin.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdutin.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdutin.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdutin.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdutin.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdutin.o: /usr/include/sys/_types/_int16_t.h
c_mdutin.o: /usr/include/sys/_types/_int32_t.h
c_mdutin.o: /usr/include/sys/_types/_int64_t.h
c_mdutin.o: /usr/include/sys/_types/_u_int8_t.h
c_mdutin.o: /usr/include/sys/_types/_u_int16_t.h
c_mdutin.o: /usr/include/sys/_types/_u_int32_t.h
c_mdutin.o: /usr/include/sys/_types/_u_int64_t.h
c_mdutin.o: /usr/include/sys/_types/_intptr_t.h
c_mdutin.o: /usr/include/sys/_types/_uintptr_t.h
c_mdutin.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdutin.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdutin.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdutin.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdutin.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdutin.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdutin.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdutin.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdutin.o: /usr/include/sys/_types/_u_char.h
c_mdutin.o: /usr/include/sys/_types/_u_short.h
c_mdutin.o: /usr/include/sys/_types/_u_int.h
c_mdutin.o: /usr/include/sys/_types/_caddr_t.h
c_mdutin.o: /usr/include/sys/_types/_dev_t.h
c_mdutin.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdutin.o: /usr/include/sys/_types/_blksize_t.h
c_mdutin.o: /usr/include/sys/_types/_gid_t.h
c_mdutin.o: /usr/include/sys/_types/_in_addr_t.h
c_mdutin.o: /usr/include/sys/_types/_in_port_t.h
c_mdutin.o: /usr/include/sys/_types/_ino_t.h
c_mdutin.o: /usr/include/sys/_types/_ino64_t.h
c_mdutin.o: /usr/include/sys/_types/_key_t.h
c_mdutin.o: /usr/include/sys/_types/_mode_t.h
c_mdutin.o: /usr/include/sys/_types/_nlink_t.h
c_mdutin.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdutin.o: /usr/include/sys/_types/_uid_t.h
c_mdutin.o: /usr/include/sys/_types/_clock_t.h
c_mdutin.o: /usr/include/sys/_types/_time_t.h
c_mdutin.o: /usr/include/sys/_types/_useconds_t.h
c_mdutin.o: /usr/include/sys/_types/_suseconds_t.h
c_mdutin.o: /usr/include/sys/_types/_rsize_t.h
c_mdutin.o: /usr/include/sys/_types/_errno_t.h
c_mdutin.o: /usr/include/sys/_types/_fd_def.h
c_mdutin.o: /usr/include/sys/_types/_fd_setsize.h
c_mdutin.o: /usr/include/sys/_types/_fd_set.h
c_mdutin.o: /usr/include/sys/_types/_fd_clr.h
c_mdutin.o: /usr/include/sys/_types/_fd_zero.h
c_mdutin.o: /usr/include/sys/_types/_fd_isset.h
c_mdutin.o: /usr/include/sys/_types/_fd_copy.h
c_mdutin.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdutin.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdutin.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdutin.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdutin.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdutin.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdutin.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdutin.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdutin.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdutin.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdutin.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdutin.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdutin.o: ../../../.././include/ncarg/c.h
c_mputin.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mputin.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mputin.o: /usr/include/sys/_posix_availability.h
c_mputin.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mputin.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mputin.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mputin.o: /usr/include/sys/_pthread/_pthread_types.h
c_mputin.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mputin.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mputin.o: /usr/include/sys/_types/_int16_t.h
c_mputin.o: /usr/include/sys/_types/_int32_t.h
c_mputin.o: /usr/include/sys/_types/_int64_t.h
c_mputin.o: /usr/include/sys/_types/_u_int8_t.h
c_mputin.o: /usr/include/sys/_types/_u_int16_t.h
c_mputin.o: /usr/include/sys/_types/_u_int32_t.h
c_mputin.o: /usr/include/sys/_types/_u_int64_t.h
c_mputin.o: /usr/include/sys/_types/_intptr_t.h
c_mputin.o: /usr/include/sys/_types/_uintptr_t.h
c_mputin.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mputin.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mputin.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mputin.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mputin.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mputin.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mputin.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mputin.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mputin.o: /usr/include/sys/_types/_u_char.h
c_mputin.o: /usr/include/sys/_types/_u_short.h
c_mputin.o: /usr/include/sys/_types/_u_int.h
c_mputin.o: /usr/include/sys/_types/_caddr_t.h
c_mputin.o: /usr/include/sys/_types/_dev_t.h
c_mputin.o: /usr/include/sys/_types/_blkcnt_t.h
c_mputin.o: /usr/include/sys/_types/_blksize_t.h
c_mputin.o: /usr/include/sys/_types/_gid_t.h
c_mputin.o: /usr/include/sys/_types/_in_addr_t.h
c_mputin.o: /usr/include/sys/_types/_in_port_t.h
c_mputin.o: /usr/include/sys/_types/_ino_t.h
c_mputin.o: /usr/include/sys/_types/_ino64_t.h
c_mputin.o: /usr/include/sys/_types/_key_t.h
c_mputin.o: /usr/include/sys/_types/_mode_t.h
c_mputin.o: /usr/include/sys/_types/_nlink_t.h
c_mputin.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mputin.o: /usr/include/sys/_types/_uid_t.h
c_mputin.o: /usr/include/sys/_types/_clock_t.h
c_mputin.o: /usr/include/sys/_types/_time_t.h
c_mputin.o: /usr/include/sys/_types/_useconds_t.h
c_mputin.o: /usr/include/sys/_types/_suseconds_t.h
c_mputin.o: /usr/include/sys/_types/_rsize_t.h
c_mputin.o: /usr/include/sys/_types/_errno_t.h
c_mputin.o: /usr/include/sys/_types/_fd_def.h
c_mputin.o: /usr/include/sys/_types/_fd_setsize.h
c_mputin.o: /usr/include/sys/_types/_fd_set.h
c_mputin.o: /usr/include/sys/_types/_fd_clr.h
c_mputin.o: /usr/include/sys/_types/_fd_zero.h
c_mputin.o: /usr/include/sys/_types/_fd_isset.h
c_mputin.o: /usr/include/sys/_types/_fd_copy.h
c_mputin.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mputin.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mputin.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mputin.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mputin.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mputin.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mputin.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mputin.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mputin.o: /usr/include/sys/_pthread/_pthread_t.h
c_mputin.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mputin.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mputin.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mputin.o: ../../../.././include/ncarg/c.h
c_mdlach.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdlach.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdlach.o: /usr/include/sys/_posix_availability.h
c_mdlach.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdlach.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdlach.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdlach.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdlach.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdlach.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdlach.o: /usr/include/sys/_types/_int16_t.h
c_mdlach.o: /usr/include/sys/_types/_int32_t.h
c_mdlach.o: /usr/include/sys/_types/_int64_t.h
c_mdlach.o: /usr/include/sys/_types/_u_int8_t.h
c_mdlach.o: /usr/include/sys/_types/_u_int16_t.h
c_mdlach.o: /usr/include/sys/_types/_u_int32_t.h
c_mdlach.o: /usr/include/sys/_types/_u_int64_t.h
c_mdlach.o: /usr/include/sys/_types/_intptr_t.h
c_mdlach.o: /usr/include/sys/_types/_uintptr_t.h
c_mdlach.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdlach.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdlach.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdlach.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdlach.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdlach.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdlach.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdlach.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdlach.o: /usr/include/sys/_types/_u_char.h
c_mdlach.o: /usr/include/sys/_types/_u_short.h
c_mdlach.o: /usr/include/sys/_types/_u_int.h
c_mdlach.o: /usr/include/sys/_types/_caddr_t.h
c_mdlach.o: /usr/include/sys/_types/_dev_t.h
c_mdlach.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdlach.o: /usr/include/sys/_types/_blksize_t.h
c_mdlach.o: /usr/include/sys/_types/_gid_t.h
c_mdlach.o: /usr/include/sys/_types/_in_addr_t.h
c_mdlach.o: /usr/include/sys/_types/_in_port_t.h
c_mdlach.o: /usr/include/sys/_types/_ino_t.h
c_mdlach.o: /usr/include/sys/_types/_ino64_t.h
c_mdlach.o: /usr/include/sys/_types/_key_t.h
c_mdlach.o: /usr/include/sys/_types/_mode_t.h
c_mdlach.o: /usr/include/sys/_types/_nlink_t.h
c_mdlach.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdlach.o: /usr/include/sys/_types/_uid_t.h
c_mdlach.o: /usr/include/sys/_types/_clock_t.h
c_mdlach.o: /usr/include/sys/_types/_time_t.h
c_mdlach.o: /usr/include/sys/_types/_useconds_t.h
c_mdlach.o: /usr/include/sys/_types/_suseconds_t.h
c_mdlach.o: /usr/include/sys/_types/_rsize_t.h
c_mdlach.o: /usr/include/sys/_types/_errno_t.h
c_mdlach.o: /usr/include/sys/_types/_fd_def.h
c_mdlach.o: /usr/include/sys/_types/_fd_setsize.h
c_mdlach.o: /usr/include/sys/_types/_fd_set.h
c_mdlach.o: /usr/include/sys/_types/_fd_clr.h
c_mdlach.o: /usr/include/sys/_types/_fd_zero.h
c_mdlach.o: /usr/include/sys/_types/_fd_isset.h
c_mdlach.o: /usr/include/sys/_types/_fd_copy.h
c_mdlach.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdlach.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdlach.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdlach.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdlach.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdlach.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdlach.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdlach.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdlach.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdlach.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdlach.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdlach.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdlach.o: ../../../.././include/ncarg/c.h
c_mdloch.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdloch.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdloch.o: /usr/include/sys/_posix_availability.h
c_mdloch.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdloch.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdloch.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdloch.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdloch.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdloch.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdloch.o: /usr/include/sys/_types/_int16_t.h
c_mdloch.o: /usr/include/sys/_types/_int32_t.h
c_mdloch.o: /usr/include/sys/_types/_int64_t.h
c_mdloch.o: /usr/include/sys/_types/_u_int8_t.h
c_mdloch.o: /usr/include/sys/_types/_u_int16_t.h
c_mdloch.o: /usr/include/sys/_types/_u_int32_t.h
c_mdloch.o: /usr/include/sys/_types/_u_int64_t.h
c_mdloch.o: /usr/include/sys/_types/_intptr_t.h
c_mdloch.o: /usr/include/sys/_types/_uintptr_t.h
c_mdloch.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdloch.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdloch.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdloch.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdloch.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdloch.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdloch.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdloch.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdloch.o: /usr/include/sys/_types/_u_char.h
c_mdloch.o: /usr/include/sys/_types/_u_short.h
c_mdloch.o: /usr/include/sys/_types/_u_int.h
c_mdloch.o: /usr/include/sys/_types/_caddr_t.h
c_mdloch.o: /usr/include/sys/_types/_dev_t.h
c_mdloch.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdloch.o: /usr/include/sys/_types/_blksize_t.h
c_mdloch.o: /usr/include/sys/_types/_gid_t.h
c_mdloch.o: /usr/include/sys/_types/_in_addr_t.h
c_mdloch.o: /usr/include/sys/_types/_in_port_t.h
c_mdloch.o: /usr/include/sys/_types/_ino_t.h
c_mdloch.o: /usr/include/sys/_types/_ino64_t.h
c_mdloch.o: /usr/include/sys/_types/_key_t.h
c_mdloch.o: /usr/include/sys/_types/_mode_t.h
c_mdloch.o: /usr/include/sys/_types/_nlink_t.h
c_mdloch.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdloch.o: /usr/include/sys/_types/_uid_t.h
c_mdloch.o: /usr/include/sys/_types/_clock_t.h
c_mdloch.o: /usr/include/sys/_types/_time_t.h
c_mdloch.o: /usr/include/sys/_types/_useconds_t.h
c_mdloch.o: /usr/include/sys/_types/_suseconds_t.h
c_mdloch.o: /usr/include/sys/_types/_rsize_t.h
c_mdloch.o: /usr/include/sys/_types/_errno_t.h
c_mdloch.o: /usr/include/sys/_types/_fd_def.h
c_mdloch.o: /usr/include/sys/_types/_fd_setsize.h
c_mdloch.o: /usr/include/sys/_types/_fd_set.h
c_mdloch.o: /usr/include/sys/_types/_fd_clr.h
c_mdloch.o: /usr/include/sys/_types/_fd_zero.h
c_mdloch.o: /usr/include/sys/_types/_fd_isset.h
c_mdloch.o: /usr/include/sys/_types/_fd_copy.h
c_mdloch.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdloch.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdloch.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdloch.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdloch.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdloch.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdloch.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdloch.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdloch.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdloch.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdloch.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdloch.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdloch.o: ../../../.././include/ncarg/c.h
c_mdrgdl.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdrgdl.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdrgdl.o: /usr/include/sys/_posix_availability.h
c_mdrgdl.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdrgdl.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdrgdl.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdrgdl.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdrgdl.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdrgdl.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdrgdl.o: /usr/include/sys/_types/_int16_t.h
c_mdrgdl.o: /usr/include/sys/_types/_int32_t.h
c_mdrgdl.o: /usr/include/sys/_types/_int64_t.h
c_mdrgdl.o: /usr/include/sys/_types/_u_int8_t.h
c_mdrgdl.o: /usr/include/sys/_types/_u_int16_t.h
c_mdrgdl.o: /usr/include/sys/_types/_u_int32_t.h
c_mdrgdl.o: /usr/include/sys/_types/_u_int64_t.h
c_mdrgdl.o: /usr/include/sys/_types/_intptr_t.h
c_mdrgdl.o: /usr/include/sys/_types/_uintptr_t.h
c_mdrgdl.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdrgdl.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdrgdl.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdrgdl.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdrgdl.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdrgdl.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdrgdl.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdrgdl.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdrgdl.o: /usr/include/sys/_types/_u_char.h
c_mdrgdl.o: /usr/include/sys/_types/_u_short.h
c_mdrgdl.o: /usr/include/sys/_types/_u_int.h
c_mdrgdl.o: /usr/include/sys/_types/_caddr_t.h
c_mdrgdl.o: /usr/include/sys/_types/_dev_t.h
c_mdrgdl.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdrgdl.o: /usr/include/sys/_types/_blksize_t.h
c_mdrgdl.o: /usr/include/sys/_types/_gid_t.h
c_mdrgdl.o: /usr/include/sys/_types/_in_addr_t.h
c_mdrgdl.o: /usr/include/sys/_types/_in_port_t.h
c_mdrgdl.o: /usr/include/sys/_types/_ino_t.h
c_mdrgdl.o: /usr/include/sys/_types/_ino64_t.h
c_mdrgdl.o: /usr/include/sys/_types/_key_t.h
c_mdrgdl.o: /usr/include/sys/_types/_mode_t.h
c_mdrgdl.o: /usr/include/sys/_types/_nlink_t.h
c_mdrgdl.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdrgdl.o: /usr/include/sys/_types/_uid_t.h
c_mdrgdl.o: /usr/include/sys/_types/_clock_t.h
c_mdrgdl.o: /usr/include/sys/_types/_time_t.h
c_mdrgdl.o: /usr/include/sys/_types/_useconds_t.h
c_mdrgdl.o: /usr/include/sys/_types/_suseconds_t.h
c_mdrgdl.o: /usr/include/sys/_types/_rsize_t.h
c_mdrgdl.o: /usr/include/sys/_types/_errno_t.h
c_mdrgdl.o: /usr/include/sys/_types/_fd_def.h
c_mdrgdl.o: /usr/include/sys/_types/_fd_setsize.h
c_mdrgdl.o: /usr/include/sys/_types/_fd_set.h
c_mdrgdl.o: /usr/include/sys/_types/_fd_clr.h
c_mdrgdl.o: /usr/include/sys/_types/_fd_zero.h
c_mdrgdl.o: /usr/include/sys/_types/_fd_isset.h
c_mdrgdl.o: /usr/include/sys/_types/_fd_copy.h
c_mdrgdl.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdrgdl.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdrgdl.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdrgdl.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdrgdl.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdrgdl.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdrgdl.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdrgdl.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdrgdl.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdrgdl.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdrgdl.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdrgdl.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdrgdl.o: ../../../.././include/ncarg/c.h
c_mdscal.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdscal.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdscal.o: /usr/include/sys/_posix_availability.h
c_mdscal.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdscal.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdscal.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdscal.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdscal.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdscal.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdscal.o: /usr/include/sys/_types/_int16_t.h
c_mdscal.o: /usr/include/sys/_types/_int32_t.h
c_mdscal.o: /usr/include/sys/_types/_int64_t.h
c_mdscal.o: /usr/include/sys/_types/_u_int8_t.h
c_mdscal.o: /usr/include/sys/_types/_u_int16_t.h
c_mdscal.o: /usr/include/sys/_types/_u_int32_t.h
c_mdscal.o: /usr/include/sys/_types/_u_int64_t.h
c_mdscal.o: /usr/include/sys/_types/_intptr_t.h
c_mdscal.o: /usr/include/sys/_types/_uintptr_t.h
c_mdscal.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdscal.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdscal.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdscal.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdscal.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdscal.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdscal.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdscal.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdscal.o: /usr/include/sys/_types/_u_char.h
c_mdscal.o: /usr/include/sys/_types/_u_short.h
c_mdscal.o: /usr/include/sys/_types/_u_int.h
c_mdscal.o: /usr/include/sys/_types/_caddr_t.h
c_mdscal.o: /usr/include/sys/_types/_dev_t.h
c_mdscal.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdscal.o: /usr/include/sys/_types/_blksize_t.h
c_mdscal.o: /usr/include/sys/_types/_gid_t.h
c_mdscal.o: /usr/include/sys/_types/_in_addr_t.h
c_mdscal.o: /usr/include/sys/_types/_in_port_t.h
c_mdscal.o: /usr/include/sys/_types/_ino_t.h
c_mdscal.o: /usr/include/sys/_types/_ino64_t.h
c_mdscal.o: /usr/include/sys/_types/_key_t.h
c_mdscal.o: /usr/include/sys/_types/_mode_t.h
c_mdscal.o: /usr/include/sys/_types/_nlink_t.h
c_mdscal.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdscal.o: /usr/include/sys/_types/_uid_t.h
c_mdscal.o: /usr/include/sys/_types/_clock_t.h
c_mdscal.o: /usr/include/sys/_types/_time_t.h
c_mdscal.o: /usr/include/sys/_types/_useconds_t.h
c_mdscal.o: /usr/include/sys/_types/_suseconds_t.h
c_mdscal.o: /usr/include/sys/_types/_rsize_t.h
c_mdscal.o: /usr/include/sys/_types/_errno_t.h
c_mdscal.o: /usr/include/sys/_types/_fd_def.h
c_mdscal.o: /usr/include/sys/_types/_fd_setsize.h
c_mdscal.o: /usr/include/sys/_types/_fd_set.h
c_mdscal.o: /usr/include/sys/_types/_fd_clr.h
c_mdscal.o: /usr/include/sys/_types/_fd_zero.h
c_mdscal.o: /usr/include/sys/_types/_fd_isset.h
c_mdscal.o: /usr/include/sys/_types/_fd_copy.h
c_mdscal.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdscal.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdscal.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdscal.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdscal.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdscal.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdscal.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdscal.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdscal.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdscal.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdscal.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdscal.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdscal.o: ../../../.././include/ncarg/c.h
c_maplmm.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_maplmm.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_maplmm.o: /usr/include/sys/_posix_availability.h
c_maplmm.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_maplmm.o: /usr/include/_types.h /usr/include/sys/_types.h
c_maplmm.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_maplmm.o: /usr/include/sys/_pthread/_pthread_types.h
c_maplmm.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_maplmm.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_maplmm.o: /usr/include/sys/_types/_int16_t.h
c_maplmm.o: /usr/include/sys/_types/_int32_t.h
c_maplmm.o: /usr/include/sys/_types/_int64_t.h
c_maplmm.o: /usr/include/sys/_types/_u_int8_t.h
c_maplmm.o: /usr/include/sys/_types/_u_int16_t.h
c_maplmm.o: /usr/include/sys/_types/_u_int32_t.h
c_maplmm.o: /usr/include/sys/_types/_u_int64_t.h
c_maplmm.o: /usr/include/sys/_types/_intptr_t.h
c_maplmm.o: /usr/include/sys/_types/_uintptr_t.h
c_maplmm.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_maplmm.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_maplmm.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_maplmm.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_maplmm.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_maplmm.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_maplmm.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_maplmm.o: /usr/include/libkern/i386/_OSByteOrder.h
c_maplmm.o: /usr/include/sys/_types/_u_char.h
c_maplmm.o: /usr/include/sys/_types/_u_short.h
c_maplmm.o: /usr/include/sys/_types/_u_int.h
c_maplmm.o: /usr/include/sys/_types/_caddr_t.h
c_maplmm.o: /usr/include/sys/_types/_dev_t.h
c_maplmm.o: /usr/include/sys/_types/_blkcnt_t.h
c_maplmm.o: /usr/include/sys/_types/_blksize_t.h
c_maplmm.o: /usr/include/sys/_types/_gid_t.h
c_maplmm.o: /usr/include/sys/_types/_in_addr_t.h
c_maplmm.o: /usr/include/sys/_types/_in_port_t.h
c_maplmm.o: /usr/include/sys/_types/_ino_t.h
c_maplmm.o: /usr/include/sys/_types/_ino64_t.h
c_maplmm.o: /usr/include/sys/_types/_key_t.h
c_maplmm.o: /usr/include/sys/_types/_mode_t.h
c_maplmm.o: /usr/include/sys/_types/_nlink_t.h
c_maplmm.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_maplmm.o: /usr/include/sys/_types/_uid_t.h
c_maplmm.o: /usr/include/sys/_types/_clock_t.h
c_maplmm.o: /usr/include/sys/_types/_time_t.h
c_maplmm.o: /usr/include/sys/_types/_useconds_t.h
c_maplmm.o: /usr/include/sys/_types/_suseconds_t.h
c_maplmm.o: /usr/include/sys/_types/_rsize_t.h
c_maplmm.o: /usr/include/sys/_types/_errno_t.h
c_maplmm.o: /usr/include/sys/_types/_fd_def.h
c_maplmm.o: /usr/include/sys/_types/_fd_setsize.h
c_maplmm.o: /usr/include/sys/_types/_fd_set.h
c_maplmm.o: /usr/include/sys/_types/_fd_clr.h
c_maplmm.o: /usr/include/sys/_types/_fd_zero.h
c_maplmm.o: /usr/include/sys/_types/_fd_isset.h
c_maplmm.o: /usr/include/sys/_types/_fd_copy.h
c_maplmm.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_maplmm.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_maplmm.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_maplmm.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_maplmm.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_maplmm.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_maplmm.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_maplmm.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_maplmm.o: /usr/include/sys/_pthread/_pthread_t.h
c_maplmm.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_maplmm.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_maplmm.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_maplmm.o: ../../../.././include/ncarg/c.h
c_mdplmm.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdplmm.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdplmm.o: /usr/include/sys/_posix_availability.h
c_mdplmm.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdplmm.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdplmm.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdplmm.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdplmm.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdplmm.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdplmm.o: /usr/include/sys/_types/_int16_t.h
c_mdplmm.o: /usr/include/sys/_types/_int32_t.h
c_mdplmm.o: /usr/include/sys/_types/_int64_t.h
c_mdplmm.o: /usr/include/sys/_types/_u_int8_t.h
c_mdplmm.o: /usr/include/sys/_types/_u_int16_t.h
c_mdplmm.o: /usr/include/sys/_types/_u_int32_t.h
c_mdplmm.o: /usr/include/sys/_types/_u_int64_t.h
c_mdplmm.o: /usr/include/sys/_types/_intptr_t.h
c_mdplmm.o: /usr/include/sys/_types/_uintptr_t.h
c_mdplmm.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdplmm.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdplmm.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdplmm.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdplmm.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdplmm.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdplmm.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdplmm.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdplmm.o: /usr/include/sys/_types/_u_char.h
c_mdplmm.o: /usr/include/sys/_types/_u_short.h
c_mdplmm.o: /usr/include/sys/_types/_u_int.h
c_mdplmm.o: /usr/include/sys/_types/_caddr_t.h
c_mdplmm.o: /usr/include/sys/_types/_dev_t.h
c_mdplmm.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdplmm.o: /usr/include/sys/_types/_blksize_t.h
c_mdplmm.o: /usr/include/sys/_types/_gid_t.h
c_mdplmm.o: /usr/include/sys/_types/_in_addr_t.h
c_mdplmm.o: /usr/include/sys/_types/_in_port_t.h
c_mdplmm.o: /usr/include/sys/_types/_ino_t.h
c_mdplmm.o: /usr/include/sys/_types/_ino64_t.h
c_mdplmm.o: /usr/include/sys/_types/_key_t.h
c_mdplmm.o: /usr/include/sys/_types/_mode_t.h
c_mdplmm.o: /usr/include/sys/_types/_nlink_t.h
c_mdplmm.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdplmm.o: /usr/include/sys/_types/_uid_t.h
c_mdplmm.o: /usr/include/sys/_types/_clock_t.h
c_mdplmm.o: /usr/include/sys/_types/_time_t.h
c_mdplmm.o: /usr/include/sys/_types/_useconds_t.h
c_mdplmm.o: /usr/include/sys/_types/_suseconds_t.h
c_mdplmm.o: /usr/include/sys/_types/_rsize_t.h
c_mdplmm.o: /usr/include/sys/_types/_errno_t.h
c_mdplmm.o: /usr/include/sys/_types/_fd_def.h
c_mdplmm.o: /usr/include/sys/_types/_fd_setsize.h
c_mdplmm.o: /usr/include/sys/_types/_fd_set.h
c_mdplmm.o: /usr/include/sys/_types/_fd_clr.h
c_mdplmm.o: /usr/include/sys/_types/_fd_zero.h
c_mdplmm.o: /usr/include/sys/_types/_fd_isset.h
c_mdplmm.o: /usr/include/sys/_types/_fd_copy.h
c_mdplmm.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdplmm.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdplmm.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdplmm.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdplmm.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdplmm.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdplmm.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdplmm.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdplmm.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdplmm.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdplmm.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdplmm.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdplmm.o: ../../../.././include/ncarg/c.h
c_maqini.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_maqini.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_maqini.o: /usr/include/sys/_posix_availability.h
c_maqini.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_maqini.o: /usr/include/_types.h /usr/include/sys/_types.h
c_maqini.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_maqini.o: /usr/include/sys/_pthread/_pthread_types.h
c_maqini.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_maqini.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_maqini.o: /usr/include/sys/_types/_int16_t.h
c_maqini.o: /usr/include/sys/_types/_int32_t.h
c_maqini.o: /usr/include/sys/_types/_int64_t.h
c_maqini.o: /usr/include/sys/_types/_u_int8_t.h
c_maqini.o: /usr/include/sys/_types/_u_int16_t.h
c_maqini.o: /usr/include/sys/_types/_u_int32_t.h
c_maqini.o: /usr/include/sys/_types/_u_int64_t.h
c_maqini.o: /usr/include/sys/_types/_intptr_t.h
c_maqini.o: /usr/include/sys/_types/_uintptr_t.h
c_maqini.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_maqini.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_maqini.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_maqini.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_maqini.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_maqini.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_maqini.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_maqini.o: /usr/include/libkern/i386/_OSByteOrder.h
c_maqini.o: /usr/include/sys/_types/_u_char.h
c_maqini.o: /usr/include/sys/_types/_u_short.h
c_maqini.o: /usr/include/sys/_types/_u_int.h
c_maqini.o: /usr/include/sys/_types/_caddr_t.h
c_maqini.o: /usr/include/sys/_types/_dev_t.h
c_maqini.o: /usr/include/sys/_types/_blkcnt_t.h
c_maqini.o: /usr/include/sys/_types/_blksize_t.h
c_maqini.o: /usr/include/sys/_types/_gid_t.h
c_maqini.o: /usr/include/sys/_types/_in_addr_t.h
c_maqini.o: /usr/include/sys/_types/_in_port_t.h
c_maqini.o: /usr/include/sys/_types/_ino_t.h
c_maqini.o: /usr/include/sys/_types/_ino64_t.h
c_maqini.o: /usr/include/sys/_types/_key_t.h
c_maqini.o: /usr/include/sys/_types/_mode_t.h
c_maqini.o: /usr/include/sys/_types/_nlink_t.h
c_maqini.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_maqini.o: /usr/include/sys/_types/_uid_t.h
c_maqini.o: /usr/include/sys/_types/_clock_t.h
c_maqini.o: /usr/include/sys/_types/_time_t.h
c_maqini.o: /usr/include/sys/_types/_useconds_t.h
c_maqini.o: /usr/include/sys/_types/_suseconds_t.h
c_maqini.o: /usr/include/sys/_types/_rsize_t.h
c_maqini.o: /usr/include/sys/_types/_errno_t.h
c_maqini.o: /usr/include/sys/_types/_fd_def.h
c_maqini.o: /usr/include/sys/_types/_fd_setsize.h
c_maqini.o: /usr/include/sys/_types/_fd_set.h
c_maqini.o: /usr/include/sys/_types/_fd_clr.h
c_maqini.o: /usr/include/sys/_types/_fd_zero.h
c_maqini.o: /usr/include/sys/_types/_fd_isset.h
c_maqini.o: /usr/include/sys/_types/_fd_copy.h
c_maqini.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_maqini.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_maqini.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_maqini.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_maqini.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_maqini.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_maqini.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_maqini.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_maqini.o: /usr/include/sys/_pthread/_pthread_t.h
c_maqini.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_maqini.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_maqini.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_maqini.o: ../../../.././include/ncarg/c.h
c_maqtra.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_maqtra.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_maqtra.o: /usr/include/sys/_posix_availability.h
c_maqtra.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_maqtra.o: /usr/include/_types.h /usr/include/sys/_types.h
c_maqtra.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_maqtra.o: /usr/include/sys/_pthread/_pthread_types.h
c_maqtra.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_maqtra.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_maqtra.o: /usr/include/sys/_types/_int16_t.h
c_maqtra.o: /usr/include/sys/_types/_int32_t.h
c_maqtra.o: /usr/include/sys/_types/_int64_t.h
c_maqtra.o: /usr/include/sys/_types/_u_int8_t.h
c_maqtra.o: /usr/include/sys/_types/_u_int16_t.h
c_maqtra.o: /usr/include/sys/_types/_u_int32_t.h
c_maqtra.o: /usr/include/sys/_types/_u_int64_t.h
c_maqtra.o: /usr/include/sys/_types/_intptr_t.h
c_maqtra.o: /usr/include/sys/_types/_uintptr_t.h
c_maqtra.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_maqtra.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_maqtra.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_maqtra.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_maqtra.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_maqtra.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_maqtra.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_maqtra.o: /usr/include/libkern/i386/_OSByteOrder.h
c_maqtra.o: /usr/include/sys/_types/_u_char.h
c_maqtra.o: /usr/include/sys/_types/_u_short.h
c_maqtra.o: /usr/include/sys/_types/_u_int.h
c_maqtra.o: /usr/include/sys/_types/_caddr_t.h
c_maqtra.o: /usr/include/sys/_types/_dev_t.h
c_maqtra.o: /usr/include/sys/_types/_blkcnt_t.h
c_maqtra.o: /usr/include/sys/_types/_blksize_t.h
c_maqtra.o: /usr/include/sys/_types/_gid_t.h
c_maqtra.o: /usr/include/sys/_types/_in_addr_t.h
c_maqtra.o: /usr/include/sys/_types/_in_port_t.h
c_maqtra.o: /usr/include/sys/_types/_ino_t.h
c_maqtra.o: /usr/include/sys/_types/_ino64_t.h
c_maqtra.o: /usr/include/sys/_types/_key_t.h
c_maqtra.o: /usr/include/sys/_types/_mode_t.h
c_maqtra.o: /usr/include/sys/_types/_nlink_t.h
c_maqtra.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_maqtra.o: /usr/include/sys/_types/_uid_t.h
c_maqtra.o: /usr/include/sys/_types/_clock_t.h
c_maqtra.o: /usr/include/sys/_types/_time_t.h
c_maqtra.o: /usr/include/sys/_types/_useconds_t.h
c_maqtra.o: /usr/include/sys/_types/_suseconds_t.h
c_maqtra.o: /usr/include/sys/_types/_rsize_t.h
c_maqtra.o: /usr/include/sys/_types/_errno_t.h
c_maqtra.o: /usr/include/sys/_types/_fd_def.h
c_maqtra.o: /usr/include/sys/_types/_fd_setsize.h
c_maqtra.o: /usr/include/sys/_types/_fd_set.h
c_maqtra.o: /usr/include/sys/_types/_fd_clr.h
c_maqtra.o: /usr/include/sys/_types/_fd_zero.h
c_maqtra.o: /usr/include/sys/_types/_fd_isset.h
c_maqtra.o: /usr/include/sys/_types/_fd_copy.h
c_maqtra.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_maqtra.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_maqtra.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_maqtra.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_maqtra.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_maqtra.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_maqtra.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_maqtra.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_maqtra.o: /usr/include/sys/_pthread/_pthread_t.h
c_maqtra.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_maqtra.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_maqtra.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_maqtra.o: ../../../.././include/ncarg/c.h
c_maqtri.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_maqtri.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_maqtri.o: /usr/include/sys/_posix_availability.h
c_maqtri.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_maqtri.o: /usr/include/_types.h /usr/include/sys/_types.h
c_maqtri.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_maqtri.o: /usr/include/sys/_pthread/_pthread_types.h
c_maqtri.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_maqtri.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_maqtri.o: /usr/include/sys/_types/_int16_t.h
c_maqtri.o: /usr/include/sys/_types/_int32_t.h
c_maqtri.o: /usr/include/sys/_types/_int64_t.h
c_maqtri.o: /usr/include/sys/_types/_u_int8_t.h
c_maqtri.o: /usr/include/sys/_types/_u_int16_t.h
c_maqtri.o: /usr/include/sys/_types/_u_int32_t.h
c_maqtri.o: /usr/include/sys/_types/_u_int64_t.h
c_maqtri.o: /usr/include/sys/_types/_intptr_t.h
c_maqtri.o: /usr/include/sys/_types/_uintptr_t.h
c_maqtri.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_maqtri.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_maqtri.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_maqtri.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_maqtri.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_maqtri.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_maqtri.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_maqtri.o: /usr/include/libkern/i386/_OSByteOrder.h
c_maqtri.o: /usr/include/sys/_types/_u_char.h
c_maqtri.o: /usr/include/sys/_types/_u_short.h
c_maqtri.o: /usr/include/sys/_types/_u_int.h
c_maqtri.o: /usr/include/sys/_types/_caddr_t.h
c_maqtri.o: /usr/include/sys/_types/_dev_t.h
c_maqtri.o: /usr/include/sys/_types/_blkcnt_t.h
c_maqtri.o: /usr/include/sys/_types/_blksize_t.h
c_maqtri.o: /usr/include/sys/_types/_gid_t.h
c_maqtri.o: /usr/include/sys/_types/_in_addr_t.h
c_maqtri.o: /usr/include/sys/_types/_in_port_t.h
c_maqtri.o: /usr/include/sys/_types/_ino_t.h
c_maqtri.o: /usr/include/sys/_types/_ino64_t.h
c_maqtri.o: /usr/include/sys/_types/_key_t.h
c_maqtri.o: /usr/include/sys/_types/_mode_t.h
c_maqtri.o: /usr/include/sys/_types/_nlink_t.h
c_maqtri.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_maqtri.o: /usr/include/sys/_types/_uid_t.h
c_maqtri.o: /usr/include/sys/_types/_clock_t.h
c_maqtri.o: /usr/include/sys/_types/_time_t.h
c_maqtri.o: /usr/include/sys/_types/_useconds_t.h
c_maqtri.o: /usr/include/sys/_types/_suseconds_t.h
c_maqtri.o: /usr/include/sys/_types/_rsize_t.h
c_maqtri.o: /usr/include/sys/_types/_errno_t.h
c_maqtri.o: /usr/include/sys/_types/_fd_def.h
c_maqtri.o: /usr/include/sys/_types/_fd_setsize.h
c_maqtri.o: /usr/include/sys/_types/_fd_set.h
c_maqtri.o: /usr/include/sys/_types/_fd_clr.h
c_maqtri.o: /usr/include/sys/_types/_fd_zero.h
c_maqtri.o: /usr/include/sys/_types/_fd_isset.h
c_maqtri.o: /usr/include/sys/_types/_fd_copy.h
c_maqtri.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_maqtri.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_maqtri.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_maqtri.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_maqtri.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_maqtri.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_maqtri.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_maqtri.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_maqtri.o: /usr/include/sys/_pthread/_pthread_t.h
c_maqtri.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_maqtri.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_maqtri.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_maqtri.o: ../../../.././include/ncarg/c.h
c_maqtrn.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_maqtrn.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_maqtrn.o: /usr/include/sys/_posix_availability.h
c_maqtrn.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_maqtrn.o: /usr/include/_types.h /usr/include/sys/_types.h
c_maqtrn.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_maqtrn.o: /usr/include/sys/_pthread/_pthread_types.h
c_maqtrn.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_maqtrn.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_maqtrn.o: /usr/include/sys/_types/_int16_t.h
c_maqtrn.o: /usr/include/sys/_types/_int32_t.h
c_maqtrn.o: /usr/include/sys/_types/_int64_t.h
c_maqtrn.o: /usr/include/sys/_types/_u_int8_t.h
c_maqtrn.o: /usr/include/sys/_types/_u_int16_t.h
c_maqtrn.o: /usr/include/sys/_types/_u_int32_t.h
c_maqtrn.o: /usr/include/sys/_types/_u_int64_t.h
c_maqtrn.o: /usr/include/sys/_types/_intptr_t.h
c_maqtrn.o: /usr/include/sys/_types/_uintptr_t.h
c_maqtrn.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_maqtrn.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_maqtrn.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_maqtrn.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_maqtrn.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_maqtrn.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_maqtrn.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_maqtrn.o: /usr/include/libkern/i386/_OSByteOrder.h
c_maqtrn.o: /usr/include/sys/_types/_u_char.h
c_maqtrn.o: /usr/include/sys/_types/_u_short.h
c_maqtrn.o: /usr/include/sys/_types/_u_int.h
c_maqtrn.o: /usr/include/sys/_types/_caddr_t.h
c_maqtrn.o: /usr/include/sys/_types/_dev_t.h
c_maqtrn.o: /usr/include/sys/_types/_blkcnt_t.h
c_maqtrn.o: /usr/include/sys/_types/_blksize_t.h
c_maqtrn.o: /usr/include/sys/_types/_gid_t.h
c_maqtrn.o: /usr/include/sys/_types/_in_addr_t.h
c_maqtrn.o: /usr/include/sys/_types/_in_port_t.h
c_maqtrn.o: /usr/include/sys/_types/_ino_t.h
c_maqtrn.o: /usr/include/sys/_types/_ino64_t.h
c_maqtrn.o: /usr/include/sys/_types/_key_t.h
c_maqtrn.o: /usr/include/sys/_types/_mode_t.h
c_maqtrn.o: /usr/include/sys/_types/_nlink_t.h
c_maqtrn.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_maqtrn.o: /usr/include/sys/_types/_uid_t.h
c_maqtrn.o: /usr/include/sys/_types/_clock_t.h
c_maqtrn.o: /usr/include/sys/_types/_time_t.h
c_maqtrn.o: /usr/include/sys/_types/_useconds_t.h
c_maqtrn.o: /usr/include/sys/_types/_suseconds_t.h
c_maqtrn.o: /usr/include/sys/_types/_rsize_t.h
c_maqtrn.o: /usr/include/sys/_types/_errno_t.h
c_maqtrn.o: /usr/include/sys/_types/_fd_def.h
c_maqtrn.o: /usr/include/sys/_types/_fd_setsize.h
c_maqtrn.o: /usr/include/sys/_types/_fd_set.h
c_maqtrn.o: /usr/include/sys/_types/_fd_clr.h
c_maqtrn.o: /usr/include/sys/_types/_fd_zero.h
c_maqtrn.o: /usr/include/sys/_types/_fd_isset.h
c_maqtrn.o: /usr/include/sys/_types/_fd_copy.h
c_maqtrn.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_maqtrn.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_maqtrn.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_maqtrn.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_maqtrn.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_maqtrn.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_maqtrn.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_maqtrn.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_maqtrn.o: /usr/include/sys/_pthread/_pthread_t.h
c_maqtrn.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_maqtrn.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_maqtrn.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_maqtrn.o: ../../../.././include/ncarg/c.h
c_mdqini.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdqini.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdqini.o: /usr/include/sys/_posix_availability.h
c_mdqini.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdqini.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdqini.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdqini.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdqini.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdqini.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdqini.o: /usr/include/sys/_types/_int16_t.h
c_mdqini.o: /usr/include/sys/_types/_int32_t.h
c_mdqini.o: /usr/include/sys/_types/_int64_t.h
c_mdqini.o: /usr/include/sys/_types/_u_int8_t.h
c_mdqini.o: /usr/include/sys/_types/_u_int16_t.h
c_mdqini.o: /usr/include/sys/_types/_u_int32_t.h
c_mdqini.o: /usr/include/sys/_types/_u_int64_t.h
c_mdqini.o: /usr/include/sys/_types/_intptr_t.h
c_mdqini.o: /usr/include/sys/_types/_uintptr_t.h
c_mdqini.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdqini.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdqini.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdqini.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdqini.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdqini.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdqini.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdqini.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdqini.o: /usr/include/sys/_types/_u_char.h
c_mdqini.o: /usr/include/sys/_types/_u_short.h
c_mdqini.o: /usr/include/sys/_types/_u_int.h
c_mdqini.o: /usr/include/sys/_types/_caddr_t.h
c_mdqini.o: /usr/include/sys/_types/_dev_t.h
c_mdqini.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdqini.o: /usr/include/sys/_types/_blksize_t.h
c_mdqini.o: /usr/include/sys/_types/_gid_t.h
c_mdqini.o: /usr/include/sys/_types/_in_addr_t.h
c_mdqini.o: /usr/include/sys/_types/_in_port_t.h
c_mdqini.o: /usr/include/sys/_types/_ino_t.h
c_mdqini.o: /usr/include/sys/_types/_ino64_t.h
c_mdqini.o: /usr/include/sys/_types/_key_t.h
c_mdqini.o: /usr/include/sys/_types/_mode_t.h
c_mdqini.o: /usr/include/sys/_types/_nlink_t.h
c_mdqini.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdqini.o: /usr/include/sys/_types/_uid_t.h
c_mdqini.o: /usr/include/sys/_types/_clock_t.h
c_mdqini.o: /usr/include/sys/_types/_time_t.h
c_mdqini.o: /usr/include/sys/_types/_useconds_t.h
c_mdqini.o: /usr/include/sys/_types/_suseconds_t.h
c_mdqini.o: /usr/include/sys/_types/_rsize_t.h
c_mdqini.o: /usr/include/sys/_types/_errno_t.h
c_mdqini.o: /usr/include/sys/_types/_fd_def.h
c_mdqini.o: /usr/include/sys/_types/_fd_setsize.h
c_mdqini.o: /usr/include/sys/_types/_fd_set.h
c_mdqini.o: /usr/include/sys/_types/_fd_clr.h
c_mdqini.o: /usr/include/sys/_types/_fd_zero.h
c_mdqini.o: /usr/include/sys/_types/_fd_isset.h
c_mdqini.o: /usr/include/sys/_types/_fd_copy.h
c_mdqini.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdqini.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdqini.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdqini.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdqini.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdqini.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdqini.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdqini.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdqini.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdqini.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdqini.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdqini.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdqini.o: ../../../.././include/ncarg/c.h
c_mdqtra.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdqtra.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdqtra.o: /usr/include/sys/_posix_availability.h
c_mdqtra.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdqtra.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdqtra.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdqtra.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdqtra.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdqtra.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdqtra.o: /usr/include/sys/_types/_int16_t.h
c_mdqtra.o: /usr/include/sys/_types/_int32_t.h
c_mdqtra.o: /usr/include/sys/_types/_int64_t.h
c_mdqtra.o: /usr/include/sys/_types/_u_int8_t.h
c_mdqtra.o: /usr/include/sys/_types/_u_int16_t.h
c_mdqtra.o: /usr/include/sys/_types/_u_int32_t.h
c_mdqtra.o: /usr/include/sys/_types/_u_int64_t.h
c_mdqtra.o: /usr/include/sys/_types/_intptr_t.h
c_mdqtra.o: /usr/include/sys/_types/_uintptr_t.h
c_mdqtra.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdqtra.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdqtra.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdqtra.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdqtra.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdqtra.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdqtra.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdqtra.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdqtra.o: /usr/include/sys/_types/_u_char.h
c_mdqtra.o: /usr/include/sys/_types/_u_short.h
c_mdqtra.o: /usr/include/sys/_types/_u_int.h
c_mdqtra.o: /usr/include/sys/_types/_caddr_t.h
c_mdqtra.o: /usr/include/sys/_types/_dev_t.h
c_mdqtra.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdqtra.o: /usr/include/sys/_types/_blksize_t.h
c_mdqtra.o: /usr/include/sys/_types/_gid_t.h
c_mdqtra.o: /usr/include/sys/_types/_in_addr_t.h
c_mdqtra.o: /usr/include/sys/_types/_in_port_t.h
c_mdqtra.o: /usr/include/sys/_types/_ino_t.h
c_mdqtra.o: /usr/include/sys/_types/_ino64_t.h
c_mdqtra.o: /usr/include/sys/_types/_key_t.h
c_mdqtra.o: /usr/include/sys/_types/_mode_t.h
c_mdqtra.o: /usr/include/sys/_types/_nlink_t.h
c_mdqtra.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdqtra.o: /usr/include/sys/_types/_uid_t.h
c_mdqtra.o: /usr/include/sys/_types/_clock_t.h
c_mdqtra.o: /usr/include/sys/_types/_time_t.h
c_mdqtra.o: /usr/include/sys/_types/_useconds_t.h
c_mdqtra.o: /usr/include/sys/_types/_suseconds_t.h
c_mdqtra.o: /usr/include/sys/_types/_rsize_t.h
c_mdqtra.o: /usr/include/sys/_types/_errno_t.h
c_mdqtra.o: /usr/include/sys/_types/_fd_def.h
c_mdqtra.o: /usr/include/sys/_types/_fd_setsize.h
c_mdqtra.o: /usr/include/sys/_types/_fd_set.h
c_mdqtra.o: /usr/include/sys/_types/_fd_clr.h
c_mdqtra.o: /usr/include/sys/_types/_fd_zero.h
c_mdqtra.o: /usr/include/sys/_types/_fd_isset.h
c_mdqtra.o: /usr/include/sys/_types/_fd_copy.h
c_mdqtra.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdqtra.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdqtra.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdqtra.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdqtra.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdqtra.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdqtra.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdqtra.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdqtra.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdqtra.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdqtra.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdqtra.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdqtra.o: ../../../.././include/ncarg/c.h
c_mdqtri.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdqtri.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdqtri.o: /usr/include/sys/_posix_availability.h
c_mdqtri.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdqtri.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdqtri.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdqtri.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdqtri.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdqtri.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdqtri.o: /usr/include/sys/_types/_int16_t.h
c_mdqtri.o: /usr/include/sys/_types/_int32_t.h
c_mdqtri.o: /usr/include/sys/_types/_int64_t.h
c_mdqtri.o: /usr/include/sys/_types/_u_int8_t.h
c_mdqtri.o: /usr/include/sys/_types/_u_int16_t.h
c_mdqtri.o: /usr/include/sys/_types/_u_int32_t.h
c_mdqtri.o: /usr/include/sys/_types/_u_int64_t.h
c_mdqtri.o: /usr/include/sys/_types/_intptr_t.h
c_mdqtri.o: /usr/include/sys/_types/_uintptr_t.h
c_mdqtri.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdqtri.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdqtri.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdqtri.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdqtri.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdqtri.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdqtri.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdqtri.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdqtri.o: /usr/include/sys/_types/_u_char.h
c_mdqtri.o: /usr/include/sys/_types/_u_short.h
c_mdqtri.o: /usr/include/sys/_types/_u_int.h
c_mdqtri.o: /usr/include/sys/_types/_caddr_t.h
c_mdqtri.o: /usr/include/sys/_types/_dev_t.h
c_mdqtri.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdqtri.o: /usr/include/sys/_types/_blksize_t.h
c_mdqtri.o: /usr/include/sys/_types/_gid_t.h
c_mdqtri.o: /usr/include/sys/_types/_in_addr_t.h
c_mdqtri.o: /usr/include/sys/_types/_in_port_t.h
c_mdqtri.o: /usr/include/sys/_types/_ino_t.h
c_mdqtri.o: /usr/include/sys/_types/_ino64_t.h
c_mdqtri.o: /usr/include/sys/_types/_key_t.h
c_mdqtri.o: /usr/include/sys/_types/_mode_t.h
c_mdqtri.o: /usr/include/sys/_types/_nlink_t.h
c_mdqtri.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdqtri.o: /usr/include/sys/_types/_uid_t.h
c_mdqtri.o: /usr/include/sys/_types/_clock_t.h
c_mdqtri.o: /usr/include/sys/_types/_time_t.h
c_mdqtri.o: /usr/include/sys/_types/_useconds_t.h
c_mdqtri.o: /usr/include/sys/_types/_suseconds_t.h
c_mdqtri.o: /usr/include/sys/_types/_rsize_t.h
c_mdqtri.o: /usr/include/sys/_types/_errno_t.h
c_mdqtri.o: /usr/include/sys/_types/_fd_def.h
c_mdqtri.o: /usr/include/sys/_types/_fd_setsize.h
c_mdqtri.o: /usr/include/sys/_types/_fd_set.h
c_mdqtri.o: /usr/include/sys/_types/_fd_clr.h
c_mdqtri.o: /usr/include/sys/_types/_fd_zero.h
c_mdqtri.o: /usr/include/sys/_types/_fd_isset.h
c_mdqtri.o: /usr/include/sys/_types/_fd_copy.h
c_mdqtri.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdqtri.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdqtri.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdqtri.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdqtri.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdqtri.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdqtri.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdqtri.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdqtri.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdqtri.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdqtri.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdqtri.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdqtri.o: ../../../.././include/ncarg/c.h
c_mdqtrn.o: ../../../.././include/ncarg/ncargC.h /usr/include/stdio.h
c_mdqtrn.o: /usr/include/sys/cdefs.h /usr/include/sys/_symbol_aliasing.h
c_mdqtrn.o: /usr/include/sys/_posix_availability.h
c_mdqtrn.o: /usr/include/Availability.h /usr/include/AvailabilityInternal.h
c_mdqtrn.o: /usr/include/_types.h /usr/include/sys/_types.h
c_mdqtrn.o: /usr/include/machine/_types.h /usr/include/i386/_types.h
c_mdqtrn.o: /usr/include/sys/_pthread/_pthread_types.h
c_mdqtrn.o: /usr/include/sys/_types/_va_list.h /usr/include/machine/types.h
c_mdqtrn.o: /usr/include/i386/types.h /usr/include/sys/_types/_int8_t.h
c_mdqtrn.o: /usr/include/sys/_types/_int16_t.h
c_mdqtrn.o: /usr/include/sys/_types/_int32_t.h
c_mdqtrn.o: /usr/include/sys/_types/_int64_t.h
c_mdqtrn.o: /usr/include/sys/_types/_u_int8_t.h
c_mdqtrn.o: /usr/include/sys/_types/_u_int16_t.h
c_mdqtrn.o: /usr/include/sys/_types/_u_int32_t.h
c_mdqtrn.o: /usr/include/sys/_types/_u_int64_t.h
c_mdqtrn.o: /usr/include/sys/_types/_intptr_t.h
c_mdqtrn.o: /usr/include/sys/_types/_uintptr_t.h
c_mdqtrn.o: /usr/include/sys/_types/_size_t.h /usr/include/sys/_types/_null.h
c_mdqtrn.o: /usr/include/sys/stdio.h /usr/include/sys/_types/_off_t.h
c_mdqtrn.o: /usr/include/sys/_types/_ssize_t.h /usr/include/secure/_stdio.h
c_mdqtrn.o: /usr/include/secure/_common.h ../../../.././include/ncarg/gks.h
c_mdqtrn.o: /usr/include/sys/types.h /usr/include/sys/appleapiopts.h
c_mdqtrn.o: /usr/include/machine/endian.h /usr/include/i386/endian.h
c_mdqtrn.o: /usr/include/sys/_endian.h /usr/include/libkern/_OSByteOrder.h
c_mdqtrn.o: /usr/include/libkern/i386/_OSByteOrder.h
c_mdqtrn.o: /usr/include/sys/_types/_u_char.h
c_mdqtrn.o: /usr/include/sys/_types/_u_short.h
c_mdqtrn.o: /usr/include/sys/_types/_u_int.h
c_mdqtrn.o: /usr/include/sys/_types/_caddr_t.h
c_mdqtrn.o: /usr/include/sys/_types/_dev_t.h
c_mdqtrn.o: /usr/include/sys/_types/_blkcnt_t.h
c_mdqtrn.o: /usr/include/sys/_types/_blksize_t.h
c_mdqtrn.o: /usr/include/sys/_types/_gid_t.h
c_mdqtrn.o: /usr/include/sys/_types/_in_addr_t.h
c_mdqtrn.o: /usr/include/sys/_types/_in_port_t.h
c_mdqtrn.o: /usr/include/sys/_types/_ino_t.h
c_mdqtrn.o: /usr/include/sys/_types/_ino64_t.h
c_mdqtrn.o: /usr/include/sys/_types/_key_t.h
c_mdqtrn.o: /usr/include/sys/_types/_mode_t.h
c_mdqtrn.o: /usr/include/sys/_types/_nlink_t.h
c_mdqtrn.o: /usr/include/sys/_types/_id_t.h /usr/include/sys/_types/_pid_t.h
c_mdqtrn.o: /usr/include/sys/_types/_uid_t.h
c_mdqtrn.o: /usr/include/sys/_types/_clock_t.h
c_mdqtrn.o: /usr/include/sys/_types/_time_t.h
c_mdqtrn.o: /usr/include/sys/_types/_useconds_t.h
c_mdqtrn.o: /usr/include/sys/_types/_suseconds_t.h
c_mdqtrn.o: /usr/include/sys/_types/_rsize_t.h
c_mdqtrn.o: /usr/include/sys/_types/_errno_t.h
c_mdqtrn.o: /usr/include/sys/_types/_fd_def.h
c_mdqtrn.o: /usr/include/sys/_types/_fd_setsize.h
c_mdqtrn.o: /usr/include/sys/_types/_fd_set.h
c_mdqtrn.o: /usr/include/sys/_types/_fd_clr.h
c_mdqtrn.o: /usr/include/sys/_types/_fd_zero.h
c_mdqtrn.o: /usr/include/sys/_types/_fd_isset.h
c_mdqtrn.o: /usr/include/sys/_types/_fd_copy.h
c_mdqtrn.o: /usr/include/sys/_pthread/_pthread_attr_t.h
c_mdqtrn.o: /usr/include/sys/_pthread/_pthread_cond_t.h
c_mdqtrn.o: /usr/include/sys/_pthread/_pthread_condattr_t.h
c_mdqtrn.o: /usr/include/sys/_pthread/_pthread_mutex_t.h
c_mdqtrn.o: /usr/include/sys/_pthread/_pthread_mutexattr_t.h
c_mdqtrn.o: /usr/include/sys/_pthread/_pthread_once_t.h
c_mdqtrn.o: /usr/include/sys/_pthread/_pthread_rwlock_t.h
c_mdqtrn.o: /usr/include/sys/_pthread/_pthread_rwlockattr_t.h
c_mdqtrn.o: /usr/include/sys/_pthread/_pthread_t.h
c_mdqtrn.o: /usr/include/sys/_pthread/_pthread_key_t.h
c_mdqtrn.o: /usr/include/sys/_types/_fsblkcnt_t.h
c_mdqtrn.o: /usr/include/sys/_types/_fsfilcnt_t.h
c_mdqtrn.o: ../../../.././include/ncarg/c.h
