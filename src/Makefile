
all: ../StdEnv/Clean\ System\ Files/_startup.o ../StdEnv/Clean\ System\ Files/_startupTrace.o ../exe/cg ../StdEnv/Clean\ System\ Files/_system.o ../bin/patch_bin ../bin/clm tools/clm/clms ../exe/cocl ../bin/htoclean ../bin/BatchBuild ../bin/cpm

../StdEnv/Clean\ System\ Files/_startup.o: RuntimeSystem/macho64/_startup.o
	cp RuntimeSystem/macho64/_startup.o ../StdEnv/Clean\ System\ Files/_startup.o

../StdEnv/Clean\ System\ Files/_startupTrace.o: RuntimeSystem/macho64/_startupTrace.o
	cp RuntimeSystem/macho64/_startupTrace.o ../StdEnv/Clean\ System\ Files/_startupTrace.o

RuntimeSystem/macho64/_startup.o:
	cd RuntimeSystem/macho64 ; ./make.sh

RuntimeSystem/macho64/_startupTrace.o:
	cd RuntimeSystem/macho64 ; ./make.sh

../StdEnv/Clean\ System\ Files/_system.o: ../exe/cg
	cd ../StdEnv/Clean\ System\ Files ; ../../exe/cg _system

../exe/cg: CodeGenerator/cg
	cp CodeGenerator/cg ../exe/cg

CodeGenerator/cg:
	cd CodeGenerator; \
	$(MAKE) -f Makefile.macosx64

../bin/patch_bin: tools/clm/patch_bin
	cp tools/clm/patch_bin ../bin/patch_bin

tools/clm/patch_bin:
	cd tools/clm; \
	$(MAKE) -f Makefile.macho64 patch_bin

../bin/clm: tools/clm/clm
	cp tools/clm/clm ../bin/clm
	../bin/patch_bin ../bin/clm CLEANLIB $(CWD)/../exe
	../bin/patch_bin ../bin/clm CLEANPATH $(CWD)/../StdEnv

tools/clm/clm:
	cd tools/clm; \
	$(MAKE) -f Makefile.macho64 clm

tools/clm/clms:
	cd tools/clm; \
	$(MAKE) -f Makefile.macho64 clms
	../bin/patch_bin tools/clm/clms CLEANLIB $(CWD)/../exe
	../bin/patch_bin tools/clm/clms CLEANPATH $(CWD)/../StdEnv

libraries/ArgEnvUnix/ArgEnvC.o:
	cd libraries/ArgEnvUnix; make ArgEnvC.o

compiler/main/Unix/set_return_code_c.o:
	cd compiler/main/Unix; make all

../exe/cocl: compiler/cocl
	cp compiler/cocl ../exe/cocl

compiler/cocl: libraries/ArgEnvUnix/ArgEnvC.o compiler/main/Unix/set_return_code_c.o
	cd compiler; \
	cd backendC/CleanCompilerSources; make -f Makefile.linux64;  cd ../..; \
	../../bin/clm -O _system; \
	../../bin/clm -nt -nw -ci -ns -nr -I backend -I frontend -I main -I main/Unix -O compare_constructor ; \
	../../bin/clm -gcm -h 48M -nt -nw -ci -nr -I backend -I frontend -I main -I main/Unix \
	-I ../libraries/ArgEnvUnix \
	-l backendC/CleanCompilerSources/backend.a \
	cocl -o cocl

../bin/htoclean: tools/htoclean/htoclean
	cp tools/htoclean/htoclean ../bin/htoclean

tools/htoclean/htoclean:
	cd tools/htoclean/htoclean\ source\ code; \
	../../../../bin/clm -I ../../../libraries/ArgEnvUnix -h 4m -nt -nr -I unix htoclean -o ../htoclean

../bin/BatchBuild: tools/CleanIDE/BatchBuild/BatchBuild
	cp tools/CleanIDE/BatchBuild/BatchBuild ../bin/BatchBuild

tools/CleanIDE/BatchBuild/BatchBuild:
	cd tools/CleanIDE; \
	../../../bin/clm -nt -nr -I BatchBuild -I Pm -I MacOSX -I Util -I Interfaces/LinkerInterface -I ../../libraries/StdLib -I ../../libraries/Directory -I ../../libraries/ArgEnvUnix BatchBuild -o BatchBuild/BatchBuild

../bin/cpm: tools/CleanIDE/cpm/cpm
	cp tools/CleanIDE/cpm/cpm ../bin/cpm

tools/CleanIDE/cpm/cpm:
	cd tools/CleanIDE; \
	../../../bin/clm -nt -nr -I cpm -I cpm/Posix -I BatchBuild -I Pm -I MacOSX -I Util -I Interfaces/LinkerInterface -I ../../libraries/StdLib -I ../../libraries/Directory -I ../../libraries/ArgEnvUnix Cpm -o cpm/cpm

CWD = $(MCWD)$(GMCWD)
MCWD : sh=pwd
GMCWD = ${shell echo `pwd`}

