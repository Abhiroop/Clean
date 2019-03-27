#!/bin/sh
CLM=clm

(cd backendC/CleanCompilerSources; make)
(cd ../libraries/ArgEnvUnix; make ArgEnvC.o)
(cd main/Unix; make -f Makefile all);
$CLM -ci -I backend -I frontend -I main -I main/Unix -ABC -fusion backendconvert
$CLM -h 32M -nt -nw -ci -nr -I backend -I frontend -I main -I main/Unix \
	-I ../libraries/ArgEnvUnix \
	-l backendC/CleanCompilerSources/backend.a \
	cocl -o cocl
