#!/bin/sh
CLM=clm

(cd backendC/CleanCompilerSources; make -f Makefile.linux64)
(cd ../libraries/ArgEnvUnix; make ArgEnvC.o)
(cd main/Unix; make -f Makefile all);
$CLM -gcm -h 60M -s 2M -nt -nw -ci -nr -I backend -I frontend -I main -I main/Unix \
	-I ../libraries/ArgEnvUnix \
	-l backendC/CleanCompilerSources/backend.a \
	cocl -o cocl
