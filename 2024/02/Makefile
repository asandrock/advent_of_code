progs:=main1 main2 test
F90:=gfortran
FFLAGS:=-Wall -g -fbacktrace
INC:=`pkg-config --cflags stdlib`
LIB:=`pkg-config --libs stdlib`

all: ${progs}

${progs}: %: %.f90
	${F90} ${FFLAGS} ${INC} $^ ${LIB} -o $@
