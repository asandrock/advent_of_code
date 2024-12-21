progs:=main1 test #main2
F90:=gfortran
FFLAGS:=-Wall -g -fbacktrace -ffpe-trap=invalid,overflow
INC:=`pkg-config --cflags stringifor`
LIB:=`pkg-config --libs stringifor`

all: ${progs}

${progs}: %: %.f90
	${F90} ${FFLAGS} ${INC} $^ ${LIB} -o $@
