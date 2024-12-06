FC:=gfortran
FFLAGS:=-Wall
INC:=`pkg-config --cflags fortran_stdlib`
LIB:=`pkg-config --libs fortran_stdlib`

progs:=main test
all: ${progs}

${progs}: %: %.f90
	${FC} ${FFLAGS} ${INC} $^ ${LIB} -o $@
