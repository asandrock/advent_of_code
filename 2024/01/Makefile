progs:=main1 main2
F90:=gfortran
FFLAGS:=-Wall
INC:=`pkg-config --cflags stdlib`
LIB:=`pkg-config --libs stdlib`

all: ${progs}

${progs}: %: %.f90
	${F90} ${FFLAGS} ${INC} $^ ${LIB} -o $@
