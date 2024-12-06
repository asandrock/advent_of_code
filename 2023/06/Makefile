FC:=gfortran
FFLAGS:=-Wall -O3

stringifor:=${HOME}/Software/StringiFor/lib
fmlib:=${HOME}/Software/fmlib/FM_files

INC:=-I${stringifor}/mod `pkg-config --cflags scifor` -I${fmlib}
LIB:=-L${stringifor} -lstringifor `pkg-config --libs scifor` ${fmlib}/fm.o ${fmlib}/fmsave.o ${fmlib}/fmzm90.o

prog:=main test main2

all: ${prog}

${prog}: %: %.f90
	${FC} ${FFLAGS} ${INC} $^ ${LIB} -o $@ 
