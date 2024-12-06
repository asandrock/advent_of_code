FC:=gfortran
FFLAGS:=-Wall
INC:=-I${HOME}/Software/StringiFor/lib/mod
LIB:=-L${HOME}/Software/StringiFor/lib -lstringifor

prog:=main main2 test

all: ${prog}

${prog}: %: %.f90
	${FC} ${FFLAGS} ${INC} $^ ${LIB} -o $@ 
