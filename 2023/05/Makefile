FC:=gfortran
FFLAGS:=-Wall -O3
INC:=-I${HOME}/Software/StringiFor/lib/mod
LIB:=-L${HOME}/Software/StringiFor/lib -lstringifor

prog:=main main2

all: ${prog}

${prog}: %: %.f90
	${FC} ${FFLAGS} ${INC} $^ ${LIB} -o $@ 
