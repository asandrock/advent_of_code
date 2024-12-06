FC:=gfortran
FFLAGS:=-Wall
INC:=-I${HOME}/Software/StringiFor/lib/mod
LIB:=-L${HOME}/Software/StringiFor/lib -lstringifor

all: main main2

main main2: %: %.f90
	${FC} ${FFLAGS} ${INC} $^ ${LIB} -o $@ 
