FC:=gfortran
FFLAGS:=-Wall
INC:=-I${HOME}/Software/StringiFor/lib/mod\
 -I${HOME}/Software/flogging/build/include
LIB:=-L${HOME}/Software/StringiFor/lib -lstringifor\
 -L${HOME}/Software/flogging/build -lflogging

prog:=main main2
all: ${prog}

${prog}: %: %.f90
	${FC} ${FFLAGS} ${INC} $^ ${LIB} -o $@ 
