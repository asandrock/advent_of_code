FC:=gfortran
FFLAGS:=-Wall
INC:=-I${HOME}/Software/StringiFor/lib/mod\
 -I${HOME}/Software/flogging/build/include
LIB:=-L${HOME}/Software/StringiFor/lib -lstringifor\
 -L${HOME}/Software/flogging/build -lflogging

all: main main2 test

main main2 test: %: %.f90
	${FC} ${FFLAGS} ${INC} $^ ${LIB} -o $@ 
