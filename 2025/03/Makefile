F90:=gfortran
#FFLAGS:=-g -fback-trace -Wall
FFLAGS:=-O3
INC:=$(shell pkg-config --cflags stringifor flogging)
LIB:=$(shell pkg-config --libs stringifor flogging)

all: main main2

main main2: %: %.f90
	${F90} ${FFLAGS} ${INC} $^ ${LIB} -o $@
