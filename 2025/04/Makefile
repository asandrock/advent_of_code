F90:=gfortran
#FFLAGS:=-g -fbacktrace -Wall
FFLAGS:=-O3
INC:=$(shell pkg-config --cflags stringifor)
LIB:=$(shell pkg-config --libs stringifor)

all: main main2

main main2: %: %.f90
	${F90} ${FFLAGS} ${INC} $^ ${LIB} -o $@
