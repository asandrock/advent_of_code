F90:=gfortran
FFLAGS:=-g -fbacktrace -Wall
#FFLAGS:=-O3
INC:=$(shell pkg-config --cflags ftl)
LIB:=$(shell pkg-config --libs ftl)

all: main main2

main main2: %: %.f90
	${F90} ${FFLAGS} ${INC} $^ ${LIB} -o $@

#main2: ftlHashSetIntModule.o
main2: ftlArrayIntModule.o

ftlHashSetIntModule.o ftlArrayIntModule.o: %.o: %.F90
	${F90} ${FFLAGS} ${INC} $< -c
