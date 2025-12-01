F90:=gfortran
INC:=$(shell pkg-config --cflags stringifor)
LIB:=$(shell pkg-config --libs stringifor)

all: main main2

main main2: %: %.f90
	${F90} ${INC} $^ ${LIB} -o $@
