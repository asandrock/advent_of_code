FC:=gfortran
FFLAGS:=-Wall -O0 -g
#FFLAGS:=-O3

stringifor:=${HOME}/Software/StringiFor/lib
INC_stringifor:=-I${stringifor}/mod
LIB_stringifor:=-L${stringifor} -lstringifor

INC_scifor:=`pkg-config --cflags scifor`
LIB_scifor:=`pkg-config --libs scifor`

fmlib:=${HOME}/Software/fmlib/FM_files
INC_fmlib:=-I${fmlib}
LIB_fmlib:=${fmlib}/fm.o ${fmlib}/fmsave.o ${fmlib}/fmzm90.o

INC_ftl:=`pkg-config --cflags ftl`
LIB_ftl:=`pkg-config --libs ftl`

INC:=${INC_ftl}
LIB:=${LIB_ftl}

prog:=main main2

all: ${prog}

${prog}: %: %.o
	${FC} ${FFLAGS} $^ ${LIB} -o $@

main main2: ftlDynArrayIntModule.o
main.o main2.o: ftlDynArrayIntModule.o

%.o: %.f90
	${FC} ${FFLAGS} ${INC} $< -c
%.o: %.F90
	${FC} ${FFLAGS} ${INC} $< -c

clean:
	rm -r *.o *.mod ${prog}

.PHONY: all clean
