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

INC:=${INC_stringifor}
LIB:=${LIB_stringifor}

prog:=main main2

all: ${prog}

${prog}: %: %.o
	${FC} ${FFLAGS} $^ ${LIB} -o $@

%.o: %.f90
	${FC} ${FFLAGS} -c ${INC} $<
%.o: %.F90
	${FC} ${FFLAGS} -c ${INC} $<

#main.o main2.o: ftlHashMapStringDirectionModule.o
#main main2: ftlHashMapStringDirectionModule.o directionModule.o
#ftlHashMapStringDirectionModule.o: directionModule.o
