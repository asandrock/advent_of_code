FC:=gfortran
#FFLAGS:=-Wall -O0 -g
FFLAGS:=-O3

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

prog:=main main2 test_hash

all: ${prog}

${prog}: %: %.f90 hash_mod.o
	${FC} ${FFLAGS} ${INC} $^ ${LIB} -o $@

hash_mod.o: hash_mod.f90
	${FC} ${FFLAGS} ${INC} $< -c
