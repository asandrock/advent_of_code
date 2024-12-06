all: bin/main1

FC:=gfortran
INC:=`pkg-config --cflags ftl stringifor`
LIB:=`pkg-config --libs ftl stringifor`

bin/main1: main1.f90 $(addprefix bin/,$(addsuffix .o,mod_module ftlHashMapStringModModule))
	${FC} ${INC} -J bin $^ ${LIB} -o $@

bin/%.o: %.f90
	${FC} -J bin ${INC} $< -c -o $@
bin/%.o: %.F90
	${FC} -J bin ${INC} $< -c -o $@
