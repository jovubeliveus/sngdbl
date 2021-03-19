# makefile for the sgnl benchmark for linux (j.b. 9/95)

FF = ifort
CC = gcc
FFLAGS = -fast 
CCLAGS = -O3
# FFLAGS = -g -save-temps -fpic -Wl,-no_pie
# CCLAGS = -g

.f.o:
	${FF} ${FFLAGS} -c $<
.c.o:
	${CC} ${CCLAGS} -c $<

OBJECTS = sngdbl.o tick.o

sngdbl: ${OBJECTS}
	${FF} ${FFLAGS} -o $@ ${OBJECTS} ${LIBS}

test: sngdbl
	./sngdbl 1024 > results.txt

clean::
	@rm -f *.o
	@rm -f sngdbl