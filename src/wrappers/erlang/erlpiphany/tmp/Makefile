ESDK=$(EPIPHANY_HOME)
ELIBS=$(ESDK)/tools/host/lib
EINCS=$(ESDK)/tools/host/include
ELDF=$(ESDK)/bsps/current/internal.ldf
EXES=ehal.so ehal.beam ehal_test.beam open_test xmem_test

all: $(EXES)

ehal.so: ehal.c
	gcc -o ehal.so -fpic -shared ehal.c \
	-I${EPIPHANY_HOME}/tools/host/include	\
	-L${EPIPHANY_HOME}/tools/host/lib -le-hal

xmem_test: xmem_test.c
	gcc -O3 xmem_test.c -o xmem_test -I $(EINCS) -L $(ELIBS) -le-hal -lrt

open_test: open_test.c
	gcc -O3 open_test.c -o open_test -I $(EINCS) -L $(ELIBS) -le-hal -lrt

%.beam: %.erl
	erlc $<
