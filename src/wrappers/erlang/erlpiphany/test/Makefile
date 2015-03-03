ESDK=$(EPIPHANY_HOME)
ELIBS=$(ESDK)/tools/host/lib
EINCS=$(ESDK)/tools/host/include
ELDF=$(ESDK)/bsps/current/internal.ldf

INCLUDE=../include
EBIN=../ebin
EXES=ehal_test.beam

all: $(EXES)


%.beam: %.erl
	erlc -I$(INCLUDE) $<
