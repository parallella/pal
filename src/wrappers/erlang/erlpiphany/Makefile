#
# Macros
#

SUB_DIRECTORIES = src c-src

include vsn.mk
VSN = $(ERLP_VSN)

ESDK=$(EPIPHANY_HOME)
ELIBS=$(ESDK)/tools/host/lib
EINCS=$(ESDK)/tools/host/include
ELDF=$(ESDK)/bsps/current/internal.ldf

SRC=src
CSRC=c_src
INCLUDE=include
EBIN=ebin
PRIV=priv
EXES=$(PRIV)/ehal.so $(EBIN)/ehal.beam
DOCS=doc/index.html

all: $(EXES) $(DOCS)

$(PRIV)/ehal.so: $(CSRC)/ehal.c
	gcc -o $(PRIV)/ehal.so -fpic -shared $(CSRC)/ehal.c \
	-I${EPIPHANY_HOME}/tools/host/include	\
	-L${EPIPHANY_HOME}/tools/host/lib -le-hal -le-loader

$(EBIN)/%.beam: $(SRC)/%.erl
	erlc -I$(INCLUDE) -o$(EBIN) $<

doc/index.html : doc/overview.edoc
	./makedocs.escript

