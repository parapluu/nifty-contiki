.PHONY: default fast all get-deps compile dialyzer tests clean mrproper

ERL_INCLUDE = $(PWD):$(ERL_LIBS)

ifneq (,$(findstring Windows,$(OS)))
    SEP := $(strip \)
else
    SEP := $(strip /)
endif

BEAMS = ebin$(SEP)nifty_clangparse.beam \
	ebin$(SEP)nifty.beam \
	ebin$(SEP)nifty_compiler.beam \
	ebin$(SEP)nifty_filters.beam \
	ebin$(SEP)nifty_rebar.beam \
	ebin$(SEP)nifty_tags.beam \
	ebin$(SEP)nifty_types.beam \
	ebin$(SEP)nifty_utils.beam

JAVA_SRC_PATH = cooja-plugin$(SEP)java$(SEP)se$(SEP)uu$(SEP)it$(SEP)parapluu$(SEP)cooja_node$(SEP)
JAVA_SRC = $(JAVA_SRC_PATH)MessageHandler.java \
           $(JAVA_SRC_PATH)MoteObserver.java \
           $(JAVA_SRC_PATH)SerialObserver.java \
           $(JAVA_SRC_PATH)SocketControlPlugin.java

REBAR := .$(SEP)rebar

default: fast

fast: get-deps compile

all: default dialyze tests

get-deps:
	$(REBAR) get-deps

compile: plugin
	$(REBAR) compile

plugin: $(JAVA_SRC)
	ant -f cooja-plugin$(SEP)build.xml

plugin-clean:
	ant -f cooja-plugin$(SEP)build.xml clean

dialyze: compile
	dialyzer -n -nn -Wunmatched_returns ebin $(find .  -path 'deps/*/ebin/*.beam')

fdialyze: compile
	dialyzer -n -nn -Wunmatched_returns $(BEAMS)

tests: compile
	ERL_LIBS=$(ERL_INCLUDE) $(REBAR) clean compile eunit skip_deps=true

doc:
	$(REBAR) doc skip_deps=true

clean: plugin-clean
	$(REBAR) clean

mrproper: clean
	rm -rf deps/
