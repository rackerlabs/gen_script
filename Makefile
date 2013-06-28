REBAR=`which rebar || which ./rebar`
DIALYZER=`which dialyzer`
CT_RUN=`which ct_run`

all: deps compile

deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

app.plt:
	@$(DIALYZER) --build_plt --output_plt app.plt --apps erts kernel stdlib crypto

dialyze: app.plt compile
	@$(DIALYZER) -q --plt app.plt -n ebin -Wunmatched_returns -Werror_handling \
		-Wrace_conditions -Wunderspecs

test:
	@-mkdir -p logs/
	@${REBAR} ct skip_deps=true verbose=1

validate: dialyze test

clean:
	@$(REBAR) clean

repl:
	@$(ERL) -pa ebin

bertext: priv/ruby/bert/ext/bert/c/Makefile
	$(cd priv/ruby/bert/ext/bert/c; make)

priv/ruby/bert/ext/bert/c/Makefile:
	$(cd priv/ruby/bert/ext/bert/c; ruby extconf.rb)

.PHONY: all test clean validate dialyze deps bertext
