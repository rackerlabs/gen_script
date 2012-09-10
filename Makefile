REBAR=`which rebar || which ./rebar`
DIALYZER=`which dialyzer`
CT_RUN=`which ct_run`

default: unit

compile:
	@${REBAR} compile skip_deps=true

compileall:
	@${REBAR} compile

getdeps:
	@${REBAR} get-deps

clean:
	@${REBAR} clean skip_deps=true

cleanall: clean
	@rm -rf deps/*

unit:
	@-mkdir -p logs/
	@${REBAR} ct skip_deps=true verbose=1

bertext: priv/ruby/bert/ext/bert/c/Makefile
	$(cd priv/ruby/bert/ext/bert/c; make)

priv/ruby/bert/ext/bert/c/Makefile:
	$(cd priv/ruby/bert/ext/bert/c; ruby extconf.rb)

analyze: compile app.plt
	@${DIALYZER} --plt app.plt -Wno_opaque -Wunderspecs -Werror_handling -Wunmatched_returns ./ebin

app.plt:
	@${DIALYZER} --build_plt --apps stdlib kernel crypto erts compiler --output_plt app.plt -r ebin

jenkins: cleanall getdeps compileall unit analyze
