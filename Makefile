REBAR=`which rebar || which ./rebar`
DIALYZER=`which dialyzer`
CT_RUN=`which ct_run`

default: unit

compile:
	@${REBAR} compile

clean:
	@${REBAR} clean

unit:
	@-mkdir -p logs/
	@${CT_RUN} -pa ebin/ deps/*/ebin/ -dir test/ -logdir logs/

bertext: priv/ruby/bert/ext/bert/c/Makefile
	$(cd priv/ruby/bert/ext/bert/c; make)

priv/ruby/bert/ext/bert/c/Makefile:
	$(cd priv/ruby/bert/ext/bert/c; ruby extconf.rb)

analyze: compile plt
	@${DIALYZER} --plt app.plt -Wno_opaque -Wunderspecs -Wrace_conditions -Werror_handling -Wunmatched_returns ./ebin

plt: app.plt
	@${DIALYZER} --check_plt --plt app.plt

app.plt:
	@${DIALYZER} --build_plt --apps stdlib kernel crypto erts compiler --output_plt app.plt -r ebin
