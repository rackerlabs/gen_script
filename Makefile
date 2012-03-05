default: unit

compile:
	rebar compile

unit:
	rebar eunit

bertext: priv/ruby/bert/ext/bert/c/Makefile
	$(cd priv/ruby/bert/ext/bert/c; make)

priv/ruby/bert/ext/bert/c/Makefile:
	$(cd priv/ruby/bert/ext/bert/c; ruby extconf.rb)
