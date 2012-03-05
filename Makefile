bertext: priv/ruby/bert/ext/bert/c/Makefile
	cd priv/ruby/bert/ext/bert/c; make

priv/ruby/bert/ext/bert/c/Makefile:
	cd priv/ruby/bert/ext/bert/c; ruby extconf.rb
