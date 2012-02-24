extfile: priv/ruby/bert/ext/bert/c/Makefile
	cd priv/ruby/bert/ext/bert/c; ruby extconf.rb

bertext: extfile
	cd priv/ruby/bert/ext/bert/c; make
