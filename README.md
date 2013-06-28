# `gen_script`

[![Build Status](https://travis-ci.org/rackerlabs/gen_script.png)](https://travis-ci.org/rackerlabs/gen_script)

`gen_script` exposes port processes through a convenient OTP interface. It's
ambition is to support many languages, but only Ruby is
currently. [BERT](http://bert-rpc.org/) is used as a exchange protocol between
the parent VM and child process.

Please see the files in `test/` for example use.
