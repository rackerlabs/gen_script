-module(bert_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("../include/test.hrl").

-export([all/0]).
-export([bert_encoding_is_reversable/1]).
-export([encode_list_nesting_test/1, encode_tuple_nesting_test/1]).
-export([decode_list_nesting_test/1, decode_tuple_nesting_test/1]).

all() -> ?CT_REGISTER_TESTS(?MODULE).

%% encode

bert_encoding_is_reversable(_Config) ->
    P = ?FORALL(Term, term(),
		begin
		    Term =:= bert:decode( bert:encode(Term) )
		end),
    true = proper:quickcheck(P, [long_result, verbose, {numtests, 1000}]).

encode_list_nesting_test(_Config) ->
  Bert = term_to_binary([foo, {bert, true}]),
  Bert = bert:encode([foo, true]).

encode_tuple_nesting_test(_Config) ->
  Bert = term_to_binary({foo, {bert, true}}),
  Bert = bert:encode({foo, true}).

%% decode

decode_list_nesting_test(_Config) ->
  Bert = term_to_binary([foo, {bert, true}]),
  Term = [foo, true],
  Term = bert:decode(Bert).

decode_tuple_nesting_test(_Config) ->
  Bert = term_to_binary({foo, {bert, true}}),
  Term = {foo, true},
  Term = bert:decode(Bert).
