-module(bert_tests).
-include_lib("eunit/include/eunit.hrl").

%% encode

encode_list_nesting_test() ->
  Bert = term_to_binary([foo, {bert, true}]),
  Bert = bert:encode([foo, true]).

encode_tuple_nesting_test() ->
  Bert = term_to_binary({foo, {bert, true}}),
  Bert = bert:encode({foo, true}).

%% decode

decode_list_nesting_test() ->
  Bert = term_to_binary([foo, {bert, true}]),
  Term = [foo, true],
  Term = bert:decode(Bert).

decode_tuple_nesting_test() ->
  Bert = term_to_binary({foo, {bert, true}}),
  Term = {foo, true},
  Term = bert:decode(Bert).
