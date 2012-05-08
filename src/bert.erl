%%% See http://github.com/mojombo/bert.erl for documentation.
%%% MIT License - Copyright (c) 2009 Tom Preston-Werner <tom@mojombo.com>

-module(bert).
-version('1.1.0').
-author("Tom Preston-Werner").

-export([encode/1, decode/1]).
-export_type([bert_term/0, simple_term/0, complex_term/0]).
-export_type([bnil/0, bbool/0, bdict/0, btime/0, bregex/0]).

-type bert_term() :: simple_term() | complex_term().
-type complex_term() :: bnil() | bbool() | bdict() | btime() | bregex().
-type simple_term() :: integer() | float() | atom() | tuple() | [byte()] | binary().

-type bnil() :: {bert, nil}.
-type bbool() :: {bert, true} | {bert, false}.
-type bdict() :: {bert, dict, [{atom(), any()}]}.
-type btime() :: {bert, time, integer(), integer(), integer()}.
-type bregex() :: {bert, binary(), [atom()]}.

%%---------------------------------------------------------------------------
%% Public API

-spec encode(bert_term()) -> binary().

encode(Term) ->
  term_to_binary(encode_term(Term)).

-spec decode(binary()) -> bert_term().

decode(Bin) ->
  decode_term(binary_to_term(Bin)).

%%---------------------------------------------------------------------------
%% Encode

-spec encode_term(term()) -> bert_term().
encode_term([]) -> {bert, nil};
encode_term(true) -> {bert, true};
encode_term(false) -> {bert, false};
encode_term(L) when is_list(L) -> lists:map((fun encode_term/1), L);
encode_term(T) when is_tuple(T) ->
    TList = tuple_to_list(T),
    TList2 = lists:map((fun encode_term/1), TList),
    list_to_tuple(TList2);
encode_term(D) when dict =:= element(1, D) -> {bert, dict, dict:to_list(D)};
encode_term(Term) -> Term.

%%---------------------------------------------------------------------------
%% Decode

-spec decode_term(term()) -> bert_term().
decode_term({bert, nil}) -> [];
decode_term({bert, true}) -> true;
decode_term({bert, false}) -> false;
decode_term({bert, Other}) -> {bert, Other};
decode_term(L) when is_list(L) -> lists:map((fun decode_term/1), L);
decode_term(T) when is_tuple(T) ->
    TList = tuple_to_list(T),
    TList2 = lists:map((fun decode_term/1), TList),
    list_to_tuple(TList2);
decode_term({bert, dict, Dict}) -> dict:from_list(Dict);
decode_term(Term) -> Term.
