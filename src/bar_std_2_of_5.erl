-module(bar_std_2_of_5).
-behaviour(barcode).

-export([
         encode/1
        ]).

% https://barcode-coder.com/en/standard-2-of-5-specification-103.html

-define(CHARSET, "0123456789").
-define(CODE,
        {
         2#10101110111010,        % NNWWN   N = 10 W = 1110
         2#11101010101110,        % WNNNW
         2#10111010101110,        % NWNNW
         2#11101110101010,        % WWNNN
         2#10101110101110,        % NNWNW
         2#11101011101010,        % WNWNN
         2#10111011101010,        % NWWNN
         2#10101011101110,        % NNNWW
         2#11101010111010,        % WNNWN
         2#10111010111010         % NWNWN
        }).
-define(START, <<2#11011010:8>>).
-define(STOP,  <<2#11010110:8>>).

-spec encode(Text :: unicode:chardata()) -> BarCodeBitmap :: bitstring() | no_return().
encode(Text) ->
    Chars = string:to_graphemes(Text),
    lists:all(fun(Ch) -> is_member(Ch, ?CHARSET) end, Chars) orelse error(incorrect_text),
    loop(Chars, [], ?START).

loop([Ch | Rest], Values, BinAcc) ->
    Value = value(Ch, ?CHARSET),
    Code = translate(Value),
    loop(Rest, [Value | Values], <<BinAcc/bits, Code:14>>);

loop([] = _Chars, Values, BinAcc) ->
    io:format("Values: ~w~n", [lists:reverse(Values)]),
    <<BinAcc/bits, ?STOP/bits>>.

-spec value(Char :: integer(), CharSet :: list(integer())) -> Value :: non_neg_integer().
value(Char, CharSet) ->
    value(Char, CharSet, 0).

value(Char, [Ch | Rest], Pos) ->
    if Char == Ch -> Pos;
       true -> value(Char, Rest, Pos + 1)
    end.

-spec translate(Value :: non_neg_integer()) -> BitCode :: non_neg_integer().
translate(Value) ->
    element(Value + 1, ?CODE).

-spec is_member(Char :: integer(), CharSet :: list(integer())) -> boolean().
is_member(Char, CharSet) ->
    lists:member(Char, CharSet).

