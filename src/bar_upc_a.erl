-module(bar_upc_a).
-behaviour(barcode).

-export([
         encode/1
        ]).

-define(CHARSET, "0123456789").
-define(CODE,
        {
         2#0001101,
         2#0011001,
         2#0010011,
         2#0111101,
         2#0100011,
         2#0110001,
         2#0101111,
         2#0111011,
         2#0110111,
         2#0001011
        }).
-define(START, <<2#101:3>>).
-define(MIDDLE, <<2#01010:5>>).
-define(STOP, <<2#101:3>>).

-define(HEIGHT, 30).
-define(QUIET_ZONE_SIZE, 10).

-spec encode(Text) -> {Width, Height, BarCodeBitmap} | no_return() when
  Text :: binary(), Width::pos_integer(),
  Height :: pos_integer(), BarCodeBitmap :: bitstring().
encode(Text) ->
    Chars = string:to_graphemes(Text),
    11 == length(Chars) orelse error(incorrect_text),
    lists:all(fun(Ch) -> is_member(Ch, ?CHARSET) end, Chars) orelse error(incorrect_text),
    Bitstring = h1_loop(Chars, [], ?START, 1),
    Width = bit_size(Bitstring),
    Height = ?HEIGHT,
    {Width, Height, list_to_bitstring(lists:duplicate(Height, Bitstring))}.

h1_loop([Ch | Rest], Values, BinAcc, I) when I =< 6 ->
    Value = value(Ch, ?CHARSET),
    Code = translate(Value, l),
    h1_loop(Rest, [Value | Values], <<BinAcc/bits, Code:7>>, I + 1);

h1_loop(Chars, Values, BinAcc, I) when I == 7 ->
    h2_loop(Chars, Values, <<BinAcc/bits, ?MIDDLE/bits>>).

h2_loop([Ch | Rest], Values, BinAcc) ->
    Value = value(Ch, ?CHARSET),
    Code = translate(Value, r),
    h2_loop(Rest, [Value | Values], <<BinAcc/bits, Code:7>>);

h2_loop([] = _Chars, Values, BinAcc) ->
    CheckSum = calc_check_sum(Values),
    io:format("Values: ~w~n", [lists:reverse(Values)]),
    io:format("CheckSum: ~w~n", [CheckSum]),
    CheckSumCode = translate(CheckSum, r),
    add_quiet_zone(<<BinAcc/bits, CheckSumCode:7, ?STOP/bits>>, ?QUIET_ZONE_SIZE).


-spec calc_check_sum(Values :: list(non_neg_integer())) -> CheckSum :: non_neg_integer().
calc_check_sum(Values) ->
    {_, Sum} = lists:foldl(fun(Value, {Odd, Sum}) ->
                                  {not Odd, Sum + if Odd -> Value * 3; true -> Value end}
                           end, {true, 0}, Values),
    (10 - Sum rem 10) rem 10.


-spec value(Char :: integer(), CharSet :: list(integer())) -> Value :: non_neg_integer().
value(Char, CharSet) ->
    value(Char, CharSet, 0).

value(Char, [Ch | Rest], Pos) ->
    if Char == Ch -> Pos;
       true -> value(Char, Rest, Pos + 1)
    end.


-spec translate(Value :: non_neg_integer(), CodeType :: l | r) -> BitCode :: non_neg_integer().
translate(Value, l) ->
    element(Value + 1, ?CODE);

translate(Value, r) ->
    127 - translate(Value, l). % Inverted values

-spec is_member(Char :: integer(), CharSet :: list(integer())) -> boolean().
is_member(Char, CharSet) ->
    lists:member(Char, CharSet).

-spec add_quiet_zone(bitstring(), non_neg_integer()) -> bitstring().
add_quiet_zone(BarCodeData, QuietZoneSize) ->
    <<0:QuietZoneSize, BarCodeData/bits, 0:QuietZoneSize>>.
