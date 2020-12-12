-module(bar_std_2_of_5_crc).
-behaviour(barcode).

-export([
         encode/1
        ]).

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

-define(HEIGHT, 30).
-define(QUIET_ZONE_SIZE, 10).

-spec encode(Text) -> {Width, Height, BarCodeBitmap} | no_return() when
  Text :: binary(), Width::pos_integer(),
  Height :: pos_integer(), BarCodeBitmap :: bitstring().
encode(Text) ->
    Chars = string:to_graphemes(Text),
    lists:all(fun(Ch) -> is_member(Ch, ?CHARSET) end, Chars) orelse error(incorrect_text),
    Bitstring = loop(Chars, [], ?START),
    Width = bit_size(Bitstring),
    Height = ?HEIGHT,
    {Width, Height, list_to_bitstring(lists:duplicate(Height, Bitstring))}.

loop([Ch | Rest], Values, BinAcc) ->
    Value = value(Ch, ?CHARSET),
    Code = translate(Value),
    loop(Rest, [Value | Values], <<BinAcc/bits, Code:14>>);

loop([] = _Chars, Values, BinAcc) ->
    CheckSum = calc_check_sum(Values),
    io:format("Values: ~w~n", [lists:reverse(Values)]),
    io:format("CheckSum: ~w~n", [CheckSum]),
    CheckSumCode = translate(CheckSum),
    add_quiet_zone(<<BinAcc/bits, CheckSumCode:14, ?STOP/bits>>, ?QUIET_ZONE_SIZE).

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

-spec translate(Value :: non_neg_integer()) -> BitCode :: non_neg_integer().
translate(Value) ->
    element(Value + 1, ?CODE).

-spec is_member(Char :: integer(), CharSet :: list(integer())) -> boolean().
is_member(Char, CharSet) ->
    lists:member(Char, CharSet).

-spec add_quiet_zone(bitstring(), non_neg_integer()) -> bitstring().
add_quiet_zone(BarCodeData, QuietZoneSize) ->
    <<0:QuietZoneSize, BarCodeData/bits, 0:QuietZoneSize>>.
