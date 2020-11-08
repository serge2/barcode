-module(bar_upc_a).
-behaviour(barcode).

-export([
         encode/1
        ]).

-define(CHAR, "0123456789").
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

-spec encode(String :: unicode:chardata()) -> BarCodeBitmap :: bitstring().
encode(String) ->
    Chars = string:to_graphemes(String),
    11 == length(Chars) orelse error(incorrect_arg),
    {Bin1, Bin2, _, Sum} =
        lists:foldl(fun(Char, {Bin1, Bin2, I, Sum}) ->
                           Index = index(Char),
                           NewSum = Sum + ((I rem 2) * 2 + 1) * Index,
                           {NewBin1, NewBin2} =
                               if I =< 6 -> {<<Bin1/bits, (translate(Index, l)):7>>, Bin2};
                                  true -> {Bin1, <<Bin2/bits, (translate(Index, r)):7>>}
                               end,
                           {NewBin1, NewBin2, I + 1, NewSum}
                    end, {<<>>, <<>>, 1, 0}, Chars),
    CheckSum = (10 - Sum rem 10) rem 10,
    CheckSumCode = <<(translate(CheckSum, r)):7>>,
    <<?START/bits, Bin1/bits, ?MIDDLE/bits, Bin2/bits, CheckSumCode/bits, ?STOP/bits>>.

-spec index(Char :: integer()) -> Position :: non_neg_integer().
index(Char) ->
    index(Char, ?CHAR, 0).

index(Char, [Char|_], Index) -> Index;
index(Char, List, Index) -> index(Char, tl(List), Index + 1).

translate(Index, l) ->
    element(Index + 1, ?CODE);

translate(Index, r) ->
    127 - element(Index + 1, ?CODE).
