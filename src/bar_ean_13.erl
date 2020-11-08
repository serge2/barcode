-module(bar_ean_13).
-behaviour(barcode).

-export([
         encode/1
        ]).

-define(CHAR, "0123456789").

-define(HALF1_SCHEMA,
        {
         {l,l,l,l,l,l},  % 0
         {l,l,g,l,g,g},  % 1
         {l,l,g,g,l,g},  % 2
         {l,l,g,g,g,l},  % 3
         {l,g,l,l,g,g},  % 4
         {l,g,g,l,l,g},  % 5
         {l,g,g,g,l,l},  % 6
         {l,g,l,g,l,g},  % 7
         {l,g,l,g,g,l},  % 8
         {l,g,g,l,g,l}   % 9
        }).
% L-code and G-code
-define(CODE,
        {
         {2#0001101, 2#0100111},  % 0
         {2#0011001, 2#0110011},  % 1
         {2#0010011, 2#0011011},  % 2
         {2#0111101, 2#0100001},  % 3
         {2#0100011, 2#0011101},  % 4
         {2#0110001, 2#0111001},  % 5
         {2#0101111, 2#0000101},  % 6
         {2#0111011, 2#0010001},  % 7
         {2#0110111, 2#0001001},  % 8
         {2#0001011, 2#0010111}   % 9
        }).
-define(START, <<2#101:3>>).
-define(MIDDLE, <<2#01010:5>>).
-define(STOP, <<2#101:3>>).

-spec encode(String :: unicode:chardata()) -> BarCodeBitmap :: bitstring().
encode(String) ->
    [First | Chars] = string:to_graphemes(String),
    11 == length(Chars) orelse error(incorrect_arg),
    FirstIndex = index(First),
    H1Schema = half1_schema(FirstIndex),
    {Bin1, Bin2, _, Sum} =
        lists:foldl(fun(Char, {Bin1, Bin2, I, Sum}) ->
                           Index = index(Char),
                           NewSum = Sum + ((I rem 2) * 2 + 1) * Index,
                           {NewBin1, NewBin2} =
                               if I =< 6 ->
                                      Type = element(I, H1Schema),
                                      {<<Bin1/bits, (translate(Index, Type)):7>>, Bin2};
                                  true ->
                                      {Bin1, <<Bin2/bits, (translate(Index, r)):7>>}
                               end,
                           {NewBin1, NewBin2, I + 1, NewSum}
                    end, {<<>>, <<>>, 1, FirstIndex}, Chars),
    CheckSum = (10 - Sum rem 10) rem 10,
    CheckSumCode = <<(translate(CheckSum, r)):7>>,
    io:format("CheckSum:~w~n", [CheckSum]),
    <<?START/bits, Bin1/bits, ?MIDDLE/bits, Bin2/bits, CheckSumCode/bits, ?STOP/bits>>.

-spec index(Char :: integer()) -> Position :: non_neg_integer().
index(Char) ->
    index(Char, ?CHAR, 0).

index(Char, [Char|_], Index) -> Index;
index(Char, List, Index) -> index(Char, tl(List), Index + 1).

half1_schema(Index) ->
    element(Index + 1, ?HALF1_SCHEMA).

translate(Index, l) ->
    element(1, element(Index + 1, ?CODE));

translate(Index, r) ->
    127 - element(1, element(Index + 1, ?CODE));

translate(Index, g) ->
    element(2, element(Index + 1, ?CODE)).
