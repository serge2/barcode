-module(bar_std_2_of_5).
-behaviour(barcode).

-export([
         encode/1
        ]).

-define(CHAR, "0123456789").
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

-spec encode(String :: unicode:chardata()) -> BarCodeBitmap :: bitstring().
encode(String) ->
    Chars = string:to_graphemes(String),
    Odd = length(Chars) rem 2 == 1,
    {Bin, _, _, Sum} =
        lists:foldl(fun(Char, {Bin, I, IsOdd, Sum}) ->
                           Index = index(Char),
                           NewSum = Sum + if IsOdd -> Index * 3; true -> Index end,
                           NewBin = <<Bin/bits, (translate(Index)):14>>,
                           {NewBin, I + 1, not IsOdd, NewSum}
                    end, {<<>>, 1, Odd, 0}, Chars),
    CheckSum = (10 - Sum rem 10) rem 10,
    CheckSumCode = <<(translate(CheckSum)):14>>,
    <<?START/bits, Bin/bits, CheckSumCode/bits, ?STOP/bits>>.

-spec index(Char :: integer()) -> Position :: non_neg_integer().
index(Char) ->
    index(Char, ?CHAR, 0).

index(Char, [Char|_], Index) -> Index;
index(Char, List, Index) -> index(Char, tl(List), Index + 1).

translate(Index) ->
    element(Index + 1, ?CODE).
