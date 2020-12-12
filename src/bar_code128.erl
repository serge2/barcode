-module(bar_code128).
-behaviour(barcode).

-export([
         encode/1
        ]).

% https://en.wikipedia.org/wiki/Code_128
-define(CHARSET_A, " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_"++
                [0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31]).
-define(CHARSET_B, " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~").
-define(CHARSET_C, "0123456789").
-define(VALUE,
        {
         2#11011001100, 2#11001101100, 2#11001100110, 2#10010011000, 2#10010001100,     % 0 - 4
         2#10001001100, 2#10011001000, 2#10011000100, 2#10001100100, 2#11001001000,     % 5 - 9
         2#11001000100, 2#11000100100, 2#10110011100, 2#10011011100, 2#10011001110,     % 10 - 14
         2#10111001100, 2#10011101100, 2#10011100110, 2#11001110010, 2#11001011100,     % 15 - 19
         2#11001001110, 2#11011100100, 2#11001110100, 2#11101101110, 2#11101001100,     % 20 - 24
         2#11100101100, 2#11100100110, 2#11101100100, 2#11100110100, 2#11100110010,     % 25 - 29
         2#11011011000, 2#11011000110, 2#11000110110, 2#10100011000, 2#10001011000,     % 30 - 34
         2#10001000110, 2#10110001000, 2#10001101000, 2#10001100010, 2#11010001000,     % 35 - 39
         2#11000101000, 2#11000100010, 2#10110111000, 2#10110001110, 2#10001101110,     % 40 - 44
         2#10111011000, 2#10111000110, 2#10001110110, 2#11101110110, 2#11010001110,     % 45 - 49
         2#11000101110, 2#11011101000, 2#11011100010, 2#11011101110, 2#11101011000,     % 50 - 54
         2#11101000110, 2#11100010110, 2#11101101000, 2#11101100010, 2#11100011010,     % 55 - 59
         2#11101111010, 2#11001000010, 2#11110001010, 2#10100110000, 2#10100001100,     % 60 - 64
         2#10010110000, 2#10010000110, 2#10000101100, 2#10000100110, 2#10110010000,     % 65 - 69
         2#10110000100, 2#10011010000, 2#10011000010, 2#10000110100, 2#10000110010,     % 70 - 74
         2#11000010010, 2#11001010000, 2#11110111010, 2#11000010100, 2#10001111010,     % 75 - 79
         2#10100111100, 2#10010111100, 2#10010011110, 2#10111100100, 2#10011110100,     % 80 - 84
         2#10011110010, 2#11110100100, 2#11110010100, 2#11110010010, 2#11011011110,     % 85 - 89
         2#11011110110, 2#11110110110, 2#10101111000, 2#10100011110, 2#10001011110,     % 90 - 94
         2#10111101000, 2#10111100010, 2#11110101000, 2#11110100010, 2#10111011110,     % 95 - 99
         2#10111101110, 2#11101011110, 2#11110101110                                    % 100 - 102
        }).

-define(VALUE_CODE_A, 101).
-define(VALUE_CODE_B, 100).
-define(VALUE_CODE_C, 99).
-define(VALUE_START_A, 103).
-define(VALUE_START_B, 104).
-define(VALUE_START_C, 105).

-define(CODE_A, <<2#11101011110:11>>).
-define(CODE_B, <<2#10111101110:11>>).
-define(CODE_C, <<2#10111011110:11>>).
-define(START_A, <<2#11010000100:11>>).
-define(START_B, <<2#11010010000:11>>).
-define(START_C, <<2#11010011100:11>>).
-define(STOP, <<2#1100011101011:13>>).

-define(MODE_A, 1).
-define(MODE_B, 2).
-define(MODE_C, 3).

-define(HEIGHT, 30).
-define(QUIET_ZONE_SIZE, 10).

-spec encode(Text) -> {Width, Height, BarCodeBitmap} | no_return() when
  Text :: binary(), Width::pos_integer(),
  Height :: pos_integer(), BarCodeBitmap :: bitstring().
encode(Text) ->
    Chars = string:to_graphemes(Text),
    lists:all(fun(Ch) -> Ch >= 0 andalso Ch =< 127 end, Chars) orelse error(incorrect_text),
    Mode = choose_start_mode(Chars),
    Start = start_code(Mode),
    StartValue = start_value(Mode),
    Bitstring = loop(Mode, Chars, [StartValue], Start),
    Width = bit_size(Bitstring),
    Height = ?HEIGHT,
    {Width, Height, list_to_bitstring(lists:duplicate(Height, Bitstring))}.


loop(Mode, [Ch | Rest] = Chars, Values, BinAcc) when Mode == ?MODE_A; Mode == ?MODE_B->
    case is_mode_c_optimal(Chars) of
        true ->
            CodeSet = code_set(?MODE_C),
            Value = code_set_value(?MODE_C),
            loop(?MODE_C, Chars, [Value | Values], <<BinAcc/bits, CodeSet/bits>>);
        _ ->
            case value(Ch, mode_charset(Mode)) of
                {ok, Value} ->
                    Code = translate(Value),
                    loop(Mode, Rest, [Value | Values], <<BinAcc/bits, Code:11>>);
                error ->
                    NextMode = choose_next_mode(Mode, Chars),
                    CodeSet = code_set(NextMode),
                    Value = code_set_value(NextMode),
                    loop(NextMode, Chars, [Value | Values], <<BinAcc/bits, CodeSet/bits>>)
            end
    end;

loop(?MODE_C, [Ch1, Ch2 | Rest] = Chars, Values, BinAcc) ->
    case {value(Ch1, ?CHARSET_C), value(Ch2, ?CHARSET_C)} of
        {{ok, Value1}, {ok, Value2}} ->
            Value = Value1 * 10 + Value2,
            Code = translate(Value),
            loop(?MODE_C, Rest, [Value | Values], <<BinAcc/bits, Code:11>>);
        _ ->
            NextMode = choose_next_mode(?MODE_C, Chars),
            CodeSet = code_set(NextMode),
            Value = code_set_value(NextMode),
            loop(NextMode, Chars, [Value | Values], <<BinAcc/bits, CodeSet/bits>>)
    end;

loop(?MODE_C, [_] = Chars, Values, BinAcc) ->
    NextMode = choose_next_mode(?MODE_C, Chars),
    CodeSet = code_set(NextMode),
    Value = code_set_value(NextMode),
    loop(NextMode, Chars, [Value | Values], <<BinAcc/bits, CodeSet/bits>>);

loop(_, [], Values, BinAcc) ->
    CheckSum = calc_check_sum(lists:reverse(Values)),
    io:format("Values: ~w~n", [lists:reverse(Values)]),
    io:format("CheckSum: ~w~n", [CheckSum]),
    CheckSumCode = translate(CheckSum),
    add_quiet_zone(<<BinAcc/bits, CheckSumCode:11, ?STOP/bits>>, ?QUIET_ZONE_SIZE).



choose_start_mode(Chars) ->
    Length = length(Chars),
    DigitsNum = calc_chars(Chars, ?CHARSET_C),
    if DigitsNum == 2 andalso Length == 2 -> ?MODE_C;
       DigitsNum >= 4 -> ?MODE_C;
       true ->
          ModACharsNum = calc_chars(Chars, ?CHARSET_A),
          ModBCharsNum = calc_chars(Chars, ?CHARSET_B),
          if ModBCharsNum >= ModACharsNum -> ?MODE_B; true -> ?MODE_A end
    end.


choose_next_mode(?MODE_A, _Chars) -> ?MODE_B;
choose_next_mode(?MODE_B, _Chars) -> ?MODE_A;
choose_next_mode(?MODE_C, Chars) ->
    ModACharsNum = calc_chars(Chars, ?CHARSET_A),
    ModBCharsNum = calc_chars(Chars, ?CHARSET_B),
    if ModBCharsNum >= ModACharsNum -> ?MODE_B; true -> ?MODE_A end.


-spec is_mode_c_optimal(Chars :: list(integer())) -> boolean().
is_mode_c_optimal(Chars) ->
    Length = length(Chars),
    DigitsNum = calc_chars(Chars, ?CHARSET_C),
    (DigitsNum >= 6) orelse (DigitsNum >= 4 andalso DigitsNum == Length).


calc_chars(Chars, CharSet) ->
    calc_chars(Chars, CharSet, 0).

calc_chars([Ch | Chars], CharSet, Acc) ->
    case is_member(Ch, CharSet) of
        true -> calc_chars(Chars, CharSet, Acc + 1);
        _ -> Acc
    end;
calc_chars([], _CharSet, Acc) -> Acc.

start_code(?MODE_A) -> ?START_A;
start_code(?MODE_B) -> ?START_B;
start_code(?MODE_C) -> ?START_C.

code_set(?MODE_A) -> ?CODE_A;
code_set(?MODE_B) -> ?CODE_B;
code_set(?MODE_C) -> ?CODE_C.

start_value(?MODE_A) -> ?VALUE_START_A;
start_value(?MODE_B) -> ?VALUE_START_B;
start_value(?MODE_C) -> ?VALUE_START_C.

code_set_value(?MODE_A) -> ?VALUE_CODE_A;
code_set_value(?MODE_B) -> ?VALUE_CODE_B;
code_set_value(?MODE_C) -> ?VALUE_CODE_C.

mode_charset(?MODE_A) -> ?CHARSET_A;
mode_charset(?MODE_B) -> ?CHARSET_B.


calc_check_sum([StartValue | Values]) ->
    {_, Sum} = lists:foldl(fun(Value, {I, Sum}) ->
                                  {I + 1, Sum + I * Value}
                           end, {1, StartValue}, Values),
    Sum rem 103.



-spec value(Char :: integer(), CharSet :: list(integer())) -> {ok, Value :: non_neg_integer()} | error.
value(Char, CharSet) ->
    value(Char, CharSet, 0).

value(Char, [Ch | Rest], Pos) ->
    if Char == Ch -> {ok, Pos};
       true -> value(Char, Rest, Pos + 1)
    end;
value(_Char, [], _Pos) -> error.


-spec translate(Value :: integer()) -> BitCode :: non_neg_integer().
translate(Value) ->
    element(Value + 1, ?VALUE).

-spec is_member(Char :: integer(), CharSet :: list(integer())) -> boolean().
is_member(Char, CharSet) ->
    lists:member(Char, CharSet).

-spec add_quiet_zone(bitstring(), non_neg_integer()) -> bitstring().
add_quiet_zone(BarCodeData, QuietZoneSize) ->
    <<0:QuietZoneSize, BarCodeData/bits, 0:QuietZoneSize>>.

