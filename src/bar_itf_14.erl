-module(bar_itf_14).
-behaviour(barcode).

-export([
         encode/1
        ]).

-define(CHARSET, "0123456789").
-define(SCHEMA,
        {
         [n,n,w,w,n],             % n = 1bit w = 3bit
         [w,n,n,n,w],
         [n,w,n,n,w],
         [w,w,n,n,n],
         [n,n,w,n,w],
         [w,n,w,n,n],
         [n,w,w,n,n],
         [n,n,n,w,w],
         [w,n,n,w,n],
         [n,w,n,w,n]
        }).
-define(START_SCHEMA, [n,n,n,n]).
-define(STOP_SCHEMA,  [w,n,n]).

-define(HEIGHT, 30).
-define(QUIET_ZONE_SIZE, 10).
-define(BEARER_ZONE_SIZE, 5).

-spec encode(Text) -> {Width, Height, BarCodeBitmap} | no_return() when
  Text :: binary(), Width::pos_integer(),
  Height :: pos_integer(), BarCodeBitmap :: bitstring().
encode(Text) ->
    Chars = string:to_graphemes(Text),
    length(Chars) == 14 orelse error(incorrect_text),
    lists:all(fun(Ch) -> is_member(Ch, ?CHARSET) end, Chars) orelse error(incorrect_text),
    Bitstring = loop(Chars, [], gen_code(?START_SCHEMA)),
    BitstringWithBearerZone = <<-1:?BEARER_ZONE_SIZE, Bitstring/bits, -1:?BEARER_ZONE_SIZE>>,
    Width = bit_size(BitstringWithBearerZone),
    BearerZone = list_to_bitstring(lists:duplicate(?BEARER_ZONE_SIZE, <<-1:Width>>)),
    CentralZone = list_to_bitstring(lists:duplicate(?HEIGHT, BitstringWithBearerZone)),
    Bitmap = <<BearerZone/bits, CentralZone/bits, BearerZone/bits>>,
    Height = ?HEIGHT + ?BEARER_ZONE_SIZE * 2,
    {Width, Height, Bitmap}.

loop([Ch1, Ch2 | Rest], Values, BinAcc) ->
    Value1 = value(Ch1, ?CHARSET),
    Value2 = value(Ch2, ?CHARSET),
    Schema1 = translate(Value1),
    Schema2 = translate(Value2),
    CodeBits = gen_code(merge_schemas(Schema1, Schema2)),
    loop(Rest, [Value2, Value1 | Values], <<BinAcc/bits, CodeBits/bits>>);

loop([] = _Chars, Values, BinAcc) ->
    io:format("Values: ~w~n", [lists:reverse(Values)]),
    add_quiet_zone(<<BinAcc/bits, (gen_code(?STOP_SCHEMA))/bits>>, ?QUIET_ZONE_SIZE).

merge_schemas(Schema1, Schema2) ->
    lists:foldr(fun({W1, W2}, Acc) -> [W1, W2| Acc] end, [], lists:zip(Schema1, Schema2)).

gen_code(Schema) ->
    {_, Code} = lists:foldl(fun(W, {BitType, CodeAcc}) ->
                                   {1 - BitType, <<CodeAcc/bits, (gen(BitType, W))/bits>>}
                            end, {1, <<>>}, Schema), % Allways begins with black (1)
    Code.

gen(0, n) -> <<0:1>>;
gen(0, w) -> <<2#000:3>>;
gen(1, n) -> <<1:1>>;
gen(1, w) -> <<2#111:3>>.


-spec value(Char :: integer(), CharSet :: list(integer())) -> Value :: non_neg_integer().
value(Char, CharSet) ->
    value(Char, CharSet, 0).

value(Char, [Ch | Rest], Pos) ->
    if Char == Ch -> Pos;
       true -> value(Char, Rest, Pos + 1)
    end.

-spec translate(Value :: non_neg_integer()) -> list(n|w).
translate(Value) ->
    element(Value + 1, ?SCHEMA).

-spec is_member(Char :: integer(), CharSet :: list(integer())) -> boolean().
is_member(Char, CharSet) ->
    lists:member(Char, CharSet).

-spec add_quiet_zone(bitstring(), non_neg_integer()) -> bitstring().
add_quiet_zone(BarCodeData, QuietZoneSize) ->
    <<0:QuietZoneSize, BarCodeData/bits, 0:QuietZoneSize>>.
                                                                