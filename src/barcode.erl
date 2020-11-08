-module(barcode).

-callback encode(String :: unicode:chardata()) -> BarCodeBitmap :: bitstring().

-export([
         save_png/3,
         save_text/3,
         encode_to_png/2,
         encode_to_text/2
        ]).

-define(COLOR_TYPE_GRAYSCALE, 0).
-define(COLOR_TYPE_RGB, 2).
-define(COMPRESS_METHOD_DEFLATE, 0).
-define(FILTER_METHOD_0, 0).
-define(FILTER_TYPE_NONE, 0).
-define(FILTER_TYPE_SUB, 1).
-define(FILTER_TYPE_UP, 2).
-define(FILTER_TYPE_AVERAGE, 3).
-define(FILTER_TYPE_PAETH, 4).
-define(INTERLACE_METHOD_NONE, 0).
-define(INTERLACE_METHOD_ADAM7, 1).

-define(BIT_DEPTH, 1).
-define(COLOR_TYPE, ?COLOR_TYPE_GRAYSCALE).

-define(CODERS,
        [
         {code128b,   bar_code128b},
         {upc_a,      bar_upc_a},
         {ean_13,     bar_ean_13},
         {ean_8,      bar_ean_8},
         {std_2_of_5, bar_std_2_of_5}
        ]).

-spec save_png(Codec :: atom(), String :: unicode:chardata(), Filename :: file:filename_all()) -> ok.
save_png(Codec, String, Filename) ->
    Image = encode_to_png(Codec, String),
    ok = file:write_file(Filename, Image).


-spec save_text(Codec :: atom(), String :: unicode:chardata(), Filename :: file:filename_all()) -> ok.
save_text(Codec, String, Filename) ->
    Image = encode_to_text(Codec, String),
    ok = file:write_file(Filename, Image).


-spec encode_to_png(Codec :: atom(), String :: unicode:chardata()) -> FileContent :: binary().
encode_to_png(Codec, String) ->
    encode_to_png(Codec, String, 40).
encode_to_png(Codec, String, Height) ->
    BarCode = encode(Codec, String),
    QuietBarCode = add_quiet_fields(BarCode),
    InvBarCode = inverse(QuietBarCode),

    MAGIC = <<137, 80, 78, 71, 13, 10, 26, 10>>,
    IHDR = png_chunk(<<"IHDR">>, create_header(#{width => bit_size(InvBarCode),
                                                 height => Height,
                                                 bit_depth => ?BIT_DEPTH,
                                                 color_type => ?COLOR_TYPE})),
    IDAT = png_chunk(<<"IDAT">>, create_image_data(?COLOR_TYPE, ?BIT_DEPTH, InvBarCode, Height)),
    IEND = png_chunk(<<"IEND">>, <<>>),
    <<MAGIC/binary, IHDR/binary, IDAT/binary, IEND/binary>>.


-spec add_quiet_fields(bitstring()) -> bitstring().
add_quiet_fields(BarCodeData) ->
    <<0:8, BarCodeData/bits, 0:8>>.

-spec inverse(bitstring()) -> bitstring().
inverse(Bits) ->
    << <<(1 - B):1>> || <<B:1>> <= Bits >>.

-spec png_chunk(Name :: <<_:32>>, Payload :: binary()) -> Chunk :: binary().
png_chunk(Name, Payload) ->
    Length = byte_size(Payload),
    CRC = erlang:crc32(<<Name/binary, Payload/binary>>),
    <<Length:32, Name/binary, Payload/binary, CRC:32>>.

-spec create_header(Params :: map()) -> binary().
create_header(#{width := Width, height := Height, bit_depth := BitDepth, color_type := ColorType} = Params) ->
    CompressionMethod = maps:get(compression_method, Params, ?COMPRESS_METHOD_DEFLATE),
    FilterMethod = maps:get(filter_method, Params, ?FILTER_METHOD_0),
    InterlaceMethod = maps:get(interlace_method, Params, ?INTERLACE_METHOD_NONE),
    <<Width:32, Height:32, BitDepth:8, ColorType:8, CompressionMethod:8, FilterMethod:8, InterlaceMethod:8>>.

-spec create_image_data(ColorType, BitDepth, BarCodeData, Height) -> binary() when
    ColorType :: 0 | 2, BitDepth :: 1 | 2 | 4 | 8 | 16, BarCodeData :: bitstring(),
    Height :: pos_integer().
create_image_data(ColorType, BitDepth, BarCodeData, Height) ->
    Scanline = gen_scanline(ColorType, BitDepth, BarCodeData),
    FilteredPixels = filter_scanline(Scanline),
    Image = binary:copy(FilteredPixels, Height),
    zlib:compress(Image).

-spec padding(bitstring()) -> binary().
padding(Bits) ->
    TailSize = bit_size(Bits) rem 8,
    if TailSize == 0 -> Bits;
       true ->  << Bits/bits, 0:(8 - TailSize) >>
    end.

gen_scanline(?COLOR_TYPE_GRAYSCALE, BitDepth, Data) when
  BitDepth == 1; BitDepth == 2; BitDepth == 4; BitDepth == 8; BitDepth == 16 ->
    HighValue = 1 bsl BitDepth - 1,
    padding(<< <<(V * HighValue):BitDepth>> || <<V:1>> <= Data>>);

gen_scanline(?COLOR_TYPE_RGB, BitDepth, Data) when
  BitDepth == 8; BitDepth == 16 ->
    HighValue = 1 bsl BitDepth - 1,
    << <<(V * HighValue):BitDepth, (V * HighValue):BitDepth, (V * HighValue):BitDepth >> || <<V:1>> <= Data>>.

filter_scanline(Scanline) ->
    <<?FILTER_TYPE_NONE:8, Scanline/binary>>.


%-define(LINES, {"█", "▌", "▐", " "}).
-define(LINES, {"██", "█ ", " █", "  "}). % Use two symbols per two bits becouse of the issue
                                          % with display of repited "▐" characters by the konsole -
                                          % all characrers except the first one are displayed solid
                                          % (like character "█")

-spec encode_to_text(Codec :: atom(), String :: unicode:chardata()) -> FileContent :: binary().
encode_to_text(Codec, String) ->
    encode_to_text(Codec, String, 4).
encode_to_text(Codec, String, Height) ->
    encode_to_text(Codec, String, Height, true).
encode_to_text(Codec, String, Height, Inverse) ->
    BarCode = encode(Codec, String),
    QuietChars = if Inverse -> element(1, ?LINES); true -> element(4, ?LINES) end,
    Quiet = lists:duplicate(4, QuietChars),
    Code = [Quiet, bits_to_chars(BarCode, Inverse), Quiet],
    Blank = [lists:duplicate(string:length(Code) div 2, QuietChars), "\n"],
    unicode:characters_to_binary([Blank, lists:duplicate(Height, [Code, "\n"]), Blank]).


-spec bits_to_chars(Bits :: bitstring(), Inverse :: boolean()) -> unicode:chardata().
bits_to_chars(Bits, Inverse) -> bits_to_chars(Bits, Inverse, "").

bits_to_chars(<<>>, _, Acc) -> Acc;
bits_to_chars(<<Bit:1>>, Inverse, Acc) ->
    bits_to_chars(<<Bit:1, 0:1>>, Inverse, Acc);
bits_to_chars(<<Val:2, Bin/bits>>, Inverse, Acc) ->
    Index = if Inverse -> Val + 1; true -> 4 - Val end,
    bits_to_chars(Bin, Inverse, [Acc, element(Index, ?LINES)]).

-spec encode(Type :: atom(), String :: unicode:chardata()) -> BarCodeBitmap :: bitstring().
encode(Type, String) ->
    Module = proplists:get_value(Type, ?CODERS),
    apply(Module, encode, [String]).
