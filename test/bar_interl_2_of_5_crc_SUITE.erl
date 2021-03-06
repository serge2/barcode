-module(bar_interl_2_of_5_crc_SUITE).
-compile(export_all).

%% Test server callbacks
-export([
    suite/0,
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).


-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-spec suite() -> [tuple()].
suite() ->
    [{timetrap, {minutes, 1}}].

-spec init_per_suite(Config::[tuple()]) -> NewConfig::[tuple()].
init_per_suite(Config) ->
    Config.

-spec end_per_suite(Config::[tuple()]) -> term().
end_per_suite(_Config) ->
    ok.

-spec init_per_testcase(Case::atom(), Config::[tuple()]) -> NewConfig::[tuple()].
init_per_testcase(_Case, Config) ->
    Config.

-spec end_per_testcase(Case::atom(), Config::[tuple()]) -> term().
end_per_testcase(_Case, _Config) ->
    ok.

-spec all() -> [{group, GroupName::atom()} | TestCase] when TestCase::atom().
all() ->
    [
     test_odd_length_value,
     test_even_length_value,
     test_incorrect_text
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

-spec test_odd_length_value(Config::[tuple()]) -> term().
test_odd_length_value(_Config) ->
    % For odd-length values only the checksum digit is appended to the end
    %% <<2#101010101110001110001011101:27>>
    ?assertMatch({_, _, _}, bar_interl_2_of_5_crc:encode("0")),
    %% <<2#101010001011101110100010001110001010111011101:45>>
    ?assertMatch({_, _, _}, bar_interl_2_of_5_crc:encode("012")),
    %% <<2#101010001011101110100010001110001010111010001011101000111011101:63>>
    ?assertMatch({_, _, _}, bar_interl_2_of_5_crc:encode("01234")),
    %% <<2#101010001011101110100010001110001010111010001011100010111010111011101000100011101000101110001010001011100011101011101:117>>
    ?assertMatch({_, _, _}, bar_interl_2_of_5_crc:encode("01234567890")).


-spec test_even_length_value(Config::[tuple()]) -> term().
test_even_length_value(_Config) ->
    % For odd-length values zero-digit is appended to the begin and the checksum digit is appended to the end
    %% <<2#101010101110001110001011101010100011100011101:45>>
    ?assertMatch({_, _, _}, bar_interl_2_of_5_crc:encode("01")),
    %% <<2#101010101110001110001011101000101011100011101110001000101011101:63>>
    ?assertMatch({_, _, _}, bar_interl_2_of_5_crc:encode("0123")),
    %% <<2#101010101110001110001011101000101011100011101110100010100011101011101000100011101:81>>
    ?assertMatch({_, _, _}, bar_interl_2_of_5_crc:encode("012345")),
    %% <<2#101010101110001110001011101000101011100011101110100010100011101000111000101010001010111000111010001110100011101011101:117>>
    ?assertMatch({_, _, _}, bar_interl_2_of_5_crc:encode("0123456789")).

-spec test_incorrect_text(Config::[tuple()]) -> term().
test_incorrect_text(_Config) ->
    % Zero-length text
    ?assertError(incorrect_text,
                 bar_interl_2_of_5_crc:encode("")),
    % Out of the charset
    ?assertError(incorrect_text,
                 bar_interl_2_of_5_crc:encode("A0123")).


