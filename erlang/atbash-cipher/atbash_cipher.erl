-module(atbash_cipher).
-export([encode/1, decode/1]).

-define(ALPHABET, "abcdefghijklmnopqrstuvwxyz").
-define(DIGITS, "0123456789").
-define(ATBASH,
        lists:append(
          lists:zip(?ALPHABET, lists:reverse(?ALPHABET)),
          lists:zip(?DIGITS, ?DIGITS))).

-spec encode(string()) -> string().
encode(Str) ->
  Atbash = [atbash_char(Char) || Char <- normalize(Str)],
  space_out(Atbash).

-spec decode(string()) -> string().
decode(Str) ->
  [plain_char(Char) || Char <- normalize(Str)].

%% Convert `Str' to all lower case characters and remove
%% spaces and punctuation.
-spec normalize(string()) -> string().
normalize(Str) ->
  lists:foldl(
    fun(Char, Acc) ->
        case lists:keymember(Char, 1, ?ATBASH) of
          true -> Acc ++ [Char];
          false -> Acc
        end
    end,
    [],
    string:to_lower(Str)).

-spec atbash_char(char()) -> char().
atbash_char(Char) ->
  {_Char, Result} = lists:keyfind(Char, 1, ?ATBASH),
  Result.

-spec plain_char(char()) -> char().
plain_char(Char) ->
  {Result, _Char} = lists:keyfind(Char, 2, ?ATBASH),
  Result.

%% Return a string that is the original `Str' with spaces
%% inserted after every fifth character.
-spec space_out(string()) -> string().
space_out(Str) ->
  string:join(sublists(Str, 5), " ").

%% Break up `List' into sublists of length `Len'.
-spec sublists(list(), pos_integer()) -> [list()].
sublists(List, Len) ->
  lists:map(
    fun(I) -> lists:sublist(List, I, Len) end,
    lists:seq(1, length(List), Len)).

