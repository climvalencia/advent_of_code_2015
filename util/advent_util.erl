-module(advent_util).

-export([
    chomp/1
]).


-spec chomp(term()) -> term().
chomp(Term) when is_bitstring(Term) -> do_chomp(Term, binary);
chomp(Term) when is_list(Term) -> do_chomp(Term, list).

% ==================
% Internal Functions
% ==================
-spec do_chomp(term(), binary | list) -> term().
do_chomp(Term, ReturnType) ->
    re:replace(Term, <<"\n$">>, <<>>, [{return, ReturnType}]).
