-module(maxheap).
-export([get_max/1, insert/2, merge/2, remove_max/1]).


get_max(Node) when is_atom(Node) ->
    nil;
get_max({_rank, Value, Next_l, Next_r}) ->
    Value.
 
 
insert(_, Heap) when is_atom(Heap), Heap =/= nil ->
    nil;
insert(nil, Heap) ->
    Heap;
insert(Value, Heap) ->
    merge({1, Value, nil, nil}, Heap).

rank(nil) -> 0;
rank(Node) when is_atom(Node) ->
    no_node;
rank({R, _Value, _Left, _Right}) ->
    R.

remove_max(nil) ->
    nil;
remove_max(Heap) when is_atom(Heap) ->
    no_heap;
remove_max({_Rank, _Value, Left, Right}) ->
    merge(Left, Right).


% CHANGES START HERE
merge(Heap_i, Heap_j) when is_atom(Heap_i), is_atom(Heap_j)->
    nil;
merge(Heap_i, Heap_j) when is_atom(Heap_i) ->
    Heap_j;
merge(Heap_i, Heap_j) when is_atom(Heap_j) ->
    Heap_i;
merge(Heap_i, Heap_j) ->
    {I_rank, I_value, IL_sub, IR_sub} = Heap_i,
    {J_rank, J_value, JL_sub, JR_sub} = Heap_j,
    if
        I_value >= J_value ->
            build_node({I_rank, I_value, (merge(Heap_j, IL_sub)), IR_sub});
        true ->
            build_node({J_rank, J_value, merge(JL_sub, Heap_i), JR_sub})
    end.


build_node({_Rank, Value, L_heap, R_heap}) ->
    L_rank = rank(L_heap),
    R_rank = rank(R_heap),
    if
      L_rank > R_rank ->
        {rank(R_heap) + 1, Value, R_heap, L_heap};
      true ->
        {rank(L_heap) + 1, Value, L_heap, R_heap}
    end.


%%% Only include the eunit testing library and functions
%%% in the compiled code if testing is 
%%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

insert_test_()->
	[?_assertEqual({1, 7, nil, {1, 4, nil, nil}}, insert(7, {1, 4, nil, nil})),%happy path
	 ?_assertEqual({1, 10, nil, {1, 1, nil, nil}}, insert(1, {1, 10, nil, nil})),%happy path
	 ?_assertEqual({1, 13, nil, {1, 10, nil, {1, 1, nil, nil}}}, insert(13, {1, 10, nil, {1, 1, nil, nil}})),%happy path
	 ?_assertEqual({1, 20, nil, {2, 13, {1, 10, nil, nil}, {1, 9, nil, nil}}}, insert(20, {2, 13, {1, 10, nil, nil}, {1, 9, nil, nil}})),%happy path
	 ?_assertEqual({2, 18, {1, 13, nil, nil}, {1, 15, nil, {1, 1, nil, nil}}}, insert(1, {2, 18, {1, 13, nil, nil}, {1, 15, nil, nil}})),%happy path
	 ?_assertEqual({1, 7, nil, nil}, insert(7, nil)),%happy path
	 ?_assertEqual(nil, insert(nil, nil)),%happy path
	 %nasty thoughts start here
	 ?_assertEqual(nil, insert(8, fake_tree)),
	 ?_assertEqual({1, 2, nil, nil}, insert(nil, {1, 2, nil, nil}))
	].

% merge_test_()->
% 	[?_assertEqual({1, 2, {1, 4, nil, nil}, nil}, merge({1, 4, nil, nil}, {1, 2, nil, nil})),%happy path
% 	 ?_assertEqual({1, 5, {1, 6, {1, 4, nil, nil}, nil}, nil}, merge({1, 5, nil, nil}, {1, 6, {1, 4, nil, nil}, nil})),%happy path
% 	 ?_assertEqual({2, 4, {1, 5, nil, nil}, {1, 6, nil, nil}}, merge({1, 4, {1, 5, nil, nil}, nil}, {1, 6, nil, nil})),%happy path
% 	 ?_assertEqual({1, 9, nil, nil}, merge({1, 9, nil, nil}, nil)),%happy path
% 	 ?_assertEqual({1, 35, nil, nil}, merge(nil, {1, 35, nil, nil})),%happy path
%      %nasty thoughts start here
% 	 ?_assertEqual({1, 67, nil, nil}, merge(hello_world, {1, 67, nil, nil})),
% 	 ?_assertEqual(nil, merge(yes, no))
% 	].

% remove_max_test_()->
% 	[?_assertEqual(nil, remove_min(nil)),%happy path
% 	 ?_assertEqual({1, 5, {1, 8, nil, nil}, nil}, remove_min({2, 3, {1, 5, nil, nil}, {1, 8, nil, nil}})),%happy path
% 	 ?_assertEqual({2, 5, {1, 7, nil, nil}, {1, 8, nil, nil}}, remove_min({2, 3, {1, 5, {1, 7, nil, nil}, nil}, {1, 8, nil, nil}})),%happy path
% 	 %nasty thoughts start here
% 	 ?_assertEqual(no_heap, remove_min(fake_heap))
% 	].

-endif.