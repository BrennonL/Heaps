-module(heap).
-export([get_min/1, insert/2, merge/2, remove_min/1]).


get_min(nil) ->
    nil;
get_min({_rank, Value, Next_l, Next_r}) ->
    Value. 
 

insert(nil, nil) ->
    nil;
insert(Value, Heap) ->
    merge({1, Value, nil, nil}, Heap).


merge(nil, Heap_j) ->
    Heap_j;
merge(Heap_i, nil) ->
    Heap_i;
merge(Heap_i, Heap_j) ->
    {I_rank, I_value, IL_sub, IR_sub} = Heap_i,
    {J_rank, J_value, JL_sub, JR_sub} = Heap_j,
    if
        I_value =< J_value ->
            build_node({I_rank, I_value, IL_sub, (merge(IR_sub, Heap_j))});
        true ->
            build_node({J_rank, J_value, JL_sub, (merge(Heap_i, JR_sub))})
    end.


build_node({_Rank, Value, L_heap, R_heap}) ->
    L_rank = rank(L_heap),
    R_rank = rank(R_heap),
    if
      L_rank >= R_rank ->
        {rank(R_heap) + 1, Value, L_heap, R_heap};
      true ->
        {rank(L_heap) + 1, Value, R_heap, L_heap}
    end.

rank(nil) -> 0;
rank({R, _Value, _Left, _Right}) ->
    R.



remove_min(nil) ->
    to_do.
