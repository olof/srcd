-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(check_full_inflate(Name, Input, OutputIn, Tail),
Name() ->
  Output = OutputIn,
  case catch in(Input) of
    {ok, Result, Ctx} ->
      Read = size(Input) - case Tail of
        undefined -> 0;
        _ -> size(Tail)
      end,
      Written = size(Output),

      [
        ?_assertEqual({zlib, in, Tail, finalized, undefined, Read, Written}, Ctx),
        ?_assertEqual(Output, Result),
        ?_assertEqual(Tail, tail(Ctx)),
        ?_assertEqual({ok, [{read, Read}, {written, Written}]}, stats(Ctx))
      ];
    Return_Value_From_Inflate -> [
      ?_assertMatch({ok, Output, _}, Return_Value_From_Inflate)
    ]
  end
).

-define(check_full_inflate(Name, Input, Output),
       ?check_full_inflate(Name, Input, Output, <<>>)).
-endif.
