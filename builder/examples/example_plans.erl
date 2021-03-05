-module(example_plans).
-export([simple/0, involved/0]).

simple() ->
  {seq, [ {act, write_to_file, ["hello.txt", "Can we fix it?\n"]}
        , {act, write_to_file, ["hello.txt", "Yes, we can!\n"]}
        ]}
    .

involved() ->
  {all, [ {act, print, "Time to get busy, such a lot to do"}
        , {within, 600,
           {and_then,
            {act, read_file, "hello.txt"},
            fun (Content) ->
                {act, write_to_file,
                 ["olleh.txt",
                  lists:reverse(Content)]}
            end}
          }
        ]}
    .
