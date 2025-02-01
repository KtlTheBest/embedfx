open Dsl

let eff_example =
  toplevel [
    eff "E" [_char; _bool];

    _func void "app_main" [] [
    ];
  ]

let two_tasks_example =
  let printfn s = e @@ _func_call "printf" [_s (s ^ "\n")] in
  toplevel [
    _func_type "printf" [str] void;
    task "A" [
      printfn "Hello from task A";
    ];
    task "B" [
      printfn "Hello from task B";
    ];
    _func void "app_main" [] [
    ];
  ]

let two_effects_and_a_promise_example =
  toplevel [
    eff "E" [];
    promise "p" ("E", []) [
    ];

    task "sender_A" [
      raise' "E" [];
    ];
    task "sender_B" [
      raise' "E" [];
    ];
    task "receiver" [
      e @@ id "p";
    ];

    parallel [
      "sender_A";
      "sender_B";
      "receiver";
    ];

    _func void "app_main" [] [
    ];
  ]

let example1 =
  let puts s = _func_call "puts" [_s s] in
  let puts' s = e @@ puts s in
  let print_int e = _func_call "printf" [_s "%d"; e] in
  toplevel [
    eff "E" [];
    sumtype "a_or_b" [
      ("A", []);
      ("B", [])
    ];

    _func_type "puts" [str] void;
    _func_type "printf" [str; _int] void;

    promise "just_print" ("E", []) [
      puts' "Printing from promise 'just_print'"
    ];

    task "send_signal" [
      e @@ raise "E" []
    ];

    task "await_signal" [
      e @@ id "just_print" (* TODO: BUG: need to unify unit with promise. However promises have rettypes... *)
    ];

    parallel [
      "send_signal";
      "await_signal"
    ];

    _func void "app_main" [] [
      _let "one" (_i 1);
      _let "two" (_i 2);
      _let_mut "three" ((id "one") |+ (id "two"));
      e @@ print_int (id "three");
      (id "three") |= (_i 4);      
      e @@ print_int (id "three"); (* TODO: improve error messages *)
      e @@ puts "Hello, World!"
    ]
  ]

let example2 =
  let printf s = _func_call "printf" [_s s] in
  let printf' s = e @@ printf s in
  toplevel [
    _func_type "printf" [str] void;

    modetype "a_or_b" ["A"; "B"];

    task "simple_A" [
      printf' "Switching to mode B\n";
      switch "B"
    ];

    task "simple_B" [
      printf' "Switching to mode A\n";
      switch "A"
    ];

    mode_tasks "A" [ "simple_A" ];
    mode_tasks "B" [ "simple_B" ];

    start_modes [ "A" ];

    _func void "app_main" [] [
      printf' "Hello World!\n"
    ] (* BUG: when modes are involved, the check for start modes is not triggered *)
  ]

let example_traffic_light =
  let switch_on_green   = _func_call' "switch_on_green"   [] in
  let switch_off_green  = _func_call' "switch_off_green"  [] in
  let switch_on_yellow  = _func_call' "switch_on_yellow"  [] in
  let switch_off_yellow = _func_call' "switch_off_yellow" [] in
  let switch_on_red     = _func_call' "switch_on_red"     [] in
  let switch_off_red    = _func_call' "switch_off_red"    [] in
  let vTaskDelayInSec f = _func_call' "vTaskDelayInSec" [_f f] in
  toplevel [
    eff "PushButton" [];

    modetype "traffic_mode" [
      "Primary";
      "Secondary"
    ];

    _func_type "vTaskDelayInSec" [_float] void;
    _func_type "switch_on_green" [] void;
    _func_type "switch_off_green" [] void;
    _func_type "switch_on_yellow" [] void;
    _func_type "switch_off_yellow" [] void;
    _func_type "switch_on_red" [] void;
    _func_type "switch_off_red" [] void;
    _func_type "switch_off_all" [] void;
    
    interrupt "button_press" "PushButton";

    promise "switch_to_secondary" ("PushButton", []) [
      for_to' "i" (_i 0) (_i 5) [
        switch_off_green;
        vTaskDelayInSec 0.5;
        switch_on_green;
        vTaskDelayInSec 0.5;
      ];

      switch_on_yellow;
      vTaskDelayInSec 3.0;

      switch_off_yellow;
      switch_on_red;
    ];
    
    task "always_green" [
      await' "switch_to_secondary";
      switch "Secondary";
    ];

    task "secondary_back_to_primary" [
      vTaskDelayInSec 30.0;
      switch_off_red;
      switch_on_yellow;
      vTaskDelayInSec 4.0;
      switch_off_yellow;
      switch_on_green;
      vTaskDelayInSec 30.0;
      switch "Primary";
    ];

    parallel [
      "button_press";
      "always_green";
    ];

    mode_tasks "Primary" [ "always_green" ];
    mode_tasks "Secondary" [ "secondary_back_to_primary" ];

    start_modes [ "Primary" ];

    _func void "app_main" [] [
      switch_on_green;
    ];
  ]

let await_example =
  toplevel [
    eff "E" [];
    promise "p" ("E", []) [
      return (_i 2);
    ];
    task "A" [
      e @@ raise "E" [];
    ];
    task "B" [
      _let "x" (await "p");
      e @@ unit;
    ];

    parallel [
      "A"; "B"
    ];

    _func void "app_main" [] [
    ];
  ]

let await_2_example =
  let printfn s = e @@ _func_call "printf" [_s (s ^ "\n")] in
  let delay i = e @@ _func_call "vTaskDelay" [_i i] in
  toplevel [
    _func_type "printf" [str] void;
    _func_type "vTaskDelay" [_int] void;

    eff "E" [];
    eff "F" [];

    promise "p_E" ("E", []) [
      printfn "Triggered on E";
      return (_b true);
    ];

    promise "p_F" ("F", []) [
      printfn "Triggered on F";
      return (_b true);
    ];

    task "sender" [
      printfn "Sending E";
      raise' "E" [];
      delay 50;
    ];

    task "await_E" [
      e @@ await "p_E";
      printfn "p_E resolved, sending F";
      raise' "F" [];
    ];

    task "await_F" [
      e @@ await "p_F";
      printfn "p_F resolved";
    ];

    parallel [
      "sender";
      "await_E";
      "await_F";
    ];

    _func void "app_main" [] [
    ];
  ]

let interrupt_example =
  toplevel [
    eff "E" [];

    interrupt "I" "E";
    promise "p" ("E", []) [
      return (_i 2);
    ];

    task "A" [
      _let "x" (await "p");
      e @@ unit;
    ];

    parallel ["A"; "I"];
    
    _func void "app_main" [] [
    ];
  ]

let parallels_example =
  let body =
    [
      e @@ raise "E" [];
      e @@ await "p";
      e @@ unit;
    ]
  in
  toplevel [
    eff "E" [];

    promise "p" ("E", []) [
      return (_i 2);
    ];

    task "A" body;
    task "B" body;
    task "C" body;

    parallel ["A"; "B"];
    parallel ["A"; "C"];

    _func void "app_main" [] [
    ];
  ]

let simplest_promise_example =
  let printf s = _func_call "printf" [_s s] in
  let printf' s = e @@ printf s in
  let _task x = 
    task x [
      e @@ raise "E" [];
      e @@ id "p";
      printf' @@ Printf.sprintf "From task %s" x;
    ]
  in
  toplevel [
    _func_type "printf" [str] void;
    eff "E" [];

    promise "p" ("E", []) [
      printf' "Printing from promise p";
    ];

    task "A" [
      e @@ raise "E" []
    ];

    task "B" [
      e @@ id "p";
    ];

    parallel [ "A"; "B" ];

    _func void "app_main" [] [
    ];
  ]

let simple_promise_example =
  let printf s = _func_call "printf" [_s s] in
  let printf' s = e @@ printf s in
  let _task x = 
    task x [
      e @@ raise "E" [];
      e @@ id "p";
      printf' @@ Printf.sprintf "From task %s" x;
    ]
  in
  toplevel [
    _func_type "printf" [str] void;
    eff "E" [];

    promise "p" ("E", []) [
      printf' "Printing from promise p1";
    ];

    _task "A";
    _task "B";

    parallel [ "A"; "B" ];

    _func void "app_main" [] [
    ];
  ]

let promise_in_promise_example =
  let printf s = _func_call "printf" [_s s] in
  let printf' s = e @@ printf s in
  let _task x = 
    task x [
      e @@ raise "E" [];
      e @@ id "p1";
      printf' @@ Printf.sprintf "From task %s" x;
    ]
  in
  toplevel [
    _func_type "printf" [str] void;
    eff "E" [];

    promise "p2" ("E", []) [
      printf' "Printing from promise p2";
    ];

    promise "p1" ("E", []) [
      e @@ id "p2";
      printf' "Printing from promise p1";
    ];

    _task "A";
    _task "B";

    parallel [ "A"; "B" ];

    _func void "app_main" [] [
    ];
  ]

let promise_in_promise_2_example =
  let printf s = _func_call "printf" [_s s] in
  let printf' s = e @@ printf s in
  let _task x = 
    task x [
      e @@ raise "E" [];
      e @@ id "p1";
      printf' @@ Printf.sprintf "From task %s" x;
    ]
  in
  toplevel [
    _func_type "printf" [str] void;
    eff "E" [];

    promise "p2" ("E", []) [
      printf' "Printing from promise p2";
    ];

    promise "p1" ("E", []) [
      e @@ id "p2";
      printf' "Printing from promise p1";
      e @@ id "p2";
    ];

    _task "A";
    _task "B";

    parallel [ "A"; "B" ];

    _func void "app_main" [] [
    ];
  ]

let pseudo_coroutines_example =
  let tree_A = _func_call "pass_tree_A" [] in
  let tree_B = _func_call "pass_tree_B" [] in
  let sleep x = e @@ _func_call "vTaskDelay" [CastTo(CustomType("TickType_t"), _i x)] in
  let gen_task c f =
    task (Printf.sprintf "traverse_tree_%c" c) [
      global "should_search";
      _if' ((id "should_search") === (_b true)) [
        e @@ raise (Printf.sprintf "tree_%c" c) ([f]);
      ] [
        _while' (_b true) [
        ]
      ];
      sleep 10;
    ];
  in
  let puts s = e @@ _func_call "puts" [_s s] in
  let printfcc s a b = e @@ _func_call "printf" [_s (s ^ "\n"); (id a); (id b)] in
  let halt = _while' (_b true) [] in
  let _lxor a b =
    ((a === (_b true)) |><| (b === (_b false))) |<>| ((a === (_b false)) |><| (b === (_b true)))
  in
  toplevel [
    eff "tree_A" [_char];
    eff "tree_B" [_char];
    eff "read_A" [_bool];
    eff "read_B" [_bool];
    eff "should_search_eff" [_bool];

    interrupt "read_A_i" "read_A";
    interrupt "read_B_i" "read_B";

    _func_type "pass_tree_A" [] _char;
    _func_type "pass_tree_B" [] _char;
    _func_type "vTaskDelay" [CustomType("TickType_t")] void;
    _func_type "puts" [str] void;
    _func_type "printf" [str; _char; _char] void;

    global_var _bool "should_search";
    global_var _bool "should_search_A";
    global_var _bool "should_search_B";
    global_var _bool "trees_match";

    promise "sig_A" ("tree_A", ["x"]) [
      return (id "x");
    ];

    promise "sig_B" ("tree_B", ["x"]) [
      return (id "x");
    ];

    promise "p_should_read_A" ("read_A", ["v"]) [
      return (id "v");
    ];

    promise "p_should_read_B" ("read_B", ["v"]) [
      return (id "v");
    ];

    promise "p_should_search" ("should_search_eff", ["v"]) [
      return (id "v");
    ];

    task "wait_for_read_signal" [
      global "should_search";
      global "trees_match";
      _if' ((id "should_search") === (_b true)) [
        _let "a" (await "p_should_read_A");
        _let "b" (await "p_should_read_B");
        _if' (_lxor (id "a") (id "b")) [
          (id "trees_match") |= (_b false);
        ][
        ];
        _if' (((id "a") |><| (id "b")) === (_b false)) [
          e @@ raise "should_search_eff" [(_b false)];
          halt
        ][
          e @@ raise "should_search_eff" [(_b true)];
        ]
      ] [
        halt
      ]
    ];

    gen_task 'A' tree_A;
    gen_task 'B' tree_B;

    task "main_task" [
      global "should_search";
      global "trees_match";
      _if' ((id "should_search") === (_b false)) [
        puts "Trees are equal!";
        halt
      ] [
        _let "a" (await "sig_A");
        _let "b" (await "sig_B");
        _if' ((id "a") === (id "b")) [
          _if' ((id "trees_match") === (_b false)) [
            puts "Trees are not equal! One is shorter than the other!";
            halt
          ][
          ];
          _let "v" (await "p_should_search");
          (id "should_search") |= (id "v");
        ] [
          printfcc "Trees are not the same! %c != %c" ("a") ("b");
          (id "should_search") |= (_b false);
          halt
        ]
      ]
    ];

    parallel [ "main_task" 
             ; "traverse_tree_A"
             ; "traverse_tree_B"
             ];
    parallel [ "read_A_i"
             ; "read_B_i"
             ; "wait_for_read_signal"
             ];
    parallel [ "main_task"; "wait_for_read_signal" ];

    _func void "app_main" [] [
      global "should_search";
      global "should_search_A";
      global "should_search_B";
      global "trees_match";
      (id "should_search")   |= (_b true);
      (id "should_search_A") |= (_b true);
      (id "should_search_B") |= (_b true);
      (id "trees_match")     |= (_b true);
    ]
  ]

let simple_aircon_example =
  let inc_temp = e @@ _func_call "inc_temp" [] in
  let dec_temp = e @@ _func_call "dec_temp" [] in
  let printf s = e @@ _func_call "printf" [_s (s ^ "\n")] in
  toplevel [
    eff "cur_temp" [_int];
    eff "set_temp" [_int];
    eff "turn_on_or_off" [];

    _func_type "inc_temp" [] void;
    _func_type "dec_temp" [] void;
    _func_type "printf" [str] void;

    global_var _int "target_temp";
    global_var _bool "switched_on";
    global_var _int "current_temp";

    interrupt "outside_temp_sensor" "cur_temp";
    interrupt "outside_temp_control" "set_temp";
    interrupt "outside_active_switch" "turn_on_or_off";

    promise "p_outside_temp" ("cur_temp", ["v"]) [
      return (id "v");
    ];

    task "fresh_temp_data" [
      global "current_temp";
      _let "x" (await "p_outside_temp");
      (id "current_temp") |= (id "x");
    ];

    promise "p_target_temp" ("set_temp", ["v"]) [
      return (id "v");
    ];

    task "set_target_temp" [
      global "target_temp";
      _let "x" (await "p_target_temp");
      (id "target_temp") |= (id "x");
      printf "Changed the temperature";
    ];

    promise "p_switch" ("turn_on_or_off", []) [
      return (_b true);
    ];

    task "cur_mode_switch" [
      global "switched_on";
      await' "p_switch";
      _if' ((id "switched_on") === (_b true)) [
        (id "switched_on") |= (_b false);
        printf "Switched off";
      ][
        (id "switched_on") |= (_b true);
        printf "Switched on";
      ]
    ];

    task "main_task" [
      global "switched_on";
      global "target_temp";
      global "current_temp";
      _if' ((id "switched_on") === (_b true)) [
        _if' ((id "current_temp") |< (id "target_temp")) [
          inc_temp;
        ][
          _if' ((id "current_temp") |> (id "target_temp")) [
            dec_temp;
          ][
            (* do nothing *)
          ]
        ]
      ][
        (* do nothing *)
      ]
    ];

    parallel [ "outside_temp_sensor"
             ; "fresh_temp_data"
             ];

    parallel [ "cur_mode_switch"
             ; "outside_active_switch"
             ];

    parallel [ "outside_temp_control" 
             ; "set_target_temp"
             ];

    _func void "app_main" [] [
      global "current_temp";
      (id "current_temp") |= (_i 25);
    ]
  ]

let random_sleep_example =
  let delay x =
    _func_call' "vTaskDelay" [x]
  in
  let printf s' =
    _func_call' "printf" [_s (s' ^ "\n")]
  in
  let rand_sleep =
    let rand = _func_call "rand" [] in
    [ _let "sleep_time" ((rand |% (_i 30)) |+ (_i 10));
      delay ((id "sleep_time") |* (_i 20));
    ]
  in
  toplevel [
    eff "E" [];

    _func_type "rand" [] _int;
    _func_type "vTaskDelay" [_int] void;
    _func_type "printf" [str] void;

    promise "receive" ("E", []) [
      printf "MSG receiver: received";
    ];

    task "sender" ([
      printf "MSG sender: raising effect...";
      raise' "E" [];
      printf "MSG sender: sleeping for random time...";] @ 
      rand_sleep);

    task "receiver" [
      e @@ id "receive";
    ];

    parallel [
      "sender";
      "receiver";
    ];

    _func void "app_main" [] [
    ]
  ]

let receive_from_many_example =
  let delay x =
    _func_call' "vTaskDelay" [x]
  in
  let printf_i x =
    _func_call' "printf_i" [x]
  in
  let printf s' =
    _func_call' "printf" [_s s']
  in
  let printfn s' =
    _func_call' "printf" [_s (s' ^ "\n")]
  in
  let rand = _func_call "rand" [] in
  let rand_sleep =
    [ _let "sleep_time" ((rand |% (_i 30)) |+ (_i 10));
      delay ((id "sleep_time") |* (_i 20));
    ]
  in
  toplevel [
    _func_type "rand" [] _int;
    _func_type "vTaskDelay" [_int] void;
    _func_type "printf" [str] void;
    _func_type "printf_i" [_int] void;

    eff "req" [];

    eff "resp_1" [_int];
    eff "resp_2" [_int];
    eff "resp_3" [_int];

    promise "request" ("req", []) [
      return (_b true);
    ];

    promise "response_1" ("resp_1", ["x"]) [
      return (id "x")
    ];

    promise "response_2" ("resp_2", ["x"]) [
      return (id "x")
    ];

    promise "response_3" ("resp_3", ["x"]) [
      return (id "x")
    ];

    task "server_1" ([
      await' "request";
      printfn "Server 1: Received a request, processing...";
      ] @
      rand_sleep
      @ [
      _let "digit" ((rand |% (_i 9) |+ (_i 1)) |* (_i 100));
      printfn "Server 1: Produced";
      raise' "resp_1" [id "digit"];
    ]);

    task "server_2" ([
      await' "request";
      printfn "Server 2: Received a request, processing...";
      ] @
      rand_sleep
      @ [
      _let "digit" ((rand |% (_i 9) |+ (_i 1)) |* (_i 10));
      printfn "Server 2: Produced";
      raise' "resp_2" [id "digit"];
    ]);

    task "server_3" ([
      await' "request";
      printfn "Server 3: Received a request, processing...";
      ] @
      rand_sleep
      @ [
      _let "digit" (rand |% (_i 9) |+ (_i 1));
      printfn "Server 3: Produced";
      raise' "resp_3" [id "digit"];
    ]);

    task "client" ([
      printf "Client: Requesting data...";
      raise' "req" [];
      _let "a" (await "response_1");
      _let "b" (await "response_2");
      _let "c" (await "response_3");
      printf "Client: Received data: ";
      printf_i ((id "a") |+ (id "b") |+ (id "c"));
      printfn "";
      printfn "Client: Sleeping...";
    ] @ rand_sleep);

    parallel [
      "server_1";
      "server_2";
      "server_3";
      "client";
    ];

    _func_type "rand" [] _int;
    _func_type "vTaskDelay" [_int] void;
    _func_type "printf" [str] void;

    _func void "app_main" [] [
    ];
  ]

let voting_example =
  let printfn s' = e @@ _func_call "printf" [_s (s' ^ "\n")] in
  let rand = (_func_call "rand" []) in
  let rand_vote = ((rand |% (_i 3)) |+ (_i 1)) in
  let delay_n n = e @@ _func_call "vTaskDelay" [_i n] in
  let rand_delay =
    [ _let "delay" (((rand |% (_i 5)) |+ (_i 1)) |* (_i 20));
      e @@ _func_call "vTaskDelay" [id "delay"];
    ]
  in
  let voter_task i =
    task (Printf.sprintf "voter_%d" i) ([
      e @@ await "p_start";
      _let "vote" rand_vote;
      ] @ rand_delay @ [
      printfn (Printf.sprintf "Voter %d voted!\n" i);
      raise' (Printf.sprintf "vote_%d" i) [(id "vote")];
    ])
  in
  let voter_promise i =
    promise (Printf.sprintf "p_voter_%d" i) (Printf.sprintf "vote_%d" i, ["x"]) [
      return (id "x");
    ]
  in
  toplevel [
    _func_type "printf" [str] void;
    _func_type "rand" [] _int;
    _func_type "vTaskDelay" [_int] void;

    global_var _int "choice_1";
    global_var _int "choice_2";
    global_var _int "choice_3";

    eff "vote_1" [_int];
    eff "vote_2" [_int];
    eff "vote_3" [_int];
    eff "vote_4" [_int];
    eff "vote_5" [_int];
    eff "start" [];

    promise "p_start" ("start", []) [
      return (_b true);
    ];

    voter_task 1;
    voter_task 2;
    voter_task 3;
    voter_task 4;
    voter_task 5;

    voter_promise 1;
    voter_promise 2;
    voter_promise 3;
    voter_promise 4;
    voter_promise 5;

    task "judge" ([
      global "choice_1";
      global "choice_2";
      global "choice_3";

      (id "choice_1") |= (_i 0);
      (id "choice_2") |= (_i 0);
      (id "choice_3") |= (_i 0);

      printfn "Voting start!";
      raise' "start" [];

      _let "voted_1" (await "p_voter_1");
      _let "voted_2" (await "p_voter_2");
      _let "voted_3" (await "p_voter_3");
      _let "voted_4" (await "p_voter_4");
      _let "voted_5" (await "p_voter_5");
    ] @ (
      let resolve_vote i =
        let cond j =
          _if' ((id @@ Printf.sprintf "voted_%d" i) === (_i j)) [
            (id @@ Printf.sprintf "choice_%d" j) |= ((id @@ Printf.sprintf "choice_%d" j) |+ (_i 1));
          ] [
          ]
        in
        List.map cond [ 1; 2; 3 ]
      in
      List.flatten [ 
        resolve_vote 1;
        resolve_vote 2;
        resolve_vote 3;
        resolve_vote 4;
        resolve_vote 5;
      ]
    ) @ [
      _if' (((id "choice_1") === (id "choice_2")) |><|
            ((id "choice_1") |>  (id "choice_3")) |><|
            ((id "choice_2") |>  (id "choice_3"))
      )[
        printfn "Options 1 and 2 tied!";
      ][];
      _if' (((id "choice_1") === (id "choice_3")) |><|
            ((id "choice_1") |>  (id "choice_2")) |><|
            ((id "choice_3") |>  (id "choice_2"))
      )[
        printfn "Options 1 and 3 tied!";
      ][];
      _if' (((id "choice_2") === (id "choice_3")) |><|
            ((id "choice_2") |>  (id "choice_1")) |><|
            ((id "choice_3") |>  (id "choice_1"))
      )[
        printfn "Options 2 and 3 tied!";
      ][];
      _if' (((id "choice_1") |> (id "choice_2")) |><|
            ((id "choice_1") |> (id "choice_3")))
      [
        printfn "Choice 1 gets the majority!";
      ][];
      _if' (((id "choice_2") |> (id "choice_1")) |><| 
            ((id "choice_2") |> (id "choice_3")))
      [
        printfn "Choice 2 gets the majority!";
      ][];
      _if' (((id "choice_3") |> (id "choice_1")) |><|
            ((id "choice_3") |> (id "choice_2")))
      [
        printfn "Choice 3 gets the majority!";
      ][];
      printfn "Voting ended!";
      delay_n 50;
    ]);

    parallel [
      "judge";
      "voter_1";
      "voter_2";
      "voter_3";
      "voter_4";
      "voter_5";
    ];

    _func void "app_main" [] [
    ];
  ]

let compile (code : Ast.toplevel list) : string =
  Gen.gen_c @@
    Convert.convert_to_c @@ 
      Typecheck.typecheck code

(* let _ =
  print_endline @@ compile eff_example *)

let () =
  let calculate_loc ((v : int), (ex : Ast.toplevel list)) : string =
    let code = compile ex in
    let lines = String.split_on_char '\n' code in
    let nonempty_lines = List.filter (fun x -> String.trim x <> "") lines in
    let n = List.length nonempty_lines in
    let n' = float_of_int n in
    let v' = float_of_int v in
    let output = Printf.sprintf "[%d, %d, %f]," (n) (v) (n' /. v') in
    output
  in
  let examples =
    [
      (5, eff_example);
      (12, two_tasks_example);
      (23, two_effects_and_a_promise_example);
      (18, await_example);
      (37, await_2_example);
      (14, interrupt_example);
      (20, parallels_example);
      (21, simple_promise_example);
      (25, promise_in_promise_example);
      (26, promise_in_promise_2_example);
      (119, pseudo_coroutines_example);
      (92 - 14, simple_aircon_example);
      (34, random_sleep_example);
      (810 - 705 - 13, receive_from_many_example);
      (128, voting_example)
    ]
  in
  List.iter print_endline @@
    List.map calculate_loc examples
