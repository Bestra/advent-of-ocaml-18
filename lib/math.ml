let add x y = x + y

let sub x y = x - y

let%test _ = add 3 4 = 7

let%expect_test _ =
  print_endline "Hello, world!";
  [%expect{|
    Hello, world!
  |}]