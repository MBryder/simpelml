open Alcotest

let add x y = x + y

let test_addition () =
  let expected = 5 in
  let actual = add 2 3 in
  check int "addition test" expected actual

let suite = [
  test_case "2 + 3 = 5" `Quick test_addition;
]