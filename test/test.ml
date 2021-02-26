module T = struct
  let parse_and_display s = Resp3.to_string @@ Resp3.parse @@ s
end

module C = struct
  let blob_normal () =
    Alcotest.(check string) "normal" "hello" (T.parse_and_display "$5\r\nhello\r\n")

  let blob_empty () =
    Alcotest.(check string) "empty" "" (T.parse_and_display "$0\r\n\r\n")

  let string_normal () =
    Alcotest.(check string) "normal" "hello" (T.parse_and_display "+hello\r\n")

  let string_empty () =
    Alcotest.(check string) "empty" "" (T.parse_and_display "+\r\n")

  let error_normal () =
    Alcotest.(check string) "normal" "ERR: err" (T.parse_and_display "-err\r\n")

  let error_empty () =
    Alcotest.(check string) "empty" "ERR: " (T.parse_and_display "-\r\n")

  let number () =
    Alcotest.(check string) "number" "1214" (T.parse_and_display ":1214\r\n")

  let null () =
    Alcotest.(check string) "null" "Null" (T.parse_and_display "_\r\n")

  let double_int_frac () =
    Alcotest.(check string) "int_frac" "12.14" (T.parse_and_display ",12.14\r\n")

  let double_no_int () =
    Alcotest.(check string) "no_int" "0.14" (T.parse_and_display ",.14\r\n");
    Alcotest.(check string) "no_int_zero" "0." (T.parse_and_display ",.0\r\n")

  let double_no_frac () =
    Alcotest.(check string) "no_frac" "12." (T.parse_and_display ",12\r\n")

  let double_inf () =
    Alcotest.(check string) "inf" "inf" (T.parse_and_display ",inf\r\n");
    Alcotest.(check string) "+inf" "inf" (T.parse_and_display ",+inf\r\n")

  let double_neg_inf () =
    Alcotest.(check string) "-inf" "-inf" (T.parse_and_display ",-inf\r\n")
 
  let boolean_true () =
    Alcotest.(check string) "true" "true" (T.parse_and_display "#t\r\n")

  let boolean_false () =
    Alcotest.(check string) "false" "false" (T.parse_and_display "#f\r\n")

  let blob_error_normal () =
    Alcotest.(check string) "normal" "ERR code: message" (T.parse_and_display "!12\r\ncode message\r\n")

  let blob_error_empty () =
    Alcotest.(check string) "empty" "" (T.parse_and_display "$0\r\n\r\n")

  let verbatim_string () =
    Alcotest.(check string) "verbatim_string" "txt: Some string" (T.parse_and_display "=15\r\ntxt:Some string\r\n")

  let big_number () =
    Alcotest.(check string) "big_number" "3492890328409238509324850943850943825024385" (T.parse_and_display "(3492890328409238509324850943850943825024385\r\n")

  let array_normal () =
    Alcotest.(check string) "array_normal" "[[1, hello, 2, [yes, false]]]" (T.parse_and_display "*2\r\n*4\r\n:1\r\n$5\r\nhello\r\n:2\r\n*1\r\n+yes\r\n#f\r\n")

  let array_empty () =
    Alcotest.(check string) "array_empty" "[]" (T.parse_and_display "*0\r\n")

  (* map is unordered and this may fail *)
  let map_normal () =
    Alcotest.(check string) "map_normal" "{b: [{c: 2}, false], a: 1}" (T.parse_and_display "%2\r\n+a\r\n:1\r\n+b\r\n*2\r\n%1\r\n+c\r\n:2\r\n#f\r\n")

  let map_empty () =
    Alcotest.(check string) "map_empty" "{}" (T.parse_and_display "%0\r\n")

  (* map is unordered and this may fail *)
  let set_normal () =
    Alcotest.(check string) "set_normal" "{{1, hello, 2, {yes, false}}}" (T.parse_and_display "~2\r\n*4\r\n:1\r\n$5\r\nhello\r\n:2\r\n~1\r\n+yes\r\n#f\r\n")

  let set_empty () =
    Alcotest.(check string) "set_empty" "{}" (T.parse_and_display "~0\r\n")

  (* map is unordered and this may fail *)
  let attribute () =
    Alcotest.(check string) "attribute" "<b: [<c: 2>, false], a: 1>" (T.parse_and_display "|2\r\n+a\r\n:1\r\n+b\r\n*2\r\n%1\r\n+c\r\n:2\r\n#f\r\n")

  let push () =
    Alcotest.(check string) "push" "pubsub: {message}" (T.parse_and_display ">1\r\n$5\r\nhello\r\n")

  let bad_eol () =
    Alcotest.(check string) "\\r" "\\r" (T.parse_and_display "$5\r\n12345\r\n")

end

let () =
  let open Alcotest in
  run "Value" [
    "blob", [
      test_case "normal" `Quick C.blob_normal;
      test_case "empty" `Quick C.blob_empty;
    ];
    "string", [
      test_case "normal" `Quick C.string_normal;
      test_case "empty" `Quick C.string_empty;
    ];
    "error", [
      test_case "normal" `Quick C.error_normal;
      test_case "empty" `Quick C.error_empty;
    ];
    "number", [
      test_case "number" `Quick C.number;
    ];
    "null", [
      test_case "null" `Quick C.null;
    ];
    "double", [
      test_case "int_frac" `Quick C.double_int_frac;
      test_case "no_int" `Quick C.double_no_int;
      test_case "no_frac" `Quick C.double_no_frac;
      test_case "inf" `Quick C.double_inf;
      test_case "neg_inf" `Quick C.double_neg_inf;
    ];
    "boolean", [
      test_case "true" `Quick C.boolean_true;
      test_case "false" `Quick C.boolean_false;
    ];
    "blob_error", [
      test_case "normal" `Quick C.blob_error_normal;
      test_case "empty" `Quick C.blob_error_empty;
    ];
    "verbatim_string", [
      test_case "verbatim_string" `Quick C.verbatim_string;
    ];
    "big_number", [
      test_case "big_number" `Quick C.big_number;
    ];
    "array", [
      test_case "normal" `Quick C.array_normal;
      test_case "empty" `Quick C.array_empty;
    ];
    "map", [
(*       test_case "normal" `Quick C.map_normal; *)
      test_case "empty" `Quick C.map_empty;
    ];
    "set", [
(*       test_case "normal" `Quick C.array_normal; *)
      test_case "empty" `Quick C.array_empty;
    ];
    "attribute", [
(*       test_case "attribute" `Quick C.attribute; *)
    ];
    "push", [
      test_case "push" `Quick C.push;
    ];
(*
    "eol", [
      test_case "bad_eol" `Quick C.bad_eol;
    ];
*)
  ]
