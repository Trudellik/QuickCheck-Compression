open QCheck;;

external huffman_Compress : bytes -> bytes -> int -> int = "encode_stub"
external huffman_Uncompress : bytes -> bytes -> int -> int -> unit= "decode_stub"

(*  comp : string -> bytes*)
let comp s =
  let input = Bytes.of_string s in
  let length = String.length s in
  let output = Bytes.create (length + 320) in
  let resLength = (huffman_Compress input output length) in
  Bytes.sub output 0 resLength;;

(*bytes -> int -> string*)
let decomp b l =
  let output = Bytes.create l in
  let inputLength = Bytes.length b in
  let () = huffman_Uncompress b output inputLength l in
  Bytes.to_string output;;

(*  *)
let charInv i j =
  let intGen = (int_range i j) in
    Gen.map char_of_int intGen.gen;;

(*  *)

(* let rec testString i j = if i<=j then (String.make 1 (char_of_int i)) ^ (testString (i+1) j) else "";;

let s = (testString 0 255) in
    if s = (decomp (comp (s)) (String.length s))
      then (print_endline (string_of_int (String.length (comp s))))
      else print_endline "false";; *)

let nonsenseTest = Test.make ~count:1000 ~name:"Different strings must stay different"
  (pair (string_gen_of_size (int_range 0 1000).gen (charInv 0 255))(string_gen_of_size (int_range 0 1000).gen (charInv 0 255)))
  (fun (s, p) ->
    let lengthS = String.length s in
    let lengthP = String.length p in
      ((s <> p) ==> ((decomp (comp s) lengthS <> decomp (comp p) lengthP ))));;
let _= QCheck_runner.run_tests  [nonsenseTest]

let compTest = Test.make ~count:1000 ~name:"String is completely reconstructed with no change"
  (string_gen_of_size (int_range 0 1000).gen (charInv 0 255))
  (fun s ->
    let length = String.length s in
      s = decomp (comp s) length) ;;
let _= QCheck_runner.run_tests  [compTest]


(*If compressed multiple times the length will either grow or stay the same*)
let someTest = Test.make ~count:1000
  (string_gen_of_size (int_range 0 100).gen (charInv 0 255))
  (fun s ->
    String.length (comp s) <= String.length (comp (comp s)));;
let _= QCheck_runner.run_tests  [someTest]

(*~6 min to check a string of length 1.000.000.000*)

let someTest = Test.make ~count:1000
  (string_gen_of_size (int_range 0 100).gen (charInv 0 255))
  (fun s ->
    let length = String.length s in
      s = (decomp (comp s) length));;
let _= QCheck_runner.run_tests [someTest]


let noChange = Test.make ~count:1000 ~name:"The string remains same after comp and decomp"
  (string_gen_of_size (small_int).gen (charInv 0 255))
  (fun s ->
    let length = String.length s in
      s = (decomp (comp s) length));;
let _= QCheck_runner.run_tests  [noChange]

let oddSymbols = Test.make ~count:1000 ~name:"The generate odds symbols"
  (string_gen_of_size (small_int).gen (charInv 0 31))
  (fun s ->
    let length = String.length s in
      s = (decomp (comp s) length));;
let _= QCheck_runner.run_tests  [oddSymbols]

let compDecomp = Test.make ~count:1000 ~name:"Compress and decompress the same string on both sides of ="
  (string_gen_of_size (int_range 0 100).gen (charInv 0 255))
  (fun s ->
    let length = String.length s in
      (decomp (comp s) length) = (decomp (comp s) length));;
let _= QCheck_runner.run_tests [compDecomp]

let uniqueComp = Test.make ~count:1000 ~name:"All comp of same string contain same bytes"
  (string_gen_of_size (int_range 0 100).gen (charInv 0 255))
  (fun s ->
    comp s = comp s);;
let _= QCheck_runner.run_tests [uniqueComp];;

let uniqueCompLength = Test.make ~count:1000 ~name:"All comp of same string has same length"
  (string_gen_of_size (int_range 0 1000).gen (charInv 0 255))
  (fun s ->
    String.length (comp s) = String.length (comp s));;
let _= QCheck_runner.run_tests  [uniqueCompLength];;


let s1 = comp "ik";;
let s2 = comp "ik";;
(*print_endline (String.escaped (comp "k"));;*)
print_endline ("String: " ^ s1);;
print_endline ((string_of_int(Char.code (String.get s1 0))) ^ " " ^
                (string_of_int(Char.code (String.get s1 1))) ^ " " ^
                (string_of_int(Char.code (String.get s1 2))));;

(* print_endline (String.escaped (comp "k");; *)
print_endline ((string_of_int(Char.code (String.get s2 0))) ^ " " ^
                (string_of_int(Char.code (String.get s2 1))) ^ " " ^
                (string_of_int(Char.code (String.get s2 2))));;



(*Classify by ASCII values*)
let classify_test =
  let collector = set_collect
    (fun s ->
      "ASCII value: " ^ string_of_int (Char.code (String.get s 0)))
      (string_gen_of_size (int_range 1 1).gen (charInv 0 255)) in
Test.make ~count: 10000 ~name: "s = decomp(comp s)"
  collector (fun c -> let length = String.length c in
    c = decomp (comp c) length);;
let _ = QCheck_runner.run_tests [classify_test];;


(*Classify by length of the strings*)
let classify_test =
let collector = set_collect
  (fun s ->
    "Length of strings: " ^ string_of_int (String.length s))
    (string_gen_of_size (int_range 0 10).gen (charInv 0 255)) in
Test.make ~count: 1000 ~name: "String equals the compressed -> decompressed string"
  collector (fun c -> let length = String.length c in
    c = decomp (comp c) length);;
let _ = QCheck_runner.run_tests [classify_test];;
