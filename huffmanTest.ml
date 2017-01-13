open QCheck


external huffman_Compress : bytes -> bytes -> int -> int = "encode_stub"
external huffman_Uncompress : bytes -> bytes -> int -> int -> unit= "decode_stub"

(*Creating first test case*)
let s1 = Bytes.of_string ""
let l = String.length s1
let s2 = Bytes.create (l+384)

(*Printing an integer representing the length of the output*)
let testCompress = huffman_Compress s1 s2 l;;
print_int testCompress;;
print_newline ();;
(*Printing bytes, not expecting anything readable*)
let sub1 =  Bytes.sub_string s2 0 testCompress;;
print_string sub1;;
print_newline ();;


(*Decompression *)
let subBytes = Bytes.sub s2 0 testCompress;;
let lDecomp =  Bytes.length subBytes;;

let emptyB = Bytes.create l;;
(*Should return nothing, as Huffman_Uncompress has no return statement,
therefore there is no assignemtn to a value*)
huffman_Uncompress subBytes emptyB lDecomp l;;

let stringOut = Bytes.sub_string s1 0 l;;
print_string stringOut;;


let string_to_bytes s = Bytes.of_string s;;

let orig s = Bytes.of_string s;;
let len s = String.length s;;
let output s = Bytes.create ((len s)+384);;

let compress input = huffman_Compress (orig input) (output input) (len input);;


(*  comp : string -> bytes*)
let comp s =
  let input = Bytes.of_string s in
  let length = String.length s in
  let output = Bytes.create (length + 384) in
  let resLength = (huffman_Compress input output length) in
  Bytes.sub output 0 resLength;;

(*bytes -> int -> string*)
let decomp b l =
  let output = Bytes.create l in
  let inputLength = Bytes.length b in
  let () = huffman_Uncompress b output inputLength l in
  Bytes.to_string output;;

(*Test to test if tests can test (testing if QCheck works)*)
(*let testTest = Test.make
        (int)(fun i -> i=i)
let _= QCheck_runner.run_tests [testTest]*)

(*If compressed multiple times the length will either grow or stay the same*)
  let someTest = Test.make ~count:1000
              (string_gen_of_size (int_range 0 100).gen char.gen)(fun s ->
                    String.length (comp s) <= String.length (comp (comp s)));;
  let _= QCheck_runner.run_tests ~verbose:true [someTest]

(*~6 min to check a string of length 1.000.000.000*)
let someTest = Test.make ~count:1000
            (string_gen_of_size (int_range 0 100).gen char.gen)(fun s -> let length = String.length s in
                                s = (decomp (comp s) length));;
let _= QCheck_runner.run_tests [someTest]


(*Classify by ASCII values*)
let classify_test =
let collector = set_collect
    (fun s -> "ASCII value: " ^ string_of_int (Char.code (String.get s 0))) (string_gen_of_size (int_range 1 1).gen char.gen) in
Test.make ~count: 100 ~name: "decomp(comp s)"
          collector (fun c -> let length = String.length c in
                                c = decomp (comp c) length);;
let _ = QCheck_runner.run_tests [classify_test];;

(*Classify by length of the strings*)
let classify_test =
let collector = set_collect
    (fun s -> "Length of strings: " ^ string_of_int (String.length s))
          (string_gen_of_size (int_range 0 10).gen char.gen) in
Test.make ~count: 1000 ~name: "String equals the compressed -> decompressed string"
          collector (fun c -> let length = String.length c in
                                c = decomp (comp c) length);;
let _ = QCheck_runner.run_tests [classify_test];;
