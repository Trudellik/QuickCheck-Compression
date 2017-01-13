open QCheck

external huffman_Compress : bytes -> bytes -> int -> int = "encode_stub"
external huffman_Uncompress : bytes -> bytes -> int -> int -> unit= "decode_stub"

(* if ((Array.length Sys.argv) > 1) then () *)
  let s1 = Bytes.of_string "Hello"
  let l = String.length s1
  let s2 = Bytes.create (l+384)

(*
  (*Printing an integer representing the length of the output*)
  let testCompress = huffman_Compress s1 s2 l;;
  print_int testCompress;;
  (*Printing bytes, not expecting anything readable*)
  print_newline ();;
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
  print_string stringOut;; *)


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
(*Will only return a tuple containing the information related to compress*)
(*bytes -> int -> string*)
let decomp b l =
  let output = Bytes.create l in
  let inputLength = Bytes.length b in
  let () = huffman_Uncompress b output inputLength l in
  Bytes.to_string output;;

(*  *)

let compTest = Test.make ~count:1000 (*~print:string_of_inputs *)
  (string_gen_of_size (int_range 0 1000).gen char.gen)
  (fun s -> let length = String.length s in
    s = decomp (comp s) length );;

(* let revCompTest = Test.make ~count:1000 (*~print:string_of_inputs *)
  (string_gen_of_size (int_range 0 1000).gen char.gen)
  (fun s -> let length = String.length s in
    s = comp( decomp( decomp (comp s) length) length));; *)

let _= QCheck_runner.run_tests ~verbose:true [compTest]
