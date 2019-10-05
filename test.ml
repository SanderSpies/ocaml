(*      
 let rec fibonacci2 n =
    if n < 3 then
      1
    else
      fibonacci2 (n-1) + fibonacci2 (n-2)
;;
let ix = ref 0
;;

let test a b = 
  a + b
;;

let testo a =
  test 5 a 
;;

for i = 0 to 3000 do (
  ix := !ix + 1;
  print_endline ("Iteration:" ^ string_of_int i);
  (for i = 0 to 100 do 
    let x b = () in
    x ()
  done);
  
  let rec fibonacci n =
    if n < 3 then
      1
    else
      fibonacci (n-1) + fibonacci (n-2)
  in
  print_endline "Fibonacci begin";
  for n = 1 to 16 do
    print_endline (string_of_int (fibonacci n))
  done;
  print_endline "Fibonacci end";
  print_endline "Fibonacci begin 2";
  for n = 1 to 5 do
    print_endline (string_of_int (fibonacci2 n))
  done;
  print_endline "Fibonacci end 2";
  


  let something to_ check for_ me = 
    to_ + check + for_ + me
  in
  let x = something 12 3
  in
  let z = x 3 4
  in
  assert (22 = z)
  ;
  let f = x 5 6
  in
  assert (26 = f);

  let o = [|1;2;3;4;5;6|] in
  assert (2 = o.(1));
  assert (6 = Array.length o);
  print_endline "yolo 1";
  print_endline "hi";
  let x = string_of_int 12 in
  print_endline (x ^ " should be 12");
  print_int 125;
  print_endline " should be 125";

  print_endline "logged this";

  let z = [1;3;4;5;6] in
  print_endline ((string_of_int (List.nth z 2)) ^ " should be 4");
  List.iter print_int z;
  print_endline " should be 13456";
  let o = ref 2 in
  assert (!o = 2);
  o := 3;
  assert (!o = 3);

  let xy = List.length z in
  print_endline ("oo:" ^ string_of_int xy);
  print_endline "\n";


)
done;;

print_endline ("Ix: " ^ string_of_int !ix);;

let x (a, b) = a + b;;
let three = x (1, 2);;
assert (three = 3);;

assert (("A" ^ "B" ^ "C" ^ "D") = "ABCD");;


 module IntPairs =
       struct
         type t = int * int
         let compare (x0,y0) (x1,y1) =
           match Pervasives.compare x0 x1 with
               0 -> Pervasives.compare y0 y1
             | c -> c
       end

module PairsSet = Set.Make(IntPairs)

let m = PairsSet.(empty |> add (2,3) |> add (5,7) |> add (3,4));;


assert ((PairsSet.mem (2,3) m) && (PairsSet.mem (5,7) m) && (PairsSet.mem (3,4) m));;
print_int (PairsSet.cardinal m);;
print_endline " should be 3";;

let (a, b) = PairsSet.find (2,3) m;;
assert(a = 2 && b = 3);;

PairsSet.iter (fun (a, _) -> ignore(a)) m;;
assert ((PairsSet.mem (2,3) m) && (PairsSet.mem (5,7) m) && (PairsSet.mem (3,4) m));;
  

 (* let o = Sys.time();; *)
(* print_endline (string_of_int (int_of_float o));; *)
(* print_endline (string_of_float o);;  *)

 


let z = 5.5 +. 7.1 +. 9.3;;
print_endline ((string_of_float z) ^ " should be 21.9");;
assert(z = 21.9);; 
 


  type error =
    Test
exception Error of error;;


(* print_endline "BEFORE";; *)

let z a = 
  (raise (Error Test);
  a
  )
;;

assert (
  (try (
    (print_endline "a0";
    ignore(z 2);
    false)
  )
  with
  | _ -> print_endline "kaas"; true)
);;

print_endline "z1" ;;
 
assert (
  (try (
    print_endline "a1";
    true
  )
  with
  | _ -> print_endline "should not come here!"; false)
);;

type error2 =
    | Test1
    | Test2
    | Test3
    | Test4


let z y = 
  match y with 
  | Test1 -> "Test 1x"
  | Test2 -> "Test 2"
  | Test3 -> "Test 3"
  | _ -> "Test 4"
;;

    
exception Error2 of error2;; 

let z2 _a = 
  raise (Error2 Test2)  
;;

print_endline "foobar123";;


assert (
  (try (
    z2 2;
    "wrong"
  )
  with
  | Error2 Test1 -> "fromage"
  | Error2 Test2 -> "kaas"
  | Error2 Test3 -> "cheese"
  | _ -> "cow"
  ) = "kaas"
);;

print_endline "foobar123333";;

print_endline "foobar123333";;


module Dog =
  struct
    let nested_exception o =  
      (z2 o) + 2
  end;;

assert (
  (try (
    print_endline "a3";
    ignore(Dog.nested_exception 3);
    "wrong"
  )
  with
  | Error2 Test1 -> "fromage"
  | Error2 Test2 -> "kaas"
  | Error2 Test3 -> "cheese"
  | _ -> "cow"
  )
= "kaas");;
(* failwith "yolo";; *)
 
print_endline "Oh no I'm trapped";; 

 (* Random.init 999;; *)

print_endline "frak";;

if not(String.contains "aa" 'a') then
  print_endline "okay"
else 
  print_endline "expected"
;;

if String.contains "aa" 'a' then
  print_endline "expected 2"
else 
  print_endline "wut"
;; 

if String.contains "b" 'a' then
  print_endline "wrong"
else 
  print_endline "expected 3"
;;

 if String.contains "" 'a' then
  print_endline "wrong 2"
else 
  print_endline "expected 4"
;;

print_endline "wah wah wah";;

(*)
print_endline "oh hi there";;
Hashtbl.replace table "three" 3;;
match (Hashtbl.find_opt table "three") with 
| Some s -> print_endline (string_of_int s)
| _ -> print_endline "NOT FOUND"
;;


(* print_endline ("Testing " ^ (string_of_int (Random.int 12)));; *)




(* let  arr = Sys.argv;;
Array.iter (fun x -> print_endline x) arr; *)   *)
  
 let rec assoc3 x l =
  match l with
  | [] -> (
    raise Not_found
  )
  | (y1, y2, _) :: _ when y1 = x -> (
    print_endline "okay!";
    y2
  )
  | _ :: t -> assoc3 x t
;;



assert (assoc3 "-a" [("-a", "hello", "x")] = "hello");;
 *)

 (* type example = 
  | Foo of (string -> unit)
  | Shoo of string
  | Boo of (int -> unit)
  | Oh of int 
  | Ar of bool

let a = ref ""

let rec z = function
  | Foo s -> s "yolo"
  | Shoo s -> print_endline s
  | Boo p -> p 12
  | Oh i -> print_int i
  | Ar x -> if x then print_endline "true" else print_endline "false"
;;

a := "ppp";;

z (Boo (fun s -> print_endline ("dog" ^ string_of_int s))) *)

  
let source_name = ref None
let output_name = ref None
let usage = "usage: ocamllex [options] sourcefile"

let specs =
  ["-o", Arg.String (fun x -> print_endline "right"; output_name := Some x),
    " <file>  Set output file name to <file>"];;

print_endline "a1";;
let _ =
  Arg.parse
    specs
    (fun name -> 
      print_endline "01";
      source_name := Some name)
    usage    
;;

print_endline "done."