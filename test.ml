



let bork () = (
  let a = [|1;2;3|] in
  print_int (a.(2));
  print_endline "\n"
);;
bork();;
(* TEST *)

let () =
  let a = [|0;1;2;3;4;5;6;7;8;9|] in
  assert (Array.exists (fun a -> a < 10) a);
  assert (Array.exists (fun a -> a > 0) a);
  assert (Array.exists (fun a -> a = 0) a);
  assert (Array.exists (fun a -> a = 1) a);
  assert (Array.exists (fun a -> a = 2) a);
  assert (Array.exists (fun a -> a = 3) a);
  assert (Array.exists (fun a -> a = 4) a);
  assert (Array.exists (fun a -> a = 5) a);
  assert (Array.exists (fun a -> a = 6) a);
  assert (Array.exists (fun a -> a = 7) a);
  assert (Array.exists (fun a -> a = 8) a);
  assert (Array.exists (fun a -> a = 9) a);
  assert (not (Array.exists (fun a -> a < 0) a));
  assert (not (Array.exists (fun a -> a > 9) a));
  assert (Array.exists (fun _ -> true) a)
;;
 
print_endline "o0";;

let z1 () =
  let a = [|1;2;3|] in
  assert (Array.exists (fun a -> a < 3) a);
  assert (Array.exists (fun a -> a < 2) a);
  assert (not (Array.exists (fun a -> a < 1) a));
  assert (Array.exists (fun a -> a mod 2 = 0)  [|1;4;5|]);
  assert (not (Array.exists (fun a -> a mod 2 = 0)  [|1;3;5|]));
   assert (not (Array.exists (fun _ -> true) [||]));
   let matrix = Array.make_matrix 10 10 1 in
  print_endline "a";
  assert (Array.exists (fun a -> a.(9) = 1) matrix);
  print_endline "z";
  let f = Array.create_float 10 in
  Array.fill f 0 10 1.0;
  (* assert (Array.exists (fun a -> a = 1.0) f) *)
;;

z1 ();;

print_endline "o1";;


let () =
  let a: int array = [||] in
  assert (not (Array.exists (fun a -> a = 0) a));
  assert (not (Array.exists (fun a -> a = 1) a));
  assert (not (Array.exists (fun a -> a = 2) a));
  assert (not (Array.exists (fun a -> a = 3) a));
  assert (not (Array.exists (fun a -> a = 4) a));
  assert (not (Array.exists (fun a -> a = 5) a));
  assert (not (Array.exists (fun a -> a = 6) a));
  assert (not (Array.exists (fun a -> a = 7) a));
  assert (not (Array.exists (fun a -> a = 8) a));
  assert (not (Array.exists (fun a -> a = 9) a));
  assert (not (Array.exists (fun a -> a <> 0) a));
  assert (not (Array.exists (fun a -> a <> 1) a));
  assert (not (Array.exists (fun a -> a <> 2) a));
  assert (not (Array.exists (fun a -> a <> 3) a));
  assert (not (Array.exists (fun a -> a <> 4) a));
  assert (not (Array.exists (fun a -> a <> 5) a));
  assert (not (Array.exists (fun a -> a <> 6) a));
  assert (not (Array.exists (fun a -> a <> 7) a));
  assert (not (Array.exists (fun a -> a <> 8) a));
  assert (not (Array.exists (fun a -> a <> 9) a));
  assert (not (Array.exists (fun a -> a < 0) a));
  assert (not (Array.exists (fun a -> a < 1) a));
  assert (not (Array.exists (fun a -> a < 2) a));
  assert (not (Array.exists (fun a -> a < 3) a));
  assert (not (Array.exists (fun a -> a < 4) a));
  assert (not (Array.exists (fun a -> a < 5) a));
  assert (not (Array.exists (fun a -> a < 6) a));
  assert (not (Array.exists (fun a -> a < 7) a));
  assert (not (Array.exists (fun a -> a < 8) a));
  assert (not (Array.exists (fun a -> a < 9) a));
  assert (not (Array.exists (fun a -> a > 0) a));
  assert (not (Array.exists (fun a -> a > 1) a));
  assert (not (Array.exists (fun a -> a > 2) a));
  assert (not (Array.exists (fun a -> a > 3) a));
  assert (not (Array.exists (fun a -> a > 4) a));
  assert (not (Array.exists (fun a -> a > 5) a));
  assert (not (Array.exists (fun a -> a > 6) a));
  assert (not (Array.exists (fun a -> a > 7) a));
  assert (not (Array.exists (fun a -> a > 8) a));
  assert (not (Array.exists (fun a -> a > 9) a));
;;

print_endline "o2";;
let () =
  let a = [|0;1;2;3;4;5;6;7;8;9|] in
  assert (Array.for_all (fun a -> a < 10) a);
  assert (Array.for_all (fun a -> a >= 0) a);
  assert (not (Array.for_all (fun a -> a = 0) a));
  assert (not (Array.for_all (fun a -> a = 1) a));
  assert (not (Array.for_all (fun a -> a = 2) a));
  assert (not (Array.for_all (fun a -> a = 3) a));
  assert (not (Array.for_all (fun a -> a = 4) a));
  assert (not (Array.for_all (fun a -> a = 5) a));
  assert (not (Array.for_all (fun a -> a = 6) a));
  assert (not (Array.for_all (fun a -> a = 7) a));
  assert (not (Array.for_all (fun a -> a = 8) a));
  assert (not (Array.for_all (fun a -> a = 9) a));
  assert (Array.for_all (fun a -> a <> 10) a);
  assert (Array.for_all (fun a -> a <> (-1)) a);
  assert (Array.for_all (fun _ -> true) a);
;;

let z3 () =
  assert (Array.for_all (fun x -> x mod 2 = 0) [|2;4;6|]);
  assert (not (Array.for_all (fun x -> x mod 2 = 0) [|2;3;6|]));
  assert (Array.for_all (fun _ -> false) [||]);
  assert (Array.for_all (fun a -> a.(9) = 1) (Array.make_matrix 10 10 1));
  let f = Array.create_float 10 in
  Array.fill f 0 10 1.0;
  (* assert (Array.for_all (fun a -> a = 1.0) f); *)
;;

print_endline "o3";;

z3 ();;
 
let () =
  let a = [||] in
  assert (Array.for_all (fun a -> a < 10) a);
  assert (Array.for_all (fun a -> a >= 0) a);
  assert (Array.for_all (fun a -> a = 0) a);
  assert (Array.for_all (fun a -> a = 1) a);
  assert (Array.for_all (fun a -> a = 2) a);
  assert (Array.for_all (fun a -> a = 3) a);
  assert (Array.for_all (fun a -> a = 4) a);
  assert (Array.for_all (fun a -> a = 5) a);
  assert (Array.for_all (fun a -> a = 6) a);
  assert (Array.for_all (fun a -> a = 7) a);
  assert (Array.for_all (fun a -> a = 8) a);
  assert (Array.for_all (fun a -> a = 9) a);
  assert (Array.for_all (fun a -> a <> 10) a);
  assert (Array.for_all (fun a -> a <> (-1)) a);
  assert (Array.for_all (fun _ -> true) a);
;;

let () =
  let a = [|1;2;3;4;5;6;7;8;9|] in
  assert (Array.mem 1 a);
  assert (Array.mem 2 a);
  assert (Array.mem 3 a);
  assert (Array.mem 4 a);
  assert (Array.mem 5 a);
  assert (Array.mem 6 a);
  assert (Array.mem 7 a);
  assert (Array.mem 8 a);
  assert (Array.mem 9 a);
  assert (not (Array.mem 0 a));
  assert (not (Array.mem 10 a));
;;

print_endline "o4";;
let () =
  assert (Array.mem 2 [|1;2;3|]);
  assert (not (Array.mem 2 [||]));
  assert (Array.mem (ref 3) [|ref 1; ref 2; ref 3|]);
  assert (Array.mem [|1;2;3|] [|[|1;2;3|];[|2;3;4|];[|0|]|]);
  assert (Array.mem 1 (Array.make 100 1));
  assert (Array.mem (ref 1) (Array.make 100 (ref 1)));
  let f = Array.create_float 10 in
  Array.fill f 0 10 1.0;
  assert (Array.mem 1.0 f);
;;

print_endline "o5";;
let () =
  let a = [|1;2;3;4;5;6;7;8;9|] in
  assert (Array.memq 1 a);
  assert (Array.memq 2 a);
  assert (Array.memq 3 a);
  assert (Array.memq 4 a);
  assert (Array.memq 5 a);
  assert (Array.memq 6 a);
  assert (Array.memq 7 a);
  assert (Array.memq 8 a);
  assert (Array.memq 9 a);
  assert (not (Array.memq 0 a));
  assert (not (Array.memq 10 a));
;;

print_endline "o6";;
let () =
  assert (Array.memq 2 [|1;2;3|]);
  assert (not (Array.memq 2 [||]));
  assert (not (Array.memq (ref 3) [|ref 1; ref 2; ref 3|]));
  assert (not (Array.memq [|1;2;3|] [|[|1;2;3|];[|2;3;4|];[|0|]|]));
  assert (Array.memq 1 (Array.make 100 1));
  assert (not (Array.memq (ref 1) (Array.make 100 (ref 1))));
  let f = Array.create_float 10 in
  Array.fill f 0 10 1.0;
  (* FIXME
  if Config.flat_float_array then assert (not (Array.memq 1.0 f));
  *)
;;

let () = print_endline "OK"
 

let dogo_array = Array.make 10 "dogo";;

if (try
  Array.set dogo_array 12 "yolo";
  false
with
| _ -> true) = false then (
  print_endline "FIXME: Not correct thrown index out of bounds error";
  exit 0)
;;
print_endline "works it seems";;

        
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

 module IntPairs =
       struct
         type t = int * int
         let compare (x0,y0) (x1,y1) =
           match Pervasives.compare x0 x1 with
               0 -> Pervasives.compare y0 y1
             | c -> c
       end
;;

module PairsSet = Set.Make(IntPairs);; 

type error = Test;;

exception Error of error;;

type error2 =
  | Test1
  | Test2
  | Test3
  | Test4;;   
 
exception Error2 of error2;;



module Dog =
  struct
  let z2 _a = 
      raise (Error2 Test2)  

    let nested_exception o =  
      (z2 o) + 2
  end;;

type x = Foo;;

(* print_endline "a1";;

print_endline "a2";;
Gc.set ({c with verbose = 4});;
print_endline "a3";; *)

for i = 0 to 5 do (
  
  print_endline ("Iteration: " ^ (string_of_int i));
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


  let x (a, b) = a + b in
  let three = x (1, 2) in
  assert (three = 3);
  assert (("A" ^ "B" ^ "C" ^ "D") = "ABCD");

  let m = PairsSet.(empty |> add (2,3) |> add (5,7) |> add (3,4)) in


  assert ((PairsSet.mem (2,3) m) && (PairsSet.mem (5,7) m) && (PairsSet.mem (3,4) m));
  print_int (PairsSet.cardinal m);
  print_endline " should be 3";
 
  let (a, b) = PairsSet.find (2,3) m in
  assert(a = 2 && b = 3);

  PairsSet.iter (fun (a, _) -> ignore(a)) m;
  assert ((PairsSet.mem (2,3) m) && (PairsSet.mem (5,7) m) && (PairsSet.mem (3,4) m));
  

 let o = Sys.time() in
 print_endline (string_of_int (int_of_float o));
 print_endline (string_of_float o);


 let z = 5.5 +. 7.1 +. 9.3 in
 print_endline ((string_of_float z) ^ " should be 21.9");
 assert(z = 21.9);
   
  let z a = ( 
   raise (Error Test);
   a
)
in
assert (
  (try (
    (print_endline "a0";
    ignore(z 2);
    false)
  )
  with
  | _ -> print_endline "kaas"; true)
);  

assert (
  (try (
    print_endline "a1";
    true
  )
  with
  | _ -> print_endline "should not come here!"; false)
);

 
 let z y = 
  match y with 
  | Test1 -> "Test 1x"
  | Test2 -> "Test 2"
  | Test3 -> "Test 3"
  | _ -> "Test 4"
in

let z2 _a = 
  raise (Error2 Test2)  
in

(* print_endline "foobar123";  *)

 
  (try (
    z2 2;
    "wrong"
  )
  with
  | Error2 Test1 -> "fromage"
  | Error2 Test2 -> "kaas"
  | Error2 Test3 -> "cheese"
  | _ -> "cow"
  ) = "kaas";

 (* caml_string_equal errors here *)

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
= "kaas");
(* failwith "yolo"; *)
 
print_endline "Oh no I'm trapped"; 

 (* Random.init 999; *)

print_endline "frak";

if not(String.contains "aa" 'a') then
  print_endline "okay"
else 
  print_endline "expected"
;

if String.contains "aa" 'a' then
  print_endline "expected 2"
else 
  print_endline "wut"
; 

if String.contains "b" 'a' then
  print_endline "wrong"
else 
  print_endline "expected 3"
;

 if String.contains "" 'a' then
  print_endline "wrong 2"
else 
  print_endline "expected 4"
;

print_endline "wah wah wah";


print_endline "oh hi there";
(* Hashtbl.replace table "three" 3;
match (Hashtbl.find_opt table "three") with 
| Some s -> print_endline (string_of_int s)
| _ -> print_endline "NOT FOUND" *)



(* print_endline ("Testing " ^ (string_of_int (Random.int 12))); *)




(* let  arr = Sys.argv;
Array.iter (fun x -> print_endline x) arr; *) 
  
 let rec assoc3 x l = 
  match l with
  | [] -> raise Not_found  
  | (y1, y2, _) :: _ when y1 = x -> y2  
  | _  :: t -> assoc3 x t
  in


  print_endline "catch 11";

  if (assoc3 "-a" [("-a", "hello", "x")] = "hello") then
    print_endline "ok 1";
  
print_endline "catch 22";

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

  
let source_name: string option ref = ref None in 
let output_name: string option ref = ref None in
let usage = "usage: ocamllex [options] sourcefile" in

let specs =
  ["-o", Arg.String (fun x -> print_endline "right"; output_name := Some x),
    " <file>  Set output file name to <file>"]
in
let type_ x =
  if x = Obj.first_non_constant_constructor_tag then "first_non_constant_constructor_tag" 
  else if x = Obj.last_non_constant_constructor_tag then "last_non_constant_constructor_tag"
  else if x = Obj.lazy_tag then "lazy_tag"
  else if x = Obj.closure_tag then "closure_tag"
  else if x = Obj.object_tag then "object_tag"
  else if x = Obj.infix_tag then "infix_tag"
  else if x = Obj.forward_tag then "forward_tag"
  else if x = Obj.no_scan_tag then "no_scan_tag"
  else if x = Obj.abstract_tag then "abstract_tag"
  else if x = Obj.string_tag then "string_tag"
  else if x = Obj.double_tag then "double_tag"
  else if x = Obj.double_array_tag then "double_array_tag"
  else if x = Obj.custom_tag then "custom_tag"
  else if x = Obj.final_tag then "final_tag"
  else if x = Obj.int_tag then "int_tag"
  else if x = Obj.out_of_heap_tag then "out_of_heap_tag"
  else if x = Obj.unaligned_tag then "unaligned_tag"
  else "unknown!"
in
if (type_ (Obj.tag (Obj.repr "aaa"))) <> "string_tag" then
  print_endline ("1. incorrect type:" ^ (type_ (Obj.tag (Obj.repr "aaa"))))
;
if (type_ (Obj.tag (Obj.repr "aaa"))) <> "string_tag" then
  print_endline ("1a. incorrect type:" ^ (type_ (Obj.tag (Obj.repr "aaa"))))
;
if (type_ (Obj.tag (Obj.repr 5.5))) <> "double_tag" then
  print_endline ("2. incorrect type:" ^ (type_ (Obj.tag (Obj.repr 5.5))))
;
if (type_ (Obj.tag (Obj.repr (
  let x b c d = b + c + d in 
  x 3 4
)))) <> "closure_tag" then
  print_endline ("3. incorrect type:" ^ (type_ (Obj.tag (Obj.repr (
    let x b c d = b + c + d in 
    x 3 4
  )))))
;

if (type_ (Obj.tag (Obj.repr Foo))) <> "int_tag" then
  print_endline ("4. incorrect type:" ^ (type_ (Obj.tag (Obj.repr Foo))))
;

if (type_ (Obj.tag (Obj.repr [|5.5; 6.4; 8.9|]))) <> "double_array_tag" then
  print_endline ("5. incorrect type:" ^ (type_ (Obj.tag (Obj.repr [|5.5; 6.4; 8.9|]))))
;

if (type_ (Obj.tag (Obj.repr [|5.5; 6.4; 8.9|]))) <> "double_array_tag" then
  print_endline ("6. incorrect type:" ^ (type_ (Obj.tag (Obj.repr [|5.5; 6.4; 8.9|]))))
;

if (type_ (Obj.tag (Obj.repr [|5.5; 6.4; 8.9|]))) <> "double_array_tag" then
  print_endline ("7. incorrect type:" ^ (type_ (Obj.tag (Obj.repr [|5.5; 6.4; 8.9|]))))
;

if (type_ (Obj.tag (Obj.repr [|5.5; 6.4; 8.9|]))) <> "double_array_tag" then
  print_endline ("8. incorrect type:" ^ (type_ (Obj.tag (Obj.repr [|5.5; 6.4; 8.9|]))))
;

if (type_ (Obj.tag (Obj.repr [5.5; 6.4; 8.9]))) <> "first_non_constant_constructor_tag" then
  print_endline ("9. incorrect type:" ^ (type_ (Obj.tag (Obj.repr [5.5; 6.4; 8.9]))))
;

if (type_ (Obj.tag (Obj.repr [5.5; 6.4; 8.9]))) <> "first_non_constant_constructor_tag" then
  print_endline ("10. incorrect type:" ^ (type_ (Obj.tag (Obj.repr [5.5; 6.4; 8.9]))))
;

if (type_ (Obj.tag (Obj.repr ()))) <> "int_tag" then
  print_endline ("11. incorrect type:" ^ (type_ (Obj.tag (Obj.repr ()))))
;


(* 
print_endline "\na1";;
let _ =
  Arg.parse
    specs
    (fun name -> 
      print_endline "01";
      source_name := Some name)
    usage    
;;

 *)
  

)
done;;

print_endline ("Ix: " ^ string_of_int !ix);;