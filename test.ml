(* let rec fibonacci n =
  if n < 3 then
    1
  else
    fibonacci (n-1) + fibonacci (n-2)

let () =
  print_endline "Fibonacci begin";
  for n = 1 to 16 do
    print_endline (string_of_int (fibonacci n))
  done;
  print_endline "Fibonacci end"

let something to_ check for_ me = 
  to_ + check + for_ + me

let x = something 12 3
let z = x 3 4
;;
assert (22 = z)
;;
let f = x 5 6;;
assert (26 = f);;
*)
let o = [|1;2;3;4;5;6|];;
assert (2 = o.(1));;
assert (6 = Array.length o);;
print_int 12;;
(* print_int 5;;
print_endline " should be 125";; *)
(*
print_endline "logged this";;

 let z = [1;3;4;5;6];;
print_endline (string_of_int (List.nth z 2));;
List.iter print_int z;; *)
(* print_endline " should be 13456";; *)
(* let o = ref 2;;
assert (!o = 2);;
o := 3;;
assert (!o = 3);;*)
(* 
let x (a, b) = a + b;;

x (1, 2);; *)

(* 

 module IntPairs =
       struct
         type t = int * int
         let compare (x0,y0) (x1,y1) =
           match Pervasives.compare x0 x1 with
               0 -> Pervasives.compare y0 y1
             | c -> c
       end

module PairsSet = Set.Make(IntPairs)

let m = PairsSet.(empty |> add (2,3) |> add (5,7) |> add (11,13));; *)
(*   
ignore(m);;

print_int (PairsSet.cardinal m);;
print_endline " should be 3";; *)

(* let (a, b) = PairsSet.find (2,3) m in
assert(a = 2 && b = 3);; *)

(* PairsSet.iter (fun (a, _) -> ignore(a)) m;; *)
(* assert ((PairsSet.mem (2,3) m) && (PairsSet.mem (5,7) m) && (PairsSet.mem (11,13) m));; *)
  
 (* let o = Sys.time();; *)
(* print_endline (string_of_int (int_of_float o));; *)
(* print_endline (string_of_float o);; *)

(* Random.init 999;; *)

(* let _ = Hashtbl.create 12;; *)

(* print_endline "oh hi there";; *)
(* Hashtbl.replace table "three" 3;; *)
(* match (Hashtbl.find_opt table "three") with 
| Some s -> print_endline (string_of_int s)
| _ -> print_endline "NOT FOUND"
;; *)

(* FAILING


print_endline ("Testing " ^ (string_of_int (Random.int 12)));;

let xy = List.length z;;
print_endline ("oo:" ^ string_of_int xy);
print_endline "\n";; *)
