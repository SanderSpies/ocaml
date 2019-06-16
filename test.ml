 for i = 0 to 1 do (
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
(* print_endline (string_of_int (int_of_float o));; 
print_endline (string_of_float o);; *)

 


let z = 5.5 +. 7.1 +. 9.3;;
print_endline ((string_of_float z) ^ " should be 21.9");;
assert(z = 21.9);;

(* Random.init 999;; *)

(* let _ = Hashtbl.create 12;; *)

(* print_endline "oh hi there";; *)
(* Hashtbl.replace table "three" 3;; *)
(* match (Hashtbl.find_opt table "three") with 
| Some s -> print_endline (string_of_int s)
| _ -> print_endline "NOT FOUND"
;; *)


(* print_endline ("Testing " ^ (string_of_int (Random.int 12)));; *)


