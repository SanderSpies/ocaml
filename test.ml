let rec fibonacci n =
  if n < 3 then
    1
  else
    fibonacci (n-1) + fibonacci (n-2)

let () =
  for n = 1 to 16 do
    print_endline (string_of_int (fibonacci n))
  done;
  print_endline "..."

let something to_ check for_ me = 
  to_ + check + for_ + me

let x = something 12 3
let z = x 3 4
;;
print_endline ("Testing the sum of 12 + 3 + 3 + 4 = 22 === " ^ string_of_int z)
;;
let f = x 5 6
;;
print_endline ("Testing the sum of 12 + 3 + 5 + 6 = 26 === " ^ string_of_int f)

let o = [|1;2;3;4;5;6|];;
print_endline ("2 ===" ^ (string_of_int o.(1)));;
print_endline ("6 ===" ^ (string_of_int (Array.length o)));;
let z = [1;3;4;5;6];;
List.iter(fun x -> print_endline (string_of_int x)) z;
print_endline ("4 === " ^ (string_of_int (List.nth z 2)));;
let o = ref 2;;
print_endline ("testing" ^ string_of_int !o);;
o := 3;;
print_endline ("testing" ^ string_of_int !o);;
(* let o = Sys.time();;
print_endline (string_of_int (int_of_float o));;
print_endline (string_of_float o);; *)


(* let _ = §tbl.create 12;; *)

print_endline "oh hi there";;
(* Hashtbl.replace table "three" 3;; *)
(* match (Hashtbl.find_opt table "three") with 
| Some s -> print_endline (string_of_int s)
| _ -> print_endline "NOT FOUND"
;; *)

(* FAILING

Random.init 999;;
print_endline ("Testing " ^ (string_of_int (Random.int 12)));;

let xy = List.length z;;
print_endline ("oo:" ^ string_of_int xy);
print_endline "\n";; *)
