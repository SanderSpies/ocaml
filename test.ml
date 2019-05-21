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
