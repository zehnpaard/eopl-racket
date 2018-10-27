let rec factk n c = 
    if n = 0 then c 1
    else factk (n-1) (fun v -> c (n * v))
;;

let fact n = factk n (fun v -> v);;

print_int (fact 5);;
