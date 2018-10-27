let rec fibk n c =
    if n < 2 then c 1
    else fibk (n-1) (fun v -> fibk (n-2) (fun w -> c (v+w)))
;;

let fib n = fibk n (fun v -> v);;

print_int (fib 10);;
