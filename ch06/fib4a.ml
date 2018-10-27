let applyCont c v = c v;;
let endCont = fun v -> v;;
let fib2Cont n c = fun v -> applyCont c (n+v);;

let rec fib1Cont n c = fun v -> fibk (n-2) (fib2Cont v c)
and fibk n c =
    if n < 2 then applyCont c 1
    else fibk (n-1) (fib1Cont n c)
;;

let fib n = fibk n endCont;;

print_int (fib 10);;
