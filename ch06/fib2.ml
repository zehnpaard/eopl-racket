type continuation =
    | EmptyCont
    | Fib1Cont of int * continuation
    | Fib2Cont of int * continuation
;;

let rec applyCont c v = match c with
  | EmptyCont -> v
  | Fib1Cont (n, sc) -> fibk (n - 2) (Fib2Cont (v, sc))
  | Fib2Cont (n, sc) -> applyCont sc (n + v)
and fibk n c =
    if n < 2 then applyCont c 1
    else fibk (n-1) (Fib1Cont (n, c))
;;

let fib n = fibk n EmptyCont;;

print_int (fib 10);;
