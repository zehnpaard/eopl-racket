type continuation =
    | EndCont
    | Fact1Cont of int * continuation
;;

let rec applyCont c v = match c with
  | EndCont -> v
  | Fact1Cont (n, sc) -> applyCont sc (n * v)
;;

let rec factk n c =
    if n = 0 then applyCont c 1
    else factk (n-1) (Fact1Cont (n, c))
;;

let fact n = factk n EndCont;;

print_int (fact 5);;
