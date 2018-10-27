let applyCont c v = c v;;

let endCont = fun v -> v;;

let fact1Cont n c = fun v -> applyCont c (n * v);;

let rec factk n c = 
    if n = 0 then applyCont c 1
    else factk (n-1) (fact1Cont n c)
;;

let fact n = factk n endCont;;

print_int (fact 5);;
