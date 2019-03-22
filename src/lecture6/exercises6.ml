#use "topfind";;
#require "qcheck";;
#require "ppx_deriving.show";;
open QCheck;;


let rec member xs y = match xs with
    | [] -> false
    | x::xs -> x=y || member xs y;;

let list = Gen.(generate1 (list_size (int_bound 100_000) small_int));;
member list 3232321;;



let rec fac n = match n with
    | 0 -> 1
    | _ -> n * fac (n-1)

let rec fac' n =
    let rec fac_local n acc = match n with
        | 0 -> acc
        | _ -> fac_local (n-1) (n * acc)
    in fac_local n 1;;

let rec reverse xs = match xs with
    | [] -> []
    | x::xs -> (reverse xs) @ [x]

let rec reverse' xs = 
    let rec reverse_local xs acc = match xs with 
        | [] -> acc
        | x::xs -> reverse_local xs ([x]@acc)
    in reverse_local xs [];;

reverse' list;;

let list_gen = 
    let gen = Gen.list_size (Gen.int_bound 1000) Gen.small_int 
    in make gen;;

let test_reverse = Test.make ~name:"reverse agreement test" ~count:1000
            (set_shrink Shrink.list list_gen)
            (fun list -> reverse list = reverse' list);;

let tiny_int_gen = 
    let gen = Gen.int_bound 100
    in make gen;;
let test_fac = Test.make ~name:"fac agreement test" ~count:1000
            (set_shrink Shrink.int tiny_int_gen)
            (fun int -> fac int = fac' int);;

QCheck_runner.run_tests ~verbose:true [test_reverse; test_fac];;
