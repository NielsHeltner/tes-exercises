#use "topfind";;
#require "qcheck";;
open QCheck;;

let rec msb num = 
    if num = 0 then
        0
    else
        let shift = (num lsr 1) in 
        msb (shift) + 1


let fst (x, y) = x

let snd p = match p with (x, y) -> y


let rec sum l = match l with 
    | [] -> 0
    | elem::elemens -> (sum elemens) + elem

let rec member l x = match l with
    | [] -> false
    | elem::elemens -> 
            if elem = x then 
                true 
            else 
                member elemens x

let rec merge xs ys = match xs, ys with
    | [], [] -> []
    | _::_, [] -> xs
    | [], _::_ -> ys
    | xelem::xelems, yelem::yelems ->
            if xelem < yelem then
                [xelem]@merge xelems ys
            else
                [yelem]@merge xs yelems


let rec eu_gcd n m = 
    if m = 0 then
        n
    else if n > m then
        eu_gcd (n - m) m
    else
        eu_gcd n (m - n)


(* https://stackoverflow.com/questions/13708701/how-to-implement-a-dictionary-as-a-function-in-ocaml *)
let empty key = 0

let add dict key value = 
            fun key' -> 
                if key' = key then
                    value
                else
                    dict key'

let find dict key = dict key

let dict1 = add empty "x" 1
let dict2 = add dict1 "y" 2
let dict3 = add dict2 "x" 3
let dict4 = add dict3 "y" 4


let sumtest = Test.make ~name:"sumtest" ~count:100 
            (pair (list int) (list int)) 
            (fun (xs, ys) -> sum (xs @ ys) = (sum xs) + (sum ys));;

let mergetest1 = Test.make ~name:"mergetest1" ~count:100 
            (pair (list int) (list int)) 
            (fun (xs, ys) -> merge xs ys = merge ys xs);;
let mergetest2 = Test.make ~name:"mergetest2" ~count:100 
            (pair (list int) (list int)) 
            (fun (xs, ys) -> merge (List.sort compare xs) (List.sort compare ys) = List.sort compare (xs@ys));;

let eu_gcdtest = Test.make ~name:"eu_gcdtest" ~count:100
            (pair int int)
            (fun (x, y) -> eu_gcd x y = eu_gcd y x);;



QCheck_runner.run_tests ~verbose:true [sumtest; mergetest1; mergetest2];;

