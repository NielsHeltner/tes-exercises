#use "topfind";;
#require "qcheck";;
open QCheck;;


(* a datatype of arithmetic expressions *)
type aexp =
    | X
    | Lit of int
    | Plus of aexp * aexp
    | Times of aexp * aexp
    | Minus of aexp * aexp


let rec size aexp = match aexp with
    | X -> 1
    | Lit i -> 1
    | Plus (ae0, ae1) | Times (ae0, ae1) | Minus (ae0, ae1) -> (size ae0 + size ae1) + 1

let mytree = Plus (Lit 1, Times (X, Lit 3))

(* our interpreter of arithmetic expressions *)
let rec interpret xval ae = match ae with
    | X -> xval
    | Lit i -> i
    | Plus (ae0, ae1) ->
        let v0 = interpret xval ae0 in
        let v1 = interpret xval ae1 in
            v0 + v1
    | Times (ae0, ae1) ->
        let v0 = interpret xval ae0 in
        let v1 = interpret xval ae1 in
            v0 * v1
    | Minus (ae0, ae1) ->
        let v0 = interpret xval ae0 in
        let v1 = interpret xval ae1 in
            v0 - v1

let rec exp_to_string ae = match ae with
    | X -> "x"
    | Lit i -> string_of_int i
    | Plus (ae0, ae1) ->
        let s0 = exp_to_string ae0 in
        let s1 = exp_to_string ae1 in
            "(" ^ s0 ^ "+" ^ s1 ^ ")"
    | Times (ae0, ae1) ->
        let s0 = exp_to_string ae0 in
        let s1 = exp_to_string ae1 in
            "(" ^ s0 ^ "*" ^ s1 ^ ")"
    | Minus (ae0, ae1) ->
        let s0 = exp_to_string ae0 in
        let s1 = exp_to_string ae1 in
            "(" ^ s0 ^ "-" ^ s1 ^ ")"


(* a datatype of abstract machine instructions *)
type inst =
    | Load
    | Push of int
    | Add
    | Mult
    | Sub


(* our compiler from arithmetic expressions to instructions *)
let rec compile ae = match ae with
    | X -> [Load]
    | Lit i -> [Push i]
    | Plus (ae0, ae1) ->
        let is0 = compile ae0 in
        let is1 = compile ae1 in
            is0 @ is1 @ [Add]
    | Times (ae0, ae1) ->
        let is0 = compile ae0 in
        let is1 = compile ae1 in
            is0 @ is1 @ [Mult]
    | Minus (ae0, ae1) ->
        let is0 = compile ae0 in
        let is1 = compile ae1 in
            is0 @ is1 @ [Sub]

(* our interpreter of instructions *)
let rec run x insts stack = 
    let exception Exception of string in
    match insts with
    | [] -> (match stack with
        | [] -> raise (Exception "empty stack")
        | elem:: _ -> elem)
    | instr::ins -> match instr with
        | Load -> run x ins (x::stack)
        | Push i -> run x ins (i::stack)
        | Add -> (match stack with
            | [] -> raise (Exception "empty stack")
            | elem1::elem2::elems -> let result = elem1 + elem2 in run x ins (result::elems)
            | elem::elems -> raise (Exception "only 1 element in stack"))
        | Mult -> (match stack with
            | [] -> raise (Exception "empty stack")
            | elem1::elem2::elems -> let result = elem1 * elem2 in run x ins (result::elems)
            | elem::elems -> raise (Exception "only 1 element in stack"))
        | Sub -> (match stack with
            | [] -> raise (Exception "empty stack")
            | elem1::elem2::elems -> let result = elem2 - elem1 in run x ins (result::elems)
            | elem::elems -> raise (Exception "only 1 element in stack"))

let insts = [Push 1; Load; Push 3; Mult; Add]

(* run 2 insts [];; should result in 7 *)


(* ***Testing*** *)
let leafgen = Gen.oneof
                [Gen.return X;
                 Gen.map (fun i -> Lit i) Gen.int];;

(* a fixed-point generator with weights *)
let mygen =
  Gen.sized (Gen.fix (fun recgen n -> match n with
    | 0 -> leafgen
    | n ->
      Gen.frequency
	      [(1,leafgen);
	       (2,Gen.map2 (fun l r -> Plus(l,r)) (recgen(n/2)) (recgen(n/2)));
	       (2,Gen.map2 (fun l r -> Times(l,r)) (recgen(n/2)) (recgen(n/2)));
           (2,Gen.map2 (fun l r -> Minus(l,r)) (recgen(n/2)) (recgen(n/2))); ]))

let size_stats_test = let mygen = make ~stats:[("tree size", size)] mygen in
    Test.make ~count:10000 mygen (fun _ -> true);;

let aexptree x z = Plus (Lit x, Times (X, Lit z))
(* simple arithmetic test, that tests pre-defined operation pattern *)
let aexpinststest_static = Test.make ~name:"aexpinststest_static" ~count:100
            (triple int int int)
            (fun (x, y, z) -> interpret y (aexptree x z) = run y (compile (aexptree x z)) []);;

let arb_tree_gen = make ~print:exp_to_string mygen
(* same as aexpinststest_static, but uses a generator for the arithmetic expression *)
let aexpinststest_gen = Test.make ~name:"aexpinststest_gen" ~count:100
            (pair int arb_tree_gen)
            (fun (x, aexptree) -> interpret x aexptree = run x (compile aexptree) []);;


let int_gen = Gen.frequency [
    (8, Gen.int);
    (3, Gen.small_signed_int);
    (1, Gen.oneofl [min_int; -1; 0; 1; max_int])
];;

let int_gen_stat_test = let int_gen_stat = make ~stats:[("int gen stats", fun x -> x)] int_gen in
    Test.make ~count:10000 int_gen_stat (fun _ -> true);;



QCheck_runner.run_tests ~verbose:true [size_stats_test; aexpinststest_static; aexpinststest_gen; int_gen_stat_test];;

