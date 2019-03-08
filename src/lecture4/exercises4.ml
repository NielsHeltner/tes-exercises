#use "topfind";;
#require "qcheck";;
open QCheck;;
(*#directory "_build"
#load "_build/dict.cmo"
open Dict;;*)
module Dict: 
    sig
        type key = string
        type value = int
        type dictionary (*= key -> value*)

        val empty : dictionary
        val add : dictionary -> key -> value -> dictionary
        val find : dictionary -> key -> value
    end
    = struct
        type key = string
        type value = int
        type dictionary = key -> value

        let empty key = 0
        let add dict key v =
                fun key' -> if key=key' then v else dict key'
        let find dict key = dict key
    end
open Dict;;

(*print_int (find empty "foo")*)

let d = empty
let d = add d "hi" 1
let d = add d "hej" 2;;
find d "hi";;
find d "hej";;



let myshr i = 
            if i == 0 then
                Iter.empty
            else
                Iter.return (int_of_float (float_of_int i *. 0.99))
let t = Test.make (set_shrink myshr int) (fun i -> i < 432);;
QCheck_runner.run_tests ~verbose:true [t];;

