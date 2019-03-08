#use "topfind";;
#require "qcheck";;
open QCheck;;

let is_even x = x mod 2 == 0;;

Printf.printf "%B\n" (is_even 2);;
Printf.printf "%B\n" (is_even 3);;

let mytest = Test.make float (fun f -> floor f <= f);;


QCheck_runner.run_tests [mytest];;