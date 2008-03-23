#!/usr/local/bin/ocamlrun /usr/local/bin/ocaml

#load "testSimple.cma";;

open TestSimple;;

(*

Test adding the ~todo: option to a test

*)

plan 2;;

(* One test run one test expected *)

is (TestBuilder.Tester.plan 1 (fun () ->
        (ok false "... ok passed" ~todo:"Not implemented yet");
    ))
"1..1
not ok 1 - ... ok passed # TODO Not implemented yet
# Failed test '... ok passed'
# in t/004_todo_tests.ml
# Looks like you failed 1 tests of 1 run.
" 
"... got the output we expected";;

is (TestBuilder.Tester.plan 1 (fun () ->
        (ok true "... ok passed" ~todo:"Not implemented yet");
    ))
"1..1
ok 1 - ... ok passed # TODO Not implemented yet
" 
"... got the output we expected";;
