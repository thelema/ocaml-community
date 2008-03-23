#!/usr/local/bin/ocamlrun /usr/local/bin/ocaml

#load "testSimple.cma";;

open TestSimple;;

(*

This tests basic error outputs
    
*)

plan 7;;

is (TestBuilder.Tester.plan 1 (fun () ->
        (ok false "... ok passed");
    ))
"1..1
not ok 1 - ... ok passed
# Failed test '... ok passed'
# in t/003_error_output.ml
# Looks like you failed 1 tests of 1 run.
" 
"... got the output we expected";;

is (TestBuilder.Tester.plan 1 (fun () ->
        (is 1 2 "... ok passed");
    ))
"1..1
not ok 1 - ... ok passed
# Failed test '... ok passed'
# in t/003_error_output.ml
#      got: 1
# expected: 2
# Looks like you failed 1 tests of 1 run.
" 
"... got the output we expected";;

is (TestBuilder.Tester.plan 1 (fun () ->
        (is 1. 2. "... ok passed");
    ))
"1..1
not ok 1 - ... ok passed
# Failed test '... ok passed'
# in t/003_error_output.ml
#      got: 1.
# expected: 2.
# Looks like you failed 1 tests of 1 run.
" 
"... got the output we expected";;

is (TestBuilder.Tester.plan 1 (fun () ->
        (is "foo" "bar" "... ok passed");
    ))
"1..1
not ok 1 - ... ok passed
# Failed test '... ok passed'
# in t/003_error_output.ml
#      got: \"foo\"
# expected: \"bar\"
# Looks like you failed 1 tests of 1 run.
" 
"... got the output we expected";;

is (TestBuilder.Tester.plan 1 (fun () ->
        (is ["foo";"bar"] ["bar";"foo"] "... ok passed");
    ))
"1..1
not ok 1 - ... ok passed
# Failed test '... ok passed'
# in t/003_error_output.ml
#      got: [\"foo\"; \"bar\"]
# expected: [\"bar\"; \"foo\"]
# Looks like you failed 1 tests of 1 run.
" 
"... got the output we expected";;

is (TestBuilder.Tester.plan 1 (fun () ->
        (is (1, "foo") (1, "bar") "... ok passed");
    ))
"1..1
not ok 1 - ... ok passed
# Failed test '... ok passed'
# in t/003_error_output.ml
#      got: (1, \"foo\")
# expected: (1, \"bar\")
# Looks like you failed 1 tests of 1 run.
" 
"... got the output we expected";;

(* 
    
NOTE:
OCaml does not save much of any type information, so 
this is all we have left to see, this is what ExtLib.dump 
shows us. 

*)

is (TestBuilder.Tester.plan 1 (fun () ->
        (is TAPDocument.Ok TAPDocument.NotOk "... ok passed");
    ))
"1..1
not ok 1 - ... ok passed
# Failed test '... ok passed'
# in t/003_error_output.ml
#      got: 0
# expected: 1
# Looks like you failed 1 tests of 1 run.
" 
"... got the output we expected";;



