#!/usr/local/bin/ocamlrun /usr/local/bin/ocaml

#load "testSimple.cma";;

open TestSimple;;

(*

This tests the various possible types of footers
that can be generated for tests.

*)

plan 8;;

(* One test run one test expected *)

is (TestBuilder.Tester.plan 1 (fun () ->
        (ok true "... ok passed");
    ))
"1..1
ok 1 - ... ok passed
" 
"... got the output we expected";;

(* One test run two tests expected *)

is (TestBuilder.Tester.plan 2 (fun () ->
        (ok true "... ok passed");
    ))
"1..2
ok 1 - ... ok passed
# Looks like you planned 2 tests but only ran 1.
" 
"... got the output we expected";;

(* Two tests run one test expected *)

is (TestBuilder.Tester.plan 1 (fun () ->
        (ok true "... ok passed");
        (ok true "... ok passed (again)");        
    ))
"1..1
ok 1 - ... ok passed
ok 2 - ... ok passed (again)
# Looks like you planned 1 tests but ran 1 extra.
" 
"... got the output we expected";;

(* One test failed one test expected *)

is (TestBuilder.Tester.plan 1 (fun () ->
        (ok false "... ok passed");
    ))
"1..1
not ok 1 - ... ok passed
# Failed test '... ok passed'
# in t/002_test_footer.ml
# Looks like you failed 1 tests of 1 run.
" 
"... got the output we expected";;

(* One test failed two tests expected *)

is (TestBuilder.Tester.plan 2 (fun () ->
        (ok false "... ok passed");
    ))
"1..2
not ok 1 - ... ok passed
# Failed test '... ok passed'
# in t/002_test_footer.ml
# Looks like you planned 2 tests but only ran 1.
# Looks like you failed 1 tests of 1 run.
" 
"... got the output we expected";;

(* One test pass, one test failed one test expected *)

is (TestBuilder.Tester.plan 1 (fun () ->
        (ok true "... ok passed");
        (ok false "... ok passed");
    ))
"1..1
ok 1 - ... ok passed
not ok 2 - ... ok passed
# Failed test '... ok passed'
# in t/002_test_footer.ml
# Looks like you planned 1 tests but ran 1 extra.
# Looks like you failed 1 tests of 2 run.
" 
"... got the output we expected";;

(* test passing w/out plan *)

is (TestBuilder.Tester.no_plan (fun () ->
        (ok true "... ok passed");
    ))
"ok 1 - ... ok passed
1..1
" 
"... got the output we expected";;

(* test failing w/out plan *) 

is (TestBuilder.Tester.no_plan (fun () ->
        (ok false "... ok passed");
    ))
"not ok 1 - ... ok passed
# Failed test '... ok passed'
# in t/002_test_footer.ml
1..1
# Looks like you failed 1 tests of 1 run.
" 
"... got the output we expected";;
