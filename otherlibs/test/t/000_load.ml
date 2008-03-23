#!/usr/local/bin/ocamlrun /usr/local/bin/ocaml

#load "testSimple.cma";;

open TestSimple;;

(*

This tests basic functions from TestSimple

diag (output diagnostics)
ok (simple boolean tests)
is (polymorphic comparsions)
    
*)

plan 9;;

diag "... testing O'Caml TestSimple v0.01 ";;
ok true "... ok passed";;
is 2 2 "... is <int> <int> passed";;
is 2. 2. "... is <float> <float> passed";;
is "foo" "foo" "... is <string> <string> passed";;
is [] [] "... is <'a list> <'a list> passed";;
is [1;2;3] [1;2;3] "... is <int list> <int list> passed";; 
is ["foo";"bar"] ["foo";"bar"] "... is <string list> <string list> passed";;
is (1,"foo") (1,"foo") "... is <int * string> <int * string> passed";;
is TAPDocument.Ok TAPDocument.Ok "... is <type> <type> passed";;

