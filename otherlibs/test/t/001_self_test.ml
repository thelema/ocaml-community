#!/usr/local/bin/ocamlrun /usr/local/bin/ocaml

#load "testSimple.cma";;

open TestSimple;;

plan 1;;

(* Just check the output of our 000_load test *)

(is 
    (TestBuilder.Tester.plan 9 (fun () ->
        diag "... testing O'Caml TestSimple v 0.01";
        ok true "... ok passed";
        is 2 2 "... is <int> <int> passed";    
        is 2. 2. "... is <float> <float> passed";        
        is "foo" "foo" "... is <string> <string> passed"; 
        is [] [] "... is <'a list> <'a list> passed";   
        is [1;2;3] [1;2;3] "... is <int list> <int list> passed"; 
        is ["foo";"bar"] ["foo";"bar"] "... is <string list> <string list> passed";
        is (1,"foo") (1,"foo") "... is <int * string> <int * string> passed";    
        is TAPDocument.Ok TAPDocument.Ok "... is <type> <type> passed";             
    ))
"1..9
# ... testing O'Caml TestSimple v 0.01
ok 1 - ... ok passed
ok 2 - ... is <int> <int> passed
ok 3 - ... is <float> <float> passed
ok 4 - ... is <string> <string> passed
ok 5 - ... is <'a list> <'a list> passed
ok 6 - ... is <int list> <int list> passed
ok 7 - ... is <string list> <string list> passed
ok 8 - ... is <int * string> <int * string> passed
ok 9 - ... is <type> <type> passed
" 
"... got the output we expected");;













