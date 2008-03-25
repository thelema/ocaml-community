(*
 * TestSimple - A simple unit-testing framework or OCaml 
 * Copyright (C) 2007 Infinity Interactive, Inc.
 *
 * This module is heavily influenced by the Perl Test::Simple module.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(** 
    A simple unit-testing framework based on Perl's Test::Simple

    This is a very simple, but easy to use unit testing framework based on 
    Perl's very successful Test::* modules. It produces output using the 
    TAP protocol, which is easily read by the Perl module Test::Harness. This 
    means that OCaml tests can take advantage of many of the nice TAP based 
    tools that Perl has to offer.
    
*)

(**
    Test files written with TestSimple are basically a succession of 
    assertions written using the provided test functions. TestSimple
    will generate appropriate TAP output which can read and analyzed
    on it's own, or which can be processed using many of the TAP tools 
    that come with most modern versions of Perl.
    
    Here is an example usage of TestSimple taken from the TestSimple 
    test suite itself.
{[
    #!/usr/local/bin/ocamlrun /usr/local/bin/ocaml

    #load "testSimple.cma";;
    open TestSimple;;

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
    
]} 
    This file can then be run either from the command line as a 
    script, or run using the [prove] utility provided by Perl's 
    Test::Harness module. The [prove] command to run all the files
    in the [tests] directory would look like this:
{[
    > prove tests/*.ml
]} 
    For more information on [prove] or the TAP protocol in general see 
    the Test::Harness module documentation on CPAN here 
    [http://search.cpan.org/~petdance/Test-Harness/]
*)


(** {2 Test plans} *)

(** 
    Every test needs a test plan. A plan is basically a declaration of how 
    many tests your intend to run. In addition, declaring a plan tells the 
    framework that you are ready to start testing.
*)

val plan : int -> unit
(** 
    Simply pass [plan] the number of tests you plan to run, like so:
    {[
        plan 10;; 
    ]}
*)


val no_plan : unit -> unit
(** 
   On occasion you do not know the number of tests in advance, in such cases 
   it is possible to not specify a plan. This can be very useful during test 
   development, but since it can potentially hide errors (such as all tests
   not completing) it should not be used permanently.   
*)

(** {2 Testing functions} *)

val ok : ?todo:string -> bool -> string -> unit
(** 
    An [ok] tests is the most basic test, it is similar to [assert] in that 
    it expects a boolean value, or an expression which reduces to a boolean, 
    as it's basic test. It also expects a string for use as the test description.
    And lastly, there is the optional [?todo] parameter, this can be used when 
    you have a test which you know will fail, and you want to mark it as TODO 
    for later.
*)

val is : ?todo:string -> 'a -> 'a -> string -> unit
(** 
    This is similar to the [ok] test except that it takes two values and performs
    it's own comparison on them using [=]. The benefit of using this over doing 
    the comparison yourself with [ok] is that [is] will give a more informative 
    diagnostic message message (it uses ExtLib.dump to print the values passed)
*)

(** {2 Utilities} *)

val diag : string -> unit
(** 
    Occassionally it is useful to print out random diagnostic messages within
    your test suite, either for debugging or some other purpose. This function 
    can be used for that, and still keep your output TAP compliant.
*)


