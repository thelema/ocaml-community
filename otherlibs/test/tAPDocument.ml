(*
 * TAPDocument - A simplified AST to represent a TAP document 
 * Copyright (C) 2007 Infinity Interactive, Inc.
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

type status = Ok | NotOk

type directive  = Skip of string | Todo of string
type diagnostic = string

type node = TestCaseNode     of status * int * string * directive option
          | DiagnosticNode   of diagnostic
          (* TODO?: | BailOut of string *) 

type t = { 
        nodes : node list;
        plan_count : int option;
}

let rec count_test_nodes nodes (test_c, fail_c) = 
    match nodes with
        | []                              
             -> (test_c, fail_c)
        | TestCaseNode(Ok,_,_,_)::xs 
             -> count_test_nodes xs (test_c+1, fail_c)
        | TestCaseNode(NotOk,_,_,_)::xs
             -> count_test_nodes xs (test_c+1, fail_c+1)
        | DiagnosticNode(_)::xs
             -> count_test_nodes xs (test_c, fail_c)

(* These will get automatically tagged onto the document *)

(*
    TODO:
    Convert the next two functions to use 
    Format.sprintf for constructing the 
    strings
    FIXME: isn't this the job of the test harness?
*)
let create_failure_footer test_count fail_count =
    DiagnosticNode(
        Printf.sprintf "Looks like you failed %d tests of %d run."
            fail_count test_count 
    )

let create_count_footer test_count plan_count =
    DiagnosticNode(
        if test_count < plan_count then
            Printf.sprintf "Looks like you planned %d tests but only ran %d."
            plan_count test_count
        else 
            Printf.sprintf "Looks like you planned %d tests but ran %d extra."
            plan_count (test_count - plan_count)
    )

let init_document ?plans nodes = 
    let (tests, failures) = count_test_nodes nodes (0,0) in
    let footer_nodes = 
    	(match plans with 
    	  | None -> [] 
    	  | Some count when tests = count -> [] 
    	  | Some count 
    			-> [ create_count_footer tests count ]) 
    	@ 
    	if failures > 0 
    	then [ create_failure_footer tests failures ]
    	else []
	in
    { 
      plan_count = plans;
      nodes = ExtList.List.append nodes footer_nodes;
    }

(* conversions *)

let string_of_status s =
    match s with 
        | Ok    -> "ok"
        | NotOk -> "not ok"

(*
    TODO:
    There are many chances below here for 
    using Format.sprintf
*)
let string_of_diagnostic d = d

let string_of_directive  = function 
        | Todo(expl) -> "# TODO " ^ expl 
        | Skip(expl) -> "# SKIP " ^ expl

let string_of_node node =
    match node with 
        | TestCaseNode(status, num, desc, None) ->
            Printf.sprintf "%s %d - %s\n" (string_of_status status) num desc
        | TestCaseNode(status, num, desc, Some directive) ->
            Printf.sprintf "%s %d - %s %s\n" (string_of_status status) 
                num desc (string_of_directive directive)
        | DiagnosticNode(diag) ->
            "# " ^ diag ^ "\n"

let string_of_document doc = 
	(match doc.plan_count with
		None -> ""
	  | Some plans -> Printf.sprintf "1..%d\n" plans) 
	^   
    (String.concat "" (List.map string_of_node doc.nodes))
    
