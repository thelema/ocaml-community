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

(** 
    A simplified AST to represent a TAP document 

    This module is used to create document trees whose output 
    conforms to the TAP protocol. Not many user-servicable 
    parts in here, so unless you are curious, you are more 
    likely to find what you are looking for in the [TestSimple]
    or [TestBuilder] docs.
    
*)

(** All tests have a status, which represents the passing or 
    failing of that particular test. *)
type status = Ok | NotOk

(** 
Tests can have directives associated with them, right now we 
only support the TODO directive. Support for SKIP may come in
future versions.
*)
type directive  = Skip of string | Todo of string

(** 
Diagnostics are bits of text which can be associated with 
either a test case itself, or within it's own diagnostic node.
*)
type diagnostic = string

(** 
These are the possible nodes of the document, they are pretty 
much self explanitory I think.
*)
type node = TestCaseNode     of status * int * string * directive option
          | DiagnosticNode   of diagnostic

(** 
The document is simply a list of nodes plus a count of how many 
tests were planned
*)
type t = { 
        nodes : node list;
        plan_count : int option;
}

(** {2 Utilities} *)

(** Functions for counting the node types. *)

val count_test_nodes : node list -> (int * int) -> (int * int)

(** Functions for creating standard footers. 
    These are called by [init_document] *)

val create_failure_footer : int -> int -> node
val create_count_footer   : int -> int -> node

(** {2 Document initializer} *)

(** constructs a document from a node list and an optional plan count *)
val init_document : ?plans:int -> node list -> t 

(** {2 String converters} *)

(** These are string conversion functions for each of the 
    types defined in this module. *)

val string_of_status     : status     -> string
val string_of_diagnostic : diagnostic -> string
val string_of_directive  : directive  -> string
val string_of_node       : node       -> string
val string_of_document   : t          -> string 
    
