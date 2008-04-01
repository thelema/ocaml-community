(*
 * TestBuilder - A module for building test functions 
 * Copyright (C) 2007 Infinity Interactive, Inc.
 *
 * This module is heavily influenced by the Perl Test::Builder module.
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
    A module for building test functions
    
    If you want to build customized testing functions which 
    can easily comingle with TestSimple's testing functions
    you need to use this module to build those functions with. 
    
    The best documentation for this is probably the source 
    code for TestSimple itself. 
*)

(** This initalizes the test plan. *)
val create_test_plan : ?count:int -> unit -> unit

(** This builds a simple tests case. *)
val build_test_case : ?todo:string -> bool -> string -> unit

(** Utility functions to handle building diagnostic nodes *)
val build_diagnostic  : string      -> unit
val build_diagnostics : string list -> unit

(** This builds an extended diagnostic message, 
    showing the values gotten and the 
    values that were expected. *)
val build_extended_diagnostic_message : bool -> 'a -> 'a -> string -> unit
(** This function uses ExtLib.dump to display
    the got and expected values. *)

(** this builds a simpler diagnostic message *)
val build_diagnostic_message : bool -> string -> unit

(** {2 Testing the Test framework} *)

(** 
    This module can be used to test the ouput of tests 
    which are built with TestBuilder. This module is 
    actually used in the TestSimple test suite to test
    itself.
*)
module Tester :
sig
    (** This replaces [TestSimple.plan] but returns 
        a string rather then printing to stdout *)
    val plan : int -> (unit -> unit) -> string

    (** This replaces [TestSimple.no_plan] but returns 
        a string rather then printing to stdout *)
    val no_plan : (unit -> unit) -> string
end




