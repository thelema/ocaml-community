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
 
let plan count = 
    TestBuilder.create_test_plan ~count:count ()
    
let no_plan () =
    TestBuilder.create_test_plan ();;

let diag line = 
    TestBuilder.build_diagnostic(line)

let ok ?todo test description = 
    TestBuilder.build_test_case 
        ?todo:todo  
        test 
        description;
    TestBuilder.build_diagnostic_message 
    	test 
    	description

let is ?todo (got : 'a) (expected : 'a) description =
    let test = got = expected in
    TestBuilder.build_test_case 
        ?todo:todo  
        test 
        description;
    TestBuilder.build_extended_diagnostic_message 
        test 
        got 
        expected
        description
