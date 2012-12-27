(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* typetexp.ml,v 1.34.4.9 2002/01/07 08:39:16 garrigue Exp *)

(* Typechecking of type expressions for the core language *)

open Asttypes
open Misc
open Parsetree
open Typedtree
open Types
open Ctype

exception Already_bound

type error =
    Unbound_type_variable of string
  | Unbound_type_constructor of Env.t * Longident.t
  | Unbound_type_constructor_2 of Path.t
  | Type_arity_mismatch of Longident.t * int * int
  | Bound_type_variable of string
  | Recursive_type
  | Unbound_row_variable of Longident.t
  | Type_mismatch of (type_expr * type_expr) list
  | Alias_type_mismatch of (type_expr * type_expr) list
  | Present_has_conjunction of string
  | Present_has_no_type of string
  | Constructor_mismatch of type_expr * type_expr
  | Not_a_variant of type_expr
  | Variant_tags of string * string
  | Invalid_variable_name of string
  | Cannot_quantify of string * type_expr
  | Multiple_constraints_on_type of Longident.t
  | Repeated_method_label of string
  | Unbound_value of Env.t * Longident.t
  | Unbound_constructor of Env.t * Longident.t
  | Unbound_label of Env.t * Longident.t
  | Unbound_module of Env.t * Longident.t
  | Unbound_class of Env.t * Longident.t
  | Unbound_modtype of Env.t * Longident.t
  | Unbound_cltype of Env.t * Longident.t
  | Ill_typed_functor_application of Longident.t

exception Error of Location.t * error

type variable_context = int * (string, type_expr) Tbl.t

(* Local definitions *)

let instance_list = Ctype.instance_list Env.empty

(* Narrowing unbound identifier errors. *)

let rec narrow_unbound_lid_error : 'a. _ -> _ -> _ -> _ -> 'a =
  fun env loc lid make_error ->
  let check_module mlid =
    try ignore (Env.lookup_module mlid env)
    with Not_found ->
      narrow_unbound_lid_error env loc mlid
        (fun env lid -> Unbound_module (env, lid))
  in
  begin match lid with
  | Longident.Lident _ -> ()
  | Longident.Ldot (mlid, _) -> check_module mlid
  | Longident.Lapply (flid, mlid) ->
      check_module flid;
      check_module mlid;
      raise (Error (loc, Ill_typed_functor_application lid))
  end;
  raise (Error (loc, make_error env lid))

let find_component lookup make_error env loc lid =
  try
    match lid with
    | Longident.Ldot (Longident.Lident "*predef*", s) ->
        lookup (Longident.Lident s) Env.initial
    | _ -> lookup lid env
  with Not_found ->
    narrow_unbound_lid_error env loc lid make_error

let find_type =
  find_component Env.lookup_type
    (fun env lid -> Unbound_type_constructor (env, lid))
let find_constructor =
  find_component Env.lookup_constructor
    (fun env lid -> Unbound_constructor (env, lid))
let find_all_constructors =
  find_component Env.lookup_all_constructors
    (fun env lid -> Unbound_constructor (env, lid))
let find_label =
  find_component Env.lookup_label
    (fun env lid -> Unbound_label (env, lid))
let find_all_labels =
  find_component Env.lookup_all_labels
    (fun env lid -> Unbound_label (env, lid))
let find_class =
  find_component Env.lookup_class
    (fun env lid -> Unbound_class (env, lid))
let find_value =
  find_component Env.lookup_value
    (fun env lid -> Unbound_value (env, lid))
let find_module =
  find_component Env.lookup_module
    (fun env lid -> Unbound_module (env, lid))
let find_modtype =
  find_component Env.lookup_modtype
    (fun env lid -> Unbound_modtype (env, lid))
let find_class_type =
  find_component Env.lookup_cltype
    (fun env lid -> Unbound_cltype (env, lid))

let unbound_constructor_error env lid =
  narrow_unbound_lid_error env lid.loc lid.txt
    (fun env lid -> Unbound_constructor (env, lid))

let unbound_label_error env lid =
  narrow_unbound_lid_error env lid.loc lid.txt
    (fun env lid -> Unbound_label (env, lid))

(* Support for first-class modules. *)

let transl_modtype_longident = ref (fun _ -> assert false)
let transl_modtype = ref (fun _ -> assert false)

let create_package_mty fake loc env (p, l) =
  let l =
    List.sort
      (fun (s1, t1) (s2, t2) ->
         if s1.txt = s2.txt then
           raise (Error (loc, Multiple_constraints_on_type s1.txt));
         compare s1 s2)
      l
  in
  l,
  List.fold_left
    (fun mty (s, t) ->
      let d = {ptype_params = [];
               ptype_cstrs = [];
               ptype_kind = Ptype_abstract;
               ptype_private = Asttypes.Public;
               ptype_manifest = if fake then None else Some t;
               ptype_variance = [];
               ptype_loc = loc} in
      {pmty_desc=Pmty_with (mty, [ { txt = s.txt; loc }, Pwith_type d ]);
       pmty_loc=loc}
    )
    {pmty_desc=Pmty_ident p; pmty_loc=loc}
    l

(* Translation of type expressions *)

let type_variables = ref (Tbl.empty : (string, type_expr) Tbl.t)
let univars        = ref ([] : (string * type_expr) list)
let pre_univars    = ref ([] : type_expr list)
let used_variables = ref (Tbl.empty : (string, type_expr * Location.t) Tbl.t)

let reset_type_variables () =
  reset_global_level ();
  type_variables := Tbl.empty

let narrow () =
  (increase_global_level (), !type_variables)

let widen (gl, tv) =
  restore_global_level gl;
  type_variables := tv

let strict_lowercase c = (c = '_' || c >= 'a' && c <= 'z')

let validate_name = function
    None -> None
  | Some name as s ->
      if name <> "" && strict_lowercase name.[0] then s else None

let new_global_var ?name () =
  new_global_var ?name:(validate_name name) ()
let newvar ?name () =
  newvar ?name:(validate_name name) ()

let enter_type_variable strict loc name =
  try
    if name <> "" && name.[0] = '_' then
      raise (Error (loc, Invalid_variable_name ("'" ^ name)));
    let v = Tbl.find name !type_variables in
    if strict then raise Already_bound;
    v
  with Not_found ->
    let v = new_global_var ~name () in
    type_variables := Tbl.add name v !type_variables;
    v

let type_variable loc name =
  try
    Tbl.find name !type_variables
  with Not_found ->
    raise(Error(loc, Unbound_type_variable ("'" ^ name)))

let wrap_method ty =
  match (Ctype.repr ty).desc with
    Tpoly _ -> ty
  | _ -> Ctype.newty (Tpoly (ty, []))

let new_pre_univar ?name () =
  let v = newvar ?name () in pre_univars := v :: !pre_univars; v

let rec swap_list = function
    x :: y :: l -> y :: x :: swap_list l
  | l -> l

type policy = Fixed | Extensible | Univars

let ctyp ctyp_desc ctyp_type ctyp_env ctyp_loc =
  { ctyp_desc; ctyp_type; ctyp_env; ctyp_loc }

let rec transl_type env policy styp =
  let loc = styp.ptyp_loc in
  match styp.ptyp_desc with
    Ptyp_any ->
      let ty =
        if policy = Univars then new_pre_univar () else
          if policy = Fixed then
            raise (Error (styp.ptyp_loc, Unbound_type_variable "_"))
          else newvar ()
      in
      ctyp Ttyp_any ty env loc
  | Ptyp_var name ->
    let ty =
      if name <> "" && name.[0] = '_' then
        raise (Error (styp.ptyp_loc, Invalid_variable_name ("'" ^ name)));
      begin try
        instance env (List.assoc name !univars)
      with Not_found -> try
        instance env (fst(Tbl.find name !used_variables))
      with Not_found ->
        let v =
          if policy = Univars then new_pre_univar ~name () else newvar ~name ()
        in
        used_variables := Tbl.add name (v, styp.ptyp_loc) !used_variables;
        v
      end
    in
    ctyp (Ttyp_var name) ty env loc
  | Ptyp_arrow(l, st1, st2) ->
    let cty1 = transl_type env policy st1 in
    let cty2 = transl_type env policy st2 in
    let ty = newty (Tarrow(l, cty1.ctyp_type, cty2.ctyp_type, Cok)) in
    ctyp (Ttyp_arrow (l, cty1, cty2)) ty env loc
  | Ptyp_tuple stl ->
    let ctys = List.map (transl_type env policy) stl in
    let ty = newty (Ttuple (List.map (fun ctyp -> ctyp.ctyp_type) ctys)) in
    ctyp (Ttyp_tuple ctys) ty env loc
  | Ptyp_constr(lid, stl) ->
      let (path, decl) = find_type env styp.ptyp_loc lid.txt in
      if List.length stl <> decl.type_arity then
        raise(Error(styp.ptyp_loc, Type_arity_mismatch(lid.txt, decl.type_arity,
                                                           List.length stl)));
      let args = List.map (transl_type env policy) stl in
      let params = instance_list decl.type_params in
      let unify_param =
        match decl.type_manifest with
          None -> unify_var
        | Some ty ->
            if (repr ty).level = Btype.generic_level then unify_var else unify
      in
      List.iter2
        (fun (sty, cty) ty' ->
           try unify_param env ty' cty.ctyp_type with Unify trace ->
             raise (Error(sty.ptyp_loc, Type_mismatch (swap_list trace))))
        (List.combine stl args) params;
      let constr =
        newconstr path (List.map (fun ctyp -> ctyp.ctyp_type) args) in
      begin try
        Ctype.enforce_constraints env constr
      with Unify trace ->
        raise (Error(styp.ptyp_loc, Type_mismatch trace))
      end;
        ctyp (Ttyp_constr (path, lid, args)) constr env loc
  | Ptyp_object fields ->
      let fields = List.map
          (fun pf ->
            let desc =
              match pf.pfield_desc with
              | Pfield_var -> Tcfield_var
              | Pfield (s,e) ->
                  let ty1 = transl_type env policy e in
                  Tcfield (s, ty1)
            in
            { field_desc = desc; field_loc = pf.pfield_loc })
          fields in
      let ty = newobj (transl_fields env policy [] fields) in
        ctyp (Ttyp_object fields) ty env loc
  | Ptyp_class(lid, stl, present) ->
      let (path, decl, is_variant) =
        try
          let (path, decl) = Env.lookup_type lid.txt env in
          let rec check decl =
            match decl.type_manifest with
              None -> raise Not_found
            | Some ty ->
                match (repr ty).desc with
                  Tvariant row when Btype.static_row row -> ()
                | Tconstr (path, _, _) ->
                    check (Env.find_type path env)
                | _ -> raise Not_found
          in check decl;
          Location.prerr_warning styp.ptyp_loc Warnings.Deprecated;
          (path, decl,true)
        with Not_found -> try
          if present <> [] then raise Not_found;
          let lid2 =
            match lid.txt with
              Longident.Lident s     -> Longident.Lident ("#" ^ s)
            | Longident.Ldot(r, s)   -> Longident.Ldot (r, "#" ^ s)
            | Longident.Lapply(_, _) -> fatal_error "Typetexp.transl_type"
          in
          let (path, decl) = Env.lookup_type lid2 env in
          (path, decl, false)
        with Not_found ->
          raise(Error(styp.ptyp_loc, Unbound_class (env, lid.txt)))
      in
      if List.length stl <> decl.type_arity then
        raise(Error(styp.ptyp_loc, Type_arity_mismatch(lid.txt, decl.type_arity,
                                                       List.length stl)));
      let args = List.map (transl_type env policy) stl in
      let params = instance_list decl.type_params in
      List.iter2
        (fun (sty, cty) ty' ->
           try unify_var env ty' cty.ctyp_type with Unify trace ->
             raise (Error(sty.ptyp_loc, Type_mismatch (swap_list trace))))
        (List.combine stl args) params;
        let ty_args = List.map (fun ctyp -> ctyp.ctyp_type) args in
      let ty =
        try Ctype.expand_head env (newconstr path ty_args)
        with Unify trace ->
          raise (Error(styp.ptyp_loc, Type_mismatch trace))
      in
      let ty = match ty.desc with
        Tvariant row ->
          let row = Btype.row_repr row in
          List.iter
            (fun l -> if not (List.mem_assoc l row.row_fields) then
              raise(Error(styp.ptyp_loc, Present_has_no_type l)))
            present;
          let fields =
            List.map
              (fun (l,f) -> l,
                if List.mem l present then f else
                match Btype.row_field_repr f with
                | Rpresent (Some ty) ->
                    Reither(false, [ty], false, ref None)
                | Rpresent None ->
                    Reither (true, [], false, ref None)
                | _ -> f)
              row.row_fields
          in
          let row = { row_closed = true; row_fields = fields;
                      row_bound = (); row_name = Some (path, ty_args);
                      row_fixed = false; row_more = newvar () } in
          let static = Btype.static_row row in
          let row =
            if static then { row with row_more = newty Tnil }
            else if policy <> Univars then row
            else { row with row_more = new_pre_univar () }
          in
          newty (Tvariant row)
      | Tobject (fi, _) ->
          let _, tv = flatten_fields fi in
          if policy = Univars then pre_univars := tv :: !pre_univars;
          ty
      | _ ->
          assert false
      in
        ctyp (Ttyp_class (path, lid, args, present)) ty env loc
  | Ptyp_alias(st, alias) ->
      let cty =
        try
          let t =
            try List.assoc alias !univars
            with Not_found ->
              instance env (fst(Tbl.find alias !used_variables))
          in
          let ty = transl_type env policy st in
          begin try unify_var env t ty.ctyp_type with Unify trace ->
            let trace = swap_list trace in
            raise(Error(styp.ptyp_loc, Alias_type_mismatch trace))
          end;
          ty
        with Not_found ->
          if !Clflags.principal then begin_def ();
          let t = newvar () in
          used_variables := Tbl.add alias (t, styp.ptyp_loc) !used_variables;
          let ty = transl_type env policy st in
          begin try unify_var env t ty.ctyp_type with Unify trace ->
            let trace = swap_list trace in
            raise(Error(styp.ptyp_loc, Alias_type_mismatch trace))
          end;
          if !Clflags.principal then begin
            end_def ();
            generalize_structure t;
          end;
          let t = instance env t in
          let px = Btype.proxy t in
          begin match px.desc with
          | Tvar None -> Btype.log_type px; px.desc <- Tvar (Some alias)
          | Tunivar None -> Btype.log_type px; px.desc <- Tunivar (Some alias)
          | _ -> ()
          end;
          { ty with ctyp_type = t }
      in
      ctyp (Ttyp_alias (cty, alias)) cty.ctyp_type env loc
  | Ptyp_variant(fields, closed, present) ->
      let name = ref None in
      let mkfield l f =
        newty (Tvariant {row_fields=[l,f]; row_more=newvar();
                         row_bound=(); row_closed=true;
                         row_fixed=false; row_name=None}) in
      let hfields = Hashtbl.create 17 in
      let add_typed_field loc l f =
        let h = Btype.hash_variant l in
        try
          let (l',f') = Hashtbl.find hfields h in
          (* Check for tag conflicts *)
          if l <> l' then raise(Error(styp.ptyp_loc, Variant_tags(l, l')));
          let ty = mkfield l f and ty' = mkfield l f' in
          if equal env false [ty] [ty'] then () else
          try unify env ty ty'
          with Unify trace -> raise(Error(loc, Constructor_mismatch (ty,ty')))
        with Not_found ->
          Hashtbl.add hfields h (l,f)
      in
      let add_field = function
          Rtag (l, c, stl) ->
            name := None;
            let tl = List.map (transl_type env policy) stl in
            let f = match present with
              Some present when not (List.mem l present) ->
                let ty_tl = List.map (fun cty -> cty.ctyp_type) tl in
                Reither(c, ty_tl, false, ref None)
            | _ ->
                if List.length stl > 1 || c && stl <> [] then
                  raise(Error(styp.ptyp_loc, Present_has_conjunction l));
                match tl with [] -> Rpresent None
                | st :: _ ->
                      Rpresent (Some st.ctyp_type)
            in
            add_typed_field styp.ptyp_loc l f;
              Ttag (l,c,tl)
        | Rinherit sty ->
            let cty = transl_type env policy sty in
            let ty = cty.ctyp_type in
            let nm =
              match repr cty.ctyp_type with
                {desc=Tconstr(p, tl, _)} -> Some(p, tl)
              | _                        -> None
            in
            begin try
              (* Set name if there are no fields yet *)
              Hashtbl.iter (fun _ _ -> raise Exit) hfields;
              name := nm
            with Exit ->
              (* Unset it otherwise *)
              name := None
            end;
            let fl = match expand_head env cty.ctyp_type, nm with
              {desc=Tvariant row}, _ when Btype.static_row row ->
                let row = Btype.row_repr row in
                row.row_fields
            | {desc=Tvar _}, Some(p, _) ->
                raise(Error(sty.ptyp_loc, Unbound_type_constructor_2 p))
            | _ ->
                raise(Error(sty.ptyp_loc, Not_a_variant ty))
            in
            List.iter
              (fun (l, f) ->
                let f = match present with
                  Some present when not (List.mem l present) ->
                    begin match f with
                      Rpresent(Some ty) ->
                        Reither(false, [ty], false, ref None)
                    | Rpresent None ->
                        Reither(true, [], false, ref None)
                    | _ ->
                        assert false
                    end
                | _ -> f
                in
                add_typed_field sty.ptyp_loc l f)
              fl;
              Tinherit cty
      in
      let tfields = List.map add_field fields in
      let fields = Hashtbl.fold (fun _ p l -> p :: l) hfields [] in
      begin match present with None -> ()
      | Some present ->
          List.iter
            (fun l -> if not (List.mem_assoc l fields) then
              raise(Error(styp.ptyp_loc, Present_has_no_type l)))
            present
      end;
      let row =
        { row_fields = List.rev fields; row_more = newvar ();
          row_bound = (); row_closed = closed;
          row_fixed = false; row_name = !name } in
      let static = Btype.static_row row in
      let row =
        if static then { row with row_more = newty Tnil }
        else if policy <> Univars then row
        else { row with row_more = new_pre_univar () }
      in
      let ty = newty (Tvariant row) in
      ctyp (Ttyp_variant (tfields, closed, present)) ty env loc
   | Ptyp_poly(vars, st) ->
      begin_def();
      let new_univars = List.map (fun name -> name, newvar ~name ()) vars in
      let old_univars = !univars in
      univars := new_univars @ !univars;
      let cty = transl_type env policy st in
      let ty = cty.ctyp_type in
      univars := old_univars;
      end_def();
      generalize ty;
      let ty_list =
        List.fold_left
          (fun tyl (name, ty1) ->
            let v = Btype.proxy ty1 in
            if deep_occur v ty then begin
              match v.desc with
                Tvar name when v.level = Btype.generic_level ->
                  v.desc <- Tunivar name;
                  v :: tyl
              | _ ->
                raise (Error (styp.ptyp_loc, Cannot_quantify (name, v)))
            end else tyl)
          [] new_univars
      in
      let ty' = Btype.newgenty (Tpoly(ty, List.rev ty_list)) in
      unify_var env (newvar()) ty';
      ctyp (Ttyp_poly (vars, cty)) ty' env loc
  | Ptyp_package (p, l) ->
      let l, mty = create_package_mty true styp.ptyp_loc env (p, l) in
      let z = narrow () in
      let mty = !transl_modtype env mty in
      widen z;
      let ptys = List.map (fun (s, pty) ->
                             s, transl_type env policy pty
                          ) l in
      let path = !transl_modtype_longident styp.ptyp_loc env p.txt in
      let ty = newty (Tpackage (path,
                       List.map (fun (s, pty) -> s.txt) l,
                       List.map (fun (_,cty) -> cty.ctyp_type) ptys))
      in
        ctyp (Ttyp_package {
                pack_name = path;
                pack_type = mty.mty_type;
                pack_fields = ptys;
                pack_txt = p;
              }) ty env loc

and transl_fields env policy seen =
  function
    [] ->
      newty Tnil
  | {field_desc = Tcfield_var}::_ ->
      if policy = Univars then new_pre_univar () else newvar ()
  | {field_desc = Tcfield(s, ty1); field_loc = loc}::l ->
      if List.mem s seen then  raise (Error (loc, Repeated_method_label s));
      let ty2 = transl_fields env policy (s::seen) l in
        newty (Tfield (s, Fpresent, ty1.ctyp_type, ty2))

(* Make the rows "fixed" in this type, to make universal check easier *)
let rec make_fixed_univars ty =
  let ty = repr ty in
  if ty.level >= Btype.lowest_level then begin
    Btype.mark_type_node ty;
    match ty.desc with
    | Tvariant row ->
        let row = Btype.row_repr row in
        if Btype.is_Tunivar (Btype.row_more row) then
          ty.desc <- Tvariant
              {row with row_fixed=true;
               row_fields = List.map
                 (fun (s,f as p) -> match Btype.row_field_repr f with
                   Reither (c, tl, m, r) -> s, Reither (c, tl, true, r)
                 | _ -> p)
                 row.row_fields};
        Btype.iter_row make_fixed_univars row
    | _ ->
        Btype.iter_type_expr make_fixed_univars ty
  end

let make_fixed_univars ty =
  make_fixed_univars ty;
  Btype.unmark_type ty

let create_package_mty = create_package_mty false

let globalize_used_variables env fixed =
  let r = ref [] in
  Tbl.iter
    (fun name (ty, loc) ->
      let v = new_global_var () in
      let snap = Btype.snapshot () in
      if try unify env v ty; true with _ -> Btype.backtrack snap; false
      then try
        r := (loc, v,  Tbl.find name !type_variables) :: !r
      with Not_found ->
        if fixed && Btype.is_Tvar (repr ty) then
          raise(Error(loc, Unbound_type_variable ("'"^name)));
        let v2 = new_global_var () in
        r := (loc, v, v2) :: !r;
        type_variables := Tbl.add name v2 !type_variables)
    !used_variables;
  used_variables := Tbl.empty;
  fun () ->
    List.iter
      (function (loc, t1, t2) ->
        try unify env t1 t2 with Unify trace ->
          raise (Error(loc, Type_mismatch trace)))
      !r

let transl_simple_type env fixed styp =
  univars := []; used_variables := Tbl.empty;
  let typ = transl_type env (if fixed then Fixed else Extensible) styp in
  globalize_used_variables env fixed ();
  make_fixed_univars typ.ctyp_type;
  typ

let transl_simple_type_univars env styp =
  univars := []; used_variables := Tbl.empty; pre_univars := [];
  begin_def ();
  let typ = transl_type env Univars styp in
  (* Only keep already global variables in used_variables *)
  let new_variables = !used_variables in
  used_variables := Tbl.empty;
  Tbl.iter
    (fun name p ->
      if Tbl.mem name !type_variables then
        used_variables := Tbl.add name p !used_variables)
    new_variables;
  globalize_used_variables env false ();
  end_def ();
  generalize typ.ctyp_type;
  let univs =
    List.fold_left
      (fun acc v ->
        let v = repr v in
        match v.desc with
          Tvar name when v.level = Btype.generic_level ->
            v.desc <- Tunivar name; v :: acc
        | _ -> acc)
      [] !pre_univars
  in
  make_fixed_univars typ.ctyp_type;
    { typ with ctyp_type =
        instance env (Btype.newgenty (Tpoly (typ.ctyp_type, univs))) }

let transl_simple_type_delayed env styp =
  univars := []; used_variables := Tbl.empty;
  let typ = transl_type env Extensible styp in
  make_fixed_univars typ.ctyp_type;
  (typ, globalize_used_variables env false)

let transl_type_scheme env styp =
  reset_type_variables();
  begin_def();
  let typ = transl_simple_type env false styp in
  end_def();
  generalize typ.ctyp_type;
  typ


(* Error report *)

open Format
open Printtyp

let spellcheck ppf fold env lid =
  let cutoff =
    match String.length (Longident.last lid) with
      | 1 | 2 -> 0
      | 3 | 4 -> 1
      | 5 | 6 -> 2
      | _ -> 3
  in
  let compare target head acc =
    let (best_choice, best_dist) = acc in
    match Misc.edit_distance target head cutoff with
      | None -> (best_choice, best_dist)
      | Some dist ->
        let choice =
          if dist < best_dist then [head]
          else if dist = best_dist then head :: best_choice
          else best_choice in
        (choice, min dist best_dist)
  in
  let init = ([], max_int) in
  let handle (choice, _dist) =
    match List.rev choice with
      | [] -> ()
      | last :: rev_rest ->
        fprintf ppf "@\nDid you mean %s%s%s?"
          (String.concat ", " (List.rev rev_rest))
          (if rev_rest = [] then "" else " or ")
          last
  in
  (* flush now to get the error report early, in the (unheard of) case
     where the linear search would take a bit of time; in the worst
     case, the user has seen the error, she can interrupt the process
     before the spell-checking terminates. *)
  fprintf ppf "@?";
  match lid with
    | Longident.Lapply _ -> ()
    | Longident.Lident s ->
      handle (fold (compare s) None env init)
    | Longident.Ldot (r, s) ->
      handle (fold (compare s) (Some r) env init)

let spellcheck_simple ppf fold extr =
  spellcheck ppf (fun f -> fold (fun decl x -> f (extr decl) x))

let spellcheck ppf fold =
  spellcheck ppf (fun f -> fold (fun s _ _ x -> f s x))

type cd = string list * int

let report_error ppf = function
  | Unbound_type_variable name ->
    fprintf ppf "Unbound type parameter %s@." name
  | Unbound_type_constructor (env, lid) ->
    fprintf ppf "Unbound type constructor %a" longident lid;
    spellcheck ppf Env.fold_types env lid;
  | Unbound_type_constructor_2 p ->
    fprintf ppf "The type constructor@ %a@ is not yet completely defined"
      path p
  | Type_arity_mismatch(lid, expected, provided) ->
    fprintf ppf
      "@[The type constructor %a@ expects %i argument(s),@ \
        but is here applied to %i argument(s)@]"
      longident lid expected provided
  | Bound_type_variable name ->
    fprintf ppf "Already bound type parameter '%s" name
  | Recursive_type ->
    fprintf ppf "This type is recursive"
  | Unbound_row_variable lid ->
      (* we don't use "spellcheck" here: this error is not raised
         anywhere so it's unclear how it should be handled *)
      fprintf ppf "Unbound row variable in #%a" longident lid
  | Type_mismatch trace ->
      Printtyp.unification_error true trace
        (function ppf ->
           fprintf ppf "This type")
        ppf
        (function ppf ->
           fprintf ppf "should be an instance of type")
  | Alias_type_mismatch trace ->
      Printtyp.unification_error true trace
        (function ppf ->
           fprintf ppf "This alias is bound to type")
        ppf
        (function ppf ->
           fprintf ppf "but is used as an instance of type")
  | Present_has_conjunction l ->
      fprintf ppf "The present constructor %s has a conjunctive type" l
  | Present_has_no_type l ->
      fprintf ppf "The present constructor %s has no type" l
  | Constructor_mismatch (ty, ty') ->
      Printtyp.reset_and_mark_loops_list [ty; ty'];
      fprintf ppf "@[<hov>%s %a@ %s@ %a@]"
        "This variant type contains a constructor"
        Printtyp.type_expr ty
        "which should be"
        Printtyp.type_expr ty'
  | Not_a_variant ty ->
      Printtyp.reset_and_mark_loops ty;
      fprintf ppf "@[The type %a@ is not a polymorphic variant type@]"
        Printtyp.type_expr ty
  | Variant_tags (lab1, lab2) ->
      fprintf ppf
        "@[Variant tags `%s@ and `%s have the same hash value.@ %s@]"
        lab1 lab2 "Change one of them."
  | Invalid_variable_name name ->
      fprintf ppf "The type variable name %s is not allowed in programs" name
  | Cannot_quantify (name, v) ->
      fprintf ppf
        "@[<hov>The universal type variable '%s cannot be generalized:@ %s.@]"
        name
        (if Btype.is_Tvar v then "it escapes its scope" else
         if Btype.is_Tunivar v then "it is already bound to another variable"
         else "it is not a variable")
  | Multiple_constraints_on_type s ->
      fprintf ppf "Multiple constraints for type %a" longident s
  | Repeated_method_label s ->
      fprintf ppf "@[This is the second method `%s' of this object type.@ %s@]"
        s "Multiple occurences are not allowed."
  | Unbound_value (env, lid) ->
      fprintf ppf "Unbound value %a" longident lid;
      spellcheck ppf Env.fold_values env lid;
  | Unbound_module (env, lid) ->
      fprintf ppf "Unbound module %a" longident lid;
      spellcheck ppf Env.fold_modules env lid;
  | Unbound_constructor (env, lid) ->
      fprintf ppf "Unbound constructor %a" longident lid;
      spellcheck_simple ppf Env.fold_constructors (fun d -> d.cstr_name)
	env lid;
  | Unbound_label (env, lid) ->
      fprintf ppf "Unbound record field %a" longident lid;
      spellcheck_simple ppf Env.fold_labels (fun d -> d.lbl_name) env lid;
  | Unbound_class (env, lid) ->
      fprintf ppf "Unbound class %a" longident lid;
      spellcheck ppf Env.fold_classs env lid;
  | Unbound_modtype (env, lid) ->
      fprintf ppf "Unbound module type %a" longident lid;
      spellcheck ppf Env.fold_modtypes env lid;
  | Unbound_cltype (env, lid) ->
      fprintf ppf "Unbound class type %a" longident lid;
      spellcheck ppf Env.fold_cltypes env lid;
  | Ill_typed_functor_application lid ->
      fprintf ppf "Ill-typed functor application %a" longident lid
