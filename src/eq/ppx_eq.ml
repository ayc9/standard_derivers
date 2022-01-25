(* Generated code should depend on the environment in scope as little as possible. 
   E.g. rather than [foo = []] do [match foo with [] ->], to eliminate the use of [=]. 
   It is especially important to not use polymorphic comparisons, since we are moving 
   more and more to code that doesn't have them in scope. *)

(* Note: I am introducing a few unnecessary explicit closures, (not all of them but 
   some are unnecessary due to the value restriction). *)

open Base
open Ppxlib 
open Ast_builder.Default

let result_type ~loc = [%type: bool]

let type_ ~hide ~loc ty =
  let loc = { loc with loc_ghost = true } in
  let ptyp_attributes =
    if hide
    then Merlin_helpers.hide_attribute :: ty.ptyp_attributes
    else ty.ptyp_attributes
  in
  let hty = { ty with ptyp_attributes } in
  [%type: [%t ty] -> [%t hty] -> [%t result_type ~loc]]

let sig_type_decl ~ctxt (_rec_flag, tds) =
  let hide = not (Expansion_context.Deriver.inline ctxt) in
  let tds = List.map tds ~f:name_type_params_in_td in
  List.map tds ~f:(fun td ->
    let compare_of = combinator_type_of_type_declaration td ~f:(type_ ~hide) in
    let name = match td.ptype_name.txt with 
      | "t" -> "equal"
      | s -> "equal" ^ "_" ^ s
    in
    let loc = td.ptype_loc in
    psig_value ~loc (value_description ~loc ~name:{ td.ptype_name with txt = name }
                        ~type_:compare_of ~prim:[]))
  ;;
Deriving.add "equal" ~sig_type_decl:(Deriving.Generator.V2.make_noarg sig_type_decl)
;;
