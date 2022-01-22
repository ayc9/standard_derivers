(* Generated code should depend on the environment in scope as little as possible. 
   E.g. rather than [foo = []] do [match foo with [] ->], to eliminate the use of [=]. 
   It is especially important to not use polymorphic comparisons, since we are moving 
   more and more to code that doesn't have them in scope. *)

(* Note: I am introducing a few unnecessary explicit closures, (not all of them but 
   some are unnecessary due to the value restriction). *)

open Base
open Ppxlib 
open Ast_builder.Default

(* Everything below is from expander.ml *)

type kind = Equal | Compare

(* Attrs from expander_intf *)
(* Both eq and compare *)
module type Attrs = sig
  val ignore_label_declaration : (label_declaration, unit) Attribute.t
  val ignore_core_type : (core_type, unit) Attribute.t
end

(* Both eq and compare *)
module type Params = sig
  val name : string
  val kind : kind
  val chain : expression -> expression -> expression
  val const : loc:Location.t -> Ordering.t -> expression
  val result_type : loc:Location.t -> core_type
  val poly : loc:Location.t -> expression -> expression -> expression
  val abstract
    :  loc:Location.t
    -> type_name:string
    -> expression
    -> expression
    -> expression
  module Attrs : Attrs
end

(* Both eq and compare *)
module Make_attrs(Name : sig val name : string end) : Attrs = struct
  (* Only for the .ignore option: *)
  let ignore_label_declaration =
    Attribute.declare (Name.name ^ ".ignore")
      Attribute.Context.label_declaration
      Ast_pattern.(pstr nil)
      ()
  let ignore_core_type =
    Attribute.declare (Name.name ^ ".ignore")
      Attribute.Context.core_type
      Ast_pattern.(pstr nil)
      ()
end

module Equal_params : Params = struct
  (* As is from JS *)
  let name = "equal"
  let kind = Equal

  (* what is chain? what are these % extensions doing? *)
  let chain a b =
    let loc = a.pexp_loc in
    [%expr
      Ppx_compare_lib.(&&) [%e a] [%e b]
    ]

  let const ~loc (ord : Ordering.t) =
    match ord with
    | Equal -> [%expr true]
    | Less | Greater -> [%expr false]
  let result_type ~loc = [%type: bool]

  (* polymorphic is its own case *)
  let poly ~loc a b = [%expr Ppx_compare_lib.polymorphic_equal [%e a] [%e b]]
  (* what is an abstract equal *)
  let abstract ~loc ~type_name a b =
    [%expr Ppx_compare_lib.equal_abstract ~type_name:[%e estring ~loc type_name]
             [%e a] [%e b]]

  module Attrs = Make_attrs(struct let name = name end)
end

module Compare_params : Params = struct
  let name = "compare"
  let kind = Compare

  let chain a b =
    let loc = a.pexp_loc in
    [%expr
      match [%e a] with
      | 0 -> [%e b]
      | n -> n
    ]
(* difference with Equal.chain? *)
  let const ~loc (ord : Ordering.t) =
    eint ~loc (match ord with
      | Less -> -1
      | Equal -> 0
      | Greater -> 1)
  let result_type ~loc = [%type: int]
  (* poly and absract same as Equal *)
  let poly ~loc a b = [%expr Ppx_compare_lib.polymorphic_compare [%e a] [%e b]]
  let abstract ~loc ~type_name a b =
    [%expr Ppx_compare_lib.compare_abstract ~type_name:[%e estring ~loc type_name]
             [%e a] [%e b]]

  module Attrs = Make_attrs(struct let name = name end)
end

module Make(Params : Params) = struct
  open Params
  module Attrs = Attrs

  (* what do we mean by attribute!! *)
  (* why not `None ()`? *)
  let is_ignored_gen ~loc ~compare_attr ~equal_attr ast =
    match kind, Attribute.get compare_attr ast, Attribute.get equal_attr ast with
    | _, Some (), Some ()
    | Compare, Some (), None
    | Equal, None, Some () -> true 
    (* three cases above, ignored is generated *)

    | _, None, None -> false

    | Compare, None, Some () ->
      Location.raise_errorf ~loc "Cannot use [@@equal.ignore] with [@@@@deriving compare]."

    | Equal, Some (), None ->
      Location.raise_errorf ~loc "Cannot use [@@compare.ignore] with [@@@@deriving equal]"
    (* Currently this function works for both E and C but can be changed to 
    only work on 1 of them. *)
  
  (* Compare only *)
  let core_type_is_ignored ty =
    is_ignored_gen
      ~loc:ty.ptyp_loc
      ~compare_attr:Compare_params.Attrs.ignore_core_type
      ~equal_attr:Equal_params.Attrs.ignore_core_type
      ty

  (* Both eq and cmp *)
  let label_is_ignored ld =
    is_ignored_gen
      ~loc:ld.pld_loc
      ~compare_attr:Compare_params.Attrs.ignore_label_declaration
      ~equal_attr:Equal_params.Attrs.ignore_label_declaration
      ld

  (* Both *)
  let with_tuple loc ~value ~tys f =
    (* generate
       let id_1, id_2, id_3, ... id_n = value in expr
       where expr is the result of (f [id_1, ty_1 ; id_2, ty_2; ...])
    *)
    (* names of compare func for tuples different? *)
    let names_types = List.map tys
                        ~f:(fun t -> gen_symbol ~prefix:"t" (), t) in
    let pattern =
      let l = List.map names_types ~f:(fun (n, _) -> pvar ~loc n) in
      ppat_tuple ~loc l
    in
    let e = f (List.map names_types ~f:(fun (n,t) -> (evar ~loc n, t))) in
    let binding  = value_binding ~loc ~pat:pattern ~expr:value in
    pexp_let ~loc Nonrecursive [binding] e

  (* Both *)
  let phys_equal_first a b cmp =
    let loc = cmp.pexp_loc in
    [%expr
      if Ppx_compare_lib.phys_equal [%e a] [%e b] then [%e const ~loc Equal] else [%e cmp]
    ]
    (* extension for working with expressions? *)

  (* Both *)
  (* diff chain function depending on module *)
  let rec chain_if ~loc = function
    | [] -> const ~loc Equal
    | [x] -> x
    | x :: xs -> chain x (chain_if ~loc:x.pexp_loc xs)

  (* Compare only *)
  let tp_name n = Printf.sprintf "_cmp__%s" n
(* when is this printed? not used, maybe for extension*)

  (* Both *)
  let type_ ~hide ~loc ty =
    let loc = { loc with loc_ghost = true } in
    let ptyp_attributes =
      if hide
      then Merlin_helpers.hide_attribute :: ty.ptyp_attributes
      else ty.ptyp_attributes
    in
    let hty = { ty with ptyp_attributes } in
    [%type: [%t ty] -> [%t hty] -> [%t result_type ~loc]]

  (* Both *)
  let function_name = function
    | "t" -> name
    | s -> name ^ "_" ^ s

  (* Compare only *)
  let compare_ignore ~loc value1 value2 =
    [%expr let _ : _ = [%e value1] and _ : _ = [%e value2] in [%e const ~loc Equal]]

  (* Compare only *)
  let rec compare_applied ~hide ~constructor ~args value1 value2 =
    let args =
      List.map args ~f:(compare_of_ty_fun ~hide ~type_constraint:false) @ [value1; value2]
    in
    type_constr_conv ~loc:(Located.loc constructor) constructor args
      ~f:function_name

  and compare_of_tuple ~hide loc tys value1 value2 =
    with_tuple loc ~value:value1 ~tys (fun elems1 ->
      with_tuple loc ~value:value2 ~tys (fun elems2 ->
        let exprs = List.map2_exn elems1 elems2 ~f:(fun (v1, t) (v2, _) ->
          compare_of_ty ~hide t v1 v2)
        in
        chain_if ~loc exprs))

  and compare_variant ~hide loc row_fields value1 value2 =
    let map = fun row ->
      match row.prf_desc with
      | Rtag ({ txt = cnstr; _ }, true, _) | Rtag ({ txt = cnstr; _ }, _, []) ->
        case ~guard:None
          ~lhs:(ppat_tuple ~loc
                  [ppat_variant ~loc cnstr None; ppat_variant ~loc cnstr None])
          ~rhs:(const ~loc Equal)
      | Rtag ({ txt = cnstr; _ }, false, tp :: _) ->
        let v1 = gen_symbol ~prefix:"_left" ()
        and v2 = gen_symbol ~prefix:"_right" () in
        let body = compare_of_ty ~hide tp (evar ~loc v1) (evar ~loc v2) in
        case ~guard:None
          ~lhs:(ppat_tuple ~loc [ ppat_variant ~loc cnstr (Some (pvar ~loc v1))
                                ; ppat_variant ~loc cnstr (Some (pvar ~loc v2))
                                ])
          ~rhs:body
      | Rinherit { ptyp_desc = Ptyp_constr (id, args); _ } ->
        (* quite sadly, this code doesn't handle:
            type 'a id = 'a with compare
            type t = [ `a | [ `b ] id ] with compare
            because it will generate a pattern #id, when id is not even a polymorphic
            variant in the first place.
            The culprit is caml though, since it only allows #id but not #([`b] id)
        *)
        let v1 = gen_symbol ~prefix:"_left" ()
        and v2 = gen_symbol ~prefix:"_right" () in
        case ~guard:None
          ~lhs:(ppat_tuple ~loc [ ppat_alias ~loc (ppat_type ~loc id) (Located.mk ~loc v1)
                                ; ppat_alias ~loc (ppat_type ~loc id) (Located.mk ~loc v2)
                                ])
          ~rhs:(compare_applied ~hide ~constructor:id ~args (evar ~loc v1) (evar ~loc v2))
      | Rinherit ty ->
        Location.raise_errorf ~loc:ty.ptyp_loc "Ppx_compare.compare_variant: unknown type"
    in
    let e =
      let matched = pexp_tuple ~loc [value1; value2] in
      match List.map ~f:map row_fields with
      | [v] -> pexp_match ~loc matched [v]
      | l     ->
        pexp_match ~loc matched
          (l @
            (* Providing we didn't screw up badly we now know that the tags of the variants
              are different. We let pervasive do its magic. *)
            [ case ~guard:None ~lhs:[%pat? (x, y)]
                ~rhs:(poly ~loc [%expr x] [%expr y]) ])
    in
    phys_equal_first value1 value2 e

  and compare_of_ty ~hide ty value1 value2 =
    let loc = ty.ptyp_loc in
    if core_type_is_ignored ty
    then compare_ignore ~loc value1 value2
    else
      match ty.ptyp_desc with
      | Ptyp_constr (constructor, args) ->
        compare_applied ~hide ~constructor ~args value1 value2
      | Ptyp_tuple tys -> compare_of_tuple ~hide loc tys value1 value2
      | Ptyp_var name -> eapply ~loc (evar ~loc (tp_name name)) [value1; value2]
      | Ptyp_arrow _ ->
        Location.raise_errorf ~loc
          "ppx_compare: Functions can not be compared."
      | Ptyp_variant (row_fields, Closed, None) ->
        compare_variant ~hide loc row_fields value1 value2
      | Ptyp_any -> compare_ignore ~loc value1 value2
      | _ ->
        Location.raise_errorf ~loc "ppx_compare: unknown type"

  and compare_of_ty_fun ~hide ~type_constraint ty =
    let loc = { ty.ptyp_loc with loc_ghost = true } in
    let do_hide hide_fun x = if hide then hide_fun x else x in
    let a = gen_symbol ~prefix:"a" () in
    let b = gen_symbol ~prefix:"b" () in
    let e_a = evar ~loc a in
    let e_b = evar ~loc b in
    let mk_pat x =
      if type_constraint then
        ppat_constraint ~loc (pvar ~loc x) ty
      else
        pvar ~loc x
    in
    let body = do_hide Merlin_helpers.hide_expression (compare_of_ty ~hide ty e_a e_b) in
    eta_reduce_if_possible
      [%expr
        fun [%p mk_pat a] [%p do_hide Merlin_helpers.hide_pattern (mk_pat b)] ->
          [%e body] ]

  let sig_type_decl ~ctxt (_rec_flag, tds) =
    let hide = not (Expansion_context.Deriver.inline ctxt) in
    let tds = List.map tds ~f:name_type_params_in_td in
    List.map tds ~f:(fun td ->
      let compare_of = combinator_type_of_type_declaration td ~f:(type_ ~hide) in
      let name = function_name td.ptype_name.txt in
      let loc = td.ptype_loc in
      psig_value ~loc (value_description ~loc ~name:{ td.ptype_name with txt = name }
                         ~type_:compare_of ~prim:[]))

  (* Compare only *)

end

(* S from expander_intf *)
module type S = sig
  (** [type_ ~hide ty] is [ty -> ty -> result_type] where [result_type] is [int] for
      [compare] and [bool] for [equal].

      [hide] controls whether some [[@merlin.hide]] attributes should be added.
  *)
  val type_ : hide:bool -> loc:Location.t -> core_type -> core_type

  (** [core_type ty] is an expression of type [ty -> ty -> result_type] *)
  (* val core_type : core_type -> expression *)

  val sig_type_decl
    :  ctxt:Expansion_context.Deriver.t
    -> rec_flag * type_declaration list
    -> signature

  module Attrs : Attrs
end

module Equal = Make(Equal_params)

module Compare = struct
  include Make(Compare_params)

  (* let equal_core_type ty =
    let loc = { ty.ptyp_loc with loc_ghost = true } in
    let arg1 = gen_symbol () in
    let arg2 = gen_symbol () in
    let body =
      Merlin_helpers.hide_expression
        [%expr
          match [%e compare_core_type ty] [%e evar ~loc arg1] [%e evar ~loc arg2] with
          | 0 -> true
          | _ -> false]
    in
    [%expr
      (fun ([%p pvar ~loc arg1] : [%t ty]) [%p pvar ~loc arg2] -> [%e body])] *)
end

(* add_deriver, equal, compare from ppx_compare.ml *)
(* add structure here later: *)
let add_deriver name (module E : S) =
  let sig_type_decl =
    Deriving.Generator.V2.make_noarg E.sig_type_decl
  in
  Deriving.add name
    ~sig_type_decl

let equal = add_deriver "equal" (module Equal)
let compare = add_deriver "compare" (module Compare)

(* What is the purpose of expander.mli and ppx_compare.mli ? *)
(* Expose: equal, compare as Deriving.t ; Equal, Compare as S or S+something *)
