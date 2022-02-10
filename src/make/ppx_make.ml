(* Generated code should depend on the environment in scope as little as
   possible.  E.g. rather than [foo = []] do [match foo with [] ->], to 
   eliminate the use of [=], which might be overwritten in the environment.
   It is especially important to not use polymorphic comparisons. *)

   open Ppxlib
   open Ast_builder.Default
   
   module Construct = struct
     (* Additional AST construction helpers *)
   
     let apply_type ~loc ~ty_name ~tps = 
      ptyp_constr ~loc (Located.lident ~loc ty_name) tps
     ;;

     let lambda ~loc patterns body =
      List.fold_left (fun acc (lab, pat) ->
       pexp_fun ~loc lab None pat acc) body patterns
     ;;
  
     let lambda_sig ~loc arg_tys body_ty =
      List.fold_left (fun acc (lab, arg_ty) ->
       ptyp_arrow ~loc lab arg_ty acc) body_ty arg_tys 
     ;;

     let record ~loc pairs =
      pexp_record
        ~loc
        (List.map (fun (name, exp) -> Located.lident ~loc name, exp) pairs)
        None
     ;;

     let sig_item ~loc name typ =
      psig_value ~loc (value_description ~loc ~name:(Located.mk ~loc name) ~type_:typ ~prim:[])
    ;;
    
     let str_item ~loc name body =
       pstr_value ~loc Nonrecursive [ value_binding ~loc ~pat:(pvar ~loc name) ~expr:body ]
     ;;
   end

   module Check = struct
    let is_derivable ~loc rec_flag tds =
      (match rec_flag with
       | Nonrecursive ->
         Location.raise_errorf ~loc "nonrec is not compatible with the `make' preprocessor."
       | _ -> ());
      let is_record td =
        match td.ptype_kind with
        | Ptype_record _ -> true
        | _ -> false
      in
      if not (List.exists is_record tds)
      then
        Location.raise_errorf
          ~loc
          (match tds with
           | [ _ ] -> "Unsupported use of make (you can only use it on records)."
           | _ ->
             "make can only be applied on type definitions in which at least one \
              type definition is a record.")
    ;;

    let is_public ~private_ ~loc = 
      (match private_ with
            | Private -> Location.raise_errorf ~loc "We cannot expose functions that explicitly create private records."
            | Public -> () )

    let has_option labels = List.exists (fun (name, _) -> match name with 
    | Optional _ -> true
    | _ -> false) labels

    let find_main labels =
      List.fold_left (fun (main, labels) ({ pld_type; pld_loc; pld_attributes ; _ } as label) ->
        if Ppx_deriving.(pld_type.ptyp_attributes @ pld_attributes |>
                         attr ~deriver:"make" "main" |> Arg.get_flag ~deriver:"make") then
          match main with
          | Some _ -> Location.raise_errorf ~loc:pld_loc "Duplicate [@deriving.%s.main] annotation" "make"
          | None -> Some label, labels
        else
          main, label :: labels)
          (None, []) labels
  end
  
   module Gen_sig = struct
     let label_arg name ty = match ty with 
     (* a' option               -> ?name        , a' *)
     | [%type: [%t? a'] option] -> Optional name, a'
     | _ -> Labelled name, ty

     let create_make_sig ~loc ~ty_name ~tps label_decls =
       let record = Construct.apply_type ~loc ~ty_name ~tps in
       let main_arg, label_decls = Check.find_main label_decls in
       let derive_type label_decl =
         let { pld_name = name; pld_type = ty; _ } = label_decl in
         label_arg name.txt ty
       in
       let types = List.map derive_type label_decls in
       let add_unit types = (
         Nolabel, 
         Ast_helper.Typ.constr ~loc { txt = Lident "unit"; loc } []
        )::types in
       let types = match main_arg with 
        | Some { pld_type ; _ } 
            -> (Nolabel, pld_type)::types
        | None when Check.has_option types -> add_unit types
        | None -> types
       in
       let t = Construct.lambda_sig ~loc types record in
       let fun_name = "make_" ^ ty_name in
       Construct.sig_item ~loc fun_name t
     ;;

     let derive_per_td (td : type_declaration) : signature =
       let { ptype_name = { txt = ty_name; loc }
           ; ptype_private = private_
           ; ptype_params
           ; ptype_kind
           ; _
           }
         =
         td
       in
       let tps = List.map (fun (tp, _variance) -> tp) ptype_params in
       match ptype_kind with
       | Ptype_record label_decls ->
         Check.is_public ~private_ ~loc ;
         let derived_item = create_make_sig ~loc ~ty_name ~tps label_decls in
        [ derived_item ]
       | _ -> []
     ;;
   
     let generate ~loc ~path:_ (rec_flag, tds) =
       Check.is_derivable ~loc rec_flag tds;
       List.concat_map (derive_per_td) tds
     ;;
   end
   
   module Gen_struct = struct
     let label_arg ~loc name ty =
         match ty with
         | [%type: [%t? _] option] -> Optional name, pvar ~loc name
         | _ -> Labelled name, pvar ~loc name
     ;;
   
     let create_make_fun ~loc ~record_name label_decls =
      let names = List.map (fun { pld_name = n; _ } -> n.txt, evar ~loc n.txt) label_decls in
       let main_arg, label_decls = Check.find_main label_decls in
       let derive_pattern label_decl = 
        let { pld_name = name; pld_type = ty; _ } = label_decl in
         label_arg ~loc name.txt ty
       in 
       let patterns = List.map derive_pattern label_decls in
       let add_unit patterns = (Nolabel, punit ~loc)::patterns in
       let patterns = match main_arg with 
        | Some { pld_name = { txt = name ; _ } ; _ } 
            -> (Nolabel, pvar ~loc name)::patterns
        | None when Check.has_option patterns -> add_unit patterns
        | None -> patterns
       in
       let create_record = Construct.record ~loc names in
       let derive_lambda = Construct.lambda ~loc patterns create_record in
       let fun_name = "make_" ^ record_name in
       Construct.str_item ~loc fun_name derive_lambda
     ;;
    
     let derive_per_td (td : type_declaration) : structure =
       let { ptype_name = { txt = record_name; loc }
           ; ptype_private = private_
           ; ptype_kind
           ; _
           }
         =
         td
       in
       match ptype_kind with
       | Ptype_record label_decls ->
        (match private_ with
        | Private -> []
        | Public -> let derived_item = create_make_fun ~loc ~record_name label_decls in
        [ derived_item ])
       | _ -> []
     ;;
   
     let generate ~loc ~path:_ (rec_flag, tds) =
       Check.is_derivable ~loc rec_flag tds;
       List.concat_map (derive_per_td) tds
     ;;
   end

   let make =
    Deriving.add "make"
       ~str_type_decl:
        (Deriving.Generator.make_noarg Gen_struct.generate)
       ~sig_type_decl:
        (Deriving.Generator.make_noarg Gen_sig.generate)
   ;;
