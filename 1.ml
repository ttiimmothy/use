open Ppxlib
module Ast = Ast_builder.Default

(** helpers *)
let loc ~ctxt = Expansion_context.Deriver.derived_item_loc ctxt

let var ~ctxt name =
  let loc = loc ~ctxt in
  Loc.make ~loc name
;;

(* [gensym ()] creates a new symbol generator that always starts at 0. It is convenient
   for when you need to iterate and name variables several times.

   If you need a global counter that won't restart, you can rebind `gensym` to `gensym ()`
*)
let gensym () =
  let counter = ref 0 in
  fun ~ctxt ->
    counter := !counter + 1;
    var ~ctxt ("v_" ^ Int.to_string !counter)
;;

let deserializer_fn_name_for_longident name =
  let name =
    match name.txt |> Longident.flatten_exn |> List.rev with
    | name :: [] -> "deserialize_" ^ name
    | name :: path -> ("deserialize_" ^ name) :: path |> List.rev |> String.concat "."
    | _ -> "unknown"
  in
  Longident.parse name
;;

let error_with_msg ~loc msg = [%expr Error (`Msg [%e Ast.estring ~loc msg])]

let is_primitive = function
  | "bool"
  | "char"
  | "float"
  | "int"
  | "int32"
  | "int64"
  | "string"
  | "list"
  | "array"
  | "unit"
  | "option" -> true
  | _ -> false
;;

(** [deserializer_for_type] creates a call to a deserializer based on a type.

    When type is a constructor (or [Ptyp_constr], which is OCaml for "any type name"),
    we will look at the number of arguments it has and 

*)
let rec deserializer_for_type ~ctxt (core_type : Parsetree.core_type) =
  let loc = loc ~ctxt in
  match core_type.ptyp_desc with
  | Ptyp_constr (name, arg :: []) when is_primitive (Longident.name name.txt) ->
    let type_ser = deserializer_for_type ~ctxt arg in
    let name = Ast.pexp_ident ~loc name in
    [%expr d ([%e name] [%e type_ser])]
  | Ptyp_constr (name, []) when is_primitive (Longident.name name.txt) -> Ast.pexp_ident ~loc name
  | Ptyp_constr (name, _args) ->
    let ser_fn = deserializer_fn_name_for_longident name |> var ~ctxt |> Ast.pexp_ident ~loc in
    [%expr d [%e ser_fn]]
  | Ptyp_any | Ptyp_var _
  | Ptyp_arrow (_, _, _)
  | Ptyp_tuple _
  | Ptyp_object (_, _)
  | Ptyp_class (_, _)
  | Ptyp_alias (_, _)
  | Ptyp_variant (_, _, _)
  | Ptyp_poly (_, _)
  | Ptyp_package _ | Ptyp_extension _ -> failwith "unsupported"
;;
