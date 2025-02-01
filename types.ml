type ground_type =
  | Unit
  | Bool
  | Char
  | Int
  | Float
  | String
  | CustomType of string
  | SizedArray of ground_type * int
  | UnsizedArray of ground_type (* pointer *)
  | Function of (ground_type list) * ground_type (* (...) -> res *)
  | Struct of string * ((string * ground_type) list)
  | Tuple of (ground_type list)
  | Sumtype of (string * string)
  | SumtypeDef of (string * (ground_type list)) list
  | Modetype of string
  | ModetypeDef of string * string list
  | Promise of string (* definition of promise is stored somewhere else *)
  | PromiseDef of (string * string * ground_type list * ground_type)
  | Effect of string (* definition of effect is stored somewhere else *)
  | EffectDef of string * (ground_type list)
  | Mutable of ground_type (* need to ensure mutable type is not self-nested *)
  | Global of ground_type  (* need to ensure global type is not self-nested *)
  | Poly of int
  | AnyType
  | InterruptType
  (* add sumtypes *)

let rec base_type_of t =
  match t with
  | Unit -> Unit
  | Bool -> Bool
  | Char -> Char
  | Int -> Int
  | Float -> Float
  | String -> String
  | InterruptType -> InterruptType
  | CustomType(_) -> t
  | SizedArray(_, _) -> t
  | UnsizedArray(_) -> t
  | Function(_, _) -> t
  | Struct(_) -> t
  | Tuple(_) -> t
  | Sumtype(_) -> t
  | SumtypeDef(_) -> t
  | Modetype(_) -> t
  | ModetypeDef(_, _) -> t
  | Promise(_) -> t
  | PromiseDef(_, _, _, _) -> t
  | Effect(_) -> t
  | EffectDef(_) -> t
  | Poly(_) -> t
  | Mutable(t') -> base_type_of t'
  | Global(t') -> base_type_of t'

let rec is_mutable t =
  match t with
  | Mutable(_) -> true
  | Global(t') -> is_mutable t'
  | _ -> false

let rec is_global t =
  match t with
  | Global(_) -> true
  | Mutable(t') -> is_global t'
  | _ -> false

let rec string_of_ground_type t =
  match t with
  | Unit -> "Unit"
  | Bool -> "Bool"
  | Char -> "Char"
  | Int -> "Int"
  | Float -> "Float"
  | String -> "String"
  | CustomType(s) -> s
  | SizedArray(a, i) -> 
    Printf.sprintf 
      "SizedArray(%s, %i)"
      (string_of_ground_type a)
      i
  | UnsizedArray(a) ->
    Printf.sprintf
      "UnsizedArray(%s)"
      (string_of_ground_type a)
  | Function(args, rettype) ->
    Printf.sprintf
      "Function([%s], %s)"
      (String.concat "; " @@ List.map string_of_ground_type args)
      (string_of_ground_type rettype)
  | Struct(s, l) ->
    l
    |> List.map (fun (name, t) -> (Printf.sprintf "(%s, %s)" name (string_of_ground_type t)))
    |> String.concat "; "
    |> Printf.sprintf "Struct(%s, [%s])" s
  | Tuple(l) ->
    l
    |> List.map (string_of_ground_type)
    |> String.concat "; "
    |> Printf.sprintf "Tuple([%s])"
  | Sumtype(name, name2) -> Printf.sprintf "Sumtype(%s, %s)" name name2
  | SumtypeDef(l) ->
    Printf.sprintf "SumtypeDef([%s])"
      (String.concat "; " @@ List.map 
        (fun (name, args) -> 
          Printf.sprintf "(%s, [%s])"
            name
            (String.concat "; " @@ List.map string_of_ground_type args)) 
        l)
  | Modetype(name) -> Printf.sprintf "Modetype(%s)" name
  | ModetypeDef(modetype_name, modes) -> Printf.sprintf "ModetypeDef(%s, [%s])"
    modetype_name (String.concat "; " modes)
  | Promise(name) -> Printf.sprintf "Promise(%s)" name
  | PromiseDef(name, effname, args, rettype) ->
    Printf.sprintf "PromiseDef(%s, %s, [%s], %s)"
      name
      effname
      (String.concat "; " @@ List.map string_of_ground_type args)
      (string_of_ground_type rettype)
  | Effect(name) -> Printf.sprintf "Effect(%s)" name
  | EffectDef(name, args) ->
    Printf.sprintf "EffectDef(%s, [%s])"
      name
      (String.concat "; " @@ List.map string_of_ground_type args)
  | Mutable(t') -> 
    Printf.sprintf "Mutable(%s)" @@ string_of_ground_type t'
  | Global(t') -> 
    Printf.sprintf "Global(%s)" @@ string_of_ground_type t'
  | Poly(id) -> Printf.sprintf "Poly(%d)" id
  | AnyType -> "AnyType"
  | InterruptType -> "InterruptType"

let rec unify_types from a b =
  print_endline @@ Printf.sprintf "DEBUG: unifying types from %s" from;
  match a, b with
  | AnyType, _ -> b
  | _, AnyType -> a
  | Unit, Unit -> Unit
  | Bool, Bool -> Bool
  | Char, Char -> Char
  | Int, Int -> Int
  | Float, Float -> Float
  | String, String -> String
  | InterruptType, InterruptType -> InterruptType
  | CustomType a', CustomType b' ->
    if a' = b' then CustomType a' else
    failwith @@ Printf.sprintf "Types %s and %s are not the same" a' b'
  | Poly a', Poly b' ->
    let min a b = if a < b then a else b in
    Poly (min a' b')
  | SizedArray (a', a_s), SizedArray (b', b_s) ->
    let t = unify_types from a' b' in
    if a_s = b_s then (SizedArray(t, a_s)) else
    failwith @@ Printf.sprintf "Couldn't unify sized arrays: sizes are different: %d and %d" a_s b_s
  | UnsizedArray (a'), UnsizedArray (b') ->
    let t = unify_types from a' b' in
    UnsizedArray(t)
  | Struct (a_name, a'), Struct (b_name, b') ->
    let fail () = failwith "Struct unification failed!" in
    let (t : (string * ground_type) list) = 
      if a_name = b_name && List.length a' = List.length b' then
        List.combine a' b' |>
        (List.map (fun ((a_name, _a), (b_name, _b)) -> 
          (if a_name <> b_name then 
            fail () 
          else
            (a_name, unify_types from _a _b))))
      else 
        fail ()
    in
    Struct (a_name, t)
  | Tuple a', Tuple b' ->
    let t = if List.length a' = List.length b' then
      List.combine a' b' |>
      List.map (fun (_a, _b) -> unify_types from _a _b)
    else failwith "Tuple unification failed!" 
    in
    Tuple t
  | Mutable a', Mutable b' ->
    Mutable (unify_types from a' b')
  | PromiseDef(_, _, _, _), Unit
  | Unit, PromiseDef(_, _, _, _) -> Unit
  | PromiseDef(_, _, _, a), b
  | a, PromiseDef(_, _, _, b) -> unify_types from a b
  | Poly _, _ -> b
  | _, Poly _ -> a
  | a, b -> failwith @@ Printf.sprintf "Type unification failed!: %s and %s" (string_of_ground_type a) (string_of_ground_type b)

(*
type effect =
  | Effect of string * (ground_type list)

type promise =
  | Promise of effect * ground_type

type computation =
  | Comp of ground_type
  | Parallel of computation * computation
*)

type global =
  | Global of ground_type

