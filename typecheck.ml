open Ast
open Util
open Types

type expr_t =
  | Unit_t
  | Bool_t of bool
  | Char_t of char
  | Int_t of int
  | Float_t of float
  | String_t of string
  | Array_t of ground_type * expr_t list
  | Tuple_t of ground_type * expr_t list
  | Ident_t of ground_type * string
  | FuncCall_t of ground_type * string * (expr_t list)
  | CastTo_t of ground_type * expr_t
  | Not_t of expr_t
  | Neg_t of expr_t
  | Add_t of expr_t * expr_t
  | Sub_t of expr_t * expr_t
  | Mul_t of expr_t * expr_t
  | Div_t of expr_t * expr_t
  | Mod_t of expr_t * expr_t
  | FAdd_t of expr_t * expr_t
  | FSub_t of expr_t * expr_t
  | FMul_t of expr_t * expr_t
  | FDiv_t of expr_t * expr_t
  | Eq_t of expr_t * expr_t
  | Neq_t of expr_t * expr_t
  | Lt_t of expr_t * expr_t
  | Le_t of expr_t * expr_t
  | Gt_t of expr_t * expr_t
  | Ge_t of expr_t * expr_t
  | LAnd_t of expr_t * expr_t
  | LOr_t of expr_t * expr_t
  | BNeg_t of expr_t
  | BAnd_t of expr_t * expr_t
  | BOr_t of expr_t * expr_t
  | BXOr_t of expr_t * expr_t
  | BShiftLeft_t of expr_t * expr_t
  | BShiftRight_t of expr_t * expr_t
  | IndexAccess_t of ground_type * expr_t * expr_t
  | MemberAccess_t of ground_type * expr_t * string
  | Match_t of ground_type * ground_type * expr_t * (ground_type * expr_t * expr_t) list
  | Await_t of ground_type * int * string * int
  | Raise_t of int * string * (expr_t list)
  | If_t of ground_type * expr_t * expr_t * expr_t
  | While_t of ground_type * expr_t * expr_t
  | ForTo_t of ground_type * string * expr_t * expr_t * expr_t
  | ForDownTo_t of ground_type * string * expr_t * expr_t * expr_t
  | ForEach_t of ground_type * string * expr_t * expr_t
  | ForEachIndex_t of ground_type * string * string * expr_t * expr_t
  | Block_t of ground_type * (stmt_t list)
  | Struct_t
  | AnonStruct_t of ground_type * (string * expr_t) list
  | Any_t
  | Variant_t of ground_type * string * expr_t list
  | Modetype_t of ground_type * string
  | Promise_t of ground_type * string * int

and stmt_t =
  | Break_t
  | Continue_t
  | BreakWith_t of (Types.ground_type * expr_t)
  | ContinueWith_t of (Types.ground_type * expr_t)
  | EmptyReturn_t
  | Return_t of expr_t
  | Dispatch_t of expr_t
  | Expr_t of expr_t
  | Global_t of string
  | Assign_t of expr_t * expr_t
  | Let_t of string * expr_t
  | LetMut_t of string * expr_t
  | ModeSwitch_t of string

let rec string_of_expr_t e =
  let f l = String.concat "; " @@ List.map (string_of_expr_t) l in
  let f_t = Types.string_of_ground_type in
  let ff = string_of_expr_t in
  match e with
  | Unit_t -> "Unit_t"
  | Bool_t(true) -> "Bool_t(true)"
  | Bool_t(false) -> "Bool_t(false)"
  | Char_t(c) -> Printf.sprintf "Char_t('%c')" c
  | Int_t(i) -> Printf.sprintf "Int_t(%d)" i
  | Float_t(f) -> Printf.sprintf "Float_t(%f)" f
  | String_t(s) -> Printf.sprintf "String_t(\"%s\")" s
  | Array_t(t, l) -> Printf.sprintf "Array_t(%s, [%s])" (f_t t) (f l)
  | Tuple_t(t, l) -> Printf.sprintf "Tuple_t(%s, [%s])" (f_t t) (f l)
  | Ident_t(t, n) -> Printf.sprintf "Ident_t(%s, %s)" (f_t t) n
  | FuncCall_t(t, n, l) -> Printf.sprintf "FuncCall_t(%s, %s, [%s])" (f_t t) n (f l)
  | CastTo_t(t, e') -> Printf.sprintf "CastTo_t(%s, %s)" (f_t t) (ff e')
  | Not_t(e') -> Printf.sprintf "Not_t(%s)" (ff e')
  | Neg_t(e') -> Printf.sprintf "Neg_t(%s)" (ff e')
  | Add_t(a, b) -> Printf.sprintf "Add_t(%s, %s)" (ff a) (ff b)
  | Sub_t(a, b) -> Printf.sprintf "Sub_t(%s, %s)" (ff a) (ff b)
  | Mul_t(a, b) -> Printf.sprintf "Mul_t(%s, %s)" (ff a) (ff b)
  | Div_t(a, b) -> Printf.sprintf "Div_t(%s, %s)" (ff a) (ff b)
  | Mod_t(a, b) -> Printf.sprintf "Mod_t(%s, %s)" (ff a) (ff b)
  | FAdd_t(a, b) -> Printf.sprintf "FAdd_t(%s, %s)" (ff a) (ff b)
  | FSub_t(a, b) -> Printf.sprintf "FSub_t(%s, %s)" (ff a) (ff b)
  | FMul_t(a, b) -> Printf.sprintf "FMul_t(%s, %s)" (ff a) (ff b)
  | FDiv_t(a, b) -> Printf.sprintf "FDiv_t(%s, %s)" (ff a) (ff b)
  | Eq_t(a, b) -> Printf.sprintf "Eq_t(%s, %s)" (ff a) (ff b)
  | Neq_t(a, b) -> Printf.sprintf "Neq_t(%s, %s)" (ff a) (ff b)
  | Lt_t(a, b) -> Printf.sprintf "Lt_t(%s, %s)" (ff a) (ff b)
  | Le_t(a, b) -> Printf.sprintf "Le_t(%s, %s)" (ff a) (ff b)
  | Gt_t(a, b) -> Printf.sprintf "Gt_t(%s, %s)" (ff a) (ff b)
  | Ge_t(a, b) -> Printf.sprintf "Ge_t(%s, %s)" (ff a) (ff b)
  | LAnd_t(a, b) -> Printf.sprintf "LAnd_t(%s, %s)" (ff a) (ff b)
  | LOr_t(a, b) -> Printf.sprintf "LOr_t(%s, %s)" (ff a) (ff b)
  | BNeg_t(e') -> Printf.sprintf "BNeg_t(%s)" (ff e')
  | BAnd_t(a, b) -> Printf.sprintf "BAnd_t(%s, %s)" (ff a) (ff b)
  | BOr_t(a, b) -> Printf.sprintf "BOr_t(%s, %s)" (ff a) (ff b)
  | BXOr_t(a, b) -> Printf.sprintf "BXOr_t(%s, %s)" (ff a) (ff b)
  | BShiftLeft_t(a, b) -> Printf.sprintf "BShiftLeft_t(%s, %s)" (ff a) (ff b)
  | BShiftRight_t(a, b) -> Printf.sprintf "BShiftRight_t(%s, %s)" (ff a) (ff b)
  | IndexAccess_t(t, a, b) -> Printf.sprintf "IndexAccess_t(%s, %s, %s)" (f_t t) (ff a) (ff b)
  | MemberAccess_t(t, m, n) -> Printf.sprintf "MemberAccess_t(%s, %s, %s)" (f_t t) (ff m) n
  | Match_t(t_a, t_b, v, l) ->
    Printf.sprintf "Match_t(%s, %s, %s, [%s])" 
      (f_t t_a)
      (f_t t_b)
      (ff v)
      (String.concat "; " @@ List.map (fun (a, b, c) -> (Printf.sprintf "(%s, %s, %s)" (f_t a) (ff b) (ff c))) l)
  | Await_t(t, i, n, pi) -> Printf.sprintf "Await_t(%s, %d, %s, %d)" (f_t t) i n pi
  | Raise_t(i, n, l) -> Printf.sprintf "Raise_t(%d, %s, [%s])" i n (f l)
  | If_t(t, a, b, c) -> Printf.sprintf "If_t(%s, %s, %s, %s)" (f_t t) (ff a) (ff b) (ff c)
  | While_t(t, c, b) -> Printf.sprintf "While_t(%s, %s, %s)" (f_t t) (ff c) (ff b)
  | ForTo_t(t, i, a, b, c) -> Printf.sprintf "ForTo_t(%s, %s, %s, %s, %s)" (f_t t) i (ff a) (ff b) (ff c)
  | ForDownTo_t(t, i, a, b, c) -> Printf.sprintf "ForDownTo_t(%s, %s, %s, %s, %s)" (f_t t) i (ff a) (ff b) (ff c)
  | ForEach_t(t, n, a, b) -> Printf.sprintf "ForEach_t(%s, %s, %s, %s)" (f_t t) n (ff a) (ff b)
  | ForEachIndex_t(t, i, n, a, b) -> 
      Printf.sprintf "ForEachIndex_t(%s, %s, %s, %s, %s)" (f_t t) i n (ff a) (ff b)
  | Block_t(t, l) -> 
    Printf.sprintf "Block_t(%s, [%s])" (f_t t) (String.concat "; " @@ List.map string_of_stmt_t l)
  | Struct_t -> Printf.sprintf "Struct_t"
  | AnonStruct_t(t, l') -> Printf.sprintf "AnonStruct_t(%s, [%s])" 
      (f_t t) 
      (String.concat "; " @@ List.map (fun (a, b) -> Printf.sprintf "(%s, %s)" a (ff b)) l')
  | Any_t -> "Any_t"
  | Variant_t(t, n, l) -> Printf.sprintf "Variant_t(%s, %s, [%s])" (f_t t) n (f l)
  | Modetype_t(t, n) -> Printf.sprintf "Modetype_t(%s, %s)" (f_t t) n
  | Promise_t(t, n, i) -> Printf.sprintf "Promise(%s, %s, %d)" (f_t t) n i

and string_of_stmt_t s =
  let f_t = Types.string_of_ground_type in
  let ff = string_of_expr_t in
  match s with
  | Break_t -> "Break_t"
  | Continue_t -> "Continue_t"
  | BreakWith_t(t, e) -> Printf.sprintf "BreakWith_t(%s, %s)" (f_t t) (ff e)
  | ContinueWith_t(t, e) -> Printf.sprintf "ContinueWith_t(%s, %s)" (f_t t) (ff e)
  | EmptyReturn_t -> "EmptyReturn_t"
  | Return_t(e) -> Printf.sprintf "Return_t(%s)" (ff e)
  | Expr_t(e) -> Printf.sprintf "Expr_t(%s)" (ff e)
  | Global_t(n) -> Printf.sprintf "Global_t(%s)" n
  | Assign_t(a, b) -> Printf.sprintf "Assign_t(%s, %s)" (ff a) (ff b)
  | Let_t(a, b) -> Printf.sprintf "Let_t(%s, %s)" a (ff b)
  | LetMut_t(a, b) -> Printf.sprintf "LetMut_t(%s, %s)" a (ff b)
  | ModeSwitch_t(n) -> Printf.sprintf "ModeSwitch_t(%s)" n
  | Dispatch_t(e) -> Printf.sprintf "Dispatch_t(%s)" (ff e)

let is_base_value = function
  | Unit_t
  | Bool_t(_)
  | Char_t(_)
  | Int_t(_)
  | Float_t(_)
  | String_t(_)
  | Array_t(_, _)
  | Tuple_t(_, _)
  | Struct_t
  | AnonStruct_t(_, _)
  | Any_t -> true
  | _ -> false

let type_of (e : expr_t) : ground_type =
  match e with
  | Unit_t -> Unit
  | Bool_t _ -> Bool
  | Char_t _ -> Char
  | Int_t  _ -> Int
  | Float_t _ -> Float
  | String_t _ -> String
  | Array_t(t, _) -> UnsizedArray(t)
  | Tuple_t(t, _) -> t
  | Ident_t(t, _) -> t
  | FuncCall_t(t, _, _) -> t
  | CastTo_t(t, _) -> t
  | Not_t _ -> Bool
  | Neg_t _ -> Int
  | Add_t _ -> Int
  | Sub_t _ -> Int
  | Mul_t(_, _) -> Int
  | Div_t(_, _) -> Int
  | Mod_t(_, _) -> Int
  | FAdd_t(_, _) -> Float
  | FSub_t(_, _) -> Float
  | FMul_t(_, _) -> Float
  | FDiv_t(_, _) -> Float
  | Eq_t(_, _) -> Bool
  | Neq_t(_, _) -> Bool
  | Lt_t(_, _) -> Bool
  | Le_t(_, _) -> Bool
  | Gt_t(_, _) -> Bool
  | Ge_t(_, _) -> Bool
  | LAnd_t(_, _) -> Bool
  | LOr_t(_, _) -> Bool
  | BNeg_t(_) -> Int
  | BAnd_t(_, _) -> Int
  | BOr_t(_, _) -> Int
  | BXOr_t(_, _) -> Int
  | BShiftLeft_t(_, _) -> Int
  | BShiftRight_t(_, _) -> Int
  | IndexAccess_t(t, _, _) -> t
  | MemberAccess_t(t, _, _) -> t
  | Match_t(t, _, _, _) -> t
  | Await_t(t, _, _, _) -> t
  | Raise_t(_, _, _) -> Unit
  (*
  | Let_t(t, _, _, _) -> t
  | LetMut_t(t, _, _, _) -> t
  *)
  | If_t(t, _, _, _) -> t
  | While_t(t, _, _) -> t
  | ForTo_t(t, _, _, _, _) -> t
  | ForDownTo_t(t, _, _, _, _) -> t
  | ForEach_t(t, _, _, _) -> t
  | ForEachIndex_t(t, _, _, _, _) -> t
  | Block_t(t, _) -> t
  | Struct_t -> failwith "TODO: type_of Struct_t"
  | AnonStruct_t(t, _) -> t
  | Any_t -> Types.AnyType
  | Variant_t(t, _, _) -> t
  | Modetype_t(t, _) -> t
  | Promise_t(t, _, _) -> t


let type_of_stmt s =
  failwith "TODO: type_of_stmt"

type typecheck_ctx = {
  vars : (string * Types.ground_type) list;
  mut_vars : (string * Types.ground_type) list;
  glb_vars : (string * Types.ground_type) list;
  globals : (string * Types.ground_type) list;
  custom_types : (string * Types.ground_type) list;
  promises : (string * (string * (string * Types.ground_type) list * Types.ground_type * expr_t)) list;
  promise_types : (string * Types.ground_type) list;
  tasks : (string * stmt_t list) list;
  interrupts : (string * string) list;
  effects: (string * Types.ground_type list) list;
  funcs: (string * ((string * Types.ground_type) list * expr_t)) list;
  tasks_and_modetypes: (string * string) list;
  modes_and_modetypes: (string * string) list;
  mode_transitions: (string * ((string * string) * (string list * string list)) list) list;
  tasks_and_promises: (string * (string list)) list;
  start_modes : (string list);
  modes_and_tasks : (string * string list) list;
  parallels: (string list) list;
  parallels_resolved: (
    (*
    ((string * string * int * int * int option) list) * 
    ((string * string * int option * int * int) list)
    *)
    ((string * string) * (string * int * int option * string) list) list *
    ((string * string) * ((string * string) * int list) list) list
    );
  in_loop: bool;
  in_promise: bool;
  in_func: bool;
  in_task: bool;
  cur_mode_type_ctx: string option;
  available_modes: string list;
}

let empty_ctx = 
  { vars = []
  ; mut_vars = []
  ; glb_vars = []
  ; globals = []
  ; custom_types = []
  ; promises = []
  ; promise_types = []
  ; tasks = []
  ; interrupts = []
  ; effects = []
  ; funcs = []
  ; tasks_and_modetypes = []
  ; modes_and_modetypes = []
  ; mode_transitions = []
  ; tasks_and_promises = []
  ; start_modes = []
  ; modes_and_tasks = []
  ; parallels = []
  ; parallels_resolved = ([], [])
  ; in_loop = false 
  ; in_promise = false
  ; in_func = false
  ; in_task = false
  ; cur_mode_type_ctx = None
  ; available_modes = []
  }

let vars_of { vars } = vars
let funcs_of { funcs } = funcs
let effects_of { effects } = effects
let mut_vars_of { mut_vars } = mut_vars
let glb_vars_of { glb_vars } = glb_vars
let globals_of { globals } = globals
let custom_types_of { custom_types } = custom_types
let promises_of { promises } = promises
let promise_types_of { promise_types } = promise_types
let tasks_of { tasks } = tasks
let interrupts_of { interrupts } = interrupts
let parallels_of { parallels } = parallels
let parallels_resolved_of { parallels_resolved } = parallels_resolved
let is_in_loop { in_loop } = in_loop
let is_in_promise { in_promise } = in_promise
let is_in_func { in_func } = in_func
let is_in_task { in_task } = in_task
let cur_mode_type_ctx_of { cur_mode_type_ctx } = cur_mode_type_ctx
let available_modes_of { available_modes } = available_modes
let tasks_and_modetypes_of { tasks_and_modetypes } = tasks_and_modetypes
let modes_and_modetypes_of { modes_and_modetypes } = modes_and_modetypes
let mode_transitions_of { mode_transitions } = mode_transitions
let tasks_and_promises_of { tasks_and_promises } = tasks_and_promises
let start_modes_of { start_modes } = start_modes
let modes_and_tasks_of { modes_and_tasks } = modes_and_tasks

let mode_type_of ctx name =
  let modes_and_modetypes = modes_and_modetypes_of ctx in
  match List.assoc_opt name modes_and_modetypes with
  | Some(modetype) -> modetype
  | None -> failwith @@ Printf.sprintf "Typecheck error: %s is not a defined mode!" name

let ctx_of_vars vars ctx = 
  { ctx with vars = vars }

let ctx_of_mut_vars mut_vars ctx =
  { ctx with mut_vars = mut_vars }

let ctx_of_glb_vars glb_vars ctx =
  { ctx with glb_vars = glb_vars }

let ctx_of_globals globals ctx =
  { ctx with globals = globals }

let ctx_of_custom_types custom_types ctx =
  { ctx with custom_types = custom_types }

let ctx_of_promises promises ctx =
  { ctx with promises = promises }

let ctx_of_promise_types promise_types ctx =
  { ctx with promise_types = promise_types }

let ctx_of_tasks tasks ctx =
  { ctx with tasks = tasks }

let ctx_of_interrupts interrupts ctx =
  { ctx with interrupts = interrupts }

let ctx_of_effects effects ctx =
  { ctx with effects = effects }

let ctx_of_funcs funcs ctx =
  { ctx with funcs = funcs }

let ctx_of_tasks_and_modetypes tasks_and_modetypes ctx =
  { ctx with tasks_and_modetypes = tasks_and_modetypes }

let ctx_of_modes_and_modetypes modes_and_modetypes ctx =
  { ctx with modes_and_modetypes = modes_and_modetypes }

let ctx_of_mode_transitions mode_transitions ctx =
  { ctx with mode_transitions = mode_transitions }

let ctx_of_tasks_and_promises tasks_and_promises ctx =
  { ctx with tasks_and_promises = tasks_and_promises }

let ctx_of_start_modes start_modes ctx =
  { ctx with start_modes = start_modes }

let ctx_of_modes_and_tasks modes_and_tasks ctx =
  { ctx with modes_and_tasks = modes_and_tasks }

let ctx_of_parallels parallels ctx =
  { ctx with parallels = parallels }

let ctx_of_parallels_resolved parallels_resolved ctx =
  { ctx with parallels_resolved = parallels_resolved }

let ctx_in_loop ctx =
  { ctx with in_loop = true }

let ctx_not_in_loop ctx = 
  { ctx with in_loop = false }

let ctx_in_promise ctx = 
  { ctx with in_promise = true }

let ctx_not_in_promise ctx =
  { ctx with in_promise = false }

let ctx_in_func ctx =
  { ctx with in_func = true }

let ctx_not_in_func ctx =
  { ctx with in_func = false }

let ctx_in_task ctx =
  { ctx with in_task = true }

let ctx_not_in_task ctx =
  { ctx with in_task = false }

let ctx_of_cur_mode_type cur_mode_type_ctx ctx =
  { ctx with cur_mode_type_ctx = cur_mode_type_ctx }

let ctx_of_available_modes available_modes ctx =
  { ctx with available_modes = available_modes }

let is_a_promise (i : string) ctx =
  let custom_types = custom_types_of ctx in
  match List.assoc_opt i custom_types with
  | Some(Promise(_)) -> true
  | Some(PromiseDef _) -> true
  | _ -> false

let defined_in_varstore (i : string) varstore =
  let _vars = vars_of varstore in
  let _mut_vars = mut_vars_of varstore in
  let _glb_vars = glb_vars_of varstore in
  match List.assoc_opt i (_vars @ _mut_vars @ _glb_vars) with
  | Some _ -> true
  | None -> false

let type_of_var i varstore =
  let _vars = vars_of varstore in
  let _mut_vars = mut_vars_of varstore in
  let _glb_vars = glb_vars_of varstore in
  let custom_types = custom_types_of varstore in
  match List.assoc_opt i (_vars @ _mut_vars @ _glb_vars @ custom_types) with
  | Some (t) -> t
  | None -> failwith @@ Printf.sprintf "type_of_var: Impossible case: %s not found" i

let lookup_type t ctx =
  match t with
  | CustomType(s) ->
    let custom_types = custom_types_of ctx in
    (match List.assoc_opt s custom_types with
    | Some(t') -> t'
    | None -> failwith @@ Printf.sprintf "Custom type %s is not defined!" s
    )
  | _ -> t

let rec can_be_used_in_struct_definition from ctx t =
  let check2 = can_be_used_in_struct_definition from ctx in
  let open Types in
  match base_type_of t with
  | Unit
  | Bool
  | Char
  | Int
  | Float
  | String -> ()
  | CustomType(tname) ->
    let custom_types = custom_types_of ctx in
    (match List.assoc_opt tname custom_types with
    | None -> failwith @@ Printf.sprintf
      "Typecheck error: %s: type %s is not defined!" from tname
    | Some(t') -> check2 t')
  | SizedArray(t', size) ->
    if size < 0 then
      failwith @@ Printf.sprintf "Typecheck error: %s can't create an array with negative size!" from
    else
      check2 t'
  | UnsizedArray(t') -> check2 t'
  | Struct(_, fields) ->
    (* I wonder if I should perform the struct checks here as well *)
    fields
    |> List.map snd
    |> List.iter check2
    |> ignore
  | Tuple(values) ->
    values
    |> List.iter check2
    |> ignore
  | Sumtype(tname, vname) -> failwith @@ Printf.sprintf
    "Typecheck error: %s: illegal case in sumtype type checking! %s %s" from tname vname
  | SumtypeDef(variants) ->
    List.iter (fun (_, types) -> List.iter check2 types) variants
  | Function(_, _) -> failwith @@ Printf.sprintf 
    "Typecheck error: %s: function is not allowed in the definition!" from
  | Modetype(_) -> failwith @@ Printf.sprintf 
    "Typecheck error: %s: mode type is not allowed in the definition!" from
  | ModetypeDef(_) -> failwith @@ Printf.sprintf 
    "Typecheck error: %s: mode type definition is not allowed in the defintion!" from
  | Promise(_) -> failwith @@ Printf.sprintf 
    "Typecheck error: %s: Promise is not allowed in the definition!" from
  | PromiseDef(_) -> failwith @@ Printf.sprintf 
    "Typecheck error: %s: Promise definition is not allowed in the defintion!" from
  | Effect(_) -> failwith @@ Printf.sprintf 
    "Typecheck error: %s: Effect is not allowed in sumtype definition!" from
  | EffectDef(_) -> failwith @@ Printf.sprintf 
    "Typecheck error: %s: Effect definition is not allowed in sumtype definition!" from
  | Mutable(_) -> failwith @@ Printf.sprintf "%s: Impossible case 1" from
  | Global(_) -> failwith @@ Printf.sprintf "%s: Impossible case 2" from
  | Poly(_) -> failwith @@ Printf.sprintf
    "Typecheck error: %s: something went wrong, polymorphic types not yet allowed in the definition!" from
  | AnyType -> failwith @@ Printf.sprintf 
    "Typecheck error: %s: something went wrong, Any types not allowed in the definition!" from

(*
let typecheck_sumtype ctx s =
  (* Need to ensure that the name is not taken *)
  (* Need to ensure that the constructor names are all distinct *)
  (* Need to ensure that the constructor names are unique (can be relaxed later) *)
  (* Need to ensure that the types are well-defined *)
  (* Need to ensure that the definitions are not recursive *)
  failwith "TODO: typecheck_sumtype"
*)

(*
let typecheck_mode ctx m =
  (* Almost similar to typechecking sumtypes *)
  failwith "TODO: typecheck_mode"
*)

let typecheck_start_modes ctx l =
  (* Need to ensure that the ALL the modes are mentioned *)
  (* Need to ensure that ONLY ONE mode from each mode type is used *)
  failwith "TODO: typecheck_start_modes"

let eff_counter = ref 0
let eff_c () =
  let c = !eff_counter in
  incr eff_counter;
  c

let pro_counter = ref 0
let pro_c () =
  let c = !pro_counter in
  incr pro_counter;
  c

let await_counter = ref 0
let await_c () =
  let c = !await_counter in
  incr await_counter;
  c

let rec typecheck_expr et ctx (e : Ast.expr) =
  let open Ast in
  let open Types in
  let typecheck_binop et ctx t a b =
    let a' = typecheck_expr et ctx a in
    let b' = typecheck_expr et ctx b in
    let a_t = base_type_of @@ type_of a' in
    let b_t = base_type_of @@ type_of a' in
    print_endline @@ Printf.sprintf "DEBUG: typecheck_binop: a: %s" (string_of_expr_t a');
    print_endline @@ Printf.sprintf "DEBUG: typecheck_binop: b: %s" (string_of_expr_t b');
    unify_types "typecheck binop 1" t a_t |> ignore; 
    unify_types "typecheck binop 2" t b_t |> ignore;
    (a', b')
  in
  match e with
  | Any -> Any_t
  | Unit -> Unit_t
  | Bool b -> Bool_t b
  | Char c -> Char_t c
  | Int i -> Int_t i
  | Float f -> Float_t f
  | String s -> String_t s
  | Ident i -> 
    if defined_in_varstore i ctx then
      let tp = type_of_var i ctx in
      Ident_t(tp, i)
    else
      (if is_a_promise i ctx then
        (* Ident_t(type_of_var i ctx, i) *)
        Promise_t(type_of_var i ctx, i, pro_c ())
      else (
        print_endline @@ Printf.sprintf 
          "DEBUG: type of %s is %s" 
          i 
          (Types.string_of_ground_type @@ type_of_var i ctx);
        failwith @@ Printf.sprintf "Typecheck error: Variable %s is not defined in this context!" i))
  | Array(l) ->
    let rec ensure_same_types l =
      List.fold_left (Types.unify_types "typecheck expr Array") (Types.Poly 1000000) l
    in
    let l_t = 
      l
      |> List.map (typecheck_expr et ctx)
    in
    let l_ts =
      l_t
      |> List.map (type_of)
      |> ensure_same_types
    in
    Array_t(l_ts, l_t)
  | Tuple(l) ->
    let l_t = List.map (typecheck_expr et ctx) l in
    let l_ts = List.map type_of l_t in
    Tuple_t(Types.Tuple(l_ts), l_t)
  | CastTo(t, e') ->
    let e'' = typecheck_expr et ctx e' in
    CastTo_t(t, e'')
  | Not(e') -> Not_t(typecheck_expr et ctx e')
  | Neg(e') -> Neg_t(typecheck_expr et ctx e')
  | Add(a, b) ->
    let (a', b') = typecheck_binop et ctx (Types.Int) a b in
    Add_t(a', b')
  | Sub(a, b) ->
    let (a', b') = typecheck_binop et ctx (Types.Int) a b in
    Sub_t(a', b')
  | Mul(a, b) ->
    let (a', b') = typecheck_binop et ctx (Types.Int) a b in
    Mul_t(a', b')
  | Div(a, b) ->
    let (a', b') = typecheck_binop et ctx (Types.Int) a b in
    Div_t(a', b')
  | Mod(a, b) ->
    let (a', b') = typecheck_binop et ctx (Types.Int) a b in
    Mod_t(a', b')
  | FAdd(a, b) ->
    let (a', b') = typecheck_binop et ctx (Types.Float) a b in
    FAdd_t(a', b')
  | FSub(a, b) ->
    let (a', b') = typecheck_binop et ctx (Types.Float) a b in
    FSub_t(a', b')
  | FMul(a, b) ->
    let (a', b') = typecheck_binop et ctx (Types.Float) a b in
    FMul_t(a', b')
  | FDiv(a, b) ->
    let (a', b') = typecheck_binop et ctx (Types.Float) a b in
    FDiv_t(a', b')
  | Eq(a, b) ->
    let a' = typecheck_expr et ctx a in
    let b' = typecheck_expr et ctx b in
    let a_t = base_type_of @@ type_of a' in
    let b_t = base_type_of @@ type_of b' in
    unify_types "typecheck expr Eq" a_t b_t |> ignore;
    Eq_t(a', b')
  | Neq(a, b) ->
    let a' = typecheck_expr et ctx a in
    let b' = typecheck_expr et ctx b in
    let a_t = base_type_of @@ type_of a' in
    let b_t = base_type_of @@ type_of b' in
    unify_types "typecheck expr Neq" a_t b_t |> ignore;
    Neq_t(a', b')
  | Lt(a, b) ->
    let a' = typecheck_expr et ctx a in
    let b' = typecheck_expr et ctx b in
    let a_t = base_type_of @@ type_of a' in
    let b_t = base_type_of @@ type_of b' in
    unify_types "typecheck expr Lt" a_t b_t |> ignore;
    Lt_t(a', b')
  | Le(a, b) ->
    let a' = typecheck_expr et ctx a in
    let b' = typecheck_expr et ctx b in
    let a_t = base_type_of @@ type_of a' in
    let b_t = base_type_of @@ type_of b' in
    unify_types "typecheck expr Le" a_t b_t |> ignore;
    Le_t(a', b')
  | Gt(a, b) ->
    let a' = typecheck_expr et ctx a in
    let b' = typecheck_expr et ctx b in
    let a_t = base_type_of @@ type_of a' in
    let b_t = base_type_of @@ type_of b' in
    unify_types "typecheck expr Gt" a_t b_t |> ignore;
    Gt_t(a', b')
  | Ge(a, b) ->
    let a' = typecheck_expr et ctx a in
    let b' = typecheck_expr et ctx b in
    let a_t = base_type_of @@ type_of a' in
    let b_t = base_type_of @@ type_of b' in
    unify_types "typecheck expr Ge" a_t b_t |> ignore;
    Ge_t(a', b')
  | LAnd(a, b) ->
    let a' = typecheck_expr et ctx a in
    let b' = typecheck_expr et ctx b in
    let a_t = base_type_of @@ type_of a' in
    let b_t = base_type_of @@ type_of b' in
    print_endline @@ Printf.sprintf "DEBUG: typecheck LAnd: a: %s" (string_of_expr_t a');
    print_endline @@ Printf.sprintf "DEBUG: typecheck LAnd: b: %s" (string_of_expr_t b');
    unify_types "typecheck expr LAnd 1" Types.Bool a_t |> ignore;
    unify_types "typecheck expr LAnd 2" Types.Bool b_t |> ignore;
    LAnd_t(a', b')
  | LOr(a, b) ->
    let a' = typecheck_expr et ctx a in
    let b' = typecheck_expr et ctx b in
    let a_t = base_type_of @@ type_of a' in
    let b_t = base_type_of @@ type_of b' in
    unify_types "typecheck expr LOr 1" Types.Bool a_t |> ignore;
    unify_types "typecheck expr LOr 2" Types.Bool b_t |> ignore;
    LOr_t(a', b')
  | BNeg(e') ->
    let e'' = typecheck_expr et ctx e' in
    let e_t = base_type_of @@ type_of e'' in
    unify_types "typecheck expr BNeg" Types.Int e_t |> ignore;
    BNeg_t(e'')
  | BAnd(a, b) ->
    let a' = typecheck_expr et ctx a in
    let b' = typecheck_expr et ctx b in
    let a_t = base_type_of @@ type_of a' in
    let b_t = base_type_of @@ type_of b' in
    unify_types "typecheck expr BAnd 1" Types.Int a_t |> ignore;
    unify_types "typecheck expr BAnd 2" Types.Int b_t |> ignore;
    BAnd_t(a', b')
  | BOr(a, b) ->
    let a' = typecheck_expr et ctx a in
    let b' = typecheck_expr et ctx b in
    let a_t = base_type_of @@ type_of a' in
    let b_t = base_type_of @@ type_of b' in
    unify_types "typecheck expr BOr 1" Types.Int a_t |> ignore;
    unify_types "typecheck expr BOr 2" Types.Int b_t |> ignore;
    BOr_t(a', b')
  | BXOr(a, b) ->
    let a' = typecheck_expr et ctx a in
    let b' = typecheck_expr et ctx b in
    let a_t = base_type_of @@ type_of a' in
    let b_t = base_type_of @@ type_of b' in
    unify_types "typecheck expr BXOr 1" Types.Int a_t |> ignore;
    unify_types "typechcek expr BXOr 2" Types.Int b_t |> ignore;
    BOr_t(a', b')
  | BShiftLeft(a, b) ->
    let a' = typecheck_expr et ctx a in
    let b' = typecheck_expr et ctx b in
    let a_t = base_type_of @@ type_of a' in
    let b_t = base_type_of @@ type_of b' in
    unify_types "typecheck expr BShiftLeft 1" Types.Int a_t |> ignore;
    unify_types "typecheck expr BShiftLeft 2" Types.Int b_t |> ignore;
    BShiftLeft_t(a', b')
  | BShiftRight(a, b) ->
    let a' = typecheck_expr et ctx a in
    let b' = typecheck_expr et ctx b in
    let a_t = base_type_of @@ type_of a' in
    let b_t = base_type_of @@ type_of b' in
    unify_types "typecheck expr BShiftRight 1" Types.Int a_t |> ignore;
    unify_types "typecheck expr BShiftRight 2" Types.Int b_t |> ignore;
    BShiftRight_t(a', b')
  | IndexAccess(a, i) ->
    let a' = typecheck_expr et ctx a in
    let i' = typecheck_expr et ctx i in
    let t_i = base_type_of @@ type_of i' in
    unify_types "typecheck expr IndexAccess 1" (Types.Int) t_i |> ignore;
    (match unify_types "typecheck expr IndexAccess 2" (Types.UnsizedArray (Poly 1000000)) (type_of a') with
    | UnsizedArray(t) -> IndexAccess_t(t, a', i')
    | _ -> failwith "Impossible case"
    )
  | MemberAccess(s, name) ->
    let s' = typecheck_expr et ctx s in
    let s_t = type_of s' in
    let s_tt = lookup_type s_t ctx in
    (match s_tt with
    | Struct(_, l) ->
      let member = List.assoc_opt name l in
      (match member with
      | Some(t') -> MemberAccess_t(t', s', name)
      | None -> failwith @@ Printf.sprintf "%s is not a member of a struct!" name
      )
    | _ -> failwith "Can't do member access on anything other than struct!"
    )
  | Match(v, branches) -> 
    let v' = typecheck_expr et ctx v in
    let t_v = Types.base_type_of @@ type_of v' in
    (* to typecheck the pattern matching, need to *)
    (* - ensure the type correctness *)
    (* - check exhaustiveness *)
    (* - ensure the same types of branches *)
    let is_same_constructor a b =
      match a, b with
      | Any_t, Any_t
      | Unit_t, Unit_t
      | Bool_t(true), Bool_t(true)
      | Bool_t(false), Bool_t(false)
      | Struct_t, Struct_t -> true
      | Ident_t(t1, _), Ident_t(t2, _) -> t1 = t2
      | Array_t(t1, a1), Array_t(t2, a2)
      | Tuple_t(t1, a1), Tuple_t(t2, a2) ->
        (t1 = t2) && (List.length a1 = List.length a2)
      | Variant_t(t1, name1, members1), Variant_t(t2, name2, members2) ->
        (t1 = t2) && (name1 = name2) && (List.length members1 = List.length members2)
      | AnonStruct_t(t1, a1), AnonStruct_t(t2, a2) ->
        (t1 = t2) && (List.map fst a1 = List.map fst a2)
      | _, _ -> false
    in
    let check_pattern_types t_v branches =
      let check_pattern_type t_v pat =
        let open Types in
        let t_pat = Types.base_type_of @@ type_of pat in
        Types.unify_types "typecheck expr Match" t_pat t_v |> ignore
      in
      branches
      |> List.map fst
      |> List.map (check_pattern_type t_v)
      |> ignore
    in
    let check_pattern_exhaustiveness t_v branches =
      (* can ensure exhaustiveness in case of *)
      (* - Unit *)
      (* - Bool *)
      (* - Char *)
      (* - Sumtype *)
      (* - Any *)

      (* There must be some unifying algorithm *)
      (* http://moscova.inria.fr/~maranget/papers/warn/warn.pdf *)
      (* https://www.cs.tufts.edu/~nr/cs257/archive/luc-maranget/jun08.pdf *)
      (* But I didn't understand it... *)
      let is_ident = function
        | Ident_t(_, _) -> true
        | _ -> false
      in
      let is_any = function
        | Any_t -> true
        | _ -> false
      in
      let is_legal_pattern = function
        | Unit_t
        | Bool_t(_)
        | Char_t(_)
        | Int_t(_)
        | Float_t(_)
        | String_t(_)
        | Array_t(_, _)
        | Tuple_t(_, _)
        (* identifier is a same as named any *)
        | Ident_t(_, _)
        | Struct_t
        | AnonStruct_t(_, _)
        | Variant_t(_, _, _)
        | Any_t -> true
        | _ -> false
      in
      let rec useful p_mat_type p_mat q_vec : bool =
        let rec specialize constructor patterns =
          let basic_value p =
            p
            |> List.filter 
            (fun (first_pat :: _) -> (constructor = first_pat) || (is_any first_pat) || (is_ident first_pat))
            |> List.map (List.tl)
          in
          match constructor with
          | Unit_t
          | Bool_t(_)
          | Char_t(_)
          | Int_t(_)
          | Float_t(_)
          | String_t(_)
          | Ident_t(_, _)
          | Struct_t
          | Any_t -> basic_value patterns
          | Array_t(_, a) ->
            patterns
            |> List.map (fun (first_pat :: rest_of_pat) ->
                match (first_pat) with
                | Array_t(_, a2) -> if (List.length a = List.length a2) then Some(a2 @ rest_of_pat) else None
                | Ident_t(_, _) -> Some((List.init (List.length a) (fun _ -> Any_t)) @ rest_of_pat)
                | Any_t -> Some((List.init (List.length a) (fun _ -> Any_t)) @ rest_of_pat)
                | _ -> None
            )
            |> List.filter (fun x -> x <> None)
            |> List.map (fun (Some(x)) -> x)
          | Tuple_t(_, t) ->
            patterns
            |> List.map (fun (first_pat :: rest_of_pat) ->
                match (first_pat) with
                | Tuple_t(_, t2) -> if (List.length t = List.length t2) then Some(t2 @ rest_of_pat) else None 
                | Ident_t(_, _) -> Some((List.init (List.length t) (fun _ -> Any_t)) @ rest_of_pat)
                | Any_t -> Some((List.init (List.length t) (fun _ -> Any_t)) @ rest_of_pat)
                | _ -> None
            )
            |> List.filter (fun x -> x <> None)
            |> List.map (fun (Some(x)) -> x)
          | AnonStruct_t(_, fields) -> 
            patterns
            |> List.map (fun (first_pat :: rest_of_pat) ->
                match (first_pat) with
                | AnonStruct_t(_, a2) -> 
                  if (List.length fields = List.length a2) then 
                    Some((List.map snd a2) @ rest_of_pat) 
                  else 
                    None
                | Ident_t(_, _) -> Some((List.init (List.length fields) (fun _ -> Any_t)) @ rest_of_pat)
                | Any_t -> Some((List.init (List.length fields) (fun _ -> Any_t)) @ rest_of_pat)
                | _ -> None
            )
            |> List.filter (fun x -> x <> None)
            |> List.map (fun (Some(x)) -> x)
          | Variant_t(_, varname, values) ->
            patterns
            |> List.map (fun (first_pat :: rest_of_pat) ->
                match (first_pat) with
                | Variant_t(_, varname2, values2) ->
                  if varname = varname2 then Some(values2 @ rest_of_pat)
                  else None
                | Ident_t(_, _) -> Some((List.init (List.length values) (fun _ -> Any_t)) @ rest_of_pat)
                | Any_t -> Some((List.init (List.length values) (fun _ -> Any_t)) @ rest_of_pat)
                | _ -> None
            )
            |> List.filter (fun x -> x <> None)
            |> List.map (fun (Some(x)) -> x)
          | _ -> failwith "Illegal value when pattern matching!"
        in
        let specialize_single c v =
          match (specialize c [v]) with
          | [v'] -> v'
          | _ -> 
            failwith 
            "Typecheck error: expected specialize single to return a single vector, got something else instead..."
        in
        let rec default patterns =
          match patterns with
          | [] -> failwith "default: impossible case"
          | (Any_t :: rest_of_pat) :: rest -> rest_of_pat :: (default rest)
          | (Ident_t(_, _) :: rest_of_pat) :: rest -> rest_of_pat :: (default rest)
          | _ :: rest -> default rest
        in
        match q_vec with
        | [] -> (match p_mat with
                | [] -> false
                | _ -> true)
        | (Unit_t                 as hd) :: rest_q_vec
        | (Bool_t(_)              as hd) :: rest_q_vec
        | (Char_t(_)              as hd) :: rest_q_vec
        | (Int_t(_)               as hd) :: rest_q_vec
        | (Float_t(_)             as hd) :: rest_q_vec
        | (String_t(_)            as hd) :: rest_q_vec
        | (Ident_t(_, _)          as hd) :: rest_q_vec
        | (Struct_t               as hd) :: rest_q_vec
        | (Array_t(_, _)          as hd) :: rest_q_vec
        | (Tuple_t(_, _)          as hd) :: rest_q_vec
        | (AnonStruct_t(_, _)     as hd) :: rest_q_vec
        | (Variant_t(_, _, _) as hd) :: rest_q_vec ->
            useful (List.tl p_mat_type) (specialize hd p_mat) (specialize_single hd q_vec)
        | Any_t :: rest_q_vec -> 
          (* Here's the difficult part *)
          let rec complete_constructors t =
            let open Types in
            (match t with
            | Unit -> [Unit_t]
            | Bool -> [Bool_t(true); Bool_t(false)]
            | Char -> []
            | Int -> []
            | Float -> []
            | String -> []
            | CustomType(name) -> 
              let custom_types = custom_types_of ctx in
              (match List.assoc_opt name custom_types with
              | Some(tt) -> complete_constructors tt
              | None -> failwith @@ Printf.sprintf "Typecheck error: type %s is not defined!" name
              )
            | SizedArray(at, n) -> 
              let c = complete_constructors at in
              List.init (List.length c) (fun x -> 
                let c' = List.nth c x in
                Array_t(t, List.init n (fun _ -> c'))) (* officially becoming lost here *)
            | UnsizedArray(at) -> complete_constructors at
            | Struct(_, fields) -> (* not sure what to do here *)
              [AnonStruct_t(t, List.map (fun (name, x) -> (name, Any_t)) fields)] (* Incomplete *)
            | Tuple(t') -> [Tuple_t(t, List.init (List.length t') (fun _ -> Any_t))]
            | Sumtype(sumtype_name, var_name) -> 
              let custom_types = custom_types_of ctx in
              (match List.assoc_opt sumtype_name custom_types with
              | Some((SumtypeDef(_) as st)) -> complete_constructors st
              | Some(other_t) -> failwith @@ 
                Printf.sprintf 
                  "Typecheck error: expected %s to be a sumtype, got %s instead"
                  (sumtype_name)
                  (Types.string_of_ground_type other_t)
              | None -> failwith @@ Printf.sprintf "Type %s is not defined!" sumtype_name
              )
            | SumtypeDef(st_def) ->
              List.map 
                (fun (name, values) -> Variant_t(t, name, List.init (List.length values) (fun _ -> Any_t)))
                st_def
            | Modetype(modetype_name) -> 
              let custom_types = custom_types_of ctx in
              (match List.assoc_opt modetype_name custom_types with
              | Some(ModetypeDef(modet_name, modetypes)) -> 
                  complete_constructors (Types.ModetypeDef(modet_name, modetypes))
              | Some(t') -> failwith @@ 
                Printf.sprintf "Typecheck error: expected %s to be a modetype, instead got %s"
                (modetype_name)
                (Types.string_of_ground_type t')
              | None -> failwith @@ Printf.sprintf "Type %s is not defined!" modetype_name
              )
            | ModetypeDef(_) -> failwith "TODO: modetype def ..."
            | AnyType -> [Any_t]
            | _ -> failwith @@ 
              Printf.sprintf "Typecheck error: %s is not supported for pattern matching!"
              (Types.string_of_ground_type t)
            )
          in
          let cur_tp = List.hd p_mat_type in
          let first_constructors = List.map (List.hd) p_mat in
          let unique_first_constructors = Util.unique_naive first_constructors in
          let compl_const = complete_constructors cur_tp in
          let is_complete_pat compl_const uniq_const =
            let is_subset l1 l2 =
              let rec loop l =
                match l with
                | [] -> true
                | x :: rest ->
                  (match List.find_opt (fun y -> is_same_constructor x y) l2 with
                  | Some(_) -> loop rest
                  | None -> false)
              in
              loop l1
            in
            match compl_const with
            | [] -> false (* There are infinite number of constructors... *)
            | _ -> (is_subset compl_const uniq_const) && (is_subset uniq_const compl_const)
          in
          (match is_complete_pat compl_const unique_first_constructors with
          | true -> List.fold_left (fun a b -> a || b) false @@ 
            List.map 
              (fun c -> 
                useful (List.tl p_mat_type) (specialize c p_mat) (specialize_single c q_vec)) 
              compl_const
          | false -> useful (List.tl p_mat_type) (default p_mat) (rest_q_vec)
          )
        | _ -> failwith "TODO: implement u_rec wildcard 2" (* concrete value or constructor *)
      in
      (* Need to construct p_mat_type *)
      (* Construct it from q_vec *)
      let pats = List.map fst branches in
      let pats = List.map (fun x -> [x]) pats in
      let p_mat_type = [t_v] in (* From initial pattern matching on match statement *)
      match (useful p_mat_type pats [Any_t]) with
      | true -> false
      | false -> true
    in
    let ensure_branch_valid_patterns branches =
      let rec check branch =
        match branch with
        | Any_t -> true
        | Unit_t -> true
        | Bool_t(true) -> true
        | Bool_t(false) -> true
        | Char_t(_) -> true
        | Int_t(_) -> true
        | Float_t(_) -> true
        | String_t(_) -> true
        | Array_t(_, values) ->
          values
          |> List.map check
          |> List.fold_left (fun a b -> a && b) true
        | Tuple_t(_, values) ->
          values
          |> List.map check
          |> List.fold_left (fun a b -> a && b) true
        | Ident_t(_, _) -> true
        | Variant_t(_, _, values) ->
          values
          |> List.map check
          |> List.fold_left (fun a b -> a && b) true
        | AnonStruct_t(_, values) ->
          values
          |> List.map snd
          |> List.map check
          |> List.fold_left (fun a b -> a && b) true
        | _ -> false
      in
      let rec loop l =
        match l with
        | [] -> ()
        | (x, _) :: rest ->
          (* check valid patterns *)
          (match check x with
          | true -> loop rest
          | false -> failwith "Typecheck error: some non-pattern is used in pattern matching!")
      in
      loop branches
    in
    let ensure_valid_name_bindings_in_patterns branches =
      let check_uniq values =
        let identifiers = List.filter (function | Ident_t(_, _) -> true | _ -> false) values in
        let ids = List.map (fun (Ident_t(t, name)) -> (t, name)) identifiers in
        let rec loop l =
          match l with
          | [] -> true
          | (t, x) :: rest ->
            let     vars =     vars_of ctx in
            let glb_vars = glb_vars_of ctx in
            let mut_vars = mut_vars_of ctx in
            let f = List.assoc_opt x in
            (match f vars, f glb_vars, f mut_vars with
            | None, None, None -> loop rest
            | _, _, _ -> false)
        in
        let names = List.map snd ids in
        let uniq_identifiers = Util.unique_naive names in
        (loop ids) && (Util.sets_equal (=) names uniq_identifiers)
      in
      let check pat =
        match pat with
        | Any_t -> true
        | Unit_t -> true
        | Bool_t(true) -> true
        | Bool_t(false) -> true
        | Char_t(_) -> true
        | Int_t(_) -> true
        | Float_t(_) -> true
        | String_t(_) -> true
        | Ident_t(_, _) -> true
        (* Need to focus on these four *)
        (* Basically need to ensure that the names that are used are unique *)
        (* And don't conflict with the existing context *)
        | Array_t(_, values)
        | Tuple_t(_, values)
        | Variant_t(_, _, values) -> check_uniq values
        | AnonStruct_t(_, values) ->
          values
          |> List.map snd
          |> check_uniq
        | _ -> failwith "ensure_valid_bindings_in_patterns: check: Impossible case"
      in
      let rec loop l =
        match l with
        | [] -> ()
        | (x, _) :: rest ->
          (match check x with
          | true -> loop rest
          | false -> failwith "Typecheck error: some name-bindings are used incorrectly")
      in
      loop branches
    in
    let check_branch_types branches =
      let check pat v =
        let rec bind_identifiers t_v pat vars =
          match pat with
          | Any_t
          | Unit_t
          | Bool_t(true)
          | Bool_t(false)
          | Char_t(_)
          | Int_t(_)
          | Float_t(_)
          | String_t(_) -> vars
          | Ident_t(t', name) -> 
            (match (unify_types "typecheck expr Match branch 1" t' t_v |> ignore) with
            | () -> (name, t') :: vars )
            (* | false -> failwith "Typecheck error: something wrong happened when binding variables...") *)
          | Array_t(SizedArray(t', _), values)
          | Array_t(UnsizedArray(t'), values) ->
            List.fold_left (fun res x -> bind_identifiers t' x res) [] values
          | Array_t(_, _) -> failwith "Typecheck error: expected array to have SizedArray or UnsizedArray types!"
          | Tuple_t(t', values) ->
            (match t' with
            | Tuple(t') ->
              List.fold_left (fun res (t', v) -> bind_identifiers t' v res) [] @@ List.combine t' values
            | _ -> failwith "Typecheck error: expected tuple to have a Tuple type!")
          | Variant_t(_, varname, values) -> 
            (match t_v with
            | SumtypeDef(variants) ->
              (match List.assoc_opt varname variants with
              | Some(t') ->
                List.fold_left (fun res (t', v) -> bind_identifiers t' v res) [] @@ List.combine t' values
              | None -> failwith @@ 
                Printf.sprintf "Typecheck error: %s is not found in sumtype definition!" varname)
            | _ -> failwith "Typecheck error: expected a sumtype definition!")
          | AnonStruct_t(t', values) ->
            (match t' with
            | Struct(_, fields) ->
              let types = List.map snd fields in
              let values = List.map snd values in
              List.fold_left (fun res (t', v) -> bind_identifiers t' v res) [] @@ List.combine types values
            | _ -> failwith "Typecheck error: expected AnonStruct to have a Struct type!")
          | _ -> failwith "check_branch_types: check: bind_identifiers: Impossible case"
        in
        let vars = vars_of ctx in
        let new_vars = bind_identifiers (type_of pat) pat vars in
        let new_ctx = ctx_of_vars vars ctx in
        let v' = typecheck_expr et new_ctx v in
        let t_v = type_of v' in
        (t_v, v')
      in
      let rec loop ans l =
        match l with
        | [] -> ans
        | (pat, v) :: rest ->
          let (t_v, v') = check pat v in
          loop (ans @ [(t_v, pat, v')]) rest
      in
      let branches' = loop [] branches in
      let t_branches = List.map (fun (a, _, _) -> a) branches' in
      let t_final = List.fold_left (fun a b -> unify_types "typecheck expr Match branch 2" a b) Types.AnyType t_branches in
      (t_final, branches')
    in
    (* Done: Need to ensure that the patterns used in pattern matching are valid patterns *)
    (* Done: Need to check the exhaustiveness of the patterns *)
    (* Done: Need to ensure that the name bindings are properly used in patterns and branches *)
    (* Need to ensure that the return types of the branches are valid types *)
    (* Done: Need to ensure that the return types of the branchse are the same *)
    (* Need to bring them all together *)
    let branches' = List.map (fun (a, b) ->
      (typecheck_expr (Types.AnyType) ctx a, b)
    ) branches in
    check_pattern_types t_v branches';
    check_pattern_exhaustiveness t_v branches'; 
    ensure_branch_valid_patterns branches';
    let (t_final, branches'') = check_branch_types branches' in
    Match_t(t_final, t_v, v', branches'')
  | Await(s) ->
    (* TODO: need to ensure this is in the promise or task *)
    let promise_types = promise_types_of ctx in
    let t_opt = List.assoc_opt s promise_types in
    (match t_opt with
    | Some(Unit) -> failwith @@ Printf.sprintf "Typecheck error: cannot await a promise with type Unit! %s" s
    | Some(t') -> Await_t(t', await_c (), s, pro_c ())
    | None -> failwith @@ Printf.sprintf "No promise with name %s is defined!" s
    )
  | Raise(s, args) ->
    (* TODO: need to ensure this is in the promise or task *)
    let args' = List.map (typecheck_expr et ctx) args in
    let custom_types = custom_types_of ctx in
    let eff_def = List.assoc_opt s custom_types in
    (match eff_def with
    | Some(EffectDef(effname, expected_args)) ->
      let args_t = List.map (fun x -> Types.base_type_of @@ type_of x) args' in
      (match args_t = expected_args with
      | true -> (Raise_t(eff_c (), s, args'))
      | false -> failwith @@ Printf.sprintf "Typecheck error: expected [%s], but instead got [%s]"
        (String.concat "; " @@ List.map Types.string_of_ground_type expected_args)
        (String.concat "; " @@ List.map Types.string_of_ground_type args_t))
    | _ -> failwith @@ Printf.sprintf "Typecheck error: %s effect is not defined!" s)
  (*
  | Let(name, m, n) ->
    failwith "TODO: typecheck let expressions"
  | LetMut(name, m, n) ->
    failwith "TODO: typecheck let mut expressions"
  *)
  | If(c, t, f) ->
    let c' = typecheck_expr et ctx c in
    let t_c = type_of c' in
    (match Types.base_type_of t_c with
    | Bool ->
      let t' = typecheck_expr et ctx t in
      let f' = typecheck_expr et ctx f in
      let t_t = type_of t' in
      let t_f = type_of f' in
      unify_types "typecheck expr If" (Types.base_type_of t_t) (Types.base_type_of t_f) |> ignore;
      If_t(Types.base_type_of t_t, c', t', f')
    | _ -> failwith @@ Printf.sprintf 
      "Typecheck error: expected bool for conditional statement, instead got %s" 
      (Types.string_of_ground_type t_c))
  | While(c, b) ->
    let c' = typecheck_expr et ctx c in
    let t_c = type_of c' in
    let b' = typecheck_expr et ctx b in
    unify_types "typecheck expr While" Types.Bool t_c |> ignore;
    let t_b = type_of b' in
    While_t(t_b, c', b')
  | ForTo(name, from, _to, b) ->
    let vars = vars_of ctx in
    (match List.assoc_opt name vars with
    | Some(_) -> failwith @@ Printf.sprintf "Typecheck error: the %s is already taken" name
    | None ->
      let from' = typecheck_expr et ctx from in
      let _to' = typecheck_expr et ctx _to in
      let t_from = type_of from' in
      let t_to = type_of _to' in
      unify_types "typecheck expr ForTo 1" (Types.base_type_of t_from) (Types.Int) |> ignore;
      unify_types "typecheck expr ForTo 2" (Types.base_type_of t_to) (Types.Int) |> ignore;
      let new_vars = (name, Types.Int) :: vars in
      let new_ctx = ctx_of_vars new_vars ctx in
      let b' = typecheck_expr et new_ctx b in
      let t_b = Types.base_type_of @@ type_of b' in
      ForTo_t(t_b, name, from', _to', b')
    )
  | ForDownTo(name, from, _to, b) ->
    let vars = vars_of ctx in
    (match List.assoc_opt name vars with
    | Some(_) -> failwith @@ Printf.sprintf "Typecheck error: the %s is already taken" name
    | None ->
      let from' = typecheck_expr et ctx from in
      let _to' = typecheck_expr et ctx _to in
      let t_from = type_of from' in
      let t_to = type_of _to' in
      unify_types "typecheck expr ForDownTo 1" (Types.base_type_of t_from) (Types.Int) |> ignore;
      unify_types "typecheck expr ForDownTo 2" (Types.base_type_of t_to) (Types.Int) |> ignore;
      let new_vars = (name, Types.Int) :: vars in
      let new_ctx = ctx_of_vars new_vars ctx in
      let b' = typecheck_expr et new_ctx b in
      let t_b = Types.base_type_of @@ type_of b' in
      ForTo_t(t_b, name, from', _to', b')
    )
  | ForEach(name, arr, b) ->
    let vars = vars_of ctx in
    (match List.assoc_opt name vars with
    | Some(_) -> failwith @@ Printf.sprintf "Typecheck error: %s is already taken" name
    | None ->
      let arr' = typecheck_expr et ctx arr in
      let t_arr = Types.base_type_of @@ type_of arr' in
      (match t_arr with
      | Types.UnsizedArray(t')
      | Types.SizedArray(t', _) ->
        let new_vars = (name, t') :: vars in
        let new_ctx = ctx_of_vars vars ctx in
        let b' = typecheck_expr et new_ctx b in
        let t_b = Types.base_type_of @@ type_of b' in
        ForEach_t(t_b, name, arr', b')
      | _ -> failwith "Typecheck error: Can't do foreach on anything other than array")
    )
  | ForEachIndex(name, i, arr, b) ->
    let vars = vars_of ctx in
    (match List.assoc_opt name vars with
    | Some(_) -> failwith @@ Printf.sprintf "Typecheck error: %s is already taken" name
    | None ->
      (match List.assoc_opt i vars with
      | Some(_) -> failwith @@ Printf.sprintf "Typecheck error: %s is already taken" i
      | None ->
        let arr' = typecheck_expr et ctx arr in
        let t_arr = Types.base_type_of @@ type_of arr' in
        (match t_arr with
        | Types.UnsizedArray(t')
        | Types.SizedArray(t', _) ->
          let new_vars = (name, t') :: (i, Types.Int) :: vars in
          let new_ctx = ctx_of_vars vars ctx in
          let b' = typecheck_expr et new_ctx b in
          let t_b = Types.base_type_of @@ type_of b' in
          ForEachIndex_t(t_b, name, i, arr', b')
        | _ -> failwith "Typecheck error: Can't do foreach on anything other than array")
      )
    )
  | Variant(name, members) ->
    let custom_types = custom_types_of ctx in
    (match List.assoc_opt name custom_types with
    | Some(Sumtype(sumtype_name, varname)) -> (match List.assoc_opt sumtype_name custom_types with
      | Some(SumtypeDef(members_types)) -> 
        let members' = List.map (typecheck_expr et ctx) members in
        let t_members = List.map (fun x -> Types.base_type_of @@ type_of x) members' in
        let test_1 = List.filter (fun x -> x <> Any_t) members' in
        assert (List.length test_1 = List.length members');
        (match List.assoc_opt name members_types with
        | Some(members_types') ->
            List.combine t_members members_types' |>
            List.iter (fun (a, b) -> Types.unify_types "typecheck expr Variant" a b |> ignore);
            Variant_t(Types.Sumtype(sumtype_name, name), name, members')
        | None -> failwith @@ Printf.sprintf "Typecheck error: %s is not a valid variant!" name)
      | Some(t') -> failwith @@ 
        Printf.sprintf 
          "Typecheck error: expected a sumtype definition, instead got %s" 
          (Types.string_of_ground_type t')
      | None -> failwith @@ Printf.sprintf "Typecheck error: sumtype %s is not defined!" sumtype_name)
    | Some(t') -> failwith @@ 
      Printf.sprintf "Typecheck error: expected sumtype, instead got %s" (Types.string_of_ground_type t')
    | None -> failwith @@ Printf.sprintf "Typecheck error: undefined type constructor %s" name)
    (* failwith "TODO: typecheck variant" *)
  | Struct -> Struct_t
  | AnonStruct(_) -> 
    (* Need to typecheck all of the elements and check which struct matches the fields *)
    failwith "TODO: typecheck anon struct"
  | FuncCall(fname, args) -> 
    (* Need to typecheck the args and check the function *)
    let custom_types = custom_types_of ctx in
    (match List.assoc_opt fname custom_types with
    | Some(Function(args_types, rettype)) -> (* Perform the typechecking*)
      let args' = List.map (typecheck_expr et ctx) args in
      let args_t = List.map (fun x -> Types.base_type_of @@ type_of x) args' in
      let rec loop ans a b =
        match a, b with
        | [], [] -> ans
        | (x :: x_rest), (y :: y_rest) ->
          let t = Types.unify_types "typecheck expr FuncCall" x y in
          loop (ans @ [t]) x_rest y_rest
        | _, _ -> failwith @@ Printf.sprintf 
        "Typecheck error: the number of argments to function %s doesn't match!" fname
      in
      let new_arg_types = loop [] args_t args_types in
      (* Update the variable types here if possible *)
      FuncCall_t(rettype, fname, args')
    | Some(_) -> failwith @@ Printf.sprintf 
      "Typecheck error: expected %s to be a function, got something else instead!" fname
    | None -> failwith @@ Printf.sprintf "Typecheck error: function %s is not found!" fname)
  | Block(sl) -> 
    let (sl', _, t) = List.fold_left (fun (cur_sl, cur_ctx, _) x ->
      let (x', new_ctx, t) = typecheck_stmt et cur_ctx x in
      (cur_sl @ [x'], new_ctx, t)
    ) ([], ctx, Unit) sl in
    Block_t(t, sl')
  (*
  | _ -> failwith 
  @@ Printf.sprintf "Unhandled case: %s" 
  @@ Ast.string_of_expr e
  *)


and typecheck_stmt et ctx (s: Ast.stmt) =
  let not_occupied name =
    let assoc_opt = List.assoc_opt in
    let free v l = 
      (match assoc_opt v l with
      | None -> true
      | Some(_) -> false)
    in
    let mut_vars = mut_vars_of ctx in
    let glb_vars = mut_vars_of ctx in
    let vars = vars_of ctx in
    let free' = free name in
    free' vars && free' mut_vars && free' glb_vars
  in
  let open Ast in
  match s with
  (* Need to ensure that is used within the for or while loop *)
  (* Need to ensure that the with variants have consistent types *)
  | Break ->
    if is_in_loop ctx then 
      (Break_t, ctx, Types.Unit)
    else
      failwith "Typecheck error: using break outside of loop"
  | BreakWith(e) ->
    if is_in_loop ctx then
      let e' = typecheck_expr et ctx e in
      let t = type_of e' in
      (BreakWith_t(t, e'), ctx, Types.Unit)
    else
      failwith "Typecheck error: using break_with outside of loop"
  | Continue ->
    if is_in_loop ctx then
      (Continue_t, ctx, Types.Unit)
    else
      failwith "Typecheck error: using continue outside of loop"
  | ContinueWith(e) ->
    if is_in_loop ctx then
      let e' = typecheck_expr et ctx e in
      let t = type_of e' in
      (ContinueWith_t(t, e'), ctx, Types.Unit)
    else
      failwith "Typecheck error: using continue_with outside of loop"
  | EmptyReturn -> 
    if is_in_func ctx || is_in_promise ctx then
      (EmptyReturn_t, ctx, Types.Unit)
    else
      failwith "Typecheck error: using empty return outside of function or promise"
  | Return e ->
    if is_in_func ctx || is_in_promise ctx then
      let e' = typecheck_expr et ctx e in
      let t = type_of e' in
      unify_types "typecheck stmt Return" et t |> ignore;
      match is_in_func ctx, is_in_promise ctx with
      | true, false -> (Return_t e', ctx, t)
      | false, true -> (Dispatch_t e', ctx, t)
      | true, true -> failwith "typecheck_stmt: Return: Impossible case 1"
      | false, false -> failwith "typecheck_stmt: Return: Impossible case 2"
      (* (Return_t e', ctx, t) *)
    else
      failwith "Typecheck error: using return outside of function or promise"
  | Expr e ->
    let e' = typecheck_expr et ctx e in
    let t = type_of e' in
    (Expr_t e', ctx, t)
  | Global name ->
    let glb_vars = glb_vars_of ctx in
    (match List.assoc_opt name glb_vars with
    | None ->
      let globals = globals_of ctx in
      (match List.assoc_opt name globals with
      | Some(t) ->
        (* Make sure that the name is not occupied *)
        let (t : Types.ground_type) = Types.Global(t) in
        let vars = vars_of ctx in
        let mut_vars = mut_vars_of ctx in
        (match List.assoc_opt name vars, List.assoc_opt name mut_vars with
        | None, None ->
          (* The name is not taken, define the global var *)
          let new_glb_vars = (name, t) :: glb_vars in
          let new_ctx = ctx_of_glb_vars new_glb_vars ctx in
          (Global_t(name), new_ctx, t)
        | _, _ ->
          (* The name is taken, fail for now, in the future can have more complicated tests *)
          failwith @@ 
          Printf.sprintf "Typecheck error: the name %s is already taken by either let or let mut bindings" name
        )
      | None -> failwith @@ Printf.sprintf "Typecheck error: the global variable %s is not globally defined" name
      )
    | Some(_) -> failwith @@ Printf.sprintf "Typecheck error: redefined global var %s" name)
  | Assign(a, b) ->
    (* Need to ensure that the both lvalue and rvalue typecheck *)
    (* Need to ensure that the left value is lvalue *)
    (* Need to ensure that their types are the same *)
    (* Need to ensure that the lvalue is mutable *)
    let rec is_lvalue e =
      match e with
      | Ident_t(_) -> true
      | IndexAccess_t(_, i, _) -> is_lvalue i
      | MemberAccess_t(_, i, _) -> is_lvalue i
      | _ -> false
    in
    let a' = typecheck_expr et ctx a in
    let b' = typecheck_expr et ctx b in
    let t_a = type_of a' in
    let t_b = type_of b' in
    unify_types "typecheck stmt Assign" (Types.base_type_of t_a) (Types.base_type_of t_b) |> ignore;
    (match Types.is_mutable t_a || Types.is_global t_a with
    | true ->
      (match is_lvalue a' with
      | true -> (Assign_t(a', b'), ctx, Types.Unit)
      | false -> failwith "Typecheck error: can't assign to a non-lvalue!")
    | false -> failwith "Typecheck error: can't assign to a non-mutable value!")
  | Let(name, v) -> 
    (* Need to support shadowing, not supported at the moment *)
    let v' = typecheck_expr et ctx v in
    let t_v = type_of v' in
    (match not_occupied name with
    | false -> failwith @@ Printf.sprintf "Typecheck error: %s conflicts with already defined variable in this context" name
    | true -> 
      let p = (name, t_v) in
      let new_vars = p :: (vars_of ctx) in
      let new_ctx = ctx_of_vars new_vars ctx in
      (Let_t(name, v'), new_ctx, t_v)
    )
  | LetMut(name, v) ->
    let v' = typecheck_expr et ctx v in
    let t_v = Types.Mutable(type_of v') in
    (match not_occupied name with
    | false -> failwith @@ Printf.sprintf "Typecheck error: %s conflicts with already defined variable in this context" name
    | true -> 
      let p = (name, t_v) in
      let new_mut_vars = p :: (mut_vars_of ctx) in
      let new_ctx = ctx_of_mut_vars new_mut_vars ctx in
      (LetMut_t(name, v'), new_ctx, t_v)
    )
  | ModeSwitch(name) -> 
    (* Very interesting case *)
    (* Modes exist in the global scope *)
    (* However, there's a need for a controlled *)
    (* mode switching *)

    (* Need to ensure valid mode name *)
    (* Need to ensure that the mode can be switched from the context *)

    (* TODO: ensure the correctness of the typechecking context *)
    (* Making a lot of assumptions here *)
    let mode_type = mode_type_of ctx name in
    let custom_types = custom_types_of ctx in
    (match List.assoc_opt mode_type custom_types with
    | Some(ModetypeDef(modetype_name, modes)) ->
      (match List.find_opt (fun x -> x = name) modes with
      | Some(_) -> (ModeSwitch_t(name), ctx, Types.Unit)
      | None -> failwith @@ Printf.sprintf "Typecheck error: the mode %s is not a part of %s mode type" name mode_type)
    | Some(_) -> failwith @@ Printf.sprintf "Typecheck error: expected a definition of mode type, instead got something else"
    | None -> failwith @@ Printf.sprintf "Typecheck error: modetype %s is not defined!" mode_type)

type t = {
  typechecked_code : Ast.t
}

let typecheck_add_user_funcs ctx code =
  let user_funcs_of code =
    let is_user_func = function
      | TypecheckFuncDecl _ -> true
      | _ -> false
    in
    let unpack_user_func = function
      | TypecheckFuncDecl(name, args, rettype) -> (name, args, rettype)
      | _ -> failwith "typecheck_add_user_funcs: Impossible case"
    in
    code
    |> List.filter is_user_func
    |> List.map unpack_user_func
  in
  let user_funcs = user_funcs_of code in
  let func_types = 
    List.map 
      (fun (name, args, rettype) -> (name, Types.Function(args, rettype))) 
      user_funcs 
  in
  let custom_types = custom_types_of ctx in
  let new_custom_types = func_types @ custom_types in
  let new_ctx = ctx_of_custom_types new_custom_types ctx in
  new_ctx

let typecheck_structs ctx code =
  (* This function will return a new ctx *)
  let is_false x = (x = false) in
  let structs_of code =
    let is_struct = function
      | StructDef _ -> true
      | _ -> false
    in
    let unpack_struct = function
      | StructDef(name, fields) -> (name, fields) 
      | _ -> failwith "Impossible case"
    in
    code
    |> List.filter is_struct
    |> List.map unpack_struct
  in
  let check_unique_names structs =
    structs |> check_unique_strings
  in
  let check_unique_field_names_of_struct fields =
    fields |> List.map fst |> check_unique_names
  in
  let rec check_valid_types_of_struct typestore fields =
    let is_valid_type t =
      let open Types in
      match t with
      | CustomType(s) -> StringSet.mem s typestore
      | Unit
      | Bool
      | Char
      | Int
      | Float
      | String -> true
      | _ -> false
    in
    List.(
      fields
      |> map snd
      |> map is_valid_type
      |> filter is_false) = []
  in
  let f acc (name, (fields : (string * Types.ground_type) list)) =
    assert (check_unique_field_names_of_struct fields = true);
    assert (check_valid_types_of_struct acc fields = true);
    StringSet.add name acc
  in
  let (structs : (string * (string * Types.ground_type) list) list) = structs_of code in
  let struct_names = structs |> List.map fst in
  assert (check_unique_names struct_names = true);
  let s = StringSet.empty in
  List.fold_left f s structs |> ignore;
  (* Add structs to the context *)
  let convert_structs_to_struct_types structs =
    List.map (fun (name, members) -> (name, Types.Struct(name, members))) structs
  in
  let structs' = convert_structs_to_struct_types structs in
  let custom_types = custom_types_of ctx in
  let new_custom_types = structs' @ custom_types in
  let new_ctx = ctx_of_custom_types new_custom_types ctx in
  new_ctx

let typecheck_effects ctx (code : Ast.toplevel list) =
  let effects_of code : ((string * Types.ground_type list) list) =
    let open Ast in
    let is_effect = function
      | EffectDef _ -> true
      | _ -> false
    in
    let unpack_effect e =
      match e with
      | EffectDef(name, fields) -> (name, fields) 
      | _ -> failwith "Impossible case"
    in
    code
    |> List.filter is_effect
    |> List.map unpack_effect
  in
  let check_unique_names effects =
    effects |> check_unique_strings
  in
  let check_valid_types ctx (effects : (string * Types.ground_type list) list) =
    let f = can_be_used_in_struct_definition "typecheck_effects: check_valid_types" ctx in
    List.iter (fun (_, fields) -> List.iter f fields) effects
  in
  let custom_types = custom_types_of ctx in
  let (effects : (string * (Types.ground_type list)) list) = effects_of code in
  let effect_names = effects |> List.map fst in
  let struct_names = List.map fst custom_types in
  (* assert (check_unique_names effect_names = true); *)
  (match Util.every_element_unique (effect_names @ struct_names) with
  | true -> 
    check_valid_types ctx effects;
    let effects' = List.map (fun (name, def) -> (name, Types.EffectDef(name, def))) effects in
    let new_custom_types = effects' @ custom_types in
    let new_ctx = ctx_of_custom_types new_custom_types ctx in
    new_ctx
  | false -> failwith "Typecheck error: some of the names of the effects conflict with other defined names!")
  (* assert (check_valid_types typestore effects = true); *)
  (* effects *)

let typecheck_interrupts ctx code =
  let extract_interrupts code =
    let is_interrupt = function
      | Interrupt(_, _) -> true
      | _ -> false
    in
    let unpack_interrupt = function
      | Interrupt(a, b) -> (a, b)
      | _ -> failwith "typecheck_interrupts: Impossible case"
    in
    code
    |> List.filter is_interrupt
    |> List.map unpack_interrupt
  in
  let interrupts = extract_interrupts code in
  let ensure_unique_names ctx interrupts =
    let custom_types = custom_types_of ctx in
    let interrupt_names = List.map fst interrupts in
    List.iter (fun x -> 
        match List.assoc_opt x custom_types with
        | None -> ()
        | Some(_) -> failwith @@ Printf.sprintf
          "Typecheck error: Interrupt name %s is already occupied!" x
      ) interrupt_names
  in
  let ensure_valid_effects ctx interrupts =
    let interrupt_effects = List.map snd interrupts in
    let custom_types = custom_types_of ctx in
    List.iter (fun x -> 
        match List.assoc_opt x custom_types with
        | Some(EffectDef(_, _)) -> ()
        | Some(_) -> failwith @@ Printf.sprintf "Typecheck error: %s is not an effect!" x
        | None -> failwith @@ Printf.sprintf "Typecheck error: effect %s is not defined!" x
      ) interrupt_effects
  in
  let update_ctx ctx interrupts =
    let custom_types = custom_types_of ctx in
    let interrupt_types = List.map (fun (a, _) -> (a, Types.InterruptType)) interrupts in
    let new_custom_types = interrupt_types @ custom_types in
    ctx
    |> ctx_of_interrupts interrupts
    |> ctx_of_custom_types new_custom_types
  in
  ensure_unique_names ctx interrupts;
  ensure_valid_effects ctx interrupts;
  update_ctx ctx interrupts
  (* failwith "TODO: typecheck_interrupts" *)

let typecheck_basic_funcs ctx (code : Ast.toplevel list) =
  let functions_of code =
    let is_func = function
      | FuncDef _ -> true
      | _ -> false
    in
    let unwrap_func = function
      | FuncDef(rettype, name, args, body) -> (rettype, name, args, body)
      | _ -> failwith "Impossible case"
    in
    code
    |> List.filter is_func
    |> List.map unwrap_func
  in
  let order_funcs (l: (Types.ground_type * Util.StringSet.elt * (Types.ground_type * string) list * Ast.stmt list) list) : string list =
    (* check for acyclicity and topoligcally sort *)
    let build_graph l =
      let funcs_and_body = List.map (fun (a, b, c, d) -> (b, d)) l in
      let rec called_funcs_of_expr e =
        let combine l = Util.concat @@ List.map called_funcs_of_expr l in
        match e with
        | FuncCall(n, args) ->
          n :: (combine args)
        | Match(v, branches) ->
          combine @@ 
            List.cons v @@ 
              List.map snd branches
        | Raise(_, l) -> combine l
        (*
        | Let(_, m, n) -> combine [m; n]
        | LetMut(_, m, n) -> combine [m; n]
        *)
        | If(a, b, c) -> combine [a; b; c]
        | IndexAccess(e', _)
        | MemberAccess(e', _)
        | Not(e')
        | Neg(e')
        | BNeg(e') -> called_funcs_of_expr e'
        | Add(a, b)
        | Sub(a, b)
        | Mul(a, b)
        | Div(a, b)
        | Mod(a, b)
        | FAdd(a, b)
        | FSub(a, b)
        | FMul(a, b)
        | FDiv(a, b)
        | Eq(a, b)
        | Neq(a, b)
        | Lt(a, b)
        | Le(a, b)
        | Gt(a, b)
        | Ge(a, b)
        | LAnd(a, b)
        | LOr(a, b)
        | BAnd(a, b)
        | BOr(a, b)
        | BXOr(a, b)
        | BShiftLeft(a, b)
        | BShiftRight(a, b) -> combine [a; b]
        | While(c, b) -> combine [c; b]
        | ForTo(_, from, _to, e) -> combine [from; _to; e]
        | ForDownTo(_, from, _to, e) -> combine [from; _to; e]
        | ForEach(_, v, e) -> combine [v; e]
        | ForEachIndex(_, _, v, e) -> combine [v; e]
        | Block(sl) -> Util.concat @@ List.map called_funcs_of_stmt sl
        | _ -> []
      and called_funcs_of_stmt s =
        let combine_e l = Util.concat @@ List.map called_funcs_of_expr l in
        match s with
        | EmptyReturn -> []
        | Return(e) -> called_funcs_of_expr e
        | Expr(e) -> called_funcs_of_expr e
        | Assign(a, b) -> combine_e [a; b]
        | Let(_, e) -> called_funcs_of_expr e
        | LetMut(_, e) -> called_funcs_of_expr e
        | Break
        | Continue
        | BreakWith _
        | ContinueWith _
        | Global _
        | ModeSwitch _ -> []
      in
      let simple_graph = 
        funcs_and_body
        |> List.map (
          fun (a, b) ->
            let unique_calls = Ast.Block(b) |> called_funcs_of_expr |> Util.unique_strings in
            (a, unique_calls))
      in
      simple_graph
    in
    let has_cycle graph =
      let in_cycle = ref Util.StringSet.empty in
      let already_passed = ref Util.StringSet.empty in
      let does_have_cycle = ref false in
      let rec loop cur =
        (match StringSet.find_opt cur !in_cycle with
        | Some(_) -> does_have_cycle := true; ()
        | None ->
          in_cycle := StringSet.add cur !in_cycle;
          let neighbors = List.assoc_opt cur graph in
          (match neighbors with
          | None -> () (* could be a foreign function *)
          | Some(ns) -> 
            let rec walk l =
              match l with
              | [] -> ()
              | x :: rest ->
                loop x;
                if !does_have_cycle = true then
                  ()
                else
                  walk rest
            in
            walk ns
          );
          in_cycle := StringSet.remove cur !in_cycle;
          already_passed := StringSet.add cur !already_passed
        )
      in
      loop "app_main";
      !does_have_cycle
    in
    let topsort graph cur : string list = 
      let visited = ref [] in
      let rec topsort_loop cur =
        visited := (cur :: !visited);
        let edges = List.assoc cur graph in
        let not_visited l = List.find_opt (is_equal cur) !visited = None in
        edges
        |> List.filter not_visited
        |> List.map topsort_loop
        |> List.fold_left (@) []
        |> List.cons cur
      in
      topsort_loop cur
    in
    let graph = build_graph l in
    if has_cycle graph then failwith "Recursion exists among the function calls!" else
    graph 
    |> List.map fst 
    |> List.map (topsort graph)
    |> List.fold_left (@) []
  in
  let typecheck_func ctx rettype args body =
    let body' = Ast.Block(body) in
    let varstore = vars_of ctx in
    let new_varstore = args @ varstore in
    let new_ctx = ctx_of_vars new_varstore ctx in
    let body'' = typecheck_expr rettype new_ctx body' in
    let body_t = type_of body'' in
    unify_types "typecheck func" rettype body_t |> ignore;
    (args, body'') (* TODO: update the types of args here possibly? *)
  in
  let to_assoc (a, b, c, d) = (b, (a, c, d)) in
  let funcs = functions_of code in
  let assoc_funcs = List.map to_assoc funcs in
  let (ordered_funcs : string list) = order_funcs funcs in
  let typechecked_funcs = 
    let f x =
      let v = List.assoc_opt x assoc_funcs in
      match v with
      | None -> failwith "typechecked_funcs: Impossible case"
      | Some(rettype, args, body) ->
        let args' = List.map (fun (a,b) -> (b,a)) args in
        (x, typecheck_func ctx rettype args' body)
    in
    List.map f ordered_funcs
  in
  (* Forgot what goes here *)
  let typechecked_func_types = 
    List.map 
      (fun (name, (args, body)) -> 
        let arg_types = List.map snd args in
        let rt = type_of body in
        (name, Types.Function(arg_types, rt)))
      typechecked_funcs
  in
  let custom_types = custom_types_of ctx in
  let new_custom_types = typechecked_func_types @ custom_types in
  let new_ctx = 
    ctx
    |> ctx_of_funcs typechecked_funcs
    |> ctx_of_custom_types new_custom_types
  in
  new_ctx

let typecheck_sumtypes ctx code =
  (* Need to extract the sumtype defintions and typecheck them *)
  let open Ast in
  let is_sumtype_def = function
    | SumTypeDef(_, _) -> true
    | _ -> false
  in
  let unpack_sumtype_def = function
    | SumTypeDef(name, variants) -> (name, variants)
    | _ -> failwith "typecheck_sumtypes: unpack_sumtype_def: Impossible case"
  in
  let sumtypes =
    code
    |> List.filter is_sumtype_def
    |> List.map unpack_sumtype_def
  in
  let ensure_absence_of_name_conflicts ctx sumtypes =
    let check (name, _) =
      let custom_types = custom_types_of ctx in
      match List.assoc_opt name custom_types with
      | Some(_) -> failwith @@ Printf.sprintf "Typecheck error: %s name is already occupied!" name
      | None -> ()
    in
    List.iter check sumtypes
  in
  let ensure_absence_of_variant_name_conflicts ctx variants =
    let variants_names = List.map fst variants in
    (* Need to ensure that the names don't appear anywhere! *)
    let check_name ctx name =
      let custom_types = custom_types_of ctx in
      match List.assoc_opt name custom_types with
      | Some(_) -> failwith @@ Printf.sprintf "Typecheck error: variant name %s is already occupied!" name
      | None -> ()
    in
    List.iter (check_name ctx) variants_names;
    match Util.every_element_unique variants_names with
    | true -> ()
    | false -> failwith "Typecheck error: some variants names are not unique!"
  in
  let ensure_proper_variant_types ctx sumtypes =
    (* Need to ensure that all the types that are used in the definition are proper *)
    let check ctx (variant_name, (def : Types.ground_type list)) =
      let rec check2 t =
        (* Proper types are: *)
        (* - base types: unit, bool, int, etc. *)
        (* - other sumtypes if they are not recursive *)
        let open Types in
        match base_type_of t with
        | Unit
        | Bool
        | Char
        | Int
        | Float
        | String -> ()
        | CustomType(tname) ->
          let custom_types = custom_types_of ctx in
          (match List.assoc_opt tname custom_types with
          | None -> failwith @@ Printf.sprintf
              "Typecheck error: in ensure_proper_variant_types: check2: type %s is not defined!" tname
          | Some(t') -> check2 t')
        | SizedArray(t', size) ->
          if size < 0 then
            failwith "Typecheck error: can't create an array with negative size!"
          else
            check2 t'
        | UnsizedArray(t') -> check2 t'
        | Struct(_, fields) ->
          (* I wonder if I should perform the struct checks here as well *)
          fields
          |> List.map snd
          |> List.iter check2
          |> ignore
        | Tuple(values) ->
          values
          |> List.iter check2
          |> ignore
        | Sumtype(tname, vname) -> failwith @@ Printf.sprintf
            "Typecheck error: illegal case in sumtype type checking! %s, %s" tname vname
        | SumtypeDef(variants) ->
          List.iter (fun (_, types) -> List.iter check2 types) variants
        | Function(_, _) -> failwith "Typecheck error: function is not allowed in sumtype definition!"
        | Modetype(_) -> failwith "Typecheck error: mode type is not allowed in sumtype definition!"
        | ModetypeDef(_) -> failwith "Typecheck error: mode type definition is not allowed in sumtype defintion!"
        | Promise(_) -> failwith "Typecheck error: Promise is not allowed in sumtype definition!"
        | PromiseDef(_) -> failwith "Typecheck error: Promise definition is not allowed in sumtype defintion!"
        | Effect(_) -> failwith "Typecheck error: Effect is not allowed in sumtype definition!"
        | EffectDef(_) -> failwith "Typecheck error: Effect definition is not allowed in sumtype definition!"
        | Mutable(_) -> failwith "ensure_proper_variant_types: Impossible case 1"
        | Global(_) -> failwith "ensure_proper_variant_types: Impossible case 2"
        | Poly(_) -> failwith 
          "Typecheck error: something went wrong, polymorphic types not yet allowed in sumtype definition!"
        | AnyType -> failwith 
          "Typecheck error: something went wrong, Any types not allowed in sumtype definition!"
      in
      def
      |> List.iter check2
    in
    List.fold_left 
      (fun cur_ctx (sumtype_name, sumtype_variants) ->
        List.iter (check cur_ctx) sumtype_variants;
        (* Need to add the sumtype definition to the context *)
        (* Need to assign variants their types as well *)
        let custom_types = custom_types_of ctx in
        let sumtype_def_type = Types.SumtypeDef(sumtype_variants) in
        let sumtype_type vname = Types.Sumtype(sumtype_name, vname) in
        let variant_types = sumtype_variants |> List.map fst |> List.map (fun x -> (x, sumtype_type x)) in
        let new_custom_types = (sumtype_name, sumtype_def_type) :: (variant_types @ custom_types) in
        let new_ctx = ctx_of_custom_types new_custom_types cur_ctx in
        new_ctx
      ) 
      ctx 
      sumtypes
  in
  ensure_absence_of_name_conflicts ctx sumtypes;
  ensure_absence_of_variant_name_conflicts ctx sumtypes;
  let new_ctx = ensure_proper_variant_types ctx sumtypes in
  new_ctx (* Return type is a new context with added sumtype definitions *)

let typecheck_mode_defs ctx code =
  let is_mode_def = function
    | ModeDef(_, _) -> true
    | _ -> false
  in
  let unpack_mode_def = function
    | ModeDef(modename, modes) -> (modename, modes)
    | _ -> failwith "typecheck_mode_defs: unpack_mode_def: Impossible case"
  in
  let modedefs = 
    code
    |> List.filter is_mode_def
    |> List.map unpack_mode_def
  in
  let ensure_names_within_mode_unique modedefs =
    let check (modename, modes) =
      match (List.find_opt (fun x -> x = modename) modes) with
      | Some(_) -> failwith "Typecheck error: name %s is used in mode definition as one of the modes!"
      | None -> 
        (match Util.every_element_unique modes with
        | true -> ()
        | false -> failwith @@ Printf.sprintf 
          "Typecheck error: some mode names are reused in mode definition of %s" modename)
    in
    List.iter check modedefs
  in
  let ensure_all_names_unique ctx modedefs =
    let check ctx (modename, modes) =
      let custom_types = custom_types_of ctx in
      (match List.assoc_opt modename custom_types with
      | Some(_) -> failwith @@ Printf.sprintf "Typecheck error: %s name is already taken!" modename
      | None -> 
        let rec loop l =
          match l with
          | [] -> ()
          | x :: rest ->
            (match List.assoc_opt x custom_types with
            | Some(_) -> failwith @@ Printf.sprintf "Typecheck error: %s name is already taken!" x
            | None -> loop rest)
        in
        loop modes)
    in
    List.fold_left (fun cur_ctx (modename, modes) ->
      check cur_ctx (modename, modes);
      let custom_types = custom_types_of cur_ctx in
      let modedef_type = Types.ModetypeDef(modename, modes) in
      let mode_type = Types.Modetype(modename) in
      let modedef = (modename, modedef_type) in
      let modes' = List.map (fun x -> (x, mode_type)) modes in
      let new_custom_types = modedef :: (modes' @ custom_types) in
      let modes_and_modetypes_to_add = List.map (fun x -> (x, modename)) modes in
      let new_modes_and_modetypes = (modes_and_modetypes_to_add) @ (modes_and_modetypes_of cur_ctx) in
      let new_ctx = 
        cur_ctx
        |> ctx_of_custom_types new_custom_types
        |> ctx_of_modes_and_modetypes new_modes_and_modetypes
      in
      new_ctx
    ) ctx modedefs
  in
  ensure_names_within_mode_unique modedefs;
  let new_ctx = ensure_all_names_unique ctx modedefs in
  new_ctx

let typecheck_tasks ctx code =
  let open Ast in
  let is_task = function
    | TaskDef(_, _) -> true
    | _ -> false
  in
  let unpack_task = function
    | TaskDef(name, stmts) -> (name, stmts)
    | _ -> failwith "typecheck_tasks: unpack_task: Impossible case"
  in
  let ensure_unique_names ctx tasks =
    let names = List.map fst tasks in
    let custom_types = custom_types_of ctx in
    let rec loop l =
      match l with
      | [] -> ()
      | x :: rest ->
        (match List.assoc_opt x custom_types with
        | Some(_) -> failwith @@ Printf.sprintf "Typecheck error: task's name %s is already occupied!" x
        | None -> loop rest)
    in
    loop names
  in
  let typecheck_tasks_definitions ctx tasks =
    let typecheck_single_task_definition ctx (name, stmts) =
      let task_ctx = ctx_in_task ctx in
      let body = Ast.Block(stmts) in
      let body' = typecheck_expr (Types.Unit) task_ctx body in
      let body_t = type_of body' in
      unify_types "typecheck task body" body_t (Types.Unit) |> ignore;
      let stmts' = 
        match body' with 
        | Block_t(_, v) -> v 
        | _ -> failwith "typecheck_single_task_definition: Impossible case"
      in
      let tasks = tasks_of task_ctx in
      let new_tasks = (name, stmts') :: tasks in
      let new_ctx = 
        task_ctx
        |> ctx_of_tasks new_tasks 
        |> ctx_not_in_task
      in
      new_ctx
    in
    List.fold_left typecheck_single_task_definition ctx tasks
  in
  let tasks =
    code
    |> List.filter is_task
    |> List.map unpack_task
  in
  ensure_unique_names ctx tasks;
  let new_ctx = typecheck_tasks_definitions ctx tasks in
  new_ctx

(*
let rec used_promises_of_expr e =
  let loop = used_promises_of_expr in
  match e with
  | Promise(t, name, pid) -> [(name, pid, None)]
  | Unit
  | Bool(_)
  | Char(_)
  | Int(_)
  | Float(_)
  | String(_) -> []
  | Ident(_, _) -> [] 
  | Array(_, l)
  | Tuple(_, l)
  | FuncCall(_, _, l)
  | Raise(_, _, l)
  | Variant(_, _, l) ->
    List.fold_left (fun acc x -> acc @ (loop x)) [] l
  | Block(_, l) -> List.fold_left (fun acc x -> acc @ (used_promises_of_stmt x)) [] l
  | CastTo(_, e)
  | Not(e) 
  | Neg(e)
  | BNeg(e)
  | MemberAccess(_, e, _) -> loop e
  | Add(a, b)
  | Sub(a, b)
  | Mul(a, b)
  | Div(a, b)
  | Mod(a, b)
  | FAdd(a, b)
  | FSub(a, b)
  | FMul(a, b)
  | FDiv(a, b)
  | Eq(a, b)
  | Neq(a, b)
  | Lt(a, b)
  | Le(a, b)
  | Gt(a, b)
  | Ge(a, b)
  | LAnd(a, b)
  | LOr(a, b)
  | BAnd(a, b)
  | BOr(a, b)
  | BXOr(a, b)
  | BShiftLeft(a, b)
  | BShiftRight(a, b)
  | IndexAccess(_, a, b) -> (loop a) @ (loop b)
  | Match(_, _, e, l) ->
    let l' = List.map (fun (_, _, v) -> v) l in
    List.fold_left (fun acc x -> acc @ (loop x)) [] (e :: l')
  | Struct -> []
  | Any -> []
  | Await(_, c, name, pid) -> [(name, pid, Some c)]
  | If(_, c, t, f) -> (loop c) @ (loop t) @ (loop f)
  | While(_, c, b) -> (loop c) @ (loop b)
  | ForTo(_, _, bg, en, b)
  | ForDownTo(_, _, bg, en, b) -> 
    (loop bg) @ (loop en) @ (loop b)
  | ForEach(_, _, l, b)
  | ForEachIndex(_, _, _, l, b) ->
    (loop l) @ (loop b)
  | AnonStruct(_, l) ->
    l
    |> List.map snd
    |> List.fold_left (fun acc x -> acc @ (loop x)) []
  | Modetype(_, _) -> []
and used_promises_of_stmt s =
  match s with
  | Break
  | Continue
  | EmptyReturn
  | ModeSwitch(_)
  | Global(_) -> []
  | BreakWith(_, e)
  | ContinueWith(_, e)
  | Return(e)
  | Expr(e)
  | Assign(_, e) 
  | Let(_, e)
  | LetMut(_, e)
  | Dispatch(e) -> used_promises_of_expr e
*)

let rec used_promises_of_expr_t e =
  let loop = used_promises_of_expr_t in
  match e with
  | Promise_t(t, name, pid) -> [(name, pid, None)]
  | Unit_t
  | Bool_t(_)
  | Char_t(_)
  | Int_t(_)
  | Float_t(_)
  | String_t(_) -> []
  | Ident_t(_, _) -> [] 
  | Array_t(_, l)
  | Tuple_t(_, l)
  | FuncCall_t(_, _, l)
  | Raise_t(_, _, l)
  | Variant_t(_, _, l) ->
    List.fold_left (fun acc x -> acc @ (loop x)) [] l
  | Block_t(_, l) -> List.fold_left (fun acc x -> acc @ (used_promises_of_stmt_t x)) [] l
  | CastTo_t(_, e)
  | Not_t(e) 
  | Neg_t(e)
  | BNeg_t(e)
  | MemberAccess_t(_, e, _) -> loop e
  | Add_t(a, b)
  | Sub_t(a, b)
  | Mul_t(a, b)
  | Div_t(a, b)
  | Mod_t(a, b)
  | FAdd_t(a, b)
  | FSub_t(a, b)
  | FMul_t(a, b)
  | FDiv_t(a, b)
  | Eq_t(a, b)
  | Neq_t(a, b)
  | Lt_t(a, b)
  | Le_t(a, b)
  | Gt_t(a, b)
  | Ge_t(a, b)
  | LAnd_t(a, b)
  | LOr_t(a, b)
  | BAnd_t(a, b)
  | BOr_t(a, b)
  | BXOr_t(a, b)
  | BShiftLeft_t(a, b)
  | BShiftRight_t(a, b)
  | IndexAccess_t(_, a, b) -> (loop a) @ (loop b)
  | Match_t(_, _, e, l) ->
    let l' = List.map (fun (_, _, v) -> v) l in
    List.fold_left (fun acc x -> acc @ (loop x)) [] (e :: l')
  | Struct_t -> []
  | Any_t -> []
  | Await_t(_, c, name, pid) -> [(name, pid, Some c)]
  | If_t(_, c, t, f) -> (loop c) @ (loop t) @ (loop f)
  | While_t(_, c, b) -> (loop c) @ (loop b)
  | ForTo_t(_, _, bg, en, b)
  | ForDownTo_t(_, _, bg, en, b) -> 
    (loop bg) @ (loop en) @ (loop b)
  | ForEach_t(_, _, l, b)
  | ForEachIndex_t(_, _, _, l, b) ->
    (loop l) @ (loop b)
  | AnonStruct_t(_, l) ->
    l
    |> List.map snd
    |> List.fold_left (fun acc x -> acc @ (loop x)) []
  | Modetype_t(_, _) -> []
and used_promises_of_stmt_t s =
  match s with
  | Break_t
  | Continue_t
  | EmptyReturn_t
  | ModeSwitch_t(_)
  | Global_t(_) -> []
  | BreakWith_t(_, e)
  | ContinueWith_t(_, e)
  | Return_t(e)
  | Expr_t(e)
  | Assign_t(_, e) 
  | Let_t(_, e)
  | LetMut_t(_, e)
  | Dispatch_t(e) -> used_promises_of_expr_t e

let rec raised_effects_in_expr_t e =
  let loop = raised_effects_in_expr_t in
  match e with
  | Raise_t(c, effname, l) -> 
    (effname, c) :: (List.fold_left (fun acc x -> acc @ (loop x)) [] l)
  | Unit_t
  | Bool_t(_)
  | Char_t(_)
  | Int_t(_)
  | Float_t(_)
  | String_t(_)
  | Ident_t(_, _)
  | Promise_t(_, _, _) -> []
  | Array_t(_, l)
  | Tuple_t(_, l)
  | FuncCall_t(_, _, l)
  | Variant_t(_, _, l) ->
    List.fold_left (fun acc x -> acc @ (loop x)) [] l
  | Block_t(_, l) -> List.fold_left (fun acc x -> acc @ (raised_effects_in_stmt_t x)) [] l
  | CastTo_t(_, e)
  | Not_t(e) 
  | Neg_t(e)
  | BNeg_t(e)
  | MemberAccess_t(_, e, _) -> loop e
  | Add_t(a, b)
  | Sub_t(a, b)
  | Mul_t(a, b)
  | Div_t(a, b)
  | Mod_t(a, b)
  | FAdd_t(a, b)
  | FSub_t(a, b)
  | FMul_t(a, b)
  | FDiv_t(a, b)
  | Eq_t(a, b)
  | Neq_t(a, b)
  | Lt_t(a, b)
  | Le_t(a, b)
  | Gt_t(a, b)
  | Ge_t(a, b)
  | LAnd_t(a, b)
  | LOr_t(a, b)
  | BAnd_t(a, b)
  | BOr_t(a, b)
  | BXOr_t(a, b)
  | BShiftLeft_t(a, b)
  | BShiftRight_t(a, b)
  | IndexAccess_t(_, a, b) -> (loop a) @ (loop b)
  | Match_t(_, _, e, l) ->
    let l' = List.map (fun (_, _, v) -> v) l in
    List.fold_left (fun acc x -> acc @ (loop x)) [] (e :: l')
  | Struct_t -> []
  | Any_t -> []
  | Await_t(_, _, _, _) -> []
  | If_t(_, c, t, f) -> 
      (loop c) @ (loop t) @ (loop f)
  | While_t(_, c, b) -> (loop c) @ (loop b)
  | ForTo_t(_, _, bg, en, b)
  | ForDownTo_t(_, _, bg, en, b) -> 
    (loop bg) @ (loop en) @ (loop b)
  | ForEach_t(_, _, l, b)
  | ForEachIndex_t(_, _, _, l, b) ->
    (loop l) @ (loop b)
  | AnonStruct_t(_, l) ->
    l
    |> List.map snd
    |> List.fold_left (fun acc x -> acc @ (loop x)) []
  | Modetype_t(_, _) -> []
and raised_effects_in_stmt_t s =
  match s with
  | Break_t
  | Continue_t
  | EmptyReturn_t
  | ModeSwitch_t(_)
  | Global_t(_) -> []
  | BreakWith_t(_, e)
  | ContinueWith_t(_, e)
  | Return_t(e)
  | Expr_t(e)
  | Assign_t(_, e) 
  | Let_t(_, e)
  | LetMut_t(_, e)
  | Dispatch_t(e) -> raised_effects_in_expr_t e

let used_promise_names_of_expr_t e =
  List.map (fun (a, _, _) -> a) @@ used_promises_of_expr_t e

let used_promise_names_of_stmt_t e =
  List.map (fun (a, _, _) -> a) @@ used_promises_of_stmt_t e

let typecheck_promises ctx code =
  (* TODO: need to construct promise types and add them to custom_types *)
  let open Ast in
  let is_promise = function
    | PromiseDef(_, _, _, _) -> true
    | _ -> false
  in
  let unpack_promise = function 
    | PromiseDef(name, effname, effargs, body) -> (name, (effname, effargs, body))
    | _ -> failwith "typecheck_promises: unpack_promise: Impossible case"
  in
  let ensure_unique_promises_names ctx promises =
    let names = List.map fst promises in
    let custom_types = custom_types_of ctx in
    (match Util.every_element_unique names with
    | true -> 
      let rec loop l =
        match l with
        | [] -> ()
        | x :: rest ->
          (match List.assoc_opt x custom_types with
          | Some(_) -> failwith @@ Printf.sprintf 
            "Typecheck error: promise's name %s is already occupied!" x
          | None -> loop rest)
      in
      loop names
    | false -> failwith "Typecheck error: some of the promises names are duplicated!")
  in
  let ensure_valid_effects_names ctx promises = 
    let f (name, (effname, effargs, _)) =
      (effname, effargs, name)
    in
    let custom_types = custom_types_of ctx in
    let pairs = List.map f promises in
    let effects = effects_of ctx in
    let rec check l =
      match l with
      | [] -> ()
      | (ename, eargs, name) :: rest ->
        (match List.assoc_opt ename custom_types with
        | Some(EffectDef(_, t_s)) -> 
          (match List.length eargs = List.length t_s with
          | true -> 
            (match Util.every_element_unique eargs with
            | true -> check rest
            | false -> failwith @@ Printf.sprintf 
              "Typecheck error: there are duplicate names among the args of effect %s in promise definition %s!" 
              ename 
              name)
          | false -> failwith @@ Printf.sprintf 
            "Typecheck error: the number of arguments to effect %s in promise definition %s don't match!" 
            ename 
            name)
        | Some(_) -> failwith @@ Printf.sprintf 
          "Typecheck error: expected %s to be an effect, but got something else instead!" ename
        | None -> failwith @@ Printf.sprintf "
          Typecheck error: effect %s in promise definition %s is not defined!" ename name)
    in
    check pairs
  in
  let typecheck_promise_definitions ctx promises =
    (* Need to typecheck the promise as if it is a function *)
    (* For that: *)
    (* - Need to ensure that the promise name is not reused in the promise definition *)
    (* - Need to allow effects, ensure proper effects are used *)
    (* - Need to allow modes, ensure proper modes are used *)
    let typecheck_single_promise_definition ctx (name, (effname, effargs, body)) =
      let promise_ctx = ctx_in_promise ctx in
      let vars = vars_of promise_ctx in
      let custom_types = custom_types_of promise_ctx in
      let Types.EffectDef(_, eff_args_t) = List.assoc effname custom_types in
      let bound_args = List.combine effargs eff_args_t in
      let new_vars = bound_args @ vars in
      let new_promise_ctx = ctx_of_vars new_vars promise_ctx in
      let body' = typecheck_expr (Types.Poly(0)) new_promise_ctx (Ast.Block(body)) in
      let body_t = type_of body' in (* TODO: maybe need to wrap it into a future type *)
      let res = (name, (effname, bound_args, body_t, body')) in
      let promises = promises_of promise_ctx in
      let promise_types = promise_types_of promise_ctx in
      let new_promises = res :: promises in
      let new_promise_types = (name, body_t) :: promise_types in
      let ctx' =
        promise_ctx
        |> ctx_of_promises new_promises
        |> ctx_of_promise_types new_promise_types
        |> ctx_not_in_promise
      in
      ctx'
    in
    let promise_ctx = ctx_in_promise ctx in
    let new_ctx = 
      List.fold_left 
        (fun ctx p -> 
          typecheck_single_promise_definition ctx p) 
        promise_ctx 
        promises 
    in
    new_ctx
    (* failwith "TODO: typecheck_promises: typecheck_promise_definitions" *)
  in
  let ensure_promise_acyclicity ctx promises =
    let module M = Map.Make(String) in
    let m = ref (List.fold_left (fun acc x -> M.add x 0 acc) M.empty (List.map fst promises)) in
    let rec loop x =
      match (M.find x !m) with
      | 0 ->
        m := (M.add x 1 !m);
        let (_, _, _, promise_body) = 
          match List.assoc_opt x promises with
          | Some(y) -> y
          | None -> failwith @@ Printf.sprintf "Typecheck error: %s is not a valid promise name!" x
        in
        let edges = Util.unique_strings @@ used_promise_names_of_expr_t promise_body in
        List.iter loop edges;
        m := (M.add x 2 !m)
      | 1 -> failwith "Typecheck error: there is a recursion among the promises!"
      | 2 -> ()
      | _ -> failwith "ensure_promise_acyclicity: Impossible case 1"
    in
    List.iter (loop) (List.map fst promises)
  in
  let promises =
    code
    |> List.filter is_promise
    |> List.map unpack_promise
  in
  let promise_types =
    let promise_names = List.map fst promises in
    let types = List.map (fun x -> (x, Types.Promise(x))) promise_names in
    types
  in
  ensure_unique_promises_names ctx promises;
  ensure_valid_effects_names ctx promises;
  let custom_types = custom_types_of ctx in
  let new_custom_types = promise_types @ custom_types in
  let new_ctx = ctx_of_custom_types new_custom_types ctx in
  let new_ctx = typecheck_promise_definitions new_ctx promises in
  let promises' = promises_of new_ctx in
  ensure_promise_acyclicity ctx promises';
  let promise_types =
    promises'
    |> List.map (fun (x, (effname, args, rettype, _)) -> 
        let custom_types = custom_types_of ctx in
        let args' = List.map snd args in
        (x, Types.PromiseDef(x, effname, args', rettype)))
  in
  let custom_types = custom_types_of new_ctx in
  let new_custom_types = promise_types @ custom_types in
  let new_ctx = ctx_of_custom_types new_custom_types new_ctx in
  new_ctx
  (* failwith "TODO: typecheck_promises" *)

let type_of_stmts stmts =
  let rec loop l =
    match l with
    | [] -> Types.Unit
    | [Expr_t(e)] -> type_of e
    | [Break_t] -> Types.Unit
    | [Continue_t] -> Types.Unit
    | [BreakWith_t(t, _)] -> t
    | [ContinueWith_t(t, _)] -> t
    | [EmptyReturn_t] -> Types.Unit
    | [Return_t(e)] -> type_of e
    | [Global_t(_)] -> Types.Unit
    | [Assign_t(_, _)] -> Types.Unit
    | [Let_t(_, _)] -> Types.Unit
    | [LetMut_t(_, _)] -> Types.Unit
    | [ModeSwitch_t(_)] -> Types.AnyType (* IMPORTANT: If AnyType from type_of_stmts then it is mode switch *)
    | _ :: rest -> loop rest
  in
  loop stmts

let typecheck_promise_communications ctx code =
  (* In this function need to check what tasks and promises can send signals to each other *)
  let open Ast in
  let is_parallel = function
    | ParallelDef(_) -> true
    | _ -> false
  in
  let unpack_parallel = function
    | ParallelDef(l) -> l
    | _ -> failwith "typecheck_promise_communications: unpack_parallel: Impossible case"
  in
  let ensure_proper_task_names parallels =
    let tasks = tasks_of ctx in
    let interrupts = interrupts_of ctx in
    let task_names = List.map fst tasks in
    let interrupt_names = List.map fst interrupts in
    let check_single_parallel l =
      let rec loop l =
        match l with
        | [] -> ()
        | x :: rest ->
          (match List.find_opt (fun y -> y = x) task_names with
          | Some(_) -> loop rest
          | None -> 
            (match List.find_opt (fun y -> y = x) interrupt_names with
            | Some(_) -> loop rest
            | None -> failwith @@ Printf.sprintf 
              "Typecheck error: the %s is not a valid task!" x))
      in
      loop l;
      (match Util.every_element_unique l with
      | true -> ()
      | false -> failwith "Typecheck error: some of the tasks in parallel definition occur more than once!")
    in
    List.iter check_single_parallel parallels
  in
  let construct_parallel_communications ctx parallels =
    let triggers a b =
      let rec loop l =
        match l with
        | [] -> false
        | x :: y ->
          let module S = Set.Make(String) in
          let s = S.of_list x in
          if S.mem a s && S.mem b s then
            true
          else
            loop y
      in
      loop parallels
    in
    let module MSS = MapStringString in
    let tasks = tasks_of ctx in
    let interrupts = interrupts_of ctx in
    let effects_and_triggered_promises =
      let promises = promises_of ctx in
      let e_and_p = 
        List.map (fun (n, (e, _, _, _)) -> (e, n)) promises
      in
      let module M = Map.Make(String) in
      let m = 
        List.fold_left 
          (fun acc (x, y) -> 
            match M.find_opt x acc with
            | Some(l) -> M.add x (l @ [y]) acc
            | None -> M.add x ([y]) acc) 
          (M.empty) 
          e_and_p 
      in
      m
      |> M.to_seq
      |> List.of_seq
    in
    let effects_in_single_task (taskname) =
      let interrupts = interrupts_of ctx in
      (match List.assoc_opt taskname interrupts with
      | Some(effname) -> 
        print_endline "DEBUG: Found interrupt when resolving communications!";
        (taskname, [(effname, -1, taskname)])
      | None ->
        let stmts = 
          let tasks = tasks_of ctx in
          List.assoc taskname tasks
        in
        let rec raised_effects_in_expr_t_rec x =
          let effs = raised_effects_in_expr_t x in
          let promises = used_promises_of_expr_t x in
          let promise_names = List.map (fun (a, _, _) -> a) promises in
          let promise_defs = promises_of ctx in
          let used_promise_bodies = 
            List.map (fun x -> 
                match List.assoc_opt x promise_defs with
                | Some((_, _, _, b)) -> b
                | None -> failwith @@ Printf.sprintf 
                  "Typecheck error: Promise %s is not defined!" x
              ) 
              promise_names
          in
          List.fold_left 
            (fun acc x -> acc @ raised_effects_in_expr_t_rec x) 
            effs
            used_promise_bodies
        in
        let l = raised_effects_in_expr_t_rec (Block_t(type_of_stmts stmts, stmts)) in
        (taskname, List.map (fun (a, b) -> (a, b, taskname)) l)
      )
    in
    let promises_in_single_task (taskname) =
      let interrupts = interrupts_of ctx in
      (match List.assoc_opt taskname interrupts with
      | Some(_) -> (taskname, [])
      | None -> 
        let stmts = 
          let tasks = tasks_of ctx in
          List.assoc taskname tasks
        in
        let rec used_promises_of_expr_t_rec e =
          let promises = used_promises_of_expr_t e in
          let promise_names = List.map (fun (a, _, _) -> a) promises in
          let promise_defs = promises_of ctx in
          let used_promise_bodies = 
            List.map (fun x -> 
                match List.assoc_opt x promise_defs with
                | Some((_, _, _, b)) -> b
                | None -> failwith @@ Printf.sprintf 
                  "Typecheck error: Promise %s is not defined!" x
              ) 
              promise_names
          in
          List.fold_left 
            (fun acc x -> acc @ used_promises_of_expr_t_rec x) 
            promises 
            used_promise_bodies
        in
        let l = used_promises_of_expr_t_rec (Block_t(type_of_stmts stmts, stmts)) in
        (taskname, List.map (fun (a, b, c) -> (a, b, c, taskname)) l))
    in
    let effects_in_tasks =
      let task_names = 
        (List.map fst tasks) @ (List.map fst interrupts)
      in
      List.map effects_in_single_task task_names
    in
    let promises_in_tasks =
      let task_names = 
        (List.map fst tasks) @ (List.map fst interrupts)
      in
      List.map promises_in_single_task task_names
    in
    let construct_single_parallel2 ctx cur_parallel_tasks =
      let rec alg ans l =
        match l with
        | [] -> ans
        | cur_task :: rest ->
          print_endline @@ Printf.sprintf "DEBUG: %s" cur_task;
          let (_, cur_task_effects) = 
            effects_in_tasks
            |> List.filter (fun (x, _) -> x = cur_task)
            |> List.hd
          in
          let promises_in_other_tasks =
            promises_in_tasks
            |> List.filter (fun (x, _) -> x <> cur_task && triggers x cur_task)
            |> List.map (fun (_, x) -> x)
            |> List.flatten
          in
          let rec calculate_for_each_effect ans l =
            match l with
            | [] -> ans
            | (effname, c, taskname) :: rest' ->
              let needed_promises =
                match (List.assoc_opt effname effects_and_triggered_promises) with
                | Some(l) -> l
                | None -> failwith @@ Printf.sprintf
                  "Typecheck error: something impossible happened! Effect %s is not defined!"
                  effname
              in
              let module S = Set.Make(String) in
              let s = S.of_list needed_promises in
              let actual_promises =
                List.filter (fun (p, _, _, _) -> S.mem p s) promises_in_other_tasks
              in
              let cur_l = 
                match MSS.find_opt (effname, taskname) ans with
                | Some(l) -> l
                | None -> []
              in
              let new_l = (* Util.unique_naive *) (cur_l @ actual_promises) in
              let new_ans = MSS.add (effname, taskname) new_l ans in
              calculate_for_each_effect (new_ans) rest'
          in
          let new_ans = calculate_for_each_effect ans cur_task_effects in
          alg new_ans rest
      in
      let value = alg (MSS.empty) cur_parallel_tasks in
      value
    in
    (*
    let rec merge_parallels cur_progress l =
      match l with
      | [] -> cur_progress
      | cur_parallel :: rest_parallels ->
        let new_progress =
          MSS.fold (fun k v acc -> 
            match MSS.find_opt k acc with
            | Some(l') -> MSS.add k (l' @ v) acc
            | None -> MSS.add k v acc)
            cur_progress
            (construct_single_parallel2 ctx (MSS.empty) cur_parallel)
        in
        merge_parallels (new_progress) rest_parallels
    in
    *)
    (* let data = merge_parallels (MSS.empty) parallels in *)
    let data = construct_single_parallel2 ctx @@ ((List.map fst tasks) @ (List.map fst interrupts)) in
    let data_as_list = List.of_seq @@ MSS.to_seq data in
    let recombine l =
      let rec expand l =
        match l with
        | [] -> []
        | ((eff, tsk), l1) :: l2 ->
          let l2' = expand l2 in
          let l1' = List.map (fun (a, b, c, d) -> (eff, tsk, a, b, c, d)) l1 in
          l1' @ l2'
      in
      let expanded = expand l in
      let rec compress_on_promises m l = 
        match l with 
        | [] -> m
        | (eff, eff_task, p, pid, Some(await_id), p_task) :: rest ->
          let new_m =
            (match MSS.find_opt (p, p_task) m with
            | Some(n) -> 
              (let n' = 
                (match MSS.find_opt (eff, eff_task) n with
                | Some(l') -> MSS.add (eff, eff_task) (Util.unique_naive (l' @ [await_id])) n
                | None -> MSS.add (eff, eff_task) [await_id] n) 
              in
              MSS.add (p, p_task) n' m)
            | None ->
              (let n = MSS.empty in
              let n' = MSS.add (eff, eff_task) [await_id] n in
              MSS.add (p, p_task) n' m))
          in
          compress_on_promises new_m rest
        | (eff, eff_task, p, pid, None, p_task) :: rest -> 
          let new_m =
            match (MSS.find_opt (p, p_task) m) with
            | Some(n) -> m
            | None ->
              let n = MSS.empty in
              let n' = MSS.add (eff, eff_task) [] n in
              let m' = MSS.add (p, p_task) n' m in
              m'
          in
          compress_on_promises new_m rest
      in
      let compressed = compress_on_promises (MSS.empty) expanded in
      let true_value1 = List.of_seq @@ MSS.to_seq compressed in
      let true_value2 = List.map (fun (k, x) -> (k, List.of_seq @@ MSS.to_seq x)) true_value1 in
      true_value2
    in
    let recombined = recombine data_as_list in
    let effects_and_triggered_promises = data_as_list in
    let promises_and_awaits = recombined in
    (effects_and_triggered_promises, promises_and_awaits)
    (* failwith "TODO: construct_parallel_communications" *)
  in
  let parallels =
    code
    |> List.filter is_parallel
    |> List.map unpack_parallel
  in
  ensure_proper_task_names parallels;
  let comms = construct_parallel_communications ctx parallels in
  ctx
  |> ctx_of_parallels parallels
  |> ctx_of_parallels_resolved comms
  (* failwith "TODO: typecheck_promise_communications" *)

let typecheck_start_modes ctx code =
  let open Ast in
  let is_start_modes = function
    | StartModesDef(_) -> true
    | _ -> false
  in
  let unpack_start_modes = function
    | StartModesDef(l) -> l
    | _ -> failwith "typecheck_modes: unpack_start_modes: Impossible case"
  in
  let ensure_unique_names ctx modes =
    let custom_types = custom_types_of ctx in
    let flatten l =
      let rec loop ans l =
        match l with
        | [] -> ans
        | (a, b) :: rest -> loop (ans @ (a :: b)) rest
      in
      loop [] l
    in
    (match Util.every_element_unique (flatten modes) with
    | true ->
      let rec check l =
        match l with
        | [] -> ()
        | (modename, modes) :: rest ->
          (match List.assoc_opt modename custom_types with
          | Some(t') -> failwith @@ Printf.sprintf 
            "Typecheck error: the mode's name %s is already occupied! %s" 
            modename 
            (Types.string_of_ground_type t')
          | None -> (* Do the rest of the checks *)
            let rec check2 l =
              match l with
              | [] -> ()
              | x :: rest ->
                (match List.assoc_opt x custom_types with
                | Some(_) -> failwith @@ Printf.sprintf 
                  "Typecheck error: modetype's name %s is already occupied!" x
                | None -> check2 rest)
            in
            check2 modes;
            check rest)
      in
      check modes
    | false -> failwith "Typecheck error: some of the names of modes and modetypes are reused!")
  in
  let modes_and_modetypes = modes_and_modetypes_of ctx in
  (match modes_and_modetypes with
  | [] -> 
    ctx (* no modes, no typechecking *)
  | _ ->
    (* TODO: typecheck start modes *)
    let check_start_modes l ctx =
      (* to typecheck start modes, need to ensure several things: *)
      (* - EACH modetype is present *)
      (* - No modetype is referred twice *)
      (* Bonus: no conflicting start mode definitions *)
      let module S = Set.Make(String) in
      let module M = Map.Make(String) in 
      let dict = M.empty in
      let modes_and_modetypes = modes_and_modetypes_of ctx in
      let modes_s = S.empty in
      let modetypes_s = S.empty in
      let complete_modetypes_s = S.of_list @@ List.map snd modes_and_modetypes in
      let (dict, modes_s, modetypes_s) =
        List.fold_left
          (fun (dict, modes_s, modetypes_s) x ->
            (match List.assoc_opt x modes_and_modetypes with
            | Some(mt) ->
              (match S.mem mt modetypes_s with
              | false ->
                let dict' = M.add mt x dict in
                let modes_s' = S.add x modes_s in
                let modetypes_s' = S.add mt modetypes_s in
                (dict', modes_s', modetypes_s')
              | true ->
                failwith @@ Printf.sprintf
                  "Typecheck error: conflicting start modes information with regards to modetype %s: %s"
                  (mt)
                  (String.concat ", " @@ [x; M.find mt dict])
              )
            | None -> 
              failwith @@ Printf.sprintf 
                "Typecheck error: when defining start modes: mode %s is not defined!"
                x
            )
          )
          (dict, modes_s, modetypes_s)
          l
      in
      (match S.equal modetypes_s complete_modetypes_s with
      | true ->
        let start_modes' = List.of_seq @@ S.to_seq modes_s in
        let new_ctx = ctx_of_start_modes start_modes' ctx in
        new_ctx
      | false ->
        failwith @@ Printf.sprintf 
          "Typecheck error: start modes definition is incomplete! Need modes from following modetypes: %s"
          (String.concat ", " @@ 
             List.of_seq @@ 
               S.to_seq @@ 
                 S.diff complete_modetypes_s modetypes_s)
      )
      (* failwith "TODO: check_start_modes" *)
    in
    let start_modes =
      code
      |> List.filter is_start_modes
      |> List.map unpack_start_modes
      |> List.flatten
    in
    ctx
    |> check_start_modes start_modes
  )

let typecheck_mode_transitions ctx code =
  (* In this function need to check the mode transitions *)
  (* Ah, the fun begins *)
  (* Need to extract mode transitions from tasks *)
  let open Ast in
  let is_mode_tasks_def = function
    | ModeTasksDef(_, _) -> true
    | _ -> false
  in
  let unpack_mode_tasks_def = function
    | ModeTasksDef(m, l) -> (m, l)
    | _ -> failwith "typecheck_mode_transitions: unpack_mode_tasks_def: Impossible case"
  in
  let mode_tasks =
    code
    |> List.filter is_mode_tasks_def
    |> List.map unpack_mode_tasks_def
  in
  let module M = Map.Make(String) in
  let dict = ref M.empty in
  let ctx = ctx_of_modes_and_tasks mode_tasks ctx in
  let sanity_checks mode_tasks =
    (* check that the names of used modes are valid (DONE) *)
    (* check that the names of used tasks are valid (DONE) *)
    (* check that nothing but tasks names are used (DONE) *)
    (* check that each mode is defined at most once (DONE) *)
    (* check that the tasks are not used accross the modetypes (DONE) *)
    let tasks = tasks_of ctx in
    let custom_types = custom_types_of ctx in
    let used_modes = mode_tasks |> List.map fst in
    let used_tasks = mode_tasks |> List.map snd |> List.flatten |> Util.unique_naive in
    let is_mode m =
      match (List.assoc_opt m custom_types) with
      | Some(Modetype(_)) -> ()
      | Some(t') -> failwith @@ Printf.sprintf 
      "Typecheck error: %s is not a mode type! Instead: %s" m (Types.string_of_ground_type t')
      | None -> failwith @@ Printf.sprintf "Typecheck error: %s is not defined!" m
    in
    let is_task t =
      match (List.assoc_opt t tasks) with
      | Some(_) -> ()
      | None -> failwith @@ Printf.sprintf "Typecheck error: task %s is not defined!" t
    in
    let modes_defined_at_most_once modes =
      match (modes = Util.unique_naive modes) with
      | true -> ()
      (* TODO: improve error reporting here *)
      | false -> failwith 
      "Typecheck error: some of the modes for mode tasks definitions are defined more than once!"
    in
    let tasks_not_used_across_mode_types mode_tasks =
      let module M = Map.Make(String) in
      let rec loop m l =
        match l with
        | [] -> ()
        | (mode, tasks) :: rest ->
          let modetype1 = mode_type_of ctx mode in
          let rec check cur_m l =
            match l with
            | [] -> cur_m
            | tname :: t_rest ->
              (match M.find_opt tname cur_m with
              | Some(modetype2) ->
                if modetype1 = modetype2 then
                  check cur_m t_rest
                else
                  failwith @@ Printf.sprintf
                  "Typecheck error: task %s is used in both modetype defintions: %s and %s!"
                  (tname)
                  (modetype1)
                  (modetype2)
              | None ->
                check (M.add tname modetype1 cur_m) t_rest
              )
          in
          loop (check m tasks) rest
      in
      loop M.empty mode_tasks
    in
    List.iter is_mode used_modes;
    List.iter is_task used_tasks;
    modes_defined_at_most_once used_modes;
    tasks_not_used_across_mode_types mode_tasks
    (* failwith "TODO: sanity_checks" *)
  in
  let tasks = tasks_of ctx in
  let modes_and_modetypes = modes_and_modetypes_of ctx in
  (* UNUSED
    let grouped_tasks_and_modetypes =
      (* For each modetype there's a list of modes and list of tasks *)
      failwith "TODO: grouped_tasks_and_modetypes"
    in
  *)
  (* Run through the code and do fancy analysis *)
  let analysis mode_tasks_list =
    (* the output of this function is a list of unconditional transitions and conditional transitions *)
    (* for each of N^2 mode transitions I need a list of: *)
    (* - old tasks and promises, new tasks and promises *)
    (* or even better *)
    (* - delete these tasks and promises, create these new tasks and promises *)
    let grouped_modes =
      let module M = Map.Make(String) in
      let custom_types = custom_types_of ctx in
      let rec loop ans l =
        match l with
        | [] -> ans
        | (mode_name, tasks) :: rest ->
          let mode_type_name =
            (match List.assoc_opt mode_name custom_types with
            | Some(Modetype(mode_type_name)) -> mode_type_name
            | Some(t') -> failwith @@ Printf.sprintf 
            "Typecheck error: expected mode type for %s, but instead got %s" 
            (mode_name) 
            (Types.string_of_ground_type t')
            | None -> failwith @@ Printf.sprintf 
            "Typecheck error: expected %s to be a mode type, but it is not defined!" mode_name)
          in
          let new_ans =
            (match M.find_opt mode_type_name ans with
            | Some(l) ->
              let new_l = (mode_name, tasks) :: l in
              M.add mode_type_name new_l ans
            | None ->
              M.add mode_type_name [(mode_name, tasks)] ans) 
          in
          loop new_ans rest
      in
      List.of_seq @@ M.to_seq @@ loop M.empty mode_tasks_list
    in
    let analyze_diffs (mode_type, modes_and_tasks) =
      let criss_cross a b =
        let rec loop1 a =
          match a with
          | [] -> []
          | x :: rest ->
            let rec loop2 x b =
              match b with
              | [] -> []
              | y :: rest2 ->
                (x, y) :: (loop2 x rest2)
            in
            (loop2 x b) @ (loop1 rest)
        in
        loop1 a
      in
      (* TODO: make this a global function to remove duplication *)
      let promises_of_task taskname : string list =
        let tasks = tasks_of ctx in
        let promises = promises_of ctx in
        let rec promises_in_expr e =
          match e with
          | Promise_t(_, name, _) -> [name]
          | Unit_t
          | Bool_t(_)
          | Char_t(_)
          | Int_t(_)
          | Float_t(_)
          | String_t(_) -> []
          | Ident_t(_, name) -> [] 
          (*
            (match List.assoc_opt name promises with
            | Some(_) -> (* [(name, None)] *) [name]
            | None -> [])
          *)
          | Array_t(_, l)
          | Tuple_t(_, l)
          | FuncCall_t(_, _, l)
          | Raise_t(_, _, l)
          | Variant_t(_, _, l) ->
            List.fold_left (fun acc x -> acc @ (promises_in_expr x)) [] l
          | Block_t(_, l) -> List.fold_left (fun acc x -> acc @ (promises_in_stmt x)) [] l
          | CastTo_t(_, e)
          | Not_t(e) 
          | Neg_t(e)
          | BNeg_t(e)
          | MemberAccess_t(_, e, _) -> promises_in_expr e
          | Add_t(a, b)
          | Sub_t(a, b)
          | Mul_t(a, b)
          | Div_t(a, b)
          | Mod_t(a, b)
          | FAdd_t(a, b)
          | FSub_t(a, b)
          | FMul_t(a, b)
          | FDiv_t(a, b)
          | Eq_t(a, b)
          | Neq_t(a, b)
          | Lt_t(a, b)
          | Le_t(a, b)
          | Gt_t(a, b)
          | Ge_t(a, b)
          | LAnd_t(a, b)
          | LOr_t(a, b)
          | BAnd_t(a, b)
          | BOr_t(a, b)
          | BXOr_t(a, b)
          | BShiftLeft_t(a, b)
          | BShiftRight_t(a, b)
          | IndexAccess_t(_, a, b) -> (promises_in_expr a) @ (promises_in_expr b)
          | Match_t(_, _, e, l) ->
            let l' = List.map (fun (_, _, v) -> v) l in
            List.fold_left (fun acc x -> acc @ (promises_in_expr x)) [] (e :: l')
          | Struct_t -> []
          | Any_t -> []
          | Await_t(_, _, name, _) -> (* [(name, Some c)] *) [name]
          | If_t(_, c, t, f) -> (promises_in_expr c) @ (promises_in_expr t) @ (promises_in_expr f)
          | While_t(_, c, b) -> (promises_in_expr c) @ (promises_in_expr b)
          | ForTo_t(_, _, bg, en, b)
          | ForDownTo_t(_, _, bg, en, b) -> 
            (promises_in_expr bg) @ (promises_in_expr en) @ (promises_in_expr b)
          | ForEach_t(_, _, l, b)
          | ForEachIndex_t(_, _, _, l, b) ->
            (promises_in_expr l) @ (promises_in_expr b)
          | AnonStruct_t(_, l) ->
            l
            |> List.map snd
            |> List.fold_left (fun acc x -> acc @ (promises_in_expr x)) []
          | Modetype_t(_, _) -> []
        and promises_in_stmt s =
          match s with
          | Break_t
          | Continue_t
          | EmptyReturn_t
          | ModeSwitch_t(_)
          | Global_t(_) -> []
          | BreakWith_t(_, e)
          | ContinueWith_t(_, e)
          | Return_t(e)
          | Expr_t(e)
          | Assign_t(_, e) 
          | Let_t(_, e)
          | LetMut_t(_, e) -> promises_in_expr e
        in
        let promises_in_single_task (taskname, stmts) =
          (taskname, promises_in_expr (Block_t(type_of_stmts stmts, stmts)))
        in
        (match List.assoc_opt taskname tasks with
        | Some(stmts) -> snd @@ promises_in_single_task (taskname, stmts)
        | None -> failwith @@ Printf.sprintf
        "analyze_diffs: promises_of_task: task with name %s is not found or does not exist!" taskname)
      in
      let cartesian_on_modes a b =
        let rec loop1 a =
          match a with
          | [] -> []
          | (mode_name_a, tasks_a) :: rest_a ->
            let rec loop2 mode_name_a b =
              match b with
              | [] -> []
              | (mode_name_b, tasks_b) :: rest_b ->
                (* TODO: Would be nice to get promises of a task as well and add them here *)
                let promises_of_tasks_a =
                  tasks_a
                  |> List.map promises_of_task
                in
                let promises_of_tasks_a =
                  tasks_a
                  |> List.map promises_of_task
                  |> List.flatten
                  |> Util.unique_naive
                in
                let promises_of_tasks_b =
                  tasks_b
                  |> List.map promises_of_task
                  |> List.flatten
                  |> Util.unique_naive
                in
                dict := (
                  List.fold_left (fun d x ->
                    let promises = promises_of_task x in
                    M.add x promises d
                  ) !dict tasks_a
                );
                dict := (
                  List.fold_left (fun d x ->
                    let promises = promises_of_task x in
                    M.add x promises d
                  ) !dict tasks_b
                );
                ((mode_name_a, mode_name_b), 
                (promises_of_tasks_a @ tasks_a, promises_of_tasks_b @ tasks_b)) :: (loop2 mode_name_a rest_b)
            in
            (loop2 mode_name_a b) @ (loop1 rest_a)
        in
        loop1 a
      in
      let transition_table = cartesian_on_modes (modes_and_tasks) (modes_and_tasks) in
      let calculate_diff a b =
        (* Given two lists, need to output two other lists: *)
        (* - Items that need to be removed from the first that are not present in the second *)
        (* - Items that need to be added to the first that are present in the second *)
        let module S = Set.Make(String) in
        let a_s = S.of_list a in
        let b_s = S.of_list b in
        let removed = S.diff a_s b_s |> S.to_seq |> List.of_seq in
        let added = S.diff b_s a_s |> S.to_seq |> List.of_seq in
        (removed, added)
      in
      let f ((mode_a, mode_b), (a, b)) =
        ((mode_a, mode_b), (calculate_diff a b))
      in
      (mode_type, List.map f transition_table)
    in
    (List.map analyze_diffs grouped_modes)
    (* failwith "TODO: typecheck_mode_transitions: analysis" *)
  in
  let construct_tasks_and_modetypes ctx mode_tasks =
    let custom_types = custom_types_of ctx in
    let f (mode, cur_tasks) =
      let modetype = 
        (match List.assoc_opt mode custom_types with
        | Some(Modetype(modetype)) -> modetype
        | _ -> failwith @@ Printf.sprintf
              "Typecheck error: expected %s to be a modetype, but something went wrong!"
              mode
        )
      in
      List.map (fun (x) -> (x, modetype)) cur_tasks
    in
    mode_tasks
    |> List.map f
    |> List.fold_left (@) []
  in
  sanity_checks mode_tasks;
  let table = analysis mode_tasks in
  let t_and_m = construct_tasks_and_modetypes ctx mode_tasks in
  let tasks_and_promises' = List.of_seq @@ M.to_seq !dict in
  let new_ctx = 
    ctx
    |> ctx_of_mode_transitions table
    |> ctx_of_tasks_and_modetypes t_and_m
    |> ctx_of_tasks_and_promises tasks_and_promises'
  in
  new_ctx
  (* failwith "TODO: typecheck_mode_transitions" *)

let typecheck_globals ctx code =
  let globals =
    let is_global (x : Ast.toplevel) = 
      match x with
      | Global _ -> true
      | _ -> false
    in
    let unpack_global (x : Ast.toplevel) =
      match x with
      | Global(name, t) -> (name, t)
      | _ -> failwith "typecheck_globals: unpack_global: Impossible case"
    in
    code
    |> List.filter is_global
    |> List.map unpack_global
  in
  let ensure_unique_names globals =
    let custom_types = custom_types_of ctx in
    let names = List.map (fun (a, _) -> a) globals in
    List.iter (fun x -> 
      match List.assoc_opt x custom_types with
      | None -> ()
      | Some(_) -> failwith @@ Printf.sprintf "Typecheck error: name %s is already occupied!" x
      ) 
      names;
    match (List.length names = List.length (Util.unique_strings names)) with
    | true -> ()
    | false -> failwith "Typecheck error: some global names are defined more than once!"
  in
  ensure_unique_names globals;
  let custom_types = custom_types_of ctx in
  let new_custom_types = globals @ custom_types in
  let new_ctx = 
    ctx
    |> ctx_of_custom_types new_custom_types
    |> ctx_of_globals globals
  in
  new_ctx

let typecheck code =
  (* Need to typecheck *)
  (* - Structs (DONE) *)
  (* - Effects (DONE) *)
  (* - Sumtypes *)
  (* - Modes *)
  (* - Globals *)
  (* - Functions (WIP) *)
  (* - Tasks *)
  (* - Communications of tasks, effects and promises (DONE, HAVEN'T TESTED YET) *)
  (* - Mode switches (DONE, I think) *)
  (* - Start modes (NOT YET) *)
  let ctx = empty_ctx in
  let ctx = typecheck_add_user_funcs ctx code in
  let ctx = typecheck_structs ctx code in
  let ctx = typecheck_sumtypes ctx code in
  let ctx = typecheck_effects ctx code in
  let ctx = typecheck_globals ctx code in
  let ctx = typecheck_interrupts ctx code in
  let ctx = typecheck_mode_defs ctx code in
  (*
  let ctx = 
    empty_ctx 
    |> ctx_of_custom_types (List.map (fun (name, l) -> (name, Types.Struct(l))) structs) 
    |> ctx_of_effects effects 
  in
  *)
  let ctx = typecheck_basic_funcs ctx code in
  let ctx = typecheck_promises ctx code in
  let ctx = typecheck_tasks ctx code in
  let ctx = typecheck_start_modes ctx code in
  let ctx = typecheck_promise_communications ctx code in
  let ctx = typecheck_mode_transitions ctx code in
  ctx
