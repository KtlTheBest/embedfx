let task_enum_of s = Printf.sprintf "TASK_ENUM_VALUE_%s" s

type c_type =
  | Void
  | Bool
  | Char
  | Int
  | Float
  | String
  | ArrayWSize of c_type * int
  | Array of c_type
  | FuncDeclType of c_type list * c_type
  | CustomType of string
  | Pointer of c_type
  | Int8
  | Int16
  | Int32
  | Int64
  | UInt8
  | UInt16
  | UInt32
  | UInt64

and c_expr =
  | Null
  | BoolLit of bool
  | CharLit of char
  | IntLit of int
  | FloatLit of float
  | StringLit of string
  | Ident of string
  | Array of c_expr list
  | AnonStruct of (string * c_expr) list
  | IndexAccess of c_expr * c_expr
  | MemberAccess of c_expr * string
  | AddrOf of c_expr
  | Deref of c_expr
  | CastTo of c_type * c_expr
  | Not of c_expr
  | Neg of c_expr
  | Add of c_expr * c_expr
  | Sub of c_expr * c_expr
  | Mul of c_expr * c_expr
  | Div of c_expr * c_expr
  | Mod of c_expr * c_expr
  | Eq of c_expr * c_expr
  | NEq of c_expr * c_expr
  | Lt of c_expr * c_expr
  | Le of c_expr * c_expr
  | Gt of c_expr * c_expr
  | Ge of c_expr * c_expr
  | Or of c_expr * c_expr
  | And of c_expr * c_expr
  | BNeg of c_expr
  | BOr of c_expr * c_expr
  | BAnd of c_expr * c_expr
  | BXOr of c_expr * c_expr
  | BSL of c_expr * c_expr
  | BSR of c_expr * c_expr
  | FuncCall of string * (c_expr list)
  | Ternary of c_expr * c_expr * c_expr
  | SizeOf of c_type

type c_stmt =
  | Break
  | Continue
  | EmptyReturn
  | Return of c_expr
  | Expr of c_expr
  | Assign of c_expr * c_expr
  | VarDef of c_type * string * c_expr
  | EmptyVarDef of c_type * string
  | Block of c_stmt list
  | WhileLoop of c_expr * (c_stmt list)
  | ForLoop of (c_stmt option) * c_expr * c_stmt * (c_stmt list)
  | IfStmt of c_expr * (c_stmt list) * (c_stmt list)
  | ElifChain of (c_expr * (c_stmt list)) list
  | IncPost of c_expr
  | DecPost of c_expr
  | IncPre of c_expr
  | DecPre of c_expr
  | Goto of string
  | Label of string

let rec string_of_type t =
  let f types = 
    types
    |> List.map string_of_type
    |> String.concat "; "
  in
  match t with
  | Void -> "Void"
  | Bool -> "Bool"
  | Char -> "Char"
  | Int -> "Int"
  | Float -> "Float"
  | String -> "String"
  | ArrayWSize(t', i) -> Printf.sprintf "ArrayWSize(%s, %d)" (string_of_type t') i
  | Array(t') -> Printf.sprintf "Array(%s)" (string_of_type t')
  | FuncDeclType(ts, rt) -> Printf.sprintf "FuncDeclType([%s], %s)" (f ts) (string_of_type rt)
  | CustomType(s) -> Printf.sprintf "CustomType(%s)" s
  | Pointer(t') -> Printf.sprintf "Pointer(%s)" (string_of_type t')
  | Int8 -> "Int8"
  | Int16 -> "Int16"
  | Int32 -> "Int32"
  | Int64 -> "Int64"
  | UInt8 -> "UInt8"
  | UInt16 -> "UInt16"
  | UInt32 -> "UInt32"
  | UInt64 -> "UInt64"

let rec string_of_expr e =
  let f exprs = 
    exprs
    |> List.map string_of_expr
    |> String.concat "; "
  in
  let g exprs = 
    exprs
    |> List.map (fun (field, v) -> Printf.sprintf "(%s, %s)" field (string_of_expr v))
    |> String.concat "; "
  in
  match e with
  | Null -> "Null"
  | BoolLit(true) -> "BoolLit(true)"
  | BoolLit(false) -> "BoolLit(false)"
  | CharLit(c) -> Printf.sprintf "CharLit('%c')" c
  | IntLit(i) -> Printf.sprintf "IntLit(%d)" i
  | FloatLit(f) -> Printf.sprintf "FloatLit(%f)" f
  | StringLit(s) -> Printf.sprintf "StringLit(\"%s\")" s
  | Ident(i) -> Printf.sprintf "Ident(%s)" i
  | Array(a) -> Printf.sprintf "Array([%s])" (f a)
  | AnonStruct(l) -> Printf.sprintf "AnonStruct([%s])" (g l)
  | IndexAccess(a, b) -> Printf.sprintf "IndexAccess(%s, %s)" (string_of_expr a) (string_of_expr b)
  | MemberAccess(a, b) -> Printf.sprintf "MemberAccess(%s, %s)" (string_of_expr a) b
  | AddrOf(e') -> Printf.sprintf "AddrOf(%s)" (string_of_expr e')
  | Deref(e') -> Printf.sprintf "Deref(%s)" (string_of_expr e')
  | CastTo(t, e') -> Printf.sprintf "CastTo(%s, %s)" (string_of_type t) (string_of_expr e')
  | Not(e') -> Printf.sprintf "Not(%s)" (string_of_expr e')
  | Neg(e') -> Printf.sprintf "Neg(%s)" (string_of_expr e')
  | Add(a, b) -> Printf.sprintf "Add(%s, %s)" (string_of_expr a) (string_of_expr b)
  | Sub(a, b) -> Printf.sprintf "Sub(%s, %s)" (string_of_expr a) (string_of_expr b)
  | Mul(a, b) -> Printf.sprintf "Mul(%s, %s)" (string_of_expr a) (string_of_expr b)
  | Div(a, b) -> Printf.sprintf "Div(%s, %s)" (string_of_expr a) (string_of_expr b)
  | Mod(a, b) -> Printf.sprintf "Mod(%s, %s)" (string_of_expr a) (string_of_expr b)
  | Eq(a, b) -> Printf.sprintf "Eq(%s, %s)" (string_of_expr a) (string_of_expr b)
  | NEq(a, b) -> Printf.sprintf "NEq(%s, %s)" (string_of_expr a) (string_of_expr b)
  | Lt(a, b) -> Printf.sprintf "Lt(%s, %s)" (string_of_expr a) (string_of_expr b)
  | Le(a, b) -> Printf.sprintf "Le(%s, %s)" (string_of_expr a) (string_of_expr b)
  | Gt(a, b) -> Printf.sprintf "Gt(%s, %s)" (string_of_expr a) (string_of_expr b)
  | Ge(a, b) -> Printf.sprintf "Ge(%s, %s)" (string_of_expr a) (string_of_expr b)
  | Or(a, b) -> Printf.sprintf "Or(%s, %s)" (string_of_expr a) (string_of_expr b)
  | And(a, b) -> Printf.sprintf "And(%s, %s)" (string_of_expr a) (string_of_expr b)
  | BNeg(e') -> Printf.sprintf "BNeg(%s)" (string_of_expr e')
  | BOr(a, b) -> Printf.sprintf "BOr(%s, %s)" (string_of_expr a) (string_of_expr b)
  | BAnd(a, b) -> Printf.sprintf "BAnd(%s, %s)" (string_of_expr a) (string_of_expr b)
  | BXOr(a, b) -> Printf.sprintf "BXOr(%s, %s)" (string_of_expr a) (string_of_expr b)
  | BSL(a, b) -> Printf.sprintf "BSL(%s, %s)" (string_of_expr a) (string_of_expr b)
  | BSR(a, b) -> Printf.sprintf "BSR(%s, %s)" (string_of_expr a) (string_of_expr b)
  | FuncCall(fname, args) -> Printf.sprintf "FuncCall(%s, [%s])" fname (f args)
  | Ternary(a, b, c) -> 
    Printf.sprintf 
      "Ternary(%s, %s, %s)" 
      (string_of_expr a) 
      (string_of_expr b) 
      (string_of_expr c)
  | SizeOf(t) -> Printf.sprintf "SizeOf(%s)" (string_of_type t)

let rec string_of_stmt s =
  let f stmts = 
    stmts
    |> List.map string_of_stmt
    |> String.concat "; "
  in
  match s with
  | Break -> "Break"
  | Continue -> "Continue"
  | EmptyReturn -> "EmptyReturn"
  | Return(e) -> Printf.sprintf "Return(%s)" (string_of_expr e)
  | Expr(e) -> Printf.sprintf "Expr(%s)" (string_of_expr e)
  | Assign(a, b) -> Printf.sprintf "Assign(%s, %s)" (string_of_expr a) (string_of_expr b)
  | VarDef(t, name, v) -> Printf.sprintf "VarDef(%s, %s, %s)" (string_of_type t) name (string_of_expr v)
  | EmptyVarDef(t, name) -> Printf.sprintf "EmptyVarDef(%s, %s)" (string_of_type t) name
  | Block(b) -> Printf.sprintf "Block([%s])" (f b)
  | WhileLoop(c, b) -> Printf.sprintf "WhileLoop(%s, [%s])" (string_of_expr c) (f b)
  | ForLoop(Some(init), cond, step, b) ->
    let init' = string_of_stmt init in
    let cond' = string_of_expr cond in
    let step' = string_of_stmt step in
    Printf.sprintf "ForLoop(Some(%s), %s, %s, [%s])"
      init'
      cond'
      step'
      (f b)
  | ForLoop(None, cond, step, b) ->
    let cond' = string_of_expr cond in
    let step' = string_of_stmt step in
    Printf.sprintf "ForLoop(None, %s, %s, [%s])"
      cond'
      step'
      (f b)
  | IfStmt(c, _t, _f) ->
    Printf.sprintf "IfStmt(%s, [%s], [%s])"
      (string_of_expr c)
      (f _t)
      (f _f)
  | ElifChain(l) ->
    let s' = 
      l
      |> List.map (fun (cond, body) -> Printf.sprintf "%s, [%s]" (string_of_expr cond) (f body))
      |> String.concat "; "
    in
    Printf.sprintf "ElifChain([%s])" s'
  | IncPost(e) -> Printf.sprintf "IncPost(%s)" (string_of_expr e)
  | DecPost(e) -> Printf.sprintf "DecPost(%s)" (string_of_expr e)
  | IncPre(e) -> Printf.sprintf "IncPre(%s)" (string_of_expr e)
  | DecPre(e) -> Printf.sprintf "DecPre(%s)" (string_of_expr e)
  | Goto(s) -> Printf.sprintf "Goto(%s)" s
  | Label(s) -> Printf.sprintf "Label(%s)" s

type s_or_e = S of c_stmt | E of c_expr

type struct_decl = string * ((string * c_type) list)
type union_decl = string * ((string * c_type) list)

(*
type struct_w_unions =
  | PlainStruct of string * ((string * c_type) list)
  (* name, common fields, unionized members... *)
  | StructAndUnions of string * ((string * c_type) list) * (struct_w_unions list)
*)

type struct_w_unions = (* StructAndUnions of *) string * ((string * c_type) list) * ((string * c_type) list)

type c_toplevel =
  | LocalImport of string
  | GlobalImport of string
  | StructDecl of struct_decl
  | GlobalVarDecl of c_type * string
  | Macro of string * c_expr
  | Enum of string * string list
  | StructWithUnions of struct_w_unions
  | Const of string * c_expr
  | FuncDecl of c_type * string * (c_type list)
  | FuncDef of c_type * string * (c_type * string) list * c_stmt list
  | MultiLineComment of string
  (* Need to be able to combine structs and unions... *)

type t_or_p =
  | T of string
  | P of string

type context = {
  local_imports : string list;
  global_imports : string list;
  includes : c_toplevel list;
  structs  : c_toplevel list;
  effects  : c_toplevel list;
  enums : c_toplevel list;
  modetypes: c_toplevel list;
  globals  : c_toplevel list;
  macros   : c_toplevel list;
  func_decls : c_toplevel list;
  func_defs : c_toplevel list;
  init_stmts : c_stmt list;
  task_create_init_stmts : c_stmt list;
  queue_create_init_stmts : c_stmt list;
  semaphore_create_init_stmts : c_stmt list;
  var_init_init_stmts : c_stmt list;
  type_translation_table : (Types.ground_type * int) list;
  cur_name : string;
  task_or_promise : t_or_p list;
  multiline_comments: c_toplevel list
}

let empty_ctx = 
  { local_imports = []
  ; global_imports = []
  ; includes = []
  ; structs = []
  ; effects = []
  ; enums = []
  ; modetypes = []
  ; globals = []
  ; macros = []
  ; func_decls = []
  ; func_defs = []
  ; init_stmts = []
  ; task_create_init_stmts = []
  ; queue_create_init_stmts = []
  ; semaphore_create_init_stmts = []
  ; var_init_init_stmts = []
  ; type_translation_table = []
  ; cur_name = ""
  ; task_or_promise = []
  ; multiline_comments = []
  }

let local_imports_of { local_imports } = local_imports
let global_imports_of { global_imports } = global_imports
let includes_of { includes } = includes
let structs_of { structs } = structs
let effects_of { effects } = effects
let enums_of { enums } = enums
let modetypes_of { modetypes } = modetypes
let globals_of { globals } = globals
let macros_of { macros } = macros
let func_decls_of { func_decls } = func_decls
let func_defs_of { func_defs } = func_defs
let init_stmts_of { init_stmts } = init_stmts
let type_translation_table_of { type_translation_table } = type_translation_table
let cur_name_of { cur_name } = cur_name
let task_or_promise_of { task_or_promise } = task_or_promise
let multine_comments_of { multiline_comments } = multiline_comments

let ctx_of_local_imports li ctx =
  { ctx with local_imports = li }

let ctx_of_global_imports gi ctx =
  { ctx with global_imports = gi }

let ctx_of_includes i ctx =
  { ctx with includes = i }

let ctx_of_added_includes i ctx =
  let { includes } = ctx in
  { ctx with includes = i @ includes }

let ctx_of_structs st ctx =
  { ctx with structs = st }

let ctx_of_added_structs st ctx =
  let { structs } = ctx in
  { ctx with structs = st @ structs }

let ctx_of_effects ef ctx =
  { ctx with effects = ef }

let ctx_of_enums en ctx =
  { ctx with enums = en }

let ctx_of_added_enums en ctx =
  let { enums } = ctx in
  { ctx with enums = en @ enums }

let ctx_of_modetypes mt ctx =
  { ctx with modetypes = mt }

let ctx_of_globals gl ctx =
  { ctx with globals = gl }

let ctx_of_added_globals gl ctx =
  let { globals } = ctx in
  { ctx with globals = gl @ globals }

let ctx_of_macros mc ctx =
  { ctx with macros = mc }

let ctx_of_added_macros mc ctx =
  let { macros } = ctx in
  { ctx with macros = mc @ macros }

let ctx_of_func_decls fd ctx =
  { ctx with func_decls = fd }

let ctx_of_added_func_decls fd ctx =
  let { func_decls } = ctx in
  { ctx with func_decls = (fd @ func_decls) }

let ctx_of_func_defs fd ctx =
  { ctx with func_defs = fd }

let ctx_of_added_func_defs fd ctx =
  let { func_defs } = ctx in
  { ctx with func_defs = (fd @ func_defs) }

let ctx_of_init_stmts st ctx =
  { ctx with init_stmts = st }

let ctx_of_added_init_stmts st ctx = let { init_stmts } = ctx in
  { ctx with init_stmts = init_stmts @ st }

let ctx_of_type_translation_table ttt ctx =
  { ctx with type_translation_table = ttt }

let ctx_of_added_type_translation_table ttt ctx =
  let { type_translation_table } = ctx in
  { ctx with type_translation_table = ttt @ type_translation_table }

let ctx_of_cur_name n ctx =
  { ctx with cur_name = n }

let ctx_of_task_or_promise tp ctx =
  { ctx with task_or_promise = tp }

let ctx_of_added_task_or_promise tp ctx =
  let { task_or_promise } = ctx in
  { ctx with task_or_promise = tp @ task_or_promise }

let ctx_of_multiline_comments mc ctx =
  { ctx with multiline_comments = mc }

let ctx_of_added_multiline_comments mc ctx =
  let { multiline_comments } = ctx in
  { ctx with multiline_comments = mc @ multiline_comments }

let var_cnt = ref 0
let new_var_name () =
  let c = !var_cnt in
  incr var_cnt;
  Printf.sprintf "new_local_%d" c

let rec convert_my_type_to_c t =
  let open Types in
  match t with
  | Unit -> Void
  | Bool -> Bool
  | Char -> Char
  | Int -> Int
  | Float -> Float
  | String -> String
  | CustomType(s) -> CustomType(s)
  | SizedArray(t', n) -> failwith "TODO: convert sized_array type to C"
  | UnsizedArray(t') -> failwith "TODO: convert unsized_array type to C"
  | Function(_, _) -> failwith "TODO: convert function type to C"
  | Struct(_) -> failwith "TODO: convert struct type to C"
  | Tuple(_) -> failwith "TODO: convert tuple type to C"
  | Sumtype(_) -> failwith "TODO: convert sumtype to C"
  | SumtypeDef(_) -> failwith "TODO: convert sumtype def to C"
  | Modetype(_) -> failwith "TODO: convert modetype to C"
  | ModetypeDef(_) -> failwith "TODO: convert modetype def to C"
  | Promise(_) -> failwith "TODO: convert promise type to C"
  | PromiseDef(_, _, _, _) -> failwith "TODO: convert promise def type to C"
  | Effect(_) -> failwith "TODO: convert effect type to C"
  | EffectDef(_, _) -> failwith "TODO: convert effect def type to C"
  | Mutable(t') -> convert_my_type_to_c t'
  | Global(t') -> convert_my_type_to_c t'
  | Poly(i) -> CustomType(Printf.sprintf "weak_%d" i)
  | AnyType -> Pointer(Void)

let rec translate_my_type_to_c t_ctx ctx t =
  let f = translate_my_type_to_c t_ctx ctx in
  let type_translation_table = type_translation_table_of ctx in
  let open Types in
  match t with
  | Unit -> Void
  | Bool -> Bool
  | Char -> Char
  | Int -> Int
  | Float -> Float
  | String -> String
  | CustomType(s) -> CustomType(s)
  | SizedArray(t', i) -> ArrayWSize(f t', i)
  | UnsizedArray(t') -> Array(f t')
  | Function(args, rtype) -> FuncDeclType(List.map f args, f rtype)
  | Struct(_) -> convert_my_type_to_c t
  | Tuple(l) -> 
    (match List.assoc_opt t type_translation_table with
    | Some(i) -> CustomType(Printf.sprintf "my_type_%d" i)
    | None -> failwith @@ Printf.sprintf "Can't translate my type to C: %s" (Types.string_of_ground_type t))
  | Sumtype(_) ->
    (match List.assoc_opt t type_translation_table with
    | Some(i) -> CustomType(Printf.sprintf "my_type_%d" i)
    | None -> failwith @@ Printf.sprintf "Can't translate my type to C: %s" (Types.string_of_ground_type t))
  | SumtypeDef(_) -> (* failwith "Can't translate the sumtype definition!" *)
    (match List.assoc_opt t type_translation_table with
    | Some(i) -> CustomType(Printf.sprintf "my_type_%d" i)
    | None -> failwith @@ Printf.sprintf "Can't translate my type to C: %s" (Types.string_of_ground_type t))
  | Modetype(_) ->
    (match List.assoc_opt t type_translation_table with
    | Some(i) -> CustomType(Printf.sprintf "my_type_%d" i)
    | None -> failwith @@ Printf.sprintf "Can't translate my type to C: %s" (Types.string_of_ground_type t))
  | ModetypeDef(modetype_name, modes) -> 
    CustomType(modetype_name)
      (* failwith "Can't translate modetype definition!" *)
  | Promise(s) -> failwith "translate_my_type_to_c: Should be not necessary"
  | PromiseDef(_, _, _, _) -> failwith "translate_my_type_to_c: Should be not necessary"
  | Effect(n) -> CustomType(n)
      (*
    (match List.assoc_opt t type_translation_table with
    | Some(i) -> CustomType(Printf.sprintf "my_type_%d" i)
    | None -> failwith @@ Printf.sprintf "Can't translate my type to C: %s" (Types.string_of_ground_type t))
    *)
  | EffectDef(n, _) -> CustomType(n)
      (*
    (match List.assoc_opt t type_translation_table with
    | Some(i) -> CustomType(Printf.sprintf "my_type_%d" i)
    | None -> failwith @@ Printf.sprintf "Can't translate my type to C: %s" (Types.string_of_ground_type t))
    *)
    (* failwith "Can't translate effect definition!" *)
  | Mutable(t') -> f t'
  | Global(t') -> f t'
  | Poly(_) -> convert_my_type_to_c t
  | AnyType -> convert_my_type_to_c t
  (* failwith "TODO: translate_my_type_to_c" *)

(*
let unsafe_e = function
  | E(e) -> e
  | S(_) -> failwith "unsafe_e: expected expr, but got stmt instead!"

let unsafe_s = function
  | S(s) -> s
  | E(_) -> failwith "unsafe_s: expected stmt, but got expr instead!"
*)

let rec is_instantiable_type t =
  let open Types in
  match t with
  | Bool
  | Char
  | Int
  | Float
  | String
  | CustomType(_)
  | Struct(_, _) -> true (* assume that all struct fields are ok *)
  | Tuple(tl) -> 
    ([] = List.filter (fun x -> x = false) @@ List.map is_instantiable_type tl)
  | SizedArray(t, _)
  | UnsizedArray(t) -> is_instantiable_type t
  | Sumtype(_, _) -> true (* assume that sumtype is ok *)
  | Modetype(_) -> true
  | Effect(_) -> true (* assume that effect is ok *)
  | Unit -> false
  | _ -> false

type c_expr_res =
  { prelude : c_stmt list
  ; value : c_expr
  ; e_macros : c_toplevel list
  ; e_init_stmts : c_stmt list
  ; e_globals : c_toplevel list
  ; used_mode_switch : bool
  }

let prelude_of { prelude } = prelude
let value_of { value } = value
let e_macros_of { e_macros } = e_macros
let e_init_stmts_of { e_init_stmts } = e_init_stmts
let e_globals_of { e_globals } = e_globals
let used_mode_switch_of { used_mode_switch } = used_mode_switch

let res_with_value v res =
  { res with value = v }

let res_with_prelude p res =
  { res with prelude = p }

let res_with_used_mode_switch res =
  { res with used_mode_switch = true }

let res_with_globals g res =
  { res with e_globals = g }

let res_with_init_stmts i res =
  { res with e_init_stmts = i }

let pack prelude value e_macros e_init_stmts e_globals used_mode_switch =
  { prelude; value; e_macros; e_init_stmts; e_globals; used_mode_switch }

let unpack { prelude; value; e_macros; e_init_stmts; e_globals; used_mode_switch } =
  (prelude, value, e_macros, e_init_stmts, e_globals, used_mode_switch)

let empty_res = pack [] Null [] [] [] false

let pack_v v =
  pack [] v [] [] [] false
  
let pack_pv p v =
  pack p v [] [] [] false

let pack_pvm p v m =
  pack p v m [] [] false

let pack_pvi p v i =
  pack p v [] i [] false

let pack_concat_l a b =
  let ( p_a, v_a, m_a, i_a, g_a, u_a) = unpack a in
  let ( p_b,   _, m_b, i_b, g_b, u_b) = unpack b in
  pack (p_a @ p_b) (v_a) (m_a @ m_b) (i_a @ i_b) (g_a @ g_b) (u_a || u_b)

let pack_concat_r a b =
  let ( p_a,   _, m_a, i_a, g_a, u_a) = unpack a in
  let ( p_b, v_b, m_b, i_b, g_b, u_b) = unpack b in
  pack (p_a @ p_b) (v_b) (m_a @ m_b) (i_a @ i_b) (g_a @ g_b) (u_a || u_b)

let rec prune_empty_blocks l =
  let rec loop l =
    match l with
    | [] -> []
    | Block(l') :: rest ->
      (match prune_empty_blocks l' with
      | [] -> loop rest
      | x -> Block(x) :: (loop rest))
    | x :: rest -> (x) :: (loop rest)
  in
  loop l

let rec c_expr_of_expr (cur_task : string option) (cur_promise: (string * int option * string) option) t_ctx c_ctx e =
  let f = c_expr_of_expr cur_task cur_promise t_ctx c_ctx in
  let binop_e g a b =
    let res_1 = f a in
    let res_2 = f b in
    let res = pack_concat_r res_1 res_2 in
    let a' = value_of res_1 in
    let b' = value_of res_2 in
    res_with_value (g a' b') res
  in
  let open Typecheck in
  match e with
  | Unit_t -> pack_v Null
  | Bool_t(b) -> pack_v (BoolLit b)
  | Char_t(c) -> pack_v (CharLit c)
  | Int_t(i) -> pack_v (IntLit i)
  | Float_t(f) -> pack_v (FloatLit f)
  | String_t(s) -> pack_v (StringLit s)
  | Ident_t(t, i) -> pack_v (Ident i)
  | Array_t(t, a) -> 
    let (res', l) = 
      List.fold_left 
        (fun (acc, l) x ->
          let x' = f x in
          let x_v = value_of x' in
          (pack_concat_r acc x', l @ [x_v]))
        (empty_res, [])
        a 
    in
    res_with_value (Array(l)) res'
  | Tuple_t(t, l) -> 
    (* Need to create a variable of a tuple struct and assign all the fields *)
    (* failwith "TODO: c_expr_of_expr: convert tuple to struct" *)
    let t' = translate_my_type_to_c t_ctx c_ctx t in
    let (res', l) =
      List.fold_left
        (fun (acc, l) x ->
          let x' = f x in
          let v = value_of x' in
          (pack_concat_r (acc) x', (l @ [("pos_", v)])))
        (empty_res, [])
        l
    in
    res_with_value 
      (CastTo(
        t', 
        AnonStruct(
          List.mapi 
            (fun i (name, x) -> (name ^ string_of_int i, x)) 
            l
        )
      )) 
      res'
  | FuncCall_t (_, name, args) -> 
    let (res, args') =
      List.fold_left 
        (fun (acc, l') (x) ->
          let x' = f x in
          let v = value_of x' in
          (pack_concat_r acc x', (l' @ [v])))
        (empty_res, [])
        args
    in
    let v = FuncCall(name, args') in
    res_with_value v res
    (* failwith "TODO: convert funccall" *)
  | CastTo_t (t, e') -> 
    let t' = translate_my_type_to_c t_ctx c_ctx t in
    let res = f e' in
    let v = value_of res in
    res_with_value (CastTo(t', v)) res
    (* failwith "TODO: convert cast to" *)
  | Not_t e' -> 
    let res = f e' in
    let v = value_of res in
    res_with_value (Not(v)) res
    (* failwith "TODO: convert not" *)
  | Neg_t e' -> 
    let res = f e' in
    let v = value_of res in
    res_with_value (Neg(v)) res
    (* failwith "TODO: convert neg" *)
  | Add_t (a, b) -> 
    let res = binop_e (fun a b -> Add(a, b)) a b in
    assert (prelude_of res = (prelude_of (f a)) @ (prelude_of (f b)));
    res
    (* failwith "TODO: convert add" *)
  | Sub_t (a, b) -> 
    binop_e (fun a b -> Sub(a, b)) a b
    (* failwith "TODO: convert sub" *)
  | Mul_t (a, b) -> 
    binop_e (fun a b -> Mul(a, b)) a b
    (* failwith "TODO: convert mul" *)
  | Div_t (a, b) -> 
    binop_e (fun a b -> Div(a, b)) a b
    (* failwith "TODO: convert div" *)
  | Mod_t (a, b) -> 
    binop_e (fun a b -> Mod(a, b)) a b
    (* failwith "TODO: convert mod" *)
  | FAdd_t (a, b) -> 
    binop_e (fun a b -> Add(a, b)) a b
    (* failwith "TODO: convert fadd" *)
  | FSub_t (a, b) -> 
    binop_e (fun a b -> Sub(a, b)) a b
    (* failwith "TODO: convert fsub" *)
  | FMul_t (a, b) -> 
    binop_e (fun a b -> Mul(a, b)) a b
    (* failwith "TODO: convert fmul" *)
  | FDiv_t (a, b) -> 
    binop_e (fun a b -> Div(a, b)) a b
    (* failwith "TODO: convert fdiv" *)
  | Eq_t (a, b) -> 
    binop_e (fun a b -> Eq(a, b)) a b
    (* failwith "TODO: convert eq" *)
  | Neq_t (a, b) -> 
    binop_e (fun a b -> NEq(a, b)) a b
    (* failwith "TODO: convert neq" *)
  | Lt_t (a, b) -> 
    binop_e (fun a b -> Lt(a, b)) a b
    (* failwith "TODO: convert lt" *)
  | Le_t (a, b) -> 
    binop_e (fun a b -> Le(a, b)) a b
    (* failwith "TODO: convert le" *)
  | Gt_t (a, b) -> 
    binop_e (fun a b -> Gt(a, b)) a b
    (* failwith "TODO: convert gt" *)
  | Ge_t (a, b) -> 
    binop_e (fun a b -> Ge(a, b)) a b
    (* failwith "TODO: convert ge" *)
  | LAnd_t (a, b) -> 
    binop_e (fun a b -> And(a, b)) a b
    (* failwith "TODO: convert land" *)
  | LOr_t (a, b) -> 
    binop_e (fun a b -> Or(a, b)) a b
    (* failwith "TODO: convert lor" *)
  | BNeg_t e' -> 
    let res = f e' in
    let v = value_of res in
    res_with_value (BNeg(v)) res
    (* failwith "TODO: convert bneg" *)
  | BAnd_t (a, b) -> 
    binop_e (fun a b -> BOr(a, b)) a b
    (* failwith "TODO: convert band" *)
  | BOr_t (a, b) -> 
    binop_e (fun a b -> BAnd(a, b)) a b
    (* failwith "TODO: convert bor" *)
  | BXOr_t (a, b) -> 
    binop_e (fun a b -> BXOr(a, b)) a b
    (* failwith "TODO: convert bxor" *)
  | BShiftLeft_t (a, b) -> 
    binop_e (fun a b -> BSL(a, b)) a b
    (* failwith "TODO: convert bshiftleft" *)
  | BShiftRight_t (a, b) -> 
    binop_e (fun a b -> BSR(a, b)) a b
    (* failwith "TODO: convert bshiftright" *)
  | IndexAccess_t (_, a, b) -> 
    binop_e (fun a b -> IndexAccess(a, b)) a b
    (* failwith "TODO: convert index access" *)
  | MemberAccess_t (_, a, b) -> 
    let res = f a in
    let a' = value_of res in
    res_with_value (MemberAccess(a', b)) res
    (* failwith "TODO: convert member access" *)
  | Match_t (t, _, v, branches) -> 
    (* How to convert this to expr...? *)
    (* need to convert pattern to if statement *)
    (* need to bind variables to patterns *)
    (* need to convert branch to code *)
    (* need to assign result of the branch to res var *)
    (* let (prelude_v, v') = f v in *)
    let res_v = f v in
    let v' = value_of res_v in
    let prelude_v = prelude_of res_v in
    let pat_to_cond pat =
      let fold_left_i (f : int -> 'a -> 'b -> 'a) zero l =
        let rec loop i succ l =
          match l with
          | [] -> succ
          | x :: rest -> loop (i + 1) (f i succ x) rest
        in
        loop 0 zero l
      in
      let rec loop cur_path pat =
      (* In case of Any pat, the value is true *)
      (* In case of variable, the value is true, but *)
        (* - The variable binds value, so we need *)
        (* - var name *)
        (* - var type *)
        (* - the path to bind the var to *)
        let open Typecheck in
        match pat with
        | Unit_t -> [], []
        | Bool_t(true) -> [], [Eq(cur_path, BoolLit(true))]
        | Bool_t(false) -> [], [Eq(cur_path, BoolLit(false))]
        | Char_t(c) -> [], [Eq(cur_path, CharLit(c))]
        | Int_t(i) -> [], [Eq(cur_path, IntLit(i))]
        | Float_t(_f) -> [], [Eq(cur_path, FloatLit(_f))]
        | String_t(s) -> [], [
            Eq(
              FuncCall(
                "strncmp", 
                [ cur_path
                ; StringLit(s)
                ; IntLit(String.length s)
                ]
              ), 
              IntLit(0)
            )
          ]
        | Ident_t(t, ident) ->
          [(ident, t, cur_path)], []
          (* failwith "TODO: convert pattern matching on identifier" *)
        | Array_t(arr_t, arr_values) -> 
          (* How to pattern match on array? *)
          let fn i (vars, conds) pat =
            let cur_path' = IndexAccess(cur_path, IntLit(i)) in
            let (a, b) = loop cur_path' pat in
            (vars @ a, conds @ b)
          in
          fold_left_i fn ([], []) arr_values
          (* failwith "TODO: convert pattern matching on array" *)
        | Tuple_t(tuple_t, tuple_values) ->
          (* How to pattern match on tuple? *)
          let fn i (vars, conds) pat =
            let cur_path' = MemberAccess(cur_path, Printf.sprintf "pos_%d" i) in
            let (a, b) = loop cur_path' pat in
            (vars @ a, conds @ b)
          in
          let (vars, tuples) = fold_left_i fn ([], []) tuple_values in
          let tuples' = List.fold_left (fun acc x -> (And(acc, x))) (BoolLit(true)) tuples in
          (vars, [tuples'])
          (* failwith "TODO: convert pattern matching on tuple" *)
        | AnonStruct_t(_, anon_struct) ->
          let fn (vars, conds) (mem, pat) =
            let cur_path' = MemberAccess(cur_path, mem) in
            let (a, b) = loop cur_path' pat in
            (vars @ a, conds @ b)
          in
          List.fold_left fn ([], []) anon_struct
          (* failwith "TODO: convert pattern matching on anon struct" *)
        | Variant_t(t, name, values) ->
          let custom_types = Typecheck.custom_types_of t_ctx in
          let sumtype_name =
            match t with
            | Sumtype(sumtype_name, _) -> sumtype_name
            | _ -> failwith @@ Printf.sprintf 
              "Conversion error: expected %s to have sumtype type, but something went wrong"
              name
          in
          let sumtype_def = 
            match List.assoc_opt sumtype_name custom_types with
            | Some(Types.SumtypeDef(variants)) -> variants
            | None -> failwith "Conversion error: expected %s to be a sumtype def"
          in
          let find_variant n =
            let rec loop i l =
              match l with
              | [] -> failwith @@ Printf.sprintf 
                      "Conversion error: Couldn't find the variant %s" n
              | (x, _) :: rest ->
                if x = n then
                  i
                else
                  loop (i + 1) rest
            in
            loop 0 sumtype_def
          in
          let cond = Eq(MemberAccess(cur_path, "var_type"), IntLit(find_variant name)) in
          let tt = Types.Sumtype(sumtype_name, name) in
          let cast_t = translate_my_type_to_c t_ctx c_ctx tt in
          let acc = CastTo(cast_t, MemberAccess(cur_path, "contents")) in
          let fn i (vars, conds) (pat) =
            let cur_path' = MemberAccess(acc, Printf.sprintf "pos_%d" i) in
            let (a, b) = loop cur_path' pat in
            (vars @ a, conds @ b)
          in
          fold_left_i fn ([], []) values
          (* failwith "TODO: convert pattern matching on variant" *)
        | Modetype_t(t, mode) ->
          let cond = Eq(cur_path, Ident(mode)) in
          [], [cond]
          (* failwith "TODO: convert pattern matching on modetype" *)
        | Any_t ->
          [], []
          (* failwith "TODO: convert pattern matching on any pat" *)
        (* failwith "TODO: pat_to_cond" *)
      in
      loop v' pat
    in
    (match is_instantiable_type t with
    | true -> (**)
      let t' = translate_my_type_to_c t_ctx c_ctx t in
      let res_var = new_var_name () in
      let res_var_decl = EmptyVarDef(t', res_var) in
      let translate_branch (_, pat, branch) =
        let (vars, cond) = pat_to_cond pat in
        let (cond' : c_expr) = List.fold_left (fun acc x -> (And(acc, x))) (BoolLit(true)) cond in
        let res_branch = f branch in
        let branch' = value_of res_branch in
        let prelude_branch = prelude_of res_branch in
        (* let (prelude_branch, branch') = f branch in *)
        let var_decls = 
          List.map (fun (name, t, v) ->
            let t' = translate_my_type_to_c t_ctx c_ctx t in
            VarDef(t', name, v)
          )
          vars
        in
        let assign = Assign(Ident(res_var), branch') in
        let prelude_branch' = assign :: (var_decls @ prelude_branch) in
        (res_branch, cond', prelude_branch', res_var)
      in
      let branches' =
        List.map (translate_branch) branches 
      in
      let res_var = 
        match branches' with
        | [] -> 
          failwith "Conversion error: didn't expect the pattern matching patterns to be empty"
        | (_, _, _, x) :: _ -> x
      in
      let branches'' = 
        List.map (fun (_, b, c, _) -> (b, c)) branches' 
      in
      let _if = ElifChain(branches'') in
      let res_branches = 
        res_with_value Null @@
          res_with_prelude [] @@
            List.fold_left pack_concat_r empty_res @@ 
              List.cons res_v @@
                List.map (fun (x, _, _, _) -> x) branches'
      in
      (* failwith "TODO: c_expr_of_expr: match case: instantiable type" *)
      res_branches
      |> res_with_prelude [res_var_decl; _if]
      |> res_with_value (Ident(res_var))
    | false -> (**)
      let translate_branch (_, pat, branch) =
        let (vars, cond) = pat_to_cond pat in
        let (cond' : c_expr) = List.fold_left (fun acc x -> (And(acc, x))) (BoolLit(true)) cond in
        (* let (prelude_branch, branch') = f branch in *)
        let res_branch = f branch in
        let branch' = value_of res_branch in
        let prelude_branch = prelude_of res_branch in
        let var_decls = 
          List.map (fun (name, t, v) ->
            let t' = translate_my_type_to_c t_ctx c_ctx t in
            VarDef(t', name, v)
          )
          vars
        in
        let prelude_branch' = (var_decls @ prelude_branch) in
        (res_branch, cond', prelude_branch', Null)
      in
      let branches' = List.map (translate_branch) branches in
      let branches'' = List.map (fun (_, b, c, _) -> (b, c)) branches' in
      let res_branches = 
        res_with_value Null @@
          res_with_prelude [] @@
            List.fold_left pack_concat_r empty_res @@ 
              List.cons res_v @@
                List.map (fun (x, _, _, _) -> x) branches'
      in
      let _if = ElifChain(branches'') in
      res_branches
      |> res_with_prelude [_if]
      |> res_with_value (Null)
      (* [_if], Null *)
      (* failwith "TODO: c_expr_of_expr: match case: non-instantiable type" *)
    )
    (* failwith "Conversion error: can't convert match statement to expr!" *)
  | Await_t(t, o, prom_name, p_id) -> 
    (* need to await the result from the promise *)
    let t' = translate_my_type_to_c t_ctx c_ctx t in
    let res_var = new_var_name () in
    let res_var_decl = EmptyVarDef(t', res_var) in
    let cur_task_name =
      match cur_task with
      | Some(name) -> name
      | None -> failwith "Conversion error: awaiting outside of task context!"
    in
    let queue_name = Printf.sprintf "queue_%s_%s_%d" prom_name cur_task_name o in
    let queue_wait_macro = Printf.sprintf "QUEUE_%s_%d_WAIT_TICKS" prom_name o in
    let queue_size_macro = Printf.sprintf "QUEUE_%s_%d_ITEMS_NO" prom_name o in
    (* BUG: possibly the name of the queue type is wrong *)
    let globals = [ GlobalVarDecl(CustomType("QueueHandle_t"), queue_name) ] in 
    let macros = 
      [ Macro(queue_wait_macro, CastTo(CustomType("TickType_t"), IntLit(20)))
      ; Macro(queue_size_macro, CastTo(CustomType("TickType_t"), IntLit(20))) 
      ] 
    in
    let init_stmts = [ Assign(Ident(queue_name), FuncCall("xQueueCreate", [ Ident(queue_size_macro); SizeOf(t') ])) ] in
    let _while = 
      WhileLoop(BoolLit(true), [
        IfStmt(
          Eq(
            FuncCall(
              "xQueueReceive", 
              [ Ident(queue_name)
              ; AddrOf(Ident(res_var))
              ; CastTo(CustomType("TickType_t"), Ident(queue_wait_macro))
              ]), Ident("pdTRUE")
          ),
          [ Break ],
          []
        )
      ]) 
    in
    let prelude = [res_var_decl; _while] in
    let v = Ident(res_var) in
    pack prelude v macros init_stmts globals false
    (* failwith "TODO: convert await" *)
  | Raise_t (o, effname, args) -> 
    (* If there are less than 64 queues, we can use the bitmasks to efficiently utilize *)
    (* the space. *)
    (* The bit 1 signifies that we are yet to send the signal and *)
    (* the bit 0 signifies that we have sent the signal. *)
    (* Let's generate some efficient code *)
    let generate_guards l =
      (* Need to write code that given the statements, properly guards them *)
      let n = List.length l in
      let generate_63_guards n' l =
        let guard_var = new_var_name () in
        let t = 
          match n' with
          | n' when n' < 8 -> UInt8
          | n' when n' < 16 -> UInt16
          | n' when n' < 32 -> UInt32
          | n' when n' < 64 -> UInt64
          | _ -> failwith @@ "Conversion error: the number was expected to be less than " ^ 
                             "64 when generating guards!"
        in
        (* let t' = translate_my_type_to_c t_ctx c_ctx t in *)
        let t' = t in
        let bitmask = Sub(BSL(CastTo(t, IntLit(1)), IntLit(n)), IntLit(1)) in
        let guard_var_decl = VarDef(t', guard_var, bitmask) in
        let _if_of i name q =
          (* Trying to extract the bit *)
          let is_not_ok = Eq(BAnd(BSR(CastTo(t, Ident(guard_var)), IntLit(i)), IntLit(1)), IntLit(1)) in
          let queue_is_null = Eq(Ident(name), Null) in
          let cond = And(is_not_ok, Or(queue_is_null, q)) in
          let zero_mask = BSL(CastTo(t, IntLit(1)), IntLit(i)) in
          IfStmt(cond, [Assign(Ident(guard_var), Sub(CastTo(t, Ident(guard_var)), zero_mask))], [])
        in
        let ifs = 
          List.mapi (fun i x -> 
            match x with
            (*
            FuncCall(
              "xQueueSend", 
              [ Ident(x)
              ; AddrOf(Ident(eff_struct))
              ; CastTo(CustomType("TickType_t"), Ident(wait_macro))
              ]
            )
            *)
            | FuncCall("xQueueSend", [Ident(x'); AddrOf(Ident(eff_struct)); CastTo(CustomType("TickType_t"), _)]) as fcall -> 
              let _stmt = Eq(fcall, Ident("pdPASS")) in
              _if_of i x' _stmt
            | _ -> 
              print_endline @@ Printf.sprintf "DEBUG: something went wrong: %s" (string_of_expr x);
              failwith "Impossible case"
          ) 
          l 
        in
        (guard_var_decl, guard_var, ifs)
      in
      let pace l =
        let rec loop n l =
          let rec take_n n l =
            match n, l with
            | 0, _ -> [], l 
            | _, x :: rest ->
              let y, z = take_n (n - 1) rest in
              x :: y, z
          in
          if n > 63 then
            let a, b = take_n 63 l in
            let (guard_var_decl, guard_var, guards) = generate_63_guards 63 a in
            let (other_guard_var_decls, other_guard_vars, other_guards) = loop (n - 63) b in
            (guard_var_decl :: other_guard_var_decls, guard_var :: other_guard_vars, guards @ other_guards)
          else
            let (guard_var_decl, guard_var, guards) = generate_63_guards n l in
            ([guard_var_decl], [guard_var], guards)
        in
        loop (List.length l) l
      in
      let (guard_var_decls, guard_vars, guards) = pace l in
      let (f, rest) = 
        match guard_vars with 
        | x :: rest -> x, rest 
        | [] -> failwith "Generating guards: Impossible case"
      in
      let is_not_0 x = NEq(Ident(x), IntLit(0)) in 
      let land_chain = 
        List.fold_left 
          (fun acc x -> 
            Or(acc, is_not_0 x)) 
          (is_not_0 f) 
          rest
      in
      (* let yield_stmt = Expr(FuncCall("taskYIELD", [])) in *)
      let yield_stmt = Expr(FuncCall("vTaskDelay", [CastTo(CustomType("TickType_t"), IntLit(10))])) in
      guard_var_decls @ [WhileLoop(land_chain, guards @ [yield_stmt])]
      (* generate the if and do the magic *)
    in
    (* What I know: *)
    (* Raising of effects occurs in the context of one modetype at most *)
    let (res_args, args') = 
      List.fold_left (fun (p_cur, a_cur) x ->
        let x' = f x in
        (pack_concat_r p_cur x', a_cur @ [value_of x'])
      )
      (empty_res, [])
      args
    in
    let prelude_args = prelude_of res_args in
    let t = Types.CustomType(effname) in
    let t' = translate_my_type_to_c t_ctx c_ctx t in
    let eff_struct = new_var_name () in
    let cur_task_name = 
      match cur_task with
      | Some(name) -> name
      | None -> failwith @@ Printf.sprintf "Conversion error: raising outside of task context!"
    in
    let values = Ident(task_enum_of cur_task_name) :: args' in
    let fields = List.mapi (fun i x -> (Printf.sprintf "pos_%d" i, x)) values in
    let eff_decl = VarDef(t', eff_struct, AnonStruct(fields)) in
    let cur_name = cur_name_of c_ctx in 
    let parallels_resolved = Typecheck.parallels_resolved_of t_ctx in
    let needed_queues = 
      let (effs_to_promises, _) = parallels_resolved in
      let cur_task_name =
        match cur_task with
        | Some(name) -> name
        | None -> failwith "Conversion error: raising outside of the task context!"
      in
      let necessary_effs =
        List.filter (fun ((a, b), _) -> a = effname && b = cur_task_name) effs_to_promises
      in
      let triggered_promises = List.flatten @@ List.map snd necessary_effs in
      let queue_names = 
        List.map 
          (fun (p, _, v, t) -> 
            match v with
            | None -> Printf.sprintf "queue_%s_%s" p t
            | Some(v') -> Printf.sprintf "queue_%s_%s_%d_r" p t v'
          ) 
          triggered_promises
      in
      queue_names
      (* let effs_to_promises = List.flatten effs_to_promises in *)
      (*
      let promise_names =
        effs_to_promises
        |> List.filter (fun (effname', promise_name, i, _, _) ->
             effname = effname' && i = o
           )
        |> List.map (fun (effname', promise_name, _, _, _) -> 
             Printf.sprintf "queue_%s_%s" promise_name)
      in
      promise_names
      *)
    in
    let (putting_on_queue_statements, queue_macros, queue_init_stmts, queue_globals) =
      List.fold_left (fun (stmts, macros, init_stmts, globals) x -> 
        let wait_macro = Printf.sprintf "QUEUE_%s_SEND_WAIT_TICKS" x in
        (* let size_macro = Printf.sprintf "QUEUE_%s_CAPACITY" x in *)
        let wait_macro_def = Macro(wait_macro, CastTo(CustomType("TickType_t"), IntLit(20))) in
        (* let size_macro_def = Macro(size_macro, IntLit(20)) in *)
        (* let global = GlobalVarDecl(CustomType("QueueHandle_t"), x) in *)
        (* let init_stmt = Assign(Ident(x), FuncCall("xQueueCreate", [ Ident(size_macro); SizeOf(t') ])) in *)
        let stmt = 
          FuncCall(
            "xQueueSend", 
            [ Ident(x)
            ; AddrOf(Ident(eff_struct))
            ; CastTo(CustomType("TickType_t"), Ident(wait_macro))
            ]
          )
        in
        (stmt :: stmts, wait_macro_def :: macros, [ (* init_stmt *) ], globals)
      ) 
      ([], [], [], [])
      needed_queues
    in 
    let associated_modetype = 
      let tasks_and_modetypes = Typecheck.tasks_and_modetypes_of t_ctx in
      let cur_name = cur_name_of c_ctx in
      List.assoc_opt cur_name tasks_and_modetypes
    in
    (* Need to figure out if I need to put the guard of mutex *)
    (* The name of the mutexes will follow the mutex_MODETYPE_switch format *)
    let s, partial_init_stmts, partial_globals, used_mode_switching =
      match associated_modetype with
      | None ->
        (* no mode switching to think of: easy-peasy *)
        (* No, it is not so easy, but then again *)
        let _while = generate_guards putting_on_queue_statements in
        prelude_args @ [eff_decl] @ _while, [], [], false
        (* used_mode_switching is false *)
      | Some(modetype) ->
        (* have to consider the mode switching as well... *)
        (* Finish the if condition *)
        (* Make aggressive signal propagation *)
        let mutex_name = Printf.sprintf "mutex_%s_switch" modetype in
        let globals = [ GlobalVarDecl(CustomType("xSemaphoreHandle"), mutex_name) ] in
        let init_stmts = [ Expr(FuncCall("xSemaphoreCreateMutex", [])) ] in
        let _while = generate_guards putting_on_queue_statements in
        let check_if_mutex_free =
          IfStmt(
            Eq(
              FuncCall("xQueuePeek", [
                CastTo(CustomType("QueueHandle_t"), Ident(mutex_name));
                CastTo(Pointer(Void), Null);
                CastTo(CustomType("portTickType"), Null)
              ]), 
              Ident("pdFALSE")
            ),
            (* mutex is free *)
            (prelude_args @ [eff_decl] @ _while),
            [
              (* mutex is taken *)
            ]
          )
        in
        [check_if_mutex_free], init_stmts, globals, true
        (* used mode switching *)
    in
    let globals = partial_globals @ queue_globals in
    let macros = queue_macros in
    let init_stmts = partial_init_stmts @ queue_init_stmts in
    pack s Null macros init_stmts globals used_mode_switching
    (* Need to figure out on which queue to put the effect (DONE) *)
    (* failwith "TODO: convert raise" *)
  (*
  | Let (_, _, _) -> failwith "TODO: convert let"
  | LetMut (_, _, _) -> failwith "TODO: convert let mut"
  *)
  | If_t (t, c, _true, _false) -> 
    (* need to pattern match on type if it is assignable *)
    (match (is_instantiable_type t) with
    | true ->
      let _if_t_var = new_var_name () in
      let t_var_decl = EmptyVarDef(translate_my_type_to_c t_ctx c_ctx t, _if_t_var) in
      let res_c = f c in
      let res_t = f _true in
      let res_f = f _false in
      let ( prelude_c
          , c'
          , macros_c
          , init_stmts_c
          , globals_c
          , used_mode_switching_c
          ) = unpack res_c 
      in
      let ( prelude_t
          , _true'
          , macros_t
          , init_stmts_t
          , globals_t
          , used_mode_switching_t
          ) = unpack res_t 
      in
      let ( prelude_f
          , _false'
          , macros_f
          , init_stmts_f
          , globals_f
          , used_mode_switching_f
          ) = unpack res_f 
      in
      let assign_true = Assign(Ident(_if_t_var), _true') in
      let assign_false = Assign(Ident(_if_t_var), _false') in
      let _if = IfStmt(c', prelude_t @ [assign_true], prelude_f @ [assign_false]) in
      pack 
        (prelude_c @ [_if]) 
        (Ident(_if_t_var)) 
        (macros_c @ macros_t @ macros_f) 
        (init_stmts_c @ init_stmts_t @ init_stmts_f)
        (globals_c @ globals_t @ globals_f)
        (used_mode_switching_c || used_mode_switching_t || used_mode_switching_f)
      (* ... *)
      (* [t_var_decl] @ prelude_c @ [_if], Ident(_if_t_var) *)
    | false ->
      let res_c = f c in
      let res_t = f _true in
      let res_f = f _false in
      let ( prelude_c
          , c'
          , macros_c
          , init_stmts_c
          , globals_c
          , used_mode_switching_c
          ) = unpack res_c 
      in
      let ( prelude_t
          , _true'
          , macros_t
          , init_stmts_t
          , globals_t
          , used_mode_switching_t
          ) = unpack res_t 
      in
      let ( prelude_f
          , _false'
          , macros_f
          , init_stmts_f
          , globals_f
          , used_mode_switching_f
          ) = unpack res_f 
      in
      let _if = IfStmt(c', prelude_t, prelude_f) in
      pack 
        (prelude_c @ [_if]) 
        (Null) 
        (macros_c @ macros_t @ macros_f) 
        (init_stmts_c @ init_stmts_t @ init_stmts_f)
        (globals_c @ globals_t @ globals_f)
        (used_mode_switching_c || used_mode_switching_t || used_mode_switching_f))
    (* failwith "Conversion error: Can't convert if statement to expr!" *)
  | Struct_t -> failwith "Conversion error: should not have need to convert Struct_t"
  | AnonStruct_t(_, _as) -> 
    let (res, l') = 
      List.fold_left (fun (prelude, l') (field, x) ->
        let x' = f x in
        (pack_concat_r prelude x', l' @ [(field, value_of x')])
        (* (prelude @ p', l' @ [(field, x')]) *)
      )
      (empty_res, [])
      _as 
    in
    let st = AnonStruct(l') in
    res_with_value st res
    (* prelude, AnonStruct(l') *)
    (* AnonStruct(List.map (fun (field, x) -> (field, f x)) _as) *)
    (* failwith "TODO: convert anon struct" *)
  | While_t(t, c, b) -> 
    (match is_instantiable_type t with
    | true ->
      let res_c = f c in
      let res_b = f b in
      let ( prelude_c
          , c'
          , macros_c
          , init_stmts_c
          , globals_c
          , used_mode_switching_c
          ) = unpack res_c
      in
      let ( prelude_b
          , b'
          , macros_b
          , init_stmts_b
          , globals_b
          , used_mode_switching_b
          ) = unpack res_b
      in
      let t' = translate_my_type_to_c t_ctx c_ctx t in
      let t_var = new_var_name () in
      let var_decl = EmptyVarDef(t', t_var) in
      let e'' = b' in
      let assign = Assign(Ident(t_var), e'') in
      let _while = WhileLoop(c', prelude_b @ [assign] @ prelude_c) in
      pack
        (prelude_c @ [_while])
        (Ident(t_var))
        (macros_c @ macros_b)
        (init_stmts_c @ init_stmts_b)
        (globals_c @ globals_b)
        (used_mode_switching_c || used_mode_switching_b)
      (* (prelude_c @ [_while], Ident(t_var)) *)
      (* failwith "TODO: c_expr_of_expr: while branch" *)
    | false ->
      let res_c = f c in
      let res_b = f b in
      let ( prelude_c
          , c'
          , macros_c
          , init_stmts_c
          , globals_c
          , used_mode_switching_c
          ) = unpack res_c
      in
      let ( prelude_b
          , b'
          , macros_b
          , init_stmts_b
          , globals_b
          , used_mode_switching_b
          ) = unpack res_b
      in
      let _while = WhileLoop(c', prelude_b @ prelude_c) in
      pack
        (prelude_c @ [_while])
        (Null)
        (macros_c @ macros_b)
        (init_stmts_c @ init_stmts_b)
        (globals_c @ globals_b)
        (used_mode_switching_c || used_mode_switching_b)
      (* (prelude_c @ [_while], Null) *)
    )
    (*
    let (prelude_c, c') = f c in
    let (prelude_b, b') = f b in
    *)
    (* failwith "Conversion error: can't convert while to expr!" *)
  | ForTo_t(t, vname, beg, _end, body) -> 
    (match is_instantiable_type t with
    | true ->
      let res_beg = f beg in
      let res_end = f _end in
      let res_body = f body in
      let ( prelude_beg
          , beg'
          , macros_beg
          , init_stmts_beg
          , globals_beg
          , used_mode_switching_beg
          ) = unpack res_beg
      in
      let ( prelude_end
          , _end'
          , macros_end
          , init_stmts_end
          , globals_end
          , used_mode_switching_end
          ) = unpack res_end
      in
      let ( prelude_body
          , body'
          , macros_body
          , init_stmts_body
          , globals_body
          , used_mode_switching_body
          ) = unpack res_body
      in
      let t_var = new_var_name () in
      let t' = translate_my_type_to_c t_ctx c_ctx t in
      let var_decl = EmptyVarDef(t', t_var) in
      let assign = Assign(Ident(t_var), body') in
      let index = new_var_name () in
      let _for = ForLoop(
        Some(VarDef(Int, index, beg')), 
        Lt(Ident(index), _end'), 
        IncPost(Ident(index)),
        prelude_body @ [assign] @ prelude_beg @ prelude_end
        )
      in
      pack
        ([var_decl] @ prelude_beg @ prelude_end @ [_for])
        (Ident(t_var))
        (macros_beg @ macros_end @ macros_body)
        (init_stmts_beg @ init_stmts_end @ init_stmts_body)
        (globals_beg @ globals_end @ globals_body)
        (used_mode_switching_beg || used_mode_switching_end || used_mode_switching_body)
      (* ([var_decl] @ prelude_beg @ prelude_end @ [_for], Ident(t_var)) *)
    | false ->
      let res_beg = f beg in
      let res_end = f _end in
      let res_body = f body in
      let ( prelude_beg
          , beg'
          , macros_beg
          , init_stmts_beg
          , globals_beg
          , used_mode_switching_beg
          ) = unpack res_beg
      in
      let ( prelude_end
          , _end'
          , macros_end
          , init_stmts_end
          , globals_end
          , used_mode_switching_end
          ) = unpack res_end
      in
      let ( prelude_body
          , body'
          , macros_body
          , init_stmts_body
          , globals_body
          , used_mode_switching_body
          ) = unpack res_body
      in
      let index = new_var_name () in
      let _for = ForLoop(
        Some(VarDef(Int, index, beg')), 
        Lt(Ident(index), _end'), 
        IncPost(Ident(index)),
        prelude_body @ prelude_beg @ prelude_end
        )
      in
      pack
        (prelude_beg @ prelude_end @ [_for])
        (Null)
        (macros_beg @ macros_end @ macros_body)
        (init_stmts_beg @ init_stmts_end @ init_stmts_body)
        (globals_beg @ globals_end @ globals_body)
        (used_mode_switching_beg || used_mode_switching_end || used_mode_switching_body)
      (* (prelude_beg @ prelude_end @ [_for], Null) *)
    )
    (* failwith "Conversion error: can't convert ForTo to expr!" *)
  | ForDownTo_t(t, vname, beg, _end, body) -> 
    (match is_instantiable_type t with
    | true ->
      let res_beg = f beg in
      let res_end = f _end in
      let res_body = f body in
      let ( prelude_beg
          , beg'
          , macros_beg
          , init_stmts_beg
          , globals_beg
          , used_mode_switching_beg
          ) = unpack res_beg
      in
      let ( prelude_end
          , _end'
          , macros_end
          , init_stmts_end
          , globals_end
          , used_mode_switching_end
          ) = unpack res_end
      in
      let ( prelude_body
          , body'
          , macros_body
          , init_stmts_body
          , globals_body
          , used_mode_switching_body
          ) = unpack res_body
      in
      let t_var = new_var_name () in
      let t' = translate_my_type_to_c t_ctx c_ctx t in
      let var_decl = EmptyVarDef(t', t_var) in
      let assign = Assign(Ident(t_var), body') in
      let index = new_var_name () in
      let _for = ForLoop(
        Some(VarDef(Int, index, beg')), 
        Gt(Ident(index), _end'), 
        DecPost(Ident(index)),
        prelude_body @ [assign] @ prelude_beg @ prelude_end
        )
      in
      pack
        ([var_decl] @ prelude_beg @ prelude_end @ [_for])
        (Ident(t_var))
        (macros_beg @ macros_end @ macros_body)
        (init_stmts_beg @ init_stmts_end @ init_stmts_body)
        (globals_beg @ globals_end @ globals_body)
        (used_mode_switching_beg || used_mode_switching_end || used_mode_switching_body)
      (* ([var_decl] @ prelude_beg @ prelude_end @ [_for], Ident(t_var)) *)
    | false ->
      let res_beg = f beg in
      let res_end = f _end in
      let res_body = f body in
      let ( prelude_beg
          , beg'
          , macros_beg
          , init_stmts_beg
          , globals_beg
          , used_mode_switching_beg
          ) = unpack res_beg
      in
      let ( prelude_end
          , _end'
          , macros_end
          , init_stmts_end
          , globals_end
          , used_mode_switching_end
          ) = unpack res_end
      in
      let ( prelude_body
          , body'
          , macros_body
          , init_stmts_body
          , globals_body
          , used_mode_switching_body
          ) = unpack res_body
      in
      let index = new_var_name () in
      let _for = ForLoop(
        Some(VarDef(Int, index, beg')), 
        Gt(Ident(index), _end'), 
        DecPost(Ident(index)),
        prelude_body @ prelude_beg @ prelude_end
        )
      in
      pack
        (prelude_beg @ prelude_end @ [_for])
        (Null)
        (macros_beg @ macros_end @ macros_body)
        (init_stmts_beg @ init_stmts_end @ init_stmts_body)
        (globals_beg @ globals_end @ globals_body)
        (used_mode_switching_beg || used_mode_switching_end || used_mode_switching_body)
      (* (prelude_beg @ prelude_end @ [_for], Null) *)
    )
    (* failwith "Conversion error: can't convert ForDownTo to expr!" *)
  | ForEach_t(t, vname, a, body) -> 
    (* Need to define an array structure at this stage... *)
    (* There are three things we need for an array to work properly: *)
    (* - Type *)
    (* - Number of elements *)
    (* - Pointer to the array *)
    (match is_instantiable_type t with
    | true -> 
      let a_t = Types.base_type_of @@ Typecheck.type_of a in
      (match a_t with
      | SizedArray(a_el_t, n) -> 
        (* There must be a struct for each type of the array *)
        (* If the type and the size are known, then we can have only one field: contents *)
        (match a_el_t with
        | Poly(_) -> (* Need to go through all of the defined types, generate a switch table, *)
                     (* and simulate runtime polymorphism *)
            failwith "Conversion error: Can't yet generate runtime polymorphism! 1"
        | AnyType -> failwith "Conversion error: didn't expect any type in for each!"
        | t'' ->
          let res_a                 = f a                                                        in
          let a'                    = value_of res_a                                             in
          let prelude_a             = prelude_of res_a                                           in
          (* let (prelude_a, a')    = f a                                                        in *)
          let t_s                   = translate_my_type_to_c t_ctx c_ctx t''                     in
          let index                 = new_var_name ()                                            in
          let declare_index_var     = VarDef(Int, index, IntLit(0))                              in
          let end_boundary          = IntLit(n)                                                  in
          let cond                  = Lt(Ident(index), end_boundary)                             in
          let inc_step              = IncPost(Ident(index))                                      in
          let res_body = f body in
          let body' = value_of res_body in
          let prelude_body = prelude_of res_body in
          (* let (prelude_body, body') = f body                                                  in *)
          let res_var               = new_var_name ()                                            in
          let res_var_decl          = EmptyVarDef(translate_my_type_to_c t_ctx c_ctx t, res_var) in
          let assign                = Assign(Ident(res_var), body')                              in
          let arr_acc               = IndexAccess(MemberAccess(a', "contents"), Ident(index))    in
          let vname_decl            = VarDef(t_s, vname, arr_acc)                                in
          let _for = 
            ForLoop(
              Some declare_index_var, 
              cond, 
              inc_step, 
              [vname_decl] @ prelude_body @ [assign])
          in
          pack_concat_r res_a res_body
          |> res_with_prelude ([res_var_decl] @ prelude_a @ [_for])
          |> res_with_value (Ident(res_var))
          (* ([res_var_decl] @ prelude_a @ [_for], Ident(res_var)) *)
        )
      | UnsizedArray(a_el_t) ->
        (match a_el_t with
        | Poly(_) -> failwith "Conversion error: Can't yet generate runtime polymorphism! 3"
        | AnyType -> failwith "Conversion error; didn't expecy any type in for each!"
        | t'' ->
          (* let (prelude_a, a') = f a in *)
          let res_a = f a in
          let a' = value_of res_a in
          let prelude_a = prelude_of res_a in
          let t_s = translate_my_type_to_c t_ctx c_ctx t'' in
          let index = new_var_name () in
          let declare_index_var = VarDef(Int, index, IntLit(0)) in
          let end_boundary = 
            (* access the size field of the struct *) 
            MemberAccess(a', "size")
          in
          let cond = Lt(Ident(index), end_boundary) in
          let inc_step = IncPost(Ident(index)) in
          let res_body = f body in
          (* let (prelude_body, body') = f body in *)
          let body' = value_of res_body in
          let prelude_body = prelude_of res_body in
          let res_var = new_var_name () in
          let res_var_decl = EmptyVarDef(translate_my_type_to_c t_ctx c_ctx t, res_var) in
          let assign = Assign(Ident(res_var), body') in
          let arr_acc = IndexAccess(MemberAccess(a', "contents"), Ident(index)) in
          let vname_decl = VarDef(t_s, vname, arr_acc) in
          let _for =
            ForLoop(
              Some declare_index_var,
              cond,
              inc_step,
              [vname_decl] @ prelude_body @ [assign]
            )
          in
          pack_concat_r res_a res_body
          |> res_with_prelude ([res_var_decl] @ prelude_a @ [_for])
          |> res_with_value (Ident(res_var))
          (* ([res_var_decl] @ prelude_a @ [_for], Ident(res_var)) *)
        )
        (* failwith "TODO: convert for_each in case of unsized array" *)
        (* If the array is unsized, we need to have an additional struct member: size *)
      | _ -> failwith @@ "Conversion error: expected the element of for_each to be an array, " ^ 
                         "but got something else instead! 1"
      )
    | false ->
      let a_t = Types.base_type_of @@ Typecheck.type_of a in
      (match a_t with
      | SizedArray(a_el_t, n) -> 
        (* There must be a struct for each type of the array *)
        (* If the type and the size are known, then we can have only one field: contents *)
        (match a_el_t with
        | Poly(_) -> (* Need to go through all of the defined types, generate a switch table, *)
                     (* and simulate runtime polymorphism *)
            failwith "Conversion error: Can't yet generate runtime polymorphism! 2"
        | AnyType -> failwith "Conversion error: didn't expect any type in for each!"
        | t'' ->
          (* let (prelude_a, a')    = f a                                                        in *)
          let res_a                 = f a                                                        in
          let a'                    = value_of res_a                                             in
          let prelude_a             = prelude_of res_a                                           in
          let t_s                   = translate_my_type_to_c t_ctx c_ctx t''                     in
          let index                 = new_var_name ()                                            in
          let declare_index_var     = VarDef(Int, index, IntLit(0))                              in
          let end_boundary          = IntLit(n)                                                  in
          let cond                  = Lt(Ident(index), end_boundary)                             in
          let inc_step              = IncPost(Ident(index))                                      in
          let res_body              = f body                                                     in
          let body'                 = value_of res_body                                          in
          let prelude_body          = prelude_of res_body                                        in
          (* let (prelude_body, body') = f body                                                  in  *)
          let arr_acc               = IndexAccess(MemberAccess(a', "contents"), Ident(index))    in
          let vname_decl            = VarDef(t_s, vname, arr_acc)                                in
          let _for = 
            ForLoop(
              Some declare_index_var, 
              cond, 
              inc_step, 
              [vname_decl] @ prelude_body)
          in
          pack_concat_r res_a res_body
          |> res_with_prelude (prelude_a @ [_for])
          |> res_with_value (Null)
          (* (prelude_a @ [_for], Null) *)
        )
      | UnsizedArray(a_el_t) ->
        (match a_el_t with
        | Poly(_) -> failwith "Conversion error: Can't yet generate runtime polymorphism! 3"
        | AnyType -> failwith "Conversion error; didn't expecy any type in for each!"
        | t'' ->
          (* let (prelude_a, a') = f a in *)
          let res_a = f a in
          let a' = value_of res_a in
          let prelude_a = prelude_of res_a in
          let t_s = translate_my_type_to_c t_ctx c_ctx t'' in
          let index = new_var_name () in
          let declare_index_var = VarDef(Int, index, IntLit(0)) in
          let end_boundary = 
            (* access the size field of the struct *) 
            MemberAccess(a', "size")
          in
          let cond = Lt(Ident(index), end_boundary) in
          let inc_step = IncPost(Ident(index)) in
          let res_body = f body in
          (* let (prelude_body, body') = f body in *)
          let body' = value_of res_body in
          let prelude_body = prelude_of res_body in
          let arr_acc = IndexAccess(MemberAccess(a', "contents"), Ident(index)) in
          let vname_decl = VarDef(t_s, vname, arr_acc) in
          let _for =
            ForLoop(
              Some declare_index_var,
              cond,
              inc_step,
              [vname_decl] @ prelude_body
            )
          in
          pack_concat_r res_a res_body
          |> res_with_prelude (prelude_a @ [_for])
          |> res_with_value (Null)
          (* (prelude_a @ [_for], Null) *)
        )
        (* failwith "TODO: convert for_each in case of unsized array" *)
        (* If the array is unsized, we need to have an additional struct member: size *)
      | _ -> failwith @@ "Conversion error: expected the element of for_each to be an array, " ^ 
                         "but got something else instead! 2"
      )
    )
    (* failwith "Conversion error: can't convert ForEach to expr!" *)
  | ForEachIndex_t(t, index, vname, a, body) -> 
    let a_t = Types.base_type_of @@ Typecheck.type_of a in
    (match is_instantiable_type t with
    | true -> (**)
      (match a_t with
      | SizedArray(a_el_t, n) -> (**)
        (match a_el_t with
        | Poly(_) -> failwith "Conversion error: Can't yet generate runtime polymorphism! 5"
        | AnyType -> failwith "Conversion error; didn't expecy any type in for each!"
        | t'' -> (**)
          (* let (prelude_a, a')       = f a                                                     in *)
          let res_a                 = f a                                                        in
          let a'                    = value_of res_a                                             in
          let prelude_a             = prelude_of res_a                                           in
          let t_s                   = translate_my_type_to_c t_ctx c_ctx t''                     in
          let declare_index_var     = VarDef(Int, index, IntLit(0))                              in
          let end_boundary          = IntLit(n)                                                  in
          let cond                  = Lt(Ident(index), end_boundary)                             in
          let inc_step              = IncPost(Ident(index))                                      in
          (* let (prelude_body, body') = f body                                                  in *)
          let res_body              = f body                                                     in
          let body'                 = value_of res_body                                          in
          let prelude_body          = prelude_of res_body                                        in
          let res_var               = new_var_name ()                                            in
          let res_var_decl          = EmptyVarDef(translate_my_type_to_c t_ctx c_ctx t, res_var) in
          let assign                = Assign(Ident(res_var), body')                              in
          let arr_acc               = IndexAccess(MemberAccess(a', "contents"), Ident(index))    in
          let vname_decl            = VarDef(t_s, vname, arr_acc)                                in
          let _for = 
            ForLoop(
              Some declare_index_var, 
              cond, 
              inc_step, 
              [vname_decl] @ prelude_body @ [assign])
          in
          pack_concat_r res_a res_body
          |> res_with_prelude ([res_var_decl] @ prelude_a @ [_for])
          |> res_with_value (Ident(res_var))
          (* ([res_var_decl] @ prelude_a @ [_for], Ident(res_var)) *)
        )
      | UnsizedArray(a_el_t) -> (**)
        (match a_el_t with
        | Poly(_) -> failwith "Conversion error: Can't yet generate runtime polymorphism! 6"
        | AnyType -> failwith "Conversion error; didn't expecy any type in for each!"
        | t'' -> (**)
          (* let (prelude_a, a') = f a in *)
          let res_a = f a in
          let a' = value_of res_a in
          let prelude_a = prelude_of res_a in
          let t_s = translate_my_type_to_c t_ctx c_ctx t'' in
          let declare_index_var = VarDef(Int, index, IntLit(0)) in
          let end_boundary = 
            (* access the size field of the struct *) 
            MemberAccess(a', "size")
          in
          let cond = Lt(Ident(index), end_boundary) in
          let inc_step = IncPost(Ident(index)) in
          (* let (prelude_body, body') = f body in *)
          let res_body              = f body                                                     in
          let body'                 = value_of res_body                                          in
          let prelude_body          = prelude_of res_body                                        in
          let res_var = new_var_name () in
          let res_var_decl = EmptyVarDef(translate_my_type_to_c t_ctx c_ctx t, res_var) in
          let assign = Assign(Ident(res_var), body') in
          let arr_acc = IndexAccess(MemberAccess(a', "contents"), Ident(index)) in
          let vname_decl = VarDef(t_s, vname, arr_acc) in
          let _for =
            ForLoop(
              Some declare_index_var,
              cond,
              inc_step,
              [vname_decl] @ prelude_body @ [assign]
            )
          in
          pack_concat_r res_a res_body
          |> res_with_prelude ([res_var_decl] @ prelude_a @ [_for])
          |> res_with_value (Ident(res_var))
          (* ([res_var_decl] @ prelude_a @ [_for], Ident(res_var)) *)
        )
      | _ -> failwith @@ "Conversion error: expected the element of for_each to be an array, " ^ 
                         "but got something else instead! 4"
      )
    | false -> (**)
      (match a_t with
      | SizedArray(a_el_t, n) -> (**)
        (match a_el_t with
        | Poly(_) -> failwith "Conversion error: Can't yet generate runtime polymorphism! 7"
        | AnyType -> failwith "Conversion error; didn't expecy any type in for each!"
        | t'' -> (**)
          (* let (prelude_a, a')       = f a                                                     in *)
          let res_a                 = f a                                                        in
          let a'                    = value_of res_a                                             in
          let prelude_a             = prelude_of res_a                                           in
          let t_s                   = translate_my_type_to_c t_ctx c_ctx t''                     in
          let declare_index_var     = VarDef(Int, index, IntLit(0))                              in
          let end_boundary          = IntLit(n)                                                  in
          let cond                  = Lt(Ident(index), end_boundary)                             in
          let inc_step              = IncPost(Ident(index))                                      in
          (* let (prelude_body, body') = f body                                                  in *)
          let res_body              = f body                                                     in
          let body'                 = value_of res_body                                          in
          let prelude_body          = prelude_of res_body                                        in
          let arr_acc               = IndexAccess(MemberAccess(a', "contents"), Ident(index))    in
          let vname_decl            = VarDef(t_s, vname, arr_acc)                                in
          let _for = 
            ForLoop(
              Some declare_index_var, 
              cond, 
              inc_step, 
              [vname_decl] @ prelude_body)
          in
          pack_concat_r res_a res_body
          |> res_with_prelude (prelude_a @ [_for])
          |> res_with_value (Null)
          (* (prelude_a @ [_for], Null) *)
        )
      | UnsizedArray(a_el_t) -> (**)
        (match a_el_t with
        | Poly(_) -> failwith "Conversion error: Can't yet generate runtime polymorphism! 8"
        | AnyType -> failwith "Conversion error; didn't expecy any type in for each!"
        | t'' -> (**)
          (* let (prelude_a, a') = f a in *)
          let res_a = f a in
          let a' = value_of res_a in
          let prelude_a = prelude_of res_a in
          let t_s = translate_my_type_to_c t_ctx c_ctx t'' in
          let declare_index_var = VarDef(Int, index, IntLit(0)) in
          let end_boundary = 
            (* access the size field of the struct *) 
            MemberAccess(a', "size")
          in
          let cond = Lt(Ident(index), end_boundary) in
          let inc_step = IncPost(Ident(index)) in
          (* let (prelude_body, body') = f body in *)
          let res_body = f body in
          let body' = value_of res_body in
          let prelude_body = prelude_of res_body in
          let arr_acc = IndexAccess(MemberAccess(a', "contents"), Ident(index)) in
          let vname_decl = VarDef(t_s, vname, arr_acc) in
          let _for =
            ForLoop(
              Some declare_index_var,
              cond,
              inc_step,
              [vname_decl] @ prelude_body
            )
          in
          pack_concat_r res_a res_body
          |> res_with_prelude (prelude_a @ [_for])
          |> res_with_value (Null)
          (* (prelude_a @ [_for], Null) *)
        )
      | _ -> failwith @@ "Conversion error: expected the element of for_each to be an array, " ^ 
                         "but got something else instead! 5"
      )
    )
    (* failwith "Conversion error: can't convert ForEachIndex to expr!" *)
  | Block_t(t, b) -> 
    print_endline @@ Printf.sprintf 
      "DEBUG: block length before conversion: [%s]" 
      (String.concat "; " @@ List.map (Typecheck.string_of_stmt_t) b);
    let res = 
      (match Util.last_el_of b with
      | Some(Expr_t(e')) -> 
        let t = Types.base_type_of @@ Typecheck.type_of e' in
        (match is_instantiable_type t with
        | true ->
          print_endline "DEBUG: converting a block with instantiable type";
          let t_var = new_var_name () in
          let var_decl = EmptyVarDef(translate_my_type_to_c t_ctx c_ctx t, t_var) in
          let res_sl = List.fold_left pack_concat_r empty_res @@ List.map (c_stmt_of_stmt t_var cur_task cur_promise t_ctx c_ctx) b in
          let sl = prelude_of res_sl in
          let e'' = 
            match Util.last_el_of sl with
            | Some(Expr(e'')) -> e''
            | _ -> failwith "c_expr_of_expr: Block branch: Impossible case"
          in
          let assign = Assign(Ident(t_var), e'') in
          let sl' = (Util.without_last_el sl) @ [assign] in
          let b' = Block(sl') in
          let prelude = [var_decl; b'] in
          res_sl
          |> res_with_prelude prelude
          |> res_with_value (Ident(t_var))
          (* prelude, Ident(t_var) *)
        | false ->
          print_endline "DEBUG: converting a block with non-instantiable type 1";
          let res_b = List.fold_left (pack_concat_r) empty_res @@ List.map (c_stmt_of_stmt "" cur_task cur_promise t_ctx c_ctx) b in
          let sl = prelude_of res_b in
          let v = Block(sl) in
          let new_prelude = [v] in
          res_b
          |> res_with_prelude new_prelude
          |> res_with_value Null
        )
      | _ -> 
        (*
        let l' =
          List.fold_left 
            (fun (res, l) x ->
              failwith "TODO: expr of Block, need to define what is the return type of stmt"
            )
            (failwith "TODO: put the initial value here")
            (failwith "TODO: put the list here")
        in
        *)
        print_endline "DEBUG: converting a block with non-instantiable type 2";
        (* List.fold_left (pack_concat_r) empty_res b *)
        (* List.fold_left (pack_concat_r) empty_res @@ List.map (c_stmt_of_stmt "" t_ctx c_ctx) b *)
        let res_b = List.fold_left (pack_concat_r) empty_res @@ List.map (c_stmt_of_stmt "" cur_task cur_promise t_ctx c_ctx) b in
        let sl = prelude_of res_b in
        let v = Block(sl) in
        let new_prelude = [v] in
        res_b
        |> res_with_prelude new_prelude
        |> res_with_value Null
        (* [Block(List.fold_left (@) [] @@ List.map (fun x -> c_stmt_of_stmt "" t_ctx c_ctx x) b)], Null *)
      )
    in
    print_endline @@ Printf.sprintf "DEBUG: the block after conversion:\n%swith value %s" (String.concat "" @@ List.map string_of_stmt @@ prelude_of res) (string_of_expr @@ value_of res);
    assert (prelude_of res <> []);
    res
    (* c_expr_of_stmt_list t_ctx c_ctx b *)
    (* failwith "Conversion error: can't convert block to expr!" *)
  (* failwith "TODO: c_expr_of_expr" *)

(*
and c_expr_of_stmt t_ctx c_ctx s =
  failwith "TODO: c_expr_of_stmt"

and c_stmt_of_expr t_ctx c_ctx e =
  failwith "TODO: c_stmt_of_expr"
*)

and c_stmt_of_stmt vname (cur_task : string option) cur_promise t_ctx c_ctx s =
  let open Typecheck in
  print_endline @@ Printf.sprintf "DEBUG: converting statement: %s" (Typecheck.string_of_stmt_t s);
  let res =
    match s with
    | Break_t -> res_with_prelude [Break] empty_res
    | Continue_t -> res_with_prelude [Continue] empty_res
    | EmptyReturn_t -> res_with_prelude [EmptyReturn] empty_res
    | BreakWith_t(_, v) -> 
      let res_v = c_expr_of_expr cur_task cur_promise t_ctx c_ctx v in
      let v' = value_of res_v in
      let prelude_v = prelude_of res_v in
      res_v
      |> res_with_prelude (prelude_v @ [Assign(Ident(vname), v'); Break])
      |> res_with_value (Null)
      (* prelude_v @ [Assign(Ident(vname), v'); Break] *)
    | ContinueWith_t(_, v) -> 
      let res_v = c_expr_of_expr cur_task cur_promise t_ctx c_ctx v in
      let v' = value_of res_v in
      let prelude_v = prelude_of res_v in
      res_v
      |> res_with_prelude (prelude_v @ [Assign(Ident(vname), v'); Continue])
      |> res_with_value (Null)
      (* prelude_v @ [Assign(Ident(vname), v'); Continue] *)
    | Return_t(v) ->
      let res_v = c_expr_of_expr cur_task cur_promise t_ctx c_ctx v in
      let v' = value_of res_v in
      let prelude_v = prelude_of res_v in
      res_v
      |> res_with_prelude (prelude_v @ [Return v'])
      |> res_with_value (Null)
      (* prelude_v @ [Return v'] *)
    | Expr_t(Ident_t(PromiseDef _, _)) -> empty_res
    | Expr_t(Promise_t _) -> empty_res
    | Expr_t(e) -> 
      let res_e = c_expr_of_expr cur_task cur_promise t_ctx c_ctx e in
      let stmts = prelude_of res_e in
      let v = value_of res_e in
      let res_e' =
        let l' =
          (match v with
          | Null -> []
          | Ident(_) -> []
          | _ -> [Expr(v)])
        in
        let stmts' = stmts @ l' in
        res_e
        |> res_with_prelude stmts'
        |> res_with_value Null
      in
      res_e'
    | Global_t(_) -> empty_res (* Globals are important for typechecking purposes only *)
    | Assign_t(a, b) ->
      (*
      let (prelude_b, b') = c_expr_of_expr t_ctx c_ctx b in
      let (prelude_a, a') = c_expr_of_expr t_ctx c_ctx a in
      *)
      let res_b = c_expr_of_expr cur_task cur_promise t_ctx c_ctx b in
      let res_a = c_expr_of_expr cur_task cur_promise t_ctx c_ctx a in
      let a' = value_of res_a in
      let b' = value_of res_b in
      let prelude_a = prelude_of res_a in
      let prelude_b = prelude_of res_b in
      let res =
        pack_concat_r res_a res_b
        |> res_with_prelude (prelude_b @ prelude_a @ [Assign(a', b')])
        |> res_with_value Null
      in
      assert (prelude_of res = (prelude_b @ prelude_a @ [Assign(a', b')]));
      res
      (* prelude_b @ prelude_a @ [Assign(a', b')] *)
    | Let_t(name, e)
    | LetMut_t(name, e) -> (* both let and let mut produce the same code, the difference is in *)
                           (* typechecking *)
      let res_e = c_expr_of_expr cur_task cur_promise t_ctx c_ctx e in
      let e' = value_of res_e in
      let prelude_e = prelude_of res_e in
      (* let (prelude_e, e') = c_expr_of_expr t_ctx c_ctx e in *)
      let t = Types.base_type_of @@ Typecheck.type_of e in
      let t' = translate_my_type_to_c t_ctx c_ctx t in
      let decl = VarDef(t', name, e') in
      let res' =
        res_e
        |> res_with_prelude (prelude_e @ [decl])
        |> res_with_value Null
      in
      assert (List.length prelude_e < List.length (prelude_of res'));
      res'
    | ModeSwitch_t(mode) ->
      (* The most annoying part of the code generation starts here *)
      (* Need to know if it is called from the possible mode switch context or from outside *)
      let cur_name = cur_name_of c_ctx in
      let tasks_and_modetypes = Typecheck.tasks_and_modetypes_of t_ctx in
      let modes_and_modtypes = Typecheck.modes_and_modetypes_of t_ctx in
      let custom_types = Typecheck.custom_types_of t_ctx in
      let mode_transitions = Typecheck.mode_transitions_of t_ctx in
      let waiting_room = Printf.sprintf "task_%s_waiting_room" cur_name in
      let loop_beginning = Printf.sprintf "task_%s_loop_beginning" cur_name in
      let transitions_of modetype =
        let transitions = 
          match List.assoc_opt modetype mode_transitions with
          | Some(transitions) -> transitions
          | None -> failwith @@ Printf.sprintf 
            "Conversion error: something went wrong when retrieving mode transitions of %s"
            modetype
        in
        transitions
      in
      let generate_task_switches prev_modetype_mode cur_modetype_mode take_mutex give_mutex modetype =
        let generate_task_switches_for_transition (to_delete, to_install) =
          let self_delete = List.exists (fun x -> x = cur_name) to_delete in
          (* current task needs to be deleted last*)
          let to_delete' = List.filter (fun x -> x <> cur_name) to_delete in 
          let task_handle task_name = Printf.sprintf "task_%s_handle" task_name in
          let task_stack_size task_name = Printf.sprintf "TASK_%s_STACK_SIZE" task_name in
          let delete_f_call task_name = Expr(FuncCall("vTaskDelete", [Ident(task_handle task_name)])) in
          let delete_if_guard task_name = 
            IfStmt(NEq(Ident(task_handle task_name), Null), [delete_f_call task_name], []) 
          in
          let create_task task_name = 
            Expr(
              FuncCall(
                "xTaskCreate", 
                [ CastTo(CustomType("TaskFunction_t"), AddrOf(Ident(Printf.sprintf "task_%s" task_name)))
                ; StringLit(task_name)
                ; Ident(task_stack_size task_name)
                ; Null (* parameters to pass *)
                ; Ident("tskIDLE_PRIORITY")
                ; AddrOf(Ident(task_handle task_name))
                ]
              )
            ) 
          in
          let delete_commands = List.map (delete_if_guard) to_delete' in
          let create_commands = List.map (create_task) to_install in
          let last_command = 
            match self_delete with
            | true -> [ give_mutex; Expr(FuncCall("vTaskDelete", [Null]))]
            | false -> [ give_mutex ]
          in
          delete_commands @ create_commands @ last_command
          (* failwith "TODO: generate_task_switches_for_transition" *)
        in
        let _if_current_mode mode' (to_delete, to_install) =
          let _if = 
            IfStmt(
              Eq(Ident(cur_modetype_mode), Ident(mode')), 
              [
                (* Need two variables *)
                (* - prev_%modetype%_mode *)
                (* - cur_%modetype%_mode *)
                Assign (Ident(prev_modetype_mode), Ident(cur_modetype_mode));
                Assign (Ident(cur_modetype_mode), Ident(mode))
              ] @ (generate_task_switches_for_transition (to_delete, to_install)),
              []
            ) 
          in
          _if
        in
        let transitions = transitions_of modetype in
        let transitions' = 
          List.filter 
            (fun ((_from, _to), (to_delete, to_install)) -> 
              _to = mode && not (_to = mode && _from = mode)) 
            transitions
        in
        let _ifs = 
          List.map 
            (fun ((_from, _), (to_delete, to_install)) -> 
              _if_current_mode _from (to_delete, to_install)
            )
            transitions'
        in
        (* List.map (fun (x) -> (_if_current_mode)) *)
        (* failwith "TODO: generate_task_switches" *)
        _ifs
      in
      (match List.assoc_opt cur_name tasks_and_modetypes with
      | Some(modetype) -> 
        (* called from the modetype switch *)
        (* the mutex is this: mutex_modetype_switch *)
        let prev_modetype_mode = Printf.sprintf "PREV_%s_MODE" modetype in
        let cur_modetype_mode = Printf.sprintf "CUR_%s_MODE" modetype in
        let mutex = Printf.sprintf "mutex_%s_switch" modetype in
        let mutex_globals = [ GlobalVarDecl(CustomType("SemaphoreHandle_t"), mutex) ] in
        let mutex_init_stmts = [ Expr(FuncCall("vSemaphoreCreateBinary", [ Ident(mutex) ])) ] in
        let take_mutex = 
          FuncCall(
            "xSemaphoreTake", 
            [ Ident(mutex)
            ; CastTo(CustomType("TickType_t"), IntLit(10))
            ]
          )
        in
        let give_mutex =
          FuncCall(
            "xSemaphoreGive",
            [ Ident(mutex) ]
          )
        in
        let take_mutex_s = Expr(take_mutex) in
        let give_mutex_s = Expr(give_mutex) in
        let modes = 
          match List.assoc_opt modetype custom_types with
          | Some(ModetypeDef(modetype_name, modes)) -> modes
          | _ -> failwith @@ Printf.sprintf
            "Conversion error: expected %s to be modetypedef, but something went wrong" modetype
        in
        (* To properly do the switch, we must *)
        (* Block all the putting of effects on queues *)
        (* When all of the queues are empty, initiate the switch *)
        (* When all the switching is done, and the task didn't delete, go back to beginning of the task loop *)
        let context_switching_code =
          (* all the code gets generated here ... *)
          (* if we failed to take a mutex, it means that some other contex switch is occuring *)
          (* either the current task gets deleted, or it doesn't *)
          (* if it does, it can go to a waiting room and do nothing *)
          (* if it doesn't, then it has to go into beginning of the task *)
          (* the problem with the latter case is that it assumes that the stack will be *)
          (* managed properly. In C++ it seems that the destructors are called appropriately *)
          (* I can only check that in C later, nobody seems to address this issue. Is it a common knowledge? *)
          (* Need two labels: *)
          (* - task_%taskname%_loop_beginning *)
          (* - task_%taskname%_waiting_room *)
          (* Need a list of transitions in which the current task gets deleted... *)
          let transitions = transitions_of modetype in
          let cur_task_deleted_when = 
            List.map (fun ((_from, _to), (to_delete, to_install)) ->
              let c =
                List.exists (fun x -> x = cur_name) to_delete  = true &&
                List.exists (fun x -> x = cur_name) to_install = false
              in
              (_from, _to, c)
            )
            transitions
          in
          let _if_goto_guards =
            List.map (fun (_from, _to, c) ->
              IfStmt(
                And(
                  Eq(
                    Ident(prev_modetype_mode),
                    Ident(_from)
                  ),
                  Eq(
                    Ident(cur_modetype_mode),
                    Ident(_to)
                  )
                ),
                [
                  match c with
                  | true -> (* task gets deleted, waiting room *)
                    Goto(waiting_room)
                  | false ->
                    Goto(loop_beginning)
                ],
                []
              )
            )
            cur_task_deleted_when
          in
          let _try_mode_switching =
            IfStmt(
              Eq(
                take_mutex,
                Ident("pdPASS")
              ),
              (* performs task switching *)
              (generate_task_switches prev_modetype_mode cur_modetype_mode take_mutex_s give_mutex_s modetype), 
              (* 'goto's to a correct position *)
              (_if_goto_guards)
            )
          in
          _try_mode_switching
          (* failwith "TODO: context_switching_code" *)
        in
        (* TODO: what goes here? *)
        empty_res
        |> res_with_prelude ([context_switching_code])
        |> res_with_used_mode_switch
        |> res_with_globals mutex_globals
        |> res_with_init_stmts mutex_init_stmts
        (* failwith "TODO: convert mode switching within context" *)
      | None ->
        (* called from the outside of modetype switch context *)

        (* To properly do the switch, we must *)
        (* Block all the putting of effects on queues *)
        (* When all of the queues are empty, initiate the switch *)
        (* When all the switching is done, release the block *)
        (* Continue *)
        let modetype =
          match List.assoc_opt mode custom_types with
          | Some(Modetype(modetype)) -> modetype
          | _ -> failwith @@ Printf.sprintf 
            "Conversion error: tried to do mode switching from outside of modetype context in task %s, but something went wrong"
            mode
        in
        let prev_modetype_mode = Printf.sprintf "PREV_%s_MODE" modetype in
        let cur_modetype_mode = Printf.sprintf "CUR_%s_MODE" modetype in
        let mutex = Printf.sprintf "mutex_%s_switch" modetype in
        let mutex_globals = [ GlobalVarDecl(CustomType("SemaphoreHandle_t"), mutex) ] in
        let mutex_init_stmts = [ Expr(FuncCall("vSemaphoreCreateBinary", [ Ident(mutex) ])) ] in
        let take_mutex = 
          FuncCall(
            "xSemaphoreTake", 
            [ Ident(mutex)
            ; CastTo(CustomType("TickType_t"), IntLit(10))
            ]
          )
        in
        let give_mutex =
          FuncCall(
            "xSemaphoreGive",
            [ Ident(mutex) ]
          )
        in
        let take_mutex_s = Expr(take_mutex) in
        let give_mutex_s = Expr(give_mutex) in
        let _try_mode_switching =
          WhileLoop(
            BoolLit(true),
            [
              IfStmt(
                Eq(
                  take_mutex,
                  Ident("pdPASS")
                ),
                (* performs task switching *)
                (generate_task_switches prev_modetype_mode cur_modetype_mode take_mutex_s give_mutex_s modetype) @ 
                  [Break], 
                [Continue]
              )
            ]
          )
        in
        empty_res
        |> res_with_prelude ([_try_mode_switching])
        |> res_with_globals mutex_globals
        |> res_with_init_stmts mutex_init_stmts
        (* failwith "TODO: convert mode switching outside context" *)
      )
    | Dispatch_t e ->
      let e' = c_expr_of_expr cur_task cur_promise t_ctx c_ctx e in
      let res_val = value_of e' in
      let res_var, var_decl =
        match res_val with
        | Ident(_) -> res_val, []
        | _ ->
          let t = type_of e in
          let res_var_name = new_var_name () in
          let var_decl = VarDef(translate_my_type_to_c t_ctx c_ctx t, res_var_name, res_val) in
          Ident(res_var_name), [var_decl]
      in
      let p = prelude_of e' in
      let e' =
        e'
        |> res_with_prelude (p @ var_decl)
        |> res_with_value res_var
      in
      let cur_task_name =
        match cur_task with
        | Some(name) -> name
        | None -> failwith "Conversion error: using Dispatch outside of task context!"
      in
      let (cur_prom_name, await_id, origin_var) =
        match cur_promise with
        | Some(name, await_id, var) -> name, await_id, var
        | None -> failwith "Conversion error: using Dispatch outside of promise context!"
      in
      let prom_name = cur_prom_name in
      let taskname = cur_task_name in
      let promise_wait_macro = Printf.sprintf "PROMISE_%s_RECEIVE_WAIT" prom_name in
      let queue_send_wait_macro = Printf.sprintf "PROMISE_%s_SEND_WAIT" prom_name in
      let promises_resolved = 
        t_ctx
        |> Typecheck.parallels_resolved_of
        |> snd
      in
      let resolve_eff_destinations =
        (match await_id with
        | None -> [
            Expr(FuncCall("vTaskDelay", [CastTo(CustomType("TickType_t"), IntLit(10))]));
            Goto("loop_beginning")
          ]
        | Some(v) -> [
            WhileLoop(IntLit(1), [
              IfStmt(Eq(FuncCall("xQueueSend", [ Ident(Printf.sprintf "queue_%s_%s_%d" prom_name taskname v)
                                               ; AddrOf(res_var)
                                               ; CastTo(CustomType("TickType_t"), IntLit(10))
                                               ]), 
                        Ident("pdPASS")),
              [
                Break
              ],
              [
              ])
            ]);
            Expr(FuncCall("vTaskDelay", [CastTo(CustomType("TickType_t"), IntLit(10))]));
            Goto("loop_beginning")
          ]
        )
      in
      let p = prelude_of e' in
      e'
      |> res_with_prelude (p @ resolve_eff_destinations)
      (* failwith "TODO: c_stmt_of_stmt: Dispatch_t" *)
  in
  res
  (* failwith "TODO: c_stmt_of_stmt" *)

let typ_counter = ref 0
let new_type_cnt () =
  let c = !typ_counter in
  incr typ_counter;
  c

let convert_struct_defs_to_c t_ctx ctx =
  let custom_types = Typecheck.custom_types_of t_ctx in
  let structs_of' ct =
    ct
    |> List.filter 
        (fun (x, t) -> 
          (match t with
          | Types.Struct(_) -> true
          | _ -> false))
    |> List.map
        (fun (x, t) ->
          (match t with
          | Types.Struct(_, fields) -> (x, fields)
          | _ -> failwith "convert_struct_defs_to_c: structs_of: Impossible case"))
  in
  let convert ctx (name, fields) =
    let fields' = List.map (fun (fname, f_type) -> (fname, translate_my_type_to_c t_ctx ctx f_type)) fields in
    (* need to add a new type to translation table as well *)
    let structs = structs_of ctx in
    let ttt = type_translation_table_of ctx in
    let new_ttt = (Types.Struct(name, fields), new_type_cnt ()) :: ttt in
    let new_ctx = ctx_of_type_translation_table new_ttt ctx in
    let new_name = 
      match List.assoc_opt name custom_types with
      | Some(t') -> (match translate_my_type_to_c t_ctx new_ctx t' with
        | CustomType(new_name) -> new_name
        | _ -> failwith "convert_struct_defs_to_c: convert: Something went wrong...")
      | None -> failwith @@ Printf.sprintf "Conversion error: custom type %s is not declared!" name
    in
    let new_structs = StructDecl(new_name, fields') :: structs in
    let new_ctx = 
      new_ctx
      |> ctx_of_type_translation_table new_ttt
      |> ctx_of_added_structs new_structs 
    in
    (* need to put converted struct into the context instead *)
    (* return a context instead *)
    new_ctx
    (* failwith "TODO: convert_struct_defs_to_c: convert" *)
  in
  let structs_data = structs_of' custom_types in
  let new_ctx = 
    List.fold_left 
      (fun ctx (name, fields) -> convert ctx (name, fields)) 
      ctx 
      structs_data 
  in
  new_ctx

let convert_effects_defs_to_c typecheck_ctx converted_ctx =
  let custom_types = Typecheck.custom_types_of typecheck_ctx in
  let effects_of' ct =
    ct
    |> List.filter 
        (fun (x, t) -> 
          (match t with
          | Types.EffectDef(_, _) -> true
          | _ -> false))
    |> List.map
        (fun (x, t) ->
          (match t with
          | Types.EffectDef(name, types) -> (Types.EffectDef(name, types))
          | _ -> failwith "convert_effects_defs_to_c: structs_of: Impossible case"))
  in
  let convert t_ctx c_ctx eff_type =
    let (name, types) =
      match eff_type with
      | Types.EffectDef(name, types) -> (name, types)
      | _ -> failwith "convert_effects_defs_to_c: convert: Impossible case"
    in
    let fields = 
      List.mapi 
        (fun i x -> (Printf.sprintf "pos_%d" (i + 1), translate_my_type_to_c t_ctx c_ctx x)) 
        types
    in
    let fields' = ("pos_0", CustomType("TASKS_ENUM_TYPE")) :: fields in
    let new_name =
      match (translate_my_type_to_c t_ctx c_ctx (Types.EffectDef(name, types))) with
      | CustomType(new_name) -> new_name
      | _ -> failwith "convert_effects_def_to_c: convert: Something went wrong..."
    in
    StructDecl(new_name, fields')
    (* failwith "TODO: convert_effects_defs_to_c: convert" *)
  in
  let custom_types = Typecheck.custom_types_of typecheck_ctx in
  let effects = effects_of' custom_types in
  let ttt = type_translation_table_of converted_ctx in
  let new_ttt = (List.map (fun x -> (x, new_type_cnt ())) effects) @ ttt in
  let new_ctx = ctx_of_type_translation_table new_ttt converted_ctx in
  let converted_effects = List.map (convert typecheck_ctx new_ctx) effects in
  let new_ctx = ctx_of_added_structs converted_effects new_ctx in
  new_ctx
  (* failwith "TODO: convert_effects_defs_to_c" *)

let convert_modetype_defs_to_c t_ctx c_ctx =
  let modes_and_modetypes = Typecheck.modes_and_modetypes_of t_ctx in
  let construct m_and_m =
    let module M = Map.Make(String) in
    let rec loop ans l =
      match l with
      | [] -> ans
      | (a, b) :: rest ->
        let new_ans = 
          match M.find_opt b ans with
          | Some(l) -> (M.add b (a :: l) ans)
          | None -> (M.add b [a] ans)
        in
        loop new_ans rest
    in
    List.of_seq @@ M.to_seq @@ loop (M.empty) m_and_m
    (* failwith "TODO: convert_modetype_defs_to_c: construct" *)
  in
  let convert t_ctx c_ctx modetype_name =
    let custom_types = Typecheck.custom_types_of t_ctx in
    let (modes, t') = 
      print_endline @@ Printf.sprintf "DEBUG: modetype_name: %s" modetype_name;
      match List.assoc_opt modetype_name custom_types with
      | Some((Types.ModetypeDef(modetype_name, modes)) as t') -> (modes, t')
      | Some(_) -> failwith "convert_modetype_defs_to_c: convert: Impossible case 1"
      | None    -> failwith "convert_modetype_defs_to_c: convert: Impossible case 2"
    in
    let ttt = type_translation_table_of c_ctx in
    let enum_name = 
      match translate_my_type_to_c t_ctx c_ctx t' with
      | CustomType(enum_name) -> enum_name
      | _ -> failwith "convert_modetype_defs_to_c: convert: Impossible case 3"
    in
    Enum(enum_name, modes)
    (* failwith "TODO: convert_modetype_defs_to_c: convert" *)
  in
  let constructed_mode_defs = construct modes_and_modetypes in
  let ttt = type_translation_table_of c_ctx in
  let custom_types = Typecheck.custom_types_of t_ctx in
  let new_ttt =
    let m_types = 
      constructed_mode_defs
      |> List.map (fun (x, _) -> 
          (match List.assoc_opt x custom_types with
          | Some((Types.ModetypeDef(_)) as t') -> t'
          | Some(_) -> failwith @@ Printf.sprintf
            "Conversion error: expected a modetype when searching for %s, got something else instead!"
            x
          | None -> failwith @@ Printf.sprintf
            "Conversion error: expected a modetype when searching for %s, but found nothing!"
            x))
      |> List.map (fun x -> (x, new_type_cnt ()))
    in
    m_types @ ttt 
  in
  let new_ctx = ctx_of_type_translation_table new_ttt c_ctx in
  let converted_modetype_defs =
    modes_and_modetypes
    |> List.map snd
    |> Util.unique_strings
    |> List.map (convert t_ctx new_ctx)
  in
  let new_ctx = ctx_of_modetypes converted_modetype_defs new_ctx in
  let new_ctx = ctx_of_added_enums converted_modetype_defs new_ctx in
  new_ctx
  (* failwith "TODO: convert_modetype_defs_to_c" *)

let convert_sumtype_defs_to_c t_ctx c_ctx =
  (* TODO: need to add sumtype definitions to the list *)
  let custom_types = Typecheck.custom_types_of t_ctx in
  let sumtypes_of' ct =
    ct
    |> List.filter (fun (_, x) ->
        match x with
        | Types.SumtypeDef(_) -> true
        | _ -> false
      )
    |> List.map (fun (name, x) ->
        match x with
        | Types.SumtypeDef(variants) -> (name, variants)
        | _ -> failwith "convert_sumtype_defs_to_c: sumtypes_of': Impossible case")
  in
  let convert t_ctx c_ctx (name, variants) =
    let t' = Types.SumtypeDef(variants) in
    let new_name = 
      match translate_my_type_to_c t_ctx c_ctx t' with
      | CustomType(new_name) -> new_name
      | _ -> failwith "Conversion error: convert_sumtype_defs_to_c: convert: Something went wrong"
    in
    let rec construct_loop (cur_name, variants) =
      (* Something went wrong... *)
      let transform l =
        List.map 
          (fun (vname, fields) -> 
            let var_name = Printf.sprintf "variant_%s" (String.lowercase_ascii vname) in
            print_endline @@ Printf.sprintf "DEBUG: vname: %s" var_name;
            (* (var_name, translate_my_type_to_c t_ctx c_ctx (Types.Sumtype(cur_name, vname)))) *)
            (var_name, CustomType(vname)))
          l
      in
      (cur_name, [("var_type", Int)], transform variants)
    in
    StructWithUnions(construct_loop (name, variants))
  in
  let sumtypes = sumtypes_of' custom_types in
  let rec assign_structs_to_variants l =
    let module S = Set.Make(String) in
    let custom_types = Typecheck.custom_types_of t_ctx in
    (*
    let rec loop s (ttt, structs) l =
      match l with
      | [] -> (ttt, structs)
      | (s_name, variants) :: rest ->
        let rec loop2 (ttt2, structs2) l2 =
          match l2 with
          | [] -> (ttt2, structs2)
          | (v_name, fields) :: rest2 ->
            (*
            let rec loop3 (l3 : Types.ground_type list) : ((Types.ground_type * 'a) list * ('b list)) =
              match l3 with
              | [] -> ([], [])
              | (Types.Sumtype(s_name, v_name)) :: rest3 ->
                let t' = Types.Sumtype(s_name, v_name) in
                let (counters, structs) = loop3 rest3 in
                let (new_counters : (Types.ground_type * 'a) list) = 
                  (t', new_type_cnt ()) :: counters 
                in
                let st_fields = 
                  List.mapi 
                    (fun i t -> 
                      (Printf.sprintf "pos_%d" i, translate_my_type_to_c t_ctx c_ctx t)) 
                    fields 
                in
                let new_structs = (StructDecl(v_name, st_fields)) :: structs in
                (new_counters, new_structs)
              | _ :: rest3 -> loop3 rest3
            in
            let ((ll : (Types.ground_type * 'a) list), structs) = loop3 fields in
            let l' = 
              (match List.assoc_opt (Types.Sumtype(s_name, v_name)) ll with
              | Some(_) -> ll
              | None -> (Types.Sumtype(s_name, v_name), new_type_cnt ()) :: ll)
            in
            *)
            let rec find_nested_variants l =
              match l with
              | [] -> ([], [])
              | (Types.Sumtype(s_name, v_name)) :: rest3 ->
                let (a', b') = find_nested_variants rest3 in
                let t' = Types.Sumtype(s_name, v_name) in
                (match .)
              | _ :: rest3 -> find_nested_variants rest3
            in
            let (a', b') = loop2 rest2 in
            (l' @ a', structs @ b')
        in
        let (a', b') = loop2 variants in
        let (c', d') = loop rest in
        (a' @ c', b' @ d')
    in
    *)
    let rec loop l =
      match l with
      | [] -> ([], [])
      | (s_name, variants) :: rest ->
        let rec search_variants l2 =
          match l2 with
          | [] -> ([], [])
          | (v_name, fields) :: rest2 ->
            let t' = (Types.Sumtype(s_name, v_name)) in
            let (counters, raw_structs) = search_variants rest2 in
            ((t', new_type_cnt ()) :: counters, (v_name, fields) :: raw_structs)
        in
        let (ttt1, structs1) = search_variants variants in
        let (ttt2, structs2) = loop rest in
        ((Types.SumtypeDef(variants), new_type_cnt ()) :: ttt1 @ ttt2, structs1 @ structs2)
    in
    let (vars, raw_structs) = loop l in
    (* let assocs = List.map (fun (name, _) -> (List.assoc name custom_types, new_type_cnt ())) l in *)
    let ttt = type_translation_table_of c_ctx in
    let new_ttt = vars @ ttt in
    let new_ctx = ctx_of_type_translation_table new_ttt c_ctx in
    let structs = 
      List.map 
        (fun (name, fields) -> 
          StructDecl(
            name, 
            List.mapi 
              (fun i b -> (Printf.sprintf "pos_%d" i, translate_my_type_to_c t_ctx c_ctx b)) 
              fields
          )
        ) 
        raw_structs 
    in
    let new_ctx = ctx_of_added_structs structs new_ctx in
    new_ctx
    (* failwith "TODO: assign_structs_to_variants" *)
  in
  let new_ctx = assign_structs_to_variants sumtypes in
  (* let new_ctx = ctx_of_added_type_translation_table new_ttt c_ctx in *)
  let converted_effects = List.map (convert t_ctx new_ctx) sumtypes in
  let new_ctx = 
    new_ctx
    |> ctx_of_added_structs converted_effects 
  in
  new_ctx
  (* failwith "TODO: convert_sumtype_defs_to_c" *)

let convert_globals_to_c t_ctx c_ctx =
  let glb_vars = (Typecheck.glb_vars_of t_ctx) @ (Typecheck.globals_of t_ctx) in
  let converted_globals = List.map (fun (name, t) -> 
    GlobalVarDecl(translate_my_type_to_c t_ctx c_ctx t, name)) glb_vars 
  in
  let new_ctx = ctx_of_globals converted_globals c_ctx in
  new_ctx
  (* failwith "TODO: convert_globals_to_c" *)

let convert_funcs_to_c t_ctx c_ctx =
  let funcs = Typecheck.funcs_of t_ctx in
  let custom_types = Typecheck.custom_types_of t_ctx in
  let convert t_ctx c_ctx (fname, args_and_body) =
    let (args, body) = args_and_body in
    let ret_type = 
      match List.assoc_opt fname custom_types with
      | Some(t) -> translate_my_type_to_c t_ctx c_ctx t
      | None -> failwith @@ Printf.sprintf 
        "Conversion error: couldn't find return type of %s among custom types!" fname
    in
    (* let body_stmts, body' = c_expr_of_expr t_ctx c_ctx body in *)
    let res_body = c_expr_of_expr None None t_ctx c_ctx body in
    let ( body_stmts
        , body'
        , macros
        , init_stmts
        , globals
        , _
        ) = unpack res_body
    in
    let body_stmts' = 
      match is_instantiable_type (Types.base_type_of @@ Typecheck.type_of body) with
      | false -> body_stmts
      | true -> body_stmts @ [Return (body')] (* assume that the last expression is returned *)
      (* | _ -> failwith "Conversion error: something went wrong when converting function..." *)
    in
    let func_decl =
      let args_types = List.map snd args in
      let args_types' = List.map (translate_my_type_to_c t_ctx c_ctx) args_types in
      FuncDecl(ret_type, fname, args_types')
      (* failwith "TODO: func_decl" *)
    in
    let func_def = 
      let args_and_types' =
        List.map (fun (a, b) -> (translate_my_type_to_c t_ctx c_ctx b, a)) args
      in
      FuncDef(ret_type, fname, args_and_types', body_stmts')
      (* failwith "TODO: func_def" *)
    in
    func_decl, func_def, macros, init_stmts, globals
    (* TODO: add all the information from the res into the context *)
    (* return function declaration and function definition *)
    (* failwith "TODO: convert_funcs_to_c: convert" *)
  in
  let (func_decls, func_defs, macros, init_stmts, globals) = 
    Util.split5 @@ List.map (convert t_ctx c_ctx) funcs 
  in
  let macros = List.flatten macros in
  let init_stmts = List.flatten init_stmts in
  let globals = List.flatten globals in
  assert (func_defs <> []);
  let new_ctx =
    c_ctx
    |> ctx_of_added_func_decls func_decls
    |> ctx_of_added_func_defs func_defs
    |> ctx_of_added_macros macros
    |> ctx_of_added_init_stmts init_stmts
    |> ctx_of_added_globals globals
  in
  new_ctx
  (* failwith "TODO: convert_funcs_to_ctx" *)

let convert_tasks_to_c t_ctx c_ctx =
  (* to convert task to C: *)
  (* - generate labels: loop beginning, waiting room *)
  (* - generate the macros? *)
  (* - generate the main loop *)
  (* - convert the body to C *)
  (* - generate function definition and declaration *)
  let tasks = Typecheck.tasks_of t_ctx in
  let interrupts = Typecheck.interrupts_of t_ctx in
  let tasks_enum =
    let task_names = "NO_TASK" :: ((List.map fst tasks) @ (List.map fst interrupts)) in
    let task_enum_values = List.map task_enum_of task_names in
    let enum = Enum("TASKS_ENUM_TYPE", task_enum_values) in
    enum
  in
  let convert (t_name, t_body) =
    let c_ctx = ctx_of_cur_name t_name c_ctx in
    let t_name_off = Printf.sprintf "task_%s" t_name in
    let ts = T(t_name) in
    let block_t = 
      let open Typecheck in
      match Util.last_el_of t_body with
      | Some(Expr_t(e)) -> Types.base_type_of @@ Typecheck.type_of e
      | Some(_) -> Unit
      | None -> Unit
    in
    (* body_stmts is incomplete *)
    let res_body = c_expr_of_expr (Some(t_name)) None t_ctx c_ctx (Block_t(block_t, t_body)) in
    let ( body_stmts
        , body'
        , macros
        , init_stmts
        , globals
        , used_mode_switching ) = unpack res_body 
    in
    (match body_stmts with
    | [] -> None
    | _ ->
      (match used_mode_switching with
      | true ->
        let task_handle = Printf.sprintf "task_%s_handle" t_name in
        let loop_beginning_label = Printf.sprintf "task_%s_loop_beginning" t_name in
        let waiting_room_label = Printf.sprintf "task_%s_waiting_room" t_name in
        let task_wait_room_macro = Printf.sprintf "TASK_%s_WAIT_ROOM_DELAY" t_name in
        let task_stack_size_macro = Printf.sprintf "TASK_%s_STACK_SIZE" t_name in
        let macros' = macros @ [ Macro(task_wait_room_macro, CastTo(CustomType("TickType_t"), IntLit(10))) 
                               ; Macro(task_stack_size_macro, IntLit(4196))
                               ] 
        in
        let task_delay = Expr(FuncCall("vTaskDelay", [CastTo(CustomType("TickType_t"), IntLit(10))])) in (* TODO: convert to macro *)
        let body_stmts' = body_stmts @ [task_delay] in
        let _while_loop = WhileLoop(BoolLit(true), [Label(loop_beginning_label)] @ body_stmts') in
        let waiting_room = 
          [ Label(waiting_room_label)
          ; WhileLoop(
              BoolLit(true), 
              [
                Expr(
                  FuncCall(
                    "vTaskDelay", 
                    [Ident(task_wait_room_macro)]
                  )
                )
              ]
            )
          ]
        in
        let self_delete = Expr(FuncCall("vTaskDelete", [Null])) in
        let func_decl = FuncDecl(Void, t_name_off, []) in
        let func_def = FuncDef(Void, t_name_off, [], 
          [_while_loop] @ waiting_room @ [self_delete]) 
        in
        let task_globals = [ GlobalVarDecl(CustomType("TaskHandle_t"), task_handle) ] in
        let task_init_stmts = 
          [ Expr(
              FuncCall(
                "xTaskCreate", 
                [ CastTo(CustomType("TaskFunction_t"), AddrOf(Ident(t_name_off)))
                ; StringLit(t_name_off)
                ; Ident(task_stack_size_macro)
                ; Null
                ; Ident("tskIDLE_PRIORITY")
                ; AddrOf(Ident(task_handle))
                ]
              )
            )
          ] 
        in
        Some((func_decl, func_def, macros', init_stmts @ task_init_stmts, globals @ task_globals, ts))
      | false ->
        let task_delay = Expr(FuncCall("vTaskDelay", [CastTo(CustomType("TickType_t"), IntLit(10))])) in
        (match prune_empty_blocks body_stmts with
        | [] -> None
        | body_stmts' ->
          let task_handle = Printf.sprintf "task_%s_handle" t_name in
          let body_stmts'' = body_stmts' @ [task_delay] in
          let _while_loop = WhileLoop(BoolLit(true), body_stmts'') in
          let self_delete = Expr(FuncCall("vTaskDelete", [Null])) in
          let func_decl = FuncDecl(Void, t_name_off, []) in
          let func_def = FuncDef(Void, t_name_off, [], [_while_loop; self_delete]) in
          let task_stack_size_macro = Printf.sprintf "TASK_%s_STACK_SIZE" t_name in
          let macros' = macros @ [ Macro(task_stack_size_macro, IntLit(4196)) ] in
          let task_globals = [ GlobalVarDecl(CustomType("TaskHandle_t"), task_handle) ] in
          let task_init_stmts = 
            [ Expr(
                FuncCall(
                  "xTaskCreate", 
                  [ CastTo(CustomType("TaskFunction_t"), AddrOf(Ident(t_name_off)))
                  ; StringLit(t_name_off)
                  ; Ident(task_stack_size_macro)
                  ; Null
                  ; Add(Ident("tskIDLE_PRIORITY"), IntLit(1))
                  ; AddrOf(Ident(task_handle))
                  ]
                )
              )
            ] 
          in
          Some((func_decl, func_def, macros', init_stmts @ task_init_stmts, globals @ task_globals, ts))
          (* Some((func_decl, func_def, macros, init_stmts, globals)) *)
        )
      )
    )
  in
  let func_decls, func_defs, macros, init_stmts, globals, t_or_p =
    tasks
    |> List.map convert
    |> List.filter (Option.is_some)
    |> List.map (Option.get)
    |> Util.split6
  in
  let macros = List.flatten macros in
  let init_stmts = List.flatten init_stmts in
  let globals = List.flatten globals in
  let new_ctx =
    c_ctx
    |> ctx_of_added_func_decls func_decls
    |> ctx_of_added_func_defs func_defs
    |> ctx_of_added_macros macros
    |> ctx_of_added_init_stmts init_stmts
    |> ctx_of_added_globals globals
    |> ctx_of_added_enums [tasks_enum]
    |> ctx_of_added_task_or_promise t_or_p
  in
  new_ctx
  (* failwith "TODO: convert_tasks_to_c" *)


let convert_promises_to_c t_ctx c_ctx =
  let promises = Typecheck.promises_of t_ctx in
  let custom_types = Typecheck.custom_types_of t_ctx in
  let promises_resolved = 
    t_ctx
    |> Typecheck.parallels_resolved_of
    |> snd
  in
  let convert_for_task taskname await_id (prom_name, rest) =
    (* to convert promise, we need to: *)
    (* - await the effect *)
    (* - bind the variables *)
    (* - translate the body *)
    (* - resolve the destinations *)
    let ps = P(prom_name) in
    let prom_name_full = 
      match await_id with
      | None    -> Printf.sprintf "promise_%s_%s" prom_name taskname
      | Some(v) -> Printf.sprintf "promise_%s_%s_%d" prom_name taskname v
    in
    let (eff_name, arg_names_and_types, _, body) = rest in
    (*
    print_endline @@ Printf.sprintf 
      "DEBUG: body of %s before conversion: %s" 
      (prom_name) 
      (Typecheck.string_of_expr body);
      *)
    (* let rt = translate_my_type_to_c t_ctx c_ctx rettype in *)
    (* let (eff_name, arg_names_and_types) = eff in *)
    let eff_t = 
      match List.assoc_opt eff_name custom_types with
      | Some(EffectDef(_) as t') -> t'
      | Some(t') -> failwith @@ Printf.sprintf
        "Conversion error: expected %s to be an effect, but something went wrong: %s" 
        eff_name
        (Types.string_of_ground_type t')
      | None -> failwith @@ Printf.sprintf
        "Conversion error: expected %s to be an effect, but it is not defined!"
        eff_name
    in
    let eff_t' = translate_my_type_to_c t_ctx c_ctx eff_t in
    let queue_name = 
      match await_id with
      | None -> Printf.sprintf "queue_%s_%s" prom_name taskname 
      | Some(v) -> Printf.sprintf "queue_%s_%s_%d_r" prom_name taskname v
    in
    let queue_size_macro = Printf.sprintf "QUEUE_%s_EL_CAPACITY" prom_name in
    let promise_stack_size_macro = Printf.sprintf "TASK_%s_STACK_SIZE" prom_name in
    let prom_handle = 
      match await_id with
      | None -> Printf.sprintf "promise_%s_%s_handle" prom_name taskname 
      | Some(v) -> Printf.sprintf "promise_%s_%s_%d_handle" prom_name taskname v
    in
    let prom_globals = 
      [ GlobalVarDecl(CustomType("TaskHandle_t"), prom_handle)
      ; GlobalVarDecl(CustomType("QueueHandle_t"), queue_name)
      ] 
    in
    let init_stmts = 
      [ Assign(
          Ident(queue_name),
          FuncCall(
            "xQueueCreate", 
            [ Ident(queue_size_macro)
            ; SizeOf(eff_t')
            ]
          )
        );
        Expr(
          FuncCall(
            "xTaskCreate",
            [ CastTo(CustomType("TaskFunction_t"), AddrOf(Ident(prom_name_full)))
            ; StringLit(prom_name_full)
            ; Ident(promise_stack_size_macro)
            ; Null
            ; Add(Ident("tskIDLE_PRIORITY"), IntLit(1))
            ; AddrOf(Ident(prom_handle))
            ]
          )
        )
      ] 
    in
    let eff_receive_var = new_var_name () in
    let eff_receive_var_decl = EmptyVarDef(eff_t', eff_receive_var) in
    let promise_wait_macro = Printf.sprintf "PROMISE_%s_RECEIVE_WAIT" prom_name in
    let queue_send_wait_macro = Printf.sprintf "PROMISE_%s_SEND_WAIT" prom_name in
    let macros = 
      [ Macro(promise_wait_macro, CastTo(CustomType("TickType_t"), IntLit(20)))
      ; Macro(queue_send_wait_macro, CastTo(CustomType("TickType_t"), IntLit(20))) 
      ; Macro(queue_size_macro, IntLit(20))
      ; Macro(promise_stack_size_macro, IntLit(4196))
      ] 
    in
    let wait_for_effect_loop = 
      WhileLoop(
        BoolLit(true),
        [
          IfStmt(
            Eq(
              FuncCall(
                "xQueueReceive", 
                [ Ident(queue_name)
                ; AddrOf(Ident(eff_receive_var))
                ; CastTo(CustomType("TickType_t"), Ident(promise_wait_macro))
                ]),
              Ident("pdTRUE")
            ),
            [ Break ],
            []
          )
        ]
      )
    in
    let origin_var = "origin" in
    let args_and_types' = (origin_var, Types.CustomType("TASKS_ENUM_TYPE")) :: arg_names_and_types in
    let (bind_var_defs, bind_args_to_vars) =
      args_and_types'
      |> List.mapi (fun i (name, tp) ->
          let tp' = translate_my_type_to_c t_ctx c_ctx tp in
          let var_def = EmptyVarDef(tp', name) in
          let var_assign = 
            Assign(Ident(name), MemberAccess(Ident(eff_receive_var), Printf.sprintf "pos_%d" i)) 
          in
          (var_def, var_assign))
      |> List.split 
    in
    (* let (body_stmts, body') = c_expr_of_expr t_ctx c_ctx body in *)
    let res_body = c_expr_of_expr (Some(taskname)) (Some(prom_name, await_id, origin_var)) t_ctx c_ctx body in
    let body' = value_of res_body in
    let body_stmts = prelude_of res_body in
    print_endline @@ 
      Printf.sprintf "DEBUG: body of a %s: %s" prom_name @@ 
          (String.concat "" @@ 
            List.map string_of_stmt body_stmts);
    (* 
    let (res_var, res_var_decl, assign) =
      match is_instantiable_type rettype with
      | true -> 
        let res_var = new_var_name () in
        res_var, [EmptyVarDef(rt, res_var)], [Assign(Ident(res_var), body')]
      | false -> "", [], []
    in
    *)
    let loop_beginning_label = 
      let rec find_goto_in_stmt_list l =
        let f = find_goto_in_stmt_list in
        match l with
        | [] -> false
        | x :: rest ->
          (match x with
          | Goto("loop_beginning") -> true
          | WhileLoop(_, b)
          | ForLoop(_, _, _, b)
          | Block(b) -> f b || f rest
          | IfStmt(_, a, b) ->
            f a || f b || f rest
          | ElifChain(l') ->
            (List.fold_left (fun acc (_, b) -> acc || f b) false l') || f rest
          | _ -> f rest)
      in
      print_endline @@ Printf.sprintf "DEBUG: checking if need loop_beginning label for %s: %s" prom_name @@ 
          (String.concat "" @@ 
            List.map string_of_stmt body_stmts);
      (match find_goto_in_stmt_list body_stmts with
      | true -> [Label("loop_beginning")] 
      | false -> []
      )
    in
    let sleep = Expr(FuncCall("vTaskDelay", [CastTo(CustomType("TickType_t"), IntLit(10))])) in
    let global_while_loop =
      WhileLoop(BoolLit(true),
        (* res_var_decl @ *)
         loop_beginning_label  @
        [eff_receive_var_decl] @
        [wait_for_effect_loop] @
        bind_var_defs @
        bind_args_to_vars @
        body_stmts @
        (* assign @ *)
        [sleep]
      )
    in
    let func_decl = FuncDecl(Void, prom_name_full, []) in
    let func_def = FuncDef(Void, prom_name_full, [], [global_while_loop]) in
    let (_, _, macros_body, init_stmts_body, globals_body, _) = unpack res_body in
    let globals = (globals_body @ prom_globals) in
    (func_decl, func_def, (macros_body @ macros), (init_stmts_body @ init_stmts), globals, ps)
    (* failwith "TODO: convert_promises_to_c: convert_for_task" *)
  in
  let convert (prom_name, rest) =
    let effects_and_triggered_promises = 
      t_ctx
      |> Typecheck.parallels_resolved_of
      |> fst
    in
    let module SSS = Util.SetStringString in
    let module SSOS = Util.SetStringOptionString in
    let module MS = Map.Make(String) in
    let promises_and_tasks =
      effects_and_triggered_promises
      |> List.map (fun (_, x) -> x)
      |> List.flatten
      |> List.map (fun (a, _, x, b) -> (a, x, b))
      |> SSOS.of_list
      |> SSOS.to_seq
      |> List.of_seq
      |> List.fold_left 
          (fun acc (a, b, c) ->
            match MS.find_opt a acc with
            | Some(l) -> MS.add a (Util.unique_naive (l @ [(b, c)])) acc
            | None -> MS.add a [(b, c)] acc
          ) 
          (MS.empty)
      |> MS.to_seq
      |> List.of_seq
    in
    let copies = 
      match List.assoc_opt prom_name promises_and_tasks with
      (* | Some(tasks_list) -> List.map (fun x -> convert_for_task x (prom_name, rest)) tasks_list *)
      | Some(tasks_list) -> 
          List.map (fun (await_id, x) -> convert_for_task x await_id (prom_name, rest)) tasks_list
      | None -> failwith @@ Printf.sprintf 
        "Conversion error: Promise %s is not defined when converting promises!" prom_name
    in
    copies
  in
  let (func_decls, func_defs, macros, init_stmts, globals, ps) = 
    promises
    |> List.map (convert)
    |> List.flatten
    |> Util.split6
  in
  let macros = List.flatten macros in
  let init_stmts = List.flatten init_stmts in
  let globals = List.flatten globals in
  let new_ctx =
    c_ctx
    |> ctx_of_added_func_decls func_decls
    |> ctx_of_added_func_defs func_defs
    |> ctx_of_added_macros macros
    |> ctx_of_added_init_stmts init_stmts
    |> ctx_of_added_globals globals
    |> ctx_of_added_task_or_promise ps
  in
  new_ctx
  (* failwith "TODO: convert_promises_to_c" *)

let add_necessary_includes t_ctx c_ctx =
  let free_rtos_includes =
    [ LocalImport "freertos/FreeRTOS.h"
    ; LocalImport "freertos/task.h"
    ]
  in
  let necessary_includes_of stmts =
    let rec loop_t t =
      match t with
      | Void -> []
      | Bool -> []
      | Char -> []
      | Int -> []
      | Float -> []
      | String -> []
      | ArrayWSize(t', _)
      | Array(t') -> loop_t t'
      | FuncDeclType(a, b) -> List.fold_left (@) [] @@ List.map loop_t @@ (a @ [b])
      | CustomType(n) -> (* TODO: depending on the type, different libraries may be required *)
        []
      | Pointer(t') -> loop_t t'
      | Int8
      | Int16
      | Int32
      | Int64
      | UInt8
      | UInt16
      | UInt32
      | UInt64 -> []
    in
    let rec loop_e e =
      match e with
      | Null
      | BoolLit(_)
      | CharLit(_)
      | IntLit(_)
      | FloatLit(_)
      | StringLit(_) -> []
      | Ident(_) -> [] (* BUG: Could be wrong about this one *)
      | Array(l) ->
        List.fold_left (@) [] @@ List.map loop_e l
      | AnonStruct(l) ->
        List.fold_left (@) [] @@ List.map (fun (_, x) -> loop_e x) l
      | IndexAccess(a, b) -> (loop_e a) @ (loop_e b)
      | MemberAccess(a, _) -> (loop_e a)
      | AddrOf(e') -> loop_e e'
      | Deref(e') -> loop_e e'
      | CastTo(t, e') -> (loop_t t) @ (loop_e e')
      | Not(e')
      | Neg(e') 
      | BNeg(e') -> loop_e e'
      | Add(a, b)
      | Sub(a, b)
      | Mul(a, b)
      | Div(a, b)
      | Mod(a, b)
      | Eq(a, b)
      | NEq(a, b)
      | Lt(a, b)
      | Le(a, b)
      | Gt(a, b)
      | Ge(a, b)
      | Or(a, b)
      | And(a, b)
      | BOr(a, b)
      | BAnd(a, b)
      | BXOr(a, b)
      | BSL(a, b)
      | BSR(a, b) -> (loop_e a) @ (loop_e b)
      | FuncCall(fname, args) ->
        let res = List.fold_left (@) [] @@ List.map (loop_e) args in
        (match fname with
        | "xQueueSend" -> [ LocalImport "freertos/queue.h" ]
        | "xQueueReceive" -> [ LocalImport "freertos/queue.h" ]
        | "xTaskCreate" -> [ LocalImport "freertos/task.h" ]
        | "xTaskDelete" -> [ LocalImport "freertos/task.h" ]
        | "printf" -> [ GlobalImport "stdio.h" ]
        | "puts" -> [ GlobalImport "stdio.h" ]
        | "memcpy" -> [ GlobalImport "string.h" ]
        | "rand" -> [ GlobalImport "stdlib.h" ]
        | _ -> []
        )
      | Ternary (a, b, c) -> (loop_e a) @ (loop_e b) @ (loop_e c)
    in
    let rec f b =
      List.fold_left (@) [] @@ List.map (loop_s) b 
    and loop_s s =
      match s with
      | Break
      | Continue
      | EmptyReturn -> []
      | Return(e)
      | Expr(e) -> loop_e e
      | Assign(a, b) -> (loop_e a) @ (loop_e b)
      | VarDef(t, _, v) -> (loop_t t) @ (loop_e v)
      | EmptyVarDef(t, _) -> (loop_t t)
      | Block(b) -> f b
      | WhileLoop(c, stmts) -> (loop_e c) @ (f stmts)
      | ForLoop(Some(init), cond, step, stmts) ->
        (loop_s init) @ (loop_e cond) @ (loop_s step) @ (f stmts)
      | ForLoop(None, cond, step, stmts) ->
        (loop_e cond) @ (loop_s step) @ (f stmts)
      | IfStmt(c, _t, _f) ->
        (loop_e c) @ (f _t) @ (f _f)
      | ElifChain(l) ->
        List.fold_left (@) [] @@ List.map (fun (c, stmts) -> (loop_e c) @ (f stmts)) l
      | IncPost(e)
      | DecPost(e)
      | IncPre(e)
      | DecPre(e) -> loop_e e
      | Goto _
      | Label _ -> []
    in
    Util.unique_naive @@ f stmts
    (* failwith "TODO: necessary_includes_of" *)
  in
  let func_defs = func_defs_of c_ctx in
  let includes = 
    let unpack_func_def = function
      | FuncDef(_, _, _, stmts) -> (stmts)
      | _ -> failwith "add_necessary_includes: Impossible case"
    in
    func_defs
    |> List.map (unpack_func_def)
    |> List.map (necessary_includes_of)
    |> List.fold_left (@) []
    |> (@) free_rtos_includes
    |> Util.unique_naive
  in
  let new_ctx = ctx_of_added_includes includes c_ctx in
  new_ctx
  (* failwith "TODO: add_necessary_includes" *)

let augment_app_main t_ctx c_ctx =
  let func_defs = func_defs_of c_ctx in
  let app_main_func = 
    List.filter 
    (fun x ->
      match x with
      | FuncDef(_, "app_main", _, _) -> true
      | _ -> false
    )
    func_defs
  in
  let body = 
    let v =
      List.map 
        (fun x -> 
          match x with
          | FuncDef(_, _, _, body) -> body
          | _ -> failwith "augment_app_main: Impossible case"
        ) 
        app_main_func
    in
    match v with
    | [] -> failwith "Conversion error: problem when augmenting app_main!"
    | x :: _ -> x
  in
  let _while_loop = 
    WhileLoop(
      BoolLit(true), 
      [
        Expr(
          FuncCall("vTaskDelay", [CastTo(CustomType("TickType_t"), IntLit(1))])
        )
      ]
    ) 
  in
  let setup_func = Expr(FuncCall("setup", [])) in
  (* let start_scheduler = Expr(FuncCall("vTaskStartScheduler", [])) in *)
  let body' = body @ [setup_func; (* start_scheduler; *) _while_loop] in
  let app_main_func' = FuncDef(Void, "app_main", [], body') in
  let func_defs' = 
    List.filter 
      (fun x ->
        match x with
        | FuncDef(_, "app_main", _, _) -> false
        | _ -> true
      )
      func_defs
  in
  let func_defs'' = func_defs' @ [app_main_func'] in
  let new_ctx = ctx_of_func_defs func_defs'' c_ctx in
  new_ctx
  (* failwith "TODO: augment_app_main" *)

let convert_mode_types_to_c t_ctx c_ctx =
  let custom_types = Typecheck.custom_types_of t_ctx in
  let modetypedefs = 
    custom_types
    |> List.filter (function
      | _, Types.ModetypeDef(_, _) -> true
      | _ -> false
    )
    |> List.map (function
      | _, Types.ModetypeDef(name, modes) -> (name, modes)
      | _ -> failwith "convert_mode_tyeps_to_c: Impossible case")
  in
  let convert (name, modes) = Enum(name, modes) in
  let new_enums = List.map convert modetypedefs in
  let new_ctx = ctx_of_added_enums new_enums c_ctx in
  new_ctx
  (* failwith "TODO: convert_mode_types_to_c" *)

let convert_start_modes_to_c t_ctx c_ctx =
  let start_modes = Typecheck.start_modes_of t_ctx in
  let custom_types = Typecheck.custom_types_of t_ctx in
  let modes_and_modetypes = List.map (fun x ->
      match List.assoc x custom_types with
      | Modetype(modetype) -> (x, modetype)
      | _ -> failwith "convert_start_modes_to_c: Impossible case"
    ) 
    start_modes 
  in
  let global_var_decls = List.map (fun (_, x) -> 
    ([ GlobalVarDecl(CustomType(x), Printf.sprintf "PREV_%s_MODE" x)
     ; GlobalVarDecl(CustomType(x), Printf.sprintf "CUR_%s_MODE" x) 
     ])
    )
    modes_and_modetypes
  in
  let init_stmts = List.map 
    (fun (a, b) -> (
      [ Assign(Ident(Printf.sprintf "PREV_%s_MODE" b), Ident(a))
      ; Assign(Ident(Printf.sprintf "CUR_%s_MODE" b), Ident(a))
      ]
    )) 
    modes_and_modetypes 
  in
  let global_var_decls = List.flatten global_var_decls in
  let init_stmts = List.flatten init_stmts in
  let new_ctx =
    c_ctx
    |> ctx_of_added_init_stmts init_stmts
    |> ctx_of_added_globals global_var_decls
  in
  new_ctx
  (* failwith "TODO: convert_start_modes_to_c" *)

let sort_init_stmts t_ctx c_ctx =
  let init_stmts = init_stmts_of c_ctx in
  let is_task_creation x =
    match x with
    | Expr(FuncCall("xTaskCreate", _)) -> true
    | _ -> false
  in
  let task_creation_stmts = List.filter (is_task_creation) init_stmts in
  let others_stmts = List.filter (fun x -> not @@ is_task_creation x) init_stmts in
  let actual_task_names = 
    List.map 
      (fun (x) ->
        match x with
        | Expr(FuncCall(_, CastTo(_, AddrOf(Ident(name))) :: _)) -> name
        | _ -> failwith "sort_init_stmts: is_started_initially: task_names: Impossible case")
      task_creation_stmts
  in
  let module S = Set.Make(String) in
  let start_modes = Typecheck.start_modes_of t_ctx in
  let start_modes_s = S.of_list start_modes in
  let tasks_and_modetypes = Typecheck.tasks_and_modetypes_of t_ctx in
  let modes_and_tasks = Typecheck.modes_and_tasks_of t_ctx in
  let tasks_and_promises = Typecheck.tasks_and_promises_of t_ctx in
  let t_or_p = task_or_promise_of c_ctx in
  let rename x =
    let v = List.find_opt (fun y -> 
        match y with
        | T(n) -> n = x
        | P(n) -> n = x
      ) t_or_p 
    in
    match v with
    | Some(T(n)) -> Printf.sprintf "task_%s" n
    | Some(P(n)) -> Printf.sprintf "promise_%s" n
    | None -> failwith @@ Printf.sprintf "Conversion error: %s is not a task nor a promise" x
  in
  let starting_tasks_from_modes = 
    modes_and_tasks
    |> List.filter (fun (mode, _) -> S.mem mode start_modes_s)
    |> List.map snd
    |> List.flatten
    |> List.map (fun x -> x :: (List.assoc x tasks_and_promises))
    |> List.flatten
    |> List.map rename
    |> Util.unique_strings
  in
  let non_starting_tasks_from_modes =
    modes_and_tasks
    |> List.filter (fun (mode, _) -> not @@ S.mem mode start_modes_s)
    |> List.map snd
    |> List.flatten
    |> List.map (fun x -> x :: (List.assoc x tasks_and_promises))
    |> List.flatten
    |> List.map rename
    |> Util.unique_strings
  in
  let starting_tasks_from_modes_s = S.of_list starting_tasks_from_modes in
  let non_starting_tasks_from_modes_s = S.of_list non_starting_tasks_from_modes in
  let tasks = Typecheck.tasks_of t_ctx in
  let all_tasks = S.of_list actual_task_names in
  let initial_tasks_s = 
    S.(union 
        (diff 
          all_tasks
          non_starting_tasks_from_modes_s) 
        starting_tasks_from_modes_s) 
  in
  let task_creation_stmts' = 
    List.filter 
      (fun (x) ->
        print_endline @@ Printf.sprintf "DEBUG: trying to work on this: %s" (string_of_stmt x);
        match x with
        | Expr(FuncCall("xTaskCreate", (CastTo(_, AddrOf(Ident(n)))) :: _)) ->
          S.mem n initial_tasks_s
        | _ -> failwith "task_create_init_stmts': Impossible case"
      )
      task_creation_stmts
  in
  let new_init_stmts = others_stmts @ task_creation_stmts' in
  List.iter (fun (x) -> 
    match x with
    | Expr(FuncCall("xTaskCreate", CastTo(_, AddrOf(Ident(v))) :: _)) ->
      print_endline @@ Printf.sprintf "DEBUG: Initial task creation: %s" v
    | _ -> failwith "LOLKEK: Impossible case"
  ) task_creation_stmts';
  let new_ctx = ctx_of_init_stmts new_init_stmts c_ctx in
  new_ctx
  (* failwith "TODO: sort_init_stmts" *)

let convert_interrups_to_c t_ctx c_ctx =
  let interrupts = Typecheck.interrupts_of t_ctx in
  let parallels_resolved = Typecheck.parallels_resolved_of t_ctx in
  let (effects, _) = parallels_resolved in
  let convert (i_name, i_eff) =
    let eff_types =
      let custom_types = Typecheck.custom_types_of t_ctx in
      (match List.assoc_opt i_eff custom_types with
      | Some(Types.EffectDef(_, types)) -> types
      | Some(_) -> failwith @@ Printf.sprintf
          "Conversion error: expected effect def, when searching for %s in custom types!" i_eff
      | None -> failwith @@ Printf.sprintf
          "Conversion error: %s effect is not defined!" i_eff)
    in
    let eff_types_message =
      let types' = 
        List.mapi 
          (fun i x -> (Printf.sprintf "pos_%d" (i + 1), Types.string_of_ground_type x))
          eff_types
      in
      let types'' = ("pos_0", Printf.sprintf "TASKS_ENUM_TYPE with %s" (task_enum_of i_name)) :: types' in
      let types_as_string =
        String.concat "\n" @@ List.map (fun (a, b) -> Printf.sprintf "    %s %s" a b) types''
      in
      Printf.sprintf "Need to send the following effect: %s\n%s" i_eff types_as_string
    in
    let promises_to_trigger = 
      let relevant_effects = List.filter (fun ((_, t_name), _) -> t_name = i_name) effects in
      match relevant_effects with
      | [] -> failwith "Conversion error: convert_interrputs: Impossible case"
      | (_, x) :: _ -> x
    in
    let queues = 
      String.concat "\n" @@
        List.map (fun (a, _, v, b) -> 
          match v with
          | None -> Printf.sprintf "  queue_%s_%s" a b
          | Some(v') -> Printf.sprintf "  queue_%s_%s_%d_r" a b v') 
        promises_to_trigger 
    in
    let queues_message =
      Printf.sprintf "Need to trigger the following queues:\n%s" queues 
    in
    let message =
      Printf.sprintf "  %s\n  %s" eff_types_message queues_message
    in
    let comment = MultiLineComment(message) in
    comment
  in
  let comments = List.map convert interrupts in
  ctx_of_added_multiline_comments comments c_ctx

let convert_to_c typecheck_ctx =
  (* Need to convert my langauge to C *)
  (* Convert functions to C *)
  (* Convert tasks to FreeRTOS tasks and functions (DONE) *)
  (* Convert promises to FreeRTOS tasks and functions *)
  (* Convert effects to structs and queues *)
  (* Convert structs to structs (DONE) *)
  (* Convert sumtypes to structs *)
  (* Convert modetypes to enums (DONE) *)
  (* BUG: I am not installing mutexes in places of accessing of global variables *)
  let converted_ctx = empty_ctx in
  let converted_ctx = convert_struct_defs_to_c typecheck_ctx converted_ctx in
  let converted_ctx = convert_effects_defs_to_c typecheck_ctx converted_ctx in
  let converted_ctx = convert_modetype_defs_to_c typecheck_ctx converted_ctx in
  let converted_ctx = convert_sumtype_defs_to_c typecheck_ctx converted_ctx in
  let converted_ctx = convert_globals_to_c typecheck_ctx converted_ctx in
  let converted_ctx = convert_interrups_to_c typecheck_ctx converted_ctx in
  let converted_ctx = convert_funcs_to_c typecheck_ctx converted_ctx in
  let converted_ctx = convert_promises_to_c typecheck_ctx converted_ctx in
  let converted_ctx = convert_tasks_to_c typecheck_ctx converted_ctx in
  let converted_ctx = convert_start_modes_to_c typecheck_ctx converted_ctx in
  let converted_ctx = add_necessary_includes typecheck_ctx converted_ctx in
  let converted_ctx = augment_app_main typecheck_ctx converted_ctx in
  let converted_ctx = sort_init_stmts typecheck_ctx converted_ctx in
  converted_ctx
  (* failwith "TODO: convert_to_c" *)
