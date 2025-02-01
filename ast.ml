type t =
  | Not of t
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | FNeg of t
  | FAdd of t * t
  | FSub of t * t
  | Eq of t * t
  | LE of t * t
  | If of t * t * t
  | Let of string * t * t
(*| LetTuple of (string * My_types.t) list * t * t*)
  | FunctionCall of (string) * t list
(*| ExplicitArray of t * int*)
(*| ImplicitArray of t*)
  | RaiseEffect of (string * t list)
  | Promise of (string * string * string list * t * t) (* should be able to pattern match here *)
  | Await of t * string * t
  | Run of t
(*| Parallelize of t * t*)
  | Sequence of t * t
  | Return of t

type expr =
  | Unit
  | Bool of bool
  | Char of char
  | Int of int
  | Float of float
  | String of string
  | Array of expr list
  | Struct
  | AnonStruct of (string * expr) list
  | Tuple of expr list
  | Ident of string
  | FuncCall of string * (expr list)
  | CastTo of Types.ground_type * expr
  | Not of expr
  | Neg of expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Mod of expr * expr
  | FAdd of expr * expr
  | FSub of expr * expr
  | FMul of expr * expr
  | FDiv of expr * expr
  | Eq of expr * expr
  | Neq of expr * expr
  | Lt of expr * expr
  | Le of expr * expr
  | Gt of expr * expr
  | Ge of expr * expr
  | LAnd of expr * expr
  | LOr of expr * expr
  | BNeg of expr
  | BAnd of expr * expr
  | BOr of expr * expr
  | BXOr of expr * expr
  | BShiftLeft of expr * expr
  | BShiftRight of expr * expr
  | IndexAccess of expr * expr
  | MemberAccess of expr * string
  | Match of expr * (expr * expr) list
  | Await of string
  | Raise of string * (expr list)
  | If of expr * expr * expr
  | While of expr * expr (* the proper way to build them is always with Block *)
  | ForTo of string * expr * expr * expr
  | ForDownTo of string * expr * expr * expr
  | ForEach of string * expr * expr
  | ForEachIndex of string * string * expr * expr
  | Block of stmt list
  | Any
  | Variant of string * expr list

and stmt =
  (* The With variations are needed because *) 
  (* while and for-loop are actually expressions *)
  (* If we break in the middle of the for-loop, *)
  (* We still need to provide some value *)
  (* In case of continue, if the next iteration *)
  (* is the last, we still need to have some value, *)
  (* to ensure the type-safety *)
  | Break
  | BreakWith of expr
  | Continue
  | ContinueWith of expr
  (* Return depending on the context means different things: *)
  (* In functions it is traditional return *)
  (* In promises it "returns" values to await statements *)
  (* In tasks and app_main it doesn't make sense *)
  | EmptyReturn
  | Return of expr
  | Expr of expr
  | Global of string
  | Assign of expr * expr
  | Let of string * expr
  | LetMut of string * expr
  | ModeSwitch of string

type lib_include =
  | Local of string
  | System of string

type toplevel =
  | LibInclude of lib_include
  | StructDef of string * (string * Types.ground_type) list
  | EffectDef of string * (Types.ground_type list)
  | Global of string * Types.ground_type
  | FuncDecl of Types.ground_type * string * (Types.ground_type list)
  | FuncDef of Types.ground_type * string * (Types.ground_type * string) list * stmt list
  | TaskDef of string * stmt list
  | PromiseDef of string * string * string list * stmt list
  | ParallelDef of string list
  | InterruptDef
  | SumTypeDef of string * ((string * (Types.ground_type list)) list )
  | ModeDef of string * string list
  | ModeTasksDef of string * string list
  | StartModesDef of string list
  | TypecheckFuncDecl of string * (Types.ground_type list) * Types.ground_type
  | Interrupt of string * string

let rec string_of_expr e =
  let string_of_binop a b s =
    s ^ "(" ^ (string_of_expr a) ^ ", " ^ (string_of_expr b) ^ ")"
  in
  match e with
  | Unit -> "Unit"
  | Bool true -> "Bool(true)"
  | Bool false -> "Bool(false)"
  | Char c -> Printf.sprintf "Char(%c)" c
  | Int i -> Printf.sprintf "Int(%i)" i
  | Float f -> Printf.sprintf "Float(%f)" f
  | String s -> Printf.sprintf "String(%s)" s
  | Array a -> 
    a
    |> List.map (string_of_expr) 
    |> String.concat "; "
    |> Printf.sprintf "Array(%s)"
  | Struct -> "Struct"
  | AnonStruct(l) ->
    Printf.sprintf "AnonStruct([%s])" @@
    String.concat "; " @@
    List.map (fun (a, b) ->
      Printf.sprintf "(%s, %s)" a (string_of_expr b)) 
      l
  | Tuple t ->
    t
    |> List.map (string_of_expr)
    |> String.concat ", "
    |> Printf.sprintf "Tuple(%s)"
  | Ident i -> Printf.sprintf "Ident(%s)" i
  | FuncCall(name, args) ->
    Printf.sprintf
      "FuncCall(%s, %s)"
      (name)
      (String.concat ", " @@ List.map (string_of_expr) args)
  | CastTo(t, e') -> 
    Printf.sprintf 
      "CastTo(%s, %s)" 
      (Types.string_of_ground_type t) 
      (string_of_expr e')
  | Not e' -> Printf.sprintf "Not(%s)" @@ string_of_expr e'
  | Neg e' -> Printf.sprintf "Neg(%s)" @@ string_of_expr e'
  | Add(a, b) -> string_of_binop a b "Add"
  | Sub(a, b) -> string_of_binop a b "Sub"
  | Mul(a, b) -> string_of_binop a b "Mul"
  | Div(a, b) -> string_of_binop a b "Div"
  | Mod(a, b) -> string_of_binop a b "Mod"
  | FAdd(a, b) -> string_of_binop a b "FAdd"
  | FSub(a, b) -> string_of_binop a b "FSub"
  | FMul(a, b) -> string_of_binop a b "FMul"
  | FDiv(a, b) -> string_of_binop a b "FDiv"
  | Eq(a, b) -> string_of_binop a b "Eq"
  | Neq(a, b) -> string_of_binop a b "Neq"
  | Lt(a, b) -> string_of_binop a b "Lt"
  | Le(a, b) -> string_of_binop a b "Le"
  | Gt(a, b) -> string_of_binop a b "Gt"
  | Ge(a, b) -> string_of_binop a b "Ge"
  | LAnd(a, b) -> string_of_binop a b "LAnd"
  | LOr(a, b) -> string_of_binop a b "LOr"
  | BNeg e' -> Printf.sprintf "BNeg(%s)" @@ string_of_expr e'
  | BAnd(a, b) -> string_of_binop a b "BAnd"
  | BOr(a, b) -> string_of_binop a b "BOr"
  | BXOr(a, b) -> string_of_binop a b "BXOr"
  | BShiftLeft(a, b) -> string_of_binop a b "BShiftLeft"
  | BShiftRight(a, b) -> string_of_binop a b "BShiftRight"
  | IndexAccess(a, i) -> 
    Printf.sprintf "IndexAccess(%s, %s)"
      (string_of_expr a)
      (string_of_expr i)
  | MemberAccess(s, m) ->
    Printf.sprintf "MemberAccess(%s, %s)"
      (string_of_expr s)
      m
  | Match(v, branches) ->
    Printf.sprintf 
      "Match(%s, %s)"
      (string_of_expr v)
      (String.concat "\n" @@ List.map (fun (_, v) -> Printf.sprintf "Pattern, %s" @@ string_of_expr v) branches)
  | Await(s) -> Printf.sprintf "Await(%s)" s
  | Raise (effname, e') -> 
    Printf.sprintf "Raise(%s, %s)" 
      effname
      (e' |> List.map (string_of_expr) |> String.concat ", ")
  | If(c, t, f) ->
    Printf.sprintf
      "If(%s, %s, %s)"
      (string_of_expr c)
      (string_of_expr t)
      (string_of_expr f)
  | While(c, b) ->
    Printf.sprintf "While(%s, %s)"
      (string_of_expr c)
      (string_of_expr b)
  | ForTo(id, from, _to, b) -> Printf.sprintf "ForTo(%s, %s, %s, %s)"
    id
    (string_of_expr from)
    (string_of_expr _to)
    (string_of_expr b)
  | ForDownTo(id, from, _to, b) -> Printf.sprintf "ForDownTo(%s, %s, %s, %s)"
    id
    (string_of_expr from)
    (string_of_expr _to)
    (string_of_expr b)
  | ForEach(id, arr, b) -> Printf.sprintf "ForEach(%s, %s, %s)"
    id
    (string_of_expr arr)
    (string_of_expr b)
  | ForEachIndex(id, i, arr, b) -> Printf.sprintf "ForEachIndex(%s, %s, %s, %s)"
    id
    i
    (string_of_expr arr)
    (string_of_expr b)
  | Block(b) -> Printf.sprintf "Block([%s])"
    (String.concat "; " @@ List.map string_of_stmt b)
  | Any -> "Any"
  | Variant(name, l) -> Printf.sprintf "Variant(%s, [%s])"
    name
    (String.concat "; " @@ List.map string_of_expr l)

  (*| Let(name, m, n) ->
    Printf.sprintf "Let(%s, %s, %s)"
      name
      (string_of_expr m)
      (string_of_expr n)
  | LetMut(name, m, n) ->
    Printf.sprintf "LetMut(%s, %s, %s)"
      name
      (string_of_expr m)
      (string_of_expr n)*)

and string_of_stmt s =
  match s with
  | Break -> "Break"
  | BreakWith(v) -> Printf.sprintf "BreakWith(%s)" (string_of_expr v)
  | Continue -> "Continue"
  | ContinueWith(v) -> Printf.sprintf "ContinueWith(%s)" (string_of_expr v)
  | EmptyReturn -> "EmptyReturn"
  | Return(v) -> Printf.sprintf "Return(%s)" (string_of_expr v)
  | Expr(e) -> Printf.sprintf "Expr(%s)" (string_of_expr e)
  | Global(s) -> Printf.sprintf "Global(%s)" s
  | Assign(a, b) -> Printf.sprintf "Assign(%s, %s)"
    (string_of_expr a)
    (string_of_expr b)
  | Let(name, v) -> Printf.sprintf "Let(%s, %s)"
    name
    (string_of_expr v)
  | LetMut(name, v) -> Printf.sprintf "LetMut(%s, %s)"
    name
    (string_of_expr v)
  | ModeSwitch(name) -> Printf.sprintf "ModeSwitch(%s)" name
