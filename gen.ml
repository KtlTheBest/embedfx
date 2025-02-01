open Convert

let rec gen_c_type = function
  | Void -> "void"
  | Bool -> "bool"
  | Char -> "char"
  | Int -> "int"
  | Float -> "float"
  | String -> "char *"
  | ArrayWSize(t', i) -> Printf.sprintf "%s[%d]" (gen_c_type t') i
  | Array(t') -> Printf.sprintf "%s[]" (gen_c_type t')
  | FuncDeclType(_, t') -> gen_c_type t'
  | CustomType(s) -> s
  | Pointer(t') -> Printf.sprintf "%s *" (gen_c_type t')
  | Int8 -> "int8_t"
  | Int16 -> "int16_t"
  | Int32 -> "int32_t"
  | Int64 -> "int64_t"
  | UInt8 -> "uint8_t"
  | UInt16 -> "uint16_t"
  | UInt32 -> "uint32_t"
  | UInt64 -> "uint64_t"

let rec gen_c_expr = function
  | Null -> "NULL"
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | CharLit(c) -> Printf.sprintf "'%c'" c
  | IntLit(i) -> Printf.sprintf "%d" i
  | FloatLit(f) -> Printf.sprintf "%f" f
  | StringLit(s) -> Printf.sprintf "\"%s\"" s
  | Ident(i) -> i
  | Array(l) ->
    l
    |> List.map gen_c_expr
    |> String.concat ", "
    |> Printf.sprintf "[%s]"
  | AnonStruct(l) ->
    l
    |> List.map (fun (_field, v) -> Printf.sprintf ".%s = %s " _field (gen_c_expr v))
    |> String.concat ","
    |> Printf.sprintf "{ %s}"
  | IndexAccess(a, b) -> 
    let a' = gen_c_expr a in
    let b' = gen_c_expr b in
    Printf.sprintf "%s[%s]" a' b'
  | MemberAccess(a, name) ->
    Printf.sprintf "%s.%s" (gen_c_expr a) name
  | AddrOf(e') ->
    Printf.sprintf "&(%s)" (gen_c_expr e')
  | Deref(e') ->
    Printf.sprintf "*(%s)" (gen_c_expr e')
  | CastTo(t, e') ->
    let t' = gen_c_type t in
    Printf.sprintf "(%s)(%s)" t' (gen_c_expr e')
  | Not(e') ->
    Printf.sprintf "!(%s)" (gen_c_expr e')
  | Neg(e') ->
    Printf.sprintf "-(%s)" (gen_c_expr e')
  | Add(a, b) ->
    let a' = gen_c_expr a in
    let b' = gen_c_expr b in
    Printf.sprintf "(%s) + (%s)" a' b'
  | Sub(a, b) ->
    let a' = gen_c_expr a in
    let b' = gen_c_expr b in
    Printf.sprintf "(%s) - (%s)" a' b'
  | Mul(a, b) ->
    let a' = gen_c_expr a in
    let b' = gen_c_expr b in
    Printf.sprintf "(%s) * (%s)" a' b'
  | Div(a, b) ->
    let a' = gen_c_expr a in
    let b' = gen_c_expr b in
    Printf.sprintf "(%s) / (%s)" a' b'
  | Mod(a, b) ->
    let a' = gen_c_expr a in
    let b' = gen_c_expr b in
    Printf.sprintf "(%s) %% (%s)" a' b'
  | Eq(a, b) ->
    let a' = gen_c_expr a in
    let b' = gen_c_expr b in
    Printf.sprintf "(%s) == (%s)" a' b'
  | NEq(a, b) ->
    let a' = gen_c_expr a in
    let b' = gen_c_expr b in
    Printf.sprintf "(%s) != (%s)" a' b'
  | Lt(a, b) ->
    let a' = gen_c_expr a in
    let b' = gen_c_expr b in
    Printf.sprintf "(%s) < (%s)" a' b'
  | Le(a, b) ->
    let a' = gen_c_expr a in
    let b' = gen_c_expr b in
    Printf.sprintf "(%s) <= (%s)" a' b'
  | Gt(a, b) ->
    let a' = gen_c_expr a in
    let b' = gen_c_expr b in
    Printf.sprintf "(%s) > (%s)" a' b'
  | Ge(a, b) ->
    let a' = gen_c_expr a in
    let b' = gen_c_expr b in
    Printf.sprintf "(%s) >= (%s)" a' b'
  | Or(a, b) ->
    let a' = gen_c_expr a in
    let b' = gen_c_expr b in
    Printf.sprintf "(%s) || (%s)" a' b'
  | And(a, b) ->
    let a' = gen_c_expr a in
    let b' = gen_c_expr b in
    Printf.sprintf "(%s) && (%s)" a' b'
  | BNeg(e') ->
    Printf.sprintf "~(%s)" (gen_c_expr e')
  | BOr(a, b) ->
    let a' = gen_c_expr a in
    let b' = gen_c_expr b in
    Printf.sprintf "(%s) | (%s)" a' b'
  | BAnd(a, b) ->
    let a' = gen_c_expr a in
    let b' = gen_c_expr b in
    Printf.sprintf "(%s) & (%s)" a' b'
  | BXOr(a, b) ->
    let a' = gen_c_expr a in
    let b' = gen_c_expr b in
    Printf.sprintf "(%s) ^ (%s)" a' b'
  | BSL(a, b) ->
    let a' = gen_c_expr a in
    let b' = gen_c_expr b in
    Printf.sprintf "(%s) << (%s)" a' b'
  | BSR(a, b) ->
    let a' = gen_c_expr a in
    let b' = gen_c_expr b in
    Printf.sprintf "(%s) >> (%s)" a' b'
  | FuncCall(fname, args) ->
    let args' =
      args
      |> List.map (gen_c_expr)
      |> String.concat ", "
    in
    Printf.sprintf "%s(%s)" fname args'
  | Ternary(a, b, c) ->
    let a' = gen_c_expr a in
    let b' = gen_c_expr b in
    let c' = gen_c_expr c in
    Printf.sprintf "(%s) ? (%s) : (%s)" a' b' c'
  | SizeOf(n) -> Printf.sprintf "sizeof(%s)" (gen_c_type n)

let rec gen_c_stmt tab s =
  let gen_stmts stmts =
    stmts
    |>          List.map (gen_c_stmt ("  " ^ tab))
    |>          String.concat ""
  in
  match s with
  | Break -> Printf.sprintf "%sbreak;\n" tab
  | Continue -> Printf.sprintf "%scontinue;\n" tab
  | EmptyReturn -> Printf.sprintf "%sreturn;\n" tab
  | Return(e) -> Printf.sprintf "%sreturn (%s);\n" tab (gen_c_expr e)
  | Expr(e) -> Printf.sprintf "%s%s;\n" tab (gen_c_expr e)
  | Assign(a, b) -> Printf.sprintf "%s%s = %s;\n" tab (gen_c_expr a) (gen_c_expr b)
  | VarDef(t, vname, e) -> Printf.sprintf "%s%s %s = %s;\n" tab (gen_c_type t) vname (gen_c_expr e)
  | EmptyVarDef(t, vname) -> Printf.sprintf "%s%s %s;\n" tab (gen_c_type t) vname
  | Block(stmts) -> Printf.sprintf "%s{\n%s%s}\n" tab (gen_stmts stmts) tab
  | WhileLoop(cond, stmts) ->
    let cond' = gen_c_expr cond in
    Printf.sprintf "%swhile(%s){\n%s%s}\n" tab cond' (gen_stmts stmts) tab
  | ForLoop(Some(init), cond, step, stmts) ->
    let init' = gen_c_stmt "" init in
    let cond' = gen_c_expr    cond in
    let step' = gen_c_stmt "" step in
    Printf.sprintf "%sfor(%s; %s; %s){\n%s%s}\n"
      tab
      init'
      cond'
      step'
      (gen_stmts stmts)
      tab
  | ForLoop(None, cond, step, stmts) ->
    let cond' = gen_c_expr    cond in
    let step' = gen_c_stmt "" step in
    Printf.sprintf "%sfor(; %s; %s){\n%s%s}\n"
      tab
      cond'
      step'
      (gen_stmts stmts)
      tab
  | IfStmt(cond, _t, []) ->
    let cond' = gen_c_expr cond in
    let t' = gen_stmts _t in
    Printf.sprintf "%sif(%s){\n%s%s}\n" tab cond' t' tab
  | IfStmt(cond, _t, _f) ->
    let cond' = gen_c_expr cond in
    let t' = gen_stmts _t in
    let f' = gen_stmts _f in
    Printf.sprintf "%sif(%s){\n%s%s} else {\n%s%s}\n" tab cond' t' tab f' tab
  | ElifChain(l) ->
    let gen (cond, stmts) =
      let cond' = gen_c_expr cond in
      let stmts' = gen_stmts stmts in
      Printf.sprintf "%sif(%s){\n%s%s}" tab cond' stmts' tab
    in
    l
    |> List.map gen
    |> String.concat " else\n"
    |> fun x -> x ^ "\n"
  | IncPost(e) -> Printf.sprintf "%s%s++;\n" tab (gen_c_expr e)
  | DecPost(e) -> Printf.sprintf "%s%s--;\n" tab (gen_c_expr e)
  | IncPre (e) -> Printf.sprintf "%s++%s;\n" tab (gen_c_expr e)
  | DecPre (e) -> Printf.sprintf "%s--%s;\n" tab (gen_c_expr e)
  | Goto(s) -> Printf.sprintf "%sgoto %s;\n" tab s
  | Label(s) -> Printf.sprintf "%s%s:\n" tab s

let gen_includes c_ctx =
  let includes = includes_of c_ctx in
  let gen v =
    match v with
    | LocalImport s -> Printf.sprintf "#include \"%s\"\n" s
    | GlobalImport s -> Printf.sprintf "#include <%s>\n" s
    | _ -> failwith "gen_includes: gen: Impossible case"
  in
  String.concat "" @@ List.map gen includes

let gen_structs c_ctx =
  let (structs_raw, structs_union_raw) =
    let structs' = structs_of c_ctx in
    let is_struct = function
      | StructDecl(name, fields) -> true
      | _ -> false
    in
    let is_struct_union = function
      | StructWithUnions(name, _, l) -> true
      | _ -> false
    in
    let unpack_struct = function
      | StructDecl(name, fields) -> (name, fields)
      | _ -> failwith "gen_structs: unpack_struct: Impossible case 1"
    in
    let unpack_struct_union = function
      | StructWithUnions(name, _, l) -> (name, l)
      | _ -> failwith "gen_structs: unpack_struct: Impossible case 2"
    in
    let structs_unpacked = 
      structs'
      |> List.filter is_struct
      |> List.map unpack_struct
    in
    let structs_w_union_unpacked =
      structs'
      |> List.filter is_struct_union
      |> List.map unpack_struct_union
    in
    (structs_unpacked, structs_w_union_unpacked)
  in
  let gen_struct (name, fields) =
    let (fields' : string) = 
      String.concat "" @@ 
        List.map (fun (name, t) -> Printf.sprintf "  %s %s;\n" (gen_c_type t) name) fields 
    in
    let struct_def = Printf.sprintf "typedef struct {\n%s} %s;\n" fields' name in
    struct_def
  in
  let gen_struct_union (name, fields) =
    let (fields' : string) = 
      String.concat "" @@ 
        List.map (fun (name, t) -> Printf.sprintf "    %s %s;\n" (gen_c_type t) name) fields 
    in
    let struct_def = 
      Printf.sprintf 
        "typedef struct {\n  int var_type;\n  union {\n%s  } content;\n} %s;\n" 
        fields' 
        name 
    in
    struct_def
  in
  let structs_genned =
    structs_raw
    |> List.map gen_struct
    |> String.concat "\n"
  in
  let structs_w_structs_genned =
    structs_union_raw
    |> List.map gen_struct_union
    |> String.concat "\n"
  in
  structs_genned ^ "\n" ^ structs_w_structs_genned
  (* failwith "TODO: gen_structs" *)

let gen_func_def c_ctx =
  let extract = function
    | FuncDef(rt, fname, args, body) ->
      (rt, fname, args, body)
    | _ -> failwith "Generation error: expected function definition, but got something else!"
  in
  let gen (rt, fname, args, body) =
    let open Util in
    debug @@ Printf.sprintf "Generating function: %s" fname;
    debug @@ string_of_stmt (Block(body));
    let rt' = gen_c_type rt in
    let args' = 
      args
      |> List.map (fun (t, a) -> (Printf.sprintf "%s %s" (gen_c_type t) a))
      |> String.concat ", "
    in
    let body' = gen_c_stmt "" (Block(body)) in
    match args' with
    | "" -> Printf.sprintf "%s %s(void)%s\n" rt' fname body'
    | _ -> Printf.sprintf "%s %s(%s)%s\n" rt' fname args' body'
  in
  let funcs_raw = func_defs_of c_ctx in
  assert (funcs_raw <> []);
  let funcs_extracted = List.map extract funcs_raw in
  let funcs_code = List.map gen funcs_extracted in
  String.concat "" funcs_code
  (* failwith "TODO: gen_func_def" *)

let gen_macros c_ctx =
  let extract = function
    | Macro(name, value) -> (name, value)
    | _ -> failwith "gen_macros: Impossible case"
  in
  let gen (name, value) =
    Printf.sprintf "#define %s %s\n" name (gen_c_expr value)
  in
  let macros = macros_of c_ctx in
  let macros_extracted = List.map extract macros in
  let macros_code = List.map gen macros_extracted in
  String.concat "" macros_code
  (* failwith "TODO: gen_macros" *)

let gen_init_func c_ctx =
  let stmts_raw = init_stmts_of c_ctx in
  let stmts = String.concat "" @@ List.map (gen_c_stmt "  ") stmts_raw in
  Printf.sprintf "void setup(void){\n%s}\n" stmts
  (* failwith "TODO: gen_init_func" *)

let gen_globals c_ctx =
  let unpack x =
    match x with
    | GlobalVarDecl(t, n) -> (t, n)
    | _ -> failwith "gen_globals: Impossible case"
  in
  let gen (t, n) =
    Printf.sprintf "%s %s;\n" (gen_c_type t) n
  in
  let globals = globals_of c_ctx in
  let globals_raw = List.map unpack globals in
  let globals' = String.concat "" @@ List.map gen globals_raw in
  globals'
  (* failwith "TODO: gen_globals" *)

let gen_func_decl c_ctx =
  let func_decls = func_decls_of c_ctx in
  let unpack x =
    match x with
    | FuncDecl(t, n, args) -> (t, n, args)
    | _ -> failwith "gen_func_decl: Impossible case"
  in
  let gen (t, n, args) =
    (match args with
    | [] ->
      Printf.sprintf "%s %s(void);" (gen_c_type t) n
    | args' ->
      let t' = gen_c_type t in
      let args'' = String.concat ", " @@ List.map gen_c_type args' in
      Printf.sprintf "%s %s(%s);" t' n args''
    )
  in
  let func_decls_code = String.concat "\n" @@ List.map gen @@ List.map unpack func_decls in
  func_decls_code

let gen_enums c_ctx =
  let enums = enums_of c_ctx in
  let unpack = function
    | Enum(name, values) -> (name, values)
    | _ -> failwith "gen_enums: unpack: Impossible case"
  in
  let gen (name, values) =
    Printf.sprintf "typedef enum {\n  %s\n} %s;\n" (String.concat ",\n  " values) name
  in
  enums
  |> List.map unpack
  |> List.map gen
  |> String.concat ""
  (* failwith "TODO: gen_enums" *)

let gen_multiline_comments c_ctx =
  let comments = multine_comments_of c_ctx in
  let gen = function
    | MultiLineComment(x) -> Printf.sprintf "/*\n  %s\n*/" x
    | _ -> failwith "gen_multiline_comments: Impossible case"
  in
  Printf.sprintf "%s\n" @@ String.concat "\n\n" @@ List.map gen comments

let gen_c c_ctx =
  let includes = gen_includes c_ctx in
  let structs = gen_structs c_ctx in
  let func_decls = gen_func_decl c_ctx in
  let funcs = gen_func_def c_ctx in
  let macros = gen_macros c_ctx in
  let enums = gen_enums c_ctx in
  let init_func = gen_init_func c_ctx in
  let globals = gen_globals c_ctx in
  let multiline_comments = gen_multiline_comments c_ctx in
  String.concat "\n" [ includes
                     ; macros
                     ; enums
                     ; structs
                     ; globals
                     ; multiline_comments
                     ; func_decls
                     ; init_func
                     ; funcs 
                     ]
  (* failwith "TODO: gen_c" *)
