open Ast

let e e' = Expr(e')

let void = Types.Unit
let str = Types.String
let _int = Types.Int
let _bool = Types.Bool
let _char = Types.Char
let _float = Types.Float

let unit = Unit
let _b b = Bool b
let _c c = Char c
let _i i = Int i
(*
let _i8 i = Int8 i
let _i16 i = Int16 i
let _i32 i = Int32 i
let _i64 i = Int64 i     Can I somehow add those to the ast and type system?
let _u8 i = UInt8 i
let _u16 i = UInt16 i
let _u32 i = UInt32 i
let _u64 i = UInt64 i
*)
let _f f = Float f
let _s s = String (String.escaped s)
let id i = Ident i
let arr a = Array a
let tup a = Tuple a

let cast_to (t : Types.ground_type) e =
  CastTo (t, e)

let neg a = Neg a
let not a = Not a
let ( |+ ) a b = Add (a, b)
let ( |- ) a b = Sub (a, b)
let ( |* ) a b = Mul (a, b)
let ( |/ ) a b = Div (a, b)
let ( |% ) a b = Mod (a, b)

let fneg a = FNeg a
let ( |+. ) a b = FAdd (a, b)
let ( |-. ) a b = FSub (a, b)
let ( |*. ) a b = FMul (a, b)
let ( |/. ) a b = FDiv (a, b)

let ( |== ) a b = Eq (a, b)
let ( |!= ) a b = Neq (a, b)
let ( |< ) a b = Lt (a, b)
let ( |<= ) a b = Le (a, b)
let ( |> ) a b = Gt (a, b)
let ( |>= ) a b = Ge (a, b)

let ( |><| ) a b = LAnd (a, b)
let ( |<>| ) a b = LOr (a, b)

let ( |! ) e = BNeg e
let ( |!& ) a b = BAnd (a, b)
let ( |!| ) a b = BOr (a, b)
let ( |!^ ) a b = BXOr (a, b)
let ( |!<< ) a b = BShiftLeft (a, b)
let ( |!>> ) a b = BShiftRight (a, b)

let ( ! ) a i = IndexAccess (a, i)
let ( => ) a s = MemberAccess (a, s)

let ( |= ) a b = Assign(a, b)

let await e' = Await e'
let await' e' = e @@ Await e'
let raise s e' = Raise(s, e')
let raise' s e' = e @@ Raise(s, e')

let _let name e = Let (name, e)
let _let_mut name e = LetMut(name, e)

let _func t name args body = FuncDef(t, name, args, body)
let toplevel l = l

let promise name (effname, args) body =
  PromiseDef(name, effname, args, body)

let task name body =
  TaskDef(name, body)

let parallel tasks_and_promises =
  ParallelDef tasks_and_promises

let _func_type name args rettype =
  TypecheckFuncDecl(name, args, rettype)

let _func_call name args =
  FuncCall(name, args)

let _func_call' name args =
  e @@ FuncCall(name, args)

let eff name values =
  EffectDef(name, values)

let sumtype name variants =
  SumTypeDef(name, variants)
  
let modetype n variants =
  ModeDef(n, variants)

let mode_tasks n tasks =
  ModeTasksDef(n, tasks)

let start_modes modes =
  StartModesDef(modes)

let switch m =
  ModeSwitch(m)

let interrupt n effname =
  Interrupt(n, effname)

let for_to n a b s =
  ForTo(n, a, b, Block(s))

let for_to' n a b s =
  e @@ ForTo(n, a, b, Block(s))

let return e =
  Return(e)

let global s : Ast.stmt =
  Global(s)

let global_var t a : Ast.toplevel =
  Global(a, t)

let block s = Block(s)

let _if cond t f =
  If(cond, block t, block f)

let _if' cond t f = 
  Expr(_if cond t f)

let (===) a b =
  Eq(a, b)

let (!==) a b =
  Neq(a, b)

let _while c s =
  While(c, block s)

let _while' c s =
  Expr(_while c s)

