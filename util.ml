let check_unique (type a t) (module M: Set.S with type elt = a and type t = t) l =
  List.length (List.of_seq @@ M.to_seq @@ M.of_list l) = List.length l

let unique (type a t) (module M: Set.S with type elt = a and type t = t) l =
List.of_seq @@ M.(to_seq @@ of_list l)

module StringString = struct 
  type t = (string * string)
  let compare (a, b) (c, d) =
    let v = compare a c in
    if v = 0 then
      compare b d
    else
      v
end

module StringOptionString = struct 
  type t = (string * int option * string)
  let compare (a, _, b) (c, _, d) =
    let v = compare a c in
    if v = 0 then
      compare b d
    else
      v
end

module StringSet = Set.Make(String)
module MapStringString = Map.Make(StringString)
module SetStringString = Set.Make(StringString)
module SetStringOptionString = Set.Make(StringOptionString)

let check_unique_strings = fun l -> check_unique (module StringSet) l
let is_equal v = fun x -> x = v
let is_false = is_equal false
let concat l = List.fold_left (@) [] l
let unique_strings l = unique (module StringSet) l

let unique_naive l =
  let rec loop cur l =
    match l with
    | [] -> cur
    | x :: rest ->
      (match List.find_opt (fun v -> v = x) cur with
      | Some(_) -> loop cur rest
      | None -> loop (x :: cur) rest)
  in
  List.rev (loop [] l)


let is_subset f l1 l2 = let rec loop l =
    match l with
    | [] -> true
    | x :: rest ->
      (match List.find_opt (f x) l2 with
      | Some(_) -> loop rest
      | None -> false)
  in
  loop l1

let sets_equal f l1 l2 = (is_subset f l1 l2) && (is_subset f l2 l1)

let every_element_unique l =
  sets_equal (=) l (unique_naive l)

let intersection a b =
  let rec loop ans l =
    match l with
    | [] -> ans
    | x :: rest ->
      (match List.find_opt (fun y -> y = x) b with
      | Some(_) -> loop (ans @ [x]) rest
      | None -> loop ans rest)
  in
  loop [] a


let rec last_el_of l =
  match l with
  | [] -> None
  | [x] -> Some x
  | _ :: rest -> last_el_of rest

let rec without_last_el l =
  match l with
  | []
  | [_] -> []
  | x :: rest ->
    x :: (without_last_el rest)

let debug s =
  print_endline @@ "DEBUG: " ^ s

let split3 l =
  let rec loop ans l =
    match l with
    | [] -> ans
    | (a, b, c) :: rest ->
      let (a', b', c') = ans in
      loop (a :: a', b :: b', c :: c') rest
  in 
  loop ([], [], []) l

let split4 l =
  let rec loop ans l =
    match l with
    | [] -> ans
    | (a, b, c, d) :: rest ->
      let (a', b', c', d') = ans in
      loop (a :: a', b :: b', c :: c', d :: d') rest
  in 
  loop ([], [], [], []) l

let split5 l =
  let rec loop ans l =
    match l with
    | [] -> ans
    | (a, b, c, d, e) :: rest ->
      let (a', b', c', d', e') = ans in
      loop (a :: a', b :: b', c :: c', d :: d', e :: e') rest
  in 
  loop ([], [], [], [], []) l

let split6 l =
  let rec loop ans l =
    match l with
    | [] -> ans
    | (a, b, c, d, e, f) :: rest ->
      let (a', b', c', d', e', f') = ans in
      loop (a :: a', b :: b', c :: c', d :: d', e :: e', f :: f') rest
  in 
  loop ([], [], [], [], [], []) l
