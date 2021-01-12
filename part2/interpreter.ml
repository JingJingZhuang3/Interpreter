(* CSE-305 JingJingZhuang *)
type const = Int of int | Bool of bool | Stri of string | Name of string | Unit | Error

let is_int str = 
  try (int_of_string str |> string_of_int) = str
  with Failure _ -> false

(* programs **
com ::= push const | add | sub | mul | div | rem | neg | swap | pop | cat | and | or | not | lessThan | equal | if | bind | let com {com} end | funBind com {com} [ return ] funEnd | call
*)
type com = Push of const | Add | Sub | Mul | Div | Rem | Neg | Swap | Pop | Quit | ToString | Println | Cat | And | Or | Not | LessThan | Equal | If | Bind | Let | End (*| Call*)

let explode str =
  let rec exp x y = 
    if x < 0 then y
    else exp (x - 1) (str.[x]::y) in exp (String.length str - 1) []

(* check is s:char list a simple ASCII*)
let rec simpleASCII s : bool =
  match s with
    hd::tl -> if Char.code hd <= 255
              then simpleASCII tl
              else false
  | _ -> true

(* check is the name start with a digit *)
let check_letter (l : char) : bool =
  if (Char.code l >= 65 && Char.code l <= 90) || (Char.code l >= 97 && Char.code l <= 122)
  then true 
  else false

let check_digit (d : char) : bool =
  if Char.code d >= 48 && Char.code d <= 57
  then true
  else false

(* name = digits or letters or _ *)
let rec check_name (n : char list) : bool =
  match n with
    hd::tl -> if hd = '_' || check_letter hd || check_digit hd
              then check_name tl
              else false
  | [] -> true

(* bool, error, unit = start with : *)
let check_beu (beu : string) : const =
  match beu with
    ":true:" -> Bool(true)
  | ":false:" -> Bool(false)
  | ":error:" -> Error
  | ":unit:" -> Unit
  | _ -> Error

(* identity the const type *)
let identify_const (c : string) : const =
  if c = "-0"
  then Int (0)
  else if is_int c (* check is c an int *)
  then Int(int_of_string c)
  else
    match explode c with
        '\"'::tl -> (match (String.sub c (String.length(c) - 1) 1) with (* check is the string close with quotation mark *)
                    | "\"" -> if simpleASCII (explode (String.sub c 1 (String.length(c)-2))) 
                              then Stri(String.sub c 1 (String.length(c)-2))
                              else Error
                    | _ -> Error)
      | ':'::tl -> check_beu c (* bool, error, unit = start with : *)
      | hd::tl -> (if hd = '_' || check_letter hd
                  then if check_name tl (* name = digits or letters or _ *)
                       then Name(c)
                       else Error
                  else Error)
      | _ -> Error

(* get commands *)
let rec get_com (l_list : string list) : (com list) =
  match l_list with
    [] -> []
  | line::tl -> (let word = String.split_on_char ' ' line in (* split the com in word list *)
                match word with  (* only store the command after 'push' *)
                  "push"::rs -> Push(identify_const (String.sub line 5 (String.length(line)-5)))::get_com tl
                | "add"::rs -> Add::get_com tl
                | "sub"::rs -> Sub::get_com tl
                | "mul"::rs -> Mul::get_com tl
                | "div"::rs -> Div::get_com tl
                | "rem"::rs -> Rem::get_com tl
                | "neg"::rs -> Neg::get_com tl
                | "swap"::rs -> Swap::get_com tl
                | "pop"::rs -> Pop::get_com tl
                | "toString"::rs -> ToString::get_com tl
                | "println"::rs -> Println::get_com tl
                | "cat"::rs -> Cat::get_com tl
                | "and"::rs -> And::get_com tl
                | "or"::rs -> Or::get_com tl
                | "not"::rs -> Not::get_com tl
                | "equal"::rs -> Equal::get_com tl
                | "lessThan"::rs ->LessThan::get_com tl
                | "bind"::rs -> Bind::get_com tl
                | "if"::rs -> If::get_com tl
                | "let"::rs -> Let::get_com tl
                | "end"::rs -> End::get_com tl
                | "quit"::rs -> Quit::get_com tl
                | x::rs -> Push Error::get_com tl
                | _ -> [])

(* check is the name previously bound *)
let rec is_bound ((name : const), (bindlist : const list)) : bool =
  match bindlist with
   x::Name(n)::tl -> (match name with
                        Name(n1) -> if n = n1 then true else is_bound(name, tl)
                      | _ -> is_bound(name, tl))
  | _ -> false

(* find and return the name's value *)
let rec findVal ((name : const), (bindlist : const list)) : const =
  match bindlist with
   x::Name(n)::tl -> (match name with
                        Name(n1) -> if n = n1 then x else findVal(name, tl) 
                     | _ -> findVal (name, tl))
  | _ -> name (* this func call would never reach to else, cuz it would always check is_bound before calling this func*)

(* add stack *)
let com_add ((stack : const list), (bindlist : const list)) : const list =
  match stack with
    Int(x)::Int(y)::tl -> Int(x+y)::tl
  | Name(x)::Name(y)::tl -> if is_bound(Name(x), bindlist) && is_bound(Name(y), bindlist)
                            then let v1 = findVal(Name(x), bindlist) in
                                 let v2 = findVal(Name(y), bindlist) in
                                  match v1 with
                                  Int(x1) -> (match v2 with
                                               Int(y1) -> Int(x1+y1)::tl
                                              | _ -> Error::stack)
                                  | _ -> Error::stack
                            else Error::stack
  | Name(x)::Int(y)::tl -> if is_bound(Name(x), bindlist)
                            then let v1 = findVal(Name(x), bindlist) in
                                  match v1 with
                                  Int(x1) -> Int(x1+y)::tl
                                  | _ -> Error::stack
                            else Error::stack
  | Int(x)::Name(y)::tl -> if is_bound(Name(y), bindlist)
                            then let v1 = findVal(Name(y), bindlist) in
                                  match v1 with
                                  Int(y1) -> Int(x+y1)::tl
                                  | _ -> Error::stack
                            else Error::stack
  | _ -> Error::stack

(* subtract stack *)
let com_sub ((stack : const list), (bindlist : const list)) : const list =
  match stack with
    Int(x)::Int(y)::tl -> Int(y-x)::tl
  | Name(x)::Name(y)::tl -> if is_bound(Name(x), bindlist) && is_bound(Name(y), bindlist)
                            then let v1 = findVal(Name(x), bindlist) in
                                 let v2 = findVal(Name(y), bindlist) in
                                  match v1 with
                                  Int(x1) -> (match v2 with
                                              Int(y1) -> Int(y1-x1)::tl
                                              | _ -> Error::stack)
                                  | _ -> Error::stack
                            else Error::stack
  | Name(x)::Int(y)::tl -> if is_bound(Name(x), bindlist)
                            then let v1 = findVal(Name(x), bindlist) in
                                  match v1 with
                                  Int(x1) -> Int(y-x1)::tl
                                  | _ -> Error::stack
                            else Error::stack
  | Int(x)::Name(y)::tl -> if is_bound(Name(y), bindlist)
                            then let v1 = findVal(Name(y), bindlist) in
                                  match v1 with
                                  Int(y1) -> Int(y1-x)::tl
                                  | _ -> Error::stack
                            else Error::stack
  | _ -> Error::stack

(* multiply stack *)
let com_mul ((stack : const list), (bindlist : const list)) : const list =
  match stack with
    Int(x)::Int(y)::tl -> Int(x*y)::tl
  | Name(x)::Name(y)::tl -> if is_bound(Name(x), bindlist) && is_bound(Name(y), bindlist)
                            then let v1 = findVal(Name(x), bindlist) in
                                 let v2 = findVal(Name(y), bindlist) in
                                  match v1 with
                                  Int(x1) -> (match v2 with
                                              Int(y1) -> Int(x1*y1)::tl
                                              | _ -> Error::stack)
                                  | _ -> Error::stack
                            else Error::stack
  | Name(x)::Int(y)::tl -> if is_bound(Name(x), bindlist)
                            then let v1 = findVal(Name(x), bindlist) in
                                  match v1 with
                                  Int(x1) -> Int(x1*y)::tl
                                  | _ -> Error::stack
                            else Error::stack
  | Int(x)::Name(y)::tl -> if is_bound(Name(y), bindlist)
                            then let v1 = findVal(Name(y), bindlist) in
                                  match v1 with
                                  Int(y1) -> Int(x*y1)::tl
                                  | _ -> Error::stack
                            else Error::stack
  | _ -> Error::stack

(* divide stack *)
let com_div ((stack : const list), (bindlist : const list)) : const list =
  match stack with
    Int(x)::Int(y)::tl -> if x != 0 then Int(y/x)::tl 
                          else Error::stack
  | Name(x)::Name(y)::tl -> if is_bound(Name(x), bindlist) && is_bound(Name(y), bindlist)
                            then let v1 = findVal(Name(x), bindlist) in
                                 let v2 = findVal(Name(y), bindlist) in
                                  match v1 with
                                  Int(x1) -> (match v2 with
                                              Int(y1) -> if x1 != 0 then Int(y1/x1)::tl 
                                                         else Error::stack
                                              | _ -> Error::stack)
                                  | _ -> Error::stack
                            else Error::stack
  | Name(x)::Int(y)::tl -> if is_bound(Name(x), bindlist)
                            then let v1 = findVal(Name(x), bindlist) in
                                  match v1 with
                                  Int(x1) -> if x1 != 0 then Int(y/x1)::tl
                                             else Error::stack
                                  | _ -> Error::stack
                            else Error::stack
  | Int(x)::Name(y)::tl -> if is_bound(Name(y), bindlist)
                            then let v1 = findVal(Name(y), bindlist) in
                                  match v1 with
                                  Int(y1) -> if x != 0 then Int(y1/x)::tl
                                             else Error::stack
                                  | _ -> Error::stack
                            else Error::stack
  | _ -> Error::stack

(* remainder *)
let com_rem ((stack : const list), (bindlist : const list)) : const list =
  match stack with
    Int(x)::Int(y)::tl -> if x != 0 then Int(y mod x)::tl
                          else Error::stack
  | Name(x)::Name(y)::tl -> if is_bound(Name(x), bindlist) && is_bound(Name(y), bindlist)
                            then let v1 = findVal(Name(x), bindlist) in
                                 let v2 = findVal(Name(y), bindlist) in
                                  match v1 with
                                  Int(x1) -> (match v2 with
                                              Int(y1) -> if x1 != 0 then Int(y1 mod x1)::tl
                                                         else Error::stack
                                              | _ -> Error::stack)
                                  | _ -> Error::stack
                            else Error::stack
  | Name(x)::Int(y)::tl -> if is_bound(Name(x), bindlist)
                            then let v1 = findVal(Name(x), bindlist) in
                                  match v1 with
                                  Int(x1) -> if x1 != 0 then Int(y mod x1)::tl
                                             else Error::stack
                                  | _ -> Error::stack
                            else Error::stack
  | Int(x)::Name(y)::tl -> if is_bound(Name(y), bindlist)
                            then let v1 = findVal(Name(y), bindlist) in
                                  match v1 with
                                  Int(y1) -> if x != 0 then Int(y1 mod x)::tl
                                             else Error::stack
                                  | _ -> Error::stack
                            else Error::stack
  | _ -> Error::stack

(* negation *)
let com_neg ((stack : const list), (bindlist : const list)) : const list =
  match stack with
    Int(x)::tl -> Int(x * -1)::tl
  | Name(x)::tl -> if is_bound(Name(x), bindlist)
                   then let v1 = findVal(Name(x), bindlist) in
                    match v1 with
                     Int(x1) -> Int(x1 * -1)::tl
                     | _ -> Error::stack
                    else Error::stack
  | _ -> Error::stack

let com_swap (stack : const list) : const list =
  match stack with
    x::y::tl -> y::x::tl
  | _ -> Error::stack

let com_pop (stack : const list) : const list =
  match stack with
    hd::tk -> tk
  | _ -> Error::stack

let com_cat ((stack : const list), (bindlist : const list)) : const list =
  match stack with
    Stri(x)::Stri(y)::tl -> Stri(y ^ x)::tl
  | Name(x)::Name(y)::tl -> if is_bound(Name(x), bindlist) && is_bound(Name(y), bindlist)
                            then let v1 = findVal(Name(x), bindlist) in
                                 let v2 = findVal(Name(y), bindlist) in
                                  match v1 with
                                  Stri(x1) -> (match v2 with
                                               Stri(y1) -> Stri(y1 ^ x1)::tl
                                              | _ -> Error::stack)
                                  | _ -> Error::stack
                            else Error::stack
  | Name(x)::Stri(y)::tl -> if is_bound(Name(x), bindlist)
                            then let v1 = findVal(Name(x), bindlist) in
                                  match v1 with
                                  Stri(x1) -> Stri(y ^ x1)::tl
                                  | _ -> Error::stack
                            else Error::stack
  | Stri(x)::Name(y)::tl -> if is_bound(Name(y), bindlist)
                            then let v1 = findVal(Name(y), bindlist) in
                                  match v1 with
                                  Stri(y1) -> Stri(y1 ^ x)::tl
                                  | _ -> Error::stack
                            else Error::stack
  | _ -> Error::stack

let com_and ((stack : const list), (bindlist : const list)) : const list =
  match stack with
    Bool(x)::Bool(y)::tl -> if x && y then Bool(true)::tl
                            else Bool(false)::tl
  | Name(x)::Name(y)::tl -> if is_bound(Name(x), bindlist) && is_bound(Name(y), bindlist)
                            then let v1 = findVal(Name(x), bindlist) in
                                 let v2 = findVal(Name(y), bindlist) in
                                  match v1 with
                                  Bool(x1) -> (match v2 with
                                               Bool(y1) -> if x1 && y1 then Bool(true)::tl
                                                          else Bool(false)::tl
                                              | _ -> Error::stack)
                                  | _ -> Error::stack
                            else Error::stack
  | Name(x)::Bool(y)::tl -> if is_bound(Name(x), bindlist)
                            then let v1 = findVal(Name(x), bindlist) in
                                  match v1 with
                                  Bool(x1) -> if x1 && y then Bool(true)::tl
                                             else Bool(false)::tl
                                  | _ -> Error::stack
                            else Error::stack
  | Bool(x)::Name(y)::tl -> if is_bound(Name(y), bindlist)
                            then let v1 = findVal(Name(y), bindlist) in
                                  match v1 with
                                  Bool(y1) -> if x && y1 then Bool(true)::tl
                                             else Bool(false)::tl
                                  | _ -> Error::stack
                            else Error::stack
  | _ -> Error::stack

let com_or ((stack : const list), (bindlist : const list)) : const list =
  match stack with
    Bool(x)::Bool(y)::tl -> if x || y then Bool(true)::tl
                            else Bool(false)::tl
  | Name(x)::Name(y)::tl -> if is_bound(Name(x), bindlist) && is_bound(Name(y), bindlist)
                            then let v1 = findVal(Name(x), bindlist) in
                                 let v2 = findVal(Name(y), bindlist) in
                                  match v1 with
                                  Bool(x1) -> (match v2 with
                                               Bool(y1) -> if x1 || y1 then Bool(true)::tl
                                                          else Bool(false)::tl
                                              | _ -> Error::stack)
                                  | _ -> Error::stack
                            else Error::stack
  | Name(x)::Bool(y)::tl -> if is_bound(Name(x), bindlist)
                            then let v1 = findVal(Name(x), bindlist) in
                                  match v1 with
                                  Bool(x1) -> if x1 || y then Bool(true)::tl
                                             else Bool(false)::tl
                                  | _ -> Error::stack
                            else Error::stack
  | Bool(x)::Name(y)::tl -> if is_bound(Name(y), bindlist)
                            then let v1 = findVal(Name(y), bindlist) in
                                  match v1 with
                                  Bool(y1) -> if x || y1 then Bool(true)::tl
                                             else Bool(false)::tl
                                  | _ -> Error::stack
                            else Error::stack
  | _ -> Error::stack

let com_not ((stack : const list), (bindlist : const list)) : const list =
  match stack with
    Bool(x)::tl -> if not x then Bool(true)::tl
                   else Bool(false)::tl
  | Name(x)::tl -> if is_bound(Name(x), bindlist)
                   then let v1 = findVal(Name(x), bindlist) in
                    match v1 with
                     Bool(x1) -> if not x1 then Bool(true)::tl
                                 else Bool(false)::tl
                     | _ -> Error::stack
                    else Error::stack
  | _ -> Error::stack

let com_eq ((stack : const list), (bindlist : const list)) : const list =
  match stack with
    Int(x)::Int(y)::tl -> if x==y then Bool(true)::tl
                    else Bool(false)::tl
  | Name(x)::Name(y)::tl -> if is_bound(Name(x), bindlist) && is_bound(Name(y), bindlist)
                            then let v1 = findVal(Name(x), bindlist) in
                                 let v2 = findVal(Name(y), bindlist) in
                                  match v1 with
                                  Int(x1) -> (match v2 with
                                              Int(y1) -> if x1==y1 then Bool(true)::tl
                                                         else Bool(false)::tl
                                              | _ -> Error::stack)
                                  | _ -> Error::stack
                            else Error::stack
  | Name(x)::Int(y)::tl -> if is_bound(Name(x), bindlist)
                            then let v1 = findVal(Name(x), bindlist) in
                                  match v1 with
                                  Int(x1) -> if x1 == y then Bool(true)::tl
                                             else Bool(false)::tl
                                  | _ -> Error::stack
                            else Error::stack
  | Int(x)::Name(y)::tl -> if is_bound(Name(y), bindlist)
                            then let v1 = findVal(Name(y), bindlist) in
                                  match v1 with
                                  Int(y1) -> if x == y1 then Bool(true)::tl
                                             else Bool(false)::tl
                                  | _ -> Error::stack
                            else Error::stack
  | _ -> Error::stack

let com_less ((stack : const list), (bindlist : const list)) : const list =
  match stack with
    Int(x)::Int(y)::tl -> if x > y then Bool(true)::tl
                          else Bool(false)::tl
  | Name(x)::Name(y)::tl -> if is_bound(Name(x), bindlist) && is_bound(Name(y), bindlist)
                            then let v1 = findVal(Name(x), bindlist) in
                                 let v2 = findVal(Name(y), bindlist) in
                                  match v1 with
                                  Int(x1) -> (match v2 with
                                              Int(y1) -> if x1 > y1 then Bool(true)::tl
                                                         else Bool(false)::tl
                                              | _ -> Error::stack)
                                  | _ -> Error::stack
                            else Error::stack
  | Name(x)::Int(y)::tl -> if is_bound(Name(x), bindlist)
                            then let v1 = findVal(Name(x), bindlist) in
                                  match v1 with
                                  Int(x1) -> if x1 > y then Bool(true)::tl
                                             else Bool(false)::tl
                                  | _ -> Error::stack
                            else Error::stack
  | Int(x)::Name(y)::tl -> if is_bound(Name(y), bindlist)
                            then let v1 = findVal(Name(y), bindlist) in
                                  match v1 with
                                  Int(y1) -> if x > y1 then Bool(true)::tl
                                             else Bool(false)::tl
                                  | _ -> Error::stack
                            else Error::stack
  | _ -> Error::stack

let com_if ((stack : const list), (bindlist : const list)) : const list =
  match stack with
   x::y::Bool(z)::tl -> if z then x::tl else y::tl
  | x::y::Name(z)::tl -> if is_bound(Name(z), bindlist)
                          then let v = findVal(Name(z), bindlist) in
                              match v with
                               Bool(z1) -> if z1 then x::tl else y::tl
                              | _ -> Error::stack
                          else Error::stack
  | _ -> Error::stack

let com_bind ((stack : const list), (bindlist : const list)) : const list =
  match stack with
    Int(v)::Name(n)::tl -> Unit::tl
  | Stri(s)::Name(n)::tl -> Unit::tl
  | Bool(b)::Name(n)::tl -> Unit::tl
  | Unit::Name(n)::tl -> Unit::tl
  | Name(x)::Name(n)::tl -> if is_bound (Name(x), bindlist) then Unit::tl
                            else Error::stack
  | _ -> Error::stack

let rec restore_bind ((value : const), (name : const), (bindlist : const list), (newbind : const list)) : const list =
  match bindlist with
   x::Name(n)::tl -> (match name with
                      Name(n1) -> if n = n1 then value::Name(n)::newbind @ tl else restore_bind (value, name, tl, x::Name(n)::newbind)
                     | _ -> restore_bind (value, name, tl, x::Name(n)::newbind))
  | _ -> bindlist

let store_bind ((stack : const list), (bindlist : const list)) : const list =
  match stack with
    Int(v)::Name(n)::tl -> if is_bound(Name(n), bindlist)
                           then restore_bind(Int(v), Name(n), bindlist, [])
                           else Int(v)::Name(n)::bindlist
  | Stri(s)::Name(n)::tl -> if is_bound(Name(n), bindlist)
                            then restore_bind(Stri(s), Name(n), bindlist, [])
                            else Stri(s)::Name(n)::bindlist
  | Bool(b)::Name(n)::tl -> if is_bound(Name(n), bindlist)
                           then restore_bind(Bool(b), Name(n), bindlist, [])
                           else Bool(b)::Name(n)::bindlist
  | Unit::Name(n)::tl -> if is_bound(Name(n), bindlist)
                           then restore_bind(Unit, Name(n), bindlist, [])
                           else Unit::Name(n)::bindlist
  | Name(x)::Name(n)::tl -> if is_bound(Name(x), bindlist)
                            then if is_bound(Name(n), bindlist)
                                 then restore_bind(findVal(Name(x), bindlist), Name(n), bindlist, [])
                                 else findVal(Name(x), bindlist)::Name(n)::bindlist
                            else bindlist
  | _ -> bindlist

let printString (c : const) : string =
  match c with
    Int(i) -> Printf.sprintf "%d" i
  | Bool(b) -> ":" ^ string_of_bool b ^":"
  | Stri(s) -> s
  | Name(n) -> n
  | Unit -> ":unit:"
  | Error -> ":error:"

let toString (stack : const list) : const list =
  match stack with
    Int(i)::tl -> Stri(string_of_int i)::tl
  | Bool(b)::tl -> Stri(":" ^ string_of_bool b ^":")::tl
  | Stri(s)::tl -> Stri(s)::tl
  | Name(n)::tl -> Stri(n)::tl
  | Unit::tl -> Stri(":unit:")::tl
  | Error::tl -> Stri(":error:")::tl
  | _ -> stack

let rec check_end (com_list : com list) : bool =
  match com_list with
    hd::tl-> (match hd with
              End -> false
              | Quit -> true
              | _ -> check_end tl)
  | [] -> true

let rec find_end2 (com_list : com list) : com list =
  match com_list with
    hd::tl -> (match hd with
               Let -> find_end2(find_end2 tl)
              | End -> if check_end tl then End::tl else tl
              | _ -> find_end2 tl)
  | [] -> com_list

let rec find_end (com_list : com list) : com list =
  match com_list with
    hd::tl -> (match hd with
               Let -> find_end(find_end tl)
              | End -> tl
              | _ -> find_end tl)
  | [] -> com_list

let ret_letend (lestack : const list) : const list =
  match lestack with
   x::tl -> x::[]
  | _ -> Error::[]

let rec get_letend ((com_list : com list), (stack : const list), (bindlist : const list)) : const list =
  match com_list with
    hd::tl -> (match hd with
                Push(cst) -> get_letend (tl, cst::stack, bindlist)
              | Add -> get_letend (tl, com_add (stack, bindlist), bindlist)
              | Sub -> get_letend (tl, com_sub (stack, bindlist), bindlist)
              | Mul -> get_letend (tl, com_mul (stack, bindlist), bindlist)
              | Div -> get_letend (tl, com_div (stack, bindlist), bindlist)
              | Rem -> get_letend (tl, com_rem (stack, bindlist), bindlist)
              | Neg -> get_letend (tl, com_neg (stack, bindlist), bindlist)
              | Swap -> get_letend (tl, com_swap stack, bindlist)
              | Pop -> get_letend (tl, com_pop stack, bindlist)
              | Cat -> get_letend (tl, com_cat (stack, bindlist), bindlist)
              | And -> get_letend(tl, com_and (stack, bindlist), bindlist)
              | Or -> get_letend(tl, com_or (stack, bindlist), bindlist)
              | Not -> get_letend(tl, com_not (stack, bindlist), bindlist)
              | Equal -> get_letend(tl, com_eq (stack, bindlist), bindlist)
              | LessThan -> get_letend(tl, com_less (stack, bindlist), bindlist)
              | Bind -> get_letend(tl, com_bind (stack, bindlist), store_bind (stack, bindlist))
              | If -> get_letend(tl, com_if (stack, bindlist), bindlist)
              | Let -> get_letend(find_end2 tl, get_letend(tl, stack, bindlist)@stack, bindlist)
              | End -> ret_letend stack
              | ToString -> get_letend (tl, toString stack, bindlist)
              | Println -> get_letend (tl, stack, bindlist)
              | Quit -> ret_letend stack)
  | [] -> ret_letend stack

let rec get_stack ((com_list : com list), (stack : const list), (bindlist : const list), (lestack : const list), (print_stack : const list)) : const list =
  match com_list with
    hd::tl -> (match hd with
                Push(cst) -> get_stack (tl, cst::stack, bindlist, lestack, print_stack)
              | Add -> get_stack (tl, com_add (stack, bindlist), bindlist, lestack, print_stack)
              | Sub -> get_stack (tl, com_sub (stack, bindlist), bindlist, lestack, print_stack)
              | Mul -> get_stack (tl, com_mul (stack, bindlist), bindlist, lestack, print_stack)
              | Div -> get_stack (tl, com_div (stack, bindlist), bindlist, lestack, print_stack)
              | Rem -> get_stack (tl, com_rem (stack, bindlist), bindlist, lestack, print_stack)
              | Neg -> get_stack (tl, com_neg (stack, bindlist), bindlist, lestack, print_stack)
              | Swap -> get_stack (tl, com_swap stack, bindlist, lestack, print_stack)
              | Pop -> get_stack (tl, com_pop stack, bindlist, lestack, print_stack)
              | Cat -> get_stack(tl, com_cat (stack, bindlist), bindlist, lestack, print_stack)
              | And -> get_stack(tl, com_and (stack, bindlist), bindlist, lestack, print_stack)
              | Or -> get_stack(tl, com_or (stack, bindlist), bindlist, lestack, print_stack)
              | Not -> get_stack(tl, com_not (stack, bindlist), bindlist, lestack, print_stack)
              | Equal -> get_stack(tl, com_eq (stack, bindlist), bindlist, lestack, print_stack)
              | LessThan -> get_stack(tl, com_less (stack, bindlist), bindlist, lestack, print_stack)
              | Bind -> get_stack(tl, com_bind (stack, bindlist), store_bind (stack, bindlist), lestack, print_stack)
              | If -> get_stack(tl, com_if (stack, bindlist), bindlist, lestack, print_stack)
              | Let -> get_stack(End::(find_end tl), stack, bindlist, get_letend(tl, lestack, bindlist), print_stack)
              | End -> get_stack(tl, lestack@stack, bindlist, [], print_stack)
              | ToString -> get_stack (tl, toString stack, bindlist, lestack, print_stack)
              | Println -> (match stack with
                             v::rs -> (match v with
                                        Stri(x) -> get_stack (tl, rs, bindlist, lestack, Stri(x)::print_stack)
                                      | _ -> get_stack (tl, rs, bindlist, lestack, Error::print_stack))
                            | [] -> Error::print_stack)
              | Quit -> print_stack) (* return stack *)
  | [] -> print_stack

(************************** main function **************************)
let interpreter ((input : string), (output : string)) : unit =
  (*read and write file*)
  let ic = open_in input in
  let oc = open_out output in
  let rec loop_read acc =
    try
      let l = String.trim(input_line ic) in loop_read (l::acc) (*store lines in acc list*)
    with
    | End_of_file -> List.rev acc in
  
  let file_write str = Printf.fprintf oc "%s\n" str in
  let input_list = loop_read [] in
  let coms = get_com(input_list) in (* convert input_list to command list *)
  let stack = get_stack (coms, [], [], [], []) in  (* return stack *)
  let stack = List.rev stack in
  (*....... pending .........*)
  let print stack =
    file_write (printString stack) in
  List.iter print stack;;
(*interpreter ("input7.txt", "output7.txt")*)