(* CSE-305 JingJingZhuang *)
type const = Int of int | Bool of bool | Stri of string | Name of string | Unit | Error | FunName of string * com
(* programs **
com ::= push const | add | sub | mul | div | rem | neg | swap | pop | cat | and | or | not | lessThan | equal | if | bind | let com {com} end | funBind com {com} [ return ] funEnd | call
*)
and name = string * string
and funBind = Fun of name | InOutFun of name
and com = Push of const | Add | Sub | Mul | Div | Rem | Neg | Swap | Pop | Quit | ToString | Println | Cat | And | Or | Not | LessThan | Equal | If | Bind | Let | End | FunBind of funBind * com list * const list | Return | FunEnd | Call

let is_int str = 
  try (int_of_string str |> string_of_int) = str
  with Failure _ -> false

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

let rec check_funcname (c : string) : bool =
 (*compare command name*)
  match explode c with
   hd::tl -> (if hd = '_' || check_letter hd
                  then if check_name tl(* name = digits or letters or _ *)
                       then true
                       else false
                  else true)
  | _ -> false

(* this check is the name be any of the keywords in our language *)
let rec check_funcname2 ((c : string), (names : string list)) : bool =
  match names with
   n::rs -> if c = n then false else check_funcname2 (c, rs)
  | _ -> true

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

let rec check_funend (l_list : string list) : bool =
  match l_list with
    [] -> true
  | line::tl -> (let word = String.split_on_char ' ' line in (* split the com in word list *)
                match word with  (* only store the command after 'push' *)
                 "funEnd"::rs -> false
                | "quit"::rs -> true
                | _ -> check_funend tl)

let rec get_tl (l_list : string list) : (string list) =
  match l_list with
    [] -> l_list
  | line::tl -> (let word = String.split_on_char ' ' line in (* split the com in word list *)
                match word with  (* only store the command after 'push' *)
                  "fun"::rs -> get_tl (get_tl tl)
                | "inOutFun"::rs -> get_tl (get_tl tl)
                | "funEnd"::rs -> if check_funend tl then l_list else tl
                | _ -> get_tl tl)

let rec get_tl2 (l_list : string list) : (string list) =
  match l_list with
    [] -> l_list
  | line::tl -> (let word = String.split_on_char ' ' line in (* split the com in word list *)
                match word with  (* only store the command after 'push' *)
                  "fun"::rs -> get_tl2 (get_tl2 tl)
                | "inOutFun"::rs -> get_tl2 (get_tl2 tl)
                | "funEnd"::rs -> tl
                | _ -> get_tl2 tl)

(* store function commands *)
let rec store_fun (l_list : string list) : (com list) =
  match l_list with
    [] -> []
  | line::tl -> (let word = String.split_on_char ' ' line in (* split the com in word list *)
                match word with  (* only store the command after 'push' *)
                  "push"::rs -> Push(identify_const (String.sub line 5 (String.length(line)-5)))::store_fun tl
                | "add"::rs -> Add::store_fun tl
                | "sub"::rs -> Sub::store_fun tl
                | "mul"::rs -> Mul::store_fun tl
                | "div"::rs -> Div::store_fun tl
                | "rem"::rs -> Rem::store_fun tl
                | "neg"::rs -> Neg::store_fun tl
                | "swap"::rs -> Swap::store_fun tl
                | "pop"::rs -> Pop::store_fun tl
                | "toString"::rs -> ToString::store_fun tl
                | "println"::rs -> Println::store_fun tl
                | "cat"::rs -> Cat::store_fun tl
                | "and"::rs -> And::store_fun tl
                | "or"::rs -> Or::store_fun tl
                | "not"::rs -> Not::store_fun tl
                | "equal"::rs -> Equal::store_fun tl
                | "lessThan"::rs ->LessThan::store_fun tl
                | "bind"::rs -> Bind::store_fun tl
                | "if"::rs -> If::store_fun tl
                | "let"::rs -> Let::store_fun tl
                | "fun"::n1::n2::rs -> let names = ["push"; "add"; "sub"; "mul"; "div"; "rem"; "neg"; "swap"; "pop"; "toSrting"; "println"; "cat"; "and"; "or"; "not"; "equal"; "lessThan"; "bind"; "if"; "let"; "fun"; "funEnd"; "call"; "end"; "quit"] in
                                       if check_funcname n1 && check_funcname n2 && check_funcname2 (n1, names) && check_funcname2 (n2, names) && not (n1 = n2) 
                                       then FunBind(Fun(n1, n2), store_fun tl, [])::store_fun (get_tl tl)
                                       else Push Error::store_fun tl
                | "inOutFun"::n1::n2::rs -> let names = ["push"; "add"; "sub"; "mul"; "div"; "rem"; "neg"; "swap"; "pop"; "toSrting"; "println"; "cat"; "and"; "or"; "not"; "equal"; "lessThan"; "bind"; "if"; "let"; "fun"; "funEnd"; "call"; "end"; "quit"] in
                                       if check_funcname n1 && check_funcname n2 && check_funcname2 (n1, names) && check_funcname2 (n2, names) && not (n1 = n2) 
                                       then FunBind(InOutFun(n1, n2), store_fun tl, [])::store_fun (get_tl tl)
                                       else Push Error::store_fun tl
                | "return"::rs -> Return::store_fun tl
                | "funEnd"::rs -> []
                | "call"::rs -> Call::store_fun tl
                | "end"::rs -> End::store_fun tl
                | "quit"::rs -> Quit::store_fun tl
                | x::rs -> Push Error::store_fun tl
                | _ -> [])

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
                | "fun"::n1::n2::rs -> let names = ["push"; "add"; "sub"; "mul"; "div"; "rem"; "neg"; "swap"; "pop"; "toSrting"; "println"; "cat"; "and"; "or"; "not"; "equal"; "lessThan"; "bind"; "if"; "let"; "fun"; "funEnd"; "call"; "end"; "quit"] in
                                       if check_funcname n1 && check_funcname n2 && check_funcname2 (n1, names) && check_funcname2 (n2, names) && not (n1 = n2) 
                                       then FunBind(Fun(n1, n2), store_fun tl, [])::(get_com ("funEnd"::get_tl2 tl))
                                       else Push Error::get_com tl
                | "inOutFun"::n1::n2::rs -> let names = ["push"; "add"; "sub"; "mul"; "div"; "rem"; "neg"; "swap"; "pop"; "toSrting"; "println"; "cat"; "and"; "or"; "not"; "equal"; "lessThan"; "bind"; "if"; "let"; "fun"; "funEnd"; "call"; "end"; "quit"] in
                                       if check_funcname n1 && check_funcname n2 && check_funcname2 (n1, names) && check_funcname2 (n2, names) && not (n1 = n2) 
                                       then FunBind(InOutFun(n1, n2), store_fun tl, [])::(get_com ("funEnd"::get_tl2 tl))
                                       else Push Error::get_com tl
                | "return"::rs -> Return::get_com tl
                | "funEnd"::rs -> FunEnd::get_com tl
                | "call"::rs -> Call::get_com tl
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

let rec is_func ((name : const), (funclist : com list)) : bool =
  match funclist with
   FunBind(Fun(n1,n2), c, b)::tl -> (match name with
                                      Name(arg) -> if arg = n1 then true else is_func(name, tl)
                                    | _ -> is_func(name, tl))
  | FunBind(InOutFun(n1,n2), c, b)::tl -> (match name with
                                      Name(arg) -> if arg = n1 then true else is_func(name, tl)
                                    | _ -> is_func(name, tl))
  | _ -> false

let rec get_funcbind ((name : const), (funclist : com list)) : com =
  match funclist with
  | FunBind(Fun(n1,n2), c, b)::tl -> (match name with
                                      Name(arg) -> if arg = n1 then FunBind(Fun(n1,n2), c, b) else get_funcbind(name, tl)
                                    | _ -> get_funcbind(name, tl))
  | FunBind(InOutFun(n1,n2), c, b)::tl -> (match name with
                                      Name(arg) -> if arg = n1 then FunBind(Fun(n1,n2), c, b) else get_funcbind(name, tl)
                                    | _ -> get_funcbind(name, tl))
  | _ -> Push(Error)

let rec test (funclist : com list) : const list =
  match funclist with
   FunBind(Fun(n1,n2), c, b)::tl -> Stri("-> func: "^n1)::test tl
  | FunBind(InOutFun(n1,n2), c, b)::tl -> Stri("-> func: "^n1)::test tl
  | _ -> []

(* find and return the name's value in bindlist *)
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

let com_bind ((stack : const list), (bindlist : const list), (funclist : com list)) : const list =
  match stack with
    Int(v)::Name(n)::tl -> Unit::tl
  | Stri(s)::Name(n)::tl -> Unit::tl
  | Bool(b)::Name(n)::tl -> Unit::tl
  | Unit::Name(n)::tl -> Unit::tl
  | Name(x)::Name(n)::tl -> if is_bound (Name(x), bindlist) || is_func (Name(x), funclist) then Unit::tl
                            else Error::stack
  | FunName(x,y)::Name(n)::tl -> Unit::tl
  | _ -> Error::stack

let rec restore_bind ((value : const), (name : const), (bindlist : const list), (newbind : const list)) : const list =
  match bindlist with
   x::Name(n)::tl -> (match name with
                      Name(n1) -> if n = n1 then value::Name(n)::newbind @ tl else restore_bind (value, name, tl, x::Name(n)::newbind)
                     | _ -> restore_bind (value, name, tl, x::Name(n)::newbind))
  | _ -> bindlist

let store_bind ((stack : const list), (bindlist : const list), (funclist : com list)) : const list =
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
                            else if is_func(Name(x), funclist) 
                                 then Name(x)::Name(n)::bindlist
                                 else bindlist
  | FunName(x,y)::Name(n)::tl -> if is_bound(Name(n), bindlist)
                                 then restore_bind(FunName(x,y), Name(n), bindlist, [])
                                 else FunName(x,y)::Name(n)::bindlist
  | _ -> bindlist

let printString (c : const) : string =
  match c with
    Int(i) -> Printf.sprintf "%d" i
  | Bool(b) -> ":" ^ string_of_bool b ^":"
  | Stri(s) -> s
  | Name(n) -> n
  | Unit -> ":unit:"
  | Error -> ":error:"
  | FunName(x,y) -> x

let toString (stack : const list) : const list =
  match stack with
    Int(i)::tl -> Stri(string_of_int i)::tl
  | Bool(b)::tl -> Stri(":" ^ string_of_bool b ^":")::tl
  | Stri(s)::tl -> Stri(s)::tl
  | Name(n)::tl -> Stri(n)::tl
  | Unit::tl -> Stri(":unit:")::tl
  | Error::tl -> Stri(":error:")::tl
  | FunName(x,y)::tl -> Stri(x)::tl
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

let rec func_ret ((stack : const list), (bindlist : const list), (ret : const list)) : const list =
  match stack with
   Name(x)::rs -> func_ret (rs, bindlist, findVal (Name(x), bindlist)::ret)
  | x::rs -> func_ret (rs, bindlist, x::ret)
  | _ -> ret

let get_arg (stack : const list) : const =
  match stack with
   arg::fn::tl -> arg
  | _ -> Error

let rec is_inout2 ((name : const), (funclist : com list)) : bool =
  match funclist with
    FunBind(Fun(n1,n2), c, b)::tl -> is_inout2(name, tl)
  | FunBind(InOutFun(n1,n2), c, b)::tl -> (match name with
                                      Name(arg) -> if arg = n1 then true else is_inout2(name, tl)
                                    | _ -> is_inout2(name, tl))
  | _ -> false

let is_inout ((stack : const list) , (bindlist : const list), (func_list : com list)) : bool =
  match stack with
   arg::Name(fn)::tl -> if is_bound (Name(fn), bindlist) then
                        let fn1 = findVal(Name(fn), bindlist) in
                        match fn1 with
                         FunName(x, y) -> (match y with
                                       | FunBind(InOutFun(n1,n2), c, b) -> true
                                       | _ -> false )
                        | Name(x) -> if is_inout2 (Name(x), func_list) then true else false
                        | _ -> false 
                         else if is_inout2 (Name(fn), func_list) then true else false
  | _ -> false

let rec get_c2 ((name : const), (funclist : com list)) : com list =
  match funclist with
  | FunBind(InOutFun(n1,n2), c, b)::tl -> (match name with
                                      Name(arg) -> if arg = n1 then c else get_c2(name, tl)
                                    | _ -> get_c2 (name, tl))
  | _ -> []

let get_c ((stack : const list) , (bindlist : const list), (func_list : com list)) : com list =
  match stack with
   arg::Name(fn)::tl -> if is_bound (Name(fn), bindlist) then
                        let fn1 = findVal(Name(fn), bindlist) in
                        match fn1 with
                         FunName(x, y) -> (match y with
                                       | FunBind(InOutFun(n1,n2), c, b) -> c
                                       | _ -> [] )
                        | Name(x) -> get_c2 (Name(x), func_list)
                        | _ -> [] 
                         else get_c2 (Name(fn), func_list)
  | _ -> []

let rec has_ret (c : com list) : bool =
  match c with
  | [] -> false
  | Return::tl -> true
  | hd::tl -> has_ret tl

let rec get_letend ((com_list : com list), (stack : const list), (bindlist : const list), (func_list : com list) ,(argname : string), (functype : string)) : const list =
  match com_list with
    hd::tl -> (match hd with
                Push(cst) -> get_letend (tl, cst::stack, bindlist, func_list, argname, functype)
              | Add -> get_letend (tl, com_add (stack, bindlist), bindlist, func_list, argname, functype)
              | Sub -> get_letend (tl, com_sub (stack, bindlist), bindlist, func_list, argname, functype)
              | Mul -> get_letend (tl, com_mul (stack, bindlist), bindlist, func_list, argname, functype)
              | Div -> get_letend (tl, com_div (stack, bindlist), bindlist, func_list, argname, functype)
              | Rem -> get_letend (tl, com_rem (stack, bindlist), bindlist, func_list, argname, functype)
              | Neg -> get_letend (tl, com_neg (stack, bindlist), bindlist, func_list, argname, functype)
              | Swap -> get_letend (tl, com_swap stack, bindlist, func_list, argname, functype)
              | Pop -> get_letend (tl, com_pop stack, bindlist, func_list, argname, functype)
              | Cat -> get_letend (tl, com_cat (stack, bindlist), bindlist, func_list, argname, functype)
              | And -> get_letend(tl, com_and (stack, bindlist), bindlist, func_list, argname, functype)
              | Or -> get_letend(tl, com_or (stack, bindlist), bindlist, func_list, argname, functype)
              | Not -> get_letend(tl, com_not (stack, bindlist), bindlist, func_list, argname, functype)
              | Equal -> get_letend(tl, com_eq (stack, bindlist), bindlist, func_list, argname, functype)
              | LessThan -> get_letend(tl, com_less (stack, bindlist), bindlist, func_list, argname, functype)
              | Bind -> get_letend(tl, com_bind (stack, bindlist, func_list), store_bind (stack, bindlist, func_list), func_list, argname, functype)
              | If -> get_letend(tl, com_if (stack, bindlist), bindlist, func_list, argname, functype)
              | Let -> get_letend(find_end2 tl, get_letend(tl, stack, bindlist, func_list, argname, functype)@stack, bindlist, func_list, argname, functype)
              | End -> ret_letend stack
              | FunBind(Fun(n1,n2), c, b) -> get_letend (tl, stack, bindlist, FunBind(Fun(n1,n2), c, bindlist)::func_list, argname, functype)
              | FunBind(InOutFun(n1,n2), c, b) -> get_letend (tl, stack, bindlist, FunBind(InOutFun(n1,n2), c, bindlist)::func_list, argname, functype)
              | Return -> if functype = "inout" then
                            (match stack with(*match List.rev (func_ret (stack, bindlist, [])) with*)
                            Name(x)::tail -> if is_func(Name(x), func_list)
                                              then findVal(Name(argname), bindlist)::FunName(x, get_funcbind (Name(x), func_list))::[]
                                              else findVal(Name(argname), bindlist)::findVal(Name(x), bindlist)::[]
                            | retval::tail -> findVal(Name(argname), bindlist)::retval::[]
                            | _ -> [])
                          else (match stack with(*match List.rev (func_ret (stack, bindlist, [])) with*)
                            Name(x)::tail -> if is_func(Name(x), func_list)
                                              then FunName(x, get_funcbind (Name(x), func_list))::[]
                                              else findVal(Name(x), bindlist)::[]
                            | retval::tail -> retval::[]
                            | _ -> [])
              | FunEnd -> get_letend (tl, Unit::stack, bindlist, func_list, argname, functype)
              | Call ->  let retstack = com_call (stack, bindlist, func_list, func_list) in
                          if is_inout (stack, bindlist, func_list) then
                            if has_ret (get_c (stack, bindlist, func_list)) then
                              (match retstack with
                              he::xx -> (let getarg = get_arg stack in
                                        match getarg with
                                         Name(k) -> get_letend (tl, retstack, store_bind (he::getarg::[], bindlist, func_list), func_list, argname, functype)
                                        | _ -> get_letend (tl, retstack, bindlist, func_list, argname, functype))
                              | _-> [])
                            else (match retstack with
                              he::xx -> (let getarg = get_arg stack in
                                        match getarg with
                                         Name(k) -> get_letend (tl, xx, store_bind (he::getarg::[], bindlist, func_list), func_list, argname, functype)
                                        | _ -> get_letend (tl, xx, bindlist, func_list, argname, functype))
                              | _-> [])
                          else get_letend (tl, retstack, bindlist, func_list, argname, functype)
              | ToString -> get_letend (tl, toString stack, bindlist, func_list, argname, functype)
              | Println -> get_letend (tl, stack, bindlist, func_list, argname, functype)
              | Quit -> ret_letend stack)
  | [] -> findVal(Name(argname), bindlist)::[]

and get_stack ((com_list : com list), (stack : const list), (bindlist : const list), (lestack : const list), (print_stack : const list), (func_list : com list), (argname : string), (functype : string)) : const list =
  match com_list with
    hd::tl -> (match hd with
                Push(cst) -> get_stack (tl, cst::stack, bindlist, lestack, print_stack, func_list, argname, functype)
              | Add -> get_stack (tl, com_add (stack, bindlist), bindlist, lestack, print_stack, func_list, argname, functype)
              | Sub -> get_stack (tl, com_sub (stack, bindlist), bindlist, lestack, print_stack, func_list, argname, functype)
              | Mul -> get_stack (tl, com_mul (stack, bindlist), bindlist, lestack, print_stack, func_list, argname, functype)
              | Div -> get_stack (tl, com_div (stack, bindlist), bindlist, lestack, print_stack, func_list, argname, functype)
              | Rem -> get_stack (tl, com_rem (stack, bindlist), bindlist, lestack, print_stack, func_list, argname, functype)
              | Neg -> get_stack (tl, com_neg (stack, bindlist), bindlist, lestack, print_stack, func_list, argname, functype)
              | Swap -> get_stack (tl, com_swap stack, bindlist, lestack, print_stack, func_list, argname, functype)
              | Pop -> get_stack (tl, com_pop stack, bindlist, lestack, print_stack, func_list, argname, functype)
              | Cat -> get_stack(tl, com_cat (stack, bindlist), bindlist, lestack, print_stack, func_list, argname, functype)
              | And -> get_stack(tl, com_and (stack, bindlist), bindlist, lestack, print_stack, func_list, argname, functype)
              | Or -> get_stack(tl, com_or (stack, bindlist), bindlist, lestack, print_stack, func_list, argname, functype)
              | Not -> get_stack(tl, com_not (stack, bindlist), bindlist, lestack, print_stack, func_list, argname, functype)
              | Equal -> get_stack(tl, com_eq (stack, bindlist), bindlist, lestack, print_stack, func_list, argname, functype)
              | LessThan -> get_stack(tl, com_less (stack, bindlist), bindlist, lestack, print_stack, func_list, argname, functype)
              | Bind -> get_stack(tl, com_bind (stack, bindlist, func_list), store_bind (stack, bindlist, func_list), lestack, print_stack, func_list, argname, functype)
              | If -> get_stack(tl, com_if (stack, bindlist), bindlist, lestack, print_stack, func_list, argname, functype)
              | Let -> get_stack(End::(find_end tl), stack, bindlist, get_letend(tl, lestack, bindlist, func_list, argname, functype), print_stack, func_list, argname, functype)
              | End -> get_stack(tl, lestack@stack, bindlist, [], print_stack, func_list, argname, functype)
              | FunBind(Fun(n1,n2), c, b) -> get_stack (tl, stack, bindlist, lestack, print_stack, FunBind(Fun(n1,n2), c, bindlist)::func_list, argname, functype)
              | FunBind(InOutFun(n1,n2), c, b) -> get_stack (tl, stack, bindlist, lestack, print_stack, FunBind(InOutFun(n1,n2), c, bindlist)::func_list, argname, functype)
              | Return -> if functype = "inout" then
                            (match stack with(*match List.rev (func_ret (stack, bindlist, [])) with*)
                            Name(x)::tail -> if is_func(Name(x), func_list)
                                              then findVal(Name(argname), bindlist)::FunName(x, get_funcbind (Name(x), func_list))::[]
                                              else findVal(Name(argname), bindlist)::findVal(Name(x), bindlist)::[]
                            | retval::tail -> findVal(Name(argname), bindlist)::retval::[]
                            | _ -> [])
                          else (match stack with(*match List.rev (func_ret (stack, bindlist, [])) with*)
                            Name(x)::tail -> if is_func(Name(x), func_list)
                                              then FunName(x, get_funcbind (Name(x), func_list))::[]
                                              else findVal(Name(x), bindlist)::[]
                            | retval::tail -> retval::[]
                            | _ -> [])
              | FunEnd -> get_stack (tl, Unit::stack, bindlist, lestack, print_stack, func_list, argname, functype)
              | Call ->  let retstack = com_call (stack, bindlist, func_list, func_list) in
                          if is_inout (stack, bindlist, func_list) then
                            if has_ret (get_c (stack, bindlist, func_list)) then
                              (match retstack with
                              argv::retv::xx -> (let getarg = get_arg stack in
                                        match getarg with
                                         Name(k) -> get_stack (tl, retv::xx, store_bind (argv::getarg::[], bindlist, func_list), lestack, print_stack, func_list, argname, functype)
                                        | _ -> get_stack (tl, retv::xx, bindlist, lestack, print_stack, func_list, argname, functype))
                              | _-> [])
                            else (match retstack with
                              he::xx -> (let getarg = get_arg stack in
                                        match getarg with
                                         Name(k) -> get_stack (tl, xx, store_bind (he::getarg::[], bindlist, func_list), lestack, print_stack, func_list, argname, functype)
                                        | _ -> get_stack (tl, xx, bindlist, lestack, print_stack, func_list, argname, functype))
                              | _-> [])
                          else get_stack (tl, retstack, bindlist, lestack, print_stack, func_list, argname, functype)
              | ToString -> get_stack (tl, toString stack, bindlist, lestack, print_stack, func_list, argname, functype)
              | Println -> (match stack with
                             v::rs -> (match v with
                                        Stri(x) -> get_stack (tl, rs, bindlist, lestack, Stri(x)::print_stack, func_list, argname, functype)
                                      | _ -> get_stack (tl, rs, bindlist, lestack, Error::print_stack, func_list, argname, functype))
                            | [] -> Error::print_stack)
              | Quit -> print_stack) (* return stack *)
  | [] -> findVal(Name(argname), bindlist)::[] 

and com_call ((stack : const list), (bindlist : const list), (func_list : com list), (trace_funclist : com list)) : const list =
  match stack with
    Error::Name(fn)::tl -> Error::stack
  | FunName(n, f)::Name(fn)::tl -> if is_bound (Name(fn), bindlist)
                                   then let fn1 = findVal(Name(fn), bindlist) in
                                        match fn1 with
                                         FunName(x, y) -> (match y with
                                                            FunBind(Fun(n1,n2), c, b) -> get_stack (c, [], store_bind (FunName(n, f)::Name(n2)::[], b, func_list), [], [], func_list, n2, "fun")@tl
                                                           | FunBind(InOutFun(n1,n2), c, b) -> get_stack (c, [], store_bind (FunName(n, f)::Name(n2)::[], b, func_list), [], [], func_list, n2, "inout")@tl
                                                           | _ -> Error::stack )
                                        | _ -> (match trace_funclist with
                                                FunBind(Fun(n1,n2), c, b)::rs -> (match fn1 with
                                                                                    Name(fn2) -> if fn2 = n1   (* comlist, stack, bindlist, lestack, funclist *)
                                                                                                then get_stack (c, [], store_bind (FunName(n, f)::Name(n2)::[], b, func_list), [], [], func_list, n2, "fun")@tl (* function exist -> call this function*)
                                                                                                else com_call (stack, bindlist, func_list, rs)
                                                                                  | _ -> Error::stack)
                                               | FunBind(InOutFun(n1,n2), c, b)::rs -> (match fn1 with
                                                                                    Name(fn2) -> if fn2 = n1   (* comlist, stack, bindlist, lestack, funclist *)
                                                                                                then get_stack (c, [], store_bind (FunName(n, f)::Name(n2)::[], b, func_list), [], [], func_list, n2, "inout")@tl (* function exist -> call this function*)
                                                                                                else com_call (stack, bindlist, func_list, rs)
                                                                                  | _ -> Error::stack)
                                               | _ -> Error::stack)
                                    else (match trace_funclist with
                                          FunBind(Fun(n1,n2), c, b)::rs -> if fn = n1   (* comlist, stack, bindlist, lestack, funclist *)
                                                                           then get_stack (c, [], store_bind (FunName(n, f)::Name(n2)::[], b, func_list), [], [], func_list, n2, "fun")@tl (* function exist -> call this function*)
                                                                           else com_call (stack, bindlist, func_list, rs)
                                          | FunBind(InOutFun(n1,n2), c, b)::rs -> if fn = n1   (* comlist, stack, bindlist, lestack, funclist *)
                                                                           then get_stack (c, [], store_bind (FunName(n, f)::Name(n2)::[], b, func_list), [], [], func_list, n2, "inout")@tl (* function exist -> call this function*)
                                                                           else com_call (stack, bindlist, func_list, rs)
                                          | _ -> Error::stack)
  | Name(arg)::Name(fn)::tl -> if is_bound (Name(arg), bindlist) (* if arg bounded with something*)
                               then let v1 = findVal(Name(arg), bindlist) in
                                    if is_bound (Name(fn), bindlist)  (* if function name bounded with something*)
                                    then let fn1 = findVal(Name(fn), bindlist) in
                                        match fn1 with
                                         FunName(x, y) -> (match y with
                                                           FunBind(Fun(n1,n2), c, b) -> get_stack (c, [], store_bind (v1::Name(n2)::[], b, func_list), [], [], func_list, n2, "fun")@tl
                                                           | FunBind(InOutFun(n1,n2), c, b) -> get_stack (c, [], store_bind (v1::Name(n2)::[], b, func_list), [], [], func_list, n2, "inout")@tl
                                                           | _ -> Error::stack )
                                        | _ -> (match trace_funclist with
                                                 FunBind(Fun(n1,n2), c, b)::rs -> (match fn1 with
                                                                                    Name(fn2) -> if fn2 = n1   (* comlist, stack, bindlist, lestack, funclist *)
                                                                                                then get_stack (c, [], store_bind (v1::Name(n2)::[], b, func_list), [], [], func_list, n2, "fun")@tl (* function exist -> call this function*)
                                                                                                else com_call (stack, bindlist, func_list, rs)
                                                                                  | _ -> Error::stack)
                                                | FunBind(InOutFun(n1,n2), c, b)::rs ->  (match fn1 with
                                                                                           Name(fn2) -> if fn2 = n1   (* comlist, stack, bindlist, lestack, funclist *)
                                                                                                        then get_stack (c, [], store_bind (v1::Name(n2)::[], b, func_list), [], [], func_list, n2, "inout")@tl (* function exist -> call this function*)
                                                                                                        else com_call (stack, bindlist, func_list, rs)
                                                                                          | _ -> Error::stack)
                                                | _ -> Error::stack)
                                    else (match func_list with (* if function name not bound with something*)
                                          FunBind(Fun(n1,n2), c, b)::rs -> if fn = n1   (* comlist, stack, bindlist, lestack, funclist *)
                                                                           then get_stack (c, [], store_bind (v1::Name(n2)::[], b, func_list), [], [], func_list, n2, "fun")@tl (* function exist -> call this function*)
                                                                           else com_call (stack, bindlist, func_list, rs)
                                         | FunBind(InOutFun(n1,n2), c, b)::rs -> if fn = n1   (* comlist, stack, bindlist, lestack, funclist *)
                                                                                then get_stack (c, [], store_bind (v1::Name(n2)::[], b, func_list), [], [], func_list, n2, "inout")@tl (* function exist -> call this function*)
                                                                                else com_call (stack, bindlist, func_list, rs)
                                         | _ -> Error::stack)
                               else if is_func (Name(arg), func_list)
                                    then if is_bound (Name(fn), bindlist)
                                         then let fn1 = findVal(Name(fn), bindlist) in
                                              match fn1 with
                                              FunName(x, y) -> (match y with
                                                                FunBind(Fun(n1,n2), c, b) -> get_stack (c, [], store_bind (Name(arg)::Name(n2)::[], b, func_list), [], [], func_list, n2, "fun")@tl
                                                                | FunBind(InOutFun(n1,n2), c, b) -> get_stack (c, [], store_bind (Name(arg)::Name(n2)::[], b, func_list), [], [], func_list, n2, "inout")@tl
                                                                | _ -> Error::stack )
                                              | _ -> (match trace_funclist with
                                                      FunBind(Fun(n1,n2), c, b)::rs -> (match fn1 with
                                                                                          Name(fn2) -> if fn2 = n1   (* comlist, stack, bindlist, lestack, funclist *)
                                                                                                        then get_stack (c, [], store_bind (Name(arg)::Name(n2)::[], b, func_list), [], [], func_list, n2, "fun")@tl (* function exist -> call this function*)
                                                                                                        else com_call (stack, bindlist, func_list, rs)
                                                                                          | _ -> Error::stack)
                                                      | FunBind(InOutFun(n1,n2), c, b)::rs -> (match fn1 with
                                                                                                Name(fn2) -> if fn2 = n1   (* comlist, stack, bindlist, lestack, funclist *)
                                                                                                              then get_stack (c, [], store_bind (Name(arg)::Name(n2)::[], b, func_list), [], [], func_list, n2, "inout")@tl (* function exist -> call this function*)
                                                                                                              else com_call (stack, bindlist, func_list, rs)
                                                                                                | _ -> Error::stack)
                                                      | _ -> Error::stack)
                                         else (match trace_funclist with
                                                FunBind(Fun(n1,n2), c, b)::rs -> if fn = n1   (* comlist, stack, bindlist, lestack, funclist *)
                                                                                 then get_stack (c, [], store_bind (Name(arg)::Name(n2)::[], b, func_list), [], [], func_list, n2, "fun")@tl (* function exist -> call this function*)
                                                                                 else com_call (stack, bindlist, func_list, rs)
                                              | FunBind(InOutFun(n1,n2), c, b)::rs ->  if fn = n1   (* comlist, stack, bindlist, lestack, funclist *)
                                                                                      then get_stack (c, [], store_bind (Name(arg)::Name(n2)::[], b, func_list), [], [], func_list, n2, "inout")@tl (* function exist -> call this function*)
                                                                                      else com_call (stack, bindlist, func_list, rs)
                                              | _ -> Error::stack)
                                    else Error::stack(*Int(List.length func_list)::stack test(Name(arg), func_list)@stack   Stri("-7-")::stack*)
  | arg::Name(fn)::tl -> if is_bound (Name(fn), bindlist)
                         then let fn1 = findVal(Name(fn), bindlist) in
                             match fn1 with
                              FunName(x, y) -> (match y with
                                                FunBind(Fun(n1,n2), c, b) -> get_stack (c, [], store_bind (arg::Name(n2)::[], b, func_list), [], [], func_list, n2, "fun")@tl
                                                | FunBind(InOutFun(n1,n2), c, b) -> get_stack (c, [], store_bind (arg::Name(n2)::[], b, func_list), [], [], func_list, n2, "inout")@tl
                                                | _ -> Error::stack )
                              | _ -> (match trace_funclist with
                                      FunBind(Fun(n1,n2), c, b)::rs -> (match fn1 with
                                                                          Name(fn2) -> if fn2 = n1   (* comlist, stack, bindlist, lestack, funclist *)
                                                                                      then get_stack (c, [], store_bind (arg::Name(n2)::[], b, func_list), [], [], func_list, n2, "fun")@tl (* function exist -> call this function*)
                                                                                      else com_call (stack, bindlist, func_list, rs)
                                                                        | _ -> Error::stack)
                                      | FunBind(InOutFun(n1,n2), c, b)::rs -> (match fn1 with
                                                                               Name(fn2) -> if fn2 = n1   (* comlist, stack, bindlist, lestack, funclist *)
                                                                                            then get_stack (c, [], store_bind (arg::Name(n2)::[], b, func_list), [], [], func_list, n2, "inout")@tl (* function exist -> call this function*)
                                                                                            else com_call (stack, bindlist, func_list, rs)
                                                                              | _ -> Error::stack)
                                      | _ -> Error::stack)
                         else (match trace_funclist with
                               FunBind(Fun(n1,n2), c, b)::rs -> if fn = n1   (* comlist, stack, bindlist, lestack, funclist *)
                                                                then get_stack (c, [], store_bind (arg::Name(n2)::[], b, func_list), [], [], func_list, n2, "fun")@tl (* function exist -> call this function*)
                                                                else com_call (stack, bindlist, func_list, rs)
                              | FunBind(InOutFun(n1,n2), c, b)::rs -> if fn = n1   (* comlist, stack, bindlist, lestack, funclist *)
                                                                      then get_stack (c, [], store_bind (arg::Name(n2)::[], b, func_list), [], [], func_list, n2, "inout")@tl (* function exist -> call this function*)
                                                                      else com_call (stack, bindlist, func_list, rs)
                              | _ -> Error::stack)
  | Name(x)::FunName(n, fn)::tl -> if is_bound (Name(x), bindlist) (* if arg bounded with something*)
                                   then let v1 = findVal(Name(x), bindlist) in
                                        match fn with
                                         FunBind(Fun(n1,n2), c, b) -> get_stack (c, [], store_bind (v1::Name(n2)::[], b, func_list), [], [], func_list, n2, "fun")@tl
                                        | FunBind(InOutFun(n1,n2), c, b) ->get_stack (c, [], store_bind (v1::Name(n2)::[], b, func_list), [], [], func_list, n2, "inout")@tl
                                        | _ -> Error::stack
                                   else (match fn with
                                         FunBind(Fun(n1,n2), c, b) -> get_stack (c, [], store_bind (Name(x)::Name(n2)::[], b, func_list), [], [], func_list, n2, "fun")@tl
                                        | FunBind(InOutFun(n1,n2), c, b) -> get_stack (c, [], store_bind (Name(x)::Name(n2)::[], b, func_list), [], [], func_list, n2, "inout")@tl
                                        | _ -> Error::stack )
  | FunName(x, xfn)::FunName(n, fn)::tl -> (match fn with
                                             FunBind(Fun(n1,n2), c, b) -> get_stack (c, [], store_bind (FunName(x, xfn)::Name(n2)::[], b, func_list), [], [], func_list, n2, "fun")@tl
                                            | FunBind(InOutFun(n1,n2), c, b) ->get_stack (c, [], store_bind (FunName(x, xfn)::Name(n2)::[], b, func_list), [], [], func_list, n2, "inout")@tl
                                            | _ -> Error::stack )
  | arg::FunName(n, fn)::tl -> (match fn with
                                FunBind(Fun(n1,n2), c, b) -> get_stack (c, [], store_bind (arg::Name(n2)::[], b, func_list), [], [], func_list, n2, "fun")@tl
                               | FunBind(InOutFun(n1,n2), c, b) ->get_stack (c, [], store_bind (arg::Name(n2)::[], b, func_list), [], [], func_list, n2, "inout")@tl
                               | _ -> Error::stack )
  | _ -> Error::stack (*Stri("-11-")::stack*)

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
  let stack = get_stack (coms, [], [], [], [], [], "", "") in  (* return stack *)
  let stack = List.rev stack in
  (*....... pending .........*)
  let print stack =
    file_write (printString stack) in
  List.iter print stack;;
(*interpreter ("hw6_input.txt", "hw6_output.txt")*)
(*interpreter ("input17.txt", "output17.txt")*)