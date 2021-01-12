(*CSE-305 JingJingZhuang
constants**********
const ::= int | bool | error | string | name | unit
int ::= [-] digit { digit }
bool ::= :true: | :false:
error ::= :error:
unit ::= :unit:
string ::= "simpleASCII { simpleASCII }"
simpleASCII ::= ASCII \ {'\', '"'}
name ::= {_} letter {letter | digit | _}
*)
type const = Int of int | Bool of bool | Stri of string | Name of string | Unit | Error

let is_int str = 
  try (int_of_string str |> string_of_int) = str
  with Failure _ -> false

(*programs***********
com ::= push const | add | sub | mul | div | rem | neg | swap | pop | cat | and | or | not | lessThan | equal | if | bind | let com {com} end | funBind com {com} [ return ] funEnd | call
*)
type com = Push of const | Add | Sub | Mul | Div | Rem | Neg | Swap | Pop | Quit | ToString | Println (*| Cat | And | Or | Not | LessThan | Equal | If | Bind | Let | End | Call*)

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
                | "quit"::rs -> Quit::get_com tl
                | x::rs -> Push Error::get_com tl
                | _ -> [])

(* add stack *)
let com_add (stack : const list) : const list =
  match stack with
    Int(x)::Int(y)::tl -> Int(x+y)::tl
  | _ -> Error::stack

(* subtract stack *)
let com_sub (stack : const list) : const list =
  match stack with
    Int(x)::Int(y)::tl -> Int(y-x)::tl
  | _ -> Error::stack

(* multiply stack *)
let com_mul (stack : const list) : const list =
  match stack with
    Int(x)::Int(y)::tl -> Int(x*y)::tl
  | _ -> Error::stack

(* divide stack *)
let com_div (stack : const list) : const list =
  match stack with
    Int(x)::Int(y)::tl -> if x != 0 then Int(y/x)::tl 
                          else Error::stack
  | _ -> Error::stack

(* remainder *)
let com_rem (stack : const list) : const list =
  match stack with
    Int(x)::Int(y)::tl -> if x != 0 then Int(y mod x)::tl
                          else Error::stack
  | _ -> Error::stack

(* negation *)
let com_neg (stack : const list) : const list =
  match stack with
    Int(x)::tl -> Int(x * -1)::tl
  | _ -> Error::stack

let com_swap (stack : const list) : const list =
  match stack with
    x::y::tl -> y::x::tl
  | _ -> Error::stack

let com_pop (stack : const list) : const list =
  match stack with
    hd::tk -> tk
  | _ -> Error::stack

let toString (c : const) : string =
  match c with
    Int(i) -> Printf.sprintf "%d" i
  | Bool(b) -> ":" ^ string_of_bool b ^":"
  | Stri(s) -> s
  | Name(n) -> n
  | Unit -> ":unit:"
  | Error -> ":error:"

let rec get_stack ((com_list : com list), (stack : const list)) : const list =
  match com_list with
    hd::tl -> (match hd with
                Push(cst) -> get_stack (tl, cst::stack)
              | Add -> get_stack (tl, com_add stack)
              | Sub -> get_stack (tl, com_sub stack)
              | Mul -> get_stack (tl, com_mul stack)
              | Div -> get_stack (tl, com_div stack)
              | Rem -> get_stack (tl, com_rem stack)
              | Neg -> get_stack (tl, com_neg stack)
              | Swap -> get_stack (tl, com_swap stack)
              | Pop -> get_stack (tl, com_pop stack)
              | ToString -> get_stack (tl, stack)
              | Println -> get_stack (tl, stack)
              | Quit -> stack) (* return stack *)
  | [] -> stack

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
  let stack = get_stack (coms, []) in  (* return stack *)
  (*....... pending .........*)
  let print stack =
    file_write (toString stack) in
  List.iter print stack;;
interpreter ("input9.txt", "output9.txt")