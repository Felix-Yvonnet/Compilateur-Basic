(* asyntax.ml *)
exception Error of string

type exp =
    Int of int
    | Float of float
    | App of string * exp
    | Op of string * exp * exp;;

let app_int_list = ["float"; "-"]
and app_float_list = ["int"; "-."]
and op_int_list = ["+";"-";"*";"/";"%"]
and op_float_list = ["+.";"-.";"*."]
;;



(* vérifications de types *)
let intp exp =
  match exp with
  | Int _ -> false
  | Float _ -> false
  | App _ -> false
  | Op _ -> true

let applp exp =
  match exp with
  | Int _ -> false
  | Float _ -> false
  | App _ -> true
  | Op _ -> false


(* tests le typage correcte de l'expression *)
let rec typage exp = match exp with
    | Int(x) -> "int"
    | Float(x) -> "float"
    | App(f,x) when List.mem f app_int_list-> if typage x = "int" then (if f = "float" then "float" else "int") else raise (Error "Wrong type, expected int with this function")
    | App(f,x) when List.mem f app_float_list-> if typage x = "float" then (if f = "int" then "int" else "float") else raise (Error "Wrong type, expected float with this function")
    | App("()",x) -> typage x
    | Op(op,x,y) when List.mem op op_float_list -> if ((typage x = "float") && (typage y = "float")) then "float" else raise (Error "Wrong type, expected int with this function")
    | Op(op,x,y) when List.mem op op_float_list -> if ((typage x = "float") && (typage y = "float")) then "float" else raise (Error "Wrong type, expected float with this function")
    | _ -> raise (Error "What append ?")
;;


