(* ass.ml *)
open Format
open X86_64
open Asyntax


(* afficher le résultat *)
let ender t =match t with
    | "int" -> movq !%rax !%rdi ++ inline ("\tcall print_int\n") ++ movq (imm 0) !%rax
    | "float" -> inline ("\tcall print_float\n") ++ movq (imm 0) !%rax
    | _ -> raise(Error " Mauvais typage, relis toi !")


(* définition des fonctions supplémentaires *)
let fact = inline "fact:\n" ++ movq (imm 1) !%r12 ++ inline "\n.loopfact:\n" ++ testq !%rax !%rax ++ jle ".endfact" ++ imulq !%rax !%r12 ++ decq !%rax ++ call ".loopfact" ++ inline "\n.endfact:\n" ++ movq !%r12 !%rax ++ ret ++ inline "\n"
let pow = inline "pow:\n" ++ movq (imm 1) !%r12 ++ inline "\n.loopow:\n" ++ testq !%rbx !%rbx ++ jle ".endpow" ++ imulq !%rax !%r12 ++ decq !%rbx ++ call ".loopow" ++ inline "\n.endpow:\n" ++ movq !%r12 !%rax ++ ret ++ inline "\n"


(* contiendra les floats qu'on souhaite utiliser *)
let stack = ref []

(* Permet de convertir stack en instruction assembleur utilisable *)
let rec destack = function
    | [] -> "\n"
    | x::q -> let sx = string_of_float x in "f" ^ sx ^ ":\n\t.double " ^ sx ^ "\n" ^ (destack q)


(* Pour éviter les répétitions de définitions de variables *)
let rec wash = function
    | [] -> []
    | t::q when List.mem t q -> wash q
    | t::q -> t::(wash q) 




(* reconnais l'application/l'opérateur utilisée *)

let int_app_recon s = match s with
    | "-" -> negq !%rax
    | "!" -> call "fact"
    | "float" ->  cvtsi2sdq !%rax xmm0
    | _-> raise (Error "Where are we ?")


let float_app_recon s = match s with
    | "-" -> mulsd (immf (-. 1.0) stack) xmm0
    | "int" -> cvttsd2siq !%xmm0 rax
    | _-> raise (Error "Am I alive ?")


let int_op_recon s = match s with
    | "+" -> addq !%rbx !%rax
    | "-" -> subq !%rbx !%rax
    | "*" -> imulq !%rbx !%rax
    | "/" -> movq (imm 0) !%rdx ++ idivq !%rbx
    | "%" -> movq (imm 0) !%rdx ++ idivq !%rbx ++ movq !%rdx !%rax
    | "^" -> call "pow"
    | _ -> raise (Error "It's sunday already ?!!!")


let float_op_recon s =
    match s with
    | "+." -> addsd !%xmm1 xmm0
    | "-." -> subsd !%xmm1 xmm0
    | "*." -> mulsd !%xmm1 xmm0
    | "/." -> divsd xmm0 !%xmm1
    | _ -> raise (Error "What am I doing here ?")



(* pour traiter des problèmes de signe dans la factorielle et la puissance *)
let fact_pow_recon s n = match s with
    | "!" | "^" -> (match n with   | Int(a) when a >= 0 -> ()
    | _ -> raise (Error "positive number expected for factorial and power"))
    | _ -> ()




(* Attribue le bon code asm dans le bon ordre à l'expression rencontrée *)

let rec calculatrice truc  = match truc with
   | Int(x) -> movq (imm x) !%rax
   | Float(x) -> movsd (immf x stack) xmm0
   | App(f, x) when List.mem f app_int_list           -> fact_pow_recon f x; calculatrice x  ++ int_app_recon f
   | App(f, x) when List.mem f app_float_list         -> calculatrice x  ++ float_app_recon f
   | Op(op, x, y) when List.mem op op_int_list        -> fact_pow_recon op y; calculatrice y  ++ pushq !%rax ++ calculatrice x  ++ popq rbx ++ int_op_recon op
   | Op(op, x, y) when List.mem op op_float_list      -> calculatrice y  ++ pushsd !%xmm0 ++ calculatrice x  ++ popsd xmm1 ++ float_op_recon op
   | _ -> raise (Error "error i guess")


(* la fonction principale ! *)
let core truc t =
    let res = calculatrice truc in
        (res ++ (ender t), !stack)