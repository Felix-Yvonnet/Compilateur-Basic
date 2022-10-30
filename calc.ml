(* File calc.ml *)
open Asyntax
open Ass
open X86_64
open String
open Printexc


(* les élements à afficher souvent *)
let header = inline ("\t.globl main\n" ^ "\n" ^ "main:\n")

(* afficher les trucs finaux pour pas les répéter *)
let print_int = inline ("\nprint_int:\n" ^"\tpushq %rbp\n" ^ "\tmov %rdi, %rsi\n" ^ "\tmov $pi, %rdi\n" ^ "\tmov $0, %rax\n" ^ "\tcall printf\n" ^ "\tpopq %rbp\n" ^ "\tret\n")
let print_float = inline ("\nprint_float:\n\tmovq $pf, %rdi\n"^"\tmovq $1, %rax\n"^"\tcall printf\n"^"\tret\n")


let msg = inline ("pi:\n\t.string \"%d\\n\"\n" ^ "pf:\n\t.string \"%f\\n\"\n")


(* Fonction main *)
let _ =
  let fichier = Sys.argv.(1) in                                         (* récupère le titre du fichier donné en argument *)
  let fichier = String.sub fichier 0 (String.length fichier - 4) in     (* retire l'extension *) 
  if not ((fichier^".exp") = Sys.argv.(1))                              (* vérifie que c'est bien du .exp *)
    then raise (Error "Invalid argument, .exp expected" );
  let text = Arg.read_arg (fichier^".exp") in                           (* récupère la matrice des lignes du document *)
  let rez = ref header and vars = ref [] in                             (* rez = le corps du texte, vars = la liste des variables flottantes*)
  for k = 0 to Array.length text - 1 do
    try
      if text.(k) = "" then raise (Error "Empty Line");                 (* sinon ça bug sur les dernières lignes *)
      let lexbuf = Lexing.from_string (text.(k) ^"\n") in               (* lit le fichier ligne par ligne *)
        let ast = Parser.main Lexer.token lexbuf in                     (* Parsent et Lexent le tout *)
        let res,var = core ast (typage ast) in                         
        rez:= !rez ++ res;
        vars := var;
    with
      | End_of_file -> exit 0
      | Error "Empty Line" -> ()
      | e -> print_string ("erreur "^ (to_string e)^" survenue ligne "^ (string_of_int (k+1))^"\n")

  done;
  vars := wash !vars;                                                   (* éviter les doublons de définitions de variables *)
  rez := !rez ++ ret ++ inline"\n" ++ fact ++ pow ++ print_int ++  inline "\n" ++ print_float;
  print_in_file (fichier^".s") { text = !rez; data = msg ++ (inline (destack !vars)); };




