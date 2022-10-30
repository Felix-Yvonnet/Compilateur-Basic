(* File lexer.mll *)

{
    open Parser        (* le type token est défini dans parser.mli *)
    exception Eof
}

(* reconnait les tokens prédéfinis *)
rule token = parse
      [' ' '\t']                                                                          { token lexbuf }  (* skip blanks *)
    | ['\n' ]                                                                             { EOL }
    | ['0'-'9']+ as lxm                                                                   { Int(int_of_string lxm) }
    | ['0'-'9']* '.' ['0' - '9']+ | ['0'-'9']+ '.' ['0' - '9']* as lxm                    { Float(float_of_string lxm) }
    | '+'                                                                                 { PLUS }
    | '-'                                                                                 { MINUS }
    | '*'                                                                                 { TIMES }
    | '/'                                                                                 { DIV }
    | '^'                                                                                 { POW }
    | "+."                                                                                { PLUS_F }
    | "-."                                                                                { MINUS_F }
    | "*."                                                                                { TIMES_F }
    | "/."                                                                                { DIV_F }
    | '%'                                                                                 { REST }
    | '('                                                                                 { LPAR }
    | ')'                                                                                 { RPAR }
    | "int"                                                                               { To_Int }
    | "float"                                                                             { To_Float }
    | "!"                                                                                 { FACT }
    | '#' [^ '\n']*                                                                       { token lexbuf } (* skip the comments *)
    | eof                                                                                 { raise Eof }


