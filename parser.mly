/* 
* parser.mly
* construit l'arbre syntaxique
*/

%{
    open Asyntax
%}


%token <int> Int
%token <float> Float
%token PLUS MINUS TIMES DIV REST To_Int To_Float TIMES_F MINUS_F PLUS_F DIV_F FACT POW
%token LPAR RPAR
%token EOL
%left PLUS MINUS PLUS_F MINUS_F           /* lowest precedence  */
%left TIMES DIV TIMES_F REST DIV_F POW   /* medium precedence  */
%nonassoc UMINUS UPLUS FACT             /* highest precedence */
%start main                            /*  the entry point   */
%type <Asyntax.exp> main

%%

main:
    expr EOL                                     { $1 }
;

expr:
      Int                                         { Int($1) }
    | Float                                       { Float($1) }
    | To_Int LPAR expr RPAR                       { App("int", $3) }
    | To_Float LPAR expr RPAR                     { App("float", $3) }
    | LPAR expr RPAR                              { $2 }
    | expr PLUS expr                              { Op("+", $1, $3) }
    | expr MINUS expr                             { Op("-", $1, $3) }
    | expr TIMES expr                             { Op("*", $1, $3) }
    | expr DIV expr                               { Op("/", $1, $3) }
    | expr REST expr                              { Op("%", $1, $3) }
    | expr PLUS_F expr                            { Op("+.", $1, $3) }
    | expr MINUS_F expr                           { Op("-.", $1, $3) }
    | expr TIMES_F expr                           { Op("*.", $1, $3) }
    | expr DIV_F expr                             { Op("/.", $1, $3) }
    | MINUS LPAR expr RPAR %prec UMINUS           { App("-", $3) }
    | PLUS LPAR expr RPAR %prec UPLUS             { $3 }
    | MINUS  Int  %prec UMINUS                    { App("-", Int($2)) }
    | PLUS Int %prec UPLUS                        { Int($2) }
    | MINUS Float %prec UMINUS                    { App("-.", Float($2)) }
    | PLUS Float %prec UPLUS                      { Float($2) }
    | Int FACT                                    { App("!", Int($1)) }       
    | expr POW expr                               { Op("^", $1, $3) }
;