grammar Expr;
root : procediment* EOF ;

instancies: instancia*;
instancia: expr
    | assignacio
    | write
    | condicional
    | iteracioWhile
    | iteracioFor
    | read
    | proc
    ;




procediment: 'void' PROCESNAME '(' (VAR (',' VAR)*)? ')' '{' instancies '}';

PROCESNAME: [a-zA-Z][a-zA-Z0-9_]+;

proc: PROCESNAME '(' (expr (',' expr)*)? ')';

expr : <assoc=right> expr MUL expr #Mul
    | expr DIV expr #Div
    | expr MOD expr #Mod
    | expr SUM expr #Sum
    | expr RES expr #Res
    | NUM #Num
    | VAR #Var
    ;

condicional: 'if' '(' comparacio ')' '{' instancies '}' ('else' '{' instancies '}')?;

iteracioWhile: 'while' '(' comparacio ')' '{' instancies '}';

iteracioFor: 'for' '(' assignacio ';' comparacio ';' assignacio ')' '{' instancies '}';


comparacio: expr EQ expr #Eq
    | expr NEQ expr #Neq
    | expr GT expr #Gt
    | expr LT expr #Lt
    | expr GEQ expr #Geq
    | expr LEQ expr #Leq
    ;


assignacio: VAR ASSIG expr;

write: 'write' '(' expr (',' expr)*? ')';

read: 'read' '(' VAR ')';

NUM : [0-9]+ ;
MUL : '*' ;
DIV : '/' ;
MOD : '%' ;
SUM : '+' ;
RES : '-' ;
EQ : '==' ;
NEQ : '<>' ;
GT : '>' ;
LT : '<' ;
GEQ : '>=' ;
LEQ : '<=' ;

VAR : [a-z]+ ;
ASSIG : '=';
WS : [ \n]+ -> skip ;
