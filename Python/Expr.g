grammar Expr;
root : procediments EOF ;

procediments: procediment*;

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





instancies: instancia*;
instancia: expr
    | assignacio
    | write
    | condicional
    | iteracioWhile
    | iteracioFor
    | read
    | proc
    | taulesArray
    | taulesSet
    ;

proc: VAR '(' (expr (',' expr)*)? ')';


procediment: 'void' VAR '(' (VAR (',' VAR)*)? ')' '{' instancies '}';



expr : <assoc=right> expr MUL expr #Mul
    | expr DIV expr #Div
    | expr MOD expr #Mod
    | expr SUM expr #Sum
    | expr RES expr #Res
    | NUM #Num
    | VAR #Var
    | TEXT #Coment
    | 'get' '(' expr ',' expr ')' #TaulesGet
    ;


condicional: 'if' '(' comparacio ')' '{' instancies '}' ('else' '{' instancies '}')?;

iteracioWhile: 'while' '(' comparacio ')' '{' instancies '}';

iteracioFor: 'for' '(' assignacio ';' comparacio ';' assignacio ')' '{' instancies '}';

taulesArray: 'array' '(' expr ','  expr ')';

taulesSet: 'set' '(' expr ',' expr ',' expr ')';


comparacio: expr EQ expr #Eq
    | expr NEQ expr #Neq
    | expr GT expr #Gt
    | expr LT expr #Lt
    | expr GEQ expr #Geq
    | expr LEQ expr #Leq
    ;


assignacio: VAR ASSIG expr;

ASSIG : '=';

write: 'write' '(' expr (',' expr)*? ')';


read: 'read' '(' VAR ')';

VAR : [a-zA-Z][a-zA-Z0-9]*;
TEXT: '"' .*? '"';

COMENTARI: '#' ~[\r\n]* -> skip;
WS : [ \t\r\n]+ -> skip ;
