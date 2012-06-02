grammar FL;
options {
    output=AST;
    ASTLabelType=CommonTree;
}

@header {
package fl;
}

@lexer::header {
package fl;
}

prog:   (expr NEWLINE)+
    ;

exprItem
    :   atom
    |   name
    |   seq
    |   '(' expr ')'
    |   cond
    |   '~' PRIME expr
    |   '[' PRIME exprList ']'
    |   '[|' PRIME exprList '|]'
    ;

exprTail
    :   ':' PRIME expr
    |   'where' expr
    ;

// Infix expr is defined as: expr expr expr. I don't think that's right...
// Leaving it out for now.
expr:   exprItem PRIME exprTail?
    ;

exprList
    :   ((expr ',')* expr)?
    ;

atom:   '\'' CHARACTER
    |   NUMBER
    |   'T'
    |   'F'
    |   '_' // bottom
    ;

seq :   '<' exprList '>'
    |   string
    ;

string
    :   '"' CHARACTER* '"'
    ;

cond:   expr '->' PRIME expr (';' expr)?
    |   pat '->' expr (';' expr)?
    ;

patList
    :   (patExpr ',')* pat (',' patExpr)*
    ;

pat :   name '.' (patExpr)?
    |   '[|' patList '|]'
    ;

patExpr
    :   pat
    |   expr
    ;

env :   envItem (('uses'|'where'|'union') env)?
    ;

envItem
    :   '{' defn+ '}'
    |   'export(' nameList ')' env
    |   'hide(' nameList ')' env
    |   'lib(' string ')'
    |   'PF'
    |   '{' env '}'
    ;

nameList
    :   (name ',')* name
    ;

defn:   'def' name (patExpr)? '=:' expr
    |   'nrdef' name (patExpr)? '=:' expr
    |   'type' identifier '=:' pat
    ;

name:   identifier
    |   OPERATOR
    ;

identifier
    :   LETTER ID_CHAR*
    ;

PRIME
    :   '`'*
    ;

CHARACTER
    :   .
    ;

LETTER
    :   ('A'..'Z'|'a'..'z')
    ;

NUMBER
    :   ('+'|'-')? ('0'..'9')+ ('.' ('0'..'9')+)?
    ;

ID_CHAR
    :   (LETTER|'0'..'9'|'_')
    ;

OPERATOR
    :   ('+'|'-'|'*'|'%') // This is most certainly NOT the list of operators
    ;

NEWLINE
    :   '\r'? '\n'
    ;

WS  :   (' '|'\t')+ {skip();}
    ;
