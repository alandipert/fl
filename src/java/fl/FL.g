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

expr:   atom
    |   name
    |   seq
    |   expr ':' PRIME expr
    |   '(' expr ')'
    |   expr PRIME
    |   cond
    |   '~' PRIME expr
    |   '[' PRIME exprList ']'
    |   '[|' PRIME exprList '|]'
    |   expr expr expr
    |   expr 'where' expr
    ;

exprList
    :   ((expr ',')* expr)?
    ;

atom:   '\'' CHARACTER
    |   NUMBER
    |   'T'
    |   'F'
    |   '|-' // bottom
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

patlist
    :   (patExpr ',')* pat (',' patExpr)*
    ;

pat :   name '.' (patExpr)?
    |   '[|' patList '|]'
    ;

patExpr
    :   pat
    |   expr
    ;

env :   '{' defn+ '}'
    |   'export(' nameList ')' env
    |   'hide(' nameList ')' env
    |   'lib(' string ')'
    |   'PF'
    |   env 'uses' env
    |   env 'where' env
    |   env 'union' env
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
    :   LETTER (ID_CHAR)*
    ;

PRIME
    :   '`'*
    ;

CHARACTER
    : .
    ;

LETTER
    :   ('A'..'Z'|'a'..'z')
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
