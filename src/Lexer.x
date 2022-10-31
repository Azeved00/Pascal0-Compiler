{
module Lexer where
import DataTypes
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z_]		-- alphabetic characters

tokens :-
$white+;

"(*" (~[\*])* (\*)+ ( (~\)\*]) (~[\*])* "*"+ )* ")";
\;                                              { \_ -> SEMICOLON }
\:											    { \_ -> DDOT}
\:=                                             { \_ -> ASSIGN }
\.                                              { \_ -> DOT }
\,                                              { \_ -> COMMA }
\(                                              { \_ -> LPARENT }
\)                                              { \_ -> RPARENT }
\[                                              { \_ -> LBRACKET }
\]                                              { \_ -> RBRACKET }

div                                             { \_ -> OP DIV}
mod                                             { \_ -> OP MOD }
\-                                              { \_ -> OP MINUS }
\*                                              { \_ -> OP MULT }
\+                                              { \_ -> OP PLUS }

"="                                             { \_ -> OP EQUAL }
"<"                                             { \_ -> OP LESS }
"<="                                            { \_ -> OP LEQUAL }
">"                                             { \_ -> OP GREAT }
">="                                            { \_ -> OP GEQUAL }
"<>"                                            { \_ -> OP DIFF }
not                                             { \_ -> OP NOT }
and                                             { \_ -> OP AND }
or                                              { \_ -> OP OR}

\:=                                             { \_ -> ASSIGN }
program                                         { \_ -> PROGRAM }
function                                        { \_ -> FUNCTION }
procedure                                       { \_ -> PROCEDURE }

if												{ \_ -> IF }
then                                            { \_ -> THEN }
break											{ \_ -> BREAK}
else                                            { \_ -> ELSE }
while                                           { \_ -> WHILE }
do												{ \_ -> DO }
begin                                           { \_ -> BEGIN }
end                                             { \_ -> END }
for                                             { \_ -> FOR }
to                                              { \_ -> TO }
of                                              { \_ -> OF }

var                                             { \_ -> VAR }
const                                           { \_ -> CONST }
array                                           { \_ -> ARRAY}
integer                                         { \_ -> TYPE INTEGER  }
boolean                                         { \_ -> TYPE BOOLEAN }
string                                          { \_ -> TYPE STRING }
double                                          { \_ -> TYPE DOUBLE }

true                                            { \_ -> BOOL True }
false                                           { \_ -> BOOL False }
$alpha($alpha|$digit)*                          { \s -> IDENT s }
$digit+                                         { \s -> NUM (read s) }
$digit+"."$digit+                               { \s -> REAL (read s) }


{}
