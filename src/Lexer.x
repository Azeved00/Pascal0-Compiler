{
module Lexer where
import DataTypes
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z_]		-- alphabetic characters

tokens :-
$white+;

"/*" (~[\*])* (\*)+ ( (~[\/\*]) (~[\*])* "*"+ )* "/";
\;                                              { \_ -> SEMICOLON }
\:												{ \_ -> DDOT}
\:=                                             { \_ -> ASSIGN }
\.                                              { \_ -> DOT }

div                                             { \_ -> ARITOP DIV}
mod                                             { \_ -> ARITOP MOD }
\-                                              { \_ -> ARITOP MINUS }
\*                                              { \_ -> ARITOP MULT }
\+                                              { \_ -> ARITOP PLUS }
"<="                                            { \_ -> BOOLOP LEQUAL }


program                                         { \_ -> PROGRAM }
function                                        { \_ -> FUNCTION }
procedure                                       { \_ -> PROCEDURE }

if												{ \_ -> IF }
break											{ \_ -> BREAK}
else                                            { \_ -> ELSE }
while                                           { \_ -> WHILE }
do												{ \_ -> DO }
end                                             { \_ -> END }
for                                             { \_ -> FOR }
to                                              { \_ -> TO }
of                                              { \_ -> OF }

var                                             { \_ -> VAR }
const                                           { \_ -> CONST }
integer                                         { \_ -> TYPE INTEGER  }
boolean                                         { \_ -> TYPE BOOLEAN }
string                                          { \_ -> TYPE STRING }
double                                          { \_ -> TYPE DOUBLE }

array                                           { \_ -> TYPE ARRAY }
true                                            { \_ -> BOOL True }
false                                           { \_ -> BOOL False }
$alpha($alpha|$digit)*                          { \s -> IDENT s }
$digit+                                         { \s -> NUM (read s) }
$digit+"."$digit+                               { \s -> REAL (read s) }


{}
