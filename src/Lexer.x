{
module Lexer where
import Data.Char
import DataTypes
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z_]		-- alphabetic characters
$all = [\x00-\x10ffff]  -- all characters

$a = [aA]
$b = [bB]
$c = [cC]
$d = [dD]
$e = [eE]
$f = [fF]
$g = [gG]
$h = [hH]
$i = [iI]
$j = [jJ]
$k = [kK]
$l = [lL]
$m = [mM]
$n = [nN]
$o = [oO]
$p = [pP]
$q = [qQ]
$r = [rR]
$s = [sS]
$t = [tT]
$u = [uU]
$v = [vV]
$w = [wW]
$x = [xX]
$y = [yY]
$z = [zZ]

tokens :-
$white+;

\(\* ($all # [\*])* \*+ ( ($all # [\)]) ($all # [\*])* \*+ )* \) ;
\;                                              { \_ -> SEMICOLON }
\:											    { \_ -> DDOT}
\:=                                             { \_ -> ASSIGN }
\.                                              { \_ -> DOT }
\,                                              { \_ -> COMMA }
\(                                              { \_ -> LPARENT }
\)                                              { \_ -> RPARENT }
\[                                              { \_ -> LBRACKET }
\]                                              { \_ -> RBRACKET }

$d$i$v                                          { \_ -> OP DIV}
$m$o$d                                          { \_ -> OP MOD }
\-                                              { \_ -> OP MINUS }
\*                                              { \_ -> OP MULT }
\+                                              { \_ -> OP PLUS }

"="                                             { \_ -> OP EQUAL }
"<"                                             { \_ -> OP LESS }
"<="                                            { \_ -> OP LEQUAL }
">"                                             { \_ -> OP GREAT }
">="                                            { \_ -> OP GEQUAL }
"<>"                                            { \_ -> OP DIFF }
$n$o$t                                          { \_ -> OP NOT }
$a$n$d                                          { \_ -> OP AND }
$o$r                                            { \_ -> OP OR}

\:=                                             { \_ -> ASSIGN }
$p$r$o$g$r$a$m                                  { \_ -> PROGRAM }
$f$u$n$c$t$i$o$n                                { \_ -> FUNCTION }
$p$r$o$c$e$d$u$r$e                              { \_ -> PROCEDURE }

$i$f											{ \_ -> IF }
$t$h$e$n                                        { \_ -> THEN }
$b$r$e$a$k										{ \_ -> BREAK}
$e$l$s$e                                        { \_ -> ELSE }
$w$h$i$l$e                                      { \_ -> WHILE }
$d$o											{ \_ -> DO }
$b$e$g$i$n                                      { \_ -> BEGIN }
$e$n$d                                          { \_ -> END }
$f$o$r                                          { \_ -> FOR }
$t$o                                            { \_ -> TO }
$o$f                                            { \_ -> OF }

$v$a$r                                          { \_ -> VAR }
$c$o$n$s$t                                      { \_ -> CONST }
$a$r$r$a$y                                      { \_ -> ARRAY}
$i$n$t$e$g$e$r                                  { \_ -> TYPE INTEGER  }
$b$o$o$l$e$a$n                                  { \_ -> TYPE BOOLEAN }
$s$t$r$i$n$g                                    { \_ -> TYPE STRING }
-- double                                       { \_ -> TYPE DOUBLE }

$t$r$u$e                                        { \_ -> BOOL True }
$f$a$l$s$e                                      { \_ -> BOOL False }
$alpha($alpha|$digit)*                          { \s -> IDENT (map toLower s) }
$digit+                                         { \s -> NUM (read s) }
-- $digit+"."$digit+                            { \s -> REAL (read s) }
\'(~[\'])*\'                                    { \s -> STR s }

{

}
