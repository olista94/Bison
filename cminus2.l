%{
	#include <stdio.h>
	#include "cminus2.tab.h"
	int num_linea = 1;
%}

%x comentarios
%x cadenas

IDENTIFICADOR [A-Za-z_][0-9A-Za-z_]*

DECIMALES [0-9]+
OCTALES 0[0-7]+
HEXADECIMALES 0[xX][0-9A-Fa-f]+

REAL ([0-9]*\.[0-9]+)|(0[xX][0-9A-Fa-f]*\.[0-9A-Fa-f]+)
DECIMAL_CON_MANTISA ([0-9]*\.?[0-9]+)[eE][+-]?[0-9]+
HEXADECIMAL_CON_MANTISA (0[xX][0-9A-Fa-f]*)\.([0-9A-Fa-f]+)[eE][+-]?[0-9]+

CARACTERES_SIMPLES '[^\n'\\]'
CARACTERES_ESCAPADOS '\\['"?\\nrtv]'
CARACTERES_OCTALES '\\([1]?[0-7]{1,2})'
CARACTERES_HEXADECIMALES '\\x([0-7]?[0-9A-Fa-f])'

CADENAS_SIN_SALTO \"(([^\"\n]|\\\")*[^\\])?\"
CADENAS \"(([^\"]|\\\")*[^\\])?\"

PATH <.*[^\n"/>]+\/?.+[^\n"/>]>

DELIMITADOR \(|\)|\{|\}|\?|:|;|,

OPERADORES_ARITMETICOS \+\+|\-\-|\*\*|\+=|-=|\*=|\/=|%=|\+|-|\*|\/|%
OPERADORES_DE_BITS ~|&=|\|=|<<=|>>=|<<|>>|&|\^=|\|
OPERADORES_DE_ACCESO_MEMORIA \.|->|\*|&|\[|\]
OPERADORES_RELACIONALES <=|>=|<>|<|>|==
OPERADORES_LOGICOS !|&&|\|\|
OPERADORES_OTROS =|#|sizeof

COMENTARIO_SIMPLE \/\/.*

%%
\n num_linea++;
\t ;
\\\n ;
[ ] ;

"++" {printf("Linea %d, operador: %s\n", num_linea, yytext);return(INC);}
"--" {printf("Linea %d, operador: %s\n", num_linea, yytext);return(DEC);}
"**" {printf("Linea %d, operador: %s\n", num_linea, yytext);return (POTENCIA);}
"+=" {printf("Linea %d, operador: %s\n", num_linea, yytext);return(SUMA_ASIG);}
"-=" {printf("Linea %d, operador: %s\n", num_linea, yytext);return(RESTA_ASIG);}
"*=" {printf("Linea %d, operador: %s\n", num_linea, yytext);return(MULT_ASIG);}
"/=" {printf("Linea %d, operador: %s\n", num_linea, yytext);return(DIV_ASIG);}
"%=" {printf("Linea %d, operador: %s\n", num_linea, yytext);return (MOD_ASIG);}
"+" {printf("Linea %d, operador: %s\n", num_linea, yytext);return (yytext[0]);}
"-" {printf("Linea %d, operador: %s\n", num_linea, yytext);return (yytext[0]);}
"*" {printf("Linea %d, operador: %s\n", num_linea, yytext);return (yytext[0]);}
"/" {printf("Linea %d, operador: %s\n", num_linea, yytext);return (yytext[0]);}
"%" {printf("Linea %d, operador: %s\n", num_linea, yytext);return (yytext[0]);}
"~" {printf("Linea %d, operador: %s\n", num_linea, yytext);return (yytext[0]);}
"&=" {printf("Linea %d, operador: %s\n", num_linea, yytext);return(AND_ASIG);}
"|=" {printf("Linea %d, operador: %s\n", num_linea, yytext);return(OR_ASIG);}
"<<=" {printf("Linea %d, operador: %s\n", num_linea, yytext);return(DESPI_ASIG);}
">>=" {printf("Linea %d, operador: %s\n", num_linea, yytext);return(DESPD_ASIG);}
"<<" {printf("Linea %d, operador: %s\n", num_linea, yytext);return(DESPI);}
">>" {printf("Linea %d, operador: %s\n", num_linea, yytext);return (DESPD);}
"&" {printf("Linea %d, operador: %s\n", num_linea, yytext);return (yytext[0]);}
"^=" {printf("Linea %d, operador: %s\n", num_linea, yytext);return (XOR_ASIG);}
"|" {printf("Linea %d, operador: %s\n", num_linea, yytext);return (yytext[0]);}
"." {printf("Linea %d, operador: %s\n", num_linea, yytext);return (yytext[0]);}
"->" {printf("Linea %d, operador: %s\n", num_linea, yytext);return(PTR_ACCESO);}
"[" {printf("Linea %d, operador: %s\n", num_linea, yytext);return (yytext[0]);}
"]" {printf("Linea %d, operador: %s\n", num_linea, yytext);return (yytext[0]);}
"<=" {printf("Linea %d, operador: %s\n", num_linea, yytext);return(LE);}
">=" {printf("Linea %d, operador: %s\n", num_linea, yytext);return(GE);}
"<>" {printf("Linea %d, operador: %s\n", num_linea, yytext);return (NEQ);}
"<" {printf("Linea %d, operador: %s\n", num_linea, yytext);return (yytext[0]);}
">" {printf("Linea %d, operador: %s\n", num_linea, yytext);return (yytext[0]);}
"==" {printf("Linea %d, operador: %s\n", num_linea, yytext);return (EQ);}
"!" {printf("Linea %d, operador: %s\n", num_linea, yytext);return (yytext[0]);}
"&&" {printf("Linea %d, operador: %s\n", num_linea, yytext);return (AND);}
"||" {printf("Linea %d, operador: %s\n", num_linea, yytext);return (OR);}
"=" {printf("Linea %d, operador: %s\n", num_linea, yytext);return (yytext[0]);}
"#" {printf("Linea %d, operador: %s\n", num_linea, yytext);return (yytext[0]);}


"auto" {printf("Linea %d, palabra reservada: %s\n", num_linea, yytext);return (AUTO);}
"break" {printf("Linea %d, palabra reservada: %s\n", num_linea, yytext);return (BREAK);}
"case" {printf("Linea %d, palabra reservada: %s\n", num_linea, yytext);return (CASE);}
"char" {printf("Linea %d, palabra reservada: %s\n", num_linea, yytext);return (CHAR);}
"continue" {printf("Linea %d, palabra reservada: %s\n", num_linea, yytext);return (CONTINUE);}
"default" {printf("Linea %d, palabra reservada: %s\n", num_linea, yytext);return (DEFAULT);}
"define" {printf("Linea %d, palabra reservada: %s\n", num_linea, yytext);return (DEFINE);}
"do" {printf("Linea %d, palabra reservada: %s\n", num_linea, yytext);return (DO);}
"double" {printf("Linea %d, palabra reservada: %s\n", num_linea, yytext);return (DOUBLE);}
"else" {printf("Linea %d, palabra reservada: %s\n", num_linea, yytext);return (ELSE);}
"enum" {printf("Linea %d, palabra reservada: %s\n", num_linea, yytext);return (ENUM);}
"extern" {printf("Linea %d, palabra reservada: %s\n", num_linea, yytext);return (EXTERN);}
"float" {printf("Linea %d, palabra reservada: %s\n", num_linea, yytext);return (FLOAT);}
"for" {printf("Linea %d, palabra reservada: %s\n", num_linea, yytext);return (FOR);}
"goto" {printf("Linea %d, palabra reservada: %s\n", num_linea, yytext);return (GOTO);}
"if" {printf("Linea %d, palabra reservada: %s\n", num_linea, yytext);return (IF);}
"include" {printf("Linea %d, palabra reservada: %s\n", num_linea, yytext);return INCLUDE;}
"int" {printf("Linea %d, palabra reservada: %s\n", num_linea, yytext);return (INT);}
"long" {printf("Linea %d, palabra reservada: %s\n", num_linea, yytext);return (LONG);}
"register" {printf("Linea %d, palabra reservada: %s\n", num_linea, yytext);return (RETURN);}
"return" {printf("Linea %d, palabra reservada: %s\n", num_linea, yytext);return (RETURN);}
"short" {printf("Linea %d, palabra reservada: %s\n", num_linea, yytext);return (SHORT);}
"signed" {printf("Linea %d, palabra reservada: %s\n", num_linea, yytext);return (SIGNED);}
"static" {printf("Linea %d, palabra reservada: %s\n", num_linea, yytext);return (STATIC);}
"struct" {printf("Linea %d, palabra reservada: %s\n", num_linea, yytext);return (STRUCT);}
"switch" {printf("Linea %d, palabra reservada: %s\n", num_linea, yytext);return( SWITCH);}
"typedef" {printf("Linea %d, palabra reservada: %s\n", num_linea, yytext);return (TYPEDEF);}
"union" {printf("Linea %d, palabra reservada: %s\n", num_linea, yytext);return (UNION);}
"unsigned" {printf("Linea %d, palabra reservada: %s\n", num_linea, yytext);return (UNSIGNED);}
"void" {printf("Linea %d, palabra reservada: %s\n", num_linea, yytext);return (VOID);}
"while" {printf("Linea %d, palabra reservada: %s\n", num_linea, yytext);return (WHILE);}
"sizeof" {printf("Linea %d, operador: %s\n", num_linea, yytext);return (SIZEOF);}

{OCTALES} {printf("Linea %d, esto es una constante entera octal: %s\n",num_linea,yytext); return (ENTERO);}
{HEXADECIMALES} {printf("Linea %d, esto es una constante entera hexadecimal: %s\n",num_linea,yytext); return (ENTERO);}
{DECIMALES} {printf("Linea %d, esto es una constante entera decimal: %s\n",num_linea,yytext); return (ENTERO);}

{IDENTIFICADOR} {printf("Linea %d, esto es un IDENTIFICADOR: %s\n",num_linea,yytext);return (IDENTIFICADOR);}

{REAL} {printf("Linea %d, esto es una constante real: %s\n",num_linea,yytext); return (REAL);}
{DECIMAL_CON_MANTISA} {printf("Linea %d, esto es una constante real decimal con mantisa/exponente: %s\n",num_linea,yytext);
return (REAL);}
{HEXADECIMAL_CON_MANTISA} {printf("Linea %d, esto es una constante real hexadecimal con mantisa/exponente: %s\n",num_linea,yytext);
return (REAL);}

{CARACTERES_SIMPLES} {printf("Linea %d, esto es un caracter: %s\n",num_linea,yytext); return (CARACTER);}
{CARACTERES_ESCAPADOS} {printf("Linea %d, esto es un caracter: %s\n",num_linea,yytext); return (CARACTER);}
{CARACTERES_OCTALES} {printf("Linea %d, esto es un caracter: %s\n",num_linea,yytext); return (CARACTER);}
{CARACTERES_HEXADECIMALES} {printf("Linea %d, esto es un caracter: %s\n",num_linea,yytext); return (CARACTER);}

{CADENAS_SIN_SALTO} {printf("Linea %d, esto es una cadena: %s\n",num_linea,yytext); return (STRING);}

{DELIMITADOR} {printf("Linea %d, esto es un delimitador: %s\n",num_linea,yytext); return (yytext[0]);}

{PATH} {printf("Linea %d, esto es un path: %s\n",num_linea,yytext); return (PATH);}

{COMENTARIO_SIMPLE} ;

"/*"      {yymore(); BEGIN(comentarios);}
<comentarios>[^*\n\\]* {yymore();}
<comentarios>"*"+[^*/\n]* {yymore();}
<comentarios>\n  {num_linea++; yymore();}
<comentarios>"*"+"/"  {BEGIN(INITIAL);}

\"     {yymore(); printf("Linea %d: ",num_linea); BEGIN(cadenas);}
<cadenas>[^\n\"] {yymore();}
<cadenas>\n  {num_linea++; yymore();}
<cadenas>\"  {printf("esto es una cadena: %s\n",yytext); BEGIN(INITIAL);}

. printf("Linea %d, esto es un error: %s\n",num_linea,yytext);

%%
