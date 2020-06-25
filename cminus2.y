%{
  #include <stdlib.h>
  #include <stdio.h>
  extern FILE *yyin;
  extern int num_linea;

  #define YYDEBUG 1

%}


%token IDENTIFICADOR ENTERO REAL STRING CARACTER SIZEOF PATH POTENCIA
%token PTR_ACCESO INC DEC DESPI DESPD LE GE EQ NEQ AND OR MULT_ASIG
%token DIV_ASIG MOD_ASIG SUMA_ASIG RESTA_ASIG DESPI_ASIG DESPD_ASIG
%token AND_ASIG XOR_ASIG OR_ASIG

%token INCLUDE DEFINE TYPEDEF EXTERN STATIC AUTO REGISTER CHAR SHORT
%token INT LONG SIGNED UNSIGNED FLOAT DOUBLE VOID STRUCT UNION ENUM

%token CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN

%%
/********************************************* PROGRAMA *********************************************/

//programa ::= [ bloque ]+

programa :
	varios_bloques { printf (" programa  -> varios_bloques \n"); }
	;

varios_bloques :
	bloque { printf (" varios_bloques  -> bloque \n"); }
	| varios_bloques bloque { printf (" varios_bloques  -> varios_bloques bloque \n"); }
	;
	
//bloque ::= definicion_funcion | declaracion | macros

bloque :
	definicion_funcion
	| declaracion
	| macros
	;
	
//definicion_funcion ::= [ declaracion_tipo [ '*' ]* ]? IDENTIFICADOR bloque_instrucciones

definicion_funcion :
	IDENTIFICADOR bloque_instrucciones { printf (" definicion_funcion  -> IDENTIFICADOR bloque_instrucciones \n"); }
	| declaracion_tipo IDENTIFICADOR bloque_instrucciones { printf(" definicion_funcion -> declaracion_tipo IDENTIFICADOR bloque_instrucciones \n");}
	| declaracion_tipo varios_asteriscos IDENTIFICADOR bloque_instrucciones { printf (" definicion_funcion  -> declaracion_tipo varios_asteriscos IDENTIFICADOR bloque_instrucciones \n"); }
	;

varios_asteriscos :
	'*' { printf (" varios_asteriscos -> * \n"); }
	| varios_asteriscos '*' { printf (" varios_asteriscos -> varios_asteriscos * \n"); }
	;
	
//macros ::= '#' 'include' PATH | '#' 'define' IDENTIFICADOR macro

macros :
	'#' INCLUDE PATH { printf (" macros -> '#' INCLUDE PATH \n"); }
	|'#' DEFINE IDENTIFICADOR macro { printf (" macros -> '#' DEFINE IDENTIFICADOR macro \n"); }
	;
	
//macro ::= ENTERO | REAL | STRING | CARACTER | instruccion

macro :
	ENTERO { printf (" macro -> ENTERO \n"); }
	| REAL { printf (" macro -> REAL \n"); }
	| STRING { printf (" macro -> STRING \n"); }
	| CARACTER { printf (" macro -> CARACTER \n"); }
	| instruccion { printf (" macro -> instruccion \n"); }
	;
	
/********************************************* DECLARACIONES *********************************************/
//declaracion ::= declaracion_tipo ( nombre )* [ '#' ]? ';'
//| 'typedef' declaracion_tipo IDENTIFICADOR ';'

declaracion :
	declaracion_tipo ';' { printf (" declaracion -> declaracion_tipo \n"); }
	| declaracion_tipo varios_nombres ';' { printf (" declaracion -> declaracion_tipo varios_nombres ';' \n"); }
	| declaracion_tipo varios_nombres '#' ';' { printf (" declaracion -> declaracion_tipo varios_nombres '#' ';' \n"); }
	| declaracion_tipo '#' ';' { printf (" declaracion -> declaracion_tipo varios_nombres '#' ';' \n"); }
	| TYPEDEF declaracion_tipo IDENTIFICADOR ';' { printf (" declaracion -> TYPEDEF declaracion_tipo IDENTIFICADOR ';' \n"); }
	;
	
varios_nombres :
	nombre { printf (" varios_nombres -> nombre \n"); };
	| varios_nombres ',' nombre { printf (" varios_nombres -> varios_nombres nombre \n"); };
	;
	
//declaracion_tipo ::= [ almacenamiento ]* tipo_basico_modificado
//| [ almacenamiento ]* definicion_struct_union
//| [ almacenamiento ]* definicion_enum

declaracion_tipo :
	tipo_basico_modificado { printf (" declaracion_tipo -> tipo_basico_modificado \n"); }
	| varios_almacenamiento tipo_basico_modificado { printf (" declaracion_tipo -> varios_almacenamiento varios_almacenamiento \n"); }
	| definicion_struct_union { printf (" declaracion_tipo -> definicion_struct_union \n"); }
	| varios_almacenamiento definicion_struct_union { printf (" declaracion_tipo -> varios_almacenamiento \n"); }
	| definicion_enum { printf (" declaracion_tipo -> definicion_enum \n"); }
	| varios_almacenamiento definicion_enum { printf (" declaracion_tipo -> varios_almacenamiento definicion_enum \n"); }
	;

varios_almacenamiento : 
	almacenamiento { printf (" varios_almacenamiento -> almacenamiento ';' \n"); }
	| varios_almacenamiento almacenamiento { printf (" varios_almacenamiento -> varios_almacenamiento almacenamiento ';' \n"); }
	;
	
//tipo_basico_modificado ::= [ signo ]? [ longitud ]? tipo_basico
//| '[' IDENTIFICADOR ']'

tipo_basico_modificado :
	tipo_basico { printf (" tipo_basico_modificado -> tipo_basico \n"); }
	| signo tipo_basico { printf (" tipo_basico_modificado -> signo tipo_basico \n"); }
	| longitud tipo_basico { printf (" tipo_basico_modificado ->  longitud tipo_basico \n"); }
	| signo longitud tipo_basico { printf (" tipo_basico_modificado -> signo longitud tipo_basico \n"); }
	| '[' IDENTIFICADOR ']' { printf (" tipo_basico_modificado -> '[' IDENTIFICADOR ']' \n"); }
	;

//almacenamiento ::= 'extern' | 'static' | 'auto' | 'register'

almacenamiento :
	EXTERN { printf (" almacenamiento -> EXTERN \n"); }
	| STATIC { printf (" almacenamiento -> STATIC \n"); }
	| AUTO { printf (" almacenamiento -> AUTO \n"); }
	| REGISTER { printf (" almacenamiento -> REGISTER \n"); }
	;

//longitud ::= 'short' | 'long'

longitud :
	SHORT { printf (" longitud -> SHORT  \n"); }
	| LONG { printf (" longitud -> LONG  \n"); }
	;

//signo ::= 'signed' | 'unsigned'

signo :
	SIGNED { printf (" signo -> SIGNED  \n"); }
	| UNSIGNED { printf (" signo -> UNSIGNED  \n"); }
	;

//tipo_basico ::= 'void' | 'char' | 'int' | 'float' | 'double'

tipo_basico :
	VOID { printf (" tipo_basico -> VOID \n"); }
	| CHAR { printf (" tipo_basico -> CHAR \n"); }
	| INT { printf (" tipo_basico -> INT \n"); }
	| FLOAT { printf (" tipo_basico -> FLOAT \n"); }
	| DOUBLE { printf (" tipo_basico -> DOUBLE \n"); }
	;
	
//definicion_struct_union ::= struct_union [ IDENTIFICADOR ]? '{' [ declaracion_struct ]+ '}'
//| struct_union IDENTIFICADOR

definicion_struct_union :
	struct_union '{' varios_declaracion_struct '}' { printf (" definicion_struct_union : struct_union '{' varios_declaracion_struct '}' \n"); }
	| struct_union IDENTIFICADOR '{' varios_declaracion_struct '}' { printf (" definicion_struct_union : struct_union IDENTIFICADOR '{' varios_declaracion_struct '}' \n"); }
	| struct_union IDENTIFICADOR { printf (" definicion_struct_union :struct_union IDENTIFICADOR \n"); }
	;
	
varios_declaracion_struct :
	declaracion_struct { printf (" varios_declaracion_struct -> declaracion_struct \n"); }
	| varios_declaracion_struct declaracion_struct { printf (" varios_declaracion_struct -> varios_declaracion_struct declaracion_struct \n"); }
	;

//struct_union ::= 'struct' | 'union'

struct_union :
	STRUCT { printf (" struct_union -> STRUCT \n"); } 
	| UNION { printf (" struct_union -> UNION \n"); } 
	;

//declaracion_struct ::= tipo_basico_modificado ( nombre )+ ';'
//| definicion_struct_union ( nombre )+ ';'

declaracion_struct :
	tipo_basico_modificado varios_nombres ';' { printf (" declaracion_struct -> tipo_basico_modificado varios_nombres \n"); }
	| definicion_struct_union varios_nombres ';' { printf (" declaracion_struct -> definicion_struct_union varios_nombres  \n"); }
	;
	
//nombre ::= dato [ '=' elementos ]?

nombre :
	dato  { printf (" nombre -> dato  \n"); }
	| dato '=' elementos { printf (" nombre -> dato '=' elementos  \n"); }
	;

//dato ::= [ '*' ]* IDENTIFICADOR [ '[' [ expresion ]? ']' ]*

dato :
	IDENTIFICADOR { printf (" dato -> IDENTIFICADOR \n"); }
	| IDENTIFICADOR varias_expresiones_corchetes { printf (" dato -> IDENTIFICADOR varias_expresiones_corchetes \n"); }
	| varios_asteriscos IDENTIFICADOR varias_expresiones_corchetes { printf (" dato -> varios_asteriscos IDENTIFICADOR varias_expresiones_corchetes \n"); }
	| varios_asteriscos IDENTIFICADOR { printf (" dato -> varios_asteriscos IDENTIFICADOR \n"); }
	;
	
varias_expresiones_corchetes:
	'[' ']' { printf (" varias_expresiones_corchetes -> '[' ']' \n"); }
	| '[' expresion ']' { printf (" varias_expresiones_corchetes -> '[' expresion ']' \n"); }
	| varias_expresiones_corchetes '[' ']' { printf (" varias_expresiones_corchetes -> varias_expresiones_corchetes '[' ']' \n"); }
	| varias_expresiones_corchetes '[' expresion ']' { printf (" varias_expresiones_corchetes -> varias_expresiones_corchetes '[' expresion ']' \n"); }
	; 

//elementos ::= expresion | '{' ( elementos )+ '}'

elementos :
	expresion { printf (" elementos -> expresion \n"); }
	| '{' varios_elementos '}' { printf (" elementos-> '{' varios_elementos '}' \n"); }
	;
	
varios_elementos :
	elementos { printf (" varios_elementos -> elementos \n"); }
	| varios_elementos ',' elementos { printf (" varios_elementos -> varios_elementos elementos \n"); }

//definicion_enum ::= 'enum' IDENTIFICADOR [ ':' tipo_basico_modificado ]? cuerpo_enum

definicion_enum :
	ENUM IDENTIFICADOR cuerpo_enum { printf (" definicion_enum -> ENUM IDENTIFICADOR cuerpo_enum \n"); }
	| ENUM IDENTIFICADOR ':' tipo_basico_modificado cuerpo_enum { printf (" definicion_enum -> ENUM IDENTIFICADOR ':' tipo_basico_modificado cuerpo_enum \n"); }
	;

//cuerpo_enum ::= '{' ( declaracion_miembro_enum )+ '}'

cuerpo_enum : 
	'{' varios_declaracion_miembro_enum '}' { printf (" cuerpo_enum->'{' varios_declaracion_miembro_enum '}' \n"); }
	;
	
varios_declaracion_miembro_enum :
	declaracion_miembro_enum { printf (" varios_declaracion_miembro_enum -> declaracion_miembro_enum \n"); }
	| varios_declaracion_miembro_enum ',' declaracion_miembro_enum { printf (" varios_declaracion_miembro_enum -> varios_declaracion_miembro_enum declaracion_miembro_enum \n"); }
	;

//declaracion_miembro_enum ::= IDENTIFICADOR [ '=' expresion ]?

declaracion_miembro_enum :
	IDENTIFICADOR { printf (" declaracion_miembro_enum -> IDENTIFICADOR \n"); }
	| IDENTIFICADOR '=' expresion { printf (" declaracion_miembro_enum -> IDENTIFICADOR '=' expresion  \n"); }
	;

/********************************************* INSTRUCCIONES *********************************************/

//instruccion ::= bloque_instrucciones
//| instruccion_expresion
//| instruccion_bifurcacion
//| instruccion_bucle
//| instruccion_salto
//| instruccion_destino_salto
//| instruccion_retorno
//| ';'

instruccion :
	bloque_instrucciones { printf (" instruccion -> bloque_instrucciones \n"); }
	| instruccion_expresion { printf (" instruccion -> instruccion_expresion \n"); }
	| instruccion_bifurcacion { printf (" instruccion -> instruccion_bifurcacion \n"); }
	| instruccion_bucle { printf (" instruccion -> instruccion_bucle \n"); }
	| instruccion_salto { printf (" instruccion -> instruccion_salto \n"); }
	| instruccion_destino_salto { printf (" instruccion -> instruccion_destino_salto \n"); }
	| instruccion_retorno { printf (" instruccion -> instruccion_retorno \n"); }
	| ';' { printf (" instruccion -> ';' \n"); }
	;

//bloque_instrucciones ::= '{' [ declaracion ]* [ instruccion ]* '}'

bloque_instrucciones :
	'{' '}' { printf (" bloque_instrucciones ->  '{' '}' \n"); }
	| '{' varias_declaraciones '}' { printf (" bloque_instrucciones ->  '{' varias_declaraciones '}' \n"); }
	| '{' varias_instrucciones '}' { printf (" bloque_instrucciones ->  '{' varias_instrucciones '}' \n"); }
	| '{' varias_declaraciones varias_instrucciones '}' { printf (" bloque_instrucciones ->  '{' varias_declaraciones varias_instrucciones '}' \n"); }
	;
	
varias_declaraciones :
	declaracion { printf (" varias_declaraciones ->  declaracion  \n"); }
	| varias_declaraciones declaracion { printf (" lista_declaraciones -> varias_declaraciones declaracion \n"); }
	;

varias_instrucciones :
	instruccion { printf (" varias_instrucciones ->  instruccion \n"); }
	| varias_instrucciones instruccion { printf (" varias_instrucciones ->  varias_instrucciones instruccion \n"); }
	;

//instruccion_expresion ::= expresion_funcional ';' | asignacion ';'

instruccion_expresion :
	expresion_funcional ';' { printf (" instruccion_expresion -> expresion_funcional ';' \n"); }
	| asignacion ';' { printf (" instruccion_expresion -> asignacion ';' \n"); }
	;

//asignacion ::= expresion_indexada operador_asignacion expresion

asignacion :
	expresion_indexada operador_asignacion expresion { printf (" asignacion -> expresion_indexada operador_asignacion expresion \n"); }
	;

//operador_asignacion ::= '=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '&=' | '^=' | '|='

operador_asignacion :
	'=' { printf (" operador_asignacion -> '=' \n"); }
	| MULT_ASIG { printf (" operador_asignacion -> MULT_ASIG \n"); }
	| DIV_ASIG { printf (" operador_asignacion -> DIV_ASIG \n"); }
	| MOD_ASIG { printf (" operador_asignacion -> MOD_ASIG \n"); }
	| SUMA_ASIG { printf (" operador_asignacion -> SUMA_ASIG \n"); }
	| RESTA_ASIG { printf (" operador_asignacion -> RESTA_ASIG \n"); }
	| DESPI_ASIG { printf (" operador_asignacion -> DESPI_ASIG \n"); }
	| DESPD_ASIG { printf (" operador_asignacion -> DESPD_ASIG \n"); }
	| AND_ASIG { printf (" operador_asignacion -> AND_ASIG \n"); }
	| XOR_ASIG { printf (" operador_asignacion -> XOR_ASIG \n"); }
	| OR_ASIG { printf (" operador_asignacion -> OR_ASIG \n"); }
	;

//instruccion_bifurcacion ::= 'if' '(' expresion ')' instruccion [ 'else' instruccion ]?
//| 'switch' '(' expresion ')' '{' [ instruccion_caso ]+ '}'

instruccion_bifurcacion :
	IF '(' expresion ')' instruccion { printf (" instruccion_bifurcacion -> IF '(' expresion ')' instruccion \n"); }
	| IF '(' expresion ')' instruccion ELSE instruccion { printf (" instruccion_bifurcacion -> IF '(' expresion ')' instruccion ELSE instruccion \n"); }
	| SWITCH '(' expresion ')' '{' varias_instrucciones_caso '}' { printf (" instruccion_bifurcacion -> SWITCH '(' expresion ')' '{' varias_instrucciones_caso '}' \n"); }
	;
	
varias_instrucciones_caso :
	instruccion_caso { printf (" varias_instrucciones_caso -> instruccion_caso \n"); }
	| varias_instrucciones_caso instruccion_caso { printf (" varias_instrucciones_caso .>  varias_instrucciones_caso instruccion_caso \n"); }
	;

//instruccion_caso ::= 'case' expresion ':' instruccion
//| 'default' ':' instruccion

instruccion_caso :
	CASE expresion ':' instruccion { printf (" instruccion_caso -> CASE expresion ':' instruccion  \n"); } 
	| DEFAULT ':' instruccion { printf (" instruccion_caso -> DEFAULT ':' instruccion \n"); } 
	;
	
//instruccion_bucle ::= 'while' '(' expresion ')' instruccion
//| 'do' instruccion 'while' '( expresion ')' ';'
//| 'for' '(' ( definicion_asignacion )* ';' expresion ';' expresion ')' instruccion
//| 'for' '(' [ declaracion_tipo [ '*' ]* ]? IDENTIFICADOR ';' expresion ';' sentido ')' instruccion

instruccion_bucle :
	WHILE '(' expresion ')' instruccion { printf (" instruccion_bucle -> WHILE '('expresion ')' instruccion \n"); } 
	| DO instruccion WHILE '(' expresion ')' ';' { printf (" instruccion_bucle -> DO instruccion WHILE '(' expresion ')' ';' \n"); }
	| FOR '(' ';' expresion ';' expresion ')' instruccion { printf (" instruccion_bucle -> FOR '(' ';' expresion ';' expresion ')' instruccion \n"); }
	| FOR '(' varias_definiciones_asignacion ';' expresion ';' expresion ')' instruccion { printf (" instruccion_bucle -> FOR '(' varias_definiciones_asignacion ';' expresion ';' expresion ')' instruccion \n"); }
	| FOR '(' IDENTIFICADOR ';' expresion ';' sentido ')' instruccion { printf (" instruccion_bucle -> FFOR '(' IDENTIFICADOR ';' expresion ';' sentido ')' instruccion \n"); }
	| FOR '(' declaracion_tipo IDENTIFICADOR ';' expresion ';' sentido ')' instruccion { printf (" instruccion_bucle -> FFOR '(' declaracion_tipo IDENTIFICADOR ';' expresion ';' sentido ')' instruccion \n"); }
	| FOR '(' declaracion_tipo varios_asteriscos IDENTIFICADOR ';' expresion ';' sentido ')' instruccion { printf (" instruccion_bucle -> FFOR '(' declaracion_tipo varios_asteriscos IDENTIFICADOR ';' expresion ';' sentido ')' instruccion \n"); }
	;

varias_definiciones_asignacion :
	definicion_asignacion { printf (" varias_definiciones_asignacion -> definicion_asignacion  \n"); } 
	| varias_definiciones_asignacion ',' definicion_asignacion { printf (" varias_definiciones_asignacion -> varias_definiciones_asignacion definicion_asignacion \n"); } 
	;

//definicion_asignacion ::= asignacion
//| declaracion_tipo [ '*' ]* expresion_indexada '=' expresion

definicion_asignacion :
	asignacion { printf (" definicion_asignacion -> asignacion  \n"); } 
	| declaracion_tipo expresion_indexada '=' expresion { printf (" definicion_asignacion -> declaracion_tipo expresion_indexada '=' expresion \n"); } 
	| declaracion_tipo varios_asteriscos expresion_indexada '=' expresion { printf (" definicion_asignacion -> declaracion_tipo varios_asteriscos expresion_indexada '=' expresion \n"); } 
	;
	
//sentido ::= '--' | '++'

sentido :
	DEC { printf (" sentido  -> DEC\n"); }
	| INC { printf (" sentido  -> INC\n"); }
	;

//instruccion_salto ::= 'goto' IDENTIFICADOR ';' | 'continue' ';' | 'break' ';'

instruccion_salto :
	GOTO IDENTIFICADOR ';' { printf (" instruccion_salto  -> GOTO IDENTIFICADOR ';'\n"); }
	| CONTINUE ';' { printf (" instruccion_salto  -> CONTINUE ';'\n"); }
	| BREAK ';' { printf (" instruccion_salto  -> BREAK ';'\n"); }
	;

//instruccion_destino_salto ::= IDENTIFICADOR ':' instruccion ';'

instruccion_destino_salto :
	IDENTIFICADOR ':' instruccion ';' { printf (" instruccion_destino_salto  -> IDENTIFICADOR ':' instruccion ';'\n"); }
	;

//instruccion_retorno ::= 'return' [ expresion ]? ';'

instruccion_retorno :
	RETURN ';' { printf (" instruccion_retorno  -> RETURN ';'\n"); }
	| RETURN expresion ';' { printf (" instruccion_retorno  -> RETURN expresion ';'\n"); }
	;

/**********************************EXPRESIONES*****************************/

//expresion_constante ::= ENTERO | REAL | STRING | CARACTER

expresion_constante :
	ENTERO { printf (" expresion_constante  -> ENTERO\n"); }
	| REAL { printf (" expresion_constante  -> REAL\n"); }
	| STRING { printf (" expresion_constante  -> STRING\n"); }
	| CARACTER { printf (" expresion_constante  -> CARACTER\n"); }
	;

//expresion_parentesis ::= '(' expresion ')'

expresion_parentesis :
	'(' expresion ')' { printf (" expresion_parentesis  -> '(' expresion ')'\n"); }
	;

//expresion_funcional ::= IDENTIFICADOR '(' ( expresion )* ')'

expresion_funcional :
	IDENTIFICADOR '(' ')' { printf (" expresion_funcional  -> IDENTIFICADOR '(' ')'\n"); }
	| IDENTIFICADOR '(' varias_expresiones ')' { printf (" expresion_funcional  -> IDENTIFICADOR '(' varias_expresiones ')'\n"); }
	;
	
varias_expresiones :
	expresion { printf (" varias_expresiones  -> expresion\n"); }
	| varias_expresiones ',' expresion { printf (" varias_expresiones  -> varias_expresiones expresion\n"); }
	;

//expresion_indexada ::= IDENTIFICADOR
//| expresion_indexada '[' expresion ']'
//| expresion_indexada '.' IDENTIFICADOR
//| expresion_indexada '->' IDENTIFICADOR

expresion_indexada :
	IDENTIFICADOR { printf (" expresion_indexada  -> IDENTIFICADOR\n"); }
	| expresion_indexada '[' expresion ']' { printf (" expresion_indexada  -> expresion_indexada '[' expresion ']'\n"); }
	| expresion_indexada '.' IDENTIFICADOR { printf (" expresion_indexada  -> expresion_indexada '.' IDENTIFICADOR\n"); }
	| expresion_indexada PTR_ACCESO IDENTIFICADOR { printf (" expresion_indexada  -> expresion_indexada PTR_ACCESO IDENTIFICADOR\n"); }
	;
	
//expresion_postfija ::= expresion_constante
//| expresion_parentesis
//| expresion_funcional
//| expresion_indexada
//| expresion_postfija '++'
//| expresion_postfija '--'

expresion_postfija :
	expresion_constante { printf (" expresion_postfija  -> expresion_constante\n"); }
	| expresion_parentesis { printf (" expresion_postfija  -> expresion_parentesis\n"); }
	| expresion_funcional { printf (" expresion_postfija  -> expresion_funcional\n"); }
	| expresion_indexada { printf (" expresion_postfija  -> expresion_indexada\n"); }
	| expresion_postfija INC { printf (" expresion_postfija  -> expresion_postfija INC\n"); }
	| expresion_postfija DEC { printf (" expresion_postfija  -> expresion_postfija DEC\n"); }
	;
	
//expresion_prefija ::= expresion_postfija
//| 'sizeof' expresion_prefija
//| 'sizeof' '(' nombre_tipo ')'
//| operador_unario expresion_cast

expresion_prefija :
	expresion_postfija { printf (" expresion_prefija  -> expresion_postfija\n"); }
	| SIZEOF expresion_prefija { printf (" expresion_prefija  -> SIZEOF expresion_prefija\n"); }
	| SIZEOF '(' nombre_tipo ')' { printf (" expresion_prefija  -> SIZEOF '(' nombre_tipo ')'\n"); }
	| operador_unario expresion_cast { printf (" expresion_prefija  -> operador_unario expresion_cast\n"); }
	;

//operador_unario ::= '++' | '--' | '&' | '*' | '+' | '-' | '~' | '!'

operador_unario :
	INC { printf (" operador_unario  -> INC\n"); }
	| DEC { printf (" operador_unario  -> DEC\n"); }
	| '&' { printf (" operador_unario  -> &\n"); }
	| '*' { printf (" operador_unario  -> *\n"); }
	| '+' { printf (" operador_unario  -> +\n"); }
	| '-' { printf (" operador_unario  -> -\n"); }
	| '~' { printf (" operador_unario  -> ~\n"); }
	| '!' { printf (" operador_unario  -> !\n"); }
	;
	
//expresion_cast ::= expresion_prefija | '(' nombre_tipo ')' expresion_prefija

expresion_cast :
	expresion_prefija { printf (" expresion_cast  -> expresion_prefija\n"); }
	| '(' nombre_tipo ')' expresion_prefija { printf (" expresion_cast  -> '(' nombre_tipo ')' expresion_prefija\n"); }
	;

//nombre_tipo ::= tipo_basico_modificado [ '*' ]*

nombre_tipo :
	tipo_basico_modificado { printf (" nombre_tipo  -> tipo_basico_modificado\n"); }
	| tipo_basico_modificado varios_asteriscos { printf (" nombre_tipo  -> tipo_basico_modificado varios_asteriscos\n"); }
	;
	
expresion_or_logico :
	expresion_or_logico OR expresion_and_logico  { printf (" expresion_or_logico  -> expresion_or_logico OR expresion_and_logico\n"); }
	| expresion_and_logico  { printf (" expresion_or_logico  -> expresion_and_logico\n"); }
	;
	
expresion_and_logico :
	expresion_and_logico AND expresion_igual_distinto { printf (" expresion_and_logico  -> expresion_and_logico AND expresion_igual_distinto\n"); }
	| expresion_igual_distinto { printf (" expresion_and_logico  -> expresion_igual_distinto\n"); }
	;
	
expresion_igual_distinto :
	expresion_igual_distinto EQ expresion_menor_mayor { printf (" expresion_igual_distinto  -> expresion_igual_distinto EQ expresion_menor_mayor\n"); }
	| expresion_igual_distinto NEQ expresion_menor_mayor { printf (" expresion_igual_distinto  -> expresion_igual_distinto NEQ expresion_menor_mayor\n"); }
	| expresion_menor_mayor { printf (" expresion_igual_distinto  -> expresion_menor_mayor\n"); }
	;
	
expresion_menor_mayor :
	expresion_menor_mayor '<' expresion_or_binario { printf (" expresion_menor_mayor  -> expresion_menor_mayor '<' expresion_or_binario\n"); }
	| expresion_menor_mayor '>' expresion_or_binario { printf (" expresion_menor_mayor  -> expresion_menor_mayor '>' expresion_or_binario\n"); }
	| expresion_menor_mayor LE expresion_or_binario { printf (" expresion_menor_mayor  -> expresion_menor_mayor LE expresion_or_binario\n"); }
	| expresion_menor_mayor GE expresion_or_binario { printf (" expresion_menor_mayor  -> expresion_menor_mayor GE expresion_or_binario\n"); }
	| expresion_or_binario { printf (" expresion_menor_mayor  -> expresion_or_binario\n"); }
	;
	
expresion_or_binario :
	expresion_or_binario '|' expresion_xor_binario { printf (" expresion_or_binario  -> expresion_or_binario '|' expresion_xor_binario\n"); }
	| expresion_xor_binario { printf (" expresion_or_binario  -> expresion_xor_binario\n"); }
	;
	
expresion_xor_binario :
	expresion_xor_binario '^' expresion_and_binario { printf (" expresion_xor_binario  -> expresion_xor_binario '^' expresion_and_binario\n"); }
	| expresion_and_binario { printf (" expresion_xor_binario  -> expresion_and_binario\n"); }
	;
	
expresion_and_binario :
	expresion_and_binario '&' expresion_operadores_desplazamiento { printf (" expresion_and_binario  -> expresion_and_binario '&' expresion_operadores_desplazamiento\n"); }
	| expresion_operadores_desplazamiento { printf (" expresion_and_binario  -> expresion_operadores_desplazamiento\n"); }
	;
	
expresion_operadores_desplazamiento :
	expresion_operadores_desplazamiento DESPI expresion_suma_resta { printf (" expresion_operadores_desplazamiento  -> expresion_operadores_desplazamiento DESPI expresion_suma_resta\n"); }
	| expresion_operadores_desplazamiento DESPD expresion_suma_resta { printf (" expresion_operadores_desplazamiento  -> expresion_operadores_desplazamiento DESPD expresion_suma_resta\n"); }
	| expresion_suma_resta { printf (" expresion_operadores_desplazamiento  -> expresion_suma_resta\n"); }
	;
	
expresion_suma_resta :
	expresion_suma_resta '+' expresion_mult_div_mod { printf (" expresion_suma_resta  -> expresion_suma_resta '+' expresion_mult_div_mod\n"); }
	| expresion_suma_resta '-' expresion_mult_div_mod { printf (" expresion_suma_resta  -> expresion_suma_resta '-' expresion_mult_div_mod\n"); }
	| expresion_mult_div_mod { printf (" expresion_suma_resta  -> expresion_mult_div_mod\n"); }
	;
	
expresion_mult_div_mod :
	expresion_mult_div_mod '*' expresion_potencia { printf (" expresion_mult_div_mod  -> expresion_mult_div_mod '*' expresion_potencia\n"); }
	| expresion_mult_div_mod '/' expresion_potencia { printf (" expresion_mult_div_mod  -> expresion_mult_div_mod '/' expresion_potencia\n"); }
	| expresion_mult_div_mod '%' expresion_potencia { printf (" expresion_mult_div_mod  -> expresion_mult_div_mod '%' expresion_potencia\n"); }
	| expresion_potencia { printf (" expresion_mult_div_mod  -> expresion_potencia\n"); }
	;
	
expresion_potencia :
	expresion_cast POTENCIA expresion_potencia { printf (" expresion_potencia  -> expresion_cast POTENCIA expresion_potencia\n"); }
	| expresion_cast { printf (" expresion_potencia  -> expresion_cast\n"); }
	;

expresion_logica :
	expresion_or_logico { printf (" expresion_logica  -> expresion_or_logico\n"); }
	;
	
//expresion ::= expresion_logica [ '?' expresion ':' expresion ]?

expresion :
	expresion_logica { printf (" expresion  -> expresion_logica\n"); }
	| expresion_logica '?' expresion ':' expresion { printf (" expresion  -> expresion_logica '?' expresion ':' expresion\n"); }
	;

%%


int yyerror(char *s) {
  fflush(stdout);
  printf("Error linea %d, %s\n", num_linea,s);
  }

int yywrap() {
  return(1);
  }

int main(int argc, char *argv[]) {

  yydebug = 0;

  if (argc < 2) {
    printf("Uso: ./c2minus NombreArchivo\n");
    }
  else {
    yyin = fopen(argv[1],"r");
    yyparse();
    }

  }
