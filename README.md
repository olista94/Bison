Analizador Sintactico de C--

$ bison -dv cminus2.y
$ flex cminus2.l
$ gcc -o cminus2 cminus2.tab.c lex.yy.c prueba.c--

o

$ make
$ ./sminus2 prueba-c--
