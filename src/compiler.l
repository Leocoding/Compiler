%{

#include <limits.h>
#include <stdarg.h>
#include "ex1.tab.h"
void string_to_int(int *v, const char *s);  

%}

%option noyywrap

%%
[[:digit:]]+ { string_to_int(&yylval.entier, yytext); return NUMBER; }
[[:blank:]]
\n
"if"         { return IF; }
"else"       { return ELSE; }
"while"      { return WHILE; }
"print"      { return PRINT; }
"return"     { return RETURN; }
"int"			   { return INT; }
"bool"			 { return BOOL; }
"true" 			 { return TRUE; }
"false" 		 { return FALSE; }
"&&" 			   { return AND; }
"||" 			   { return OR; }
"!" 			   { return NEG; }
"==" 			   { return EQ; }
"!=" 			   { return NEQ; }
">=" 			   { return GEQ; }
"<=" 			   { return LEQ; }
">" 			   { return GT; }
"<" 			   { return LT; }
\/\/.*\n     { return COMMENT; }

([a-zA-Z])([a-zA-Z]|[0-9])* {	
  char *string = malloc(sizeof(strlen(yytext)) + 1);
  if (string == NULL) {
    fprintf(stderr, "Erreur d'allocation.\n");
    exit(EXIT_FAILURE);					
  }
  strcpy(string,yytext);
  yylval.chaine = string;
  return ID;
}

\"([^"]|\\\")*\" { 
  char *string = malloc(sizeof(strlen(yytext)) + 1);
  if (string == NULL) {
    fprintf(stderr, "Erreur d'allocation.\n");
    exit(EXIT_FAILURE);					
  }
  strcpy(string,yytext);
  yylval.chaine = string;
  return STRING; 
}

.       { return yytext[0]; }
%%

void string_to_int(int *r, const char *s) {
  char *p;
  long v;
  errno = 0;
  v = strtol(yytext, &p, 10);
  if ((*p != '\0' || (errno == ERANGE && (v == LONG_MIN || v == LONG_MAX))) 
          || (v < INT_MIN || v > INT_MAX)) {
    fprintf(stderr, "Error converting string to int\n");
    exit(EXIT_FAILURE);
  } 
  *r = v;
}
