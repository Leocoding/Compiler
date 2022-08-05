%{

#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <stdarg.h>
#include <stdbool.h>
#include <string.h>
#include <errno.h>
#include "types.h"
#include "stable.h"

// taille buffer pour les labels
#define MAXBUF 200

// compteur de variable locales dans la fonction courante
size_t nb_var_loc = 0;
// pointeur sur la fonction courante
symbol_table_entry *current_function = NULL;
// pointeur sur la fonction appelé
symbol_table_entry *called_function = NULL;
// booleen de gestion de valeur de retour du main
bool return_value_main = false;

// Enumération des types (gestion du typage)
enum {
  T_INT, T_BOOL, T_STRING, ERR_TYPE
};

// Déclaration des fonctions C

int yylex(void);
void yyerror(char const *);
void fail_with(const char *format, ...);
static unsigned int new_label_number();
static void create_label(char *buf, size_t buf_size, const char *format, ...);

%}
// Union pour gérer les chaines et nombre (token : NUMBER, STRING, ID)
%union {
  int entier;
  char *chaine;
} 

// prioritées
%precedence ')'
%precedence NON_ELSE
%precedence ELSE
%left NEG
%left NEQ EQ LT GT GEQ LEQ
%left OR
%left AND
%left '+' '-'
%left '*' '/'

// typage production
%type<entier> NUMBER AND OR NEG GEQ GT LEQ LT TRUE FALSE INT BOOL expr decl type instr RETURN avinstr whileexpr blockinstr sparams spparams param
%type<chaine> ID STRING

%start lignes

// mots clé dans les production (et non du langage)
%token NUMBER AND OR NEG EQ NEQ GEQ GT LEQ LT TRUE FALSE INT BOOL ID IF ELSE WHILE PRINT NON_ELSE RETURN COMMENT STRING

%%

// Règles pour les lignes 
lignes:
  lignes decl
  | lignes comment
  | lignes deffun
  | deffun
  | decl
  | instr
  | comment
  | lignes instr
;

// Règle commentaires
comment:
  COMMENT
;    
       
// Non terminal (opération à effectuer dans le if avant l'instruction)
avinstr : %empty {
            int n = new_label_number();
            $$ = n;
            printf("\tpop ax\n"
                    "\tconst bx,0\n"
                    "\tconst cx,if:faux:%d\n"
                    "\tcmp ax,bx\n"
                    "\tjmpz cx\n"
            , n); 
          }
;

// Non terminal (opération à effectuer pour le while en amont)
whileexpr : %empty { 
              int n = new_label_number();
              $$ = n;
              printf(":while:expr:%d\n", n);  
            }
;

// Non terminaux qui gerent les suites d'instructions
sinstrs:
  %empty
  | spinstrs
;

spinstrs:
  instr
  | spinstrs instr
;

// Règles des instructions
instr:

  // instruction vide 
  expr ';' 
  {
    printf("\tpop ax\n");
    if ($1 == T_INT) {
      $$ = T_INT;
    } else if ($1 == T_BOOL) {
      $$ = T_BOOL;
    } else {
      fail_with("Error on type for empty instruction");
    }
  }
  
  // instruction d'affichage d'expression
  | PRINT expr ';' 
  {
    if ($2 == T_INT) {
      printf("\tcp ax,sp\n"
            "\tcallprintfd ax\n"
            "\tpop ax\n");
      $$ = T_INT;
    } else if ($2 == T_BOOL) {
      unsigned int n = new_label_number();
      printf("\tconst cx,chaine:vrai\n"
            "\tconst dx,faux:%d\n"
            "\tconst bx,0\n"
            "\tpop ax\n"
            "\tcmp ax,bx\n"
            "\tjmpc dx\n"
            ":vrai:%d\n"
            "\tcallprintfs cx\n"
            "\tconst dx,end:%d\n"
            "\tjmp dx\n"
            ":faux:%d\n"
            "\tconst cx,chaine:faux\n"
            "\tcallprintfs cx\n"
            ":end:%d\n" , n, n, n, n, n);
      $$ = T_BOOL;
    } else if ($2 == T_STRING) {
      printf("\tpop ax\n"
            "\tcallprintfs ax\n");
      $$ = T_STRING;
    } else {
      fail_with("Error on type");
    }
  }    
  
  // réafectation d'une valeur à une variable
  | ID '=' expr ';' 
  {
    symbol_table_entry *ste = search_symbol_table($1);
    if (ste == NULL) {
      fail_with("Not an existing symbol");
    } else {
      if (ste->desc[0] == INT_T && $3 == T_INT) {
        $$ = T_INT;
      } else if (ste->desc[0] == BOOL_T && $3 == T_BOOL) {
        $$ = T_BOOL;
      } else { 
        fail_with("Type error on %s", $1);
      }
      if (ste->class == GLOBAL_VARIABLE){
        printf("\tconst ax,var:%s\n"
              "\tpop bx\n"
              "\tstorew bx,ax\n", $1);
      } else if (ste->class == LOCAL_VARIABLE) {
        printf("\tcp bx,bp\n"
              "\tconst cx,%zu\n"
              "\tconst dx,%zu\n"
              "\tadd cx,dx\n"
              "\tconst dx,1\n"
              "\tadd cx,dx\n"
              "\tconst dx,%zu\n"
              "\tsub cx,dx\n"
              "\tconst dx,2\n"
              "\tmul cx,dx\n"
              "\tsub bx,cx\n"
              "\tpop ax\n"
              "\tstorew ax,bx\n"
        ,current_function->nParams, current_function->nLocalVariables, ste->add + 1 - current_function->nParams); // AVERIFIER !!!!!!!!
      } else {
        fail_with("Fatal error : not a local or global variable");
      }
    }
  }
  
  // if forme simple
  | IF '(' expr ')' avinstr blockinstr %prec NON_ELSE 
  {
    printf(":if:faux:%d\n", $5);
    if ($3 == T_BOOL) {
      $$ = T_BOOL;
    } else {
      fail_with("Type error in if condition");
    }
  }
  
  // if/else 
  | IF '(' expr ')' avinstr blockinstr ELSE
  { 
    printf("\tconst bx,if:fin:%d\n"
          "\tjmp bx\n"
          ":if:faux:%d\n", $5, $5); 
  } blockinstr 
  {
    printf(":if:fin:%d\n", $5);
    if ($3 == T_BOOL) {
      $$ = T_BOOL;
    } else {
      fail_with("Type error in if condition");
    }
  }
  
  // boucle while
  | WHILE whileexpr '(' expr ')' 
  {
    printf("\tpop ax\n"
          "\tconst bx,0\n"
          "\tconst cx,while:fin:%d\n"
          "\tcmp ax,bx\n"
          "\tjmpc cx\n", $2);
  } blockinstr 
  {
    printf("\tconst dx,while:expr:%d\n"
          "\tjmp dx\n"
          ":while:fin:%d\n", $2, $2);
    if ($4 == T_BOOL) {
      $$ = T_BOOL;
    } else {
      fail_with("Type error in while condition");
    }
  }
  
  // retour de fonction (vide)
  | RETURN ';' 
  {
    if(current_function == NULL) {
      fail_with("Return statement out of function");
    }
    printf("\tpop dx\n"
          "\tpop cx\n"
          "\tpop bx\n"
          "\tpop bp\n"
          "\tret\n");
  }
  
  // retour de valeur d'une fonction
  | RETURN expr ';' 
  {
    if(current_function == NULL) {
      fail_with("Return statement out of function");
    } else if ($2 == T_INT && current_function->desc[0] != INT_T) {
      fail_with("Type error on return type");
    } else if ($2 == T_BOOL && current_function->desc[0] != BOOL_T) {
      fail_with("Type error on return type");
    }
    if(strcmp(current_function->name, "main") == 0) {
      return_value_main = true;
    }
    printf("\tpop ax\n"
          "\tpop dx\n"
          "\tpop cx\n"
          "\tpop bx\n"
          "\tpop bp\n"
          "\tret\n");
  }
  
  // block d'instructions
  | blockinstr
;

// Règles d'un block d'instructions
blockinstr:
  '{' sdecls 
  {
    size_t paramNb = current_function->nParams;
    size_t localNb = nb_var_loc;
    size_t curr_index = localNb;
    for (size_t i = 0; i < localNb; ++i) {
      printf("\tpop ax\n"
            "\tcp bx,bp\n"
            "\tconst cx,%zu\n"
            "\tconst dx,%zu\n"
            "\tadd cx,dx\n"
            "\tconst dx,1\n"
            "\tadd cx,dx\n"
            "\tconst dx,%zu\n"
            "\tsub cx,dx\n"
            "\tconst dx,2\n"
            "\tmul cx,dx\n"
            "\tsub bx,cx\n"
            "\tstorew ax,bx\n", paramNb, localNb, curr_index);
            --curr_index;
    }
  } sinstrs '}' 
  {
    if (current_function != NULL) {
      current_function->nLocalVariables = nb_var_loc;
    }
    for(int i = nb_var_loc; i > 0; --i) {
      free_first_symbol_table_entry();
    }
    nb_var_loc = 0;
  }
;


// Non terminaux qui gerent les suites de déclarations
sdecls: 
  %empty 
  | spdecls 
;

spdecls:
  decl {++nb_var_loc;}
  | spdecls decl {++nb_var_loc;}
;

// Règle des déclarations
decl:
  type ID '=' expr ';' 
  {
    symbol_table_entry *ste = search_symbol_table($2);
    if (ste != NULL && current_function == NULL) {
      fail_with("Symbol already exist");
    }
    symbol_table_entry *new = new_symbol_table_entry($2);
    if ($1 == T_INT && $4 == T_INT) {
      new->desc[0] = INT_T;
      $$ = T_INT;
    } else if ($1 == T_BOOL && $4 == T_BOOL) {
      new->desc[0] = BOOL_T;
      $$ = T_BOOL;
    } else {
      fail_with("Type error");
    }
    if(current_function == NULL) {
      new->class = GLOBAL_VARIABLE;
      char buf1[MAXBUF];
      char buf2[MAXBUF];
      create_label(buf1, MAXBUF, "%s:%s", "var", $2);
      create_label(buf2, MAXBUF, "%s:%s", "apres:var", $2);
      printf("\tconst ax,%s\n"
          "\tjmp ax\n"
          ":%s\n"
          "@int 0\n"
          ":%s\n"
          "\tpop ax\n"
          "\tconst bx,%s\n"
          "\tstorew ax,bx\n", buf2, buf1, buf2, buf1);
    } else {
      new->class = LOCAL_VARIABLE;
      new->add = nb_var_loc;
      //printf("\tconst ax,%d\n"
      //      "\tpush ax\n", $4);
    }
  }
;

// Règle des definitions de fonction
deffun:
  type ID 
  { 
    if (current_function != NULL) {
      fail_with("Already in function definition");
    }
    symbol_table_entry *ste = search_symbol_table($2);
    if (ste != NULL) {
      fail_with("Symbol already exist");
    } else {
      symbol_table_entry *new = new_symbol_table_entry($2);
      current_function = new;
      new->class = FUNCTION;
      new->nParams = 0;
      new->nLocalVariables = 0;
      if ($1 == T_INT) {
        new->desc[0] = INT_T;
      } else if ($1 == T_BOOL) {
        new->desc[0] = BOOL_T;
      } else {
        fail_with("Type error 0");
      }
    }
    printf("\tconst ax,function:%s:fin\n"
          "\tjmp ax\n"
          ":function:%s\n"
          "\tpush bp\n"
          "\tcp bp,sp\n"
          "\tpush bx\n"
          "\tpush cx\n"
          "\tpush dx\n", $2, $2); 
  } '(' sparams ')' blockinstr 
  {
    printf(":function:%s:fin\n", $2);
    for (int i = current_function->nParams; i > 0; --i) {
      free_first_symbol_table_entry();
    }
    current_function = NULL;
  }
;

// Non terminaux qui gerent les suites de parametres
sparams:
  %empty {$$ = 0;}
  | spparams {$$ = $1;}
;

spparams:
  param {$$ = $1;}
  | spparams ',' param {$$ = $3;} 

// Règle des paramètres
param:
  type ID 
  {
    symbol_table_entry *new_param = new_symbol_table_entry($2);
    new_param->class = LOCAL_VARIABLE;
    new_param->add = current_function->nParams;
    if ($1 == T_INT) {
      new_param->desc[0] = INT_T;
    } else if ($1 == T_BOOL){
      new_param->desc[0] = BOOL_T;
    } else {
      fail_with("Type error on parametre(s) : %s", $2);
    }
    current_function->nParams++;
    current_function->desc[current_function->nParams] = $1;
    $$ = current_function->nParams;
  }
;

// Types accéptes pour les parametres, variables, fonctions...
type:
  INT { $$ = T_INT; } 
  | BOOL {$$ = T_BOOL; }
;

// Règles des expressions
expr:

  // addition
  expr '+' expr  
  {
    if ($1 == T_INT && $3 == T_INT) {
      printf("\tpop ax\n"
        "\tpop bx\n"
        "\tadd bx,ax\n"
        "\tpush bx\n");
      $$ = T_INT;
    } else {
      $$ = ERR_TYPE;
    }
  }
  
  // soustraction
  | expr '-' expr  
  {
    if ($1 == T_INT && $3 == T_INT) {
      printf("\tpop ax\n"
        "\tpop bx\n"
        "\tsub bx,ax\n"
        "\tpush bx\n"
        );
      $$ = T_INT;
    } else {
      $$ = ERR_TYPE;
    }
  }
  
  // multiplication
  | expr '*' expr    
  {
    if ($1 == T_INT && $3 == T_INT) {
      printf("\tpop ax\n"
          "\tpop bx\n"
          "\tmul bx,ax\n"
          "\tpush bx\n");
      $$ = T_INT;
    } else {
      $$ = ERR_TYPE;
    }
  }
  
  // division
  | expr '/' expr  
  {
    if ($1 == T_INT && $3 == T_INT) {
      printf("\tpop ax\n"
          "\tpop bx\n"
          "\tconst cx,error:div0\n"
          "\tdiv bx,ax\n"
          "\tjmpe cx\n"
          "\tpush bx\n"
          "\tnop\n");
      $$ = T_INT;
    } else {
      $$ = ERR_TYPE;
    }
  }
  
  // Nombres
  | NUMBER
  {
    printf("\tconst ax,%d\n"
        "\tpush ax\n", $1);
    $$ = T_INT;
  }
  
  // négation 
  | NEG expr      
  {
    if ($2 == T_BOOL) {
      char buf1[MAXBUF];
      char buf2[MAXBUF];
      unsigned ln = new_label_number();
      create_label(buf1, MAXBUF, "%s:%u:%s", "case", ln, "n0");
      create_label(buf2, MAXBUF, "%s:%u:%s", "case", ln, "end");
      printf("\tpop ax\n"
            "\tconst bx,0\n"
            "\tpush bx\n"
            "\tconst cx,%s\n"
            "\tcmp bx,ax\n"
            "\tjmpc cx\n"
            "\tconst cx,%s\n"
            "\tjmp cx\n"
            ":%s\n"
            "\tpop bx\n"
            "\tconst ax,1\n"
            "\tpush ax\n"
            ":%s\n"
            "\tnop\n", buf1, buf2, buf1, buf2);
      $$ = T_BOOL;
    } else {
      $$ = ERR_TYPE;
    }
  }
  
  // parenthesage
  | '(' expr ')' 
  {
    if ($2 == T_BOOL) {
      $$ = T_BOOL;
    } else if ($2 == T_INT) {
      $$ = T_INT;
    } else {
      $$ = ERR_TYPE;
    }
  }
  
  // ET logique (&&)
  | expr AND expr 
  {
    if ($1 == T_BOOL && $3 == T_BOOL) {
      printf("\tpop ax\n"
          "\tpop bx\n"
          "\tmul bx,ax\n"
          "\tpush bx\n");
      $$ = T_BOOL;
    } else {
      $$ = ERR_TYPE;
    }
  }
  
  // OU logique (||)
  | expr OR expr {
    if ($1 == T_BOOL && $3 == T_BOOL) {
      printf("\tpop ax\n"
          "\tpop bx\n"
          "\tadd bx,ax\n"
          "\tpush bx\n");
      $$ = T_BOOL;
    } else {
      $$ = ERR_TYPE;
    }
  }
  
  // egalité
  | expr EQ expr   {
    if (($1 == T_INT && $3 == T_INT) || ($1 == T_BOOL && $3 == T_BOOL)) {
      char buf1[MAXBUF];
      char buf2[MAXBUF];
      unsigned ln = new_label_number();
      create_label(buf1, MAXBUF, "%s:%u:%s", "case", ln, "n0");
      create_label(buf2, MAXBUF, "%s:%u:%s", "case", ln, "end");
      printf("\tpop ax\n"
          "\tpop bx\n"
          "\tconst cx,0\n"
          "\tpush cx\n"
          "\tconst cx,%s\n"
          "\tcmp bx,ax\n"
          "\tjmpc cx\n"
          "\tconst cx,%s\n"
          "\tjmp cx\n"
          ":%s\n"
          "\tpop bx\n"
          "\tconst ax,1\n"
          "\tpush ax\n"
          ":%s\n"
          "\tnop\n", buf1, buf2, buf1, buf2);
      $$ = T_BOOL;
    } else {
      $$ = ERR_TYPE;
    }
  }
  
  // non egalité
  | expr NEQ expr {
    if (($1 == T_INT && $3 == T_INT) || ($1 == T_BOOL && $3 == T_BOOL)) {
      char buf1[MAXBUF];
      char buf2[MAXBUF];
      unsigned ln = new_label_number();
      create_label(buf1, MAXBUF, "%s:%u:%s", "case", ln, "n0");
      create_label(buf2, MAXBUF, "%s:%u:%s", "case", ln, "end");
      printf("\tpop ax\n"
          "\tpop bx\n"
          "\tconst cx,0\n"
          "\tpush cx\n"
          "\tconst cx,%s\n"
          "\tcmp bx,ax\n"
          "\tjmpc cx\n"
          "\tconst cx,%s\n"
          "\tjmp cx\n"
          ":%s\n"
          "\tpop bx\n"
          "\tconst ax,1\n"
          "\tpush ax\n"
          ":%s\n"
          "\tnop\n", buf1, buf2, buf1, buf2);
      $$ = T_BOOL;
    } else {
      $$ = ERR_TYPE;
    }
  }
  
  // plus grand ou égal
  | expr GEQ expr 
  {
    if ($1 == T_INT && $3 == T_INT) {
      char buf1[MAXBUF];
      char buf2[MAXBUF];
      unsigned ln = new_label_number();
      create_label(buf1, MAXBUF, "%s:%u:%s", "case", ln, "n0");
      create_label(buf2, MAXBUF, "%s:%u:%s", "case", ln, "end");
      printf("\tpop ax\n"
          "\tpop bx\n"
          "\tconst cx,0\n"
          "\tpush cx\n"
          "\tconst cx,%s\n"
          "\tsless ax,bx\n"
          "\tjmpc cx\n"
          "\tnop\n"
          "\tcmp ax,bx\n"
          "\tjmpc cx\n"
          "\tconst cx,%s\n"
          "\tjmp cx\n"
          ":%s\n"
          "\tpop bx\n"
          "\tconst ax,1\n"
          "\tpush ax\n"
          ":%s\n"
          "\tnop\n", buf1, buf2, buf1, buf2);
      $$ = T_BOOL;
    } else {
      $$ = ERR_TYPE;
    }
  }
  
  // plus grand que
  | expr GT expr 
  {
    if ($1 == T_INT && $3 == T_INT) {
      char buf1[MAXBUF];
      char buf2[MAXBUF];
      unsigned ln = new_label_number();
      create_label(buf1, MAXBUF, "%s:%u:%s", "case", ln, "n0");
      create_label(buf2, MAXBUF, "%s:%u:%s", "case", ln, "end");
      printf("\tpop ax\n"
          "\tpop bx\n"
          "\tconst cx,0\n"
          "\tpush cx\n"
          "\tconst cx,%s\n"
          "\tsless ax,bx\n"
          "\tjmpc cx\n"
          "\tconst cx,%s\n"
          "\tjmp cx\n"
          ":%s\n"
          "\tpop bx\n"
          "\tconst ax,1\n"
          "\tpush ax\n"
          ":%s\n"
          "\tnop\n", buf1, buf2, buf1, buf2);
      $$ = T_BOOL;
    } else {
      $$ = ERR_TYPE;
    }
  }
  
  // plus petit ou égal
  | expr LEQ expr 
  {
    if ($1 == T_INT && $3 == T_INT) {
      char buf1[MAXBUF];
      char buf2[MAXBUF];
      unsigned ln = new_label_number();
      create_label(buf1, MAXBUF, "%s:%u:%s", "case", ln, "n0");
      create_label(buf2, MAXBUF, "%s:%u:%s", "case", ln, "end");
      printf("\tpop ax\n"
          "\tpop bx\n"
          "\tconst cx,0\n"
          "\tpush cx\n"
          "\tconst cx,%s\n"
          "\tsless bx,ax\n"
          "\tjmpc cx\n"
          "\tnop\n"
          "\tcmp ax,bx\n"
          "\tjmpc cx\n"
          "\tconst cx,%s\n"
          "\tjmp cx\n"
          ":%s\n"
          "\tpop bx\n"
          "\tconst ax,1\n"
          "\tpush ax\n"
          ":%s\n"
          "\tnop\n", buf1, buf2, buf1, buf2);
      $$ = T_BOOL;
    } else {
      $$ = ERR_TYPE;
    }
  }
  
  // plus petit que
  | expr LT expr    
  {
    if ($1 == T_INT && $3 == T_INT) {
      char buf1[MAXBUF];
      char buf2[MAXBUF];
      unsigned ln = new_label_number();
      create_label(buf1, MAXBUF, "%s:%u:%s", "case", ln, "n0");
      create_label(buf2, MAXBUF, "%s:%u:%s", "case", ln, "end");
      printf("\tpop ax\n"
          "\tpop bx\n"
          "\tconst cx,0\n"
          "\tpush cx\n"
          "\tconst cx,%s\n"
          "\tsless bx,ax\n"
          "\tjmpc cx\n"
          "\tconst cx,%s\n"
          "\tjmp cx\n"
          ":%s\n"
          "\tpop bx\n"
          "\tconst ax,1\n"
          "\tpush ax\n"
          ":%s\n"
          "\tnop\n", buf1, buf2, buf1, buf2);
      $$ = T_BOOL;
    } else {
      $$ = ERR_TYPE;
    }
  }
  
  // Vrai (booleen)
  | TRUE 
  {
    printf("\tconst ax,1\n"
        "\tpush ax\n"
        );
    $$ = T_BOOL;
  }
  
  // faux (booleen)
  | FALSE 
  {
    printf("\tconst ax,0\n"
            "\tpush ax\n"
    );
    $$ = T_BOOL;
  }
  
  // identifiant au sens de sa valeur
  | ID 
  {
    symbol_table_entry *ste = search_symbol_table($1);
    if (ste == NULL) {
      fail_with("Not an existing symbol");
    } else if (ste->class == GLOBAL_VARIABLE) {
      printf("\tconst ax,var:%s\n"
          "\tloadw bx,ax\n"
          "\tpush bx\n", $1);
    } else if (ste->class == LOCAL_VARIABLE){
      if (ste->add < current_function->nParams) {
        printf("\tcp bx,bp\n"
              "\tconst cx,%zu\n"
              "\tconst dx,1\n"
              "\tadd cx,dx\n"
              "\tconst dx,%d\n"
              "\tsub cx,dx\n"
              "\tconst dx,2\n"
              "\tmul cx,dx\n"
              "\tsub bx,cx\n"
              "\tloadw ax,bx\n"
              "\tpush ax\n", ste->nParams, ste->add + 1);
      } else if (ste->add <= current_function->nParams + current_function->nLocalVariables){
        printf("\tcp bx,bp\n"
              "\tconst cx,%zu\n"
              "\tconst dx,%zu\n"
              "\tadd cx,dx\n"
              "\tconst dx,1\n"
              "\tadd cx,dx\n"
              "\tconst dx,%zu\n"
              "\tsub cx,dx\n"
              "\tconst dx,2\n"
              "\tmul cx,dx\n"
              "\tsub bx,cx\n"
              "\tloadw ax,bx\n"
              "\tpush ax\n" 
        ,current_function->nParams, current_function->nLocalVariables, ste->add - current_function->nParams + 1);
      } else {
        fail_with("Fatal error on index of id : %s !", $1);
      }
    } else {
      fail_with("Fatal error on ID type of : %s !", $1);
    }
    if (ste->desc[0] == INT_T) {
      $$ = T_INT;
    } else if (ste->desc[0] == BOOL_T) {
      $$ = T_BOOL;
    } else {
      fail_with("Erreur de type sur id : %s", $1);
    }
  }
  
  // Chaine de caractère ( uniquement pour print (voir instructions)
  | STRING 
  {
    unsigned int n = new_label_number();
    char buf1[MAXBUF];
    char buf2[MAXBUF];
    create_label(buf1, MAXBUF, "%s:%d", "var", n);
    create_label(buf2, MAXBUF, "%s:%d", "apres:var", n);
    printf("\tconst ax,%s\n"
          "\tjmp ax\n"
          ":%s\n"
          "@string %s\n"
          ":%s\n"
          "\tconst bx,%s\n"
          "\tpush bx\n", buf2, buf1, $1, buf2, buf1);
    $$ = T_STRING;
  }
  
  // appel de fonction
  | ID 
  {
    symbol_table_entry *ste = search_symbol_table($1);
    if (ste == NULL) {
      fail_with("Not an existing symbol");
    }
    for(int i = 0; i < ste->nLocalVariables; ++i) {
      printf("\tconst ax,0\n"
            "\tpush ax\n");
    }
  } '(' sexpr ')' 
  {
    printf("\tconst ax,function:%s\n"
          "\tcall ax\n"
          "\tpush ax\n", $1);
    symbol_table_entry *ste = search_symbol_table($1);
    if(ste == NULL) {
      fail_with("Fatal error");
    }
    for(int j = 0; j < ste->nLocalVariables + ste->nParams; ++j) {
      printf("\tpop bx\n");
    }
    if (ste->desc[0] == INT_T) {
      $$ = T_INT;
    } else if (ste->desc[0] == BOOL_T) {
      $$ = T_BOOL;
    } else {
      fail_with("Type error");
    }
  } 
;

// Non terminaux qui gerent les suites d'expression 
// valeurs de parametres lors de l'appel de fonction
sexpr:
  %empty
  | spexpr 
;

spexpr:
  expr 
  | spexpr ',' expr 
;

%%

void fail_with(const char *format, ...) {
  va_list ap;
  va_start(ap, format);
  vfprintf(stderr, format, ap);
  va_end(ap);
  exit(EXIT_FAILURE);
}

// DEFINITION DES FONCTIONS C

static unsigned int new_label_number() {
  static unsigned int current_label_number = 0u;
  if (current_label_number == UINT_MAX) {
    fail_with("Error: maximum label number reached !\n");
  }
  return current_label_number++;
}

static void create_label(char *buf, size_t buf_size, const char *format, ...) {
  va_list ap;
  va_start(ap, format);
  if (vsnprintf(buf, buf_size, format, ap) >= buf_size) {
    va_end(ap);
    fail_with("Error in label generation: size of label exceeds maximum size!\n");
  }
  va_end(ap);
}

void yyerror(char const *s) {
  fprintf(stderr, "%s\n", s);
}

// FONCTION PRINCIPALE

int main(void) {

  // code en amont de l'analyse
  printf(
      "\tconst ax,debut\n"
      "\tjmp ax\n"
      "\n:str:end:prog\n"
      "@string \"The program end with value :\"\n"
      "\n:chaine:vrai\n"
      "@string \"true\"\n"
      "\n:chaine:faux\n"
      "@string \"false\"\n"
      "\n:msg:error:div0\n"
      "@string \"Erreur : Division par zéro\"\n"
      "\n:error:div0\n"
      "\tconst ax,msg:error:div0\n"
      "\tcallprintfs ax\n"
      "\tend\n"
      "\n:debut\n"
      "\tconst bp,pile\n"
      "\tconst sp,pile\n"
      "\tconst ax,2\n"
      "\tsub sp,ax\n");
      
  //analyse de l'entrée
  yyparse();
  
  // tests
  symbol_table_entry *ste = search_symbol_table("main");
  if (ste == NULL) {
    fail_with("No reference to a main function");
  }
  if(!return_value_main) {
    fail_with("No return value for main function");
  }
  
  // code de fin de programme + appel main
  for(int i = 0; i < ste->nLocalVariables; ++i) {
    printf("\tconst ax,0\n"
          "\tpush ax\n");
  }
  printf("\tconst ax,function:main\n"
        "\tcall ax\n");
  for(int j = 0; j < ste->nLocalVariables + ste->nParams; ++j) {
    printf("\tpop bx\n");
  }
  printf ("\tpush ax\n"
        "\tconst bx,str:end:prog\n"
        "\tcallprintfs bx\n"
        "\tcp ax,sp\n"
        "\tcallprintfd ax\n"
        "\tpop ax\n"
        "\tend\n"
        ":pile\n"
        "@int 0\n");
  return 0;
}
