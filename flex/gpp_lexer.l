%{
	#include <stdio.h>
	#include <stdlib.h>
	#include <string.h>
    
    enum TokenType {
        KW_AND, 
        KW_OR, 
        KW_NOT, 
        KW_EQUAL, 
        KW_LESS, 
        KW_NIL,
        KW_LIST, 
        KW_APPEND, 
        KW_CONCAT, 
        KW_SET, 
        KW_DEF, 
        KW_FOR,
        KW_IF, 
        KW_EXIT, 
        KW_LOAD, 
        KW_DISPLAY, 
        KW_TRUE, 
        KW_FALSE,
        OP_PLUS, 
        OP_MINUS, 
        OP_DIV, 
        OP_MULT, 
        OP_OP, 
        OP_CP, 
        OP_COMMA,
        COMMENT, VALUEF, IDENTIFIER
    };

    void printToken(enum TokenType type);
%}


%%

quit                { printf("Goodbye!\n"); exit(0); } // this is not a rule, however, to exit the program we need something
and                 { printToken(KW_AND); }
or                  { printToken(KW_OR); }
not                 { printToken(KW_NOT); }
equal               { printToken(KW_EQUAL); }
less                { printToken(KW_LESS); }
nil                 { printToken(KW_NIL); }
list                { printToken(KW_LIST); }
append              { printToken(KW_APPEND); }
concat              { printToken(KW_CONCAT); }
set                 { printToken(KW_SET); }
def                 { printToken(KW_DEF); }
for                 { printToken(KW_FOR); }
if                  { printToken(KW_IF); }
exit                { printToken(KW_EXIT);}
load                { printToken(KW_LOAD); }
display             { printToken(KW_DISPLAY); }
true                { printToken(KW_TRUE); }
false               { printToken(KW_FALSE); }
\+                  { printToken(OP_PLUS); }
\-                  { printToken(OP_MINUS); }
\/                  { printToken(OP_DIV); }
\*                  { printToken(OP_MULT); }
\(                  { printToken(OP_OP); }
\)                  { printToken(OP_CP); }
,                   { printToken(OP_COMMA); }
;;.*                { printToken(COMMENT); }
([1-9]+[0-9]*|[0])[b][1-9]+[0-9]* { printToken(VALUEF); }
[a-zA-Z][a-zA-Z0-9]* { printToken(IDENTIFIER); } 
[ \t\r\n]+          ; // Ignore whitespace characters
.                   { printf("SYNTAX_ERROR %s cannot be tokenized\n", yytext); }

%%

void printToken(enum TokenType type) {
    switch(type) {
        case KW_AND:
            printf("KW_AND\n");
            break;
        case KW_OR:
            printf("KW_OR\n");
            break;
        case KW_NOT:
            printf("KW_NOT\n");
            break;
        case KW_EQUAL:
            printf("KW_EQUAL\n");
            break;
        case KW_LESS:
            printf("KW_LESS\n");
            break;
        case KW_NIL:
            printf("KW_NIL\n");
            break;
        case KW_LIST:
            printf("KW_LIST\n");
            break;
        case KW_APPEND:
            printf("KW_APPEND\n");
            break;
        case KW_CONCAT:
            printf("KW_CONCAT\n");
            break;
        case KW_SET:
            printf("KW_SET\n");
            break;
        case KW_DEF:
            printf("KW_DEF\n");
            break;
        case KW_FOR:
            printf("KW_FOR\n");
            break;
        case KW_IF:
            printf("KW_IF\n");
            break;
        case KW_EXIT:
            printf("KW_EXIT\n");
            break;
        case KW_LOAD:
            printf("KW_LOAD\n");
            break;
        case KW_DISPLAY:
            printf("KW_DISPLAY\n");
            break;
        case KW_TRUE:
            printf("KW_TRUE\n");
            break;
        case KW_FALSE:
            printf("KW_FALSE\n");
            break;
        case OP_PLUS:
            printf("OP_PLUS\n");
            break;
        case OP_MINUS:
            printf("OP_MINUS\n");
            break;
        case OP_DIV:
            printf("OP_DIV\n");
            break;
        case OP_MULT:
            printf("OP_MULT\n");
            break;
        case OP_OP:
            printf("OP_OP\n");
            break;
        case OP_CP:
            printf("OP_CP\n");
            break;
        case OP_COMMA:
            printf("OP_COMMA\n");
            break;
        case COMMENT:
            printf("COMMENT\n");
            break;
        case VALUEF:
            printf("VALUEF\n");
            break;
        case IDENTIFIER:
            printf("IDENTIFIER\n");
            break;
        default:
            printf("Unknown token type\n");
            break;
    }
}

int yywrap(void) { return 1; } // This function is needed for the program to compile

int main(int argc, char **argv) {
	if (argc > 1) {
		yyin = fopen(argv[1], "r");
		if (yyin == NULL) {
			printf("File does not exist.\n");
			return 0;
		}
	}
    printf("Welcome to the interpreter! (type 'quit' to finish program)\n");
	yylex();
	return 0;
}
