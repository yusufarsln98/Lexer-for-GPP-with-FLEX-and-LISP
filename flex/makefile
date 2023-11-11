CC=gcc
CFLAGS=-Wall -Wextra -pedantic -std=c99

all: gpp_lexer run

gpp_lexer: gpp_lexer.c
	$(CC) $(CFLAGS) -o gpp_lexer gpp_lexer.c

run: gpp_lexer
	./gpp_lexer

test1: gpp_lexer
	./gpp_lexer test1.gpp

test2: gpp_lexer
	./gpp_lexer test2.gpp

make clean:
	rm -f gpp_lexer gpp_lexer.c
