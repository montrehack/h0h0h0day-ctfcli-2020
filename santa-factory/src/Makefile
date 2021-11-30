all:
	gcc -g -l dl -DDEBUG factory.c -o santa-dbg
	gcc -l dl -o santa-factory factory.c && strip santa-factory
