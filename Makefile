
all: t.s runtime.c
	gcc -o t t.s runtime.c

t.s: compiler.ss test.ss
	petite test.ss

clean:
	rm -f t t.s

test: all
	./t

