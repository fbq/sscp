#include <stdio.h>
#include <stdlib.h>


#define SCHEME_ENTRY _scheme_entry

extern long SCHEME_ENTRY(void);

void print(long v)
{
	printf("%ld\n", v);
}

int main(int argc, char** argv)
{
	if (argc != 1)
		fprintf(stderr, "usage: %s\n", argv[0]);

	print(SCHEME_ENTRY());

	return 0;
}
