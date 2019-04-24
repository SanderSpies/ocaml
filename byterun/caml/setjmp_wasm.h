#define _JBLEN		(10 + 16 + 2)

typedef int jmp_buf[_JBLEN];


extern int	setjmp2(jmp_buf);
extern void longjmp2(jmp_buf, int);