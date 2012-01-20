#include <sys/types.h>
#include <sys/stat.h>

extern int  __HEAP_START;

static unsigned char *heap = NULL;
caddr_t _sbrk(int incr)
{
	unsigned char *prev_heap;
	
	if (heap == NULL)
	{
		heap = (unsigned char *)&__HEAP_START;
	}
	prev_heap = heap;
	/* check removed to show basic approach */
	
	heap += incr;
	return (caddr_t)prev_heap;
}

int _open(const char *name, int flags, int mode)
{
	return -1;
}

int _close(int fd)
{
	return 0;
}

int _fstat(int fd, struct stat *st)
{
	st->st_mode = S_IFCHR;
	return 0;
}

int _isatty(int fd)
{
	return 1;
}

int _lseek(int fd, int ptr, int dir)
{
	return 0;
}

int _read(int fd, char *buf, int len)
{
	return 0;
}

int _write(int fd, const void *buf, int len)
{
	return len;
}

