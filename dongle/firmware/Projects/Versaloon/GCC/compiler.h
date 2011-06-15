#ifndef __COMPILER_H_INCLUDED__
#define __COMPILER_H_INCLUDED__

//#define USE_BUILDIN_STRING_H

#ifdef USE_BUILDIN_STRING_H
#include <string.h>
#else
extern void* memcpy(void *m0, const void *m1, int len);
extern int memcmp(const void *m0, const void *m1, int len);
extern void *memset(void *m, int c, int len);
#endif

#define ROOTFUNC	

#endif	// __COMPILER_H_INCLUDED__
