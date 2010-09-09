/*! \file
    \brief Some common shared defintions.
*/
#ifndef _COMMON_H_
#define _COMMON_H_

typedef unsigned char      U8;   //!< unsigned 8-bit value
#if TARGET_HARDWARE!=H_VERSALOON
typedef unsigned int       U16;  //!< unsigned 16-bit value
typedef unsigned long int  U32;  //!< unsigned 32-bit value
#else
typedef unsigned short     U16;  //!< unsigned 16-bit value
typedef unsigned int       U32;  //!< unsigned 32-bit value
#endif
typedef signed   char      S8;   //!< signed 8-bit value
#if TARGET_HARDWARE!=H_VERSALOON
typedef signed   int       S16;  //!< signed 16-bit value
typedef signed   long int  S32;  //!< signed 32-bit value
#else
typedef signed   short     S16;  //!< signed 16-bit value
typedef signed   int       S32;  //!< signed 32-bit value
#endif

//! Used for manipulating Little/Big-Endian longwords (32 bits)
//!
typedef union {
    U32 longword;          //!< Treat as native 32-bit value
    U8  bytes[4];          //!< Treat as array of bytes
    } U32u;

//! Used for manipulating Little/Big-Endian words (16 bits)
//!
typedef union {
    U8  bytes[2];                //!< Treat as array of bytes
    U16 word;                    //!< Treat as native 16-bit value
    struct {U8 lo; U8 hi;} le;   //!< Little-endian order
    struct {U8 hi; U8 lo;} be;   //!< Big-endian order
    } U16u;

#if TARGET_HARDWARE!=H_VERSALOON
#define disableInterrupts()   asm("sei")        //!< Disable all interrupts
#define enableInterrupts()    asm("cli")        //!< Enable interrupts
#define backgroundDebugMode() asm("bgnd")       //!< Enter Background Debug Mode if enabled

#ifdef __HC08__
#define wait()                asm("wait")       //!< Enter WAIT mode if enabled
#define stop()                asm("stop")       //!< Enter STOP mode if enabled
#define reset()               asm("dcb 0x8d")   //!< Force Illegal Operation (instruction) reset
#elif defined __HC12__
#define wait()                asm("wai")        //!< Enter WAIT mode if enabled
#define stop()                asm("stop")       //!< Enter STOP mode if enabled
//!< Force reset using COP
#define reset() \
   COPCTL = 7,     /* Enable COP                  */ \
   ARMCOP = 0xFF   /* Trigger immediate COP reset */
#endif

#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE 1
#endif

#endif

#endif //_COMMON_H_