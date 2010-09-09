/*****************************************************
Termio.c for 9S12C32/128 family
 *****************************************************/

#include <termio.h>
#include "Derivative.h"     /* derivative information */
#include "Configure.h"

#ifndef SCID
#define SCID         SCI2D
#define SCIS1_RDRF   SCI2S1_RDRF
#define SCIS1_TDRE   SCI2S1_TDRE
#define SCIBD        SCI2BD
#define SCIC2_TE     SCI2C2_TE
#define SCIC2_RE     SCI2C2_RE
#endif

char TERMIO_GetChar(void) {
   /* receives character from the terminal channel */
   while (!SCIS1_RDRF){ /* wait for input char */
      };
   return SCID; 
}

void TERMIO_PutChar(char ch) {
   /* sends a character to the terminal channel */
   while (!SCIS1_TDRE) { /* wait for output buffer empty */
      };    
   SCID = ch;
}
    
#define BAUD_RATE    (19200UL)        // Desired serial baud rate
#define BAUDDIVIDER  (BUS_FREQ/(16*BAUD_RATE))

void TERMIO_Init(void) {
  /* initializes the communication channel */ 
    SCIBD     = BAUDDIVIDER;  /* set baud rate */
    SCIC2_TE = 1; /* Enable Tx */
    SCIC2_RE = 1; /* Enable Rx */
}


