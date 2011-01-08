#ifndef _BDMCOMMON_H_
#define _BDMCOMMON_H_

//==============================================================
//  Timer Usage:
//
//   TPMx-CHa - BDM_IN pin, SYNC measuring, ACKN detection (IC rising & falling edges)
//   TPMx-CHb - ACKN & SYNC Timeouts (Output compare)
//
//==============================================================*/

// TPM (SYNC & ACKN) timer constants, 24MHz ticks
#define ACKN_TIMER_PRESCALE_MASK (0)
#define ACKN_TIMER_PRESCALE      (1<<ACKN_TIMER_PRESCALE_MASK)
#define ACKN_TIMER_FREQ          (BUS_FREQ/ACKN_TIMER_PRESCALE)
#define ACKN_MICROSECOND(x)      ((U16)(((x)*(ACKN_TIMER_FREQ/1000))/1000UL))  // SYNC timer ticks in 1 us
#define ACKN_TPMxSC_VALUE        (TPMSC_CLKSA_MASK|ACKN_TIMER_PRESCALE_MASK)

// TPM Fast timer constants ~333ns resolution, usable to 20ms
#define FAST_TIMER_PRESCALE_MASK (3)
#define FAST_TIMER_PRESCALE      (1<<FAST_TIMER_PRESCALE_MASK)
#define FAST_TIMER_FREQ          (BUS_FREQ/FAST_TIMER_PRESCALE)
#define MICROSECOND(x)           ((U16)(((x)*(FAST_TIMER_FREQ/1000))/1000UL))  // Fast timer ticks in 1 us
#define FAST_TPMxSC_VALUE        (TPMSC_CLKSA_MASK|FAST_TIMER_PRESCALE_MASK)

// TPM Slow timer constants ~5us resolution, usable to 320ms
#define SLOW_TIMER_PRESCALE_MASK (FAST_TIMER_PRESCALE_MASK+4)
#define SLOW_TIMER_PRESCALE      (1<<SLOW_TIMER_PRESCALE_MASK)
#define SLOW_TIMER_FREQ          (BUS_FREQ/SLOW_TIMER_PRESCALE)
#define MILLISECOND(x)           ((U16)(((x)*SLOW_TIMER_FREQ)/1000UL))  // Slow timer ticks in 1 ms
#define SLOW_TPMxSC_VALUE        (TPMSC_CLKSA_MASK|SLOW_TIMER_PRESCALE_MASK)

// General Time intervals
#define VDD_RISE_TIME    2000U // us - minimum time to allow for controlled target Vdd rise
#define BKGD_WAIT        2000U // us - time to hold BKGD pin low after reset pin rise for special modes (allowance made for slow Reset rise)
#define RESET_RECOVERY      5U // ms - how long to wait after reset before new commands are allowed
#define RESET_SETTLE     3000U // us - time to wait for signals to settle in us, this should certainly be longer than the soft reset time

#if (SLOW_TIMER_PRESCALE_MASK >= 8)
#error "SLOW_TIMER_PRESCALE_MASK value out of range"
#endif

/*! \brief A Macro to wait for given time (makes use of timer).

    @param  t  Time to wait in \e microseconds.
*/
#if TARGET_HARDWARE!=H_VERSALOON
#define WAIT_US(t) fastTimerWait(MICROSECOND(t))
#else
#define WAIT_US(t) DelayUS(t)
#endif
/*! \brief A Macro to wait for given time (makes use of timer).

    @param  t  Time to wait in \e milliseconds.
*/
#if TARGET_HARDWARE!=H_VERSALOON
#define WAIT_MS(t) slowTimerWait(MILLISECOND(t))
#else
#define WAIT_MS(t) DelayMS(t)
#endif
/*! \brief A Macro to wait for given time (makes use of timer).

    @param  t  Time to wait in \e seconds.
*/
#if TARGET_HARDWARE!=H_VERSALOON
#define WAIT_S(t)  {int tt = 4*(t); \
                    while(tt-->0)   \
                       slowTimerWait(MILLISECOND(250));}
#else
#define WAIT_S(t)  DelayMS(1000 * (t))
#endif

/*! \brief A Macro to wait for given time or until a condition is met

    @param  t  Maximum time to wait in \e microseconds.
    @param  c  Condition to exit wait early
*/
#if TARGET_HARDWARE!=H_VERSALOON
#define WAIT_WITH_TIMEOUT_US(t,c) {                 \
   TPMSC                = FAST_TPMxSC_VALUE;        \
   TPMCNTH              = 0;                        \
   TIMEOUT_TPMxCnSC_CHF = 0;                        \
   TIMEOUT_TPMxCnVALUE  = MICROSECOND(t);           \
   while (!(c) && (TIMEOUT_TPMxCnSC_CHF==0)){       \
   }                                                \
   TPMSC                = ACKN_TPMxSC_VALUE;        \
}
#else
#define WAIT_WITH_TIMEOUT_US(t,c) {                 \
   U32 tt = (t);                                    \
   while (!(c) && tt--){DelayUS(1):}                \
}
#endif

/*! \brief A Macro to wait for given time or until a condition is met

    @param  t  Maximum time to wait in \e milliseconds.
    @param  c  Condition to exit wait early
*/
#if TARGET_HARDWARE!=H_VERSALOON
#define WAIT_WITH_TIMEOUT_MS(t,c) {                 \
   TPMSC                = FAST_TPMxSC_VALUE;        \
   TPMCNTH              = 0;                        \
   TIMEOUT_TPMxCnSC_CHF = 0;                        \
   TIMEOUT_TPMxCnVALUE  = MILLISECOND(t);           \
   while (!(c) && (TIMEOUT_TPMxCnSC_CHF==0)){       \
   }                                                \
   TPMSC                = ACKN_TPMxSC_VALUE;        \
}
#else
#define WAIT_WITH_TIMEOUT_MS(t,c) {                 \
   U32 tt = (t);                                    \
   while (!(c) && tt--){DelayMS(1);}                \
}
#endif

/*! \brief A Macro to wait for given time or until a condition is met

    @param  t  Maximum time to wait in \e seconds.
    @param  c  Condition to exit wait early (checked every ~10 ms and affects timing)
*/
#if TARGET_HARDWARE!=H_VERSALOON
#define WAIT_WITH_TIMEOUT_S(t,c) {       \
    int tt = 100*(t);                    \
      do {                               \
         slowTimerWait(MILLISECOND(10)); \
      } while (!(c) & (tt-->0));         \
   }
#else
#define WAIT_WITH_TIMEOUT_S(t,c) {       \
    int tt = 100*(t);                    \
      do {                               \
         WAIT_MS(10);                    \
      } while (!(c) & (tt-->0));         \
   }
#endif

#if TARGET_HARDWARE!=H_VERSALOON
void fastTimerWait(U16 delay);
void slowTimerWait(U16 delay);
#endif
U8   initTimers(void);

void usbdm_bdm_init(void);
U8   bdm_setTarget(U8 target);
void bdm_checkTargetVdd(void);
void bdm_suspend(void);
U8   bdm_cycleTargetVddOn(U8 mode);
U8   bdm_cycleTargetVdd(U8 mode);
U16  bdm_targetVddMeasure(void);
U8   bdm_setTargetVdd( void );  // Low-level - bdm_cycleTargetVddOn() preferred

#if TARGET_HARDWARE!=H_VERSALOON
// Interrupt monitoring routines
interrupt void bdm_resetSense(void);
interrupt void bdm_targetVddSense(void);
#endif

#endif // _BDMCOMMON_H_
