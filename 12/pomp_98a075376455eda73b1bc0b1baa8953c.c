/* pomp model file: pomp_98a075376455eda73b1bc0b1baa8953c */
/*  Time: 2018-04-02 10:44:20.026 -0400 Salt: C9B4771D22B256144EE9360D */

#include <C:/Users/Elliott/R/win-library/3.4/pomp/include/pomp.h>
#include <R_ext/Rdynload.h>

 
#define Beta	(__p[__parindex[0]])
#define gamma	(__p[__parindex[1]])
#define rho	(__p[__parindex[2]])
#define N	(__p[__parindex[3]])
#define H	(__x[__stateindex[0]])
#define S	(__x[__stateindex[1]])
#define I	(__x[__stateindex[2]])
#define R	(__x[__stateindex[3]])
#define B	(__y[__obsindex[0]])
#define C	(__y[__obsindex[1]])
#define DH	(__f[__stateindex[0]])
#define DS	(__f[__stateindex[1]])
#define DI	(__f[__stateindex[2]])
#define DR	(__f[__stateindex[3]])
#define TBeta	(__pt[__parindex[0]])
#define Tgamma	(__pt[__parindex[1]])
#define Trho	(__pt[__parindex[2]])
#define TN	(__pt[__parindex[3]])
#define lik	(__lik[0])

void __pomp_initializer (double *__x, const double *__p, double t, const int *__stateindex, const int *__parindex, const int *__covindex, const double *__covars)
{

  S = nearbyint(N)-1;
  I = 1;
  R = 0;
  H = 0;
 
}


void __pomp_rmeasure (double *__y, const double *__x, const double *__p, const int *__obsindex, const int *__stateindex, const int *__parindex, const int *__covindex, int __ncovars, const double *__covars, double t)
{
B = rbinom(H,rho); 
}


void __pomp_dmeasure (double *__lik, const double *__y, const double *__x, const double *__p, int give_log, const int *__obsindex, const int *__stateindex, const int *__parindex, const int *__covindex, int __ncovars, const double *__covars, double t)
{
lik = dbinom(B,H,rho,give_log); 
}


void __pomp_stepfn (double *__x, const double *__p, const int *__stateindex, const int *__parindex, const int *__covindex, int __covdim, const double *__covars, double t, double dt)
{

  double dN_SI = rbinom(S,1-exp(-Beta*I/N*dt));
  double dN_IR = rbinom(I,1-exp(-gamma*dt));
  S -= dN_SI;
  I += dN_SI - dN_IR;
  R += dN_IR;
  H += dN_IR;
 
}

#undef Beta
#undef gamma
#undef rho
#undef N
#undef H
#undef S
#undef I
#undef R
#undef B
#undef C
#undef DH
#undef DS
#undef DI
#undef DR
#undef TBeta
#undef Tgamma
#undef Trho
#undef TN

static int __pomp_load_stack = 0;

void __pomp_load_stack_incr (void) {
  ++__pomp_load_stack;
}

void __pomp_load_stack_decr (int *val) {
  *val = --__pomp_load_stack;
}

void R_init_pomp_98a075376455eda73b1bc0b1baa8953c (DllInfo *info)
{
R_RegisterCCallable("pomp_98a075376455eda73b1bc0b1baa8953c", "__pomp_load_stack_incr", (DL_FUNC) __pomp_load_stack_incr);
R_RegisterCCallable("pomp_98a075376455eda73b1bc0b1baa8953c", "__pomp_load_stack_decr", (DL_FUNC) __pomp_load_stack_decr);
R_RegisterCCallable("pomp_98a075376455eda73b1bc0b1baa8953c", "__pomp_initializer", (DL_FUNC) __pomp_initializer);
R_RegisterCCallable("pomp_98a075376455eda73b1bc0b1baa8953c", "__pomp_rmeasure", (DL_FUNC) __pomp_rmeasure);
R_RegisterCCallable("pomp_98a075376455eda73b1bc0b1baa8953c", "__pomp_dmeasure", (DL_FUNC) __pomp_dmeasure);
R_RegisterCCallable("pomp_98a075376455eda73b1bc0b1baa8953c", "__pomp_stepfn", (DL_FUNC) __pomp_stepfn);
}
