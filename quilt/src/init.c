
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

SEXP quilt_tpolygon(SEXP scenep, SEXP xp, SEXP yp, 
		    SEXP npolyp, SEXP istartp, SEXP iendp,
		    SEXP colp, SEXP fillp, SEXP ltyp, SEXP lwdp);

SEXP quilt_trect(SEXP scenep, SEXP xp, SEXP yp, SEXP wp, SEXP hp, 
		 SEXP colp, SEXP fillp, SEXP ltyp, SEXP lwdp);


void R_init_quilt(DllInfo *dll);

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

static R_CallMethodDef CallEntries[] = {
    CALLDEF(quilt_tpolygon, 10),
    CALLDEF(quilt_trect, 9),
    {NULL, NULL, 0}
};


void R_init_quilt(DllInfo *dll)
{
    // Register C routines
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

