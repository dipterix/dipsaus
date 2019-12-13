#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
 Check these declarations against the C/Fortran source code.
 */

/* .Call calls */
extern SEXP _dipsaus_collapser(SEXP, SEXP, SEXP);
extern SEXP _dipsaus_arrayShift(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _dipsaus_baselineArray(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"_dipsaus_arrayShift", (DL_FUNC) &_dipsaus_arrayShift, 5},
  {"_dipsaus_collapser", (DL_FUNC) &_dipsaus_collapser, 3},
  {"_dipsaus_baselineArray", (DL_FUNC) &_dipsaus_baselineArray, 8},
  {NULL, NULL, 0}
};

void R_init_dipsaus(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}

