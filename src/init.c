#include <stdlib.h>
#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Lapack.h>
#include <R_ext/Boolean.h>

/* C Prototypes */
extern void post_results(double *prior_mat, int *G, int *dimX, double *H_err, double *R_err, double *T_err);
extern void cellwise(double *s, double *z, double *h, double *r, double *t, double *Xc, double *Xp, int *dimX, int *epochs);
extern void wcellwise(double *s, double *z, double *h, double *r, double *t, double *Xc, double *Xp, int *dimX, double *wt, int *epochs);
extern void bayeswise(double *s, int *G, double *z, double *h, double *r, double *t, double *Xc, double *Xp, int *dimX, int *epochs);
extern void wbayeswise(double *s, int *G, double *z, double *h, double *r, double *t, double *Xc, double *Xp, int *dimX, double *wt, int *epochs);
extern void dif(double *res, double *dta, int *dimD, int *nt, int *nss);
extern void normalize(double *dta, int *dim, int *gr, int *ng, double *res);
extern void history_check(double *hScore, double *zScore, double *x, double *w, int *n);
extern void tail_check(double *dta, int *dim, int *gr, int *ng, double *tScore);
extern void relat_check(double *A, int *dim);
extern void history_res(double *hRes, double *zScore, double *x, double *w, int *n);
extern void tail_res(double *dta, int *dim, int *gr, int *ng, double *tRes);
extern void relat_res(double *A, int *dim);
extern void gif(double *res, double *dta, int *dimD, int *nt, int *nss);
extern void getNCores(int *n);
extern void getNThreads(int *n);
extern void setNThreads(int *n);
extern void bayes_boot(double *th, int *B, double *s, int *nn, double *theta);

static R_NativePrimitiveArgType post_results_type[] = { REALSXP, INTSXP, INTSXP, REALSXP, REALSXP, REALSXP };
static R_NativePrimitiveArgType cellwise_type[] = { REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, INTSXP, INTSXP };
static R_NativePrimitiveArgType wcellwise_type[] = { REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, INTSXP, REALSXP, INTSXP };
static R_NativePrimitiveArgType bayeswise_type[] = { REALSXP, INTSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, INTSXP, INTSXP };
static R_NativePrimitiveArgType wbayeswise_type[] = { REALSXP, INTSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, REALSXP, INTSXP, REALSXP, INTSXP };
static R_NativePrimitiveArgType dif_type[] = { REALSXP, REALSXP, INTSXP, INTSXP, INTSXP };
static R_NativePrimitiveArgType normalize_type[] = { REALSXP, INTSXP, INTSXP, INTSXP, REALSXP };
static R_NativePrimitiveArgType history_check_type[] = { REALSXP, REALSXP, REALSXP, REALSXP, INTSXP };
static R_NativePrimitiveArgType tail_check_type[] = { REALSXP, INTSXP, INTSXP, INTSXP, REALSXP };
static R_NativePrimitiveArgType relat_check_type[] = { REALSXP, INTSXP };
static R_NativePrimitiveArgType history_res_type[] = { REALSXP, REALSXP, REALSXP, REALSXP, INTSXP };
static R_NativePrimitiveArgType tail_res_type[] = { REALSXP, INTSXP, INTSXP, INTSXP, REALSXP };
static R_NativePrimitiveArgType relat_res_type[] = { REALSXP, INTSXP };
static R_NativePrimitiveArgType gif_type[] = { REALSXP, REALSXP, INTSXP, INTSXP, INTSXP };
static R_NativePrimitiveArgType getNCores_type[] = { INTSXP };
static R_NativePrimitiveArgType getNThreads_type[] = { INTSXP };
static R_NativePrimitiveArgType setNThreads_type[] = { INTSXP };
static R_NativePrimitiveArgType bayes_boot_type[] = { REALSXP, INTSXP, REALSXP, INTSXP, REALSXP };

static const R_CMethodDef CEntries[] = {
  {"post_results", (DL_FUNC) &post_results, 6, post_results_type},
  {"cellwise", (DL_FUNC) &cellwise, 9, cellwise_type},
  {"wcellwise", (DL_FUNC) &wcellwise, 10, wcellwise_type},
  {"bayeswise", (DL_FUNC) &bayeswise, 10, bayeswise_type},
  {"wbayeswise", (DL_FUNC) &wbayeswise, 11, wbayeswise_type},
  {"dif", (DL_FUNC) &dif, 5, dif_type},
  {"normalize", (DL_FUNC) &normalize, 5, normalize_type},
  {"history_check", (DL_FUNC) &history_check, 5, history_check_type},
  {"tail_check", (DL_FUNC) &tail_check, 5, tail_check_type},
  {"relat_check", (DL_FUNC) &relat_check, 2, relat_check_type},
  {"history_res", (DL_FUNC) &history_res, 5, history_res_type},
  {"tail_res", (DL_FUNC) &tail_res, 5, tail_res_type},
  {"relat_res", (DL_FUNC) &relat_res, 2, relat_res_type},
  {"gif", (DL_FUNC) &gif, 5, gif_type},
  {"getNCores", (DL_FUNC) &getNCores, 1, getNCores_type},
  {"getNThreads", (DL_FUNC) &getNThreads, 1, getNThreads_type},
  {"setNThreads", (DL_FUNC) &setNThreads, 1, setNThreads_type},
  {"bayes_boot", (DL_FUNC) &bayes_boot, 5, bayes_boot_type},
  {NULL, NULL, 0}
};

/* Call Prototypes */
extern SEXP isOmp(void);
extern SEXP openMP_version(void);
extern SEXP pif(SEXP dta, SEXP _prx, SEXP _nt, SEXP _nss, SEXP max_depth, SEXP dst_fun, SEXP Rnv);

static const R_CallMethodDef CallEntries[] = {
  {"isOmp", (DL_FUNC) &isOmp, 0},
  {"openMP_version", (DL_FUNC) &openMP_version, 0},
  {"pif", (DL_FUNC) &pif, 7},
  {NULL, NULL, 0}
};

void R_init_HRTnomaly(DllInfo *info) {
  R_registerRoutines(info, CEntries, CallEntries, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
  R_forceSymbols(info, TRUE);
}
