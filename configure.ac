##
## Process this file with autoconf to produce a configure script.
##

AC_PREREQ(2.69)

## ---[ VERSION ]-------------------------------------------------------------

AC_INIT()


RCPP_PKG_LIBS=`${R_HOME}/bin/Rscript -e "Rcpp:::LdFlags()"`
RCPP_PARALLRL_LIBS=`${R_HOME}/bin/Rscript -e "RcppParallel::RcppParallelLibs()"`


if test `uname` = "Darwin" ; then
  darwin="yes"
else
  darwin="no"
fi

## --- Output ----------------------------------------------------------------
AC_SUBST(RCPP_PKG_LIBS)
AC_SUBST(RCPP_PARALLRL_LIBS)

AC_OUTPUT(src/Makevars)

