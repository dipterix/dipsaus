
AC_PREREQ([2.61])

## Update this version number along with DESCRIPTION
AC_INIT
AC_CONFIG_SRCDIR([dipsaus])


## --- Output ----------------------------------------------------------------
## Substitute and generate Makevars
AC_SUBST([PKG_CFLAGS],["${PKG_CFLAGS} -I../inst/include"])
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT

echo "
================================
compiler flags: ${PKG_CFLAGS}
link argument:  ${PKG_LIBS}
================================
"
