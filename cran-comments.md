# Current submission:

No error/warning/note under current R (`4.4.x`).

Under the development version, self check showed 1 note

```
File 'dipsaus/libs/x64/dipsaus.dll':
  Found non-API calls to R: 'CLOENV', 'ENCLOS'
```

Solution:

Removed `CLOENV` and `ENCLOS` calls in the source code, and used `Rcpp` instead. 

However, R-check is still reporting this note. This is because `Rcpp` has not 
yet fixed this calls for `R-4.5.0`. A recent commit in `Rcpp` addressed this
issue and you will not see this error once they release the new version for `Rcpp`
