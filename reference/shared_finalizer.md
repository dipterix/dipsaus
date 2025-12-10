# Create Shared Finalization to Avoid Over Garbage Collection

Generates a function to be passed to
[`reg.finalizer`](https://rdrr.io/r/base/reg.finalizer.html)

## Usage

``` r
shared_finalizer(x, key, fin, onexit = FALSE, ...)

# Default S3 method
shared_finalizer(x, key, fin, onexit = FALSE, ...)

# S3 method for class 'R6'
shared_finalizer(x, key, fin, onexit = TRUE, ...)

# S3 method for class 'fastmap'
shared_finalizer(x, key, fin, onexit = FALSE, ...)

# S3 method for class 'fastmap2'
shared_finalizer(x, key, fin, onexit = FALSE, ...)
```

## Arguments

- x:

  object to finalize

- key:

  characters that should be identical if finalization method is to be
  shared

- fin:

  Shared finalization: function to call on finalization; see
  [`reg.finalizer`](https://rdrr.io/r/base/reg.finalizer.html). See
  details.

- onexit:

  logical: should the finalization be run if the object is still
  uncollected at the end of the R session? See
  [`reg.finalizer`](https://rdrr.io/r/base/reg.finalizer.html)

- ...:

  passed to other methods

## Details

The main purpose of this function is to allow multiple objects that
point to a same source (say a temporary file) to perform clean up when
all the objects are garbage collected.

Base function
[`reg.finalizer`](https://rdrr.io/r/base/reg.finalizer.html) provides
finalization to to garbage collect single R environment. However, when
multiple environments share the same file, finalizing one single
environment will result in removing the file so that all the other
environment lose the reference. (See example "Native `reg.finalizer`
fails example")

The argument of `fin` varies according to different types of `x`. For
environments, `fin` contains and only contains one parameter, which is
the environment itself. This is the same as `reg.finalizer`. For `R6`
classes, `fin` is ignored if class has `"shared_finalize"` method
defined. For
[`fastmap`](https://r-lib.github.io/fastmap/reference/fastmap.html) or
[`fastmap2`](https://dipterix.org/dipsaus/reference/fastmap2.md)
instances, `fin` accepts no argument.

## Examples

``` r
# ------------ Environment example ------------
file_exists <- TRUE
clear_files <- function(e){
  print('Clean some shared files')
  # do something to remove files
  file_exists <<- FALSE
}

# e1, e2 both require file existence
e1 <- new.env()
e1$valid <- function(){ file_exists }
e2 <- new.env()
e2$valid <- function(){ file_exists }

e1$valid(); e2$valid()
#> [1] TRUE
#> [1] TRUE

# we don't want to remove files when either e1,e2 gets
# garbage collected, however, we want to run `clear_files`
# when system garbage collecting *both* e1 and e2

# Make sure `key`s are identical
shared_finalizer(e1, 'cleanXXXfiles', clear_files)
shared_finalizer(e2, 'cleanXXXfiles', clear_files)

# Now remove e1, files are not cleaned, and e2 is still valid
rm(e1); invisible(gc(verbose = FALSE))
e2$valid()  # TRUE
#> [1] TRUE
file_exists # TRUE
#> [1] TRUE

# remove both e1 and e2, and file gets removed
rm(e2); invisible(gc(verbose = FALSE))
#> [1] "Clean some shared files"
file_exists  # FALSE
#> [1] FALSE

# ------------ R6 example ------------

cls <- R6::R6Class(
  classname = '...demo...',
  cloneable = TRUE,
  private = list(
    finalize = function(){
      cat('Finalize private resource\n')
    }
  ),
  public = list(
    file_path = character(0),
    shared_finalize = function(){
      cat('Finalize shared resource - ', self$file_path, '\n')
    },
    initialize = function(file_path){
      self$file_path = file_path
      shared_finalizer(self, key = self$file_path)
    }
  )
)
e1 <- cls$new('file1')
rm(e1); invisible(gc(verbose = FALSE))
#> Finalize private resource
#> Finalize shared resource -  file1 

e1 <- cls$new('file2')

# A copy of e1
e2 <- e1$clone()
# unfortunately, we have to manually register
shared_finalizer(e2, key = e2$file_path)

# Remove e1, gc only free private resource
rm(e1); invisible(gc(verbose = FALSE))
#> Finalize private resource

# remove e1 and e2, run shared finalize
rm(e2); invisible(gc(verbose = FALSE))
#> Finalize shared resource -  file2 
#> Finalize private resource

# ------------ fastmap/fastmap2 example -----------

# No formals needed for fastmap/fastmap2
fin <- function(){
  cat('Finalizer is called\n')
}
# single reference case
e1 <- dipsaus::fastmap2()
shared_finalizer(e1, 'fin-fastmap2', fin = fin)
invisible(gc(verbose = FALSE)) # Not triggered
rm(e1); invisible(gc(verbose = FALSE)) # triggered
#> Finalizer is called

# multiple reference case
e1 <- dipsaus::fastmap2()
e2 <- dipsaus::fastmap2()
shared_finalizer(e1, 'fin-fastmap2', fin = fin)
shared_finalizer(e2, 'fin-fastmap2', fin = fin)

rm(e1); invisible(gc(verbose = FALSE)) # Not triggered
rm(e2); invisible(gc(verbose = FALSE)) # triggered
#> Finalizer is called

# ------------ Native reg.finalizer fails example ------------

# This example shows a failure case using base::reg.finalizer

file_exists <- TRUE
clear_files <- function(e){
  print('Clean some shared files')
  # do something to remove files
  file_exists <<- FALSE
}

# e1, e2 both require file existence
e1 <- new.env()
e1$valid <- function(){ file_exists }
e2 <- new.env()
e2$valid <- function(){ file_exists }

reg.finalizer(e1, clear_files)
#> NULL
reg.finalizer(e2, clear_files)
#> NULL
gc()
#>           used (Mb) gc trigger  (Mb) max used  (Mb)
#> Ncells 1428982 76.4    2248520 120.1  2248520 120.1
#> Vcells 2653253 20.3   12317911  94.0 24058341 183.6
file_exists
#> [1] TRUE

# removing e1 will invalidate e2
rm(e1); gc()
#> [1] "Clean some shared files"
#>           used (Mb) gc trigger  (Mb) max used  (Mb)
#> Ncells 1429000 76.4    2248520 120.1  2248520 120.1
#> Vcells 2653319 20.3   12317911  94.0 24058341 183.6
e2$valid()    # FALSE
#> [1] FALSE

# Clean-ups
rm(e2); gc()
#> [1] "Clean some shared files"
#>           used (Mb) gc trigger  (Mb) max used  (Mb)
#> Ncells 1429014 76.4    2248520 120.1  2248520 120.1
#> Vcells 2653323 20.3   12317911  94.0 24058341 183.6
```
