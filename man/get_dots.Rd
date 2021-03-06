% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/language.R
\name{get_dots}
\alias{get_dots}
\title{Get element from dots \code{'...'}}
\usage{
get_dots(..name, ..default = NULL, ...)
}
\arguments{
\item{..name}{character name of the argument}

\item{..default}{R object to return if argument not found}

\item{...}{dots that contains argument}
}
\description{
Get specific key values from \code{'...'} without
evaluating the rest of arguments.
}
\details{
One could use \code{list(...)[[name]]} to extract any keys
from the dots. However, such way reduces code readability. If
some arguments have not evaluated, \code{list(...)} will
\code{\link{force}} evaluating them. Normally it's fine if
these expressions take little time to run, but if the
expression require time to run, \code{\link{get_dots}} avoids
unnecessary evaluations.
}
\examples{


# ------------------------ Basic Usage ---------------------------
plot2 <- function(...){
  title = get_dots('main', 'There is no title', ...)
  plot(...)
  title
}

plot2(1:10)
plot2(1:10, main = 'Scatter Plot of 1:10')

# ------------------------ Comparisons ----------------------------
f1 <- function(...){ get_dots('x', ...) }
f2 <- function(...){ list(...)[['x']] }
delayedAssign('y', { cat('y is evaluated!') })

# y will not evaluate
f1(x = 1, y = y)

# y gets evaluated
f2(x = 1, y = y)

# -------------------- Decorator example --------------------------
ret_range <- function(which_range = 'y'){
  function(f){
    function(...){
      f(...)
      y_range <- range(get_dots(which_range, 0, ...))
      y_range
    }
  }
}
plot_ret_yrange <- plot \%D\% ret_range('y')
plot_ret_yrange(x = 1:10, y = rnorm(10))


}
