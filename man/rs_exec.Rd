% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-rstudio.R
\name{rs_exec}
\alias{rs_exec}
\title{Schedule a Background Job}
\usage{
rs_exec(
  expr,
  name = "Untitled",
  quoted = FALSE,
  rs = TRUE,
  as_promise = FALSE,
  wait = FALSE,
  packages = NULL,
  focus_on_console = FALSE,
  ...,
  nested_ok = FALSE
)
}
\arguments{
\item{expr}{R expression}

\item{name}{used by 'RStudio' as name of the job}

\item{quoted}{is \code{expr} quoted}

\item{rs}{whether to use 'RStudio' by default}

\item{as_promise}{whether to return as a \code{\link[promises]{promise}}
object; default is no}

\item{wait}{whether to wait for the result.}

\item{packages}{packages to load in the sub-sessions}

\item{focus_on_console}{whether to return back to console after creating
jobs; useful when users want to focus on writing code; default is false.
This feature works with 'RStudio' (\code{>=1.4})}

\item{...}{internally used}

\item{nested_ok}{whether nested \code{rs_exec} is allowed; default is false;
Set to true to allow nested parallel code, but use at your own risk.}
}
\value{
If \code{wait=TRUE}, returns evaluation results of \code{expr},
otherwise a function that can track the state of job.
}
\description{
Utilizes 'RStudio' job scheduler if correct environment is
detected, otherwise call system command via \code{Rscript}
}
\details{
'RStudio' provides interfaces \code{\link[rstudioapi]{jobRunScript}} to
schedule background jobs. However, this
functionality only applies using 'RStudio' IDE. When launching R from
other places such as terminals, the job scheduler usually result in
errors. In this case, the alternative is to call system command via
\code{Rscript}

The expression \code{expr} will run a clean environment. Therefore R objects
created outside of the context will be inaccessible from within the child
environment, and packages except for base packages will not be loaded.

There is a small difference when running within and without 'RStudio'.
When running via \code{Rscript}, the environment will run under
\code{vanilla} argument, which means no load, no start-up code. If you
have start-up code stored at \code{~/.Rprofile}, the start-up code will be
ignored. When running within 'RStudio', the start-up code will be executed.
As of \code{rstudioapi} version 0.11, there is no 'vanilla' option. This
feature is subject to change in the future.
}
\examples{

if(interactive()){
  h <- rs_exec(
    {
      Sys.sleep(2)
      print(Sys.getpid())
    },
    wait = FALSE, name = 'Test',
    focus_on_console = TRUE
  )
  code <- h()
  print(code)

  # wait 3 seconds
  Sys.sleep(3)
  code <- h()
  attributes(code)
}

}
