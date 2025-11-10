# Progress-bar Handler

Handler for
[`progress2`](https://dipterix.org/dipsaus/reference/progress2.md) to
support
[`progressr::handlers`](https://progressr.futureverse.org/reference/handlers.html).
See examples for detailed use case

## Usage

``` r
handler_dipsaus_progress(
  title = getOption("dipsaus.progressr.title", "Progress"),
  intrusiveness = getOption("progressr.intrusiveness.gui", 1),
  target = if (is.null(shiny::getDefaultReactiveDomain())) "terminal" else "gui",
  enable = interactive() || shiny_is_running(),
  ...
)
```

## Arguments

- title:

  default title of
  [`progress2`](https://dipterix.org/dipsaus/reference/progress2.md)

- intrusiveness:

  A non-negative scalar on how intrusive (disruptive) the reporter to
  the user

- target:

  where progression updates are rendered

- enable:

  whether the progress should be reported

- ...:

  passed to
  [`make_progression_handler`](https://progressr.futureverse.org/reference/make_progression_handler.html)

## Examples

``` r

library(progressr)
library(shiny)
library(future)

## ------------------------------ Setup! -------------------------------
handlers(handler_dipsaus_progress())

# ------------------------------ A simple usage ------------------------
xs <- 1:5
handlers(handler_dipsaus_progress())
with_progress({
  p <- progressor(along = xs)
  y <- lapply(xs, function(x) {
    p(sprintf("x=%g", x))
    Sys.sleep(0.1)
    sqrt(x)
  })
})

# ------------------------ A future.apply case -------------------------
plan(sequential)
# test it yourself with plan(multisession)

handlers(handler_dipsaus_progress())
with_progress({
  p <- progressor(along = xs)
  y <- future.apply::future_lapply(xs, function(x) {
    p(sprintf("x=%g", x))
    Sys.sleep(0.1)
    sqrt(x)
  })
})

# ------------------------ A shiny case --------------------------------

ui <- fluidPage(
  actionButton('ok', 'Run Demo')
)

server <- function(input, output, session) {
  handlers(handler_dipsaus_progress())
  make_forked_clusters()

  observeEvent(input$ok, {
    with_progress({
      p <- progressor(along = 1:100)
      y <- future.apply::future_lapply(1:100, function(x) {
        p(sprintf("Input %d|Result %d", x, x+1))
        Sys.sleep(1)
        x+1
      })
    })
  })
}

if(interactive()){
  shinyApp(ui, server)
}


```
