# Enable progress tracking for directory uploads

Helper function to set up progress tracking using
[`progress2`](https://dipterix.org/dipsaus/reference/progress2.md) for
[`fancyDirectoryInput`](https://dipterix.org/dipsaus/reference/fancyDirectoryInput.md).
Call this in your server function to enable automatic progress updates.

## Usage

``` r
observeDirectoryProgress(inputId, session = shiny::getDefaultReactiveDomain())
```

## Arguments

- inputId:

  the input ID of the `fancyDirectoryInput`

- session:

  the Shiny session object (default:
  [`shiny::getDefaultReactiveDomain()`](https://rdrr.io/pkg/shiny/man/domains.html))

## Value

invisible NULL; sets up reactive observers for progress tracking

## Examples

``` r
if (FALSE) { # \dontrun{
server <- function(input, output, session) {
  # Enable progress tracking
  observeDirectoryProgress("dir_input")

  # Your other server logic...
}
} # }
```
