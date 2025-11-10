# Update compound inputs

Update compound inputs

## Usage

``` r
updateCompoundInput2(
  session,
  inputId,
  value = NULL,
  ncomp = NULL,
  initialization = NULL,
  ...
)
```

## Arguments

- session:

  shiny session or session proxy

- inputId:

  character see `compoundInput2`

- value:

  list of lists, see `compoundInput2` or examples

- ncomp:

  integer, non-negative number of groups to update, `NULL` to remain
  unchanged

- initialization, ...:

  named list of other updates

## Value

none

## See also

[`compoundInput2`](https://dipterix.org/dipsaus/reference/compoundInput2.md)
for how to define components.

## Examples

``` r
if (FALSE) { # \dontrun{
library(shiny); library(dipsaus)

## UI side
compoundInput2(
  'input_id', 'Group',
    div(
    textInput('text', 'Text Label'),
    sliderInput('sli', 'Slider Selector', value = 0, min = 1, max = 1)
  ),
  label_color = 1:10,
  value = list(
    list(text = '1'),  # Set text first group to be "1"
    '',                # no settings for second group
    list(sli = 0.2)    # sli = 0.2 for the third group
  ))

## server side:
updateCompoundInput2(session, 'inputid',
                     # Change the first 3 groups
                     value = lapply(1:3, function(ii){
                       list(sli = runif(1))
                     }),
                     # Change text label for all groups
                     initialization = list(
                       text = list(label = as.character(Sys.time()))
                     ))
} # }
```
