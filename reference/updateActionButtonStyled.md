# Update styled action button

Update styled action button

## Usage

``` r
updateActionButtonStyled(
  session,
  inputId,
  label = NULL,
  icon = NULL,
  type = NULL,
  disabled = NULL,
  ...
)
```

## Arguments

- session, inputId, label, icon:

  passed to
  [`shiny::updateActionButton`](https://rdrr.io/pkg/shiny/man/updateActionButton.html)

- type:

  button type to update

- disabled:

  whether to disable the button

- ...:

  ignored

## Value

none

## See also

[`actionButtonStyled`](https://dipterix.org/dipsaus/reference/actionButtonStyled.md)
for how to define the button.
