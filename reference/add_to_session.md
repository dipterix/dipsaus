# Store/Get key-value pairs in 'shiny' session

If key is missing, it'll be created, otherwise ignored or overwritten.

## Usage

``` r
add_to_session(
  session,
  key = "rave_id",
  val = paste(sample(c(letters, LETTERS, 0:9), 20), collapse = ""),
  override = FALSE
)
```

## Arguments

- session:

  'Shiny' session

- key:

  character, key to store

- val:

  value to store

- override:

  if key exists, whether to overwrite its value

## Value

If session is shiny session, returns current value stored in session,
otherwise returns `NULL`
