# Generate a random password

Please note that this function is not meant to be used in production. It
is not meant to be used for highly secured cryptographic purposes.

## Usage

``` r
get_credential(
  master_password,
  method = c("get_or_create", "replace", "query"),
  service = NULL,
  special_chr = "~`! @#$%^&*()_-+={[}]|:;'<,>.?/",
  tokenfile = NULL,
  verbose = FALSE
)
```

## Arguments

- master_password:

  a master password that only you know, should have at least 8
  characters

- method:

  whether to query token map, or to create the password, choices are
  `'get_or_create'` (default), `'replace'`, `'query'`; see 'Details'

- service:

  service name, must only contains letters, digits, equal sign,
  underscore, comma, dot, dash

- special_chr:

  special characters allowed in the password

- tokenfile:

  a file containing all the tokens. Warning: if you lose the token book,
  it is hard (not impossible, but impractical) to restore the passwords

- verbose:

  whether to print out service names; default is false

## Value

If method is `'query'`, returns token map; otherwise returns the
password itself

## Details

Please note that this function is not meant to be used in production or
anything that requires high security level. This is most likely for my
personal use since I am tired of storing the passwords on the cloud or
having to buy the services.

The encryption adopts `'sha256'` algorithm provided by
[`digest`](https://rdrr.io/pkg/digest/man/digest.html) function. To
restore a password, you will need twp components: `master_password`, a
token book ( `tokenfile`). If any of them is missing, then the password
is lost. Please store the token book properly (for example, in 'Dropbox'
vault).

The token book could be shared. Anyone who do not have master password
will be unlikely to restore the service password. Do not share the
master password with anyone other than yourself.

By default, `method='get_or_create'` will try to retrieve existing
tokens to generate password. If the token is missing, then a new token
will be generated. The `method='replace'` will ignore existing tokens
and directly create a new one.

## See also

[`digest`](https://rdrr.io/pkg/digest/man/digest.html)

## Examples

``` r
tokenfile <- tempfile()

# ---------- Create a password and store the tokens to token book ------
pass1 <- get_credential(
  master_password = "my password",
  service = "google.com:my_username",
  special_chr = "@#$%^&*",
  tokenfile = tokenfile
)
print(pass1)
#> [1] "u&tpjmm%^@bp3@hhwuk%47604y^u2fmt"

# ---------- Query existing tokens ------
token_params <- get_credential(
  method = "query",
  tokenfile = tokenfile,
  verbose = TRUE
)
#> google.com:my_username
#> 

print(token_params)
#> $`google.com:my_username`
#> $`google.com:my_username`$service
#> [1] "google.com:my_username"
#> 
#> $`google.com:my_username`$token
#> [1] "vPg5bEhgHAmJaOUHb3hW6YXVIxVakm54raJcUw7uj2yVu0r2M0"
#> 
#> $`google.com:my_username`$seed
#> [1] 1922935892         13         19         15         13
#> 
#> $`google.com:my_username`$specials
#> [1] "@#$%^&*"
#> 
#> 

# ---------- retrieve stored password ----------
pass2 <- get_credential(
  master_password = "my password",
  service = "google.com",
  tokenfile = tokenfile
)
identical(pass1, pass2)
#> [1] FALSE

# Using wrong master password
pass3 <- get_credential(
  master_password = "wrong password",
  service = "google.com",
  tokenfile = tokenfile
)
identical(pass1, pass3)
#> [1] FALSE

# ---------- Replace token ----------
# Existing token will be replaced with a new token
pass4 <- get_credential(
  master_password = "my password",
  method = "replace",
  service = "google.com",
  special_chr = "@#$%^&*",
  tokenfile = tokenfile
)
print(pass4)
#> [1] "yqaryhhscrq7xd@oyk@krkygwqfkoo08"
identical(pass1, pass4)
#> [1] FALSE
```
