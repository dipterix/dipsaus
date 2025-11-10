# Wrapper to cache key-value pairs and persist across sessions

This class is designed to persist arbitrary R objects locally and share
across different sessions. The container consists two-level caches. The
first one is session-based, meaning it's only valid under current R
session and will be cleared once the session is shut down. The second is
the persist-level map, which will persist to hard drive and shared
across sessions. See `cache` method in 'details'.

## Public Methods

- `initialize(..., backend = rds_map)`:

  The constructor. backend must inherit `AbstractMap`, `...` will be
  passed to `backend$new(...)`. To check available back-ends and their
  use cases, see [`map`](https://dipterix.org/dipsaus/reference/map.md).

- `reset(all = FALSE)`:

  Reset container. If all is set to be true, then reset session-based
  and hard-drive-based, otherwise only reset session-based container.

- `destroy(all = FALSE)`:

  destroy the container. Only use it when you want to finalize the
  container in
  [`reg.finalizer`](https://rdrr.io/r/base/reg.finalizer.html).

- `has(key, signature = NULL)`:

  returns a list of true/false (logical) vectors indicating whether keys
  exist in the container, if signature is used when caching the
  key-value pairs, then it also checks whether signature matches. This
  is very important as even if the keys match but signature is wrong,
  the results will be false.

- `remove(keys, all = TRUE)`:

  Remove keys in the container. Default is to remove the keys in both
  levels. If `all=FALSE`, then only remove the key in current session

- `cache(key, value, signature = NULL, replace = FALSE, persist = FALSE)`:

  `key` and `signature` together form the unique identifier for the
  value. By default `signature` is none, but it's very useful when value
  if large, or `key` is not a string. `replace` indicates whether to
  force replace the key-value pairs even if the entry exists. If
  `persist` is true, then the value is stored in hard-disks, otherwise
  the value will be deleted once the session is closed.

## See also

[`map`](https://dipterix.org/dipsaus/reference/map.md)

## Examples

``` r
container = PersistContainer$new(tempfile())

# Reset the container so that values are cleared
container$reset(all = TRUE)

# Store `1` to 'a' with signature 111 to a non-persist map
# returns 1
container$cache(key = 'a', value = 1, signature = 111, persist = FALSE)
#> [1] 1

# Replace 'a' with 3
# returns 3
container$cache(key = 'a', value = 3, signature = 111,
                persist = TRUE, replace = TRUE)
#> [1] 3

# check if 'a' exists with signature 111
container$has('a', signature = 111)    # TRUE
#>    a 
#> TRUE 
# When you only have 'a' but no signature
container$has('a')                     # TRUE
#>    a 
#> TRUE 
# check if 'a' exists with wrong signature 222
container$has('a', signature = 222)    # FALSE
#>     a 
#> FALSE 


# Store 'a' with 2 with same signature
# will fail and ignore the value (value will not be evaluated if signatured)
# Return 2 (Important! use cached values)
container$cache(key = 'a', value = {
  print(123)
  return(2)
}, signature = 111, replace = FALSE)
#> [1] 3

# When no signature is present
# If the key exists (no signature provided), return stored value
# returns 3
container$cache(key = 'a', value = 4)
#> [1] 3

# replace is TRUE (no signature provided), signature will be some default value
container$cache(key = 'a', value = 2, replace = TRUE)
#> [1] 2

# destroy the container to free disk space
container$destroy()
```
