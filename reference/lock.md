# Create or Unlock a Lock

A wrapper for 'synchronicity' package, but user can interrupt the lock
procedure anytime, and don't have to worry about whether the lock exists
or not.

## Usage

``` r
dipsaus_lock(name, timeout = 10, exclusive = TRUE)

dipsaus_unlock(name, timeout = 10, exclusive = TRUE)

dipsaus_resetlocks(name)
```

## Arguments

- name:

  character, the locker's name, must be only letters and digits

- timeout:

  numeric, seconds to wait for the locker to lock or unlock

- exclusive:

  ignored

## Value

Logical, whether the operation succeed.

## Examples

``` r
# Clear existing locks
dipsaus::dipsaus_resetlocks()

# unlock to prepare for the example
dipsaus_unlock('testlocker', timeout = 0.01)
#> [1] TRUE

# Create a locker, return TRUE
lock_success = dipsaus_lock('testlocker')
if(lock_success){
  cat2('testlocker has been locked')
}
#> testlocker has been locked 

# test whether locker has been locked
lock_success = dipsaus_lock('testlocker', timeout = 0.01)
if(!lock_success){
  cat2('attempt to lock testlocker failed')
}

# unlock
dipsaus_unlock('testlocker', timeout = 0.01)
#> [1] TRUE

# clean up
dipsaus::dipsaus_resetlocks()

```
