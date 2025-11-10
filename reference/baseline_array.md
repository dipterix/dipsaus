# Calculate Contrasts of Arrays in Different Methods

Provides five methods to baseline an array and calculate contrast.

## Usage

``` r
baseline_array(
  x,
  along_dim,
  baseline_indexpoints,
  unit_dims = seq_along(dim(x))[-along_dim],
  method = c("percentage", "sqrt_percentage", "decibel", "zscore", "sqrt_zscore",
    "subtract_mean")
)
```

## Arguments

- x:

  array (tensor) to calculate contrast

- along_dim:

  integer range from 1 to the maximum dimension of `x`. baseline along
  this dimension, this is usually the time dimension.

- baseline_indexpoints:

  integer vector, which index points are counted into baseline window?
  Each index ranges from 1 to `dim(x)[[along_dim]]`. See Details.

- unit_dims:

  integer vector, baseline unit: see Details.

- method:

  character, baseline method options are: `"percentage"`,
  `"sqrt_percentage"`, `"decibel"`, `"zscore"`, and `"sqrt_zscore"`

## Value

Contrast array with the same dimension as `x`.

## Details

Consider a scenario where we want to baseline a bunch of signals
recorded from different locations. For each location, we record `n`
sessions. For each session, the signal is further decomposed into
frequency-time domain. In this case, we have the input `x` in the
following form: \$\$session x frequency x time x location\$\$ Now we
want to calibrate signals for each session, frequency and location using
the first 100 time points as baseline points, then the code will be
\$\$baseline_array(x, along_dim=3, 1:100, unit_dims=c(1,2,4))\$\$
`along_dim=3` is dimension of time, in this case, it's the third
dimension of `x`. `baseline_indexpoints=1:100`, meaning the first 100
time points are used to calculate baseline. `unit_dims` defines the unit
signal. Its value `c(1,2,4)` means the unit signal is per session (first
dimension), per frequency (second) and per location (fourth).

In some other cases, we might want to calculate baseline across
frequencies then the unit signal is \\frequency x time\\, i.e. signals
that share the same session and location also share the same baseline.
In this case, we assign `unit_dims=c(1,4)`.

There are five baseline methods. They fit for different types of data.
Denote \\z\\ is an unit signal, \\z_0\\ is its baseline slice. Then
these baseline methods are:

- `"percentage"`:

  \$\$ \frac{z - \bar{z\_{0}}}{\bar{z\_{0}}} \times 100\\ \$\$

- `"sqrt_percentage"`:

  \$\$ \frac{\sqrt{z} - \bar{\sqrt{z\_{0}}}}{\bar{\sqrt{z\_{0}}}} \times
  100\\ \$\$

- `"decibel"`:

  \$\$ 10 \times ( \log\_{10}(z) - \bar{\log\_{10}(z\_{0})} ) \$\$

- `"zscore"`:

  \$\$ \frac{z-\bar{z\_{0}}}{sd(z\_{0})} \$\$

- `"sqrt_zscore"`:

  \$\$ \frac{\sqrt{z}-\bar{\sqrt{z\_{0}}}}{sd(\sqrt{z\_{0}})} \$\$

## Examples

``` r

library(dipsaus)
set.seed(1)

# Generate sample data
dims = c(10,20,30,2)
x = array(rnorm(prod(dims))^2, dims)

# Set baseline window to be arbitrary 10 timepoints
baseline_window = sample(30, 10)

# ----- baseline percentage change ------

# Using base functions
re1 <- aperm(apply(x, c(1,2,4), function(y){
  m <- mean(y[baseline_window])
  (y/m - 1) * 100
}), c(2,3,1,4))

# Using dipsaus
re2 <- baseline_array(x, 3, baseline_window, c(1,2,4),
                      method = 'percentage')

# Check different, should be very tiny (double precisions)
range(re2 - re1)
#> [1] -4.547474e-13  1.818989e-12

# Check speed for large dataset
if(interactive()){
  dims = c(200,20,300,2)
  x = array(rnorm(prod(dims))^2, dims)
  # Set baseline window to be arbitrary 10 timepoints
  baseline_window = seq_len(100)
  f1 <- function(){
    aperm(apply(x, c(1,2,4), function(y){
      m <- mean(y[baseline_window])
      (y/m - 1) * 100
    }), c(2,3,1,4))
  }
  f2 <- function(){
    # equivalent as bl = x[,,baseline_window, ]
    #
    baseline_array(x, along_dim = 3,
                   baseline_indexpoints = baseline_window,
                   unit_dims = c(1,2,4), method = 'sqrt_percentage')
  }
  microbenchmark::microbenchmark(f1(), f2(), times = 3L)
}


```
