## Resubmission

Feedback from `Jelena Saf`:

```
What are you referring to with the phrase "dipping sauce"? Would there
maybe be a way to make the use of this package clearer?

Please add \value to .Rd files and explain the functions results in the
documentation.
e.g.: col2hexStr.Rd
```

Solution:

1. I edited `DESCRIPTION` to point out the goal of this package from four perspectives:
  * shiny inputs
  * high-performance computing using `RcppParallel` and `future`
  * modify `R` calls
  * system utility functions
2. Added values and examples to newly added functions.


