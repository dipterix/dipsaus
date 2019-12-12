context("collapse")

test_that('testing collapse with 100x100x10x10', {
  dim <- c(100, 3, 1, 100)
  x <- array(rnorm(prod(dim)), dim = dim)

  # Sum
  r1 <- apply(x, c(3,2), sum)
  r2 <- collapse(x, c(3,2))

  expect_lt(max(abs(r1 - r2)), 1e-10)

  # Mean
  r1 <- apply(x, c(3,2), mean)
  r2 <- collapse(x, c(3,2), average = TRUE)

  expect_lt(max(abs(r1 - r2)), 1e-10)

})

test_that('testing collapse with complex number', {
  dim <- c(100, 3, 1, 100)
  re <- array(rnorm(prod(dim)), dim = dim)
  im <- array(rnorm(prod(dim)), dim = dim)
  x <- re + 1i*im

  # Sum
  r1 <- apply(x, c(3,2), sum)
  r2 <- collapse(x, c(3,2))

  expect_lt(max(abs(r1 - r2)), 1e-10)

  # Mean
  r1 <- apply(x, c(3,2), mean)
  r2 <- collapse(x, c(3,2), average = TRUE)

  expect_lt(max(abs(r1 - r2)), 1e-10)
})



context("shift_array")
test_that('testing shift_array correctness', {
  dim = c(10,10,30,1)
  x = array(rnorm(prod(dim)), dim)

  tidx = 3
  sidx = 1
  shifts = sample(dim[3], dim[1])
  shifts[1] = NA
  f1 = function(){
    dipsaus::shift_array(x, 3, 1, shifts)
  }
  f2 = function(){
    tm = seq_len(dim[3])
    re = sapply(seq_len(dim[1]), function(ii){
      shift = shifts[ii]
      new_idx = tm + shift
      new_idx[new_idx > dim[3]] = NA
      x[ii,,new_idx,]
    })
    dim(re) = c(dim(x)[-1], dim[1])
    re = aperm(re, c(4,1,2,3))
    re
  }

  rg = range(f2()-f1(), na.rm = TRUE)
  expect_equivalent(rg, c(0,0))
})

