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



