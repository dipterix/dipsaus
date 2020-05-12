require(testthat)


context('fastmap2')

test_that('fastmap2', {
  map <- fastmap2(missing_default = '')

  expect_equal(map$a, '')
  expect_equal(map[['a']], '')
  expect_equal(map[[1]], '')

  map$a <- 123
  map[1] <- 234

  expect_equal(map$a, 123)
  expect_equal(map[['a']], 123)
  expect_equal(map[[1]], 234)
  expect_equal(map[['1']], 234)

  map[1:10] <- as.list(1:10)
  expect_equal(map[['1']], 1)
  map[1:10] <- 1
  expect_equal(map[['1']], 1)
  expect_equal(map[['2']], 1)

  map[1:10] <- NULL
  expect_null(map[['1']])

  list_to_fastmap2(list(a = NULL), map)
  expect_null(map$a)


})
