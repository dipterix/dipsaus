
context("map")

test_map <- function(generator, ...){
  make_map <- function(...){
    generator$new(...)
  }
  self <- make_map(...)
  private <- self$.__enclos_env__$private
  self$reset()
  self$validate()
  expect_equal(self$size(), 0)
  expect_identical(self$as_list(), list())

  expect_identical(self$set('a', 124), self$digest(124))
  self$validate()
  expect_equal(self$get('a'), 124)
  self$set('b', 11231)
  expect_equal(self$get('b'), 11231)
  self$validate()
  self$set('a', 123)
  self$validate()
  expect_true(all(c('a', 'b') %in% self$keys()))

  expect_equal(dim(self$keys(include_signatures = TRUE)), c(2,2))
  expect_true(self$has(keys = 'a'))
  expect_true(self$has(keys = 'a', signature = 123))
  expect_true(self$has(keys = 'a', signature = self$digest(123), sig_encoded = TRUE))

  expect_false(self$has(keys = 'a', signature = self$digest(123), sig_encoded = FALSE))

  self$validate()
  expect_equal(self$mget(c('a', 'b', 'a')), list(a=123, b = 11231, a = 123))

  expect_equal(self$as_list(sort = TRUE), list(a=123, b = 11231))

  expect_null( self$get('c') )
  expect_equal( self$get('c', missing_default = 'aaa'), 'aaa' )
  self$missing_default <- 'hahaha'
  expect_equal(self$get('c'), 'hahaha')
  expect_equal( self$get('c', missing_default = 'aaa'), 'aaa' )

  self$remove('a')

  self <- make_map(...)

  expect_equivalent(self$has(c('a', 'b')), c(FALSE, TRUE))

  expect_equal(self$size(), 1)

  self$reset()
  expect_equal(self$size(), 0)
  expect_equivalent(self$has(c('a', 'b')), c(FALSE, FALSE))

  expect_true(self$is_valid)
  self$destroy()

  expect_false(self$is_valid)

  utils::capture.output(expect_error(suppressWarnings(self$validate())))
}


test_that('Session map', {
  map <- fastmap::fastmap()
  test_map( dipsaus:::SessionMap, map )
})

test_that('RDS map', {
  path <- tempfile()
  test_map( dipsaus:::FileMap, path )
})

# test_that('QS map', {
#   path <- tempfile()
#   test_map( dipsaus:::QsMap, path )
# })

test_that('Redis map', {
  has_redis <- tryCatch({
    new( RcppRedis::Redis )
    TRUE
  }, error = function(e){
    FALSE
  })
  skip_if(!has_redis, message = 'No Redis detected, skip :)')
  test_map( dipsaus:::RedisMap, 'test' )
})

test_that('Persist container', {
  container <- dipsaus::PersistContainer$new(tempfile())

  v <- container$cache(key = 'a', value = 1, signature = 111, persist = FALSE)
  expect_equal(v, 1)
  v <- container$cache(key = 'a', value = 3, signature = 111, persist = TRUE, replace = TRUE)
  expect_equal(v, 3)

  expect_true(container$has('a', signature = 111))
  expect_true(container$has('a'))

  v <- container$cache(key = 'a', value = 2, signature = 111, replace = FALSE)
  expect_equal(v, 3)

  v <- container$cache(key = 'a', value = 4)
  expect_equal(v, 3)

  expect_true(container$has('a'))
  v <- container$cache(key = 'a', value = 2)
  expect_equal(v, 3)

  # We still have 'a' with signature 111 because that value was persisted
  expect_true(container$has('a', signature = 111))

  v <- container$cache(key = 'a', value = 4, persist = TRUE, replace = TRUE)
  expect_equal(v, 4)

  # persisted value is overwritten
  expect_false(container$has('a', signature = 111))

  container$destroy()
})

