# Test queues

context("queue")

testthat::skip('queue is deprecated')

test_queue <- function(generator, ...){
  make_queue <- function(...){
    generator$new(...)
  }

  self <- make_queue(...)
  self$reset()

  expect_equal(self$total, 0)
  self$push(1)
  self$push(1:2)
  self$push(list(1,2,3))

  expect_true(self$count == 3)

  re <- self$pop()

  expect_identical(re[[1]]$value, 1)
  expect_true(self$total == 3)
  expect_true(self$head == 1)

  expect_equal(nrow(self$log(all=TRUE)), 3)

  self$clean()

  expect_true(self$total == 2)
  expect_true(self$head == 0)

  re <- self$pop()

  expect_identical(re[[1]]$value, 1:2)
  expect_true(self$total == 2)
  expect_true(self$head == 1)

  self <- make_queue(...)

  re <- self$pop()

  expect_identical(re[[1]]$value, list(1,2,3))

  expect_true(self$count == 0)

  if( !identical(generator, dipsaus:::TextQueue) ){
    # save environment
    e <- new.env()
    e$a <- 123
    self$push(e)

    ep <- self$pop()
    expect_equal(ep[[1]]$value$a, 123)
  }


  self$destroy()


  utils::capture.output(expect_error(suppressWarnings(self$validate())))
}


test_that('testing SessionQueue', {
  map <- fastmap::fastmap()
  test_queue(dipsaus:::SessionQueue, map = map)
})

test_that('testing FileQueue', {
  path <- tempfile()
  test_queue(dipsaus:::FileQueue, path)
})

test_that('testing TextQueue', {
  path <- tempfile()
  test_queue(dipsaus:::TextQueue, path)
})


test_that('testing QsQueue', {
  path <- tempfile()
  test_queue(dipsaus:::QsQueue, path)
})



test_that('testing RedisQueue', {
  has_redis <- tryCatch({
    new( RcppRedis::Redis )
    TRUE
  }, error = function(e){
    FALSE
  })
  skip_if(!has_redis, message = 'No Redis detected, skip :)')
  test_queue(dipsaus:::RedisQueue, 'junk')
})
