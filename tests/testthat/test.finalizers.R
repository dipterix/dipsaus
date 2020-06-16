context('Finalizer')

test_that('Finalizer', {

  counts <- dipsaus::fastmap2()
  counts[['run']] <- 0
  fin <- function(e){
    counts[['run']] <- counts[['run']] + 1
    invisible()
  }

  e1 <- new.env()
  dipsaus_sessionfinalizer$finalize()

  finalizer <- dipsaus_sessionfinalizer$register(key = digest::digest(fin), object = e1,
                                                 finalizer = fin, onexit = FALSE)
  reg.finalizer(e1, finalizer)
  invisible(gc(verbose = FALSE))
  expect_equal(counts[['run']], 0)
  rm(e1)
  invisible(gc(verbose = FALSE))

  expect_equal(counts[['run']], 1)

  e1 <- new.env()
  finalizer1 <- dipsaus_sessionfinalizer$register(key = '123', object = e1,
                                                  finalizer = fin, onexit = FALSE)
  reg.finalizer(e1, finalizer1)
  finalizer12 <- dipsaus_sessionfinalizer$register(key = '124', object = e1,
                                                   finalizer = fin, onexit = FALSE)
  reg.finalizer(e1, finalizer12)
  finalizer13 <- dipsaus_sessionfinalizer$register(key = '124', object = e1,
                                                   finalizer = fin, onexit = FALSE)
  reg.finalizer(e1, finalizer13)

  e2 <- new.env()
  finalizer2 <- dipsaus_sessionfinalizer$register(key = '123', object = e2,
                                                  finalizer = fin, onexit = FALSE)

  reg.finalizer(e2, finalizer2)

  invisible(gc(verbose = FALSE))
  expect_equal(counts[['run']], 1)
  rm(e1)
  invisible(gc(verbose = FALSE))
  expect_equal(counts[['run']], 2)
  rm(e2)
  invisible(gc(verbose = FALSE))
  expect_equal(counts[['run']], 3)

})

