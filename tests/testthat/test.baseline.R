context("baseline")

test_that('testing baseline correctness - full unit_dims', {

  dims <- c(10,20,30,2)
  x <- array(rnorm(prod(dims))^2, dims)
  baseline_window <- sample(30, 10)

  # time_idx = 3
  # unit_indices = c(1,2,4)

  # ----- baseline percentage change ------
  re <- apply(x, c(1,2,4), function(y){
    m <- mean(y[baseline_window])
    (y/m - 1) * 100
  })
  re <- aperm(re, c(2,3,1,4))

  dif0 <- re - baseline_array(x, 3, baseline_window, c(1,2,4), method = 'percentage')
  dif0 <- max(abs(range(dif0)))

  expect_lt(dif0, 1e-10)

  # ----- sqrt, then baseline percentage change ------
  re <- apply(sqrt(x), c(1,2,4), function(y){
    m <- mean(y[baseline_window])
    (y/m - 1) * 100
  })
  re <- aperm(re, c(2,3,1,4))
  dif0 <- re - baseline_array(x, 3, baseline_window, c(1,2,4), method = 'sqrt_percentage')
  dif0 <- max(abs(range(dif0)))

  expect_lt(dif0, 1e-10)

  # ----- decibel contrast ------
  re <- apply(log10(x) * 10, c(1,2,4), function(y){
    m <- mean(y[baseline_window])
    y - m
  })
  re <- aperm(re, c(2,3,1,4))
  dif0 <- re - baseline_array(x, 3, baseline_window, c(1,2,4), method = 'decibel')
  dif0 <- max(abs(range(dif0)))

  expect_lt(dif0, 1e-10)


  # ----- zscore ------
  re <- apply(x, c(1,2,4), function(y){
    m <- mean(y[baseline_window])
    sd <- sd(y[baseline_window])
    (y - m) / sd
  })
  re <- aperm(re, c(2,3,1,4))
  dif0 <- re - baseline_array(x, 3, baseline_window, c(1,2,4), method = 'zscore')
  dif0 <- max(abs(range(dif0)))

  expect_lt(dif0, 1e-10)



  # ----- sqrt then zscore ------
  re <- apply(sqrt(x), c(1,2,4), function(y){
    m <- mean(y[baseline_window])
    sd <- sd(y[baseline_window])
    (y - m) / sd
  })
  re <- aperm(re, c(2,3,1,4))
  dif0 <- re - baseline_array(x, 3, baseline_window, c(1,2,4), method = 'sqrt_zscore')
  dif0 <- max(abs(range(dif0)))

  expect_lt(dif0, 1e-10)

})

test_that('testing baseline correctness - partial unit_dims', {

  dims <- c(10,20,30,2)
  x <- array(rnorm(prod(dims))^2, dims)
  baseline_window <- sample(30, 10)

  # time_idx = 3
  # unit_indices = c(1,2,4)

  # ----- baseline percentage change ------
  re <- apply(x, c(1,4), function(y){
    m <- mean(y[,baseline_window])
    (y/m - 1) * 100
  })
  dim(re) <- c(20, 30, 10, 2)
  re <- aperm(re, c(3,1,2,4))

  dif0 <- re - baseline_array(x, 3, baseline_window, c(1,4), method = 'percentage')
  dif0 <- max(abs(range(dif0)))

  expect_lt(dif0, 1e-10)

  # ----- sqrt, then baseline percentage change ------
  re <- apply(sqrt(x), c(1,4), function(y){
    m <- mean(y[,baseline_window])
    (y/m - 1) * 100
  })
  dim(re) <- c(20, 30, 10, 2)
  re <- aperm(re, c(3,1,2,4))

  dif0 <- re - baseline_array(x, 3, baseline_window, c(1,4), method = 'sqrt_percentage')
  dif0 <- max(abs(range(dif0)))

  expect_lt(dif0, 1e-10)

  # ----- decibel contrast ------
  re <- apply(10*log10(x), c(1,4), function(y){
    m <- mean(y[,baseline_window])
    y - m
  })
  dim(re) <- c(20, 30, 10, 2)
  re <- aperm(re, c(3,1,2,4))

  dif0 <- re - baseline_array(x, 3, baseline_window, c(1,4), method = 'decibel')
  dif0 <- max(abs(range(dif0)))

  expect_lt(dif0, 1e-10)


  # ----- zscore ------
  re <- apply(x, c(1,4), function(y){
    m <- mean(y[,baseline_window])
    sd <- sd(y[,baseline_window])
    (y - m) / sd
  })
  dim(re) <- c(20, 30, 10, 2)
  re <- aperm(re, c(3,1,2,4))

  dif0 <- re - baseline_array(x, 3, baseline_window, c(1,4),  method = 'zscore')
  dif0 <- max(abs(range(dif0)))

  expect_lt(dif0, 1e-10)



  # ----- sqrt then zscore ------
  re <- apply(sqrt(x), c(1,4), function(y){
    m <- mean(y[,baseline_window])
    sd <- sd(y[,baseline_window])
    (y - m) / sd
  })
  dim(re) <- c(20, 30, 10, 2)
  re <- aperm(re, c(3,1,2,4))

  dif0 <- re - baseline_array(x, 3, baseline_window, c(1,4),  method = 'sqrt_zscore')
  dif0 <- max(abs(range(dif0)))

  expect_lt(dif0, 1e-10)

})

