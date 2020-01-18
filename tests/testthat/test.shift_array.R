context("shift_array")


f <- function(...){
  dim <- c(10,12,30,1)
  x <- array(rnorm(prod(dim)), dim)

  shifts <- sample(dim[3], dim[1])
  shifts[1] <- NA
  f1 <- function(){
    dipsaus::shift_array(x, 3, 1, shifts)
  }
  f2 <- function(){
    tm <- seq_len(dim[3])
    re <- sapply(seq_len(dim[1]), function(ii){
      shift <- shifts[ii]
      new_idx <- tm + shift
      new_idx[new_idx > dim[3]] <- NA
      x[ii,,new_idx,]
    })
    dim(re) <- c(dim(x)[-1], dim[1])
    re <- aperm(re, c(4,1,2,3))
    re
  }

  rg <- range(f2()-f1(), na.rm = TRUE)
  expect_equivalent(rg, c(0,0))


}

test_that('testing shift_array correctness', {

  capture_output({
    lapply(1:5, f)

    dim <- c(10,12,30,1)
    x <- array(rnorm(prod(dim)), dim)

    shifts <- sample(dim[3], dim[1])
    shifts[1] <- NA
    expect_error({
      dipsaus::shift_array(x, 3, 3, shifts)
    })

    expect_error({
      dipsaus::shift_array(x, 3, 0, shifts)
    })

    expect_error({
      dipsaus::shift_array(x, 3, -1, shifts)
    })

    expect_error({
      dipsaus::shift_array(x, 3, 5, shifts)
    })

    expect_error({
      dipsaus::shift_array(x, 0, 1, shifts)
    })
    expect_error({
      dipsaus::shift_array(x, -1, 1, shifts)
    })
    expect_error({
      dipsaus::shift_array(x, 5, 1, shifts)
    })
  })
})

