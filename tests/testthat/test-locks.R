testthat::test_that("dipsaus_lock", {

  testthat::skip_on_cran()

  # Simulate concurrent access to PersistContainer
  library(parallel)

  # Define a function to simulate concurrent writes
  test_concurrent_access <- function(){
    # Create a temporary directory for the rds_map
    temp_dir <- tempfile()
    dir.create(temp_dir)

    # Initialize the PersistContainer
    container <- PersistContainer$new(temp_dir)

    # Define a worker function to perform cache operations
    worker <- function(id) {
      key <- paste0("key", id)
      value <- id
      signature <- id

      # cat(sprintf("Worker %d: Caching key %s with value %d and signature %d\n", id, key, value, signature))

      # Cache the value
      container$cache(key = key, value = value, signature = signature, replace = TRUE, persist = TRUE)

      # cat(sprintf("Worker %d: Checking if key %s exists with signature %d\n", id, key, signature))

      # Retrieve the value
      retrieved <- container$has(key, signature = signature)
      # cat(sprintf("Worker %d: Key %s exists: %s\n", id, key, retrieved))
      return(retrieved)
    }

    # Run the workers in parallel
    cl <- makeCluster(2, type = "PSOCK", outfile = "")
    clusterExport(cl, varlist = c("container", "worker"), envir = environment())
    re <- parLapply(cl, 1:2, worker)

    stopCluster(cl)
    re
  }

  # Run the simulation
  testthat::expect_no_warning({
    res <- test_concurrent_access()
  })

  testthat::expect_equal(unname(unlist(res)), rep(TRUE, 2))
})
