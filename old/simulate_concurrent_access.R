# Simulate concurrent access to PersistContainer
library(parallel)
library(dipsaus)

# Define a function to simulate concurrent writes
test_concurrent_access <- function(n = 10){
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

    cat(sprintf("Worker %d: Caching key %s with value %d and signature %d\n", id, key, value, signature))

    # Cache the value
    re <- container$cache(key = key, value = value, signature = signature, replace = TRUE, persist = TRUE)

    cat(sprintf("Worker %d: Checking if key %s exists with signature %d\n", id, key, signature))

    # Retrieve the value
    retrieved <- container$has(key, signature = signature)
    cat(sprintf("Worker %d: Key %s exists: %s\n", id, key, retrieved))
    ifelse(retrieved, retrieved, re)
  }

  # Run the workers in parallel
  cl <- makeCluster(n, type = "PSOCK", outfile = "")
  on.exit({ stopCluster(cl) })
  clusterExport(cl, varlist = c("container", "worker"), envir = environment())
  retrieved <- parLapply(cl, 1:n, worker)

  print(unname(unlist(retrieved)))

  invisible(retrieved)
}

# Run the simulation
retrieved <- replicate(20, {
  test_concurrent_access(3)
})

print(retrieved)
all(unlist(retrieved))
