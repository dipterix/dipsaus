
test_that('testing shift_array correctness', {
  # Make sure the temporary folders are cleared
  lapply(c("data", "cache", "config"), function(which) {
    root_path <- dipsaus:::R_user_dir("dipsaus", which = which)
    dipsaus:::remove_empty_dir(root_path, recursive = TRUE)

    sub_dirs <- list.dirs(root_path, full.names = TRUE, recursive = TRUE)
    for(dir in sub_dirs) {
      expr <- bquote(testthat::expect_gt(
        length(list.files(.(dir))), 0
      ))
      eval(expr)
    }
  })

})
