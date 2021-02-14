# devtools::load_all()

test_that("System command", {

  get_os()
  get_cpu()
  get_ram()

  cmd <- rand_string()
  if(Sys.which(cmd) == ""){
    expect_true(is.na(safe_system2(cmd, character(0L))))
  }


})
