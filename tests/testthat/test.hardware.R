context("hardwares")

test_that('testing get_ram', {
  ram <- dipsaus::get_ram()
  if(dipsaus::get_os() %in% c("windows", "linux", "darwin")){
    expect(length(ram) == 1 && ram > 0, failure_message = 'RAM is not a positive number')
  } else {
    expect(length(ram) == 1 && is.na(ram), failure_message = 'RAM is not NA')
  }

})
