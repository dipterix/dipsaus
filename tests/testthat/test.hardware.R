context("hardwares")

test_that('testing get_ram', {
  ram <- dipsaus::get_ram()
  expect(length(ram) == 1 && !is.na(ram) && ram > 0, failure_message = 'RAM is not a positive number')
})
