context("async_*apply")

make_forked_clusters()

test_that('testing async_expr', {
  re = async_expr(1:10, { x })
  expect_equal(deparse_svec(unlist(re)), '1-10')

  re = async_expr(1:10, { async({x}); x+1 })
  expect_equal(deparse_svec(unlist(re)), '1-10')

  re = async_expr(1:10, { async({if(x > 1) x else NULL}) })
  expect_length(re, 10)
  expect_length(drop_nulls(re), 9)
})
