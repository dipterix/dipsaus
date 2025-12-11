test_that("calc_401kloan basic functionality", {
  
  # Basic test with 401k loan being optimal
  result <- calc_401kloan(
    loan_amount = 30000,
    k401_balance = 100000,
    cash_balance = 20000,
    k401_yield = 0.07,
    cash_yield = 0.02,
    k401_loan_apr = 0.04,
    market_loan_apr = 0.08,
    n_months = 60
  )
  
  expect_true(inherits(result, "calc_401kloan_result"))
  expect_true(is.list(result))
  expect_true("inputs" %in% names(result))
  expect_true("recommended_401k_loan" %in% names(result))
  expect_true("recommended_market_loan" %in% names(result))
  expect_true("strategy" %in% names(result))
  expect_true("final_401k_balance" %in% names(result))
  expect_true("final_cash_balance" %in% names(result))
  expect_true("total_final_balance" %in% names(result))
  
  # Check that strategy is one of the expected values
  expect_true(result$strategy %in% c("401k_only", "market_only", "mixed"))
  
  # Check that recommended loans sum to loan amount (approximately)
  expect_equal(
    result$recommended_401k_loan + result$recommended_market_loan,
    result$inputs$loan_amount,
    tolerance = 0.01
  )
  
  # Check that final balances are positive
  expect_true(result$final_401k_balance > 0)
  expect_true(result$final_cash_balance >= 0)  # Can be 0 if all cash used
  
})

test_that("calc_401kloan with down payment", {
  
  result <- calc_401kloan(
    loan_amount = 50000,
    k401_balance = 200000,
    cash_balance = 30000,
    k401_yield = 0.07,
    cash_yield = 0.02,
    k401_loan_apr = 0.045,
    market_loan_apr = 0.06,
    down_payment_pct = 0.20,
    n_months = 60
  )
  
  expect_true(inherits(result, "calc_401kloan_result"))
  expect_true(is.numeric(result$recommended_401k_loan))
  expect_true(is.numeric(result$recommended_market_loan))
  expect_true(is.numeric(result$total_final_balance))
  
  # If market loan is used, down payment should be reflected
  if (result$recommended_market_loan > 0) {
    expect_true(result$details$down_payment >= 0)
  }
  
})

test_that("calc_401kloan input validation", {
  
  # Negative loan amount
  expect_error(
    calc_401kloan(
      loan_amount = -1000,
      k401_balance = 100000,
      cash_balance = 20000,
      k401_yield = 0.07,
      cash_yield = 0.02,
      k401_loan_apr = 0.04,
      market_loan_apr = 0.08
    ),
    "loan_amount must be positive"
  )
  
  # Negative 401k balance
  expect_error(
    calc_401kloan(
      loan_amount = 30000,
      k401_balance = -100000,
      cash_balance = 20000,
      k401_yield = 0.07,
      cash_yield = 0.02,
      k401_loan_apr = 0.04,
      market_loan_apr = 0.08
    ),
    "k401_balance must be non-negative"
  )
  
  # Invalid down payment percentage
  expect_error(
    calc_401kloan(
      loan_amount = 30000,
      k401_balance = 100000,
      cash_balance = 20000,
      k401_yield = 0.07,
      cash_yield = 0.02,
      k401_loan_apr = 0.04,
      market_loan_apr = 0.08,
      down_payment_pct = 1.5
    ),
    "down_payment_pct must be between 0 and 1"
  )
  
})

test_that("calc_401kloan with zero interest rates", {
  
  # Test with zero 401k loan APR
  result <- calc_401kloan(
    loan_amount = 20000,
    k401_balance = 100000,
    cash_balance = 30000,
    k401_yield = 0.05,
    cash_yield = 0.01,
    k401_loan_apr = 0,
    market_loan_apr = 0.05,
    n_months = 60
  )
  
  expect_true(inherits(result, "calc_401kloan_result"))
  expect_true(is.numeric(result$total_final_balance))
  
})

test_that("calc_401kloan with different loan periods", {
  
  # 12 months
  result_12 <- calc_401kloan(
    loan_amount = 10000,
    k401_balance = 50000,
    cash_balance = 15000,
    k401_yield = 0.07,
    cash_yield = 0.02,
    k401_loan_apr = 0.04,
    market_loan_apr = 0.08,
    n_months = 12
  )
  
  expect_true(inherits(result_12, "calc_401kloan_result"))
  expect_equal(result_12$inputs$n_months, 12)
  
  # 120 months
  result_120 <- calc_401kloan(
    loan_amount = 10000,
    k401_balance = 50000,
    cash_balance = 15000,
    k401_yield = 0.07,
    cash_yield = 0.02,
    k401_loan_apr = 0.04,
    market_loan_apr = 0.08,
    n_months = 120
  )
  
  expect_true(inherits(result_120, "calc_401kloan_result"))
  expect_equal(result_120$inputs$n_months, 120)
  
  # Longer period should generally result in higher final balances
  # (due to compound growth, though depends on rates)
  
})

test_that("calc_401kloan print method", {
  
  result <- calc_401kloan(
    loan_amount = 30000,
    k401_balance = 100000,
    cash_balance = 20000,
    k401_yield = 0.07,
    cash_yield = 0.02,
    k401_loan_apr = 0.04,
    market_loan_apr = 0.08,
    n_months = 60
  )
  
  # Test that print method works without error
  expect_output(print(result), "401k Loan Calculator Results")
  expect_output(print(result), "Strategy:")
  expect_output(print(result), "Recommended 401k Loan:")
  expect_output(print(result), "Expected Balances")
  
})

test_that("calc_401kloan with insufficient balances", {
  
  # Test when neither 401k nor cash has enough
  expect_error(
    calc_401kloan(
      loan_amount = 100000,
      k401_balance = 30000,
      cash_balance = 1000,
      k401_yield = 0.07,
      cash_yield = 0.02,
      k401_loan_apr = 0.04,
      market_loan_apr = 0.08,
      down_payment_pct = 0.20,
      n_months = 60
    ),
    "No viable loan strategy"
  )
  
})

test_that("calc_401kloan returns all expected details", {
  
  result <- calc_401kloan(
    loan_amount = 25000,
    k401_balance = 80000,
    cash_balance = 15000,
    k401_yield = 0.06,
    cash_yield = 0.015,
    k401_loan_apr = 0.045,
    market_loan_apr = 0.07,
    n_months = 48
  )
  
  expect_true(is.list(result$details))
  expect_true("monthly_401k_payment" %in% names(result$details))
  expect_true("monthly_market_payment" %in% names(result$details))
  expect_true("all_strategies" %in% names(result$details))
  
  # Check that all_strategies contains at least one strategy
  expect_true(length(result$details$all_strategies) >= 1)
  
})

test_that("calc_401kloan with custom 401k loan limit", {
  
  result <- calc_401kloan(
    loan_amount = 20000,
    k401_balance = 100000,
    cash_balance = 25000,
    k401_yield = 0.08,
    cash_yield = 0.025,
    k401_loan_apr = 0.05,
    market_loan_apr = 0.09,
    max_401k_loan_limit = 40000,
    n_months = 60
  )
  
  expect_equal(result$inputs$max_401k_loan_limit, 40000)
  expect_true(inherits(result, "calc_401kloan_result"))
  
})
