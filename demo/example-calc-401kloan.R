# Demo: calc_401kloan function
# This demonstrates the use of the calc_401kloan function for comparing
# 401k loan vs. market loan strategies

library(dipsaus)

cat("\n==================================================\n")
cat("  401k Loan Calculator Demo\n")
cat("==================================================\n\n")

# Scenario 1: Should I take a 401k loan or a car loan?
cat("Scenario 1: Car Purchase - $30,000\n")
cat("--------------------------------------------------\n")
cat("You need $30,000 for a car. You have:\n")
cat("  - $100,000 in your 401k (growing at 7%/year)\n")
cat("  - $20,000 in savings (growing at 2%/year)\n")
cat("  - 401k loan available at 4% APR\n")
cat("  - Car loan available at 8% APR\n")
cat("  - 60-month term\n\n")

result1 <- calc_401kloan(
  loan_amount = 30000,
  k401_balance = 100000,
  cash_balance = 20000,
  k401_yield = 0.07,
  cash_yield = 0.02,
  k401_loan_apr = 0.04,
  market_loan_apr = 0.08,
  n_months = 60
)

print(result1)
cat("\n")

# Scenario 2: Home down payment with existing savings
cat("\nScenario 2: Home Down Payment - $50,000\n")
cat("--------------------------------------------------\n")
cat("You need $50,000 for a home down payment. You have:\n")
cat("  - $200,000 in your 401k (growing at 7%/year)\n")
cat("  - $30,000 in savings (growing at 2%/year)\n")
cat("  - 401k loan available at 4.5% APR\n")
cat("  - Personal loan available at 6% APR (20% down required)\n")
cat("  - 60-month term\n\n")

result2 <- calc_401kloan(
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

print(result2)
cat("\n")

# Scenario 3: Quick comparison
cat("\nScenario 3: Short-term Need - $10,000\n")
cat("--------------------------------------------------\n")
cat("You need $10,000 for a short-term expense:\n")
cat("  - $50,000 in your 401k\n")
cat("  - $15,000 in savings\n")
cat("  - 12-month term\n\n")

result3 <- calc_401kloan(
  loan_amount = 10000,
  k401_balance = 50000,
  cash_balance = 15000,
  k401_yield = 0.07,
  cash_yield = 0.02,
  k401_loan_apr = 0.04,
  market_loan_apr = 0.08,
  n_months = 12
)

print(result3)

cat("\n==================================================\n")
cat("Key Insights:\n")
cat("- The calculator considers compound growth in both accounts\n")
cat("- 401k loans pay yourself back the interest\n")
cat("- Market loans preserve 401k growth potential\n")
cat("- The optimal strategy depends on rate differences\n")
cat("==================================================\n")
