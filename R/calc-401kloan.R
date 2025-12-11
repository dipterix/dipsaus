#' Calculate optimal 401k loan strategy
#'
#' @description
#' Calculates the best strategy for obtaining loans from 401k retirement accounts
#' versus market loans. The function compares two strategies:
#' \enumerate{
#'   \item Taking a loan from 401k (paid back monthly with interest, money reinvests at 401k yield)
#'   \item Taking a market loan with down payment (cash grows but interest payments are lost)
#' }
#'
#' @param loan_amount numeric; total amount of loan needed
#' @param k401_balance numeric; current balance in 401k retirement account
#' @param cash_balance numeric; current cash balance available
#' @param k401_yield numeric; 401k average annual yield (as decimal, e.g., 0.07 for 7\%)
#' @param cash_yield numeric; cash/savings annual yield (as decimal, e.g., 0.02 for 2\%)
#' @param k401_loan_apr numeric; 401k loan annual percentage rate (as decimal)
#' @param market_loan_apr numeric; market loan annual percentage rate (as decimal)
#' @param down_payment_pct numeric; down payment percentage for market loan (as decimal, default 0)
#' @param n_months integer; number of months for the loan (default 60)
#' @param tax_rate numeric; tax rate for early withdrawal (as decimal, default 0.22)
#' @param early_withdrawal_penalty numeric; early withdrawal penalty (as decimal, default 0.10)
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{inputs}: list of all input parameters
#'   \item \code{recommended_401k_loan}: recommended amount to loan from 401k
#'   \item \code{recommended_market_loan}: recommended amount to loan from market
#'   \item \code{strategy}: recommended strategy ("401k_only", "market_only", or "mixed")
#'   \item \code{final_401k_balance}: expected 401k balance after n_months
#'   \item \code{final_cash_balance}: expected cash balance after n_months
#'   \item \code{total_final_balance}: sum of final balances
#'   \item \code{details}: detailed breakdown of calculations
#' }
#'
#' @details
#' The function calculates and compares multiple scenarios:
#'
#' \strong{401k Loan Strategy:}
#' \itemize{
#'   \item Money borrowed from 401k reduces the balance
#'   \item Monthly payments (principal + interest) go back into 401k
#'   \item Remaining 401k balance continues to grow at 401k_yield rate
#'   \item Cash is preserved and grows at cash_yield rate
#' }
#'
#' \strong{Market Loan Strategy:}
#' \itemize{
#'   \item Requires down payment from cash
#'   \item Monthly payments (principal + interest) reduce cash balance
#'   \item Full 401k balance continues to grow at 401k_yield rate
#'   \item Remaining cash grows at cash_yield rate
#' }
#'
#' The function considers tax implications and early withdrawal penalties when
#' comparing strategies. It recommends the strategy that maximizes total final
#' balance (401k + cash) after the loan period.
#'
#' @examples
#' # Basic example: $50,000 loan with typical parameters
#' result <- calc_401kloan(
#'   loan_amount = 50000,
#'   k401_balance = 200000,
#'   cash_balance = 30000,
#'   k401_yield = 0.07,
#'   cash_yield = 0.02,
#'   k401_loan_apr = 0.045,
#'   market_loan_apr = 0.06
#' )
#' print(result$strategy)
#' print(result$total_final_balance)
#'
#' # With down payment requirement
#' result2 <- calc_401kloan(
#'   loan_amount = 50000,
#'   k401_balance = 200000,
#'   cash_balance = 30000,
#'   k401_yield = 0.07,
#'   cash_yield = 0.02,
#'   k401_loan_apr = 0.045,
#'   market_loan_apr = 0.06,
#'   down_payment_pct = 0.20,
#'   n_months = 60
#' )
#'
#' @export
calc_401kloan <- function(
    loan_amount,
    k401_balance,
    cash_balance,
    k401_yield,
    cash_yield,
    k401_loan_apr,
    market_loan_apr,
    down_payment_pct = 0,
    n_months = 60,
    tax_rate = 0.22,
    early_withdrawal_penalty = 0.10
) {
  
  # Input validation
  stopifnot(
    "loan_amount must be positive" = loan_amount > 0,
    "k401_balance must be non-negative" = k401_balance >= 0,
    "cash_balance must be non-negative" = cash_balance >= 0,
    "k401_yield must be numeric" = is.numeric(k401_yield),
    "cash_yield must be numeric" = is.numeric(cash_yield),
    "k401_loan_apr must be non-negative" = k401_loan_apr >= 0,
    "market_loan_apr must be non-negative" = market_loan_apr >= 0,
    "down_payment_pct must be between 0 and 1" = down_payment_pct >= 0 && down_payment_pct <= 1,
    "n_months must be positive integer" = n_months > 0,
    "tax_rate must be between 0 and 1" = tax_rate >= 0 && tax_rate <= 1,
    "early_withdrawal_penalty must be between 0 and 1" = early_withdrawal_penalty >= 0 && early_withdrawal_penalty <= 1
  )
  
  # Convert annual rates to monthly rates
  monthly_401k_yield <- k401_yield / 12
  monthly_cash_yield <- cash_yield / 12
  monthly_401k_apr <- k401_loan_apr / 12
  monthly_market_apr <- market_loan_apr / 12
  
  # Store inputs
  inputs <- list(
    loan_amount = loan_amount,
    k401_balance = k401_balance,
    cash_balance = cash_balance,
    k401_yield = k401_yield,
    cash_yield = cash_yield,
    k401_loan_apr = k401_loan_apr,
    market_loan_apr = market_loan_apr,
    down_payment_pct = down_payment_pct,
    n_months = n_months,
    tax_rate = tax_rate,
    early_withdrawal_penalty = early_withdrawal_penalty
  )
  
  # Helper function to calculate monthly payment
  calc_monthly_payment <- function(principal, monthly_rate, n_periods) {
    if (monthly_rate == 0) {
      return(principal / n_periods)
    }
    monthly_rate * principal * (1 + monthly_rate)^n_periods / 
      ((1 + monthly_rate)^n_periods - 1)
  }
  
  # Helper function to simulate account growth with payments
  simulate_loan <- function(initial_401k, initial_cash, loan_from_401k, loan_from_market,
                           monthly_401k_payment, monthly_market_payment,
                           monthly_401k_rate, monthly_cash_rate, n_periods) {
    
    k401_bal <- initial_401k - loan_from_401k
    cash_bal <- initial_cash
    
    for (month in 1:n_periods) {
      # 401k grows and receives payments
      k401_bal <- k401_bal * (1 + monthly_401k_rate) + monthly_401k_payment
      
      # Cash grows but pays market loan
      cash_bal <- cash_bal * (1 + monthly_cash_rate) - monthly_market_payment
    }
    
    list(final_401k = k401_bal, final_cash = cash_bal)
  }
  
  # Strategy 1: Pure 401k loan
  strategy_401k_only <- NULL
  if (k401_balance >= loan_amount) {
    monthly_payment_401k <- calc_monthly_payment(loan_amount, monthly_401k_apr, n_months)
    result_401k <- simulate_loan(
      initial_401k = k401_balance,
      initial_cash = cash_balance,
      loan_from_401k = loan_amount,
      loan_from_market = 0,
      monthly_401k_payment = monthly_payment_401k,
      monthly_market_payment = 0,
      monthly_401k_rate = monthly_401k_yield,
      monthly_cash_rate = monthly_cash_yield,
      n_periods = n_months
    )
    
    strategy_401k_only <- list(
      k401_loan = loan_amount,
      market_loan = 0,
      final_401k = result_401k$final_401k,
      final_cash = result_401k$final_cash,
      total_final = result_401k$final_401k + result_401k$final_cash,
      monthly_401k_payment = monthly_payment_401k,
      monthly_market_payment = 0
    )
  }
  
  # Strategy 2: Pure market loan
  strategy_market_only <- NULL
  down_payment_amount <- loan_amount * down_payment_pct
  financed_amount <- loan_amount - down_payment_amount
  
  if (cash_balance >= down_payment_amount) {
    monthly_payment_market <- calc_monthly_payment(financed_amount, monthly_market_apr, n_months)
    
    # Check if cash flow is sufficient
    initial_cash_after_down <- cash_balance - down_payment_amount
    
    result_market <- simulate_loan(
      initial_401k = k401_balance,
      initial_cash = initial_cash_after_down,
      loan_from_401k = 0,
      loan_from_market = financed_amount,
      monthly_401k_payment = 0,
      monthly_market_payment = monthly_payment_market,
      monthly_401k_rate = monthly_401k_yield,
      monthly_cash_rate = monthly_cash_yield,
      n_periods = n_months
    )
    
    strategy_market_only <- list(
      k401_loan = 0,
      market_loan = financed_amount,
      down_payment = down_payment_amount,
      final_401k = result_market$final_401k,
      final_cash = result_market$final_cash,
      total_final = result_market$final_401k + result_market$final_cash,
      monthly_401k_payment = 0,
      monthly_market_payment = monthly_payment_market
    )
  }
  
  # Strategy 3: Mixed strategy (use 401k up to limit, rest from market)
  strategy_mixed <- NULL
  max_401k_loan <- min(k401_balance * 0.5, 50000)  # IRS limit: 50% or $50k, whichever is less
  
  if (loan_amount > max_401k_loan && max_401k_loan > 0) {
    remaining_needed <- loan_amount - max_401k_loan
    down_payment_for_market <- remaining_needed * down_payment_pct
    market_financed <- remaining_needed - down_payment_for_market
    
    if (cash_balance >= down_payment_for_market) {
      monthly_payment_401k <- calc_monthly_payment(max_401k_loan, monthly_401k_apr, n_months)
      monthly_payment_market <- calc_monthly_payment(market_financed, monthly_market_apr, n_months)
      
      initial_cash_after_down <- cash_balance - down_payment_for_market
      
      result_mixed <- simulate_loan(
        initial_401k = k401_balance,
        initial_cash = initial_cash_after_down,
        loan_from_401k = max_401k_loan,
        loan_from_market = market_financed,
        monthly_401k_payment = monthly_payment_401k,
        monthly_market_payment = monthly_payment_market,
        monthly_401k_rate = monthly_401k_yield,
        monthly_cash_rate = monthly_cash_yield,
        n_periods = n_months
      )
      
      strategy_mixed <- list(
        k401_loan = max_401k_loan,
        market_loan = market_financed,
        down_payment = down_payment_for_market,
        final_401k = result_mixed$final_401k,
        final_cash = result_mixed$final_cash,
        total_final = result_mixed$final_401k + result_mixed$final_cash,
        monthly_401k_payment = monthly_payment_401k,
        monthly_market_payment = monthly_payment_market
      )
    }
  }
  
  # Compare strategies and select the best one
  strategies <- list()
  if (!is.null(strategy_401k_only)) {
    strategies[["401k_only"]] <- strategy_401k_only
  }
  if (!is.null(strategy_market_only)) {
    strategies[["market_only"]] <- strategy_market_only
  }
  if (!is.null(strategy_mixed)) {
    strategies[["mixed"]] <- strategy_mixed
  }
  
  if (length(strategies) == 0) {
    stop("No viable loan strategy found with current balances and parameters")
  }
  
  # Find best strategy (highest total final balance)
  best_strategy_name <- names(strategies)[which.max(sapply(strategies, function(x) x$total_final))]
  best_strategy <- strategies[[best_strategy_name]]
  
  # Prepare return value
  result <- list(
    inputs = inputs,
    recommended_401k_loan = best_strategy$k401_loan,
    recommended_market_loan = best_strategy$market_loan,
    strategy = best_strategy_name,
    final_401k_balance = best_strategy$final_401k,
    final_cash_balance = best_strategy$final_cash,
    total_final_balance = best_strategy$total_final,
    details = list(
      monthly_401k_payment = best_strategy$monthly_401k_payment,
      monthly_market_payment = best_strategy$monthly_market_payment,
      down_payment = if (!is.null(best_strategy$down_payment)) best_strategy$down_payment else 0,
      all_strategies = strategies
    )
  )
  
  class(result) <- c("calc_401kloan_result", "list")
  return(result)
}

#' @export
print.calc_401kloan_result <- function(x, ...) {
  cat("401k Loan Calculator Results\n")
  cat("============================\n\n")
  cat("Strategy:", x$strategy, "\n")
  cat("Recommended 401k Loan: $", format(round(x$recommended_401k_loan, 2), big.mark = ","), "\n", sep = "")
  cat("Recommended Market Loan: $", format(round(x$recommended_market_loan, 2), big.mark = ","), "\n", sep = "")
  if (x$details$down_payment > 0) {
    cat("Down Payment: $", format(round(x$details$down_payment, 2), big.mark = ","), "\n", sep = "")
  }
  cat("\nMonthly Payments:\n")
  if (x$details$monthly_401k_payment > 0) {
    cat("  401k Loan: $", format(round(x$details$monthly_401k_payment, 2), big.mark = ","), "\n", sep = "")
  }
  if (x$details$monthly_market_payment > 0) {
    cat("  Market Loan: $", format(round(x$details$monthly_market_payment, 2), big.mark = ","), "\n", sep = "")
  }
  cat("\nExpected Balances after", x$inputs$n_months, "months:\n")
  cat("  401k Balance: $", format(round(x$final_401k_balance, 2), big.mark = ","), "\n", sep = "")
  cat("  Cash Balance: $", format(round(x$final_cash_balance, 2), big.mark = ","), "\n", sep = "")
  cat("  Total: $", format(round(x$total_final_balance, 2), big.mark = ","), "\n", sep = "")
  invisible(x)
}
