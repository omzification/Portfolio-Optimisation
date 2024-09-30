# Load required libraries
library(dplyr)
library(PortfolioAnalytics)

# Read data from Excel file
data <- readxl::read_excel("data.xlsx")

# Assuming a constant risk-free rate of 0.04% per month
risk_free_rate <- 0.0004

# Convert Date to a proper date format
data$Date <- as.Date(data$Date, format = "%d-%m-%Y")

# Convert data to a time series
data <- xts(data[, -1], order.by = data$Date)

# Calculate log returns & Remove NAs
returns <- Return.calculate(data, method) %>% 
  na.omit(returns)

# Calculate Correlation Matrix
cor_matrix <- cor(returns)
cov_matrix <- cov(returns)

# Find the pair of stocks with the lowest correlation
min_cor_pair <- which(cor_matrix == min(cor_matrix[cor_matrix > 0]), arr.ind = TRUE)
stock1 <- rownames(cor_matrix)[min_cor_pair[1, 1]]
stock2 <- colnames(cor_matrix)[min_cor_pair[1, 2]]

# Print the selected stocks
cat("Selected Stocks:", stock1, "and", stock2, "\n")

# Create a portfolio with selected stocks
portfolio <- portfolio.spec(assets = c(stock1, stock2))

# Add constraints
portfolio <- add.constraint(portfolio, type = "weight_sum", min_sum = 1, max_sum = 1)
portfolio <- add.constraint(portfolio, type = "box", min = 0, max = 1)

# Specify the objective function (minimize risk)
portfolio <- add.objective(portfolio, type = "risk", name = "StdDev")

# Optimize the portfolio
optimized_portfolio <- optimize.portfolio(returns, portfolio)

# Display results
cat("Global Minimum-Variance Portfolio:\n")
print(optimized_portfolio)


# Calculate Expected Returns
expected_returns <- colMeans(returns[, c(stock1, stock2)])

# Calculate the expected return of the portfolio
portfolio_expected_return <- sum(optimized_portfolio$weights * expected_returns)

cat("\nExpected Return of the Portfolio:", portfolio_expected_return, "\n")

# Define a range of weights for the first asset
weights_range <- seq(0, 1, by = 0.1)

# Initialize vectors to store results
portfolio_returns <- numeric()
portfolio_volatility <- numeric()

# Simulate portfolios with different weights
for (w1 in weights_range) {
  w2 <- 1 - w1  # Weight of the second asset
  
  # Calculate portfolio returns and volatility
  portfolio_return <- w1 * expected_returns[1] + w2 * expected_returns[2]
  portfolio_volatility_value <- sqrt(w1^2 * var(returns[, stock1]) + w2^2 * var(returns[, stock2]) + 2 * w1 * w2 * cov(returns[, stock1], returns[, stock2]))
  
  # Append results to vectors
  portfolio_returns <- c(portfolio_returns, portfolio_return)
  portfolio_volatility <- c(portfolio_volatility, portfolio_volatility_value)
}

# Calculate the excess return and Sharpe ratio for each portfolio
excess_returns <- portfolio_returns - risk_free_rate
sharpe_ratios <- excess_returns / portfolio_volatility

# Find the portfolio with the maximum Sharpe ratio (Tangency Portfolio)
tangency_portfolio_index <- which.max(sharpe_ratios)

# Plot the Investment Opportunity Set
plot(portfolio_volatility, portfolio_returns, type = "l", col = "blue", lwd = 2, xlab = "Portfolio Standard Deviation", ylab = "Portfolio Expected Return", main = "Investment Opportunity Set")

# Add points for individual portfolios
points(sqrt(var(returns[, stock1])), expected_returns[1], col = "red", pch = 16, cex = 1.5)  # Point for SBRY
points(sqrt(var(returns[, stock2])), expected_returns[2], col = "green", pch = 16, cex = 1.5)  # Point for SHEL
points(portfolio_volatility, portfolio_returns, col = "blue", pch = 16, cex = 0.8)  # Other portfolio points

# Add the Tangency Portfolio P
points(portfolio_volatility[tangency_portfolio_index], portfolio_returns[tangency_portfolio_index], col = "orange", pch = 16, cex = 1.5)

# Draw the Capital Allocation Line (CAL)
abline(a = risk_free_rate, b = sharpe_ratios[tangency_portfolio_index], col = "purple", lty = 2)

# Label the Tangency Portfolio
text(portfolio_volatility[tangency_portfolio_index] + 0.002, portfolio_returns[tangency_portfolio_index], "P", col = "orange", pos = 3)

# Calculate the expected return, standard deviation, and Sharpe Ratio at the Tangency Portfolio
expected_return_tangency <- portfolio_returns[8]
standard_deviation_tangency <- portfolio_volatility[8]
sharpe_ratio_tangency <- sharpe_ratios[8]

# Display the results
cat("Expected Return at Tangency Portfolio (P):", expected_return_tangency, "\n")
cat("Standard Deviation at Tangency Portfolio (P):", standard_deviation_tangency, "\n")
cat("Sharpe Ratio at Tangency Portfolio (P):", sharpe_ratio_tangency, "\n")

# Assuming risk aversion parameter A = 3
A <- 3

# Calculate the weight of the risky asset in the complete portfolio (C)
weight_risky_asset_C <- (expected_return_tangency - risk_free_rate) / (A * standard_deviation_tangency^2)

# Calculate the weight of the risk-free asset in the complete portfolio (C)
weight_risk_free_C <- 1 - weight_risky_asset_C

# Display the results
cat("Weight of the Risky Asset in the Complete Portfolio (C):", weight_risky_asset_C, "\n")
cat("Weight of the Risk-Free Asset in the Complete Portfolio (C):", weight_risk_free_C, "\n")

# Calculate the expected return of the complete portfolio (C)
expected_return_complete_portfolio <- weight_risky_asset_C * expected_return_tangency + weight_risk_free_C * risk_free_rate

# Expected returns of investing all wealth in SBRY or SHEL
expected_return_SBRY <- expected_returns[1]
expected_return_SHEL <- expected_returns[2]

# Display the results
cat("Expected Return of the Complete Portfolio (C):", expected_return_complete_portfolio, "\n")
cat("Expected Return of Investing All Wealth in SBRY:", expected_return_SBRY, "\n")
cat("Expected Return of Investing All Wealth in SHEL:", expected_return_SHEL, "\n")

# Calculate the expected return of the complete portfolio (C)
expected_return_complete_portfolio <- weight_risky_asset_C * expected_return_tangency + weight_risk_free_C * risk_free_rate

# Calculate the standard deviation of the complete portfolio (C)
standard_deviation_complete_portfolio <- weight_risky_asset_C * standard_deviation_tangency

# Display the results
cat("Expected Return of the Complete Portfolio (C):", expected_return_complete_portfolio, "\n")
cat("Standard Deviation of the Complete Portfolio (C):", standard_deviation_complete_portfolio, "\n")

# Assuming you have FTSE 100 data in a variable named 'returns_FTSE'
returns_FTSE <- returns$FTSE100

# Calculate returns and standard deviation of FTSE 100
expected_return_FTSE <- mean(returns_FTSE)
standard_deviation_FTSE <- sd(returns_FTSE)

# Display results
cat("Expected Return of FTSE 100:", expected_return_FTSE, "\n")
cat("Standard Deviation of FTSE 100:", standard_deviation_FTSE, "\n")


