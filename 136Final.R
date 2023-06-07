#Current Market Conditions
# Set seed for reproducibility
set.seed(123)

# Number of sellers and buyers
num_sellers <- 500
num_buyers <- 4000

# Generate values for buyers' v (value for item)
buyer_values <- rnorm(num_buyers, mean = 200, sd = 100)

# Initialize empty vectors for prices, total shoes sold, and buyer payoffs
prices <- c()
total_shoes_sold <- 0
price_sd <- 50  # Standard deviation of prices per sale
num_sales <- 0
buyer_payoffs <- c()

# Simulate market
for (seller in 1:num_sellers) {
  # Generate seller's price
  price <- rnorm(1, mean = 300 + total_shoes_sold, sd = price_sd)
  
  # Find eligible buyers who are willing to purchase the shoe at the seller's price
  eligible_buyers <- buyer_values >= price
  
  # Check if there are any eligible buyers
  if (any(eligible_buyers)) {
    # Get the eligible buyers and their corresponding values
    buyer_indices <- which(eligible_buyers)
    buyer_values_eligible <- buyer_values[buyer_indices]
    
    # Find the buyer with the highest value among the eligible buyers
    buyer_index <- buyer_indices[which.max(buyer_values_eligible)]
    buyer_value <- buyer_values[buyer_index]
    
    # Add price to vector
    prices <- c(prices, price)
    
    # Print seller's price, buyer's value, and the buyer who purchased the shoe
    cat("Seller", seller, "Price:", price, "Buyer Value:", buyer_value, "Buyer:", buyer_index, "\n")
    
    # Update total shoes sold
    total_shoes_sold <- total_shoes_sold + 1
    
    # Update number of sales completed
    num_sales <- num_sales + 1
    
    # Calculate buyer payoff
    buyer_payoff <- buyer_value - price
    buyer_payoffs <- c(buyer_payoffs, buyer_payoff)
    
    # Remove the buyer from the eligible buyers pool (since they have bought a sneaker)
    buyer_values <- buyer_values[-buyer_index]
  }
}

# Calculate average shoe price for sneakers that actually sell
avg_price <- mean(prices)

# Print average shoe price
cat("Average Shoe Price (Sneakers that Sold):", avg_price, "\n")

# Print the number of sales completed
cat("Number of Sales Completed:", num_sales, "\n")

# Calculate average buyer payoff
avg_buyer_payoff <- mean(buyer_payoffs)

# Print average buyer payoff
cat("Average Buyer Payoff:", avg_buyer_payoff, "\n")

# Create a graph of prices per sale
plot(prices, type = "l", xlab = "Sale Number", ylab = "Price", main = "Prices per Sale")

# Create a histogram of the number of sales for each price
hist(prices, breaks = 30, xlim = c(100, 600), xlab = "Price", ylab = "Number of Sales", main = "Number of Sales by Price")

# Create a graph of buyer payoffs
plot(buyer_payoffs, type = "l", xlab = "Sale Number", ylab = "Buyer Profit", main = "Buyer Profit")

#Hypothetical Lottery Based Market
# Set seed for reproducibility
set.seed(123)

# Number of sellers and buyers
num_sellers <- 500
num_buyers <- 4000

# Number of winners in each lottery
num_winners <- 500

# Initialize vectors for prices, buyer profits, and seller revenues
ticket_prices <- seq(0.10, 1.00, by = 0.10)
buyer_profits <- c()
seller_revenues <- c()

# Simulate lotteries for different ticket prices
for (price in ticket_prices) {
  # Generate buyer values and risk coefficients
  buyer_values <- rnorm(num_buyers, mean = 200, sd = 100)
  buyer_risk_coeffs <- rnorm(num_buyers, mean = 0.5, sd = 0.25)
  buyer_risk_coeffs <- pmax(pmin(buyer_risk_coeffs, 1), 0)
  
  # Initialize total profit and revenue for the current price
  total_profit <- 0
  total_revenue <- 0
  
  # Simulate lottery
  for (buyer in 1:num_buyers) {
    # Calculate buyer utility
    buyer_utility <- buyer_values[buyer] * log(1 + buyer_risk_coeffs[buyer])
    
    # Calculate the number of tickets to buy for the buyer
    num_tickets_to_buy <- floor(buyer_utility)
    
    # Calculate buyer profit
    if (buyer <= num_winners) {
      buyer_profit <- buyer_values[buyer] - (num_tickets_to_buy * price)
    } else {
      buyer_profit <- 0 - (num_tickets_to_buy * price)
    }
    
    # Accumulate the total profit and revenue for the current price
    total_profit <- total_profit + buyer_profit
    total_revenue <- total_revenue + (num_tickets_to_buy * price)
  }
  
  # Calculate average buyer profit for the current price
  average_profit <- total_profit / num_buyers
  
  # Store the average buyer profit and average seller revenue in the vectors
  buyer_profits <- c(buyer_profits, average_profit)
  seller_revenues <- c(seller_revenues, total_revenue / num_sellers)
}

# Create a data frame for ticket prices, average buyer profits, and average seller revenues
data <- data.frame(TicketPrice = ticket_prices, AverageProfit = buyer_profits, AverageRevenue = seller_revenues)

# Print the table with average seller revenue by ticket price
cat("Average Seller Revenue by Ticket Price:\n")
print(data)

# Find the ticket price corresponding to seller revenue of 343.98
target_revenue <- 343.98
target_price <- data$TicketPrice[which.min(abs(data$AverageRevenue - target_revenue))]

# Print the ticket price that yields the target seller revenue
cat("\nTicket price for Seller Revenue =", target_revenue, ": $", target_price, "\n")

# Find the buyer profit at ticket price 0.5
target_price_buyer_profit <- 0.5
buyer_profit <- data$AverageProfit[which.min(abs(data$TicketPrice - target_price_buyer_profit))]

# Print the buyer profit at ticket price 0.5
cat("\nBuyer Profit at Ticket Price =", target_price_buyer_profit, ": $", buyer_profit, "\n")

# Create a data frame for buyer profit points
buyer_profit_points <- data.frame(TicketPrice = c(target_price_buyer_profit),
                                  AverageProfit = c(buyer_profit))

# Print the table with average buyer profit by ticket price
cat("\nAverage Buyer Profit by Ticket Price:\n")
print(buyer_profit_points)

# Plot the average seller revenue
plot(data$TicketPrice, data$AverageRevenue, type = "o", pch = "", col = "red",
     xlab = "Ticket Price", ylab = "Average Seller Revenue",
     main = "Average Seller Revenue by Ticket Price")

# Add gridlines
grid()

# Add a point and label for the target seller revenue
points(target_price, target_revenue, pch = 19, col = "blue")
text(target_price, target_revenue, paste0("(", target_price, ", ", target_revenue, ")"), pos = 3)

# Add a dashed line from the target seller revenue to the corresponding price
segments(target_price, 0, target_price, target_revenue, col = "blue", lty = 2)
segments(0, target_revenue, target_price, target_revenue, col = "blue", lty = 2)

# Plot buyer's profit versus ticket price
plot(data$TicketPrice, data$AverageProfit, type = "o", pch = "", col = "green",
     xlab = "Ticket Price", ylab = "Average Buyer Profit",
     main = "Average Buyer Profit by Ticket Price")

# Add gridlines
grid()

# Add a point and label for the target buyer profit
points(target_price_buyer_profit, buyer_profit, pch = 19, col = "blue")
text(target_price_buyer_profit, buyer_profit, paste0("(", target_price_buyer_profit, ", ", buyer_profit, ")"), pos = 3)

# Add a dashed line from the target buyer profit to the corresponding price
segments(target_price_buyer_profit, 0, target_price_buyer_profit, buyer_profit, col = "blue", lty = 2)
segments(target_price_buyer_profit, buyer_profit, 0.5, buyer_profit, col = "blue", lty = 2)
segments(0.5, buyer_profit, 0, buyer_profit, col = "blue", lty = 2)

# Add a large dashed line at ticket price = 0.5
segments(0.5, min(data$AverageProfit), 0.5, max(data$AverageProfit), col = "blue", lty = 2, lwd = 2)
