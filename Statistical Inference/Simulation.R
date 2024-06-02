# Set the parameters
lambda <- 0.2
n <- 40
num_simulations <- 1000

# Simulate the exponential distribution
set.seed(123)  # Setting seed for reproducibility
simulations <- replicate(num_simulations, rexp(n, lambda))

# Calculate the sample means
sample_means <- colMeans(simulations)

# Theoretical mean and variance
theoretical_mean <- 1 / lambda
theoretical_variance <- (1 / lambda)^2 / n

# Sample mean and variance
sample_mean <- mean(sample_means)
sample_variance <- var(sample_means)

# Display the results
cat("Sample Mean:", sample_mean, "\n")
cat("Theoretical Mean:", theoretical_mean, "\n")
cat("Sample Variance:", sample_variance, "\n")
cat("Theoretical Variance:", theoretical_variance, "\n")

# Plot the distribution of the sample means
hist(sample_means, probability = TRUE, 
     main = "Distribution of Sample Means of 40 Exponentials",
     xlab = "Sample Mean", 
     col = "lightblue", 
     border = "black")
lines(density(sample_means), col = "red", lwd = 2)

# Overlay the theoretical normal distribution
x <- seq(min(sample_means), max(sample_means), length.out = 100)
y <- dnorm(x, mean = theoretical_mean, sd = sqrt(theoretical_variance))
lines(x, y, col = "blue", lwd = 2, lty = 2)

# QQ-plot to show normality
qqnorm(sample_means, main = "QQ-Plot of Sample Means")
qqline(sample_means, col = "red")

# Explanatory text
cat("\nThe sample mean is very close to the theoretical mean, illustrating the Law of Large Numbers.")
cat("\nThe sample variance is very close to the theoretical variance, as expected.")
cat("\nThe histogram of the sample means and the QQ-plot show that the distribution of the sample means is approximately normal, demonstrating the Central Limit Theorem.")
