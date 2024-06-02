# Load necessary libraries
library(ggplot2)

# Generate sample data
model_names <- c("Unigram", "Bigram", "Trigram", "RNN")
accuracy <- c(0.60, 0.70, 0.75, 0.85)
perplexity <- c(250, 150, 120, 100)

# Create accuracy plot
accuracy_df <- data.frame(Model = model_names, Accuracy = accuracy)
accuracy_plot <- ggplot(accuracy_df, aes(x = Model, y = Accuracy)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  ggtitle("Model Accuracy Comparison")

# Save accuracy plot
ggsave("accuracy_plot.png", accuracy_plot)

# Create perplexity plot
perplexity_df <- data.frame(Model = model_names, Perplexity = perplexity)
perplexity_plot <- ggplot(perplexity_df, aes(x = Model, y = Perplexity)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  theme_minimal() +
  ggtitle("Model Perplexity Comparison")

# Save perplexity plot
ggsave("perplexity_plot.png", perplexity_plot)
