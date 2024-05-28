# Load necessary libraries
library(stringr)
library(dplyr)

# Function to sample lines from a large file
sample_file <- function(file_path, sample_fraction = 0.001) {
  con <- file(file_path, "r")
  lines <- readLines(con, warn = FALSE)
  close(con)
  
  sample_size <- ceiling(length(lines) * sample_fraction)
  sampled_lines <- sample(lines, sample_size)
  return(sampled_lines)
}

# Tokenization function using stringr
tokenize <- function(text) {
  tokens <- unlist(str_extract_all(text, boundary("word")))
  return(tokens)
}

# Function to remove profane words from tokens
remove_profanity <- function(tokens, profane_words) {
  clean_tokens <- tokens[!tokens %in% profane_words]
  return(clean_tokens)
}

# Create n-gram models
create_ngram <- function(tokens, n) {
  ngrams <- unlist(lapply(seq_along(tokens), function(i) {
    if (i <= length(tokens) - n + 1) {
      paste(tokens[i:(i + n - 1)], collapse = " ")
    }
  }))
  ngrams <- ngrams[!is.na(ngrams)]  # Remove NA values
  ngram_table <- table(ngrams)
  ngram_df <- as.data.frame(ngram_table)
  colnames(ngram_df) <- c("ngram", "frequency")
  ngram_df <- ngram_df[order(-ngram_df$frequency),]
  return(ngram_df)
}

# Load and preprocess the data
file_path <- "en_US.blogs.txt"  # Adjust this path as necessary
sampled_lines <- sample_file(file_path, 0.001)  # Smaller sample size
text_sample <- paste(sampled_lines, collapse = " ")
tokens <- tokenize(tolower(text_sample))

# Profanity filtering
badwords_url <- "https://storage.googleapis.com/google-code-archive-downloads/v2/code.google.com/badwordslist/badwords.txt"
download.file(badwords_url, destfile = "bad-words.txt")
badwords <- readLines("bad-words.txt", encoding="UTF-8", warn = FALSE)

# Ensure last line ends properly
if (length(badwords) > 0 && !nzchar(tail(badwords, 1))) {
  badwords <- head(badwords, -1)
}

clean_tokens <- remove_profanity(tokens, badwords)

# Create n-gram models
unigram_df <- create_ngram(clean_tokens, 1)
bigram_df <- create_ngram(clean_tokens, 2)
trigram_df <- create_ngram(clean_tokens, 3)

# Debugging: Print some examples from the n-gram models
print("Unigram model head:")
print(head(unigram_df))
print("Bigram model head:")
print(head(bigram_df))
print("Trigram model head:")
print(head(trigram_df))

# Save the n-gram models
save(unigram_df, bigram_df, trigram_df, file = "ngram_models.RData")

