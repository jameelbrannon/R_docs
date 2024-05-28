library(shiny)
library(stringr)
library(dplyr)

# Load the n-gram models
load("ngram_models.RData")

# Define the prediction function with debugging information
predict_next_word <- function(input_text, unigram_df, bigram_df, trigram_df) {
  input_tokens <- unlist(str_extract_all(tolower(input_text), boundary("word")))
  print(paste("Input tokens:", paste(input_tokens, collapse = " ")))  # Debugging: Print the input tokens
  input_length <- length(input_tokens)
  
  if (input_length >= 2) {
    last_bigram <- paste(input_tokens[(input_length-1):input_length], collapse = " ")
    print(paste("Last bigram:", last_bigram))  # Debugging: Print the last bigram
    trigram_match <- trigram_df[grep(paste0("^", last_bigram, " "), trigram_df$ngram),]
    print("Trigram matches:")  # Debugging: Print the trigram matches
    print(trigram_match)
    if (nrow(trigram_match) > 0) {
      next_word <- strsplit(as.character(head(trigram_match$ngram, 1)), " ")[[1]][3]
      print(paste("Next word from trigram:", next_word))  # Debugging: Print the next word from trigram
      return(next_word)
    }
  }
  
  if (input_length >= 1) {
    last_unigram <- input_tokens[input_length]
    print(paste("Last unigram:", last_unigram))  # Debugging: Print the last unigram
    bigram_match <- bigram_df[grep(paste0("^", last_unigram, " "), bigram_df$ngram),]
    print("Bigram matches:")  # Debugging: Print the bigram matches
    print(bigram_match)
    if (nrow(bigram_match) > 0) {
      next_word <- strsplit(as.character(head(bigram_match$ngram, 1)), " ")[[1]][2]
      print(paste("Next word from bigram:", next_word))  # Debugging: Print the next word from bigram
      return(next_word)
    }
  }
  
  print("Returning unigram default")  # Debugging: Print when returning the default unigram
  return(as.character(head(unigram_df$ngram, 1)))
}

shinyServer(function(input, output) {
  output$prediction <- renderPrint({
    req(input$phrase)
    
    # Call the prediction function
    prediction <- predict_next_word(input$phrase, unigram_df, bigram_df, trigram_df)
    prediction
  })
})
