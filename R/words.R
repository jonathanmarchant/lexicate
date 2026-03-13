library(dplyr)
library(tibble)

summarise_word_log <- function(word_log, wordlist) {
  summary <- word_log |> 
    filter(assistance_level == 0) |> 
    group_by(word) |> 
    slice_max(event_datetime, n = 5) |> 
    summarise(attempts = n(),
              correct = sum(success_indicator)) |> 
    full_join(wordlist |> select(-sample_sentence, -homophone),
      by="word") |> 
    tidyr::replace_na(list(attempts = 0, correct = 0))

  summary
}

choose_word <- function(log_summary) {
  max_difficulty <- max(log_summary$difficulty)

  log_summary |> 
    mutate(priority = case_when(
      correct > 0 & correct < attempts ~ 1,
      attempts > 0 & attempts < 5 ~ 2,
      attempts == 0 & difficulty < max_difficulty ~ 3,
      correct == 0 & difficulty < max_difficulty ~ 4,
      attempts == 0 ~ 5,
      correct == 0 ~ 6,
      correct == 5 ~ 7,
      .default = 8 
    ),
    length = stringr::str_length(word)
  ) |> 
    arrange(priority, length, word) |> 
    slice_head(n = 10) |> 
    mutate(sample_weight = 11 - row_number()) |> 
    slice_sample(n = 1, weight_by = sample_weight) |> 
    pull(word)
}

context_sentence <- function(current_word, wordlist) {
  wordlist |> 
    filter(word == current_word) |> 
    pull(sample_sentence)
}

is_homophone <- function(current_word, wordlist) {
  result <- wordlist |> 
    filter(word == current_word) |> 
    pull(homophone)

  if_else(result == 1, TRUE, FALSE)
}
