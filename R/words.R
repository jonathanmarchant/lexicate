library(dplyr)
library(tibble)

y1_wordlist <- tibble(
  word = c("a", "are", "ask", "be", "by", "come", "do", "friend", "full", "go",
  "has", "he", "here", "his", "house", "I", "is", "love", "me", "my", "no", "of", "once",
  "one", "our", "pull", "push", "put", "said", "says", "school", "she", "so", "some", "the",
  "there", "they", "to", "today", "was", "we", "were", "where", "you", "your")) |> 
  mutate(difficulty = 1)

y2_wordlist <- tibble(
  word = c("after", "again", "any", "bath", "beautiful", "because", "behind", "both", "break",
  "busy", "child", "children", "class", "climb", "clothes", "cold", "could", "door", "even", "every",
  "everybody", "eye", "fast", "father", "find", "floor", "gold", "grass", "great", "half", "hold",
  "hour", "improve", "kind", "last", "many", "mind", "money", "most", "move", "Mr", "Mrs", "old",
  "only", "parents", "pass", "past", "path", "people", "plant", "poor", "pretty", "prove", "should",
  "steak", "sugar", "sure", "told", "water", "who", "whole", "wild", "would")) |> 
  mutate(difficulty = 2)

create_wordlist <- function(min_difficulty = 1, max_difficulty = 2) {
  bind_rows(y1_wordlist, y2_wordlist) |> 
    filter(difficulty >= min_difficulty & difficulty <= max_difficulty)
}

wordlist <- create_wordlist()

summarise_word_log <- function(word_log, wordlist) {
  summary <- word_log |> 
    filter(assistance_level == 0) |> 
    group_by(word) |> 
    slice_max(event_datetime, n = 5) |> 
    summarise(attempts = n(),
              correct = sum(success_indicator)) |> 
    full_join(wordlist, by="word") |> 
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