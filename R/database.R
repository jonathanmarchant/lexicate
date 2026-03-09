library(DBI)
library(dplyr)
library(dbplyr)
library(gt)
# Sys.setenv(POSTGRES_PASS = askpass::askpass())

con <- dbConnect(RPostgres::Postgres(),
                 dbname = 'db_lexicate', 
                 host = 'pg-lexicate-jonathan-c732.d.aivencloud.com',
                 port = 20727,
                 user = 'lexicate',
                 password = Sys.getenv("POSTGRES_PASS"))

write_word_log <- function(con, word, assistance_level = 0, user = "jrm", success = 1, local_word_log) {
  row = tibble::tibble(
    event_datetime = lubridate::now(),
    user = user,
    word = word,
    assistance_level = assistance_level, 
    success_indicator = success)
  
  purrr::safely(dbAppendTable(con, SQL('"lexdata"."wordlog"'), row))
  
  bind_rows(local_word_log, row)
}

get_word_log <- function(con, wordlist, selected_user) {
  word_vector <- wordlist |> pull("word")

  word_log <- tbl(con, I("lexdata.wordlog")) |> 
    filter(user == selected_user) |> 
    filter(word %in% word_vector) |> 
    collect()

  word_log
}

user_summary <- function(con, user, word_log) {
  form_log <- word_log |>
    filter(assistance_level == 0) |>
    mutate(result = if_else(success_indicator == 1, "✅", "❌")) |>
    group_by(word) |>
    arrange(event_datetime) |>
    slice_tail(n = 5) |>
    summarise(form = paste0(result, collapse = ""))

  summary_log <- word_log |>
    filter(assistance_level == 0) |>
    summarise(
      last_attempt = max(event_datetime),
      correct = sum(success_indicator),
      attempts = n(),
      .by = word
    ) |>
    full_join(form_log, by = "word") |>
    arrange(desc(last_attempt))

  gt(summary_log) |>
    fmt_datetime(last_attempt)
}
