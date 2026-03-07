library(DBI)
# Sys.setenv(POSTGRES_PASS = askpass::askpass())

con <- dbConnect(RPostgres::Postgres(),
                 dbname = 'db_lexicate', 
                 host = 'pg-lexicate-jonathan-c732.d.aivencloud.com',
                 port = 20727,
                 user = 'lexicate',
                 password = Sys.getenv("POSTGRES_PASS"))

write_word_log <- function(word, assistance_level = 0, user = "jrm", success = 1) {
  row = tibble::tibble(
    event_datetime = lubridate::now(),
    user = user,
    word = word,
    assistance_level = assistance_level, 
    success_indicator = success)
  
  purrr::safely(dbAppendTable(con, SQL('"lexdata"."wordlog"'), row))
  return(invisible(TRUE))
}

