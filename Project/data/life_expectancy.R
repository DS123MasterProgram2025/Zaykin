library(rvest)
library(dplyr)
library(purrr)
library(DBI)
library(RSQLite)
library(janitor)

scrape_life_expectancy <- function() {
  html <- read_html(
    "https://web.archive.org/web/20170616025729/http://ukrstat.gov.ua/operativ/operativ2007/ds/nas_rik/nas_u/nas_rik_u.html",
  )

  life_expectancy <- html %>%
    html_nodes("table") %>%
    pluck(10) %>%
    html_table()

  life_expectancy <- life_expectancy %>%
    slice(-1) %>%
    row_to_names(row_number = 1)

  colnames(life_expectancy)[1] <- "Тривалість життя"

  life_expectancy <- life_expectancy %>%
    mutate(across(2:last_col(), ~ as.double(gsub(",", ".", gsub("[^0-9,]", "", .))))) %>%
    rename_with(~ str_replace_all(., "\\s+", " ") %>% str_trim()) %>%
    rename(
      Year = 1,
      `Обидві статі` = 2,
      Чоловіки = 3,
      Жінки = 4
    ) %>%
    pivot_longer(-Year, names_to = "Тривалість життя", values_to = "Value") %>%
    pivot_wider(names_from = Year, values_from = Value)

  names(life_expectancy)[-1] <- substr(names(life_expectancy), 1, 4)

  fit_char <- rawToChar(serialize(life_expectancy, NULL, TRUE))

  db <- dbConnect(SQLite(), dbname = "data/database.sqlite")
  dbGetQuery(db, "DROP TABLE IF EXISTS life_expectancy")
  dbGetQuery(db, "CREATE TABLE IF NOT EXISTS life_expectancy
           (id INT PRIMARY KEY,
           value FLOAT)")
  df <- data.frame(id = 1, mdl=fit_char)
  dbGetPreparedQuery(db, 'INSERT INTO life_expectancy (value) values (:mdl)',
                   bind.data = df)
  dbDisconnect(db)
}

scrape_life_expectancy()