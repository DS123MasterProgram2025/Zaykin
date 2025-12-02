library(rvest)
library(dplyr)
library(purrr)
library(DBI)
library(RSQLite)

scrape_population <- function() {
  html <- read_html(
    "https://uk.wikipedia.org/wiki/%D0%A7%D0%B8%D1%81%D0%B5%D0%BB%D1%8C%D0%BD%D1%96%D1%81%D1%82%D1%8C_%D0%BD%D0%B0%D1%81%D0%B5%D0%BB%D0%B5%D0%BD%D0%BD%D1%8F_%D0%A3%D0%BA%D1%80%D0%B0%D1%97%D0%BD%D0%B8",
    encoding = "UTF-8"
  )

  population <- html %>%
    html_nodes("table") %>%
    pluck(1) %>%
    html_table()

  colnames(population) <- make.unique(colnames(population), sep = "_")
  colnames(population)[1] <- "Регіон"

  population <- population %>%
    select(1, matches("^\\d{4}$")) %>%
    mutate(
      across(
        2:last_col(),
        ~ as.double(gsub(",", ".", gsub("[^0-9,]", "", .)))
      )
    )

  fit_char <- rawToChar(serialize(population, NULL, TRUE))

  db <- dbConnect(SQLite(), dbname = "data/database.sqlite")
  dbGetQuery(db, "DROP TABLE IF EXISTS population")
  dbGetQuery(db, "CREATE TABLE IF NOT EXISTS population
           (id INT PRIMARY KEY,
           value FLOAT)")
  df <- data.frame(id = 1, mdl=fit_char)
  dbGetPreparedQuery(db, 'INSERT INTO population (value) values (:mdl)',
                   bind.data = df)
  dbDisconnect(db)
}

scrape_population()