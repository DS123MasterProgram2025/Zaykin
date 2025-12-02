library(rvest)
library(dplyr)
library(purrr)
library(DBI)
library(RSQLite)
library(tidyverse)
library(tidyr)

scrape_birth_wiki_table <- function() {
  html <- read_html(
    "https://uk.wikipedia.org/wiki/%D0%9F%D1%80%D0%B8%D1%80%D0%BE%D0%B4%D0%BD%D0%B8%D0%B9_%D1%80%D1%83%D1%85_%D0%BD%D0%B0%D1%81%D0%B5%D0%BB%D0%B5%D0%BD%D0%BD%D1%8F_%D0%A3%D0%BA%D1%80%D0%B0%D1%97%D0%BD%D0%B8",
    encoding = "UTF-8"
  )

  birth_wiki <- html %>%
    html_nodes("table.collapsible") %>%
    pluck(3) %>%
    html_table()

  colnames(birth_wiki) <- make.unique(colnames(birth_wiki), sep = "_")
  colnames(birth_wiki)[1] <- "Динаміка населення"

  birth_wiki <- birth_wiki %>%
    filter(.[[1]] == "Україна") %>%
    mutate("Динаміка населення" = ifelse(row_number() == 1, "Народжуванiсть", "Динаміка населення")) %>%
    mutate(across(2:last_col(), ~ as.numeric(gsub("[^0-9]", "", .))))

  return(birth_wiki)
}

scrape_death_wiki_table <- function() {
  html <- read_html(
    "https://uk.wikipedia.org/wiki/%D0%A1%D0%BC%D0%B5%D1%80%D1%82%D0%BD%D1%96%D1%81%D1%82%D1%8C_%D0%B2_%D0%A3%D0%BA%D1%80%D0%B0%D1%97%D0%BD%D1%96",
    encoding = "UTF-8"
  )

  death_wiki <- html %>%
    html_nodes("table") %>%
    pluck(4) %>%
    html_table()

  colnames(death_wiki)[1] <- "Динаміка населення"
  colnames(death_wiki)[2] <- "Смертність"

  death_wiki <- death_wiki %>%
    select(1:2) %>%
    mutate(
      `Динаміка населення` = as.character(`Динаміка населення`),
      `Смертність` = as.numeric(gsub("[^0-9]", "", `Смертність`))
    ) %>%
    mutate(Category = "Смертність") %>%
    pivot_wider(names_from = `Динаміка населення`, values_from = `Смертність`)

  colnames(death_wiki)[1] <- "Динаміка населення"
  names(death_wiki)[-1] <- substr(names(death_wiki), 1, 4)

  return(death_wiki)
}

scrape_birth_death_opendabot_table <- function() {
  html <- read_html(
    "https://opendatabot.ua/analytics/birth-death-2025",
    encoding = "UTF-8"
  )

  birth_death_opendabot <- html %>%
    html_nodes("table") %>%
    pluck(1) %>%
    html_table()

  colnames(birth_death_opendabot)[1] <- "Динаміка населення"

  birth_death_opendabot <- birth_death_opendabot %>%
    mutate(across(
      2:last_col(),
      ~ as.double(gsub("[^0-9]", "", .))
    )) %>%

    return(birth_death_opendabot)
}

scrape_population_dynamic <- function() {
  birth_wiki <- scrape_birth_wiki_table()
  death_wiki <- scrape_death_wiki_table()
  birth_death_opendabot <- scrape_birth_death_opendabot_table()

  standardize_names <- function(df) {
    df %>%
      mutate(`Динаміка населення` = case_when(
        grepl("Народж", `Динаміка населення`) ~ "Народжуваність",
        grepl("Смерт", `Динаміка населення`) ~ "Смертність"
      ))
  }

  # Combine all tables
  final_table <- bind_rows(
    birth_death_opendabot %>% standardize_names() %>% pivot_longer(-`Динаміка населення`, names_to = "Year", values_to = "Value"),
    birth_wiki %>% standardize_names() %>% pivot_longer(-`Динаміка населення`, names_to = "Year", values_to = "Value"),
    death_wiki %>% standardize_names() %>% pivot_longer(-`Динаміка населення`, names_to = "Year", values_to = "Value"),
  ) %>%
    filter(!is.na(Value)) %>%
    distinct(`Динаміка населення`, Year, .keep_all = TRUE) %>%
    pivot_wider(names_from = Year, values_from = Value)

  # Sort columns by year
  year_cols <- names(final_table)[-1]
  final_table <- final_table %>%
    select(`Динаміка населення`, all_of(year_cols[order(as.numeric(year_cols))])) %>%
    select(1, matches("^\\d{4}$"))

  fit_char <- rawToChar(serialize(final_table, NULL, TRUE))

  db <- dbConnect(SQLite(), dbname = "data/database.sqlite")
  dbGetQuery(db, "DROP TABLE IF EXISTS population_dynamic")
  dbGetQuery(db, "CREATE TABLE IF NOT EXISTS population_dynamic
           (id INT PRIMARY KEY,
           value FLOAT)")
  df <- data.frame(id = 1, mdl=fit_char)
  dbGetPreparedQuery(db, 'INSERT INTO population_dynamic (value) values (:mdl)',
                   bind.data = df)
  dbDisconnect(db)
}

scrape_population_dynamic()