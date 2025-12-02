library(shiny)
library(bslib)
library(DBI)
library(RSQLite)
library(plotly)
library(dplyr)
library(tidyr)
library(bsicons)
library(shinycssloaders)

source("data/population_dynamic.R")
source("data/life_expectancy.R")
source("data/population.R")

# Custom theme with Ukrainian colors
ukraine_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#0057B7",
  secondary = "#FFD700",
  success = "#198754",
  info = "#17a2b8",
  warning = "#FFD700",
  danger = "#dc3545",
  base_font = font_google("Nunito"),
  heading_font = font_google("Montserrat"),
  "navbar-bg" = "#0057B7",
  "navbar-light-color" = "#FFD700"
)

# Custom CSS for enhanced styling
custom_css <- "
/* Animated gradient background for navbar */
.navbar {
  background: linear-gradient(135deg, #0057B7 0%, #003d82 50%, #0057B7 100%) !important;
  box-shadow: 0 4px 20px rgba(0, 87, 183, 0.3);
}

/* Card enhancements */
.card {
  border: none;
  border-radius: 16px;
  box-shadow: 0 4px 25px rgba(0, 0, 0, 0.08);
  transition: all 0.3s ease;
  overflow: hidden;
}

.card:hover {
  transform: translateY(-5px);
  box-shadow: 0 12px 40px rgba(0, 87, 183, 0.15);
}

.card-header {
  border-radius: 16px 16px 0 0 !important;
  font-weight: 600;
  letter-spacing: 0.5px;
}

/* Value box animations */
.bslib-value-box {
  border-radius: 16px !important;
  transition: all 0.3s ease;
  border: none !important;
}

.bslib-value-box:hover {
  transform: scale(1.03);
  box-shadow: 0 10px 30px rgba(0, 87, 183, 0.2);
}

/* Sidebar styling */
.sidebar {
  background: linear-gradient(180deg, #f8f9fa 0%, #e9ecef 100%);
  border-radius: 0 16px 16px 0;
}

/* Custom scrollbar */
::-webkit-scrollbar {
  width: 8px;
}

::-webkit-scrollbar-track {
  background: #f1f1f1;
  border-radius: 4px;
}

::-webkit-scrollbar-thumb {
  background: #0057B7;
  border-radius: 4px;
}

::-webkit-scrollbar-thumb:hover {
  background: #003d82;
}

/* Button styling */
.btn-primary {
  background: linear-gradient(135deg, #0057B7 0%, #003d82 100%);
  border: none;
  border-radius: 8px;
  transition: all 0.3s ease;
}

.btn-primary:hover {
  transform: translateY(-2px);
  box-shadow: 0 5px 15px rgba(0, 87, 183, 0.4);
}

/* Input styling */
.form-control, .form-select {
  border-radius: 8px;
  border: 2px solid #e9ecef;
  transition: all 0.3s ease;
}

.form-control:focus, .form-select:focus {
  border-color: #0057B7;
  box-shadow: 0 0 0 3px rgba(0, 87, 183, 0.1);
}

/* Checkbox and radio styling */
.form-check-input:checked {
  background-color: #0057B7;
  border-color: #0057B7;
}

/* DataTable styling */
.dataTables_wrapper {
  padding: 15px;
}

table.dataTable {
  border-radius: 12px;
  overflow: hidden;
}

table.dataTable thead th {
  background: linear-gradient(135deg, #0057B7 0%, #003d82 100%);
  color: white;
  font-weight: 600;
}

/* Page background */
.bslib-page-fill {
  background: linear-gradient(180deg, #f8fafc 0%, #e2e8f0 100%);
}

/* Fade-in animation */
@keyframes fadeInUp {
  from {
    opacity: 0;
    transform: translateY(20px);
  }
  to {
    opacity: 1;
    transform: translateY(0);
  }
}

.nav-panel-content > * {
  animation: fadeInUp 0.5s ease-out;
}

/* Tooltip styling */
.tooltip-inner {
  background: linear-gradient(135deg, #0057B7 0%, #003d82 100%);
  border-radius: 8px;
}

/* Footer styling */
.app-footer {
  background: linear-gradient(135deg, #0057B7 0%, #003d82 100%);
  color: white;
  padding: 15px;
  text-align: center;
  border-radius: 16px 16px 0 0;
  margin-top: 20px;
}

/* Spinner customization */
.shiny-spinner-output-container {
  min-height: 200px;
}
"

ui <- page_navbar(
  title = tags$span(
    tags$img(
      src = "https://flagcdn.com/w40/ua.png", 
      height = "28px", 
      style = "margin-right: 12px; box-shadow: 0 2px 8px rgba(0, 0, 0, 0.3); border-radius: 4px;",
    ),
    tags$span("Статистика Населення України", style = "font-weight: 700; letter-spacing: 1px;")
  ),
  theme = ukraine_theme,
  fillable = TRUE,
  header = tags$head(
    tags$style(HTML(custom_css)),
    tags$link(rel = "icon", type = "image/png", href = "https://flagcdn.com/w40/ua.png")
  ),

  # Overview Tab
  nav_panel(
    title = "Огляд",
    icon = bs_icon("house"),
    layout_columns(
      fill = FALSE,
      value_box(
        title = "Населення (2021)",
        value = textOutput("total_population", inline = TRUE),
        showcase = bs_icon("people-fill"),
        theme = "primary",
        p("Загальна чисельність")
      ),
      value_box(
        title = "Тривалість життя",
        value = textOutput("life_exp_value", inline = TRUE),
        showcase = bs_icon("heart-pulse-fill"),
        theme = "success",
        p("Середня очікувана")
      ),
      value_box(
        title = "Народжуваність (2024)",
        value = textOutput("birth_rate", inline = TRUE),
        showcase = bs_icon("emoji-smile-fill"),
        theme = "info",
        p("Народжених за рік")
      ),
      value_box(
        title = "Смертність (2024)",
        value = textOutput("death_rate", inline = TRUE),
        showcase = bs_icon("activity"),
        theme = "warning",
        p("Померлих за рік")
      ),
      value_box(
        title = "Природний приріст (2024)",
        value = textOutput("natural_growth", inline = TRUE),
        showcase = bs_icon("arrow-down-up"),
        theme = "danger",
        p("Народжуваність - Смертність")
      ),
      col_widths = c(3, 3, 2, 2, 2)
    ),
    layout_columns(
      card(
        card_header(
          class = "bg-primary text-white",
          tags$span(bs_icon("people-fill"), " Динаміка населення україни (тис. осіб)")
        ),
        card_body(
          withSpinner(plotlyOutput("overview_population", height = "280px"), type = 6, color = "#0057B7")
        )
      ),
      card(
        card_header(
          class = "bg-primary text-white",
          tags$span(bs_icon("graph-up-arrow"), " Динаміка народжуваності та смертності")
        ),
        card_body(
          withSpinner(plotlyOutput("overview_dynamics", height = "300px"), type = 6, color = "#0057B7")
        )
      ),
      card(
        card_header(
          class = "bg-primary text-white",
          tags$span(bs_icon("heart-pulse"), " Тривалість життя за статтю")
        ),
        card_body(
          withSpinner(plotlyOutput("overview_life_exp", height = "300px"), type = 6, color = "#0057B7")
        )
      ),
      col_widths = c(4, 4, 4)
    ),
    # layout_columns(
    #   col_widths = 12
    # )
  ),
  
  # Population Dynamics Tab
  nav_panel(
    title = "Динаміка населення",
    icon = bs_icon("graph-up"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Фільтри",
        width = 280,
        checkboxGroupInput(
          "dynamics_type",
          "Показники:",
          choices = c("Народжуваність", "Смертність"),
          selected = c("Народжуваність", "Смертність")
        ),
        sliderInput(
          "year_range",
          "Період років:",
          min = 1990,
          max = 2024,
          value = c(2000, 2024),
          step = 1,
          sep = ""
        ),
        radioButtons(
          "chart_type_dynamics",
          "Тип графіку:",
          choices = c("Лінійний" = "line", "Стовпчастий" = "bar", "Область" = "area"),
          selected = "line"
        )
      ),
      card(
        full_screen = TRUE,
        card_header(
          class = "bg-primary text-white",
          tags$span(bs_icon("bar-chart-line"), " Народжуваність та смертність в Україні")
        ),
        card_body(
          withSpinner(plotlyOutput("population_dynamics_plot", height = "500px"), type = 6, color = "#0057B7")
        )
      )
    )
  ),
  
  # Life Expectancy Tab
  nav_panel(
    title = "Тривалість життя",
    icon = bs_icon("heart-pulse"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Фільтри",
        width = 280,
        checkboxGroupInput(
          "gender_filter",
          "Стать:",
          choices = c("Обидві статі", "Чоловіки", "Жінки"),
          selected = c("Обидві статі", "Чоловіки", "Жінки")
        ),
        radioButtons(
          "chart_type_life",
          "Тип графіку:",
          choices = c("Лінійний" = "line", "Стовпчастий" = "bar"),
          selected = "line"
        )
      ),
      card(
        full_screen = TRUE,
        card_header(
          class = "bg-success text-white",
          tags$span(bs_icon("heart-pulse-fill"), " Середня тривалість життя в Україні")
        ),
        card_body(
          withSpinner(plotlyOutput("life_expectancy_plot", height = "500px"), type = 6, color = "#198754")
        )
      )
    )
  ),
  
  # Regional Population Tab
  nav_panel(
    title = "Населення регіонів",
    icon = bs_icon("geo-alt"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Фільтри",
        width = 280,
        selectInput(
          "region_select",
          "Оберіть регіони:",
          choices = NULL,
          multiple = TRUE
        ),
        selectInput(
          "year_select",
          "Рік для порівняння:",
          choices = NULL
        ),
        radioButtons(
          "region_view",
          "Вигляд:",
          choices = c("Топ-10 регіонів" = "top10", "Обрані регіони" = "selected", "Всі регіони" = "all"),
          selected = "top10"
        )
      ),
      layout_columns(
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-info text-white",
            tags$span(bs_icon("bar-chart-fill"), " Населення за регіонами")
          ),
          card_body(
            withSpinner(plotlyOutput("regional_bar", height = "450px"), type = 6, color = "#17a2b8")
          )
        ),
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-info text-white",
            tags$span(bs_icon("graph-up"), " Динаміка населення регіонів")
          ),
          card_body(
            withSpinner(plotlyOutput("regional_trend", height = "450px"), type = 6, color = "#17a2b8")
          )
        ),
        col_widths = c(6, 6)
      )
    )
  ),
  
  # Data Table Tab
  nav_panel(
    title = "Дані",
    icon = bs_icon("table"),
    layout_columns(
      fill = FALSE,
      card(
        card_header(
          class = "bg-secondary",
          tags$div(
            class = "d-flex justify-content-between align-items-center",
            tags$span(bs_icon("download"), " Завантажити дані"),
          )
        ),
        card_body(
          layout_columns(
            downloadButton("download_dynamics", "Динаміка населення (CSV)", class = "btn-primary"),
            downloadButton("download_life", "Тривалість життя (CSV)", class = "btn-success"),
            downloadButton("download_regions", "Регіони (CSV)", class = "btn-info"),
            col_widths = c(4, 4, 4)
          )
        )
      ),
      col_widths = 12
    ),
    navset_card_tab(
      title = "Таблиці даних",
      nav_panel(
        "Динаміка населення",
        DT::dataTableOutput("table_dynamics")
      ),
      nav_panel(
        "Тривалість життя",
        DT::dataTableOutput("table_life")
      ),
      nav_panel(
        "Регіони",
        DT::dataTableOutput("table_regions")
      )
    )
  ),
  
  nav_spacer(),
  nav_item(
    tags$a(
      bs_icon("github"),
      href = "https://github.com",
      target = "_blank",
      style = "color: white; text-decoration: none;",
      title = "GitHub Repository"
    )
  ),
  nav_item(
    tags$a(
      bs_icon("info-circle"),
      href = "#",
      onclick = "alert('Статистика Населення України - інтерактивна панель для візуалізації демографічних даних України. Дані зібрані з офіційних джерел.');",
      style = "color: white; text-decoration: none; cursor: pointer;",
      title = "Про додаток"
    )
  ),
  footer = tags$footer(
    class = "app-footer",
    tags$div(
      class = "container",
      tags$div(
        class = "row align-items-center",
        tags$div(
          class = "col-md-4",
          tags$img(src = "https://flagcdn.com/w40/ua.png", height = "24px", style = "margin-right: 10px;"),
          tags$span("Статистика Населення України", style = "font-weight: 600;")
        ),
        tags$div(
          class = "col-md-4 text-center",
          tags$span("© ", format(Sys.Date(), "%Y"), " | Дані з офіційних джерел")
        ),
        tags$div(
          class = "col-md-4 text-end",
          tags$a(href = "https://ukrstat.gov.ua/", target = "_blank", style = "color: #FFD700;", "Держстат"),
          tags$span(" | ", style = "color: rgba(255,255,255,0.5);"),
          tags$a(href = "https://uk.wikipedia.org/", target = "_blank", style = "color: #FFD700;", "Вікіпедія"),
          tags$span(" | ", style = "color: rgba(255,255,255,0.5);"),
          tags$a(href = "https://opendatabot.ua", target = "_blank", style = "color: #FFD700;", "Опендабот")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Load data from database with error handling
  tryCatch({
    db <- dbConnect(SQLite(), dbname = "data/database.sqlite")
    
    population_dynamic_raw <- dbGetQuery(db, "SELECT * FROM population_dynamic")
    population_dynamic <- unserialize(charToRaw(population_dynamic_raw$value))
    
    life_expectancy_raw <- dbGetQuery(db, "SELECT * FROM life_expectancy")
    life_expectancy <- unserialize(charToRaw(life_expectancy_raw$value))
    
    population_raw <- dbGetQuery(db, "SELECT * FROM population")
    population <- unserialize(charToRaw(population_raw$value))
    
    dbDisconnect(db)
  }, error = function(e) {
    showNotification(
      paste("Помилка завантаження даних:", e$message),
      type = "error",
      duration = NULL
    )
  })
  
  # Session end cleanup
  session$onSessionEnded(function() {
    # Cleanup code if needed
  })
  
  # Prepare data for plots
  dynamics_long <- population_dynamic %>%
    pivot_longer(-`Динаміка населення`, names_to = "Year", values_to = "Value") %>%
    mutate(Year = as.numeric(Year))
  
  life_exp_long <- life_expectancy %>%
    pivot_longer(-`Тривалість життя`, names_to = "Year", values_to = "Value") %>%
    mutate(Year = as.numeric(Year))
  
  population_long <- population %>%
    pivot_longer(-`Регіон`, names_to = "Year", values_to = "Value") %>%
    mutate(Year = as.numeric(Year))
  
  # Update UI elements
  observe({
    # Filter out Ukraine total from regions list
    regions <- unique(population$`Регіон`)
    regions <- regions[!grepl("Україна|україна", regions, ignore.case = TRUE)]
    updateSelectInput(session, "region_select", choices = regions, selected = regions[1:5])
    
    years <- names(population)[-1]
    updateSelectInput(session, "year_select", choices = years, selected = tail(years, 1))
  })
  
  # Overview value boxes
  output$total_population <- renderText({
    total <- population %>%
      filter(`Регіон` == "Україна" | grepl("^Україна", `Регіон`)) %>%
      pull(`2021`) %>%
      first()
    if (is.na(total)) {
      total <- population %>% 
        summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>%
        pull(`2021`)
    }
    paste0(total, " тис.")
  })
  
  output$life_exp_value <- renderText({
    val <- life_expectancy %>%
      filter(`Тривалість життя` == "Обидві статі") %>%
      select(last_col()) %>%
      pull() %>%
      first()
    paste0(round(val, 1), " років")
  })
  
  output$birth_rate <- renderText({
    val <- population_dynamic %>%
      filter(`Динаміка населення` == "Народжуваність") %>%
      select(last_col()) %>%
      pull() %>%
      first()
    format(val, big.mark = " ", scientific = FALSE)
  })
  
  output$death_rate <- renderText({
    val <- population_dynamic %>%
      filter(`Динаміка населення` == "Смертність") %>%
      select(last_col()) %>%
      pull() %>%
      first()
    format(val, big.mark = " ", scientific = FALSE)
  })
  
  output$natural_growth <- renderText({
    births <- population_dynamic %>%
      filter(`Динаміка населення` == "Народжуваність") %>%
      select(last_col()) %>%
      pull() %>%
      first()
    deaths <- population_dynamic %>%
      filter(`Динаміка населення` == "Смертність") %>%
      select(last_col()) %>%
      pull() %>%
      first()
    growth <- births - deaths
    sign <- if (growth >= 0) "+" else ""
    paste0(sign, format(growth, big.mark = " ", scientific = FALSE))
  })
  
  # Overview plots
  output$overview_dynamics <- renderPlotly({
    plot_ly(dynamics_long, x = ~Year, y = ~Value, color = ~`Динаміка населення`,
            type = 'scatter', mode = 'lines+markers',
            colors = c("Народжуваність" = "#0057B7", "Смертність" = "#dc3545")) %>%
      layout(
        xaxis = list(title = "Рік"),
        yaxis = list(title = "Кількість осіб"),
        legend = list(orientation = "h", y = -0.15),
        hovermode = "x unified"
      )
  })
  
  output$overview_life_exp <- renderPlotly({
    colors <- c("Обидві статі" = "#6c757d", "Чоловіки" = "#0057B7", "Жінки" = "#FFD700")
    
    plot_ly(life_exp_long, x = ~Year, y = ~Value, color = ~`Тривалість життя`,
            type = 'scatter', mode = 'lines+markers',
            colors = colors) %>%
      layout(
        xaxis = list(title = "Рік"),
        yaxis = list(title = "Роки життя"),
        legend = list(orientation = "h", y = -0.15),
        hovermode = "x unified"
      )
  })

  # Overall Ukraine population trend
  output$overview_population <- renderPlotly({
    ukraine_data <- population_long %>%
      filter(grepl("Україна|україна", `Регіон`, ignore.case = TRUE))
    
    plot_ly(ukraine_data, x = ~Year, y = ~Value,
            type = 'scatter', mode = 'lines+markers',
            line = list(color = '#0057B7', width = 3),
            marker = list(color = '#FFD700', size = 8, line = list(color = '#0057B7', width = 2)),
            fill = 'tozeroy',
            fillcolor = 'rgba(0, 87, 183, 0.1)') %>%
      layout(
        xaxis = list(title = "Рік", dtick = 5),
        yaxis = list(title = "Населення (тис. осіб)"),
        hovermode = "x unified"
      )
  })
  
  # Population Dynamics Plot
  output$population_dynamics_plot <- renderPlotly({
    req(input$dynamics_type)
    
    filtered_data <- dynamics_long %>%
      filter(
        `Динаміка населення` %in% input$dynamics_type,
        Year >= input$year_range[1],
        Year <= input$year_range[2]
      )
    
    colors <- c("Народжуваність" = "#0057B7", "Смертність" = "#dc3545")
    
    p <- plot_ly(filtered_data, x = ~Year, y = ~Value, color = ~`Динаміка населення`,
                 colors = colors)
    
    if (input$chart_type_dynamics == "line") {
      p <- p %>% add_trace(type = 'scatter', mode = 'lines+markers')
    } else if (input$chart_type_dynamics == "bar") {
      p <- p %>% add_bars()
    } else {
      p <- p %>% add_trace(type = 'scatter', mode = 'lines', fill = 'tozeroy')
    }
    
    p %>% layout(
      xaxis = list(title = "Рік", dtick = 2),
      yaxis = list(title = "Кількість осіб"),
      legend = list(orientation = "h", y = -0.1),
      hovermode = "x unified",
      barmode = "group"
    )
  })
  
  # Life Expectancy Plot
  output$life_expectancy_plot <- renderPlotly({
    req(input$gender_filter)
    
    filtered_data <- life_exp_long %>%
      filter(`Тривалість життя` %in% input$gender_filter)
    
    colors <- c("Обидві статі" = "#6c757d", "Чоловіки" = "#0057B7", "Жінки" = "#FFD700")
    
    p <- plot_ly(filtered_data, x = ~Year, y = ~Value, color = ~`Тривалість життя`,
                 colors = colors)
    
    if (input$chart_type_life == "line") {
      p <- p %>% add_trace(type = 'scatter', mode = 'lines+markers')
    } else {
      p <- p %>% add_bars()
    }
    
    p %>% layout(
      xaxis = list(title = "Рік"),
      yaxis = list(title = "Тривалість життя (роки)", range = c(60, 80)),
      legend = list(orientation = "h", y = -0.1),
      hovermode = "x unified",
      barmode = "group"
    )
  })
  
  # Regional Population Bar Chart
  output$regional_bar <- renderPlotly({
    req(input$year_select)
    
    year_col <- input$year_select
    
    filtered_data <- population %>%
      filter(!grepl("Україна|україна", `Регіон`, ignore.case = TRUE)) %>%
      select(`Регіон`, all_of(year_col)) %>%
      rename(Value = 2) %>%
      filter(!is.na(Value))
    
    if (input$region_view == "top10") {
      filtered_data <- filtered_data %>%
        arrange(desc(Value)) %>%
        head(10)
    } else if (input$region_view == "selected" && length(input$region_select) > 0) {
      filtered_data <- filtered_data %>%
        filter(`Регіон` %in% input$region_select)
    }
    
    filtered_data <- filtered_data %>%
      arrange(Value)
    
    plot_ly(filtered_data, y = ~reorder(`Регіон`, Value), x = ~Value,
            type = 'bar', orientation = 'h',
            marker = list(color = '#0057B7')) %>%
      layout(
        xaxis = list(title = "Населення (тис. осіб)"),
        yaxis = list(title = ""),
        margin = list(l = 150)
      )
  })
  
  # Regional Population Trend
  output$regional_trend <- renderPlotly({
    regions_to_show <- if (input$region_view == "selected" && length(input$region_select) > 0) {
      input$region_select
    } else {
      year_col <- input$year_select
      population %>%
        filter(!grepl("Україна|україна", `Регіон`, ignore.case = TRUE)) %>%
        arrange(desc(.data[[year_col]])) %>%
        head(5) %>%
        pull(`Регіон`)
    }
    
    filtered_data <- population_long %>%
      filter(`Регіон` %in% regions_to_show)
    
    plot_ly(filtered_data, x = ~Year, y = ~Value, color = ~`Регіон`,
            type = 'scatter', mode = 'lines+markers') %>%
      layout(
        xaxis = list(title = "Рік"),
        yaxis = list(title = "Населення (тис. осіб)"),
        legend = list(orientation = "h", y = -0.15),
        hovermode = "x unified"
      )
  })
  
  # Data Tables
  output$table_dynamics <- DT::renderDataTable({
    DT::datatable(
      population_dynamic,
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Ukrainian.json")
      ),
      class = "table table-striped table-hover"
    )
  })
  
  output$table_life <- DT::renderDataTable({
    DT::datatable(
      life_expectancy,
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Ukrainian.json")
      ),
      class = "table table-striped table-hover"
    )
  })
  
  output$table_regions <- DT::renderDataTable({
    DT::datatable(
      population,
      options = list(
        scrollX = TRUE,
        pageLength = 15,
        language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Ukrainian.json")
      ),
      class = "table table-striped table-hover"
    )
  })
  
  # Download handlers
  output$download_dynamics <- downloadHandler(
    filename = function() {
      paste("population_dynamics_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(population_dynamic, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  output$download_life <- downloadHandler(
    filename = function() {
      paste("life_expectancy_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(life_expectancy, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  output$download_regions <- downloadHandler(
    filename = function() {
      paste("population_regions_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(population, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui, server)
