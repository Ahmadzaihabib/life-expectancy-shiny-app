
# Healthcare Workforce and Life Expectancy: A Global Policy Simulation Dashboard
# Author: Habib Gul Ahmadzai
# Course: Global Data Analytics
# Date: 4/23/2026

# Note: The datafile used here is a product of the previous data cleaning and preparation script.


library(shiny)
library(tidyverse)
library(plotly)
library(broom)

# Loading the final cleaned data file created before during the data cleaning and preparation part.
health_data <- read_csv("health_data.csv", show_col_types = FALSE)

# Recreating logged variables
health_data <- health_data %>%
  mutate(
    log_gdp_per_capita = log(gdp_per_capita),
    log_health_exp_per_capita = log(health_exp_per_capita)
  )

# Running regression model
model <- lm(
  life_expectancy ~ physicians_per_1000 + log_gdp_per_capita + log_health_exp_per_capita,
  data = health_data
)

ui <- fluidPage(
  
  titlePanel("Healthcare Workforce and Life Expectancy: A Global Policy Simulation Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h4("What-If Policy Simulation"),
      
      sliderInput(
        "physicians",
        "Physicians Per 1,000 people:",
        min = floor(min(health_data$physicians_per_1000, na.rm = TRUE)),
        max = ceiling(max(health_data$physicians_per_1000, na.rm = TRUE)),
        value = median(health_data$physicians_per_1000, na.rm = TRUE),
        step = 0.1
      ),
      
      sliderInput(
        "gdp",
        "GDP Per Capita (USD):",
        min = floor(min(health_data$gdp_per_capita, na.rm = TRUE)),
        max = ceiling(max(health_data$gdp_per_capita, na.rm = TRUE)),
        value = median(health_data$gdp_per_capita, na.rm = TRUE),
        step = 500
      ),
      
      sliderInput(
        "health_exp",
        "Health Expenditure Per Capita (USD):",
        min = floor(min(health_data$health_exp_per_capita, na.rm = TRUE)),
        max = ceiling(max(health_data$health_exp_per_capita, na.rm = TRUE)),
        value = median(health_data$health_exp_per_capita, na.rm = TRUE),
        step = 50
      ),
      
      br(),
      
      selectizeInput(
        "countries",
        "Search and Select Countries for Comparison:",
        choices = sort(unique(health_data$country)),
        selected = c("Afghanistan", "Pakistan"),
        multiple = TRUE,
        options = list(
          placeholder = "Type country names...",
          maxItems = 5
        )
      ),
      
      br(),
      p("Move the sliders to simulate how changes in healthcare workforce and economic conditions affect predicted life expectancy."),
      p("Use the country selector to compare life expectancy and physician density trends across countries.")
    ),
    
    mainPanel(
      h3("Predicted Life Expectancy"),
      verbatimTextOutput("prediction_text"),
      br(),
      
      h3("Physician Density and Life Expectancy"),
      plotlyOutput("scatter_plot"),
      br(),
      
      h3("Global Trend in Life Expectancy"),
      plotlyOutput("trend_plot"),
      br(),
      
      h3("Life Expectancy Comparison by Country"),
      plotlyOutput("country_plot"),
      br(),
      
      h3("Physician Density Over Time by Country"),
      plotlyOutput("physicians_plot"),
      br(),
      
      h3("Model Coefficients"),
      tableOutput("model_table"),
      br(),
      
      h4("Model Insight"),
      p("This interactive dashboard uses a multiple regression model to estimate life expectancy based on three key factors: physician density (physicians per 1,000 people), GDP per capita, and health expenditure per capita. The What-If simulation tool allows users to adjust these inputs using the sliders and instantly observe the predicted impact on life expectancy. This helps illustrate how changes in healthcare capacity and economic conditions are associated with population health outcomes."),
      
      p("Forvcountry-specific trends exploration, use the search bar in the sidebar to select one or more countries. The selected countries will be applied across all comparison charts. This will allowe you to examine how life expectancy and physician density have evolved over time for the selected countries.Search and select multiple countries to compare their trajectories side by side."),
      
      p("Note: Some countries have missing observations in earlier years due to data availability limitations. As a result, lines in the charts may begin at different points in time. This reflects real-world data gaps.")
    )
  )
)

server <- function(input, output) {
  
  predicted_value <- reactive({
    new_data <- data.frame(
      physicians_per_1000 = input$physicians,
      log_gdp_per_capita = log(input$gdp),
      log_health_exp_per_capita = log(input$health_exp)
    )
    
    predict(model, newdata = new_data)
  })
  
  output$prediction_text <- renderText({
    paste0("Predicted Life Expectancy: ", round(predicted_value(), 2), " years")
  })
  
  output$scatter_plot <- renderPlotly({
    p <- ggplot(health_data, aes(x = physicians_per_1000, y = life_expectancy)) +
      geom_point(alpha = 0.30, size = 0.8, color = "black") +
      geom_smooth(method = "lm", se = TRUE) +
      geom_point(
        aes(x = input$physicians, y = predicted_value()),
        size = 2.5,
        color = "darkred"
      ) +
      labs(
        title = "Relationship Between Physician Density and Life Expectancy",
        x = "Physicians per 1,000 people",
        y = "Life Expectancy (years)"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$trend_plot <- renderPlotly({
    
    trend_data <- health_data %>%
      group_by(year) %>%
      summarise(
        avg_life_expectancy = mean(life_expectancy, na.rm = TRUE),
        .groups = "drop"
      )
    
    p <- ggplot(trend_data, aes(x = year, y = avg_life_expectancy)) +
      geom_line(size = 1, color = "darkblue") +
      geom_point(size = 1.5, color = "black")
      labs(
        title = "Average Global Life Expectancy Over Time",
        x = "Year",
        y = "Average Life Expectancy (years)"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$country_plot <- renderPlotly({
    
    country_data <- health_data %>%
      filter(country %in% input$countries)
    
    p <- ggplot(country_data, aes(x = year, y = life_expectancy, color = country)) +
      geom_line(size = 1) +
      geom_point(size = 1.5) +
      labs(
        title = "Life Expectancy Comparison by Country",
        x = "Year",
        y = "Life Expectancy (years)",
        color = "Country"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$physicians_plot <- renderPlotly({
    
    country_data <- health_data %>%
      filter(country %in% input$countries)
    
    p <- ggplot(country_data, aes(x = year, y = physicians_per_1000, color = country)) +
      geom_line(size = 1) +
      geom_point(size = 1.5) +
      labs(
        title = "Physician Density Over Time by Country",
        x = "Year",
        y = "Physicians per 1,000 people",
        color = "Country"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$model_table <- renderTable({
    broom::tidy(model) %>%
      mutate(
        estimate = round(estimate, 3),
        std.error = round(std.error, 3),
        statistic = round(statistic, 3),
        p.value = ifelse(p.value < 0.001, "<0.001", round(p.value, 4))
      ) %>%
      rename(
        Variable = term,
        Estimate = estimate,
        `Std. Error` = std.error,
        `t value` = statistic,
        `p-value` = p.value
      )
  })
}

shinyApp(ui = ui, server = server)