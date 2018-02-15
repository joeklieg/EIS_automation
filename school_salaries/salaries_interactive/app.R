# Load packages
library(shiny)
library(ggplot2)
library(tidyverse)
library(DT)

# Load data
total_vis <- read_csv(url("https://raw.githubusercontent.com/joeklieg/EIS_automation/master/school_salaries/salaries_interactive/total_data"))
institutions <- read_csv(url("https://raw.githubusercontent.com/joeklieg/EIS_automation/master/school_salaries/salaries_interactive/institutions"))
all_metrics <- sort(unique(total_vis$metric))
# Define UI for application that plots features of movies
ui <- fluidPage(

        br(),

        # Sidebar layout with a input and output definitions
        sidebarLayout(
                # Inputs
                sidebarPanel(
                        # Select variable for display
                        selectInput(inputId = "metric",
                                    label = "Select metric:",
                                    choices = all_metrics,
                                    selected = "average",
                                    multiple = TRUE),
                width = 2),

                # Output:
                mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel(title = "Plot",
                                             plotOutput(outputId = "scatterplot",
                                                        brush = "plot_brush"),
                                             br(),
                                             dataTableOutput(outputId = "salarytable"),
                                             br()
                                             ),
                                    tabPanel(title = "Codebook",
                                             dataTableOutput(outputId = "codebook"),
                                             br())
                                             ),
                width = 10)
        )
)

# Define server function required to create the scatterplot
server <- function(input, output) {

        # Create scatterplot object the plotOutput function is expecting
        output$scatterplot <- renderPlot({
                req(input$metric)
                selected_metric <- total_vis %>%
                        filter(metric %in% input$metric) %>%
                        select(department, prof_rank, salary, metric)
                ggplot(selected_metric, aes(department, salary, color = metric)) +
                        geom_point() +
                        facet_grid(. ~ prof_rank) +
                        labs(color = "Salary Metric") +
                        scale_y_discrete(breaks = pretty(total_vis$salary, n = 10)) +
                        expand_limits(y = c(70000, 185000)) +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                        labs(title = "Engineering Professors' Salaries by Discipline")
        })

        # Create data table
        output$salarytable <- DT::renderDataTable({
                req(input$metric)
                selected_metric <- total_vis %>%
                        filter(metric %in% input$metric) %>%
                        select(department, prof_rank, salary, metric)
                brushedPoints(selected_metric, input$plot_brush) %>%
                        select(department, prof_rank, salary, metric)
        })

        output$codebook <- DT::renderDataTable({
                datatable(data = institutions,
                          rownames = FALSE)
        })

}

# Create a Shiny app object
shinyApp(ui = ui, server = server)