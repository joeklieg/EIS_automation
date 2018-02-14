# Load packages
library(shiny)
library(ggplot2)
library(tidyverse)
library(DT)

# Load data
total_vis <- read_csv("~/Desktop/ENG_data_group/school_salaries/salaries_interactive/total_data")
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
                                    multiple = TRUE)
                ),

                # Output:
                mainPanel(
                        # Show scatterplot with brushing capability
                        plotOutput(outputId = "scatterplot", hover = "plot_hover"),
                        # Show data table
                        dataTableOutput(outputId = "salarytable"),
                        br()
                )
        )
)

# Define server function required to create the scatterplot
server <- function(input, output) {

        # Create scatterplot object the plotOutput function is expecting
        output$scatterplot <- renderPlot({
                selected_metric <- total_vis %>%
                        filter(total_vis$metric == (input$metric))
                ggplot(selected_metric, aes(department, salary, color = metric)) +
                        geom_point() +
                        facet_grid(. ~ prof_rank) +
                        labs(color = "Salary Metric") +
                        scale_y_discrete(breaks = pretty(total_vis$salary, n = 10)) +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1))
        })

        # Create data table
        output$salarytable <- DT::renderDataTable({
                nearPoints(total_vis, coordinfo = input$plot_hover) %>%
                        select(department, prof_rank, salary, metric)
        })

}

# Create a Shiny app object
shinyApp(ui = ui, server = server)