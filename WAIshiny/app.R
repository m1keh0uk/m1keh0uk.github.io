library(shiny)
library(ggplot2)
library(dplyr)

absorbanceShiny <- read.csv("absorbanceShiny.csv")

ui <- fluidPage(
  titlePanel("Mean Absorbance by Identifier"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "selected_identifiers",
        "Select Studies:",
        choices = unique(absorbanceShiny$Identifier),
        selected = unique(absorbanceShiny$Identifier)
      )
    ),
    
    mainPanel(
      plotOutput("absorbance_plot"),
      br(),
      div(
        style = "margin-top: 20px;",
        "To access the data used here, visit this website: http://www.science.smith.edu/wai-database/. This graph was meant to mimic the work of Susan E Voss, her much better looking graph can be seen at this website: https://pmc.ncbi.nlm.nih.gov/articles/PMC7093226/"
      )
    )
  )
)

server <- function(input, output, session) {

  filtered_data <- reactive({
    absorbanceShiny %>%
      filter(Identifier %in% input$selected_identifiers)
  })

  output$absorbance_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = frequency, y = mean_absorbance, group = Identifier, color = colorID)) +
      geom_line() +
      ylim(0, 1) +
      xlim(0, 8000) +
      scale_x_log10() +
      labs(
        x = "Frequency (Hz)",
        y = "Mean Absorbance",
        title = "Mean absorbance from each publication in WAI database",
        color = ""
      ) +
      theme(
        legend.position = c(0.17, 0.78),
        legend.text = element_text(size = 5),
        legend.key.size = unit(0.3, "cm"),
        legend.spacing.x = unit(0.2, "cm"),
        legend.spacing.y = unit(0.1, "cm"),
        legend.background = element_rect(fill = "white", size = 0.5)
      )
  })
}

shinyApp(ui, server)
