library(shiny)
library(bslib)
library(scales)  

# Custom Theme
theme <- bs_theme(
  bootswatch = "flatly",
  font_scale = 1.2
)

# Define UI for app that draws a histogram
ui <- page_sidebar(
  theme = theme,  
  title = "Central Limit Theorem Demonstration",
  
  tags$div(
    class = "text-muted mb-3",
    "Created by Leykun Getaneh"
  ),

  # Sidebar with controls
  sidebar = sidebar(
    title = "",
    numericInput("n", "Sample size", 
                 value = 100
    ),
    checkboxInput("pause", "Pause animation", FALSE),
  ),
  
  # Main panel with plot
  card(
    card_body(
      plotOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  data <- reactive({
    input$resample
    if (!isTRUE(input$pause)) {
      invalidateLater(1000)
    }
    rnorm(input$n)
  })
  
  output$plot <- renderPlot({
    par(mar = c(4, 4, 2, 1))  # Adjust margins
    
    # Create histogram
    hist(data(),
         breaks = 40,
         xlim = c(-3, 3),
         ylim = c(0, 0.5),
         col = alpha('#3498DB', 0.7),  # Semi-transparent blue
         border = 'white',
         xlab = "Value",
         freq = FALSE,
         main = "",  
         cex.lab = 1,
         cex.axis = 1
    )
    
    # Add normal distribution curve
    x <- seq(from = -3, to = 3, by = 0.01)
    y <- dnorm(x)
    lines(x, y, lwd = 2, col = 'darkgreen')
    
    # Add reference lines
    abline(v = 0, col = "red", lwd = 3, lty = 2)  # Population mean
    abline(v = mean(data()), col = "blue", lwd = 3, lty = 1)  # Sample mean
    
    # Add grid and legend
    grid(nx = NULL, ny = NULL, col = alpha("black", 0.1), lty = "dotted")
    
    legend("topright", 
           legend = c("Normal Distribution", "Population Mean", "Sample Mean"),
           col = c('darkgreen', "red", "blue"),
           lty = c(1, 2, 1),
           lwd = c(2, 3, 3),
           bty = "n",
           cex = 1.1,
           bg = alpha("white", 0.8)
    )
  }, res = 140, height = 500)
}

# Create Shiny app
shinyApp(ui = ui, server = server)