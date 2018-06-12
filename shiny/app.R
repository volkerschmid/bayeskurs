#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Binomialmodell mit Beta-Priori"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("a", 
                     "Priori-Parameter a:",
                     min = 0.01,
                     max = 10,
                     value = 1),
         sliderInput("b", 
                     "Priori-Parameter b:",
                     min = 0.01,
                     max = 10,
                     value = 1),
        sliderInput("s", 
                     "Anzahl Erfolge:",
                     min = 0,
                     max = 1000,
                     value = 100),
         sliderInput("f", 
                     "Anzahl Mißerfolge:",
                     min = 0,
                     max = 1000,
                     value = 100)
         
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
    p <- seq(0, 1, by = 0.001)
    prior <- dbeta(p,input$a, input$b)
    post <- dbeta(p, input$a+input$s, input$b+input$f)
    ylim=range(c(prior,post))
    if (ylim[2]>20)ylim[2]<-20
    plot(p, prior, type="l", lty=2, ylim=ylim)
    lines(p, post)
    legend(0,ylim[2],legend=c("Priori","Posteriori"),lty=c(1,2))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

