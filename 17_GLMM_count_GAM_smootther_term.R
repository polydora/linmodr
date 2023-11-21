
smoother <- function (b0 = 1, b1, b2, b3){
  library(dplyr)
  df <- data.frame(x = seq(0, 1, 0.01))
  df <- df %>% mutate(smooth = 1 + b1*x - b2*x^2 + b3*x^3)

  ggplot(df, aes(x = x, y = smooth) ) + geom_line()



}




library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Конструирование базиса для сглаживающей функции"),

  sidebarLayout(
    sidebarPanel(
      #Задаем параметры для кубического сплайна
      sliderInput(inputId = "b1",
                  label = "Коэффициент $b_1$",
                  min = 0,
                  max = 1,
                  value = 0),
      sliderInput(inputId = "b2",
                  label = "Коэффициент $b_2$",
                  min = 0,
                  max = 10,
                  value = 0),
      sliderInput(inputId = "b3",
                  label = "Коэффициент $b_3$",
                  min = 0,
                  max = 10,
                  value = 0)
  ),



    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("my_graph")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$my_graph <- renderPlot({

    #Здесь может быть код
    #Функция рисующая график
    smoother(b1 = input$b1, b2 = input$b2, b3 =  input$b3)


  })
}

# Run the application
shinyApp(ui = ui, server = server)
