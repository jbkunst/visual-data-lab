library(shiny)
library(randomForest)

set.seed(2026)

iris_rf <- transform(
  iris,
  target = factor(ifelse(Species == "virginica", "yes", "no"), levels = c("no", "yes"))
)

model <- randomForest(
  target ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
  data = iris_rf,
  ntree = 100
)

ui <- fluidPage(
  titlePanel("randomForest Shinylive test"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("sepal_length", "Sepal length", 4, 8, 6, 0.1),
      sliderInput("sepal_width", "Sepal width", 2, 4.5, 3, 0.1),
      sliderInput("petal_length", "Petal length", 1, 7, 4.5, 0.1),
      sliderInput("petal_width", "Petal width", 0.1, 2.5, 1.5, 0.1)
    ),
    mainPanel(
      h3(textOutput("probability")),
      verbatimTextOutput("model_info")
    )
  )
)

server <- function(input, output, session) {
  current_profile <- reactive({
    data.frame(
      Sepal.Length = input$sepal_length,
      Sepal.Width = input$sepal_width,
      Petal.Length = input$petal_length,
      Petal.Width = input$petal_width
    )
  })

  output$probability <- renderText({
    pd <- predict(model, current_profile(), type = "prob")[, "yes"]
    sprintf("P(virginica) = %.1f%%", 100 * pd)
  })

  output$model_info <- renderPrint({
    model
  })
}

shinyApp(ui, server)
