library(shiny)
library(DT)

ui <- fluidPage(
  sliderInput(inputId = 'bins',
              label = 'Slide to change bin value',
              min = 2,
              max = 50,
              value = 10),
  plotOutput(outputId = 'distplot'),
  sliderInput(inputId = 'highlight',
              label = 'Slide to highlight table',
              min = 1,
              max = 5,
              value = 1),
  dataTableOutput('table')

)

server <- function(input, output, session){

  output$distplot <- renderPlot({

  # generate bins based on input$bins from ui.R
  x    <- faithful[, 2]
  bins <- seq(min(x), max(x), length.out = input$bins + 1)

  # draw the histogram with the specified number of bins
  hist(x, breaks = bins, col = 'darkgray', border = 'white',
       xlab = 'Waiting time to next eruption (in mins)',
       main = 'Histogram of waiting times')
  })

  df <- faithful[1:5,]

  vals <- reactiveValues(
    eruptions = df[, 1],
    row_color = rep('white', 5)
  )

  observeEvent(input$highlight, {
    vals$eruptions <-
      c(vals$eruptions[1:input$highlight],
        vals$eruptions[input$highlight+1:length(vals$eruptions)])
    vals$row_color <- c(rep('yellow', input$highlight),
                        rep('white', length(vals$eruptions) - input$highlight))
  })

  output$table <- renderDataTable({
    datatable(df) |> formatStyle(
      'eruptions', target = 'row',
      backgroundColor = styleEqual(vals$eruptions,
                                   vals$row_color,
                                   default = 'white'
                                   )
    )
  })

}

shinyApp(ui, server)
