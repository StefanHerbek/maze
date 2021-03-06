library(maze)
palcol <- c(RColorBrewer::brewer.pal(9, "Greys"), RColorBrewer::brewer.pal(8, "Dark2"))
ui <- shinyUI(ui = {
  pageWithSidebar(
    headerPanel("Maze"),
    sidebarPanel(
      h3("Maze parameters:"),
      sliderInput("row", label = "# row:", min = 2, max = 50, value = 10, step = 1),
      sliderInput("col", label = "# col:", min = 2, max = 50, value = 10, step = 1),
      selectInput("weightfunc", "weight function:", c("runif", "rnorm"), selected = "runif"),
      hr(),
      actionButton("compute", "Compute maze", icon = icon("cog", lib = "glyphicon")),
      h3("Plot parameters:"),
      checkboxInput("path.show", "Show path", value = FALSE),
      selectInput("path.start", "Start", choices = 1),
      selectInput("path.end", "End", choices = 1),
      hr(),
      conditionalPanel(condition = "input.tabs == 'Maze'",
        sliderInput("wall.size", label = "wall.size", min = 0, max = 10, value = 5, step = 1),
        sliderInput("tile.number.size", label = "tile.number.size", min = 0, max = 10, value = 0, step = 1),
        sliderInput("tile.size", label = "tile.size", min = 0, max = 1, value = .1, step = .1),
        colourpicker::colourInput("tile.color", "tile.color:", value = palcol[3], showColour = "background", palette = "limited", allowedCols = palcol),
        hr(),
        actionButton("update.plotmaze", "Update plot")
      ),
      conditionalPanel(condition = "input.tabs == 'Graph'",
        sliderInput("vertex.size", label = "vertex.size", min = 0, max = 50, value = 15, step = 1),
        colourpicker::colourInput("vertex.color", "vertex.color:", value = palcol[9], showColour = "background", palette = "limited", allowedCols = palcol),
        sliderInput("vertex.label.size", label = "vertex.label.size", min = 0, max = 10, value = 5, step = 1),
        colourpicker::colourInput("vertex.label.color", "vertex.label.color:", value = palcol[1], showColour = "background", palette = "limited", allowedCols = palcol),
        colourpicker::colourInput("edge.color", "edge.color:", value = palcol[3], showColour = "background", palette = "limited", allowedCols = palcol),
        hr(),
        actionButton("update.plotgraph", "Update plot")
      )
    ),
    mainPanel(
      tabsetPanel(id = "tabs",
        tabPanel("Maze",
                 plotOutput("plotmaze", width = "600px", height = "600px"),
                 downloadButton("maze.down", label = "")
                 ),
        tabPanel("Graph",
                 plotOutput("plotgraph", width = "600px", height = "600px"),
                 downloadButton("graph.down", label = "")
                 ),
        tabPanel("About",
                 h3("Author"),
                 verbatimTextOutput("author"),
                 h3("Version"),
                 verbatimTextOutput("session")
                 )
      )
    )
  )
})

server <- shinyServer(func = function(input, output, session) {
  maze <- eventReactive(input$compute, {
    isolate({
      make_maze(input$row, input$col, input$weightfunc)
    })
  })

  plotmaze <- function(g) {
    plot_maze(
      g,
      wall.size = input$wall.size,
      tile.color = input$tile.color,
      tile.show = as.logical(input$tile.size),
      tile.size = input$tile.size,
      tile.number.show = as.logical(input$tile.number.size),
      tile.number.size = input$tile.number.size,
      path.show = input$path.show,
      path.start = input$path.start,
      path.end = input$path.end
    )
  }

  output$plotmaze <- renderPlot({
    input$update.plotmaze
    g <- maze()
    isolate(plotmaze(g))
  })

  output$maze.down <- downloadHandler(
    filename = "maze.pdf",
    content = function(file) {
      print(input$row/input$col)
      ggplot2::ggsave(filename = file, plot = plotmaze(maze()), scale = input$row/input$col)
    }
  )

  observe({
    n <- input$col*input$row
    updateSelectInput(session, inputId = "path.start", choices = 1:n, selected = 1)
    updateSelectInput(session, inputId = "path.end", choices = 1:n, selected = n)
  })

  plotgraph <- function(g) {
    plot_graph(
      g,
      vertex.fill = input$vertex.color,
      vertex.color = input$vertex.color,
      vertex.size = input$vertex.size,
      vertex.label.color = input$vertex.label.color,
      vertex.label.size = input$vertex.label.size,
      edge.color = input$edge.color,
      path.show = input$path.show,
      path.start = input$path.start,
      path.end = input$path.end,
      lwd = 2
    )
  }

  output$plotgraph <- renderPlot({
    input$update.plotgraph
    g <- maze()
    isolate(plotgraph(g))
  })

  output$graph.down <- downloadHandler(
    filename = "graph.pdf",
    content = function(file) {
      ggplot2::ggsave(filename = file, plot = plotgraph(maze()))
    }
  )

  output$author <- renderPrint({
    cat("Diego Diez\n")
    cat("https://github.com/ddiez/maze")
  })

  output$session <- renderPrint({
    sessionInfo()
  })
})

shinyApp(ui = ui, server = server)
