#igraph.options(vertex.label.family = "sans")

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
        sliderInput("wall.size", label = "wall.size", min = 1.0, max = 10.0, value = 5.0, step = .5),
        checkboxInput("tile.number.show", "show tile number", value = FALSE),
        sliderInput("tile.number.size", label = "tile.number.size", min = 0.1, max = 10, value = 5, step = .1),
        checkboxInput("tile.show", "show tile", value = FALSE),
        sliderInput("tile.size", label = "tile.size", min = 0.1, max = 5, value = .1, step = .1),
        #selectInput("tile.color", "tile.color:", c("white", "grey"), selected = "white"),
        #colourInput("tile.color", "tile.color:", value = "white", showColour = "background"),
        #colourInput("tile.color", "tile.color:", value = NULL, showColour = "background", palette = "limited", allowedCols = sub("..$", "", inferno(10))),
        colourInput("tile.color", "tile.color:", value = NULL, showColour = "background", palette = "limited", allowedCols = brewer.pal(9, "Greys")),
        hr(),
        actionButton("update.plotmaze", "Update plot")
      ),
      conditionalPanel(condition = "input.tabs == 'Graph'",
        sliderInput("vertex.size", label = "vertex.size", min = 1, max = 50, value = 15, step = 1),
        selectInput("vertex.color", "vertex.color:", c("black", "steelblue", "grey"), selected = "black"),
        sliderInput("vertex.label.cex", label = "vertex.label.cex", min = 0.1, max = 2, value = 1, step = .1),
        selectInput("vertex.label.color", "vertex.label.color:", c("black", "red", "white"), selected = "white"),
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
      maze_graph(input$row, input$col, input$weightfunc)
    })
  })

  plotmaze <- function(g) {
    plotMaze(
      g,
      wall.size = input$wall.size,
      tile.color = input$tile.color,
      tile.show = input$tile.show,
      tile.size = input$tile.size,
      tile.number.show = input$tile.number.show,
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
      ggsave(filename = file, plot = plotmaze(maze()), scale = input$row/input$col)
    }
  )

  observe({
    n <- input$col*input$row
    updateSelectInput(session, inputId = "path.start", choices = 1:n, selected = 1)
    updateSelectInput(session, inputId = "path.end", choices = 1:n, selected = n)
  })

  plotgraph <- function(g) {
    E(g)$color <- "grey"
    V(g)$color <- input$vertex.color
    V(g)$label <- V(g)

    if (input$path.show) {
      sp <- get.shortest.paths(g, from = input$path.start, to = input$path.end)$vpath[[1]]

      V(g)[sp]$color <- "red"
      E(g, path = sp)$color <- "red"
    }

    plotGraph(
      g,
      layout = g$layout,
      vertex.fill = V(g)$color,
      vertex.color = V(g)$color,
      vertex.size = input$vertex.size,
      edge.color = E(g)$color,
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
      ggsave(filename = file, plot = plotgraph(maze()))
    }
  )

  output$author <- renderPrint({
    print("Diego Diez")
  })

  output$session <- renderPrint({
    #sessionInfo()
    })
})


#' Shiny app for plotting mazes.
#'
#' @return NULL
#' @export
#'
#' @examples
#' NULL
mazeUI <- function() {
  shinyApp(ui = ui, server = server)
}
