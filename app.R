library(shiny)
library(igraph)
source("plot.R")
igraph.options(vertex.label.family = "sans"
               )

ui <- shinyUI(ui = {
  pageWithSidebar(
    headerPanel("Maze"),
    sidebarPanel(
      sliderInput("row", label = "rows:", min = 2, max = 50, value = 10, step = 1),
      sliderInput("col", label = "cols:", min = 2, max = 50, value = 10, step = 1),
      selectInput("weightfunc", "weight function:", c("runif", "rnorm"), selected = "runif"),
      sliderInput("wall.size", label = "wall.size", min = 1.0, max = 10.0, value = 5.0, step = .5),
      checkboxInput("tile.number.show", "show tile number", value = FALSE),
      sliderInput("tile.number.size", label = "tile.number.size", min = 0.1, max = 10, value = .1, step = .1),
      checkboxInput("tile.show", "show tile", value = FALSE),
      sliderInput("tile.size", label = "tile.size", min = 0.1, max = 5, value = .1, step = .1),
      selectInput("tile.color", "tile.color:", c("white", "grey"), selected = "white"),
      sliderInput("vertex.size", label = "vertex.size", min = 1, max = 50, value = 20, step = 1),
      selectInput("vertex.color", "vertex.color:", c("black", "steelblue", "white"), selected = "black"),
      sliderInput("vertex.label.cex", label = "vertex.label.cex", min = 0.1, max = 2, value = 1, step = .1),
      selectInput("vertex.label.color", "vertex.label.color:", c("black", "red", "white"), selected = "white")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Maze",
                 selectInput("path.start", "Start", choices = 1),
                 selectInput("path.end", "End", choices = 1),
                 checkboxInput("path.show", "Show path", value = FALSE),
                 plotOutput("maze", width = "600px", height = "600px")
                 ),
        tabPanel("Graph",
                 plotOutput("graph", width = "600px", height = "600px")
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
  maze <- reactive({
    nr <- input$row
    nc <- input$col
    g <- graph.lattice(c(nr, nc))
    nv <- vcount(g)
    ne <- ecount(g)

    switch(input$weightfunc,
           runif = wf <- runif,
           rnorm = wf <- rnorm
    )
    w <- wf(ne)

    s <- mst(g, weights = w, algorithm = "prim")
    s$layout <- layout_on_grid(s, width = nr, height = nc)
    s
  })

  output$maze <- renderPlot({
    g <- maze()
    plotMaze(
      g,
      nrow = input$col,
      ncol = input$row,
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
  })

  observe({
    n <- input$col*input$row
    updateSelectInput(session, inputId = "path.start", choices = 1:n, selected = 1)
    updateSelectInput(session, inputId = "path.end", choices = 1:n, selected = n)
  })

  output$graph <- renderPlot({
    g <- maze()
    plot(g,
         edge.width = 5,
         vertex.size = input$vertex.size,
         vertex.color = input$vertex.color,
         vertex.label.cex = input$vertex.label.cex,
         vertex.label.color = input$vertex.label.color)
  })

  output$author <- renderPrint({
    print("Diego Diez")
  })

  output$session <- renderPrint({sessionInfo()})
})

shinyApp(ui = ui, server = server)
