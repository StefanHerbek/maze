library(shiny)
library(igraph)
source("plot_maze.R")
igraph.options(vertex.label.family = "sans"
               )

ui <- shinyUI(ui = {
  pageWithSidebar(
    headerPanel("Maze"),
    sidebarPanel(
      sliderInput("row", label = "rows:", min = 2, max = 50, value = 10, step = 1),
      sliderInput("col", label = "cols:", min = 2, max = 50, value = 10, step = 1),
      selectInput("weightfunc", "weight function:", c("runif", "rnorm"), selected = "runif"),
      sliderInput("wall.size", label = "wall.size", min = 1, max = 10, value = 5, step = 1),
      sliderInput("vertex.size", label = "vertex.size", min = 1, max = 50, value = 20, step = 1),
      selectInput("vertex.color", "vertex.color:", c("black", "steelblue", "white"), selected = "black"),
      sliderInput("vertex.label.cex", label = "vertex.label.cex", min = 0.1, max = 2, value = 1, step = .1),
      selectInput("vertex.label.color", "vertex.label.color:", c("black", "red", "white"), selected = "white")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Maze",
                 plotOutput("maze", width = "600px", height = "600px")
                 ),
        tabPanel("Grid"),
        tabPanel("MST"),
        tabPanel("About")
      )
    )
  )
})

server <- shinyServer(func = function(input, output, server) {
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
    s$layout <- layout_on_grid(s,width = nr, height = nc)
    s
  })

  output$maze <- renderPlot({
    g <- maze()
    plotMaze(g, nrow = input$col, ncol = input$row, lwd = input$wall.size)
    # plot(g,
    #      edge.width = 5,
    #      vertex.size = input$vertex.size,
    #      vertex.color = input$vertex.color,
    #      vertex.label.cex = input$vertex.label.cex,
    #      vertex.label.color = input$vertex.label.color)
  })
})

shinyApp(ui = ui, server = server)
