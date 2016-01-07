library(shiny)
library(igraph)
source("plot.R")
theme_set(.theme_base)
igraph.options(vertex.label.family = "sans"
               )

ui <- shinyUI(ui = {
  pageWithSidebar(
    headerPanel("Maze"),
    sidebarPanel(
      h3("Maze parameters:"),
      sliderInput("row", label = "# row:", min = 2, max = 50, value = 10, step = 1),
      sliderInput("col", label = "# col:", min = 2, max = 50, value = 10, step = 1),
      selectInput("weightfunc", "weight function:", c("runif", "rnorm"), selected = "runif"),
      actionButton("maze", "Compute maze", icon = icon("cog", lib = "glyphicon")),
      h3("Plot parameters:"),
      conditionalPanel(condition = "input.tabs == 'Maze'",
        checkboxInput("path.show", "Show path", value = FALSE),
        selectInput("path.start", "Start", choices = 1),
        selectInput("path.end", "End", choices = 1),
        sliderInput("wall.size", label = "wall.size", min = 1.0, max = 10.0, value = 5.0, step = .5),
        checkboxInput("tile.number.show", "show tile number", value = FALSE),
        sliderInput("tile.number.size", label = "tile.number.size", min = 0.1, max = 10, value = 5, step = .1),
        checkboxInput("tile.show", "show tile", value = FALSE),
        sliderInput("tile.size", label = "tile.size", min = 0.1, max = 5, value = .1, step = .1),
        selectInput("tile.color", "tile.color:", c("white", "grey"), selected = "white"),
        actionButton("update.plotmaze", "Update plot")
      ),
      conditionalPanel(condition = "input.tabs == 'Graph'",
        sliderInput("vertex.size", label = "vertex.size", min = 1, max = 50, value = 15, step = 1),
        selectInput("vertex.color", "vertex.color:", c("black", "steelblue", "grey"), selected = "black"),
        sliderInput("vertex.label.cex", label = "vertex.label.cex", min = 0.1, max = 2, value = 1, step = .1),
        selectInput("vertex.label.color", "vertex.label.color:", c("black", "red", "white"), selected = "white"),
        actionButton("update.plotgraph", "Update plot")
      )
    ),
    mainPanel(
      tabsetPanel(id = "tabs",
        tabPanel("Maze",
                 plotOutput("plotmaze", width = "600px", height = "600px"),
                 downloadButton("maze.down")
                 ),
        tabPanel("Graph",
                 plotOutput("plotgraph", width = "600px", height = "600px"),
                 downloadButton("graph.down")
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
    input$maze

    isolate({
      nr <- input$row
      nc <- input$col
      g <- graph.lattice(c(nr, nc))
      nv <- vcount(g)
      ne <- ecount(g)
      g$ncol <- nc
      g$nrow <- nr

      switch(input$weightfunc,
             runif = wf <- runif,
             rnorm = wf <- rnorm
      )
      w <- wf(ne)

      s <- mst(g, weights = w, algorithm = "prim")
      s$layout <- layout_on_grid(s, width = nr, height = nc)
      s
    })
  })

  output$plotmaze <- renderPlot({
    g <- maze()
    input$update.plotmaze

    isolate({
      plotMaze(
        g,
        #nrow = input$col,
        #ncol = input$row,
        nrow = g$ncol,
        ncol = g$nrow,
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
  })

  output$maze.down <- downloadHandler(
    filename = "maze.pdf",
    content = function(file) {
      ggsave(filename = file)
    }
  )

  observe({
    n <- input$col*input$row
    updateSelectInput(session, inputId = "path.start", choices = 1:n, selected = 1)
    updateSelectInput(session, inputId = "path.end", choices = 1:n, selected = n)
  })

  output$plotgraph <- renderPlot({
    g <- maze()
    input$update.plotgraph

    isolate({
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
    })


    # if (input$path.show) {
    #   sp <- get.shortest.paths(g, from = input$path.start, to = input$path.end)$vpath[[1]]
    #
    #   V(g)$color <- input$vertex.color
    #   V(g)[sp]$color <- "red"
    #
    #   E(g)$color <- "grey"
    #   E(g, path = sp)$color <- "red"
    # } else {
    #   V(g)$color <- input$vertex.color
    #   E(g)$color <- "grey"
    # }
    # plot(g,
    #      edge.width = 5,
    #      vertex.size = input$vertex.size,
    #      vertex.label.cex = input$vertex.label.cex,
    #      vertex.label.color = input$vertex.label.color)
  })

  output$graph.down <- downloadHandler(
    filename = "graph.pdf",
    content = function(file) {
      ggsave(filename = file)
    }
  )

  output$author <- renderPrint({
    print("Diego Diez")
  })

  output$session <- renderPrint({sessionInfo()})
})

shinyApp(ui = ui, server = server)
