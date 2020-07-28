library(shiny)
library(htmlwidgets)
library(graph)
library(igraph)
library(jsonlite)
library(DT)

if(!require(cyjShiny)) { devtools::install_github('cytoscape/cyjShiny') }

# FUNCTIONS ----
source("graphToJSON.R")

roundDf <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  df[,nums] <- round(df[,nums], digits = digits)
  df
}

makeIgraphObj <- function(selectedCondition, curGraphFile) {
  curColors <- paste0(colorPrefix, selectedCondition)
  curCondition <- paste0(conditionPrefix, selectedCondition)
  
  igraphFilename <- paste0(strsplit(curGraphFile, "\\.")[[1]][1], ".rds")
  
  graphFilePath <- file.path(dataDir, igraphFilename)
  g <- readRDS(graphFilePath)
  
  if(curColors %in% colnames(conditions)) {
    idx <- which(conditions$gene %in% V(g)$name)
    
    V(g)$color <- conditions[idx, curColors]
    V(g)$value <- conditions[idx, curCondition]
    
    # DEBUG
    # str(sort(idx))
    # tmp <- conditions[idx, curCondition]
    # names(tmp) <- conditions$gene[idx]
    # str(tmp)
  } else {
    cat("HEY")
    V(g)$color <- "grey"
    V(g)$value <- "NA"
  }
  
  return(g)
}

# PARAMETERS ----
dataDir <- "data"
  
conditionsFile <- file.path(dataDir, "conditions.rds")
#graphFiles <- c("citrate_cycle_tca_cycle.json", "glycolysis__gluconeogenesis.json", "pentose_phosphate_pathway.json") 
graphFiles <- sort(dir(path=dataDir, pattern=".json"))
names(graphFiles) <- sapply(graphFiles, function(x) { strsplit(x, "\\.")[[1]][1] }, USE.NAMES = FALSE) # Remove extension
graphFile <- graphFiles[1]
graphFile

conditionChoices <- c("Select Condition"="", "Condition 1"="1", "Condition 2"="2", "Condition 3"="3")

styleFile <- "default_style.js"

layouts <- c("cose", "cola", "circle", "concentric", "breadthfirst", "grid", "random", "dagre", "cose-bilkent") 
layoutName <- "preset"

conditionPrefix <- "cond"
colorPrefix <- "colors"
genePrefix <- "gene"

# LOAD DATA ----
conditions <- readRDS(conditionsFile)
#g <- readRDS(graphFile)

# PRE-PROCESS ----
#nodeAttrs <- nodeData(g, attr="label")
nodeIds <- sort(conditions$gene)

graph <- readLines(file.path(dataDir, graphFile))

# GLOBAL ----
curGraphFile <- graphFile
curIgraphObj <- NULL

# UI ----
ui = shinyUI(fluidPage(

  tags$head(
    # NOTE: Network will be 40% of browser height
    tags$style("#cyjShiny{height: 40vh !important; border: 1px solid #ffffff;}"), 
    tags$link(rel = "stylesheet", type = "text/css",
              href = "http://maxcdn.bootstrapcdn.com/font-awesome/4.2.0/css/font-awesome.min.css")),
  sidebarLayout(
      sidebarPanel(
          selectInput("selectCondition", "Select Condition:", choices=conditionChoices),
          selectInput("selectNetwork", "Select Network:", choices=graphFiles),
          uiOutput("selectNode"),
          #selectInput("selectNode", "Select Node:", choices = c("", nodeIds)),
          
          actionButton("fit", "Fit Network", icon=icon("compress")),
          br(),br(),
          actionButton("getSelectedNodes", "Refresh Table After Mouse Select", icon=icon("object-group")),
          br(),br(),
          actionButton("unselectNodes", "Refresh Table After Mouse Unselect", icon=icon("object-ungroup")),
          br(),br(),
          #actionButton("selectRandomNetwork", "Select Random Network"), 
          #br(),
          downloadButton("savePdf", "Save PDF"),
          br(),br(),
          downloadButton("saveGml", "Save GML (yEd/Cytoscape)"),
          br(),br(),
          downloadButton("saveCyjs", "Save Cytoscape JSON"),
          br(),br(),
          helpText("TIP: Customize visualization in Cytoscape"),
          width=4
      ),
      mainPanel(
          h3("Network Name"),
          textOutput("networkName"),
          h3("Network"),
          p('Zoom: Scroll; Pan/Move: Drag; Select: Click; Select Multiple: Shift+Click; Unselect: Click on empty area'),
          cyjShinyOutput('cyjShiny'),
          #h3("Selected Nodes"), # NOTE: Must be below otherwise the mouse positions will be wrong for network
          #verbatimTextOutput("selectedNodes"),
          h3("Selected Node Table (All if No Selection)"),
          DT::dataTableOutput("selectedNodeTable"),
          h3("Selected Node Edges Table (All if No Selection)"),
          DT::dataTableOutput("selectedNetworkTable"),
          width=8
      )
  ) # sidebarLayout
))

# SERVER ----
server = function(input, output, session) {
  
    # EVENTS ----
    
    # Re-scale network
    observeEvent(input$fit, ignoreInit=TRUE, {
       cyjShiny::fit(session, 30)
    })

    # Set colors 
    observeEvent(input$selectCondition, ignoreInit=TRUE, {
      curGraphFile <- curGraphFileReactive() 
      graphFile <- file.path(dataDir, curGraphFile)
      json <- jsonlite::fromJSON(graphFile)
      nodeIds <- json$elements$nodes$data$id
      
      attributeName <- "val"
      curCondition <- paste0(conditionPrefix, input$selectCondition)
      expressionVector <- conditions[which(conditions$gene %in% nodeIds), curCondition]
      cyjShiny::setNodeAttributes(session, attributeName=attributeName, nodes=nodeIds, values=expressionVector)
      
      attributeName <- "color"
      curColors <- paste0(colorPrefix, input$selectCondition)
      colorVector <- conditions[which(conditions$gene %in% nodeIds), curColors]
      cyjShiny::setNodeAttributes(session, attributeName=attributeName, nodes=nodeIds, values=colorVector)
      
      # DEBUG
      str(nodeIds)
      cat("COLOR: ", curColors, "\n")
      cat("COND: ", curCondition, "\n")
      str(colorVector)
    })

    # Select nodes and grab selected node vector
    observeEvent(input$selectNode,  ignoreInit=TRUE,{
        cat("sendCustomMessage, selectNodes\n")
        session$sendCustomMessage(type="selectNodes", message=list(input$selectNode))
        session$sendCustomMessage(type="getSelectedNodes", message=list())
    })

    # Grab selected node vector
    observeEvent(input$getSelectedNodes, ignoreInit=TRUE, {
        cat("sendCustomMessage, getSelectedNodes\n")
        session$sendCustomMessage(type="getSelectedNodes", message=list())
    })

    # Unselect nodes and grab selected node vector
    observeEvent(input$unselectNodes,  ignoreInit=TRUE, {
        cat("sendCustomMessage, unselectNodes\n")
        session$sendCustomMessage(type="unselectNodes", message=list())
        session$sendCustomMessage(type="getSelectedNodes", message=list())
        
        selectedNodesReactive(NULL)
    })
    
    # observeEvent(input$selectRandomNetwork, ignoreInit=TRUE, {
    #   removeGraph(session)
    #   
    #   idx <- sample(1:length(graphFiles), 1)
    #   graphFile <- graphFiles[idx]
    #   curGraphFile(graphFile)
    #   curGraphFile <- curGraphFileReactive()
    #   
    #   cat("curGraphFile: ", curGraphFile, "\n")
    # 
    #   graphFilePath <- file.path(dataDir, graphFile)
    #   addGraphFromJsonFile(session, graphFilePath)
    #   #doLayout(session, layoutName)
    # })
    
    # Replace network if user changes pathway
    observeEvent(input$selectNetwork, ignoreInit=TRUE, {
      removeGraph(session)
      
      graphFile <- input$selectNetwork
      curGraphFileReactive(graphFile)
      curGraphFile <- curGraphFileReactive() 
      
      cat("curGraphFile: ", curGraphFile, "\n")
      
      graphFilePath <- file.path(dataDir, graphFile)
      cyjShiny::addGraphFromJsonFile(session, graphFilePath)
    })
    
    # REACTIVE VARIABLES ----
    
    # selectedNodes <- eventReactive(input$selectedNodes, ignoreInit=FALSE, {
    #   selectedNodes <- input$selectedNodes
    #   cat("receiveCustomMessage, getSelectedNodes\n")
    #   cat("SELECTED NODES: ", selectedNodes, "\n")
    #   selectedNodes
    # })
    # Set default selected node vector; react to messages from cytoscape.js
    selectedNodesReactive <- reactiveVal(value=NULL)
    observeEvent(input$selectedNodes,{
      selectedNodesReactive(input$selectedNodes)
    })
    
    curGraphFileReactive <- reactiveVal(value=curGraphFile)
    
    # OUTPUTS ----
    # Save GML file from igraph
    output$saveGml <- downloadHandler(
      filename = "network.gml",
      content = function(file) {
        selectedCondition <- input$selectCondition
        curGraphFile <- curGraphFileReactive()
        g <- makeIgraphObj(selectedCondition, curGraphFile)
        
        igraph::write_graph(g, file, format="gml")
      }
    )
    
    # Save cytoscapejs JSON file from igraph
    output$saveCyjs <- downloadHandler(
      filename = "network.json",
      content = function(file) {
        selectedCondition <- input$selectCondition
        curGraphFile <- curGraphFileReactive()
        g <- makeIgraphObj(selectedCondition, curGraphFile)
        
        g_nel <- igraph::as_graphnel(g)
        json <- graphToJSON(g_nel)
        
        writeLines(json, file)
      }
    )
    
    # Save PDF file from igraph
    output$savePdf <- downloadHandler(
      filename = "network.pdf",
      content = function(file) {
        selectedCondition <- input$selectCondition
        curGraphFile <- curGraphFileReactive()
        g <- makeIgraphObj(selectedCondition, curGraphFile)
        
        tmpLayout <- matrix(c(V(g)$xPos, V(g)$yPos), ncol=2)
        
        # NOTE: Attempt to re-scale the network and its properties
        # TODO: MAKE BETTER, TEST MORE
        nodeCount <- length(V(g)$name)
        #nodeCount <- 359
        pdfSize <- max(10, 2*floor(nodeCount/10))
        
        vertexSize <- ifelse(nodeCount < 100, 200/nodeCount, 500/nodeCount)
        vertexLabelCex <- 1
        edgeWidth <- 1
        edgeArrowSize <- 1 
        
        pdf(file, height=pdfSize, width=pdfSize)
        print(plot(g, 
                   layout=tmpLayout, 
                   
                   edge.arrow.size=edgeArrowSize, 
                   edge.width=edgeWidth, 
                   vertex.size=vertexSize,
                   vertex.label.cex=vertexLabelCex,
                   
                   vertex.label.family="Helvetica", 
                   vertex.label.color = "black"
        ))
        dev.off()
      }
    )
    
    # Write selected nodes 
    output$selectedNodes <- renderText({ 
      selectedNodes <- selectedNodesReactive()
      selectedNodes
    })
    
    # Write network name
    output$networkName <- renderText({ 
      curGraphFileReactive()
    })
    
    # Make select node GUI element
    # NOTE: Only allow users to select nodes that are in the data and current graph
    output$selectNode <- renderUI({
      curGraphFile <- curGraphFileReactive()
      graphFilePath <- file.path(dataDir, curGraphFile)
      json <- fromJSON(graphFilePath)
      
      nodeIds <- json$elements$nodes$data$id
      availableNodes <- intersect(conditions$gene, nodeIds)
      
      selectInput("selectNode", "Select Node:", choices = c("Select Node"="", sort(availableNodes)))
    })
    
    # Render data table for selected nodes
    output$selectedNodeTable <- DT::renderDataTable({
      selectedNodes <- selectedNodesReactive()
      condition <- input$selectCondition
      cat("Selected Nodes: ", selectedNodes, "\n")
      
      curCondition <- paste0(conditionPrefix, condition)
      cat("COLS: ", c(genePrefix, curCondition), "\n")
      
      # Subset table unless no conditions or nodes selected
      if(curCondition %in% colnames(conditions)) {
        selectedCols <- c(genePrefix, curCondition)
      } else {
        # Initially show everything but the color columns
        selectedCols <- which(!grepl("colors", colnames(conditions)))
      }
      
      if(!is.null(selectedNodes)) {
        selectedRows <- which(conditions$gene %in% selectedNodes)
      } else {
        selectedRows <- 1:nrow(conditions)
      }
      
      tmpDat <- conditions[selectedRows, selectedCols]
      cat("ROWS: ", selectedRows, "\n")    
      cat("COLS: ", selectedCols, "\n")    
      cat("DIM: ", dim(tmpDat), "\n")
      
      if(nrow(tmpDat) > 0) {
        tmpDat <- roundDf(tmpDat, 3)
      } else {
        tmpDat <- NULL 
      }
      
      tmpDat
    }, 
    options = list(pageLength = 5, autoWidth = FALSE),
    rownames= FALSE)
    
    # Render data table for selected network
    output$selectedNetworkTable <- DT::renderDataTable({
      curGraphFile <- curGraphFileReactive()
      igraphFilename <- paste0(strsplit(curGraphFile, "\\.")[[1]][1], ".rds")
      cat("GRAPHFILE: ", igraphFilename, "\n")
      
      graphFilePath <- file.path(dataDir, igraphFilename)
      g <- readRDS(graphFilePath)
      
      tmpDat <- igraph::as_edgelist(g)
      tmpDat <- as.data.frame(tmpDat, stringsAsFactors=FALSE)
      colnames(tmpDat) <- c("source", "target")
      
      selectedNodes <- selectedNodesReactive()
      cat("SELECTED NODES: ", selectedNodes, "\n")
      
      if(!is.null(selectedNodes)) {
        selectedRows <- which((tmpDat$source %in% selectedNodes) | (tmpDat$target %in% selectedNodes))
      } else {
        selectedRows <- 1:nrow(tmpDat)
      }
      
      tmpDat[selectedRows,]
    }, 
    options = list(pageLength = 5, autoWidth = FALSE),
    rownames= FALSE)
    
    # Main network element
    output$cyjShiny <- renderCyjShiny(
      cyjShiny(graph, layoutName=layoutName, styleFile=styleFile)
    )
} 

# MAIN ----
app <- shinyApp(ui = ui, server = server)
