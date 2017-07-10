## fitxer "mare" el que te la Sandra amb colors, comentaris, etc.
# mare <- "./dat/original.xlsx"
## fitxer "nou", del qual s'han d'eliminar el registres que son a "mare"
# nou <- "./dat/DataProblem.xlsx"

server <- function(input,output){

  output$tablenames1list <- renderUI({
    inFile <- input$file1
    if (is.null(inFile)) return(invisible(NULL))
    chn <- try(XLConnect::loadWorkbook(inFile$datapath),silent=TRUE)
    if (inherits(chn,"try-error")) return(invisible(NULL))
    tablenames <- try(XLConnect::getSheets(chn),silent=TRUE)
    if (inherits(tablenames,"try-error")) return(invisible(NULL))
    selectInput("tablenames1", "Tria el full", tablenames)
  })
  
  output$tablenames2list <- renderUI({
    inFile <- input$file2
    if (is.null(inFile)) return(invisible(NULL))
    chn <- try(XLConnect::loadWorkbook(inFile$datapath),silent=TRUE)
    if (inherits(chn,"try-error")) return(invisible(NULL))
    tablenames <- try(XLConnect::getSheets(chn),silent=TRUE)
    if (inherits(tablenames,"try-error")) return(invisible(NULL))
    selectInput("tablenames2", "Tria el full", tablenames)
  })  
    
  mare <- reactive({
    ## llegeixo les dades
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    dades <- read.xlsx(file=inFile$datapath, sheetName=input$tablenames1, encoding="UTF-8", stringsAsFactors=FALSE)
    ## arreglo marrons
    dades$problem <- gsub("supensión", "suspensión", dades$problem)
    ## creo els "id" comuns
    dades$id <- with(dades, paste(pacid, centreid, paccentreid, trim(type), trim(problem), sep=""))    
    ## return
    dades
  })
  
  nou <- reactive({
    ## llegeixo les dades
    inFile <- input$file2
    if (is.null(inFile)) return(NULL)
    dades <- read.xlsx(file=inFile$datapath, sheetName=input$tablenames2, encoding="UTF-8", stringsAsFactors=FALSE)
    ## arreglo marrons
    dades$problem <- gsub("Se sabe el da pero", "Se sabe el día pero", dades$problem)
    dades$problem <- gsub(" das desde 1r", " días desde 1r", dades$problem)
    dades$problem <- gsub(". Das = ", ". Días = ", dades$problem)
    dades$problem <- gsub("supensin", "suspensión", dades$problem)
    dades$problem <- gsub(" das ", " días ", dades$problem)
    dades$problem <- gsub("suspensin", "suspensión", dades$problem)    
    ## creo els "id" comuns
    dades$id <- with(dades, paste(pacid, centreid, paccentreid, trim(type), trim(problem), sep=""))
    ## return
    dades
  })

  output$down <- downloadHandler(
    filename = function() "newqueries.xlsx",
    content = function(ff) {
      mare <- mare()
      nou <- nou()
      ## selecciono els nous
      newqueries <- subset(nou, id%nin%mare$id)
      newqueries <- remove.vars(newqueries, "id")
      ## salvo el fitxer
      write.xlsx(newqueries, file=ff, row.names=FALSE)
    }
  )

}
