library(shiny)
library(SSES)
library(leaflet)

shinyServer(function(input, output, session) {

options(shiny.maxRequestSize=1000*1024^2)


  # Reactive code
  # Fpanel<-reactiveVal(0)
  #output$Fpanel <- reactive({ Fpanel()})
  #outputOptions(output,"Fpanel",suspendWhenHidden=FALSE)
  #output$Fpanelout <- renderText({ paste("Fishery",Fpanel(),"/ 19")})

  # Update UI
  output$Version<-renderText(paste0("social ecological systems model    (v", Version, ")")) #"method evaluation and risk assessment    (MSC-DLMtool App v4.1.7)"
  output$Dependencies<-renderText(paste0("Powered by: SSES v", packageVersion('SSES'))) #"method evaluation and risk assessment    (MSC-DLMtool App v4.1.7)"

  # Some useful things
  USERID<-Sys.getenv()[names(Sys.getenv())=="USERNAME"]
  SessionID<-paste0(USERID,"-",strsplit(as.character(Sys.time())," ")[[1]][1],"-",strsplit(as.character(Sys.time())," ")[[1]][2])
  output$SessionID<-renderText(SessionID)

  CurrentYr<-as.integer(substr(as.character(Sys.time()),1,4))
  Copyright<-"Open Source, GPL-2"

 # observe all changes go controls or tabs
  #observeEvent(sapply(inputtabs, function(x) input[[x]]),{
    #UpPanelState()
  #})

  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(lng=c(100,170),lat=c(100,170))
  })
  #output$markers <- renderPrint({print(dataUpload())})

  #observeEvent(input$Mode,{

   # AM(paste0("Mode selected: ", input$Mode))

  #})

  # == File I/O ==========================================================================

  # Plan save
  #output$Save_Plan<- downloadHandler(

   # filename = function()paste0(namconv(input$Name),".Plan"),

    #content=function(file){

     # doprogress("Saving Evaluation data")
      #saveRDS(list(MSEobj=MSEobj,MSEobj_reb=MSEobj_reb),file)

    #}

  #)

  # Plan load
  #observeEvent(input$Load_Plan,{

   # filey<-input$Load_Plan

    #tryCatch({
     # listy<<-readRDS(file=filey$datapath)
      #if (class(listy[[1]]) !='MSE') stop()
    #},
    #error = function(e){
    #  shinyalert("File read error", "This does not appear to be a MERA evaluation object", type = "error")
    #  return(0)
    #}
    #)

    #cond<-class(listy[[1]])=="MSE" & class(listy[[2]])=="MSE" & listy[[1]]@nMPs>1

    #if(cond){
    #  MSEobj<<-listy[[1]]
    #  MSEobj_reb<<-listy[[2]]
    #  updateTabsetPanel(session,"Res_Tab",selected="1")
    #}else{
    #  shinyalert("File read error", "This does not appear to be a MERA planning object", type = "error")
    #}

  #})

  # End of file I/O ===================================================================================


#############################################################################################################################################################################
### Calculation functions
#############################################################################################################################################################################

  # OM conditioning ============================

  # ===== Reports ==================================================================================================

  # OM questionnaire report
  #output$Build_OM <- downloadHandler(


   # filename =  function(){  paste0(namconv(input$Name),"_Questionnaire_Report.html") },
    #content = function(file) {
     # withProgress(message = "Building questionnaire report", value = 0, {

    #  src <- normalizePath('Source/Markdown/OMRep.Rmd')
     # owd <- setwd(tempdir())
    #  on.exit(setwd(owd))
     # file.copy(src, 'OMRep.Rmd', overwrite = TRUE)

    #  library(rmarkdown)
    #  params <- list(test = input$Name)

    # knitr::knit_meta(class=NULL, clean = TRUE)
    #  output<-render(input="OMRep.Rmd",output_format="html_document", params = params)
    #  file.copy(output, file)

     # }) # end of progress meter
    #}
  #)


  # ======================= Explanatory Plots ===================================
  # Scheme
  fcol = rgb(0.4,0.8,0.95)#"#0299f"
  fcol2 = "dark grey"
  icol <- "dodgerblue4"
  maxcol="cadetblue"
  mincol="dark grey"

  # Fishery
  #output$plotM <- renderPlot(plotM())


})
