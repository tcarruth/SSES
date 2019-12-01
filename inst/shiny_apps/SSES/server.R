library(shiny)
library(SSES)
library(leaflet)
library(leaflet.extras)
library(sp)

shinyServer(function(input, output, session) {

options(shiny.maxRequestSize=1000*1024^2)

  #data_of_click <- reactiveValues(clickedMarker = list())

  output$Lmap <- renderLeaflet({
    leaflet() %>%
      addTiles()%>%
      fitBounds(lng1=as.vector(quantile(obj@lakex,0.02)),lng2=as.vector(quantile(obj@lakex,0.98)),
                lat1=as.vector(quantile(obj@lakey,0.01)),lat2=as.vector(quantile(obj@lakey,0.95))) %>%
      addDrawToolbar(
        position="topright",
        targetGroup='Selected',
        markerOptions = FALSE,
        polylineOptions= FALSE,
        circleMarkerOptions = FALSE,
        rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                            ,color = 'red'
                                                                            ,weight = 3)),
        polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                        ,color = 'red'
                                                                        ,weight = 3)),
        circleOptions = FALSE,
        editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions()))
  })

  observeEvent(input$Lmap_draw_new_feature,{
    #Only add new layers for bounded locations
    #saveRDS(shape, file="C:/temp/shape.rda")
    #saveRDS(obj, file="C:/temp/obj.rda")
    shape = input$Lmap_draw_new_feature
    #saveRDS(shape, file="C:/temp/shape.rda")
    if("circle"%in%unlist(shape)){
      temp<-unlist(shape)
      xy<-as.numeric(as.character(temp[grepl("coordinates",names(temp))]))
      n = 200 # nr of pts
      r =(as.numeric(as.character(temp[grepl("radius",names(temp))]))/(6371000*4.5))*360
      pts = seq(0, 2 * pi, length.out = 200)
      poly=data.frame(Lon=xy[1] + r * sin(pts),Lat=xy[2] + r * cos(pts))

    }else{
      temp<-unlist(shape)
      poly<-as.data.frame(t(matrix(as.numeric(as.character(temp[grepl("coordinates",names(temp))])),nrow=2,byrow=F)))
      names(poly)<-c("Lon","Lat")

    }


    newIDs<-obj@longnam[point.in.polygon(obj@lakex,obj@lakey,poly$Lon, poly$Lat)==1]

    selled<-input$Lsel
    if(input$LMode=="Add"){
      updateSelectInput(session=session,"Lsel",selected=c(selled,newIDs))
    }else if(input$LMode=="Subtract"){
      newselled<-selled[!selled%in%newIDs]
      updateSelectInput(session=session,"Lsel",selected=newselled)
    }else{
      newselled<-selled[!selled%in%newIDs]
      newIDs<-newIDs[!newIDs%in%selled]
      updateSelectInput(session=session,"Lsel",selected=c(newselled,newIDs))
    }

  })

  # Reactive code
  # Fpanel<-reactiveVal(0)
  #output$Fpanel <- reactive({ Fpanel()})
  #outputOptions(output,"Fpanel",suspendWhenHidden=FALSE)
  #output$Fpanelout <- renderText({ paste("Fishery",Fpanel(),"/ 19")})

  # Update UI
  Version<<-"1.1.1"
  output$Version<-renderText(paste0("spatial social ecological systems (v", Version, ")"))
  output$Dependencies<-renderText(paste0("Powered by: SSES v", packageVersion('SSES')))

  # Some useful things
  USERID<-Sys.getenv()[names(Sys.getenv())=="USERNAME"]
  SessionID<-paste0(USERID,"-",strsplit(as.character(Sys.time())," ")[[1]][1],"-",strsplit(as.character(Sys.time())," ")[[1]][2])
  output$SessionID<-renderText(SessionID)

  CurrentYr<-as.integer(substr(as.character(Sys.time()),1,4))
  Copyright<-"Open Source, GPL-2"

  # Load default landscape object
  #Ldat<<-data.frame(lng=obj@lakex,lat=obj@lakey,area=obj@lakearea,name=obj@lakenam,longname=obj@longnam,sel=rep(FALSE,obj@nl))

  Ldat<-reactive({

    newdata<-data.frame(lng=obj@lakex,lat=obj@lakey,area=as.numeric(obj@lakearea),name=obj@lakenam,longname=obj@longnam,col=rep("blue",length(obj@longnam)),stringsAsFactors = FALSE)
    newdata$col[match(input$Lsel,obj@longnam)]<-"red"
    class(newdata$area)<-"numeric"

    #saveRDS(newdata,"C:/temp/newdata")
    return(newdata)
  })

  Pdat<-reactive({
    newdata<-data.frame(lng=obj@pcx,lat=obj@pcy,np=as.numeric(obj@pcsize[1,]),name=obj@pcnam,col=rep("black",length(obj@pcy)))
    #newdata$col[match(input$Psel,obj@pcnam)]<-"red"
    return(newdata)
  })

  # Load source code
  source("./Source/Misc/Misc.R",local=TRUE)
  obj<-readRDS(file="./Data/Landscape.rda")

  observeEvent(input$LAll,
      updateSelectInput(session=session,"Lsel",selected=obj@longnam)
  )

  observeEvent(input$LClear,
      updateSelectInput(session=session,"Lsel",selected="")
  )

  #observe all changes go controls or tabs
  #observeEvent(sapply(inputtabs, function(x) input[[x]]),{
    #UpPanelState()
  #})

  circfunc<-function(x){
    1.5+x^0.28
  }
  observe({

      leafletProxy('Lmap')%>%
        addCircleMarkers(data=Ldat(),lng=~lng,lat=~lat,label=~longname,radius=circfunc(obj@lakearea),stroke=FALSE,color=~col,fillOpacity=0.45,layerId=~longname)#%>%
          #addCircleMarkers(data=Pdat(),lng=~lng,lat=~lat,label=~name,radius=~np,color=~col,stroke=FALSE,fillOpacity=0.45,layerId=~name)
  })

  observeEvent(input$Lmap_marker_click, {
    out <- input$Lmap_marker_click
    updateTextAreaInput(session, "Log", value=out[1])
    selled<-input$Lsel
    if(sum(out[1]%in%selled)>0){
      updateSelectInput(session=session,"Lsel",selected=selled[selled!=out[1]])
    }else{
      updateSelectInput(session=session,"Lsel",selected=c(selled,out[1]))
    }
    saveRDS(selled,file="C:/temp/selled.rda")
    saveRDS(out,file="C:/temp/out.rda")
    #AM(p)

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
