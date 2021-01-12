library(shiny)
library(SSES)
library(leaflet)
library(leaflet.extras)
library(sp)
library(abind)

shinyServer(function(input, output, session) {

options(shiny.maxRequestSize=1000*1024^2)

  Version<<-packageVersion('SSES')
  output$Version<-renderText(paste0("spatial social ecological systems (v", Version, ")"))
  output$Dependencies<-renderText(paste0("Powered by: SSES v", packageVersion('SSES')))

  # Some useful things
  USERID<-Sys.getenv()[names(Sys.getenv())=="USERNAME"]
  SessionID<-paste0(USERID,"-",strsplit(as.character(Sys.time())," ")[[1]][1],"-",strsplit(as.character(Sys.time())," ")[[1]][2])
  output$SessionID<-renderText(SessionID)

  CurrentYr<-as.integer(substr(as.character(Sys.time()),1,4))
  Copyright<-"Open Source, GPL-2"

  obj<<-readRDS(file="./Data/Landscape.rda")
  #obj<-reactiveVal(readRDS(file="./Data/Landscape_test.rda"))
  manage<-reactiveValues(lxslev=obj@lxslev, lxattr=obj@lxattr)
  select<-reactiveValues(Lind=rep(F,obj@nl),MType=NULL)
  Misc  <-reactiveValues(Name=obj@Name)

  Calc<-reactiveVal(1) # Has effort been calculated
  output$Calc   <- reactive({ Calc()})
  outputOptions(output,"Calc",suspendWhenHidden=FALSE)

  NoSel<-reactiveVal(1) # Has a lake been selected?
  output$NoSel   <- reactive({ NoSel()})
  outputOptions(output,"NoSel",suspendWhenHidden=FALSE)

  output$Lmap <- renderLeaflet({
    leaflet() %>%
      addTiles()%>%
      fitBounds(lng1=as.vector(quantile(obj@lakex,0.02)),lng2=as.vector(quantile(obj@lakex,0.98)),
                lat1=as.vector(quantile(obj@lakey,0.01)),lat2=as.vector(quantile(obj@lakey,0.95)))%>%
    addControl(actionButton("LClear","Clear"),position="topright")%>%
      addControl(actionButton("LAll","All"),position="topright")%>%
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

  output$AllAtt<-renderDT({
    datatable(
      data.frame(Lake=obj@longnam, Code=obj@lakenam,Selected=select$Lind,Reg=obj@misc$regs,Size = obj@lakearea, GDD = round(obj@GDD[1,],0), Dist = round(apply(array(obj@pcsize,c(obj@npc,obj@nl))*obj@pcxl[1,,],2,sum)/sum(obj@pcsize),0),row.names=obj@longnam),
      selection = 'none', options=list(pageLength=10,lengthChange = FALSE),
      colnames=c("Lake","Code","Sel.","Reg.","Size(ha)","GDD","Ang. dist (km)"),rownames=F)%>%
      formatStyle(columns=1, valueColumns=3, color = styleEqual(c(T,F),c("red","blue")))%>%
      formatStyle(columns=2, valueColumns=3, color = styleEqual(c(T,F),c("red","blue")))%>%
      formatStyle(columns=3, valueColumns=3, color = styleEqual(c(T,F),c("red","blue")))
  })

  observeEvent(input$Lmap_draw_new_feature,{
    shape = input$Lmap_draw_new_feature
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

    selled<-obj@longnam[select$Lind]
    if(input$LMode=="Add"){
      #updateSelectInput(session=session,"Lsel",selected=c(selled,newIDs))
      UpdateSel(c(selled,newIDs))
    }else if(input$LMode=="Subtract"&length(selled)>0){
      #newselled<-selled[!selled%in%newIDs]
      #updateSelectInput(session=session,"Lsel",selected=newselled)
      UpdateSel(selled[!selled%in%newIDs])
    }else if(input$LMode=="Intersection"&length(selled)>0){
      #newselled<-selled[selled%in%newIDs]
      #updateSelectInput(session=session,"Lsel",selected=newselled)
      UpdateSel(selled[selled%in%newIDs])
    }else if(input$LMode=="Difference"&length(selled)>0){
      newselled<-selled[!selled%in%newIDs]
      newIDs<-newIDs[!newIDs%in%selled]
      UpdateSel(c(newselled,newIDs))
      #updateSelectInput(session=session,"Lsel",selected=c(newselled,newIDs))
    }

  })

  # Reactive code
  # Fpanel<-reactiveVal(0)
  #output$Fpanel <- reactive({ Fpanel()})
  #outputOptions(output,"Fpanel",suspendWhenHidden=FALSE)
  #output$Fpanelout <- renderText({ paste("Fishery",Fpanel(),"/ 19")})

  # Update UI

  Ldat<-reactive({

    newdata<-data.frame(lng=obj@lakex,lat=obj@lakey,area=as.numeric(obj@lakearea),name=obj@lakenam,longname=obj@longnam,col=rep("blue",length(obj@longnam)),stringsAsFactors = FALSE)
    #newdata$col[match(input$Lsel,obj@longnam)]<-"red"
    newdata$col[select$Lind]<-"red"
    class(newdata$area)<-"numeric"

    return(newdata)

  })

  #Pdat<-reactive({
   # newdata<-data.frame(lng=obj@pcx,lat=obj@pcy,np=as.numeric(obj@pcsize[1,]),name=obj@pcnam,col=rep("black",length(obj@pcy)))
    #newdata$col[match(input$Psel,obj@pcnam)]<-"red"
    #return(newdata)
  #})

  # Load source code
  source("./Source/Misc/Misc.R",local=TRUE)
  source("./Source/Plots/Landscape_plots.R",local=TRUE)

  output$Man_plot_S <- renderPlot({
    plotMan(Sel=T)

  })

  output$Man_plot <- renderPlot({
    plotMan()

  })

  output$Out_plot_S <- renderPlot({
    plotOut(Sel=T)

  })

  output$Out_plot <- renderPlot({
    plotOut()

  })

  output$Conv <- renderPlot({

    dummy<-sum(Calc())+sum(manage$lxslev)+sum(manage$lxattr)+sum(select$Lind)
    plotconv2(obj,totE=T)

  })

  observeEvent(input$nits,{
    if(input$nits!=100)Calc(0)
  })

  observeEvent(input$varind,{
    if(input$varind!=0.2)Calc(0)
  })

  observeEvent(input$uprat,{
    if(input$uprat!=0.05)Calc(0)
  })

  output$Out_plot_comp<-renderPlot(plotOutComp())

  observeEvent(input$LAll,{
      updateSelectInput(session=session,"Lsel",selected=obj@longnam)
      select$Lind<-rep(T,obj@nl)
      UpdateSel(obj@longnam)
  })

  observeEvent(input$LClear,{
      updateSelectInput(session=session,"Lsel",selected="")
      leafletProxy("Lmap") %>% clearShapes()#
      select$Lind<-rep(F,obj@nl)
      UpdateSel(NULL)
  }
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
    #AM(out)
    selled<-obj@longnam[select$Lind]
    if(sum(out[1]%in%selled)>0){
      tosel=selled[selled!=out[1]]
      UpdateSel(tosel)
    }else{
      UpdateSel(tosel=c(selled,out[1]))#updateSelectInput(session=session,"Lsel",selected=c(selled,out[1]))
    }

    #AM(p)

  })

  UpdateSel<-function(tosel){ # temporary function for when attributes should be shown


    if(length(tosel)>0){
      tosel<-unique(tosel)
      mind<-select$Mind
      Lind<-obj@longnam%in%tosel
      select$Lind<-Lind
      updateSelectInput(session=session,"Lsel",selected=tosel)
      # Stocking tables
      smat<-matrix(manage$lxslev[mind,Lind,],nrow=length(tosel))
      x<<-data.frame(smat)
      names(x)<<-obj@stnam
      row.names(x)<<-obj@longnam[Lind]
      output$SelAtt<-renderDT(x, selection = 'none', editable = T,options=list(pageLength=15,lengthMenu = c(5, 10, 15)))
      x2<-as.data.frame(matrix(apply(smat,2,sum)/1000,ncol=length(obj@stnam)))
      names(x2)<-obj@stnam
      row.names(x2)<-paste0("All lakes (n = ",sum(Lind),")")
      output$GrpAtt<-renderDT(x2, selection = 'none',escape=FALSE, caption='',
                              class = 'display',options = list(dom = 't'))

      # Regulation tables
      BoatRes<<-c("No boats","Car top","Trailer")
      MotorRes<<-c("Any","Less than 10hp","Elec. only")
      GearRes<<-c("None","Bait ban","Barbless","Fly only")
      TakeLim<<-c("No take","1 fish","4 fish","5+ fish")

      rmat<-cbind(BoatRes[obj@lxattr[mind,Lind,1]],
                  MotorRes[obj@lxattr[mind,Lind,3]],
                  GearRes[obj@lxattr[mind,Lind,4]],
                  TakeLim[obj@lxattr[mind,Lind,6]])


      xr<<-data.frame(rmat)
      names(xr)<<-c("Boat","Motor","Gear","Take")
      row.names(xr)<<-obj@longnam[Lind]
      output$RegAtt<-renderDT(xr, selection = 'none', editable = T,options=list(pageLength=15,lengthMenu = c(5, 10, 15)))


      #xr2<-
      #output$RegGrpAtt<-renderDTclass(xr2, selection = 'none',escape=FALSE, caption='',
      #                        class = 'display',options = list(dom = 't'))
      NoSel(0)

    }else{
      updateSelectInput(session=session,"Lsel",choices=obj@longnam,selected=NULL)
      nulltab<-data.frame(matrix(rep("-",length(obj@nst)),ncol=obj@nst))
      names(nulltab)<-obj@stnam
      row.names(nulltab)<-"No lakes selected"
      output$SelAtt<-renderDT(nulltab,selection = 'none',escape=FALSE, caption='',
                              class = 'display',options = list(dom = 't'))
      output$GrpAtt<-renderDT(nulltab,selection = 'none',escape=FALSE, caption='',
                              class = 'display',options = list(dom = 't'))
      NoSel(1)
    }

  }

  observeEvent(input$LAttSel,{

    if(input$LTType=="Region"){

      regs<-(1:length(obj@misc$region_names))[obj@misc$region_names%in%input$Region]


      if(length(regs)>0){

        newIDs<-list()

        for(i in 1:length(regs)){

          newIDs[[i]]<-obj@longnam[point.in.polygon(obj@lakex,obj@lakey,obj@misc$region_shapes[[regs[i]]][,1], obj@misc$region_shapes[[regs[i]]][,2])==1]

        }

        newIDs<-unlist(newIDs)

      }

    }else if(input$LTType=="Size"){

      Ls<-input$LSize
      if(Ls[2]==round(quantile(obj@lakearea,0.98)/100,0)*100)Ls[2]<-Inf
      newIDs=obj@longnam[obj@lakearea>=Ls[1]&obj@lakearea<=Ls[2]]

    }else if(input$LTType=="GDD"){
      newIDs=obj@longnam[obj@GDD>=input$LGDD[1]&obj@GDD<=input$LGDD[2]]

    }else if(input$LTType=="Dist."){

      Ldist<-input$Ldist
      if("All"%in%input$LPsel){
        Ld<-rep(T,obj@npc)
      }else{
        Ld<-obj@pcnam%in%input$LPsel
      }
      Ldarr<-array(Ld,c(obj@npc,obj@nl))
      LParr<-obj@pcxl[1,,]>=Ldist[1]&obj@pcxl[1,,]<=Ldist[2]
      newIDs<-obj@longnam[apply(LParr&Ldarr,2,sum)==1]

    }else if(input$LTType=="Stock."){

      starr<-array(rep(obj@stnam%in%input$Stype,each=obj@nl),c(obj@nl,obj@nst))
      mind<-select$Mind
      Sarr<-obj@lxslev[mind,,]>=(input$Slev[1]*1000) & obj@lxslev[mind,,]<=(input$Slev[2]*1000)
      newIDs<-obj@longnam[apply(starr&Sarr,1,sum)>0]

    }else if(input$LTType=="Effort"){

      mind<-select$Mind
      effsum<-apply(obj@eff[mind,1,,,],2,sum)
      newIDs=obj@longnam[effsum>=input$Effort[1] & effsum<=input$Effort[2]]

    }else{ #"Manage."

      mind<-select$Mind

      selarr<-cbind(obj@lxattr[mind,,1]%in%input$BoatRes,
                    obj@lxattr[mind,,3]%in%input$MotorRes,
                    obj@lxattr[mind,,4]%in%input$GearRes,
                    obj@lxattr[mind,,6]%in%input$TakeLim)

      newIDs=obj@longnam[apply(selarr,1,sum)>0]

    }

    selled<-obj@longnam[select$Lind]
    if(input$LMode=="Add"){
      UpdateSel(c(selled,newIDs))
      #updateSelectInput(session=session,"Lsel",selected=c(selled,newIDs))
    }else if(input$LMode=="Subtract"&length(selled)>0){

      UpdateSel(selled[!selled%in%newIDs])
      #updateSelectInput(session=session,"Lsel",selected=newselled)
    }else if(input$LMode=="Intersection"&length(selled)>0){
      UpdateSel(selled[selled%in%newIDs])
      #updateSelectInput(session=session,"Lsel",selected=newselled)
    }else if(input$LMode=="Difference"&length(selled)>0){
      newselled<-selled[!selled%in%newIDs]
      newIDs<-newIDs[!newIDs%in%selled]
      UpdateSel[c(newselled,newIDs)]
      #updateSelectInput(session=session,"Lsel",selected=c(newselled,newIDs))
    }

  })



  getcosts<-function(){
    sind<-obj@stnam%in%input$stypes
    lind<-select$Lind
    if(sum(sind)>0&sum(lind)>0){
      mind<-select$Mind
      costarr<-obj@Scosts[1,lind,sind]
      sarr<-obj@lxslev[mind,lind,sind]
      f1<-sum(sarr)/1000
      c1<-sum(sarr*costarr)/1000
      f2<-(input$sfac-1)*f1
      c2<-(input$sfac-1)*c1
      return(round(c(f1,c1,f2,c2),1))

      return()
    }else{
      return(rep("-",4))
    }
  }

  output$s1f <- renderText({

    getcosts()[1]

  })

  output$s1c <- renderText({

    getcosts()[2]

  })

  output$Name<-renderText({

    paste0(Misc$Name, ", ",obj@nl," lakes, ",obj@npc, " pop centres")

  })

  output$s2f <- renderText({

    getcosts()[3]

  })

  output$s2c <- renderText({

    getcosts()[4]

  })

  observeEvent(input$Rename,{
    obj@Name<-input$NewNam
    Misc$Name<-paste0(input$NewNam)
   }
  )

  observeEvent(input$MakeNewMan,{

    if(!(input$NewMan %in% obj@misc$Mnams)&obj@nmanage<6){
      tocopy<-select$Mind
      obj<<-AddMan(obj,tocopy=tocopy)
      manage$lxslev<-obj@lxslev
      manage$lxattr<-obj@lxattr
      select$Mind<-c(rep(F,obj@nmanage-1),T)
      newnams<-c(obj@misc$Mnams,input$NewMan)
      updateSelectInput(session=session,"MType",choices=newnams,selected=newnams[select$Mind])
      obj@misc$Mnams<<-newnams
      updateSelectInput(session,"Del",choices=newnams)
      updateSelectInput(session,"MToComp",choices=newnams)
      UpScen()
      UpdateSel(obj@longnam[input$Lind])
    }

  })

  observeEvent(input$AppRegs,{

    Lind<-select$Lind
    mind<-select$Mind

    boats<-input$EBoatRes

    if(boats>0)obj@lxattr[mind,Lind,1]<<-as.numeric(boats)
    motors<-input$EMotorRes
    if(motors>0)obj@lxattr[mind,Lind,3]<<-as.numeric(motors)
    gears<-input$EGearRes
    if(gears>0)obj@lxattr[mind,Lind,4]<<-as.numeric(gears)
    take<-input$ETakeLim
    if(take>0)obj@lxattr[mind,Lind,6]<<-as.numeric(take)

    manage$lxattr<-obj@lxattr
    UpdateSel(obj@longnam[Lind])
    Calc(0)

  })

  observeEvent(input$AppStks,{

    Lind<-select$Lind
    mind<-select$Mind

    stsel<-input$stypes
    tosel<-obj@stnam%in%stsel

    if(sum(tosel)>0){

      obj@lxslev[mind,Lind,tosel]<<-obj@lxslev[mind,Lind,tosel]*input$sfac

    }

    manage$lxslev<-obj@lxslev
    UpdateSel(obj@longnam[Lind])
    Calc(0)


  })

  observeEvent(input$SelAtt_cell_edit, {
    info = input$SelAtt_cell_edit
    i = info$row
    j = info$col
    v = info$value
    lind<-select$Lind
    mind<-select$Mind
    obj@lxslev[mind,lind,j]<<-as.numeric(v)

    Calc(0)

  })


  observeEvent(input$RegAtt_cell_edit, {
    info = input$RegAtt_cell_edit
    i = info$row
    j = info$col
    v = info$value
    if(j==1)v2<-match(v,BoatRes)
    if(j==2)v2<-match(v,MotorRes)
    if(j==3)v2<-match(v,GearRes)
    if(j==4)v2<-match(v,TakeLim)
    j2<-c(1,3,4,6)[j]
    lind<-select$Lind
    mind<-select$Mind
    obj@lxattr[mind,lind,j2]<<-as.numeric(v2)

    Calc(0)

  })

  observeEvent(input$Rename_cell_edit, {
    info = input$Rename_cell_edit
    i = info$row
    j = info$col
    v = info$value
    obj@misc$Mnams[i]<<-v
    newnams<-obj@misc$Mnams

    updateSelectInput(session=session,"MType",choices=newnams,selected=newnams[length(newnams)])
    updateSelectInput(session,"Del",choices=newnams)
    updateSelectInput(session,"MToComp",choices=newnams)


  })


  # ---- output tables -------------------------------------------------------

  output$OTeff<-renderDT({
    datatable(
      as.data.frame(cbind(obj@longnam, obj@lakenam, select$Lind, round(apply(obj@eff,c(4,1),sum),2)),stringsAsFactors=F),
      selection = 'none',  colnames=c("Lake","Code","Sel",obj@misc$Mnams),rownames=F,
      extensions='Buttons',options = list(pageLength=15,lengthChange = FALSE,dom = 'Btpl',buttons = c('csv', 'excel'))) %>%
      formatStyle(columns=1, valueColumns=3, color = styleEqual(c('TRUE','FALSE'),c("red","blue")))%>%
      formatStyle(columns=2, valueColumns=3, color = styleEqual(c('TRUE','FALSE'),c("red","blue")))%>%
      formatStyle(columns=3, valueColumns=3, color = styleEqual(c('TRUE','FALSE'),c("red","blue")))
  })

  output$OTcost<-renderDT({
    datatable(
      as.data.frame(cbind(obj@longnam, obj@lakenam, select$Lind,
                          round(matrix(apply(obj@lxslev[,,,drop=F]*array(rep(obj@Scosts[,,,drop=F],each=obj@nmanage),c(obj@nmanage,obj@nl,obj@nst)),2:1,sum),ncol=obj@nmanage)/1000
                                                                        ,2)),stringsAsFactors=F),
      selection = 'none',  colnames=c("Lake","Code","Sel",obj@misc$Mnams),rownames=F,
      extensions='Buttons',options = list(pageLength=15,lengthChange = FALSE,dom = 'Btpl',buttons = c('csv', 'excel'))) %>%
      formatStyle(columns=1, valueColumns=3, color = styleEqual(c('TRUE','FALSE'),c("red","blue")))%>%
      formatStyle(columns=2, valueColumns=3, color = styleEqual(c('TRUE','FALSE'),c("red","blue")))%>%
      formatStyle(columns=3, valueColumns=3, color = styleEqual(c('TRUE','FALSE'),c("red","blue")))
  })

  output$OTcr<-renderDT({
    datatable(
      as.data.frame(cbind(obj@longnam, obj@lakenam, select$Lind, round(apply(obj@cr,c(3,1),sum),2)),stringsAsFactors=F),
      selection = 'none',  colnames=c("Lake","Code","Sel",obj@misc$Mnams),rownames=F,
      extensions='Buttons',options = list(pageLength=15,lengthChange = FALSE,dom = 'Btpl',buttons = c('csv', 'excel'))) %>%
      formatStyle(columns=1, valueColumns=3, color = styleEqual(c('TRUE','FALSE'),c("red","blue")))%>%
      formatStyle(columns=2, valueColumns=3, color = styleEqual(c('TRUE','FALSE'),c("red","blue")))%>%
      formatStyle(columns=3, valueColumns=3, color = styleEqual(c('TRUE','FALSE'),c("red","blue")))
  })

  output$OTavs<-renderDT({
    datatable(
      as.data.frame(cbind(obj@longnam, obj@lakenam, select$Lind, round(apply(obj@avs,c(3,1),sum)/10,2)),stringsAsFactors=F),
      selection = 'none',  colnames=c("Lake","Code","Sel",obj@misc$Mnams),rownames=F,
      extensions='Buttons',options = list(pageLength=15,lengthChange = FALSE,dom = 'Btpl',buttons = c('csv', 'excel'))) %>%
      formatStyle(columns=1, valueColumns=3, color = styleEqual(c('TRUE','FALSE'),c("red","blue")))%>%
      formatStyle(columns=2, valueColumns=3, color = styleEqual(c('TRUE','FALSE'),c("red","blue")))%>%
      formatStyle(columns=3, valueColumns=3, color = styleEqual(c('TRUE','FALSE'),c("red","blue")))
  })

  observeEvent(input$MType,{

    select$Mind=match(input$MType,obj@misc$Mnams)
    UpdateSel(obj@longnam[select$input])

  })

  UpScen<-function(){
    Scenarios<-as.data.frame(obj@misc$Mnams)
    names(Scenarios)<-"Scenarios"
    row.names(Scenarios)<-1:length(obj@misc$Mnams)
    output$Rename<-renderDT(Scenarios, selection = 'none',escape=FALSE, caption='',
                            class = 'display',options = list(dom = 't'),editable=T)
  }

  observeEvent(input$tabs,{
    UpScen()

  })

  observeEvent(input$DelMan,{

    # delete a management option
    todel<-match(input$Del,obj@misc$Mnams)
    obj<<-DelMan(obj,todel)
    obj@misc$Mnams<<- obj@misc$Mnams[obj@misc$Mnams!=input$Del]
    newnams<-obj@misc$Mnams
    updateSelectInput(session=session,"MType",choices=newnams,selected=newnams[length(newnams)])
    updateSelectInput(session,"Del",choices=newnams)
    updateSelectInput(session,"MToComp",choices=newnams)
    UpScen()

  })

  observeEvent(input$Calc,{

    Calc(0)
    nits<-input$nits
    varind<-input$varind
    uprat<-input$uprat

    withProgress(message = "Solving for IFD of Effort", value = 0, {

      obj<<-calceff(obj,nits=nits,couch=T,startE=T,varind=varind,uprat=uprat,quiet=F,shiny=T)

    })

    Calc(1)


  })

  #output$markers <- renderPrint({print(dataUpload())})

  #observeEvent(input$Mode,{

   # AM(paste0("Mode selected: ", input$Mode))

  #})

  # == File I/O ==========================================================================


  output$Save<- downloadHandler(

    filename = function()paste0(namconv(obj@Name),".SSES"),

    content=function(file){

      saveRDS(obj,file)

    }

  )


  observeEvent(input$Load,{

    filey<-input$Load

    tryCatch({
      temp<<-readRDS(file=filey$datapath)
      if (class(temp) !='Landscape') stop()
      obj<<-temp
      UpScen()
      UpdateSel(NULL)
      manage$lxslev<-obj@lxslev
      manage$lxattr<-obj@lxattr
      select$Lind<-rep(F,obj@nl)
    },
    error = function(e){
      shinyalert("File read error", "This does not appear to be an SSES Landscape object", type = "error")
      return(0)
    }
    )

  })

  # End of file I/O ===================================================================================


  observeEvent(input$Defaults,{

    updateSliderInput(session,"nits",value=100)
    updateSliderInput(session,"varind",value=0.2)
    updateSliderInput(session,"uprat",value=0.05)
  })


  # ===== Automatic Reports ==================================================================================================

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

  observeEvent(input$Lsel, {
    select$Lind<-obj@longnam%in%input$Lsel
    UpdateSel(input$Lsel)
  })
  # ======================= Explanatory Plots ===================================

  output$Land_plot <- renderPlot(plotLand())


})
