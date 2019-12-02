
library(shinyjs)
library(shinyalert)
library(leaflet)
obj<-readRDS(file="./Data/Landscape.rda")
js_code <<- "shinyjs.browseURL = function(url) {window.open(url,'_blank')}"
js_code <- "
shinyjs.browseURL = function(url) {
  window.open(url,'_blank');
}
"
shinyUI(

  fluidPage(

    useShinyjs(),
    extendShinyjs(text = js_code, functions = 'browseURL'),
    useShinyalert(),
    tags$head(
      tags$style(type="text/css", ".recalculating {opacity: 1.0;}"),
      tags$style(HTML("hr {border-top: 1.4px solid #E3E1DE;}
                      h4 { font-size:15px;}
                      h5 { font-size:13px;}
                      h6 { font-size:11px;}
                      textarea{ font-size:13px;}
                      button{ font-size:13px}
                      input{ font-size:13px;}
                      title{ font-size:13px;}
                      caption{ font-size:13px;font-weight:normal}
                      label{ font-size:13px;}
                      legend{ font-size:13px;}
                      link{ font-size:13px;}
                      select{ font-size:13px;}
                      menu{ font-size:13px;}
                      option{ font-size:13px;}
                      output{ font-size:13px;}
                      tbody{ font-size:13px;}
                      style{ font-size:13px;}
                      .datatables .display {margin-left: 0;}

                      .shiny-notification {
                      height: 60px;
                      width: 300px;
                      position:fixed;
                      top: calc(50% - 50px);
                      left: calc(50% - 400px);
                      }
                      #SessionID{font-size:11px;}
                      #Dependencies{font-size:11px;}


                      ")),

      tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Raleway|Cabin:400,700');

                      h2 {
                      font-family: 'Raleway', cursive;
                      font-weight: 500;
                      line-height: 1.1;
                      }

                      ")),
      tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                  type="text/javascript") # added for dynamic size iframe on website
    ),

    includeCSS("www/custom.css"),

    fluidRow(

      column(1,style="height:65px",
             tags$a(h1("SSES"),href="https://github.com/tcarruth/SSES",target='_blank')
      ),
      column(3,style="height:65px",
             h5(textOutput("Version") ,style="padding:22px;")
      ),
      column(5),

      column(3,style="padding:14px;height:65px",

          column(6,tags$a(img(src = "FFS.jpg", height = 50, width = 110),href="https://www.gofishbc.com/",target='_blank')),
          column(6,tags$a(img(src = "logo 1.png", height = 52, width = 150),href="http://www.bluematter.ca/",target='_blank'))

      )


    ),
    hr(),

    h4("Welcome to SSES, an open-source tool for calculating landscape-scale outcomes of alternative fishery options.",style = "color:black"),
    h5("For further information visit the ", a("SSES website",href="https://github.com/tcarruth/SSES",target="blank"), " consult the ", a("manual", href="https://dlmtool.github.io/DLMtool/MERA/MERA_User_Guide_5_1.html", target="_blank")," or read the ", a("paper.", href="https://www.nrcresearchpress.com/doi/abs/10.1139/cjfas-2018-0168?mobileUi=0#.XddppVdKhPY", target="_blank"), style = "color:grey"),
    h5("For technical questions or bug reports please contact ", a("bluemattersci@gmail.com", href="mailto:bluemattersci@gmail.com", target="_blank"),style = "color:grey"),
    hr(),

    fluidRow(
    column(8,
      mainPanel(
        tabsetPanel(id = "tabs",selected=2,
          tabPanel("Performance",
                   leafletOutput("Imap",width="151%",height="650px"), value=1),
          tabPanel("Management Scenarios",
                   leafletOutput("Lmap",width="151%",height="650px"), value=2),
          tabPanel("Calculation options", h5("<Calculation options>"),value=3),
          tabPanel("Edit Landscape",h5("<Add / remove lakes"),value=4)

        )
      ),
      column(12,style="padding:18px",
          selectInput("Lsel",label="Selected lakes",choices=obj@longnam,selected="",multiple=TRUE)
      ),
      column(12, style="padding:18px",
          textAreaInput("Log", "Log",height="120px"),
      )
    ),

    column(4,
      HTML("<br>"),
      hr(),

      # Edit lakes tab
      conditionalPanel("input.tabs==2",


        h4("Management Scenario to Edit:"),
        column(4,selectInput("MType",label=NULL,choices=obj@misc$Mnams,selected=obj@misc$Mnams[length(obj@misc$Mnams)])),

        column(12,style="padding-top:15px; padding-left:0px",
               hr(),
               h4("Lake Selection")
        ),

        column(8,radioButtons("LMode",label=NULL,choices=c("Add","Subtract","Intersect"),selected="Add",inline=TRUE),style="padding-top:7px"),
        column(4,
               actionButton("LAll","Select all"),
               actionButton("LClear","Clear")
        ),
        column(12,style="padding-top:10px"),

        # Selection methods by Attribute
        column(8,radioButtons("LTType",label=NULL,choices=c("Size","GDD","Dist.","Stocking","Effort","Manage."),selected="Size",inline=TRUE)),
        column(4,actionButton("LAttSel","Select by Attributes")),
        conditionalPanel("input.LTType=='Size'",
          column(12, sliderInput("LSize","Hectares",0,round(quantile(obj@lakearea,0.98)/100,0)*100,value=c(100,200),step=1,round=T))

        ),
        conditionalPanel("input.LTType=='GDD'",
          column(12, sliderInput("LGDD","Growing Degree Days",0,round(max(obj@GDD),0),value=c(1000,1200),step=5,round=T))

        ),
        conditionalPanel("input.LTType=='Dist.'",
          column(12, sliderInput("Ldist","Road kilometers",0,round(max(obj@pcxl)/1000,0)*1000,value=c(200,400),step=5,round=T)),
          column(12, selectInput("LPsel","From pop. center:",choices=c("All",obj@pcnam),selected="All",multiple=TRUE))
        ),
        conditionalPanel("input.LTType=='Stocking'",
          column(12, checkboxGroupInput("Stype","Stocking type",choices=obj@stnam,selected=obj@stnam[1],inline=TRUE)),
          column(12, sliderInput("Slev","Stocking numbers (,000s)",0,round((max(obj@lxslev)*1.05)/10000,0)*10,value=c(5,20),step=1,round=T)),

        ),
        conditionalPanel("input.LTType=='Effort'",
          column(12, sliderInput("Effort","Annual fishing effort (days)",0,round((max(obj@eff)*1.05)/100,0)*100,value=c(100,500),step=10,round=T))

        ),
        conditionalPanel("input.LTType=='Manage.'",
          column(6, checkboxGroupInput("BoatRes","Boat Restrictions",choiceNames=c("No boats","Car top","Trailer"),choiceValues=1:3,inline=TRUE)),
          column(6, checkboxGroupInput("MotorRes","Motor Restrictions",choiceNames=c("Any","Less than 10hp","Electric only"),choiceValues=1:3,inline=TRUE)),
          column(6, checkboxGroupInput("GearRes","Gear Restrictions",choiceNames=c("None","Bait ban","Barbless hooks","Fly fish. only"),choiceValues=1:4,inline=TRUE)),
          column(6, checkboxGroupInput("Take","Take Limit",choiceNames=c("No take","1 fish","4 fish","5+ fish"),choiceValues=1:4,inline=TRUE))

        ),
        column(12,style="padding-top:15px; padding-left:0px",
               hr(),
               h4("Selection Attributes")

        )


      ),

      conditionalPanel("input.tabs==1",
        h5("Info panel output")
      )

    ),


    hr(),
    fluidRow(

     column(12),
     hr(),

     column(6,style="height:40px"),
     column(2,style="height:40px; padding:9px",textOutput("Dependencies")),
     column(2,style="height:40px; padding:9px",textOutput("SessionID")),
     column(2,style="height:40px", h6("Open Source, GPL-2, 2019"))
    ) # end of fluid row
    )

    ) # end of fluid page
  ) # end of server





