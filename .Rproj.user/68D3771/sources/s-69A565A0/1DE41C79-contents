
library(shinyjs)
library(shinyalert)
library(leaflet)
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
             tags$a(h1("SSES"),href="http://www.merafish.org/",target='_blank')
      ),
      column(3,style="height:65px",
             h5(textOutput("Version") ,style="padding:22px;")
      ),
      column(5),

      column(3,style="padding:14px;height:65px",

          column(6,tags$a(img(src = "FFS.jpg", height = 45, width = 145),href="https://www.gofishbc.com/",target='_blank')),
          column(6,tags$a(img(src = "logo 1.png", height = 50, width = 140),href="http://www.bluematter.ca/",target='_blank'))

      )


    ),
    hr(),

    h4("Welcome to SSES, an open-source tool for calculating landscape-scale outcomes of alternative fishery options risk, guiding fishery improvement projects, and evaluating management strategies for certification.",style = "color:black"),
    h5("MERA links a graphical questionnaire to the powerful DLMtool and MSEtool libraries to calculate population status and management performance. ",style = "color:grey"),
    h5("For further information visit the ", a("SSES website",href="https://merafish.org",target="blank"), " or check the ", a("manual.", href="https://dlmtool.github.io/DLMtool/MERA/MERA_User_Guide_5_1.html", target="_blank"),style = "color:grey"),
    h5("The SSES paper is also available ", a("here.", href="https://www.nrcresearchpress.com/doi/abs/10.1139/cjfas-2018-0168?mobileUi=0#.XddppVdKhPY", target="_blank"), style = "color:grey"),
    h5("For technical questions or bug reports please contact ", a("bluemattersci@gmail.com", href="mailto:bluemattersci@gmail.com", target="_blank"),style = "color:grey"),
    hr(),

    actionButton("recalc", "New points"),
    mainPanel(
      tabsetPanel(
        tabPanel("Order Locations", leafletOutput("map",width="80%",height="400px")),
        tabPanel("Markers", verbatimTextOutput("markers"))
      )
    ),

     fluidRow(


      column(2,
            # actionButton("recalc", "New points")
      ),

      column(8,
             #p(),
             #leafletOutput("MMap")
      ),
      column(2),

      column(1),
      column(10, textAreaInput("Log", "Log",height="120px")),

       column(12),
       hr(),

       column(6,style="height:40px"),
       column(2,style="height:40px; padding:9px",textOutput("Dependencies")),
       column(2,style="height:40px; padding:9px",textOutput("SessionID")),
       column(2,style="height:40px", h6("Open Source, GPL-2, 2019"))
    ) # end of fluid row

    ) # end of fluid page
  ) # end of server





