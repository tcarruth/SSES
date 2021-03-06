
library(shinyjs)
library(shinyalert)
library(leaflet)
library(DT)
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
      column(3,style="height:55px;padding-top:13px",
             h5(textOutput("Version") ,style="padding:10px;")
      ),
      column(5),

      column(3,style="padding-top:15px;height:55px",
          column(3),
          column(4,tags$a(img(src = "FFS.jpg", height = 50, width = 110),href="https://www.gofishbc.com/",target='_blank')),
          column(4,tags$a(img(src = "logo 1.jpg", height = 50, width = 93),href="http://www.bluematter.ca/",target='_blank'))

      )


    ),
    hr(),

    h4("Welcome to SSES, an open-source tool for calculating landscape-scale outcomes of alternative fishery managament policies.",style = "color:black"),
    h5("For further information visit the ", a("SSES website",href="https://github.com/tcarruth/SSES",target="blank"), " consult the ", a("manual", href="https://blue-matter.github.io/openMSE/SSES-User-Guide.html", target="_blank")," or read the ", a("paper.", href="https://www.nrcresearchpress.com/doi/abs/10.1139/cjfas-2018-0168?mobileUi=0#.XddppVdKhPY", target="_blank"), style = "color:grey"),
    h5("For technical questions or bug reports please contact ", a("tom@bluematterscience.com", href="mailto:tom@bluematterscience.com", target="_blank"),style = "color:grey"),
    column(12,hr()),
    #column(7,
    #     ),
    #column(5),

    fluidRow(
    column(7,

           column(8,textOutput('Name')),

                 # h5(paste0(output.Name,", n lakes = ",obj@nl,", n pop. centres = ",obj@npc))),
           column(2,h5("Management Scenario:")),
           column(2,
                  selectInput("MType",label=NULL,choices=obj@misc$Mnams,selected=obj@misc$Mnams[length(obj@misc$Mnams)])
          ),
      mainPanel(
          leafletOutput("Lmap",width="152%",height="705px")#, value=2)
      ),

      column(12, style="padding:18px",
         div(style='height:125px; overflow-y: scroll; width: 100%',
            selectInput("Lsel",label="Selected Lakes:",choices=obj@longnam,selected="",multiple=TRUE)
             #textAreaInput("Log", "Log",height="125px"),
         )
      )
    ),

    column(5,style="padding:0px;height:770px",

      tabsetPanel(id = "tabs",selected=1,

       tabPanel("Landscape Info",
            tabsetPanel(id = "Landtab",selected=1,
                tabPanel(h5("Lake attributes"),
                         plotOutput("Land_plot",height=240),
                         column(12,dataTableOutput("AllAtt")),
                value=1),
                tabPanel(h5("Management - all lakes",style = "color:blue"),
                         plotOutput("Man_plot",height=650),
                value=2),
                tabPanel(h5("Management - selected lakes",style = "color:red"),

                plotOutput("Man_plot_S",height=650),
                value=3)
            ),

          value=1
       ),


       tabPanel("Ext. Selection",
                column(12,h5("Selection Mode:"),style='padding-top:15px'),
                column(12,radioButtons("LMode",label=NULL,choices=c("Add","Subtract","Intersection","Difference"),selected="Add",inline=TRUE),style="padding-top:0px"),

                column(12,hr(),style="padding:0px"),

                column(12,h5("Select by attributes:")),
                # Selection methods by Attribute
                column(9,radioButtons("LTType",label=NULL,choices=c("Region","Size","GDD","Dist.","Stock.","Eff.","Manage."),selected="Region",inline=TRUE)),
                column(12,style="height:280px; padding-left:40px",
                       conditionalPanel("input.LTType=='Size'",
                                        column(12, sliderInput("LSize","Hectares",0,round(quantile(obj@lakearea,0.98)/100,0)*100,value=c(100,200),step=1,round=T))

                       ),
                       conditionalPanel("input.LTType=='GDD'",
                                        column(12, sliderInput("LGDD","Growing Degree Days",0,round(max(obj@GDD)*1.02,0),value=c(1000,1200),step=5,round=T))

                       ),
                       conditionalPanel("input.LTType=='Dist.'",
                                        column(12, sliderInput("Ldist","Road kilometers",0,round(max(obj@pcxl)/1000,0)*1000,value=c(200,400),step=5,round=T)),
                                        column(12, selectInput("LPsel","From pop. center:",choices=c("All",obj@pcnam),selected="All",multiple=TRUE))
                       ),
                       conditionalPanel("input.LTType=='Stock.'",
                                        column(12, checkboxGroupInput("Stype","Stocking type",choices=obj@stnam,selected=obj@stnam[1],inline=TRUE)),
                                        column(12, sliderInput("Slev","Stocking numbers (,000s)",0,round((max(obj@lxslev)*1.05)/10000,0)*10,value=c(5,20),step=1,round=T))

                       ),
                       conditionalPanel("input.LTType=='Effort'",
                                        column(12, sliderInput("Effort","Annual fishing effort (days)",0,round((max(apply(obj@eff[1,1,,,],2,sum))*1.01)/100)*100,value=c(100,500),step=10,round=T))

                       ),
                       conditionalPanel("input.LTType=='Manage.'",
                                        column(6, checkboxGroupInput("BoatRes","Boat Restrictions",choiceNames=BoatRes,choiceValues=1:length(BoatRes),inline=TRUE)),
                                        column(6, checkboxGroupInput("MotorRes","Motor Restrictions",choiceNames=MotorRes,choiceValues=1:length(MotorRes),inline=TRUE)),
                                        column(6, checkboxGroupInput("GearRes","Gear Restrictions",choiceNames=GearRes,choiceValues=1:length(GearRes),inline=TRUE)),
                                        column(6, checkboxGroupInput("TakeLim","Take Limit",choiceNames=TakeLim,choiceValues=1:length(TakeLim),inline=TRUE))
                       ),
                       conditionalPanel("input.LTType=='Region'",
                                        column(12, checkboxGroupInput("Region",label=NULL,choices=obj@misc$region_names,inline=TRUE))
                       ),
                       column(8),
                       column(12,actionButton("LAttSel","Select",style="color:red"),style="padding-top:5px; padding-left:15px")


                ), # end of fixed height box
                #column(12,style="padding-top:10px; padding-left:15px",
                       #actionButton("LAll","Select all lakes",style="color:red"),
                       #actionButton("LClear","Clear all lakes",style="color:blue")
                #),
                #column(12,style="padding-top:5px; padding-left:15px",hr(),
                 #      h4("Selected Lakes:")),
               # div(style='height:300px; overflow-y: scroll; width: 95%',
                #    column(12,style="padding:18px",
                          # selectInput("Lsel",label=NULL,choices=obj@longnam,selected="",multiple=TRUE)
                 #   )
                #)
                value=2),



       tabPanel("Regulations",
            conditionalPanel("output.NoSel==0",
                column(12,style="padding-top:15px; padding-left:0px",

                       column(2,style="padding-top:8px",
                              h5("Edit by:")
                       ),
                       column(10,style="padding-top:15px",
                              radioButtons("Rcontrol",label=NULL,choices=c("Individual lakes","Selected lakes"),inline=T)
                       )
                ),
                column(12, hr(),style="padding-left:0px"),
                conditionalPanel("input.Rcontrol=='Selected lakes'",
                  column(12,style="padding-top:0px; padding-left:20px",
                     column(6, radioButtons("EBoatRes","Boat Restrictions",choiceNames=c("NC",BoatRes),choiceValues=0:length(BoatRes),inline=TRUE)),
                     column(6, radioButtons("EMotorRes","Motor Restrictions",choiceNames=c("NC",MotorRes),choiceValues=0:length(MotorRes),inline=TRUE)),
                     column(6, radioButtons("EGearRes","Gear Restrictions",choiceNames=c("NC",GearRes),choiceValues=0:length(GearRes),inline=TRUE)),
                     column(6, radioButtons("ETakeLim","Take Limit",choiceNames=c("NC",TakeLim),choiceValues=0:length(TakeLim),inline=TRUE)),
                     #column(12,actionButton("Deselect","No regulation changes",style="color:blue"))
                     #column(12,
                    #        DTOutput("RegGrpAtt")
                     #),
                     column(4,style="padding-top:0px",
                       conditionalPanel("!(input.EBoatRes==0&input.EMotorRes==0&input.EGearRes==0&input.ETakeLim==0)",
                            actionButton("AppRegs","Apply Changes to Selection",style="color:red")
                       )

                     )
                  )
                ),
                column(12,style="padding-left:20px;padding-top:10px",
                  conditionalPanel("input.Rcontrol=='Individual lakes'",

                     column(12,
                            DTOutput("RegAtt")
                     )

                  )
                )
            ),
            conditionalPanel("output.NoSel==1",
               column(12,
                      h5("< No lakes selected >",style="color:grey"),
                      style="padding:25px")

            ),
            value=3),

      tabPanel("Stocking",
          conditionalPanel("output.NoSel==0",
               column(12,style="padding-top:15px; padding-left:0px",

                      column(2,style="padding-top:8px",
                             h5("Edit by:")
                      ),
                      column(10,style="padding-top:15px",
                             radioButtons("Scontrol",label=NULL,choices=c("Individual lakes","Selected lakes"),inline=T)
                      )
               ),
               column(12, hr(),style="padding-left:0px"),
               column(12,style="padding-top:10px;padding-left:20px",
                      conditionalPanel("input.Scontrol=='Individual lakes'",
                                       column(12,
                                              DTOutput("SelAtt")
                                       )
                      ),
                      conditionalPanel("input.Scontrol=='Selected lakes'",
                                       column(12,checkboxGroupInput("stypes","Stocking type(s) to edit:",choices=obj@stnam,selected=obj@stnam,inline=T)),
                                       column(12,sliderInput("sfac","Stocking factor (multiplier of current level):",0,5,1,step=0.02)),
                                       column(3,h5("Current No. fish (k):")),
                                       column(3,h5("Current cost ($k)")),
                                       column(3,h5("Proposed fish inc (k):")),
                                       column(3,h5("Proposed cost inc ($k):")),
                                       column(3,textOutput("s1f")),
                                       column(3,textOutput("s1c")),
                                       column(3,textOutput("s2f")),
                                       column(3,textOutput("s2c")),
                                       column(12,
                                              DTOutput("GrpAtt")
                                       ),
                                       column(8,style="padding-top:15px",

                                              actionButton("AppStks","Apply Changes to Selected Lakes",style="color:red")

                                       )
                      )
               )
          ),
          conditionalPanel("output.NoSel==1",
               column(12,
                      h5("< No lakes selected >",style="color:grey"),
                      style="padding:25px")
          ),
          value=4),

      tabPanel("Outcomes",
               conditionalPanel("output.Calc==1",
                    tabsetPanel(id = "Outtab",selected=1,

                                tabPanel(h5("All lakes",style = "color:blue"),
                                         plotOutput("Out_plot",height=550),
                                         value=1),
                                tabPanel(h5("Selected lakes",style = "color:red"),
                                         plotOutput("Out_plot_S",height=550),
                                         value=2),
                                tabPanel(h5("Comparisons"),
                                         column(12,style='padding-top:10px',
                                         selectInput("MToComp","Reference scenario",choices=obj@misc$Mnams,width=400),
                                         plotOutput("Out_plot_comp",height=550)),
                                         value=3),
                                tabPanel(h5("Effort (d)"),column(12, style='padding-top:10px',DTOutput("OTeff"))),
                                tabPanel(h5("Cost ($k)"),column(12,style='padding-top:10px', DTOutput("OTcost"))),
                                tabPanel(h5("CR (f/d)"),column(12,style='padding-top:10px',DTOutput("OTcr"))),
                                tabPanel(h5("Av Size (cm)"),column(12,style='padding-top:10px',DTOutput("OTavs"))),
                                tabPanel(h5("Convergence"),
                                         plotOutput("Conv",height=400,width=500),
                                         value=4)
                    )


               ),

               conditionalPanel("output.Calc==0",
                                column(12,
                                       h5("< Management scenarios have changed, effort requires recalculation >",style="color:grey"),
                                       style="padding:25px")
               ),

               value=5
      ),


      tabPanel("Options",

               column(12, h5("File"),style="padding-top:10px;padding-bottom:0px"),
               column(12,
                  column(6,style="padding-top:10px; padding-bottom:0px",
                      fileInput("Load","Load  (.SSES)")
                  ),
                  column(6,style="padding-bottom:0px",
                      h5("Save",style="font-weight:bold"),
                       downloadButton("Save","")
                  )
               ),
               column(12, hr(),style="height=5px"),
               column(12,h5("Rename landscape")),
               column(5,
                      actionButton("Rename","Rename landscape:")
               ),
               column(7,
                      textInput("NewNam",label=NULL,"My Landscape")

               ),
               column(12, hr(),style="padding:0px"),
               column(12,h5("Organize Management Scenarios")),
               column(6,
                      actionButton("MakeNewMan","Copy currently selected & name it:")
               ),
               column(6,
                      textInput("NewMan",label=NULL,"3: Alternative")

               ),
               column(5,
                      actionButton("DelMan","Delete:")
               ),
               column(7,
                      selectInput("Del",label=NULL,choices=obj@misc$Mnams)

               ),
               #column(12, hr(),style="padding:0px"),
               column(5,h5("Rename:"),style="padding-top:20px"),
               column(7,DTOutput("Rename")),

               column(12, hr()),
               h5("Calculation options"),
               column(3,sliderInput('nits',"Number of iterations",10,1000,100,step=10)),
               column(3,sliderInput('varind',"Slope of logit choice",0.05,0.5,0.2,step=0.01)),
               column(3,sliderInput('uprat',"Effort update fraction",0.01,0.5,0.05,step=0.01)),
               column(3,actionButton('Defaults',"Reset to Defaults"),style="padding-top:20px"),
               value=6)

      )

    ),

    column(5,style='padding:0px',
           column(12,hr(),style="padding:0px"),
           column(3),
           column(6,style="padding-top:7px",
                conditionalPanel("output.Calc==0",
                  actionButton("Calc","RE-CALCULATE EFFORT",style='color:green;height:60px;width:400px;border-color:green')
                )
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





