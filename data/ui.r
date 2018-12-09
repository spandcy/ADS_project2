packages.used=c("shiny","ggmap","leaflet","dplyr","shinyBS","plotly","extrafont","grDevices","shinyjs")

# check packages that need to be installed.
packages.needed=setdiff(packages.used,intersect(installed.packages()[,1],packages.used))

# install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}


library(shiny)
library(ggmap)
library(leaflet)
library(dplyr)
library(shinyBS)
library(plotly)
library(extrafont)
library(grDevices)
library(shinyjs)
library(shinythemes)



#Read in Files

college =  read.csv("data/college_top.csv")

### UI
ui <- navbarPage(theme=shinytheme("lumen"),
  includeCSS("style.css"),
    tabPanel(
      title="Maps", icon=icon("map")),
    
  
   ################ Comparison ##############
    tabPanel(
      title="Comparison",icon=icon("balance-scale"),
      wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 600px",
                #tags$hr(style="border-color: #6088B1;"),
                # h1("Side-by-Side Two School Comparison",align= "center",style = "color: #333333; font-family: Times;
                #    font-size:50pt"),
                #tags$hr(style="border-color: #6088B1;"),
                
                #### select schools ###
                br(),
                
                
                absolutePanel(id = "select schools",left="2%",width="90%",fixed=T,top="13%",
                              fluidRow(
                                         column(6,selectInput("input1",label="Select school A",choices=college$INSTNM)),
                                         #column(1),
                                         column(6,selectInput("input2",label="Select school B",choices=college$INSTNM))
                              )),
                # fluidRow(splitLayout(cellWidths = c("50%","50%"), 
                #                      strong(column(width=5,"Select School A "))
                #                      ,
                #                      strong(column(width=5,"Select School B "))
                # )
                # ),

                br(),br(),
                
                fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"),
                                                    uiOutput("ui.1"),
                                                    uiOutput("ui.2")
                )
                ),
                br(),br(),
                # fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"),
                #                                     imageOutput("logo1",height = "400", width = "400"),imageOutput("logo2",height = "400", width = "400")
                # )),
                # 
                # 
                # br(),
                # 
                
                
                # ==== Title in Orange
                fluidRow(align="center",
                         style="opacity:0.9; background-color: white ;margin-top: 0px; width: 100%;",
                         column(6,offset=3,
                                br(),hr(style="color:#808080"),
                                helpText( strong("Basic Information" , style="color:#6088B1 ; font-family: 'times'; font-size:30pt ; font-type:bold" )) ,
                                hr(style="color:#808080")
                         )),
                
                # === Some text to explain the Figure:
                
                br(),
                # === display instnm
                # fluidRow(align = "center",splitLayout(cellWidths = c("50%","50%"),
                #                                       fluidRow(strong(column(width=2,offset=1,"Institution Name: ")),textOutput("instnm1")),
                #                                       fluidRow(strong(column(width=2,offset=1,"Institution Name: ")),textOutput("instnm2")))
                # ),br(),
                # === display city
                fluidRow(align = "justify",splitLayout(cellWidths = c("50%","50%"),
                                                       fluidRow(strong(column(width=2,offset=1,"City: ")),textOutput("city1")),
                                                       fluidRow(strong(column(width=2,offset=1,"City: ")),textOutput("city2")))
                ),br(),
                
                # === display clevel
                fluidRow(align = "justify",splitLayout(cellWidths = c("50%","50%"),
                                                       fluidRow(strong(column(width=4,offset=1,"Level of Institution: ")),textOutput("iclevel1")),
                                                       fluidRow(strong(column(width=4,offset=1,"Level of Institution: ")),textOutput("iclevel2")))
                ),br(),
                # === display control
                
                fluidRow(align="justify",splitLayout(cellWidths = c("50%","50%"),
                                                     fluidRow(strong(column(width=4,offset=1,"Control of Institution: ")),textOutput("control1")),
                                                     fluidRow(strong(column(width=4,offset=1,"Control of Institution: ")),textOutput("control2")))
                ),br(),
                # === display highest degree
                
                fluidRow(align="justify",splitLayout(cellWidths = c("50%","50%"),
                                                     fluidRow(strong(column(width=3,offset=1,"Highest Degree: ")),textOutput("highdeg1")),
                                                     fluidRow(strong(column(width=3,offset=1,"Highest Degree: ")),textOutput("highdeg2")))
                ),br(),
                # === display crime rate
                fluidRow(align = "justify",splitLayout(cellWidths = c("50%","50%"),
                                                       fluidRow(strong(column(width=2,offset=1,"Crime rate: ")),textOutput("cr1")),
                                                       fluidRow(strong(column(width=2,offset=1,"Crime rate: ")),textOutput("cr2")))
                ),br(),
                # === display locale
                
                # fluidRow(align="justify",splitLayout(cellWidths = c("50%","50%"),
                #                                      fluidRow(strong(column(width=2,offset=1,"Locale: ")),textOutput("locale1")),
                #                                      fluidRow(strong(column(width=2,offset=1,"Locale: ")),textOutput("locale2")))
                # ),br(),
                
                # ==== Title in Orange Adimission====
                fluidRow(align="center",
                         style="opacity:0.9; background-color: white ;margin-top: 0px; width: 100%;",
                         column(6,offset=3,
                                br(),hr(style="color:#808080"),
                                helpText( strong("Adimission" , style="color:#6088B1 ; font-family: 'times'; font-size:30pt ; font-type:bold" )) ,
                                hr(style="color:#808080")
                         )),
                ####display admission rate
                
                fluidRow(align="justify",splitLayout(cellWidths = c("50%","50%"),
                                                     fluidRow(strong(column(width=4,offset=1,"Admission Rate: ")),textOutput("adm_rate1")),
                                                     fluidRow(strong(column(width=4,offset=1,"Admission Rate: ")),textOutput("adm_rate2")))
                ),br(),
                ### SAT & ACT
                fluidRow(align="center",
                         style="opacity:0.9; background-color: white ;margin-top: 0px; width: 100%;",
                         column(6,offset=3,
                                br(),hr(style="color:#808080"),
                                helpText( strong("SAT & ACT Scores" , style="color:#6088B1 ; font-family: 'times'; font-size:20pt ; font-type:bold" )) ,
                                helpText( strong("25th-75th Percentile" , style="color:#6088B1 ; font-family: 'times'; font-size:10pt ; font-type:bold" )),
                                hr(style="color:#808080")
                         )),
                br(),
                
                # fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"),
                #                                     textOutput("school1.2"),
                #                                     textOutput("school2.2"))
                #          
                # ),
                
                fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"),
                                                    tableOutput("sat1"),
                                                    tableOutput("sat2"))
                         
                ),br(),
                
                fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"),
                                                    tableOutput("act1"),
                                                    tableOutput("act2"))
                         
                ),
                br(),
                br(),
                # === Bar with corresponding widget
                fluidRow(align="center",column(4,h2("Major Diversity",
                                                    style="color:#4C4C4C ; font-family: Times"),
                                               tags$hr(style="border-color: #6088B1;")),br()),
                fluidRow(align="center",
                         splitLayout(cellWidths = c("50%","50%"),
                                     plotlyOutput("my_barplot1" , height = "500px"),
                                     plotlyOutput("my_barplot2" , height = "500px")
                         )
                         
                ),br(),br(),
                # ==== Title in Orange Cost & Aid====
                fluidRow(align="center",
                         style="opacity:0.9; background-color: white ;margin-top: 0px; width: 100%;",
                         column(6,offset=3,
                                br(),hr(style="color:#808080"),
                                helpText( strong("Cost & Aid" , style="color:#6088B1 ; font-family: 'times'; font-size:30pt ; font-type:bold" )) ,
                                hr(style="color:#808080")
                         )),
                ### display in-state tuition
                
                fluidRow(align="justify",splitLayout(cellWidths = c("50%","50%"), 
                                                     fluidRow(strong(column(width=4,offset=1,"In-State Tuition: ")),textOutput("tuitionfee_in1")),
                                                     fluidRow(strong(column(width=4,offset=1,"In-State Tuition: ")),textOutput("tuitionfee_in2")))
                ),br(),
                ### display out-of-state tuition

                fluidRow(align="justify",splitLayout(cellWidths = c("50%","50%"),
                                                     fluidRow(strong(column(width=4,offset=1,"Out-of-State Tuition: ")),textOutput("tuitionfee_out1")),
                                                     fluidRow(strong(column(width=4,offset=1,"Out-of-State Tuition: ")),textOutput("tuitionfee_out2")))
                ),br(),
                ### display percentage of federal loans
                
                fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"),
                                                    fluidRow(strong(column(width=7,offset=1,"Percentage of Students Receiving Federal Loans: ")),
                                                             textOutput("pctfloan1")),
                                                    fluidRow(strong(column(width=7,offset=1,"Percentage of Students Receiving Federal Loans: ")),
                                                             textOutput("pctfloan2")))
                ),br(),
               
                #### Median debt given income
                fluidRow(align="center",
                         style="opacity:0.9; background-color: white ;margin-top: 0px; width: 100%;",
                         column(6,offset=3,
                                br(),hr(style="color:#808080"),
                                helpText( strong("Calculate Median Debt by Family Income" , style="color:#6088B1 ; font-family: 'times'; font-size:20pt ; font-type:bold" )) ,
                                hr(style="color:#808080")
                         )),
                br(),

                # === display sliderInput for family income
                fluidRow(align="center",sliderInput("fincome","Family Income: ",
                                                    min=0,max=200000,value=0,width = 600)
                         
                ),br(),
                # === display median debt based on family income input 
                
                
                fluidRow(align="justify",splitLayout(cellWidths = c("50%","50%"), 
                                                     fluidRow(
                                                              strong(column(width=7,offset = 1,"Median Debt based on Family Income : ")),br(),
                                                              textOutput("debt1")),
                                                     fluidRow(
                                                              strong(column(width=7,offset=1,"Median Debt based on Family Income: ")),br(),
                                                              textOutput("debt2")))
                ),
                br(),
                br(),
                
                # =======  EARNING =======
                fluidRow(align="center",
                         style="opacity:0.9; background-color: white ;margin-top: 0px; width: 100%;",
                         column(6,offset=3,
                                br(),hr(style="color:#808080"),
                                helpText( strong("Earning" , style="color:#6088B1 ; font-family: 'times'; font-size:30pt ; font-type:bold" )) ,
                                hr(style="color:#808080")
                         )),
                br(),
                
                #### display pct over 25K
                fluidRow(align="justify",splitLayout(cellWidths = c("50%","50%"),
                                                     fluidRow(strong(column(width=7,offset=1,"Share of students earning over 25K (6 yrs): \n")),
                                                              textOutput("gt25k_out1")),
                                                     fluidRow(strong(column(width=7,offset=1,"Share of students earning over 25K (6 yrs): \n")),
                                                              textOutput("gt25k_out2")))
                ),br(),

                ### earning 25-75 percentile
                fluidRow(align="center",
                         style="opacity:0.9; background-color: white ;margin-top: 0px; width: 100%;",
                         column(6,offset=3,
                                br(),hr(style="color:#808080"),
                                helpText( strong("Income" , style="color:#6088B1 ; font-family: 'times'; font-size:20pt ; font-type:bold" )) ,
                                helpText( strong("25th-75th Percentile" , style="color:#6088B1 ; font-family: 'times'; font-size:10pt ; font-type:bold" )),
                                hr(style="color:#808080")
                         )),
                br(),

                # fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"),
                #                                     textOutput("school1.2"),
                #                                     textOutput("school2.2"))
                # 
                # ),

                fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"),
                                                    tableOutput("earning1"),
                                                    tableOutput("earning2"))

                ),br(),
                  br(),
               
                
                 #=======Demographics of Students=====
                
                fluidRow(align="center",
                         style="opacity:0.9; background-color: white ;margin-top: 0px; width: 100%;",
                         column(6,offset=3,
                                br(),
                                hr(style="color:#808080"),
                                helpText( strong("Demographics of Students" , style="color: #6088B1 ; font-family: 'times'; font-size:30pt ; font-type:bold" )) ,
                                hr(style="color:#808080")
                         )),
                br(),
                #### display total Undergraduates Seeking Degrees
                
                fluidRow(align="center",splitLayout(cellWidths = c("50%","50%"), 
                                                    fluidRow(strong(column(width=5,offset=1,"Total Undergraduates Seeking Degrees: ")),
                                                             textOutput("ugds1")),
                                                    fluidRow(strong(column(width=5,offset=1,"Total Undergraduates Seeking Degrees: ")),
                                                             textOutput("ugds2")))
                ),
                br(),
                br(),
                
                # === pie chart of ethnicity
                fluidRow(align="center",column(8,h2("Degree-Seeking Undergraduates by Ethnicity",
                                                    style="color:#4C4C4C ; font-family: Times"),
                                               tags$hr(style="border-color: #6088B1;")),br()),
                fluidRow(align="center",
                         splitLayout(cellWidths = c("50%","50%"),
                                     plotlyOutput("demographics1",height="550"),
                                     plotlyOutput("demographics2",height="550"))
                         
                ),br(),
                fluidRow(align="center",column(8,h2("Degree-Seeking Undergraduates by Gender",
                                                    style="color:#4C4C4C ; font-family: Times"),
                                               tags$hr(style="border-color: #6088B1;")),br()),
                fluidRow(align="center",
                         splitLayout(cellWidths = c("50%","50%"),
                                     plotlyOutput("female1",height="450"),
                                     plotlyOutput("female2",height="450")
                                     
                         ))
              
                )
 
))



  
  
  
  

