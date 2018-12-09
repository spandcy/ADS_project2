#UI part

packages.used=c("DT","shiny","ggmap","leaflet","dplyr","shinyBS","plotly","extrafont","grDevices","shinyjs")

# check packages that need to be installed.
packages.needed=setdiff(packages.used,intersect(installed.packages()[,1],packages.used))

# install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}

library(DT)
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

schooldata <- read.csv(file="final3data_with_tuition.csv",stringsAsFactors = FALSE)
#Support data frames
major = c("Agriculture","Natural Resources And Conservation", "Architecture And Related Services",
          "Computer And Information Sciences And Support Services"," Education","Engineering"," Biological And Biomedical Sciences",
          "Mathematics And Statistics", "Psychology","Social Sciences","Business, Management, Marketing, And Related Support Services","History")
major.index =colnames(schooldata)[16:27]
major.frame = data.frame(major = major, index = major.index)
choicelist<-as.list(unique(as.data.frame(major.frame)[,2]))
namelist <- as.list(schooldata$Name)

#Homepage
ui <- navbarPage(theme=shinytheme("flatly"),
  includeCSS("style.css"),
    tabPanel(
      title="Home",icon=icon("home"),
      fluidRow(
        column(width=12,div(style="height:100px;"))),

      fluidRow(
        column(width=4,offset=4,div(style="height:70px;"),h1("Schools Hunter",align="center",style="color:white;font-family:Montserrat;")
        )
      ),
      fluidRow(column(width=12,div(style="height:30px;"))),
      fluidRow(
        column(width=4,offset=4,div(style="height:100px",p("Our project takes all available data on colleges and universities across US and creates a useful 
                                                           shiny app that allows users to explore and compare schools based on user-specific filtering criteria. 
                                                           The purpose of our design is to provide users with a 
                                                           bird's eye view of New York colleges and universities; 
                                                           allow them to filter, search, and group schools by their preferred criteria; 
                                                           and further compare two schools on a more micro level.",align="center",style="color:#e6e6e6")))
        ),
      fluidRow(column(width=12,div(style="height:300px;"))
              )
      
        ),
  
#Map 
    tabPanel(
      title="Maps", icon=icon("map"),
      div(class="outer",  
          # lealfet map
          leafletOutput("my_map", width="100%", height= "700px"),
          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                        style="opacity:0.8;font-family:Montserrat;",
                        draggable = TRUE, top = 100, left = 5, bottom = "auto",
                        width = "auto", height = "auto", cursor = "move",
                        wellPanel(style = "overflow-y:scroll; max-height: 600px",
                                  bsCollapse(id="collapse.filter",open="Filter", 
                                             
                                             bsCollapsePanel(tags$strong("Academic",style="font-family:Franklin Gothic Medium;"),style="primary",
                                                             tags$style(type="text/css",
                                                                        ".shiny-output-error { visibility: hidden; }",
                                                                        ".shiny-output-error:before { visibility: hidden; }"),
                                                             fluidRow(column(12,checkboxGroupInput("filter",tags$b("Major",style="font-family:Franklin Gothic Medium;"),choices=choicelist,selected=1))),
                                                             fluidRow(column(10,sliderInput('SAT',label=tags$b('SAT Range',style="font-family:Franklin Gothic Medium;"),min=800,max=1600,value=c(1200,1600))))),
                                             
                                             
                                             
                                             
                                             bsCollapsePanel(tags$strong("Location Preference",style="font-family:Franklin Gothic Medium;"),style = "primary",
                                                             bsCollapsePanel(tags$strong("City Type",style="font-family:Franklin Gothic Medium;"),style="info",
                                                                             fluidRow(column(10,selectInput("Citytype",tags$strong("Type prefer",style="font-family:Franklin Gothic Medium;"),choices = c('Suburb','City','Town','Rural'),selected = "None"))
                                                                             )
                                                             ),
                                                             bsCollapsePanel(tags$strong("Crime Rate",style="font-family:Franklin Gothic Medium;"),style="info",
                                                                             fluidRow(column(10,sliderInput("CrimeRate",label=tags$b('Crime Range',style="font-family:Franklin Gothic Medium;"),min=0,max=1300,value=c(100,500))))
                                                                             
                                                             ),
                                                             
                                                             bsCollapsePanel(tags$strong("Happy Score",style="font-family:Franklin Gothic Medium;"),style="info",  
                                                                             fluidRow(column(10,sliderInput("HappyScore",tags$strong("Happy Score",style="font-family:Franklin Gothic Medium;"),min=30,max=80,value=c(50,60))))
                                                             )
                                             )
                                  ),                                                      
                                  actionButton("search", tags$strong("Searching!"))
                        )#WellPanel ends here
                        
          )
          
          
          
      )
     
    ),

#Comparison
    tabPanel(
      title="Comparison",icon=icon("balance-scale"),
      fluidRow(column(width=4,offset=4,tags$h1("Side By side comparison",style="color:white;font-family:Montserrat;"))),
      fluidRow(column(width=5,wellPanel(style="opacity:0.8;font-family:Montserrat;",
                                        tags$b("School A"),
                                        selectInput("sname_a",label=tags$b("Name",style="font-family:Franklin Gothic Medium"),choices=namelist),
                                        #imageOutput("logo_a",height = "400", width = "400"),
                                        dataTableOutput("comp_a")
                                        
                                        )),
              column(width=5,offset=2,wellPanel(style="opacity:0.8;font-family:Montserrat;",
                                       tags$b("School B"),
                                       selectInput("sname_b",label=tags$b("Name",style="font-family:Franklin Gothic Medium"),choices=namelist),
                                       #imageOutput("logo_b",height = "400", width = "400"),
                                       dataTableOutput("comp_b")))
                         )),
     

#Recommendation
    tabPanel(
      title="Recommendation System",icon=icon("thumbs-up"),
     
       fluidRow(
                column(width=2,style="padding:0px;",
                       
                      wellPanel(top=50,style="opacity:0.8;font-family:Montserrat;",
                          
                      h3("Select Filters",style="color:black;font-family:Franklin Gothic Medium;"),
             
                      sliderInput("satscore",label=tags$b("SAT Score",style="color:black;font-family:Franklin Gothic Medium;"),min=400, max=1600, value=1000,step=10),
                      br(),
              
                      selectInput("city",label=tags$b("Located in city/rural",style="color:black;font-family:Franklin Gothic Medium;"),choices=c("City","Not City")),
                      br(),
              
                      sliderInput("crime",label=tags$b("Crime Rate (Per 100000 people)",style="color:black;font-family:Franklin Gothic Medium;"),min=20,max=1300,value=600),
                      br(),
                 
                      sliderInput("tuition", label=tags$b("Tuition (Per Year)",style="color:black;font-family:Franklin Gothic Medium;"),min=10000,max=60000,value=30000),
                      br(),
                      sliderInput("adm",label=tags$b("Admission Rate",style="color:black;font-family:Franklin Gothic Medium;"),min=0,max=1,value=0.5,step=0.1)),
      
                      wellPanel(top=50,style="opacity:0.8;font-family:Montserrat;",
                               
                      h3("Instruction",style="color:black;font-family:Franklin Gothic Medium;"),
                      
                      h6("This is a system designed for finding your perfect school, choose your SAT score, your preference of the location of university, the maximum crime rate(per 100000 people) and tuition(per year) you can accept, and your preference of major. Then hit the search SCHOOL button to get matched school in the summary table.", style="color:grey;font-family:Franklin Gothic Medium;"))),
              
               column(width=2, style="padding:0px;",
                     
                      wellPanel(top=50,style="opacity:0.8;font-family:Montserrat;",
             
                      checkboxGroupInput("Major",label=tags$b("Major",style="color:black;font-family:Franklin Gothic Medium;"),choices=choicelist,selected=1),
              
                      actionButton("getschool",label="Search SCHOOL")
              
    )),
               column(width=5,titlePanel(tags$b("Summary Table",style="color:white;font-family:Franklin Gothic Medium;")),
                      
                      wellPanel(dataTableOutput("uni"),style="opacity:0.8;font-family;Montserrat;")),
   
               column(width=3,div(style="height:50px"),
          
                      plotlyOutput("radarplot"))
                 
      )
    
    )
    
  ,div(class="footer", "Applied Data Science Fall18 Group 2")
    )
    
      
  
  


  
  
  
  

