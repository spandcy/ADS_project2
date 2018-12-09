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

#Read in Files



schooldata <- read.csv(file="final3data_with_tuition.csv",stringsAsFactors = FALSE)

#Support data frames
major = c("Agriculture","Natural Resources And Conservation", "Architecture And Related Services",
          "Computer And Information Sciences And Support Services"," Education","Engineering"," Biological And Biomedical Sciences",
          "Mathematics And Statistics", "Psychology","Social Sciences","Business, Management, Marketing, And Related Support Services","History")
major.index =colnames(schooldata)[16:27]
major.frame = data.frame(major = major, index = major.index)


#Convert City types into 1 and o (1 for city and 0 for not city)
school1 <- schooldata %>% mutate(city_nocity=ifelse(schooldata$Citytype=='City',1,0))%>%
  mutate(c_nc=ifelse(city_nocity==1,"City","Not City"))

#Prepare font list for radar plot
font1 <- list(family="Montserrat",size=16,color="white")

server <- function(input, output,session){
##Map start here
  Major<-reactive({
    Major<-unlist(input$filter)
  })
  
  SAT<-reactive({
    SAT<-input$SAT
  })
  
  Citytype<-reactive({
    Citytype<-input$Citytype
  })
  
  CrimeRate<-reactive({
    CrimeRate<-input$CrimeRate
  })
  
  HappyScore<-reactive({
    HappyScore<-input$HappyScore
  })
  
  v1<-reactive({
    
    if (Major() == "") {
      v1<-schooldata
    } 
    else {
      v1 <- schooldata[(schooldata %>% select(Major()))>=1,]
    }
    
  })
  
  
  v2<- reactive({
    V2 <- filter(v1(),
                 as.numeric(SAT) >= SAT()[1] &
                   as.numeric(SAT) <= SAT()[2])
  })  
  
  
  v3<- reactive({
    if (Citytype() == "None") {
      v3<- v2()} 
    else {
      v3<- filter(v2(), v2()$Citytype==Citytype()) 
    }}) 
  
  
  v4<- reactive({
    v4 <- filter(v3(),
                 as.numeric(CrimeRate) >= CrimeRate()[1] &
                   as.numeric(CrimeRate) <= CrimeRate()[2]) 
  }) 
  
  v5<- reactive({
    v5 <- filter(v4(),
                 as.numeric(HappyScore) >= HappyScore()[1] &
                   as.numeric(HappyScore) <= HappyScore()[2]) 
  })
  
  
  
  
  
  
  
  output$my_map <- renderLeaflet({
    
    
    
    #load icon
    icon.ion <- makeAwesomeIcon(icon = "users", markerColor = "green",
                                library = "ion")
    
    #
    sub_data <- v5()
    
    
    
    if(nrow(sub_data) == 0){
      
      #load map
      map = leaflet(sub_data) %>% setView(-98.35  , 39.48, zoom = 4) %>% 
        #addTiles()%>% addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
        #                      attribution = paste(
        #                       '&copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors',
        #                      '&copy; <a href="http://cartodb.com/attributions">CartoDB</a>'
        #                   ))
        addProviderTiles(providers$Esri.WorldStreetMap)
      
      
      
    }
    
    else{
      
      #data = head(arrange(airdata(),desc(frequency)), input$NumberofDestination)
      
      #load map
      map = leaflet(sub_data) %>% setView(-98.35  , 39.48, zoom = 4) %>% 
        #addTiles()%>% addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
        #                      attribution = paste(
        #                       '&copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors',
        #                      '&copy; <a href="http://cartodb.com/attributions">CartoDB</a>'
        #                   ))
        addProviderTiles(providers$Esri.WorldStreetMap) %>%
        addAwesomeMarkers(lng = v5()$Longitude, lat = v5()$Latitude, icon=icon.ion, popup = paste(v5()$Name,"         ;Rank:",v5()$Rank,sep="\n"),label=v5()$URL)
      
      
      
      
      
    }
    
    #add marker for dest in the map
    # 
    
    
    
    
  })  
  
##Comparison starts here
  # output$logo_a = renderImage({
  #   my_schools_file <- input$sname_a
  #   regex_image <- ".png"
  #   filename <- normalizePath(file.path(
  #     paste("../output/www/",my_schools_file, regex_image, sep = "")))
  #   
  #   list(src=filename)
  # },deleteFile = FALSE)
  # 
  # output$logo_b = renderImage({
  #   my_schools_file <- input$sname_b
  #   regex_image <- ".png"
  #   filename <- normalizePath(file.path(
  #     paste("../output/www/",my_schools_file, regex_image, sep = "")))
  #   
  #   list(src=filename)
  # },deleteFile = FALSE)

  name1 <- reactive({input$sname_a})
  name2 <- reactive({input$sname_b})
  
  d7 <- reactive({d7 <- filter(school1, Name == name1() )})
  d8 <- reactive({d8 <- filter(school1, Name == name2())})
  
  output$comp_a <- DT::renderDataTable({
    school_dt <-
      subset(
        d7(),
        select = c(
          "Name",
          "SAT",
          "c_nc",
          "Tuition.and.fees.y",
          "CrimeRate",
          "ADMrate"
          
        )
      )
    
    colnames(school_dt) <-
      c(
        "School Name",
        "SAT Score",
        "City/Rural",
        "Tuition",
        "Crime Rate (Per 100000 people)",
        "Admission Rate"
        
      )
    
    datatable(
      school_dt,
      rownames = F,
      selection = "single",
      options = list(order = list(list(0, 'asc'), list(1, "asc")))
    )  %>%
      formatCurrency(("Tuition"), digits = 0)
  }, server = T)

  output$comp_b <- DT::renderDataTable({
    school_dt <-
      subset(
        d8(),
        select = c(
          "Name",
          "SAT",
          "c_nc",
          "Tuition.and.fees.y",
          "CrimeRate",
          "ADMrate"
          
        )
      )
    
    colnames(school_dt) <-
      c(
        "School Name",
        "SAT Score",
        "City/Rural",
        "Tuition",
        "Crime Rate (Per 100000 people)",
        "Admission Rate"
        
      )
    
    datatable(
      school_dt,
      rownames = F,
      selection = "single",
      options = list(order = list(list(0, 'asc'), list(1, "asc")))
    )  %>%
      formatCurrency(("Tuition"), digits = 0)
  }, server = T)
  
    
#Recommendation System Plot
legendtitle_1 <- list(yref='paper',xref="paper",y=1.05,x=1, text="City Radar Chart",showarrow=F)
legendtitle_2 <- list(yref='paper',xref="paper",y=1.05,x=1, text="Not City Radar Chart",showarrow=F)

sat_score <- reactive({input$satscore/16})
tuition <- reactive({input$tuition/500})
crime <- reactive({10000/input$crime})
adm <- reactive({input$adm*100})
 
data_radar <- reactive({c(sat_score(),tuition(),crime(),adm())})
  
  observeEvent(input$city,{
    if(input$city=="City"){
  output$radarplot <- renderPlotly({
    plot_ly(
     type="scatterpolar",
     showlegend=T,
     mode='markers+lines+text',
     line=list(color="#8D8680",width=2),
     marker=list(color="#8D8680",size=15,opacity=0.8,symbol="star",line=list(color="#8D8680",width=1)),
     
      r=data_radar(),
      theta = c("SAT","Tuition","Crime Rate","Admission Rate"),
      fill='toself',fillcolor='rgba(255,0,0,0.5')%>%
      layout(
        polar = list(bgcolor='rgba(0,0,0,0.5)',
          radialaxis = list(
          
            linewidth=2,
            linecolor="white",
            tickwidth=2,
            tickcolor="white",
            showline = T,
            gridcolor="white",
            range = c(0,100)
          )
      ))%>%
        layout(plot_bgcolor='#00000000')%>%
        layout(paper_bgcolor='#00000000')%>%
        layout(font=font1,annotations=legendtitle_1)
     
    
  })}})
  
  observeEvent(input$city,{
    if(input$city=="Not City"){
      output$radarplot <- renderPlotly({
        plot_ly(
          type="scatterpolar",
          
          mode='markers+lines',
          line=list(color="#8D8680",width=2),
          marker=list(color="#8D8680",size=15,opacity=0.8,symbol="star",line=list(color="#8D8680",width=1)),
          r=data_radar(),
          theta = c("SAT","Tuition","Crime Rate","Admission Rate"),
          fill='toself',fillcolor='rgba(0,0,255,0.5')%>%
          layout(
            polar = list(bgcolor='rgba(0,0,0,0.5)',
                        
                         radialaxis = list(
                           
                           linewidth=2,
                           linecolor="white",
                           tickwidth=2,
                           tickcolor="white",
                           showline = T,
                           gridcolor="white",
                           range = c(0,100)
                         )
            ))%>%
          layout(plot_bgcolor='#00000000')%>%
          layout(paper_bgcolor='#00000000')%>%
          layout(font=font1,annotations=legendtitle_2)
        
        
      })}})
  
###Recommendation data table####
city1 <- reactive(if(input$city=="City"){city1 <- 1}
                    else {city1 <- 0})
satscore1 <- reactive({satscore1 <- input$satscore})
tuition1 <- reactive({tuition1 <- input$tuition})
adm1 <- reactive({adm1 <- input$adm})
crime1 <- reactive({crime1 <- input$crime})
Major1 <- reactive({Majo1r<-unlist(input$Major)})

d1<-reactive({if (Major1() == "") {d1<-school1} 
  else {d1 <- school1[(school1 %>% select(Major1()))>=1,]}})
d2 <- reactive({d2 <- filter(d1(), as.numeric(SAT)<= satscore1())})
d3 <- reactive({d3 <- filter(d2(), as.numeric(CrimeRate) <= crime1())})
d4 <- reactive({d4 <- filter(d3(),as.numeric(gsub('\\$|,', '', Tuition.and.fees.y)) <= tuition1())})
d5 <- reactive({ if (city1() == 1){d5 <- filter(d4(),as.numeric(city_nocity)==1)}
   else{d5 <- filter(d4(),as.numeric(city_nocity)==0)} })
d6 <- reactive({d6 <- filter(d5(),as.numeric(ADMrate)<=adm1())})


observeEvent(input$getschool,{
  output$uni <- DT::renderDataTable({
  
   school_dt <-
      subset(
        d6(),
        #d5(),
        select = c(
          "Name",
          "SAT",
          "c_nc",
          "Tuition.and.fees.y",
          "CrimeRate",
          "ADMrate"
          
        )
      )
    
    colnames(school_dt) <-
      c(
        "School Name",
        "SAT Score",
        "City/Rural",
        "Tuition",
        "Crime Rate (Per 100000 people)",
        "Admission Rate"
        
      )
    
    datatable(
      school_dt,
      rownames = F,
      selection = "single",
      options = list(order = list(list(0, 'asc'), list(1, "asc")))
    )  %>%
      formatCurrency(("Tuition"), digits = 0)
  }, server = T)
}) 

}

