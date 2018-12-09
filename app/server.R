packages.used=c("DT","shiny","ggmap","leaflet","dplyr","shinyBS","plotly","extrafont","grDevices","shinyjs", "formattable")

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
library(formattable)

#Read in Files
college=read.csv("college_top.csv")
college1=read.csv("college_top1.csv")
college1$PCT25_EARN_WNE_P6=college1$PCT25_EARN_WNE_P6%>%as.character()
college1$PCT75_EARN_WNE_P6=college1$PCT75_EARN_WNE_P6%>%as.character()
college1$PCT25_EARN_WNE_P8=college1$PCT25_EARN_WNE_P8%>%as.character()
college1$PCT75_EARN_WNE_P8=college1$PCT75_EARN_WNE_P8%>%as.character()
college1$PCT25_EARN_WNE_P10=college1$PCT25_EARN_WNE_P10%>%as.character()
college1$PCT75_EARN_WNE_P10=college1$PCT75_EARN_WNE_P10%>%as.character()


schooldata <- read.csv(file="final3data_with_tuition.csv",stringsAsFactors = FALSE)

#Support data frames
major = c("Agriculture","Natural Resources And Conservation", "Architecture And Related Services",
          "Computer And Information Sciences And Support Services"," Education","Engineering"," Biological And Biomedical Sciences",
          "Mathematics And Statistics", "Psychology","Social Sciences","Business, Management, Marketing, And Related Support Services","History")
major.index =colnames(schooldata)[16:27]
major.frame = data.frame(major = major, index = major.index)

for(i in 1:nrow(schooldata)){
  if(schooldata$Rank[i] <= 10)
  {
    schooldata$RankType[i] = "Ambitious"
  } else if(schooldata$Rank[i] <=30 & schooldata$Rank[i] > 10)
  {
    schooldata$RankType[i] = "Mid Level"
  } else schooldata$RankType[i] = "Safe"
}

#Data Preprocessing
schooldata$RankType <- as.factor(schooldata$RankType)
schooldata$ADMrate <- as.double(schooldata$ADMrate)
schooldata$Tuition.and.fees.y <- as.numeric(currency(schooldata$Tuition.and.fees.y))
schooldata$ADMrate <- round(ifelse(schooldata$ADMrate == "NULL", mean(as.numeric(schooldata$ADMrate),na.rm = TRUE),as.numeric(schooldata$ADMrate)),3)

#Get Icons based on Ranks
getColor <- function(k) {
  sapply(k$RankType, function(RankType) {
    if(RankType == "Ambitious" ) {"red"} else if(RankType == "Mid Level") {"orange"} else {"green"} })}

#Convert City types into 1 and o (1 for city and 0 for not city)
school1 <- schooldata %>% mutate(city_nocity=ifelse(schooldata$Citytype=='City',1,0))%>%
  mutate(c_nc=ifelse(city_nocity==1,"City","Not City"))

#Prepare font list for radar plot
font1 <- list(size=16,color="white")

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
  
  ADMrate<-reactive({
    ADMrate<-input$ADMrate
  
  })
  
  Tuition.and.fees.y<-reactive({
    Tuition.and.fees.y<-input$Tuition.and.fees.y
    
  })
  
  RankType<-reactive({
    RankType <- input$RankType
    
  })
  
  
  v2<-reactive({
    
    if (Major() == "") {
      v2<-v1()
    } 
    else {
      v2 <- v1()[(v1() %>% select(Major()))>=1,]
    }
    
  })
  
  
  v3<- reactive({
    v3 <- filter(v2(),
                 as.numeric(SAT) >= SAT()[1] &
                   as.numeric(SAT) <= SAT()[2])
  })  
  
  
  v4<- reactive({
    if (Citytype() == "None") {
      v4<- v3()} 
    else {
      v4<- filter(v3(), v3()$Citytype == Citytype()) 
    }}) 
  
  
  v5<- reactive({
    v5 <- filter(v4(),
                 as.numeric(CrimeRate) >= CrimeRate()[1] &
                   as.numeric(CrimeRate) <= CrimeRate()[2]) 
  }) 
  
  v6<- reactive({
    v6 <- filter(v5(),
                 as.numeric(HappyScore) >= HappyScore()[1] &
                   as.numeric(HappyScore) <= HappyScore()[2]) 
  })
  
  v7<- reactive({
    v7 <- filter(v6(),
                 ADMrate >= ADMrate()[1] &
                   ADMrate <= ADMrate()[2])  
  })
  
  v8<- reactive({
    v8 <- filter(v7(),
                 as.numeric(Tuition.and.fees.y) >= Tuition.and.fees.y()[1] &
                   as.numeric(Tuition.and.fees.y) <= Tuition.and.fees.y()[2])
  
  })
  
  v1<- reactive({
      v1<- filter(schooldata, schooldata$RankType == RankType()) 
    })

  
  
  
  
  output$my_map <- renderLeaflet({
    
    
    
    #load icon
    icon.ion <- awesomeIcons(icon = "ios-close",
                             iconColor = "black",
                             library = "ion",
                             markerColor = getColor(v8()))
    
    #
    sub_data <- v8()
    
    
    
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
      
      
      #load map
      map = leaflet(sub_data) %>% setView(-98.35  , 39.48, zoom = 4) %>% 
        #addTiles()%>% addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
        #                      attribution = paste(
        #                       '&copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors',
        #                      '&copy; <a href="http://cartodb.com/attributions">CartoDB</a>'
        #                   ))
        addProviderTiles(providers$Esri.WorldStreetMap) %>%
        addAwesomeMarkers(lng = v8()$Longitude, lat = v8()$Latitude, icon=icon.ion, 
                          popup = paste(v8()$Name,"         ;Rank:",v8()$Rank,sep="\n"),label=v8()$URL,
                          options = markerOptions(riseOnHover = T))
      
      
      
      
      
    }
    
    #add marker for dest in the map
    # 
    
    
    
    
  })  
  
##Comparison starts here
   output$logo_a = renderImage({
     my_schools_file <- input$sname_a
     regex_image <- ".jpg"
     filename <- normalizePath(file.path(
       paste("../app/www/logos/",my_schools_file, regex_image, sep = "")))
     
     list(src=filename)
   },deleteFile = FALSE)
   
   output$logo_b = renderImage({
     my_schools_file <- input$sname_b
     regex_image <- ".jpg"
     filename <- normalizePath(file.path(
       paste("../app/www/logos/",my_schools_file, regex_image, sep = "")))
     
     list(src=filename)
   },deleteFile = FALSE)

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
  ####### Earning percentile ####### 
  
  MY_earning_data1=reactive({
    
    # Get the needed reactive objects:
    my_schools=c(input$sname_a,input$sname_b)
    
    get.earning.data <- function(data,school){
      
      
      relevant.data <- data[data$INSTNM==school,]
      
      my.vector <- c(relevant.data$PCT25_EARN_WNE_P6,relevant.data$PCT75_EARN_WNE_P6,
                     relevant.data$PCT25_EARN_WNE_P8,relevant.data$PCT75_EARN_WNE_P8,
                     relevant.data$PCT25_EARN_WNE_P10,relevant.data$PCT75_EARN_WNE_P10
      )
      my.vector=my.vector%>% as.numeric()
      return(my.vector)
    }
    output <- get.earning.data(college1,my_schools[1])
    return(output)
    
  })
  
  MY_earning_data2=reactive({
    
    # Get the needed reactive objects:
    my_schools=c(input$sname_a,input$sname_b)
    
    get.earning.data <- function(data,school){
      
      
      relevant.data <- data[data$INSTNM==school,]
      
      my.vector <- c(relevant.data$PCT25_EARN_WNE_P6,relevant.data$PCT75_EARN_WNE_P6,
                     relevant.data$PCT25_EARN_WNE_P8,relevant.data$PCT75_EARN_WNE_P8,
                     relevant.data$PCT25_EARN_WNE_P10,relevant.data$PCT75_EARN_WNE_P10
      )
      my.vector=my.vector %>% as.numeric()
      return(my.vector)
    }
    output <- get.earning.data(college1,my_schools[2])
    return(output)
    
  })
  
  output$earning1 <- renderTable(
    
    
    cbind(After_Entry = c("6 yrs" ,"8 yrs", "10 yrs"),
          Income = c(paste("$",MY_earning_data1()[1],"-","$",MY_earning_data1()[2]),
                     paste("$",MY_earning_data1()[3],"-","$",MY_earning_data1()[4]),
                     paste("$",MY_earning_data1()[5],"-","$",MY_earning_data1()[6])))
  )
  output$earning2 <- renderTable(
    
    
    cbind(After_Entry = c("6 yrs" ,"8 yrs", "10 yrs"),
          Income = c(paste("$",MY_earning_data2()[1],"-","$",MY_earning_data2()[2]),
                     paste("$",MY_earning_data2()[3],"-","$",MY_earning_data2()[4]),
                     paste("$",MY_earning_data2()[5],"-","$",MY_earning_data2()[6])))
  )
  ####### end of earning percnetile ###### 
  ###### gender pie chart#######
  
  ####get data used to draw pie chart of female students
  MY_female_data1=reactive({
    
    # Get the needed reactive objects:
    my_schools=c(input$sname_a,input$sname_b)
    
    get.mf.data <- function(data,school){
      index <- which(data$INSTNM == school)
      female <- data$FEMALE[index]%>%as.character()%>%as.numeric()
      
      if (is.na(female)) {female <- "PrivacySuppressed"}
      if(female=="PrivacySuppressed"){
        out.val<-data.frame(1,"Privacy Suppressed",color='rgb(128,133,133)')
      } else{
        female<-as.numeric(female) 
        out.val<-data.frame(c(female,1-female),c("Female","Male"), color=c(1,2))
      }
      
      return(out.val)
    }
    output <- get.mf.data(college, my_schools[1])
    #colnames(output) <- c("1","mf")
    return(output)
    
  })
  output$female1 <- renderPlotly(
    plot_ly(MY_female_data1(), labels = ~MY_female_data1()[,2], values = ~MY_female_data1()[,1], type = 'pie',
            marker = list(colors = MY_female_data1()[,3], 
                          line = list(color = '#FFFFFF', width = 1)), 
            width = 400, height = 400, textposition = 'inside+outside',
            textinfo = 'label',
            insidetextfont = list(color = '#FFFFFF'), showlegend=F
    ) 
    %>%
      layout(title = paste("Gender & Ethnicity Diversity of <br>",name1()),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             legend=list(orientation='h'),
             margin = list(l = 50,r = 50,b = 100,t = 100,pad = 4))%>%
      layout(paper_bgcolor='#00000000')
  )
  
  
  MY_female_data2=reactive({
    # Get the needed reactive objects:
    my_schools=c(input$sname_a,input$sname_b)
    
    get.mf.data <- function(data,school){
      index <- which(data$INSTNM == school)
      female <- data$FEMALE[index]%>%as.character()%>%as.numeric()
      
      if (is.na(female)) {female <- "PrivacySuppressed"}
      
      #relevent_data <- data[data$INSTNM == school,]
      #female <- relevent_data$FEMALE
      if(female=="PrivacySuppressed"){
        out.val<-data.frame(1,"Privacy Suppressed",color='rgb(128,133,133)')
      } else{
        female<-as.numeric(female) 
        out.val<-data.frame(c(female,1-female),c("Female","Male"),color=c(1,2))
      }
      return(out.val)
      
    }
    output <- get.mf.data(college, my_schools[2])
    #colnames(output) <- c("2","mf")
    return(output)
    
  })
  output$female2 <- renderPlotly(
    plot_ly(MY_female_data2(), labels = ~MY_female_data2()[,2], values = ~MY_female_data2()[,1], type = 'pie',
            marker = list(colors = MY_female_data2()[,3], 
                          line = list(color = '#FFFFFF', width = 1)), 
            width = 400, height = 400, textposition = 'inside+outside',
            textinfo = 'label',
            insidetextfont = list(color = '#FFFFFF'), showlegend=F
    ) 
    %>%
      layout(title = paste("Gender & Ethnicity Diversity of <br>",name2()),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             legend=list(orientation='h'),
             margin = list(l = 50,r = 50,b = 100,t = 100,pad = 4))%>%
      layout(paper_bgcolor='#00000000')
  )
  
  #####end of gender pie chart#####
  
  ######## ethnicity diversity####
  ##get data used to draw pie chart of ethnicity
  
  MY_ethnicity_data1=reactive({
    
    # Get the needed reactive objects:
    my_schools=c(input$sname_a,input$sname_b)
    
    get.pie.chart <- function(data, school){
      my.text <- "UGDS_2*[A-Z]+"
      indices <- grepl(my.text,colnames(data))
      my.data <- data[data$INSTNM==school,indices]
      my.data <- my.data[,1:9]
      my.data <- as.vector(as.matrix(my.data[1,]))
      
      demo.names <- c("White","Black","Hispanic","Asian","American Indian/Alaska Native",
                      "Native Hawaiian/Pacific Islander","2 or More Races","Non-resident Aliens",
                      "Unknown")
      #colnames(my.data) <- demo.names
      my.df <- data.frame(my.data,demo.names,colors=c(1,2,3,4,5,6,7,8,9)
      )
      
      to.remove <- NULL
      for (i in 1:length(my.data)){
        if (my.data[i] == 0 | is.na(my.data[i])){
          to.remove <- cbind(to.remove,i)
        }
      }
      to.remove <- as.vector(to.remove[1,])
      
      if (length(my.data) == length(to.remove)){
        output.df <- data.frame(1,"Privacy Suppressed",color='rgb(128,133,133)')
        #colnames(my.df) <- "NA"
        return(output.df)
      } else if (!is.null(to.remove)){
        #my.df <- my.data[-to.remove]
        #colnames(my.df) <- demo.names[-to.remove]
        output.df <- my.df[-to.remove,]
        return(output.df)
      } else {return(my.df)}
      
    }
    
    #output <- as.matrix(get.pie.chart(college, my_schools[2]))
    #rownames(output) <- 2
    output <- get.pie.chart(college,my_schools[1])
    return(output)
    
  })
  #colors <- c('rgb(128,133,133)','rgb(211,94,96)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
  
  output$demographics1 <-
    renderPlotly(
      plot_ly(MY_ethnicity_data1(), labels = MY_ethnicity_data1()[,2],
              values = MY_ethnicity_data1()[,1], 
              type = 'pie',marker = list(colors = MY_ethnicity_data1()[,3],
                                         line = list(color = '#FFFFFF', width = 1)),
              width = 400, height = 400, textposition = 'inside+outside',
              textinfo = 'label',
              insidetextfont = list(color = '#FFFFFF'), showlegend=F )
      %>%
        layout(title = NULL,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               legend=list(orientation='h'), 
               margin = list(l = 100,r = 100,b = 20,t = 100,pad = 5))%>%
        layout(paper_bgcolor='#00000000')
      
      
    )
  
  MY_ethnicity_data2=reactive({
    
    # Get the needed reactive objects:
    my_schools=c(input$sname_a,input$sname_b)
    
    get.pie.chart <- function(data, school){
      my.text <- "UGDS_2*[A-Z]+"
      indices <- grepl(my.text,colnames(data))
      my.data <- data[data$INSTNM==school,indices]
      my.data <- my.data[,1:9]
      my.data <- as.vector(as.matrix(my.data[1,]))
      
      demo.names <- c("White","Black","Hispanic","Asian","American Indian/Alaska Native",
                      "Native Hawaiian/Pacific Islander","2 or More Races","Non-resident Aliens",
                      "Unknown")
      #colnames(my.data) <- demo.names
      my.df <- data.frame(my.data,demo.names,colors=c(1,2,3,4,5,6,7,8,9)
      )
      
      to.remove <- NULL
      for (i in 1:length(my.data)){
        if (my.data[i] == 0 | is.na(my.data[i])){
          to.remove <- cbind(to.remove,i)
        }
      }
      to.remove <- as.vector(to.remove[1,])
      
      if (length(my.data) == length(to.remove)){
        output.df <- data.frame(1,"Privacy Suppressed",color='rgb(128,133,133)')
        #colnames(my.df) <- "NA"
        return(output.df)
      } else if (!is.null(to.remove)){
        #my.df <- my.data[-to.remove]
        #colnames(my.df) <- demo.names[-to.remove]
        output.df <- my.df[-to.remove,]
        return(output.df)
      } else {return(my.df)}
      
    }
    
    #output <- as.matrix(get.pie.chart(college, my_schools[2]))
    #rownames(output) <- 2
    output <- get.pie.chart(college,my_schools[2])
    return(output)
    
  })
  
  output$demographics2 <-
    renderPlotly(
      plot_ly(MY_ethnicity_data2(), labels =MY_ethnicity_data2()[,2],
              values = MY_ethnicity_data2()[,1], 
              type = 'pie',marker = list(colors = MY_ethnicity_data2()[,3],
                                         line = list(color = '#FFFFFF', width = 1)),
              width = 400, height = 400, textposition = 'inside+outside',
              textinfo = 'label',
              insidetextfont = list(color = '#FFFFFF'), showlegend=F )
      %>%
        layout(title = NULL,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               legend=list(orientation='h'),
               margin = list(l = 100,r = 100,b = 20,t = 100,pad = 5))%>%
        layout(paper_bgcolor='#00000000')
      
    )
  
  
  
  ###### end of ethnicity####
  
  
    
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

