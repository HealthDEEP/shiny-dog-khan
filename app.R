library(shiny)
library(shinyWidgets)
library(shinythemes)
library(leaflet)
library(leafpop)
library(DT)
library(dplyr)
library(ggplot2)
library(likert)
library(grid)

# get dog
mydog <- readxl::read_excel("./data/data.dogs.parasites.khanh.xlsx")
mydog$latitude <- as.numeric(mydog$latitude)
mydog$longitude <- as.numeric(mydog$longitude)

# get questionnaire
myquest <- readxl::read_excel("./data/data.questionnaire.dogs.khanh.xlsx")
myquest <- as.data.frame(myquest)
myquest$village <- as.factor(myquest$village)
myquest$village_group_landscape <- as.factor(myquest$village_group_landscape)

myquest[sapply(myquest, is.character)] <- lapply(myquest[sapply(myquest, is.character)], 
                                       as.factor)


# village
list_village <- mydog %>%
  select(village) %>%
  distinct() %>%
  arrange()

# landscape
list_landscape <- mydog %>%
  select(landscape) %>%
  distinct() %>%
  arrange()


# ui object
ui <- fluidPage(
  theme = shinytheme("flatly"),
  # tags$head(includeScript("google-analytics.js")),
  tags$title("Dogs at Saenthong"),
  column(width = 12,
         h1("Blood parasites"),
         HTML(
           "<div class='alert alert-info'>",
           "ANR FurureHealthSEA",
           "</div>"
         )
  ),
  sidebarLayout(
    sidebarPanel(
      pickerInput("Village",  label ="Choose village",  
                  choices=unique(list_village$village),
                  options = list(`actions-box` = TRUE),
                  selected =unique(list_village$village), multiple = T),
      pickerInput("Landscape",  label ="Choose landscape", 
                  choices = list_landscape$landscape,options = list(`actions-box` = TRUE),
                  selected =unique(list_landscape$landscape), multiple = T),
      selectInput("Question",  label ="Choose question", 
                  choices = unique(names(myquest[9:26]))),
      br(),
      hr(),
      h4("Data"),
      helpText(HTML("Project FutureHealthSEA")),
      hr(),
      h4("Conception"),
      helpText(HTML("HealthDEEP")),
      br(), 
      width=2
    ),
     mainPanel(
      hr(),
      leafletOutput(outputId = "map"),
      #hr(),
      fluidRow(
        column(width = 6,
               h3("Epidemiology"),
               plotOutput("epidemiology", height = 350
               )
        )
      ,
      column(width = 6, h3("Response question"), plotOutput("likertplot", height = 350))
      )
      ,
      hr(),
      h2("Data Table"),
      DTOutput(outputId = "table",width = "100%", height = "auto")
    )
  )
)




server = function(input, output) {
  
  data <- reactive({
    filter(mydog,
           village  %in%  input$Village,
           landscape  %in%  input$Landscape)
  })
  
  output$table <- renderDT({
    datatable(
      data(),
      extensions = "Buttons",
      options = list(paging = TRUE,
                     scrollX=TRUE,
                     pageLength = 5,
                     searching = TRUE,
                     ordering = TRUE,
                     dom = 'l<"sep">Bfrtip',
                     buttons = c('csv', 'excel'),
                     lengthMenu=c(5,10,20,50,100)
      )
    )
  }, server = FALSE)
  
  
  output$epidemiology <- renderPlot({
    
    DFtall <- data() %>% 
      select(village, Babesia_canis_vogeli:Hepatozoon_canis) %>% 
      tidyr::drop_na() %>%
      tidyr::gather(key = Parasites, value = Serology, Babesia_canis_vogeli:Hepatozoon_canis)
    DFtall
    
    ggplot(DFtall %>% filter (Serology == "positive"), aes(Serology, fill = Parasites)) +
      facet_grid(~ Parasites) +
      # facet_wrap(~ Parasites) +
      geom_bar(position = "dodge", show.legend = FALSE) +
      # geom_bar(aes(y = (..count..)/sum(..count..)), position = "stack") +
      ylab("Number of seropositive rodents") +
      theme_bw()
    
  })
  
  
  output$likertplot <- renderPlot({
    
    plot(likert(items = myquest[,input$Question, drop=FALSE],
                grouping =myquest[,"village"]),
         include.histogram = TRUE)
    
  })
  

  
  output$map <- renderLeaflet({
    
    datafiltered <- mydog %>%
      select(village, landscape, longitude, latitude,id_individual, Babesia_canis_vogeli,  Babesia_gibsoni,
             Ehrlichia_canis,Hepatozoon_canis) %>%
      filter(village  %in%  input$Village,
             landscape  %in%  input$Landscape)
    
    Babesia_canis <-datafiltered %>% 
      select(longitude, latitude,id_individual, Babesia_canis_vogeli) %>%
      filter(Babesia_canis_vogeli == "positive")
    
    Babesia_gibsoni <- datafiltered %>% 
      select(longitude, latitude,id_individual, Babesia_gibsoni) %>%
      filter(Babesia_gibsoni == "positive")
    
    Ehrlichia <- datafiltered %>% 
      select(longitude, latitude,id_individual, Ehrlichia_canis) %>%
      filter(Ehrlichia_canis=="positive")
    
    Hepatozoon <-datafiltered %>% 
      select(longitude, latitude,id_individual, Hepatozoon_canis) %>%
      filter(Hepatozoon_canis == "positive")
    
    if (nrow(datafiltered) == 0) {
      datafiltered  %>%
        leaflet() %>%
        addTiles() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE),
                         group ="Tamen"
        ) %>%
        addControl("NO DATA", position = "topleft",className = "info legend") %>%
        addProviderTiles(providers$Esri.WorldStreetMap,group ="WSM") %>%
        addProviderTiles("Esri.WorldImagery",group ="Esri World Imagery") %>%
        addLayersControl(baseGroups = c("Esri World Imagery","Tamen","WSM"),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        addScaleBar(position = "bottomright",
                    scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = FALSE,
                                    updateWhenIdle = TRUE))
    }else{
      datafiltered  %>%
        leaflet() %>%
        addTiles() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE),
                         group ="Tamen"
        ) %>%
        addProviderTiles(providers$Esri.WorldStreetMap,group ="WSM") %>%
        addProviderTiles("Esri.WorldImagery",group ="Esri World Imagery") %>%
        addCircleMarkers(data=datafiltered,
                         ~longitude, ~latitude,group ="all dogs",
                         label = ~as.character(datafiltered$id_individual),
                         labelOptions = labelOptions(noHide = FALSE, textOnly = FALSE,
                                                     style=list(
                                                       'color'='red',
                                                       'font-family'= 'serif',
                                                       'font-style'= 'italic',
                                                       'box-shadow' = '3px 3px rgba(0,0,0,0.25)',
                                                       'font-size' = '12px',
                                                       'border-color' = 'rgba(0,0,0,0.5)'
                                                     )),
                         # popup = popupTable(alldogs, zcol = 1:4),
                         fillColor = "blue",
                         weight = 1, radius = 5,
                         stroke = FALSE, fillOpacity = 1) %>%
        addCircleMarkers(data= Babesia_canis,
                         ~longitude, ~latitude,group ="Babesia canis",
                         label = ~as.character(datafiltered$id_individual),
                         labelOptions = labelOptions(noHide = FALSE, textOnly = FALSE,
                                                     style=list(
                                                       'color'='red',
                                                       'font-family'= 'serif',
                                                       'font-style'= 'italic',
                                                       'box-shadow' = '3px 3px rgba(0,0,0,0.25)',
                                                       'font-size' = '12px',
                                                       'border-color' = 'rgba(0,0,0,0.5)'
                                                     )),
                         # popup = popupTable(Babesias, zcol = 1:5),
                         fillColor = "yellow",
                         weight = 1, radius = 5,
                         stroke = FALSE, fillOpacity = 1)%>%
        addCircleMarkers(data= Babesia_gibsoni,
                         ~longitude, ~latitude,group ="Babesia gibsoni",
                         label = ~as.character(datafiltered$id_individual),
                         labelOptions = labelOptions(noHide = FALSE, textOnly = FALSE,
                                                     style=list(
                                                       'color'='red',
                                                       'font-family'= 'serif',
                                                       'font-style'= 'italic',
                                                       'box-shadow' = '3px 3px rgba(0,0,0,0.25)',
                                                       'font-size' = '12px',
                                                       'border-color' = 'rgba(0,0,0,0.5)'
                                                     )),
                         # popup = popupTable(Ehrlichias, zcol = 1:5),
                         fillColor = "orange",
                         weight = 1, radius = 5,
                         stroke = FALSE, fillOpacity = 1)%>%
        addCircleMarkers(data= Ehrlichia,
                         ~longitude, ~latitude,group ="Ehrlichia canis",
                         label = ~as.character(datafiltered$id_individual),
                         labelOptions = labelOptions(noHide = FALSE, textOnly = FALSE,
                                                     style=list(
                                                       'color'='red',
                                                       'font-family'= 'serif',
                                                       'font-style'= 'italic',
                                                       'box-shadow' = '3px 3px rgba(0,0,0,0.25)',
                                                       'font-size' = '12px',
                                                       'border-color' = 'rgba(0,0,0,0.5)'
                                                     )),
                         # popup = popupTable(Ehrlichias, zcol = 1:5),
                         fillColor = "red",
                         weight = 1, radius = 5,
                         stroke = FALSE, fillOpacity = 1)%>%
        addCircleMarkers(data=Hepatozoon,
                         ~longitude, ~latitude,group ="Hepatozoon canis",
                         label = ~as.character(datafiltered$id_individual),
                         labelOptions = labelOptions(noHide = FALSE, textOnly = FALSE,
                                                     style=list(
                                                       'color'='red',
                                                       'font-family'= 'serif',
                                                       'font-style'= 'italic',
                                                       'box-shadow' = '3px 3px rgba(0,0,0,0.25)',
                                                       'font-size' = '12px',
                                                       'border-color' = 'rgba(0,0,0,0.5)'
                                                     )),
                         # popup = popupTable(Hepatozoons, zcol = 1:5),
                         fillColor = "greend",
                         weight = 1, radius = 5,
                         stroke = FALSE, fillOpacity = 0.4) %>%
        addLayersControl(baseGroups = c("Esri World Imagery","WSM"),
                         # baseGroups = c("Esri World Imagery","Tamen","WSM"),
                         overlayGroups = c("all dogs","Babesia canis","Babesia gibsoni","Ehrlichia canis","Hepatozoon canis"),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        addScaleBar(position = "bottomright",
                    scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = FALSE,
                                    updateWhenIdle = TRUE))
    }
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)

