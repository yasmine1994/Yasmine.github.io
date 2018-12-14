library(shinydashboard)
library(datasets)
library(ggplot2)
library(factoextra)
library(FactoMineR)
library(leaflet)
library(maptools)
library(sp)
library(shapefiles)
library(sunburstR)
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Basic dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "Introduction", icon = icon("dashboard")),
      
      menuItem("Data",tabName = "Data",icon = icon("list")),
      
      menuItem("Statistique_Descriptive",tabName = "Statistique_Descriptive",icon = icon("list")),
      
      menuItem("Contact",tabName = "Contact",icon = icon("th"))
      
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      # First tab content
      
      tabItem(tabName = "Introduction",box(title = "Introduction",status = "info",solidHeader = TRUE,collapsible = TRUE,width = 12 ,h4("La conférence Tunisie 2020, aussi appelée conférence Internationale pour l'investissement ou Tunisie à l'horizon 2020, sur le chemin de l'intégration, de l'efficacité et de la durabilité, est une conférence Internationale qui vise à relancer l'économie et l'investissement en Tunisie. Elle est organisée les 29 et 30 novembre 2016 à Tunis1.
                                                                                                                                       
                                                                                                                                       La Tunisie connaît une révolution entre le 17 décembre 2010 et le 14 janvier 2011. Elle renverse le régime en place, et à partir de cette date, le pays vit de grandes difficultés économiques.
                                                                                                                                       
                                                                                                                                       Le but de cette conférence est d'inviter plusieurs acteurs internationaux pour investir en Tunisie, le gouvernement lançant plusieurs projets pour en discuter.
                                                                                                                                       
                                                                                                                                       
                                                                                                                                       Cette application met en oeuvre quelques visiualisations pour comprendre la répartition des projets selon les gouvernorats , les secteurs et les activités.")),
              
              fluidRow(
                # A static infoBox
                infoBox("Gouvernorat", 24, icon = icon("list"),fill=TRUE,color = "red"),
                infoBox("Projets", 141, icon = icon("credit-card"),fill=TRUE,color = "red"),
                infoBox("Coût",65149, icon = icon("credit-card"),fill=TRUE,color = "red")
                
              )
              ,box(title = "Nombre de projet par gouvernorat",status = "info",solidHeader = TRUE,collapsible = TRUE,width = 6,
                   fluidPage(
                     leafletOutput("mymap")
                   )
              ),
              box(title = "Coût de projet par gouvernorat",status = "info",solidHeader = TRUE,collapsible = TRUE,width = 6,
                  fluidPage(
                    leafletOutput("mymap1")
                  )
              )
      ),
      
      tabItem(tabName ="Data",
              fluidPage(
                titlePanel("Basic DataTable"),
                
                # Create a new Row in the UI for selectInputs
                fluidRow(
                  column(4,
                         selectInput("soc",
                                     "Activité:",
                                     c("All",
                                       unique(as.character(tab1$activity))))
                  ),
                  column(4,
                         selectInput("prof",
                                     "secteur:",
                                     c("All",
                                       unique(as.character(tab1$sector))))
                  ),
                  column(4,
                         selectInput("stat",
                                     "Gouvernorat:",
                                     c("All",
                                       unique(as.character(tab1$governorate))))
                  )
                ),
                # Create a new row for the table.
                fluidRow(
                  DT::dataTableOutput("table")
                )
              )
      ),
      
      
      tabItem(tabName = "Contact", h4("Yasmine Ben Taarit Elève ingénieurr à l'Ecole Supérieur de Statistique et de l'Analyse de l'Information
                                      "), fluidRow(
                                        # A static infoBox
                                        infoBox("Email", h6("
                                                            yasminebentaarit123@gmail.com"), icon = icon("credit-card"),fill=TRUE,color="red")
                                        
                                        )),
      
      tabItem(tabName = "Statistique_Descriptive",
              box(title = "sunburstPlot",status = "warning",solidHeader = TRUE,collapsible = TRUE,width = 12,collapsed = TRUE, 
                  # Use a fluid Bootstrap layout
                  fluidPage(    
                    
                    # Give the page a title
                    titlePanel("choisir une variable"),
                    
                    
                    # Create a spot for the barplot
                    mainPanel(
                      sunburstOutput("sunburstPlot", height = "500", width = "150%")
                    )
                    
                  )
              ),
              box(title = "Répartition selon le cout",status = "warning",solidHeader = TRUE,collapsible = TRUE,width = 12, collapsed = TRUE,  
                  # Use a fluid Bootstrap layout
                  fluidPage(    
                    
                    # Give the page a title
                    titlePanel("choisir une variable"),
                    
                    # Generate a row with a sidebar
                    sidebarLayout(      
                      
                      # Define the sidebar with one input
                      sidebarPanel(
                        selectInput("type1", "Type:", 
                                    choices=colnames(tab1[,c(2,3)])),
                        hr(),
                        helpText("Repartition selon secteur ou activités")
                        
                      ),
                      
                      # Create a spot for the barplot
                      mainPanel(
                        plotOutput("tunisiaPlot1")  
                      )
                      
                    )
                  )
                  
              ),
              
              
              box(title = "Répartition selon le nombre de projets",status = "warning",solidHeader = TRUE,collapsible = TRUE,width = 12,   collapsed = TRUE,
                  # Use a fluid Bootstrap layout
                  fluidPage(    
                    
                    # Give the page a title
                    titlePanel("choisir une variable"),
                    
                    # Generate a row with a sidebar
                    sidebarLayout(      
                      
                      # Define the sidebar with one input
                      sidebarPanel(
                        selectInput("type", "Type:", 
                                    choices=colnames(tab1[,c(2,3)])),
                        hr(),
                        helpText("Repartition selon secteur ou activités")
                        
                      ),
                      
                      
                      
                      # Create a spot for the barplot
                      mainPanel(
                        plotOutput("tunisiaPlot")  
                      )
                      
                    )
                  )
                  
              )
      )
      )
    
    
      ))

server <- function(input, output) {
  
  output$table <- DT::renderDataTable(DT::datatable({
    data<-tab1
    
    if (input$soc != "All") {
      data <- data[tab1$activity== input$soc,]
    }
    if (input$prof != "All") {
      data <- data[tab1$sector == input$prof,]
    }
    if (input$stat != "All") {
      data <- data[tab1$governorate == input$stat,]
    }
    data
  }))
  fdc <- readShapePoly("C:/Users/User/Desktop/projet_tunisia2020 - Copie/Tunisie_snuts4.shp")
  i=match(fdc1$HASC_1,Datafrayme$gov1.1)
  xn=fdc1a@data$nbr1
  output$mymap <- renderLeaflet({
    MyPaletteColor <- colorQuantile("Reds", NULL, 4)
    my_popup <- paste0("<strong>",fdc1a@data$L,"</strong>",xn,"Projet")
    nn<-leaflet(data = fdc1a) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(fillColor = ~MyPaletteColor(xv), 
                  fillOpacity = 0.8, 
                  color = "#BDBDC3", 
                  weight = 1, 
                  popup = my_popup)
    pal=colorNumeric(palette = "Reds",domain = nbr1) 
    nn %>%   addLegend("bottomright", pal = pal, values = ~nbr1,
                       title = "Projet",
                       labFormat = labelFormat(prefix = ""),
                       opacity = 1
    )
  })
  xv=fdc1a@data$Total
  output$mymap1 <- renderLeaflet({
    MyPaletteColor <- colorQuantile("Reds", NULL, 8)
    my_popup <- paste0("<strong>",fdc1a@data$L,"</strong>",xv,"MDT")
    mm<-leaflet(data = fdc1a) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(fillColor = ~MyPaletteColor(xv), 
                  fillOpacity = 0.8, 
                  color = "#BDBDC3", 
                  weight = 1, 
                  popup = my_popup)
    pal=colorNumeric(palette = "Reds",domain = Total) 
    mm %>%   addLegend("bottomright", pal = pal, values = ~Total,
                       title = "Cost.MDT",
                       labFormat = labelFormat(prefix = "MDT"),
                       opacity = 1
    )
    
  })
  output$tunisiaPlot <- reactivePlot(function() {
    # check for the input variable
    if (input$type == "activity") {
      
      p<-ggplot(tab1)+geom_bar(aes(x=governorate,fill=activity))+theme(axis.text.x = element_text(angle=65,vjust = 0.6))
    }
    else {
      p<-ggplot(tab1)+geom_bar(aes(x=governorate,fill=sector))+theme(axis.text.x = element_text(angle=65,vjust = 0.6))
    }
    
    print(p)
  }
  
  )
  output$tunisiaPlot1 <- reactivePlot(function() {
    # check for the input variable
    if (input$type1 == "activity") {
      g=ggplot(tab1, aes(x=activity, y=cost.MDT)) + geom_bar(stat="identity", width=.5, fill="tomato3") + labs(title="Ordered Bar Chart", subtitle="Répartition des couts par activité") + theme(axis.text.x = element_text(angle=65, vjust=0.6))
    }
    else {
      g=ggplot(tab1, aes(x=sector, y=cost.MDT)) + geom_bar(stat="identity", width=.5, fill="tomato3") + labs(title="Ordered Bar Chart", subtitle="Répartition des couts par secteur") + theme(axis.text.x = element_text(angle=65, vjust=0.6))    }
    
    print(g)
  }
  
  )
  library(sunburstR)
  library(plyr)
  output$sunburstPlot <- renderSunburst({ 
    sunburst(A[,c(5,4)],count=TRUE)
  })
}


shinyApp(ui, server)