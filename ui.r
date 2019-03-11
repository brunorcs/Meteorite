library("shiny")
library("shinydashboard")
source("helper.R")
library("plotly")
library("leaflet")
ui <-dashboardPage(
  
  #Cabecera el dashboard
  dashboardHeader(
    
    title = 'AED',
    titleWidth = 150
    
    ),
  
  #Menu lateral
  dashboardSidebar(
    
    width = 150,
    
    sidebarMenu(
      menuItem('Climatologia', tabName = 'clima', icon = icon('fas fa-book'))
      )
    ),
    dashboardBody(
      
      tabItems(
        
        #Analisis espacial
        tabItem(tabName = 'clima',
                fluidRow(
                  box(width = 5,title = 'Ubicación Geográfica',leafletOutput("mymap")), #leaflet
                  box(width = 5,title = 'Gráfica',
                      plotlyOutput(outputId = "distPlot")
                  ),
                  box(
                    width = 2,
                    selectInput(inputId = 'sel',
                                label = 'Seleccione un departamento',
                                choices = stn ,
                                selected = "Lima"
                    ),
                    h3('Capital:'),
                    verbatimTextOutput(outputId = 'dista', placeholder = TRUE),
                    h3('Estación:'),
                    verbatimTextOutput(outputId = 'holo', placeholder = TRUE),
                    radioButtons("radio", h3("Tipo de gráfica"),
                                 choices = Graf,selected = "Temperatura")
                      ),
                  
                box(width = 6, title = 'Estadísticos', side = 'center',
                      tableOutput(outputId = 'brun')
                  )
                               )
                  )
                )
    )
)

        
      
  

