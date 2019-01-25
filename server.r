source("helper.R")
library("dplyr")
library("ggplot2")
library("plotly")
library("shiny")
library("shinydashboard")
library("leaflet")


server <- function(input, output, ...) {

  output$distPlot <- renderPlotly({
    if(input$radio == "Temperatura"){
      x1 = filter(cristina, Departamento == input$sel)
      x1$average <- rowMeans(x1[,c("High", "Low")])  
   p <- plot_ly(x1, x = x1$Ch_Mes, y = x1$High,type = 'scatter', mode = 'lines',
                line = list(color = 'transparent'),
                showlegend = FALSE, name = 'High') %>%
     add_trace(y = x1$Low, type = 'scatter', mode = 'lines',
               fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
               showlegend = FALSE, name = 'Low') %>%
     add_trace(x = x1$Ch_Mes, y = x1$Tnclim, type = 'scatter', mode = 'lines',
               line = list(color='rgb(0,100,80)'),
               name = 'average') %>%
    add_trace(x = x1$Ch_Mes, y = x1$alto,type = 'scatter', mode = 'lines',
              line = list(color = 'transparent'),
              showlegend = FALSE, name = 'High') %>%
    add_trace(y = x1$bajo, type = 'scatter', mode = 'lines',
              fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
              showlegend = FALSE, name = 'Low') %>%
    add_trace(x = x1$Ch_Mes, y = x1$average, type = 'scatter', mode = 'lines',
              line = list(color='rgb(0,100,80)'),
              name = 'average') %>%
     layout(paper_bgcolor='rgb(255,255,255)', plot_bgcolor='rgb(229,229,229)',
           xaxis = list(title = "Meses",
                        gridcolor = 'rgb(255,255,255)',
                         showgrid = TRUE,
                         showline = FALSE,
                         showticklabels = TRUE,
                         tickcolor = 'rgb(127,127,127)',
                         ticks = 'outside',
                         zeroline = FALSE),
            yaxis = list(title = "Temperatura (grados Celsius)",
                         gridcolor = 'rgb(255,255,255)',
                         showgrid = TRUE,
                         showline = FALSE,
                         showticklabels = TRUE,
                         tickcolor = 'rgb(127,127,127)',
                         ticks = 'outside',
                         zeroline = FALSE))
          p$elementId <- NULL
          p
    }else if(input$radio == "Precipitacion"){
      x2 = filter(prueba, Departamento == input$sel)
      p1 <- ggplot(x2, aes(x = Ch_Mes, y = Precs)) + geom_boxplot()
      p1 + xlab('Meses') + ylab('Precipitacion')
    }else{
       print("Seleccione un tipo de grafica")
        
    }
   })
    
    output$brun <- renderTable({
    and <- rbind(filter(var_fe, Departamento == input$sel),filter(var_fi,
    Departamento == input$sel),filter(var_vi, Departamento == input$sel))
    and = select(and, Variable,Media, Max, Min, Mediana, Desv.est, Q1, Q3)
  })
    output$dista <- renderText({
if (input$sel == 'Lima'){
  'Lima'}else if(input$sel == 'Ucayali'){ 'Pucallpa'}else if(input$sel == 'Junin'){
    'Huancayo'}else if(input$sel == 'Huanuco'){'Huanuco'}else if(input$sel == 'Ancash'){
      'Huaraz'
    }
      
     })
    output$holo <- renderText({
      if (input$sel == 'Lima'){
        'Nana'}else if(input$sel == 'Ucayali'){ 'El Maronal'}else if(input$sel == 'Junin'){
          'Santa Ana'}else if(input$sel == 'Huanuco'){'Huanuco'}else if(input$sel == 'Ancash'){
            'Recuay'
          }
    })
    output$mymap <- renderLeaflet({
      m <- leaflet()
      m <- setView(m,lng = -76, lat = -10, zoom = 4.8)
      m <- addTiles(m)
      m
    })
    observe({
      adicta = filter(adriana, Nombre == input$sel)
      proxy <- leafletProxy("mymap", data = adicta)
      proxy <- setView(proxy, lng = adicta$Lon, lat = adicta$Lat, zoom = 9)
      proxy <- addMarkers(proxy, lng= adicta$Lon, lat=adicta$Lat,popup= adicta$Estacion)
    })
    }
    
 
 

