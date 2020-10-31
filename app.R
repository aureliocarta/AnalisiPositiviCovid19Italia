

# Librerie
require(shiny)
require(tidyverse)
require(shinythemes)
require(shinydashboard) 
require(shinyWidgets)
require(ggplot2) 
require(plotly)
require(lubridate)
require(sf)
require(leaflet)


# Fonti
source("dati.R")
source("funzioni.R")





ui <- navbarPage(
  theme = shinytheme("united"),
  #themeSelector() per vedere il selettore di temi,
  title = 'Analisi positivi al Covid-19 in Italia',
  # First tab: Overview
  
  tabPanel(
    title = "App Desktop",
    icon = icon("desktop"),
    
    
    div(style = "margin-top:-0.5em"),
    
    
    
    
    
    tags$style(HTML("hr {border-top: 1px solid #000000}")),
    tags$hr(),
    
    sidebarLayout(
      sidebarPanel(
        width = 12,
        h3("Seleziona la data:"),
        br(),
        
        
        dateInput(inputId="date", label = NULL,  format="dd/mm/yyyy",
                  language="it", min = min(reg$data), value = max(reg$data), )
      ),
    
    mainPanel( width = 12,
               useShinydashboard(),
               includeCSS("color.css"),
               fluidRow(
                 valueBoxOutput("isolamento"),
                 valueBoxOutput("ricoverati"),
                 valueBoxOutput("terapia"),
                 
                 style = "text-align:justify;padding:20px;"
               ),
               fluidRow(
                 box(width=4,addSpinner(plotlyOutput(outputId = "map1"), spin = "fading-circle", color = "#375a7f")),
                     box(width=4,addSpinner(plotlyOutput(outputId = "map2"), spin = "fading-circle", color = "#375a7f")),
                         box(width=4,addSpinner(plotlyOutput(outputId = "map3"), spin = "fading-circle", color = "#375a7f"))
               )
    )
  ),
    
    
    
    tags$hr(),
    
    sidebarLayout(
      sidebarPanel(
        width = 12,
        selectInput(
          "denominazione",
          h3("Seleziona il territorio:"),
          choices =  c("Italia", sort(unique(reg$denominazione_regione))),
          selected = "Italia"
        )
      ),
      mainPanel(width = 12,
                
                fluidRow(
                  box(width=4,addSpinner(plotlyOutput(outputId = "pointline1"), spin = "fading-circle", color = "#375a7f")),
                  box(width=4,addSpinner(plotlyOutput(outputId = "pointline2"), spin = "fading-circle", color = "#375a7f")),
                  box(width=4,addSpinner(plotlyOutput(outputId = "pointline3"), spin = "fading-circle", color = "#375a7f"))
                  ) , 
                  
                  br(),
                  br(),
                  br(),
                  
                  
                  
                  fluidRow(
                    box(width=12,addSpinner(plotlyOutput(outputId = "Bar1"), spin = "fading-circle", color = "#375a7f"))))
    )
  )
  ,

      
  tabPanel(
    title = "Info",
    icon = icon("info"),
    
    
    
    
    div(style = "margin-top:-0.5em"),
    
    
    
    
    
    tags$style(HTML("hr {border-top: 1px solid #000000}")),
    tags$hr(),
    div(includeMarkdown("info.Rmd"))
  )
)

server = function(input, output) {
  tdy <- reactive({
    max(ita$data)
  })
  
  tod_summary <- reactive({
    situazione_odierna(da = ita, data = input$date)
  })
  
  
  # Value boxes
  output$isolamento <- renderValueBox({
    valueBox(
      value = prettyNum(tod_summary()$`Isolamento domiciliare`[1], big.mark = " "),
      subtitle = HTML(
        paste0(
          "<b>Isolamento domiciliare</b>"
        )
      ),
      icon = icon("home"),
      #color = "orange"
    )
  })
  
  
  
  output$ricoverati <- renderValueBox({
    valueBox(
      value = prettyNum(tod_summary()$`Ricoverati con sintomi`[1], big.mark = " "),
      subtitle = HTML(
        paste0(
          "<b>Ricoverati con sintomi</b"
        )
      ),
      icon = icon("hospital"),
      #color = "red"
    )
  })
  
  
  output$terapia <- renderValueBox({
    valueBox(
      value = prettyNum(tod_summary()$`Terapia intensiva`[1], big.mark = " "),
      subtitle = HTML(
        paste0(
          "<b>Terapia intensiva</b>"
        )
      ),
      icon = icon("procedures"),
      #color = "maroon"
    )
  })
  

  
  # Map
  
  filtered_data2 <- reactive({
    dplyr::filter(ita2, ita2$data  == input$date)
  })
  
  
  map_reg <- reactive({
    regshp %>% 
      sp::merge(filtered_data2(), by.y = "denominazione_regione", by.x = "DEN_REG") 
  })
  
  output$map1 <- renderPlotly({
    crp <- colorRampPalette(c("#ffffff", "#8BC24A"))
    plot_ly(data = map_reg(),  stroke = I("black"), span = I(1), split = ~DEN_REG, color = ~isolamento_domiciliare, colors = crp(9) ,  
            alpha = 1, type = "scatter",
            text = ~paste0(DEN_REG, "\n", isolamento_domiciliare), hoveron = "fills", hoverinfo = "text", showlegend = F) %>%
      
      colorbar( title = "Isolamento domiciliare") 
    
    
  })
  
  output$map2 <- renderPlotly({
    crp <- colorRampPalette(c("#ffffff", "#2196F3"))
    plot_ly(data = map_reg(),  stroke = I("black"), span = I(1), split = ~DEN_REG, color = ~ricoverati_con_sintomi, colors = crp(9) ,  
            alpha = 1, type = "scatter",
            text = ~paste0(DEN_REG, "\n", ricoverati_con_sintomi), hoveron = "fills", hoverinfo = "text", showlegend = F) %>%
      
      colorbar( title = "Ricoverati con sintomi") 
  })
  
  output$map3 <- renderPlotly({
    crp <- colorRampPalette(c("#ffffff", "#F44236"))
    plot_ly(data = map_reg(),  stroke = I("black"), span = I(1), split = ~DEN_REG, color = ~terapia_intensiva, colors = crp(9),
            alpha = 1, type = "scatter",
            text = ~paste0(DEN_REG, "\n", terapia_intensiva), hoveron = "fills", hoverinfo = "text", showlegend = F) %>%
      
      colorbar( title = "Terapia intensiva")
  })
  
  
  


filtered_data <- reactive({
  dplyr::filter(ita2, ita2$denominazione_regione  == input$denominazione)
})



#plot
output$pointline1 <- renderPlotly({
  plot_ly(
    filtered_data(),
    x = ~ data,
    y = ~ isolamento_domiciliare,
    type = 'scatter',
    mode = 'lines+markers',
    name = 'Isolamento domiciliare',
    color = I("#8BC24A")
  ) %>%
    layout(
      title = paste(
        "Serie storica isolamento domiciliare:",
        input$denominazione
      ),
      yaxis = list(title = 'Pazienti'),
      xaxis = list(title = '')
    )
  
})

output$pointline2 <- renderPlotly({
  plot_ly(
    filtered_data(),
    x = ~ data,
    y = ~ ricoverati_con_sintomi,
    type = 'scatter',
    mode = 'lines+markers',
    name = 'Ricoverati con sintomi',
    color = I("#2196F3")
  ) %>%
    layout(
      title = paste(
        "Serie storica ricoverati con sintomi:",
        input$denominazione
      ),
      yaxis = list(title = 'Pazienti'),
      xaxis = list(title = '')
    )
  
})

output$pointline3 <- renderPlotly({
  plot_ly(
    filtered_data(),
    x = ~ data,
    y = ~ terapia_intensiva,
    type = 'scatter',
    mode = 'lines+markers',
    name = 'Terapia intensiva',
    color = I("#F44236")
  ) %>%
    layout(
      title = paste(
        "Serie storica terapia intensiva: ",
        input$denominazione
      ),
      yaxis = list(title = 'Pazienti'),
      xaxis = list(title = '')
    )
  
})


output$Bar1 <- renderPlotly({
  plot_ly(
    filtered_data(),
    x = ~ data,
    y = ~ terapia_intensiva,
    type = 'bar',
    name = 'Terapia intensiva',
    color = I("#F44236")
  )  %>%
    add_trace(
      y = ~ ricoverati_con_sintomi,
      name = 'Ricoverati con sintomi',
      color = I("#2196F3")
    ) %>%
    add_trace(
      y = ~ isolamento_domiciliare,
      name = 'Isolamento domiciliare',
      color = I("#8BC34A")
    )  %>%
    layout(
      title = paste(
        "Distribuzione attualmente positivi nel tempo: ",
        input$denominazione
      ),
      yaxis = list(title = 'Pazienti'),
      xaxis = list(title = ''),
      barmode = 'stack'
    )
  
})

}

shiny::addResourcePath('www', here::here("www"))
shinyApp(ui, server)

#Copia e incolla nella console per vedere il codice
#runApp("app.R", display.mode = "showcase")
