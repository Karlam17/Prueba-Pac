library(shiny)
library(tidyverse)
library(readxl)
library(plotly)
library(ggplot2)
library(DT)
library(reshape2)
library(readxl)

# UI del dashboard
ui <- fluidPage(
  titlePanel(
    div(
      h1("Determinantes de la Expectativa de Vida en América del Sur (2000–2022)"),
      p("Mostrar cómo distintos factores sociales, económicos y sanitarios están asociados con la expectativa de vida en países de América del Sur entre 2000 y 2022.")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("pais", "Seleccione país o países:", choices = NULL, multiple = TRUE),
      sliderInput("rango_anios", "Seleccione rango de años:", min = 2000, max = 2022, value = c(2000, 2022), sep = ""),
      selectInput("tipo_grafico", "Tipo de gráfico:", choices = c("Línea", "Dispersión", "Barras", "Pastel", "Mapa")),
      downloadButton("descargar_datos", "Descargar datos filtrados")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Expectativa de vida (línea)",
                 plotlyOutput("linea_expectativa"),
                 textOutput("resumen_panel")),
        
        tabPanel("Dispersión (determinantes)",
                 plotlyOutput("scatter_fertilidad"),
                 plotlyOutput("scatter_mortalidad"),
                 plotlyOutput("scatter_pib"),
                 plotlyOutput("scatter_vacuna"),
                 plotlyOutput("scatter_urbano")),
        
        tabPanel("Gráficos de Barras y Pastel",
                 plotlyOutput("grafico_barras"),
                 plotlyOutput("grafico_pastel")),
        
        tabPanel("Matriz de correlación",
                 plotlyOutput("correlacion_plot")),
        
        tabPanel("KPIs",
                 verbatimTextOutput("kpis")),
        
        tabPanel("Datos",
                 dataTableOutput("tabla_datos"))
      )
    )
  )
)

# Server del dashboard
server <- function(input, output, session) {
  archivo <- "C:/Users/mercy/OneDrive/Escritorio/Maestria/Herramientas Informaticas/base_union.xlsx"
  base_union <- read_excel(archivo)
  
  # Asegúrate de que el campo "año" esté en formato numérico
  base_union$año <- as.numeric(base_union$año)
  
  updateSelectInput(session, "pais", choices = unique(base_union$pais))
  
  base_filtrada <- reactive({
    base_union %>% 
      filter(año >= input$rango_anios[1], año <= input$rango_anios[2]) %>%
      filter(if(length(input$pais) > 0) pais %in% input$pais else TRUE)
  })
  
  output$linea_expectativa <- renderPlotly({
    ggplotly(
      ggplot(base_filtrada(), aes(x = año, y = expectativa, color = pais)) +
        geom_line() +
        stat_summary(fun = mean, aes(group = 1), geom = "line", color = "black", linetype = "dashed") +
        labs(title = "Evolución de la expectativa de vida",
             x = "Año", y = "Expectativa de vida (años)") +
        theme_minimal()
    )
  })
  
  output$scatter_fertilidad <- renderPlotly({
    ggplotly(
      ggplot(base_filtrada(), aes(x = fertilidad, y = expectativa, color = pais, size = poblacion)) +
        geom_point(alpha = 0.7) +
        labs(title = "Expectativa de vida vs Tasa de fertilidad") +
        theme_minimal()
    )
  })
  
  output$scatter_mortalidad <- renderPlotly({
    ggplotly(
      ggplot(base_filtrada(), aes(x = mortalidad, y = expectativa, color = pais, size = poblacion)) +
        geom_point(alpha = 0.7) +
        labs(title = "Expectativa de vida vs Mortalidad infantil") +
        theme_minimal()
    )
  })
  
  output$scatter_pib <- renderPlotly({
    ggplotly(
      ggplot(base_filtrada(), aes(x = pib_pc, y = expectativa, color = pais, size = poblacion)) +
        geom_point(alpha = 0.7) +
        labs(title = "Expectativa de vida vs Ingreso per cápita") +
        theme_minimal()
    )
  })
  
  output$scatter_vacuna <- renderPlotly({
    ggplotly(
      ggplot(base_filtrada(), aes(x = vacuna, y = expectativa, color = pais, size = poblacion)) +
        geom_point(alpha = 0.7) +
        labs(title = "Expectativa de vida vs Tasa de vacunación") +
        theme_minimal()
    )
  })
  
  output$scatter_urbano <- renderPlotly({
    ggplotly(
      ggplot(base_filtrada(), aes(x = urbano, y = expectativa, color = pais, size = poblacion)) +
        geom_point(alpha = 0.7) +
        labs(title = "Expectativa de vida vs Población urbana") +
        theme_minimal()
    )
  })
  
  output$grafico_barras <- renderPlotly({
    datos <- base_filtrada() %>% 
      group_by(pais) %>% 
      summarise(promedio_vida = mean(expectativa, na.rm = TRUE))
    
    plot_ly(datos, x = ~pais, y = ~promedio_vida, type = 'bar', name = 'Expectativa promedio') %>%
      layout(title = "Expectativa de vida promedio por país",
             xaxis = list(title = "País"),
             yaxis = list(title = "Expectativa de vida"))
  })
  
  output$grafico_pastel <- renderPlotly({
    datos <- base_filtrada() %>% 
      group_by(pais) %>% 
      summarise(promedio_vida = mean(expectativa, na.rm = TRUE))
    
    plot_ly(datos, labels = ~pais, values = ~promedio_vida, type = 'pie') %>%
      layout(title = "Distribución de la expectativa de vida promedio por país")
  })
  
  output$correlacion_plot <- renderPlotly({
    datos_cor <- base_filtrada() %>% 
      select(expectativa, fertilidad, mortalidad, pib_pc, vacuna, urbano) %>%
      cor(use = "complete.obs")
    plot_ly(z = datos_cor, type = "heatmap", x = colnames(datos_cor), y = rownames(datos_cor), colorscale = "Viridis")
  })
  
  output$kpis <- renderPrint({
    datos <- base_filtrada()
    promedio <- mean(datos$expectativa, na.rm = TRUE)
    max_pais <- datos %>% filter(expectativa == max(expectativa, na.rm = TRUE)) %>% select(pais, expectativa) %>% slice(1)
    min_pais <- datos %>% filter(expectativa == min(expectativa, na.rm = TRUE)) %>% select(pais, expectativa) %>% slice(1)
    inicio <- datos %>% filter(año == input$rango_anios[1]) %>% summarise(avg = mean(expectativa, na.rm = TRUE))
    fin <- datos %>% filter(año == input$rango_anios[2]) %>% summarise(avg = mean(expectativa, na.rm = TRUE))
    cambio <- round(100 * (fin$avg - inicio$avg) / inicio$avg, 1)
    
    cat("Promedio regional de expectativa de vida:", round(promedio,1), "años\n")
    cat("País con mayor expectativa de vida:", max_pais$pais, "(", max_pais$expectativa, ")\n")
    cat("País con menor expectativa de vida:", min_pais$pais, "(", min_pais$expectativa, ")\n")
    cat("Cambio porcentual entre", input$rango_anios[1], "y", input$rango_anios[2], ":", cambio, "%\n")
  })
  
  output$tabla_datos <- renderDataTable({
    datatable(base_filtrada())
  })
  
  output$resumen_panel <- renderText({
    datos <- base_filtrada()
    if (length(input$pais) == 1) {
      pais_sel <- input$pais[1]
      ini <- datos %>% filter(año == input$rango_anios[1], pais == pais_sel) %>% pull(expectativa)
      fin <- datos %>% filter(año == input$rango_anios[2], pais == pais_sel) %>% pull(expectativa)
      delta <- round(fin - ini, 1)
      paste0("Entre ", input$rango_anios[1], " y ", input$rango_anios[2], ", la expectativa de vida en ", pais_sel,
             " cambió en ", delta, " años.")
    } else {
      "Seleccione un solo país para ver el resumen textual."
    }
  })
  
  output$descargar_datos <- downloadHandler(
    filename = function() {
      paste("datos_vida_filtrados", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(base_filtrada(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)