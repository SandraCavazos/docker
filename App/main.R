library(shinydashboard)
library(shiny)
library(dplyr)
library(tidyquant)
library(quantmod)
library(rvest)
library(dygraphs)
library(forecast)
library(lubridate)
library(timeDate)
library(DT)


#----- funciones -------
obtenerTickersSP500 <- function() {
  url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
  pagina <- read_html(url)
  tabla <- html_node(pagina, xpath = '//*[@id="constituents"]')
  df <- html_table(tabla)
  tickers <- df$Symbol
  return(tickers)
}

#-----------


#cramos las opciones de los tickers
tickers <- obtenerTickersSP500()
tickers <- sort(tickers)


#--------- Definición de la UI -------
ui <- dashboardPage(
  
  dashboardHeader(title = "Dashboard"),
  dashboardSidebar(
    selectInput("ticker", "Seleccione un Ticker:", choices = tickers)
  ),
  
  
  dashboardBody(
    dygraphOutput("precioCierre"),
    DTOutput("resumenDatos")
  )#fin del dashboardBody
  
  
)#fin ui
#---------


#------------- Definición del servidor ------------
server <- function(input, output) {
  
  
  
  output$precioCierre <- renderDygraph({

    datos <- getSymbols(input$ticker, src = "yahoo", auto.assign = FALSE)
    preciosCierre <- Cl(datos) # Obtén los precios de cierre
    
    # Convertir a serie de tiempo
    ts_datos <- ts(preciosCierre, frequency = 252) 
    
    # Ajustar un modelo Holt-Winters
    fit <- HoltWinters(ts_datos)
    
    # Pronosticar a un año (252 días de trading)
    forecasted_values <- forecast(fit, h = 252)
    forecasted_values <- as.data.frame(forecasted_values)
    ultimo_valor <- index(preciosCierre)[length(index(preciosCierre))] + days(1) 
    fecha_final <- ultimo_valor + days(365) 
    secuencia_timeDate <- as.timeDate(seq(ultimo_valor, fecha_final, by = "day"))
    dias_habiles <- isBizday(secuencia_timeDate)
    secuencia_dias_habiles <- secuencia_timeDate[dias_habiles]
    secuencia_dias_habiles <- secuencia_dias_habiles[1:252]
    secuencia_dias_habiles <- as.Date(secuencia_dias_habiles)
    
    #modifcamos el indice de la serie de tiempo
    forecasted_values_xts <- xts(forecasted_values, order.by = secuencia_dias_habiles)
    
    #unimos los datos de forecasted_values_xts y preciosCierre
    combined_xts <- merge.xts(preciosCierre, forecasted_values_xts)
    #cambiamos el nombre de la columna 1 a V1
    colnames(combined_xts)[1] <- c("V1")
    
    
    # Crear el gráfico dygraph
    dygraph_obj <- dygraph(combined_xts, main = paste0("Análisis de Pronóstico: ", input$ticker)) %>%
      dySeries("V1", label = "Datos Reales") %>%
      dySeries("Point.Forecast", label = "Pronóstico") %>%
      dySeries(name = c("Lo.80", "Point.Forecast", "Hi.80"), label = "Banda 80% Confianza") %>%
      dySeries(name = c("Lo.95", "Point.Forecast", "Hi.95"), label = "Banda 95% Confianza") %>%
      dyRangeSelector(height = 25, dateWindow = NULL) %>% # Hace que la leyenda siga el cursor
      dyLegend(show = "follow")
    
    dygraph_obj
    })#fin del renderPlot
  
  
  output$resumenDatos <- renderDataTable({
    datos <- getSymbols(input$ticker, src = "yahoo", auto.assign = FALSE)
    datos <- Cl(datos) # Obtén los precios de cierre
    
    # Calcula el crecimiento YoY y MoM
    fecha_mas_reciente <- format(index(last(datos)), "%Y-%m-%d")
    ultimo_precio <- coredata(last(datos))
    precio_hace_un_año <- coredata(datos[length(datos) - 252]    ) # Aprox. 252 días de trading en un año
    precio_hace_un_mes <- coredata(datos[length(datos) - 21]    ) # Aprox. 21 días de trading en un mes
    
    crecimiento_YoY <- (ultimo_precio / precio_hace_un_año - 1) * 100
    crecimiento_MoM <- (ultimo_precio / precio_hace_un_mes - 1) * 100
    
    
    media <- mean(datos, na.rm = TRUE)
    mediana <- median(datos, na.rm = TRUE)
    minimo <- min(datos, na.rm = TRUE)
    maximo <- max(datos, na.rm = TRUE)
    desviacion_std <- sd(datos, na.rm = TRUE)
    
    resumen <- data.frame(
      Metrica = c("Fecha más reciente", "Crecimiento YoY (%)", "Crecimiento MoM (%)", "Media", "Mediana", "Mínimo", "Máximo", "Desviación Estándar"),
      Valor = c(fecha_mas_reciente, crecimiento_YoY, crecimiento_MoM, media, mediana, minimo, maximo, desviacion_std)
    )
    
    
    resumen$Valor <- as.character(resumen$Valor) #
    
  
    resumen$Valor[-1] <- sapply(resumen$Valor[-1], function(x) {
      if (!grepl("[^0-9.-]", x)) { 
        return(sprintf("%.2f", as.numeric(x))) 
      } else {
        return(x)
      }
    })
    
   
    datatable(resumen, 
              options = list(
                paging = FALSE,  # Desactiva la paginación, ya que quieres eliminar "Showing 1 to 8 of 8 entries"
                searching = FALSE,  # Desactiva la caja de búsqueda
                lengthChange = FALSE,  # Desactiva la opción para cambiar la cantidad de filas mostradas
                info = FALSE  # Oculta el texto 
              ),
              rownames = FALSE # Desactiva los índices/numeros de fila automáticos
    )
    
   
  })#fin del renderDataTable
  
  
}#fin del server
#--------------------




# Ejecutar la aplicación
app <- shinyApp(ui = ui, server = server)

# Luego, usar runApp() con el objeto de aplicación
shiny::runApp(app = app, host = "0.0.0.0", port = 3838)