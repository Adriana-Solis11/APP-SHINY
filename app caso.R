library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(plotly)
library(randomForest)

# -----------------------------------------------------------------------------
# 1. Carga y preparación del dataset (Car Evaluation)
# -----------------------------------------------------------------------------
# Intentamos descargar el dataset original directamente de UCI Machine Learning Repository
# Si no hay conexión, se genera una pequeña muestra simulada.
url_data <- "https://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data"

col_names <- c("buying", "maint", "doors", "persons", "lug_boot", "safety", "class")

car_data <- tryCatch({
  read.csv(url_data, header = FALSE, col.names = col_names, stringsAsFactors = TRUE)
}, error = function(e) {
  # Dataset alternativo por si no hay conexión externa al ejecutar
  expand.grid(
    buying = factor(c("vhigh", "high", "med", "low")),
    maint = factor(c("vhigh", "high", "med", "low")),
    doors = factor(c("2", "3", "4", "5more")),
    persons = factor(c("2", "4", "more")),
    lug_boot = factor(c("small", "med", "big")),
    safety = factor(c("low", "med", "high"))
  ) %>% mutate(class = factor(sample(c("unacc", "acc", "good", "vgood"), n(), replace = TRUE)))
})

# Entrenar un modelo de prueba con RandomForest
set.seed(123)
rf_model <- randomForest(class ~ ., data = car_data, ntree = 100)

# -----------------------------------------------------------------------------
# 2. Interfaz de Usuario (UI)
# -----------------------------------------------------------------------------
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Car Evaluation Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(" Resumen & Datos", tabName = "data", icon = icon("table")),
      menuItem(" Análisis Exploratorio (EDA)", tabName = "eda", icon = icon("chart-bar")),
      menuItem(" Predicción con ML", tabName = "predict", icon = icon("robot"))
    )
  ),
  dashboardBody(
    tabItems(
      # --- TAB 1: Datos y Resumen ---
      tabItem(
        tabName = "data",
        fluidRow(
          valueBoxOutput("total_records", width = 3),
          valueBoxOutput("unacc_count", width = 3),
          valueBoxOutput("acc_count", width = 3),
          valueBoxOutput("vgood_count", width = 3)
        ),
        fluidRow(
          box(
            title = "Filtros de Búsqueda", width = 12, status = "primary", solidHeader = TRUE,
            fluidRow(
              column(4, selectInput("filter_buying", "Precio de Compra:", choices = c("Todos", levels(car_data$buying)))),
              column(4, selectInput("filter_safety", "Seguridad:", choices = c("Todos", levels(car_data$safety)))),
              column(4, selectInput("filter_class", "Evaluación Final:", choices = c("Todos", levels(car_data$class))))
            )
          )
        ),
        fluidRow(
          box(
            title = "Dataset Car Evaluation", width = 12, status = "primary",
            DTOutput("car_table")
          )
        )
      ),
      
      # --- TAB 2: Gráficos EDA ---
      tabItem(
        tabName = "eda",
        fluidRow(
          box(
            title = "Distribución de Evaluaciones de Autos", width = 6, status = "info", solidHeader = TRUE,
            plotlyOutput("plot_class_dist")
          ),
          box(
            title = "Evaluación según la Seguridad", width = 6, status = "info", solidHeader = TRUE,
            plotlyOutput("plot_safety_class")
          )
        ),
        fluidRow(
          box(
            title = "Relación Precio de Compra vs Evaluación", width = 12, status = "info", solidHeader = TRUE,
            plotlyOutput("plot_buying_class")
          )
        )
      ),
      
      # --- TAB 3: Predicción con ML ---
      tabItem(
        tabName = "predict",
        fluidRow(
          box(
            title = "Selecciona las características del vehículo", width = 6, status = "warning", solidHeader = TRUE,
            selectInput("p_buying", "Precio de compra (Buying):", choices = levels(car_data$buying)),
            selectInput("p_maint", "Costo de mantenimiento (Maint):", choices = levels(car_data$maint)),
            selectInput("p_doors", "Número de puertas (Doors):", choices = levels(car_data$doors)),
            selectInput("p_persons", "Capacidad de personas (Persons):", choices = levels(car_data$persons)),
            selectInput("p_lug_boot", "Tamaño del maletero (Lug Boot):", choices = levels(car_data$lug_boot)),
            selectInput("p_safety", "Nivel de seguridad (Safety):", choices = levels(car_data$safety)),
            actionButton("btn_predict", "Calcular Evaluación", class = "btn-warning btn-block")
          ),
          box(
            title = "Resultado de la Evaluación", width = 6, status = "success", solidHeader = TRUE,
            div(style = "text-align: center; padding: 20px;",
                h3("Resultado Estimado:"),
                h1(textOutput("pred_result"), style = "color: #00a65a; font-weight: bold;"),
                hr(),
                h4("Probabilidades por Categoría:"),
                tableOutput("pred_probs")
            )
          )
        )
      )
    )
  )
)

# -----------------------------------------------------------------------------
# 3. Lógica del Servidor (Server)
# -----------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Dataset filtrado dinámicamente
  filtered_data <- reactive({
    df <- car_data
    if (input$filter_buying != "Todos") df <- df %>% filter(buying == input$filter_buying)
    if (input$filter_safety != "Todos") df <- df %>% filter(safety == input$filter_safety)
    if (input$filter_class != "Todos") df <- df %>% filter(class == input$filter_class)
    df
  })
  
  # ValueBoxes informativos
  output$total_records <- renderValueBox({
    valueBox(nrow(car_data), "Total Registros", icon = icon("car"), color = "blue")
  })
  output$unacc_count <- renderValueBox({
    valueBox(sum(car_data$class == "unacc"), "Inaceptables (unacc)", icon = icon("thumbs-down"), color = "red")
  })
  output$acc_count <- renderValueBox({
    valueBox(sum(car_data$class == "acc"), "Aceptables (acc)", icon = icon("thumbs-up"), color = "yellow")
  })
  output$vgood_count <- renderValueBox({
    valueBox(sum(car_data$class == "vgood"), "Muy Buenos (vgood)", icon = icon("star"), color = "green")
  })
  
  # Tabla interactiva
  output$car_table <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 10, scrollX = TRUE), filter = "top")
  })
  
  # Gráficos de EDA con Plotly
  output$plot_class_dist <- renderPlotly({
    p <- ggplot(car_data, aes(x = class, fill = class)) +
      geom_bar() +
      theme_minimal() +
      labs(x = "Clase", y = "Cantidad") +
      scale_fill_brewer(palette = "Set2")
    ggplotly(p)
  })
  
  output$plot_safety_class <- renderPlotly({
    p <- ggplot(car_data, aes(x = safety, fill = class)) +
      geom_bar(position = "fill") +
      theme_minimal() +
      labs(x = "Nivel de Seguridad", y = "Proporción") +
      scale_fill_brewer(palette = "Set3")
    ggplotly(p)
  })
  
  output$plot_buying_class <- renderPlotly({
    p <- ggplot(car_data, aes(x = buying, fill = class)) +
      geom_bar(position = "dodge") +
      theme_minimal() +
      labs(x = "Precio de Compra", y = "Cantidad") +
      scale_fill_brewer(palette = "Spectral")
    ggplotly(p)
  })
  
  # Predicción ML
  prediction_data <- eventReactive(input$btn_predict, {
    new_sample <- data.frame(
      buying = factor(input$p_buying, levels = levels(car_data$buying)),
      maint = factor(input$p_maint, levels = levels(car_data$maint)),
      doors = factor(input$p_doors, levels = levels(car_data$doors)),
      persons = factor(input$p_persons, levels = levels(car_data$persons)),
      lug_boot = factor(input$p_lug_boot, levels = levels(car_data$lug_boot)),
      safety = factor(input$p_safety, levels = levels(car_data$safety))
    )
    
    pred_class <- predict(rf_model, new_sample)
    pred_prob <- predict(rf_model, new_sample, type = "prob")
    
    list(class = as.character(pred_class), probs = pred_prob)
  }, ignoreNULL = FALSE)
  
  output$pred_result <- renderText({
    res <- prediction_data()
    toupper(res$class)
  })
  
  output$pred_probs <- renderTable({
    res <- prediction_data()
    probs_df <- as.data.frame(res$probs) * 100
    colnames(probs_df) <- c("Aceptable (acc)", "Bueno (good)", "Inaceptable (unacc)", "Muy Bueno (vgood)")
    probs_df
  }, digits = 2)
}

# -----------------------------------------------------------------------------
# 4. Lanzar Aplicación
# -----------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
