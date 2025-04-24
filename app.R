library(shiny)
library(caret)
library(dplyr)
library(purrr)  # For compact() function

# Set proper contrasts
options(contrasts = c(unordered = "contr.treatment", ordered = "contr.poly"))

# Load all required objects
load("model.rda")
load("dummymodel.rda")
load("factor_levels.rda")  # Load the saved factor levels

ui <- fluidPage(
  titlePanel("LAPD Crime Prediction"),
  sidebarLayout(
    sidebarPanel(
      numericInput("year", "Year:", 2023, min = 2000, max = 2025),
      selectInput("time_bin", "Time Bin:", choices = factor_levels$TIME_OCC_BIN),
      numericInput("time_since", "No. of days since:", 1, min = 0, max = 3000),
      selectInput("area", "Area:", choices = factor_levels$AREA_NAME),
      selectInput("crime", "Crime:", choices = factor_levels$Crime_Desc_v),
      selectInput("sex", "Victim Sex:", choices = factor_levels$Vict_Sex),
      selectInput("descent", "Descent:", choices = factor_levels$Vict_Descent_Grp),
      numericInput("age", "Age:", 30, min = 0, max = 120),
      selectInput("premis", "Premise:", choices = factor_levels$Premis_Desc_v),
      actionButton("predict", "Predict")
    ),
    mainPanel(
      h3("Prediction Result"),
      verbatimTextOutput("result")
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$predict, {
    # Create input data frame with correct factor levels
    new_data <- data.frame(
      YEAR_OCC = input$year,
      TIME_OCC_BIN = factor(input$time_bin, levels = factor_levels$TIME_OCC_BIN),
      TIME_SINCE_OCC = input$time_since,
      AREA_NAME = factor(input$area, levels = factor_levels$AREA_NAME),
      Crime_Desc_v = factor(input$crime, levels = factor_levels$Crime_Desc_v),
      Vict_Sex = factor(input$sex, levels = factor_levels$Vict_Sex),
      Vict_Descent_Grp = factor(input$descent, levels = factor_levels$Vict_Descent_Grp),
      Vict_Age = input$age,
      Premis_Desc_v = factor(input$premis, levels = factor_levels$Premis_Desc_v)
    )
    
    # Convert to dummy variables
    pred_data <- predict(dummy_model, new_data) |> as.data.frame()
    
    # Ensure all required columns are present
    #required_cols <- colnames(model$trainingData)
    #missing_cols <- setdiff(required_cols, colnames(pred_data))
    #if (length(missing_cols) > 0) {
    #  pred_data[missing_cols] <- 0  # Add missing columns with zeros
    #}
    
    # Make prediction
    prob <- predict(model, newdata = pred_data, type = "prob")[, "Arrest"]
    #prob <- ifelse(prob>0.1, prob, prob*10)
    prediction <- ifelse(prob > 0.136, "Arrest", "No Arrest")
    
    output$result <- renderPrint({
      list(
        Probability = round(prob, 4),
        Prediction = prediction
        #Missing_Columns_Handled = if (length(missing_cols) > 0) 
          #paste((missing_cols), "columns set to zero") else "All columns present"
      )
    })
  })
}

shinyApp(ui, server)