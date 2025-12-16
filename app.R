# ============================================================
# STRATIFIED SAMPLING – SHINY APPLICATION
# Proportional, Neyman & Optimised Allocation
# ============================================================

library(shiny)

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  
  titlePanel("Stratified Sampling – Sample Size Allocation"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Stratum Data"),
      
      numericInput("Nh1", "Nh (Stratum A)", value = 300, min = 1),
      numericInput("Sh1", "Sh (Stratum A)", value = 12, min = 0),
      numericInput("C1",  "Cost (Stratum A)", value = 1, min = 0.1),
      
      numericInput("Nh2", "Nh (Stratum B)", value = 500, min = 1),
      numericInput("Sh2", "Sh (Stratum B)", value = 20, min = 0),
      numericInput("C2",  "Cost (Stratum B)", value = 2, min = 0.1),
      
      numericInput("Nh3", "Nh (Stratum C)", value = 200, min = 1),
      numericInput("Sh3", "Sh (Stratum C)", value = 15, min = 0),
      numericInput("C3",  "Cost (Stratum C)", value = 3, min = 0.1),
      
      hr(),
      
      numericInput("d", "Allowable Error (d)", value = 2, min = 0.1),
      numericInput("conf", "Confidence Level", value = 0.95, min = 0.8, max = 0.99),
      
      actionButton("calc", "Calculate", class = "btn-primary")
    ),
    
    mainPanel(
      
      h4("Sample Size Summary"),
      tableOutput("summary"),
      
      hr(),
      
      h4("Visual Comparison"),
      plotOutput("plots")
    )
  )
)

# -----------------------------
# SERVER
# -----------------------------
server <- function(input, output) {
  
  results <- eventReactive(input$calc, {
    
    strata_data <- data.frame(
      Stratum = c("A", "B", "C"),
      Nh = c(input$Nh1, input$Nh2, input$Nh3),
      Sh = c(input$Sh1, input$Sh2, input$Sh3),
      Cost = c(input$C1, input$C2, input$C3)
    )
    
    N <- sum(strata_data$Nh)
    strata_data$Wh <- strata_data$Nh / N
    
    Z <- qnorm((1 + input$conf) / 2)
    d <- input$d
    
    # -------- Proportional Allocation --------
    n_prop <- ceiling((Z^2 * sum(strata_data$Wh * strata_data$Sh^2)) / d^2)
    strata_data$n_prop <- round(n_prop * strata_data$Wh)
    
    # -------- Neyman Allocation --------
    n_neyman <- ceiling((Z^2 * (sum(strata_data$Wh * strata_data$Sh))^2) / d^2)
    strata_data$n_neyman <- round(
      n_neyman * (strata_data$Wh * strata_data$Sh) /
        sum(strata_data$Wh * strata_data$Sh)
    )
    
    # -------- Optimised Allocation --------
    n_opt <- ceiling((Z^2 * (sum(strata_data$Wh * strata_data$Sh *
                                   sqrt(strata_data$Cost)))^2) / d^2)
    
    strata_data$n_opt <- round(
      n_opt * (strata_data$Wh * strata_data$Sh / sqrt(strata_data$Cost)) /
        sum(strata_data$Wh * strata_data$Sh / sqrt(strata_data$Cost))
    )
    
    strata_data
  })
  
  output$summary <- renderTable({
    req(results())
    results()[, c("Stratum", "Nh", "Sh", "Cost",
                  "n_prop", "n_neyman", "n_opt")]
  })
  
  output$plots <- renderPlot({
    req(results())
    
    par(mfrow = c(1, 3))
    
    barplot(results()$n_prop,
            names.arg = results()$Stratum,
            main = "Proportional",
            ylab = "Sample Size")
    
    barplot(results()$n_neyman,
            names.arg = results()$Stratum,
            main = "Neyman",
            ylab = "Sample Size")
    
    barplot(results()$n_opt,
            names.arg = results()$Stratum,
            main = "Optimised",
            ylab = "Sample Size")
    
    par(mfrow = c(1, 1))
  })
}

# -----------------------------
# RUN APP
# -----------------------------
shinyApp(ui = ui, server = server)
