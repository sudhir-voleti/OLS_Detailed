```r
# app.R

library(shiny)
library(DT)
library(pastecs)
library(RColorBrewer)
library(Hmisc)
library(ggplot2)
library(reshape2)
library(olsrr)
library(stats)
library(skedastic)
library(lmtest)
library(dplyr)

ui <- fluidPage(
  titlePanel(div(img(src = "logo.png", align = "right"), "OLS App")),
  sidebarLayout(
    sidebarPanel(
      h5("Data Input"),
      fileInput("file",  "Upload input data (csv file with header)"),
      fileInput("filep", "Upload Prediction data"),
      h5("Data Selection"),
      htmlOutput("yvarselect"),
      htmlOutput("xvarselect"),
      htmlOutput("fxvarselect"),
      br()
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
        
        # Overview with new sample files
        tabPanel("Overview",
          h4("App Description"),
          p("The application runs Ordinary Least Squares regressions on a user-provided empirical dataset/input file."),
          br(),
          h4("Download Sample Files"),
          downloadButton("downloadECGpred",  "East Coast grocers beer prediction data.csv"),
          br(), br(),
          downloadButton("downloadECGsales","East Coast grocers beer sales.csv"),
          br(), br(),
          downloadButton("downloadGlowPred","glowup cosmetics prediction data.csv"),
          br(), br(),
          downloadButton("downloadGlowPerf","glowup cosmetics weekly perf.csv")
        ),
        
        # Data Overview
        tabPanel("Data Overview",
          DT::dataTableOutput("datatable")
        ),
        
        # Summary Stats
        tabPanel("Summary Stats",
          verbatimTextOutput("summary")
        ),
        
        # Summary OLS
        tabPanel("Summary OLS",
          h4("OLS Formula"),
          verbatimTextOutput("olsformula"),
          h5("Residuals Summary (5-number)"),
          verbatimTextOutput("residualSummary"),
          h5("Coefficients"),
          DT::dataTableOutput("olssummary"),
          h4("Summary OLS standardized model"),
          DT::dataTableOutput("olssummarystd"),
          verbatimTextOutput("fstatistic"),
          verbatimTextOutput("rsquared")
        ),
        
        # Tests for Assumptions
        tabPanel("Tests for Assumptions",
          h3("Normality"),
          h4("Q–Q Plot of Residuals"),
          plotOutput("QQplot"),
          h4("Kolmogorov–Smirnov Test"),
          verbatimTextOutput("KSTest"),
          h3("Multicollinearity"),
          h4("Correlation Matrix"),
          verbatimTextOutput("correlation"),
          plotOutput("heatmap"),
          h4("VIF"),
          DT::dataTableOutput("VIF")
        ),
        
        # Tests for Assumptions 2
        tabPanel("Tests for Assumptions 2",
          h3("Heteroscedasticity"),
          h4("Breusch–Pagan Test"),
          verbatimTextOutput("BPTest"),
          h4("White's Test"),
          verbatimTextOutput("WhiteTest"),
          h3("Autocorrelation"),
          h4("ACF Plot"),
          plotOutput("ACFPlot"),
          h4("Durbin–Watson Test"),
          verbatimTextOutput("DWTest")
        ),
        
        # Prediction
        tabPanel("Prediction",
          downloadButton("downloadData1", "Download Predicted Data"),
          br(), br(),
          h4("Uploaded Prediction Data (first 10 rows)"),
          DT::dataTableOutput("predPreview"),
          br(), br(),
          DT::dataTableOutput("prediction")
        )
        
      ) # end tabsetPanel
    )   # end mainPanel
  )     # end sidebarLayout
)       # end fluidPage

server <- function(input, output, session) {
  
  # --- Data reactives ---
  Dataset <- reactive({
    req(input$file)
    read.csv(input$file$datapath, header = TRUE, sep = ",", stringsAsFactors = FALSE)
  })
  predData <- reactive({
    req(input$filep)
    read.csv(input$filep$datapath, header = TRUE, sep = ",", stringsAsFactors = FALSE)
  })
  
  # --- UI selectors ---
  output$yvarselect <- renderUI({
    req(Dataset())
    selectInput("yAttr", "Select Y variable",
                choices = colnames(Dataset()), selected = colnames(Dataset())[1])
  })
  output$xvarselect <- renderUI({
    req(Dataset(), input$yAttr)
    selectInput("xAttr", "Select X variables", multiple = TRUE, selectize = TRUE,
                choices = setdiff(colnames(Dataset()), input$yAttr),
                selected = setdiff(colnames(Dataset()), input$yAttr))
  })
  output$fxvarselect <- renderUI({
    req(Dataset(), input$yAttr)
    selectInput("fxAttr", "Select non-metric variable(s) in X", multiple = TRUE, selectize = TRUE,
                choices = setdiff(colnames(Dataset()), input$yAttr),
                selected = character(0))
  })
  
  # --- Original Summary Stats ---
  out <- reactive({
    df      <- Dataset()[, c(input$yAttr, input$xAttr), drop = FALSE]
    classes <- sapply(df, class)
    nu.data <- df[, classes %in% c("numeric","integer"), drop = FALSE]
    fac.data<- df[, classes %in% c("factor","character"), drop = FALSE]
    num.stat<- round(stat.desc(nu.data)[c(4,5,6,8,9,12,13), ], 4)
    fac.stat<- Hmisc::describe(fac.data)
    list(Numeric.data = num.stat, factor.data = fac.stat)
  })
  output$summary <- renderPrint({
    out()[1:2]
  })
  
  # --- Prepare model data ---
  mydata <- reactive({
    df <- Dataset()[, c(input$yAttr, input$xAttr), drop = FALSE]
    for (f in input$fxAttr) df[[f]] <- as.factor(df[[f]])
    df
  })
  dataStd <- reactive({
    nums   <- setdiff(input$xAttr, input$fxAttr)
    scaled <- scale(mydata()[, nums, drop = FALSE])
    cbind(
      setNames(data.frame(mydata()[[input$yAttr]]), input$yAttr),
      as.data.frame(scaled),
      mydata()[, input$fxAttr, drop = FALSE]
    )
  })
  
  # --- Fit models ---
  olsFit <- reactive({
    lm(reformulate(input$xAttr, response = input$yAttr), data = mydata())
  })
  olsStd <- reactive({
    lm(reformulate(input$xAttr, response = input$yAttr), data = dataStd())
  })
  
  # --- Summary OLS ---
  output$olsformula      <- renderPrint({ print(formula(olsFit())) })
  output$residualSummary <- renderPrint({
    rs <- summary(residuals(olsFit()))
    print(round(rs, 3))
  })
  output$olssummary      <- DT::renderDataTable({
    DT::datatable(round(summary(olsFit())$coefficients, 3), options = list(pageLength = 5))
  })
  output$olssummarystd   <- DT::renderDataTable({
    DT::datatable(round(summary(olsStd())$coefficients, 3), options = list(pageLength = 5))
  })
  output$fstatistic      <- renderPrint({
    f <- summary(olsFit())$fstatistic
    cat(sprintf("F-statistic: %.3f on %d and %d DF\n", f[1], f[2], f[3]))
  })
  output$rsquared        <- renderPrint({
    s <- summary(olsFit())
    cat(sprintf("Multiple R-Squared: %.3f, Adjusted R-Squared: %.3f\n",
                s$r.squared, s$adj.r.squared))
  })
  
  # --- Tests for Assumptions ---
  output$QQplot    <- renderPlot({ qqnorm(residuals(olsFit())); qqline(residuals(olsFit())) })
  output$KSTest    <- renderPrint({ print(ks.test(residuals(olsFit()), "pnorm"), digits = 3) })
  output$correlation <- renderPrint({
    corr <- cor(mydata()[, input$xAttr, drop = FALSE], use = "pairwise.complete.obs")
    print(round(corr, 3))
  })
  output$heatmap   <- renderPlot({
    corr <- cor(mydata()[, input$xAttr, drop = FALSE], use = "pairwise.complete.obs")
    ggplot(melt(corr), aes(Var1, Var2, fill = value)) +
      geom_tile() + scale_fill_gradient2(limits = c(-1, 1))
  })
  output$VIF       <- DT::renderDataTable({
    vtab <- ols_vif_tol(olsFit())
    vtab$VIF       <- round(vtab$VIF,       3)
    vtab$Tolerance <- round(vtab$Tolerance, 3)
    DT::datatable(vtab, options = list(pageLength = 5))
  })
  output$BPTest    <- renderPrint({ print(bptest(olsFit()), digits = 3) })
  output$WhiteTest <- renderPrint({ print(white(olsFit()), digits = 3) })
  output$ACFPlot   <- renderPlot({ acf(residuals(olsFit()), main = "ACF of Residuals") })
  output$DWTest    <- renderPrint({ print(dwtest(olsFit()), digits = 3) })
  
  # --- Data Overview ---
  output$datatable <- DT::renderDataTable({
    DT::datatable(Dataset(), options = list(pageLength = 10))
  })
  
  # --- Prediction Preview ---
  output$predPreview <- DT::renderDataTable({
    req(predData())
    DT::datatable(head(predData(), 10),
                  options = list(pageLength = 5, searching = FALSE),
                  rownames = FALSE)
  })
  
  # --- Prediction Results ---
  output$prediction <- DT::renderDataTable({
    req(predData())
    commonCols <- intersect(input$xAttr, colnames(predData()))
    validate(need(length(commonCols) > 0,
                  "None of the model's predictor columns were found in the uploaded file."))
    df_pred <- predData()[, commonCols, drop = FALSE]
    # coerce types to match training
    for (v in commonCols) {
      if (is.factor(mydata()[[v]])) {
        df_pred[[v]] <- factor(df_pred[[v]], levels = levels(mydata()[[v]]))
      } else {
        df_pred[[v]] <- as.numeric(df_pred[[v]])
      }
    }
    yhat  <- predict(olsFit(), newdata = df_pred)
    df_num <- df_pred[, vapply(df_pred, is.numeric, logical(1)), drop = FALSE]
    df_pred[, names(df_num)] <- lapply(df_num, round, 3)
    out   <- cbind(Yhat = round(yhat, 3), df_pred)
    DT::datatable(out, options = list(pageLength = 10))
  })
  
  # --- Download Predicted Data ---
  output$downloadData1 <- downloadHandler(
    filename = "Predicted_Data.csv",
    content = function(file) {
      commonCols <- intersect(input$xAttr, colnames(predData()))
      df_pred <- predData()[, commonCols, drop = FALSE]
      for (v in commonCols) {
        if (is.factor(mydata()[[v]])) {
          df_pred[[v]] <- factor(df_pred[[v]], levels = levels(mydata()[[v]]))
        } else {
          df_pred[[v]] <- as.numeric(df_pred[[v]])
        }
      }
      yhat <- predict(olsFit(), newdata = df_pred)
      numCols <- vapply(df_pred, is.numeric, logical(1))
      df_pred[numCols] <- lapply(df_pred[numCols], round, 3)
      write.csv(cbind(Yhat = round(yhat, 3), df_pred),
                file, row.names = FALSE)
    }
  )
  
  # --- New Sample File Downloads ---
  output$downloadECGpred <- downloadHandler(
    filename = "East Coast grocers beer prediction data.csv",
    content  = function(f) file.copy("data/East Coast grocers beer prediction data.csv", f)
  )
  output$downloadECGsales <- downloadHandler(
    filename = "East Coast grocers beer sales.csv",
    content  = function(f) file.copy("data/East Coast grocers beer sales.csv", f)
  )
  output$downloadGlowPred <- downloadHandler(
    filename = "glowup cosmetics prediction data.csv",
    content  = function(f) file.copy("data/glowup cosmetics prediction data.csv", f)
  )
  output$downloadGlowPerf <- downloadHandler(
    filename = "glowup cosmetics weekly perf.csv",
    content  = function(f) file.copy("data/glowup cosmetics weekly perf.csv", f)
  )
}

shinyApp(ui, server)
```
