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
                        
                        # Overview
                        tabPanel("Overview",
                                 h4("App Description"),
                                 p("The application runs Ordinary Least Squares regressions on a user-provided empirical dataset/input file."),
                                 br(),
                                 h4("Download Sample Input Files"),
                                 downloadButton('downloadData',  'Download model training input file'),
                                 br(), br(),
                                 downloadButton('downloadData2', 'Download prediction input file'),
                                 br(), br(),
                                 downloadButton('downloadData3', 'Download mtcars.csv'),
                                 br(), br(),
                                 downloadButton('downloadData5', 'Download diamonds.csv (5000 entries)')
                        ),
                        
                        # Data Overview
                        tabPanel("Data Overview",
                                 DT::dataTableOutput("datatable")
                        ),
                        
                        # Summary Stats (original)
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
                                 downloadButton('downloadData1', 'Download Predicted Data'),
                                 br(), br(),
                                 h4("Uploaded Prediction Data (first 10 rows)"),
                                 DT::dataTableOutput("predPreview"),
                                 br(), br(),
                                 DT::dataTableOutput("prediction")
                        )
                        
            )
        )
    )
)

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
        fa.data <- df[, classes %in% c("factor","character"), drop = FALSE]
        num.stat <- round(stat.desc(nu.data)[c(4,5,6,8,9,12,13), ], 4)
        fac.stat <- Hmisc::describe(fa.data)
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
    
    # --- Models ---
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
    
    # --- Prediction Results with type reconciliation & selective rounding ---
    output$prediction <- DT::renderDataTable({
        req(predData())
        
        # 1) Check presence of all required predictors
        required <- input$xAttr
        missing  <- setdiff(required, colnames(predData()))
        validate(
            need(length(missing) == 0,
                 paste("Missing predictor columns:", paste(missing, collapse = ", ")))
        )
        
        # 2) Copy and coerce types to match training
        df_pred <- predData()
        for (var in required) {
            orig_cls <- class(mydata()[[var]])
            if ("factor" %in% orig_cls) {
                df_pred[[var]] <- factor(df_pred[[var]],
                                         levels = levels(mydata()[[var]]))
            } else {
                df_pred[[var]] <- as.numeric(df_pred[[var]])
            }
        }
        
        # 3) Subset to model vars & predict
        df_model <- df_pred[, required, drop = FALSE]
        yhat     <- predict(olsFit(), newdata = df_model)
        
        # 4) Round only numeric columns
        num_vars <- names(df_model)[vapply(df_model, is.numeric, logical(1))]
        df_model[num_vars] <- lapply(df_model[num_vars], round, 3)
        
        # 5) Assemble output with Yhat first
        out_df <- cbind(Yhat = round(yhat, 3), df_model)
        
        DT::datatable(out_df, options = list(pageLength = 10))
    })
    
    # --- Download Predicted Data (same logic) ---
    output$downloadData1 <- downloadHandler(
        filename = "Predicted_Data.csv",
        content = function(file) {
            required <- input$xAttr
            df_pred  <- predData()
            missing  <- setdiff(required, colnames(df_pred))
            if (length(missing) > 0) {
                stop("Cannot download – missing columns: ", paste(missing, collapse = ", "))
            }
            for (var in required) {
                orig_cls <- class(mydata()[[var]])
                if ("factor" %in% orig_cls) {
                    df_pred[[var]] <- factor(df_pred[[var]],
                                             levels = levels(mydata()[[var]]))
                } else {
                    df_pred[[var]] <- as.numeric(df_pred[[var]])
                }
            }
            df_model <- df_pred[, required, drop = FALSE]
            yhat     <- predict(olsFit(), newdata = df_model)
            num_vars <- names(df_model)[vapply(df_model, is.numeric, logical(1))]
            df_model[num_vars] <- lapply(df_model[num_vars], round, 3)
            write.csv(cbind(Yhat = round(yhat, 3), df_model),
                      file, row.names = FALSE)
        }
    )
    
    # --- Other Download Handlers ---
    output$downloadData  <- downloadHandler(
        filename = "model_training_input.csv",
        content  = function(f) file.copy("data/beer data.csv", f)
    )
    output$downloadData2 <- downloadHandler(
        filename = "prediction_input_sample.csv",
        content  = function(f) file.copy("data/beer data - prediction sample.csv", f)
    )
    output$downloadData3 <- downloadHandler(
        filename = "mtcars.csv",
        content  = function(f) file.copy("data/mtcars dataset.csv", f)
    )
    output$downloadData5 <- downloadHandler(
        filename = "diamonds.csv",
        content  = function(f) file.copy("data/diamonds_section.csv", f)
    )
}

shinyApp(ui, server)
