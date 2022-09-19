####################################################
#      Summary & OLS App                           #
####################################################

library("shiny")
#library("foreign")

shinyUI(fluidPage(
  # Header:
  #headerPanel("OLS App"),
  titlePanel(title=div(img(src="logo.png",align='right'),"OLS App")),
  # Input in sidepanel:
  sidebarPanel(

    h5(p("Data Input")),
    fileInput("file", "Upload input data (csv file with header)"),
    #fileInput("filep", "Upload prediction data (csv file with header)"),
    h5(p("Data Selection")),
    htmlOutput("yvarselect"),
    htmlOutput("xvarselect"),
    htmlOutput("fxvarselect"),
    br()
  ),
  # Main:
  mainPanel( 
    
    tabsetPanel(type = "tabs",
                #
                
                tabPanel("Overview",
                         h4("App Description"),
                         p('The application runs Ordinary Least Squares regressions on a user-provided empirical dataset/input file.'),
                         p('Other tabs also include tests for assumptions that are assumed to be true before infering OLS results.'),
                         p('These assumptions include :'),
                         p('1. Linear regression model is linear in parameters.'),
                         p('2. There is a random sampling of observations.'),
                         p('3. The conditional mean should be zero.'),
                         p('4. There is no multi-collinearity.'),
                         p('5. There is homoscedasticity and no autocorrelation.'),
                         p('6. [Optional] Error terms should be normally distributed.'), br(),
                         p('For more information, check this page.'),
                         a(href="https://en.wikipedia.org/wiki/Ordinary_least_squares","- Wikipedia"),
                         h4(p("Download Sample Input Files")),
                         br(),
                         downloadButton('downloadData', 'Download model training input file.'),
                         br(),
                         br(),
                         downloadButton('downloadData2', 'Download prediction input file.'),
                         br(),
                         br(),
                         downloadButton('downloadData3', 'Download mtcars.csv.'),
                         #br(),
                         #br(),
                         #downloadButton('downloadData4', 'Download full dataset for diamonds.csv.'),
                         br(),
                         br(),
                         downloadButton('downloadData5', 'Download diamonds.csv (5000 entries).'),
                         br()
                         
                         
                ),
                tabPanel("Data Overview",DT::dataTableOutput("datatable")),
                tabPanel("Summary Stats", 
                         verbatimTextOutput("summary")),
                tabPanel("Summary OLS", 
                         h4("Summary OLS Model"),
                         verbatimTextOutput("olsformula"),
                        
                         h5('Residuals'),
                         verbatimTextOutput("AccompaniedResults2"),
                         DT::dataTableOutput("olssummary"),
                         verbatimTextOutput("AccompaniedResults"),
                         verbatimTextOutput("AccompaniedResults3"),
                         h4("Summary OLS standardized model"),
                         DT::dataTableOutput("olssummarystd")
                        ),

                tabPanel("Tests for Assumptions",
                         h3("Tests for Normality"),
                         h4("Q-Q Plots"),
                         plotOutput("QQplot"),
                         p("If the points in the plot roughly fall along a straight diagonal line, then the data is assumed to be normally distributed."),
                         h4("Kolmogorov-Smirnov Test"),
                         p("This test answers the question, 'What is the probability that this collection of samples could have been drawn from that probability distribution?' or, in the second case, 'What is the probability that these two sets of samples were drawn from the same (but unknown) probability distribution?'."),
                         p("If the p-value of the test is less than 0.05, it indicates that the data is not normally distributed."),
                         verbatimTextOutput("KSTest"),
                         h3("Multicollinearity"),
                         h4('Correlation'),
                         verbatimTextOutput("correlation"),
                         plotOutput("heatmap"),
                         h4("VIF"),
                         p("The Variance Inflation Factor (VIF) measures the inflation in the coefficient of the independent variable due to the collinearities among the other independent variables."),
                         p("As a rule of thumb, a VIF exceeding 5 requires further investigation, whereas VIFs above 10 indicate multicollinearity. VIFs are ideally below 3."),
                         p("A VIF of 1 means that the regression coefficient is not inflated by the presence of other predictors and hence, multicollinearity does not exist."),
                         DT::dataTableOutput("VIF")
                        ),
                tabPanel("Tests for Assumptions 2",
                         h3("Heteroscedasticity"),
                         h4("Breusch-Pagan Test"),
                         p("This test checks whether the variance of the residuals depends on the value of the independent variable."),
                         verbatimTextOutput("BPTest"),
                         h4("White's Test"),
                         p("The White Test is a special case of the BP-test. It is an asymptotic test and is therefore meant to be used on large samples. The test results of smaller samples must be interpreted with caution. If the model has many dependent variables, the test might be hard to calculate."),
                         verbatimTextOutput("WhiteTest"),
                         h3("Autocorrelation"),
                         h4('ACF Plot'),
                         plotOutput('ACFPlot'),
                         p('The interpretation of the ACF plot is as follows; The X-axis corresponds to the different lags of the residuals (lag-0, lag-1, lag-2, etc). Whereas, the Y-axis shows the correlation of each lag. Finally, the dashed blue line represents the significance levels.'),
                         h4('Durbin-Watson Test'),
                         p('The Durbin-Watson test statistic always has a value between 0 and 4, where'), br(),
                         p('[0-2): means positive autocorrelation'),br(),
                         p('2 means no autocorrelation'),br(),
                         p('(2-4]: means negative autocorrelation'),
                         verbatimTextOutput('DWTest')
                        ),

                         plotOutput("resplot1"))#,
                #tabPanel("Data with predicted Y",tableOutput("datatable2")),
                #tabPanel("Prediction",br(),
                #         h4("First 10 rows of predicted data"),
                #         p('"Yhat" column is the predicted value.'),
                #         verbatimTextOutput('prediction'),
                #         h4("Download Predicted Data"),
                #         downloadButton('downloadData1', 'Download Predicted Data')
                #)                


                )
      ) 
    ) 
  )
