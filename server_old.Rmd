#################################################
#      Summary & OLS App                      #
#################################################

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
# library(gplot)

shinyServer(function(input, output,session) {
  
Dataset <- reactive({
  if (is.null(input$file)) { return(NULL) }
  else{
    Dataset <- as.data.frame(read.csv(input$file$datapath ,header=TRUE, sep = ","))
    return(Dataset)
  }
})


pred.readdata <- reactive({
  if (is.null(input$filep)) { return(NULL) }
  else{
    readdata <- as.data.frame(read.csv(input$filep$datapath ,header=TRUE, sep = ","))
    return(readdata)
  }
})

# Select variables:
output$yvarselect <- renderUI({
  if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
  
  selectInput("yAttr", "Select Y variable",
                     colnames(Dataset()), colnames(Dataset())[1])
  
})

output$xvarselect <- renderUI({
  if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
  
  selectInput("xAttr", "Select X variables",
              multiple = TRUE, selectize = TRUE,
                     setdiff(colnames(Dataset()),input$yAttr), setdiff(colnames(Dataset()),input$yAttr))
  
})

output$fxvarselect <- renderUI({
  if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)

  selectInput("fxAttr", "Select non-metric variable(s) in X",multiple = TRUE,
              selectize = TRUE,
                     setdiff(colnames(Dataset()),input$yAttr),"" )
  
})

mydata = reactive({
  mydata = Dataset()[,c(input$yAttr,input$xAttr)]

  if (length(input$fxAttr) >= 1){
  for (j in 1:length(input$fxAttr)){
      mydata[,input$fxAttr[j]] = factor(mydata[,input$fxAttr[j]])
  }
  }
  return(mydata)
  
})


Dataset.Predict <- reactive({
  fxc = setdiff(input$fxAttr, input$yAttr)
  mydata = pred.readdata()[,c(input$xAttr)]
  
  if (length(fxc) >= 1){
    for (j in 1:length(fxc)){
      mydata[,fxc[j]] = as.factor(mydata[,fxc[j]])
    }
  }
  return(mydata)
})

out = reactive({
data = mydata()
Dimensions = dim(data)
Head = head(data)
Tail = tail(data)
Class = NULL
for (i in 1:ncol(data)){
  c1 = class(data[,i])
  Class = c(Class, c1)
}

nu = which(Class %in% c("numeric","integer"))
fa = which(Class %in% c("factor","character"))
nu.data = data[,nu] 
fa.data = data[,fa] 
Summary = list(Numeric.data = round(stat.desc(nu.data)[c(4,5,6,8,9,12,13),] ,4), factor.data = describe(fa.data))
# Summary = list(Numeric.data = round(stat.desc(nu.data)[c(4,5,6,8,9,12,13),] ,4), factor.data = describe(fa.data))

a = seq(from = 0, to=200,by = 4)
j = length(which(a < ncol(nu.data)))
out = list(Dimensions = Dimensions,Summary =Summary ,Tail=Tail,fa.data,nu.data,a,j)
return(out)
})

output$summary = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    out()[1:2]
      }
})


output$heatmap = renderPlot({ 
  
    qplot(x=Var1, y=Var2, data=melt(cor(out()[[5]], use = "pairwise.complete.obs")), fill=value, geom="tile") +
    scale_fill_gradient2(limits=c(-1, 1))
  
})

output$correlation = renderPrint({
  cor(out()[[5]], use = "pairwise.complete.obs")
  })

ols = reactive({
    rhs = paste(input$xAttr, collapse = "+")
    ols = lm(paste(input$yAttr,"~", rhs , sep=""), data = mydata())
  return(ols)
})

ols2 = reactive({
  
  drop = which(input$yAttr == colnames(out()[[5]]))
               
  x0 = out()[[5]][,-drop]
  x01 = scale(x0)
  
  y = out()[[5]][,drop]
  
  dstd = data.frame(y,x01)
  colnames(dstd) = c(input$yAttr,colnames(x01))
  
  if (ncol(data.frame(out()[[4]])) == 1) {
    fdata = data.frame(out()[[4]])
    colnames(fdata) = input$fxAttr
    dstd = data.frame(dstd,fdata)
  }
  
  else if (ncol(data.frame(out()[[4]])) > 1) {
    fdata = data.frame(out()[[4]])
    dstd = data.frame(dstd,fdata)
  }
  
  rhs = paste(input$xAttr, collapse = "+")
  ols = lm(paste(input$yAttr,"~", rhs , sep=""), data = dstd)
  return(ols)

  })

output$resplot1 = renderPlot({
  plot(ols()$residuals, xlab = 'Residuals', ylab='',pch=16)#,cex=0.4)
  #title(main = "Fitted Values v/s Y"))
})

output$resplot2 = renderPlot({
    plot(ols()$residuals,ols()$fitted.values, xlab = 'Residuals', ylab = 'Fitted Values')
  title(main = "Fitted Values v/s Residuals")
})

output$resplot3 = renderPlot({
  plot(mydata()[,input$yAttr],ols()$fitted.values, xlab = 'Dependent Variables', ylab = 'Fitted Values')
  title(main = "Fitted Values v/s Chosen Y Variable")

})

output$olsformula <- renderPrint({
   result <- summary(ols())
   a00 = result$terms 
   formula = formula(a00)
   #toPrint3 <- paste0('Equation : ',formula)
   formula
})
output$olssummary = DT::renderDataTable({
  #summary(ols())
  
  result <- summary(ols())$coefficients
  
  #result <- as.data.frame(result) %>% mutate(SigCod = case_when('Pr(>|t|)' < 0.001 ~ "***", 'Pr(>|t|)' < 0.01 ~"**", 'Pr(>|t|)' < 0.1 ~ "*"))
  
  DT::datatable(round(result,3))
  })

output$olssummarystd = DT::renderDataTable({
  #summary(ols2())
  
  result <- as.data.frame(summary(ols2())$coefficients)
  #result <- result %>% mutate(SigCod = case_when(p.value < 0.001 ~ "***", p.value < 0.01 ~"**", p.value < 0.1 ~ "*"))
  DT::datatable(round(result,3))
})

 output$AccompaniedResults <- renderPrint({
 result <- summary(ols())
 toPrint <- paste0('F-statistic: ',result$fstatistic[1], ' on ',result$fstatistic[2], ' and ', result$fstatistic[3], ' DF')
 toPrint 
 })
  
  output$AccompaniedResults2 <- renderPrint({
 result <- summary(ols())
 toPrint2 <- summary(result$residuals)
 toPrint2
  })
 
  output$AccompaniedResults3 <- renderPrint({
 result <- summary(ols())
 toPrint3 <- paste0('Multiple R-Squared: ',result$r.squared,', Adjusted R-Squared:  ',result$adj.r.squared)
  toPrint3
 
 })
  
output$WhiteTest <-renderPrint({
  skedastic::white_lm(ols())
})  
  
output$QQplot <- renderPlot({
  set.seed(1234)
  par(mfrow=c(1,2))
  qqnorm(Dataset()[,c(input$yAttr)])
  qqline(Dataset()[,c(input$yAttr)])
})  
  
output$KSTest <- renderPrint({
ks.test(Dataset()[,c(input$yAttr)],'pnorm')
})  
  
output$VIF <- DT::renderDataTable({
  DT::datatable(ols_vif_tol(ols()))
   
  })  
  
output$BPTest <- renderPrint({
  lmtest::bptest(ols())
})  
  
output$DWTest <- renderPrint({
  lmtest::dwtest(ols())
})  
  
output$ACFPlot <- renderPlot({
  plot <- stats::acf(ols()$residuals, type="correlation", title = "Autocorrelation Factor Plot")
  #plot <- title(main = "Autocorrelation Factor Plot")
  plot
})  
  
output$datatable2 = renderTable({
  Y.hat = ols()$fitted.values
  data.frame(Y.hat,mydata())
})

output$datatable = DT::renderDataTable({
  DT::datatable(Dataset())
})

prediction = reactive({
  val = predict(ols(),Dataset.Predict())
  out = data.frame(Yhat = val, pred.readdata())
})

output$prediction =  renderPrint({
  if (is.null(input$filep)) {return(NULL)}
  head(prediction(),10)
})

#------------------------------------------------#
output$downloadData1 <- downloadHandler(
  filename = function() { "Predicted Data.csv" },
  content = function(file) {
    if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
    write.csv(prediction(), file, row.names=F, col.names=F)
  }
)
output$downloadData <- downloadHandler(
  filename = function() { "beer data.csv" },
  content = function(file) {
    write.csv(read.csv("data/beer data.csv"), file, row.names=F, col.names=F)
  }
)

output$downloadData2 <- downloadHandler(
  filename = function() { "beer data - prediction sample.csv" },
  content = function(file) {
    write.csv(read.csv("data/beer data - prediction sample.csv"), file, row.names=F, col.names=F)
  }
)

output$downloadData3 <- downloadHandler(
  filename = function() { "mtcars.csv" },
  content = function(file) {
    write.csv(read.csv("data/mtcars dataset.csv"), file, row.names=F, col.names=F)
  }
)  
 
  
output$downloadData4 <- downloadHandler(
  filename = function() { "Diamonds_Full.csv" },
  content = function(file) {
    write.csv(read.csv("data/diamonds.csv"), file, row.names=F, col.names=F)
  }
)  
 
  
output$downloadData5 <- downloadHandler(
  filename = function() { "Diamonds_section.csv" },
  content = function(file) {
    write.csv(read.csv("data/diamonds_section.csv"), file, row.names=F, col.names=F)
  }
)
  
})

