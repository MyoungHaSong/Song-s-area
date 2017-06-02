

if (interactive()){
  library(shiny)
  
  ### ui
  ui <- fluidPage(
    titlePanel("Multiple Regression Analysis"),
    
    sidebarLayout(
      
      sidebarPanel(
        radioButtons("dataset", h5("Select data"), choices = c("matcars","iris","acs","radial")),
        uiOutput('dv'),  # dependent variable(종속변수)
        uiOutput('iv'),  # independent variable(독립변수)
        actionButton("analysis", "Analysis")   # 실행버튼
      ),
      
      mainPanel(
        verbatimTextOutput("modelsummary"),
        plotOutput("plot")
      )
    )
  )
  
  
  ### server
  server <-  shinyServer(function(input, output) {
    
    datasetInput <- reactive({
      switch(input$dataset, "mtcars"=mtcars, "iris"=iris, "acs"=acs, "radial"=radial, "Your Data" = df())
    })
    
    # 종속변수
    output$dv = renderUI({
      selectInput('dv', h5('Response Variable(종속변수)'), choices = names(datasetInput()))
    })
    
    # 독립변수
    output$iv = renderUI({
      selectInput('iv', h5('Explanatory Variable(s)(독립변수)'), choices = names(datasetInput()))
    })
    
    # 실행버튼
    #eventReactive( input$analysis, {
    observeEvent( input$analysis, {
      
      # 회귀분석 공식
      regFormula <- reactive({
        as.formula(paste(input$dv, '~', input$iv))
      })
      
      # 이변수 모형
      model <- reactive({
        lm(regFormula(), data = datasetInput())
      })
      
      
      ## 결과 출력 ##
      # 데이터시트 
      output$view <- renderTable({
        head(datasetInput(), n = input$obs)
      })
      
      # 산점도
      output$plot <- renderPlot({
        plot(datasetInput()[,input$iv], datasetInput()[,input$dv], xlab = input$iv, ylab = input$dv, pch = 16, col = "black", cex = 1) # 산점도
        abline(lm(datasetInput()[,input$dv]~datasetInput()[,input$iv]), col="red", lwd = 2) # 추세선
      })
      
      # 요약표
      output$modelsummary <- renderPrint({
        summary(model())
      })
    })
  })
  shinyApp(ui, server)
}








