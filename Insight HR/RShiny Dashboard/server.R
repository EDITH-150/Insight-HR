server <- function(input, output) {
  
  output$totalRows <- renderValueBox({
    valueBox(
      nrow(mydata),
      "Total Records",
      icon = icon("database"),
      color = "blue"
    )
  })
  
  output$totalFeatures <- renderValueBox({
    valueBox(
      ncol(mydata),
      "Total Features",
      icon = icon("chart-line"),
      color = "purple"
    )
  })
  
  output$scatterPlot <- renderPlotly({
    p <- ggplot(mydata, aes_string(x = input$xaxis, y = input$yaxis, color = input$colorby)) +
      geom_point(alpha = input$alpha) +
      theme_minimal() +
      labs(title = paste(input$yaxis, "vs", input$xaxis)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p) %>% 
      layout(autosize = TRUE,
             margin = list(l = 50, r = 50, b = 100, t = 50),
             height = 600)
  })
  
  output$densityPlot <- renderPlotly({
    p <- ggplot(mydata, aes_string(x = input$xaxis, fill = "Domain")) +
      geom_density(alpha = 0.5) +
      theme_minimal() +
      labs(title = paste("Density Distribution of", input$xaxis, "by Domain")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p) %>% 
      layout(autosize = TRUE,
             margin = list(l = 50, r = 50, b = 100, t = 50),
             height = 600)
  })
  
  output$boxPlot <- renderPlotly({
    p <- ggplot(mydata, aes_string(x = input$xaxis, y = input$yaxis, fill = input$colorby)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = paste("Box Plot of", input$yaxis, "vs", input$xaxis)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p) %>% 
      layout(autosize = TRUE,
             margin = list(l = 50, r = 50, b = 100, t = 50),
             height = 600)
  })
  
  output$heatmap <- renderPlot({
    numeric_cols <- sapply(mydata, is.numeric)
    correlation_matrix <- cor(mydata[, numeric_cols])
    
    heatmap(correlation_matrix, 
            main = "Correlation Heatmap",
            col = colorRampPalette(c("#4575B4", "white", "#D73027"))(100),
            margins = c(10, 10))
  })
  
  output$pivotTable <- renderRpivotTable({
    rpivotTable(
      data = mydata,
      rows = "Domain",
      cols = "Turnover",
      aggregatorName = "Count",
      rendererName = "Table",
      width = "100%",
      height = "100%"
    )
  })
  
  output$ptable <- renderDT({
    datatable(mydata,
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                scrollY = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel', 'pdf')
              ),
              extensions = 'Buttons',
              height = "600px")
  })
}