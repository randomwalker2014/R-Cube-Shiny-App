library(shiny)
library(shinyTree)
library(datasets)
library(ggplot2)
library(scales)

source("olap/OLAP.R",local = FALSE)

shinyServer(function(input, output,session) {
  
  cube <- reactive({
     as.vector(input$cubeChoice)
  })
  
  dimSelected <- reactive({
    as.vector(input$dNodeSelect)
  })
  
  measureSelected <- reactive({
    as.vector(input$mNodeSelect)
  })
  
  regionFilterSelected <- reactive({
          as.vector(input$region)
  })
    
  stateFilterSelected <- reactive({
    as.vector(input$state)
  }) 
  
  cityFilterSelected <- reactive({
          as.vector(input$city)
  })
  
  productNameFilterSelected <- reactive({
          as.vector(input$productName)
  })
  
  productCategoryFilterSelected <- reactive({
          as.vector(input$productCategory)
  })
  
  yearFilterSelected <- reactive({
          as.vector(input$year)
  })
  
  monthFilterSelected <- reactive({
          as.vector(input$month)
  })
  
  quarterFilterSelected <- reactive({
          as.vector(input$quarter)
  })
  
  storeIdFilterSelected <- reactive({
          as.vector(input$storeId)
  })
  
  storeSizeFilterSelected <- reactive({
          as.vector(input$storeSize)
  })
  
  # Dimension Tree
  output$tree_dimension <- renderTree({ 
    if( cube() %in% c("Select","store")) {
    list(
      Geography = structure(list(region = "region",state = "state",city="city"),stclass="green-node",stopened=TRUE),
      Product = structure(list(product_name = "product_name",product_category = "product_category"),stclass="green-node",stopened=TRUE), 
      Store = structure(list(store_id = "store_id", store_size = "store_size"),stclass="green-node",stopened=TRUE),
      Time = structure(list(year = "year",month= "month",quarter = "quarter"),stclass="green-node",stopened=TRUE)
    )
    } 
  })
  
  # Measures Tree
  output$tree_measure <- renderTree({
    if(cube() %in% c("Select","store")) {
      list(
        sales_quantity = structure("sales_quantity", stclass="green-node"),
        total_sales = structure("total_sales", stclass="green-node"),
        gross_profit  = structure("gross_profit", stclass="green-node")
      )
    } 
  })
  
  # Apply Filters
  applyFilters <- function(dimensions,measures){
    
    regionFilter <- regionFilterSelected()
    stateFilter <- stateFilterSelected()
    cityFilter <- cityFilterSelected()
    
    yearFilter <- yearFilterSelected()
    monthFilter <- monthFilterSelected()
    quarterFilter <- quarterFilterSelected()
    
    productNameFilter <- productNameFilterSelected()
    productCategoryFilter <- productCategoryFilterSelected()
    
    storeIdFilter <- storeIdFilterSelected()
    storeSizeFilter <- storeSizeFilterSelected()
    
    store_cube$group <- apply( store_cube[ , dimensions,drop=FALSE] , 1 , paste, collapse = " / ")
    store_cube_filtered <- store_cube
    
    if(!is.null(regionFilter) && c("region") %in% dimensions) {
      store_cube_filtered <- store_cube_filtered[which(store_cube_filtered$region == regionFilter),]
    }else{
      regionFilter <- NULL   
    }
    
    if(!is.null(stateFilter) && c("state") %in% dimensions) {
      store_cube_filtered <- store_cube_filtered[which(store_cube_filtered$state == stateFilter),]
    }else{
      stateFilter <- NULL   
    }
    
    if(!is.null(cityFilter) && c("city") %in% dimensions) {
      store_cube_filtered <- store_cube_filtered[which(store_cube_filtered$city == cityFilter),]
    }else{
      cityFilter <- NULL   
    }
    
    if(!is.null(yearFilter) && c("year") %in% dimensions){
      store_cube_filtered <- store_cube_filtered[which(store_cube_filtered$year == yearFilter),]
    }else{
      yearFilter <- NULL   
    }
    
    if(!is.null(monthFilter) && c("month") %in% dimensions){
      store_cube_filtered <- store_cube_filtered[which(store_cube_filtered$month == monthFilter),]
    }else{
      monthFilter <- NULL   
    }
    
    if(!is.null(quarterFilter) && c("quarter") %in% dimensions){
      store_cube_filtered <- store_cube_filtered[which(store_cube_filtered$quarter == quarterFilter),]
    }else{
      quarterFilter <- NULL   
    }
    
    if(!is.null(productNameFilter) && c("product_name") %in% dimensions){
      store_cube_filtered <- store_cube_filtered[which(store_cube_filtered$product_name == productNameFilter),]
    }else{
      productNameFilter <- NULL   
    }
    
    if(!is.null(productCategoryFilter) && c("product_category") %in% dimensions){
      store_cube_filtered <- store_cube_filtered[which(store_cube_filtered$product_category == productCategoryFilter),]
    }else{
      productCategoryFilter <- NULL   
    }
    
    if(!is.null(storeIdFilter) && c("store_id") %in% dimensions){
      store_cube_filtered <- store_cube_filtered[which(store_cube_filtered$store_id == storeIdFilter),]
    }else{
      storeIdFilter <- NULL   
    }
    
    if(!is.null(storeSizeFilter) && c("store_size") %in% dimensions){
      store_cube_filtered <- store_cube_filtered[which(store_cube_filtered$store_size == storeSizeFilter),]
    }else{
      storeSizeFilter <- NULL   
    }
    
    return(store_cube_filtered)
  }
  
  
    # Generate the Plot
    output$plot <- renderPlot({
       if(!is.null(dimSelected()) && !is.null(measureSelected())) {
         
         dimensions <- dimSelected()
         dimensions <- dimensions[!dimensions %in% c("Geography","Product","Store","Time")]
         measures <-  measureSelected()
         
         store_cube_filtered <- applyFilters(dimensions,measures)
         
   
         options(scipen=1000)
         p <- ggplot(data=store_cube_filtered, aes_string(x="group",y=measures)) 
         p <- p + geom_bar(width=0.4,stat="identity",fill="#9EBD69")
         
         p <- p + theme_bw()
         p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
         p <- p + theme(axis.text.x = element_text(colour="grey28",size=11,face="bold"))
         p <- p + theme(axis.text.y = element_text(colour="grey28",size=12,face="bold"))
         p <- p + theme(axis.title.x = element_text(colour="blue",size=14,face="bold"))
         p <- p + theme(axis.title.y = element_text(colour="blue",size=14,face="bold"))
         p <- p + theme(axis.title.x = element_blank())
         p <- p + theme(axis.title.y = element_text(vjust=0.9))
         p <- p + theme(legend.position="none")
         p <- p + scale_y_continuous(labels = comma)

         print(p)
      
    } else{
      return(NULL)
    }
    })
  

  # Generate Data Table
  output$table <- renderDataTable({
    if(!is.null(dimSelected()) && !is.null(measureSelected())) {
      
            dimensions <- dimSelected()
            dimensions <- dimensions[!dimensions %in% c("Geography","Product","Store","Time")]
            measures <-  measureSelected()
            
            store_cube_filtered <- applyFilters(dimensions,measures)
      
      aggregate(store_cube_filtered[measures],by=store_cube_filtered[dimensions],FUN=sum)
    }
    
  },options = list(pageLength = 5,autoWidth = TRUE,searchable = TRUE,searching = FALSE,lengthChange = FALSE
  ))
  
  # Dynamic UI elements 
  output$region <- renderUI({ 
          
          checkedDimension <- dimSelected()
          checkedMeasures <- measureSelected() 
          if(!is.null(checkedDimension) &&  c("region") %in% checkedDimension) {
                  selectizeInput("region", "Region:",
                                 as.vector(dim_geography$region_name),
                                 multiple=TRUE,options = list(maxItems = 5),
                                 selected = input$region
                  )
          }
          
  })
  
  output$state <- renderUI({ 
   
    checkedDimension <- dimSelected()
    checkedMeasures <- measureSelected() 
    
    if(!is.null(checkedDimension) &&  c("state") %in% checkedDimension) {
      selectizeInput("state", "State:",
                  as.vector(dim_geography$region_state),
                  multiple=TRUE,options = list(maxItems = 5),
                  selected = input$state
      )
    }
    
  })
  
  output$city <- renderUI({ 
          
          checkedDimension <- dimSelected()
          checkedMeasures <- measureSelected() 
          
          if(!is.null(checkedDimension) &&  c("city") %in% checkedDimension) {
                  selectizeInput("city", "City:",
                                 as.vector(dim_geography$region_city),
                                 multiple=TRUE,options = list(maxItems = 5),
                                 selected = input$city
           )
          }
  })
  

  output$productName <- renderUI({ 
                    
          checkedDimension <- dimSelected()
          checkedMeasures <- measureSelected()
          
          if(!is.null(checkedDimension) && c("product_name") %in% checkedDimension) {
                  selectizeInput("productName", "Product:",
                              as.vector(dim_product$product_name),
                              multiple=TRUE,options = list(maxItems = 5),
                              selected = input$productName
                  )
          }
          
  })
  
  
  output$productCategory <- renderUI({ 
          
          checkedDimension <- dimSelected()
          checkedMeasures <- measureSelected()
          
          if(!is.null(checkedDimension) && c("product_category") %in% checkedDimension) {
                  selectizeInput("productCategory", "Product Category:",
                                 as.vector(dim_product$product_category),
                                 multiple=TRUE,options = list(maxItems = 5),
                                 selected = input$productCategory
                  )
          }
          
  })
  
  
  output$year <- renderUI({ 
          
          checkedDimension <- dimSelected()
          checkedMeasures <- measureSelected()
          
          if(!is.null(checkedDimension) && c("year") %in% checkedDimension) {
                  selectizeInput("year", "Year:",
                                 as.vector(dim_time$time_year),
                                 multiple=TRUE,options = list(maxItems = 5),
                                 selected = input$year
                  )
          }
          
  })
  
    
  output$month <- renderUI({ 
      
    checkedDimension <- dimSelected()
    checkedMeasures <- measureSelected()
    
    if(!is.null(checkedDimension) && c("month") %in% checkedDimension) {
      selectizeInput("month", "Month:",
                  as.vector(dim_time$time_month),
                  multiple=TRUE,options = list(maxItems = 5),
                  selected = input$month
      )
   }
    
  })
  
  output$quarter <- renderUI({ 
          
          checkedDimension <- dimSelected()
          checkedMeasures <- measureSelected()
          
          if(!is.null(checkedDimension) && c("quarter") %in% checkedDimension) {
                  selectizeInput("quarter", "Quarter:",
                                 as.vector(dim_time$time_quarter),
                                 multiple=TRUE,options = list(maxItems = 5),
                                 selected = input$quarter
                  )
          }
          
  })
  
  
  output$storeId <- renderUI({ 
          
          checkedDimension <- dimSelected()
          checkedMeasures <- measureSelected()
          
          if(!is.null(checkedDimension) && c("store_id") %in% checkedDimension) {
                  selectizeInput("storeId", "Store ID:",
                                 as.vector(dim_store$store_id),
                                 multiple=TRUE,options = list(maxItems = 5),
                                 selected = input$storeId
                  )
          }
          
  })
  
  output$storeSize <- renderUI({ 
          
          checkedDimension <- dimSelected()
          checkedMeasures <- measureSelected()
          
          if(!is.null(checkedDimension) && c("store_size") %in% checkedDimension) {
                  selectizeInput("storeSize", "Store Size:",
                                 as.vector(dim_store$store_size),
                                 multiple=TRUE,options = list(maxItems = 5),
                                 selected = input$storeSize
                  )
          }
          
  })
  
  
})
