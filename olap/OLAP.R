# Build the Data Mart and the Data Cube

dim_geography <- 
  data.frame(region_name=c("East","West","MidWest","South","SouthWest","North"),
             region_state=c("Virginia","California","Illinois","Alabama","Arizona","New York"),        
             region_city=c("Fairfax","Sacramento","Chicago","Birmingham","Tempe","Buffalo"))

dim_time <- 
  data.frame(time_year=c(2012,2012,2012,2012,2012,2012,2012,2012,2012,2012,2012,2012,2013,2013,2013,2013,2013,2013,2013,2013,2013,2013,2013,2013),
             time_month=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec","Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
             time_quarter=c("Q1","Q1","Q1","Q2","Q2","Q2","Q3","Q3","Q3","Q4","Q4","Q4","Q1","Q1","Q1","Q2","Q2","Q2","Q3","Q3","Q3","Q4","Q4","Q4"))

dim_product <- 
  data.frame(product_name=c("Spring Water", "Pepsi Cola", "Cinnamon","Iodized Salt","Sugar","Milk","Butter","Cream Cheese","Oats","Ham","Turkey Breast","Beef","Apples","Bananas","Salmon fillet","Jumbo Shrimp"),
             product_category=c("Beverages","Beverages","Condiments","Condiments","Condiments","Dairy","Dairy","Dairy","Cereals","Meat","Meat","Meat","Produce","Produce","Seafood","Seafood"),
             product_price=c(5.25, 6.10, 3.29,2.50,3.35,4.19,2.79,3.15,4.59,6.99,5.89,7.47,2.34,1.89,6.10,5.60),
             product_cost=c(4.50,3.50,1.79,1.20,2.00,2.49,2.30,2.00,3.15,4.00,4.25,5.50,1.10,0.95,4.20,5.10))

dim_store <- 
  data.frame(store_id=c("Store101","Store102","Store103","Store104","Store105","Store106","Store107","Store108","Store109","Store110","Store111","Store112"),
             store_size=c("large","medium","medium","large","large","small","small","medium","large","large","medium","large"))



# Function to generate the Sales table
genSales <- function(no_of_recs) {
    
  # Setup the dimension tables
  set.seed(20141121)
  
  # Generate transaction data randomly
  region_name_K <- sort(sample(dim_geography$region_name, no_of_recs,replace=T,prob=c(2,5,6,3,3,2)))
  temp <- merge(region_name_K, dim_geography, by.x=1, by.y=1)
  region_state_K <- temp[order(temp$x),2]
  region_city_K <- temp[order(temp$x),3]
    
  time_year_K <- sample(dim_time$time_year, no_of_recs,prob=c(1,3,1,2,1,2,5,2,7,4,2,3,1,3,1,2,1,2,5,2,7,4,4,3), replace=T)
  temp <- merge(time_year_K, dim_time, by.x=1, by.y=1)
  time_month_K <- temp[order(as.factor(time_year_K)),2]
  time_quarter_K <- temp[order(as.factor(time_year_K)),3]
  
  product_name_K <- sort(sample(dim_product$product_name, no_of_recs, prob=c(rep(1,16)),replace=T))
  temp <- merge(product_name_K, dim_product, by.x=1, by.y=1)
  product_category_K <- temp[order(temp$x),2]
  
  store_id_K <- sort(sample(dim_store$store_id, no_of_recs, prob=c(1,3,1,2,1,2,5,2,7,4,4,3),replace=T))
  temp <- merge(store_id_K, dim_store, by.x=1, by.y=1)
  store_size_K <- temp[order(temp$x),2]
  
  sales_quantity <- sample(c(1250,2040,3300,4300,5331,6030,9342,1828,3021,575),prob=c(1,4,1,8,3,7,5,2,6,4), no_of_recs, replace=T)
  temp <- merge(product_name_K,dim_product,by.x=1, by.y=1)
  total_sales <- sales_quantity*temp$product_price 
  total_cost <- sales_quantity*temp$product_cost
  gross_profit <- (total_sales - total_cost)
  
  
  sales <- data.frame(region=region_name_K,
                      state=region_state_K,
                      city=region_city_K,
                      year=time_year_K,
                      month=time_month_K,
                      quarter=time_quarter_K,
                      product_name=product_name_K,
                      product_category=product_category_K,
                      store_id = store_id_K,
                      store_size = store_size_K,
                      sales_quantity=sales_quantity,
                      total_sales=total_sales,
                      gross_profit=gross_profit)
  
  # Sort the records by time order
  sales <- sales[order(sales$year, sales$month,sales$quarter),]
  row.names(sales) <- NULL
  
  return(sales)
}

  # Create the Sales Fact mart table
  sales_fact <- genSales(100000)
  
  # Create the Store Cube
  store_cube <- aggregate(. ~ region + state + city + year + month + quarter + product_name + product_category + store_id + store_size,
                          data = sales_fact, sum)
  
  
  





