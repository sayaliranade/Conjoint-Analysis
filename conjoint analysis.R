data <- read.csv("design matrix.csv")

library(dplyr)

data <- data %>%
  select(c(1:5))

preference_1 <- c(11,19,14,20,6,13,7,21,5,12,24,8,4,18,9,17,15,1,22,10,2,23,16,3) 
preference_2 <- c(10,18,8,22,24,20,9,11,7,21,23,19,4,6,2,15,17,13,3,5,1,14,16,12)
preference_3 <- c(8,12,4,20,24,16,6,10,2,18,22,14,7,11,3,19,23,15,5,9,1,17,21,13)
preference_4 <- c(17,18,9,23,24,20,6,7,5,13,14,12,15,16,8,22,21,19,3,4,2,10,11,1) 
preference <- data.frame(cbind(preference_1, preference_2, preference_3, preference_4))

my_design_1 <- c(1, 0, 1, 0, 0)
my_design_2 <- c(1, 0, 1, 0, 0)
my_design_3 <- c(1, 0, 1, 0, 0)
my_design_4 <- c(1, 0, 1, 0, 0)
my_design <- data.frame(cbind(my_design_1, my_design_2, my_design_3, my_design_4))

competitor_1 <- c(1, 1, 0, 1, 1)
competitor_2 <- c(1, 0, 1, 1, 0)

conjoint <- function(preference, data, my_design, competitor_1, competitor_2){
  
  out <- list()
  
  for(k in 1:4){
    data1 <- cbind(preference[,k], data)
    colnames(data1) <- c("Rank","Screen.75.inch","Screen.85.inch","Resolution.4K","Sony","Price.low")
    
    model_1 <- lm(Rank ~ ., data = data1)
    
    # part-worth
    
    partworths <- data.frame(summary(model_1)$coefficients[,1])
    colnames(partworths) <- c("partworth")
    
    # summary
    summary <- data.frame(summary(model_1)$coefficients[,1], summary(model_1)$coefficients[,2], summary(model_1)$coefficients[,3])
    colnames(summary) <- c("partworth", "se", "t-val")
    
    # price per utility
    price_per_utility <- (2500-2000)/abs(summary(model_1)$coefficients[6,1])
    
    # Attribute Importance
    
    screen_size <- c(summary(model_1)$coefficients[2,1], summary(model_1)$coefficients[3,1], 0)
    screen_resolution <- c(summary(model_1)$coefficients[4,1], 0)
    brand_name <- c(summary(model_1)$coefficients[5,1], 0)
    price <- c(summary(model_1)$coefficients[6,1], 0)
    
    r_screen_size <- max(screen_size)-min(screen_size)
    r_screen_resolution <- max(screen_resolution)-min(screen_resolution)
    r_brand_name <- max(brand_name)-min(brand_name)
    r_price <- max(price)-min(price)
    
    total = r_screen_size + r_screen_resolution + r_brand_name + r_price
    
    importance <- data.frame(round((r_screen_size/total)*100, 2), round((r_screen_resolution/total)*100, 2), round((r_brand_name/total)*100, 2),
                             round((r_price/total)*100, 2))
    colnames(importance) <- c("screen_size", "screen_resolution", "brand_name", "price")
    
    # Willingness to Pay
    
    wtp <- data.frame(summary(model_1)$coefficients[2,1]*price_per_utility, summary(model_1)$coefficients[3,1]*price_per_utility,
                      summary(model_1)$coefficients[4,1]*price_per_utility, summary(model_1)$coefficients[5,1]*price_per_utility)
    colnames(wtp) <- c("Screen.75.inch", "Screen.85.inch", "Resolution.4K", "Brand.Sony")
    wtp <- wtp %>% select_if(~any(. > 0))

    # optimal price
    
    cost <- c(1000, 500, 1000, 250, 250)
    
    net_cost <- sum(my_design[,k]*cost)
    
    market_size <- 100
    
    profile <- data.frame(cbind(my_design[,k], competitor_1, competitor_2))
    rownames(profile) <- c("(Intercept)","Screen.75.inch","Screen.85.inch","Resolution.4K","Sony")
    
    price_table <- list()
    j <- 1
    
    i <- vector()
    
    for(i in seq(1500,2700,100)){
      
      a <- sum(profile[,1]*partworths[1:5,])+(partworths[6,]*(i-2000)/500)
      b <- sum(profile[,2]*partworths[1:5,])+(partworths[6,]*(2500-2000)/500)
      c <- sum(profile[,3]*partworths[1:5,])+(partworths[6,]*(2000-2000)/500)
      
      utility <- c(a,b,c)
      
      attractiveness <- exp(utility)
      
      market_share <- c(attractiveness[1]/sum(attractiveness), attractiveness[2]/sum(attractiveness), attractiveness[3]/sum(attractiveness))
      market_share <- round(market_share, 2)
      
      data_row <- c(i, market_share[1], market_share[1]*market_size, i-net_cost, (market_share[1]*market_size)*(i-net_cost))
      
      price_table[[j]] <- data_row
      
      j = j + 1
    }
    
    price_table = data.frame(do.call(rbind, price_table))
    colnames(price_table) <- c("price", "share", "sales", "margin", "profit")
    
    maximum_profit <- max(price_table$profit)
    
    optimal_price <- price_table[price_table$profit == maximum_profit, 1]
    
    out[[k]] <- list(partworths, summary, importance, wtp, price_table, maximum_profit, optimal_price)
    
  }
  
  print("partworth")
  partworth <- cbind(out[[1]][[1]], out[[2]][[1]], out[[3]][[1]], out[[4]][[1]])
  colnames(partworth) <- c("Preference 1", "Preference 2", "Preference 3", "Preference 4")
  print(partworth)
  
  print("Model output for each preference")
  for(l in 1:ncol(preference)){
    print(paste("Model output for preference", l))
    print(out[[l]][[2]])
  }
  
  print("Attribute Importance for each preference")
  for(l in 1:ncol(preference)){
    print(paste("Attribute Importance for preference", l))
    print(out[[l]][[3]])
  }
  
  print("Willingness to pay for each preference")
  for(l in 1:ncol(preference)){
    print(paste("Willingness to pay for preference", l))
    print(out[[l]][[4]])
  }
  
  print("Optimal Price for each preference")
  op <- cbind(out[[1]][[6]], out[[2]][[6]], out[[3]][[6]], out[[4]][[6]])
  colnames(op) <- c("Preference 1", "Preference 2", "Preference 3", "Preference 4")
  print(op)
  
  print("Maximum profit for each preference")
  mp <- cbind(out[[1]][[7]], out[[2]][[7]], out[[3]][[7]], out[[4]][[7]])
  colnames(mp) <- c("Preference 1", "Preference 2", "Preference 3", "Preference 4")
  print(mp)
  
  sales <- data.frame(out[[1]][[5]]$price, out[[1]][[5]]$sales, out[[2]][[5]]$sales, out[[3]][[5]]$sales, out[[4]][[5]]$sales)
  colnames(sales) <- c("price", "sales_preference_1",  "sales_preference_2", "sales_preference_3",  "sales_preference_4")
  
  profit <- data.frame(out[[1]][[5]]$price, out[[1]][[5]]$profit, out[[2]][[5]]$profit, out[[3]][[5]]$profit, out[[4]][[5]]$profit)
  colnames(profit) <- c("price", "profit_preference_1",  "profit_preference_2", "profit_preference_3",  "profit_preference_4")
  
  require(ggplot2)
  
  print(ggplot(sales, aes(price)) +                    
    geom_line(aes(y=sales_preference_1), colour="red") +  
    geom_line(aes(y=sales_preference_2), colour="green") +
    geom_line(aes(y=sales_preference_3), colour="blue") +
    geom_line(aes(y=sales_preference_3), colour="yellow"))
  
  print(ggplot(profit, aes(price)) +                    
    geom_line(aes(y=profit_preference_1), colour="red") +  
    geom_line(aes(y=profit_preference_2), colour="green") +
    geom_line(aes(y=profit_preference_3), colour="blue") +
    geom_line(aes(y=profit_preference_4), colour="yellow"))
  
  return(out)
}

output <- list()

output = conjoint(preference, data, my_design, competitor_1, competitor_2)

