rm(list=ls())

#source files --------------------------------------------------------------
source("./Code/src/config.R")
source("./Code/src/common.R")


# Read data -----------------------------------------------------------
sales <- fread("./Data/raw_data/sales_train.csv")  #sales data
items <- fread('./Data/raw_data/items.csv')        #item & item category mapping
# item_cat   <- fread('./Data/raw_data/item_categories.csv')
# shops      <- fread('./Data/raw_data/shops.csv')


# Inspection---------------------------------------------------------
dim(sales);str(sales)
sales[,date := as.Date(x = date,format = "%d.%m.%Y")]

#Merge item cat column
sales<- 
  merge(sales,items[,.(item_id,item_category_id)],
        by ='item_id');rm(items)

sales[,`:=`(shop_id = paste0('shop',shop_id),
            item_category_id = paste0("item_cat",item_category_id))]

descriptive_summary_function(sales)
## 34 unique time points spanned over 3 years 2013-2015
## 60 shops , 84 item categories , 21807 items

## Negative price and negative sales are doubtful.lets check
sales[item_price<0,.N]    # 1 record of negative price
sales[item_cnt_day<0,.N]  # 7356 records of negative sales
## drop these negative records
sales <- sales[item_price >0 & item_cnt_day >0]
length(unique(sales[,item_id]))  # Now 21804 items

##lets check how many records are there for each shop_id and item_id
sales[,.N,(shop_id)]
temp_df <- sales[,.N,.(item_id)][order(-N)]
quantile(temp_df$N,probs = seq(0.1,1,0.1))
keep_item_ids <- temp_df[N>=6,item_id]

sales <- sales[item_id %in% keep_item_ids]

## Create monthly series
monthly_sales <- 
  sales[,.(item_cnt_month = sum(item_cnt_day),
           avg_item_price_month = mean(item_price)),
        .(date_block_num,shop_id,item_id)];rm(sales)


descriptive_summary_function(monthly_sales)


### Overall series  ###
combined_series <- monthly_sales[,.(item_sales_total = sum(item_cnt_month),
                                 item_price_avg = mean(avg_item_price_month)),
                              .(date_block_num)][order(date_block_num)]

g <- ggplot(combined_series, aes(x=date_block_num))+labs(x="Months")
g1 <- g+geom_line(aes(y=item_sales_total),col='red')+
  ylab("Total Items Sold(Monthly)")
g2 <- g+geom_line(aes(y=item_price_avg),col='blue')+
  ylab( 'Average item Price(Monthly)')

grid.arrange(g1,g2)
cor(combined_series$item_sales_total,
    combined_series$item_price_avg)
#fwrite(all_items_df,"./Data/processed_data/combiend_series.csv")
library(tseries)

sales <- ts(data = combined_series$item_sales_total,start = 2013,frequency = 12)
autoplot(object = sales,series="Data")+
  autolayer(ma(sales,3),series = '3-MA')+
  autolayer(ma(sales,6),series = '6-MA')+
  autolayer(ma(sales,12),series = '12-MA')+
  labs(x='Time',y='Monthly Sales',
       title = "Monthly Total Sales")

  
library(magrittr)

sales %>% decompose(type = 'm') %>%
  autoplot() %>% labs(x="Year",title = "Classical multiplicative decomposition of the Series")

sales %>% adf.test()

acf(sales)
pacf(sales)
sales %>% ggtsdisplay()
fit <- auto.arima(sales)
checkresiduals(fit)

sales %>%
  auto.arima() %>%
  forecast() %>%
  autoplot() +
  ylab("H02 sales (million scripts)") + xlab("Year")









###   Shop wise plot   ###
shop_df <- monthly_sales[,.(Total_sales = sum(item_cnt_month),
                 Avg_price = mean(avg_item_price_month)),
                 .(date_block_num,shop_id)]

g <- ggplot(shop_df, aes(x=date_block_num,group=shop_id,color= factor(shop_id)))+
  labs(x="Months")+scale_color_discrete(name= "Shops")
g1 <- g+geom_line(aes(y=Total_sales))+ylab("Shopwise Monthly Items Sold(Total)")

g2 <- g+geom_line(aes(y=Avg_price))+ylab( 'Shopwise Monthly Price(Average)')

grid.arrange(g1,g2)

##  shop wise item wise series  ##
item_df <- monthly_sales[,.(Total_sales = sum(item_cnt_month),
                            Avg_price = mean(avg_item_price_month)),
                         .(date_block_num,item_id)]

g <- ggplot(item_df, aes(x=date_block_num,group=item_id,color= factor(item_id)))+
  labs(x="Months")+scale_color_discrete(name= "Items")
g1 <- g+geom_line(aes(y=Total_sales))+ylab("Itemwise Monthly Items Sold(Total)")
g2 <- g+geom_line(aes(y=Avg_price))+ylab( 'itemwise Monthly Price(Average)')


x <- rnorm(1000)  # no unit-root
adf.test(x)


y <- diffinv(x)   # contains a unit-root
adf.test(y)
