# Problem 1: Dataset Import & Cleaning
orders <- read.csv('data/Orders.csv')
orders$Profit <- as.numeric(gsub('\\$|,', '', orders$Profit))
orders$Sales <- as.numeric(gsub('\\$|,', '', orders$Sales))
orders$Order.Date <- as.Date(orders$Order.Date, format='%m/%d/%y')
orders$Ship.Date <- as.Date(orders$Ship.Date, format='%m/%d/%y')

# Problem 2: Inventory Management
library(ggplot2)
library(dplyr)

x_breaks <- c('2012-03', '2012-06', '2012-09', '2012-12',
              '2013-03', '2013-06', '2013-09', '2013-12',
              '2014-03', '2014-06', '2014-09', '2014-12',
              '2015-03', '2015-06', '2015-09', '2015-12')

orders_monthly_quantity <- orders %>% group_by(month=format(Order.Date, '%Y-%m')) %>% summarise(monthly_quantity=sum(Quantity))
ggplot(data = orders_monthly_quantity, aes(x=month, y=monthly_quantity, group=1)) +
  geom_line() +
  geom_point() +
  scale_x_discrete(breaks=x_breaks)

orders_monthly_category_quantity <- orders %>% group_by(month=format(Order.Date, '%Y-%m'), category=Category) %>% summarise(monthly_quantity=sum(Quantity))
ggplot(data = orders_monthly_category_quantity, aes(x=month, y=monthly_quantity, group=category, color=category)) +
  geom_line() +
  geom_point() +
  scale_x_discrete(breaks=x_breaks)

# Problem 3: Why did customers make returns?
returns <- read.csv('data/Returns.csv')

orders$Order.ID = as.character(orders$Order.ID)
returns$Order.ID = as.character(returns$Order.ID)

orders_returns <- left_join(orders, returns, by='Order.ID')
orders_returns$Returned = as.character(orders_returns$Returned)
orders_returns$Returned[is.na(orders_returns$Returned)] <- 'No'

# 1. How much profit did we lose due to returns each year?
lose_profit <- orders_returns[orders_returns$Returned == 'Yes',] %>% group_by(year=format(Order.Date, '%Y')) %>% summarise(total_lost_profit=sum(Profit))
ggplot(data = lose_profit, aes(x=year, y=total_lost_profit, group=1)) +
  geom_line() +
  geom_point()

# 2. How many customer returned more than once? more than 5 times?
customer_returned <- orders_returns[orders_returns$Returned == 'Yes',] %>% group_by(custimer=Customer.ID) %>% summarise(total_returned=n())
nrow(customer_returned) # 1061
nrow(customer_returned[customer_returned$total_returned > 5,]) # 46

# 3. Which regions are more likely to return orders?
regeon_returned <- returns %>% group_by(region=Region) %>% summarise(total_returned=n()) %>% arrange(desc(total_returned))
ggplot(data = regeon_returned, aes(x=reorder(region, total_returned), y=total_returned)) +
  geom_bar(stat='identity') +
  coord_flip()

# 4. Which categories (sub-categories) of products are more likely to be returned?
category_returned <- orders_returns[orders_returns$Returned == 'Yes',] %>% group_by(category=Sub.Category) %>% summarise(total_returned=n()) %>% arrange(desc(total_returned))
ggplot(data = category_returned, aes(x=reorder(category, total_returned), y=total_returned)) +
  geom_bar(stat='identity') +
  coord_flip()
