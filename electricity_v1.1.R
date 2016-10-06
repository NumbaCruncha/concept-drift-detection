library(rattle)
library(dplyr)
library(ggplot2)
library(reshape2)
library(rpart)
library(gam)
electricity = read.csv('elecNorm.csv')
e = electricity[-9]
y = as.vector(electricity[9])


train = seq(1,15104,1)
validation = seq(15105,30208,1)
test = seq(30209,45312,1)



#plot all observations and plot for each state
demand_by_date = e %>% 
  group_by(date,period) %>%
  select(date,period, nswdemand,vicdemand, class) %>% 
  summarise_each(funs(sum), nswdemand,vicdemand) %>%
  melt(id.vars = c("date", "period"), variable.name = "location", value.name = "demand_by_date") %>%
  mutate(observ = date+period)
arrange(observ, location, demand_by_date)
ggplot(demand_by_date, aes(x=observ, y=demand_by_date, colour = location)) + geom_point(size=0.8)


#plot all observations and plot for each state
daily_demand = e %>% 
  group_by(date,period) %>%
  select(date,period, nswdemand,vicdemand, class) %>% 
  summarise_each(funs(sum), nswdemand,vicdemand) %>%
  melt(id.vars = c("date", "period"), variable.name = "location", value.name = "daily_demand") %>%
  mutate(observ = date+period)
arrange(observ, location, daily_demand)
ggplot(daily_demand, aes(x=observ, y=daily_demand, colour = location)) + geom_point(size=0.8)


#find average demand per date and plot for each state
avg_demand_by_date = e %>% 
  group_by(date) %>%
  select(date, nswdemand,vicdemand) %>% 
  summarise_each(funs(mean), nswdemand,vicdemand) %>%
  melt(id.vars = c("date"), variable.name = "location", value.name = "avg_demand_by_date")

ggplot(avg_demand_by_date, aes(x=date, y=avg_demand_by_date, colour = location)) + geom_point(size=0.8)


#find average demand per date and plot for each state
avg_demand_by_day = e %>% 
  group_by(day) %>%
  select(day, nswdemand,vicdemand) %>% 
  summarise_each(funs(mean), nswdemand,vicdemand) %>%
  melt(id.vars = c("day"), variable.name = "location", value.name = "avg_demand_by_day")

ggplot(avg_demand_by_day, aes(x=day, y=avg_demand_by_day, colour = location)) + geom_line(size=0.8)





# display average price per period
avg_price_by_period = e %>% 
  group_by(period) %>%
  select(period, nswprice,vicprice, class) %>% 
  summarise_each(funs(mean), nswprice,vicprice) %>%
  melt(id="period", variable.name = "location",value.name = "avg_price")

ggplot(avg_price_by_period, aes(period, avg_price, colour = location)) + geom_path( size=0.8)

# display average demand per period
avg_demand_by_period = e %>% 
  group_by(period) %>%
  select(period, nswdemand,vicdemand, class) %>% 
  summarise_each(funs(mean), nswdemand,vicdemand) %>%
  melt(id="period", variable.name = "location",value.name = "avg_demand")

ggplot(avg_demand_by_period, aes(period, avg_demand, colour = location)) + geom_path( size=0.8)

# display average price per day
avg_price_by_day= e %>% 
  group_by(day) %>%
  select(day, nswprice,vicprice, class) %>% 
  summarise_each(funs(mean), nswprice,vicprice) %>%
  melt(id="day", variable.name = "location",value.name = "avg_price")

ggplot(avg_price_by_day, aes(day, avg_price, colour = location)) + geom_path( size=0.8)

# display average demand per day
avg_demand_by_day = e %>% 
  group_by(day) %>%
  arrange(day,c(2,3,4,5,6,7,1)) %>%
  select(day, nswdemand,vicdemand, class) %>% 
  summarise_each(funs(mean), nswdemand,vicdemand) %>%
  melt(id="day", variable.name = "location",value.name = "avg_demand")

ggplot(avg_demand_by_day, aes(day, avg_demand, colour = location)) + geom_path( size=0.8)



# decision tree

tree1 = rpart(y~e, method = "class")

# Model a logit regression
glm.mod1 = glm(class ~ nswdemand, data = electricity[train,], family = "binomial")
pred.mod1 = predict(glm.mod1, newdata = electricity[validation,])
mse1 = ((pred.mod1-electricity$class[test])^2)

# plot(x = electricity$nswdemand[train], y = glm.mod1$fitted.values, main = "Training D")
glm.mod2 = glm(class ~ vicdemand, data = electricity[train,], family = "binomial")
pred.mod2 = predict(glm.mod2, newdata = electricity[validation,])

