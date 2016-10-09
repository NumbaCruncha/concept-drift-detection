library(dplyr)
library(ggplot2)
library(reshape2)
library(zoo)
library(cluster)
library(matrixStats)


electricity = read.csv('elecNorm.csv')
elec2 <- read.csv("C:/Users/micha/OneDrive - University of Otago/UoO/INFO411/Project/elec2.data", header=FALSE)
elec2$V1 = as.character(elec2$V1)
elec2$V1 = as.Date(elec2$V1, "%y%m%d")
colnames(elec2) = c("date", "day", "period", "newprice", "nswdemand","vicprice","vicdemand","transfer","class")

electricity[1] = elec2$date
electricity[3] = elec2$period
month = months(electricity$date)
year = year(electricity$date)
electricity = cbind(electricity,year)

e = electricity[-9]
y = as.vector(electricity[9])

train = seq(1,15104,1)
validation = seq(15105,30208,1)
test = seq(30209,45312,1)


nswdemand.table = build.table(nswdemand_wide[,-1])  
nswdemand.table = cbind(nswdemand_wide[1],nswdemand.table)

vicdemand.table = build.table(vicdemand_wide[,-1])  
vicdemand.table = cbind(vicdemand_wide[1],vicdemand.table)
#Build explantory variable table
nswclusters <- do.cluster(nswdemand.table[-1]) 
vicclusters <- do.cluster(nswdemand.table[-1]) 

# Perform principal components analysis on NSW, VIC demand tables
NSW.PCA = prcomp(nswdemand.table[-1])
VIC.PCA = prcomp(vicdemand.table[-1])
###############################################
# Visualise clustering
plot(nswdemand.table[-1], col = clusters$cluster, main = "NSW Extracted Features By Cluster")
plot(vicdemand.table[-1], col = clusters$cluster, main = "VIC Extracted Features By Cluster")
#Visulualise PCA
clusplot(nswdemand.table, nswclusters$cluster, color=TRUE, shade=TRUE,labels=4,lines=0)
clusplot(vicdemand.table, nswclusters$cluster, color=TRUE, shade=TRUE,labels=4,lines=0)

for (i in 2:5){
  plot(x = nswdemand.table[,1], y=nswdemand.table[,i], col = nswclusters$cluster, main = colnames(nswdemand.table[i]))
}


###################################################
# feature extraction

#reshape input data for NSW, VIC demand
nswdemand_long = melt(e, id.vars = c("date","period"), measure.vars = c("nswdemand"),  variable.name = c("nswdemand_variable"), value.name = c("nswdemand_value"))
nswdemand_wide = dcast(nswdemand_long, date ~ period, fun.aggregate=mean, value.var = c("nswdemand_value"))
vicdemand_long = melt(e, id.vars = c("date","period"), measure.vars = c("vicdemand"),  variable.name = c("vicdemand_variable"), value.name = c("vicdemand_value"))
vicdemand_wide = dcast(vicdemand_long, date ~ period, fun.aggregate=mean, value.var = c("vicdemand_value"))
###########################################################################
# simple.fn
#--------------------------------------------------------------------------
# Example simple function to be called by apply
# IN: x - the row of data for one household
#     fn - the simple function (mean, min, etc.) to apply to the vector x
#   col.range - the columns to select from x to apply fn. Defaults to all columns
# OUT: the result of applying fn to x for those columns
#########################################################################
simple.fn <- function(x,fn,cols=1:length(x))
{
  result <- fn(x[cols])
  return(round(result,3))
}
###################################################################################
#Calculate difference between columns
#-------------------------------------------------------------------------------------
p.diff = function(p){
    return(cbind(seq(0,0,length.out = 48),rowDiffs(as.matrix(p))))
}
##############################################################################
# build.table
#--------------------------------------------------------------------------
# Build the table of explanatory variables using the 
# original electricity data (p)
# IN: p - the original electricity dataset
# OUT: The constructed table of data to use with kmeans
# NOTE: Each row of p is a household
#       The columns of p are the 48 readings in time for each household
##############################################################################
build.table <- function(p)
{
  diff = p.diff(p)
  # tab <- apply(p,1,simple.fn,mean)                      # Daily Mean
  # tab <- cbind(tab,apply(p,1,simple.fn,max,18:32))      # Daytime Occupancy Maximum
  
  #Max over quarters of the day
  tab <- apply(diff,1,simple.fn,max,1:12)         # Max.Diff Q1
  tab <- cbind(tab,apply(diff,1,simple.fn,max,13:24))          # Max.Diff Q2
  tab <- cbind(tab,apply(diff,1,simple.fn,max,25:36))          # Max.Diff Q3
  tab <- cbind(tab,apply(diff,1,simple.fn,max,37:48))          # Max.Diff Q4
  
  
  #Min Difference over quarters of the day
  # tab <- cbind(tab,apply(p,1,simple.fn,sum,46:6))             # Sum Night-shift
  
  
  #Intra-day summation
  # tab <- cbind(tab,apply(p,1,simple.fn,sum,9:12))           # Sum Morning Peak A
  # tab <- cbind(tab,apply(p,1,simple.fn,sum,13:18))          # Sum Morning Peak B
  # tab <- cbind(tab,apply(p,1,simple.fn,sum,30:33))          # Sum Evening Group A
  # tab <- cbind(tab,apply(p,1,simple.fn,sum,34:37))          # Sum Evening Group B
  # tab <- cbind(tab,apply(p,1,simple.fn,sum,38:40))          # Sum Evening Group C
  # tab <- cbind(tab,apply(p,1,simple.fn,sum,41:45))          # Sum Evening Group D
  
  #Difference in Variance over quarters of the day
  # Var Q2
  #tab <- cbind(tab,apply(p.diff,1,simple.fn,var,25:36))     # Var Q3
  #tab <- cbind(tab,apply(p.diff,1,simple.fn,var,37:48))     # Var Q4
  
  #Max First Derivate of All Periods
  #tab <- cbind(tab,apply(p,1,predict.deriv.fn,max,1:48))      # Var.Diff Q1
  
  #labels for each column
  colnames(tab) <- c( #"DailyMean","DaytimeMax",
                      "Max.DiffQ1" ,"Max.DiffQ2","Max.DiffQ3","Max.DiffQ4"
                      #"NightShift",
                      #"MaxAM_GrpA" ,"MaxAM_GrpB","MaxPM_GrpA","MaxPM_GrpB","MaxPM_GrpC","MaxPM_GrpD"
                      # "VarQ1","VarQ2","VarQ3","VarQ4"
                      #"Max First Deriv"
  )
  as.data.frame(tab)   # Return the final table
}


do.cluster <- function(power.table,num.clusters=9)
{	
  kmeans(scale(power.table),centers=num.clusters)
}

##########################################################################
#Data Visualisation
#-------------------------------------------------------------------------
#Plot correlation betwee variables in power.table
par(mfrow=c(3,3))

#plot boxplots
for (i in 1:9){
  boxplot(nswdemand_wide[which(clusters$cluster==i),-1], xlim=c(1,48), ylim = c(0,1), xlab = "Half Hourly Periods", ylab="Energy Use [kWh]",main = paste("NSW Cluster",i))
}
for (i in 1:9){
  boxplot(vicdemand_wide[which(clusters$cluster==i),-1], xlim=c(1,48), ylim = c(0,1), xlab = "Half Hourly Periods", ylab="Energy Use [kWh]",main = paste("VIC Cluster",i))
}


#plot cluster means
for (i in 1:9){
  plot(apply(nswdemand_wide[which(clusters$cluster==i),-1],2,mean),type='l', xlim=c(1,48), ylim = c(0,1), col='black', xlab = "Half Hourly Periods", ylab = "Cluster Mean", main = paste("NSW Cluster",i))
}
for (i in 1:9){
  plot(apply(vicdemand_wide[which(clusters$cluster==i),-1],2,mean),type='l', xlim=c(1,48), ylim = c(0,1), col='black', xlab = "Half Hourly Periods", ylab = "Cluster Mean", main = paste("VIC Cluster",i))
}
par(mfrow=c(1,1))

#####################################################################################################################################################################

# plot simple moving average for demand by state
ma.nsw = ma(electricity$nswdemand,order = 1000)
ma.vic = ma(electricity$vicdemand,order = 1000)

plot(x = e$date, y = ma.nsw, main = "NSW Demand - Moving Average", pch = 16, cex = 0.1)
plot(x = e$date, y = ma.vic, main = "Vic Demand - Moving Average", pch = 16, cex = 0.1)
hist(x = ma.nsw)
hist(x = ma.vic)

ggplot(e, aes(y = ma.nsw, x = e$date)) + geom_bar(aes(scale_x_date(date_labels = "%m", date_breaks = "Monthly")))

# boxplot demand data by state
boxplot(e$nswdemand~e$date, main = "NSW Demand")
boxplot(e$vicdemand~e$date, main = "VIC Demand")

# plot max, min, mean lines graph for all observations and for each state
demand_by_state = e %>% 
  select(date, period, nswdemand, vicdemand) %>%
  group_by(date) %>%
  summarise_each(funs(mean), nswdemand,vicdemand) %>%
  melt(id.vars = c("date"), variable.name = "location", value.name = "demand") %>%
ggplot(demand_by_state, aes(x=date, y=demand, colour = location)) + geom_point(size=0.8)

crosstab = e %>%
  select(day, period, month)








#plot all observations and plot for each state
demand_by_date = e %>% 
  group_by(date,period) %>%
  select(date,period, nswdemand,vicdemand, class) %>% 
  summarise_each(funs(sum), nswdemand,vicdemand) %>%
  melt(id.vars = c("date", "period"), variable.name = "location", value.name = "demand_by_date") %>%
  mutate(observ = date+period)
arrange(observ, location, demand_by_date)
ggplot(demand_by_date, aes(x=observ, y=demand_by_date, colour = location)) + geom_point(size=0.8)


# boxplot <- demand per day
daily_demand = e %>% 
  group_by(date,period) %>%
  select(date,period, nswdemand,vicdemand) %>% 
  summarise_each(funs(sum), nswdemand,vicdemand) %>%
  melt(id.vars = c("date", "period"), variable.name = "location", value.name = "daily_demand") %>%
  mutate(observ = date+period)
arrange(observ, location, daily_demand)
boxplot(daily_demand)
# ggplot(daily_demand, aes(x=observ, y=daily_demand, colour = location)) + geom_point(size=0.8)


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


# time series analysis
plot(unique(e$date))
length(unique(e$date[which(e$date < 0.8)])) #unique dates below 
# decision tree

tree1 = rpart(y~e, method = "class")

# Model a logit regression
glm.mod1 = glm(class ~ nswdemand, data = electricity[train,], family = "binomial")
pred.mod1 = predict(glm.mod1, newdata = electricity[validation,])
mse1 = ((pred.mod1-electricity$class[test])^2)

# plot(x = electricity$nswdemand[train], y = glm.mod1$fitted.values, main = "Training D")
glm.mod2 = glm(class ~ vicdemand, data = electricity[train,], family = "binomial")
pred.mod2 = predict(glm.mod2, newdata = electricity[validation,])

avg_price_by_period = e %>% 
  group_by(period) %>%
  select(period, nswprice,vicprice, class) %>% 
  summarise_each(funs(mean), nswprice,vicprice) %>%
  melt(id="period", variable.name = "location",value.name = "avg_price")



#plot all observations and plot for each state
daily_demand2 = electricity %>% 
  select(date, nswdemand)
plot(daily_demand2)
  group_by(date) %>%
  melt(id.vars = c("date"), variable.name = "location", value.name = "demand_by_date") %>%
  arrange(date, location, daily_demand2)
ggplot(demand_by_date, aes(x=observ, y=demand_by_date, colour = location)) + geom_point(size=0.8)

#compute daily mean demand for both regions
nsw_mean_daily = rollapply(e$nswdemand, 48, mean, by = 48)
vic_mean_daily = rollapply(e$vicdemand, 48, mean, by = 48)
plot(nsw_mean_daily, col = "blue", main = "Mean daily Demand", xlab = "Days", ylab = "Average Daily Demand")
points(vic_mean_daily, col = "red")
legend(x = 800, y = 0.3, c("nswdemand","vicdemand"), pch = c(1,1), col = c(4,2))



