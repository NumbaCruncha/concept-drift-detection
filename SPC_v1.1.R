

electricity = read.csv('elecNorm.csv')
elec2 <- read.csv("C:/Users/micha/OneDrive - University of Otago/UoO/INFO411/Project/elec2.data", header=FALSE)
elec2$V1 = as.character(elec2$V1)
elec2$V1 = as.Date(elec2$V1, "%y%m%d")
colnames(elec2) = c("date", "day", "period", "newprice", "nswdemand","vicprice","vicdemand","transfer","class")

electricity[1] = elec2$date
electricity[3] = elec2$period
month = months(electricity$date)

e = electricity[-9]
y = as.vector(electricity[9])

train = seq(1,15104,1)
validation = seq(15105,30208,1)
test = seq(30209,45312,1)

###############################################################################
# initialiase variables
i = index(electricity)
tw = 2 # warning threshold
td = 3 # detection threshold
w = seq(1,48,1) # warm up window
pmin = Inf # min classifcation error
sigmin = Inf # std. dev. of the min classification error 
fw = FALSE # warning flag

###############################################################################
# run classfier

for (j in index){
  fitted = glm.fit(last(w)+i)
  lead(index,48)
  result[i] = paste(i,"th fitted value = ",fitted)
  result
  i=i+1
  }
result


while (i < w0){
 
  
  
  
}


##############################################################################
# functions

# run logisitic classifion learner

glm.fit = function(w){
  model = glm(class~nswdemand,data=electricity[w,],family="binomial")
  return(model$fitted.values)
}

