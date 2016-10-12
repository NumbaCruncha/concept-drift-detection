library(dplyr)
library(rpart)
library(zoo)
library(rpart.plot)
library(glmnet)
electricity = read.csv('elecNorm.csv')
elec2 <- read.csv("C:/Users/micha/OneDrive - University of Otago/UoO/INFO411/Project/elec2.data", header=FALSE)
elec2$V1 = as.character(elec2$V1)
elec2$V1 = as.Date(elec2$V1, "%y%m%d")
colnames(elec2) = c("date", "day", "period", "newprice", "nswdemand","vicprice","vicdemand","transfer","class")
output =c()

electricity[1] = elec2$date
electricity[3] = elec2$period
month = months(electricity$date)

e = electricity[-9]
y = as.vector(electricity[9])

train = seq(1,15104,1)
validation = seq(15105,30208,1)
test = seq(30209,45312,1)

###############################################################################
# initialise variables

    idx = index(electricity)
    tw = 2 # warning threshold
    td = 3 # detection threshold
    pmin = Inf # min classifcation error
    smin = Inf # std. dev. of the min classification error 
    fw = FALSE # warning flag

    # initialise window
    j=1
    w.start = 1
    w.size = 1
    w.end = 48
    w0 = seq(w.start,w.end*7)

##################################################################################
# FUNCTION: cd = Concept Drift Detector
# INPUT: initial variables
# RETURN: detection instances

cd = function(idx, tw, td, pmin, smin, fw, w.size, j, w.start, w.end, w0){
  # browser()
    st=0
    pt=0
    DT = data.frame("Period Detected" = as.numeric(), "Error Rate" = as.numeric())
  for (j in 1:(length(idx)/w.size))  {
    # window = j * w.size
    DT[nrow(DT)+1, ] <- c(j,pt)
    # run main loop over all remaining items in the dataset
    if (w.end < length(w0)) {
      # grow initial window
      w.end = w.end + w.size
        } else { # run classfier for initial window and return pt
          w = seq(w.start, w.end)
          pt = glm.mod(w)                        
          st = (pt * (1 - pt)) / length(w)       # calcuate st from pt
      
        if ((pt + st) < (pmin + smin)) { # update min error and sigma
                pmin = pt
                smin = st
                }
        if (((pt + st) > (pmin + (3 * smin))) && (fw == TRUE)) { # change detected, reset window
          browser()
                w.start = tw
                w.end = j * w.size
                pmin = Inf
                smin = Inf
                tw = Inf
                                }
        else if ((pt + st) > (pmin + (2 * smin))) { # warning level reached     
                if (fw == FALSE) {
                  fw = TRUE
                  tw = j * w.size
            }} else {
        fw = FALSE
        w.end = w.end + w.size
            }
        }
    }
      
    print(paste("Algorithm Terminated at , j"))
    return(DT)
}
detections = cd(idx, tw, td, pmin, smin, fw, w.size, j, w.start, w.end, w0)
###############################################################################
# FUNCTION: tree.mod = Decision Tree Classifier
# INPUT: w = window
# RETURN: pt = probability of observing misclassified values
#           = (false positive + false negatives)/window size
tree.mod = function(w){
  # tryCatch(stop(model), silent = FALSE, finally = print("Hello"))
  model = rpart(class~nswdemand,data=electricity, subset = w, method = "class")
  actual = electricity[w,9]
  tree.pred = predict(model, type = "class")
  cont.table = table(actual, tree.pred)
  mis.class = (cont.table[2]+cont.table[3])/sum(cont.table)
  return(mis.class)
}
#######################################################################################
# FUNCTION: logistic regression Learner
# INPUT: w = window
# RETURN: pt = probability of observing misclassified values
#           = (false positive + false negatives)/window size
glm.mod = function(w){
model = glm(class~nswdemand,data=electricity[w,], family = binomial)
actual = electricity[w,9]
glm.probs = predict(model, type = "response")
glm.pred = rep("DOWN",length(w))
glm.pred[glm.probs > 0.5] = "UP"
cont.table = table(actual, glm.pred)
mis.class = (cont.table[2]+cont.table[3])/sum(cont.table)
return(mis.class)
}


#####################################################################################
output$avg.daily.demand = stats::lag(electricity$nswdemand,48,mean(nsw$demand))
