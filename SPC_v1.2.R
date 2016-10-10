library(dplyr)
library(rpart)
library(zoo)
library(rpart.plot)
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
# initialise variables

    idx = index(electricity)
    tw = 2 # warning threshold
    td = 3 # detection threshold
    pmin = Inf # min classifcation error
    smin = Inf # std. dev. of the min classification error 
    fw = FALSE # warning flag

    # initialise window
    w.size = 48
    j=1
    w.start = j
    w.next = j
    w0 = seq(j,w.size,1)

##################################################################################
# FUNCTION: cd = Concept Drift Detector
# INPUT: initial variables
# RETURN: detection instances

cd = function(idx, tw, td, pmin, smin, fw, w.size, j, w.start, w.next, w0){
    st=0
  for (j in idx)  {
    # run main loop over all remaining items in the dataset
    if (j < length(w0)) {
      # grow initial window
      
      w.next = w.next + 1
        } else { # run classfier for initial window and return pt
          # browser()
          w = seq(w.start, w.next)
          pt = tree.mod(w)                        
          st = (pt * (1 - pt)) / length(w)       # calcuate st from pt
      
        if ((pt + st) < (pmin + smin)) {
                pmin = pt
                smin = st
                }
        if (((pt + st) > (pmin + (3 * smin))) && (fw == TRUE)) {
              td = j
                w.next = j - tw + 1
                pmin = Inf
                smin = Inf
                tw = Inf
                }
        else if ((pt + st) > (pmin + (2 * smin))) {
                if (fw == FALSE) {
                  fw = TRUE
                  tw = j
            }} else {
        fw = FALSE
        w.next = w.next + 1
            }
        }
    }
      
    DT =  tw
    return(DT)
}

###############################################################################
# FUNCTION: tree.mod = Decision Tree Classifier
# INPUT: w = window
# RETURN: pt = probability of observing misclassified values
#           = (false positive + false negatives)/window size
tree.mod = function(w){
  model = rpart(class~nswdemand,data=electricity[w,], method = "class")
  actual = electricity[w,9]
  tree.pred = predict(model, type = "class")
  cont.table = table(actual, tree.pred)
  mis.class = (cont.table[2]+cont.table[3])/sum(cont.table)
  return(mis.class)
}

#####################################################################################

