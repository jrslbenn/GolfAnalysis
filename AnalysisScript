library(hexView)

my.data <- readEViews("golf.wf1", as.data.frame = TRUE)
summary(my.data)

#add modification of variables (squared and dummy multiplications)
my.data$FEE.SQ      <- my.data$FEE * my.data$FEE
my.data$MGS.FEE     <- my.data$MGS * my.data$FEE
my.data$MGS.FEESUB  <- my.data$MGS * my.data$FEESUB
my.data$FEESUB.SQ   <- my.data$FEESUB * my.data$FEESUB
my.data$RAIN.SQ     <- my.data$RAIN * my.data$RAIN
my.data$TEMP.SQ     <- my.data$TEMP * my.data$TEMP
my.data$CART.WINTER <- my.data$CART * my.data$WINTER
my.data$WINTER.FEE  <- my.data$WINTER * my.data$FEE

#run regression of rounds on explanatory variables
the.regression <- lm(ROUNDS ~ WINTER + FEE + FEE.SQ + MGS.FEE
                            + MGS.FEESUB + FEESUB + FEESUB.SQ + RATING + 
                            + SLOPE+ RAIN + RAIN.SQ + TEMP + TEMP.SQ
                            + CART.WINTER + DISTANCE + RANGE + WINTER.FEE
                            + YARD, data = my.data)


#avoid scientific notation and print the regression
options(scipen=999)
summary(the.regression)

#extract explanatory variables
explanatory.variable.matrix <- my.data[, !(colnames(my.data) %in% c("AVEFEE","CART","COURSE", "MGS", "MONTH", "ROUNDS"))]

#create and plot correlation matrix
correlation.matrix <- cor(explanatory.variable.matrix)

library('corrplot')
corrplot(correlation.matrix, method = "circle")

# convert to f test function

#CorrelationChecker <- function(a.data.frame, a.explanatory.variable.list ) {
#  for (i in a.explanatory.variable.list) {
#      for(j in a.explanatory.variable.list) {
#       with(a.explanatory.variable.list(cor(a.explantory.variable.list[i], a.explanatory.variable.list[j]))
#    }
#  }
#  return
#}