library(hexView)
library(corrplot)

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
round(correlation.matrix, 2)
corrplot(correlation.matrix, method = "circle")

# Stepwise variable selection
library(MASS)
step <- stepAIC(the.regression, direction="both")
step$anova

step.model <- lm(ROUNDS ~ WINTER + FEE + FEE.SQ + MGS.FEE + MGS.FEESUB + FEESUB + 
                   RATING + SLOPE + RAIN + RAIN.SQ + TEMP + TEMP.SQ + CART.WINTER + 
                   DISTANCE, data = my.data)

summary(step.model)

# All Subsets of Explanatory Variable Regression
#modify nbest to see more combinations, large enough nbest shows all combinations
library(leaps)
leaps <- regsubsets(ROUNDS ~ WINTER + FEE + FEE.SQ + MGS.FEE
                    + MGS.FEESUB + FEESUB + FEESUB.SQ + RATING + 
                    + SLOPE+ RAIN + RAIN.SQ + TEMP + TEMP.SQ
                    + CART.WINTER + DISTANCE + RANGE + WINTER.FEE
                    + YARD, data = my.data, nbest = 300, nvmax=18, really.big=T)
# view results 
summary(leaps)
models.fit.vector <- sort(c(round(summary(leaps)$adjr2,5)))
models.fit.vector

