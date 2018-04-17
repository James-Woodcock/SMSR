#################################################################################
#----- INFORMATION --------------------------------------------------------------
# Title       : Stochastic Model Selection for Multiple Linear Regression Models.
# Author      : James Woodcock
# Date        : April 2018
# Description : Implementation in R of the Xin and Zhu (2011) paper 
#               "Stochastic Stepwise Ensembles for Variable Selection"
#################################################################################

library(dplyr)
library(smbinning)
library(olsrr)

SMSR <- function(y, data, kappa, lambda = 0.5, varRemove, measure = "AIC") {
    
    varsInModel <- c()
    
    # 1) Input Sanitation.
    availableVars <- removeResponseVars(data = data,
                               y = y)
    maxBag <- getMaxBag(vars = availableVars,
                       lambda = lambda)
    
    # 2) Initialise first model with random sample, 
    numVars <- uniformRandom(maxBag)
    varsToAdd <- sample(availableVars, numVars, replace = FALSE)
    
    modelFormula <- as.formula(paste(y, "~", paste(varsToAdd,collapse = " + ")))
    intModel <- lm(modelFormula, data = data)
    
        # 2.1) Update available selections.
        availableVars <- setdiff(availableVars,varsToAdd)
        varsInModel <- c(varsToAdd,varsInModel)

    
    
    # 3) Add/Subtract bag and loop. Return AIC.
    
}

removeResponseVars <- function(data,y) {
    vars <- colnames(data)
    vars <- vars[vars != y]
    return(vars)
}

uniformRandom <- function(x)  {
    return(round(runif(1,min = 1, max = x)))
}

getMaxBag <- function(vars,lambda) {
    return(floor(lambda * length(vars)))
}

SMSR(y = "FlagGB",
     data = chileancredit,
     kappa = 1,
     lambda = 0.5,
     varRemove = NULL)
