# Cleaning Environment & Setting Project Directory
rm(list=ls())
setwd('C:/Users/GN07HL/OneDrive - ING/Desktop/MyRchive/imbs_proj')

library(tidymodels)       # Modelling Framework
library(themis)           # Imbalanced Sampling
library(readr)            # Reading Data
library(purrr)            # Functional Programming  
source('Scripts/utils.R') # Utility functions and Objects


# Data reading
dev <- read_csv('Data/input/dev.csv')


# Argument Setting

# Argument data 
data <- dev
# Argument methods take sampling methods as input, multiple methods can be given
methods <- 'smote' # 'all', smote', 'rose', 'bsmote', 'adasyn','nearmiss','downsample','upsample','tomek','upsample 
# Argument models take ML model names as input, multiple model can be given
models  <- 'LogisticRegression' # Bagging, RandomForest, 'DecisionTree'


# Seting Target variable
target     <- 'bad_flag' # Variable modelling on
predictors <- c('.')     # Independent variables, . means all in the data
formula <- as.formula(paste0(target,'~',predictors))

# Transforming target variable into factor 
if(!is.factor(data[target])){
  data[target] <- factor(unlist(data[target],use.names = F))
}

