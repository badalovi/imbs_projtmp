# Cleaning Environment & Setting Project Directory
rm(list=ls())
setwd('C:/Users/GN07HL/OneDrive - ING/Desktop/MyRchive/Projects/Own Projects/imbs_projtmp')

library(tidymodels)       # Modelling Framework
library(themis)           # Imbalanced Sampling
library(readr)            # Reading Data
library(purrr)            # Functional Programming  
source('Scripts/utils.R') # Utility functions and Objects


# Data reading
dev <- read_csv('Data/input/dev.csv')


# Argument Setting



target     <- 'bad_flag' # Variable modelling on
predictors <- c('.')     # Independent variables, . means all in the data

arg_data       <- dev
arg_method     <- c('rose','adasyn','bsmote') 
arg_formula    <- as.formula(paste0(target,'~',predictors))
arg_val_method <- 'none'
arg_criteria   <- 'auc'


models  <- 'LogisticRegression' # Bagging, RandomForest, 'DecisionTree'


# Seting Target variable


# Transforming target variable into factor 
if(!is.factor(arg_data[target])){
  arg_data[target] <- factor(unlist(arg_data[target],use.names = F))
}

