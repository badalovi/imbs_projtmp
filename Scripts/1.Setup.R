# Cleaning Environment & Setting Project Directory
rm(list=ls())
setwd('C:/Users/GN07HL/OneDrive - ING/Desktop/MyRchive/Projects/Own Projects/imbs_projtmp')

library(tidymodels)       # Modelling Framework
library(themis)           # Imbalanced Sampling
library(readr)            # Reading Data
library(purrr)            # Functional Programming  
library(openxlsx)         # Writing excel outputs
source('Scripts/utils.R') # Utility functions and Objects


# Data reading
dev <- read_csv('Data/input/dev.csv')


# Argument Setting


target     <- 'bad_flag' # Variable modelling on
predictors <- c('.')     # Independent variables, . means all in the data

arg_data       <- dev
arg_formula    <- as.formula(paste0(target,'~',predictors))
arg_recipe     <- recipe(arg_formula,arg_data)

arg_method     <- c('downsample')
arg_model_tune <- 'RidgeReg' # Model to tune methods
arg_val_method <- 'none'
arg_criteria   <- 'roc_auc'
arg_model      <- c('LogisticReg','ElasticnetReg','RidgeReg')
arg_val_model  <- 'cv'
arg_metrics    <- metric_set(roc_auc,accuracy,j_index)
arg_tune_ln    <- 20 
arg_model_par  <- list(ElasticnetReg=list(mixture=0.5),
                       RidgeReg=list(mixture=0.3,alpha=0.3)) 


# Transforming target variable into factor 
if(!is.factor(arg_data[target])){
  arg_data[target] <- factor(unlist(arg_data[target],use.names = F))
}



# Establishing Directory
path_out <- paste0('Output/out_',format(Sys.time(),'%y-%m-%d_%H-%M'))
path_int <- paste0(path_out,'/Intermediate_Output')
path_fin <- paste0(path_out,'/Final_Output')
if(!dir.exists(path_out)){
  walk(c(path_out,
         path_int,
         path_fin
         ),
      dir.create
      )
}

