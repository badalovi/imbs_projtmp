# Cleaning Environment & Setting Project Directory
rm(list=ls())
setwd('C:/Users/GN07HL/OneDrive - ING/Desktop/MyRchive/Projects/Own Projects/imbs_projtmp')


source('Scripts/pkgs.R')  # Install and load relevant packages
source('Scripts/utils.R') # Utility functions and Objects


##########################  User Section ##########################

# Data reading
train <- read_csv('Data/input/dev.csv')


# Argument Setting

target     <- 'bad_flag' # Variable modelling on
predictors <- c('.')     # Independent variables, . means all in the data

arg_method     <- c('downsample','nosample')
arg_model_tune <- 'RidgeReg' # Model to tune methods
arg_val_method <- 'validation'
arg_criteria   <- 'roc_auc'
arg_model      <- c('RidgeReg','ElasticnetReg','DecisionTree')
arg_val_model  <- 'cv'
arg_metrics    <- metric_set(roc_auc,accuracy,j_index,precision)
arg_tune_ln    <- 10
arg_model_par  <- NULL

###################################################################


arg_data       <- train 
arg_formula    <- as.formula(paste0(target,'~',predictors))
arg_recipe     <- recipe(arg_formula,arg_data)


# Transforming target variable into factor 
if(!is.factor(arg_data[target])){
  arg_data[target] <- factor(unlist(arg_data[target],use.names = F))
}


# Establishing Directory
path_out <- paste0('Data/Output/out_',format(Sys.time(),'%y-%m-%d_%H-%M'))
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
