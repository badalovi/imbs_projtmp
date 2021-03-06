# Completed almost main part of imbs_tune_method
# Missing parts:
# - method argument matching +
# - method 'all' adjusting to run all methods +
# - tomek and nosample should not be included in imbs_rf since they dont have parameters to tune +
# - add setbest method to change best hyperparameters for model tuning step +
# - Divide imbs_map into two part for model metadata as well +
# - Improve gridf and grids to work with more than 2 parameters smoothly +
# - Set seed within method tuning +
# - Consider adding name to step_best metadata in method +
# - Think of data frame after method applied as metadata for user +
# - Export intermediate datas, best tuned data for each method+
# - Make model optional to user within method tuning step instead of forcing only logistic regression  +
# - Make map2 eligible for unequal model and arg lists since each model might not have user defined argument +
# - Add validation method argument into model tuning step +
# - Add model parameter grid range +
# - Add metrics argument into model tuning step +
# - Make model_args method's default argument inaccessible to user such as engine, mixture(in Ridge or Lasso) +
# - Add tune_vector(mixture,tree_depth) to adjust tuning length in model step+
# - Add nosample+tomek +


# - Method setbest should take more than one argument 
# - Align model setbest with method's one
# - Add model tuning intermediate data output function
# - Adjust output directory to make it in Data
# - Add test dataset evaluation/performance to model tuning step
# - When method tuning produce same performance over the grid, add if+warning



# - Find a way to control randomness in method tuning step
# - Think of parameter complexity in terms of method tuning

# - NOTE: When setting seed param within step_down/upsample it produces same performance
#   since proportion increase only adds new samples to previous(lower prop) one so data gets
#   same with the to the proportion extent of previous ratio.
# ---------------------------

# Method >> imbs_setref >> search within grid matrix >> assign/update grid result to step_grid in imbs_map
# Methods >> imbs_tune >> run process above for each method >> retrieve the best hypterparams for each method

# Model Tuning:
# 1. Argument adjustment
# 2. Preparing Model List
# 3. Preparing Recipe List
# 4. Cross Workflow


# - Write extensive unit test

