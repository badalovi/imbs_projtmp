library(testthat)



test_that(desc = 'sample function',
          {expect_type(object = sapply(list(1:5,3:8),mean),type = 'double')}
)

expect_length(1:3,3)   
method_vec <- c('smote','bsmote','adasyn','nearmiss','downsample','upsample')
model_vec  <- c('LogisticReg','RidgeReg','ElasticnetReg')

# Testing Type
test_that('Method and Model tuning methods worked correctly', {
  map(method_vec,~expect_success(expect_type(imbs_rf$method_list(.x),'list')))
  map(method_vec,~expect_success(expect_type(imbs_rf$method_fun(.x),'character')))
  map(method_vec,~expect_success(expect_type(imbs_rf$method_args(.x),'character')))
  map(method_vec,~expect_success(expect_type(imbs_rf$method_args(.x),'character')))
  map(method_vec,~expect_success(expect_type(imbs_rf$method_grid(.x),'list')))
  map(method_vec,~expect_success(expect_type(imbs_rf$method_lngth(.x),'integer')))
  map(method_vec,~expect_success(expect_output(imbs_rf$method_prog(1,.x),'% completed')))
  map(method_vec,~expect_success(expect_type(imbs_rf$method_getbest(.x),'list')))
  map(model_vec,~expect_success(expect_type(imbs_rf$model_list(.x),'list')))
  map(model_vec,~expect_success(expect_type(imbs_rf$model_fun(.x),'closure')))
  map(model_vec,~expect_success(expect_type(imbs_rf$model_args(.x),'list')))
  }
)


