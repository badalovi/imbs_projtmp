# Setting methods metadata map

imbs_map <-list(
  method =
    list(
      smote = list(
        fun  = 'step_smote',
        args = c('recipe','over_ratio','neighbors',''),
        best = list(),
        ind  = 1:2,
        data = tibble(),
        grid = tibble(
          expand.grid(
            over_ratio = seq(0.1,1,by=0.1),
            neighbors = 3:16,
            perf       = 0,
            method     = 'smote'
          )
        )
      ),
      
      rose = list(
        fun  = 'step_rose',
        args = c('recipe','over_ratio','minority_smoothness',''),
        best = list(),
        ind  = 1:4,
        data = tibble(),
        grid = tibble(
          expand.grid(
            over_ratio           = seq(0.1,1,by=0.1),
            minority_prop        = seq(0.1,1,by=0.25),
            minority_smoothness  = seq(0.1,1,by=0.25),
            majority_smoothness  = seq(0.1,1,by=0.25),
            perf          = 0,
            method        = 'rose'
          )
        )
      ),
      
      bsmote = list(
        fun  = 'step_bsmote',
        args = c('recipe','over_ratio','neighbors',''),
        best = list(),
        ind  = 1:2,
        data = tibble(),
        grid = tibble(
          expand.grid(
            over_ratio = seq(0.1,1,by=0.1),
            neighbors = 2:3,
            perf       = 0,
            method     = 'bsmote'
          )
        )
      ),
      
      adasyn = list(
        fun  = 'step_adasyn',
        args = c('recipe','over_ratio','neighbors',''),
        best = list(),
        ind  = 1:2,
        data = tibble(),
        grid = tibble(
          expand.grid(
            over_ratio = seq(0.1,1,by=0.1),
            neighbors = 3:16,
            perf       = 0,
            method     = 'adasyn'
          )
        )
      ),
      
      nearmiss = list(
        fun  = 'step_nearmiss',
        args = c('recipe','under_ratio','neighbors',''),
        best = list(),
        ind  = 1:2,
        data = tibble(),
        grid = tibble(
          expand.grid(
            under_ratio = seq(0.1,1,by=0.1),
            neighbors = 3:16,
            perf       = 0,
            method     = 'adasyn'
          )
        )
      ),
      
      downsample = list(
        fun  = 'step_downsample',
        args = c('recipe','under_ratio',''),
        best = list(),
        ind  = 1,  
        data = tibble(),
        grid = tibble(
          expand.grid(
            under_ratio = seq(0.1,1,by=0.1),
            perf        = 0,
            method      = 'downsample'
          )
        )
      ),
      
      upsample = list(
        fun  = 'step_upsample',
        args = c('recipe','over_ratio',''),
        best = list(),
        data = tibble(),
        grid = tibble(
          expand.grid(
            over_ratio = seq(0.1,1,by=0.1),
            perf        = 0,
            method      = 'upsample'
          )
        )
      ),
      
      tomek = list(
        fun  = 'step_tomek',
        args = NULL,
        best = list(),
        data = tibble(),
        grid = tibble()
      )
        
    ),
  
  model =
    list(
      LogisticReg = list(
        spec  = logistic_reg,
        argsd = c('mode','engine'), 
        args  = list(mode = 'classification',
                     engine = 'glm'
        ),
        grid  = NULL
      ),
      
      RidgeReg = list(
        spec  = logistic_reg,
        argsd = c('mode','engine','mixture'),
        args  = list(mode    = 'classification',
                     engine  = 'glmnet',
                     mixture = 0,
                     penalty = tune()
        ),
        grid  = grid_regular(penalty(),
                             levels = 20
                             )
      ),
      
      LassoReg = list(
        spec  = logistic_reg,
        grid  = grid_regular(penalty(),levels = 20),
        argsd = c('mode','engine','mixture'),
        args  = list(mode    = 'classification',
                     engine  = 'glmnet',
                     mixture = 1,
                     penalty = tune()
        ),
        grid  = grid_regular(penalty(),
                             levels = 20
                             )
      ),
      
      ElasticnetReg = list(
        spec  = logistic_reg,
        argsd = c('mode','engine'),
        args  = list(mode    = 'classification',
                     engine  = 'glmnet',
                     mixture = tune(),
                     penalty = tune()
        ),
        grid  = grid_regular(penalty(),
                             mixture(),
                             levels = 20
                             )
      ),
      
      DecisionTree = list(
        spec  = decision_tree,
        argsd = c('mode','engine'),
        args  = list(mode            = 'classification',
                     engine          = 'rpart',
                     cost_complexity = tune(),
                     tree_depth      = tune(),
                     min_n           = tune()
        ),
        grid  = grid_regular(cost_complexity(),
                             tree_depth(),
                             min_n(),
                             levels = 20
                             )
      )
   )
)



# Setting imbs class

imbs_map_rf <- setRefClass('imbs',
                           fields = list(
                             map_list = 'list',
                             methods  = 'character',
                             models   = 'character'
                           ),
                           methods = list(
                             method_list = function(method){
                               return(map_list$method[[method]])
                             },
                             
                             method_fun = function(method){
                               return(method_list(method)$fun)
                             },
                             
                             method_args = function(method){
                               return(method_list(method)$args)
                             },
                             
                             method_grid = function(method){
                               return(method_list(method)$grid)
                             },
                             
                             method_ind = function(method){
                               return(method_list(method)$ind)
                             },
                             
                             method_gridin = function(method,i){
                               
                               params <-
                                 method_grid(method) %>%
                                 map(.x = .[method_ind(method)], .f = ~.x[i])
                               
                               return(params)
                             },
                             
                             method_lngth = function(method,seql=T){
                               if(seql){
                                 l <- seq_len(nrow(method_grid(method)))
                               }
                               else{
                                 l <- nrow(method_grid(method))
                               }
                               return(l)
                             },
                             
                             method_rbase = function(formula,data){
                               
                               recipe(
                                 formula = formula, 
                                 data    = data
                               )
                             },
                             
                             method_prog = function(i,m){
                               cat('\014')
                               cat(paste0(m,': ',round(i /method_lngth(m,F) * 100), '% completed\n'))
                             },
                             
                             method_perf = function(data_aug,i,method,val_method){
                               
                               switch(val_method,
                                 'none' =
                                   {
                                     measure  <- list(roc_auc,accuracy,j_index)
                                     msr_args <- list(
                                       auc      = list(data_aug,truth='bad_flag',estimate='.pred_0'),
                                       accuracy = list(data_aug,truth='bad_flag',estimate='.pred'),
                                       j_index  = list(data_aug,truth='bad_flag',estimate='.pred')
                                     )
                                     
                                     perf_df <-
                                       pmap(list(measure,msr_args), ~do.call(.x,.y)) %>%
                                       map(~select(.x,-.estimator)) %>%
                                       do.call(bind_rows,.) %>% 
                                       mutate(val_method = val_method) %>% 
                                       list()
                                     
                                     map_list$method[[method]]$grid$perf[i] <<- perf_df
                                 
                                   },
                                 
                                 'cv' =,

                                 'validation' =
                                   {
                                     perf_df <-
                                       data_aug %>% 
                                       collect_metrics() %>% 
                                       rename(.estimate=mean) %>% 
                                       select(.metric,.estimate) %>%
                                       mutate(val_method = val_method) %>% 
                                       list()
                                     
                                     map_list$method[[method]]$grid$perf[i] <<- perf_df
                                   } 
                                
                               )
                                 
                             },
                             
                             method_bestune = function(method,recipe_base,criteria){
                               
                               best_par <-
                                 method_grid(method) %>% 
                                 unnest(perf) %>% 
                                 filter(.metric==criteria) %>% 
                                 top_n(n = 1, wt = .estimate) %>%
                                 select(-c(.metric:method)) %>%
                                 as.list()
                               
                               
                               recipe_list <- 
                                 append(
                                   list(recipe = recipe_base,
                                        'bad_flag'),
                                   best_par,
                                   after = 1
                                 )
                               
                               best_data <- 
                                 do.call(
                                   method_fun(method),
                                   recipe_list
                                 ) %>% 
                                 prep(new_data=NULL) %>% 
                                 juice()
                               
                               map_list$method[[method]]$data <<- best_data
                               map_list$method[[method]]$best <<- best_par
                             },
                             
                             method_getbest = function(method){
                               method_list(method)$best
                             },
                             
                             method_setbest = function(method,...){
                               map_list$method[[method]]$best <<- list(...)
                             },
                             
                             method_bestout = function(methods){
                               cat('\014')
                               cat('Method tuning done',
                                   '\nIntermediate datasets for methods are exporting...')
                               
                               map(set_names(methods,methods),
                                   ~method_list(.x)$data) %>% 
                                 write.xlsx(file = paste0(path_int,'/best_method_data.xlsx'))
                               
                               map(set_names(methods,methods),
                                   ~method_list(.x)$grid %>% unnest(perf)) %>% 
                                 write.xlsx(file = paste0(path_int,'/method_grid.xlsx'))
                             },
                             
                             method_spec = function(method){
                               
                               if(method=='nosample'){
                                 imbs_rf$method_rbase(arg_formula,arg_data)
                               }
                               else{
                                 recipe_list <- 
                                   append(
                                     list(recipe = method_rbase(arg_formula,arg_data),
                                          'bad_flag'),
                                     imbs_rf$method_getbest(method),
                                     after = 1
                                     )
                                 
                                 do.call(imbs_rf$method_fun(method),
                                         recipe_list
                                         )
                               }
                             },
                             
                             
                             # Model Tuning
                             model_list = function(model){
                               return(map_list$model[[model]])
                             },
                             
                             model_fun = function(model){
                               return(model_list(model)$spec)
                             },
                             
                             model_grid = function(model){
                               return(model_list(model)$grid)
                             },
                             
                             model_args = function(model){
                               return(model_list(model)$args)
                             },
                             
                             model_setbest = function(model,...){
                               
                               args <- list(...)
                               .ind <- names(args) %in% imbs_rf$model_list(model)$argsd
                               if(any(.ind) & !is.null(args)){
                                 args <- args[!.ind]
                                 message('Model default arguments can not be changed!',
                                         '\nOnly ',names(args),' will be used for ',model)
                               }
                               
                               if(!is.null(args)){
                                 args_vec <- which(
                                   names(model_list(model)$args) %in% names(args)
                                 ) 
                                 map_list$model[[model]]$args[args_vec] <<- args 
                               }
                             },
                             
                             model_bestout = function(wf_sample){
                               
                               wf_sample %>%
                                 select(wflow_id,result) %>%
                                 unnest(result) %>%
                                 select(wflow_id,.metrics) %>%
                                 unnest(.metrics) %>% 
                                 group_by(wflow_id) %>%
                                 group_split() %>%
                                 set_names(wf_sample %>% pull(wflow_id) %>% sort()) %>% 
                                 map(ungroup) %>% 
                                 map(~ .x %>% select_if(~ all(!is.na(.x)))) %>% 
                                 map(~ .x %>% select(-c( .estimator,.estimate))) %>% 
                                 map(~ .x %>% inner_join(wf_sample %>% collect_metrics(),
                                                         by=c('wflow_id','.config','.metric'))) %>% 
                                 map(~ .x %>% distinct()) %>% 
                                 write.xlsx(file = paste0(path_int,'/model_grid.xlsx'))
                             },
                             
                             model_spec = function(model){
                               do.call(
                                 model_fun(model),
                                 model_args(model)
                               )
                             }
                           )
)
 


# Setting function to create full recipe


imbs_tune_method <- function(data,
                             method,
                             model,
                             model_tune,
                             formula,
                             val_method,
                             criteria){
  
  # Creating imbs class object
  imbs_rf <- imbs_map_rf$new(
    map_list = imbs_map,
    methods  = method,
    models   = model
  )
  

  
  # Argument matching
  
  methods_vec <-
   c('smote','rose','bsmote',
     'adasyn','nearmiss','downsample',
     'upsample'
   )
  
  method <- method[method %in% methods_vec]
  
  if(!length(method)){
    assign('imbs_rf',imbs_rf,envir = .GlobalEnv)
    message('Tuning has not been done since all selected methods do not require tuning.
            Continue with modelling step.')
    return(invisible(NULL))
  }
  
  stopifnot(
    (all(method %in% methods_vec) & all(!method %in% 'all')) | method %in% 'all'
  )
  
  stopifnot(
    is.data.frame(data) | is_tibble(data) | is_formula(formula)
  )
  
  if(length(method)==1 && method=='all'){
    method <- methods_vec
  }
  
  val_method <- match.arg(val_method, c('none','cv','validation'))
  critera    <- match.arg(criteria, c('roc_auc','accuracy','precision'))
  
  
  
  # Selecting best model to tune methods
  recipe_base <-
    imbs_rf$method_rbase(formula,data)
  
  spec_base <- 
    imbs_rf$model_spec(model_tune)
  
  if(model_tune != 'LogisticReg'){
    
    grid_base <-
      imbs_rf$model_grid(model_tune) 
    
    rsmpl_base <-
      vfold_cv(data = arg_data,v = 5,strata = all_of(target))
    
    wrkflw_base <-
      workflow(recipe_base,spec_base) 
    
    tune_base <-
      tune_grid(
        object    = wrkflw_base,
        resamples = rsmpl_base,
        grid      = grid_base
      )
    
    spec_final <-
      finalize_workflow(
        wrkflw_base,
        tune_base %>% 
          select_best(arg_criteria)
      ) %>%
      extract_spec_parsnip()
  }
  else{
    spec_final <- spec_base
  }
  
  
  
  # Method Tuning
  for(m in method){
    
    for(i in imbs_rf$method_lngth(m)){
      
      imbs_rf$method_prog(i,m)
      
      recipe_list <- append(
        list(recipe = recipe_base,
             'bad_flag'),
        imbs_rf$method_gridin(method = m,i = i),
        after = 1
      )
      
      recipe_final <- 
        do.call(
          imbs_rf$method_fun(m),
          recipe_list
        )
      
      wf_set <-
        workflow() %>%
        add_recipe(recipe_final) %>%
        add_model(spec_final)
      
      
      switch(val_method,
             'none' = 
               wf_set %>%
               fit(data = data) %>%
               augment(new_data = data) %>%
               mutate(.pred=factor(ifelse(.pred_1>0.5,1,0))) %>% 
               imbs_rf$method_perf(i = i,method = m,val_method=val_method),
             
             'cv' = 
               fit_resamples(
                 object = wf_set,
                 metrics = metric_set(roc_auc,accuracy,j_index),
                 vfold_cv(data = arg_data, v = 5, strata = all_of(target))
               ) %>% 
               imbs_rf$method_perf(i = i, method = m, val_method = val_method),
             
             'validation' =
               fit_resamples(
                 object = wf_set,
                 metrics = metric_set(roc_auc,accuracy,j_index),
                 validation_split(data = arg_data, prop = 0.2, strata  = all_of(target))
               ) %>% 
               imbs_rf$method_perf(i = i, method = m, val_method = val_method)
      )
    }
    
    imbs_rf$method_bestune(method = m,recipe_base = recipe_base,criteria = criteria)
  }
  
  imbs_rf$method_bestout(method)
  assign('imbs_rf',imbs_rf,envir = .GlobalEnv)
}



imbs_tune_model <- function(data,
                            model,
                            metrics,
                            formula,
                            model_par,
                            val_model,
                            tune_ln){
  
  prep_ind <- model %in% names(model_par)
  model_par <- model_par[model[prep_ind]]
  
  model_list <-
    map2(model[prep_ind],model_par,function(m,a){
      imbs_rf$model_set_args(m,a)
      imbs_rf$model_spec(m)
    }
    ) %>%
    append(
      map(model[!prep_ind],~imbs_rf$model_spec(.x))
    ) %>% 
    set_names(c(model[prep_ind],model[!prep_ind]))
  
  
  
  method_list <-
    map(imbs_rf$methods,~imbs_rf$method_spec(.x)) %>% 
    set_names(imbs_rf$methods)
  
  
  wf_set <- 
    workflow_set(
      preproc = method_list,
      models  = model_list,
      cross   = TRUE
    )
  
  train_resamples <-
    switch (val_model,
            'cv' = 
              vfold_cv(
                data   = data,
                strata = all_of(target),
                v      = 5
              ),
            'validation' =
              validation_split(
                data   = data,
                strata = all_of(target),
                prop   = 0.2
              )
    )
  
  
  class_metric <- metrics
  
  
  wf_sample_exp <-
    workflow_map(
      object     = wf_set,
      resamples  = train_resamples,
      verbose    = TRUE,
      metrics    = class_metric,
      seed       = 123,
      grid       = tune_ln
    )
  
  imbs_rf$model_bestout(wf_sample_exp)
  assign('wf_full',wf_sample_exp,envir = .GlobalEnv)

}
