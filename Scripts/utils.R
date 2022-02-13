# Setting methods metadata map

imbs_map <-list(
  method =
    list(
      smote = list(
        step_fun  = 'step_smote',
        step_args = c('recipe','over_ratio','neighbors',''),
        step_best = list(),
        step_grid = tibble(
          expand.grid(
            over_ratio = seq(0.1,1,by=0.1),
            neighbours = 3:16,
            perf       = 0,
            method     = 'smote'
          )
        )
      ),
      
      rose = list(
        step_fun  = 'step_rose',
        step_args = c('recipe','over_ratio','minority_smoothness',''),
        step_best = list(),
        step_grid = tibble(
          expand.grid(
            over_ratio    = seq(0.1,1,by=0.1),
            minority_smth = seq(0.1,1,by=0.1),
            perf          = 0,
            method        = 'rose'
          )
        )
      ),
      
      bsmote = list(
        step_fun  = 'step_bsmote',
        step_args = c('recipe','over_ratio','neighbors',''),
        step_best = list(),
        step_grid = tibble(
          expand.grid(
            over_ratio = seq(0.1,1,by=0.1),
            neighbours = 2:3,
            perf       = 0,
            method     = 'bsmote'
          )
        )
      ),
      
      adasyn = list(
        step_fun  = 'step_adasyn',
        step_args = c('recipe','over_ratio','neighbors',''),
        step_best = list(),
        step_grid = tibble(
          expand.grid(
            over_ratio = seq(0.1,1,by=0.1),
            neighbours = 3:16,
            perf       = 0,
            method     = 'adasyn'
          )
        )
      ),
      
      nearmiss = list(
        step_fun  = 'step_nearmiss',
        step_args = c('recipe','under_ratio','neighbors',''),
        step_best = list(),
        step_grid = tibble(
          expand.grid(
            under_ratio = seq(0.1,1,by=0.1),
            neighbours = 3:16,
            perf       = 0,
            method     = 'adasyn'
          )
        )
      ),
      
      downsample = list(
        step_fun  = 'step_downsample',
        step_args = c('recipe','under_ratio',''),
        step_best = list(),
        step_grid = tibble(
          expand.grid(
            under_ratio = seq(0.1,1,by=0.1),
            perf        = 0,
            method      = 'downsample'
          )
        )
      ),
      
      upsample = list(
        step_fun  = 'step_upsample',
        step_args = c('recipe','over_ratio',''),
        step_best = list(),
        step_grid = tibble(
          expand.grid(
            under_ratio = seq(0.1,1,by=0.1),
            perf        = 0,
            method      = 'upsample'
          )
        )
      )
    ),
  
  model =
    list(
      LogisticReg = list(
        spec = logistic_reg,
        args  = list(mode = 'classification',
                     engine = 'glm'
        )
      ),
      
      RidgeReg = list(
        spec = logistic_reg,
        args  = list(mode    = 'classification',
                     engine  = 'glm',
                     mixture = 0,
                     penalty = tune()
        )
      ),
      
      ElasticnetReg = list(
        spec = logistic_reg,
        args  = list(mode    = 'classification',
                     engine  = 'glm',
                     mixture = tune(),
                     penalty = tune()
        )
      )
    )
  )



# Setting imbs class

imbs_map_rf <- setRefClass('imbs',
                           fields = list(
                             map_list ='list',
                             methods  = 'character',
                             models   = 'character'
                           ),
                           methods = list(
                             method_list = function(method){
                               return(map_list$method[[method]])
                             },
                             
                             method_fun = function(method){
                               return(method_list(method)$step_fun)
                             },
                             
                             method_args = function(method){
                               return(method_list(method)$step_args)
                             },
                             
                             method_grid = function(method){
                               return(method_list(method)$step_grid)
                             },
                             
                             method_gridf = function(method){
                               return(pull(method_list(method)$step_grid,1))
                             },
                             
                             method_grids = function(method){
                               if(method %in% c('downsample','upsample')){
                                 return(NULL)
                               } 
                               else{
                                 return(pull(method_list(method)$step_grid,2))
                               }
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
                                     
                                     map_list$method[[method]]$step_grid$perf[i] <<- perf_df
                                 
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
                                     
                                     map_list$method[[method]]$step_grid$perf[i] <<- perf_df
                                   } 
                                
                               )
                                 
                             },
                             method_bestune = function(method,criteria){
                               
                               best_par <-
                                 method_grid(method) %>% 
                                 unnest(perf) %>% 
                                 filter(.metric==criteria) %>% 
                                 top_n(n = 1, wt = .estimate) %>%
                                 select(-c(.metric:method)) %>%
                                 pivot_longer(cols = everything()) %>% 
                                 pull(value)
                               
                               map_list$method[[method]]$step_best <<- best_par
                             },
                             
                             method_setbest = function(method,...){
                               map_list$method[[method]]$step_best <<- c(...)
                             },
                             
                             # Model Tuning
                             model_list = function(model){
                               return(map_list$model[[model]])
                             },
                             
                             model_fun = function(model){
                               return(model_list(model)$spec)
                             },
                             
                             model_args = function(model){
                               return(model_list(model)$args)
                             },
                             
                             model_set_args = function(model,args){
                               
                               if(!is.null(args)){
                                 args_vec <- which(
                                   names(model_list(model)$args) %in% names(args)
                                 ) 
                                 map_list$model[[model]]$args[args_vec] <<- args 
                               }
                             }
                           )
)
 


# Setting function to create full recipe


imbs_tune_method <- function(data, method, formula, val_method, criteria){


  methods_vec <- 
    c('smote','rose','bsmote',
      'adasyn','nearmiss','downsample',
      'upsample'
      )
  
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
  
  
  imbs_rf <-imbs_map_rf$new(
    map_list = imbs_map,
    methods  = method
  )
  
  recipe_base <- recipe(
    formula = formula, 
    data    = data
  )

  logistic_reg_spc <-
    logistic_reg() %>%
    set_mode('classification') %>%
    set_engine('glm')

  
  for(m in method){
    
    for(i in imbs_rf$method_lngth(m)){
      
      imbs_rf$method_prog(i,m)
      
      step_list <- list(
        recipe_base,
        imbs_rf$method_gridf(m)[i],
        imbs_rf$method_grids(m)[i],
        'bad_flag'
      )
      
      recipe_final <- 
        do.call(
          imbs_rf$method_fun(m),
          set_names(
            discard(step_list,~is.null(.x) || is.na(.x)),
            imbs_rf$method_args(m)
          )
        ) 
      
      wf_set <-
        workflow() %>%
        add_recipe(recipe_final) %>%
        add_model(logistic_reg_spc)
      
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
    
    imbs_rf$method_bestune(method = m,criteria = criteria)
  }
  
  assign('imbs_rf',imbs_rf,envir = .GlobalEnv)
}


imbs_tune_model <- function(data, method, formula, val_method){
  
  method_list <- set_names(as.list(method),method)
  
  recipe_base <- recipe(
    formula = arg_formula, 
    data    = arg_data
  )
  
  recipe_list <-
    map(method_list,function(m){
      
      step_list <- list(
        recipe_base,
        imbs_rf$maplist(m)$step_best[1],
        imbs_rf$maplist(m)$step_best[2],
        'bad_flag'
      )
      
      do.call(imbs_rf$fun(m),
              set_names(
                discard(step_list,~is.null(.x) || is.na(.x)),
                imbs_rf$args(m)
              ))
    })
  
  return(recipe_list)
  
}