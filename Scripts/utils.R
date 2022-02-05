# Setting methods metadata map

imbs_map <-
  list(
    smote = list(
      step_fun  = 'step_smote',
      step_args = c('recipe','over_ratio','neighbors',''),
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
      step_grid = tibble(
        expand.grid(
          over_ratio = seq(0.1,1,by=0.1),
          neighbours = 2:4,
          perf       = 0,
          method     = 'bsmote'
        )
      )
    ),
    
    adasyn = list(
      step_fun  = 'step_adasyn',
      step_args = c('recipe','over_ratio','neighbors',''),
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
      step_grid = tibble(
        expand.grid(
          under_ratio = seq(0.1,1,by=0.1),
          perf        = 0,
          method      = 'upsample'
        )
      )
    )
  )


# Setting imbs class

imbs_map_rf <- setRefClass('imbs',
                           fields = list(
                             map_list ='list',
                             methods  = 'character'
                           ),
                           methods = list(
                             maplist = function(method){
                               return(map_list[[method]])
                             },
                             
                             fun = function(method){
                               return(maplist(method)$step_fun)
                             },
                             
                             args = function(method){
                               return(maplist(method)$step_args)
                             },
                             
                             grid = function(method){
                               return(maplist(method)$step_grid)
                             },
                             
                             gridf = function(method){
                               return(pull(maplist(method)$step_grid,1))
                             },
                             
                             grids = function(method){
                               if(method %in% c('downsample','upsample')){
                                 return(NULL)
                               } 
                               else{
                                 return(pull(maplist(method)$step_grid,2))
                               }
                             },
                             
                             lngth = function(method,seql=T){
                               if(seql){
                                 l <- seq_len(nrow(grid(method)))
                               }
                               else{
                                 l <- nrow(grid(method))
                               }
                               return(l)
                             },
                             
                             prog = function(i,m){
                               cat('\014')
                               cat(paste0(m,': ',round(i /lngth(m,F) * 100), '% completed\n'))
                             },
                             
                             perf = function(data_aug,i,method,val_method){
                               
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
                                     
                                     map_list[[method]]$step_grid$perf[i] <<- perf_df
                                 
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
                                     
                                     map_list[[method]]$step_grid$perf[i] <<- perf_df
                                   } 
                                
                               )
                                 
                             },
                             bestune = function(){
                               maplist()
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
  
  if(length(method)==1 & method=='all'){
    method <- methods_vec
  }
  
  val_method <- match.arg(val_method, c('none','cv','validation'))
  critera    <- match.arg(criteria, c('auc','accuracy','precision'))
  
  
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
    
    for(i in imbs_rf$lngth(m)){
      
      imbs_rf$prog(i,m)
      
      step_list <- list(
        recipe_base,
        imbs_rf$gridf(m)[i],
        imbs_rf$grids(m)[i],
        'bad_flag'
      )
      
      recipe_final <- 
        do.call(
          imbs_rf$fun(m),
          set_names(
            discard(step_list,is.null),
            imbs_rf$args(m)
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
               imbs_rf$perf(i = i,method = m,val_method=val_method),
             
             'cv' = 
               fit_resamples(
                 object = wf_set,
                 metrics = metric_set(roc_auc,accuracy,j_index),
                 vfold_cv(data = arg_data, v = 5, strata = all_of(target))
               ) %>% 
               imbs_rf$perf(i = i, method = m, val_method = val_method),
             
             'validation' =
               fit_resamples(
                 object = wf_set,
                 metrics = metric_set(roc_auc,accuracy,j_index),
                 validation_split(data = arg_data, prop = 0.2, strata  = all_of(target))
               ) %>% 
               imbs_rf$perf(i = i, method = m, val_method = val_method)
      )
    }
  }
  
  assign('imbs_rf',imbs_rf,envir = .GlobalEnv)
}




