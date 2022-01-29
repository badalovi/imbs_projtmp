# Setting methods metadata map

imbs_map <-
  list(
    smote = list(
      step_fun  = 'step_smote',
      step_args = c('recipe','over_ratio','neighbors','spec'),
      step_grid = tibble(
        expand.grid(
          over_ratio = seq(0.1,1,by=0.1),
          neighbours = 3:16,
          gini       = 0,
          perf       = 0,
          method     = 'smote' 
        )
      )
    ),
    
    
    rose = list(
      step_fun  = 'step_rose',
      step_args = c('recipe','over_ratio','minority_smoothness','spec'),
      step_grid = tibble(
        expand.grid(
          over_ratio    = seq(0.1,1,by=0.1),
          minority_smth = seq(0.1,1,by=0.1),
          gini          = 0,
          perf          = 0,
          method        = 'rose' 
        )
      )
    ),
    
    
    bsmote = list(
      step_fun  = 'step_bsmote',
      step_args = c('recipe','over_ratio','neighbors','spec'),
      step_grid = tibble(
        expand.grid(
          over_ratio = seq(0.1,1,by=0.1),
          neighbours = 2:4,
          gini       = 0,
          perf       = 0,
          method     = 'bsmote' 
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
                               return(pull(maplist(method)$step_grid,2))
                             },
                             grids = function(method){
                               return(pull(maplist(method)$step_grid,2))
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
                             gini = function(data,i,method){
                               
                               is_bad   <- unlist(data['bad_flag'])==1
                               preds    <- unlist(data['.pred_1'])
                               good_sum <- sum(!is_bad)
                               bad_sum  <- sum(is_bad)
                               
                               U <- sum(rank(preds)[!is_bad]) - good_sum*(good_sum+1)/2
                               res <- (1-U/good_sum/bad_sum)*2-1
                               map_list[[method]]$step_grid$gini[i] <<- res
                             },
                             perf = function(data_aug,i,method){
                               
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
                                 list()
                               
                               map_list[[method]]$step_grid$perf[i] <<- perf_df
                               
                             },
                             bestune = function(){
                               maplist()
                             }
                             
                             
                           )
                           
)


# Setting function to create full recipe


imbs_tune_method <- function(data, method, formula, val_method, criteria){
  
  # method     <- match.arg(method,c('all','smote','bsmote','rose','adasyn',
  #                                   'nearmiss','upsample','downsample','tomek'))
  val_method <- match.arg(val_method, c('none','cv','validation'))
  critera    <- match.arg(criteria, c('gini','accuracy','precision'))
  
  
  imbs_rf <-imbs_map_rf$new(
    map_list = imbs_map,
    methods  = method
  )
  
  recipe_base <- 
    recipe(formula, 
           data = data
    )
  
  logistic_reg_spc <- logistic_reg() %>%
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
            step_list,
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
               imbs_rf$perf(i = i,method = m),
             'cv' = 
               fit_resamples(
                 wf_set,
                 vfold_cv(data = data,
                          v    = 5,
                          strata = all_of(target)
                 )
               )
             
      )
    }
  }
  
  assign('imbs_rf',imbs_rf,envir = .GlobalEnv)
}




