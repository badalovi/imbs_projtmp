
imbs_load_libs <- function(){
  
  # Available Packages
  pkg_vec <- rownames(installed.packages())
  
  # Required Packages
  libs <- 
    c('tidymodels','themis',
      'readr','purrr',
      'openxlsx'
      )
  
  pkg_install <- libs[!libs %in% pkg_vec]
  
  if(length(pkg_install)){
    invisible(sapply(pkg_install, install.packages))
    message('Following packages are installed: ',pkg_install)
  }
  
  invisible(sapply(libs, require,character.only = TRUE))
}


imbs_load_libs()