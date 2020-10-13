# Functions to calculate averages accros grouped variables
avg_tab <- function(col, data){
  var_avg <- data %>%
    group_by_at(paste0(col, '_group')) %>%
    summarize_at(paste0(col, '_raw'), mean) 
  names(var_avg) <- c('group', paste0(col, '_avg'))
  var_avg$group <- as.character(var_avg$group)
  return(var_avg)
}

avg_tab_wrapper <- function(data_orig, data_grouped, id_cols){
# loop through all variables
  data_trans_avg <- data_orig %>%
    inner_join(data_grouped, by = id_cols, suffix = c('_raw', '_group'))
  
  cols_to_calc <- colnames(
    data_orig %>%
    # all except ids
      select(-all_of(id_cols)) %>%
    # only numeric variables
      select_if(is.numeric)
  )
  
  tab_avgs <- map(cols_to_calc, 
                  avg_tab, 
                  data=data_trans_avg) %>%
    Reduce(function(x, y) full_join(x, y, by='group'), .) %>%
    mutate_at('group', as.numeric) %>%
    arrange(group) %>%
    mutate_at('group', as.character)
  return(tab_avgs)
}
