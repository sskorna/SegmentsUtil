# Functions to separate variables into quantiles
# assuming that first two columns are ids
quantiles <- function(col, data, n_quantiles) {

  if(is.numeric(data[[col]])){
    # discretize variale into quantiles
    breaks <- unique(c(quantile(data[[col]], probs = seq(0, 1, by=1/n_quantiles))))
    labels <- 1:(length(breaks)-1)
    # labels <- paste0(1:(length(breaks)-1), 'q')
    disc_var <- cut(data[[col]], breaks=breaks, labels=labels, include.lowest=TRUE)
  }else{
    # non_numeric variable - leave as it is for now
    disc_var <- data[[col]]
  }
  disc_var <- list(disc_var)
  names(disc_var) <- col
  return(disc_var)
}

quantiles_wrapper <- function(data, n_quantiles_max, id_cols) {
  # loop through all variables except first two, which are assumed to be ids
  data_disc <- map(colnames(data %>% select(-all_of(id_cols))),
                   quantiles,
                   data=data,
                   n_quantiles=n_quantiles_max) %>%
    Reduce(bind_cols, .) %>%
    cbind(data[id_cols], .)

  return(data_disc)
}
