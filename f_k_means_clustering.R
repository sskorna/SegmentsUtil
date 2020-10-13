# Functions to tranform variables into clusters one by one
k_means <- function(col, data, k) {
  clustering <- kmeans(data[col], k)
  clustered <- as.factor(clustering$cluster)
  new_labels <- cbind(label = 1:k, clustering$center) %>%
    tibble::as_tibble() %>%
    arrange_at(col) %>%
    mutate(r_n = row_number()) %>%
    arrange_at('label') %>%
    pull(r_n) %>%
    as.character()
  levels(clustered) <- new_labels
  clustered_ordered <- list(clustered)

  names(clustered_ordered) <- col

  return(clustered_ordered)
}

k_means_wrapper <- function(data, k, id_cols) {

  data_clustered <- map(colnames(data %>% select(-all_of(id_cols))),
      k_means,
      data=data,
      k=k) %>%
    Reduce(bind_cols, .) %>%
    cbind(data[id_cols], .)

  return(data_clustered)
}
