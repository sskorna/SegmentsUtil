# Functions for mining patterns in transactional type of data
# Based on minimal length of rule (in terms of numeber of variables used) and
# on minimal support value 

# library(arules)
get_cluster <- function(n, data, rules){
  # Helper function to get rules from arules object to readable filtering conditions as text
  vars = rules[[n]] %>% sub("\\=.*", "", .)
  cluster <- data %>% 
    filter(rules[[n]] %>% 
             gsub("=", "=='", .) %>% 
             paste0(collapse="'&") %>% 
             paste0("'") %>%
             parse(text=.) %>%
             eval()
    ) %>%
    select(all_of(vars)) %>%
    group_by_at(vars) %>%
    summarize(n = n())
  return(cluster)
}

clusters_tab <- function(data, id_cols, support=0.2, minlen=2, print_freq=TRUE, inspect_items=TRUE){
  # Main function for calculating and returnig table with items separated by rules with counts
  # Assuming that data have first two columns filled with ids
  transactions <- as(data %>% select(-all_of(id_cols)) %>% mutate_all(as.factor), 
                     'transactions')
  if(print_freq == TRUE){
    itemFrequencyPlot(transactions)
  } 
  items <- apriori(transactions,
                   parameter=list(target='frequent itemsets', 
                                  support=support,
                                  minlen=minlen))
  if(inspect_items == TRUE){
    inspect(items)
  }
  
  rules_ls <- as(items@items, 'list')
  
  tab_clusters <- 1:length(rules_ls) %>%
    map(~ get_cluster(data=data, rules=rules_ls, n=.x)) %>%
    # reduce list to tibble 
    Reduce(union_all, .) %>%
    # rearrange columns
    select(-n, n) 
  return(tab_clusters)
}
