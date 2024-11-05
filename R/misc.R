
amlu <- list(
  A = mean,
  M = median,
  L = function(x) quantile(x, 0.025, na.rm = T),
  U = function(x) quantile(x, 0.975, na.rm = T)
)


sample_table <- function(df, n_sims) {
  keys <- unique(df$Key)
  id_map <- tibble(ID = 1:n_sims, Key = sample(keys, n_sims, rep = T))
  
  id_map %>% 
    left_join(df, relationship = "many-to-many", by = "Key") %>% 
    select(-Key) %>% 
    rename(Key = ID)
}


shuffle_table <- function(df) {
  keys <- unique(df$Key)
  id_map <- tibble(ID = keys, Key = sample(unique(df$Key)))
  
  id_map %>% 
    left_join(df, relationship = "many-to-many", by = "Key") %>% 
    select(-Key) %>% 
    rename(Key = ID)
}


get_pars <- function(ps, i) {
  lapply(ps, function(df) {
    if(!is.data.frame(df)) {
      return(df)
    } else if ("Key" %in% names(df)) {
      return(df %>% filter(Key == i) %>% select(-Key))
    } else {
      return(df)
    }
  })
}
