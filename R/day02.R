get_list <- function(file_nam) {
  lst <- read_csv(file_nam, 
                  col_names = FALSE,
                  show_col_types = FALSE) %>% 
    mutate(vars = str_count(X1, " ") + 1)
  
  c_names <- paste0("V", c(1:max(lst$vars)))
  
  lst <- lst %>% 
    separate(X1, c_names,
             fill = "right") %>% 
    select(-vars)  
    
  return(as.data.frame(sapply(lst, as.numeric)))
}



safe_reports <- function(rpts) {
  rpt_safety <- rpts %>% 
    {list(.[-length(.)], .[-1])} %>% 
    reduce(`-`) %>% 
    rename_all(~ str_c("diff", seq_along(.))) %>% 
    bind_cols(rpts, .) %>% 
    mutate(
      across( 
        .cols = starts_with("diff"),
        .fns = list(~ if_else((abs(.x) > 0 & abs(.x) <= 3) | is.na(.x),
                              0, 1)),
        .names = "unsafe_{.col}"),
      across( 
        .cols = starts_with("diff"),
        .fns = list(~ if_else((.x) > 0  | is.na(.x),
                              0, 1)),
        .names = "incr_{.col}"),
      across( 
        .cols = starts_with("diff"),
        .fns = list(~ if_else((.x) < 0  | is.na(.x),
                              0, 1)),
        .names = "decr_{.col}")
    ) %>% 
    mutate(
      not_gradual = rowSums(select(., contains("unsafe"))),
      not_incr = rowSums(select(., contains("incr"))),
      not_decr = rowSums(select(., contains("decr"))),
      safe = if_else(
        not_gradual == 0 & (not_incr == 0 | not_decr == 0),
        1, 0
      )
    )      
  
  
  total_safe = sum(rpt_safety$safe)
  
  return(total_safe)
}
