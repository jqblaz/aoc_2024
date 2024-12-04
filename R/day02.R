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

get_reports <- function(list_dat) {
  rpt_data <- list_dat %>% 
    rownames_to_column(var = "rpt") %>% 
    pivot_longer(
      cols = -rpt,
      names_to = c(".value", "obs"),
      names_pattern = "(.)(.)"
    ) %>% 
    dplyr::filter(!is.na(V))
  
  return(rpt_data)
}

safe_reports <- function(rpts) {
  rpt_safety <- rpts %>% 
    group_by(rpt) %>% 
    mutate(
      diff = V - dplyr::lead(V),
      not_grad = if_else((abs(diff) > 0 & abs(diff) <= 3),
                         0, 1, NA),
      incr = if_else(diff > 0 , 1, 0, NA),
      decr = if_else(diff < 0, 1, 0, NA)
    ) %>% 
    summarise(
      not_grad = sum(not_grad, na.rm = T),
      not_incr = sum(incr, na.rm = T),
      not_decr = sum(decr, na.rm = T)
    ) %>% 
    ungroup() %>% 
    mutate(
      safe = if_else(not_grad == 0 & (not_incr == 0 | not_decr == 0), 1, 0)
    )
  
  total_safe = sum(rpt_safety$safe)
  
  return(total_safe)
}

safe_reports_dampened <- function(rpts_dat) {
  rpts <- rpts_dat %>% 
    group_by(rpt) %>% 
    mutate(
      diff = V - dplyr::lead(V),
      not_grad = if_else((abs(diff) > 0 & abs(diff) <= 3),
                         0, 1, NA),
      incr = if_else(diff > 0 , 1, 0, NA),
      decr = if_else(diff < 0, 1, 0, NA)
    ) %>% 
    ungroup()
  
  rpt_safetyind <- rpts %>% 
    group_by(rpt) %>% 
    summarise(
      not_grad = sum(not_grad, na.rm = T),
      not_incr = sum(incr, na.rm = T),
      not_decr = sum(decr, na.rm = T)
    ) %>% 
    ungroup() %>% 
    mutate(
      safe = if_else(not_grad == 0 & (not_incr == 0 | not_decr == 0), 1, 0)
    ) 
  
  safe_ids = dplyr::filter(rpt_safetyind, safe == 1)$rpt
  
  
  test1 <- rpts_dat %>% 
    dplyr::filter(!(rpt %in% safe_ids) & obs != 1) %>% 
    group_by(rpt) %>% 
    mutate(
      diff = V - dplyr::lead(V),
      not_grad = if_else((abs(diff) > 0 & abs(diff) <= 3),
                         0, 1, NA),
      incr = if_else(diff > 0 , 1, 0, NA),
      decr = if_else(diff < 0, 1, 0, NA)
    ) %>% 
    summarise(
      not_grad = sum(not_grad, na.rm = T),
      not_incr = sum(incr, na.rm = T),
      not_decr = sum(decr, na.rm = T)
    ) %>% 
    ungroup() %>% 
    mutate(
      safe = if_else(not_grad == 0 & (not_incr == 0 | not_decr == 0), 1, 0)
    ) %>% 
    dplyr::filter(safe == 1)
  
  safe_ids = c(safe_ids, test1$rpt)
  
  test2 <- rpts_dat %>% 
    dplyr::filter(!(rpt %in% safe_ids) & obs != 2) %>% 
    group_by(rpt) %>% 
    mutate(
      diff = V - dplyr::lead(V),
      not_grad = if_else((abs(diff) > 0 & abs(diff) <= 3),
                         0, 1, NA),
      incr = if_else(diff > 0 , 1, 0, NA),
      decr = if_else(diff < 0, 1, 0, NA)
    ) %>% 
    summarise(
      not_grad = sum(not_grad, na.rm = T),
      not_incr = sum(incr, na.rm = T),
      not_decr = sum(decr, na.rm = T)
    ) %>% 
    ungroup() %>% 
    mutate(
      safe = if_else(not_grad == 0 & (not_incr == 0 | not_decr == 0), 1, 0)
    ) %>% 
    dplyr::filter(safe == 1)
  
  safe_ids = c(safe_ids, test2$rpt)
  
  test3 <- rpts_dat %>% 
    dplyr::filter(!(rpt %in% safe_ids) & obs != 3) %>% 
    group_by(rpt) %>% 
    mutate(
      diff = V - dplyr::lead(V),
      not_grad = if_else((abs(diff) > 0 & abs(diff) <= 3),
                         0, 1, NA),
      incr = if_else(diff > 0 , 1, 0, NA),
      decr = if_else(diff < 0, 1, 0, NA)
    ) %>% 
    summarise(
      not_grad = sum(not_grad, na.rm = T),
      not_incr = sum(incr, na.rm = T),
      not_decr = sum(decr, na.rm = T)
    ) %>% 
    ungroup() %>% 
    mutate(
      safe = if_else(not_grad == 0 & (not_incr == 0 | not_decr == 0), 1, 0)
    ) %>% 
    dplyr::filter(safe == 1)
  
  safe_ids = c(safe_ids, test3$rpt)
  
  test4 <- rpts_dat %>% 
    dplyr::filter(!(rpt %in% safe_ids) & obs != 4) %>% 
    group_by(rpt) %>% 
    mutate(
      diff = V - dplyr::lead(V),
      not_grad = if_else((abs(diff) > 0 & abs(diff) <= 3),
                         0, 1, NA),
      incr = if_else(diff > 0 , 1, 0, NA),
      decr = if_else(diff < 0, 1, 0, NA)
    ) %>% 
    summarise(
      not_grad = sum(not_grad, na.rm = T),
      not_incr = sum(incr, na.rm = T),
      not_decr = sum(decr, na.rm = T)
    ) %>% 
    ungroup() %>% 
    mutate(
      safe = if_else(not_grad == 0 & (not_incr == 0 | not_decr == 0), 1, 0)
    ) %>% 
    dplyr::filter(safe == 1)
  
  safe_ids = c(safe_ids, test4$rpt)
  
  test5 <- rpts_dat %>% 
    dplyr::filter(!(rpt %in% safe_ids) & obs != 5) %>% 
    group_by(rpt) %>% 
    mutate(
      diff = V - dplyr::lead(V),
      not_grad = if_else((abs(diff) > 0 & abs(diff) <= 3),
                         0, 1, NA),
      incr = if_else(diff > 0 , 1, 0, NA),
      decr = if_else(diff < 0, 1, 0, NA)
    ) %>% 
    summarise(
      not_grad = sum(not_grad, na.rm = T),
      not_incr = sum(incr, na.rm = T),
      not_decr = sum(decr, na.rm = T)
    ) %>% 
    ungroup() %>% 
    mutate(
      safe = if_else(not_grad == 0 & (not_incr == 0 | not_decr == 0), 1, 0)
    ) %>% 
    dplyr::filter(safe == 1)
  
  safe_ids = c(safe_ids, test5$rpt)
  
  test6 <- rpts_dat %>% 
    dplyr::filter(!(rpt %in% safe_ids) & obs != 6) %>% 
    group_by(rpt) %>% 
    mutate(
      diff = V - dplyr::lead(V),
      not_grad = if_else((abs(diff) > 0 & abs(diff) <= 3),
                         0, 1, NA),
      incr = if_else(diff > 0 , 1, 0, NA),
      decr = if_else(diff < 0, 1, 0, NA)
    ) %>% 
    summarise(
      not_grad = sum(not_grad, na.rm = T),
      not_incr = sum(incr, na.rm = T),
      not_decr = sum(decr, na.rm = T)
    ) %>% 
    ungroup() %>% 
    mutate(
      safe = if_else(not_grad == 0 & (not_incr == 0 | not_decr == 0), 1, 0)
    ) %>% 
    dplyr::filter(safe == 1)
  
  safe_ids = c(safe_ids, test6$rpt)
  
  test7 <- rpts_dat %>% 
    dplyr::filter(!(rpt %in% safe_ids) & obs != 7) %>% 
    group_by(rpt) %>% 
    mutate(
      diff = V - dplyr::lead(V),
      not_grad = if_else((abs(diff) > 0 & abs(diff) <= 3),
                         0, 1, NA),
      incr = if_else(diff > 0 , 1, 0, NA),
      decr = if_else(diff < 0, 1, 0, NA)
    ) %>% 
    summarise(
      not_grad = sum(not_grad, na.rm = T),
      not_incr = sum(incr, na.rm = T),
      not_decr = sum(decr, na.rm = T)
    ) %>% 
    ungroup() %>% 
    mutate(
      safe = if_else(not_grad == 0 & (not_incr == 0 | not_decr == 0), 1, 0)
    ) %>% 
    dplyr::filter(safe == 1)
  
  safe_ids = c(safe_ids, test7$rpt)
  
  test8 <- rpts_dat %>% 
    dplyr::filter(!(rpt %in% safe_ids) & obs != 8) %>% 
    group_by(rpt) %>% 
    mutate(
      diff = V - dplyr::lead(V),
      not_grad = if_else((abs(diff) > 0 & abs(diff) <= 3),
                         0, 1, NA),
      incr = if_else(diff > 0 , 1, 0, NA),
      decr = if_else(diff < 0, 1, 0, NA)
    ) %>% 
    summarise(
      not_grad = sum(not_grad, na.rm = T),
      not_incr = sum(incr, na.rm = T),
      not_decr = sum(decr, na.rm = T)
    ) %>% 
    ungroup() %>% 
    mutate(
      safe = if_else(not_grad == 0 & (not_incr == 0 | not_decr == 0), 1, 0)
    ) %>% 
    dplyr::filter(safe == 1)
  
  safe_ids = c(safe_ids, test8$rpt)
  
  return(length(unique(safe_ids)))
}
