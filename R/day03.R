get_day03_inputs <- function(file_nam) {
  con <- file(file_nam, open="r")
  dat <- readLines(con)
  close(con)
  
  return(dat)
}

sum_of_products <- function(txt) {
  mul_list <- str_extract_all(txt, "mul\\([0-9]{1,3},[0-9]{1,3}\\)") %>% 
    as.data.frame(.[[1]])
  
  names(mul_list) <- "muls"
  
  prods <- mul_list %>% 
    mutate(
      V1 = str_extract(muls, "[0-9]{1,3},[0-9]{1,3}")
    ) %>% 
    separate(V1, c("V1","V2")) %>% 
    mutate(
      prod = as.numeric(V1) * as.numeric(V2)
    )
  
  return(sum(prods$prod))
}


sum_of_enabled_products <- function(txt) {
  mul_list <- str_remove_all(txt, "(don't\\(\\)(.*?)(do\\(\\)))") %>%
    str_extract_all("mul\\([0-9]{1,3},[0-9]{1,3}\\)") %>% 
    as.data.frame(.[[1]])
  
  names(mul_list) <- "muls"
  
  prods <- mul_list %>% 
    mutate(
      V1 = str_extract(muls, "[0-9]{1,3},[0-9]{1,3}")
    ) %>% 
    separate(V1, c("V1","V2")) %>% 
    mutate(
      prod = as.numeric(V1) * as.numeric(V2)
    )
  
  return(sum(prods$prod))
}
