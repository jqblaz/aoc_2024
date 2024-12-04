get_data <- function(file) {
  read.table(file)
}

total_distance <- function(list_data) {
  #split lists and sort individually (is there an easier way to do this?)
  a <- sort(list_data[,1])
  b <- sort(list_data[,2])
  
  # calculate the difference between the paired lists
  diff <- sum(abs(a-b))
  
  return(diff)
}

similarity_score <- function(list_data) {
  # what numbers are in a?
  list1 <- select(list_data, V1)
  
  # count items in b
  tbl2 <- janitor::tabyl(list_data, V2)
  
  # merge counts with v1
  tots <- list1 %>% 
    dplyr::left_join(tbl2, by = c("V1" = "V2")) %>% 
    tidyr::replace_na(replace = list("n" = 0)) %>% 
    dplyr::mutate(
      totals = V1 * n
    )
  
  # calculate sum
  similarity = sum(tots$totals)
  
  return(similarity)
}
