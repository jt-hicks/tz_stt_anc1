tanz_data_process_allonly <- function(data, level = c('District','Region'), remove_before=NULL){
  if(is.null(remove_before)){
    remove_before <- min(data$yearmon)
  }
  level_call <- ifelse(level=='District','Council','Region')

  all <- data.frame(month = zoo::as.yearmon(data$yearmon),
                    tested = data$tested,
                    positive = data$positive,
                    site = data[,level_call])
  all <- all %>%
    filter(positive<=tested)

  split_tibble <- function(tibble, col = 'col') tibble %>% split(., .[, col])

  all_list <- split_tibble(all[all$month>=remove_before,],level_call)


  return(all_list)
}
