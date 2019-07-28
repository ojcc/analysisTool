## base dates
get_base_dates <- function(team_comps = c("league", "cup"), squad_table_df){  
  
  base_dates <- list()  
  for(i in 1:length(team_comps)){
    base_url <- as.character(squad_table_df[[1,i+2]])  
    session <- bow(base_url)
    
    base_raw <- scrape(session) %>% 
      html_nodes("div.responsive-table:nth-child(3) > table:nth-child(1)") %>% 
      html_table(fill = TRUE) %>% 
      .[[1]] %>% 
      janitor::clean_names() %>% 
      slice(-n())
    
    base_dates_i <- base_raw %>% 
      select(date, home = home_team_2, away = away_team_2,
             result, goal = x, assist = x_2, own_goal = x_3,
             yellow = x_4, dbl_yellow = x_5, red = x_6,
             sub_in = x_7, sub_out = x_8, minutes = x_9) %>% 
      ## make sure minutes == 0 for BASE
      mutate(date = lubridate::mdy(date),
             minutes = 0) %>% 
      ## set sub_in, sub_out = 0
      mutate(sub_in = 0,
             sub_out = 0) %>% 
      ## set goals/assists = 0
      mutate(goal = 0,
             assist = 0,
             own_goal = 0,
             yellow = 0,
             dbl_yellow = 0,
             red = 0) %>% 
      ## separate result
      separate(result, into = c("home_goal", "away_goal"), 
               sep = ":", convert = TRUE) %>% 
      ## home - away and rank
      mutate(home_goal = as.integer(ifelse(nchar(home_goal) > 2, substr(home_goal, 1, 1), home_goal)),
             away_goal = as.integer(ifelse(nchar(away_goal) > 2, substr(away_goal, 1, 1), away_goal)),
             home_rank = home %>% str_extract("[0-9]+") %>% as.numeric,
             away_rank = away %>% str_extract("[0-9]+") %>% as.numeric,
             home = home %>% str_remove_all("\\(.*\\)"),
             away = away %>% str_remove_all("\\(.*\\)"))
    
    base_dates[[i]] <- base_dates_i
  }
  return(base_dates)
}
