## get_appearances() function
get_player_appearances <- function(link, base_dates) {
  
  session <- bow(link)
  
  appearances_raw <- scrape(session) %>% 
    html_nodes("div.responsive-table:nth-child(3) > table:nth-child(1)") %>% 
    html_table(fill = TRUE)
  
  if(length(appearances_raw) > 0){
    appearances_raw <- appearances_raw[[1]] %>% 
      #magrittr::extract2(1) %>% 
      janitor::clean_names() %>% 
      slice(-n())
    
    appearances_clean <- appearances_raw %>% 
      select(date, home = home_team_2, away = away_team_2,
             result, position = pos, goal = x, assist = x_2,
             own_goal = x_3, yellow = x_4, dbl_yellow = x_5, red = x_6,
             sub_in = x_7, sub_out = x_8, minutes = x_9) %>% 
      ## fix minutes
      ## mutate_at(), mutate_if()...
      mutate(date = lubridate::mdy(date),
             minutes =  
               if_else(str_detect(minutes, "'"), 
                       str_replace_all(minutes, "'", ""), minutes),
             minutes = if_else(str_detect(minutes, "^[0-9]+$"),
                               minutes, "0") %>% as.numeric()) %>% 
      ## fix sub_in, sub_out
      mutate(sub_in =  
               if_else(str_detect(sub_in, "'"), 
                       str_replace_all(sub_in, "'", ""), sub_in),
             sub_in = if_else(str_detect(sub_in, "^[0-9]+$"),
                              sub_in, "0") %>% as.numeric(),
             sub_out =  
               if_else(str_detect(sub_out, "'"), 
                       str_replace_all(sub_out, "'", ""), sub_out),
             sub_out = if_else(str_detect(sub_out, "^[0-9]+$"),
                               sub_out, "0") %>% as.numeric(),
             yellow =  
               if_else(str_detect(yellow, "'"), 
                       str_replace_all(yellow, "'", ""), yellow),
             yellow = if_else(str_detect(yellow, "^[0-9]+$"),
                              yellow, "0") %>% as.numeric(),
             dbl_yellow =  
               if_else(str_detect(dbl_yellow, "'"), 
                       str_replace_all(dbl_yellow, "'", ""), dbl_yellow),
             dbl_yellow = if_else(str_detect(dbl_yellow, "^[0-9]+$"),
                                  dbl_yellow, "0") %>% as.numeric(),
             red =  
               if_else(str_detect(red, "'"), 
                       str_replace_all(red, "'", ""), red),
             red = if_else(str_detect(red, "^[0-9]+$"),
                           red, "0") %>% as.numeric()) %>% 
      ## fix goals/assists
      mutate(goal = if_else(str_detect(goal, "^[0-9]+$"),
                            goal, "0") %>% as.numeric(),
             assist = if_else(str_detect(assist, "^[0-9]+$"),
                              assist, "0") %>% as.numeric(),
             own_goal = if_else(str_detect(own_goal, "^[0-9]+$"),
                                own_goal, "0") %>% as.numeric()) %>% 
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
    
    ## deal with no match rows:
    ## basically using base df, anti_join on dates and 
    ## insert info for rows where missing
    add_df <- base_dates %>% 
      anti_join(appearances_clean, by = c("date"))
    
    ## combine missing data with existing
    appearances_clean <- appearances_clean %>% 
      full_join(add_df, by = c("date", "home", "away", "home_goal", "away_goal", "goal", "assist", "own_goal", "yellow", "dbl_yellow", "red", "sub_in", "sub_out", "minutes", "home_rank", "away_rank")) %>% 
      arrange(date)
  }
}