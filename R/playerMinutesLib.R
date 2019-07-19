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

## get_appearances() function
get_appearances <- function(link, base_dates) {
  
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

get_appearances_df_raw <- function(team_comps = c("league", "cup"), squad_table_df, base_dates){
  appearances_df_raw <- data.frame()
  for(i in 1:nrow(squad_table_df)){
    appearances_comp_df_raw <- data.frame()
    for(j in 1:length(team_comps)){
      link <- squad_table_df[[i,j+2]]
      name <- squad_table_df$player_name[i]
      cat(i,":",name,"--",team_comps[j],"\n")
      appearances_comp_df_raw_i <- get_appearances(link = link, base_dates[[j]])
      if(length(appearances_comp_df_raw_i) > 0 ){
        appearances_comp_df_raw_i <- mutate(appearances_comp_df_raw_i, name = name)
        appearances_comp_df_raw <- rbind(appearances_comp_df_raw, appearances_comp_df_raw_i)
      }
    }
    appearances_df_raw <- rbind(appearances_df_raw, appearances_comp_df_raw)
    if(length(appearances_comp_df_raw) > 0 ){
      add_df <- do.call(rbind, base_dates) %>% 
        anti_join(appearances_comp_df_raw, by = c("date")) %>%
        mutate(name = name,
               position = "")
      appearances_df_raw <- rbind(appearances_df_raw, add_df)
    }
  }
  appearances_df_raw <- arrange(appearances_df_raw, name, date)
  return(appearances_df_raw)
}

plot_player_stats <- function(app_df){
  n_players <- n_distinct(subset(app_df, status %in% c("Started", "On as sub", "on the bench"))$name) + 0.5
  season_mins <- max(app_df$match_num)*90
  ## dataframes for vertical + horizontal divider lines
  divide_lines <- tibble(yint = seq(0.5, n_players, by = 1))
  
  game_dates <- unique(app_df$date)
  
  verticolo <- tibble(verts_start = seq(0, season_mins, by = 90),
                      verts_end = seq(0, season_mins, by = 90),
                      y_low = 0.5,
                      y_high = n_players)
  
  app_df %>% 
    ggplot(aes(x = dur, xend = end, y = name, yend = name)) + 
    ## Starters
    geom_segment(data = app_df %>% filter(minutes > 0 & sub_in == 0),
                 aes(group = match_num),
                 size = 5, color = "darkgreen") +
    ## Subs
    geom_segment(data = app_df %>% filter(sub_in > 0),
                 aes(group = match_num),
                 size = 5, color = "orange") +
    geom_segment(data = app_df %>% filter(red > 0 | dbl_yellow > 0),
                 aes(group = match_num),
                 size = 3, color = "red") +
    geom_segment(data = app_df %>% filter(yellow > 0),
                 aes(group = match_num),
                 size = 1, color = "yellow") +
    geom_segment(data = app_df %>% filter(status == "on the bench"),
                 aes(group = match_num),
                 size = 5, color = "skyblue") +
    geom_segment(data = verticolo, 
                 aes(x = verts_start, xend = verts_end, 
                     y = y_low, yend = y_high)) +
    ## Dividers
    geom_hline(data = divide_lines, aes(yintercept = yint),
               size = 0.5) +
    scale_x_continuous(breaks = seq(45, season_mins, 90),
                       labels = game_dates,
                       expand = c(0, 0)) +
    expand_limits(y = c(0.1, n_players + 3.5)) +
    # geom_isotype_col(img_width = grid::unit(0.5, "native"), img_height = NULL,
    #                  ncol = NA, nrow = 1, hjust = 0, vjust = 0.5) +
    labs(title = sprintf("Player Minutes | %s | Season %s-%s",team_name, season, season+1),
         #subtitle = "Players Ordered by Position (ST, MF, DF, GK)",
         x = "Minutes Played per Game", y = "",
         caption = glue::glue("
                            Data: transfermarkt.com
                            Adaptation of Code by: @R_by_Ryo")) +
    theme_minimal() +
    theme(text = element_text(family = "Roboto Condensed"),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(color = "black", size = 9, angle = 90),
          axis.text.y = element_text(color = "black", size = 10),
          panel.grid = element_blank(),
          plot.title = element_text(size = 20),
          plot.subtitle = element_text(size = 16),
          plot.caption = element_text(size = 12))
}

plot_minutes <- function(team_name, tm_team_id, season = 2018, team_comps = c("ENG1")){
  
  tm_team_name <- team_name
  tm_team_name_url <- str_replace_all(tolower(tm_team_name), " ", "-")
  team_name_file <- str_replace_all(team_name, " ", "_")
  
  positions <- c("AM", "CB", "CF", "CM", "DM", "GK", "LB", "LW", "RB", "RW", "SS", "RM", "LM", "RWB", "LWB")
  
  ## Squad details for given season
  url <- paste0("https://www.transfermarkt.com/", tm_team_name_url, "/leistungsdaten/verein/", 
                tm_team_id, "/reldata/", team_comps[1], "%26", season, "/plus/1/sort/im_kader.desc")
  
  session <- bow(url)
  
  squad_table_raw <- scrape(session) %>% 
    html_nodes(".hauptlink > div > span > a") %>% 
    html_attr("href")
  
  squad_table_df <- squad_table_raw %>% 
    enframe() %>% 
    select(-name) %>% 
    distinct() %>% 
    separate(value, into = c("1", "2", '3', '4', '5'), sep = "\\/") %>% 
    select(player_name = 2, id_num = 5)
  
  
  ## add links
  for(i in 1:length(team_comps)){
    squad_table_df <- squad_table_df %>% 
      mutate(!!paste0(team_comps[i],"_link") := glue::glue(
        "https://www.transfermarkt.com/{player_name}/leistungsdatendetails/spieler/{id_num}/saison/{season}/verein/{tm_team_id}/liga/0/wettbewerb/{team_comps[i]}/pos/0/trainer_id/0/plus/1") 
      )
  }
  
  glimpse(squad_table_df)
  
  base_dates <- get_base_dates(team_comps = team_comps, squad_table_df = squad_table_df)
  
  saveRDS(base_dates, file = here::here(sprintf("data/base_%s_%s_dates_df.RDS", team_name_file, season)))
  base_dates <- readRDS(file = here::here(sprintf("data/base_%s_%s_dates_df.RDS", team_name_file, season)))
  
  appearances_df_raw <- get_appearances_df_raw(team_comps = team_comps, squad_table_df = squad_table_df, base_dates = base_dates)
  saveRDS(appearances_df_raw, 
          file = glue(sprintf("{here::here()}/data/appearances_df_raw_%s_%s.RDS", team_name_file, season)))
  appearances_df_raw <- readRDS(
    file = glue(sprintf("{here::here()}/data/appearances_df_raw_%s_%s.RDS", team_name_file, season)))
  
  appearances_df_raw <- appearances_df_raw %>% 
    arrange(desc(date)) %>% 
    group_by(name) %>% 
    mutate(match_num = row_number()) %>%
    arrange(name, date)
  
  positions_df <- group_by(subset(appearances_df_raw, nchar(position) == 2), name, position) %>% summarise(n = n()) %>% arrange(desc(n)) %>% slice(1L)
  
  season_mins <- max(appearances_df_raw$match_num)*90
  
  appearances_df_raw <- rename(appearances_df_raw, status = position) %>% mutate(status = case_when(sub_in == 0 & minutes > 0 ~ "Started",
                                                                                                    sub_in > 0 ~ "On as sub",
                                                                                                    !status %in% positions ~ status,
  ))
  
  appearances_df_raw <- mutate(appearances_df_raw, minutes = case_when(status == "Started" ~ 90,
                                                                       sub_out > 0 ~ sub_out - sub_in,
                                                                       TRUE ~ minutes))
  
  appearances_df_clean <-  ungroup(appearances_df_raw) %>% group_by(date) %>%
    #mutate(minutes = ifelse(max(minutes)==120,minutes*0.75,minutes)) %>%
    group_by(name) %>%
    mutate(end = seq(from = 90, to = season_mins, by = 90),
           start = lag(end, default = 0),
           dur = if_else(minutes == 90 | status == "on the bench", start, end - minutes)) %>% 
    ## for sub-outs
    mutate(end = case_when(
      sub_out != 0 ~ start + sub_out,
      TRUE ~ end),
      dur = case_when(
        sub_out != 0 ~ start,
        TRUE ~ dur)) %>% 
    ungroup() %>% 
    left_join(., positions_df, by = "name") %>%
    ## fix names and label positions
    mutate(position = ifelse(is.na(position), "--", position),
           name = str_replace_all(name, "-", " ") %>% str_to_title(),
           name = as_factor(name)) %>% 
    arrange(name) %>%
    mutate(name = paste(url_decode(name)," ",position))
  
  ## save
  saveRDS(appearances_df_clean, 
          file = glue(sprintf("{here::here()}/data/appearances_df_%s_%s.RDS", team_name_file, season)))
  appearances_df_clean <- readRDS(
    file = glue(sprintf("{here::here()}/data/appearances_df_%s_%s.RDS", team_name_file, season)))
  
  appearances_df_clean <- group_by(appearances_df_clean, name) %>% 
    mutate(goal_bar = goal*30+start,      
           league_yellows = cumsum(yellow>0),
           red_card = (red>0 | dbl_yellow > 0)) %>% group_by(name) %>% 
    mutate(total_mins = sum(minutes)) %>% ungroup() %>% arrange(desc(total_mins), name, date)
  
  red_df <- subset(appearances_df_clean, red_card) %>% mutate()
  
  appearances_df_clean <- mutate(appearances_df_clean, name = as_factor(name) %>% fct_relevel(unique(appearances_df_clean$name))) %>% arrange(name, date)
  
  ## plot
  p <- plot_player_stats(appearances_df_clean)
  
  ggsave(filename = here::here(sprintf("results/player_minutes_%s_%s.png", team_name_file, season)),
         height = 9, width = 12)
  
  return(p)
}

plot_lineup <- function(team_name, team_colour){
  
  team_name_file <- str_replace_all(team_name, " ", "_")
  
  apps_df <- readRDS(
    file = glue::glue("data/appearances_df_raw_{team_name_file}_2018.RDS"))
  
  pos_df2 <- select(apps_df, name, date, home, away, position, minutes) %>% subset(minutes > 0 & position != "") %>%
    group_by(date) %>%
    mutate(position = ifelse(position == "SS", "CF", position),
           position = case_when(sum(position=="CF" & minutes >= 45) == 2 & position == "RW" ~ "RM",
                                sum(position=="CF" & minutes >= 45) == 2 & position == "LW" ~ "LM",
                                sum(position=="CF" & minutes >= 45) == 1 & position == "RM" ~ "RW",
                                sum(position=="CF" & minutes >= 45) == 1 & position == "LM" ~ "LW",
                                sum(position=="CM" & minutes >= 45) == 1 & position == "DM" ~ "CM",
                                TRUE ~ position)) 
  pos_tot <- group_by(pos_df2, name, position) %>% summarise(minutes = sum(minutes))
  pos_rnk <- group_by(pos_tot, position) %>% arrange(desc(minutes)) %>% 
    mutate(rank = row_number()) %>% ungroup()
  pos_rnk <- mutate(pos_rnk, position = case_when(position == "CM" & rank %% 2 == 0 ~ "LCM",
                                                  position == "CM" & rank %% 2 != 0 ~ "RCM",
                                                  position == "CB" & rank %% 2 == 0 ~ "LCB",
                                                  position == "CB" & rank %% 2 != 0 ~ "RCB",
                                                  TRUE ~ position),
                    rank = ifelse(position %in% c("RCB", "LCB", "RCM", "LCM"), ceiling(rank/2),rank),
                    name = str_replace_all(name, "-", " ") %>% str_to_title())
  
  pos_rnk <- mutate(pos_rnk, x = case_when(position %in% c("GK", "DM", "AM", "SS", "CF", "ST") ~ 50,
                                           position %in% c("LB", "LWB", "LM", "LW") ~ 11,
                                           position %in% c("RCB", "RCM") ~ 65,
                                           position %in% c("LCB", "LCM") ~ 35,
                                           position %in% c("RB", "RWB", "RM", "RW") ~ 89),
                    y1 = case_when(position == "GK" ~ 8.4,
                                   position %in% c("RB", "RCB", "LCB", "LB") ~ 20,
                                   position %in% c("RWB", "LWB", "DM") ~ 35,
                                   position %in% c("RCM", "LCM", "RM", "LM") ~ 48,
                                   position %in% c("AM", "RW", "LW", "SS") ~ 70,
                                   position == "CF" ~ 90))
  pos_rnk <- mutate(pos_rnk, y = y1-(rank-1)*2,
                    position = ifelse(position == "RCM" & !"LCM" %in% pos_rnk$position, "CM", position))  
  
  pos_point <- subset(pos_rnk, rank == 1) %>% select(position, x, y) %>% unique() %>% mutate(y = y + 4)
  
  
  
  grey_pos <- group_by(pos_rnk, position) %>% summarise(minutes = sum(minutes)) %>% 
    arrange(desc(minutes)) %>% mutate(rank = row_number()) %>% subset(rank > 11)
  
  grey_pos_rnk <- subset(pos_rnk, position %in% grey_pos$position)
  pos_rnk <- subset(pos_rnk, !position %in% grey_pos$position)
  
  p <- ggplot(data = pos_rnk, aes(x = x, y = y)) 
  p <- addPitchToGraph(p) +
    geom_point(data = subset(pos_point, !position %in% grey_pos$position), colour = team_colour, size = 8) +
    geom_point(data = subset(pos_point, position %in% grey_pos$position), colour = "grey", alpha = 0.2, size = 8) +
    geom_text(stat = "identity", label = pos_point$position, data = pos_point, size = 2) +
    xlim(0, 100) + ylim(0,100) +
    theme(axis.title = element_blank(), axis.ticks = element_blank(),  axis.text = element_blank(),
          panel.grid = element_blank(), panel.background = element_rect(fill = '#009933', colour = 'white')) +
    annotate(geom = "text", x = pos_rnk$x, y = pos_rnk$y, 
             label = sprintf("%s (%s)", pos_rnk$name, pos_rnk$minutes), colour = "white", size = 2.5) + 
    annotate(geom = "text", x = grey_pos_rnk$x, y = grey_pos_rnk$y, 
             label = sprintf("%s (%s)", grey_pos_rnk$name, grey_pos_rnk$minutes), colour = "white", alpha = 0.75, size = 2.5)
  p
  
  ggsave(filename = here::here(sprintf("results/%s_lineup_%s.png", team_name_file, season)),
         height = 9, width = 12)
  
  return(p)
}
