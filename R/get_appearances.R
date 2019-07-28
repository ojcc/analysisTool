get_appearances <- function(team_name, tm_team_id, season = 2018, team_comps = c("ENG1")){
  
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
  
  appearances_df_raw <- get_team_appearances(team_comps = team_comps, squad_table_df = squad_table_df, base_dates = base_dates)
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
  
  appearances_df_clean <- group_by(appearances_df_clean, name) %>% 
    mutate(goal_bar = goal*30+start,      
           league_yellows = cumsum(yellow>0),
           red_card = (red>0 | dbl_yellow > 0)) %>% group_by(name) %>% 
    mutate(total_mins = sum(minutes)) %>% ungroup() %>% arrange(desc(total_mins), name, date)
  
  red_df <- subset(appearances_df_clean, red_card) %>% mutate()
  
  appearances_df_clean <- mutate(appearances_df_clean, name = as_factor(name) %>% fct_relevel(unique(appearances_df_clean$name))) %>% arrange(name, date)
  
  ## save
  saveRDS(appearances_df_clean, 
          file = glue(sprintf("{here::here()}/data/appearances_df_%s_%s.RDS", team_name_file, season)))
}
