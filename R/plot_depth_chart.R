plot_depth_chart <- function(team_name, team_colour){
  
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
  
  ggsave(filename = here::here(sprintf("results/%s_lineup_2018.png", team_name_file)),
         height = 9, width = 12)
  
  return(p)
}
