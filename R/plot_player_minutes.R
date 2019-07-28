plot_player_minutes <- function(team_name, season){
  
  team_name_file <- str_replace_all(team_name, " ", "_")
  
  app_df <- readRDS(
    file = glue(sprintf("{here::here()}/data/appearances_df_%s_%s.RDS", team_name_file, season)))
  
  ggsave(filename = here::here(sprintf("results/player_minutes_%s_%s.png", team_name_file, season)),
         height = 9, width = 12)
  
  
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