get_team_appearances <- function(team_comps = c("league", "cup"), squad_table_df, base_dates){
  appearances_df_raw <- data.frame()
  for(i in 1:nrow(squad_table_df)){
    appearances_comp_df_raw <- data.frame()
    for(j in 1:length(team_comps)){
      link <- squad_table_df[[i,j+2]]
      name <- squad_table_df$player_name[i]
      cat(i,":",name,"--",team_comps[j],"\n")
      appearances_comp_df_raw_i <- get_player_appearances(link = link, base_dates[[j]])
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