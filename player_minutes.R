## pacman pkg to load/install libraries from cran
## polite is a github only pkg though.
pacman::p_load(tidyverse, polite, urltools, scales, ggimage, ggforce,
               rvest, glue, extrafont, ggrepel, ggtextures)
loadfonts()

source(glue('{here::here()}/R/get_base_dates.R'))
source(glue('{here::here()}/R/get_appearances.R'))
source(glue('{here::here()}/R/get_player_appearances.R'))
source(glue('{here::here()}/R/get_team_appearances.R'))
source(glue('{here::here()}/R/plot_player_minutes.R'))
source(glue('{here::here()}/R/plot_depth_chart.R'))
source(glue('{here::here()}/lib/addPitchToGraph.R'))

get_appearances(team_name = "Havant amp Waterlooville FC", ####INSERT team's Transfermarkt name (As it would appear in URL)
             tm_team_id = 2794, ####INSERT team's Transfermarkt ID
             season = 2018) ####INSERT year the season started



plot_player_minutes("Havant amp Waterlooville FC", 2018)
plot_depth_chart("Gol Gohar Sirjan FC", "salmon")
