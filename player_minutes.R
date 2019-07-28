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

get_appearances(team_name = "Tractor Sazi FC", ####INSERT team's Transfermarkt name (As it would appear in URL)
             tm_team_id = 12935, ####INSERT team's Transfermarkt ID
             season = 2018, ####INSERT year the season started
             team_comps = c("IRN1", "IRNP")) #INSERT the Transfermarkt codes for the relevant competitions


plot_player_minutes("Tractor Sazi FC", 2018)
plot_depth_chart("Tractor Sazi FC", "salmon")
