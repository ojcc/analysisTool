## pacman pkg to load/install libraries from cran
## polite is a github only pkg though.
pacman::p_load(tidyverse, polite, urltools, scales, ggimage, ggforce,
               rvest, glue, extrafont, ggrepel, ggtextures)
loadfonts()

source(glue('{here::here()}/R/playerMinutesLib.R'))

plot_minutes(team_name = "Tractor Sazi FC", ####INSERT team's Transfermarkt name (As it would appear in URL)
             tm_team_id = 12935, ####INSERT team's Transfermarkt ID
             season = 2018, ####INSERT year the season started
             team_comps = c("IRN1", "IRNP")) #INSERT the Transfermarkt codes for the relevant competitions



