library(dplyr)
library(ggplot2)

source(glue('{here::here()}/addPitchToGraph.R'))
source(glue('{here::here()}/R/playerMinutesLib.R'))

plot_lineup("Aston Villa", "firebrick")
plot_lineup("FC Arsenal", "red")
plot_lineup("AFC Bournemouth", "firebrick")

plot_lineup("Persepolis FC", "red")
plot_lineup("Tractor Sazi FC", "salmon")
