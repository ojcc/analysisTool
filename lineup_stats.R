library(dplyr)
library(ggplot2)

source(glue('{here::here()}/addPitchToGraph.R'))
source(glue('{here::here()}/R/playerMinutesLib.R'))

plot_depth_chart("Aston Villa", "firebrick")
plot_depth_chart("FC Arsenal", "red")
plot_depth_chart("AFC Bournemouth", "firebrick")

plot_depth_chart("Persepolis FC", "red")
plot_depth_chart("Tractor Sazi FC", "salmon")
