
library(dplyr)

csi = 0

source("functions/fit_dynamic_model.R")
load("raw-data/results.RData")
load("raw-data/intervals.RData")
load("data/matches_to_ignore.RData")

# Fitting models for all dates of matches to be simulated
dates = results %>%
  anti_join(matches_to_ignore) %>%
  .$date %>%
  unique()

mod0 = list()
mod1 = list()
mod2 = list()
mod3 = list()
mod4 = list()

for(i in 1:length(dates)) {
  available_data = intervals %>%
    filter(date < dates[i])
  mod0[[i]] = fit_dynamic_model(available_data, csi = csi, estimation_date = dates[i], par_log_market_value_difference = FALSE, par_second_half = FALSE, par_goal_difference = FALSE, par_player_difference = FALSE)
  mod1[[i]] = fit_dynamic_model(available_data, csi = csi, estimation_date = dates[i], par_second_half = FALSE, par_goal_difference = FALSE, par_player_difference = FALSE)
  mod2[[i]] = fit_dynamic_model(available_data, csi = csi, estimation_date = dates[i], par_goal_difference = FALSE, par_player_difference = FALSE)
  mod3[[i]] = fit_dynamic_model(available_data, csi = csi, estimation_date = dates[i], par_player_difference = FALSE)
  mod4[[i]] = fit_dynamic_model(available_data, csi = csi, estimation_date = dates[i])
  print(paste0(round(100*i/length(dates), 2), "%"))
}

names(mod0) = dates
names(mod1) = dates
names(mod2) = dates
names(mod3) = dates
names(mod4) = dates

models = list(mod0, mod1, mod2, mod3, mod4)
names(models) = 0:4
save(models, file = "data/models.RData")
