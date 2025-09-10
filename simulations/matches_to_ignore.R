
library(dplyr)

source("functions/fit_dynamic_model.R")
load("raw-data/results.RData")

teams = unique(results$home_team)

# First 4 matches of each team
tmp = list()
for(i in 1:length(teams)) {
  tmp[[i]] = head(sort(c(which(results$home_team == teams[i]), which(results$away_team == teams[i]))), 4)
}
tmp = sort(unique(do.call(c, tmp)))

first_matches = results[tmp,]

matches_to_ignore = results %>%
  filter(season == 2015) %>%
  full_join(first_matches)

save(matches_to_ignore, file = "data/matches_to_ignore.RData")