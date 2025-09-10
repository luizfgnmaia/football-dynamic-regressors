
library(dplyr)

load("raw-data/results.RData")
load("data/simulations.RData")
load("data/matches_to_ignore.RData")

mean_log <- function(x) {
  x = x[x>0]
  sum(x*log(x))
}

var_log <- function(x) {
  x = x[x>0]
  ex = sum(x*log(x))
  ex2 = sum(x*log(x)^2)
  ex2 - ex^2
}

results = results %>%
  anti_join(matches_to_ignore) %>%
  mutate(result = ifelse(home_goals > away_goals, home_team,
                         ifelse(home_goals == away_goals, "Draw", away_team)),
         score = paste(home_goals, away_goals, sep = "-"))

p_result_0_0 = NULL
p_result_0_15 = NULL
p_result_0_30 = NULL
p_result_0_45 = NULL
p_result_0_60 = NULL
p_result_0_75 = NULL

p_result_1_0 = NULL
p_result_1_15 = NULL
p_result_1_30 = NULL
p_result_1_45 = NULL
p_result_1_60 = NULL
p_result_1_75 = NULL

p_result_2_0 = NULL
p_result_2_15 = NULL
p_result_2_30 = NULL
p_result_2_45 = NULL
p_result_2_60 = NULL
p_result_2_75 = NULL

p_result_3_0 = NULL
p_result_3_15 = NULL
p_result_3_30 = NULL
p_result_3_45 = NULL
p_result_3_60 = NULL
p_result_3_75 = NULL

p_result_4_0 = NULL
p_result_4_15 = NULL
p_result_4_30 = NULL
p_result_4_45 = NULL
p_result_4_60 = NULL
p_result_4_75 = NULL

p_score_0_0 = NULL
p_score_0_15 = NULL
p_score_0_30 = NULL
p_score_0_45 = NULL
p_score_0_60 = NULL
p_score_0_75 = NULL

p_score_1_0 = NULL
p_score_1_15 = NULL
p_score_1_30 = NULL
p_score_1_45 = NULL
p_score_1_60 = NULL
p_score_1_75 = NULL

p_score_2_0 = NULL
p_score_2_15 = NULL
p_score_2_30 = NULL
p_score_2_45 = NULL
p_score_2_60 = NULL
p_score_2_75 = NULL

p_score_3_0 = NULL
p_score_3_15 = NULL
p_score_3_30 = NULL
p_score_3_45 = NULL
p_score_3_60 = NULL
p_score_3_75 = NULL

p_score_4_0 = NULL
p_score_4_15 = NULL
p_score_4_30 = NULL
p_score_4_45 = NULL
p_score_4_60 = NULL
p_score_4_75 = NULL

mean_log_result_0_0 = NULL
mean_log_result_0_15 = NULL
mean_log_result_0_30 = NULL
mean_log_result_0_45 = NULL
mean_log_result_0_60 = NULL
mean_log_result_0_75 = NULL

mean_log_result_1_0 = NULL
mean_log_result_1_15 = NULL
mean_log_result_1_30 = NULL
mean_log_result_1_45 = NULL
mean_log_result_1_60 = NULL
mean_log_result_1_75 = NULL

mean_log_result_2_0 = NULL
mean_log_result_2_15 = NULL
mean_log_result_2_30 = NULL
mean_log_result_2_45 = NULL
mean_log_result_2_60 = NULL
mean_log_result_2_75 = NULL

mean_log_result_3_0 = NULL
mean_log_result_3_15 = NULL
mean_log_result_3_30 = NULL
mean_log_result_3_45 = NULL
mean_log_result_3_60 = NULL
mean_log_result_3_75 = NULL

mean_log_result_4_0 = NULL
mean_log_result_4_15 = NULL
mean_log_result_4_30 = NULL
mean_log_result_4_45 = NULL
mean_log_result_4_60 = NULL
mean_log_result_4_75 = NULL

mean_log_score_0_0 = NULL
mean_log_score_0_15 = NULL
mean_log_score_0_30 = NULL
mean_log_score_0_45 = NULL
mean_log_score_0_60 = NULL
mean_log_score_0_75 = NULL

mean_log_score_1_0 = NULL
mean_log_score_1_15 = NULL
mean_log_score_1_30 = NULL
mean_log_score_1_45 = NULL
mean_log_score_1_60 = NULL
mean_log_score_1_75 = NULL

mean_log_score_2_0 = NULL
mean_log_score_2_15 = NULL
mean_log_score_2_30 = NULL
mean_log_score_2_45 = NULL
mean_log_score_2_60 = NULL
mean_log_score_2_75 = NULL

mean_log_score_3_0 = NULL
mean_log_score_3_15 = NULL
mean_log_score_3_30 = NULL
mean_log_score_3_45 = NULL
mean_log_score_3_60 = NULL
mean_log_score_3_75 = NULL

mean_log_score_4_0 = NULL
mean_log_score_4_15 = NULL
mean_log_score_4_30 = NULL
mean_log_score_4_45 = NULL
mean_log_score_4_60 = NULL
mean_log_score_4_75 = NULL

var_log_result_0_0 = NULL
var_log_result_0_15 = NULL
var_log_result_0_30 = NULL
var_log_result_0_45 = NULL
var_log_result_0_60 = NULL
var_log_result_0_75 = NULL

var_log_result_1_0 = NULL
var_log_result_1_15 = NULL
var_log_result_1_30 = NULL
var_log_result_1_45 = NULL
var_log_result_1_60 = NULL
var_log_result_1_75 = NULL

var_log_result_2_0 = NULL
var_log_result_2_15 = NULL
var_log_result_2_30 = NULL
var_log_result_2_45 = NULL
var_log_result_2_60 = NULL
var_log_result_2_75 = NULL

var_log_result_3_0 = NULL
var_log_result_3_15 = NULL
var_log_result_3_30 = NULL
var_log_result_3_45 = NULL
var_log_result_3_60 = NULL
var_log_result_3_75 = NULL

var_log_result_4_0 = NULL
var_log_result_4_15 = NULL
var_log_result_4_30 = NULL
var_log_result_4_45 = NULL
var_log_result_4_60 = NULL
var_log_result_4_75 = NULL

var_log_score_0_0 = NULL
var_log_score_0_15 = NULL
var_log_score_0_30 = NULL
var_log_score_0_45 = NULL
var_log_score_0_60 = NULL
var_log_score_0_75 = NULL

var_log_score_1_0 = NULL
var_log_score_1_15 = NULL
var_log_score_1_30 = NULL
var_log_score_1_45 = NULL
var_log_score_1_60 = NULL
var_log_score_1_75 = NULL

var_log_score_2_0 = NULL
var_log_score_2_15 = NULL
var_log_score_2_30 = NULL
var_log_score_2_45 = NULL
var_log_score_2_60 = NULL
var_log_score_2_75 = NULL

var_log_score_3_0 = NULL
var_log_score_3_15 = NULL
var_log_score_3_30 = NULL
var_log_score_3_45 = NULL
var_log_score_3_60 = NULL
var_log_score_3_75 = NULL

var_log_score_4_0 = NULL
var_log_score_4_15 = NULL
var_log_score_4_30 = NULL
var_log_score_4_45 = NULL
var_log_score_4_60 = NULL
var_log_score_4_75 = NULL

for(i in 1:length(simulations)) {
  p_result_0_0[i] = simulations[[i]]$`0`$`0`$Result[results$result[i]]
  p_result_0_15[i] = simulations[[i]]$`0`$`15`$Result[results$result[i]]
  p_result_0_30[i] = simulations[[i]]$`0`$`30`$Result[results$result[i]]
  p_result_0_45[i] = simulations[[i]]$`0`$`45`$Result[results$result[i]]
  p_result_0_60[i] = simulations[[i]]$`0`$`60`$Result[results$result[i]]
  p_result_0_75[i] = simulations[[i]]$`0`$`75`$Result[results$result[i]]
  
  p_result_1_0[i] = simulations[[i]]$`1`$`0`$Result[results$result[i]]
  p_result_1_15[i] = simulations[[i]]$`1`$`15`$Result[results$result[i]]
  p_result_1_30[i] = simulations[[i]]$`1`$`30`$Result[results$result[i]]
  p_result_1_45[i] = simulations[[i]]$`1`$`45`$Result[results$result[i]]
  p_result_1_60[i] = simulations[[i]]$`1`$`60`$Result[results$result[i]]
  p_result_1_75[i] = simulations[[i]]$`1`$`75`$Result[results$result[i]]
  
  p_result_2_0[i] = simulations[[i]]$`2`$`0`$Result[results$result[i]]
  p_result_2_15[i] = simulations[[i]]$`2`$`15`$Result[results$result[i]]
  p_result_2_30[i] = simulations[[i]]$`2`$`30`$Result[results$result[i]]
  p_result_2_45[i] = simulations[[i]]$`2`$`45`$Result[results$result[i]]
  p_result_2_60[i] = simulations[[i]]$`2`$`60`$Result[results$result[i]]
  p_result_2_75[i] = simulations[[i]]$`2`$`75`$Result[results$result[i]]
  
  p_result_3_0[i] = simulations[[i]]$`3`$`0`$Result[results$result[i]]
  p_result_3_15[i] = simulations[[i]]$`3`$`15`$Result[results$result[i]]
  p_result_3_30[i] = simulations[[i]]$`3`$`30`$Result[results$result[i]]
  p_result_3_45[i] = simulations[[i]]$`3`$`45`$Result[results$result[i]]
  p_result_3_60[i] = simulations[[i]]$`3`$`60`$Result[results$result[i]]
  p_result_3_75[i] = simulations[[i]]$`3`$`75`$Result[results$result[i]]
  
  p_result_4_0[i] = simulations[[i]]$`4`$`0`$Result[results$result[i]]
  p_result_4_15[i] = simulations[[i]]$`4`$`15`$Result[results$result[i]]
  p_result_4_30[i] = simulations[[i]]$`4`$`30`$Result[results$result[i]]
  p_result_4_45[i] = simulations[[i]]$`4`$`45`$Result[results$result[i]]
  p_result_4_60[i] = simulations[[i]]$`4`$`60`$Result[results$result[i]]
  p_result_4_75[i] = simulations[[i]]$`4`$`75`$Result[results$result[i]]
  
  p_score_0_0[i] = simulations[[i]]$`0`$`0`$Score[results$score[i]]
  p_score_0_15[i] = simulations[[i]]$`0`$`15`$Score[results$score[i]]
  p_score_0_30[i] = simulations[[i]]$`0`$`30`$Score[results$score[i]]
  p_score_0_45[i] = simulations[[i]]$`0`$`45`$Score[results$score[i]]
  p_score_0_60[i] = simulations[[i]]$`0`$`60`$Score[results$score[i]]
  p_score_0_75[i] = simulations[[i]]$`0`$`75`$Score[results$score[i]]
  
  p_score_1_0[i] = simulations[[i]]$`1`$`0`$Score[results$score[i]]
  p_score_1_15[i] = simulations[[i]]$`1`$`15`$Score[results$score[i]]
  p_score_1_30[i] = simulations[[i]]$`1`$`30`$Score[results$score[i]]
  p_score_1_45[i] = simulations[[i]]$`1`$`45`$Score[results$score[i]]
  p_score_1_60[i] = simulations[[i]]$`1`$`60`$Score[results$score[i]]
  p_score_1_75[i] = simulations[[i]]$`1`$`75`$Score[results$score[i]]
  
  p_score_2_0[i] = simulations[[i]]$`2`$`0`$Score[results$score[i]]
  p_score_2_15[i] = simulations[[i]]$`2`$`15`$Score[results$score[i]]
  p_score_2_30[i] = simulations[[i]]$`2`$`30`$Score[results$score[i]]
  p_score_2_45[i] = simulations[[i]]$`2`$`45`$Score[results$score[i]]
  p_score_2_60[i] = simulations[[i]]$`2`$`60`$Score[results$score[i]]
  p_score_2_75[i] = simulations[[i]]$`2`$`75`$Score[results$score[i]]
  
  p_score_3_0[i] = simulations[[i]]$`3`$`0`$Score[results$score[i]]
  p_score_3_15[i] = simulations[[i]]$`3`$`15`$Score[results$score[i]]
  p_score_3_30[i] = simulations[[i]]$`3`$`30`$Score[results$score[i]]
  p_score_3_45[i] = simulations[[i]]$`3`$`45`$Score[results$score[i]]
  p_score_3_60[i] = simulations[[i]]$`3`$`60`$Score[results$score[i]]
  p_score_3_75[i] = simulations[[i]]$`3`$`75`$Score[results$score[i]]
  
  p_score_4_0[i] = simulations[[i]]$`4`$`0`$Score[results$score[i]]
  p_score_4_15[i] = simulations[[i]]$`4`$`15`$Score[results$score[i]]
  p_score_4_30[i] = simulations[[i]]$`4`$`30`$Score[results$score[i]]
  p_score_4_45[i] = simulations[[i]]$`4`$`45`$Score[results$score[i]]
  p_score_4_60[i] = simulations[[i]]$`4`$`60`$Score[results$score[i]]
  p_score_4_75[i] = simulations[[i]]$`4`$`75`$Score[results$score[i]]
  
  mean_log_result_0_0[i] = mean_log(simulations[[i]]$`0`$`0`$Result)
  mean_log_result_0_15[i] = mean_log(simulations[[i]]$`0`$`15`$Result)
  mean_log_result_0_30[i] = mean_log(simulations[[i]]$`0`$`30`$Result)
  mean_log_result_0_45[i] = mean_log(simulations[[i]]$`0`$`45`$Result)
  mean_log_result_0_60[i] = mean_log(simulations[[i]]$`0`$`60`$Result)
  mean_log_result_0_75[i] = mean_log(simulations[[i]]$`0`$`75`$Result)
  
  mean_log_result_1_0[i] = mean_log(simulations[[i]]$`1`$`0`$Result)
  mean_log_result_1_15[i] = mean_log(simulations[[i]]$`1`$`15`$Result)
  mean_log_result_1_30[i] = mean_log(simulations[[i]]$`1`$`30`$Result)
  mean_log_result_1_45[i] = mean_log(simulations[[i]]$`1`$`45`$Result)
  mean_log_result_1_60[i] = mean_log(simulations[[i]]$`1`$`60`$Result)
  mean_log_result_1_75[i] = mean_log(simulations[[i]]$`1`$`75`$Result)
  
  mean_log_result_2_0[i] = mean_log(simulations[[i]]$`2`$`0`$Result)
  mean_log_result_2_15[i] = mean_log(simulations[[i]]$`2`$`15`$Result)
  mean_log_result_2_30[i] = mean_log(simulations[[i]]$`2`$`30`$Result)
  mean_log_result_2_45[i] = mean_log(simulations[[i]]$`2`$`45`$Result)
  mean_log_result_2_60[i] = mean_log(simulations[[i]]$`2`$`60`$Result)
  mean_log_result_2_75[i] = mean_log(simulations[[i]]$`2`$`75`$Result)
  
  mean_log_result_3_0[i] = mean_log(simulations[[i]]$`3`$`0`$Result)
  mean_log_result_3_15[i] = mean_log(simulations[[i]]$`3`$`15`$Result)
  mean_log_result_3_30[i] = mean_log(simulations[[i]]$`3`$`30`$Result)
  mean_log_result_3_45[i] = mean_log(simulations[[i]]$`3`$`45`$Result)
  mean_log_result_3_60[i] = mean_log(simulations[[i]]$`3`$`60`$Result)
  mean_log_result_3_75[i] = mean_log(simulations[[i]]$`3`$`75`$Result)
  
  mean_log_result_4_0[i] = mean_log(simulations[[i]]$`4`$`0`$Result)
  mean_log_result_4_15[i] = mean_log(simulations[[i]]$`4`$`15`$Result)
  mean_log_result_4_30[i] = mean_log(simulations[[i]]$`4`$`30`$Result)
  mean_log_result_4_45[i] = mean_log(simulations[[i]]$`4`$`45`$Result)
  mean_log_result_4_60[i] = mean_log(simulations[[i]]$`4`$`60`$Result)
  mean_log_result_4_75[i] = mean_log(simulations[[i]]$`4`$`75`$Result)
  
  mean_log_score_0_0[i] = mean_log(simulations[[i]]$`0`$`0`$Score)
  mean_log_score_0_15[i] = mean_log(simulations[[i]]$`0`$`15`$Score)
  mean_log_score_0_30[i] = mean_log(simulations[[i]]$`0`$`30`$Score)
  mean_log_score_0_45[i] = mean_log(simulations[[i]]$`0`$`45`$Score)
  mean_log_score_0_60[i] = mean_log(simulations[[i]]$`0`$`60`$Score)
  mean_log_score_0_75[i] = mean_log(simulations[[i]]$`0`$`75`$Score)
  
  mean_log_score_1_0[i] = mean_log(simulations[[i]]$`1`$`0`$Score)
  mean_log_score_1_15[i] = mean_log(simulations[[i]]$`1`$`15`$Score)
  mean_log_score_1_30[i] = mean_log(simulations[[i]]$`1`$`30`$Score)
  mean_log_score_1_45[i] = mean_log(simulations[[i]]$`1`$`45`$Score)
  mean_log_score_1_60[i] = mean_log(simulations[[i]]$`1`$`60`$Score)
  mean_log_score_1_75[i] = mean_log(simulations[[i]]$`1`$`75`$Score)
  
  mean_log_score_2_0[i] = mean_log(simulations[[i]]$`2`$`0`$Score)
  mean_log_score_2_15[i] = mean_log(simulations[[i]]$`2`$`15`$Score)
  mean_log_score_2_30[i] = mean_log(simulations[[i]]$`2`$`30`$Score)
  mean_log_score_2_45[i] = mean_log(simulations[[i]]$`2`$`45`$Score)
  mean_log_score_2_60[i] = mean_log(simulations[[i]]$`2`$`60`$Score)
  mean_log_score_2_75[i] = mean_log(simulations[[i]]$`2`$`75`$Score)
  
  mean_log_score_3_0[i] = mean_log(simulations[[i]]$`3`$`0`$Score)
  mean_log_score_3_15[i] = mean_log(simulations[[i]]$`3`$`15`$Score)
  mean_log_score_3_30[i] = mean_log(simulations[[i]]$`3`$`30`$Score)
  mean_log_score_3_45[i] = mean_log(simulations[[i]]$`3`$`45`$Score)
  mean_log_score_3_60[i] = mean_log(simulations[[i]]$`3`$`60`$Score)
  mean_log_score_3_75[i] = mean_log(simulations[[i]]$`3`$`75`$Score)
  
  mean_log_score_4_0[i] = mean_log(simulations[[i]]$`4`$`0`$Score)
  mean_log_score_4_15[i] = mean_log(simulations[[i]]$`4`$`15`$Score)
  mean_log_score_4_30[i] = mean_log(simulations[[i]]$`4`$`30`$Score)
  mean_log_score_4_45[i] = mean_log(simulations[[i]]$`4`$`45`$Score)
  mean_log_score_4_60[i] = mean_log(simulations[[i]]$`4`$`60`$Score)
  mean_log_score_4_75[i] = mean_log(simulations[[i]]$`4`$`75`$Score)
  
  var_log_result_0_0[i] = var_log(simulations[[i]]$`0`$`0`$Result)
  var_log_result_0_15[i] = var_log(simulations[[i]]$`0`$`15`$Result)
  var_log_result_0_30[i] = var_log(simulations[[i]]$`0`$`30`$Result)
  var_log_result_0_45[i] = var_log(simulations[[i]]$`0`$`45`$Result)
  var_log_result_0_60[i] = var_log(simulations[[i]]$`0`$`60`$Result)
  var_log_result_0_75[i] = var_log(simulations[[i]]$`0`$`75`$Result)
  
  var_log_result_1_0[i] = var_log(simulations[[i]]$`1`$`0`$Result)
  var_log_result_1_15[i] = var_log(simulations[[i]]$`1`$`15`$Result)
  var_log_result_1_30[i] = var_log(simulations[[i]]$`1`$`30`$Result)
  var_log_result_1_45[i] = var_log(simulations[[i]]$`1`$`45`$Result)
  var_log_result_1_60[i] = var_log(simulations[[i]]$`1`$`60`$Result)
  var_log_result_1_75[i] = var_log(simulations[[i]]$`1`$`75`$Result)
  
  var_log_result_2_0[i] = var_log(simulations[[i]]$`2`$`0`$Result)
  var_log_result_2_15[i] = var_log(simulations[[i]]$`2`$`15`$Result)
  var_log_result_2_30[i] = var_log(simulations[[i]]$`2`$`30`$Result)
  var_log_result_2_45[i] = var_log(simulations[[i]]$`2`$`45`$Result)
  var_log_result_2_60[i] = var_log(simulations[[i]]$`2`$`60`$Result)
  var_log_result_2_75[i] = var_log(simulations[[i]]$`2`$`75`$Result)
  
  var_log_result_3_0[i] = var_log(simulations[[i]]$`3`$`0`$Result)
  var_log_result_3_15[i] = var_log(simulations[[i]]$`3`$`15`$Result)
  var_log_result_3_30[i] = var_log(simulations[[i]]$`3`$`30`$Result)
  var_log_result_3_45[i] = var_log(simulations[[i]]$`3`$`45`$Result)
  var_log_result_3_60[i] = var_log(simulations[[i]]$`3`$`60`$Result)
  var_log_result_3_75[i] = var_log(simulations[[i]]$`3`$`75`$Result)
  
  var_log_result_4_0[i] = var_log(simulations[[i]]$`4`$`0`$Result)
  var_log_result_4_15[i] = var_log(simulations[[i]]$`4`$`15`$Result)
  var_log_result_4_30[i] = var_log(simulations[[i]]$`4`$`30`$Result)
  var_log_result_4_45[i] = var_log(simulations[[i]]$`4`$`45`$Result)
  var_log_result_4_60[i] = var_log(simulations[[i]]$`4`$`60`$Result)
  var_log_result_4_75[i] = var_log(simulations[[i]]$`4`$`75`$Result)
  
  var_log_score_0_0[i] = var_log(simulations[[i]]$`0`$`0`$Score)
  var_log_score_0_15[i] = var_log(simulations[[i]]$`0`$`15`$Score)
  var_log_score_0_30[i] = var_log(simulations[[i]]$`0`$`30`$Score)
  var_log_score_0_45[i] = var_log(simulations[[i]]$`0`$`45`$Score)
  var_log_score_0_60[i] = var_log(simulations[[i]]$`0`$`60`$Score)
  var_log_score_0_75[i] = var_log(simulations[[i]]$`0`$`75`$Score)
  
  var_log_score_1_0[i] = var_log(simulations[[i]]$`1`$`0`$Score)
  var_log_score_1_15[i] = var_log(simulations[[i]]$`1`$`15`$Score)
  var_log_score_1_30[i] = var_log(simulations[[i]]$`1`$`30`$Score)
  var_log_score_1_45[i] = var_log(simulations[[i]]$`1`$`45`$Score)
  var_log_score_1_60[i] = var_log(simulations[[i]]$`1`$`60`$Score)
  var_log_score_1_75[i] = var_log(simulations[[i]]$`1`$`75`$Score)
  
  var_log_score_2_0[i] = var_log(simulations[[i]]$`2`$`0`$Score)
  var_log_score_2_15[i] = var_log(simulations[[i]]$`2`$`15`$Score)
  var_log_score_2_30[i] = var_log(simulations[[i]]$`2`$`30`$Score)
  var_log_score_2_45[i] = var_log(simulations[[i]]$`2`$`45`$Score)
  var_log_score_2_60[i] = var_log(simulations[[i]]$`2`$`60`$Score)
  var_log_score_2_75[i] = var_log(simulations[[i]]$`2`$`75`$Score)
  
  var_log_score_3_0[i] = var_log(simulations[[i]]$`3`$`0`$Score)
  var_log_score_3_15[i] = var_log(simulations[[i]]$`3`$`15`$Score)
  var_log_score_3_30[i] = var_log(simulations[[i]]$`3`$`30`$Score)
  var_log_score_3_45[i] = var_log(simulations[[i]]$`3`$`45`$Score)
  var_log_score_3_60[i] = var_log(simulations[[i]]$`3`$`60`$Score)
  var_log_score_3_75[i] = var_log(simulations[[i]]$`3`$`75`$Score)
  
  var_log_score_4_0[i] = var_log(simulations[[i]]$`4`$`0`$Score)
  var_log_score_4_15[i] = var_log(simulations[[i]]$`4`$`15`$Score)
  var_log_score_4_30[i] = var_log(simulations[[i]]$`4`$`30`$Score)
  var_log_score_4_45[i] = var_log(simulations[[i]]$`4`$`45`$Score)
  var_log_score_4_60[i] = var_log(simulations[[i]]$`4`$`60`$Score)
  var_log_score_4_75[i] = var_log(simulations[[i]]$`4`$`75`$Score)
}

probabilities = results %>%
  cbind(p_result_0_0, p_result_0_15, p_result_0_30, p_result_0_45, p_result_0_60, p_result_0_75,
        p_result_1_0, p_result_1_15, p_result_1_30, p_result_1_45, p_result_1_60, p_result_1_75,
        p_result_2_0, p_result_2_15, p_result_2_30, p_result_2_45, p_result_2_60, p_result_2_75,
        p_result_3_0, p_result_3_15, p_result_3_30, p_result_3_45, p_result_3_60, p_result_3_75,
        p_result_4_0, p_result_4_15, p_result_4_30, p_result_4_45, p_result_4_60, p_result_4_75,
        
        p_score_0_0, p_score_0_15, p_score_0_30, p_score_0_45, p_score_0_60, p_score_0_75,
        p_score_1_0, p_score_1_15, p_score_1_30, p_score_1_45, p_score_1_60, p_score_1_75,
        p_score_2_0, p_score_2_15, p_score_2_30, p_score_2_45, p_score_2_60, p_score_2_75,
        p_score_3_0, p_score_3_15, p_score_3_30, p_score_3_45, p_score_3_60, p_score_3_75,
        p_score_4_0, p_score_4_15, p_score_4_30, p_score_4_45, p_score_4_60, p_score_4_75,
        
        mean_log_result_0_0, mean_log_result_0_15, mean_log_result_0_30, mean_log_result_0_45, mean_log_result_0_60, mean_log_result_0_75,
        mean_log_result_1_0, mean_log_result_1_15, mean_log_result_1_30, mean_log_result_1_45, mean_log_result_1_60, mean_log_result_1_75,
        mean_log_result_2_0, mean_log_result_2_15, mean_log_result_2_30, mean_log_result_2_45, mean_log_result_2_60, mean_log_result_2_75,
        mean_log_result_3_0, mean_log_result_3_15, mean_log_result_3_30, mean_log_result_3_45, mean_log_result_3_60, mean_log_result_3_75,
        mean_log_result_4_0, mean_log_result_4_15, mean_log_result_4_30, mean_log_result_4_45, mean_log_result_4_60, mean_log_result_4_75,
        
        mean_log_score_0_0, mean_log_score_0_15, mean_log_score_0_30, mean_log_score_0_45, mean_log_score_0_60, mean_log_score_0_75,
        mean_log_score_1_0, mean_log_score_1_15, mean_log_score_1_30, mean_log_score_1_45, mean_log_score_1_60, mean_log_score_1_75,
        mean_log_score_2_0, mean_log_score_2_15, mean_log_score_2_30, mean_log_score_2_45, mean_log_score_2_60, mean_log_score_2_75,
        mean_log_score_3_0, mean_log_score_3_15, mean_log_score_3_30, mean_log_score_3_45, mean_log_score_3_60, mean_log_score_3_75,
        mean_log_score_4_0, mean_log_score_4_15, mean_log_score_4_30, mean_log_score_4_45, mean_log_score_4_60, mean_log_score_4_75,
        
        var_log_result_0_0, var_log_result_0_15, var_log_result_0_30, var_log_result_0_45, var_log_result_0_60, var_log_result_0_75,
        var_log_result_1_0, var_log_result_1_15, var_log_result_1_30, var_log_result_1_45, var_log_result_1_60, var_log_result_1_75,
        var_log_result_2_0, var_log_result_2_15, var_log_result_2_30, var_log_result_2_45, var_log_result_2_60, var_log_result_2_75,
        var_log_result_3_0, var_log_result_3_15, var_log_result_3_30, var_log_result_3_45, var_log_result_3_60, var_log_result_3_75,
        var_log_result_4_0, var_log_result_4_15, var_log_result_4_30, var_log_result_4_45, var_log_result_4_60, var_log_result_4_75,
        
        var_log_score_0_0, var_log_score_0_15, var_log_score_0_30, var_log_score_0_45, var_log_score_0_60, var_log_score_0_75,
        var_log_score_1_0, var_log_score_1_15, var_log_score_1_30, var_log_score_1_45, var_log_score_1_60, var_log_score_1_75,
        var_log_score_2_0, var_log_score_2_15, var_log_score_2_30, var_log_score_2_45, var_log_score_2_60, var_log_score_2_75,
        var_log_score_3_0, var_log_score_3_15, var_log_score_3_30, var_log_score_3_45, var_log_score_3_60, var_log_score_3_75,
        var_log_score_4_0, var_log_score_4_15, var_log_score_4_30, var_log_score_4_45, var_log_score_4_60, var_log_score_4_75)

save(probabilities, file = "data/probabilities.RData")
