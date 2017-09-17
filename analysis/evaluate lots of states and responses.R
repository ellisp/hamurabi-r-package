# this script is about getting some random states of the city, letting
# the computer do one random (but legal) move, and seeing the result.  What could
# go wrong?

library(tidyverse)
library(mgcv)
library(doParallel)

cl <- makeCluster(7)
registerDoParallel(cl)

clusterEvalQ(cl, {
  library(hamurabi)
})

many_states <- expand.grid(
  people        = round(seq(from = 30, to = 150, length.out = 20)),
  orig_bushels_store = round(seq(from = 400, to = 5000, length.out = 20)),
  orig_acres_owned   = round(seq(from = 100, to = 2000, length.out = 20)),
  price              = round(seq(from = 17, to = 26, length.out = 5))
)

system.time({
  many_states_response <- t(parApply(cl, many_states, 1, function(x){
    turn <- ham_legal_moves(x[1], x[2], x[3], x[4], 1)
    state <- list(acres_owned = turn["acres_owned"],
                  acres_planted = turn["acres_planted"],
                  people = turn["people"],
                  bushels_fed = turn["bushels_fed"],
                  bushels_store = turn["bushels_store"])
    
    hy <- ham_year(state)
    
    result <- ham_eval(prop_starved = as.numeric(hy$changes$prop_starved), 
                       av_acres = hy$state$acres_owned / hy$state$people, 
                       last_pop = hy$state$people)$eval  
    
    return(unlist(c(turn, result = result)))
  }))

})

all_data <- as_tibble(cbind(many_states, many_states_response[ ,-5]))


table(all_data$result)

stopCluster(cl)