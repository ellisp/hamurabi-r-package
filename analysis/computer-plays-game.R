library(hamurabi)
library(tidyverse)

current_state <- list(people = 100, bushels_store = 2800, acres_owned = 1000)
statuses <- data.frame(people = numeric(), bushels_store = numeric(), acres_owned = numeric())
prop_starved <- numeric()

for(i in 1:9){
  price = sample(17:26, 1)
  statuses[i, ] <- unlist(current_state)
  # print(price)
  # candidate grid of legal moves
  cg <- with(current_state, ham_legal_moves(people, bushels_store, acres_owned, price))
  
  # subset of legal moves
  cg_small <- cg[sample(1:nrow(cg), 1000), ]
  
  # evaluate a possible outcome for each legal move
  cg_small$y <- apply(cg_small, 1, function(x){
    # x is now a row of the candidate grid cd of possible moves.
    # evaluate how it turns out:
    state <- list(acres_owned = x["acres_owned"],
                  acres_planted = x["acres_planted"],
                  people = x["people"],
                  bushels_fed = x["bushels_fed"],
                  bushels_store = x["bushels_store"])
    hy <- ham_year(state)
    ham_eval(hy$changes$prop_starved, hy$state$acres_owned / hy$state$people, hy$state$people)$eval  
  })
  
  # model predicting y based on all the elements of the candidate move
  model <- glm(I(y > 2) ~ sell * bushels_fed * acres_planted, family = "binomial", data = cg_small)
  
  # predicted chance of getting a 4 for *all* the candidate moves
  cg$pred <- predict(model, newdata = cg, type = "response")
  
  # choose a move!
  chosen <- sample_n(cg, size = 1, weight = pred)
  state <- list(acres_owned = chosen["acres_owned"],
                acres_planted = chosen["acres_planted"],
                people = chosen["people"],
                bushels_fed = chosen["bushels_fed"],
                bushels_store = chosen["bushels_store"])
  
  # how does it turn out
  hy <- ham_year(state)
  prop_starved <- c(prop_starved, as.numeric(hy$changes$prop_starved))
  
  current_state$acres_owned <- as.numeric(hy$state$acres_owned)
  current_state$people <- as.numeric(hy$state$people)
  current_state$bushels_store <- as.numeric(hy$state$bushels_store)
}

cbind(rbind(statuses, current_state), prop_starved = round(c(0, prop_starved), 2))

result <- ham_eval(prop_starved, 
         current_state$acres_owned / current_state$people, 
         last_pop = current_state$people)
cat(result$mess)
