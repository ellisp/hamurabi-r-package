cg <- ham_legal_moves(people = 100, bushels_store = 2800, acres_owned = 1000, price = 22)
head(cg)
dim(cg)
tail(cg)
range(cg$sell)

state <- cg[, ]
x <- cg[1, ]


cg$y <- apply(cg, 1, function(x){
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

table(cg$y)
head(cg)



library(tidyverse)
cg %>%
  as_tibble() %>%
  arrange(desc(y)) %>%
  slice(1:1000) %>% View

cg %>%
  filter(y == 4) %>%
  select(1:3) %>%
  sample_n(500) %>%
  pairs()
# note the evaluation basically doesn't take into account how many bushells you have left

# next job is to have this return not just the evaluation, but the state from ham_year, which can then
# have more evlauations done on that, and so on


# only the number
choice <- cg %>%
  filter(y == 4 & acres_planted > 500) %>%
  slice(1)

# the new number of people comes out directly from ham_year, and
# the number of bushels goes down by number of rats, but we
# would need to add in the yield * planted
new_state <- ham_year(choice)

choice
new_state

# would be good to rate by y first, then bushels_store after evaluation (or bushels_store per person?)

eval_matrix <- t(apply(cg, 1, function(x){
  # x is now a row of the candidate grid cd of possible moves.
  # evaluate how it turns out:
  state <- list(acres_owned = x["acres_owned"],
                acres_planted = x["acres_planted"],
                people = x["people"],
                bushels_fed = x["bushels_fed"],
                bushels_store = x["bushels_store"])
  hy <- ham_year(state)
  
  c(new_people     = hy$state$people, 
    new_bushels_pp = hy$state$bushels_store / hy$state$people,
    eval           = ham_eval(hy$changes$prop_starved, hy$state$acres_owned / hy$state$people, hy$state$people)$eval)  
}))

# best strategy is to feed all the people and hope you have a plague that kills them all.
cbind(cg, eval_matrix) %>%
  as_tibble() %>%
  select(-people, -bushels_store) %>% 
  arrange(desc(eval), desc(new_bushels_pp.bushels_store))
