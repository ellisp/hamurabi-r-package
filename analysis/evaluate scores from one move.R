cg <- ham_legal_moves(100, 2800, 1000)
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