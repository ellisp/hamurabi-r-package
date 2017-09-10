# state <- latest_state$state

#' Identify legal moves
#' 
#' Given a state in the Hamurabi game, identify all legal moves
#' 
#' @export
#' @state existing state to evaluate
#' @price price in bushelsof buying or selling an acre
ham_legal_moves <- function(state, price = 22){
  
  minimum_feed <- ceiling(state$people * 20 * 0.45)
  biggest_buy <- floor((state$bushels_store - minimum_feed) / price)
  biggest_plant <- min(state$bushels_store - minimum_feed, state$acres_owned)
  
  full_grid <- expand.grid(
    sell = round(seq(from = -biggest_buy, to = state$acres_owned, length.out = 100)),
    feed = round(seq(from = minimum_feed, to = state$bushels_store, length.out = 100)),
    plant = round(seq(from = 0, to = biggest_plant, length.out = 100)))
  
  illegal <- with(full_grid,
                state$bushels_store + sell * price - feed - plant < 0 |
                  state$acres_owned - sell < plant
                )
  
  candidate_grid <- full_grid[!illegal, ]
}

