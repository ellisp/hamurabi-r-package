# state <- latest_state$state

#' Identify legal moves
#' 
#' Given a state in the Hamurabi game, identify all legal moves
#' 
#' @export
#' @param people number of people currently in city
#' @param bushels_store number of bushels currently in store and available for planting or feeding people
#' @param acres_owned number of acres currently owned by city
#' @param price price in bushelsof buying or selling an acre
#' @param n number of legal movest to return.  If NULL, all legal moves are returned.
#' @return a data.frame with first three columns for sell, bushels_fed, and acres_planted which 
#' respectively are how many acres to sell (negative numbers meant to buy acres), how many 
#' bushels to feed the people, and how many acres to plan.  The constraints are that at least 
#' 9 bushels per people must be fed (as below this leads to automatic impeachment), and you can't 
#' go into bushels or land debt. Typically there are many legal moves eg around 1 million.
#' 
#' The final three columns acres_owned, people, and bushels_store are calculated based on what
#' you started with and the choice represented by the first three columns of each row. Between
#' them, these six numbers constitute a complete description of the state of the city, which 
#' can be fed one row at a time to ham_year(state)
#' @examples
#' cg <- ham_legal_moves(100, 2800, 1000)
#' dim(cg)
#' head(cg)
#' tail(cg)
#' range(cg$sell)
ham_legal_moves <- function(people, bushels_store, acres_owned, price = 22, n = NULL){
  
  minimum_feed <- ceiling(people * 20 * 0.45)
  biggest_buy <- floor((bushels_store - minimum_feed) / price)
  biggest_plant <- max(min(bushels_store - minimum_feed, acres_owned), 0)
  
  full_grid <- expand.grid(
    sell = round(seq(from = -biggest_buy, to = acres_owned, length.out = 100)),
    bushels_fed = round(seq(from = minimum_feed, to = bushels_store, length.out = 100)),
    acres_planted = round(seq(from = 0, to = biggest_plant, length.out = 100)))
  
  illegal <- with(full_grid,
                bushels_store + sell * price - bushels_fed - acres_planted < 0 |
                  acres_owned - sell < acres_planted |
                  acres_owned - sell < 0
                )
  
  candidate_grid <- full_grid[!illegal, ] 
  candidate_grid$acres_owned <- acres_owned - candidate_grid$sell
  candidate_grid$people <- people
  candidate_grid$bushels_store <- bushels_store - candidate_grid$bushels_fed + candidate_grid$sell * price
  
  if(!is.null(n)){
    candidate_grid <- candidate_grid[sample(1:nrow(candidate_grid), n), ]
  }
  
  return(candidate_grid)
}



