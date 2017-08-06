#' Modify the state of the city
#' 
#' Modify the state of the city for the Hamurabi game
#' @export
#' @importFrom stats runif
#' @param state list representing the current state of the city, with elements 
#' acres_owned, acres_planted, people, bushels_fed, bushels_store
#' @return a new value of state
#' @details corresponds to the code starting at line 511 of the BASIC version
ham_year <- function(state){
  # state <- list(acres_owned = 1000, acres_planted =500, people =100, bushels_fed =1800, bushels_store = 200)
  # Harvest
  orig_state <- state
  
  yield <- sample(1:5, 1)
  harvest <- yield * state$acres_planted
  
  # Rats
  if(runif(1) > 0.6){
    rats_eaten <- floor(state$bushels_store / sample(c(2, 4), 1))
  } else {
    rats_eaten <- 0
  }
  
  state$bushels_store <- state$bushels_store + harvest - rats_eaten
  state$acres_planted <- 0
  
  # Population changes
  starved <- max(floor(state$people - state$bushels_fed / 20), 0)
  state$bushels_fed <- 0
  
  new_people <- floor(sample(1:5, 1) * 
                        (20 * state$acres_owned + state$bushels_store) / 
                        state$people / 100 + 1)

  state$people <- state$people + new_people - starved
  
  plague_people <- ifelse(runif(1) < 0.15, floor(state$people / 2), 0)
  
  state$people <- state$people - plague_people
  
  # return values of state and of the changes that happened
  changes <- list(yield = yield, 
                  rats_eaten = rats_eaten, 
                  new_people = new_people, 
                  starved = starved,
                  prop_starved = starved / orig_state$people,
                  plague_people = plague_people)
  
  return(list(state= state, changes = changes))
}

