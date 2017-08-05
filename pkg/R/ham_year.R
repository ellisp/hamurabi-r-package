

#' @param state list representing the current state of the city, with elements 
#' acres_owned, acres_planted, people, bushells_fed, bushells_store
#' @value a new value of state
#' @details corresponds to the code starting at line 511 of the BASIC version
ham_year <- function(state){
  # state <- list(acres_owned = 1000, acres_planted =500, people =100, bushells_fed =1800, bushells_store = 200)
  # Harvest
  yield <- sample(1:5, 1)
  harvest <- yield * state$acres_planted
  
  # Rats
  if(runif(1) > 0.6){
    rats_eaten <- state$bushells_store / sample(c(2, 4), 1)
  } else {
    rats_eaten <- 0
  }
  
  state$bushells_store <- state$bushells_store + harvest - rats_eaten
  state$acres_planted <- 0
  
  # Population changes
  starved <- floor(state$people - state$bushells_fed / 20)
  state$bushells_fed <- 0
  
  new_people <- floor(sample(1:5, 1) * 
                        (20 * state$acres_owned + state$bushells_store) / 
                        state$people / 100 + 1)

  state$people <- state$people + new_people - starved
  
  plague_people <- ifelse(runif(1) < 0.15, floor(state$people / 2), 0)
  
  state$people <- state$people - plague_people
  
  # return values of state and of the changes that happened
  changes <- list(yield = yield, 
                  rats_eaten = rats_eaten, 
                  new_people = new_people, 
                  starved = starved,
                  plague_people = plague_people)
  
  return(list(state= state, changes = changes))
}

# test
x <- ham_year(state)
x$changes
x$state
