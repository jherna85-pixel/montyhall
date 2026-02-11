#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two
#'   doors with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
}



#' @title
#'   Contestant selects a door.
#'
#' @description
#'   `select_door()` generates a number that represents the
#'   contestant's first door selection.
#'
#' @details
#'   Contestant chooses from three closed doors from TV show,
#'   "Let's Make a Deal." The doors have a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a numeric value indicating
#'   the position the contestant is choosing.
#'
#' @examples
#'   select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   Host opens a Goat door.
#'
#' @description
#'   `open_goat_door()` generates a number that represents the
#'   door with the goat the host opens.
#'
#' @details
#'   From the TV show, "Let's Make a Deal," the contestant has made
#'   their pick, and the host now has to open a door to reveal a goat.
#'   The contestant is then given the opportunity to stay with their
#'   original selection or switch to the other unopened door.
#'
#' @param game a character vector of length 3, 2 goats and 1 car
#'   (the first argument).
#'
#' @param a.pick A number, represents contestant's first pick
#'  (the second argument).
#'
#' @return The function returns an integer indicating the door
#'   position the host chooses to reveal one goat.
#'
#' @examples
#'   open_goat_door(game = c('car','goat','goat'), a.pick = 3)
#'
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats
   if( game[ a.pick ] == "car" )
   {
     goat.doors <- doors[ game != "car" ]
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   {
     opened.door <- doors[ game != "car" & doors != a.pick ]
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'   Contestant makes final door choice.
#'
#' @description
#'   `change_door()` generates a number that represents the
#'   door the contestant picks after host reveals the goat door.
#'
#' @details
#'   From the TV show, "Let's Make a Deal," the contestant has made
#'   their first pick and the host has to opened a door to reveal a
#'   goat. The contestant is now given the opportunity to stay with
#'   their original selection or switch to the other unopened door.
#'
#' @param stay Logical. TRUE/FALSE, representing the choice
#'   to stay=TRUE, or switch stay=FALSE (the first argument).
#'
#' @param opened.door A number, representing the goat door opened
#'   by host (the second argument).
#'
#' @param a.pick A number, representing the first door pick of the
#'   contestant (the third argument).
#'
#' @return The function returns a numeric value indicating the
#'   contestant's final pick, after staying or switching.
#'
#' @examples
#'   change_door(stay=TRUE, opened.door=2, a.pick = 3)
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3)

   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ]
   }

   return( final.pick )  # number between 1 and 3
}



#' @title
#'   Determine if contestant has won.
#'
#' @description
#'   `determine_winner()` generates a character string that represents
#'    if contestant has won game.
#'
#' @details
#'   Contest playing the famous TV show game, "Let's Make a Deal," is
#'   revealed whether they have won or lost the game!
#'
#' @param final.pick A number, representing the final door pick the
#'   contestant made (the first argument).
#'
#' @param game a character vector of length 3, 2 goats and 1 car
#'   (the second argument).
#'
#' @return The function returns a character string of "win" or "lose"
#'   to indicate if the contestant has chosen the door with the car.
#'
#' @examples
#'   determine_winner(final.pick=3, game=c('car','goat','goat'))
#'
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#'   Stay or Switch: Results of "Let's Make a Deal!"
#'
#' @description
#'   `play_game()` generates a data frame representing if
#'   contestant wins or loses based on strategy: Switch, Stay.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param ... no arguments are used by the function.
#'
#' @return A data frame with two rows for strategy, switch/stay, and
#'   a column for outcome, LOSE/WIN.
#'
#' @examples
#'   play_game()
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#'  Simulate "Lets Make A Deal" the number of times you'd like.
#'
#' @description
#'  `play_n_games()` simulates the game a number of n times, the
#'  default is 100 games.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. This simulation repeats the game n
#'   times. Default is 100.
#'
#' @param n a number, represnting the number of times to simulate
#'   game, default is 100 games.
#'
#' @return a data frame, with one row per strategy for each game
#'    simulated.
#'
#' @examples
#'   play_n_games (n=100)
#'
#' @export
play_n_games <- function( n=100 )
{

  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }

  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>%
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>%
  print()

  return( results.df )

}
