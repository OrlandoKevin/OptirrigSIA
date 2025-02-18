#' Get the possible water supply decisions
#' 
#' @description 
#' This function returns the possible water supply decisions for a given time
#' step.
#' 
#' @param step [character] The time step for which the possible decisions are
#' requested.
#' 
#' @return [numeric] The possible water supply decisions.
#' 
#' @export 
#' 
getPossibleDecisions <- function(
  step = "t1"
) {
  decisions <- list(
    t1 = c(0, 20, 30),
    t2 = c(0),  # pb matériel
    t3 = c(0, 20, 30),
    t4 = c(0, 20), # restriction
    t5 = c(0, 20, 30),
    t6 = c(0, 20, 30),
    t7 = c(0, 20),
    t8 = c(0, 20),
    t9 = c(0, 20)  # restriction
  )
  return(decisions[[step]])
}