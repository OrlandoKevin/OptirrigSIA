#' Get the climate horizon projections
#'
#' @description
#' This function returns the climate horizon projections for a given scenario,
#' time step, and variables.
#'
#' @param datasets [data.frame] The datasets containing the scenario and dates
#' information.
#' @param scenario [character] The scenario for which the climate horizon
#' projections are requested.
#' @param step [character] The time step for which the climate horizon
#' projections are requested.
#' @param vars [character] The variables for which the climate horizon
#' projections are requested.
#' @param outputsModel [list] See \code{\link{optirrigCORE::getOutputsModel}}.
#'
#' @import dplyr
#' @importFrom rlang .data
#'
#' @export
#'
getClimateHorizonProjections <- function(
  datasets,
  scenario,
  step,
  vars,
  i,
  outputsModel
) {
  # Get the possible water supply decisions
  decisions <- getPossibleDecisions(step)
  # Get the climate horizon projections
  projections <- lapply(
    decisions,
    function(decision) {
      # Get projected scenario
      ## Split the scenario name
      scenario_proj <- strsplit(scenario, "_")[[1]]
      ## Replace the decision in the scenario name
      scenario_proj[as.numeric(gsub("t", "", step)) + 1] <- sprintf(
        "%02i", decision
      )
      ## Reconstruct the scenario name
      scenario_proj <- paste(scenario_proj, collapse = "_")
      # Get the climate horizon projections
      outputsModel[[
        sprintf(
          "run_%i", datasets$scenarios_simulated[datasets$scenario == scenario_proj & datasets$step == step]
        )
      ]] %>%
        merge(
          .,
          datasets %>%
            filter(scenario == datasets$scenario[i]) %>%
            select(c(dates, PSE)),
          all.x = TRUE
        ) %>%
        mutate_at("PSE", ~ ifelse(is.na(.), 0, .)) %>%
        # Calc cumulative PSE
        mutate(PSE_cum = cumsum(PSE)) %>%
        select(any_of(c("dates", vars))) %>%
        filter(
          between(.data$dates, datasets$dates[i], datasets$dates[i] + 5)
        ) %>%
        mutate(period = sprintf("irrig_%02i", decision))
    }
  ) %>%
    bind_rows()
  return(projections)
}
