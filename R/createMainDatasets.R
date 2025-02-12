#' Create main datasets for ENEO
#'
#' @export
#'
createMainDatasets <- function(
  write_results = cfg$data$write_results,
  cfg = loadConfig()
) {
  ## Create base datasets
  scenarios <- expand.grid(
    t1 = c(0, 20, 30),
    t2 = c(0),  # pb matériel
    t3 = c(0, 20, 30),
    t4 = c(0, 20), # restriction
    t5 = c(0, 20, 30),
    t6 = c(0, 20, 30),
    t7 = c(0, 20),
    t8 = c(0, 20),
    t9 = c(0, 20),  # restriction,
    stringsAsFactors = FALSE
  ) %>%
    data.frame() %>%
    mutate_all(as.numeric) %>%
    mutate(
      scenario = sprintf(
        "SIA_%02i_%02i_%02i_%02i_%02i_%02i_%02i_%02i_%02i", t1, t2, t3, t4, t5,
        t6, t7, t8, t9
      ),
      .before = 1
    )

  ## Add run name corresponding to each scenario
  scenarios_simulated <- readr::read_csv(
    file.path(wd, "doseCombinations.csv")
  ) %>%
    as.data.frame()
  scenarios_final <- scenarios_simulated %>%
    rename(scenarios_simulated = Scenario) %>%
    relocate(scenarios_simulated) %>%
    setNames(c("scenarios_simulated", paste0("t", 1:9))) %>%
    merge(scenarios) %>%
    tidyr::pivot_longer(
      !c(scenarios_simulated, scenario), names_to = "tourdeau",
      values_to = "dose_irr"
    )

  ##
}
