#' @export
#'
plotProductionFunctions <- function(
  scenarios_final,
  which_var,
  cfg = loadConfig()
) {
  scenarios_final2 <- scenarios_final %>%
    group_by(scenario) %>%
    summarise(
      Fin_max = max(Fin_max),
      Fin_total_max = max(Fin_total_max),
      TDM_max = max(TDM_max),
      R_init = 0,
      PCP_cum = 0,
      IrrCum = max(IrrCum),
    )

  plot(
    scenarios_final2,
    plot_model = "ggplot",
    aes = list(x = "IrrCum", y = which_var),
    plot_type = "point"
  )
}
