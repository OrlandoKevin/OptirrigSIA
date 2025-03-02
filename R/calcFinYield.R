#' Calc financial yield
#'
#' @param om outputsModel
#' @param f1 Coût fixe (€/ha)
#' @param f2 Coût de l'eau et de l'énergie par m³ d'eau appliquée (€/m³)
#' @param f3 Court du maïs (€/t)
#' @template cfg
#'
#' @return outputsModel
#'
#' @export
#'
calcFinYield <- function(
  om, f1 = 100, f2 = 0.15, f3 = 170, cfg = loadConfig()
) {
  om <- om %>%
    mutate(
      # Rendement = .data$HI * .data$TDM * (100 + 30) / 100,
      Rendement  = .data$yHuGs,
      F_SIA = f3 * (.data$Rendement) - f2 * 10 * .data$IrrCum - f1,
      Rendement_final = max(.data$Rendement)
    )
  return(om)
}
