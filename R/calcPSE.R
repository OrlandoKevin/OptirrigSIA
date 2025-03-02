#' @export
#'
calcPSE <- function(
  om, PSE = 1, cfg = loadConfig()
) {
  dates <- as.Date(
    c(
      "2021-06-15", "2021-06-22", "2021-06-29", "2021-07-06", "2021-07-13",
      "2021-07-20", "2021-07-27", "2021-08-03", "2021-08-10"
    )
  )
  # Niveau de restriction
  niveau_restriction <- c(
    setNames(rep("vigilance", 3), dates[1:3]),
    setNames(rep("alerte", 3), dates[4:6]),
    setNames(rep("crise", 3), dates[7:9])
  )

  om <- om %>%
    mutate(
      dates = as.Date(dates),
      ratio_R = (R1 + R2 + R3) / (R1max + R2max + R3max),
      niveau_R = ifelse(ratio_R >= 0.7, 1, ifelse(ratio_R >= 0.5, 2, 3)),
      niveau_restriction = ifelse(dates <= as.Date("2021-06-29"), "vigilance", ifelse(dates <= as.Date("2021-07-20"), "alerte", "crise")),
      PSE_factor = NA,
      F_PSE = 0
    )

  sapply(
    dates,
    function(date) {
      om$PSE_factor[om$dates == date] <<- calcPSEFactor(
        niveau_R = om$niveau_R[om$dates == date],
        niveau_restriction = om$niveau_restriction[om$dates == date]
      )
      om$F_PSE[om$dates == date] <<- om$PSE_factor[om$dates == date] * PSE
    }
  )

  om <- om %>%
    mutate(
      F_PSE = ifelse(!is.na(PSE_factor), PSE * PSE_factor, 0),
      F_PSE_cum = cumsum(F_PSE),
      F_total = F_SIA + F_PSE_cum,
      F_total_max = max(F_total, na.rm = TRUE)
    )

  return(om)
}
