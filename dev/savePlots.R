library(OptirrigSIA)
library(dplyr)
cfg = loadConfig()
version = "v02"

# OutputsModel
outputsModel <- loadOutputsModel()$SIA1
outputsModel <- lapply(
  outputsModel,
  function(x) {
    x <- calcFinYield(om = x, cfg = cfg) %>%
      calcPSE(., PSE = 40, cfg = cfg) %>%
      mutate_at("dates", as.Date) %>%
      mutate(
        P_cum = cumsum(P),
        P_total = sum(P),
        Res0 = R1[1] + R2[1] + R3[1]
      )
    return(x)
  }
)

# Scenarios
scenarios_final <- readr::read_csv(
  getDataPath("productions", version, "Scenarios.csv", cfg = cfg)
)

fun_prod <- lapply(
  outputsModel,
  function(om) {
    list(
      res0 = max(om$Res0),
      P_total = max(om$P_total),
      IrrCum = max(om$IrrCum),
      Rendement = max(om$Rendement),
      Revenu = max(om$F_total_max)
    )
  }
) %>%
  bind_rows() %>%
  mutate(WaterSupply = res0 + IrrCum + P_total)

#
# sapply(
#   1:10,
#   function(i) {
#     savePlots(
#       i = i, scenarios_final = scenarios_final, outputsModel = outputsModel,
#       fun_prod = fun_prod, version = version, cfg = cfg
#     )
#   }
# )

future::plan(future::multisession, workers = 20)
future.apply::future_sapply(
  seq.int(nrow(scenarios_final)),
  function(i) {
    savePlots(
      i = i, scenarios_final = scenarios_final, outputsModel = outputsModel,
      fun_prod = fun_prod, version = version, cfg = cfg
    )
  },
  future.envir = environment()
)
#
# plot(fun_prod, plot_model = "ggplot", aes = list(x = "WaterSupply", y = "Rendement"), plot_type = "point", alpha = 0.1, size = 0.5) +
#   coord_cartesian(xlim = c(0, NA))
