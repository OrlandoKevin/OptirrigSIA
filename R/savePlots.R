#' Save plots
#'
#' @description
#' Save plots for each scenario
#'
#'
#' @param scenarios_final [data.frame] Scenarios
#' @param i [integer] Line index of scenarios_final
#' @param outputsModel See return of \code{\link{loadOutputsModel}}
#' @inheritParams createMainDatasets
#'
#' @return NULL
#'
#' @import ggplot2
#' @importFrom dplyr bind_rows filter mutate
#' @import TalanoaHydro
#'
savePlots <- function(
  scenarios_final, i, outputsModel, version, cfg = loadConfig()
) {
  path <- getDataPath("productions", version, "plots", cfg = cfg)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  om <- outputsModel$SIA1[[
    sprintf("run_%i", scenarios_final$scenarios_simulated[i])
  ]] %>%
    mutate_at("dates", as.Date)
  om <- calcFinYield(om = om, cfg = cfg)
  om <- merge(
    om,
    scenarios_final %>%
      filter(scenario == scenarios_final$scenario[i]) %>%
      select(c(dates, PSE)),
    all.x = TRUE
  ) %>%
    mutate_at("PSE", ~ ifelse(is.na(.), 0, .)) %>%
    # Calc cumulative PSE
    mutate(PSE_cum = cumsum(PSE))
  # PCP
  pcp_current <- om[, c("dates", "P")] %>%
    filter(dates <= scenarios_final$dates[i]) %>%
    mutate(period = "current")
  pcp_proj <- om[, c("dates", "P")] %>%
    filter(
      between(
        as.Date(.data$dates), scenarios_final$dates[i] + 1,
        scenarios_final$dates[i] + 5
      )
    ) %>%
    mutate(period = "proj")
  pcp_plot <- plot(
    bind_rows(pcp_current, pcp_proj),
    plot_model = "ggplot",
    aes = list(x = "dates", y = "P", fill = "period"),
    plot_type = "bar",
    show.legend = FALSE
  ) +
    scale_fill_manual(values = c("current" = "#00a3a6", "proj" = "#00a3a6")) +
    # add a vertical line at the end of the current period
    geom_vline(
      xintercept = as.numeric(scenarios_final$dates[i]), linetype = "dashed"
    ) +
    # Set major breaks every 5 days
    scale_x_date(date_breaks = "10 days", date_labels = "%d/%m")
  # Save
  ggsave(
    filename = sprintf(
      "%s/%s_%s_PCP.png", path, scenarios_final$scenario[i],
      scenarios_final$step[i]
    ),
    plot = pcp_plot,
    height = 1080, width = 1920, dpi = 300, units = "px"
  )

  # LAI
  lai_current <- om[, c("dates", "LAI")] %>%
    filter(dates <= scenarios_final$dates[i]) %>%
    mutate(period = "current")
  lai_proj <- getClimateHorizonProjections(
    scenarios_final, scenarios_final$scenario[i], scenarios_final$step[i],
    vars = "LAI", i = i, outputsModel = outputsModel
  )
  LAI_plot <- plot(
    bind_rows(lai_current, lai_proj),
    plot_model = "ggplot",
    aes = list(x = "dates", y = "LAI", color = "period"),
    plot_type = "line",
    show.legend = FALSE
  ) +
    scale_color_manual(
      values = c(
        "current" = "#00a3a6", "irrig_00" = "#00a3a6", "irrig_20" = "#423089",
        "irrig_30" = "#797870"
      )
    ) +
    # add a vertical line at the end of the current period
    geom_vline(
      xintercept = as.numeric(scenarios_final$dates[i]), linetype = "dashed"
    ) +
    # coord_cartesian(ylim = c(0, 5)) +
    # Set major breaks every 5 days
    scale_x_date(date_breaks = "10 days", date_labels = "%d/%m")
  ggsave(
    filename = sprintf(
      "%s/%s_%s_LAI.png", path, scenarios_final$scenario[i],
      scenarios_final$step[i]
    ),
    plot = LAI_plot,
    height = 1080, width = 1920, dpi = 300, units = "px"
  )

  # RU1_RU2
  browser()
  ru_current <- om[, c("dates", "ratioRU123pmax")] %>%
    filter(dates <= scenarios_final$dates[i]) %>%
    mutate(period = "current")

  ru_proj <- getClimateHorizonProjections(
    scenarios_final, scenarios_final$scenario[i], scenarios_final$step[i],
    vars = "ratioRU123pmax", i = i, outputsModel = outputsModel
  )

  RU_plot <- plot(
    bind_rows(ru_current, ru_proj),
    plot_model = "ggplot",
    aes = list(x = "dates", y = "ratioRU123pmax", color = "period"),
    plot_type = "line",
    show.legend = FALSE
  ) +
    scale_color_manual(
      values = c(
        "current" = "#00a3a6", "irrig_00" = "#00a3a6", "irrig_20" = "#423089",
        "irrig_30" = "#797870"
      )
    ) +
    # add a vertical line at the end of the current period
    geom_vline(
      xintercept = as.numeric(scenarios_final$dates[i]), linetype = "dashed"
    ) +
    # Set major breaks every 5 days
    scale_x_date(date_breaks = "10 days", date_labels = "%d/%m")
  # Save
  ggsave(
    filename = sprintf(
      "%s/%s_%s_RU.png", path, scenarios_final$scenario[i],
      scenarios_final$step[i]
    ),
    plot = RU_plot,
    height = 1080, width = 1920, dpi = 300, units = "px"
  ) %>%
    suppressMessages() %>%
    suppressWarnings()

  # RU12
  browser()
  ru_current <- om[, c("dates", "ratioRU12")] %>%
    filter(dates <= scenarios_final$dates[i]) %>%
    mutate(period = "current")

  ru_proj <- getClimateHorizonProjections(
    scenarios_final, scenarios_final$scenario[i], scenarios_final$step[i],
    vars = "ratioRU12", i = i, outputsModel = outputsModel
  )

  RU12_plot <- plot(
    bind_rows(ru_current, ru_proj),
    plot_model = "ggplot",
    aes = list(x = "dates", y = "ratioRU12", color = "period"),
    plot_type = "line",
    show.legend = FALSE
  ) +
    scale_color_manual(
      values = c(
        "current" = "#00a3a6", "irrig_00" = "#00a3a6", "irrig_20" = "#423089",
        "irrig_30" = "#797870"
      )
    ) +
    # add a vertical line at the end of the current period
    geom_vline(
      xintercept = as.numeric(scenarios_final$dates[i]), linetype = "dashed"
    ) +
    # Set major breaks every 5 days
    scale_x_date(date_breaks = "10 days", date_labels = "%d/%m")
  # Save
  ggsave(
    filename = sprintf(
      "%s/%s_%s_RU.png", path, scenarios_final$scenario[i],
      scenarios_final$step[i]
    ),
    plot = RU_plot,
    height = 1080, width = 1920, dpi = 300, units = "px"
  ) %>%
    suppressMessages() %>%
    suppressWarnings()

  ## TDM
  tdm_current <- om[, c("dates", "TDM")] %>%
    filter(dates <= scenarios_final$dates[i]) %>%
    mutate(period = "current")

  tdm_proj <- getClimateHorizonProjections(
    scenarios_final, scenarios_final$scenario[i], scenarios_final$step[i],
    vars = "TDM", i = i, outputsModel = outputsModel
  )

  TDM_plot <- plot(
    bind_rows(tdm_current, tdm_proj),
    plot_model = "ggplot",
    aes = list(x = "dates", y = "TDM", color = "period"),
    plot_type = "line",
    show.legend = FALSE
  ) +
    scale_color_manual(
      values = c(
        "current" = "#00a3a6", "irrig_00" = "#00a3a6", "irrig_20" = "#423089",
        "irrig_30" = "#797870"
      )
    ) +
    # add a vertical line at the end of the current period
    geom_vline(
      xintercept = as.numeric(scenarios_final$dates[i]), linetype = "dashed"
    ) +
    # Set major breaks every 5 days
    scale_x_date(date_breaks = "10 days", date_labels = "%d/%m")
  # Save
  ggsave(
    filename = sprintf(
      "%s/%s_%s_TDM.png", path, scenarios_final$scenario[i],
      scenarios_final$step[i]
    ),
    plot = TDM_plot,
    height = 1080, width = 1920, dpi = 300, units = "px"
  )

  # Terme financier
  fin_current <- om[, c("dates", "F_SIA")] %>%
    filter(dates <= scenarios_final$dates[i]) %>%
    mutate(period = "current")

  fin_proj <- getClimateHorizonProjections(
    scenarios_final, scenarios_final$scenario[i], scenarios_final$step[i],
    vars = "F_SIA", i = i, outputsModel = outputsModel
  )

  FIN_plot <- plot(
    bind_rows(fin_current, fin_proj),
    plot_model = "ggplot",
    aes = list(x = "dates", y = "F_SIA", color = "period"),
    plot_type = "line",
    show.legend = FALSE
  ) +
    scale_color_manual(
      values = c(
        "current" = "#00a3a6", "irrig_00" = "#00a3a6", "irrig_20" = "#423089",
        "irrig_30" = "#797870"
      )
    ) +
    # add a vertical line at the end of the current period
    geom_vline(
      xintercept = as.numeric(scenarios_final$dates[i]), linetype = "dashed"
    ) +
    # Set major breaks every 5 days
    scale_x_date(date_breaks = "10 days", date_labels = "%d/%m")
  # Save
  ggsave(
    filename = sprintf(
      "%s/%s_%s_FIN.png", path, scenarios_final$scenario[i],
      scenarios_final$step[i]
    ),
    plot = FIN_plot,
    height = 1080, width = 1920, dpi = 300, units = "px"
  )

  # PSE
  pse_current <- om[, c("dates", "PSE", "PSE_cum")] %>%
    filter(dates <= scenarios_final$dates[i]) %>%
    mutate(period = "current")

  pse_proj <- getClimateHorizonProjections(
    scenarios_final, scenarios_final$scenario[i], scenarios_final$step[i],
    vars = "PSE_cum", i = i, outputsModel = outputsModel
  )
  PSE_plot <- plot(
    bind_rows(pse_current, pse_proj),
    plot_model = "ggplot",
    aes = list(x = "dates", y = "PSE_cum", color = "period"),
    plot_type = "line",
    show.legend = FALSE
  ) +
    scale_color_manual(
      values = c(
        "current" = "#00a3a6", "irrig_00" = "#00a3a6", "irrig_20" = "#423089",
        "irrig_30" = "#797870"
      )
    ) +
    # add a vertical line at the end of the current period
    geom_vline(
      xintercept = as.numeric(scenarios_final$dates[i]), linetype = "dashed"
    ) +
    # Set major breaks every 5 days
    scale_x_date(date_breaks = "10 days", date_labels = "%d/%m") +
    coord_cartesian(ylim = c(0, max(pse_proj$PSE_cum, pse_current$PSE_cum, 0.1, na.rm = TRUE)))
  # Save
  ggsave(
    filename = sprintf(
      "%s/%s_%s_PSE.png", path, scenarios_final$scenario[i],
      scenarios_final$step[i]
    ),
    plot = PSE_plot,
    height = 1080, width = 1920, dpi = 300, units = "px"
  )
}
