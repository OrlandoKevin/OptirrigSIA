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
#' @export
#'
savePlots <- function(
  scenarios_final, i, outputsModel, fun_prod, version, force = FALSE,
  cfg = loadConfig()
) {
  # path <- getDataPath("productions", version, "plots", cfg = cfg)
  path <- "~/Jobs/SIA2025/Data/productions/v02/plots"
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  om <- outputsModel[[
    sprintf("run_%i", scenarios_final$scenarios_simulated[i])
  ]] %>%
    mutate_at("dates", as.Date)
  om <- merge(
    om,
    scenarios_final %>%
      filter(scenario == scenarios_final$scenario[i]) %>%
      select(c(dates, PSE)),
    all.x = TRUE
  ) %>%
    mutate_at("PSE", ~ ifelse(is.na(.), 0, .)) %>%
    # Calc cumulative PSE
    mutate(
      PSE_cum = cumsum(PSE),
      ratio_R = ratio_R * 100
    )

  # Vars
  vars <- c("P", "Irr", "ratioRU12", "LAI", "TDM", "F_total", "ratio_R")
  varnames <- c(
    "P" = "PCP", "ratioRU12" = "RU", "LAI" = "LAI", "TDM" = "TDM",
    "F_total" = "Fin", "ratio_R" = "PSE"
  )

  # Current
  current <- om[, c("dates", vars)] %>%
    filter(dates <= scenarios_final$dates[i]) %>%
    mutate(
      period = "current", TDM = .data$TDM * 0.4
    )

  # Proj
  proj <- getClimateHorizonProjections(
    scenarios_final, scenarios_final$scenario[i], scenarios_final$step[i],
    vars = vars, i = i, outputsModel = outputsModel
  ) %>%
    mutate(
      TDM = .data$TDM * 0.4,
      ratio_R = ratio_R * 100
    )

  # Ts
  data <- bind_rows(current, proj) %>%
    mutate(
      P = ifelse(
        .data$period %in% c("irrig_20", "irrig_30") | .data$P == 0, NA, .data$P
      ),
      Irr = ifelse(
        .data$dates >= scenarios_final$dates[i] | .data$Irr == 0, NA, .data$Irr
      )
    )


  # Plot settings
  colors <- c(
    "current" = "#00a3a6", "irrig_00" = "#00a3a6", "irrig_20" = "#423089",
    "irrig_30" = "#797870"
  )
  plot_type <- c(
    "P" = "bar", "Irr" = "point", "LAI" = "line", "ratioRU12" = "line",
    "TDM" = "line", "F_SIA" = "line", "PSE_cum" = "line", "F_total" = "line",
    "ratio_R" = "line"
  )
  plot_axis_title <- c(
    "P" = "Précipitations [mm]", "Irr" = "Irrigation [mm]",
    "LAI" = "Indice foliaire [m²/m²]", "ratioRU12" = "Réserve utile [%]",
    "TDM" = "Rendement prévisionnel [t/ha]", "F_total" = "Revenu [€/ha]",
    "ratio_R" = "Réserve totale [%]"
  )

  #
  # vars2skip <- c("P", "Irr", "ratioRU12")
  vars2skip <- "Irr"

  lapply(
    vars[!vars %in% vars2skip],
    function(var) {
      filename <- sprintf(
        "%s/%s_%s_%s.png", path, scenarios_final$scenario[i],
        scenarios_final$step[i], varnames[var]
      )
      if (file.exists(filename) & !force) {
        return(NULL)
      }
      plot_i <- plot(
        data, plot_model = "ggplot",
        aes = list(x = "dates", y = var, color = "period", fill = "period"),
        plot_type = plot_type[var], show.legend = FALSE,
        labels = list(x = NULL, y = plot_axis_title[var])
      ) +
        scale_color_manual(values = colors) +
        scale_fill_manual(values = colors) +
        # add a vertical line at the end of the current period
        geom_vline(
          xintercept = as.numeric(scenarios_final$dates[i]), linetype = "dashed"
        ) +
        # Set major breaks every 10 days from the first date
        scale_x_date(
          breaks = seq(max(data$dates) - 5, by = -10, length.out = 20),
          date_labels = "%d/%m"
        ) +
        # Rotate y axis labels
        theme(
          axis.text.y = element_text(angle = 90, hjust = 0.5, color = "#797870"),
          axis.title.x = element_text(color = "#797870"),
          axis.title.y = element_text(color = "#797870"),
          axis.text.x = element_text(color = "#797870")
        ) +
        # Set x lim
        coord_cartesian(xlim = c(min(data$dates), max(data$dates)))

      switch (
        var,
        "P" = {
          if (any(!is.na(data$Irr))) {
            plot_i <- plot_i +
              geom_point(
                data = data[, c("dates", "Irr", "period")],
                aes(x = dates, y = Irr),
                color = "#423089",
                size = 2,
                show.legend = FALSE
              )
          }
        },
        "LAI" = {
          # plot_i <- plot_i +
          #   # Add vline at om$dates[95]
          #   geom_vline(
          #     xintercept = as.numeric(om$dates[min(which(om$TT_p > 1200))]),
          #     linetype = "dashed", color = "red"
          #   )
        },
        "TDM" = {
          # plot_i <- plot_i +
          #   # Add vline at om$dates[95]
          #   geom_vline(
          #     xintercept = as.numeric(om$dates[min(which(om$TT_p > 2200))]),
          #     linetype = "dashed", color = "red"
          #   )
        },
        "F_total" = {
        },
        "ratio_R" = {
          plot_i <- plot_i +
            # coord_cartesian(ylim = c(0, 100)) +
            # Add vline at y = 70 and 50
            geom_hline(yintercept = 70, linetype = "dashed", color = "#423089") +
            geom_hline(yintercept = 50, linetype = "dashed", color = "#423089") +
            coord_cartesian(ylim = c(0, 100))
        },
        "ratioRU12" = {
          plot_i <- plot_i +
            coord_cartesian(ylim = c(0, 100))
        }
      )

      ggsave(
        plot = plot_i,
        filename = filename,
        height = 1080, width = 1920, dpi = 300, units = "px"
      )
    }
  )

  # Final plots
  if (scenarios_final$step[i] == "t9") {
    scenarios_final2 <- scenarios_final[i, ] %>%
      mutate(
        res0 = max(om$Res0),
        P_total = max(om$P_total),
        IrrCum = max(om$IrrCum),
        Rendement = max(om$Rendement),
        Revenu = max(om$F_total_max),
        WaterSupply = res0 + IrrCum + P_total
      )
    ## Rendement
    # plot(
    #   fun_prod, plot_model = "ggplot",
    #   aes = list(x = "WaterSupply", y = "Rendement"), plot_type = "point",
    #   size = 0.5, color = "#00a3a6"
    # ) +
    #   coord_cartesian(xlim = c(580, NA)) +
    #   # Plot point corresponding to the current scenario
    #   geom_point(
    #     data = scenarios_final2, aes(x = WaterSupply, y = Rendement),
    #     color = "#423089", size = 3
    #   )

    units <- c("Rendement" = "[t/ha]", "Revenu" = "[€/ha]")

    sapply(
      c("Rendement", "Revenu"),
      function(x) {
        fun_prod_plot <- plot(
          fun_prod, plot_model = "ggplot",
          aes = list(x = "IrrCum", y = x), plot_type = "point",
          size = 0.5, color = "#00a3a6",
          labels = list(
            x = "Cumul d'irrigation [mm]",
            y = paste(x, units[x])
          )
        ) +
          # coord_cartesian(xlim = c(580, NA)) +
          # Plot point corresponding to the current scenario
          geom_point(
            data = scenarios_final2, aes(x = .data$IrrCum, y = .data[[x]]),
            color = "#423089", size = 3
          ) +
          # Rotate y axis labels
          theme(
            axis.text.y = element_text(angle = 90, hjust = 0.5, color = "#797870"),
            axis.title.x = element_text(color = "#797870"),
            axis.title.y = element_text(color = "#797870"),
            axis.text.x = element_text(color = "#797870")
          ) +
          # Ajouter vline rouge sur x = 150
          geom_vline(xintercept = 150, linetype = "dashed", color = "red")

        if (x == "Rendement") {
          fun_prod_plot <- fun_prod_plot +
            # Ajouter hline sur y = 10
            geom_hline(yintercept = 10, linetype = "dashed", color = "#423089")
        } else {
          fun_prod_plot <- fun_prod_plot +
            # Ajouter hline sur y = 1000
            geom_hline(yintercept = 1600, linetype = "dashed", color = "#423089")
        }

        x_name <- ifelse(x == "Rendement", "FP_Rendement", "FP_Revenu")

        ggsave(
          plot = fun_prod_plot,
          filename = sprintf(
            "%s/%s_%s.png", path, scenarios_final$scenario[i], x_name
          ),
          height = 1080, width = 1920, dpi = 300, units = "px"
        )
      }
    )
  }

  return(NULL)

  # # Terme financier
  # fin_current <- om[, c("dates", "F_SIA")] %>%
  #   filter(dates <= scenarios_final$dates[i]) %>%
  #   mutate(period = "current")
  #
  # fin_proj <- getClimateHorizonProjections(
  #   scenarios_final, scenarios_final$scenario[i], scenarios_final$step[i],
  #   vars = "F_SIA", i = i, outputsModel = outputsModel
  # )
  #
  # FIN_plot <- plot(
  #   bind_rows(fin_current, fin_proj),
  #   plot_model = "ggplot",
  #   aes = list(x = "dates", y = "F_SIA", color = "period"),
  #   plot_type = "line",
  #   show.legend = FALSE
  # ) +
  #   scale_color_manual(
  #     values = c(
  #       "current" = "#00a3a6", "irrig_00" = "#00a3a6", "irrig_20" = "#423089",
  #       "irrig_30" = "#797870"
  #     )
  #   ) +
  #   # add a vertical line at the end of the current period
  #   geom_vline(
  #     xintercept = as.numeric(scenarios_final$dates[i]), linetype = "dashed"
  #   ) +
  #   # Set major breaks every 5 days
  #   scale_x_date(date_breaks = "10 days", date_labels = "%d/%m")
  # # Save
  # ggsave(
  #   filename = sprintf(
  #     "%s/%s_%s_FIN.png", path, scenarios_final$scenario[i],
  #     scenarios_final$step[i]
  #   ),
  #   plot = FIN_plot,
  #   height = 1080, width = 1920, dpi = 300, units = "px"
  # )
  #
  # # PSE
  # pse_current <- om[, c("dates", "PSE", "PSE_cum")] %>%
  #   filter(dates <= scenarios_final$dates[i]) %>%
  #   mutate(period = "current")
  #
  # pse_proj <- getClimateHorizonProjections(
  #   scenarios_final, scenarios_final$scenario[i], scenarios_final$step[i],
  #   vars = "PSE_cum", i = i, outputsModel = outputsModel
  # )
  # PSE_plot <- plot(
  #   bind_rows(pse_current, pse_proj),
  #   plot_model = "ggplot",
  #   aes = list(x = "dates", y = "PSE_cum", color = "period"),
  #   plot_type = "line",
  #   show.legend = FALSE
  # ) +
  #   scale_color_manual(
  #     values = c(
  #       "current" = "#00a3a6", "irrig_00" = "#00a3a6", "irrig_20" = "#423089",
  #       "irrig_30" = "#797870"
  #     )
  #   ) +
  #   # add a vertical line at the end of the current period
  #   geom_vline(
  #     xintercept = as.numeric(scenarios_final$dates[i]), linetype = "dashed"
  #   ) +
  #   # Set major breaks every 5 days
  #   scale_x_date(date_breaks = "10 days", date_labels = "%d/%m") +
  #   coord_cartesian(ylim = c(0, max(pse_proj$PSE_cum, pse_current$PSE_cum, 0.1, na.rm = TRUE)))
  # # Save
  # ggsave(
  #   filename = sprintf(
  #     "%s/%s_%s_PSE.png", path, scenarios_final$scenario[i],
  #     scenarios_final$step[i]
  #   ),
  #   plot = PSE_plot,
  #   height = 1080, width = 1920, dpi = 300, units = "px"
  # )

  # F + PSE
  # if (scenarios_final$step[i] == "t9") browser()
  # fin_pse_current <- merge(fin_current, pse_current) %>%
  #   mutate(F_total = F_SIA + PSE_cum)
  #
  # fin_pse_proj <- merge(fin_proj, pse_proj, by = c("dates", "period")) %>%
  #   mutate(F_total = F_SIA + PSE_cum)
  #
  # FIN_PSE_plot <- plot(
  #   bind_rows(fin_current),
  #   plot_model = "ggplot",
  #   aes = list(x = "dates", y = "F_SIA"),
  #   plot_type = "line",
  #   show.legend = FALSE
  # ) +
  #   geom_line(
  #     data = fin_pse_current,
  #     aes(x = dates, y = F_total, color = "#00a3a6")
  #   ) +
  #   # Rotate y axis labels
  #   theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
  #   scale_color_manual(
  #     values = c(
  #       "current" = "#00a3a6", "irrig_00" = "#00a3a6", "irrig_20" = "#423089",
  #       "irrig_30" = "#797870"
  #     )
  #   ) +
  #   # add a vertical line at the end of the current period
  #   geom_vline(
  #     xintercept = as.numeric(scenarios_final$dates[i]), linetype = "dashed"
  #   ) +
  #   # Set major breaks every 5 days
  #   scale_x_date(date_breaks = "10 days", date_labels = "%d/%m") +
  #   geom_line(
  #     aes(x = dates, y = PSE_cum, color = "period"),
  #     data = bind_rows(pse_current, pse_proj),
  #     linetype = "dashed"
  #   ) +
  #   coord_cartesian(ylim = c(0, max(fin_pse_proj$F_SIA, fin_pse_current$F_SIA, 0.1, na.rm = TRUE)))
}
