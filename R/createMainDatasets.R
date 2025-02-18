#' @title Create main datasets
#'
#' @param version [character] Version of the dataset
#' @template write_results
#' @template cfg
#'
#' @returns [data.frame] Main datasets
#'
#' @export
#'
createMainDatasets <- function(
  version = "v02",
  do_PSE_dim = FALSE,
  force = FALSE,
  write_results = cfg$data$write_results,
  cfg = loadConfig()
) {
  # OutputsModel
  outputsModel <- loadOutputsModel()

  # Scenarios
  scenarios_final <- try(
    readr::read_csv(
      getDataPath("productions", version, "Scenarios.csv", cfg = cfg)
    ),
    silent = TRUE
  )

  if (inherits(scenarios_final, "try-error") | force) {
    # Create base datasets
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

    # Add run name corresponding to each scenario
    scenarios_simulated <- TalanoaHydro::readDelim(
      getDataPath("doseCombinations.csv", cfg = cfg
      ))%>%
      as.data.frame()
    scenarios_final <- scenarios_simulated %>%
      rename(scenarios_simulated = Scenario) %>%
      relocate(scenarios_simulated) %>%
      setNames(c("scenarios_simulated", paste0("t", 1:9))) %>%
      merge(scenarios) %>%
      tidyr::pivot_longer(
        !c(scenarios_simulated, scenario), names_to = "step",
        values_to = "dose_irr"
      ) %>%
      select(!.data$dose_irr)

    # Dates des jours de décisions
    dates_step <- as.Date(
      head(names(scenarios_simulated), -1), format = "%d/%m/%Y"
    ) %>%
      setNames(sprintf("t%i", seq.int(length(.))))

    # Niveau de restriction
    niveau_restriction <- c(
      setNames(rep("vigilance", 3), sprintf("t%i", 1:3)),
      setNames(rep("alerte", 3), sprintf("t%i", 4:6)),
      setNames(rep("crise", 3), sprintf("t%i", 7:9))
    )

    #
    scenarios_final <- scenarios_final %>%
      mutate(
        dates = dates_step[step],
        niveau_restriction = niveau_restriction[step],
        stade_pheno = NA,
        RU = NA,
        ratio_RU_RUmax = NA,
        ratio_RU12_RU12max = NA,
        niveau_RU = NA,
        precip_prev = NA,
        RU_prev_00 = NA,
        RU_prev_20 = NA,
        RU_prev_30 = NA,
        Fin = NA,
        Fin_max = NA,
        PSE_factor = NA,
        PSE = NA
      )

    for (i in seq.int(nrow(scenarios_final))) {
      # Progress
      cat(sprintf("Progress: %i/%i\n", i, nrow(scenarios_final)))

      #
      om <- outputsModel$SIA1[[
        sprintf("run_%i", scenarios_final$scenarios_simulated[i])
      ]] %>%
        mutate_at("dates", as.Date)

      # RU
      scenarios_final$RU[i] <- om$RU1_RU2[
        which(om$dates == scenarios_final$dates[i] - 1)
      ] # RU du jour précédent
      ## Previsions
      ru_proj <- getClimateHorizonProjections(
        scenarios_final, scenarios_final$scenario[i], scenarios_final$step[i],
        vars = "RU1_RU2", i = i, outputsModel = outputsModel
      )
      test <- lapply(
        setNames(nm = sort(unique(ru_proj$period))),
        function(x) {ru_proj$RU1_RU2[which.max(ru_proj$dates)]}
      )
      for (j in seq_along(test)) {
        scenarios_final[
          i, paste0("RU_prev_", gsub("irrig_", "", names(test)[j]), "")
        ] <- test[[j]]
      }

      # Ratio RU
      scenarios_final$ratio_RU_RUmax[i] <- round(
        om$ratioRU123pmax[
          which(om$dates == scenarios_final$dates[i]) # TODO fix temporality in optirrigCORE
        ] / 100,
        2
      )
      scenarios_final$ratio_RU12_RU12max[i] <- round(
        om$ratioRU12[
          which(om$dates == scenarios_final$dates[i]) # TODO fix temporality in optirrigCORE
        ] / 100,
        2
      )

      #
      scenarios_final$niveau_RU[i] <- ifelse(
        scenarios_final$ratio_RU12_RU12max[i] > 0.75, 1,
        ifelse(scenarios_final$ratio_RU12_RU12max[i] > 0.5, 2, 3)
      )

      # Pluie prévisionnel
      scenarios_final$precip_prev[i] <- sum(
        om$P[
          between(om$dates, scenarios_final$dates[i], scenarios_final$dates[i + 1])
        ]
      )

      # Rendement financier
      om <- calcFinYield(om = om, cfg = cfg)
      scenarios_final$Fin[i] <- om$F_SIA[
        which(om$dates == scenarios_final$dates[i] - 1)
      ]
      scenarios_final$Fin_max[i] <- max(om$F_SIA)

      # PSE
      ## Coefficient multiplicateur
      scenarios_final$PSE_factor[i] <- calcPSEFactor(
        niveau_RU = scenarios_final$niveau_RU[i],
        niveau_restriction = scenarios_final$niveau_restriction[i]
      )
    }

    test <- scenarios_final %>%
      group_by(scenario) %>%
      summarise(
        PSE_factor = sum(PSE_factor, na.rm = TRUE),
        Fin_max = first(Fin_max),
        .groups = "drop"
      ) %>%
      # Get max Fin_max and PSE
      filter(PSE_factor == max(PSE_factor), Fin_max == max(Fin_max))
    PSE <- 0.25 * test$Fin_max / test$PSE_factor # [€/ha]
    ##
    scenarios_final$PSE <- scenarios_final$PSE_factor * PSE

    if (write_results) {
      # Main datasets
      message("Saving main datasets")
      dir.create(
        getDataPath("productions", version, cfg = cfg), recursive = TRUE,
        showWarnings = FALSE
      )
      readr::write_csv(
        scenarios_final,
        getDataPath("productions", version, "Scenarios.csv", cfg = cfg)
      )
    }
  }
  browser()

  scenarios_final <- scenarios_final %>%
    rowwise() %>%
    mutate(
      niveau_RU = ifelse(
        ratio_RU12_RU12max > 0.7, 1,
        ifelse(ratio_RU12_RU12max > 0.4, 2, 3)
      ),
      PSE_factor = calcPSEFactor(
        niveau_RU = .data$niveau_RU,
        niveau_restriction = .data$niveau_restriction
      )
    ) %>%
    ungroup()

  test <- scenarios_final %>%
    group_by(scenario) %>%
    summarise(
      PSE_factor = sum(PSE_factor, na.rm = TRUE),
      Fin_max = first(Fin_max),
      .groups = "drop"
    )

  for (i in seq.int(nrow(scenarios_final))) {
    scenarios_final$PSE_factor[i] <- calcPSEFactor(
      niveau_RU = scenarios_final$niveau_RU[i],
      niveau_restriction = scenarios_final$niveau_restriction[i]
    )
  }

  if (write_results) {
    # Plots
    message("Saving plots")

    sapply(
      1:20,
      function(i, scenarios_final, outputsModel, version, cfg) {
        savePlots(
          i = i, scenarios_final = scenarios_final,
          outputsModel = outputsModel, version = version, cfg = cfg
        )
      },
      scenarios_final = scenarios_final,
      outputsModel = outputsModel,
      version = version,
      cfg = cfg
    )

    # future::plan(future::multicore)
    # library(progressr)
    # handlers(global = TRUE)
    # with_progress(
    #   {
    #     p <- progressor(along = seq.int(nrow(scenarios_final)))
    #     future.apply::future_sapply(
    #       seq.int(nrow(scenarios_final)),
    #       function(i) {
    #         p()
    #         savePlots(
    #           i = i, scenarios_final = scenarios_final, outputsModel = outputsModel,
    #           version = version, cfg = cfg
    #         )
    #       }
    #     )
    #   }
    # )
  }

  return(scenarios_final)
}
