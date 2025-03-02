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
        # Main
        dates = dates_step[step],
        niveau_restriction = niveau_restriction[step],
        stade_pheno = NA,
        IrrCum = NA,
        # PCP
        P_cum = NA,
        P_total = NA,
        precip_prev = NA,
        # Réserve
        RU = NA,
        ratio_RU = NA,
        ratio_R = NA,
        niveau_R = NA,
        RU_prev_00 = NA,
        RU_prev_20 = NA,
        RU_prev_30 = NA,
        # LAI
        LAI = NA,
        LAI_prev_00 = NA,
        LAI_prev_20 = NA,
        LAI_prev_30 = NA,
        LAI_max = NA,
        # TDM
        TDM = NA,
        TDM_prev_00 = NA,
        TDM_prev_20 = NA,
        TDM_prev_30 = NA,
        TDM_max = NA,
        # Fin
        Fin = NA,
        Fin_max = NA,
        # PSE
        PSE_factor = NA,
        PSE = NA,
        PSE_cum = NA,
        # Fin + PSE
        Fin_total = NA,
        Fin_total_max = NA
      )

    for (i in seq.int(nrow(scenarios_final))) {
      # Progress
      cat(sprintf("Progress: %i/%i\n", i, nrow(scenarios_final)))

      #
      om <- outputsModel[[
        sprintf("run_%i", scenarios_final$scenarios_simulated[i])
      ]]

      # Main
      ## Cumul d'irrigation
      scenarios_final$IrrCum[i] <- om$IrrCum[
        which(om$dates == scenarios_final$dates[i])
      ]

      # Previsions
      proj <- getClimateHorizonProjections(
        scenarios_final, scenarios_final$scenario[i], scenarios_final$step[i],
        vars = c("RU1_RU2", "LAI", "Rendement"), i = i, outputsModel = outputsModel
      )

      # PCP
      # Pluie total
      scenarios_final$P_total[i] <- om$P_total[
        which(om$dates == scenarios_final$dates[i] - 1)
      ]
      # Pluie cumulée
      scenarios_final$P_cum[i] <- om$P_cum[
        which(om$dates == scenarios_final$dates[i] - 1)
      ]
      # Pluie prévisionnel
      scenarios_final$precip_prev[i] <- sum(
        om$P[
          between(om$dates, scenarios_final$dates[i], scenarios_final$dates[i + 1])
        ]
      )

      # RU
      scenarios_final$RU[i] <- om$RU1_RU2[
        which(om$dates == scenarios_final$dates[i] - 1)
      ] # RU du jour précédent
      ## Previsions
      ru_proj <- lapply(
        setNames(nm = sort(unique(proj$period))),
        function(x) {
          proj_i <- proj[proj$period == x, ]
          proj_i$RU1_RU2[which.max(proj_i$dates)]
        }
      )
      for (j in seq_along(ru_proj)) {
        scenarios_final[
          i, paste0("RU_prev_", gsub("irrig_", "", names(ru_proj)[j]), "")
        ] <- ru_proj[[j]]
      }

      # Ratio RU
      scenarios_final$ratio_RU[i] <- round(
        om$ratioRU12[
          which(om$dates == scenarios_final$dates[i]) # TODO fix temporality in optirrigCORE
        ] / 100,
        2
      )

      # Ratio R
      scenarios_final$ratio_R[i] <- round(
        om$ratio_R[
          which(om$dates == scenarios_final$dates[i])
        ],
        2
      )

      #
      scenarios_final$niveau_R[i] <- round(
        om$niveau_R[
          which(om$dates == scenarios_final$dates[i])
        ],
        2
      )

      # Stade phénologique
      # TODO plot in LAI and TDM plots

      # LAI
      ## Au jour j
      scenarios_final$LAI[i] <- om$LAI[
        which(om$dates == scenarios_final$dates[i])
      ]
      ## Previsions
      lai_proj <- lapply(
        setNames(nm = sort(unique(proj$period))),
        function(x) {
          lai_proj_i <- proj[proj$period == x, ]
          lai_proj_i$LAI[which.max(lai_proj_i$dates)]
        }
      )
      for (j in seq_along(lai_proj)) {
        scenarios_final[
          i, paste0("LAI_prev_", gsub("irrig_", "", names(lai_proj)[j]), "")
        ] <- lai_proj[[j]]
      }
      # Max
      scenarios_final$LAI_max[i] <- max(om$LAI)

      # TDM
      ## Au jour j
      scenarios_final$TDM[i] <- om$Rendement[
        which(om$dates == scenarios_final$dates[i])
      ]
      ## Previsions
      tdm_proj <- lapply(
        setNames(nm = sort(unique(proj$period))),
        function(x) {
          tdm_proj_i <- proj[proj$period == x, ]
          tdm_proj_i$Rendement[which.max(tdm_proj_i$dates)]
        }
      )
      for (j in seq_along(tdm_proj)) {
        scenarios_final[
          i, paste0("TDM_prev_", gsub("irrig_", "", names(tdm_proj)[j]), "")
        ] <- tdm_proj[[j]]
      }
      ## Final
      scenarios_final$TDM_max[i] <- max(om$Rendement_final)

      # Rendement financier
      scenarios_final$Fin[i] <- om$F_SIA[
        which(om$dates == scenarios_final$dates[i] - 1)
      ]
      scenarios_final$Fin_max[i] <- max(om$F_SIA)

      # PSE
      ## Coefficient multiplicateur
      scenarios_final$PSE_factor[i] <- om$PSE_factor[
        which(om$dates == scenarios_final$dates[i])
      ]
      ## PSE
      scenarios_final$PSE[i] <- om$F_PSE[
        which(om$dates == scenarios_final$dates[i])
      ]
      ## PSE_cum
      scenarios_final$PSE_cum[i] <- om$F_PSE_cum[
        which(om$dates == scenarios_final$dates[i])
      ]

      # Fin + PSE
      scenarios_final$Fin_total[i] <- om$F_total[
        which(om$dates == scenarios_final$dates[i])
      ]
      scenarios_final$Fin_total_max[i] <- om$F_total_max[
        which(om$dates == scenarios_final$dates[i])
      ]
    }

    if (do_PSE_dim) {
      test <- scenarios_final %>%
        group_by(scenario) %>%
        summarise(
          PSE_factor = sum(PSE_factor, na.rm = TRUE),
          Fin_max = first(Fin_max),
          .groups = "drop"
        ) %>%
        # Get max Fin_max and PSE
        filter(PSE_factor == max(PSE_factor)) %>%
        filter(Fin_max == max(Fin_max))
      PSE <- 0.25 * test$Fin_max / test$PSE_factor # [€/ha]
    }

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
  plotProductionFunctions(scenarios_final = scenarios_final, which_var = "Fin_total_max")



  # browser()

  # scenarios_final <- scenarios_final %>%
  #   rowwise() %>%
  #   mutate(
  #     niveau_R = ifelse(
  #       ratio_R > 0.7, 1,
  #       ifelse(ratio_R > 0.5, 2, 3)
  #     ),
  #     PSE_factor = calcPSEFactor(
  #       niveau_R = .data$niveau_R,
  #       niveau_restriction = .data$niveau_restriction
  #     ),
  #
  #   ) %>%
  #   ungroup()
  #
  # test <- scenarios_final %>%
  #   group_by(scenario) %>%
  #   summarise(
  #     PSE_factor = sum(PSE_factor, na.rm = TRUE),
  #     Fin_max = first(Fin_max),
  #     .groups = "drop"
  #   )
  # PSE <- test %>%
  #   # Get max Fin_max and PSE
  #   filter(PSE_factor == max(PSE_factor)) %>%
  #   filter(Fin_max == max(Fin_max))
  # PSE <- 0.25 * PSE$Fin_max / PSE$PSE_factor # [€/ha]
  # ##
  # scenarios_final$PSE <- scenarios_final$PSE_factor * PSE


  if (write_results) {
    # Plots
    message("Saving plots")

    # sapply(
    #   (1:20),
    #   function(i, scenarios_final, outputsModel, version, cfg) {
    #     savePlots(
    #       i = i, scenarios_final = scenarios_final,
    #       outputsModel = outputsModel, version = version, cfg = cfg
    #     )
    #   },
    #   scenarios_final = scenarios_final,
    #   outputsModel = outputsModel,
    #   version = version,
    #   cfg = cfg
    # )

    future::plan(future::multisession, workers = 20)
    library(progressr)
    handlers(global = TRUE)
    with_progress(
      {
        p <- progressor(along = seq.int(nrow(scenarios_final)))
        future.apply::future_sapply(
          seq.int(nrow(scenarios_final)),
          function(i) {
            p()
            savePlots(
              i = i, scenarios_final = scenarios_final, outputsModel = outputsModel,
              version = version, cfg = cfg
            )
          },
          future.envir = environment()
        )
      }
    )

    # # ...existing code...
    #
    # # Optimize the future.apply::future_lapply call
    # future::plan(future::multicore, workers = 20) # Adjust the number of workers as needed
    #
    # # Define a chunk size
    # chunk_size <- 100
    #
    # # Split the indices into chunks
    # indices <- split(seq.int(nrow(scenarios_final)), ceiling(seq_along(seq.int(nrow(scenarios_final))) / chunk_size))
    #
    # # Process each chunk in parallel
    # future.apply::future_lapply(
    #   indices,
    #   function(chunk) {
    #     lapply(chunk, function(i) {
    #       savePlots(
    #         i = i, scenarios_final = scenarios_final, outputsModel = outputsModel,
    #         version = version, cfg = cfg
    #       )
    #     })
    #   },
    #   future.envir = environment()
    # )

    # ...existing code...
  }

  return(scenarios_final)
}
