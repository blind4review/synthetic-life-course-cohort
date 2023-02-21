function(distanceFns,combFns,analysisFns){

  f <- function(instructions,
                measuredData,
                simAndAnalysisSpecs_short){
    #Cross-bootstrap iteration results table
    results_table <- data.frame("dag"=character(),
                                "measurement"=character(),
                                "combining"=character(),
                                "analysis"=character(),
                                "estimates"=numeric(),
                                "n_younger"=numeric(),
                                "n_older"=numeric(),
                                "avg_max_weight"=numeric(),
                                "lb_max_weight"=numeric(),
                                "ub_max_weight"=numeric(),
                                "avg_sd_weight"=numeric(),
                                "lb_sd_weight"=numeric(),
                                "ub_sd_weight"=numeric(),
                                "avg_n_younger_unique"=numeric(),
                                "lb_n_younger_unique"=numeric(),
                                "ub_n_younger_unique"=numeric(),
                                "CI_lbs"=numeric(),
                                "CI_ubs"=numeric(),
                                "mean_se"=numeric(),
                                "se_of_estimates"=numeric(),
                                "simFailures"=numeric(),
                                "runtime_mean"=numeric(),
                                "runtime_mean_successes"=numeric(),
                                "coverage"=numeric(),
                                "coverage_min"=numeric(),
                                "coverage_max"=numeric(),
                                stringsAsFactors = FALSE
                                )

    #NOTE:  Currently written to run for a single DAG and measurement speification
    #       Uses a list of combination specifications
    #-------------------------------------------------------------------------------------
    # Determine unique combination specifications
    #-------------------------------------------------------------------------------------
    combSpecs <- unique(simAndAnalysisSpecs_short$combining)

    #-------------------------------------------------------------------------------------
    # Calculate distances
    #-------------------------------------------------------------------------------------
    #Determine unique distance function specifications used across combination function specs
    distanceSpecNames <- c()
    for(combSpec in combSpecs){
      if(!(combSpec %in% names(instructions$combining))){
        stop(paste0("Combination specification '",combSpec,"' does not exist"))
      }
      distanceSpecName <- instructions$combining[[combSpec]]$options$distance
      distanceSpecNames <-  c(distanceSpecNames,distanceSpecName)
    }
    distanceSpecNames <- unique(distanceSpecNames)

    # Loop over all distance fn specs used across all comb fn specs, and calculate distance matrices
    distanceResults <- list()
    for(distanceSpecName in distanceSpecNames){
      startTime <- Sys.time()
      distanceResults[[distanceSpecName]] <- distanceFns[[distanceSpecName]](measuredData$cohorts)
      distanceResults[[distanceSpecName]]$runtime <- as.numeric(difftime(Sys.time(),startTime))
    }

    #-------------------------------------------------------------------------------------
    # Do combining and analysis
    #-------------------------------------------------------------------------------------
    # Loop over all combination (e.g. matching) specs, run all analyses methods for each
    for(combSpec in combSpecs){
      simAndAnalysisSpecs_forCombination <- simAndAnalysisSpecs_short %>% filter(combining==combSpec)

      #----------------------------
      # Do combining
      #----------------------------
      #Determine how to combine
      distanceSpecName <- instructions$combining[[combSpec]]$options$distance
      combResults <- combFns[[combSpec]](measuredData$cohorts,
                                          distanceResults[[distanceSpecName]])

      analysisList <- simAndAnalysisSpecs_forCombination %>%
        dplyr::select(analysis) %>%
        unique()

      #Turn the weights data frame into long format
      d_weights <- t(combResults$weightsMx) #After t(), rows are older perople, cols are younger people
      colnames(d_weights) <- paste0("younger",1:ncol(d_weights))
      d_weights <- data.frame(older=1:nrow(d_weights),d_weights)
      d_weights <- pivot_longer(d_weights, starts_with("younger")) %>%
        dplyr::rename(weight=value) %>%
        select(older,name,weight)
      d_weights$younger <- as.numeric(gsub("younger","",d_weights$name))
      d_weights <- d_weights[,c("older","younger","weight")]

      #Add indices to the older and younger cohorts
      allData_older <- measuredData$fullCohortData$older$d
      allData_younger <- measuredData$fullCohortData$younger$d
      colnames(allData_older) <- paste0(colnames(allData_older),"_older")
      colnames(allData_younger) <- paste0(colnames(allData_younger),"_younger")

      d_older <- data.frame(older=1:nrow(measuredData$cohorts$older),
                            measuredData$cohorts$older,
                            allData_older)
      d_younger <- measuredData$cohorts$younger %>%
        dplyr::mutate(younger=row_number()) %>%
        select(younger,V) %>% cbind(allData_younger)

      #--------------
      # Do analysis
      #--------------
      #Run analyses
      nBootstraps <- instructions$combining[[combSpec]]$options$nBootstraps
      nMatches <- instructions$combining[[combSpec]]$options$nMatches

      bootstrap_results <- list()
      for(bs_index in 1:nBootstraps){
        print(paste0("Bootstrap: ",bs_index," (",combSpec,")"))
        #Turn the match indices data frame into long format
        matchIndices <- data.frame(combResults$matches[,bs_index:(bs_index+nMatches-1)])
        colnames(matchIndices) <- paste0("younger",1:ncol(matchIndices))
        matchIndices <- data.frame(older=1:nrow(matchIndices),matchIndices)
        d_matches <- pivot_longer(matchIndices,starts_with("younger")) %>%
          dplyr::rename(younger=value)%>%
          select(-name)

        #Merge the older and younger data
        analysis_data <- d_matches %>%
          left_join(d_older) %>%
          left_join(d_younger) %>%
          left_join(d_weights)
        write.csv(analysis_data, "temp_data.csv")
        write.csv(d_older, "temp_older.csv")
        write.csv(d_younger, "temp_younger.csv")

        #Run the analyses
        for(analysisIndex in 1:nrow(analysisList)){
          analysisName <- analysisList[analysisIndex,"analysis"]
          if(bs_index==1){
            bootstrap_results[[analysisName]] <- list()
          }
          bootstrap_results[[analysisName]][[bs_index]] <- list()
          tryCatch({
            bootstrap_results[[analysisName]][[bs_index]] <- analysisFns[[analysisName]](analysis_data,
                                                                                         d_younger)
            bootstrap_results[[analysisName]][[bs_index]]$success <- TRUE
          },error=function(e){
            print("an error occurred")
            print(e)
            bootstrap_results[[analysisName]][[bs_index]]$success <- FALSE
          })
          endTime <- Sys.time()
          bootstrap_results[[analysisName]][[bs_index]]$runtime <- as.numeric(difftime(endTime,startTime))
        }
      }

      #Info for summaries
      matchInfo <- analysis_data %>%
        group_by(older) %>%
        dplyr::summarise(max_weight=max(weight),
                         sd_weight=sd(weight),
                         n_younger_unique=length(unique(younger)))

      #Summarise across bootstraps
      for(analysisIndex in 1:nrow(analysisList)){
        analysisName <- analysisList[analysisIndex,"analysis"]
        ests <- unlist(purrr::map_depth(bootstrap_results[[analysisName]],.depth=1,"est"))
        ses <- unlist(purrr::map_depth(bootstrap_results[[analysisName]],.depth=1,"se"))
        ci_lbs <- unlist(purrr::map_depth(bootstrap_results[[analysisName]],.depth=1,"ci_lb"))
        ci_ubs <- unlist(purrr::map_depth(bootstrap_results[[analysisName]],.depth=1,"ci_ub"))
        resid_sds <- unlist(purrr::map_depth(bootstrap_results[[analysisName]],.depth=1,"resid_sd"))
        successes <- unlist(purrr::map_depth(bootstrap_results[[analysisName]],.depth=1,"success"))
        runtimes <- unlist(purrr::map_depth(bootstrap_results[[analysisName]],.depth=1,"runtime"))
        coverages <- unlist(purrr::map_depth(bootstrap_results[[analysisName]],.depth=1,"coverage"))
        coverages_min <- unlist(purrr::map_depth(bootstrap_results[[analysisName]],.depth=1,"coverage_min"))
        coverages_max <- unlist(purrr::map_depth(bootstrap_results[[analysisName]],.depth=1,"coverage_max"))

        #Measurement error correction statistics
        # correction_factors <- unlist(purrr::map_depth(bootstrap_results[[analysisName]],.depth=1,"correction_factor"))
        # sig_sqs <- unlist(purrr::map_depth(bootstrap_results[[analysisName]],.depth=1,"sig_sq"))
        # omega_sqs  <- unlist(purrr::map_depth(bootstrap_results[[analysisName]],.depth=1,"omega_sq"))
        # uncorrected_ests <- unlist(purrr::map_depth(bootstrap_results[[analysisName]],.depth=1,"uncorrected_est"))
        # uncorrected_ci_lbs <- unlist(purrr::map_depth(bootstrap_results[[analysisName]],.depth=1,"uncorrected_ci_lb"))
        # uncorrected_ci_ubs <- unlist(purrr::map_depth(bootstrap_results[[analysisName]],.depth=1,"uncorrected_ci_ub"))


        results_index <- nrow(results_table)+1
        results_table[results_index,as.character(colnames(simAndAnalysisSpecs_forCombination))] <- as.character(simAndAnalysisSpecs_forCombination[analysisIndex,])
        results_table[results_index,"n_younger"] <- nrow(analysis_data)
        results_table[results_index,"n_older"] <- length(unique(analysis_data$older))
        results_table[results_index,"avg_max_weight"] <- mean(matchInfo$max_weight,na.rm=TRUE)
        results_table[results_index,"lb_max_weight"] <- quantile(matchInfo$max_weight,0.025,na.rm=TRUE)
        results_table[results_index,"ub_max_weight"] <- quantile(matchInfo$max_weight,0.975,na.rm=TRUE)
        results_table[results_index,"avg_sd_weight"] <- mean(matchInfo$sd_weight,na.rm=TRUE)
        results_table[results_index,"lb_sd_weight"] <- quantile(matchInfo$sd_weight,0.025,na.rm=TRUE)
        results_table[results_index,"ub_sd_weight"] <- quantile(matchInfo$sd_weight,0.975,na.rm=TRUE)
        results_table[results_index,"avg_n_younger_unique"] <- mean(matchInfo$n_younger_unique,na.rm=TRUE)
        results_table[results_index,"lb_n_younger_unique"] <- quantile(matchInfo$n_younger_unique,0.025,na.rm=TRUE)
        results_table[results_index,"ub_n_younger_unique"] <- quantile(matchInfo$n_younger_unique,0.975,na.rm=TRUE)
        results_table[results_index,"estimates"] <- mean(ests,na.rm=TRUE)
        results_table[results_index,"CI_lbs"] <- quantile(ests,0.025,na.rm=TRUE)
        results_table[results_index,"CI_ubs"] <- quantile(ests,0.975,na.rm=TRUE)
        results_table[results_index,"coverage"] <- mean(coverages)
        results_table[results_index,"coverage_min"] <- min(coverages_min)
        results_table[results_index,"coverage_max"] <- max(coverages_max)

        # results_table[results_index,"simFailures"] <- sum(!successes,na.rm=TRUE)
        # results_table[results_index,"runtime_mean"] <- mean(runtimes,na.rm=TRUE)
        # results_table[results_index,"runtime_mean_successes"] <- mean(data.frame(runtimes=runtimes,
        #                                                                          successes=successes) %>% filter(successes==TRUE) %>%
        #                                                                 .$runtimes,
        #                                                               na.rm=TRUE)
        results_table[results_index,"mean_se"] <- mean(ses,na.rm=TRUE)
        results_table[results_index,"se_of_estimates"] <- sd(ests,na.rm=TRUE)/sqrt(nBootstraps)
      }

    }
    return(list(distanceResults=distanceResults,combResults=combResults,results=results_table))
  }
  return(f)
}