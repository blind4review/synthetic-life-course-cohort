rm(list=ls())
library("plyr")
library("dplyr")
library("ggplot2")
library("pacman")
library("purrr")
library("statip")
p_load(
  # tidyverse packages
  "plyr", "tidyverse", "broom", "glue","modelr",
  # data set
  "gapminder"
)

#==============================================
# Options
#==============================================
instructionsNames<- c(
  #"DEV_Instructions_dag1"#,
  #"DEV_Instructions_dag2"#,
  # "Instructions_dag2_ixn",
  # "Instructions_dag3_M",
  #"DEV_Instructions_dag3_MM2"#,
  #"DEV_Instructions_dag3a_MM2"#,
  #"DEV_Instructions_dag3b_MM2"#,
  #"DEV_Instructions_dag3a_M2only"#,
  #"DEV_Instructions_dag3b_M2only"#,
  # "Instructions_dag4_Mboth",
  "MI_only_dag1"#,
  #"DEV_Instructions_dag4_CMboth_CgtM"#,
  #"DEV_Instructions_dag4_CMboth_MgtC"#,
  # "Instructions_dag4_Mboth_Cyounger",
  # "Instructions_dag4_Mboth_Colder")
)

instructionsName <- instructionsNames[[1]]

options <- list(nIter=1000, # Use 1000 for final
                simulation=list())

#==============================================
# Directories
#==============================================
#Set up a list of directories
dirs <- list(
  instructions = file.path("Instructions"),
  helper = file.path("HelperFns"),
  DAGs = file.path("DAGs"),
  SimFns = file.path("SimFns"),
  MeasFns = file.path("MeasurementFns"),
  DistanceFns = file.path("DistanceFns"),
  CombFns = file.path("CombinationFns"),
  AnalysisFns = file.path("AnalysisFns"),
  AggregationFns = file.path("AggregationFns"),
  Results = file.path("Results", instructionsName)
)
dir.create(dirs$Results)

#==============================================
# Load instructions
#==============================================
instructions <- dget(file.path(dirs$instructions,paste0(instructionsName,".R")))
saveRDS(instructions,file.path(dirs$Results,"instructions.RDS"))

#==============================================
# Load helper functions
#==============================================
msg <- dget(file.path(dirs$helper,"helper_messager.R"))()
msg$setStatus(1) #Set to 0 to stop printing, 1+ for increasing printing detail

loadDAGStructures <- dget(file.path(dirs$helper,"helper_loadDAGstructures.R"))(dirs)
loadSimFns <- dget(file.path(dirs$helper,"helper_loadSimFns.R"))(dirs)
loadMeasFns <- dget(file.path(dirs$helper,"helper_loadMeasurementFns.R"))(dirs)
loadDistanceFns <- dget(file.path(dirs$helper,"helper_loadDistanceFns.R"))(dirs)
loadCombFns <- dget(file.path(dirs$helper,"helper_loadCombinationFns.R"))(dirs)
loadAnalysisFns <- dget(file.path(dirs$helper,"helper_loadAnalysisFns.R"))(dirs)
loadSimulatorsForDAGs <- dget(file.path(dirs$helper,"helper_loadSimulatorsForDAGs.R"))(options,msg)

h_makeSimAndAnalysisSpecsMx <- dget(file.path(dirs$helper,"helper_makeSimAndAnalysisSpecsMx.R"))()
h_calculateTruths <- dget("bigDataTruth.R")

#==============================================
# Load simulation and analysis functions
#==============================================
#Load DAGs
dags <- loadDAGStructures(instructions)

#Load functions
simFns <- loadSimFns(dags)
measFns <- loadMeasFns(instructions)
distanceFns <- loadDistanceFns(instructions)
combFns <- loadCombFns(instructions)
analysisFns <- loadAnalysisFns(instructions)
estsSummaryFn <- dget(file.path(dirs$AggregationFns,"estsSummary.R"))(dirs)

#Set up simulators for each DAG
simulators <- loadSimulatorsForDAGs(dags,simFns)
combineAndAnalyze <- dget("combineAndAnalyze.R")(distanceFns,combFns,analysisFns)

#==============================================
# Simulation-based analysis
#==============================================
#----------------------------------------------
# Simulation and analysis spec matrices
#----------------------------------------------
simAndAnalysisSpecs <- h_makeSimAndAnalysisSpecsMx(instructions)
simSpecs <- simAndAnalysisSpecs %>%
  dplyr::select(-c(analysis)) %>%
  unique()

#----------------------------------------------
# Data to record
#----------------------------------------------
#Calculate Truths
truths <- h_calculateTruths(instructions,
                            simulators)
truths

#Simulation Iteration Loop
iterResults <- list()
for(simIter in 1:options$nIter){
  print(paste0("Simulation Iter: ",simIter))
  #For each DAG
  #for(dagSpec in unique(simSpecs$dag)){
  #temp
  dagSpec <-  unique(simSpecs$dag)[[1]]
  #/temp
  simulator <- simulators[[dagSpec]] #fn for how data are generated
  #Select uniwue measurement specifications for this DAG
  measSpecs <- simSpecs %>% filter(dag == dagSpec) %>% .$measurement %>% unique()

  #For each way of measuring variables for that DAG
  #for(measSpec in measSpecs){
  #temp
  measSpec <-  measSpecs[[1]]
  #/temp
  #Create the cohorts
  measuredData <- measFns[[measSpec]](simulator)

  #Calculate performance of combination and analysis combinations
  simAndAnalysisSpecs_short <- simAndAnalysisSpecs %>%
    filter(dag==dagSpec,
           measurement==measSpec)
  results  <- combineAndAnalyze(instructions,
                                       measuredData,
                                       simAndAnalysisSpecs_short)

  #Assign analysis results to full results
  results$sim_rep_index <- simIter

  if(simIter==1){
    simResultsMx <- results$results
  }else{
    simResultsMx <- rbind(simResultsMx,results$results)
  }

  #Save out iteration results
  iterResults[[simIter]] <- list(
    weightsMx=results$combResults$weightsMx
  )
}

simResultsMx <- left_join(simResultsMx,truths)  %>%
  dplyr::relocate(c(TrueBigPop,
                    ConfoundingStrength),
                  .after=analysis)

#Calcuate Bias, MSE and other summary measures across simulation iterations
estSummary <- estsSummaryFn(simResultsMx)
View(t(estSummary))
View(simResultsMx)


saveRDS(simResultsMx,file.path(dirs$Results,"simResultsMx.rds"))
saveRDS(iterResults,file.path(dirs$Results,"iterResults.rds"))
write.csv(estSummary,
          file.path(dirs$Results,"estimate_performance.csv"))

