function(instructions, dirs){
  #Load identifiability helper
  identify <- dget(file.path("identify.R"))(instructions$effect)

  f <- function(dags){
    id_results <- list()
    id_latex_results <- ""
    for(dagName in names(dags)){
      id_result <- identify(dags[[dagName]])
      id_results[[dagName]] <- id_result
      id_latex_results <- paste0(id_latex_results,dagName,"\n",id_result$effect$P,"\n\n")
      #plot(id_result$plot + ggtitle(dagName))
    }

    saveRDS(id_results,file=file.path(dirs$Results,"identifiability_results.rds"))
    write(id_latex_results,file=file.path(dirs$Results,"identifiability_latex.txt"))
  }
  return(f)
}