####################################################################################################
####################################################################################################
################################ Main analysis file
################################ for ISCR Implementation Evals
####################################################################################################
####################################################################################################

library(config)
library(tidyverse)
library(here)

debuggingState(on = FALSE)
Sys.setenv(R_CONFIG_ACTIVE = "default")
config <- config::get()
#source(here("1_funcs.R"), echo = TRUE)

x <- get_barriers_plot(
  barrier_file = file.path(here(),config$output_path,config$barrier_score_by_time)
)

y <- get_HQI_plot(
  HQI_file = file.path(here(),config$output_path,config$HQI_overTime)
)

z <- get_ORCA_plot(
  ORCA_file = file.path(here(),config$output_path,config$ORCA)
)