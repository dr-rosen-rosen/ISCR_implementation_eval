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
source(here("1_funcs.R"), echo = TRUE)

barrier_figs <- get_barrier_plots(
  barrier_item_file = file.path(here(),config$output_path,config$barrier_score_by_time),
  barrier_cat_file_C3 = file.path(here(),config$output_path,config$barrier_score_by_CATEGORY_by_time),
  barrier_cat_file_C2 = file.path(here(),config$output_path,config$OP_1_path,config$barrier_score_by_CATEGORY_by_time)
)
#show(barrier_figs$item)
show(barrier_figs$cat)

HQI_fig <- get_HQI_plots(
  HQI_C3_file = file.path(here(),config$output_path,config$HQI_overTime),
  HQI_C2_file = file.path(here(),config$output_path,config$OP_1_path,config$HQI_overTime),
  HQI_upset_file = file.path(here(),config$output_path,config$HQI_upset)
)
show(HQI_fig$combined)

ORCA_figs <- get_ORCA_plot(
  ORCA_file_C3 = file.path(here(),config$output_path,config$ORCA),
  ORCA_file_C2 = file.path(here(),config$output_path,config$OP_1_path,config$ORCA)
)
show(ORCA_figs$domain)
show(ORCA_figs$service_line)
show(ORCA_figs$combined)

