library(lcmm)

############
#### HQI
############  

df_HQI <- read.csv(here('output','HQI_allCohorts.csv')) %>%
  select(-ends_with('item')) %>%
  filter(serviceLine != 'O') %>%
  group_by(Hospital.Name,serviceLine) %>% mutate(id = cur_group_id()) %>%
  ungroup()

mult_lin <- lcmm::multlcmm(outcome_score + process_score + leadership_score + team_score ~ administration,
                           random = ~ administration,
                           subject = 'id',
                           data = df_HQI)

