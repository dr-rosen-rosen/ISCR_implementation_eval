library(janitor)

# barriers_df <- read_csv(here('output','iscrBarriersLongAllCohorts.csv'))

# raw_data_df <- readxl::read_excel(here('data','Cohort 3_3B_4_clean data set_6.28.21.xlsx'),
#                                   sheet = 'C3, C3B, C4 all data') %>%
#   select('Hospital ID','Hospital Name') 

# barriers_df <- barriers_df %>%
#   left_join(raw_data_df, by = 'Hospital Name')

# raw_data_df <-raw_data_df %>%
#   clean_names()

AHA_HIT_df <- read_csv(here('data', 'AHA_Health_IT_2020','AHA_Health_IT_2010_2020.csv'))

# raw_data_df <- raw_data_df %>%
#   mutate(inAHA = if_else(hospital_id %in% unique(AHA_HIT_df$ID), TRUE, FALSE)) %>%
#   filter(inAHA == TRUE)

raw_data_df <- ACS_handoff_df %>%
  mutate(inAHA = if_else(hospital_id_cleaned %in% unique(AHA_HIT_df$ID), TRUE, FALSE)) %>%
  filter(inAHA == TRUE)


AHA_HIT_df <- AHA_HIT_df %>%
  filter(YEAR >= 2017 & YEAR <= 2019) %>%
  filter(ID %in% unique(raw_data_df$hospital_id_cleaned)) 

# AHA_completion <- table(AHA_HIT_df$ID,AHA_HIT_df$YEAR) %>%
#   as.data.frame.matrix() %>%
#   mutate(sum = rowSums(across(where(is.numeric)))) 
# AHA_completion %>%
#   ggplot(aes(x=sum)) +
#   geom_bar() + ggthemes::theme_tufte()

##### Code variables

provider_var_keys <- readxl::read_excel(here('data','AHA_HIT_Survey_items_key.xlsx'), sheet = 'provider') %>% 
  as.list() %>% 
  lapply(., function(x) x[!is.na(x)]) 

AHA_HIT_df <- AHA_HIT_df %>%
  mutate(across(.cols = which(colnames(AHA_HIT_df) %in% unlist(provider_var_keys, use.names = FALSE)), 
                .fns = ~if_else(.x == 1, 1, 0))) %>%
  mutate(
    ECD_score = rowMeans(.[,which(colnames(AHA_HIT_df) %in% provider_var_keys$ECD)]),
    RV_score = rowMeans(.[,which(colnames(AHA_HIT_df) %in% provider_var_keys$RV)]),
    CPOE_score = rowMeans(.[,which(colnames(AHA_HIT_df) %in% provider_var_keys$CPOE)]),
    DS_score = rowMeans(.[,which(colnames(AHA_HIT_df) %in% provider_var_keys$DS)])
    )

patient_var_keys <- readxl::read_excel(here('data','AHA_HIT_Survey_items_key.xlsx'), sheet = 'patient') %>% 
  as.list() %>% 
  lapply(., function(x) x[!is.na(x)])%>%
  lapply(., function(x) x[!endsWith(x,'OP')])

AHA_HIT_df <- AHA_HIT_df %>%
  mutate(across(.cols = which(colnames(AHA_HIT_df) %in% unlist(patient_var_keys, use.names = FALSE)), 
                .fns = ~if_else(.x == 1, 1, 0))) 

AHA_HIT_df[which(AHA_HIT_df$YEAR == 2019),'PE_score'] <- rowMeans(AHA_HIT_df[which(AHA_HIT_df$YEAR == 2019),patient_var_keys$`2019`])
AHA_HIT_df[which(AHA_HIT_df$YEAR == 2018),'PE_score'] <- rowMeans(AHA_HIT_df[which(AHA_HIT_df$YEAR == 2018),patient_var_keys$`2018`])
AHA_HIT_df[which(AHA_HIT_df$YEAR == 2017),'PE_score'] <- rowMeans(AHA_HIT_df[which(AHA_HIT_df$YEAR == 2017),patient_var_keys$`2017`])

# AHA_HIT_df %>%
#   mutate(YEAR = as.factor(YEAR)) %>%
#   ggplot(aes(x = PE_score, fill = YEAR))+ geom_histogram(position = "dodge") %>% ggthemes::theme_tufte()

PE_score_by_year <- AHA_HIT_df %>%
  select(YEAR,ID,PE_score) %>%
  mutate(
    YEAR = recode(YEAR,
      `2017` = 'Y2017',
      `2018` = 'Y2018',
      `2019` = 'Y2019')
  ) %>%
  pivot_wider(names_from = YEAR,
              values_from = PE_score)

ACS_handoff_df <- left_join(ACS_handoff_df,PE_score_by_year, by = c('hospital_id_cleaned' = 'ID'))
ACS_handoff_df <- ACS_handoff_df %>%
  mutate(
    PE_score_cohort_matched = case_when(
      cohort == 2 ~ Y2017,
      cohort == 3 ~ Y2018,
      cohort == 4 ~ Y2019
    ),
    PE_score_avg = rowMeans(.[,c('Y2017','Y2018','Y2019')],na.rm = TRUE)
  ) %>%
  select(c(-Y2017,-Y2018,-Y2019))

# ACS_handoff_df %>%
#   select(hospital_id_cleaned) %>% write.csv(., file = here('AHA_keys.csv'))

####### OTHER AHA fields

aha_vars <- read_csv(here('data','ttep8sv330ppx9hm.csv'))

raw_data_df <- ACS_handoff_df %>%
  mutate(inAHA = if_else(hospital_id_cleaned %in% unique(aha_vars$ID), TRUE, FALSE)) %>%
  filter(inAHA == TRUE)

aha_vars <- aha_vars %>%
  filter(YEAR >= 2017 & YEAR <= 2019) %>%
  filter(ID %in% unique(raw_data_df$hospital_id_cleaned)) 

BSC_by_year <- aha_vars %>%
  select(YEAR,ID,BSC) %>%
  mutate(
    YEAR = recode(YEAR,
                  `2017` = 'Y2017',
                  `2018` = 'Y2018',
                  `2019` = 'Y2019')
  ) %>%
  pivot_wider(names_from = YEAR,
              values_from = BSC)

ACS_handoff_df <- left_join(ACS_handoff_df,BSC_by_year, by = c('hospital_id_cleaned' = 'ID'))
ACS_handoff_df <- ACS_handoff_df %>%
  mutate(
    BSC_cohort_matched = case_when(
      cohort == 2 ~ Y2017,
      cohort == 3 ~ Y2018,
      cohort == 4 ~ Y2019
    )) %>%
  select(c(-Y2017,-Y2018,-Y2019))

MAPP3_by_year <- aha_vars %>%
  select(YEAR,ID,MAPP3) %>%
  mutate(
    YEAR = recode(YEAR,
                  `2017` = 'Y2017',
                  `2018` = 'Y2018',
                  `2019` = 'Y2019')
  ) %>%
  pivot_wider(names_from = YEAR,
              values_from = MAPP3)

ACS_handoff_df <- left_join(ACS_handoff_df,MAPP3_by_year, by = c('hospital_id_cleaned' = 'ID'))
ACS_handoff_df <- ACS_handoff_df %>%
  mutate(
    MAPP3_cohort_matched = case_when(
      cohort == 2 ~ Y2017,
      cohort == 3 ~ Y2018,
      cohort == 4 ~ Y2019
    )) %>%
  select(c(-Y2017,-Y2018,-Y2019))
  
CBSATYPE_by_year <- aha_vars %>%
  select(YEAR,ID,CBSATYPE) %>%
  mutate(
    YEAR = recode(YEAR,
                  `2017` = 'Y2017',
                  `2018` = 'Y2018',
                  `2019` = 'Y2019')
  ) %>%
  pivot_wider(names_from = YEAR,
              values_from = CBSATYPE)

ACS_handoff_df <- left_join(ACS_handoff_df,CBSATYPE_by_year, by = c('hospital_id_cleaned' = 'ID'))
ACS_handoff_df <- ACS_handoff_df %>%
  mutate(
    CBSATYPE_cohort_matched = case_when(
      cohort == 2 ~ Y2017,
      cohort == 3 ~ Y2018,
      cohort == 4 ~ Y2019
    )) %>%
  select(c(-Y2017,-Y2018,-Y2019))

write.csv(ACS_handoff_df,here('data','ISCR_Hospital_level_data_handoff.csv'))
