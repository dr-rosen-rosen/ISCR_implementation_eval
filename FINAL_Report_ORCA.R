library(config)
library(tidyverse)
library(here)
library(patchwork)
library(ggpattern)

debuggingState(on = FALSE)
Sys.setenv(R_CONFIG_ACTIVE = "default")
config <- config::get()
source(here("1_funcs.R"), echo = TRUE)

############
#### ORCA
############  

df_ORCA <- read.csv(file.path(here('data','FINAL_surveys','ORCA','ORCA_Cohorts_3_4_long_FINAL.csv')),header = TRUE) %>%
  janitor::clean_names()
df_ORCA$original_service_line_key <- df_ORCA$cohort
colnames(df_ORCA)[colnames(df_ORCA) == 'cohort'] <- 'service_line'
# This only uses Cohort 3 data
# ORCA_long2 <- gather(df_ORCA[c('service_line','overall_orca')], key = 'service_line', value = 'overall_orca')

df_ORCA$cohort <- ifelse(grepl('3',df_ORCA$service_line, fixed = TRUE), '3','4')
df_ORCA$service_line[df_ORCA$service_line == 'C3'] <- 'colorectal'
df_ORCA$service_line[df_ORCA$service_line == 'C3B'] <- 'colorectal'
df_ORCA$service_line[df_ORCA$service_line == 'C4'] <- 'colorectal'
df_ORCA$service_line[df_ORCA$service_line == 'HF3'] <- 'hip fracture'
df_ORCA$service_line[df_ORCA$service_line == 'HF3B'] <- 'hip fracture'
df_ORCA$service_line[df_ORCA$service_line == 'HF4'] <- 'hip fracture'
df_ORCA$service_line[df_ORCA$service_line == 'TJ3'] <- 'total joint'
df_ORCA$service_line[df_ORCA$service_line == 'TJ3B'] <- 'total joint'
df_ORCA$service_line[df_ORCA$service_line == 'TJ4'] <- 'total joint'
df_ORCA$service_line[df_ORCA$service_line == 'G3'] <- 'gynecology'
df_ORCA$service_line[df_ORCA$service_line == 'G3B'] <- 'gynecology'
df_ORCA$service_line[df_ORCA$service_line == 'G4'] <- 'gynecology'
df_ORCA$service_line[df_ORCA$service_line == 'EGS4'] <- 'EGS'

# df_ORCA$service_line <- ordered(df_ORCA$service_line, levels = c('colorectal', 'hip fracture', 'total joint', 'gynecology', 'EGS'))
ORCA_vars <- c('cohort',"hospital_id","hospital_name",'service_line','overall_orca','original_service_line_key','orca_6a','orca_6b','orca_6c','orca_6d','orca_6e','orca_6f')
ORCA_long <- df_ORCA[ORCA_vars] %>% gather(key = 'orca_scale', value = 'score', -cohort,-hospital_id,-hospital_name,-service_line,-original_service_line_key)
ORCA_long$orca_scale[ORCA_long$orca_scale == 'orca_6a'] <- 'Leadership culture'
ORCA_long$orca_scale[ORCA_long$orca_scale == 'orca_6b'] <- 'Staff culture'
ORCA_long$orca_scale[ORCA_long$orca_scale == 'orca_6c'] <- 'Leadership practice'
ORCA_long$orca_scale[ORCA_long$orca_scale == 'orca_6d'] <- 'Evaluation / accountability'
ORCA_long$orca_scale[ORCA_long$orca_scale == 'orca_6e'] <- 'Opinion leader culture'
ORCA_long$orca_scale[ORCA_long$orca_scale == 'orca_6f'] <- 'Slack resources'
ORCA_long$orca_scale[ORCA_long$orca_scale == 'overall_orca'] <- 'Overall ORCA'

#Read in C2 data
df_ORCA_C2 <- read.csv(file.path(here('data','FINAL_surveys','ORCA','ORCA_Cohort2_long_FINAL.csv')),header = TRUE) %>%
  janitor::clean_names()
df_ORCA_C2$original_service_line_key <- df_ORCA_C2$cohort
colnames(df_ORCA_C2)[colnames(df_ORCA_C2) == 'cohort'] <- 'service_line'
df_ORCA_C2$service_line[df_ORCA_C2$service_line == 'C2'] <- 'colorectal'
df_ORCA_C2$service_line[df_ORCA_C2$service_line == 'HF2'] <- 'hip fracture'
df_ORCA_C2$service_line[df_ORCA_C2$service_line == 'TJ2'] <- 'total joint'

# df_ORCA_C2$service_line <- ordered(df_ORCA_C2$service_line, levels = c('colorectal', 'hip fracture', 'total joint'))
ORCA_vars <- c("hospital_id","hospital_name",'service_line','overall_orca','original_service_line_key','orca_8a','orca_8b','orca_8c','orca_8d','orca_8e','orca_8f')
ORCA_long_C2 <- df_ORCA_C2[ORCA_vars] %>% gather(key = 'orca_scale', value = 'score', -hospital_id,-hospital_name,-service_line,-original_service_line_key)
ORCA_long_C2$cohort <- '2'
ORCA_long_C2$orca_scale[ORCA_long_C2$orca_scale == 'orca_8a'] <- 'Leadership culture'
ORCA_long_C2$orca_scale[ORCA_long_C2$orca_scale == 'orca_8b'] <- 'Staff culture'
ORCA_long_C2$orca_scale[ORCA_long_C2$orca_scale == 'orca_8c'] <- 'Leadership practice'
ORCA_long_C2$orca_scale[ORCA_long_C2$orca_scale == 'orca_8d'] <- 'Evaluation / accountability'
ORCA_long_C2$orca_scale[ORCA_long_C2$orca_scale == 'orca_8e'] <- 'Opinion leader culture'
ORCA_long_C2$orca_scale[ORCA_long_C2$orca_scale == 'orca_8f'] <- 'Slack resources'
ORCA_long_C2$orca_scale[ORCA_long_C2$orca_scale == 'overall_orca'] <- 'Overall ORCA'
#ORCA_long_C2$`ORCA scale`<- as.factor(ORCA_long_C2$ORCA_scale)

p.service_line <- ggplot(rbind(df_ORCA[c('service_line','overall_orca')],df_ORCA_C2[c('service_line','overall_orca')])) + geom_boxplot(aes(x=service_line,y=overall_orca)) + theme_light() +
  scale_x_discrete(name ="Service Line") +
  scale_y_continuous(name= "Mean Overall ORCA Score") +
  ggthemes::theme_tufte() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_brewer(palette = "Set1") + theme( legend.position = "bottom",  text = element_text(family = "sans")) +
  labs(title = 'Overall ORCA Scores by Service Line')


#Combine C2 and 3
ORCA_long <- dplyr::bind_rows(ORCA_long, ORCA_long_C2)
ORCA_long$service_line <- ordered(ORCA_long$service_line, levels = c('colorectal', 'hip fracture', 'total joint', 'gynecology', 'EGS'))
ORCA_long$cohort <- ordered(ORCA_long$cohort, levels = c('2','3','4'))
ORCA_long$orca_scale <- ordered(ORCA_long$orca_scale, levels = c('Overall ORCA', 'Leadership culture','Staff culture','Leadership practice','Evaluation / accountability','Opinion leader culture','Slack resources'))
p.domain <- ggplot(ORCA_long) + geom_boxplot(aes(x=orca_scale, y=score, fill = cohort)) + 
  scale_x_discrete(name ="ORCA Domain") +
  scale_y_continuous(name= "Mean ORCA Score") +
  ggthemes::theme_tufte() +
  theme(axis.text.x = element_text(angle = 90, hjust = .5), text = element_text(family = "sans")) +
  scale_fill_brewer(palette = "Set1") + theme( legend.position = "top" ) +
  labs(title = 'ORCA Scores by Domain')+ guides(fill=guide_legend(title="Cohort"))

p.domain.pattern <- ggplot(ORCA_long) + 
  geom_boxplot_pattern(
    aes(x=orca_scale, y=score, pattern = cohort, pattern_angle = cohort, pattern_spacing = cohort),
    pattern_color = "white",
    pattern_fill = "black",
    pattern_density = 0.35) + 
  scale_x_discrete(name ="ORCA Domain") +
  scale_y_continuous(name= "Mean ORCA Score") +
  ggthemes::theme_tufte() +
  theme(axis.text.x = element_text(angle = 90, hjust = .5), text = element_text(family = "sans")) +
  # scale_fill_brewer(palette = "Set1") + 
  # scale_color_grey() +
  scale_pattern_manual(values=c('stripe', 'crosshatch', 'weave')) +
  theme( legend.position = "top" ) +
  labs(title = 'ORCA Scores by Domain') #+ guides(fill=guide_legend(title="Cohort"))

p.combined <- p.domain + p.service_line + plot_annotation(tag_levels = 'A')

ORCA_long %>% write.csv('ORCA_long_complete.csv')

ACS_handoff_df <- ORCA_long %>%
  pivot_wider(
    names_from = orca_scale,
    values_from = score,
  ) %>%
  mutate(
    hospital_id_cleaned = hospital_id,
    hospital_id_cleaned = recode(hospital_id_cleaned,
                                 HRH0001 = '6221315',
                                 PH001 = '6743745',
                                 MUSCH001 = '6370011',
                                 AHUC001 = '6360305',
                                 `6310120B` = '6310120',
                                 ASVE01 = '6420027')
  )

df_ORCA %>%
  filter(cohort == '4') %>%
  summarize(mean = mean(Overall_ORCA),
            )

df_ORCA %>%
  filter(cohort == '4',
         Overall_ORCA < 3) %>%
  summarize(cnt = n(),
  )

############
#### Barriers
############  

barriers_df <- read.csv(here('data','FINAL_surveys','barriers','iscrBarriersLongAllCohorts_FINAL.csv'))

barriers_df <- barriers_df %>%
  mutate(
    cohort = stringr::str_remove(cohort, pattern = 'B'),
    barrier_score = as.logical(barrier_score)
  ) %>%
  group_by(cohort, Barrier_cat,administration) %>%
  summarise(
    barrier_score = sum(barrier_score) / n()
  )
barriers_df$cohort <- ordered(barriers_df$cohort, levels = c('2','3','4'))
barriers_df$administration[barriers_df$administration == 1] <- 'First check-in'
barriers_df$administration[barriers_df$administration == 2] <- 'Second check-in'
barriers_df$administration[barriers_df$administration == 3] <- 'Third check-in'
barriers_df$administration <- ordered(barriers_df$administration, levels =c('First check-in','Second check-in','Third check-in'))

barriers_df %>% ggplot(aes(y=barrier_score, x = administration, fill = cohort)) +
  geom_bar(stat = 'identity', position="dodge") +
  facet_wrap(facets = vars(Barrier_cat)) +
  ggthemes::theme_tufte() +
  labs(fill = 'Cohort') +
  scale_x_discrete(name ="Survey administration") +
  scale_y_continuous(
    name= "% of responding facilities experiencing barriers", 
    labels =  scales::label_percent(accuracy=1),
    limits=c(0,.4)) +
  scale_fill_brewer(palette = "Set1") + 
  theme(
    legend.position = "bottom", 
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
    text = element_text(size = 16, family = "sans"),
    axis.title = element_text(size = 20))
barriers_df %>%
  group_by(Barrier_cat) %>%
  summarize(avg = mean(barrier_score))

############
#### HQI
############  

df_HQI <- read.csv(here('output','HQI_allCohorts.csv')) 

df_HQI <- read.csv(here('output','HQI_allCohortsTruncatedScores_FINAL.csv')) %>%
  select(c(-Hospital.Name,-Implementation_quality,-serviceLine),-ends_with('_item')) %>%
  pivot_longer(
    cols = c(outcome_score, process_score, leadership_score, team_score),
    names_to = 'indicator',
    values_to = 'hqi_score'
  ) %>%
  mutate(
    cohort = stringr::str_remove(cohort, pattern = 'B'),
    hqi_score = as.integer(as.logical(hqi_score))
  ) %>%
  group_by(cohort,administration,indicator) %>%
  summarize(
    hqi_score = mean(hqi_score, na.rm = TRUE),
    n = n())
df_HQI$indicator[df_HQI$indicator == 'leadership_score'] <- 'Leadership involvement'
df_HQI$indicator[df_HQI$indicator == 'team_score'] <- 'Team meetings'
df_HQI$indicator[df_HQI$indicator == 'process_score'] <- 'Share process data'
df_HQI$indicator[df_HQI$indicator == 'outcome_score'] <- 'Share outcome data'
df_HQI <- df_HQI[df_HQI$indicator != 'HQI_Tot',]
df_HQI_C2$cohort <- ordered(df_HQI_C2$cohort, levels = c('2','3','4'))
df_HQI$indicator <- ordered(df_HQI$indicator, levels = c('Leadership involvement','Team meetings','Share process data','Share outcome data'))

df_HQI %>%
  group_by(indicator) %>%
  summarize(
    avg = mean(hqi_score)
  )
  
p.bar <- df_HQI %>%
  ggplot(aes(x = administration, y = hqi_score, fill = cohort)) +
  geom_bar(stat = 'identity', position="dodge") +
  facet_wrap(facets = vars(indicator)) +
  scale_x_discrete(name ="Survey Administration", limits=c("First check-in","Second check-in","Third check-in")) +
  scale_y_continuous(name= "% of facilities meeting criteria", labels =  scales::label_percent(accuracy=1)) +#, limits = c(0,.8))  + 
  ggthemes::theme_tufte() +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Frequency of High Quality Implementation Criteria",
    fill = 'Cohort') +  
  theme(
    legend.position = "top", 
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
    text = element_text(size = 16, family = "sans"),
    axis.title = element_text(size = 20))

p.upset <- read.csv(here('output','HQI_allCohortsTruncatedScores_FINAL.csv')) %>%
  select(Hospital.Name, administration, cohort, serviceLine, ends_with("_score")) %>%
  pivot_longer(
    cols = ends_with('_score'),
    names_to = 'HQI_indicator',
    values_to = 'score'
  ) %>%
  mutate(
    HQI_indicator = recode(HQI_indicator,
                           leadership_score = 'Leadership involvement',
                           team_score = "Team meetings",
                           process_score = 'Share process data',
                           outcome_score = 'Share outcome data'
                           )
  ) %>%
  as_tibble() %>%
  mutate(score = as.logical(score)) %>%
  filter(score == TRUE) %>%
  #mutate(HQI_indicator = stringr::str_to_title(HQI_indicator)) %>%
  group_by(Hospital.Name, cohort, serviceLine, administration) %>%
  summarize(HQI_indicators = list(unique(HQI_indicator))) %>%
  ggplot(aes(x = HQI_indicators)) +
  geom_bar() +
  ggthemes::theme_tufte() +
  ggupset::scale_x_upset() +
  labs(
    title = "Frequency of Patterns of High Quality Implementation", 
    y = 'Number of Facilities',
    x = 'Patterns of High Quality Implementation',
    fill = 'Cohort') +
  theme(
      #legend.position = "bottom", 
      #axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
      text = element_text(size = 16, family = "sans"),
      axis.title = element_text(size = 20))

p.combined <- (p.bar / p.upset) +  plot_annotation(tag_levels = 'A')
show(p.combined)
  