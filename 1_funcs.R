####################################################################################################
####################################################################################################
################################ Scripts for reading in and processing
################################ implementation data for ISCR
####################################################################################################
####################################################################################################
library('ggplot2')
library(scales)
library(tidyr)
library(ggupset)
library(ggthemes)
library(patchwork)

get_barrier_plots <- function(barrier_item_file, barrier_cat_file_C3, barrier_cat_file_C2) {

  df_barriers <- read.csv(barrier_item_file, header = TRUE)

  p.item <- ggplot(data = df_barriers, mapping = aes(x = administration, y = barrier_score)) +
    geom_line() +
    facet_wrap(facets = vars(item)) +
    scale_x_discrete(name ="Survey administration", limits=c("1","2","3")) +
    scale_y_continuous(name= "% of responding facilities experiencing the barrier", labels =  label_percent(accuracy=1)) +
    theme_tufte()
  
  df_barriers_cat_C3 <- read.csv(barrier_cat_file_C3, header = TRUE)
  df_barriers_cat_C3$cohort <- '3'
  df_barriers_cat_C2 <- read.csv(barrier_cat_file_C2, header = TRUE)
  df_barriers_cat_C2$cohort <- '2'
  df_barriers_cat <- dplyr::bind_rows(df_barriers_cat_C3, df_barriers_cat_C2)
  df_barriers_cat$cohort <- ordered(df_barriers_cat$cohort, levels = c('2','3'))
  df_barriers_cat$administration[df_barriers_cat$administration == 1] <- '4 months'
  df_barriers_cat$administration[df_barriers_cat$administration == 2] <- '8 months'
  df_barriers_cat$administration[df_barriers_cat$administration == 3] <- '12 months'
  df_barriers_cat$administration <- ordered(df_barriers_cat$administration, levels =c('4 months','8 months','12 months'))
  
  p.cat <- ggplot(data = df_barriers_cat, aes(y=barrier_score, x = administration, fill = cohort)) +
    geom_bar(stat = 'identity', position="dodge") +
    facet_wrap(facets = vars(Barrier_cat)) +
    theme_tufte() +
    scale_x_discrete(name ="Survey administration") +#, limits=c("1","2","3")) +
    scale_y_continuous(name= "% of responding facilities experiencing barriers", labels =  label_percent(accuracy=1)) +
    scale_fill_brewer(palette = "Set1") + theme( legend.position = "top" )
  
  return(list(item = p.item, cat = p.cat))
}

get_HQI_plots <- function(HQI_C3_file, HQI_C2_file,HQI_upset_file) {
  df_HQI_3 <- read.csv(HQI_C3_file,header = TRUE, stringsAsFactors = FALSE)
  df_HQI_3$cohort <- '3'
  df_HQI_C2 <- read.csv(HQI_C2_file,header = TRUE, stringsAsFactors = FALSE)
  df_HQI_C2$cohort <- '2'
  df_HQI <- dplyr::bind_rows(df_HQI_3, df_HQI_C2)
  #df_HQI$indicator <- ordered(df_HQI$indicator, levels = c('leadership_score','team_score','process_score','outcome_score'))
  df_HQI$indicator[df_HQI$indicator == 'leadership_score'] <- 'Leadership involvement'
  df_HQI$indicator[df_HQI$indicator == 'team_score'] <- 'Team meetings'
  df_HQI$indicator[df_HQI$indicator == 'process_score'] <- 'Share process data'
  df_HQI$indicator[df_HQI$indicator == 'outcome_score'] <- 'Share outcome data'
  df_HQI <- df_HQI[df_HQI$indicator != 'HQI_Tot',]
  df_HQI_C2$cohort <- ordered(df_HQI_C2$cohort, levels = c('2','3'))
  df_HQI$indicator <- ordered(df_HQI$indicator, levels = c('Leadership involvement','Team meetings','Share process data','Share outcome data'))
  
  p.bar <- ggplot(df_HQI, aes(x = administration, y = X0, fill = cohort)) +
    geom_bar(stat = 'identity', position="dodge") +
    facet_wrap(facets = vars(indicator)) +
    scale_x_discrete(name ="Survey administration", limits=c("4 months","8 months","12 months")) +
    scale_y_continuous(name= "% of responding facilities meeting criteria", labels =  label_percent(accuracy=1), limits = c(0,.8))  + 
    theme_tufte() +
    scale_fill_brewer(palette = "Set1") + theme( legend.position = "top" ) +
    labs(title = "Percentage of Responding Hospitals Meeting Criteria for High Quality Implementation in Four CUSP Domains")
  
  # Upset plot
  HQI_upset_df <- read.csv(HQI_upset_file)
  
  p.upset <- HQI_upset_df %>%
    as_tibble() %>%
    mutate(score = as.logical(score)) %>%
    filter(score == TRUE) %>%
    mutate(HQI_indicator = stringr::str_to_title(HQI_indicator)) %>%
    group_by(Hospital.Name, cohort, service, administration) %>%
    summarize(HQI_indicators = list(unique(HQI_indicator))) %>%
    ggplot(aes(x = HQI_indicators)) +
    geom_bar() +
    theme_tufte() +
    scale_x_upset() +
    labs(title = "Frequency of Patterns of High Quality Implementation across all Survey Administrations", y = 'Number of Facilities')
  
  p.combined <- p.bar / p.upset +  plot_annotation(tag_levels = 'A', caption = 'Plot B includes responses from all survey administrations in Cohort 3.')
  return(list(bar = p.bar, upset = p.upset, combined = p.combined))
}

get_ORCA_plot <- function(ORCA_file_C3, ORCA_file_C2) {
  df_ORCA <- read.csv(ORCA_file_C3,header = TRUE)
  ORCA_vars <- c('Overall_ORCA','ORCA_6a','ORCA_6b','ORCA_6c','ORCA_6d','ORCA_6e','ORCA_6f')
  # This only uses Cohort 3 data
  ORCA_long2 <- gather(df_ORCA[c('Cohort','Overall_ORCA')], key = 'Cohort', value = 'Overall_ORCA')
  
  df_ORCA$Cohort[df_ORCA$Cohort == 'C3'] <- 'Colorectal'
  df_ORCA$Cohort[df_ORCA$Cohort == 'C3B'] <- 'Colorectal'
  df_ORCA$Cohort[df_ORCA$Cohort == 'HF3'] <- 'Hip Fracture'
  df_ORCA$Cohort[df_ORCA$Cohort == 'HF3B'] <- 'Hip Fracture'
  df_ORCA$Cohort[df_ORCA$Cohort == 'TJ3'] <- 'Total Joint'
  df_ORCA$Cohort[df_ORCA$Cohort == 'TJ3B'] <- 'Total Joint'
  df_ORCA$Cohort[df_ORCA$Cohort == 'G3'] <- 'Gynecology'
  df_ORCA$Cohort[df_ORCA$Cohort == 'G3B'] <- 'Gynecology'
  
  p.service_line <- ggplot(df_ORCA) + geom_boxplot(aes(x=Cohort,y=Overall_ORCA)) + theme_light() +
    scale_x_discrete(name ="Service Line") +
    scale_y_continuous(name= "Mean Overall ORCA Score") +
    theme_tufte() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_fill_brewer(palette = "Set1") + theme( legend.position = "bottom" ) +
    labs(title = 'Overall ORCA Scores by Service Line')

  ORCA_long <- gather(df_ORCA[ORCA_vars],key = 'ORCA_scale', value = 'Score')
  ORCA_long$ORCA_scale[ORCA_long$ORCA_scale == 'ORCA_6a'] <- 'Leadership culture'
  ORCA_long$ORCA_scale[ORCA_long$ORCA_scale == 'ORCA_6b'] <- 'Staff culture'
  ORCA_long$ORCA_scale[ORCA_long$ORCA_scale == 'ORCA_6c'] <- 'Leadership practice'
  ORCA_long$ORCA_scale[ORCA_long$ORCA_scale == 'ORCA_6d'] <- 'Evaluation / accountability'
  ORCA_long$ORCA_scale[ORCA_long$ORCA_scale == 'ORCA_6e'] <- 'Opinion leader culture'
  ORCA_long$ORCA_scale[ORCA_long$ORCA_scale == 'ORCA_6f'] <- 'Slack resources'
  ORCA_long$ORCA_scale[ORCA_long$ORCA_scale == 'Overall_ORCA'] <- 'Overall ORCA'
  
  #Read in C2 data
  df_ORCA_C2 <- read.csv(ORCA_file_C2,header = TRUE)
  ORCA_vars <- c('Overall_ORCA','ORCA_8a','ORCA_8b','ORCA_8c','ORCA_8d','ORCA_8e','ORCA_8f')
  ORCA_long_C2 <- gather(df_ORCA_C2[ORCA_vars],key = 'ORCA_scale', value = 'Score')
  ORCA_long_C2$ORCA_scale[ORCA_long_C2$ORCA_scale == 'ORCA_8a'] <- 'Leadership culture'
  ORCA_long_C2$ORCA_scale[ORCA_long_C2$ORCA_scale == 'ORCA_8b'] <- 'Staff culture'
  ORCA_long_C2$ORCA_scale[ORCA_long_C2$ORCA_scale == 'ORCA_8c'] <- 'Leadership practice'
  ORCA_long_C2$ORCA_scale[ORCA_long_C2$ORCA_scale == 'ORCA_8d'] <- 'Evaluation / accountability'
  ORCA_long_C2$ORCA_scale[ORCA_long_C2$ORCA_scale == 'ORCA_8e'] <- 'Opinion leader culture'
  ORCA_long_C2$ORCA_scale[ORCA_long_C2$ORCA_scale == 'ORCA_8f'] <- 'Slack resources'
  ORCA_long_C2$ORCA_scale[ORCA_long_C2$ORCA_scale == 'Overall_ORCA'] <- 'Overall ORCA'
  #ORCA_long_C2$`ORCA scale`<- as.factor(ORCA_long_C2$ORCA_scale)
  
  #Combine C2 and 3
  ORCA_long_C2$cohort <- '2'
  ORCA_long$cohort <- '3'
  ORCA_long <- dplyr::bind_rows(ORCA_long, ORCA_long_C2)
  ORCA_long$cohort <- ordered(ORCA_long$cohort, levels = c('2','3'))
  ORCA_long$ORCA_scale <- ordered(ORCA_long$ORCA_scale, levels = c('Overall ORCA', 'Leadership culture','Staff culture','Leadership practice','Evaluation / accountability','Opinion leader culture','Slack resources'))
  p.domain <- ggplot(ORCA_long) + geom_boxplot(aes(x=ORCA_scale, y=Score, fill = cohort)) + 
    scale_x_discrete(name ="ORCA Domain") +
    scale_y_continuous(name= "Mean ORCA Score") +
    theme_tufte() +
    theme(axis.text.x = element_text(angle = 90, hjust = .5)) +
    scale_fill_brewer(palette = "Set1") + theme( legend.position = "top" ) +
    labs(title = 'ORCA Scores by Domain')
  p.combined <- p.domain + p.service_line + plot_annotation(tag_levels = 'A', caption = 'Plot B includes data from Cohort 3 only.')
  return(list(domain = p.domain, service_line = p.service_line, combined = p.combined))
}