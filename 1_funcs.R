####################################################################################################
####################################################################################################
################################ Scripts for reading in and processing
################################ implementation data for ISCR
####################################################################################################
####################################################################################################
library('ggplot2')
library(scales)
library(tidyr)

get_barrier_plots <- function(barrier_item_file,barrier_cat_file) {

  df_barriers <- read.csv(barrier_item_file, header = TRUE)

  p.item <- ggplot(data = df_barriers, mapping = aes(x = administration, y = barrier_score)) +
    geom_line() +
    facet_wrap(facets = vars(item)) +
    scale_x_discrete(name ="Survey administration", limits=c("1","2","3")) +
    scale_y_continuous(name= "% of responding facilities experiencing the barrier", labels =  label_percent(accuracy=1))#scales::percent)
    #theme_light()
  
  df_barriers_cat <- read.csv(barrier_cat_file, header = TRUE)
  df_barriers_cat$administration[df_barriers_cat$administration == 1] <- '4 months'
  df_barriers_cat$administration[df_barriers_cat$administration == 2] <- '8 months'
  df_barriers_cat$administration[df_barriers_cat$administration == 3] <- '12 months'
  df_barriers_cat$administration <- ordered(df_barriers_cat$administration, levels =c('4 months','8 months','12 months'))
  
  p.cat <- ggplot(data = df_barriers_cat, aes(y=barrier_score, x = administration)) +
    geom_bar(stat = 'identity') +
    facet_wrap(facets = vars(Barrier_cat)) +
    theme_light() +
    scale_x_discrete(name ="Survey administration") +#, limits=c("1","2","3")) +
    scale_y_continuous(name= "% of responding facilities experiencing barriers", labels =  label_percent(accuracy=1))#scales::percent)
  
  return(list(item = p.item, cat = p.cat))
}

get_HQI_plot <- function(HQI_file) {
  df_HQI <- read.csv(HQI_file,header = TRUE, stringsAsFactors = FALSE)
  
  #df_HQI$indicator <- ordered(df_HQI$indicator, levels = c('leadership_score','team_score','process_score','outcome_score'))
  df_HQI$indicator[df_HQI$indicator == 'leadership_score'] <- 'Leadership involvement'
  df_HQI$indicator[df_HQI$indicator == 'team_score'] <- 'Team meetings'
  df_HQI$indicator[df_HQI$indicator == 'process_score'] <- 'Share process data'
  df_HQI$indicator[df_HQI$indicator == 'outcome_score'] <- 'Share outcome data'
  df_HQI <- df_HQI[df_HQI$indicator != 'HQI_Tot',]
  
  df_HQI$indicator <- ordered(df_HQI$indicator, levels = c('Leadership involvement','Team meetings','Share process data','Share outcome data'))
  
  
  #indicator.labs <- c('Leadership involvement', 'Team meetings','Share process data','Share outcome data')
  
  p <- ggplot(data = df_HQI, mapping = aes(x = administration, y =X0)) +
    #geom_line() +
    geom_bar(stat = 'identity') +
    facet_wrap(facets = vars(indicator)) +
    scale_x_discrete(name ="Survey administration", limits=c("4 months","8 months","12 months")) +
    scale_y_continuous(name= "% of responding facilities meeting criteria", labels =  label_percent(accuracy=1), limits = c(0,.7))  + #scales::percent)
    #theme(text=element_text(family="Garamond", size=14))
    theme_light()
  return(p)
}

get_ORCA_plot <- function(ORCA_file) {
  df_ORCA <- read.csv(ORCA_file,header = TRUE)
  
  # mean(df_ORCA$Overall_ORCA)
  # sd(df_ORCA$Overall_ORCA)
  # sum(df_ORCA$Overall_ORCA < 3)
  # quantile(df_ORCA$Overall_ORCA)
  
  ORCA_vars <- c('Overall_ORCA','ORCA_6a','ORCA_6b','ORCA_6c','ORCA_6d','ORCA_6e','ORCA_6f')
  # boxplot(df_ORCA[ORCA_vars])

  ORCA_long <- gather(df_ORCA[ORCA_vars],key = 'ORCA_scale', value = 'Score')
  
  ORCA_long$ORCA_scale[ORCA_long$ORCA_scale == 'ORCA_6a'] <- 'Leadership culture'
  ORCA_long$ORCA_scale[ORCA_long$ORCA_scale == 'ORCA_6b'] <- 'Staff culture'
  ORCA_long$ORCA_scale[ORCA_long$ORCA_scale == 'ORCA_6c'] <- 'Leadership practice'
  ORCA_long$ORCA_scale[ORCA_long$ORCA_scale == 'ORCA_6d'] <- 'Evaluation / accountability'
  ORCA_long$ORCA_scale[ORCA_long$ORCA_scale == 'ORCA_6e'] <- 'Opinion leader culture'
  ORCA_long$ORCA_scale[ORCA_long$ORCA_scale == 'ORCA_6f'] <- 'Slack resources'
  ORCA_long$ORCA_scale[ORCA_long$ORCA_scale == 'Overall_ORCA'] <- 'Overall ORCA'
  
  ORCA_long$`ORCA scale`<- as.factor(ORCA_long$ORCA_scale)
  p.domain <- ggplot(ORCA_long) + geom_boxplot(aes(x=ORCA_scale, y=Score)) + theme_light() +
    scale_x_discrete(name ="ORCA Domain") +
    scale_y_continuous(name= "Mean ORCA Score") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
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
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  return(list(domain = p.domain, service_line = p.service_line))
}