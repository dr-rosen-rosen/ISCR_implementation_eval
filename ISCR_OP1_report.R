library('ggplot2')
library(scales)
df_barriers <- read.csv('Barrier_score_by_Time.csv',header = TRUE)

#scaleFUN <- function(x) sprintf("%.2f", x)

ggplot(data = df_barriers, mapping = aes(x = administration, y = barrier_score)) +
  geom_line() +
  facet_wrap(facets = vars(item)) +
  scale_x_discrete(name ="Survey administration", limits=c("1","2","3")) +
  scale_y_continuous(name= "% of responding facilities experiencing the barrier", labels =  label_percent(accuracy=1))#scales::percent)
  #theme_light()


df_HQI <- read.csv('HQI_overTime.csv',header = TRUE, stringsAsFactors = FALSE)

#df_HQI$indicator <- ordered(df_HQI$indicator, levels = c('leadership_score','team_score','process_score','outcome_score'))
df_HQI$indicator[df_HQI$indicator == 'leadership_score'] <- 'Leadership involvement'
df_HQI$indicator[df_HQI$indicator == 'team_score'] <- 'Team meetings'
df_HQI$indicator[df_HQI$indicator == 'process_score'] <- 'Share process data'
df_HQI$indicator[df_HQI$indicator == 'outcome_score'] <- 'Share outcome data'
df_HQI <- df_HQI[df_HQI$indicator != 'HQI_Tot',]

df_HQI$indicator <- ordered(df_HQI$indicator, levels = c('Leadership involvement','Team meetings','Share process data','Share outcome data'))


#indicator.labs <- c('Leadership involvement', 'Team meetings','Share process data','Share outcome data')

ggplot(data = df_HQI, mapping = aes(x = administration, y =X0)) +
  #geom_line() +
  geom_bar(stat = 'identity') +
  facet_wrap(facets = vars(indicator)) +
  scale_x_discrete(name ="Survey administration", limits=c("4 months","8 months","12 months")) +
  scale_y_continuous(name= "% of responding facilities meeting criteria", labels =  label_percent(accuracy=1), limits = c(0,.7))  + #scales::percent)
  #theme(text=element_text(family="Garamond", size=14))
  theme_light()

df_ORCA <- read.csv('ORCA_long.csv',header = TRUE)

mean(df_ORCA$Overall_ORCA)
sd(df_ORCA$Overall_ORCA)
sum(df_ORCA$Overall_ORCA < 3)
quantile(df_ORCA$Overall_ORCA)

ORCA_vars <- c('Overall_ORCA','ORCA_8a','ORCA_8b','ORCA_8c','ORCA_8d','ORCA_8e','ORCA_8f')
boxplot(df_ORCA[ORCA_vars])

library(tidyr)
ORCA_long <- gather(df_ORCA[ORCA_vars],key = 'ORCA_scale', value = 'Score')

ORCA_long$ORCA_scale[ORCA_long$ORCA_scale == 'ORCA_8a'] <- 'Leadership culture'
ORCA_long$ORCA_scale[ORCA_long$ORCA_scale == 'ORCA_8b'] <- 'Staff culture'
ORCA_long$ORCA_scale[ORCA_long$ORCA_scale == 'ORCA_8c'] <- 'Leadership practice'
ORCA_long$ORCA_scale[ORCA_long$ORCA_scale == 'ORCA_8d'] <- 'Evaluation / accountability'
ORCA_long$ORCA_scale[ORCA_long$ORCA_scale == 'ORCA_8e'] <- 'Opinion leader culture'
ORCA_long$ORCA_scale[ORCA_long$ORCA_scale == 'ORCA_8f'] <- 'Slack resources'
ORCA_long$ORCA_scale[ORCA_long$ORCA_scale == 'Overall_ORCA'] <- 'Overall ORCA'

ORCA_long$`ORCA scale`<- as.factor(ORCA_long$ORCA_scale)
ggplot(ORCA_long) + geom_boxplot(aes(x=ORCA_scale, y=Score)) + theme_light() +
  scale_x_discrete(name ="ORCA Domain") +
  scale_y_continuous(name= "Mean ORCA Score") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#ORCA_long2 <- gather(df_ORCA[c('Cohort','Overall_ORCA')], key = 'Cohort', value = 'Overall_ORCA')

df_ORCA$Cohort[df_ORCA$Cohort == 'C2'] <- 'Colorectal'
df_ORCA$Cohort[df_ORCA$Cohort == 'HF2'] <- 'Hip Fracture'
df_ORCA$Cohort[df_ORCA$Cohort == 'TJ2'] <- 'Total Joint'

ggplot(df_ORCA) + geom_boxplot(aes(x=Cohort,y=Overall_ORCA)) + theme_light() +
  scale_x_discrete(name ="Service Line") +
  scale_y_continuous(name= "Mean Overall ORCA Score") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_discrete(labels=c("C2" = "Colorectal", "HF2" = "Hip Fracture",
                            "TJ2" = "Total Joint"))
  