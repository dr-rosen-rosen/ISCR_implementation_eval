library('ggplot2')
library(scales)
df_barriers <- read.csv('Barrier_score_ITEM_by_Time.csv',header = TRUE)

#scaleFUN <- function(x) sprintf("%.2f", x)

ggplot(data = df_barriers, mapping = aes(x = administration, y = barrier_score)) +
  geom_line() +
  facet_wrap(facets = vars(item)) +
  scale_x_discrete(name ="Survey administration", limits=c("1","2","3")) +
  scale_y_continuous(name= "% of responding facilities experiencing the barrier", labels =  label_percent(accuracy=1))#scales::percent)
#theme_light()

df_barriers_cat <-read.csv('Barrier_score_by_CATEGORY_by_Time.csv',header = TRUE)

ggplot(data = df_barriers_cat, mapping = aes(x = administration, y = barrier_score)) +
  geom_line() +
  facet_wrap(facets = vars(Barrier_cat)) +
  scale_x_discrete(name ="Survey administration", limits=c("1","2","3")) +
  scale_y_continuous(name= "% of responding facilities experiencing barriers", labels =  label_percent(accuracy=1))#scales::percent)


df_barriers_cat$administration[df_barriers_cat$administration == 1] <- '4 months'
df_barriers_cat$administration[df_barriers_cat$administration == 2] <- '8 months'
df_barriers_cat$administration[df_barriers_cat$administration == 3] <- '12 months'
df_barriers_cat$administration <- ordered(df_barriers_cat$administration, levels =c('4 months','8 months','12 months'))

ggplot(data = df_barriers_cat, aes(y=barrier_score, x = administration)) +
  geom_bar(stat = 'identity') +
  facet_wrap(facets = vars(Barrier_cat)) +
  theme_light() +
  scale_x_discrete(name ="Survey administration") +#, limits=c("1","2","3")) +
  scale_y_continuous(name= "% of responding facilities experiencing barriers", labels =  label_percent(accuracy=1))#scales::percent)


