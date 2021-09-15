df_imp_qual <- readxl::read_xlsx('Implementatoion_Quality_2.xlsx')

df_imp_qual$Implementation_quality <- ordered(df_imp_qual$Implementation_quality, levels = c("Low","Margnal","Moderate","High"))

ggplot(data = df_imp_qual, mapping = aes(x = Implementation_quality, y = Percent)) +
  geom_bar(stat = 'identity') +
  facet_wrap(facets = vars(administration)) +
  #scale_x_discrete(name ="Survey administration", limits=c("4 months","8 months","12 months")) +
  scale_y_continuous(name= "% of responding facilities meeting criteria", labels =  label_percent(accuracy=1)) +# + scales::percent)
#theme(text=element_text(family="Garamond", size=14))
theme_light()