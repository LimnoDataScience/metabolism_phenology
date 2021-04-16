#### Manuscript Figure 5 ####
library(tidyverse)
library(scales)
library(patchwork)

# Read in files
seasonal.df = read_csv('Processed_Output/NEP_seasonal.csv')
cum.seasonal.df = read_csv('Processed_Output/NEP_seasonal_cumulative.csv')

seasonal.df$id <- factor(seasonal.df$id , levels= (c("Allequash","BigMuskellunge","Crystal","Sparkling", "Trout","Fish","Mendota","Monona")))

g1 = ggplot(seasonal.df[!is.na(seasonal.df$movavg),], 
            aes(as.Date(yday, origin = as.Date('2019-01-01')), (movavg*volume/area/1000), col = id)) +
  geom_ribbon(aes(ymin = movavg_min * volume/area/1000, ymax = movavg_max * volume/area/1000, col = id), size = 0.3, alpha = 0.1) +
  geom_line(aes(linetype=id, col = id), size = 1) +
  # ylab(expression("7-day mov. average of seasonal decomposed total flux rate [g DO"*~m^{-2}*""*~d^{-1}*"]")) +
  ylab(expression("Filtered seasonal total NEP [g DO"*~m^{-2}*""*~d^{-1}*"]")) +
  # xlab('Day of the year') +
  scale_x_date(breaks = '2 months', labels = date_format("%b")) +
  scale_color_brewer(palette="Dark2") +
  scale_linetype_manual(values = c(1,1,1,1,1,2,2,2))+
  theme_minimal()+
  geom_hline(yintercept = 0,linetype="dotted", 
             color = "black", size=1) +
  theme(axis.title.x = element_blank(),
        legend.text = element_text(size = 20), axis.text.x= element_text(size = 20), plot.title = element_text(size = 20),
        axis.text.y= element_text(size = 20), text = element_text(size = 15), legend.title = element_blank(), strip.text =element_text(size = 20),
        legend.position = 'bottom')+
  guides(); g1#linetype = FALSE

cum.seasonal.df$id <- factor(cum.seasonal.df$id , levels= (c("Allequash","BigMuskellunge","Crystal","Sparkling", "Trout","Fish","Mendota","Monona")))

g2=ggplot(cum.seasonal.df, aes(as.Date(yday, origin = as.Date('2019-01-01')), movavg*volume/area/1000, col = id)) +
  geom_ribbon(aes(ymin = movavg_min * volume/area/1000, ymax = movavg_max * volume/area/1000, col = id), size = 0.3, alpha = 0.1)+
  geom_line(aes(linetype=id, col = id), size = 1)+
  # ylab(expression("7-day mov. average of seasonal decomposed total flux rate [g DO"*~m^{-2}*""*~d^{-1}*"]")) +
  ylab(expression("Filtered cum. sum total NEP [g DO"*~m^{-2}*""*~d^{-1}*"]")) +
  # xlab('Day of the year') +
  scale_x_date(breaks = '2 months', labels = date_format("%b")) +
  scale_color_brewer(palette="Dark2") +
  scale_linetype_manual(values = c(1,1,1,1,1,2,2,2))+#scale_linetype_manual(values = c(1,2))+
  theme_minimal()+
  geom_hline(yintercept = 0,linetype="dotted", 
             color = "black", size=1) +
  theme(axis.title.x = element_blank(),
        legend.text = element_text(size = 20), axis.text.x= element_text(size = 20), plot.title = element_text(size = 20),
        axis.text.y= element_text(size = 20), text = element_text(size = 15), legend.title = element_blank(), strip.text =element_text(size = 20),
        legend.position = 'bottom')+
  guides();g2


g3 = g1 / g2 + plot_annotation(tag_levels = 'A') + plot_layout(guides = 'collect') &
  theme(legend.position='bottom'); g3

ggsave(file = 'Figures/Fig_5.png', g3, dpi = 300, width =300, height = 250, units='mm')
