#### Manuscript Figure 5 ####
library(tidyverse)
library(scales)
library(patchwork)

# Read in files
seasonal.df = read_csv('Processed_Output/NEP_seasonal.csv')
cum.seasonal.df = read_csv('Processed_Output/NEP_seasonal_cumulative.csv')
seasonal.df$id <- factor(seasonal.df$id , levels= (c("Allequash","BigMuskellunge","Crystal","Sparkling", "Trout","Fish","Mendota","Monona")))

# Read in files
seasonal.df.epi = read_csv('Processed_Output/NEP_seasonal-epi.csv')
cum.seasonal.df.epi = read_csv('Processed_Output/NEP_seasonal_cumulative-epi.csv')
seasonal.df.epi$id <- factor(seasonal.df$id , levels= (c("Allequash","BigMuskellunge","Crystal","Sparkling", "Trout","Fish","Mendota","Monona")))

seasonal.df.hyp = read_csv('Processed_Output/NEP_seasonal-hyp.csv')
cum.seasonal.df.hyp = read_csv('Processed_Output/NEP_seasonal_cumulative-hyp.csv')
seasonal.df.hyp$id <- factor(seasonal.df$id , levels= (c("Allequash","BigMuskellunge","Crystal","Sparkling", "Trout","Fish","Mendota","Monona")))

seasonal.df.sed = read_csv('Processed_Output/NEP_seasonal-sed.csv')
cum.seasonal.df.sed = read_csv('Processed_Output/NEP_seasonal_cumulative-sed.csv')
seasonal.df.sed$id <- factor(seasonal.df$id , levels= (c("Allequash","BigMuskellunge","Crystal","Sparkling", "Trout","Fish","Mendota","Monona")))

library(RColorBrewer)
palette_Dark2 <- colorRampPalette(brewer.pal(8, "Dark2"))

g1 = ggplot(seasonal.df[!is.na(seasonal.df$movavg),], 
            aes(as.Date(yday, origin = as.Date('2019-01-01')), (movavg) + NEP_avg2, col = id)) + #*volume/area/1000
  geom_ribbon(aes(ymin = movavg_min + NEP_avg2, ymax = movavg_max + NEP_avg2, col = id, fill = id), size = 0.3, alpha = 0.03) +
  geom_line(aes(linetype=id, col = id), size = 1.5) +
  # ylab(expression("7-day mov. average of seasonal decomposed total flux rate [g DO"*~m^{-2}*""*~d^{-1}*"]")) +
  ylab(expression(atop("Filtered seasonal total NEP flux +","long-term average [g DO"*~m^{-2}*""*~d^{-1}*"]"))) +
  # xlab('Day of the year') +
  scale_x_date(breaks = '2 months', labels = date_format("%b")) +
  scale_color_brewer(palette="Dark2") +
  scale_linetype_manual(values = c(1,1,1,1,1,4,4,4))+
  theme_minimal()+
  geom_hline(yintercept = 0,linetype="dotted", 
             color = "black", size=1) +
  theme(axis.title.x = element_blank(),
        legend.text = element_text(size = 20), axis.text.x= element_text(size = 20), plot.title = element_text(size = 20),
        axis.text.y= element_text(size = 20), text = element_text(size = 15), legend.title = element_blank(), strip.text =element_text(size = 20),
        legend.position = 'bottom')+
  guides(); g1#linetype = FALSE

g4 = ggplot(seasonal.df.epi[!is.na(seasonal.df$movavg),], 
            aes(as.Date(yday, origin = as.Date('2019-01-01')), (movavg) + NEP_avg2, col = id)) + #*volume/area/1000
  geom_ribbon(aes(ymin = movavg_min + NEP_avg2, ymax = movavg_max + NEP_avg2, col = id, fill = id), size = 0.3, alpha = 0.03) +
  geom_line(aes(linetype=id, col = id), size = 1.5) +
  # ylab(expression("7-day mov. average of seasonal decomposed total flux rate [g DO"*~m^{-2}*""*~d^{-1}*"]")) +
  ylab(expression(atop("Filtered seasonal NEP,epi flux +"," long-term average [g DO"*~m^{-2}*""*~d^{-1}*"]"))) +
  # xlab('Day of the year') +
  scale_x_date(breaks = '2 months', labels = date_format("%b")) +
  scale_color_brewer(palette="Dark2") +
  scale_linetype_manual(values = c(1,1,1,1,1,4,4,4))+
  theme_minimal()+
  geom_hline(yintercept = 0,linetype="dotted", 
             color = "black", size=1) +
  theme(axis.title.x = element_blank(),
        legend.text = element_text(size = 20), axis.text.x= element_text(size = 20), plot.title = element_text(size = 20),
        axis.text.y= element_text(size = 20), text = element_text(size = 15), legend.title = element_blank(), strip.text =element_text(size = 20),
        legend.position = 'bottom')+
  guides(); g4#linetype = FALSE

g5 = ggplot(seasonal.df.hyp[!is.na(seasonal.df$movavg),], 
            aes(as.Date(yday, origin = as.Date('2019-01-01')), (movavg) + NEP_avg2, col = id)) + #*volume/area/1000
  geom_ribbon(aes(ymin = movavg_min + NEP_avg2, ymax = movavg_max + NEP_avg2, col = id, fill = id), size = 0.3, alpha = 0.03) +
  geom_line(aes(linetype=id, col = id), size = 1.5) +
  # ylab(expression("7-day mov. average of seasonal decomposed total flux rate [g DO"*~m^{-2}*""*~d^{-1}*"]")) +
  ylab(expression(atop("Filtered seasonal NEP,hypo flux +"," long-term average [g DO"*~m^{-2}*""*~d^{-1}*"]"))) +
  # xlab('Day of the year') +
  scale_x_date(breaks = '2 months', labels = date_format("%b")) +
  scale_color_brewer(palette="Dark2") +
  scale_linetype_manual(values = c(1,1,1,1,1,4,4,4))+
  theme_minimal()+
  geom_hline(yintercept = 0,linetype="dotted", 
             color = "black", size=1) +
  theme(axis.title.x = element_blank(),
        legend.text = element_text(size = 20), axis.text.x= element_text(size = 20), plot.title = element_text(size = 20),
        axis.text.y= element_text(size = 20), text = element_text(size = 15), legend.title = element_blank(), strip.text =element_text(size = 20),
        legend.position = 'bottom')+
  guides(); g5#linetype = FALSE

g6 = ggplot(seasonal.df.sed[!is.na(seasonal.df$movavg),], 
            aes(as.Date(yday, origin = as.Date('2019-01-01')), (movavg) + NEP_avg2, col = id)) + #*volume/area/1000
  geom_ribbon(aes(ymin = movavg_min + NEP_avg2, ymax = movavg_max + NEP_avg2, col = id, fill = id), size = 0.3, alpha = 0.03) +
  geom_line(aes(linetype=id, col = id), size = 1.5) +
  # ylab(expression("7-day mov. average of seasonal decomposed total flux rate [g DO"*~m^{-2}*""*~d^{-1}*"]")) +
  ylab(expression(atop("Filtered seasonal SED flux +"," long-term average [g DO"*~m^{-2}*""*~d^{-1}*"]"))) +
  # xlab('Day of the year') +
  scale_x_date(breaks = '2 months', labels = date_format("%b")) +
  scale_color_brewer(palette="Dark2") +
  scale_linetype_manual(values = c(1,1,1,1,1,4,4,4))+
  theme_minimal()+
  geom_hline(yintercept = 0,linetype="dotted", 
             color = "black", size=1) +
  theme(axis.title.x = element_blank(),
        legend.text = element_text(size = 20), axis.text.x= element_text(size = 20), plot.title = element_text(size = 20),
        axis.text.y= element_text(size = 20), text = element_text(size = 15), legend.title = element_blank(), strip.text =element_text(size = 20),
        legend.position = 'bottom')+
  guides(); g6#linetype = FALSE

g7 = (g1 + g4) / (g5 + g6) + plot_annotation(tag_levels = 'A') + plot_layout(guides = 'collect') &
  theme(legend.position='bottom'); g7

ggsave(file = 'Figures/Fig_5.png', g7, dpi = 300, width =500, height = 300, units='mm')
ggsave(file = 'Figures/Fig_5.pdf', g7, dpi = 600, width =500, height = 300, units='mm')

ggplot() +
  geom_line(data = subset(seasonal.df.hyp[!is.na(seasonal.df$movavg),], id == c('Mendota', 'Monona')),
            aes(as.Date(yday, origin = as.Date('2019-01-01')), (movavg) + NEP_avg2, col = id, linetype = id), size = 1.5) + #*volume/area/1000
  geom_line(data = subset(seasonal.df.sed[!is.na(seasonal.df$movavg),], id == c('Mendota', 'Monona')),
            aes(as.Date(yday, origin = as.Date('2019-01-01')), (movavg) + NEP_avg2, col = id), size = 1.5, linetype = 'dashed') +
  # geom_ribbon(aes(ymin = movavg_min + NEP_avg2, ymax = movavg_max + NEP_avg2, col = id, fill = id), size = 0.3, alpha = 0.03) +
  # geom_line(aes(linetype=id, col = id), size = 1.5) +
  # ylab(expression("7-day mov. average of seasonal decomposed total flux rate [g DO"*~m^{-2}*""*~d^{-1}*"]")) +
  ylab(expression(atop("Filtered seasonal SED flux +"," long-term average [g DO"*~m^{-2}*""*~d^{-1}*"]"))) +
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
  guides(); #linetype = FALSE

cum.seasonal.df$id <- factor(cum.seasonal.df$id , levels= (c("Allequash","BigMuskellunge","Crystal","Sparkling", "Trout","Fish","Mendota","Monona")))

g2=ggplot(cum.seasonal.df, aes(as.Date(yday, origin = as.Date('2019-01-01')), movavg + NEP_avg2, col = id)) +
  geom_ribbon(aes(ymin = movavg_min + NEP_avg2, ymax = movavg_max + NEP_avg2, col = id), size = 0.3, alpha = 0.1)+
  geom_line(aes(linetype=id, col = id), size = 1.5)+
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

# ggsave(file = 'Figures/Fig_5.png', g1, dpi = 300, width =300, height = 150, units='mm')
