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

seasonal.df.north = seasonal.df %>% filter(id %in% c("Allequash","BigMuskellunge","Crystal","Sparkling", "Trout"))
seasonal.df.epi.north = seasonal.df.epi %>% filter(id %in% c("Allequash","BigMuskellunge","Crystal","Sparkling", "Trout"))
seasonal.df.hyp.north = seasonal.df.hyp %>% filter(id %in% c("Allequash","BigMuskellunge","Crystal","Sparkling", "Trout"))
seasonal.df.sed.north = seasonal.df.sed %>% filter(id %in% c("Allequash","BigMuskellunge","Crystal","Sparkling", "Trout"))

seasonal.df.south = seasonal.df %>% filter(id %in% c("Fish","Mendota","Monona"))
seasonal.df.epi.south = seasonal.df.epi %>% filter(id %in% c("Fish","Mendota","Monona"))
seasonal.df.hyp.south = seasonal.df.hyp %>% filter(id %in% c("Fish","Mendota","Monona"))
seasonal.df.sed.south = seasonal.df.sed %>% filter(id %in% c("Fish","Mendota","Monona"))

g1 = ggplot(seasonal.df.north[!is.na(seasonal.df.north$movavg),], 
            aes(as.Date(yday, origin = as.Date('2019-01-01')), (movavg) + NEP_avg2, col = id)) + #*volume/area/1000
  geom_ribbon(aes(ymin = movavg_min + NEP_avg2, ymax = movavg_max + NEP_avg2, col = id, fill = id), size = 0.3, alpha = 0.03) +
  geom_line(aes(linetype=id, col = id), size = 1.5) +
  # ylab(expression("7-day mov. average of seasonal decomposed total flux rate [g DO"*~m^{-2}*""*~d^{-1}*"]")) +
  ylab(expression(atop("Filtered seasonal total NEP flux +","long-term average [g DO"*~m^{-2}*""*~d^{-1}*"]"))) +
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

g4 = ggplot(seasonal.df.epi.north[!is.na(seasonal.df.epi.north$movavg),], 
            aes(as.Date(yday, origin = as.Date('2019-01-01')), (movavg) + NEP_avg2, col = id)) + #*volume/area/1000
  geom_ribbon(aes(ymin = movavg_min + NEP_avg2, ymax = movavg_max + NEP_avg2, col = id, fill = id), size = 0.3, alpha = 0.03) +
  geom_line(aes(linetype=id, col = id), size = 1.5) +
  # ylab(expression("7-day mov. average of seasonal decomposed total flux rate [g DO"*~m^{-2}*""*~d^{-1}*"]")) +
  ylab(expression(atop("Filtered seasonal NEP,epi flux +"," long-term average [g DO"*~m^{-2}*""*~d^{-1}*"]"))) +
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
  guides(); g4#linetype = FALSE

g5 = ggplot(seasonal.df.hyp.north[!is.na(seasonal.df.hyp.north$movavg),], 
            aes(as.Date(yday, origin = as.Date('2019-01-01')), (movavg) + NEP_avg2, col = id)) + #*volume/area/1000
  geom_ribbon(aes(ymin = movavg_min + NEP_avg2, ymax = movavg_max + NEP_avg2, col = id, fill = id), size = 0.3, alpha = 0.03) +
  geom_line(aes(linetype=id, col = id), size = 1.5) +
  # ylab(expression("7-day mov. average of seasonal decomposed total flux rate [g DO"*~m^{-2}*""*~d^{-1}*"]")) +
  ylab(expression(atop("Filtered seasonal NEP,hypo flux +"," long-term average [g DO"*~m^{-2}*""*~d^{-1}*"]"))) +
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
  guides(); g5#linetype = FALSE

g6 = ggplot(seasonal.df.sed.north[!is.na(seasonal.df.sed.north$movavg),], 
            aes(as.Date(yday, origin = as.Date('2019-01-01')), (movavg) + NEP_avg2, col = id)) + #*volume/area/1000
  geom_ribbon(aes(ymin = movavg_min + NEP_avg2, ymax = movavg_max + NEP_avg2, col = id, fill = id), size = 0.3, alpha = 0.03) +
  geom_line(aes(linetype=id, col = id), size = 1.5) +
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
  guides(); g6#linetype = FALSE

g7 = (g1 + g4) / (g5 + g6) + plot_annotation(tag_levels = 'A') + plot_layout(guides = 'collect') &
  theme(legend.position='bottom'); g7

ggsave(file = 'Figures/Fig_S2.png', g7, dpi = 300, width =500, height = 300, units='mm')

library(RColorBrewer)
my_palette = brewer.pal(8, "Dark2")[c(6,7,8)]
g1 = ggplot(seasonal.df.south[!is.na(seasonal.df.south$movavg),], 
            aes(as.Date(yday, origin = as.Date('2019-01-01')), (movavg) + NEP_avg2, col = id)) + #*volume/area/1000
  geom_ribbon(aes(ymin = movavg_min + NEP_avg2, ymax = movavg_max + NEP_avg2, col = id, fill = id), size = 0.3, alpha = 0.03) +
  geom_line(aes(linetype=id, col = id), size = 1.5) +
  # ylab(expression("7-day mov. average of seasonal decomposed total flux rate [g DO"*~m^{-2}*""*~d^{-1}*"]")) +
  ylab(expression(atop("Filtered seasonal total NEP flux +","long-term average [g DO"*~m^{-2}*""*~d^{-1}*"]"))) +
  # xlab('Day of the year') +
  scale_x_date(breaks = '2 months', labels = date_format("%b")) +
  # scale_color_brewer(palette="Dark2") +
  scale_color_manual(values = my_palette[3]) +
  scale_linetype_manual(values = c(2,2,2,1,1,2,2,2))+
  theme_minimal()+
  geom_hline(yintercept = 0,linetype="dotted", 
             color = "black", size=1) +
  theme(axis.title.x = element_blank(),
        legend.text = element_text(size = 20), axis.text.x= element_text(size = 20), plot.title = element_text(size = 20),
        axis.text.y= element_text(size = 20), text = element_text(size = 15), legend.title = element_blank(), strip.text =element_text(size = 20),
        legend.position = 'bottom')+
  guides(); g1#linetype = FALSE

g4 = ggplot(seasonal.df.epi.south[!is.na(seasonal.df.epi.south$movavg),], 
            aes(as.Date(yday, origin = as.Date('2019-01-01')), (movavg) + NEP_avg2, col = id)) + #*volume/area/1000
  geom_ribbon(aes(ymin = movavg_min + NEP_avg2, ymax = movavg_max + NEP_avg2, col = id, fill = id), size = 0.3, alpha = 0.03) +
  geom_line(aes(linetype=id, col = id), size = 1.5) +
  # ylab(expression("7-day mov. average of seasonal decomposed total flux rate [g DO"*~m^{-2}*""*~d^{-1}*"]")) +
  ylab(expression(atop("Filtered seasonal NEP,epi flux +"," long-term average [g DO"*~m^{-2}*""*~d^{-1}*"]"))) +
  # xlab('Day of the year') +
  scale_x_date(breaks = '2 months', labels = date_format("%b")) +
  scale_color_manual(values = my_palette) +
  scale_linetype_manual(values = c(2,2,2,1,1,2,2,2))+
  theme_minimal()+
  geom_hline(yintercept = 0,linetype="dotted", 
             color = "black", size=1) +
  theme(axis.title.x = element_blank(),
        legend.text = element_text(size = 20), axis.text.x= element_text(size = 20), plot.title = element_text(size = 20),
        axis.text.y= element_text(size = 20), text = element_text(size = 15), legend.title = element_blank(), strip.text =element_text(size = 20),
        legend.position = 'bottom')+
  guides(); g4#linetype = FALSE

g5 = ggplot(seasonal.df.hyp.south[!is.na(seasonal.df.hyp.south$movavg),], 
            aes(as.Date(yday, origin = as.Date('2019-01-01')), (movavg) + NEP_avg2, col = id)) + #*volume/area/1000
  geom_ribbon(aes(ymin = movavg_min + NEP_avg2, ymax = movavg_max + NEP_avg2, col = id, fill = id), size = 0.3, alpha = 0.03) +
  geom_line(aes(linetype=id, col = id), size = 1.5) +
  # ylab(expression("7-day mov. average of seasonal decomposed total flux rate [g DO"*~m^{-2}*""*~d^{-1}*"]")) +
  ylab(expression(atop("Filtered seasonal NEP,hypo flux +"," long-term average [g DO"*~m^{-2}*""*~d^{-1}*"]"))) +
  # xlab('Day of the year') +
  scale_x_date(breaks = '2 months', labels = date_format("%b")) +
  scale_color_manual(values = my_palette) +
  scale_linetype_manual(values = c(2,2,2,1,1,2,2,2))+
  theme_minimal()+
  geom_hline(yintercept = 0,linetype="dotted", 
             color = "black", size=1) +
  theme(axis.title.x = element_blank(),
        legend.text = element_text(size = 20), axis.text.x= element_text(size = 20), plot.title = element_text(size = 20),
        axis.text.y= element_text(size = 20), text = element_text(size = 15), legend.title = element_blank(), strip.text =element_text(size = 20),
        legend.position = 'bottom')+
  guides(); g5#linetype = FALSE

g6 = ggplot(seasonal.df.sed.south[!is.na(seasonal.df.sed.south$movavg),], 
            aes(as.Date(yday, origin = as.Date('2019-01-01')), (movavg) + NEP_avg2, col = id)) + #*volume/area/1000
  geom_ribbon(aes(ymin = movavg_min + NEP_avg2, ymax = movavg_max + NEP_avg2, col = id, fill = id), size = 0.3, alpha = 0.03) +
  geom_line(aes(linetype=id, col = id), size = 1.5) +
  # ylab(expression("7-day mov. average of seasonal decomposed total flux rate [g DO"*~m^{-2}*""*~d^{-1}*"]")) +
  ylab(expression(atop("Filtered seasonal SED flux +"," long-term average [g DO"*~m^{-2}*""*~d^{-1}*"]"))) +
  # xlab('Day of the year') +
  scale_x_date(breaks = '2 months', labels = date_format("%b")) +
  scale_color_manual(values = my_palette) +
  scale_linetype_manual(values = c(2,2,2,1,1,2,2,2))+
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

ggsave(file = 'Figures/Fig_S3.png', g7, dpi = 300, width =500, height = 300, units='mm')
