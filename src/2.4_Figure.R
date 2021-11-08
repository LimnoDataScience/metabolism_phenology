#### Manuscript Figure 4 ####
library(tidyverse)
library(patchwork)

# Load data
cum.flux = read_csv('Processed_Output/Fluxes_cumFlux.csv')
cum.flux_max = read_csv('Processed_Output/Fluxes_cumFluxmax.csv')
cum.flux_min = read_csv('Processed_Output/Fluxes_cumFluxmin.csv')

# total.flux$TP = c(15.7, 8.6, 5.6, 22.4, 109.5, 73.5, 7.2, 6.9)#, 40.3)

# name.chane.annual.flux <- annual.flux
# colnames(name.chane.annual.flux) <- c('Year', 'ATM', 'NEP_epi', 'NEP_hypo', 'SED', 'NEP_tot', 'ENTR_epi',
#                                       'ENTR_hypo', 'ID')
colnames(cum.flux) <- c('time', 'Atm', 'NEP,epi', 'NEP,hypo','Sed','Total NEP', 'id')
cum.flux$Sed = cum.flux$Sed * (-1)
m.cum.flux = reshape2::melt(cum.flux[,-c(1)])
m.cum.flux$time = cum.flux$time
# Downscale for plotting 
m.cum.flux.slice = m.cum.flux %>%
  filter(row_number() %% 10 == 1)

colnames(cum.flux_max) <- c('time', 'Atm', 'NEP,epi', 'NEP,hypo','Sed','Total NEP', 'id')
cum.flux_max$Sed = cum.flux_max$Sed * (-1)
m.cum.flux_max = reshape2::melt(cum.flux_max[,-c(1)])
m.cum.flux_max$time = cum.flux_max$time
# Downscale for plotting 
m.cum.flux.slice_max = m.cum.flux_max %>%
  filter(row_number() %% 10 == 1)

colnames(cum.flux_min) <- c('time', 'Atm', 'NEP,epi', 'NEP,hypo','Sed','Total NEP', 'id')
cum.flux_min$Sed = cum.flux_min$Sed * (-1)
m.cum.flux_min = reshape2::melt(cum.flux_min[,-c(1)])
m.cum.flux_min$time = cum.flux_min$time
# Downscale for plotting 
m.cum.flux.slice_min = m.cum.flux_min %>%
  filter(row_number() %% 10 == 1)

# Custom theme for all plots
theme.custom <- theme_minimal(base_size = 8) +
  theme(axis.title.y = element_text(size = 7),
                      axis.title.x = element_blank(),
                      strip.text.x = element_text( margin = margin(b=0.2, t=0)), 
                      legend.text = element_text(size = 6),
                      legend.key.width = unit(0.3,"cm"),
                      legend.key.height = unit(0.2,"cm"),
                      legend.margin=margin(t = -0.3, unit='cm'),
                      legend.title = element_blank())

# Northern Lakes Cumulative Fluxes
g1.north = ggplot(subset(m.cum.flux.slice, id == c('Allequash', 
                                                   'BigMuskellunge',
                                                   'Crystal',
                                                   'Sparkling',
                                                   'Trout'))) +
  geom_line(aes(time, value/1000, color = variable, linetype = variable), size= 0.4) +
  ylim(c(-10,7.5))+
  facet_wrap(~id, ncol = 1) +
  scale_linetype_manual(values = c(1,1,1,1,3))+
  ylab(expression("Cum. fluxes [kg DO"*~m^{-2}*""*"]")) +
  scale_color_brewer(palette="Set1") +
  theme.custom +
  theme(legend.position = 'bottom') +
    guides(linetype = guide_legend(nrow = 2)); g1.north

# Southern Lakes Cumulative Fluxes
g1.south = ggplot(subset(m.cum.flux.slice, id %in% c('Fish',
                                                     'Mendota',
                                                     'Monona'))) +
  geom_line(aes(time, value/1000, col = variable, linetype = variable), size = 0.4) +
  facet_wrap(~id, ncol = 1) +
  ylim(c(-15,21))+
  scale_linetype_manual(values = c(1,1,1,1,3)) +
  ylab(expression("Cum. fluxes [kg DO"*~m^{-2}*""*"]")) + # ylab(expression("Cum. fluxes [kg DO"*~m^{-2}*""*~d^{-1}*"]")) +
  scale_color_brewer(palette="Set1") +
  theme.custom +
  theme(legend.position = 'none'); g1.south

# All lakes total NEP
cum.flux.slice = cum.flux %>% filter(row_number() %% 10 == 1)
cum.flux.slice$id <- factor(cum.flux.slice$id , levels= (c("Allequash","BigMuskellunge","Crystal","Sparkling", "Trout","Fish","Mendota","Monona")))

g3.NEP = ggplot(cum.flux.slice) +
  geom_hline(aes(yintercept = 0)) +
  geom_line(aes(time, `Total NEP`/1000, color = id)) +
  scale_color_brewer(palette="Dark2") +
  ylab(expression("Cum. total NEP [kg DO"*~m^{-2}*""*"]")) +
  theme.custom +
  theme(axis.title.y = element_text(size = 7, color = 'darkorange'),
    legend.position = 'bottom') +
  guides(color = guide_legend(nrow=3)); g3.NEP


g1 <- g1.north + (g1.south / g3.NEP + plot_layout(heights = c(3,2))) +
  plot_annotation(tag_levels = 'A'); g1

ggsave(file = 'Figures/Fig_4.png', g1, dpi = 700, width = 6.5, height = 4.5, units='in')
ggsave(file = 'Figures/Fig_4.pdf', g1, dpi = 600, width = 6.5, height = 4.5, units='in')


# Northern Lakes Cumulative Fluxes
g1.north = ggplot(subset(m.cum.flux.slice_max, id == c('Allequash', 
                                                   'BigMuskellunge',
                                                   'Crystal',
                                                   'Sparkling',
                                                   'Trout'))) +
  geom_line(aes(time, value/1000, color = variable, linetype = variable), size= 0.4) +
  ylim(c(-12,25))+
  facet_wrap(~id, ncol = 1) +
  scale_linetype_manual(values = c(1,1,1,1,3))+
  ylab(expression("Max. cum. fluxes [kg DO"*~m^{-2}*""*"]")) +
  scale_color_brewer(palette="Set1") +
  theme.custom +
  theme(legend.position = 'bottom') +
  guides(linetype = guide_legend(nrow = 2)); g1.north

# Southern Lakes Cumulative Fluxes
g1.south = ggplot(subset(m.cum.flux.slice_max, id %in% c('Fish',
                                                     'Mendota',
                                                     'Monona'))) +
  geom_line(aes(time, value/1000, col = variable, linetype = variable), size = 0.4) +
  facet_wrap(~id, ncol = 1) +
  ylim(c(-10,55))+
  scale_linetype_manual(values = c(1,1,1,1,3)) +
  ylab(expression("Max. cum. fluxes [kg DO"*~m^{-2}*""*"]")) + # ylab(expression("Cum. fluxes [kg DO"*~m^{-2}*""*~d^{-1}*"]")) +
  scale_color_brewer(palette="Set1") +
  theme.custom +
  theme(legend.position = 'none'); g1.south

# All lakes total NEP
cum.flux.slice = cum.flux_max %>% filter(row_number() %% 10 == 1)
cum.flux.slice$id <- factor(cum.flux.slice$id , levels= (c("Allequash","BigMuskellunge","Crystal","Sparkling", "Trout","Fish","Mendota","Monona")))

g3.NEP = ggplot(cum.flux.slice) +
  geom_hline(aes(yintercept = 0)) +
  geom_line(aes(time, `Total NEP`/1000, color = id)) +
  scale_color_brewer(palette="Dark2") +
  ylab(expression("Max. cum. total NEP [kg DO"*~m^{-2}*""*"]")) +
  theme.custom +
  theme(axis.title.y = element_text(size = 7, color = 'darkorange'),
        legend.position = 'bottom') +
  guides(color = guide_legend(nrow=3)); g3.NEP


g1 <- g1.north + (g1.south / g3.NEP + plot_layout(heights = c(3,2))) +
  plot_annotation(tag_levels = 'A'); g1

ggsave(file = 'Figures/Fig_S4.png', g1, dpi = 700, width = 6.5, height = 4.5, units='in')
ggsave(file = 'Figures/Fig_S4.pdf', g1, dpi = 600, width = 6.5, height = 4.5, units='in')


# Northern Lakes Cumulative Fluxes
g1.north = ggplot(subset(m.cum.flux.slice_min, id == c('Allequash', 
                                                       'BigMuskellunge',
                                                       'Crystal',
                                                       'Sparkling',
                                                       'Trout'))) +
  geom_line(aes(time, value/1000, color = variable, linetype = variable), size= 0.4) +
  ylim(c(-25,5))+
  facet_wrap(~id, ncol = 1) +
  scale_linetype_manual(values = c(1,1,1,1,3))+
  ylab(expression("Min. cum. fluxes [kg DO"*~m^{-2}*""*"]")) +
  scale_color_brewer(palette="Set1") +
  theme.custom +
  theme(legend.position = 'bottom') +
  guides(linetype = guide_legend(nrow = 2)); g1.north

# Southern Lakes Cumulative Fluxes
g1.south = ggplot(subset(m.cum.flux.slice_min, id %in% c('Fish',
                                                         'Mendota',
                                                         'Monona'))) +
  geom_line(aes(time, value/1000, col = variable, linetype = variable), size = 0.4) +
  facet_wrap(~id, ncol = 1) +
  ylim(c(-40,5))+
  scale_linetype_manual(values = c(1,1,1,1,3)) +
  ylab(expression("Min. cum. fluxes [kg DO"*~m^{-2}*""*"]")) + # ylab(expression("Cum. fluxes [kg DO"*~m^{-2}*""*~d^{-1}*"]")) +
  scale_color_brewer(palette="Set1") +
  theme.custom +
  theme(legend.position = 'none'); g1.south

# All lakes total NEP
cum.flux.slice = cum.flux_min %>% filter(row_number() %% 10 == 1)
cum.flux.slice$id <- factor(cum.flux.slice$id , levels= (c("Allequash","BigMuskellunge","Crystal","Sparkling", "Trout","Fish","Mendota","Monona")))

g3.NEP = ggplot(cum.flux.slice) +
  geom_hline(aes(yintercept = 0)) +
  geom_line(aes(time, `Total NEP`/1000, color = id)) +
  scale_color_brewer(palette="Dark2") +
  ylab(expression("Min. cum. total NEP [kg DO"*~m^{-2}*""*"]")) +
  theme.custom +
  theme(axis.title.y = element_text(size = 7, color = 'darkorange'),
        legend.position = 'bottom') +
  guides(color = guide_legend(nrow=3)); g3.NEP


g1 <- g1.north + (g1.south / g3.NEP + plot_layout(heights = c(3,2))) +
  plot_annotation(tag_levels = 'A'); g1

ggsave(file = 'Figures/Fig_S6.png', g1, dpi = 700, width = 6.5, height = 4.5, units='in')
ggsave(file = 'Figures/Fig_S6.pdf', g1, dpi = 600, width = 6.5, height = 4.5, units='in')

