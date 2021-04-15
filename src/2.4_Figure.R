#### Manuscript Figure 4 ####
library(patchwork)

# Load data
cum.flux = read_csv('Processed_Output/Fluxes_cumFlux.csv')
cum.flux_max = read_csv('Processed_Output/Fluxes_cumFluxmax.csv')
cum.flux_min = read_csv('Processed_Output/Fluxes_cumFluxmin.csv')

# total.flux$TP = c(15.7, 8.6, 5.6, 22.4, 109.5, 73.5, 7.2, 6.9)#, 40.3)

# name.chane.annual.flux <- annual.flux
# colnames(name.chane.annual.flux) <- c('Year', 'ATM', 'NEP_epi', 'NEP_hypo', 'SED', 'NEP_tot', 'ENTR_epi',
#                                       'ENTR_hypo', 'ID')

# cum.flux$Sed = cum.flux$Sed * (-1)
m.cum.flux = reshape2::melt(cum.flux[,-c(1)])
m.cum.flux$time = cum.flux$time

# colnames(cum.flux_max) <- c('time', 'Atm', 'NEP,epi', 'NEP,hypo','Sed','Total NEP', 'id')
# cum.flux_max$Sed = cum.flux_max$Sed * (-1)
# m.cum.flux_max =reshape2::melt(cum.flux_max[,-c(1)])
# m.cum.flux_max$time = cum.flux_max$time
# 
# colnames(cum.flux_min) <- c('time', 'Atm', 'NEP,epi', 'NEP,hypo','Sed','Total NEP', 'id')
# cum.flux_min$Sed = cum.flux_min$Sed * (-1)
# m.cum.flux_min =reshape2::melt(cum.flux_min[,-c(1)])
# m.cum.flux_min$time = cum.flux_min$time
# 
# m.cum.flux.extr = m.cum.flux_min
# colnames(m.cum.flux.extr) = c('id', 'variable', 'min', 'time')
# m.cum.flux.extr$max = m.cum.flux_max$value
# m.cum.flux.extr$value = m.cum.flux$value

# idx.year = c()
# for (i in seq(1996,2019,1)){
#   idy = which(m.cum.flux.extr$time > i)
#   idz = match(m.cum.flux.extr$time[idy[which(abs(m.cum.flux.extr$time[idy] - i) == min(abs(m.cum.flux.extr$time[idy] - i)))]], 
#               m.cum.flux.extr$time)
#   idx.year = append(idx.year,
#                     idy[which(abs(m.cum.flux.extr$time[idy] - i) == min(abs(m.cum.flux.extr$time[idy] - i)))])
#   print(which(abs(m.cum.flux.extr$time[idy] - i) == min(abs(m.cum.flux.extr$time[idy] - i))))
#   if (length(which(abs(m.cum.flux.extr$time[idy] - i) == min(abs(m.cum.flux.extr$time[idy] - i)))) != 40){
#     print(i)
#   }
# }
# red.m.cum.flux.extr = m.cum.flux.extr[idx.year,]

# Downscale for plotting 
m.cum.flux.slice = m.cum.flux %>%
  filter(row_number() %% 10 == 1)

g1.north = ggplot(subset(m.cum.flux.slice, id == c('Allequash', 
                                           'BigMuskellunge',
                                           'Crystal',
                                           'Sparkling',
                                           'Trout'))) +
  geom_line(aes(time, value/1000, col = variable, linetype = variable), size= 0.4) +
  ylim(c(-8,10))+
  facet_wrap(~id) +
  scale_linetype_manual(values = c(1,1,1,1,2))+
  ylab(expression("Cum. fluxes [kg DO"*~m^{-2}*""*~d^{-1}*"]")) +
  xlab('') +
  scale_color_brewer(palette="Set1") +
  theme_minimal(base_size = 8) +
  theme( axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
         axis.title.y = element_text(size = 7),
         axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = 'none'); g1.north

g1.south = ggplot(subset(m.cum.flux.slice, id %in% c('Fish',
                                           'Mendota',
                                           'Monona'))) +
  geom_line(aes(time, value/1000, col = variable, linetype = variable), size = 0.4) +
  facet_wrap(~id) +
  ylim(c(-8,10))+
  # ylim(c(-25,25))+
  # ylim(c(-8,10))+
  scale_linetype_manual(values = c(1,1,1,1,2)) +
  ylab(expression("Cum. fluxes [kg DO"*~m^{-2}*""*~d^{-1}*"]")) +
  xlab('') +
  scale_color_brewer(palette="Set1") +
  theme_minimal(base_size = 8) +
  theme( axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
         axis.title.x = element_blank(),
         axis.title.y = element_text(size = 7),
         legend.title = element_blank(),
         legend.margin=margin(t = -0.3, unit='cm'),
         legend.position = 'bottom'); g1.south
  # theme_minimal()+
  # theme(legend.text = element_text(size = 30), axis.text.x= element_text(size = 30, angle = 90, vjust = 0.5, hjust=1), plot.title = element_text(size = 30),
  #       axis.text.y= element_text(size = 30), text = element_text(size = 30), legend.title = element_blank(), strip.text =element_text(size = 30),
  #       legend.position = 'bottom');g1.south

g1 <- g1.north / g1.south +
  plot_annotation(tag_levels = 'A')+
  plot_layout(heights = (c(2, 1))); g1

ggsave(file = 'Figures/Fig_4_test.png', g1, dpi = 700, width = 4, height = 3.5, units='in')

