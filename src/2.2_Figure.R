#### Manuscript Figure 2 ####
library(tidyverse)
library(patchwork)
interpolate_nearest.neighbor <- function(data){
  for (i in 1:ncol(data)){
    idx <- which(!is.na(data[,i]))
    for (j in seq(1, length(data[,i]))[!(seq(1, length(data[,i])) %in% idx)]){
      find.min <- which.min((idx-j)^2)
      data[j,i] = data[idx[find.min], i]
    }
  }
  return(data)
}


mendota = read_csv('Processed_Output/mendota_fluxes.csv')

custom.theme = theme_minimal() + 
  theme(axis.title.x = element_blank(),
  legend.text = element_text(size = 11), axis.text.x= element_text(size = 18), plot.title = element_text(size = 18),
  axis.text.y= element_text(size = 18), text = element_text(size = 18), legend.title = element_blank(), strip.text =element_text(size = 18),
  legend.position = 'bottom', 
  legend.margin=margin(t = -0.3, unit='cm'),
  plot.margin = unit(c(0,0.2,0,0.2), "cm"))

g.conc <- ggplot(subset(mendota, year > 2002 & year < 2006)) + 
  geom_line(aes(x=datetime, y=o2_epi_middle/1000, col = 'Epilimnion sim.')) +
  geom_ribbon(aes(x=datetime, ymin=o2_epi_lower/1000, ymax=o2_epi_upper/1000, col = 'Epilimnion sim.'),linetype =2, alpha=0.2) +
  geom_point(aes(x=datetime, y=DO_obs_epi/1000, col = 'Epilimnion obs.'), size = 2) +
  geom_line(aes(x=datetime, y=o2_hyp_middle/1000, col ='Hypolimnion sim.')) +
  geom_ribbon(aes(x=datetime, ymin=o2_hyp_lower/1000, ymax=o2_hyp_upper/1000, col = 'Hypolimnion sim.'),linetype =2, alpha=0.2) +
  geom_point(aes(x=datetime, y=DO_obs_hyp/1000, col= 'Hypolimnion obs.'), size = 2) +
  geom_point(aes(x=datetime, y=DO_obs_tot/1000, col = 'Total obs.'), size=2) +
  # facet_wrap(~ year) +
  ylab(expression("Conc. [g DO"*~m^{-3}*"]")) +
  scale_color_manual(values = c('red1','red4','lightblue3','lightblue1','gold'),
                     guide = guide_legend(override.aes = list(
                       linetype = c('blank', 'solid', 'blank', 'solid', 'blank'),
                     shape = c(16, NA, 16, NA, 16)))) +
  xlab('') +
  # ggtitle(paste0(lake.id,'_RMSE:',rmse,'_NSE:',nse))+
  ylim(c(-2,20)) +  
  custom.theme; g.conc

g.flux <- ggplot(subset(mendota, year > 2002 & year < 2006)) + 
  geom_line(aes(x=datetime, y=fnep_middle, col = 'Fnep,epi')) +
  geom_ribbon(aes(x=datetime, ymin=fnep_lower, ymax=fnep_upper, col = 'Fnep,epi'), linetype =2,alpha=0.2) +
  geom_line(aes(x=datetime, y=fmineral_middle, col = 'Fnep,hypo')) +
  geom_ribbon(aes(x=datetime, ymin=fmineral_lower, ymax=fmineral_upper, col = 'Fnep,hypo'), linetype =2, alpha=0.2) +
  geom_line(aes(x=datetime, y=(-1)*fsed2_middle, col = 'Fsed')) +
  geom_ribbon(aes(x=datetime, ymin=(-1)*fsed2_lower, ymax=(-1)*fsed2_upper, col = 'Fsed'), linetype =2,alpha=0.2) +
  # geom_line(aes(x=datetime, y=fatm_middle, col = 'Fatm')) +
  # geom_ribbon(aes(x=datetime, ymin=fatm_lower, ymax=fatm_upper, col = 'Fatm'), alpha=0.2) +
  ylab(expression("Sim. fluxes [g DO"*~m^{-3}*~d^{-1}*"]")) +
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  # ggtitle(paste0(lake.id,'_RMSE:',rmse,'_NSE:',nse))+
  custom.theme; g.flux

j.m <- mendota
j.m$nNEP <- interpolate_nearest.neighbor(data = as.matrix(j.m$NEP, col = 1))
j.m$nNEP_max <- interpolate_nearest.neighbor(data = as.matrix(j.m$NEP_max, col = 1))
j.m$nNEP_min <- interpolate_nearest.neighbor(data = as.matrix(j.m$NEP_min, col = 1))
j.m$nMIN <- interpolate_nearest.neighbor(data = as.matrix(j.m$MIN, col = 1))
j.m$nSED <- interpolate_nearest.neighbor(data = as.matrix(j.m$SED, col = 1))
j.m$nMIN_max <- interpolate_nearest.neighbor(data = as.matrix(j.m$MIN_max, col = 1))
j.m$nMIN_min <- interpolate_nearest.neighbor(data = as.matrix(j.m$MIN_min, col = 1))
j.m$nSED_max <- interpolate_nearest.neighbor(data = as.matrix(j.m$SED_max, col = 1))
j.m$nSED_min <- interpolate_nearest.neighbor(data = as.matrix(j.m$SED_min, col = 1))
g.param <- ggplot(subset(j.m, year > 2002 & year < 2006)) + 
  geom_line(aes(x=datetime, y=(nNEP), col ='Xnep,epi'),) +
  geom_ribbon(aes(x=datetime, ymin=nNEP_min, ymax=nNEP_max, col = 'Xnep,epi'), linetype =2,alpha=0.2) +
  # geom_ribbon(aes(x=datetime, ymin=fnep_lower, ymax=fnep_upper), alpha=0.2) +
  geom_line(aes(x=datetime, y=(nMIN), col ='Xnep,hypo')) +
  geom_ribbon(aes(x=datetime, ymin=nMIN_min, ymax=nMIN_max, col = 'Xnep,hypo'), linetype =2,alpha=0.2) +
  ylab(expression("Xnep [g DO"*~m^{-3}*~d^{-1}*"]")) +
  # geom_ribbon(aes(x=datetime, ymin=fmineral_lower, ymax=fmineral_upper), alpha=0.2) +
  geom_line(aes(x=datetime, y=(-1) * (nSED)/6, col = 'Xsed')) +
  geom_ribbon(aes(x=datetime, ymin=-1 * nSED_min/6, ymax= -1 *nSED_max/6, col = 'Xsed'), linetype =2,alpha=0.2) +
  scale_y_continuous(sec.axis = sec_axis(~.*6, name = 
                                           expression("Xsed [g DO"*~m^{-2}*~d^{-1}*"]"))       )+
  scale_color_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
  # ggtitle(paste0(lake.id,'_RMSE:',rmse,'_NSE:',nse))+
  custom.theme; g.param

g <-g.param /g.flux / g.conc + plot_annotation(tag_levels = 'A'); g

ggsave(file = paste0('Figures/Fig_2.png'), g, dpi = 300, width =266, height = 300,
       units='mm')

ggsave(file = paste0('Figures/Fig_2.pdf'), g, dpi = 600, width =266, height = 300,
       units='mm')


