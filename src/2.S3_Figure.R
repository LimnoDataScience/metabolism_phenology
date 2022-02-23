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

namelist = c('Allequash', 'BigMuskellunge',
             'Crystal', 'Fish', 'Mendota', 'Monona',
             'Sparkling', 'Trout')

for (i in 1:8){
  
  if (i == 1){
    data = read_csv('Processed_Output/allequash_fluxes.csv')
  }  else if (i ==2 ){
    data =read_csv('Processed_Output/bigmuskellunge_fluxes.csv')
  }else if (i ==3 ){
    data = read_csv('Processed_Output/crystal_fluxes.csv')
  }else if (i ==4 ){
    data = read_csv('Processed_Output/fish_fluxes.csv')
  }else if (i ==5 ){
    data = read_csv('Processed_Output/mendota_fluxes.csv')
  }else if (i ==6 ){
    data = read_csv('Processed_Output/monona_fluxes.csv')
  }else if (i ==7 ){
    data = read_csv('Processed_Output/sparkling_fluxes.csv')
  }else if (i ==8 ){
    data =  read_csv('Processed_Output/trout_fluxes.csv')
  }
  
  custom.theme = theme_minimal() + 
    theme(axis.title.x = element_blank(),
          legend.text = element_text(size = 11), axis.text.x= element_text(size = 18), plot.title = element_text(size = 18),
          axis.text.y= element_text(size = 18), text = element_text(size = 18), legend.title = element_blank(), strip.text =element_text(size = 18),
          legend.position = 'bottom', 
          legend.margin=margin(t = -0.3, unit='cm'),
          plot.margin = unit(c(0,0.2,0,0.2), "cm"))

  g.conc <- ggplot(data) + 
    geom_line(aes(x=doy, y=o2_epi_middle/1000, col = 'Epilimnion sim.')) +
    geom_ribbon(aes(x=doy, ymin=o2_epi_lower/1000, ymax=o2_epi_upper/1000, col = 'Epilimnion sim.'),linetype =2, alpha=0.2) +
    geom_point(aes(x=doy, y=DO_obs_epi/1000, col = 'Epilimnion obs.'), size = 2) +
    geom_line(aes(x=doy, y=o2_hyp_middle/1000, col ='Hypolimnion sim.')) +
    geom_ribbon(aes(x=doy, ymin=o2_hyp_lower/1000, ymax=o2_hyp_upper/1000, col = 'Hypolimnion sim.'),linetype =2, alpha=0.2) +
    geom_point(aes(x=doy, y=DO_obs_hyp/1000, col= 'Hypolimnion obs.'), size = 2) +
    geom_point(aes(x=doy, y=DO_obs_tot/1000, col = 'Total obs.'), size=2) +
    facet_wrap(~ year) +
    xlab('Day of the year') +
    ylab(expression("Conc. [g DO"*~m^{-3}*"]")) +
    scale_color_manual(values = c('red1','red4','lightblue3','lightblue1','gold'),
                       guide = guide_legend(override.aes = list(
                         linetype = c('blank', 'solid', 'blank', 'solid', 'blank'),
                         shape = c(16, NA, 16, NA, 16)))) +

    # ggtitle(paste0(lake.id,'_RMSE:',rmse,'_NSE:',nse))+
    ylim(c(-2,20)) +  
    custom.theme; g.conc
  
  
  g <- g.conc 
  
  ggsave(file = paste0('Figures/Fig_S3_',namelist[i],'.png'), g, dpi = 300, width =350, height = 300,
         units='mm')
  
  ggsave(file =  paste0('Figures/Fig_S3_',namelist[i],'.png'), g, dpi = 600, width =350, height = 300,
         units='mm')
}


