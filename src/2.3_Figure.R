#### Manuscript Figure 3 ####
fitdata = read_csv('Processed_Output/fitdata.csv')

g.fits <- setNames(data.frame(matrix(ncol = 5, nrow = 8)), c('id', 'MAE', 'NSE', 'RMSE', 'R2'))
g.fits$id = unique(fitdata$id)
RMSE = fitdata %>%
  group_by(id) %>%
  summarise(RMSE = sqrt(sum((obsdata-simdata)**2)/length(obsdata)) * 1/1000)
MAE = fitdata %>%
  group_by(id) %>%
  summarise(MAE = sum(abs(obsdata-simdata))/length(obsdata) * 1/1000)
NSE = fitdata %>%
  group_by(id) %>%
  summarise(NSE = 1- sum((obsdata-simdata)**2)/sum((obsdata-mean(obsdata))**2))
R2 = fitdata %>%
  group_by(id) %>%
  summarise(r2 = cor(obsdata,simdata, method = 'pearson'))

g.fits$RMSE = RMSE$RMSE
g.fits$MAE = MAE$MAE
g.fits$NSE = NSE$NSE
g.fits$R2 = R2$r2
print(g.fits)

# 
# fitdata$RMSE =NA; fitdata$MAE =NA; fitdata$NSE =NA; fitdata$R2 =NA
# for (i in unique(fitdata$id)){
#   idx = which(i == fitdata$id)
#   idy = which(i == g.fits$id)
#   fitdata$RMSE[idx] = rep(g.fits$RMSE[idy], length(idx))
#   fitdata$MAE[idx] = rep(g.fits$MAE[idy], length(idx))
#   fitdata$NSE[idx] = rep(g.fits$NSE[idy], length(idx))
#   fitdata$R2[idx] = rep(g.fits$R2[idy], length(idx))
# }
# fitdata = fitdata %>% 
#   arrange(factor(id, levels = (c("Allequash","BigMuskellunge","Crystal","Sparkling", "Trout","","Fish","Mendota","Monona"))))
# g.fits$id <- factor(g.fits$id , levels= (c("Allequash","BigMuskellunge","Crystal","Sparkling", "Trout","Fish","Mendota","Monona")))
# fitdata$id <- factor(fitdata$id , levels= (c("Allequash","BigMuskellunge","Crystal","Sparkling", "Trout","","Fish","Mendota","Monona")))


g1 <- ggplot(fitdata, aes(x=obsdata/1000, y=simdata/1000, col = type))+
  geom_point(alpha = 0.5) +
  ylab(expression("Simulated DO conc. [g DO"*~m^{-3}*"]")) +
  xlab(expression("Observed DO conc. [g DO"*~m^{-3}*"]")) +
  scale_color_manual(values = rep(c('red1','lightblue3'),1)) +
  geom_text(
    data    = g.fits,
    mapping = aes(x = 0.1, y = 12, label = paste0('MAE: ',round((MAE),2))),
    hjust   = -0.1,
    vjust   = -1,
    col = 'black'
  ) +
  geom_text(
    data    = g.fits,
    mapping = aes(x = 4.3, y = 12, label =  paste0(', RMSE: ',round((RMSE),2))),
    hjust   = -0.1,
    vjust   = -1,
    col = 'black'
  ) +
  geom_text(
    data    = g.fits,
    mapping = aes(x = 0.1, y = 10, label =  paste0('NSE: ',round((NSE),2))),
    hjust   = -0.1,
    vjust   = -1,
    col = 'black'
  ) +
  geom_text(
    data    = g.fits,
    mapping = aes(x = 4.3, y = 10, label =  paste0(', R2: ',round((R2),2))),
    hjust   = -0.1,
    vjust   = -1,
    col = 'black'
  ) +
  facet_wrap(.~id, ncol = 3, drop = FALSE)+
  xlim(0,15.5)+ ylim(0,15.5)+
  theme_minimal()+
  theme(legend.text = element_text(size = 11), axis.text.x= element_text(size = 20), plot.title = element_text(size = 20),
        axis.text.y= element_text(size = 20), text = element_text(size = 20), legend.title = element_blank(), strip.text =element_text(size = 20),
        legend.position = 'bottom'); g1

ggsave(file = 'Figures/Fig_3.png', g1, dpi = 300, width =250, height = 250,
       units='mm')

# library(gridExtra)
# library(grid)
# g <- ggplotGrob(g1)
# ## remove empty panels
# g$grobs[names(g$grobs) %in% c("panel3", "panel9", "strip_t3", "strip_t9")] <- NULL
# ## remove them from the layout
# g$layout <- g$layout[!(g$layout$name %in% c("panel-3", "panel-9", 
#                                             "strip_t-3", "strip_t-9")),]
# ## move axis closer to panel
# g$layout[g$layout$name == "axis_b-9", c("t", "b")] = c(9,9)
# grid.newpage()
# grid.draw(g)